(uiop:define-package lulamoon
  (:import-from :schemata
		#:def-schema-class
		#:populate-with-schema
		#:find-schema)
  (:import-from :json
		#:encode-json-plist-to-string)
  (:local-nicknames (:schemas :lulamoon.schemas))
  (:use #:cl #:trivia #:arrows))
(in-package #:lulamoon)

(defconstant *gateway* "wss://gateway.discord.gg/?encoding=json&v=9&compress=zlib-stream")
(defconstant *user-agent* "Mozilla/5.0 (X11; Linux x86_64; rv:138.0) Gecko/20100101 Firefox/138.0")

(defvar *dc-build-number* 409214
  "Lattest build of Discord's official client to fake gateway as best possible.
TODO: this should be updated to automatically get the lattest number
https://github.com/Pixens/Discord-Build-Number/blob/main/main.py")

(defvar *dc-launch-id* (fuuid:to-string (fuuid:make-v4))
  "To fake gateway as best possible.")

(defclass gateway ()
  ((socket :type websocket-driver.ws.client:client
	   :initform (wsd:make-client *gateway*
				      :additional-headers
				      `(("User-Agent" . ,*user-agent*)))
	   :accessor socket-of)
   (token :type string
	  :initarg :token
	  :accessor token-of)
   (user :type schemas:user
	 :accessor user-of
	 :documentation
	 "User object of the current token. It's unbound until it `connect's.")
   (users :type hash-table
	  :initform (make-hash-table :size 100)
	  :accessor users-of
	  :documentation "Maps user id to user information")
   (%heartbeat :initform nil
	       :documentation
	       "Sends Opcode 1 (Heartbeat) according to Discord's interval
https://discord.com/developers/docs/events/gateway#sending-heartbeats")
   (%latest-sequence-number
    :initform nil
    :documentation "Sent by Discord's dispatch events in the 's' field")
   (%zlib-stream :documentation "Two way binary stream to decompress zlib")
   (%decompressor :documentation
		  "Decompressor for Discord's websocket
It's initialized at `connect' because it depends on %zlib-stream
Discord gateway can optionally be compressed with zlib-stream, opting in
simulates the official client better. Decompressing with zlib-stream is
the same as the classic zlib format, but the same decompressor state must be
used for every request. This obviously makes sense from an engineering
perspective, creating a new decompressor for every message received is
wasteful. But it does make this a little bit more annoying")))

(defvar *gateway-messages* '())

;; TODO: output a UTF-8 string
(defun decompress-message (gateway compressed-message)
  (with-slots (%zlib-stream %decompressor) gateway
    (write-sequence compressed-message %zlib-stream)
    (with-output-to-string (msg)
      (handler-case (->> (read-byte %decompressor)
			 (code-char)
			 (format msg "~C")
			 (loop))
	(chipz:premature-end-of-stream (e))))))

(defun on-message (gateway message-body)
  (push message-body *gateway-messages*)
  (match message-body
    ;; HELLO
    ((alist (:op . 10)
	    (:d . (alist
		   (:heartbeat--interval . interval))))
     ;; send identify (2)
     (->> `(:op 2
	    :d (:object
		:token ,(token-of gateway)
		:capabilities 161789
		:properties
		(:object
		 :os "Linux"
		 :browser "Firefox"
		 :device ""
		 :system--locale "en-US"
		 :has--client--mods false
		 :browser--user--agent ,*user-agent*
		 :browser--version "139.0"
		 :os--version ""
		 :referrer ""
		 :referring--domain ""
		 :referrer--current ""
		 :referring--domain--current ""
		 :release--channel "stable"
		 :client--build--number ,*dc-build-number*
		 :client--event--source (:null)
		 :client--launch--id ,*dc-launch-id*
		 :client--app--state "focused"
		 :is--fast--conect (:false)
		 :latest--headless--tasks (:array)
		 :latest--headless--task--run--seconds--before (:null)
		 :gateway--connect--reasons "AppSkeleton")
		:presence (:object :status "unknown"
				   :since 0
				   :activities (:array)
				   :afk (:false))
		:compress (:false)
		:client--state (:object :guild--versions (:object))))
	  (encode-json-plist-to-string)
	  (json:with-explicit-encoder)
	  (wsd:send (socket-of gateway)))
     (format t "Identified.~%")
     
     ;; start sending heartbeat (1)
     (setf (slot-value gateway '%heartbeat)
	   (bt:make-thread
	    (lambda ()
	      (loop
		(sleep (* (/ interval 1000) (+ 0.5 (random 0.5))))
		(->> `(:op 1 :d ,(or
				  (slot-value gateway '%latest-sequence-number)
				  `(:null)))
		     (encode-json-plist-to-string)
		     (json:with-explicit-encoder)
		     (wsd:send (socket-of gateway)))
		(format t "Sent heartbeat~%")))
	    :name "discord heartbeat")))
    ;; Heartbeat ACK
    ((assoc :op 11)
     (format t "Acknowledged heartbeat~%"))
    ;; Dispatch events
    ((alist (:op . 0)
	    (:s . sequence-number))
     (setf (slot-value gateway '%latest-sequence-number) sequence-number)
     (trivia.next:next))
    ;; READY
    ((alist (:op . 0)
	    (:t . "READY")
	    (:d . (alist (:user . user-body)
			 (:users . users-list))))
     (format t "Received READY")
     
     ;; Get info about logged in user
     (setf (user-of gateway) (make-instance 'schemas:user :client gateway))
     (populate-with-schema (find-schema 'schemas:user)
			   (user-of gateway)
			   user-body)
     
     ;; Populate user cache
     (dolist (user-body users-list)
       (let ((user (make-instance 'schemas:user :client gateway)))
	 (populate-with-schema (find-schema 'schemas:user) user user-body)
	 (setf (gethash (schemas:id-of user) (users-of gateway)) user))))
    ;; Unknown dispatch event
    ((alist (:op . 0)
	    (:t . dispatch-event))
     (format t "Received unknown dispatch ~S~%" dispatch-event))
    (_
     (format t "Unknown message received (~A)~%" (assoc :op body)))))

(defun connect (gateway)
  (with-slots (socket %heartbeat %decompressor %zlib-stream) gateway
    (format t "Initializing zlib~%")
    (setf %zlib-stream (lulamoon.two-way-byte-stream:make))
    (setf %decompressor
	  (chipz:make-decompressing-stream 'chipz:zlib %zlib-stream))
    (format t "Connecting...~%")
    (wsd:on :message socket
	    (lambda (message)
	      (->> (decompress-message gateway message)
		   (json:decode-json-from-string)
		   (on-message gateway))))
    (wsd:on :open socket
	    (lambda ()
	      (format t "Connected.~%")))
    (wsd:on :error socket
	    (lambda (error)
	      (format t "Got an error: ~S~%" error)))
    (wsd:on :close socket
	    (lambda (&key code reason)
	      (bt:destroy-thread %heartbeat)
	      (format t "Closed because '~A' (Code=~A)~%" reason code)))
    (wsd:start-connection socket)))
