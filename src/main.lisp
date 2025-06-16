(uiop:define-package lulamoon
  (:import-from :schemata
		#:def-schema-class)
  (:import-from :json
		#:encode-json-plist-to-string)
  (:use #:cl #:trivia))
(in-package #:lulamoon)

(defconstant *gateway* "wss://gateway.discord.gg/?encoding=json&v=9") ; &compress=zlib-stream
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
   (%heartbeat :initform nil
	       :documentation
	       "Sends Opcode 1 (Hearbeat) according to Discord's interval
https://discord.com/developers/docs/events/gateway#sending-heartbeats")
   (%latest-sequence-number
    :initform `(:null)
    :documentation "Sent by Discord's dispatch events in the 's' field")))

(defvar *gateway-messages* '())

(defun on-message (gateway message)
  (let ((body (json:decode-json-from-string message)))
    (push body *gateway-messages*)
    (match body
      ;; HELLO
      ((alist (:op . 10)
	      (:d . (alist
		     (:heartbeat--interval . interval))))
       ;; send identify (2)
       (wsd:send (socket-of gateway)
		 (json:with-explicit-encoder
		   (encode-json-plist-to-string
		    `(:op 2
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
			  :client--state (:object :guild--versions (:object)))))))
       (format t "Identified.~%")
       
       ;; start sending heartbeat (1)
       (setf (slot-value gateway '%heartbeat)
	     (bt:make-thread
	      (lambda ()
		(loop
		  (sleep (* (/ interval 1000) (+ 0.5 (random 0.5))))
		  (wsd:send
		   (socket-of gateway)
		   (json:with-explicit-encoder
		     (encode-json-plist-to-string
		      `(:op 1 :d ,(slot-value gateway '%latest-sequence-number)))))
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
	      (:d . ready-body))
       (format t "Received READY")
       (with-open-file (str (format nil "ready-~A.txt" (random 1000))
			    :direction :output
			    :if-does-not-exist :create)
	 (format str "~A" message)))
      ;; Unknown dispatch event
      ((alist (:op . 0)
	      (:t . dispatch-event))
       (format t "Received unknown dispatch ~S~%" dispatch-event))
      (_
       (format t "Unknown message received (~A)~%" (assoc :op body))))))

(defun connect (gateway)
  (with-slots (socket %heartbeat) gateway
    (format t "Connecting...~%")
    (wsd:on :message socket
	    (lambda (message)
	      (on-message gateway message)))
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
