(defsystem "lulamoon"
  :version "0.0.1"
  :author "commonlainse"
  :maintainer "commonlainse"
  :mailto "femboygordonfreeman@gmail.com"
  :license "MIT"
  :homepage "https://github.com/fishtalking/lulamoon"
  :bug-tracker "https://github.com/fishtalking/lulamoon/issues"
  :source-control "https://github.com/fishtalking/lulamoon.git"
  :depends-on ("trivia"
               "websocket-driver-client"
               "alexandria"
               "iterate"
               "generic-serializer"
               "schemata"
               "schemata.serializers"
               "bordeaux-threads"
               "frugal-uuid"
	       "chipz"
	       "flexi-streams")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "two-way-byte-stream")
		 (:file "schemas"))))
  :description "Discord client"
  :long-description "A client as well as a library for interacting with Discord, trying to mimick in features and API to the official desktop client as close as possible"
  :in-order-to ((test-op (test-op "lulamoon/tests"))))

(defsystem "lulamoon/tests"
  :author "commonlainse"
  :license "MIT"
  :depends-on ("lulamoon"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lulamoon"
  :perform (test-op (op c) (symbol-call :rove :run c)))
