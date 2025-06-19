(uiop:define-package lulamoon.schemas
  (:import-from :schemata
		#:def-schema-class)
  (:export #:user

	   ;; accessors:
	   
	   #:id-of
	   #:username-of
	   #:discriminator-of
	   #:global-name-of
	   #:avatar-of
	   #:bot-of
	   #:system-of
	   #:mfa-enabled-of
	   #:banner-of
	   #:accent-color-of
	   #:locale-of
	   #:verified-of
	   #:email-of
	   #:flags-of
	   #:premium-type-of
	   #:public-flags-of
	   #:avatar-decoration-data-of
	   #:collectibles-of
	   #:primary-guild-of
	   #:clan-of)
  (:use #:cl))
(in-package #:lulamoon.schemas)

;; cl-json turns snake_case into this--thing

(def-schema-class user ()
  ((id :type string
       :accessor id-of)
   (username :type string
	     :accessor username-of)
   (discriminator :type string
		  :accessor discriminator-of)
   (global--name :initform nil
		 :required nil
		 :accessor global-name-of)
   (avatar :type string
	   :accessor avatar-of)
   (bot :type boolean
	:initform nil
	:required nil
	:accessor bot-of)
   (system :type boolean
	   :initform nil
	   :required nil
	   :accessor system-of)
   (mfa--enabled :type boolean
		 :initform nil
		 :required nil
		 :accessor mfa-enabled-of)
   (banner :initform nil
	   :required nil
	   :accessor banner-of
	   :documentation "string (or nil) of the banner's asset id")
   (accent--color :initform nil
		  :required nil
		  :accessor accent-color-of
		  :documentation "number representing the user's profile color")
   (locale :type string
	   :initform "en-US"
	   :required nil
	   :accessor locale-of)
   (verified :type boolean
	     :initform nil
	     :required nil
	     :accessor verified-of)
   (email :initform nil
	  :required nil
	  :accessor email-of
	  :documentation "string (or nil) of user's email")
   (flags :initform nil
	  :required nil
	  :accessor flags-of
	  :documentation "integer (or nil) representing user's flags")
   (premium--type :initform nil
		  :required nil
		  :accessor premium-type-of
		  :documentation
		  "integer (or nil) representing the user's premium type")
   (public--flags :initform nil
		  :required nil
		  :accessor public-flags-of
		  :documentation
		  "integer (or nil) represending the user's public flags")
   (avatar--decoration--data :initform nil
			     :required nil
			     :accessor avatar-decoration-data-of)
   
   ;; undocumented:
   
   (collectibles :accessor collectibles-of)
   (primary--guild :required nil
		   :accessor primary-guild-of)
   (clan :required nil
	 :accessor clan-of)))
