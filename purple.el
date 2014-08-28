(require 'cl)

(defvar purple-accounts '())
(defvar purple-buddies '())

(defclass buddy ()
  ((id :type number :initarg id)
   (accound-id :type number :initarg account-id)
   (name :initarg name)
   (alias string :initarg alias)
   (onlinep number :initarg onlinep)
   (icon :initarg icon)))

(defvar purple-dbus-service "im.pidgin.purple.PurpleService")
(defvar purple-object "/im/pidgin/purple/PurpleObject")
(defvar purple-interface "im.pidgin.purple.PurpleInterface")

(defvar buddy-props '((name	.	"PurpleBuddyGetName")
		      (alias	.	"PurpleBuddyGetAlias")
		      (onlinep	.	"PurpleBuddyIsOnline")
		      (icon	.	"PurpleBuddyGetIcon")))

(defvar purple-signals '(("ReceivedImMsg"	.	purple-received-im-msg-handler)
			 ("BuddyStatusChanged"	.	purple-buddy-status-handler)
			 ("BuddySignedOff"	.	purple-buddy-signed-off-handler)
			 ("BuddySignedOn"	.	purple-buddy-signed-on-handler)))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defun purple-register-signal (sig fun)
  (dbus-register-signal :session purple-dbus-service
			purple-object purple-interface sig fun))

(defmacro purple-call-method (method &rest args)
  `(dbus-call-method :session purple-dbus-service
		     purple-object purple-interface ,method ,@args))

(defmacro purple-call-method-async (method handler &rest args)
  `(dbus-call-method-asynchronously :session purple-dbus-service
				    purple-object purple-interface
				    ,method ,handler ,@args))

;; Init
(defun purple-init ()
  (interactive)
  (setq purple-accounts (purple-account-list))
  (unless purple-accounts
    (error "Purple seems turned off."))
  (mapc 'purple-populate-buddies purple-accounts)
  (dolist (sig purple-signals)
    (purple-register-signal (car sig) (cdr sig))))

(defun purple-account-list ()
  (purple-call-method "PurpleAccountsGetAllActive"))

(defun purple-populate-buddies (id)
  (mapc (curry 'purple-get-buddy-info-async id)
	(purple-call-method "PurpleFindBuddies" :int32 id "")))

(defun purple-buddy-info-handler (buddy field data)
  (set-slot-value buddy field data))

(defun purple-get-buddy (id)
  (find id purple-buddies
	:test (lambda (x y) (equal id (oref y id)))))

(defun purple-get-buddy-info-async (account-id id)
  (let ((buddy (or (purple-get-buddy id)
		   (buddy id 'account-id account-id 'id id))))
    (add-to-list 'purple-buddies buddy t
		 (lambda (x y) (and (equal (oref x id) (oref y id))
				    (equal (oref x account-id) (oref y account-id)))))
    (dolist (prop buddy-props)
      (purple-call-method-async (cdr prop)
				(curry 'purple-buddy-info-handler buddy (car prop))
				:int32 id))))

;; Buddy signals
(defun purple-buddy-status-handler (&rest args))

(defun purple-buddy-signed-off-handler (id)
  (message (format "%d signed off" id))
  (purple-buddy-info-handler (purple-get-buddy id) 'onlinep 0))

(defun purple-buddy-signed-on-handler (id)
  (message (format "%d signed on" id))
  (purple-buddy-info-handler (purple-get-buddy id) 'onlinep 1))

;; Chat signals
(defun purple-received-im-msg-handler (account sender text conversation flags)
  (message "IM received: account=%d sender=%s" account sender))

;; Buffers
(defun purple-buddies ()
  (with-current-buffer "*purple-buddies*"
    ()))

(provide 'purple)
