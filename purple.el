(require 'cl)
(require 'html2text)
(require 'eieio)
(require 'dbus)

(defvar purple-accounts '())
(defvar purple-buddies '())
(defvar-local purple-buddy nil)

(defclass buddy ()
  ((id :type number :initarg id)
   (accound-id :type number :initarg account-id)
   (name :initarg name)
   (alias :initarg alias)
   (onlinep :initarg onlinep)
   (typingp :initarg typingp :initform nil)
   (icon :initarg icon)))

(defvar purple-dbus-service "im.pidgin.purple.PurpleService")
(defvar purple-object "/im/pidgin/purple/PurpleObject")
(defvar purple-interface "im.pidgin.purple.PurpleInterface")

(defvar purple-buddy-history '())
(defvar purple-time-fmt "")
(defvar purple-msg-fmt  "")
(defvar purple-buddy-buffer-name-fmt "%s %s %s")

(defvar buddy-props '((name	.	"PurpleBuddyGetName")
		      (alias	.	"PurpleBuddyGetAlias")
		      (onlinep	.	"PurpleBuddyIsOnline")
		      (icon	.	"PurpleBuddyGetIcon")))

(defvar purple-signals '(("ReceivedImMsg"	.	purple-received-im-msg-handler)
			 ("BuddyStatusChanged"	.	purple-buddy-status-handler)
			 ("BuddySignedOff"	.	purple-buddy-signed-off-handler)
			 ("BuddySignedOn"	.	purple-buddy-signed-on-handler)
			 ("BuddyTyping"		.	purple-buddy-typing-handler)
			 ("BuddyTypingStopped"	.	purple-buddy-typing-stopped-handler)))

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

(defun purple-get-buddy (field value)
  (find id purple-buddies
	:test (lambda (x y) (equal value (slot-value y field)))))

(defun purple-get-buddy-info-async (account-id id)
  (let ((buddy (or (purple-get-buddy 'id id)
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

(defun purple-buddy-update-buffer-name (buddy)
  (let ((buffer (purple-get-buddy-buffer buddy)))
    (when buffer
      (with-current-buffer buffer
	(rename-buffer (purple-get-buddy-buffer-name purple-buddy))))))

(defun purple-buddy-signed-off-handler (id)
  (let ((buddy (purple-get-buddy 'id id)))
    (purple-buddy-info-handler buddy 'onlinep 0)
    (purple-buddy-update-buffer-name buddy)))

(defun purple-buddy-signed-on-handler (id)
  (let ((buddy (purple-get-buddy 'id id)))
    (purple-buddy-info-handler buddy 'onlinep 1)
    (purple-buddy-update-buffer-name buddy)))

(defun purple-buddy-typing-handler (account-id name)
  (let ((buddy (purple-get-buddy 'name name)))
    (purple-buddy-info-handler buddy 'typingp t)
    (purple-buddy-update-buffer-name buddy)))

(defun purple-buddy-typing-stopped-handler (id name)
  (let ((buddy (purple-get-buddy 'name name)))
    (purple-buddy-info-handler buddy 'typingp nil)
    (purple-buddy-update-buffer-name buddy)))

;; Chat signals
(defun purple-received-im-msg-handler (account sender text conversation flags)
  (message "IM received: account=%d sender=%s text=%s conversation=%s"
	   account sender
	   (with-temp-buffer
	     (insert text)
	     (html2text)
	     (buffer-string))
	   (with-temp-buffer
	     (insert text)
	     (html2text)
	     (buffer-string))))

;; Buffers
(define-derived-mode purple-buddies-mode tabulated-list-mode "Buddies"
  (setq tabulated-list-format [("Alias" 35 t)
			       ("Name" 40 t)
			       ("Status" 10 t)])
  (setq tabulated-list-sort-key (cons "Alias" nil))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-buddies-list nil t)
  (local-set-key (kbd "RET") 'purple-chat-with)
  (toggle-read-only t))

(defun purple-buddies-list ()
  (interactive)
  (with-current-buffer (get-buffer-create "*purple-buddies*")
    (let ((inhibit-read-only t))
      (setq tabulated-list-entries nil)
      (dolist (buddy purple-buddies)
	(push (list buddy (vector (oref buddy alias) (oref buddy name)
				  (if (= 1 (oref buddy onlinep))
				      (propertize "Online" 'face 'success)
				    (propertize "Offline" 'face 'error))))
	      tabulated-list-entries))
      (purple-buddies-mode)
      (pop-to-buffer-same-window (current-buffer)))))

(defsubst purple-propertize-buddy-alias (buddy)
  (propertize (slot-value buddy 'alias)
	      'face
	      (if (slot-value buddy 'onlinep)
		  'success
		'error)))

(defun purple-fancy-buddies ()
  (sort (mapcar 'purple-propertize-buddy-alias purple-buddies)
	(lambda (x y)
	  (eq 'success (get-text-property 0 'face x)))))

(defun purple-smart-buddy-selector ()
  (if (eq major-mode 'purple-buddies-mode)
      (let ((buddy (tabulated-list-get-id)))
	(add-to-list 'purple-buddy-history (slot-value buddy 'alias))
	buddy)
    (purple-get-buddy 'alias
		      (ido-completing-read "Choose buddy: "
					   (purple-fancy-buddies)
					   nil t nil 'purple-buddy-history))))

(defun purple-chat-with (&optional buddy)
  (interactive)
  (let ((buddy (purple-smart-buddy-selector)))
    (pop-to-buffer-same-window (purple-create-buddy-buffer buddy))))

(defun purple-get-buddy-buffer-name (buddy)
  (format purple-buddy-buffer-name-fmt
	  (slot-value buddy 'alias)
	  (if (= 1 (slot-value buddy 'onlinep))
	      "(Online)"
	    "(Offline)")
	  (if (slot-value buddy 'typingp)
		 "is typing ..."
	    "")))

(defun purple-create-buddy-buffer (buddy)
  (with-current-buffer (get-buffer-create (purple-get-buddy-buffer-name buddy))
    (purple-chat-mode)
    (setq purple-buddy buddy)
    (current-buffer)))

(defun purple-get-buddy-buffer (buddy)
  (let ((buffers (delete-if-not (curry 'eq 'purple-chat-mode) (buffer-list)
				:key (curry 'buffer-local-value 'major-mode))))
    (find (slot-value buddy 'alias) buffers
	  :key (lambda (x) (slot-value (buffer-local-value 'purple-buddy x) 'alias))
	  :test 'string=)))

(define-derived-mode purple-chat-mode fundamental-mode
  "chat-mode")

(provide 'purple)
