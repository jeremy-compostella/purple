;;; purple-buddy.el --- Purple buddy management

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'purple-group)
(require 'eieio)
(require 'cl)

(defgroup purple-buddy nil
  "Purple buddy group."
  :group 'purple)

(defvar purple-buddies '())
(defvar purple-buddy-history '())

(defclass plp-buddy ()
  ((id :type number :initarg id)
   (account :initarg account)
   (name :initarg name :initform "")
   (alias :initarg alias :initform "")
   (signed-on :initarg signed-on :initform nil)
   (status :initarg status :initform "unknown")
   (typingp :initarg typingp :initform nil)
   (group :initarg group :initform "")
   (icon :initarg icon :initform nil)))

(defcustom purple-buddy-props
  '((name	.	"PurpleBuddyGetName")
    (alias	.	"PurpleBuddyGetAlias")
    (signed-on	.	"PurpleBuddyIsOnline")
    (icon-id	.	"PurpleBuddyGetIcon")
    (presence	.	"PurpleBuddyGetPresence")
    (group-id	.	"PurpleBuddyGetGroup"))
  "List of supported buddy properties method."
  :group 'purple-buddy)

(defcustom purple-buddy-indirect-props
  '((presence	   . (active-status 	. "PurplePresenceGetActiveStatus"))
    (active-status . (status	 	. "PurpleStatusGetId"))
    (group-id      . (group		. "PurpleGroupGetName"))
    (icon-id       . (icon 		. "PurpleBuddyIconGetData")))
  "List of indirected buddy properties method."
  :group 'purple-buddy)

(defcustom purple-buddy-signals
  '(("BuddyAdded"		.	purple-buddy-added-handler)
    ("BuddyRemoved"		.	purple-buddy-removed-handler)
    ("BuddyStatusChanged"	.	purple-buddy-status-handler)
    ("BuddySignedOn"		.	purple-buddy-signed-handler)
    ("BuddySignedOff"		.	purple-buddy-signed-off-handler)
    ("BuddyTyping"		.	purple-buddy-typing-handler)
    ("BuddyTypingStopped"	.	purple-buddy-typing-stopped-handler))
  "List of supported buddy signals."
  :group 'purple-buddy)

(defvar purple-buddy-changed-hook '()
  "Hook list runned when a buddy data has changed.")

(defcustom purple-buddies-buffer-name "*purple-buddies*"
  "Buddy list buffer name"
  :group 'purple-buddy)

(defcustom purple-buddy-faces '(success warning error)
  "3 element font list associated with availability, respectively
  available, not-available and offline."
  :group 'purple-buddy)

(defcustom purple-buddy-display-images t
  "Enable the display of buddies images in the buddies list
buffer."
  :group 'purple-buddy)

(defun purple-buddy-init-for-accounts (accounts)
  (setq purple-buddies '())
  (dolist (account accounts)
    (let* ((buddies (purple-call-method "PurpleFindBuddies" :int32 (oref account id) ""))
	   (i 0)
	   (progress (make-progress-reporter
		      (format "Loading buddies for %s" (oref account name))
		      0 (length buddies) i)))
      (dolist (id buddies)
	(purple-buddy-retreive-all-info account id)
	(progress-reporter-force-update progress (incf i)))
      (progress-reporter-done progress)))
  (purple-register-signals purple-buddy-signals)
  (add-hook 'purple-account-status-changed-hook
	    'purple-buddy-account-status-changed))

(defun purple-buddy-icon-from-data (data)
  (let ((file (make-temp-file "icon"))
        (coding-system-for-write 'raw-text))
    (with-temp-buffer
      (mapc (rcurry 'insert-byte 1) data)
      (call-process-region (point-min) (point-max)
			   "convert" nil nil nil "-" "-resize" "100x" file))
    (create-image file)))

(defun purple-buddy-set-field (buddy field data)
  (when buddy
    (let ((value (cond ((eq field 'signed-on) (not (= 0 data)))
		       ((eq field 'icon) (purple-buddy-icon-from-data data))
		       (data)))
	  (indirect (assoc-default field purple-buddy-indirect-props)))
      (if indirect
	  (when (and (numberp data) (not (= 0 data)))
	    (purple-buddy-retreive-info buddy (car indirect)
					(cdr indirect) :int32 data))
	(set-slot-value buddy field value)
	(run-hook-with-args 'purple-buddy-changed-hook buddy field value)))))

(defun purple-buddy-find (field value &optional test)
  (find value purple-buddies
	:key (rcurry 'slot-value field) :test (if test test 'equal)))

(defun purple-buddy-find-by-name (value)
  (purple-buddy-find 'name value (lambda (x y) (string-prefix-p y x))))

(defun purple-buddy-eq (b1 b2)
  (when (and (plp-buddy-p b1) (plp-buddy-p b2))
    (= (oref b1 id) (oref b2 id))))

(defun purple-buddy-retreive-info (buddy sym method &rest args)
  (apply 'purple-call-method-async method
	 (curry 'purple-buddy-set-field buddy sym) args))

(defun purple-buddy-retreive-all-info (account id)
  (let ((buddy (or (purple-buddy-find 'id id)
		   (plp-buddy 'id id 'account account))))
    (add-to-list 'purple-buddies buddy t 'purple-buddy-eq)
    (dolist (prop purple-buddy-props)
      (purple-buddy-retreive-info buddy (car prop) (cdr prop) :int32 id))))

(defun purple-buddy-display-images-p ()
  (and purple-buddy-display-images
       (delq nil (mapcar (rcurry 'slot-value 'icon) purple-buddies))))

;; Signals
(defun purple-buddy-added-handler (id)
  (purple-buddy-retreive-all-info
   (purple-call-method "purpleBuddyGetAccount" :int32 id)
   id))

(defun purple-buddy-removed-handler (id)
  (setq purple-buddies
	(delete-if (curry 'purple-buddy-eq (purple-buddy-find 'id id))
		   purple-buddies)))

(defun purple-buddy-status-handler (id old-status status)
  (purple-buddy-set-field (purple-buddy-find 'id id)
			  'active-status status))

(defun purple-buddy-signed-handler (id &optional off)
  (purple-buddy-added-handler id))

(defun purple-buddy-signed-off-handler (id)
  (purple-buddy-signed-handler id t))

(defun purple-buddy-typing-handler (account-id name &optional status)
  (purple-buddy-set-field (purple-buddy-find-by-name name)
			  'typingp (not status)))

(defun purple-buddy-typing-stopped-handler (account-id name)
  (purple-buddy-typing-handler account-id name t))

;; Hook
(defun purple-buddy-account-status-changed (account)
  (let ((buddies (remove-if-not (lambda (x)
				  (purple-account-eq account (oref x account)))
				purple-buddies)))
    (cond ((eq 'signed-off (oref account status))
	   (mapc (rcurry 'purple-buddy-set-field 'status "offline") buddies)))))

;; Interactive
(define-derived-mode purple-buddies-mode tabulated-list-mode "Buddies"
  (let ((icon-length (if (purple-buddy-display-images-p) 17 0)))
    (setq tabulated-list-format (vector `("" ,icon-length t)
					'("Alias" 30 t)
					'("Name" 40 t)
					'("Status" 10 t)
					'("Group" 20 t)
					'("Account" 20 t))
	  tabulated-list-sort-key (cons "Alias" nil)))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-buddies-list nil t)
  (local-set-key (kbd "RET") 'purple-chat-with)
  (local-set-key (kbd "l") 'recenter-top-bottom)
  (local-set-key (kbd "a") 'purple-buddy-add)
  (local-set-key (kbd "d") 'purple-buddy-delete)
  (local-set-key (kbd "G a") 'purple-group-add)
  (local-set-key (kbd "G d") 'purple-group-delete)
  (toggle-read-only t))

(defun purple-buddy-face (buddy)
  (let ((status (oref buddy status)))
    (cond ((string= status "available") (nth 0 purple-buddy-faces))
	  ((string= status "offline") (nth 2 purple-buddy-faces))
	  ((identity (nth 1 purple-buddy-faces))))))

(defun purple-buddies-populate (buffer)
  "Update purple-buddies buffer"
  (setq tabulated-list-entries nil)
  (let ((display-images (purple-buddy-display-images-p)))
    (dolist (buddy purple-buddies)
      (push (list buddy (vector (if (and display-images (oref buddy icon))
				    (propertize "x" 'display (oref buddy icon))
				  "")
				(oref buddy alias) (oref buddy name)
				(propertize (capitalize (oref buddy status))
					    'face (purple-buddy-face buddy))
				(oref buddy group)
				(oref (oref buddy account) protocol)))
	    tabulated-list-entries))
    (tabulated-list-print)))

(defun purple-buddies-list ()
  "Display a list of all buddies"
  (interactive)
  (with-current-buffer (get-buffer-create purple-buddies-buffer-name)
    (let ((inhibit-read-only t))
      (unless (eq major-mode 'purple-buddies-mode)
	(purple-buddies-mode))
      (purple-buddies-populate (current-buffer))
      (pop-to-buffer (current-buffer)))))

(defun purple-buddy-propertize (buddy)
  (propertize (slot-value buddy 'alias) 'face (purple-buddy-face buddy)))

(defun purple-buddy-fancy-list ()
  (sort (mapcar 'purple-buddy-propertize purple-buddies)
	(lambda (x y) (< (position (get-text-property 0 'face x) purple-buddy-faces)
			 (position (get-text-property 0 'face y) purple-buddy-faces)))))

(defun purple-buddy-completing-read (&optional prompt)
  "Read a string in the minibuffer with ido-style completion to
select a buddy. If you enter a new buddy name, it will
dynamically create the buddy.
PROMPT is a string to prompt with."
  (interactive)
  (let ((prompt (or prompt "Buddy: ")))
    (if (eq major-mode 'purple-buddies-mode)
	(let ((buddy (tabulated-list-get-id)))
	  (add-to-list 'purple-buddy-history (slot-value buddy 'alias))
	  buddy)
      (let* ((buddy-alias (ido-completing-read prompt (purple-buddy-fancy-list)
					       nil nil nil 'purple-buddy-history))
	     (buddy (purple-buddy-find 'alias buddy-alias)))
	(if buddy
	    buddy
	  (purple-buddy-create (purple-account-completing-read)
			       buddy-alias))))))

(defun purple-buddy-create (account name)
  (let ((id (purple-call-method "PurpleBuddyNew" :int32 (oref account id) name name)))
    (purple-buddy-retreive-all-info account id)
    (purple-buddy-find 'id id)))

(defun purple-buddy-add (account name alias group)
  (interactive (list (purple-account-completing-read)
		     (read-string "Name: ")
		     (read-string "Alias: ")
		     (purple-group-completing-read "Add into group: ")))
  (let* ((account-id (oref account id))
	 (id (purple-call-method "PurpleBuddyNew" :int32 account-id name alias)))
    (purple-call-method "PurpleAccountAddBuddy" :int32 account-id :int32 id)
    (purple-call-method "PurpleBlistAddBuddy" :int32 id :int32 0
			:int32 (oref group node) :int32 0)))

(defun def-purple-ldap-browser ()
  "Load ldap-browser plugins"
  (define-key purple-buddies-mode-map (kbd "A") 'ldap-browser-add-purple-buddy))
(eval-after-load "ldap-browser-purple" '(def-purple-ldap-browser))

(defun purple-buddy-do-remove (buddy)
  (let* ((account (oref buddy account))
	 (id (oref buddy id))
	 (group (purple-call-method "PurpleBuddyGetGroup" :int32 id)))
    (purple-call-method "PurpleAccountRemoveBuddy"
			:int32 (oref account id) :int32 (oref buddy id)
			:int32 group)
    (purple-call-method "PurpleBlistRemoveBuddy" :int32 (oref buddy id))))

(defun purple-buddy-delete (buddy)
  (interactive (list (purple-buddy-completing-read)))
  (if (eq major-mode 'purple-buddies-mode)
      (when (yes-or-no-p (format "Are you sure you want to remove %s buddy ?"
				 (oref buddy alias)))
	(purple-buddy-do-remove buddy)
	(purple-buddies-populate (current-buffer)))
    (purple-buddy-do-remove buddy)))

(provide 'purple-buddy)
