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
  "Activity management group"
  :group 'purple)

(defvar purple-buddies '())
(defvar purple-buddy-history '())

(defclass ple-buddy ()
  ((id :type number :initarg id)
   (name :initarg name :initform "")
   (alias :initarg alias :initform "")
   (signed-on :initarg signed-on :initform nil)
   (status :initarg status :initform nil)
   (typingp :initarg typingp :initform nil)
   (group :initarg group :initform "")
   (icon :initarg icon :initform nil)))

(defcustom purple-buddy-props
  '((name	.	"PurpleBuddyGetName")
    (alias	.	"PurpleBuddyGetAlias")
    (signed-on	.	"PurpleBuddyIsOnline")
    (icon	.	"PurpleBuddyGetIcon")
    (presence	.	"PurpleBuddyGetPresence")
    (group-id	.	"PurpleBuddyGetGroup"))
  "List of supported buddy properties method."
  :group 'purple-buddy)

(defcustom purple-buddy-indirect-props
  '((presence		.	(active-status 	. "PurplePresenceGetActiveStatus"))
    (active-status	.	(status 	. "PurpleStatusGetId"))
    (group-id           .       (group		. "PurpleGroupGetName")))
  "List of indirected buddy properties method."
  :group 'purple-buddy)

(defcustom purple-buddy-signals
  '(("BuddyAdded"		.	purple-buddy-added-handler)
    ("BuddyRemoved"		.	purple-buddy-removed-handler)
    ("BuddyStatusChanged"	.	purple-buddy-status-handler)
    ("BuddySignedOn"		.	purple-buddy-signed-handler)
    ("BuddySignedOff"		.	(rcurry 'purple-buddy-signed-handler t))
    ("BuddyTyping"		.	purple-buddy-typing-handler)
    ("BuddyTypingStopped"	.	(rcurry 'purple-buddy-typing-handler t)))
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

(defun purple-buddy-init-for-account (id)
  (setq purple-buddies '())
  (mapc 'purple-buddy-retreive-all-info
	(purple-call-method "PurpleFindBuddies" :int32 id ""))
  (purple-register-signals purple-buddy-signals))

(defun purple-buddy-set-field (buddy field data)
  (let ((value (if (eq field 'signed-on) (not (= 0 data)) data))
	(indirect (assoc-default field purple-buddy-indirect-props)))
    (if indirect
	(purple-buddy-retreive-info buddy (car indirect)
				    (cdr indirect) :int32 data)
      (set-slot-value buddy field value)
      (run-hook-with-args 'purple-buddy-changed-hook buddy field value))))

(defun purple-buddy-find (field value)
  (find value purple-buddies
	:key (rcurry 'slot-value field) :test 'equal))

(defun purple-buddy-eq (b1 b2)
  (when (and (ple-buddy-p b1) (ple-buddy-p b2))
    (= (oref b1 id) (oref b2 id))))

(defun purple-buddy-retreive-info (buddy sym method &rest args)
  (apply 'purple-call-method-async method
	 (curry 'purple-buddy-set-field buddy sym) args))

(defun purple-buddy-retreive-all-info (id)
  (let ((buddy (or (purple-buddy-find 'id id)
		   (ple-buddy id 'id id))))
    (add-to-list 'purple-buddies buddy t 'purple-buddy-eq)
    (dolist (prop purple-buddy-props)
      (purple-buddy-retreive-info buddy (car prop) (cdr prop) :int32 id))))

;; Signals
(defun purple-buddy-added-handler (id)
  (purple-buddy-retreive-all-info id))

(defun purple-buddy-removed-handler (id)
  (setq purple-buddies
	(delete-if (curry 'purple-buddy-eq (purple-buddy-find 'id id))
		   purple-buddies)))

(defun purple-buddy-status-handler (id old-status status)
  (purple-buddy-set-field (purple-buddy-find 'id id)
			  'active-status status))

(defun purple-buddy-signed-handler (id &optional off)
  (purple-buddy-set-field (purple-buddy-find 'id id)
			  'signed-on (if off 0 1)))

(defun purple-buddy-typing-handler (account-id name &optional status)
  (purple-buddy-set-field (purple-buddy-find 'name name)
			  'typingp (not status)))

;; Interactive
(define-derived-mode purple-buddies-mode tabulated-list-mode "Buddies"
  (setq tabulated-list-format [("Alias" 30 t)
			       ("Name" 40 t)
			       ("Status" 10 t)
			       ("Group" 20 t)])
  (setq tabulated-list-sort-key (cons "Alias" nil))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-buddies-list nil t)
  (local-set-key (kbd "RET") 'purple-chat-with)
  (local-set-key (kbd "l") 'recenter-top-bottom)
  (local-set-key (kbd "a") 'purple-buddy-add)
  (local-set-key (kbd "r") 'purple-buddy-remove)
  (local-set-key (kbd "G a") 'purple-group-add)
  (local-set-key (kbd "G r") 'purple-group-remove)
  (toggle-read-only t))

(defun purple-buddy-face (buddy)
  (let ((status (oref buddy status)))
    (cond ((string= status "available") (nth 0 purple-buddy-faces))
	  ((string= status "offline") (nth 2 purple-buddy-faces))
	  ((identity (nth 1 purple-buddy-faces))))))

(defun purple-buddies-list ()
  "Display a list of all buddies"
  (interactive)
  (with-current-buffer (get-buffer-create purple-buddies-buffer-name)
    (let ((inhibit-read-only t))
      (unless (eq major-mode 'purple-buddies-mode)
	(purple-buddies-mode))
      (setq tabulated-list-entries nil)
      (dolist (buddy purple-buddies)
	(push (list buddy (vector (oref buddy alias) (oref buddy name)
				  (propertize (capitalize (oref buddy status))
					      'face (purple-buddy-face buddy))
				  (oref buddy group)))
	      tabulated-list-entries))
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun purple-buddy-propertize (buddy)
  (propertize (slot-value buddy 'alias) 'face (purple-buddy-face buddy)))

(defun purple-buddy-fancy-list ()
  (sort (mapcar 'purple-buddy-propertize purple-buddies)
	(lambda (x y) (< (position (get-text-property 0 'face x) purple-buddy-faces)
			 (position (get-text-property 0 'face y) purple-buddy-faces)))))

(defun purple-buddy-completing-read (&optional prompt)
  "Read a string in the minibuffer with ido-style completion to
select a buddy.
PROMPT is a string to prompt with."
  (interactive)
  (let ((prompt (or prompt "Buddy: ")))
    (if (eq major-mode 'purple-buddies-mode)
	(let ((buddy (tabulated-list-get-id)))
	  (add-to-list 'purple-buddy-history (slot-value buddy 'alias))
	  buddy)
      (purple-buddy-find 'alias
			(ido-completing-read prompt (purple-buddy-fancy-list)
			 nil t nil 'purple-buddy-history)))))

(defun purple-buddy-add (name alias group)
  (interactive (list (read-string "Name: ")
		     (read-string "Alias: ")
		     (purple-group-completing-read "Add into group: ")))
  (let* ((account (car purple-accounts))
	 (id (purple-call-method "PurpleBuddyNew" :int32 account name alias)))
    (purple-call-method "PurpleAccountAddBuddy" :int32 account :int32 id)
    (purple-call-method "PurpleBlistAddBuddy" :int32 id :int32 0
			:int32 (oref group node) :int32 0)))

(defun purple-buddy-do-remove (buddy)
  (let* ((account (car purple-accounts)) ;TODO: include account into
					 ;buddy definition
	 (id (oref buddy id))
	 (group (purple-call-method "PurpleBuddyGetGroup" :int32 id)))
    (purple-call-method "PurpleAccountRemoveBuddy"
			:int32 account :int32 (oref buddy id)
			:int32 group)
    (purple-call-method "PurpleBlistRemoveBuddy" :int32 (oref buddy id))))

(defun purple-buddy-remove (buddy)
  (interactive (list (purple-buddy-completing-read)))
  (if (eq major-mode 'purple-buddies-mode)
      (when (yes-or-no-p (format "Are you sure you want to remove %s buddy ?"
				 (oref buddy alias)))
	(purple-buddy-do-remove buddy))
    (purple-buddy-do-remove buddy)))

(provide 'purple-buddy)
