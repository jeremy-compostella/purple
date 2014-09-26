;;; purple-chat.el --- Purple conversation management

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

(require 'purple-buddy)
(require 'cl)

(defvar purple-chats '())
(defvar purple-chat-history '())

(defgroup purple-chat nil
  "Purple chat group."
  :group 'purple)

(defclass plp-chat ()
  ((id :type number :initarg id)
   (title :initarg title :initform "")
   (im :initarg im)
   (chat-data :initarg chat-data)
   (type :initform)
   (buddy :initarg buddy :initform nil)
   (unread :initarg unread :initform 0)))

(defcustom purple-chat-props
  '((type	.	"PurpleConversationGetType")
    (title	.	"PurpleConversationGetTitle")
    (name	.	"PurpleConversationGetName")
    (im		.	"PurpleConvIm")
    (chat-data	.	"PurpleConversationGetChatData"))
  "List of supported chat properties method."
  :group 'purple-chat)

(defcustom purple-chat-signals
  '(("ConversationCreated"	.	purple-chat-created-handler)
    ("DeletingConversation"	.	purple-chat-deleting-handler)
    ("ReceivedImMsg"		.	purple-chat-received-im-msg-handler)
    ("SentImMsg"		.	purple-chat-sent-im-msg-handler))
  "List of support chat signals."
  :group 'purple-chat)

(defvar purple-chat-changed-hook '()
  "Hook list runned when a chat data has changed.")

;; Init
(defun purple-chat-init ()
  (setq purple-chats '())
  (mapc 'purple-chat-retreive-all-info
	(purple-call-method "PurpleGetConversations"))
  (purple-register-signals purple-chat-signals))

(defun purple-chat-set-field (chat field value)
  (if (eq field 'name)
      (progn (set-slot-value chat 'buddy (or (purple-buddy-find 'name value)
                                             (plp-buddy 0 'id 0 'name value)))
             (run-hook-with-args 'purple-chat-changed-hook chat 'buddy value))
    (set-slot-value chat field value)
    (run-hook-with-args 'purple-chat-changed-hook chat field value))
  (if (and (eq field 'title) purple-chats-open-next-created)
      (progn (setq purple-chats-open-next-created nil)
	     (purple-chat-show-buffer chat))))

(defun purple-chat-eq (c1 c2)
  (when (and (plp-chat-p c1) (plp-chat-p c2))
    (= (oref c1 id) (oref c2 id))))

(defun purple-chat-find (field value &optional equal-fun)
  (let ((equal-fun (or equal-fun 'equal)))
    (find value purple-chats
	  :key (rcurry 'slot-value field) :test equal-fun)))

(defun purple-chat-retreive-all-info (id)
  (let ((chat (or (purple-chat-find 'id 0)
		  (purple-chat-find 'id id)
		  (plp-chat id 'id id))))
    (set-slot-value chat 'id id)
    (add-to-list 'purple-chats chat t 'purple-chat-eq)
    (dolist (prop purple-chat-props)
      (purple-call-method-async (cdr prop)
				(curry 'purple-chat-set-field chat (car prop))
				:int32 id))
    chat))

(defvar purple-chats-open-next-created nil)

(defun purple-chat-new (buddy)
  (setq purple-chats-open-next-created t)
  (purple-call-method "PurpleConversationNew" :int32 1
		      :int32 (oref (oref buddy account) id)
		      (oref buddy name)))

(defun purple-chat-destroy (&optional chat)
  (interactive)
  (let ((chat (if (eq major-mode 'purple-chats-mode)
		  (tabulated-list-get-id)
		chat)))
    (ignore-errors
      (purple-call-method "PurpleConversationDestroy" :int32 (oref chat id)))))

(defun purple-chat-send-im (chat msg)
  (purple-call-method "PurpleConvImSend"
		      :int32 (slot-value chat 'im) msg))

;; Signals
(defun purple-chat-created-handler (id)
  (purple-chat-retreive-all-info id))

(defun purple-chat-deleting-handler (id)
  (setq purple-chats
	(delete-if (curry 'purple-chat-eq (purple-chat-find 'id id))
		   purple-chats)))

(defun purple-chat-received-im-msg-handler (account sender msg id flags)
  (let* ((buddy (purple-buddy-find-by-name sender))
	 (chat (purple-chat-find 'id id)))
    (when (not chat)			; Chat will be created in a minute
      (setq chat (plp-chat id 'id 0))
      (add-to-list 'purple-chats chat t 'purple-chat-eq))
    (when (not buddy)
      (setq buddy (purple-buddy-create (purple-account-find 'id account) sender)))
    (when (zerop (oref chat id))
      (set-slot-value chat 'title (oref buddy alias)))
    (set-slot-value chat 'buddy buddy)
    (purple-chat-buffer-insert chat sender msg t)))

(defun purple-chat-sent-im-msg-handler (account receiver msg)
  (let* ((buddy (purple-buddy-find-by-name receiver))
	 (chat (purple-chat-find 'buddy buddy 'purple-buddy-eq)))
    (purple-chat-buffer-insert chat receiver msg nil)))

;; Interactive
(define-derived-mode purple-chats-mode tabulated-list-mode "purple-chats"
  (setq tabulated-list-format [("Title" 30 t)
			       ("Buddy Status" 15 t)
			       ("Unread" 7 t)])
  (setq tabulated-list-sort-key (cons "Unread" t))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-chats-list nil t)
  (local-set-key (kbd "k") 'purple-chat-destroy)
  (local-set-key (kbd "RET") 'purple-chat-show-buffer)
  (toggle-read-only t))

(defun purple-chats-list ()
  (interactive)
  (with-current-buffer (get-buffer-create purple-chats-buffer-name)
    (let ((inhibit-read-only t))
      (purple-chats-mode)
      (setq tabulated-list-entries nil)
      (dolist (chat purple-chats)
	(let ((buddy (oref chat buddy)))
	  (push (list chat (vector (oref chat title)
				  (if (not (= 0 (oref buddy id)))
				      (propertize (capitalize (oref buddy status))
						  'face (purple-buddy-face buddy))
				    "Unknown")
				  (propertize (number-to-string (oref chat unread))
					      'face (if (= 0 (oref chat unread))
							'error
						      'success))))
		tabulated-list-entries)))
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun purple-chat-propertize (chat)
  (let ((buddy (oref chat buddy))
	(str (oref chat title)))
    (if buddy
	(propertize str 'face (purple-buddy-face buddy))
      str)))

(defun purple-chat-fancy-list ()
  (let ((chats (sort (copy-list purple-chats)
		     (lambda (x y) (> (oref x unread) (oref y unread))))))
    (mapcar 'purple-chat-propertize chats)))

(defun purple-chat-completing-read (&optional prompt)
  "Read a string in the minibuffer with ido-style completion to
select a chat.
PROMPT is a string to prompt with."
  (interactive)
  (let ((prompt (or prompt "Chat: ")))
    (if (eq major-mode 'purple-chats-mode)
	(let ((chat (tabulated-list-get-id)))
	  (add-to-list 'purple-chat-history (slot-value chat 'title))
	  chat)
      (purple-chat-find 'title
			(ido-completing-read prompt (purple-chat-fancy-list)
			 nil t nil 'purple-chat-history)))))

(defun purple-chat-jump (&optional title)
  (interactive)
  (purple-chat-show-buffer (purple-chat-completing-read "Jump to chat: ")))

(defun purple-chat-with (buddy)
  (interactive (list (purple-buddy-completing-read "Chat with: ")))
  (let ((chat (purple-chat-find 'buddy buddy 'purple-buddy-eq)))
    (if chat
	(purple-chat-show-buffer chat)
      (purple-chat-new buddy))))

(provide 'purple-chat)
