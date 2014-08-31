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
(require 'html2text)
(require 'cl)

(defvar purple-chats '())
(defvar purple-chat-history '())

(defgroup purple-chat nil
  "Activity management group"
  :group 'purple)

(defclass purple-chat ()
  ((id :type number :initarg id)
   (title :initarg topic :initform "")
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
  '(("ReceivedImMsg"	.	purple-chat-received-im-msg-handler)
    ("WroteChatMsg"	.	purple-chat-wrote-chat-msg-handler))
  "List of support chat signals."
  :group 'purple-chat)

;; Init
(defun purple-chat-init ()
  (setq purple-chats '())
  (mapc 'purple-chat-retreive-all-info
	(purple-call-method "PurpleGetConversations"))
  (purple-register-signals purple-chat-signals))

(defun purple-chat-set-field (chat field value)
  (if (eq field 'name)
      (set-slot-value chat 'buddy (or (purple-buddy-find 'name value)
				      (purple-buddy 0 'id 0 'name value)))
    (set-slot-value chat field value)))

(defun purple-chat-eq (c1 c2)
  (= (oref c1 id) (oref c2 id)))

(defun purple-chat-find (field value)
  (find value purple-chats
	:key (rcurry 'slot-value field) :test 'equal))

(defun purple-chat-retreive-all-info (id)
  (let ((chat (or (purple-chat-find 'id id)
		  (purple-chat id 'id id))))
    (add-to-list 'purple-chats chat t 'purple-chat-eq)
    (dolist (prop purple-chat-props)
      (purple-call-method-async (cdr prop)
				(curry 'purple-chat-set-field chat (car prop))
				:int32 id))))

;; Interactive
(define-derived-mode purple-chats-mode tabulated-list-mode "purple-chats"
  (setq tabulated-list-format [("Title" 30 t)
			       ("Buddy Status" 15 t)
			       ("Unread" 7 t)])
  (setq tabulated-list-sort-key (cons "Unread" t))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-chats-list nil t)
  (local-set-key (kbd "k") (lambda () (interactive) (kill-buffer (current-buffer))))
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
      (pop-to-buffer-same-window (current-buffer)))))

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

(provide 'purple-chat)
