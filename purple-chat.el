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

(require 'purple)
(require 'html2text)
(require 'cl)

(defvar purple-buddy-buffer-name-fmt "%s %s %s")

(defvar purple-chat-signals '(("ReceivedImMsg"	.	purple-received-im-msg-handler)
			      ("WroteChatMsg"	.	purple-wrote-chat-msg-handler)))

(defvar-local purple-chat-id nil)
(defvar-local purple-chat-buddies '())
(defvar-local purple-chat-unread 0)
(defvar-local purple-chat-title "")

(defconst purple-separator "-")

(defvar-local purple-separator-marker nil)
(defvar-local purple-input-area-marker nil)

(defface purple-chat-my-message-face
  '((t (:foreground "salmon" :weight bold)))
  "face for own message")

(defface purple-chat-foreign-message-face
  '((t (:foreground "SteelBlue1" :weight bold)))
  "face for foriegn message")

;; Chat buffers management
(defun purple-get-buddy-buffer-name (buddy)
  (format purple-buddy-buffer-name-fmt
	  (slot-value buddy 'alias)
	  (if (= 1 (slot-value buddy 'onlinep))
	      "(Online)"
	    "(Offline)")))

(defun purple-create-buddy-buffer (buddy conversation)
  (with-current-buffer (get-buffer-create (purple-get-buddy-buffer-name buddy))
    (purple-chat-mode)
    (setq purple-chat-buddies (list buddy)
	  purple-chat-unread 0
	  purple-chat-title ""
	  purple-conversation conversation)
    (current-buffer)))

(defsubst purple-chat-buffers ()
  (delete-if-not (curry 'eq 'purple-chat-mode) (buffer-list)
		 :key (curry 'buffer-local-value 'major-mode)))

(defun purple-chat-insert-received-msg (buffer buddy text)
  (with-current-buffer buffer
    (insert (with-temp-buffer (insert text) (html2text) (buffer-string)))
    (insert "\n")))

(define-derived-mode purple-chats-mode tabulated-list-mode "Conversations"
  (setq tabulated-list-format [("Buddies" 60 t)
			       ("Unread" 7 t)
			       ("Title" 35 t)])
  (setq tabulated-list-sort-key (cons "Buddies" nil))
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'purple-chats-list nil t)
  (local-set-key (kbd "RET")
		 (lambda ()
		   (interactive)
		   (pop-to-buffer-same-window (tabulated-list-get-id))))
  (toggle-read-only t))

(defun purple-chats-list ()
  (interactive)
  (with-current-buffer (get-buffer-create "*purple-conversations*")
    (let ((inhibit-read-only t))
      (purple-chats-mode)
      (setq tabulated-list-entries nil)
      (dolist (buf (purple-chat-buffers))
	(let ((unread (buffer-local-value 'purple-chat-unread buf)))
	  (push (list buf (vector (buffer-local-value 'purple-chat-title buf)
				  (propertize (number-to-string unread) 'face (if (= 0 unread) 'error 'success))
				  (mapconcat (rcurry 'slot-value 'alias)
					     (buffer-local-value 'purple-chat-buddies buf)
					     ", ")))
		tabulated-list-entries)))
      (tabulated-list-print)
      (pop-to-buffer-same-window (current-buffer)))))

;; Chat signals
(defun purple-received-im-msg-handler (account sender text conversation flags)
  (let ((buddy (or (purple-get-buddy 'name sender)
		   (buddy nil 'account-id account 'name sender 'alias sender))))
    (purple-chat-insert-received-msg buffer buddy text)))

(defun purple-wrote-chat-msg-handler (account sender msg conversation flags))

;; Conversation
(defun purple-new-conversation (buddy)
  (purple-call-method "PurpleConversationNew" :int32 1
		      :int32 (oref buddy account-id) (oref buddy name)))

(defun purple-chat-with (&optional buddy)
  (interactive)
  (let* ((buddy (purple-smart-buddy-selector))
	 (conversation (or (let ((buf (purple-get-buddy-buffer buddy)))
			     (when buf
			       (buffer-local-value 'purple-conversation buf)))
			   (purple-new-conversation buddy))))
    (pop-to-buffer-same-window (purple-create-buddy-buffer buddy conversation))))

(defun purple-get-conversation-buffer (conversation)
  (find (slot-value buddy 'alias) (purple-chat-buffers)
	:key (curry 'buffer-local-value 'purple-conversation)
	:test 'equal))

(defun purple-get-buddy-buffer (buddy)
  (find (slot-value buddy 'alias) (purple-chat-buffers)
	:key (lambda (x) (slot-value (buffer-local-value 'purple-buddy x) 'alias))
	:test 'string=))

(define-derived-mode purple-chat-mode fundamental-mode
  "chat-mode"
  (define-key map "\r" 'purple-chat-send-msg))

(provide 'purple-chat)
