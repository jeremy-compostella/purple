;;; purple-chat-buffer.el --- Purple conversation buffer management

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

(require 'purple-chat)
(require 'purple-buddy)

(defgroup purple-chat-buffer nil
  "Activity management group"
  :group 'purple)

(defcustom purple-chat-buffer-name-fmt "%s"
  "Purple chat buffer name format."
  :group 'purple-chat-buffer)

(defcustom purple-chat-buffer-time-fmt "%H:%M"
  "Purple chat time prefix format."
  :group 'purple-chat-buffer)

(defcustom purple-chat-separator "---\n"
  "String separating the chat history and the entering message
  field."
  :group 'purple-chat-buffer)

(defcustom purple-chats-buffer-name "*purple-chats*"
  "Chats list buffer name."
  :group 'purple-chat-buffer)

(defface purple-chat-wrote-face
  '((t (:foreground "salmon" :weight bold)))
  "face for own message."
  :group 'purple-chat-buffer)

(defface purple-chat-received-face
  '((t (:foreground "SteelBlue1" :weight bold)))
  "face for foriegn message."
  :group 'purple-chat-buffer)

(defvar purple-chat-buffer-properties
  '(read-only t 'front-sticky t 'rear-nonsticky t))

(defvar-local purple-chat nil)
(defvar-local purple-chat-sep-marker nil)
(defvar-local purple-chat-input-marker nil)

(defvar-local purple-chat-msg-history '())

(define-derived-mode purple-chat-mode fundamental-mode
  "chat-mode"
  (local-set-key (kbd "RET") 'purple-chat-buffer-send-msg))

(defun purple-chat-buffer-create (chat)
  (with-current-buffer
      (get-buffer-create
       (format purple-chat-buffer-name-fmt
	       (oref chat title)))
    (purple-chat-mode)
    (setq purple-chat chat
	  purple-chat-sep-marker (point-marker))
    (insert (propertize purple-chat-separator
			'read-only t 'front-sticky t 'rear-nonsticky t))
    (setq purple-chat-input-marker (point-marker))
    (current-buffer)))

(defun purple-chat-buffers ()		;TODO: Make a variable
  (delete-if-not (curry 'eq 'purple-chat-mode)
		 (buffer-list)
		 :key (curry 'buffer-local-value 'major-mode)))

(defun purple-chat-buffer-find (chat)
  (find chat (purple-chat-buffers)
	:key (curry 'buffer-local-value 'purple-chat)
	:test 'purple-chat-eq))

(defun purple-chat-wash-msg (msg)
  (with-temp-buffer
    (insert msg)
    (html2text)
    (buffer-string)))

(defun purple-chat-format-message (buddy-alias msg received)
    (apply 'propertize
	   (format "[%s] %s> %s\n"
		   (format-time-string purple-chat-buffer-time-fmt)
		   buddy-alias
		   (purple-chat-wash-msg msg))
	   'face (if received
		     'purple-chat-received-face
		   'purple-chat-wrote-face)
	   purple-chat-buffer-properties))

(defun purple-chat-buffer-insert (chat from msg received)
  (with-current-buffer (or (purple-chat-buffer-find chat)
			   (purple-chat-buffer-create chat))
    (let ((inhibit-read-only t)
	  (buddy-alias (if received
			   from
			 "Me")))
      (goto-char (marker-position purple-chat-sep-marker))
      (insert-before-markers (purple-chat-format-message msg received)))))

;; Interactive
(defun purple-chat-buffer-send-msg ()
  (let ((msg (delete-and-extract-region
	      (marker-position purple-chat-input-marker)
	      (point-max))))
    (when (> 0 (length msg))
      (purple-chat-send-im purple-chat msg))))

(provide 'purple-chat-buffer)
