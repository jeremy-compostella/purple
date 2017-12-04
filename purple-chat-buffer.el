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
(require 'html2text)

(defgroup purple-chat-buffer nil
  "Purple chat buffer group."
  :group 'purple)

(defcustom purple-chat-buffer-use-ispell nil
  "When set, at buffer creation, flyspell-mode will be
enabled."
  :group 'purple-chat-buffer
  :type 'boolean)

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
(defvar-local purple-chat-msg-history-cur 0)
(defvar-local purple-chat-msg-current "")

(defun purple-chat-buffer-init ()
  (add-hook 'purple-buddy-changed-hook
	    'purple-chat-buffer-buddy-has-changed)
  (add-hook 'purple-chat-changed-hook
	    'purple-chat-buffer-chat-has-changed)
  (dolist (buf (purple-chat-buffers))
    (with-current-buffer buf
      (let* ((chat (and purple-chat (purple-chat-find 'id (oref purple-chat id))))
	     (buddy (and chat (oref chat buddy))))
	(when chat
	  (setq purple-chat chat))
	(when buddy
	  (purple-chat-buffer-header-line))))))

(defun purple-chat-buffer-chat-has-changed (chat &optional field value)
  (let ((buf (purple-chat-buffer-find chat)))
    (when buf
      (with-current-buffer buf
        (purple-chat-buffer-header-line)))))

(defun purple-chat-buffer-buddy-has-changed (buddy &optional field value)
  (let ((chat (purple-chat-find 'buddy buddy 'purple-buddy-eq)))
    (when chat
      (purple-chat-buffer-chat-has-changed chat))))

(defun purple-chat-buffer-kill ()
  (when (eq major-mode 'purple-chat-mode)
    (purple-chat-destroy purple-chat)))
  
(define-derived-mode purple-chat-mode fundamental-mode
  "chat-mode"
  (local-set-key (kbd "RET") 'purple-chat-buffer-send-msg)
  (local-set-key (kbd "M-p") 'purple-chat-buffer-prev-msg)
  (local-set-key (kbd "M-n") 'purple-chat-buffer-next-msg)
  (local-set-key (kbd "C-c q") 'quit-window)
  (local-set-key (kbd "C-c C-o") 'browse-url)
  (local-set-key (kbd "C-c C-k") 'kill-buffer) ;TODO: this does not work !!!
  (add-hook 'kill-buffer-hook 'purple-chat-buffer-kill))

(defun purple-chat-buffer-header-line ()
  (let ((buddy (oref purple-chat buddy)))
    (when buddy
      (setq header-line-format
            (list (if (oref buddy icon) (propertize "x" 'display (oref buddy icon)) "")
                  (if (oref buddy status)
		      (capitalize (propertize (oref buddy status)
					      'face (purple-buddy-face buddy)))
		    "")
                  (if (oref buddy typingp)
                      (concat " - " (oref buddy alias) " is typing")
                    ""))))))

(defun purple-chat-buffer-create (chat)
  (with-current-buffer
      (get-buffer-create
       (format purple-chat-buffer-name-fmt
	       (oref chat title)))
    (unless (eq major-mode 'purple-chat-mode)
      (purple-chat-mode)
      (when purple-chat-buffer-use-ispell
	(flyspell-mode))
      (setq default-directory (getenv "HOME")
	    purple-chat chat
	    purple-chat-sep-marker (point-marker))
      (insert (propertize purple-chat-separator
			  'read-only t 'front-sticky t 'rear-nonsticky t))
      (setq purple-chat-input-marker (point-marker))
      (current-buffer))))

(defun purple-chat-buffers ()		;TODO: Make a variable
  (delete-if-not (curry 'eq 'purple-chat-mode)
		 (buffer-list)
		 :key (curry 'buffer-local-value 'major-mode)))

(defun purple-chat-buffer-find (chat)
  (find chat (purple-chat-buffers)
	:key (curry 'buffer-local-value 'purple-chat)
	:test 'purple-chat-eq))

(defun purple-chat-buffer-find-or-create (chat)
  (or (purple-chat-buffer-find chat)
      (purple-chat-buffer-create chat)))

(defun purple-chat-wash-msg (msg)
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (with-temp-buffer
        (insert (replace-regexp-in-string "\n" "<br>" msg))
        (call-process-region (point-min) (point-max) "html2text" nil buf nil "-utf8")))
    (html2text)
    (replace-regexp-in-string "^,s?a?n?s?-?serif; color:.*;\">" ""
			      (replace-regexp-in-string " " " " (buffer-string)))))

(defun purple-chat-show-buffer (&optional chat)
  (interactive)
  (let ((chat (or chat
		  (when (eq major-mode 'purple-chats-mode)
		    (tabulated-list-get-id)))))
    (pop-to-buffer (purple-chat-buffer-find-or-create chat))
    (purple-chat-buffer-header-line)
    (set-slot-value purple-chat 'unread 0)))

(defun purple-chat-format-message (buddy-alias msg received)
  (concat (apply 'propertize
		 (format "[%s] %s> "
			 (format-time-string purple-chat-buffer-time-fmt)
			 buddy-alias)
		 'face (if received 'purple-chat-received-face 'purple-chat-wrote-face)
		 purple-chat-buffer-properties)
	  (apply 'propertize (if received
				 (purple-chat-wash-msg msg)
			       (concat msg "\n"))
		 purple-chat-buffer-properties)))

(defun purple-chat-buffer-insert (chat from msg received)
  (with-current-buffer (purple-chat-buffer-find-or-create chat)
    (save-excursion
      (let* ((inhibit-read-only t)
	     (from-buddy (purple-buddy-find-by-name from))
	     (buddy-alias (if received
			     (if from-buddy (oref from-buddy alias) from)
			   "Me")))
	(goto-char (marker-position purple-chat-sep-marker))
	(insert-before-markers (purple-chat-format-message buddy-alias msg received))
	(if (get-buffer-window (current-buffer))
	    (set-slot-value purple-chat 'unread 0)
	  (set-slot-value purple-chat 'unread (1+ (oref purple-chat unread))))))))

(defun purple-chat-buffer-replace-msg (msg)
  (delete-region (marker-position purple-chat-input-marker) (point-max))
  (goto-char (marker-position purple-chat-input-marker))
  (insert msg))

(defun purple-chat-buffer-extract-msg ()
  (delete-and-extract-region (marker-position purple-chat-input-marker)
			     (point-max)))

;; Interactive
(defun purple-chat-buffer-send-msg ()
  (interactive)
  (let ((msg (purple-chat-buffer-extract-msg)))
    (when (> (length msg) 0)
      (add-to-list 'purple-chat-msg-history msg)
      (setq purple-chat-msg-history-cur 0
	    purple-chat-msg-saved nil)
      (purple-chat-send-im purple-chat msg))))

(defun purple-chat-history-move (n)
  (when purple-chat-msg-history
    (let ((cur (+ purple-chat-msg-history-cur n)))
      (when (and (>= cur 0) (<= cur (length purple-chat-msg-history)))
	(when (zerop purple-chat-msg-history-cur)
	  (setq purple-chat-msg-current (purple-chat-buffer-extract-msg)))
	(purple-chat-buffer-replace-msg (if (zerop cur)
					    purple-chat-msg-current
					  (nth (1- cur) purple-chat-msg-history)))
	(incf purple-chat-msg-history-cur n)))))

(defun purple-chat-buffer-prev-msg ()
  (interactive)
  (purple-chat-history-move +1))

(defun purple-chat-buffer-next-msg ()
  (interactive)
  (purple-chat-history-move -1))

(provide 'purple-chat-buffer)
