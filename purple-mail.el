;;; purple-mail.el --- Purple mail completion

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

(defun purple-mail-from-buddy (prompt)
  (let ((buddy (purple-buddy-completing-read prompt)))
    (format "\"%s\" <%s>"
	    (slot-value buddy 'alias)
	    (substring (slot-value buddy 'name) 4))))

(defun purple-mail-insert-buddy (prompt &optional separator)
  (insert (or separator "")
	  (purple-mail-from-buddy prompt)))

(defun purple-mail-to ()
  (interactive)
  (if (not (eq major-mode 'message-mode))
      (message-mail (purple-mail-from-buddy "Write email to: "))
    (save-excursion
      (message-goto-to)
      (if (looking-back "To: ")
	  (purple-mail-insert-buddy "Write email to: ")
	(message-goto-cc)
	(let ((separator (if (looking-back "Cc: ") "" ", ")))
	  (purple-mail-insert-buddy "CC email to: " separator))))))

(provide 'purple-mail)
