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

(defun purple-mail-to ()
  (interactive)
  (let ((buddy (purple-buddy-select-buddy)))
    (insert (format "\"%s\" <%s>"
		    (slot-value buddy 'alias)
		    (substring (slot-value buddy 'name) 4)))))

(provide 'purple-mail)
