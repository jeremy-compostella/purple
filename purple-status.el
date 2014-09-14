;;; purple-status.el --- Purple status for Emacs

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

(defgroup purple-status nil
  "Purple status group."
  :group 'purple)

(defvar purple-status '())

(defclass plp-status ()
  ((id :type number :initarg id)
   (title :initarg title)
   (typ :initarg typ)
   (msg :initarg msg)))

(defcustom purple-status-props
  '((title	.	"PurpleSavedstatusGetTitle")
    (typ	.	"PurpleSavedstatusGetType")
    (msg	.	"PurpleSavedstatusGetMessage"))
  "List of supported status properties method."
  :group 'purple-status)

(defcustom purple-status-order
  '((offline		.	error)
    (available		.	success)
    (unavailable	.	warning)
    (invisible		.	warning)
    (away		.	warning)
    (extended-away	.	warning)
    (mobile		.	success)
    (tune		.	success))
  "Ordered list of status"
  :group 'purple-status)

(defun purple-status-init ()
  (setq purple-status '())
  (mapc 'purple-status-retreive-info
	(purple-call-method "PurpleSavedstatusesGetAll")))

(defun purple-status-find (field value &optional status-list)
  (find value (or status-list purple-status)
	:key (rcurry 'slot-value field)))

(defun purple-status-eq (s1 s2)
  (when (and (plp-status-p s1) (plp-status-p s2))
    (= (oref s1 id) (oref s2 id))))

(defun purple-status-set-field (status field data)
  (let ((value (if (eq field 'typ)
		   (car (nth (1- data) purple-status-order))
		 data)))
    (set-slot-value status field value)))

(defun purple-status-retreive-info (id)
  (let ((status (or (purple-status-find 'id id)
		    (plp-status id 'id id))))
    (add-to-list 'purple-status status t 'purple-status-eq)
    (dolist (prop purple-status-props)
      (purple-call-method-async (cdr prop)
				(curry 'purple-status-set-field status (car prop))
				:int32 id))))

(defun purple-status-by-type (type)
  (remove-if-not (curry 'eq type) purple-status
		 :key (rcurry 'slot-value 'typ)))

(defun purple-status-types ()
  (delete-duplicates (mapcar (rcurry 'slot-value typ) purple-status)))

(defun purple-status-msg (&optional type)
  (let ((status (if type
		    (purple-status-by-type type)
		  purple-status)))
    (mapcar (rcurry 'slot-value msg) status)))

(defun purple-status-completing-read ()
  "Read a string in the minibuffer with ido-style completion to
select a status.
PROMPT is a string to prompt with."
  (interactive)
  (let* ((type (intern (ido-completing-read "Status type: "
					    (mapcar 'symbol-name (purple-status-types)))))
	 (msg (ido-completing-read "Status message: " (purple-status-msg type)))
	 (status (purple-status-find 'msg msg (purple-status-by-type type))))
    status))

(defun purple-status-set ()
  (interactive)
  (let ((status (purple-status-completing-read)))
    (purple-call-method "PurpleSavedstatusActivate" :int32 (oref status id))))

(defun purple-status-get ()
  (interactive)
  (let ((status (purple-status-find 'id (purple-call-method "PurpleSavedstatusGetCurrent"))))
    (message (propertize (concat (symbol-name (oref status typ))
				 " - "
				 (oref status title))
			 'face (assoc-default (oref status typ) purple-status-order)))))

(provide 'purple-status)
