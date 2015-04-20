;;; purple-group.el --- Purple group management

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

(require 'eieio)

(defgroup purple-group nil
  "Purple group group."
  :group 'purple)

(defclass purple-group ()
  ((node :initarg node :initform nil)
   (name :initarg name :initform "")))

(defvar purple-groups '())
(defvar purple-group-history '())

(defcustom purple-group-signals
  '(("BlistNodeAdded"	.	purple-group-added-handler)
    ("BlistNodeRemoved" .	purple-group-removed-handler))
  "List of supported group signals."
  :group 'purple-group)

(defun purple-group-init ()
  (setq purple-groups '())
  (let ((node (purple-call-method "PurpleBlistGetRoot")))
    (purple-group-retreive-info-rec node))
  (purple-register-signals purple-group-signals))

(defun purple-group-find (field value)
  (find value purple-groups
	:key (rcurry 'slot-value field) :test 'equal))

(defun purple-group-eq (g1 g2)
  (= (oref g1 node) (oref g2 node)))

(defun purple-group-retreive-info (node type)
  (when (= 0 type)
    (let ((group (or (purple-group-find 'node node)
		     (purple-group 'node node))))
      (add-to-list 'purple-groups group t 'purple-group-eq)
      (purple-call-method-async "PurpleGroupGetName"
				(curry 'set-slot-value group 'name)
				 :int32 node))))

(defun purple-group-node-is-group (node)
  (purple-call-method-async "PurpleBlistNodeGetType"
			    (curry 'purple-group-retreive-info node)
			    :int32 node))

(defun purple-group-retreive-info-rec (node)
  (unless (= 0 node)
    (purple-group-node-is-group node)
    (purple-call-method-async "PurpleBlistNodeNext"
			      'purple-group-retreive-info-rec
			      :int32 node :int32 0)))
;; Signals
(defun purple-group-added-handler (node)
  (purple-group-node-is-group node))

(defun purple-group-removed-handler (node)
  (let ((group (purple-group-find 'node node)))
    (when group
      (setq purple-groups
	    (delete-if (curry 'purple-group-eq group) purple-groups)))))

;; Interactive
(defun purple-group-add (name)
  (interactive "sGroup name: ")
  (let ((id (purple-call-method "PurpleGroupNew" name))
	(node (purple-call-method "PurpleBlistGetRoot")))
    (purple-call-method "PurpleBlistAddGroup" :int32 id :int32 0)))

(defun purple-group-delete (group)
  (interactive (list (purple-group-completing-read)))
  (purple-call-method "PurpleBlistRemoveGroup" :int32 (oref group node)))

(defun purple-group-rename (group new-name)
  (interactive (list (purple-group-completing-read)
		     (read-string "New group name: ")))
  (purple-call-method "PurpleBlistRenameGroup"
		      :int32 (oref group node) new-name))

(defun purple-group-completing-read (&optional prompt)
  "Read a string in the minibuffer with ido-style completion to
select a group.
PROMPT is a string to prompt with."
  (interactive)
  (let ((prompt (or prompt "Group: ")))
    (purple-group-find
     'name
     (ido-completing-read prompt
			  (mapcar (rcurry 'slot-value 'name) purple-groups)
			  nil t nil 'purple-group-history))))

(provide 'purple-group)
