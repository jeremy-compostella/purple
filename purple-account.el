;;; purple-account.el --- Purple account for Emacs

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

(defgroup purple-account nil
  "Activity management group"
  :group 'purple)

(defvar purple-accounts '())

(defclass plp-account ()
  ((id :type number :initarg id)
   (status :initarg unknown)
   (name :initarg name)
   (protocol :initarg protocol)))

(defcustom purple-account-props
  '((name	.	"PurpleAccountGetNameForDisplay")
    (protocol	.	"PurpleAccountGetProtocolName"))
  "List of supported account properties method."
  :group 'purple-account)

(defcustom purple-account-signals
  '(("AccountSignedOn"		.	purple-account-signed-on-handler)
    ("AccountSignedOff"		.	purple-account-signed-off-handler)
    ("AccountConnecting"	.	purple-account-connecting-handler))
  "List of supported account signals."
  :group 'purple-buddy)

(defun purple-account-eq (a1 a2)
  (when (and (plp-account-p a1) (plp-account-p a2))
    (= (oref a1 id) (oref a2 id))))

(defun purple-account-init ()
  (setq purple-accounts '())
  (dolist (id (purple-call-method "PurpleAccountsGetAllActive"))
    (let ((account (plp-account id 'id id)))
      (add-to-list 'purple-accounts account t 'purple-account-eq)
      (dolist (prop purple-account-props)
	(set-slot-value account (car prop)
			(purple-call-method (cdr prop) :int32 id)))
      (cond ((= 1 (purple-call-method "PurpleAccountIsConnected" :int32 id))
	     (set-slot-value account status 'signed-on))
	    ((= 1 (purple-call-method "PurpleAccountIsConnecting" :int32 id))
	     (set-slot-value account status 'connecting))
	    ((set-slot-value account status 'signed-off)))))
  (purple-register-signals purple-account-signals))

(defun purple-account-find (field value)
  (find value purple-accounts
	:key (rcurry 'slot-value field) :test 'equal))

(defun purple-account-propertize (account)
  (propertize (oref account protocol)
	      'face
	      (cond ((eq (oref account status) 'signed-on) 'success)
		    ((eq (oref account status) 'connecting) 'warning)
		    ((eq (oref account status) 'signed-off) 'error))))

;; Signals
(defun purple-account-signed-on-handler (id)
  (set-slot-value (purple-account-find 'id id) 'status 'signed-on))

(defun purple-account-signed-off-handler (id)
  (set-slot-value (purple-account-find 'id id) 'status 'signed-off))

(defun purple-account-connecting-handler (id)
  (set-slot-value (purple-account-find 'id id) 'status 'connecting))

;; Interatives
(defun purple-account-connect (account)
  (interactive (purple-account-completing-read))
  (purple-call-method "PurpleAccountConnect" :int32 (oref account id)))

(defun purple-account-disconnect (account)
  (interactive (purple-account-completing-read))
  (purple-call-method "PurpleAccountDisconnect" :int32 (oref account id)))

(defun purple-account-completing-read (&optional prompt)
  "Read a string in the minibuffer with ido-style completion to
select an account.
PROMPT is a string to prompt with."
  (interactive)
  (let ((prompt (or prompt "Account: ")))
    (purple-account-find
     'protocol
     (ido-completing-read prompt
			  (mapcar 'purple-account-propertize purple-accounts)))))

(provide 'purple-account)
