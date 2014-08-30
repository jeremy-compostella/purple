;;; purple.el --- Purple for Emacs

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

(require 'dbus)

(require 'purple-buddy)
(require 'purple-chat)
(require 'purple-mail)

(defconst purple-dbus-service "im.pidgin.purple.PurpleService")
(defconst purple-object "/im/pidgin/purple/PurpleObject")
(defconst purple-interface "im.pidgin.purple.PurpleInterface")

(defvar purple-accounts '())

;; Shared
(defmacro purple-call-method (method &rest args)
  `(dbus-call-method :session purple-dbus-service
		     purple-object purple-interface ,method ,@args))

(defun purple-call-method-async (method handler &rest args)
  (apply 'dbus-call-method-asynchronously :session purple-dbus-service
	 purple-object purple-interface
	 method handler args))

(defsubst purple-register-signal (sig fun)
  (dbus-register-signal :session purple-dbus-service
			purple-object purple-interface sig fun))

(defun purple-register-signals (signals)
  (dolist (sig signals)
    (purple-register-signal (car sig) (cdr sig))))

;; Init
(defun purple-init ()
  (interactive)
  (setq purple-accounts (purple-account-list))
  (unless purple-accounts
    (error "Purple seems turned off."))
  (mapc 'purple-buddy-init-for-account purple-accounts))

(defun purple-account-list ()
  (purple-call-method "PurpleAccountsGetAllActive"))

;; Tools
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(provide 'purple)
