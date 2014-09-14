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

(require 'purple-account)
(require 'purple-buddy)
(require 'purple-group)
(require 'purple-chat)
(require 'purple-chat-buffer)
(require 'purple-mail)

(defgroup purple nil
  "Purple group."
  :group 'applications)

(defcustom purple-dbus-service "im.pidgin.purple.PurpleService"
  "Purple dbus service name. Default value is for pidgin."
  :group 'purple)

(defcustom purple-object "/im/pidgin/purple/PurpleObject"
  "Purple object name. Default value is for pidgin."
  :group 'purple)

(defcustom purple-interface "im.pidgin.purple.PurpleInterface"
  "Purple interface name. Default value is for pidgin."
  :group 'purple)

(defvar purple-dbus-register-objects '()
  "List of dbus object obtained when registering signals. ")

;; Shared
(defun purple-call-method (method &rest args)
  (apply 'dbus-call-method
	 :session purple-dbus-service
	 purple-object purple-interface method args))

(defun purple-call-method-async (method handler &rest args)
  (apply 'dbus-call-method-asynchronously
	 :session purple-dbus-service
	 purple-object purple-interface
	 method handler args))

(defun purple-register-signals (signals)
  (dolist (sig signals)
    (let ((progress (make-progress-reporter (format "Registering signal %s" (car sig)))))
      (add-to-list 'purple-dbus-register-objects
		   (dbus-register-signal :session purple-dbus-service
					 purple-object purple-interface
					 (car sig) (cdr sig)))
      (progress-reporter-done progress))))

;; Init
(defun purple-init ()
  "Initialize purple for Emacs."
  (interactive)
  (dolist (obj purple-dbus-register-objects)
    (dbus-unregister-object obj))
  (setq purple-dbus-register-objects '())
  (purple-account-init)
  (purple-status-init)
  (purple-buddy-init-for-accounts purple-accounts)
  (purple-group-init)
  (purple-chat-init)
  (purple-chat-buffer-init))

;; Tools
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(provide 'purple)
