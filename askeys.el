;;; askeys.el --- modal-framework for creating various modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Amirreza Askarpour

;; Author: Amirreza Askarpour <amirrezaask@protonmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;;; Code:
(defvar askeys/keymap (make-sparse-keymap) "Main askeys-mode keymap.")

(defun askeys/--register-keys (keychords) "Register given keychords to askeys main keymap.  KEYCHORDS are a tuple of kbds and functions."
       (mapcar (lambda (keyfn)
		 (let ((k (car keyfn)) (fn (cdr keyfn)))
		   (define-key askeys/keymap (kbd k) fn)
		   ))
	       keychords))
(defun askeys/--unregister-keys (keychords) "Unregister given keychords from askeys main keymap.  KEYCHORDS are a list of kbds."
       (mapcar (lambda (key)
		 (unbind-key key askeys/keymap)
		 )
	       keychords)
       )

(defun askeys/edit-mode-enable () "Enable askeys edit mode.")

(defun askeys/command-mode-enable () "Enable askeys command mode.")

(defun askeys/insert-mode-enable () "Enable askeys insert mode.")

(defun askeys/review-mode-enable () "Enable askeys review mpde."
       (askeys/--register-keys '(("i" 'previous-line))))

(define-minor-mode askeys-mode "amirreza modal keybindings for emacs" (askeys/command-mode-enable))
(provide 'modal-framework)
;;; modal-framework.el ends here
