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
(defvar askeys-mode-map (make-sparse-keymap) "Main askeys-mode keymap.")
(defvar askeys/basic-movement-keys '(("i"  'previous-line)
				     ("l"  'right-char)
				     ("j"  'left-char)
				     ("k"  'next-line)) "Askeys basic movement which is used in all modes except insert.")

(defun askeys/--register-keys (keychords) "Register given keychords to askeys main keymap.  KEYCHORDS are a tuple of kbds and functions."
       (mapcar (lambda (keyfn)
		 (message ">>> %s" keyfn)
		 (let ((k (car keyfn)) (fn (car (cdr keyfn))))
		   (message "KEY => %s fn=> %s" k fn)
		   (define-key askeys-mode-map (kbd k) fn)))
	       keychords))

(defun askeys/--unregister-keys (keychords) "Unregister given keychords from askeys main keymap.  KEYCHORDS are a tuple of kbds and functions."
       (mapcar (lambda (keyfn)
		 (unbind-key (car keyfn) askeys-mode-map)
		 )
	       keychords) )

(defun askeys/edit-mode-enable () "Enable askeys edit mode.")


(defun askeys/with-basic-movement (keys)
  "Add askeys basic movement to given keys.  KEYS are simple askeys keychords."
  (mapcar (lambda (key)
	    (message "%s" key)
	    (add-to-list 'keys key)
	   ) askeys/basic-movement-keys))

(defun askeys/--newline-and-comment () "Add a new line to the first of the line and comments that line."
       (call-interactively 'move-beginning-of-line) (insert "\n") (forward-line -1) (call-interactively 'comment-line))

(defvar askeys/review-mode-keys (askeys/with-basic-movement '(("c"  'askeys/command-mode-enable)
							      (";"  'askeys/--newline-and-comment)))
  "Designed for code review scenarios.
Basic movement defined in askeys/basic-movement-keys
  a => insert new line at first of the line (so basically upper line)
  c => comment current line")


(defvar askeys/insert-mode-keys '(
				  '("C-c C-n" . 'askeys/command-mode-enable)
				  ) "Default Emacs Behaviour plus triggers for various modes.")

(defvar askeys/command-mode-keys '(("g" (lambda () (interactive) (magit-status)))))
(defun askeys/review-mode-enable () "Enable askeys review mode."
       (askeys/--register-keys askeys/review-mode-keys))

(defun askeys/command-mode-enable () "Enable askeys command mode."
       (interactive)
       (askeys/--register-keys askeys/command-mode-keys))
(defun askeys/insert-mode-enable () "Enable askeys insert mode."
       (askeys/--register-keys askeys/insert-mode-keys))


(defun askeys/turn-on ()
  (interactive)
  (askeys-mode t)
  (askeys/command-mode-enable))

(define-minor-mode askeys-mode "amirreza modal keybindings for emacs")
(global-set-key (kbd "M-SPC") 'askeys/turn-on)
(provide 'askeys)
;;; askeys.el ends here
