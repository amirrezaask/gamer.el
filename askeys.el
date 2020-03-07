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

(defun askeys/--newline-and-comment () "Add a new line to the first of the line and comments that line."
       (call-interactively 'move-beginning-of-line) (insert "\n") (forward-line -1) (call-interactively 'comment-line))

(defun askeys/basic-movement ()
  "Add basic movement to askyes map."
  (define-key askeys-mode-map (kbd "i")  'previous-line)
  (define-key askeys-mode-map (kbd "l")  'right-char)
  (define-key askeys-mode-map (kbd "j")  'left-char)
  (define-key askeys-mode-map (kbd "k")  'next-line))


(defun askeys/command-mode-enable ()
  "Add command mode keys to askeys mode map."
  (interactive)
  (message "command mode enabled")
  (setq askeys-mode-map (make-sparse-keymap))
  (askeys/basic-movement)
  (define-key askeys-mode-map (kbd "g") 'magit-status)
  (define-key askeys-mode-map (kbd "u") 'askeys-insert-mode-enable))

(defun askeys/insert-mode-enable ()
  "Add insert mode keys (basically Emacs) but with a way to get to command mode."
  (interactive)
  (message "insert mode enabled")
  (setq askeys-mode-map (make-sparse-keymap))
  (define-key askeys-mode-map (kbd "C-c C-c") 'askeys/command-mode-enable))

(defun askeys/turn-on ()
  "Turn on askeys."
  (interactive)
  (askeys-mode t)
  (askeys/command-mode-enable)
  )

(defun askeys/turn-off ()
  "Turn off askeys."
  (interactive)
  (setq askeys-mode-map (make-sparse-keymap))
  (askeys-mode nil))


(define-minor-mode askeys-mode "amirrezaask modal keybindings for Emacs")

(global-set-key (kbd "M-SPC") 'askeys/turn-on)

(provide 'askeys.el)
;;; askeys.el ends here
