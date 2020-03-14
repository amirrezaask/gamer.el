;;; askeys.el --- Personal Emacs keybindings -*- lexical-binding: t; -*-

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
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun delete-word-backward (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive)
  (delete-word (- arg)))

(defun delete-whole-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line))

(defun askeys/--newline-and-comment () "Add a new line to the first of the line and comments that line."
       (call-interactively 'move-beginning-of-line) (insert "\n") (forward-line -1) (call-interactively 'comment-line))

(defvar askeys-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
	(define-key map (kbd "C-q") 'move-beginning-of-line)
	(define-key map (kbd "C-e") 'move-end-of-line)
	(define-key map (kbd "C-s") 'next-line)
	(define-key map (kbd "C-a") 'backward-char)
	(define-key map (kbd "C-d") 'forward-char)
	(define-key map (kbd "C-w") 'previous-line)
	(define-key map (kbd "M-d") 'forward-word)
	(define-key map (kbd "M-a") 'backward-word)
	(define-key map (kbd "M-w") 'backward-sentence)
	(define-key map (kbd "M-s") 'forward-sentence)
	(define-key map (kbd "C-M-d") 'forward-sexp)
	(define-key map (kbd "C-M-a") 'backward-sexp)
	(define-key map (kbd "C-M-w") 'scroll-down-command)
	(define-key map (kbd "C-M-s") 'scroll-up-command)
	(define-key map (kbd "M-e") 'execute-extended-command)
	(define-key map (kbd "M-x") 'next-buffer)
	(define-key map (kbd "M-z") 'previous-buffer)
	(define-key map (kbd "C-`") 'other-window)
	(define-key map (kbd "C-1") 'delete-other-windows)
	(define-key map (kbd "C-2") 'split-window-horizontally)
	(define-key map (kbd "C-3") 'split-window-vertically)
	(define-key map (kbd "C-,") 'kill-ring-save)
	(define-key map (kbd "C-.") 'yank)
	(define-key map (kbd "C-;") 'kill-region)
	(define-key map (kbd "C-l") 'kill-line)
	(define-key map (kbd "M-l") 'kill-whole-line)
	(define-key map (kbd "C-k") 'delete-forward-char)
	(define-key map (kbd "M-k") 'delete-word)
	(define-key map (kbd "C-j") 'delete-backward-char)
	(define-key map (kbd "M-j") 'delete-word-backward)
	(define-key map (kbd "C-M-j") 'kill-sentence)
	map)
     "Amirreza Keybindings.")
(defun askeys/turn-on ()
  "Turn on askeys."
  (interactive)
  (askeys-mode t))

(defun askeys/turn-off ()
  "Turn off askeys."
  (interactive)
  (askeys-mode nil))


(define-minor-mode askeys-mode "amirrezaask modal keybindings for Emacs")
(define-globalized-minor-mode global-askeys-mode askeys-mode askeys/turn-on)
(provide 'askeys.el)
;;; askeys.el ends here
