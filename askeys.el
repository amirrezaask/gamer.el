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
	(define-key map (kbd "C-d") 'move-end-of-line)
	(define-key map (kbd "C-a") 'move-beginning-of-line)
	(define-key map (kbd "C-q") 'backward-paragraph)
	(define-key map (kbd "C-e") 'forward-paragraph)
	(define-key map (kbd "C-i") 'previous-line)
	(define-key map (kbd "C-l") 'right-char)
	(define-key map (kbd "C-j") 'left-char)
	(define-key map (kbd "C-k") 'next-line)
	(define-key map (kbd "C-.") 'forward-word)
	(define-key map (kbd "C-,") 'backward-word)
	(define-key map (kbd "C-;") 'backward-sexp)
	(define-key map (kbd "C-'") 'forward-sexp)
	(define-key map (kbd "M-g") 'magit-status)
	(define-key map (kbd "M-a") 'counsel-M-x)
	(define-key map (kbd "M-b") 'counsel-switch-buffer)
	(define-key map (kbd "M-s") 'swiper)
	(define-key map (kbd "M-w") 'save-buffer)
	(define-key map (kbd "M-;") 'comment-line)
	(define-key map (kbd "M-c") 'kill-ring-save)
	(define-key map (kbd "M-v") 'yank)
	(define-key map (kbd "M-x") 'kill-region)
	(define-key map (kbd "M-e") 'eval-last-sexp)
	(define-key map (kbd "M-t") 'toggle-color-mode)
	(define-key map (kbd "M-k") 'ido-kill-buffer)
	(define-key map (kbd "M-l k") 'delete-whole-line)
        (define-key map (kbd "M-f") 'ido-find-file)
	(define-key map (kbd "M-u") 'undo)
        (define-key map (kbd "C->") 'next-buffer)
	(define-key map (kbd "C-<") 'previous-buffer)
	(define-key map (kbd "C-o") 'other-window)
	(define-key map (kbd "M-'") 'split-window-right)
	(define-key map (kbd "M-=") 'split-window-below)
        (define-key map (kbd "M-k") 'delete-window)
	(define-key map (kbd "M-m") 'delete-other-windows)
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
