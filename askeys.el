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
	(define-key map (kbd "C-h") 'move-beginning-of-line)
	(define-key map (kbd "C-;") 'move-end-of-line)
	(define-key map (kbd "C-k") 'next-line)
	(define-key map (kbd "C-j") 'backward-char)
	(define-key map (kbd "C-l") 'forward-char)
	(define-key map (kbd "H-i") 'previous-line)
	(define-key map (kbd "M-l") 'forward-word)
	(define-key map (kbd "M-j") 'backward-word)
	(define-key map (kbd "M-i") 'backward-sentence)
	(define-key map (kbd "M-k") 'forward-sentence)
	(define-key map (kbd "C-M-k") 'forward-sexp)
	(define-key map (kbd "C-M-i") 'backward-sexp)
	(define-key map (kbd "C-.") 'next-buffer)
	(define-key map (kbd "C-,") 'previous-buffer)
	(define-key map (kbd "M-/") 'other-window)
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
