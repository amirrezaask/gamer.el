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

(defun askeys/--command-mode-is-enabled-callback ()
  "Show simple note that user is in command mode."
  (interactive)
  (message "You are in command mode"))

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

(defvar askeys-mode-map (make-sparse-keymap) "Main askeys-mode keymap.")

(defun askeys/--newline-and-comment () "Add a new line to the first of the line and comments that line."
       (call-interactively 'move-beginning-of-line) (insert "\n") (forward-line -1) (call-interactively 'comment-line))

(defun askeys/command-mode-enable ()
  "Add command mode keys to askeys mode map."
  (interactive)
  (define-key askeys-mode-map (kbd "~")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "!")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "@")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "#")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "$")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "%")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "^")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "&")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "*")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "(")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd ")")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "_")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "+")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "`")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "1")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "2")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "3")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "4")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "5")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "6")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "7")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "8")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "9")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "0")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "-")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "=")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "a")  'helm-M-x)
  (define-key askeys-mode-map (kbd "b")  'kill-region)
  (define-key askeys-mode-map (kbd "c")  'delete-forward-char)
  (define-key askeys-mode-map (kbd "d")  'kill-word)
  (define-key askeys-mode-map (kbd "e")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "f")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "g")  'magit-status)
  (define-key askeys-mode-map (kbd "h")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "i")  'previous-line)
  (define-key askeys-mode-map (kbd "j")  'left-char)
  (define-key askeys-mode-map (kbd "k")  'next-line)
  (define-key askeys-mode-map (kbd "l")  'right-char)
  (define-key askeys-mode-map (kbd "m")  'kill-ring-save)
  (define-key askeys-mode-map (kbd "n")  'yank)
  (define-key askeys-mode-map (kbd "o")  'forward-word)
  (define-key askeys-mode-map (kbd "p")  'forward-sexp)
  (define-key askeys-mode-map (kbd "q")  'kill-line)
  (define-key askeys-mode-map (kbd "r")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "s")  'delete-word-backward)
  (define-key askeys-mode-map (kbd "t")  'toggle-color-mode)
  (define-key askeys-mode-map (kbd "u")  'backward-word)
  (define-key askeys-mode-map (kbd "v")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "w")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "x")  'backward-delete-char-untabify)
  (define-key askeys-mode-map (kbd "y")  'backward-sexp)
  (define-key askeys-mode-map (kbd "z")  'undo)
  (define-key askeys-mode-map (kbd "<")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd ">")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd ",")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd ".")  'xref-find-definitions)
  (define-key askeys-mode-map (kbd "/")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "\\")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "{")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "}")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "[")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "]")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "'")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd "\"")  'askeys/--command-mode-is-enabled-callback)
  (define-key askeys-mode-map (kbd ";")  'comment-line)
  (define-key askeys-mode-map (kbd "SPC") 'askeys/insert-mode-enable)
  (message "command mode enabled"))
;; (mode-line-misc-info)
 (defun askeys/insert-mode-enable ()
  "Add insert mode keys (basically Emacs) but with a way to get to command mode."
  (interactive)
  (message "insert mode enabled")
  (define-key askeys-mode-map (kbd "~")  nil)
  (define-key askeys-mode-map (kbd "!")  nil)
  (define-key askeys-mode-map (kbd "@")  nil)
  (define-key askeys-mode-map (kbd "#")  nil)
  (define-key askeys-mode-map (kbd "$")  nil)
  (define-key askeys-mode-map (kbd "%")  nil)
  (define-key askeys-mode-map (kbd "^")  nil)
  (define-key askeys-mode-map (kbd "&")  nil)
  (define-key askeys-mode-map (kbd "*")  nil)
  (define-key askeys-mode-map (kbd "(")  nil)
  (define-key askeys-mode-map (kbd ")")  nil)
  (define-key askeys-mode-map (kbd "_")  nil)
  (define-key askeys-mode-map (kbd "+")  nil)
  (define-key askeys-mode-map (kbd "`")  nil)
  (define-key askeys-mode-map (kbd "1")  nil)
  (define-key askeys-mode-map (kbd "2")  nil)
  (define-key askeys-mode-map (kbd "3")  nil)
  (define-key askeys-mode-map (kbd "4")  nil)
  (define-key askeys-mode-map (kbd "5")  nil)
  (define-key askeys-mode-map (kbd "6")  nil)
  (define-key askeys-mode-map (kbd "7")  nil)
  (define-key askeys-mode-map (kbd "8")  nil)
  (define-key askeys-mode-map (kbd "9")  nil)
  (define-key askeys-mode-map (kbd "0")  nil)
  (define-key askeys-mode-map (kbd "-")  nil)
  (define-key askeys-mode-map (kbd "=")  nil)
  (define-key askeys-mode-map (kbd "a")  nil)
  (define-key askeys-mode-map (kbd "b")  nil)
  (define-key askeys-mode-map (kbd "c")  nil)
  (define-key askeys-mode-map (kbd "d")  nil)
  (define-key askeys-mode-map (kbd "e")  nil)
  (define-key askeys-mode-map (kbd "f")  nil)
  (define-key askeys-mode-map (kbd "g")  nil)
  (define-key askeys-mode-map (kbd "h")  nil)
  (define-key askeys-mode-map (kbd "i")  nil)
  (define-key askeys-mode-map (kbd "j")  nil)
  (define-key askeys-mode-map (kbd "k")  nil)
  (define-key askeys-mode-map (kbd "l")  nil)
  (define-key askeys-mode-map (kbd "m")  nil)
  (define-key askeys-mode-map (kbd "n")  nil)
  (define-key askeys-mode-map (kbd "o")  nil)
  (define-key askeys-mode-map (kbd "p")  nil)
  (define-key askeys-mode-map (kbd "q")  nil)
  (define-key askeys-mode-map (kbd "r")  nil)
  (define-key askeys-mode-map (kbd "s")  nil)
  (define-key askeys-mode-map (kbd "t")  nil)
  (define-key askeys-mode-map (kbd "u")  nil)
  (define-key askeys-mode-map (kbd "v")  nil)
  (define-key askeys-mode-map (kbd "w")  nil)
  (define-key askeys-mode-map (kbd "x")  nil)
  (define-key askeys-mode-map (kbd "y")  nil)
  (define-key askeys-mode-map (kbd "z")  nil)
  (define-key askeys-mode-map (kbd "<")  nil)
  (define-key askeys-mode-map (kbd ">")  nil)
  (define-key askeys-mode-map (kbd ",")  nil)
  (define-key askeys-mode-map (kbd ".")  nil)
  (define-key askeys-mode-map (kbd "/")  nil)
  (define-key askeys-mode-map (kbd "\\")  nil)
  (define-key askeys-mode-map (kbd "{")  nil)
  (define-key askeys-mode-map (kbd "}")  nil)
  (define-key askeys-mode-map (kbd "[")  nil)
  (define-key askeys-mode-map (kbd "]")  nil)
  (define-key askeys-mode-map (kbd "'")  nil)
  (define-key askeys-mode-map (kbd "\"")  nil)
  (define-key askeys-mode-map (kbd ";")  nil)
  (define-key askeys-mode-map (kbd "SPC") nil)
  (define-key askeys-mode-map (kbd "M-SPC") 'askeys/command-mode-enable))

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

(global-set-key (kbd "C-c C-a") 'askeys/turn-on)

(provide 'askeys.el)
;;; askeys.el ends here
