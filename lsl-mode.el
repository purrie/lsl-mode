;;; lsl-mode.el --- Major mode for Linden Scripting Language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Purrie
;;
;; Author: Purrie <purriestarshine@gmail.com>
;; Maintainer: Purrie <purriestarshine@gmail.com>
;; Version: 0.0.1
;; Keywords: languages tools LSL Second Life
;; Homepage: https://github.com/purrie/lsl-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode for Linden Scripting Language (LSL)
;;
;;; Code:

(defun lsl-mode-format-line ()
  "Format a single line."
  (interactive)
  (lsl-mode-indent-line)
  (save-excursion
    (back-to-indentation)
    (while (re-search-forward "[\{\};]" (line-end-position) t)
      ; Detete any trailing whitespaces that follow matched symbol.
      (save-excursion
        (let ((cur (point)))
          (if (re-search-forward "[ \t]+" (line-end-position) t)
              (delete-region cur (point)))))
      ; If the line still contains characters afterwards, move them to a new line.
      (if (not (= (point) (line-end-position)))
          (progn
            (insert "\n")
            (lsl-mode-indent-line))))))

(defun lsl-mode-indent-line ()
  "Indent a single line."
  (interactive)
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss)))

      (when (eq (char-after) ?\})
        (setq indent (1- indent)))

      (delete-region (line-beginning-position)
                     (point))

      (setq indent (* tab-width indent))
      (indent-to indent))))

(defun lsl-mode-auto-indent ()
  "Automatic indentation for LSL mode."
  (lsl-mode-indent-line)
  (back-to-indentation))

(defvar lsl-mode-map
  (let ((mp (make-sparse-keymap)))
    (define-key mp (kbd "C-c C-f i") 'lsl-mode-indent-line)
    (define-key mp (kbd "C-c C-f f") 'lsl-mode-format-line)
    mp)
  "Key map for LSL mode.")

(defvar lsl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for LSL.")

;; setting all .lsl files to be opened with lsl mode automatically
(setq auto-mode-alist
      (append
       '(("\\.lsl$" . lsl-mode))
       auto-mode-alist))

(define-derived-mode lsl-mode
  prog-mode "LSL"
  "Major mode for Linden Scripting Language"
  :syntax-table lsl-mode-syntax-table
  (setq indent-line-function 'lsl-mode-auto-indent)
  (message "LSL mode enabled."))

(provide 'lsl-mode)
;;; lsl-mode.el ends here
