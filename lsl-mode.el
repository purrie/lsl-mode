;;; lsl-mode.el --- Major mode for Linden Scripting Language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Purrie
;;
;; Author: Purrie <purriestarshine@gmail.com>
;; Maintainer: Purrie <purriestarshine@gmail.com>
;; Created: December 07, 2022
;; Modified: December 07, 2022
;; Version: 0.0.1
;; Keywords: languages tools
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


(defun lsl-mode-indent-line ()
  "Indent a single line."
  (interactive)
  (let (indent)
  (save-excursion
    (back-to-indentation)
    (setq indent (car (syntax-ppss)))

    (when (or (eq (char-after) ?\))
              (eq (char-after) ?\}))
      (setq indent (1- indent)))

    (delete-region (line-beginning-position)
                   (point))
    (indent-to (* tab-width indent)))))

(defvar lsl-mode-map
  (let ((mp (make-sparse-keymap)))
    (define-key mp (kbd "C-c C-f i") 'lsl-mode-indent-line)
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
  ;; Using c-mode syntax table
  :syntax-table lsl-mode-syntax-table
  (message "LSL mode enabled."))

(provide 'lsl-mode)
;;; lsl-mode.el ends here
