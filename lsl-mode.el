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


;; setting all .lsl files to be opened with lsl mode automatically
(setq auto-mode-alist
      (append
       '(("\\.lsl$" . lsl-mode))
       auto-mode-alist))

(define-derived-mode lsl-mode
  c-mode "LSL"
  "Major mode for Linden Scripting Language"
  ;; Using c-mode syntax table
  :syntax-table nil
  (message "LSL mode enabled."))

(provide 'lsl-mode)
;;; lsl-mode.el ends here
