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

(defun lsl-mode-format-buffer ()
  "Format the entire buffer."
  (interactive)
  (lsl-mode-format-region (point-min) (point-max)))

(defun lsl-mode-format-region (&optional start end)
  "Format selected region.
The function begins at END and formats each line moving towards START point.
If arguments are not supplied, the current region beginning and end are used."
  (interactive)
  (if (not start)
      (setq start (region-beginning)))
  (if (not end)
      (setq end (region-end)))
  (save-excursion
    (goto-char end)
    (let ((do t))
      (while do
        (lsl-mode-format-line)
        ; ending the loop if we reach the start point or we can't move up any further.
        (if (or (< (point) start)
                (= (forward-line -1) -1))
            (setq do nil))))))

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
    (define-key mp (kbd "C-c C-f r") 'lsl-mode-format-region)
    (define-key mp (kbd "C-c C-f b") 'lsl-mode-format-buffer)
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
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for LSL.")

(defvar lsl-mode-font-lock
  (let (types variables tags consts preproc macros keywords standard-functions events)
    ; Using generic matcher to support user defined consts.
    (setq consts "\\<\\([[:upper:]_[:digit:]]\\)+\\>")
    ; Generic matcher, tho, one that matches only actual functions would probably be better.
    (setq standard-functions "\\<\\(ll[[:alnum:]]+\\)\\>[[:blank:]]*(")
    ; All the standard events in LSL.
    (setq events (regexp-opt '("attach" "at_rot_target" "at_target" "changed"
                               "collision" "collision_end" "collision_start" "control"
                               "dataserver" "email" "experience_permissions" "experience_permissions_denied"
                               "http_request" "http_response" "land_collision" "land_collision_start"
                               "land_collision_end" "linkset_data" "link_message" "listen" "money"
                               "moving_start" "moving_end" "not_at_rot_target" "not_at_target" "no_sensor"
                               "object_rez" "on_rez" "path_update" "remote_data" "run_time_permissions"
                               "sensor" "state_entry" "state_exit" "timer" "touch" "touch_start" "touch_end"
                               "transaction_result") 'words))
    ; Firestorm preprocessor macros
    (setq preproc (concat "^#" (regexp-opt '("define" "undef" "ifdef" "ifndef" "if" "elif" "else"
                                "endif" "warning" "error" "include") t) " "))
    ; Firestorm macros
    (setq macros (concat "__" (regexp-opt
                               '("FILE" "LINE" "SHORTFILE" "AGENTID" "AGENTKEY" "AGENTIDRAW" "AGENTNAME" "ASSETID") t)
                         "__"))
    ; LSL types.
    (setq types (regexp-opt '("integer" "float" "string" "key" "list" "vector" "rotation") 'words))
    ; This matches variable declarations, not sure if it's actually useful.
    (setq variables (concat types "[[:blank:]]*\\<\\([[:alnum:]]+\\)\\>[[:blank:]]*="))
    ; Matching for keywords.
    (setq keywords (regexp-opt '("do" "for" "while" "if" "else" "state" "default" "return" "jump") 'words))
    (setq tags "@\\([[:alnum:]]+\\);")
    ; The actual font-lock table.
    `((,standard-functions . (1 'font-lock-function-name-face))
      (,events . 'font-lock-builtin-face)
      (,preproc . 'font-lock-preprocessor-face)
      (,macros . 'font-lock-preprocessor-face)
      (,consts . 'font-lock-constant-face)
      (,types . 'font-lock-type-face)
      (,variables . (2 'font-lock-variable-name-face))
      (,tags . (1 'font-lock-keyword-face))
      (,keywords . 'font-lock-keyword-face)))
  "Code highlighting for LSL mode.")

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
  (setq font-lock-defaults '(lsl-mode-font-lock))
  (message "LSL mode enabled."))

(provide 'lsl-mode)
;;; lsl-mode.el ends here
