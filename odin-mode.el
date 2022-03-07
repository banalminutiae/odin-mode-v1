;;; odin-mode-v1-.el -- major mode for editing Odin. -*- coding: utf-8; lexical-binding: t; -*-

;; License:

;; This file is not a part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

(require 'rx)

(defvar odin-mode-v1-hook-nil
  "Hook for odin major mode.")
(defcustom odin-mode-v1-tab-width 4
  "Width of tabs for 'odin-mode'.")
(defcustom arm-comment-char "//"
  "Character to denote inline comments.")

;;;#autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode-v1))

(defconst odin-mode-v1-keywords
  '("package" "import" "proc"
    "for" "do" "in" 
    "if" "else"
    "switch" "case" "enum"
    "defer" "when" "break"
    "continue" "fallthrough"
    "return" "cast" "transmute"
    "auto_cast" "false" "true" "nil"
    "len" "distinct" "struct"
    "swizzle" "dynamic"
    "bit_set" "sizeof" "union"
    "map" "typeid_of" "cap"
    "soa_zip" "soa_unzip"
    "matrix" "using" "or_else"
    "or_return" "context" "foreign"
    "where" "align_of" ))

(defconst odin-mode-v1-types
  '("int" "uint"
    "i8" "u8"
    "i16" "u16"
    "i32" "u32"
    "i64" "u64"
    "i132" "u132"
    "uintptr"
    
    "i16le" "i16be"
    "i32le" "i32be"
    "i64le" "i64be"
    "i128le" "i128be"
    
    "u16le" "u16be"
    "u32le" "u32be"
    "u64le" "u64be"
    "u128le" "u128be"

    "f16" "f32" "f64"
    "f16le" "f16be"
    "f32le" "f32be"
    "f64le" "f64be"

    "complex32" "complex64" "complex128"
    "quaternion64" "quaternion128" "quaternion256"

    "rune" "string" "cstring"
    "rawptr" "typeid" "any" "byte"))

(defvar odin-mode-v1-tab-width 4 "Width of a tab for Odin mode")

(defconst odin-mode-v1-defaults
  '((
     ;; text between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|#\\|=\\|&\\|+\\|%\\|<\\|>\\|'\\|^\\|" . font-lock-keyword-face) ;; ew, replace with syntax table entries
     ( ,(regexp-opt odin-mode-v1-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt odin-mode-v1-types 'words) . font-lock-type-face)
     ;; somehow mangle regex to detect variables for font-lock-variable-name-face
     )))


;; (defconst odin-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?\" "\"" st)
;;     (modify-syntax-entry ?\\ "\\" st)
;;     (modify-syntax-entry ?#   "_" st)
;;     ;; Modify some syntax entries to allow ne sted block comments
;;     (modify-syntax-entry ?/ ". 124b" st)
;;     (modify-syntax-entry ?* ". 23n" st)
;;     (modify-syntax-entry ?\n "> b" st)
;;     (modify-syntax-entry ?\^m "> b" st)
;;     st))


;;;###autoload
(define-derived-mode odin-mode-v1 prog-mode "odin-mode-v1"
  "Major mode for editing the Odin language..."
  (setq font-lock-defaults '(odin-mode-v1-defaults))

  ;; this hopefully invokes emac's default tab behaviour of auto-formatting
  ;; over uniformly inserting spaces
  (when odin-mode-v1-tab-width
    (setq tab-width odin-mode-v1-tab-width))

  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local tab-always-indent nil)
  (setq-local comment-start "//")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
)

(provide 'odin-mode-v1)

;;; odin-mode.el ends here
