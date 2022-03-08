;;; odin-mode-v1-.el -- major mode for editing Odin. -*- coding: utf-8; lexical-binding: t; -*-

;; License:

;; This file is not a part of GNU Emacs.

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

(eval-when-compile (require 'rx))

(defvar odin-mode-v1-hook-nil
  "Hook for odin major mode.")
(defcustom odin-mode-v1-tab-width 4
  "Width of tabs for 'odin-mode'.")
(defcustom odin-mode-v1-comment-char "//"
  "Character to denote inline comments.")

;;;#autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode-v1))

(defvar odin-mode-v1-tab-width 4 "Width of a tab for Odin mode")

(setq odin-mode-v1-defaults
      (let* (
             (odin-keywords '("package" "import" "proc" "for" "do" "in" 
                              "if" "else" "switch" "case" "enum"
                              "defer" "when" "break" "continue" "fallthrough"
                              "return" "cast" "transmute" "auto_cast" "false" "true" "nil"
                              "len" "distinct" "struct" "swizzle" "dynamic"
                              "bit_set" "sizeof" "union" "map" "typeid_of" "cap"
                              "soa_zip" "soa_unzip" "matrix" "using" "or_else"
                              "or_return" "context" "foreign"
                              "where" "align_of"))
             
             (odin-types '("int" "uint"
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

             ;; generate regex
             (odin-keywords-regexp (regexp-opt odin-keywords 'words))
             (odin-types-regexp (regexp-opt odin-types 'words)))

           ;; map to face attributes
           `(
             (,odin-types-regexp . 'font-lock-types-face)
             (,odin-keywords-regexp . 'font-lock-keywords-face)
           )))

(defconst odin-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\( "()" st)
    
    ;; word contituents
    (modify-syntax-entry ?_   "W" st)
    (modify-syntax-entry ?-   "W" st)
    
    ;; string notation
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "'" st)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23n" st)

    (modify-syntax-entry ?\n ">" st)
    ;; == as punctuation
    (modify-syntax-entry ?= ".")
    st))


;;;###autoload
(define-derived-mode odin-mode-v1 c-mode "odin-mode-v1"
  "Major mode for editing the Odin language..."
  (setq font-lock-defaults '(odin-mode-v1-defaults))

  (when odin-mode-v1-tab-width
    (setq tab-width odin-mode-v1-tab-width))

  ;; bring in c-indent-line-or-region
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local tab-always-indent t)
  (setq-local comment-start "//")
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
)

(provide 'odin-mode-v1)

;;; odin-mode.el ends here
