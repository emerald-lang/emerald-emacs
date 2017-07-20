;;; emerald-mode.el --- Emerald Major Mode

;; Copyright (C) 2016-2017 Andrew Robert McBurney

;; Author: Andrew Robert McBurney <andrewrobertmcburney@gmail.com>
;; Maintainer: Andrew Robert McBurney <andrewrobertmcburney@gmail.com>
;; Created: 2016-12-19
;; Keywords: style

;; URL: https://github.com/emerald-lang/emerald-emacs
;; Compatibility: only tested with Spacemacs (Emacs 25.0)
;; Version: 0.0.1
;; Last-Updated: 2017-07-13

;;; License: GPLv3

;;; Commentary:

;; Emerald Language Syntax Highlighting
;; Influenced by slim-mode and jade-mode

;;; Code:

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl-lib))


(defgroup emerald nil
  "Support for the Emerald language-agnostic templating engine."
  :group 'languages
  :prefix "emerald-")


(defcustom emerald-mode-hook nil
  "Hook run when entering Emerald mode."
  :type 'hook
  :group 'emerald)


(defconst emerald-tags-re
  (concat "\\(?:^\\s-*\\|:\\s-+\\)"
          (regexp-opt
           '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
             "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body" "br"
             "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup"
             "command" "datalist" "dd" "del" "details" "dialog" "dfn" "dir"
             "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure"
             "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5"
             "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img"
             "images" "input" "ins" "keygen" "kbd" "label" "legend" "li" "link"
             "map" "main" "mark" "menu" "meta" "metas" "meter" "nav" "noframes"
             "noscript" "object" "ol" "optgroup" "option" "output" "p" "param"
             "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "scripts"
             "section" "select" "small" "source" "span" "strike" "strong"
             "style" "styles" "sub" "sup" "table" "tbody" "td" "textarea"
             "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var"
             "video" "xmp") 'words))
  "Regex for html tags.")


(defun emerald-nested-re (re)
  "Regex for nested emerald code. @param: RE."
  (concat "^\\( *\\)" re "\\(\\(\n\\(?:\\1 +[^\n]*\\)?\\)*\\)"))


(defconst emerald-font-lock-keywords
  `(("^\\s-*[[:alnum:]_#.]"
     ("\\(#[[:alnum:]_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-type-face append))
     ("\\(\\.[a-z0-9_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-builtin-face append))
     ("[[:alnum:]_)]\\(?::\\s-+[^ ]+\\|\\s-+\\)\\([^\n]*\\)"
      (beginning-of-line) nil
      (1 nil t)))
    (,emerald-tags-re (1 font-lock-function-name-face))

    ;; Variable templating
    ("\\(|[^|]*|\\)" 1 font-lock-variable-name-face prepend)

    ;; Single line comments
    ("\\(\*[^\n]*\n\\)"
     1 font-lock-comment-face append)

    ;; Single quote string
    ("[^a-z]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)

    ;; Double quoted string
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)

    ;; Plain text block
    (,(emerald-nested-re "[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\(\\.\\)")
     (3 font-lock-string-face t))

    ;; Plain text inline
    ("^ *|.*" (0 font-lock-string-face t))

    ;; (given, unless, with) simple rule
    ("\\(given\\|unless\\|with\\)\\([^\n]+\\)\n"
     (1 font-lock-keyword-face))

    ;; Each rule
    ("\\(each\\)\\(\\s-+\\(\\w\\|\\.\\)*\\s-+\\)\\(as\\)\\([^\n]+\\)\n"
     (1 font-lock-keyword-face)
     (4 font-lock-keyword-face))

    ;; Include rule
    ("\\<\\(include\\)\\([^\n]+\\)\n"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))))


(defvar emerald-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?= " " table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in emerald-mode buffers.")


(defalias 'emerald-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))


(define-derived-mode emerald-mode emerald-parent-mode "Emerald"
  "Major mode for editing Emerald files"
  (set-syntax-table emerald-mode-syntax-table)
  (setq-local font-lock-multiline t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-indent-chars '(?| ?+))
  (setq-local comment-start "*")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq font-lock-defaults '((emerald-font-lock-keywords) nil t)))


(add-to-list 'auto-mode-alist '("\\.\\(emr\\|emerald\\)\\'" . emerald-mode))
(provide 'emerald-mode)

;;; emerald-mode.el ends here
