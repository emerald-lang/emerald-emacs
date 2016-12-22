;;
;; Emerald Language Syntax Highlighting
;; Influenced by slim-mode and jade-mode
;;

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
             "div" "dl" "dt" "em" "embed" "fieldset" "figure" "font" "footer"
             "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head"
             "header" "hgroup" "hr" "html" "i" "iframe" "img" "images" "input"
             "ins" "keygen" "kbd" "label" "legend" "li" "link" "map" "main"
             "mark" "menu" "meta" "metas" "meter" "nav" "noframes" "noscript"
             "object" "ol" "optgroup" "option" "output" "p" "param" "pre"
             "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "scripts"
             "section" "select" "small" "source" "span" "strike" "strong"
             "style" "styles" "sub" "sup" "table" "tbody" "td" "textarea"
             "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var"
             "video" "xmp") 'words))
  "Regex for html tags.")

(defconst emerald-selfclosing-tags-re
  (concat "^ *"
          (regexp-opt
           '("meta" "img" "area" "base" "br" "col" "command" "embed"
             "hr" "input" "link" "param" "source" "track" "wbr") t)))

(defconst emerald-embedded-re "^ *:[a-z0-9_-]+"
  "Regexp matching filter and embedded elements.")

(defconst emerald-comment-re "^ *\*"
  "Regexp matching comment lines.")

(defconst emerald-tag-declaration-char-re "[-a-zA-Z0-9_.#+]"
  "Regexp used to match a character in a tag declaration")

(defun emerald-nested-re (re)
  (concat "^\\( *\\)" re "\\(\\(\n\\(?:\\1 +[^\n]*\\)?\\)*\\)"))

(defconst emerald-font-lock-keywords
  `(("^\\s-*[[:alnum:]_#.]"
     ("\\(#[[:alnum:]_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-keyword-face append))
     ("\\(\\.[a-z0-9_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-variable-name-face append))
     ("[[:alnum:]_)]\\(?::\\s-+[^ ]+\\|\\s-+\\)\\([^\n]*\\)"
      (beginning-of-line) nil
      (1 nil t)))
    (,emerald-tags-re (1 font-lock-function-name-face))

    ("^ *\\+\\([a-z0-9_-]+\\)"
     0 font-lock-builtin-face)

    ;; Single line comments
    ("\\(\*[^\n]*\n\\)"
     1 font-lock-comment-face append)

    (,(emerald-nested-re "\\(:[a-z0-9_]+\\)")
     (0 font-lock-preprocessor-face prepend))
    ("each\\s-+\\w*\\s-+\\(in\\)" (1 font-lock-keyword-face))

    ;; Single quote string
    ("[^a-z]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)

    ;; Double quoted string
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)

    ;; plain text block
    (,(emerald-nested-re "[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\(\\.\\)")
     (3 font-lock-string-face t))

    ;; Plain text inline
    ("^ *|.*" (0 font-lock-string-face t))

    ;; interpolation
    ("[#!]\\({[^}]+}\\|\\[[^]]+\\]\\)"
     (0 font-lock-preprocessor-face prepend))

    ;; include statements
    ("\\<\\(include\\)\\(:[^ \t]+\\|\\s-+\\)\\([^\n]+\\)\n"
     (1 font-lock-keyword-face)
     (2 font-lock-preprocessor-face)
     (3 font-lock-string-face))

    ;; attributes
    ("[a-z0-9-_]("
     ("\\(?:(\\|,?\\s-*\\)\\([[:alnum:]_-]+\\)\\(\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?"
      (backward-char) (forward-char)
      (1 font-lock-constant-face)))))

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
