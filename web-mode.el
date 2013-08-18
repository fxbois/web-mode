;;; web-mode.el --- major mode for editing html templates

;; Copyright 2011-2013 François-Xavier Bois

;; Version: 6.0.38
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: html template php javascript js css web
;;           django jsp asp erb twig jinja blade dust closure
;;           freemarker mustache velocity cheetah smarty
;; URL: http://web-mode.org
;; Repository: http://github.com/fxbois/web-mode

;; =========================================================================
;; This work is sponsored by Kernix : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code goes here

;;todo : ne mettre tag-type et tag-name que sur le '<'
;;todo : créer tag-token pour différentier de part-token : tag-token=attr,comment ???

(defgroup web-mode nil
  "Major mode for editing web templates:
   HTML files embedding parts (CSS/JavaScript)
   and blocks (PHP, Erb, Django/Twig, Smarty, JSP, ASP, etc.)."
  :version "6.0.38"
  :group 'languages)

(defgroup web-mode-faces nil
  "Faces for syntax highlighting."
  :group 'web-mode
  :group 'faces)

(defcustom web-mode-script-padding 1
  "Script element left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-style-padding 1
  "Style element left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-block-padding 0
  "Multi-line block (PHP, Ruby, Java, etc.) left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-markup-indent-offset 2
  "HTML indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-css-indent-offset 2
  "CSS indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-code-indent-offset 2
  "Code (JavaScript, PHP, etc.) indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-disable-css-colorization (not (display-graphic-p))
  "In a CSS block, do not set background according to the color: #xxx, rgb(x,x,x)."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-disable-auto-indentation (not (display-graphic-p))
  "Disable auto-indentation."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-disable-auto-pairing (not (display-graphic-p))
  "Disable auto-pairing."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-whitespaces nil
  "Enable whitespaces."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-block-face nil
  "Enable block face (useful for setting a background for example).
See web-mode-block-face."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-part-face nil
  "Enable part face (useful for setting a background for example).
See web-mode-part-face."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-heredoc-fontification nil
  "Enable heredoc fontification. The identifier should contain JS, JAVASCRIPT or HTML."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-comment-keywords nil
  "Enable highlight of keywords like FIXME, TODO, etc. in comments."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-comment-style 1
  "Comment style : 2 = server comments."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-indent-style 1
  "Indentation style.
with value 2, HTML lines beginning text are also indented (do not forget side effects, ie. content of a textarea)."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-tag-auto-close-style 1
  "Tag auto-close style:
0=no auto-closing
1=auto-close with </
2=auto-close with > and </."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-extra-php-constants '()
  "A list of additional strings to treat as PHP constants."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-php-keywords '()
  "A list of additional strings to treat as PHP keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-jsp-keywords '()
  "A list of additional strings to treat as JSP keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-python-keywords '()
  "A list of additional strings to treat as Python keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-erb-keywords '()
  "A list of additional strings to treat as ERB keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-asp-keywords '()
  "A list of additional strings to treat as ASP keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-asp-types '()
  "A list of additional strings to treat as ASP types."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-aspx-keywords '()
  "A list of additional strings to treat as ASPX keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-javascript-keywords '()
  "A list of additional strings to treat as JS keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-razor-keywords '()
  "A list of additional strings to treat as Razor keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-comment-keywords '()
  "A list of additional strings to treat as comment keywords."
  :type 'list
  :group 'web-mode)

(defface web-mode-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for warning."
  :group 'web-mode-faces)

(defface web-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-block-control-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
  :group 'web-mode-faces)

(defface web-mode-symbol-face
  '((t :foreground "gold"))
  "Face for symbols."
  :group 'web-mode-faces)

(defface web-mode-doctype-face
  '((t :foreground "Grey"))
  "Face for HTML doctype."
  :group 'web-mode-faces)

(defface web-mode-html-tag-face
  '((t :foreground "Snow4"))
  "Face for HTML tags."
  :group 'web-mode-faces)

(defface web-mode-html-attr-name-face
  '((t :foreground "Snow3"))
  "Face for HTML attribute names (including =)."
  :group 'web-mode-faces)

(defface web-mode-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for HTML attribute values."
  :group 'web-mode-faces)

(defface web-mode-block-attr-name-face
  '((t :inherit web-mode-html-attr-name-face))
  "Face for block attribute names."
  :group 'web-mode-faces)

(defface web-mode-block-attr-value-face
  '((t :inherit web-mode-html-attr-value-face))
  "Face for block attribute values."
  :group 'web-mode-faces)

(defface web-mode-css-selector-face
  '((t :inherit font-lock-keyword-face))
  "Face for CSS rules."
  :group 'web-mode-faces)

(defface web-mode-css-pseudo-class-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS pseudo-classes."
  :group 'web-mode-faces)

(defface web-mode-css-at-rule-face
  '((t :inherit font-lock-constant-face))
  "Face for CSS at-rules."
  :group 'web-mode-faces)

(defface web-mode-css-property-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for CSS props."
  :group 'web-mode-faces)

(defface web-mode-css-color-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS colors (#xxx)."
  :group 'web-mode-faces)

(defface web-mode-css-priority-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS priority (!important)."
  :group 'web-mode-faces)

(defface web-mode-css-function-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS functions."
  :group 'web-mode-faces)

(defface web-mode-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names."
  :group 'web-mode-faces)

(defface web-mode-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'web-mode-faces)

(defface web-mode-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings."
  :group 'web-mode-faces)

(defface web-mode-block-string-face
  '((t :inherit web-mode-string-face))
  "Face for block strings."
  :group 'web-mode-faces)

(defface web-mode-part-string-face
  '((t :inherit web-mode-string-face))
  "Face for part strings."
  :group 'web-mode-faces)

(defface web-mode-javascript-string-face
  '((t :inherit web-mode-string-face))
  "Face for javascript strings."
  :group 'web-mode-faces)

(defface web-mode-css-string-face
  '((t :inherit web-mode-string-face))
  "Face for css strings."
  :group 'web-mode-faces)

(defface web-mode-json-key-face
  '((t :foreground "plum"))
  "Face for json key strings."
  :group 'web-mode-faces)

(defface web-mode-json-context-face
  '((t :foreground "orchid3"))
  "Face for json context strings."
  :group 'web-mode-faces)

(defface web-mode-json-string-face
  '((t :inherit web-mode-string-face))
  "Face for json strings."
  :group 'web-mode-faces)

(defface web-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'web-mode-faces)

(defface web-mode-block-comment-face
  '((t :inherit web-mode-comment-face))
  "Face for server comments."
  :group 'web-mode-faces)

(defface web-mode-part-comment-face
  '((t :inherit web-mode-comment-face))
  "Face for part comments."
  :group 'web-mode-faces)

(defface web-mode-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for language constants."
  :group 'web-mode-faces)

(defface web-mode-type-face
  '((t :inherit font-lock-type-face))
  "Face for language types."
  :group 'web-mode-faces)

(defface web-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'web-mode-faces)

(defface web-mode-param-name-face
  '((t :foreground "Snow3"))
  "Face for server attribute names."
  :group 'web-mode-faces)

(defface web-mode-whitespace-face
  '((t :background "DarkOrchid4"))
  "Face for whitespaces."
  :group 'web-mode-faces)

(defface web-mode-block-face
  '((((class color) (min-colors 88) (background dark))
     :background "grey18")
    (((class color) (min-colors 88) (background light))
     :background "LightYellow1")
    (((class color) (min-colors 16) (background dark))
     :background "grey18")
    (((class color) (min-colors 16) (background light))
     :background "LightYellow1")
    (((class color) (min-colors 8))
     :background "Black")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "grey"))
  "Face for blocks (useful for setting a background for example).
Must be used in conjunction with web-mode-enable-block-face."
  :group 'web-mode-faces)

(defface web-mode-part-face
  '((t :inherit web-mode-block-face))
  "Face for parts."
  :group 'web-mode-faces)

(defface web-mode-folded-face
  '((t :underline t))
  "Overlay face for folded."
  :group 'web-mode-faces)

(defface web-mode-comment-keyword-face
  '((t :weight bold :box t))
  "Comment keywords."
  :group 'web-mode-faces)

(defvar web-mode-void-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr")
  "Void (self-closing) tags.")

(defvar web-mode-text-properties
  '(part-side nil part-token nil part-language nil tag-name nil tag-type nil tag-beg nil tag-end nil block-side nil block-token nil block-beg nil block-end nil face nil)
  "Text properties used for fontification and indentation.")

(defvar web-mode-is-scratch nil
  "Is scratch buffer ?")

(defvar web-mode-time nil
  "For benchmarking")

(defvar web-mode-expand-initial-pos nil
  "First mark pos.")

(defvar web-mode-expand-previous-state ""
  "Last mark state.")

;;"<\\(/?[[:alpha:]@#][[:alnum:]:_]*\\)"
(defvar web-mode-tag-regexp "<\\(/?[[:alpha:]][[:alnum:]]*\\)"
  "Regular expression for HTML/XML tag.")

(defvar web-mode-start-tag-regexp "<\\([[:alpha:]][[:alnum:]]*\\)"
  "Regular expression for HTML/XML start tag.")

(defvar web-mode-whitespaces-regexp
  "^[ \t]\\{2,\\}$\\| \t\\|\t \\|[ \t]+$\\|^[ \n\t]+\\'\\|^[ \t]?[\n]\\{2,\\}"
  "Regular expression for whitespaces.")

(defvar web-mode-engine nil
  "Template engine")

(defvar web-mode-engine-families
  '(("asp"        . ("asp"))
    ("aspx"       . ("aspx"))
    ("blade"      . ("laravel"))
    ("closure"    . ("soy"))
    ("ctemplate"  . ("mustache" "handlebars" "hapax" "ngtemplate" "ember" "kite"))
    ("django"     . ("dtl" "twig" "swig" "jinja" "jinja2" "erlydtl"))
    ("dust"       . ())
    ("erb"        . ("eruby" "erubis"))
    ("go"         . ("gtl"))
    ("jsp"        . ())
    ("python"     . ())
    ("razor"      . ("play" "play2"))
    ("velocity"   . ("vtl" "cheetah")))
  "Engine name aliases")

(defvar web-mode-content-types
  '(("css"        . "\\.css\\'")
    ("javascript" . "\\.js\\'")
    ("json"       . "\\.\\(json\\|jsonld\\)\\'")
    ("html"       . "."))
  "content types")

(defvar web-mode-engine-file-regexps
  '(("asp"        . "\\.asp\\'")
    ("aspx"       . "\\.as[cp]x\\'")
    ("blade"      . "\\.blade")
    ("closure"    . "\\.soy\\'")
    ("ctemplate"  . "\\.\\(chtml\\)\\'")
    ("django"     . "\\.\\(djhtml\\|tmpl\\|dtl\\)\\'")
    ("django"     . "twig")
    ("dust"       . "\\.dust\\'")
    ("erb"        . "\\.\\(erb\\|rhtml\\)\\'")
    ("freemarker" . "\\.ftl\\'")
    ("go"         . "\\.go\\(html\\|tmpl\\)\\'")
    ("handlebars" . "\\(handlebars\\|.\\hbs\\'\\)")
    ("jsp"        . "\\.jsp\\'")
    ("mustache"   . "\\.mustache\\'")
    ("php"        . "\\.\\(php\\|ctp\\|psp\\|inc\\)\\'")
    ("python"     . "\\.pml\\'")
    ("razor"      . "play\\|\\.scala\\.\\|\\.cshtml\\'\\|\\.vbhtml\\'")
    ("smarty"     . "\\.tpl\\'")
    ("velocity"   . "\\.\\(vsl\\|vtl\\|vm\\)\\'"))
  "Engine file extensions.")

(defvar web-mode-smart-quotes
  '("«" . "»")
  "Preferred smart quotes")

(defvar web-mode-xml-chars
  '((?\& . "&amp;")
    (?\< . "&lt;")
    (?\> . "&gt;"))
  "XML chars")

(defvar web-mode-html-entities
  '(("eacute" . "é")
    ("egrave" . "è")
    ("middot" . "·")
    ("quot"   . "\"")
    ("amp"    . "&")
    ("lt"     . "<")
    ("gt"     . ">")
    ("laquo"  . "«")
    ("raquo"  . "»")
    ("lsquo"  . "‘")
    ("rsquo"  . "’")
    ("ldquo"  . "“")
    ("rdquo"  . "”")
    ("apos"   . "'"))
  "HTML entities")

(defvar web-mode-snippets
  (list
   '("table"
     "<table><tbody>\n<tr>\n<td>"
     "</td>\n<td></td>\n</tr>\n</tbody></table>")
   '("ul"
     "<ul>\n<li>"
     "</li>\n<li></li>\n</ul>")
   '("if"
     "<?php if ( as ): ?>\n"
     "\n<?php endif; ?>")
   '("for"
     "<?php for ( ; ; ): ?>\n"
     "\n<?php endfor; ?>")
   '("foreach"
     "<?php foreach ( as ): ?>\n"
     "\n<?php endforeach; ?>")
   '("html5"
     "<!doctype html>\n<html>\n<head>\n<title></title>\n<meta charset=\"utf-8\" />\n</head>\n<body>\n"
     "\n</body>\n</html>")
   )
  "Code snippets")

(defvar web-mode-auto-pairs
  (list
   '("<?p"  "hp  ?>"   "\\?>"  3)
   '("<? "  "?>"       "\\?>"  0)
   '("<?="  "?>"       "\\?>"  0)
   '("<!-"  "-  -->"   "--"    2)
   '("<%-"  "-  --%>"  "--"    2)
   '("<%@"  "  %>"     "%>"    1)
   '("<%="  "%>"       "%>"    0)
   '("<% "  " %>"      "%>"    0)
   '("{{ "  " }}"      "}}"    0)
   '("{% "  " %}"      "%}"    0)
   '("{# "  " #}"      "#}"    0)
   )
  "Auto-Pairs")

(defvar web-mode-content-type ""
  "Buffer file type.")

(defvar web-mode-comments-invisible nil
  "Comments visbility.")

(defvar web-mode-is-narrowed nil
  "Buffer has been narrowed.")

(defvar web-mode-hook nil
  "List of functions to be executed with web-mode.")

(defvar web-mode-buffer-highlighted nil
  "Is buffer highlighted.")

(defvar web-mode-display-table nil
  "Display table.")

(defvar web-mode-hl-line-mode-flag nil
  "Is hl-line-mode enabled ?")

(defvar web-mode-blade-active-blocks
  '("else" "elseif" "foreach" "forelse" "for" "if" "section" "unless" "while")
  "Blade controls.")

(defvar web-mode-closure-active-blocks
  '("call" "case" "default" "deltemplate" "else" "elseif" "for" "foreach"
    "if" "ifempty" "let" "literal" "msg" "param" "switch" "template")
  "Closure controls.")

(defvar web-mode-django-active-blocks
  '("assets" "autoescape" "block" "cache" "call"
    "elif" "else" "elseif" "embed" "filter" "foreach" "for"
    "ifchanged" "ifequal" "ifnotequal" "if"
    "macro" "draw" "random" "sandbox" "spaceless" "trans" "with")
  "Django controls.")

(defvar web-mode-go-active-blocks
  '("else" "end" "if" "range" "with")
  "Go controls.")

(defvar web-mode-php-active-blocks
  '("else" "elseif" "for" "foreach" "if" "while")
  "PHP controls.")

(defvar web-mode-smarty-active-blocks
  '("block" "else" "elseif" "foreach" "for" "if" "section" "while")
  "Smarty controls.")

(defvar web-mode-velocity-active-blocks
  '("define" "else" "elseif" "end" "for" "foreach" "if" "macro")
  "Velocity controls.")

(defvar web-mode-active-block-regexps
  (list
   (cons "asp"        "----")
   (cons "aspx"       "----")
   (cons "blade"      (concat "@\\(end\\)?" (regexp-opt web-mode-blade-active-blocks t)))
   (cons "closure"    (concat "{/?" (regexp-opt web-mode-closure-active-blocks t)))
   (cons "ctemplate"  "{{[#^/]\\([[:alnum:]_]+\\)")
   (cons "django"     (concat "{%[-]?[ ]+\\(end\\)?" (regexp-opt web-mode-django-active-blocks t)))
   (cons "dust"       "{[#/:?@><+^]\\([[:alpha:]_]+\\)")
   (cons "erb"        "<%[-]?[ ]+\\(.* do \\|for\\|unless\\|end\\|if\\|else\\)")
   (cons "freemarker" "</?\\([[:alpha:]]+:[[:alpha:]]+\\)\\|[[<]/?[@#]\\([[:alpha:]]+\\)")
   (cons "go"         (concat "{{[ ]*" (regexp-opt web-mode-go-active-blocks t)))
   (cons "jsp"        "</?\\([[:alpha:]]+:[[:alpha:]]+\\)")
   (cons "php"        (concat "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?" (regexp-opt web-mode-php-active-blocks t)))
   (cons "razor"      "----")
   (cons "smarty"     (concat "{/?" (regexp-opt web-mode-smarty-active-blocks t)))
   (cons "velocity"   (concat "#" (regexp-opt web-mode-velocity-active-blocks t))))
  "Engine control regexps")

(defvar web-mode-close-block-regexps
  '(("asp"        . "----")
    ("aspx"       . "----")
    ("blade"      . "@\\\(end\\|else\\)")
    ("closure"    . "{\\(/\\|else\\|case\\|default\\|ifempty\\)")
    ("ctemplate"  . "{{/")
    ("django"     . "{%[-]?[ ]+\\(end\\|else\\|elseif\\|elif\\)")
    ("dust"       . "{\\(/\\|:else\\)")
    ("erb"        . "<%[-]?[ ]+\\(end\\|else\\)")
    ("freemarker" . "[<[]\\(/#\\|#els\\|#break\\)")
    ("go"         . "{{[ ]*\\(end\\|else\\)")
    ("jsp"        . "</")
    ("php"        . "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\|else\\|}\\)")
    ("razor"      . "}")
    ("smarty"     . "{\\(/\\|else\\)")
    ("velocity"   . "#\\(end\\|else\\)"))
  "Close control blocks.")

(defvar web-mode-block-regexps
  '(("asp"        . "<%")
    ("aspx"       . "<%")
    ("blade"      . "{{\\|^[ \t]*@[[:alpha:]]")
    ("closure"    . "{.\\|/\\*\\| //")
    ("ctemplate"  . "[$]?{{.")
    ("django"     . "{[#{%]")
    ("dust"       . "{.")
    ("erb"        . "<%\\|^%.")
    ("freemarker" . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]\\|</?[@#].\\|\\[/?[@#].")
    ("go"         . "{{.")
    ("jsp"        . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]")
    ("php"        . "<\\?")
    ("python"     . "<\\?")
    ("razor"      . "@.")
    ("smarty"     . "{[[:alpha:]#$/*\"]")
    ("velocity"   . "^[ \t]*#[[:alpha:]#*]\\|$[[:alpha:]!{]"))
  "Engine block regexps.")

(defvar web-mode-block-regexp nil
  "Regular expression for identifying blocks.")

(defvar web-mode-active-block-regexp nil
  "Engine control regexp")

(defvar web-mode-close-block-regexp nil
  "Engine end control regexp")

(defvar web-mode-engine-control-matcher nil
  "Engine control match")

(defvar web-mode-comment-keywords
  (regexp-opt
   (append web-mode-extra-comment-keywords
           '("FIXME" "TODO" "BUG" "KLUDGE" "WORKAROUND"
             "OPTIMIZE" "HACK" "REFACTOR")))
  "Comment keywords.")

(defvar web-mode-php-constants
  (regexp-opt
   (append web-mode-extra-php-constants
           '("TRUE" "FALSE" "NULL" "true" "false" "null"
             "STR_PAD_LEFT" "STR_PAD_RIGHT"
             "ENT_COMPAT" "ENT_QUOTES" "ENT_NOQUOTES" "ENT_IGNORE"
             "ENT_SUBSTITUTE" "ENT_DISALLOWED" "ENT_HTML401" "ENT_XML1"
             "ENT_XHTML" "ENT_HTML5"
             "LIBXML_NOBLANKS")))
  "PHP constants.")

(defvar web-mode-php-keywords
  (regexp-opt
   (append web-mode-extra-php-keywords
           '("and" "array" "as" "break"
             "callable" "case" "catch"  "catch all" "class" "const" "continue"
             "default" "die" "do"
             "echo" "else" "elseif" "empty"
             "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit" "extends"
             "finally" "for" "foreach" "function" "global"
             "if" "include" "include_once" "instanceof" "interface" "isset"
             "list" "next" "new" "or"
             "private" "protected" "public"
             "require" "require_once" "return" "static" "switch" "try" "throw" "unset" "use"
             "var" "when" "while" "xor" "yield")))
  "PHP keywords.")

(defvar web-mode-php-types
  (eval-when-compile
    (regexp-opt
     '("array" "bool" "boolean" "char" "const" "double" "float"
       "int" "integer" "long" "mixed" "object" "real" "string"
       "Exception")))
  "PHP types.")

(defvar web-mode-css-at-rules
  (eval-when-compile
    (regexp-opt
     '("charset" "import" "media" "page" "font-face" "namespace")))
  "CSS at-rules.")

(defvar web-mode-css-pseudo-classes
  (eval-when-compile
    (regexp-opt
     '("active" "after" "before" "checked" "disabled" "empty" "enabled"
       "first" "first-child" "first-letter" "first-line" "first-of-type" "focus"
       "hover" "lang" "last-child" "last-of-type" "left" "link"
       "not" "nth-child" "nth-last-child" "nth-last-of-type" "nth-of-type"
       "only-child" "only-of-type"
       "right" "root" "selection" "target" "visited")))
  "CSS pseudo-classes (and pseudo-elements).")

(defvar web-mode-python-keywords
  (regexp-opt
   (append web-mode-extra-python-keywords
           '("False" "class" "finally" "is" "return"
             "None" "continue" "for" "lambda" "try"
             "True" "def" "from" "nonlocal" "while"
             "and" "del" "global" "not" "with"
             "as" "elif" "if" "or" "yield"
             "assert" "else" "import" "pass"
             "break" "except" "in" "raise")))
  "Python keywords.")

(defvar web-mode-jsp-keywords
  (regexp-opt
   (append web-mode-extra-jsp-keywords
           '("case" "catch" "do" "else" "end" "false" "for" "function"
             "if" "in" "include" "new"
             "package" "page" "private" "protected" "public"
             "return" "tag" "taglib" "throw" "throws" "true" "try"
             "void" "while")))
  "JSP keywords.")

(defvar web-mode-erb-keywords
  (regexp-opt
   (append web-mode-extra-erb-keywords
           '("BEGIN" "END" "__FILE__" "__LINE__"
             "alias" "and" "begin" "break" "button_to_function"
             "case" "class" "csrf_meta_tag"
             "def" "defined" "do" "else" "elsif" "end"
             "ensure" "escape_javascript" "false" "for" "form_for" "h" "html_escape"
             "if" "in" "j" "javascript_include_tag" "javascript_tag"
             "link_to" "link_to_function" "module" "next" "nil" "not"
             "or" "package" "puts" "raw" "redo" "render" "rescue" "retry" "return"
             "self" "super" "then" "true" "u" "undef"
             "unless" "until" "url_encode" "when" "while" "yield"
             )))
  "ERB keywords.")

(defvar web-mode-asp-keywords
  (regexp-opt
   (append web-mode-extra-asp-keywords
           '("If" "Then" "Each" "End" "Set" "Dim" "On" "For" "Next" "Rem" "Empty"
             "IsArray" "Erase" "LBound" "UBound" "Let" "Next" "Nothing" "Null" "In"
             "True" "False" "Do" "Loop" "Each" "Select" "Case"
             "While" "Wend" "Err")))
  "ASP keywords.")

(defvar web-mode-asp-types
  (regexp-opt
   (append web-mode-extra-asp-types
           '("Application" "ASPError" "Request" "Response" "Server" "Session")))
  "ASP types.")

(defvar web-mode-aspx-keywords
  (regexp-opt
   (append web-mode-extra-aspx-keywords
           '("case" "catch" "do" "else" "end"
             "for" "foreach" "function"
             "if" "in" "include"
             "new" "package" "page" "return"
             "tag" "throw" "throws" "try" "while")))
  "ASP.Net keywords.")

(defvar web-mode-smarty-keywords
  (regexp-opt
   '("as"))
  "Smarty keywords.")

(defvar web-mode-velocity-keywords
  (eval-when-compile
    (regexp-opt
     '("in")))
  "Velocity keywords.")

(defvar web-mode-freemarker-keywords
  (eval-when-compile
    (regexp-opt
     '("as" "list"))))

(defvar web-mode-go-keywords
  (eval-when-compile
    (regexp-opt
     '("define" "else" "end" "if" "pipeline" "range" "template" "with"))))

(defvar web-mode-go-functions
  (eval-when-compile
    (regexp-opt
     '("and" "call" "html" "index" "js" "len" "not" "or"
       "print" "printf" "println" "urlquery")))
  "Go functions.")

(defvar web-mode-django-filters
  (eval-when-compile
    (regexp-opt
     '("add" "addslashes" "capfirst" "center" "cut"
       "date" "default" "default_if_none" "dictsort"
       "dictsortreversed" "divisibleby"
       "escape" "escapejs" "filesizeformat" "first"
       "fix_ampersands" "floatformat"
       "force_escape" "format_integer" "format_number"
       "get_digit" "iriencode" "join"
       "last" "length" "length_is" "linebreaks" "linebreaksbr" "linenumbers"
       "ljust" "lower" "make_list"
       "phonenumeric" "pluralize" "pprint"
       "random" "random_num" "random_range" "removetags" "rjust"
       "safe" "safeseq" "slice" "slugify" "stringformat" "striptags"
       "time" "timesince" "timeuntil" "title" "truncatechars" "truncatewords"
       "truncatewords_html" "unordered_list" "upper" "urlencode"
       "urlize" "urlizetrunc"
       "wordcount" "wordwrap" "yesno")))
  "Django filters.")

(defvar web-mode-closure-keywords
  (eval-when-compile
    (regexp-opt
     '("in" "and" "not" "or")
     ))
  "Closure keywords")

(defvar web-mode-django-keywords
  (eval-when-compile
    (regexp-opt
     '("and" "as" "autoescape" "block" "blocktrans" "break"
       "cache" "call" "comment" "context" "continue" "csrf_token" "cycle"
       "debug" "do"
       "embed" "empty" "else" "elseif" "elif" "endautoescape" "endblock"
       "endcache" "endcall" "endembed" "endfilter" "endfor" "endif"
       "endifchanged" "endifequal" "endifnotequal" "endmacro" "endrandom" "endraw"
       "endsandbox" "endset" "endspaceless" "endtrans" "endwith"
       "extends" "false" "filter" "firstof" "flush" "for" "from"
       "if" "ifchanged" "ifequal" "ifnotequal" "ignore" "import"
       "in" "include" "is"
       "load" "macro" "missing" "none" "not" "now" "or" "pluralize"
       "random" "raw" "regroup" "trans" "true"
       "sandbox" "set" "spaceless" "ssi" "static" "templatetag" "trans"
       "use" "url" "var" "verbatim" "widthratio" "with")))
  "Django keywords.")

(defvar web-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib"
       "Assembly" "Control" "Implements" "Import"
       "Master" "OutputCache" "Page" "Reference" "Register")))
  "Directives.")

(defvar web-mode-javascript-keywords
  (regexp-opt
   (append web-mode-extra-javascript-keywords
           '("arguments" "break" "case" "catch" "class" "const" "continue"
             "debugger" "default" "delete" "do" "else" "enum" "eval"
             "export" "extends" "false" "finally" "for" "function" "if"
             "implements" "import" "in" "instanceof" "interface" "let"
             "new" "null" "package" "private" "protected" "public"
             "return" "static" "super" "switch" "this" "throw"
             "true" "try" "typeof" "undefined" "var" "void" "while" "with" "yield"
             )))
  "JavaScript keywords.")

(defvar web-mode-razor-keywords
  (regexp-opt
   (append web-mode-extra-razor-keywords
           '("false" "true" "foreach" "if" "in" "var" "for" "display")))
  "Razor keywords.")

(defvar web-mode-django-expr-font-lock-keywords
  (list
   '("{{\\|}}" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-django-filters "\\)\\>") '(1 'web-mode-function-name-face))
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   )
  "Font lock keywords for dtl expr")

(defvar web-mode-dust-font-lock-keywords
  (list
   '("/?}\\|{[#/:?@><+^]?" 0 'web-mode-preprocessor-face)
   '("{[#:/?@><+^]\\([[:alpha:]_]+\\)" 1 'web-mode-block-control-face)
   '(":\\([[:alpha:]]+\\)" 1 'web-mode-keyword-face)
   '("\\<\\([[:alpha:]_]+=\\)\\(\"[^\"]*\"\\|[[:alnum:]_]*\\)"
     (1 'web-mode-block-attr-name-face)
     (2 'web-mode-block-attr-value-face))
   '("\\\([[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-smarty-font-lock-keywords
  (list
   '("}\\|{/?" 0 'web-mode-preprocessor-face)
   (cons (concat "[ ]\\(" web-mode-smarty-keywords "\\)[ ]") '(1 'web-mode-keyword-face))
   '("{/?\\([[:alpha:]_]+\\)" 1 'web-mode-block-control-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]+\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   '(" \\(\\sw+[ ]?=\\)" 1 'web-mode-param-name-face)
   '(" \\(\\sw+\\)[ }]" 1 'web-mode-param-name-face)
   '("|\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\(->\\)\\(\\sw+\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("[.]\\([[:alnum:]_-]+\\)[ ]?(" (1 'web-mode-function-name-face))
   '("[.]\\([[:alnum:]_]+\\)" (1 'web-mode-variable-name-face))
   '("#\\([[:alnum:]_]+\\)#" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-velocity-font-lock-keywords
  (list
   '("\\([#]\\)\\([[:alpha:]]+\\)\\>"
     (1 'web-mode-preprocessor-face)
     (2 'web-mode-block-control-face))
   (cons (concat "[ ]\\(" web-mode-velocity-keywords "\\)[ ]") '(1 'web-mode-keyword-face t t))
   '("#macro([ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-function-name-face)
   '("[.]\\([[:alnum:]_-]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\($[!]?[{]?\\)\\([[:alnum:]_-]+\\)[}]?" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-django-code-font-lock-keywords
  (list
   '("{%\\|%}" 0 'web-mode-preprocessor-face)
   (cons (concat "[% ]\\(" web-mode-django-keywords "\\)[ %]") '(1 'web-mode-keyword-face t t))
   (cons (concat "\\<\\(" web-mode-django-filters "\\)\\>") '(1 'web-mode-function-name-face t t))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-ctemplate-font-lock-keywords
  (list
   '("${{\\|{{[>#/{%^&]?\\|[}]?}}" 0 'web-mode-preprocessor-face)
   '("{{[#/>][ ]*\\([[:alnum:]_]+\\)" 1 'web-mode-block-control-face)
   '("[[:alnum:]_]" 0 'web-mode-variable-name-face)
   '("[ ]+\\([[:alnum:]_]+=\\)" 1 'web-mode-param-name-face t t)
   '("[:=]\\([[:alpha:]_]+\\)" 1 'web-mode-function-name-face t t)
   ))

(defvar web-mode-razor-font-lock-keywords
  (list
   '("@" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-razor-keywords "\\)\\>") '(1 'web-mode-keyword-face t t))
   '("\\<\\([[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("<\\([[:alnum:]_]+\\)>" 1 'web-mode-type-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-closure-font-lock-keywords
  (list
   '("{/?\\|/?}" 0 'web-mode-preprocessor-face)
   '("{/?\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   '("{param[ ]+\\([[:alnum:]]+\\)" 1 'web-mode-symbol-face)
   '("\\<\\(true\\|false\\|null\\)\\>" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-closure-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   '("{\\(alias\\|call\\|delcall\\|delpackage\\|deltemplate\\|namespace\\|template\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-constant-face)
   '("\\(allowemptydefault\\|data\\|desc\\|meaning\\|autoescape\\|private\\|variant\\)=" 0 'web-mode-block-attr-name-face)
   '("|\\([[:alpha:]]+\\)" 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:]]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("$\\([[:alnum:]._]+\\)" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-go-font-lock-keywords
  (list
   '("{{\\|}}" 0 'web-mode-preprocessor-face)
   '("{{\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   (cons (concat "\\<\\(" web-mode-go-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-go-functions "\\)\\>") '(1 'web-mode-function-name-face))
   '("[$.]\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face t t)
   ))

(defvar web-mode-expression-font-lock-keywords
  (list
   '("<%\\$\\|%>" 0 'web-mode-preprocessor-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

;; css rule = selector + declaration
(defvar web-mode-selector-font-lock-keywords
  (list
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") '(1 'web-mode-css-at-rule-face))
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>") '(1 'web-mode-css-pseudo-class-face))
   '("[[:alnum:]-]+" 0 'web-mode-css-selector-face)
   ))

(defvar web-mode-declaration-font-lock-keywords
  (list
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") '(1 'web-mode-css-at-rule-face))
   '("\\([[:alpha:]-]\\{3,\\}\\)[ ]?:" 1 'web-mode-css-property-name-face)
   '("\\([[:alpha:]-]+\\)[ ]?(" 1 'web-mode-css-function-face)
   '("#[[:alnum:]]\\{3,6\\}" 0 'web-mode-css-color-face t t)
   '("![ ]?important" 0 'web-mode-css-priority-face t t)
   ))

(defvar web-mode-html-font-lock-keywords
  (list
   '("</?[[:alnum:]]+\\|>" 0 'web-mode-html-tag-face)
   '(" \\([[:alnum:]-]+=\\)\\(\"[^\"]+\"\\)"
     (1 'web-mode-html-attr-name-face)
     (2 'web-mode-html-attr-value-face))
   ))

(defvar web-mode-javascript-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-javascript-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<new \\([[:alnum:]_.]+\\)\\>" 1 'web-mode-type-face)
   '("\\<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'web-mode-function-name-face)
   '("function[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\([[:alnum:]]+\\):" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-asp-font-lock-keywords
  (list
   '("<%=?\\|%>" 0 'web-mode-preprocessor-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-asp-types "\\)\\>") '(0 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-asp-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   ))

(defvar web-mode-aspx-font-lock-keywords
  (list
   '("<%[:=#]?\\|%>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-aspx-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   ))

;;Unified Expression Language
(defvar web-mode-uel-font-lock-keywords
  (list
   '("[$#{]{\\|}" 0 'web-mode-preprocessor-face)
   '("\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-freemarker-square-font-lock-keywords
  (list
   '("\\[/?[#@]\\|/?>\\|/?\\]" 0 'web-mode-preprocessor-face)
   '("\\[/?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-freemarker-font-lock-keywords
  (list
   '("</?[#@]\\|/?>\\|/?>" 0 'web-mode-preprocessor-face)
   '("</?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

;;TODO : definir web-mode-block-attr-name-face et web-mode-block-attr-name-face
(defvar web-mode-jsp-tag-font-lock-keywords
  (list
   '("</?\\|/?>" 0 'web-mode-preprocessor-face)
   '("</?\\([[:alpha:]]+:[[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-jsp-font-lock-keywords
  (list
   '("-?%>\\|<%\\(!\\|=\\|#=\\)?" 0 'web-mode-preprocessor-face)
   '("\\(throws\\|new\\|extends\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
   ))

(defvar web-mode-directive-font-lock-keywords
  (list
   '("<%@\\|%>" 0 'web-mode-preprocessor-face)
   '("<%@[ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-erb-font-lock-keywords
  (list
   '("-?%>\\|^%\\|<%[=-]?" 0 'web-mode-preprocessor-face)
   '(":\\([[:alnum:]_]+\\)" 1 'web-mode-symbol-face)
   '("\\([[:alnum:]_]+\\):[ ]+" 1 'web-mode-symbol-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-erb-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face)
   '("class[ ]+\\(\\sw*\\)" 1 'web-mode-type-face)
   '("def[ ]+\\(\\sw*\\)" 1 'web-mode-function-name-face)
   '("[[:alpha:]][[:alnum:]_]*" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-python-font-lock-keywords
  (list
   '("<\\?\\|\\?>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   ))

(defvar web-mode-php-font-lock-keywords
  (list
   '("<\\?\\(php\\|=\\)?\\|\\?>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   (cons (concat "(\\<\\(" web-mode-php-types "\\)\\>") '(1 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") '(0 'web-mode-constant-face))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-constant-face)
   '("->[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\<\\(instanceof\\|class\\|extends\\|new\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-type-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-blade-font-lock-keywords
  (append
   (list
    '("{{\\|}}" 0 'web-mode-preprocessor-face)
    '("\\(@\\)\\([[:alpha:]_]+\\)"
      (1 'web-mode-preprocessor-face)
      (2 'web-mode-block-control-face)))
   web-mode-php-font-lock-keywords))

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in web-mode buffers.")

(defvar web-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [menu-bar wm] (cons "Web-Mode" (make-sparse-keymap)))
    (define-key map [menu-bar wm blk] (cons "Block" (make-sparse-keymap)))
    (define-key map [menu-bar wm tag] (cons "Html Tag" (make-sparse-keymap)))
    (define-key map [menu-bar wm elt] (cons "Html Element" (make-sparse-keymap)))

    (define-key map [menu-bar wm sep-1] '(menu-item "--"))
    (define-key map [menu-bar wm blk blk-next] '(menu-item "Next" web-mode-block-next))
    (define-key map [menu-bar wm blk blk-prev] '(menu-item "Previous" web-mode-block-previous))
    (define-key map [menu-bar wm blk blk-end] '(menu-item "End" web-mode-block-beginning))
    (define-key map [menu-bar wm blk blk-beg] '(menu-item "Beginning" web-mode-block-beginning))
    (define-key map [menu-bar wm tag tag-sel] '(menu-item "Select" web-mode-tag-select))
    (define-key map [menu-bar wm tag tag-match] '(menu-item "Match" web-mode-tag-match))
    (define-key map [menu-bar wm tag tag-next] '(menu-item "Next" web-mode-tag-next))
    (define-key map [menu-bar wm tag tag-prev] '(menu-item "Previous" web-mode-tag-previous))
    (define-key map [menu-bar wm tag tag-end] '(menu-item "End" web-mode-tag-beginning))
    (define-key map [menu-bar wm tag tag-beg] '(menu-item "Beginning" web-mode-tag-beginning))
    (define-key map [menu-bar wm elt elt-in] '(menu-item "Inner Content" web-mode-element-content-select))
    (define-key map [menu-bar wm elt elt-parent] '(menu-item "Parent" web-mode-element-parent))
    (define-key map [menu-bar wm elt elt-sel] '(menu-item "Select" web-mode-element-select))
    (define-key map [menu-bar wm elt elt-ren] '(menu-item "Rename" web-mode-element-rename))
    (define-key map [menu-bar wm elt elt-dup] '(menu-item "Clone" web-mode-element-clone))
    (define-key map [menu-bar wm elt elt-close] '(menu-item "Close" web-mode-element-close))
    (define-key map [menu-bar wm elt elt-trav] '(menu-item "Traverse DOM" web-mode-element-traverse))
    (define-key map [menu-bar wm elt elt-child] '(menu-item "Child" web-mode-element-child))
    (define-key map [menu-bar wm elt elt-del] '(menu-item "Delete" web-mode-element-delete))
    (define-key map [menu-bar wm elt elt-next] '(menu-item "Next" web-mode-element-next))
    (define-key map [menu-bar wm elt elt-prev] '(menu-item "Previous" web-mode-element-previous))
    (define-key map [menu-bar wm elt elt-end] '(menu-item "End" web-mode-element-end))
    (define-key map [menu-bar wm elt elt-beg] '(menu-item "Beginning" web-mode-element-beginning))
    (define-key map [menu-bar wm err] '(menu-item "Show error(s)" web-mode-errors-show))
    (define-key map [menu-bar wm fold] '(menu-item "Fold/Unfold" web-mode-fold-or-unfold))
    (define-key map [menu-bar wm indent] '(menu-item "Indent buffer" web-mode-buffer-indent))
    (define-key map [menu-bar wm nav] '(menu-item "Tag/Block navigation" web-mode-tag-match))
    (define-key map [menu-bar wm expand] '(menu-item "Mark and Expand" web-mode-mark-and-expand))
    (define-key map [menu-bar wm space] '(menu-item "Toggle whitespaces" web-mode-whitespaces-show))
    (define-key map [menu-bar wm xpath] '(menu-item "XPath" web-mode-xpath))
    (define-key map [menu-bar wm snippet] '(menu-item "Insert snippet" web-mode-snippet-insert))
    (define-key map [menu-bar wm entities] '(menu-item "Replace HTML entities" web-mode-entities-replace))

    (define-key map (kbd "C-;")       'web-mode-comment-or-uncomment)
    (define-key map (kbd "M-;")       'web-mode-comment-or-uncomment)

    (define-key map (kbd "C-c C-d")   'web-mode-errors-show)
    (define-key map (kbd "C-c C-f")   'web-mode-fold-or-unfold)
    (define-key map (kbd "C-c C-i")   'web-mode-buffer-indent)
    (define-key map (kbd "C-c C-m")   'web-mode-mark-and-expand)
    (define-key map (kbd "C-c C-n")   'web-mode-tag-match)
    (define-key map (kbd "C-c C-r")   'web-mode-entities-replace)
    (define-key map (kbd "C-c C-s")   'web-mode-snippet-insert)
    (define-key map (kbd "C-c C-x")   'web-mode-xpath)
    (define-key map (kbd "C-c C-w")   'web-mode-whitespaces-show)

    (define-key map (kbd "C-c /")     'web-mode-element-close)
    (define-key map (kbd "C-c <")     'web-mode-element-beginning)
    (define-key map (kbd "C-c >")     'web-mode-element-end)

    (define-key map (kbd "C-c C-b b") 'web-mode-block-beginning)
    (define-key map (kbd "C-c C-b e") 'web-mode-block-end)
    (define-key map (kbd "C-c C-b n") 'web-mode-block-next)
    (define-key map (kbd "C-c C-b p") 'web-mode-block-previous)

    (define-key map (kbd "C-c C-e b") 'web-mode-element-beginning)
    (define-key map (kbd "C-c C-e c") 'web-mode-element-clone)
    (define-key map (kbd "C-c C-e d") 'web-mode-element-child)
    (define-key map (kbd "C-c C-e e") 'web-mode-element-end)
    (define-key map (kbd "C-c C-e i") 'web-mode-element-content-select)
    (define-key map (kbd "C-c C-e k") 'web-mode-element-delete)
    (define-key map (kbd "C-c C-e n") 'web-mode-element-next)
    (define-key map (kbd "C-c C-e p") 'web-mode-element-previous)
    (define-key map (kbd "C-c C-e r") 'web-mode-element-rename)
    (define-key map (kbd "C-c C-e s") 'web-mode-element-select)
    (define-key map (kbd "C-c C-e t") 'web-mode-element-traverse)
    (define-key map (kbd "C-c C-e u") 'web-mode-element-parent)

    (define-key map (kbd "C-c C-t b") 'web-mode-tag-beginning)
    (define-key map (kbd "C-c C-t e") 'web-mode-tag-end)
    (define-key map (kbd "C-c C-t m") 'web-mode-tag-match)
    (define-key map (kbd "C-c C-t n") 'web-mode-tag-next)
    (define-key map (kbd "C-c C-t p") 'web-mode-tag-previous)
    (define-key map (kbd "C-c C-t s") 'web-mode-tag-select)

    ;; compatibility with nxml
    (define-key map (kbd "M-C-u")     'web-mode-element-parent)
    (define-key map (kbd "M-C-d")     'web-mode-element-child)
    (define-key map (kbd "M-C-n")     'web-mode-element-next)
    (define-key map (kbd "M-C-p")     'web-mode-element-previous)

    map)
  "Keymap for `web-mode'.")

;;--- compatibility

(eval-and-compile

  (defalias 'web-mode-prog-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

  (if (fboundp 'with-silent-modifications)
      (defalias 'web-mode-with-silent-modifications 'with-silent-modifications)
    (defmacro web-mode-with-silent-modifications (&rest body)
      "For compatibility with Emacs pre 23.3"
      `(let ((old-modified-p (buffer-modified-p))
             (inhibit-modification-hooks t)
             (buffer-undo-list t))
         (unwind-protect
             ,@body
           (set-buffer-modified-p old-modified-p)))))

  );eval-and-compile

;;;###autoload
(define-derived-mode web-mode web-mode-prog-mode "Web"
  "Major mode for editing web templates."

  (make-local-variable 'after-change-functions)
  (make-local-variable 'font-lock-fontify-buffer-function)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'font-lock-unfontify-buffer-function)
  (make-local-variable 'forward-sexp-function)
  (make-local-variable 'imenu-case-fold-search)
  (make-local-variable 'imenu-create-index-function)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'require-final-newline)

  (make-local-variable 'web-mode-buffer-highlighted)
  (make-local-variable 'web-mode-comment-style)
  (make-local-variable 'web-mode-content-type)
  (make-local-variable 'web-mode-display-table)
  (make-local-variable 'web-mode-engine)
  (make-local-variable 'web-mode-block-regexps)
  (make-local-variable 'web-mode-engine-file-regexps)
  (make-local-variable 'web-mode-expand-initial-pos)
  (make-local-variable 'web-mode-expand-previous-state)
  (make-local-variable 'web-mode-hl-line-mode-flag)
  (make-local-variable 'web-mode-indent-style)
  (make-local-variable 'web-mode-is-narrowed)
  (make-local-variable 'web-mode-block-regexp)
  (make-local-variable 'web-mode-time)

  (if (and (fboundp 'global-hl-line-mode)
           global-hl-line-mode)
      (setq web-mode-hl-line-mode-flag t))

  (setq fill-paragraph-function 'web-mode-fill-paragraph
        font-lock-fontify-buffer-function 'web-mode-scan-buffer
        forward-sexp-function 'web-mode-forward-sexp
        ;;          font-lock-keywords-only t
        font-lock-unfontify-buffer-function 'web-mode-scan-buffer
        imenu-case-fold-search t
        imenu-create-index-function 'web-mode-imenu-index
        indent-line-function 'web-mode-indent-line
        indent-tabs-mode nil
        require-final-newline nil)

  (remove-hook 'after-change-functions 'font-lock-after-change-function t)

  (add-hook 'after-change-functions 'web-mode-on-after-change t t)

  (add-hook 'after-save-hook
            '(lambda ()
               (when web-mode-is-scratch
                 (web-mode-guess-engine-and-content-type)
                 (web-mode-scan-buffer)
;;                 (message "-->%S" (buffer-file-name))
                 )
               nil)
            t t)

  (when (boundp 'yas-after-exit-snippet-hook)
    (add-hook 'yas-after-exit-snippet-hook
              '(lambda () (web-mode-buffer-refresh))
              t t)
    )

  (when web-mode-enable-whitespaces
    (web-mode-whitespaces-on))

  (web-mode-guess-engine-and-content-type)
  (web-mode-scan-buffer)

  )

(defun web-mode-forward-sexp (&optional arg)
  "Move forward."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((> arg 0)
    (while
        (progn
          (web-mode-tag-next)
          (> (setq arg (1- arg)) 0))))
   ((< arg 0)
    (while
        (progn
          (web-mode-tag-previous)
          (< (setq arg (1+ arg)) 0))))
   ))

(defun web-mode-guess-engine-and-content-type ()
  "Try to guess the server engine and the content type."
  (let (buff-name elt found)

    (setq buff-name (buffer-file-name))
    (unless buff-name (setq buff-name (buffer-name)))

    (setq web-mode-is-scratch (string= buff-name "*scratch*"))

    (setq web-mode-content-type nil)

    (when (boundp 'web-mode-content-types-alist)
      (setq found nil)
      (dolist (elt web-mode-content-types-alist)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-content-type (car elt)
                found t))
        )
      )

    (unless web-mode-content-type
      (setq found nil)
      (dolist (elt web-mode-content-types)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-content-type (car elt)
                found t))
        )
      )

    (when (boundp 'web-mode-engines-alist)
      (setq found nil)
      (dolist (elt web-mode-engines-alist)
        (when (string-match-p (cdr elt) buff-name)
          (setq web-mode-engine (car elt)))
        )
      )

    (unless web-mode-engine
      (setq found nil)
      (dolist (elt web-mode-engine-file-regexps)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-engine (car elt)
                found t))
        )
      )

    (when web-mode-engine
      (setq found nil)
      (dolist (elt web-mode-engine-families)
        (when (and (not found) (member web-mode-engine (cdr elt)))
          (setq web-mode-engine (car elt)
                found t))
        )
      )

    (setq elt (assoc web-mode-engine web-mode-block-regexps))
    (if elt
        (setq web-mode-block-regexp (cdr elt))
      (setq web-mode-engine "none")
      )

;;    (message "buffer=%S engine=%S type=%S regexp=%S"
;;             buff-name web-mode-engine web-mode-content-type web-mode-block-regexp)

    (when (string= web-mode-engine "razor")
      (setq web-mode-enable-block-face t))

    (when (and (string= web-mode-content-type "html")
               (not (string= web-mode-engine "none")))
      (setq web-mode-active-block-regexp
            (cdr (assoc web-mode-engine web-mode-active-block-regexps)))
      (setq web-mode-close-block-regexp
            (cdr (assoc web-mode-engine web-mode-close-block-regexps)))
      (setq web-mode-engine-control-matcher
            (intern-soft (concat "web-mode-match-" web-mode-engine "-block")))
;;      (message "%S\n%S\n%S" web-mode-active-block-regexp web-mode-close-block-regexp web-mode-engine-control-matcher)
      )

    ))

(defun web-mode-imenu-index ()
  "Return a table of contents."
  (let (toc-index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<h\\([1-9]\\)\\([^>]*\\)>\\([^<]*\\)" nil t)
	(setq toc-index
	      (cons (cons (concat (make-string
				   (* 2 (1- (string-to-number (match-string 1))))
				   ?\s)
				  (match-string 3))
			  (line-beginning-position))
		    toc-index))))
    (nreverse toc-index)))

(defun web-mode-scan-buffer ()
  "Scan entine buffer."
  (interactive)
  (web-mode-scan-region (point-min) (point-max)))

(defun web-mode-scan-region (beg end &optional verbose)
  "Identify code blocks (client/server) and syntactic symbols (strings/comments)."
  (interactive)
  (web-mode-trace "scanning region")
  ;;  (message "scanning buffer from %d to %d" beg end)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-modification-hooks t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (setq beg (if web-mode-is-narrowed 1 beg))
           (remove-text-properties beg end web-mode-text-properties)
           (cond
            ((member web-mode-content-type '("javascript" "json" "css"))
             (web-mode-scan-part beg end web-mode-content-type))
            ((string= web-mode-engine "none")
             (web-mode-scan-parts beg end)
             (web-mode-trace "parts scanned")
             )
            (t
             (web-mode-mark-blocks beg end)
             (web-mode-trace "blocks marked")
             (web-mode-scan-parts beg end)
             (web-mode-trace "parts scanned")
             (web-mode-scan-blocks beg end)
             (web-mode-trace "blocks scanned")
             )
            );cond
           (if web-mode-enable-whitespaces
               (web-mode-scan-whitespaces beg end))
           ))))))

(defun web-mode-mark-blocks (beg end)
  "Identifies blocks (with block-side, block-beg, block-end text properties)."
  (save-excursion

    (let ((i 0)
          open close closing-string start sub1 sub2 sub3 pos tagopen l tmp)

      (goto-char beg)

      ;;      (message "%S: %Sx%S" (point) beg end)
      ;;      (message "regexp=%S" web-mode-block-regexp)
      (while (and (< i 500)
                  (> end (point))
                  (re-search-forward web-mode-block-regexp end t))

        (setq i (1+ i)
              closing-string nil
              close nil
              tagopen (match-string 0)
              open (match-beginning 0)
              pos nil)

        (when (member (string-to-char tagopen) '(?\s ?\t))
          (setq l (length tagopen))
          (setq tagopen (replace-regexp-in-string "\\`[ \t]*" "" tagopen))
          (setq open (+ open (- l (length tagopen))))
          )

        (setq sub1 (substring tagopen 0 1))
        (setq sub2 (substring tagopen 0 2))

        (cond

         ((string= web-mode-engine "php")
          (unless (looking-at-p "xml ")
            (setq closing-string '("<\\?". "\\?>"))
            )
          );php

         ((string= web-mode-engine "django")
          (cond
           ((string= sub2 "{{")
            (setq closing-string "}}"))
           ((string= sub2 "{%")
            (setq closing-string "%}"))
           (t
            (setq closing-string "#}"))
           )
          );django

         ((string= web-mode-engine "ctemplate")
          (setq closing-string "}}")
          );ctemplate

         ((or (string= web-mode-engine "asp")
              (string= web-mode-engine "aspx"))
          (setq closing-string "%>")
          );asp

         ((string= web-mode-engine "blade")
          (cond
           ((string= sub2 "{{")
            (setq closing-string "}}"))
           ((string= sub1 "@")
            (setq closing-string "EOL"))
           )
          );blade

         ((string= web-mode-engine "smarty")
          (cond
           ((string= sub2 "{*")
            (setq closing-string "*}"))
           ((string= sub2 "{#")
            (setq closing-string "#}"))
           (t
            (setq closing-string "}"))
           )
          );smarty

         ((string= web-mode-engine "dust")
          (cond
           ((string= sub2 "{!")
            (setq closing-string "!}"))
           (t
            (setq closing-string "}")
            )
           )
          );dust

         ((string= web-mode-engine "closure")
          (cond
           ((string= sub2 "//")
            (setq closing-string "EOL")
            )
           ((string= sub2 "/*")
            (setq closing-string "*/")
            )
           (t
            (setq closing-string "}")
            )
           )
          );closure

         ((string= web-mode-engine "ctemplate")
          (cond
           ((string= sub3 "{{{")
            (setq closing-string "}}}"))
           (t
            (setq closing-string "}}"))
           )
          );ctemplate

         ((string= web-mode-engine "go")
          (setq closing-string "}}")
          );go

         ((string= web-mode-engine "erb")
          (cond
           ((string= sub2 "<%")
            (setq closing-string "%>"))
           (t
            (setq closing-string "EOL"))
           )
          );erb

         ((string= web-mode-engine "jsp")
          (cond
           ((string= sub2 "<%")
            (setq closing-string "%>"))
           ((string= sub2 "${")
            (setq closing-string "}"))
           (t
            (setq closing-string ">"))
           )
          );jsp

         ((string= web-mode-engine "freemarker")
          (cond
           ((string= sub1 "<")
            (setq closing-string ">"))
           ((string= sub1 "[")
            (setq closing-string "]"))
           (t
            (setq closing-string "}"))
           )
          );freemarker

         ((string= web-mode-engine "velocity")
          (cond
           ((string= sub2 "##")
            (setq closing-string "EOL"))
           ((string= sub2 "#*")
            (setq closing-string "*#"))
           (t
            (setq closing-string "EOV"))
           )
          );velocity

         ((string= web-mode-engine "razor")
          (cond
           ((string= sub2 "@@")
            (forward-char 2)
            (setq closing-string nil))
           ((string= sub2 "@*")
            (setq closing-string "*@"))
           ((string= sub1 "@")
            (setq closing-string "EOR"))
           ((string= sub1 "}")
            (setq closing-string "EOR"))
           )
          );razor

         ((string= web-mode-engine "python")
          (unless (looking-at-p "xml ")
            (setq closing-string "?>"))
          );python

         );cond

        (when closing-string

          (cond

           ((listp closing-string)
            (if (web-mode-rsf-balanced (car closing-string) (cdr closing-string) end t)
                (progn
                  (setq close (match-end 0)
                        pos (point))
                  )
              (when (string= "<?" sub2)
                (setq close (point-max)
                      pos (point-max)))
              )
            )

           ((and (member web-mode-engine '("closure" "dust" "smarty"))
                 (string= closing-string "}"))
            (goto-char open)
            (setq tmp (web-mode-closing-paren-position (point) (line-end-position)))
            (if tmp
                (setq tmp (1+ tmp))
              (setq tmp (line-end-position)))
            (goto-char tmp)
            (setq close (point)
                  pos (point))
            )

           ((string= closing-string "EOL")
            (end-of-line)
            (setq close (point)
                  pos (point)))

           ((string= closing-string "EOR")
            (web-mode-razor-skip-forward open)
            (setq close (point)
                  pos (point)))

           ((string= closing-string "EOV")
            (web-mode-velocity-skip-forward open)
            (setq close (point)
                  pos (point)))

           ((search-forward closing-string end t)
            (setq close (match-end 0)
                  pos (point)))

           );cond

          (when (and close (>= end pos))
            ;;            (message "pos(%S) : open(%S) close(%S)" pos open close)
            (add-text-properties open close '(block-side t))
            (put-text-property open (1+ open) 'block-beg t)
            (put-text-property (1- close) close 'block-end t)
            )

          (if pos (goto-char pos))

          );when closing-string

        );while

      (when (>= i 500)
        (message "** strange loop (web-mode-mark-blocks) **"))

      )))

(defun web-mode-scan-blocks (region-beg region-end)
  "Fontify blocks. The scan relies on the 'block-beg text property."
  (let ((i 0)
        (beg region-beg)
        (end nil)
        (continue t))
    (while continue
      (setq end nil
            i (1+ i))
      (unless (get-text-property beg 'block-beg)
        (setq beg (web-mode-block-next-position beg)))
      (when (and beg (< beg region-end))
        (setq end (web-mode-block-end-position beg)))
      (cond
       ((or (null end)
            (> end region-end)
            (> i 200))
        (setq continue nil)
        (if (> i 200) (message "*** invalid loop (web-mode-scan-blocks) ***")))
       (t
        (setq end (1+ end))
        ;;(message "beg=%S end=%S" beg end)
        (web-mode-scan-block beg end)
        (when (and (member web-mode-engine '("jsp"))
                   (> (- end beg) 12)
                   (char-equal ?\< (char-after beg)))
          (web-mode-scan-jsp-tag beg end))
        (setq beg end)
        )
       );cond
      );while
    ))

(defun web-mode-scan-block (beg end)
  "Fontify a block."
  (let (sub1 sub2 sub3 regexp props start ms continue fc keywords tag token-type hddeb hdend hdflk)

    (goto-char beg)

    (setq sub1 (buffer-substring-no-properties beg (+ beg 1))
          sub2 (buffer-substring-no-properties beg (+ beg 2)))
    (setq sub3 sub2)
    (if (>= (point-max) (+ beg 3))
        (setq sub3 (buffer-substring-no-properties beg (+ beg 3))))

    (cond

     ((string= web-mode-engine "php")
      (setq regexp "//\\|/\\*\\|\"\\|'\\|<<<['\"]?\\([[:alnum:]]+\\)['\"]?"
            props '(face nil)
            keywords web-mode-php-font-lock-keywords)
      );php

     ((string= web-mode-engine "django")
      (cond
       ((string= sub2 "{{")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-django-expr-font-lock-keywords)
        )
       ((string= sub2 "{%")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-django-code-font-lock-keywords)
        )
       ((string= sub2 "{#")
        (setq props '(block-token comment face web-mode-comment-face))
        )
       )
      );django

     ((string= web-mode-engine "ctemplate")
      (cond
       ((string= sub3 "{{!")
        (setq props '(block-token comment face web-mode-comment-face)))
       ((string= sub3 "{{%")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-ctemplate-font-lock-keywords))
       (t
        (setq props '(face nil)
              keywords web-mode-ctemplate-font-lock-keywords))
       )
      );ctemplate

     ((string= web-mode-engine "go")
      (cond
       ((string= sub3 "{{/")
        (setq props '(block-token comment face web-mode-comment-face)))
       ((string= sub2 "{{")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-go-font-lock-keywords))
       )
      );go

     ((string= web-mode-engine "razor")
      (cond
       ((string= sub2 "@*")
        (setq props '(block-token comment face web-mode-comment-face)))
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-razor-font-lock-keywords))
       )
      );razor

     ((string= web-mode-engine "python")
      (setq regexp "\"\\|'\\|#"
            props '(face nil)
            keywords web-mode-python-font-lock-keywords)
      );python

     ((string= web-mode-engine "blade")
      (cond
       ((string= sub3 "{{-")
        (setq props '(block-token comment face web-mode-comment-face)))
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-blade-font-lock-keywords))
       )
      );blade

     ((string= web-mode-engine "velocity")
      (cond
       ((member sub2 '("##" "#*"))
        (setq props '(block-token comment face web-mode-comment-face)))
       ((string= sub1 "$")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-velocity-font-lock-keywords))
       ((string= sub1 "#")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-velocity-font-lock-keywords))
       )
      );velocity

     ((string= web-mode-engine "jsp")
      (cond
       ((string= sub3 "<%-")
        (setq props '(block-token comment face web-mode-comment-face)))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"
              props '(face nil)
              keywords web-mode-directive-font-lock-keywords))
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-uel-font-lock-keywords))
       ((string= sub2 "<%")
        (setq regexp "//\\|/\\*\\|\"\\|'"
              props '(face nil)
              keywords web-mode-jsp-font-lock-keywords))
       (t
        (setq props '(face nil)
              keywords web-mode-jsp-tag-font-lock-keywords)
        )
       )
      );jsp

     ((string= web-mode-engine "freemarker")
      (cond
       ((member sub3 '("<#-" "[#-"))
        (setq props '(block-token comment face web-mode-comment-face))
        )
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-uel-font-lock-keywords))
       ((or (member sub2 '("<@" "[@" "<#" "[#"))
            (member sub3 '("</@" "[/@" "</#" "[/#")))
        (setq regexp "\""
              props '(face nil)
              keywords (if (char-equal ?\[ (aref sub2 0))
                           web-mode-freemarker-square-font-lock-keywords
                         web-mode-freemarker-font-lock-keywords))
        )
       (t
        (setq props '(face nil)
              keywords web-mode-jsp-tag-font-lock-keywords)
        )
       )
      );freemarker

     ((string= web-mode-engine "erb")
      (cond
       ((string= sub3 "<%#")
        (setq props '(block-token comment face web-mode-comment-face)))
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-erb-font-lock-keywords)
        )
       )
      );erb

     ((string= web-mode-engine "asp")
      (setq regexp "//\\|/\\*\\|\"\\|'"
            props '(face nil)
            keywords web-mode-asp-font-lock-keywords)
      );asp

     ((string= web-mode-engine "aspx")
      (cond
       ((string= sub3 "<%-")
        (setq props '(block-token comment face web-mode-comment-face)))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"
              props '(face nil)
              keywords web-mode-directive-font-lock-keywords))
       ((string= sub3 "<%$")
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-expression-font-lock-keywords))
       (t
        (setq regexp "//\\|/\\*\\|\"\\|'"
              props '(face nil)
              keywords web-mode-aspx-font-lock-keywords)
        )
       )
      );aspx

     ((string= web-mode-engine "smarty")
      (cond
       ((string= sub2 "{*")
        (setq props '(block-token comment face web-mode-comment-face))
        )
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-smarty-font-lock-keywords)
        )
       )
      );smarty

     ((string= web-mode-engine "dust")
      (cond
       ((string= sub2 "{!")
        (setq props '(block-token comment face web-mode-comment-face))
        )
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-dust-font-lock-keywords)
        )
       )
      );dust

     ((string= web-mode-engine "closure")
      (cond
       ((member sub2 '("/*" "//"))
        (setq props '(block-token comment face web-mode-comment-face))
        )
       (t
        (setq regexp "\"\\|'"
              props '(face nil)
              keywords web-mode-closure-font-lock-keywords)
        )
       )
      );closure

     )

    (add-text-properties beg end props)

    (when keywords (web-mode-fontify-region beg end keywords))

    (when regexp
      (setq token-type "string")
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (setq start (match-beginning 0)
              ms (match-string 0)
              continue t)
        (setq fc (substring ms 0 1))
        (cond

         ((and (string= web-mode-engine "asp")
               (string= fc "'"))
          (setq props '(block-token comment face web-mode-block-comment-face)
                token-type "comment")
          (goto-char (if (< end (line-end-position)) end (line-end-position)))
          )

         ((string= fc "'")
          (setq props '(block-token string face web-mode-block-string-face))
          (while (and continue (search-forward "'" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= fc "\"")
          (setq props '(block-token string face web-mode-block-string-face))
          (while (and continue (search-forward "\"" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= ms "//")
          (setq props '(block-token comment face web-mode-block-comment-face)
                token-type "comment")
          (goto-char (if (< end (line-end-position)) end (line-end-position)))
          )

         ((string= ms "/*")
          (setq props '(block-token comment face web-mode-block-comment-face)
                token-type "comment")
          (search-forward "*/" end t)
          )

         ((string= fc "<")
          (when (and web-mode-enable-heredoc-fontification
                     (string-match-p "JS\\|JAVASCRIPT\\|HTM\\|CSS" (match-string 1)))
            (setq hddeb (1+ (point))
                  hdflk (if (string-match-p "HTM" (match-string 1))
                            web-mode-html-font-lock-keywords
                          web-mode-javascript-font-lock-keywords))
            )
          ;;          (message "tag=%s pos=%S" (match-string 1) (point))
          (setq props '(block-token string face web-mode-block-string-face))
          (re-search-forward (concat "^" (match-string 1)) end t)
          (when hddeb (setq hdend (1- (match-beginning 0))))
          )

         ((and (string= web-mode-engine "python") (string= fc "#"))
          (setq props '(block-token comment face web-mode-block-comment-face))
          (goto-char (if (< end (line-end-position)) end (line-end-position)))
          )

         );;cond

        ;;        (message "elt=%S" (buffer-substring start (point)))
        (add-text-properties start (point) props)

        (when hddeb
          (web-mode-fontify-region hddeb hdend hdflk)
          (setq hddeb nil))

        (cond
         ((string= token-type "comment")
          (if web-mode-enable-comment-keywords
              (web-mode-enhance-comment start (point) t))
          )
         (t
          )
         )

        );while

      );when regexp

    (when web-mode-enable-block-face
      (font-lock-prepend-text-property beg end
                                      'face
                                      'web-mode-block-face))
    ))

(defun web-mode-scan-jsp-tag (beg end)
  "Scan a jsp tag to fontify ${ } blocks"
  (save-excursion
    (goto-char (+ 4 beg))
    (setq end (1- end))
    (while (re-search-forward "${.*}" end t)
      (web-mode-fontify-region (match-beginning 0) (match-end 0)
                               web-mode-uel-font-lock-keywords)
      )
    ))

(defun web-mode-enhance-comment (beg end block-side)
  "Enhance comment"
  (save-excursion
    (let (regexp)
      (goto-char beg)
      (setq regexp (concat "\\<\\(" web-mode-comment-keywords "\\)\\>"))
      (while (re-search-forward regexp end t)
        (font-lock-prepend-text-property (match-beginning 1) (match-end 1)
                                         'face
                                         'web-mode-comment-keyword-face)
        )
      )))

;; start-tag, end-tag, tag-name, element (<a>xsx</a>, an element is delimited by tags), void-element
;; http://www.w3.org/TR/html-markup/syntax.html#syntax-elements
;;<#include "toto">
(defun web-mode-scan-parts (beg end)
  "Scan client side blocks (JavaScript / CSS / HTML Comments) and identifies strings and comments."
  (save-excursion
    (let (open limit close ms expr props closing-string start tag tag-name tag-beg tag-end tag-stop element-content-type attrs-end close-found pos markup-face prop-type prop-name)

      (goto-char beg)

      ;; <\\(!--\\|!doctype\\|/?[[:alnum:]]+[:_]?[[:alnum:]]*\\|\?xml\\)
      ;; <\\(/?[[:alnum:]]+\\|!--\\|!doctype\\|\?xml\\)
      ;; <[[:alpha:]/!?]
      (while (web-mode-rsf-client "<\\(/?[[:alnum:]]+\\|!--\\|!doctype\\|\?xml\\)" end t)
        (setq tag-name (downcase (match-string 1))
              tag-beg (match-beginning 0)
              tag-end nil
              element-content-type nil
              tag-stop (point)
              prop-name 'tag-name
              prop-type 'tag-type
              open nil
              limit end
              close nil
              pos nil
              markup-face nil
              props nil
              expr nil
              closing-string nil
              close-found nil)

        (cond
         ((string= tag-name "!--")
          (setq expr "-->"))
         ((string= tag-name "!doctype")
          (setq expr ">"))
         ((string= tag-name "?xml")
          (setq expr "?>"))
         (t
          (setq props '(face web-mode-html-tag-face))
          (cond
           ((char-equal ?\/ (string-to-char tag-name))
            (setq props (plist-put props prop-name (substring tag-name 1)))
            (setq props (plist-put props prop-type 'end))
            (setq expr ">")
            (setq limit (if (> end (line-end-position)) (line-end-position) end))
            )
           ((web-mode-is-void-element tag-name)
            (setq props (plist-put props prop-name tag-name))
            (setq props (plist-put props prop-type 'void))
            (setq expr ">")
            )
           (t
            (setq props (plist-put props prop-name tag-name))
            (setq props (plist-put props prop-type 'start))
            (setq expr ">")
            )
           );cond
          );t
         );cond

        (if (web-mode-sf-client expr limit t)
            (progn
              (setq attrs-end (- (point) (length expr))
                    tag-end (point)
                    close-found t)
              (when (char-equal ?\/ (char-after (- (point) 2)))
                (setq attrs-end (1- attrs-end)
                      props (plist-put props prop-type 'void)))
              );progn
          (setq attrs-end (line-end-position)
                tag-end (line-end-position))
          );if

        (cond
         ((string= tag-name "script")
          (setq tag (buffer-substring-no-properties tag-beg tag-end))
          (cond
           ((string-match-p " type[ ]*=[ ]*[\"']text/\\(x-handlebars\\|html\\)" tag)
            (setq element-content-type "html"))
           ((string-match-p " type[ ]*=[ ]*[\"']application/\\(ld\\+json\\|json\\)" tag)
            (setq element-content-type "json"))
           (t
            (setq element-content-type "javascript"))
           )
;;          (message "tag=%S : %S" tag element-content-type)
          );case script
         ((string= tag-name "style")
          (setq element-content-type "css")
          )
         )

;;        (message "tag=%S (%S > %S)\n%S" tag-name tag-beg tag-end props)
        (add-text-properties tag-beg tag-end props)
        (put-text-property tag-beg (1+ tag-beg) 'tag-beg t)
        (put-text-property (1- tag-end) tag-end 'tag-end t)

        (cond

         ((or (string= tag-name "!doctype") (string= tag-name "?xml"))
          (add-text-properties tag-beg tag-end '(tag-name "doctype" tag-type void face web-mode-doctype-face)))

         ((string= tag-name "!--")
          (add-text-properties tag-beg tag-end '(tag-name "comment" tag-type void part-side t part-token comment face web-mode-comment-face)))

         (close-found
          (when (and (not (char-equal ?\/ (aref tag-name 0)))
                     (> (- attrs-end tag-stop) 1))
;;            (message "tag-stop=%S attrs-end=%S" tag-stop attrs-end)
            (web-mode-scan-attrs tag-stop attrs-end)
            )
          (cond
           ((and (string= tag-name "script")
                 (member element-content-type '("javascript" "json")))
            (setq closing-string "</script>"))
           ((string= tag-name "style")
            (setq closing-string "</style>"))
           )

          ;; si <script type="document/html"> on ne fait pas la suite

          (when (and closing-string (web-mode-sf-client closing-string end t))
            (setq open tag-end
                  close (match-beginning 0))
            ;;(message "open(%S) close(%S) element-content-type(%S)" open close element-content-type)
            ;;(message "%S" (buffer-substring open close))
            (web-mode-scan-part open close element-content-type)
            (goto-char close)
            ); when
          ); close-found
         ); cond tag

        ); while

      )))

(defun web-mode-scan-part (beg end content-type)
  "Scan client part (e.g. javascript, json, css)."
  (save-excursion
    (let (regexp ch-before ch-at ch-next props start continue keywords rules-beg rules-end props-beg props-end token-type face)

      (cond
       ((string= content-type "javascript")
        (setq regexp "//\\|/\\*\\|\"\\|'"
              keywords web-mode-javascript-font-lock-keywords
              props '(part-language javascript part-side t)))
       ((string= content-type "json")
        (setq regexp "//\\|/\\*\\|\"\\|'"
              keywords web-mode-javascript-font-lock-keywords
              props '(part-language json part-side t)))
       ((string= content-type "css")
        (setq regexp "/\\*\\|\"\\|'"
              props '(part-language css part-side t)))
       )

      (add-text-properties beg end props)

      (when keywords
        (web-mode-fontify-region beg end keywords))

      (when (string= content-type "css")
        (goto-char beg)
        (setq rules-beg (if (= beg 1) 1 (1+ beg)))
        (while (and rules-beg
                    (web-mode-sf-client "{" end t)
                    (< (point) end))
          (setq rules-end (1- (point))
                props-beg (point))
          ;;          (message "rules-beg(%S) rules-end(%S)" rules-beg rules-end)
          ;;          (message "%S" font-lock-keywords)
          (web-mode-fontify-region rules-beg rules-end web-mode-selector-font-lock-keywords)
          (goto-char props-beg)
          (setq rules-beg nil)
          (setq props-end (web-mode-closing-paren-position rules-end end))
          (when props-end
;;          (when (and (search-forward "}" end t) (< (point) end))
;;            (setq props-end (1- (point))
;;                  rules-beg (point))
            (setq rules-beg (1+ props-end))
            (goto-char rules-beg)
;;            (message "props-beg(%S) props-end(%S)" props-beg props-end)
            (when (> (- props-end props-beg) 2)
              (web-mode-fontify-region props-beg props-end web-mode-declaration-font-lock-keywords)
              (goto-char props-beg)
              (while (and (not web-mode-disable-css-colorization)
                          (re-search-forward "#[0-9a-fA-F]\\{6\\}\\|#[0-9a-fA-F]\\{3\\}\\|rgb([ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)\\(.*?\\))" props-end t)
                          (< (point) props-end))
                (web-mode-colorize (match-beginning 0) (match-end 0))
                );while
              );when
            (goto-char rules-beg)
            );when
          );while
        );when

      (goto-char beg)

      (while (and regexp (web-mode-rsf-client regexp end t))
        (setq start (match-beginning 0)
              props nil
              continue t)
        (setq ch-at (char-after start))
        (setq ch-next (or (char-after (1+ start)) ?\d))
        (setq ch-before (or (char-before start) ?\d))
        (setq token-type "string")
;;        (message "beg=%S :%c%c%c" start ch-before ch-at ch-next)
        (cond

         ((char-equal ?\' ch-at)
          (unless (char-equal ?\\ ch-before)
            (while (and continue (search-forward "'" end t))
              (setq continue (or (get-text-property (1- (point)) 'block-side)
                                 (char-equal ?\\ (char-before (1- (point))))))
              )
            (cond
             ((string= content-type "javascript")
              (setq props '(part-token string face web-mode-javascript-string-face)))
             ((string= content-type "css")
              (setq props '(part-token string face web-mode-css-string-face)))
             ((string= content-type "json")
              (setq props '(part-token string face web-mode-json-string-face)))
             (t
              (setq props '(part-token string face web-mode-part-string-face)))
             );cond
            );unless
          )

         ((char-equal ?\" ch-at)
          (unless (char-equal ?\\ ch-before)
            (while (and continue (search-forward "\"" end t))
              (setq continue (or (get-text-property (1- (point)) 'block-side)
                                 (char-equal ?\\ (char-before (1- (point))))))
              )
            (cond
             ((string= content-type "json")
              (if (looking-at-p "[ ]*:")
                  (cond
                   ((char-equal ?\@ (char-after (1+ start)))
                    (setq props '(part-token string face web-mode-json-context-face))
                    )
                   (t
                    (setq props '(part-token string face web-mode-json-key-face))
                    )
                   )
                (setq props '(part-token string face web-mode-json-string-face)))
              )
             (t
              (cond
               ((string= content-type "javascript")
                (setq props '(part-token string face web-mode-javascript-string-face)))
               ((string= content-type "css")
                (setq props '(part-token string face web-mode-css-string-face)))
               (t
                (setq props '(part-token string face web-mode-part-string-face)))
               );cond
              );t
             );cond
            );unless
          )

         ((char-equal ?\/ ch-next)
          (unless (char-equal ?\\ ch-before)
            (setq props '(part-token comment face web-mode-part-comment-face)
                  token-type "comment")
            (goto-char (if (< end (line-end-position)) end (line-end-position)))
            )
          )

         ((char-equal ?\* ch-next)
          (unless (char-equal ?\\ ch-before)
            (setq props '(part-token comment face web-mode-part-comment-face)
                  token-type "comment")
            (search-forward "*/" end t)
            )
          )

         );cond

        (if props (add-text-properties start (point) props))

        (cond
         ((string= token-type "comment")
          (if web-mode-enable-comment-keywords
              (web-mode-enhance-comment start (point) t))
          )
         (t
          )
         )

        );while

      (when web-mode-enable-part-face
        (font-lock-append-text-property beg end 'web-mode-part-face face)
        )

      )))

;; http://www.w3.org/TR/html-markup/syntax.html#syntax-attributes
;; states:
;; nil(0) space(1) name(2) space-before(3) equal(4) space-after(5) value-uq(6) value-sq(7) value-dq(8)
(defun web-mode-scan-attrs (beg end)
  "Scan and fontify html attributes."
  (save-excursion
  ;;    (message "beg(%S) end(%S)" beg end)
    (let (name-beg name-end val-beg val-end (state 0) char pos escaped spaced)
      (goto-char (1- beg))

      (while (< (point) end)
        (forward-char)
        (setq pos (point)
              char (char-after))
        (setq spaced (char-equal ?\s char))
;;        (setq char (buffer-substring-no-properties pos (1+ pos)))

        (cond

         ((= pos end)
          (web-mode-propertize-attr state char name-beg name-end val-beg)
          (setq state 0
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((get-text-property pos 'block-side)
          )

         ((and spaced (= state 0))
          (setq state 1)
          )

         ((and spaced (member state '(1 3 5)))
          )

         ((and spaced (= state 2))
          (setq state 3)
          )

         ((and spaced (= state 4))
          (setq state 5)
          )

         ((and (char-equal ?\n char) (not (member state '(7 8))))
          (web-mode-propertize-attr state char name-beg name-end val-beg)
          (setq state 1
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((or (and (char-equal ?\" char) (= state 8) (not escaped))
              (and (char-equal ?\' char) (= state 7) (not escaped))
              (and (member char '(?\s ?\n ?\>)) (= state 6)))
          (web-mode-propertize-attr state char name-beg name-end val-beg)
          (setq state (if (= state 6) 1 0)
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((and (not spaced) (= state 1))
          (setq state 2)
          (setq name-beg pos)
          )

         ((and (char-equal ?\= char) (member state '(2 3)))
          (setq name-end pos)
          (setq state 4)
          )

         ((and (char-equal ?\" char) (member state '(4 5)))
          (setq val-beg pos)
          (setq state 8)
          )

         ((and (char-equal ?\' char) (member state '(4 5)))
          (setq val-beg pos)
          (setq state 7)
          )

         ((member state '(4 5))
          (setq val-beg pos)
          (setq state 6)
          )

         ((= state 1)
          (setq state 2)
          )

         );;cond

        ;;        (message "point(%S) end(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) val-end(%S)" pos end state char name-beg name-end val-beg val-end)

        (setq escaped (char-equal ?\\ char))

        );;while

      )))

(defun web-mode-propertize-attr (state char name-beg name-end val-beg &optional val-end)
  "propertize attr."
  (unless val-end (setq val-end (point)))
  ;;  (message "point(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) val-end(%S)" (point) state char name-beg name-end val-beg val-end)
  (cond
   ((and (= state 8) (not (char-equal ?\" char)))
    )
   ((and (= state 7) (not (char-equal ?\' char)))
    )
   ((= state 4)
    )
   ((null name-beg)
    )
   (t
    (if (or (and (= state 8) (char-equal ?\" char))
            (and (= state 7) (char-equal ?\' char)))
        (add-text-properties name-beg (1+ (point)) '(part-token attr face web-mode-html-attr-name-face))
      (add-text-properties name-beg (point) '(part-token attr face web-mode-html-attr-name-face)))
    (when (and val-beg val-end)
      (setq val-end (if (char-equal ?\> char) val-end (1+ val-end)))
      (add-text-properties val-beg val-end '(face web-mode-html-attr-value-face)))
    );t
   );cond
  )

(defun web-mode-velocity-skip-forward (pos)
  "find the end of a velocity block."
  (goto-char pos)
  (let (continue)
    (when (char-equal ?\# (char-after))
      (forward-char))
    ;;(message "pt=%S %c" (point) (char-after))
    (when (member (char-after) '(?\$ ?\@))
      ;;              (message "pt=%S" (point))
      (forward-char))
    (when (member (char-after) '(?\!))
      ;;              (message "pt=%S" (point))
      (forward-char))
    (if (member (char-after) '(?\{))
        (search-forward "}")
      (setq continue t)
      (while continue
        (skip-chars-forward "a-zA-Z0-9_-")
        (when (member (char-after) '(?\())
          (search-forward ")")
          )
        (if (member (char-after) '(?\.))
            (forward-char)
          (setq continue nil))
        );while
      );if
    ))

(defun web-mode-razor-skip-forward (pos)
  "find the end of a razor block."
  (goto-char pos)
  ;;            (message "pt=%S %c" (point) (char-after))
  (let (continue)
    (cond
     ((looking-at-p "@\\(if\\|for\\|section\\)")
      (search-forward "{")
      )
     ((looking-at-p "@[(}]")
      (forward-char)
      (goto-char (web-mode-closing-paren-position (point) (line-end-position)))
      )
     (t
      (forward-char)
      (setq continue t)
      (while continue
        (skip-chars-forward " a-zA-Z0-9_-"); caractère 'espace' pour '} else {'
        (cond
         ((char-equal ?\( (char-after))
          (search-forward ")")
          )
         ((char-equal ?\. (char-after))
          (forward-char))
         ((looking-at-p "[ ]*{")
          (search-forward "}")
          )
         (t
          (setq continue nil))
         );cond
        );while
      )
     );cond
    ))

(defun web-mode-colorize-foreground (color)
  "Colorize foreground based on background luminance."
  (let* ((values (x-color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (car (cdr (cdr values)))))
    (if (> 128.0 (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256))
	"white" "black")))

(defun web-mode-colorize (beg end)
  "Colorize CSS colors."
  (let (str plist len)
    (setq str (buffer-substring-no-properties beg end))
    (setq len (length str))
    (cond
     ((string= (substring str 0 1) "#")
      (setq plist (list :background str
                        :foreground (web-mode-colorize-foreground str)))
      (put-text-property beg end 'face plist))
     ((string= (substring str 0 4) "rgb(")
      (setq str (format "#%02X%02X%02X"
                        (string-to-number (match-string-no-properties 1))
                        (string-to-number (match-string-no-properties 2))
                        (string-to-number (match-string-no-properties 3))))
      (setq plist (list :background str
                        :foreground (web-mode-colorize-foreground str)))
      (put-text-property beg end 'face plist))
     );cond
    ))

(defun web-mode-fontify-region (beg end keywords)
  "Highlight block."
  (save-excursion
;;    (message "beg=%S end=%S" beg end)
    (let ((font-lock-keywords keywords)
          (font-lock-multiline nil)
          (font-lock-keywords-case-fold-search (string= web-mode-engine "asp"))
          (font-lock-keywords-only t)
          (font-lock-extend-region-functions nil)
          )
      (font-lock-fontify-region beg end)
      ))

  ;; UGLY HACK / workaround (help needed)
  (unless web-mode-buffer-highlighted
    (setq web-mode-buffer-highlighted t)
    (web-mode-fontify-region beg end keywords)
    )
  )

(defun web-mode-fill-paragraph (&optional justify)
  "fill paragraph"
  (save-excursion
    (let ((pos (point)) fill-coll
          prop pair beg end delim-beg delim-end chunk fill-col)
      (cond
       ((or (eq (get-text-property pos 'part-token) 'comment)
            (eq (get-text-property pos 'block-token) 'comment))
        (setq prop
              (if (get-text-property pos 'part-token) 'part-token 'block-token))
        (setq pair (web-mode-property-boundaries prop pos))
        (when (and pair (> (- (cdr pair) (car pair)) 6))
          (setq fill-coll (if (< fill-column 10) 70 fill-column))
          (setq beg (car pair)
                end (cdr pair))
          (goto-char beg)
          (setq chunk (buffer-substring-no-properties beg (+ beg 2)))
          (cond
           ((string= chunk "//")
            (setq delim-beg "//"
                  delim-end "EOL"))
           ((string= chunk "/*")
            (setq delim-beg "/*"
                 delim-end "*/"))
           ((string= chunk "{#")
            (setq delim-beg "{#"
                  delim-end "#}"))
           ((string= chunk "<!")
            (setq delim-beg "<!--"
                  delim-end "-->"))
           )
          ;;          (subst-char-in-region beg end ?\n ?\s)
          ;;          (message "fill-column=%S pt=%S pair=%S chunk=%S"
          ;;                   fill-column (point) pair chunk)
          )
        );comment - case

       ((web-mode-is-html-text)
        (setq pair (web-mode-property-boundaries prop pos))
        (setq beg (previous-property-change pos)
              end (next-property-change pos))
        )

       );cond
      ;;(message "beg%S end%S" beg end)
      (when (and beg end)
        (fill-region beg end))
      t)))

(defun web-mode-property-boundaries (prop &optional pos)
  "property boundaries (cdr is 1+)"
  (unless pos (setq pos (point)))
  (let (beg end val)
    (setq val (get-text-property pos prop))
    (if (null val)
        val
      (if (or (bobp)
              (not (eq (get-text-property (1- pos) prop) val)))
          (setq beg pos)
        (setq beg (previous-single-property-change pos prop))
        (when (null beg) (setq beg (point-min))))
      (if (or (eobp)
              (not (eq (get-text-property (1+ pos) prop) val)))
          (setq end pos)
        (setq end (next-single-property-change pos prop))
        (when (null end) (setq end (point-min))))
      (cons beg end))))

(defun web-mode-scan-whitespaces (beg end)
  "Scan whitespaces."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward web-mode-whitespaces-regexp end t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face web-mode-whitespace-face))
      );while
    ))

(defun web-mode-errors-show ()
  "Show unclosed tags."
  (interactive)
  (let (beg end tag pos l n tags i cont cell overlay overlays first
            (ori (point))
            (errors 0)
            (continue t)
        )
    (setq overlays (overlays-in (point-min) (point-max)))
    (when overlays
      (dolist (overlay overlays)
        (when (eq (overlay-get overlay 'face) 'web-mode-warning-face)
          (delete-overlay overlay)
          )
        )
      )
    (goto-char (point-min))
    (when (not (or (get-text-property (point) 'tag-beg)
                   (web-mode-tag-next)))
      (setq continue nil))
    (while continue
      (setq pos (point))
      (setq tag (get-text-property pos 'tag-name))
      (cond
       ((eq (get-text-property (point) 'tag-type) 'start)
        (setq tags (add-to-list 'tags (list tag pos)))
;;        (message "(%S) opening %S" pos tag)
        )
       ((eq (get-text-property (point) 'tag-type) 'end)
        (setq i 0
              l (length tags)
              cont t)
        (while (and (< i l) cont)
          (setq cell (nth i tags))
;;          (message "cell=%S" cell)
          (setq i (1+ i))
          (cond
           ((string= tag (nth 0 cell))
            (setq cont nil)
            )
           (t
            (setq errors (1+ errors))
            (setq beg (nth 1 cell))
            (setq end (web-mode-tag-end-position beg))
            (unless first
              (setq first beg))
            (setq overlay (make-overlay beg (1+ end)))
            (overlay-put overlay 'face 'web-mode-warning-face)
;;            (message "invalid <%S> at %S" (nth 0 cell) (nth 1 cell))
            )
           );cond
          );while

        (dotimes (i i)
          (setq tags (cdr tags))
;;          (setq cell (nth i tags))
;;          (message "removing=%S" cell)
          )

        )
       );cond
      (when (not (web-mode-tag-next))
        (setq continue nil))
      );while
    (message "%S error(s) detected" errors)
    (if (> errors 0)
        (progn (goto-char first)
               (recenter))
      (goto-char ori)
      );if
    ;;    (message "%S" tags)
    ))

(defun web-mode-whitespaces-show ()
  "Toggle whitespaces."
  (interactive)
  (if web-mode-enable-whitespaces
      (web-mode-whitespaces-off)
    (web-mode-whitespaces-on))
  (web-mode-scan-buffer))

(defun web-mode-whitespaces-on ()
  "Show whitespaces."
  (interactive)
  (when (null web-mode-display-table)
    ;;    http://webdesign.about.com/od/localization/l/blhtmlcodes-ascii.htm
    (setq web-mode-display-table (make-display-table))
    (aset web-mode-display-table 9  (vector ?\xBB ?\t)) ;tab
    (aset web-mode-display-table 10 (vector ?\xB6 ?\n)) ;line feed
    (aset web-mode-display-table 32 (vector ?\xB7)) ;space
    );when
  (when web-mode-hl-line-mode-flag
    (global-hl-line-mode -1))
  (setq buffer-display-table web-mode-display-table)
  (setq web-mode-enable-whitespaces t))

(defun web-mode-whitespaces-off ()
  "Hide whitespaces."
  (when web-mode-hl-line-mode-flag
    (global-hl-line-mode 1))
  (setq buffer-display-table nil)
  (setq web-mode-enable-whitespaces nil))

(defun web-mode-buffer-indent ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun web-mode-buffer-refresh ()
  "Indent and fontify buffer."
  (interactive)
  (web-mode-scan-buffer)
  (web-mode-buffer-indent))

(defun web-mode-previous-usable-server-line ()
  "Return previous non blank/comment/string line and return this line (trimmed)."
  (interactive)
  (save-excursion
    (let ((continue t)
          (line "")
          (pos (point)))
      (beginning-of-line)
      (while (and continue
                  (not (bobp))
                  (forward-line -1))
        (if (not (web-mode-is-comment-or-string-line))
            (setq line (web-mode-trim (buffer-substring (point) (line-end-position)))))
        (when (not (string= line "")) (setq continue nil))
        );while
      (if (string= line "")
          (progn (goto-char pos) nil)
        line)
      )))

(defun web-mode-previous-usable-client-line ()
  "Return previous non blank/comment/string line and return this line (trimmed)."
  (interactive)
  (save-excursion
    (let ((continue t)
          (line "")
          (pos (point)))
      (beginning-of-line)
      (while (and continue
                  (not (bobp))
                  (forward-line -1))
        (if (not (web-mode-is-part-token-line))
            (setq line (web-mode-trim (buffer-substring (point) (line-end-position)))))
        (when (not (string= line "")) (setq continue nil))
        )
      (if (string= line "")
          (progn (goto-char pos) nil)
        line)
      )))

(defun web-mode-in-code-block (open close &optional prop)
  "Detect if point is in a block delimited by open and close."
  (save-excursion
    (let ((pos (point)) pos-open pos-close start end ret)
      (when prop
        (setq start pos
              end pos)
        (when (eq (get-text-property pos prop) (get-text-property (1- pos) prop))
          (setq start (or (previous-single-property-change pos prop) (point-min))))
        (when (eq (get-text-property pos prop) (get-text-property (1+ pos) prop))
          (setq end (next-single-property-change pos prop)))
        ;;        (message "start(%S) end(%S)" start end)
        )
      (setq ret (and (web-mode-sb open start t)
                     (setq pos-open (point))
                     (web-mode-sf close end t)
                     (setq pos-close (point))
                     (>= pos-close pos)))
      (if ret
          (cons pos-open pos-close)
        ret)
      )))

(defun web-mode-line-number (&optional pos)
  "Return line number at point."
  (unless pos (setq pos (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pos)
                 (if (= (web-mode-column-at-pos pos) 0) 1 0)))
    ret))

(defun web-mode-clean-client-line (input)
  "Remove comments and server scripts."
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))
    (dotimes (i n)
      (if (or (get-text-property i 'block-side input)
              (eq (get-text-property i 'part-token input) 'comment))
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        );if
      ;;      (message "out=%s beg=%d" out beg)
      );dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (web-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

(defun web-mode-clean-server-line (input)
  "Remove comments from server line."
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))
    (dotimes (i n)
      (if (eq (get-text-property i 'block-token input) 'comment)
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        );if
      );dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (web-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

(defun web-mode-column-at-pos (&optional pos)
  "Column at point"
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (current-column)
    ))

;; doit-on considérer que '=' est un bloc ouvrant avec ';' comme char de fin ?
(defun web-mode-point-context (pos)
  "POS should be at the beginning of the indentation.
   Return ctx = plist containing
    :block-beg, :block-column,
    :first-char, :line (trimmed)
    :type (live, comment, string),
    :language (html, php, jsp, aspx, asp + javascript, css),
    :indent-offset
    :prev-line :prev-char :prev-props"
  (save-excursion
    (let (ctx pos-min
              block-beg block-column first-char line type language indent-offset
              prev-line prev-char prev-props)

      (setq pos-min (point-min))
      (setq block-beg pos-min
            block-column 0
            type "live"
            language ""
            prev-line ""
            prev-char 0)
      (cond

       ((bobp)
        )

       ((string= web-mode-content-type "css")
        (setq language "css"
              indent-offset web-mode-css-indent-offset))

       ((member web-mode-content-type '("javascript" "json"))
        (setq language "javascript"
              indent-offset web-mode-code-indent-offset))

       ((string= web-mode-content-type "php")
        (setq language "php"
              indent-offset web-mode-code-indent-offset))

       ((or (string= web-mode-content-type "xml"))
        (setq language "xml"
              indent-offset web-mode-markup-indent-offset))

       ((and (get-text-property pos 'tag-beg)
             (get-text-property pos 'tag-name))
        (setq language "html"
              indent-offset web-mode-markup-indent-offset)
        )

       ((or (and (eq (get-text-property pos 'part-token) 'comment)
                 (eq (get-text-property (1- pos) 'part-token) 'comment)
                 (progn
                   (setq block-beg (previous-single-property-change pos 'part-token))
                   t))
            (and (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property (1- pos) 'block-token) 'comment)
                 (progn
                   (setq block-beg (previous-single-property-change pos 'block-token))
                   t)))
        (setq type "comment"))

       ((or (and (eq (get-text-property pos 'part-token) 'string)
                 (eq (get-text-property (1- pos) 'part-token) 'string))
            (and (eq (get-text-property pos 'block-token) 'string)
                 (eq (get-text-property (1- pos) 'block-token) 'string)))
        (setq type "string"))

       ((and (get-text-property pos 'block-side)
             (not (get-text-property pos 'block-beg)))
        (setq block-beg (or (web-mode-block-beginning-position pos) pos-min))
        (goto-char block-beg)
        (setq block-column (current-column))
        (setq language web-mode-engine)
        (setq indent-offset web-mode-code-indent-offset)
        (cond
         ((string= web-mode-engine "razor")
          (setq block-beg (+ block-beg 2))
          (setq block-column (+ block-column 2))
          )
         ((and (string= web-mode-engine "jsp")
               (web-mode-looking-at-pos "<%@\\|<[[:alpha:]]" block-beg))
          (save-excursion
            (goto-char block-beg)
            (looking-at "<%@[ ]*[[:alpha:]]+[ ]+\\|</?[[:alpha:]]+:[[:alpha:]]+[ ]+")
            (goto-char (match-end 0))
            (setq block-column (current-column))
            )
          )
         ((and (string= web-mode-engine "freemarker")
               (web-mode-looking-at-pos "<@\\|<%@\\|<[[:alpha:]]" block-beg))
          (save-excursion
            (goto-char block-beg)
            (looking-at "<@[[:alpha:].]+[ ]+\\|<%@[ ]*[[:alpha:]]+[ ]+\\|<[[:alpha:]]+:[[:alpha:]]+[ ]+")
            (goto-char (match-end 0))
            (setq block-column (current-column))
            )
          )
         );cond
        )

       ((and (get-text-property pos 'part-side)
             (get-text-property pos 'part-language))
        (setq block-beg (or (previous-single-property-change pos 'part-side) pos-min))
        (goto-char block-beg)
        (search-backward "<")
        (setq block-column (current-column))
        (setq language (symbol-name (get-text-property pos 'part-language)))
        (cond
         ((string= language "css")
          (setq indent-offset web-mode-css-indent-offset)
          )
         (t
          (setq language "javascript"
                indent-offset web-mode-code-indent-offset)
          )
         )
        )

       (t
        (setq language "html"
              indent-offset web-mode-markup-indent-offset)
        )

       );cond

      (goto-char pos)
      (setq line (web-mode-trim (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position))))
      (setq first-char (if (string= line "") 0 (aref line 0)))

      (when (or (member language '("php" "javascript"))
                (and (string= language "html") (not (char-equal ?\< first-char))))
        (cond
         ((member language '("html" "javascript"))
          (setq prev-line (web-mode-previous-usable-client-line))
;;          (message "prev-line=%S" prev-line)
          (when prev-line
            (setq prev-line (web-mode-clean-client-line prev-line))
            (setq prev-props (text-properties-at (1- (length prev-line)) prev-line)))
          )
         (t
          (setq prev-line (web-mode-previous-usable-server-line))
          (when prev-line
            (setq prev-line (web-mode-clean-server-line prev-line)))
          )
         );cond
        (when (>= (length prev-line) 1)
          (setq prev-char (aref prev-line (1- (length prev-line))))
          (setq prev-line (substring-no-properties prev-line))
          )
        )

;;      (if (string= language "json") (setq language "javascript"))

      (when (string= web-mode-content-type "html")
        (cond
         ((string= language "javascript")
          (setq block-column (+ block-column web-mode-script-padding)))
         ((string= language "css")
          (setq block-column (+ block-column web-mode-style-padding)))
         ((not (member language '("html" "razor")))
          (setq block-column (+ block-column web-mode-block-padding)))
         )
        )

      (setq ctx (list :block-beg block-beg
                      :block-column block-column
                      :first-char first-char
                      :line line
                      :type type
                      :language language
                      :indent-offset indent-offset
                      :prev-line prev-line
                      :prev-char prev-char
                      :prev-props prev-props))
;;      (message "%S" ctx)
      ctx
      )))

(defun web-mode-indent-line ()
  "Indent current line according to language."
  (let ((inhibit-modification-hooks t)
        pos
        offset
        ctx
        block-beg
        block-column
        first-char
        line
        type
        language
        indent-offset
        prev-line
        prev-char
        prev-props)

    (save-excursion
      (back-to-indentation)
      (setq pos (point))
      (setq ctx (web-mode-point-context pos))
      (setq block-beg (plist-get ctx :block-beg))
      (setq block-column (plist-get ctx :block-column))
      (setq first-char (plist-get ctx :first-char))
      (setq line (plist-get ctx :line))
      (setq type (plist-get ctx :type))
      (setq language (plist-get ctx :language))
      (setq indent-offset (plist-get ctx :indent-offset))
      (setq prev-line (plist-get ctx :prev-line))
      (setq prev-char (plist-get ctx :prev-char))
      (setq prev-props (plist-get ctx :prev-props))

      (cond

       ((or (bobp)
            (= (line-number-at-pos pos) 1))
        (setq offset 0)
        )

       ((string= type "string")
        (setq offset nil)
        )

       ((string= type "comment")
        (goto-char (car
                    (web-mode-property-boundaries
                     (if (eq (get-text-property pos 'part-token) 'comment)
                         'part-token
                       'block-token)
                     pos)))
        (setq offset (current-column))
        (when (and (string= (buffer-substring-no-properties (point) (+ (point) 2)) "/*")
                   (char-equal ?\* first-char))
          (setq offset (1+ offset)))
        );case comment

       ((member language '("php" "jsp" "asp" "aspx" "javascript" "code" "python" "erb" "freemarker"))

        (cond

         ((string-match-p "^[?%]>" line)
          (if (web-mode-block-beginning pos)
              (setq offset (current-column)))
          )

         ((and (string= language "php") (string-match-p "^->" line))
          (when (web-mode-sb "->" block-beg)
            (setq offset (current-column)))
          )

         ((and (string= language "javascript") (char-equal ?\. first-char))
          (when (web-mode-rsb "[[:alnum:][:blank:]]\\.[[:alpha:]]" block-beg)
            (setq offset (1+ (current-column))))
          )

         ((member first-char '(?\? ?\. ?\:))
          (web-mode-rsb "[^!=][=(]" block-beg)
          (setq offset (1+ (current-column)))
          (when (and (string= web-mode-engine "php")
                     (looking-at-p " =>"))
            (setq offset (1+ offset))
            )
          )

         ((and (member prev-char '(?\. ?\+ ?\? ?\:))
               (not (string-match-p "^\\(case\\|default\\)[ :]" prev-line)))
          (web-mode-rsb "[=(]" block-beg)
          (skip-chars-forward "= (")
          (setq offset (current-column))
          )

         ((string= language "erb")
          (setq offset (web-mode-ruby-indentation pos
                                                  line
                                                  block-column
                                                  indent-offset
                                                  block-beg))
          )

         (t
          (setq offset (web-mode-bracket-indentation pos
                                                     block-column
                                                     indent-offset
                                                     block-beg))
          );t

         ));end case script block

       ((string= language "css")
        (setq offset (web-mode-bracket-indentation pos
                                                   block-column
                                                   indent-offset
                                                   block-beg))
        );case style

       (t ; case html block

        (cond

         ((and prev-props (eq (plist-get prev-props 'part-token) 'attr))
          (web-mode-tag-beginning)
          (let (skip)
            (setq skip (next-single-property-change (point) 'part-token))
            (when skip
              (goto-char skip)
              (setq offset (current-column))
              ))
          )

         ((or (and (eq (get-text-property pos 'tag-type) 'end)
                   (web-mode-match-html-tag))
              (and (get-text-property pos 'block-beg)
                   (looking-at-p web-mode-close-block-regexp)
                   (funcall web-mode-engine-control-matcher))
              )
          (setq offset (current-indentation))
          )

         ((or (eq (length line) 0)
              (= web-mode-indent-style 2)
              (get-text-property pos 'tag-beg)
              (get-text-property pos 'block-beg))
          (setq offset (web-mode-markup-indentation pos))
          )

         );cond

        );end case html block

       );end switch language block

      );save-excursion

    (when offset
      (let ((diff (- (current-column) (current-indentation))))
        (setq offset (max 0 offset))
        (indent-line-to offset)
        (if (> diff 0) (forward-char diff))
        );let
      );when

    ))

;; asp aspx blade ctemplate django erb go jsp python razor velocity
(defun web-mode-is-active-block (pos)
  "web-mode-is-active-block"
  (save-excursion
    (let (ctrl state)
      (goto-char pos)
      (when (looking-at web-mode-active-block-regexp)

        (cond

         ((string= web-mode-engine "php")
          (setq ctrl (match-string-no-properties 3))
          (if (member ctrl '("else" "elseif"))
              (setq ctrl nil)
            (setq state (not (string= "end" (match-string-no-properties 2))))
            )
          )

         ((string= web-mode-engine "django")
          (setq ctrl (match-string-no-properties 2))
          (if (member ctrl '("else" "elseif" "elif"))
              (setq ctrl nil)
            (setq state (not (string= "end" (match-string-no-properties 1))))
            )
          )

         ((string= web-mode-engine "smarty")
          (setq ctrl (match-string-no-properties 1))
          (if (member ctrl '("else" "elseif"))
              (setq ctrl nil)
            (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 1))))
            )
          )

         ((string= web-mode-engine "dust")
          (setq ctrl (match-string-no-properties 1))
          (if (or (member ctrl '("else"))
                  (char-equal ?\/ (char-after (1- (web-mode-block-end-position)))))
              (setq ctrl nil)
            (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 1))))
            )
          )

         ((string= web-mode-engine "closure")
          (setq ctrl (match-string-no-properties 1))
          (if (or (member ctrl '("else" "elseif" "case" "default"))
                  (char-equal ?\/ (char-after (1- (web-mode-block-end-position)))))
              (setq ctrl nil)
            (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 1))))
            )
          )

         ((string= web-mode-engine "ctemplate")
          (setq ctrl (match-string-no-properties 1))
          (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 2))))
          )

         ((string= web-mode-engine "velocity")
          (setq ctrl (match-string-no-properties 1))
          (if (member ctrl '("else" "elseif"))
              (setq ctrl nil)
            (setq state (not (string= "end" (match-string-no-properties 1))))
            )
          )

         ((string= web-mode-engine "blade")
          (setq ctrl (match-string-no-properties 2))
          (if (member ctrl '("else" "elseif"))
              (setq ctrl nil)
            (setq state (not (string= "end" (match-string-no-properties 1))))
            )
          )

         ((string= web-mode-engine "go")
          (setq ctrl (match-string-no-properties 1))
          (if (member ctrl '("else"))
              (setq ctrl nil)
            (setq state (not (string= "end" ctrl)))
            )
          )

         ((string= web-mode-engine "erb")
          (setq ctrl (match-string-no-properties 1))
          (if (member ctrl '("else"))
              (setq ctrl nil)
            (setq state (not (string= "end" ctrl)))
            )
          )

         ((string= web-mode-engine "jsp")
          (setq ctrl (match-string-no-properties 1))
          (if (or (member ctrl '("h:inputtext" "jsp:usebean"))
                  (char-equal ?\/ (char-after (1- (web-mode-block-end-position)))))
              (setq ctrl nil)
            (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 1))))
            )
          )

         ((string= web-mode-engine "freemarker")
          (if (or (member (aref (match-string-no-properties 0) 1) '(?\@ ?\#))
                  (member (aref (match-string-no-properties 0) 2) '(?\@ ?\#)))
              (setq ctrl (match-string-no-properties 2))
            (setq ctrl (match-string-no-properties 1))
            )
;;          (message "ctrl=%S" ctrl)
          (if (or (member ctrl '("include" "setting" "import" "global" "ftl"
                                 "nested" "return" "local" "flush" "break" "recover"))
                  (char-equal ?\/ (char-after (1- (web-mode-block-end-position)))))
              (setq ctrl nil)
            (setq state (not (char-equal ?\/ (aref (match-string-no-properties 0) 1))))
            )
          )

         );cond

        );when

;;      (message "engine=%S ctrl=%S state=%S" web-mode-engine ctrl state)

      (if ctrl (cons ctrl state) nil)
      )))

(defun web-mode-markup-indentation (pos)
  "markup indentation"
  (save-excursion
    (goto-char pos)
    (let ((offset 0) beg)
      (setq beg (web-mode-markup-indentation-origin))
      (when beg
        (goto-char beg)
        (setq offset (+ (current-indentation)
                        (if (web-mode-is-opened-element beg pos)
                            web-mode-markup-indent-offset
                          0)))
        );when
      offset
      )))

(defun web-mode-markup-indentation-origin ()
  "web-mode-indentation-origin-pos"
  (let ((continue t) pos)
    (while continue
      (forward-line -1)
      (back-to-indentation)
      (setq continue (not (bobp)))
      (when (or (get-text-property (point) 'tag-beg)
                (and (get-text-property (point) 'block-beg)
                     (web-mode-is-active-block (point))
;;                     (looking-at-p web-mode-active-block-regexp)
                     ))
        (setq continue nil
              pos (point))
        )
      );while
;;    (message "indent-origin=%S" pos)
    pos
    ))

(defun web-mode-is-opened-element (pos limit)
  "Is there any HTML element without a closing tag ?"
  (interactive)
  (let (tag
        tag-pos block-pos
        state
        n
        ret
        (continue t)
        (buffer (current-buffer))
        (h (make-hash-table :test 'equal))
        ctrl)
    (while continue
      (setq ctrl nil)
      (when (or (and (get-text-property pos 'tag-beg)
                     (member (get-text-property pos 'tag-type) '(start end)))
                (and (get-text-property pos 'block-beg)
                     (setq ctrl (web-mode-is-active-block pos))))
        (if ctrl
            (progn
              (setq tag (car ctrl)
                    state (cdr ctrl)))
          (setq tag (get-text-property pos 'tag-name)
                state (eq (get-text-property pos 'tag-type) 'start))
          )

        (setq n (gethash tag h 0))
        (if (null state)
            (when (> n 0) (puthash tag (1- n) h))
          (puthash tag (1+ n) h))
        );when
      (setq pos (1+ pos))
      (when (null tag-pos)
        (setq tag-pos (next-single-property-change pos 'tag-beg buffer limit)))
      (when (null block-pos)
        (setq block-pos (next-single-property-change pos 'block-beg buffer limit)))
      (cond
       ((and (null tag-pos)
             (null block-pos))
        (setq pos nil)
        )
       ((or (null block-pos)
            (< tag-pos block-pos))
        (setq pos tag-pos)
        (setq tag-pos nil)
        )
       (t
        (setq pos block-pos)
        (setq block-pos nil)
        )
       )
      (when (or (null pos)
                (>= pos limit))
        (setq continue nil))
      );while
;;    (message "hashtable=%S" h)
    (maphash (lambda (k v) (if (> v 0) (setq ret t))) h)
    ret))

(defun web-mode-ruby-indentation (pos line initial-column language-offset limit)
  "Calc indent column."
  (interactive)
  (unless limit (setq limit nil))
  (let (h out prev-line prev-indentation)
    (setq h (web-mode-previous-line pos limit))
    (setq out initial-column)
    (when h
      (setq prev-line (car h))
      (setq prev-indentation (cdr h))
      (cond
       ((string-match-p "^\\(end\\|else\\|elsif\\|when\\)" line)
        (setq out (- prev-indentation language-offset))
        )
       ((string-match-p "\\(when\\|if\\|else\\|elsif\\|unless\\|for\\|while\\|def\\|class\\)" prev-line)
        (setq out (+ prev-indentation language-offset))
        )
       (t
        (setq out prev-indentation)
        )
       )
      );when
    out
    ))

(defun web-mode-previous-line (pos limit)
  "Previous line"
  (save-excursion
    (let (beg end line (continue t))
      (goto-char pos)
      (while continue
        (forward-line -1)
        (setq end (line-end-position))
        (setq line (buffer-substring-no-properties (point) end))
        (when (or (not (string-match-p "^[ \t]*$" line))
                  (bobp)
                  (<= (point) limit))
          (setq continue nil))
        )
      (if (<= (point) limit)
          ;;todo : affiner (le + 3 n est pas générique cf. <?php <% <%- etc.)
          (setq beg (if (< (+ limit 3) end) (+ limit 3) end))
        (setq beg (line-beginning-position))
        );if
      (setq line (buffer-substring-no-properties beg end))
      ;;      (message "line=%s" line)
      (cons line (current-indentation))
      )))

(defun web-mode-bracket-indentation (pos initial-column language-offset &optional limit)
  "Calc indent column."
  (interactive)
  (unless limit (setq limit nil))
  (save-excursion
    (let (offset n first-char block-info col block-column (continue t))
      (goto-char pos)
      (setq first-char (char-after)
            block-column initial-column)
      (while continue
        (forward-line -1)
        (back-to-indentation)
        (cond
         ((or (> limit (point))
              (bobp))
          (setq continue nil)
          )
         ((and (= (current-indentation) initial-column)
               (not (eolp)))
          (setq continue nil)
          (setq limit (point))
          )
         )
        )
;;      (message "ic=%S point=%S limit=%S" initial-column (point) limit)
      (goto-char pos)

      (setq block-info (web-mode-count-opened-blocks pos limit))
      (setq col initial-column)
;;      (message "bi=%S" block-info)
      (if (cddr block-info)
          (progn
            (setq col (car (cdr block-info)))
            )
        (setq n (car block-info))
        (setq col initial-column)
;;        (message "initial-col=%S n=%S col=%S" initial-column n col)
        (if (member first-char '(?\} ?\) ?\])) (setq n (1- n)))
        (setq col (+ initial-column (* n language-offset)))
        );if
      (if (< col block-column) block-column col)
      )))

;; return (opened-blocks . (col-num . arg-inline))
(defun web-mode-count-opened-blocks (pos &optional limit)
  "Count opened opened block at point."
  (interactive)
  (unless limit (setq limit nil))
  (save-excursion
    (goto-char pos)
    (let ((continue t)
          (match "")
          (case-found nil)
          (case-count 0)
          (queues (make-hash-table :test 'equal))
          (opened-blocks 0)
          (col-num 0)
          (regexp "[\]\[}{)(]\\|\\(break\\|case\\|default\\)")
          (num-opened 0)
          close-char n queue arg-inline arg-inline-checked char lines)

      (while (and continue (re-search-backward regexp limit t))
        (unless (web-mode-is-comment-or-string)
          (setq match (match-string-no-properties 0)
                char (char-after))

          (cond

           ((member char '(?\{ ?\( ?\[))
            (cond
             ((char-equal char ?\() (setq close-char ?\)))
             ((char-equal char ?\{) (setq close-char ?\}))
             ((char-equal char ?\[) (setq close-char ?\])))

            (setq queue (gethash char queues nil))
            (setq queue (push (cons (point) (web-mode-line-number)) queue))
            (puthash char queue queues)
            ;;(message "%c queue=%S" char queue)

            (setq queue (gethash close-char queues nil))
            (setq n (length queue))
            (cond
             ((> n 0)
              (setq queue (cdr queue))
              (puthash close-char queue queues)
              ;;(message "%c queue=%S" close-char queue)
              (setq queue (gethash char queues nil))
              (setq queue (cdr queue))
              (puthash char queue queues)
              ;;(message "%c queue=%S" char queue)
              )
             ((= n 0)
              (setq num-opened (1+ num-opened))
              ;;(message "num-opened=%S %S" num-opened (point))
              )
             )

            (when (and (= num-opened 1) (null arg-inline-checked))
              (setq arg-inline-checked t)
              (when (not (member (char-after (1+ (point))) '(?\n ?\r ?\{)))
                (setq arg-inline t
                      continue nil
                      col-num (1+ (current-column))))
;;              (message "pt=%S" (point))
              )

            );case

           ((member char '(?\} ?\) ?\]))
            (setq queue (gethash char queues nil))
            (setq queue (push (point) queue))
            (puthash char queue queues)
            ;;            (message "%c queue=%S" char queue)
            )

           ((member match '("case" "default"))
            (setq case-found t
                  case-count (1+ case-count))
            )

           ((string= match "break")
            (setq case-count (1- case-count))
            )

           );cond

          );unless
        );while

      (unless arg-inline
        (maphash
         (lambda (char queue)
           (when (member char '(?\{ ?\( ?\[))
             ;;(message "%c => %S" char queue)
             (dolist (pair queue)
               (setq n (cdr pair))
               (unless (member n lines)
                 (push n lines))
               )
             );when
           )
         queues)
        (setq opened-blocks (length lines))
        (when (and case-found (> case-count 0))
          (goto-char pos)
          (back-to-indentation)
          (when (not (looking-at-p "case\\|}"))
            (setq opened-blocks (1+ opened-blocks))
            )
          )
        );unless

;;      (message "opened-blocks(%S) col-num(%S) arg-inline(%S)" opened-blocks col-num arg-inline)

      (cons opened-blocks (cons col-num arg-inline))

      )))

(defun web-mode-count-char-in-string (char string)
  "Count char in string."
  (let ((n 0))
    (dotimes (i (length string))
      (if (char-equal (elt string i) char)
          (setq n (1+ n))))
    n))

(defun web-mode-scan-at-pos ()
  "web mode scan at point"
  (save-excursion
    (let (scan-beg scan-end (pos (point)))
      (cond
       ((web-mode-rsb-client "^[ ]*<")
        (setq scan-beg (point))
        (goto-char pos)
        (setq scan-end (if (web-mode-rsf-client "[[:alnum:] /\"]>[ ]*$") (point) (point-max)))
        ;;              (message "scan-end=%S" scan-end)
        ;;            (setq scan-end (point-max))
        )
       (t
        (setq scan-beg 1
              scan-end (point-max))
        )
       );cond
      ;;(message "scan-region (%S) > (%S)" scan-beg scan-end)
      ;;          (setq scan-end (point-max))
      (web-mode-scan-region scan-beg scan-end)
      );save-excursion
    ))

(defun web-mode-mark-and-expand ()
  "Mark and expand."
  (interactive)
;;  (message "last-input-event=%S" last-input-event)
  (web-mode-mark (point)))

;; todo : pb du engine=go ... selection d'un bloc
(defun web-mode-mark (pos)
  "Mark at point."

  (let ((beg pos) (end pos) prop reg-beg boundaries)

    (if mark-active
        (setq reg-beg (region-beginning))
      (setq web-mode-expand-initial-pos (point)))

    ;;    (message "regs=%S %S %S %S" (region-beginning) (region-end) (point-min) (point-max))

    ;;    (message "before=%S" web-mode-expand-previous-state)

    (cond

     ((and mark-active
           (= (region-beginning) (point-min))
           (or (= (region-end) (point-max)) (= (1+ (region-end)) (point-max))))
      (deactivate-mark)
      (goto-char (or web-mode-expand-initial-pos (point-min)))
      (recenter))

    ((and (member (get-text-property pos 'block-token) '(comment string))
          (not (string= web-mode-expand-previous-state "block-token")))

      (when (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token))
        (setq beg (or (previous-single-property-change pos 'block-token) (point-min))))
      (when (eq (get-text-property pos 'block-token) (get-text-property (1+ pos) 'block-token))
        (setq end (next-single-property-change pos 'block-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "block-token"))

     ((and (eq (get-text-property pos 'block-side) t)
           (not (member web-mode-engine '(django go)))
           (setq boundaries (web-mode-in-code-block "{" "}" 'block-side))
           (not (string= web-mode-expand-previous-state "server-block")))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      ;;      (message "char=[%c]" (char-before (- (point) 1)))
      (if (char-equal ?\% (char-before (- (point) 1)))
          (setq web-mode-expand-previous-state "block-side")
        (setq web-mode-expand-previous-state "server-block"))
      (exchange-point-and-mark)
      )

     ((and (eq (get-text-property pos 'block-side) t)
           (not (string= web-mode-expand-previous-state "block-side")))
      (when (eq (get-text-property pos 'block-side) (get-text-property (1- pos) 'block-side))
        (setq beg (or (previous-single-property-change pos 'block-side) (point-min))))
      (when (eq (get-text-property pos 'block-side) (get-text-property (1+ pos) 'block-side))
        (setq end (next-single-property-change pos 'block-side)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "block-side"))

     ((and (member (get-text-property pos 'part-token) '(comment string))
           (not (string= web-mode-expand-previous-state "part-token")))

      (when (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token))
        (setq beg (previous-single-property-change pos 'part-token)))
      (when (eq (get-text-property pos 'part-token) (get-text-property (1+ pos) 'part-token))
        (setq end (next-single-property-change pos 'part-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "part-token"))

     ((and (get-text-property pos 'part-side)
           (not (string= web-mode-expand-previous-state "client-part"))
           (setq boundaries (web-mode-in-code-block "{" "}" 'part-side)))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "client-part")
      )

     ((and (eq (get-text-property pos 'part-side) t)
           (not (string= web-mode-expand-previous-state "part-side")))

      (when (eq (get-text-property pos 'part-side) (get-text-property (1- pos) 'part-side))
        (setq beg (previous-single-property-change pos 'part-side)))
      (when (eq (get-text-property pos 'part-side) (get-text-property (1+ pos) 'part-side))
        (setq end (next-single-property-change pos 'part-side)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "part-side"))

     ;;     ((and (eq (get-text-property pos 'markup-type) 'attr)
     ((and (eq (get-text-property pos 'part-token) 'attr)
           (not (string= web-mode-expand-previous-state "html-attr")))

      ;; todo: tester que le car précédent n'est pas un
      (when (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token))
        (setq beg (previous-single-property-change pos 'part-token)))
      (when (eq (get-text-property pos 'part-token) (get-text-property (1+ pos) 'part-token))
        (setq end (next-single-property-change pos 'part-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "html-attr"))

     ((and mark-active
           (char-equal ?\< (char-after)))

      (web-mode-element-parent)
      (if (= reg-beg (region-beginning))
          (mark-whole-buffer)
        (web-mode-element-select))
      )

     (t
      (web-mode-element-select)
      ;;(mark-whole-buffer)
      )

     ) ; cond

;;    (message "after=%S" web-mode-expand-previous-state)

    ))

(defun web-mode-tag-select ()
  "Select the current HTML tag."
  (interactive)
  (let (beg)
    (setq beg (web-mode-tag-beginning-position (point)))
    (when beg
      (goto-char beg)
      (set-mark (point))
      (web-mode-tag-end)
      (exchange-point-and-mark)
      )
    beg))

(defun web-mode-element-content-select ()
  "Select the content of a HTML element."
  (interactive)
  (let (pos beg end)
    (web-mode-element-select)
    (when mark-active
      (setq pos (point))
      (deactivate-mark)
      (web-mode-tag-match)
      (setq end (point))
      (goto-char pos)
      (web-mode-tag-end)
      (set-mark (point))
      (goto-char end)
      (exchange-point-and-mark)
      )))

(defun web-mode-element-select ()
  "Select the current HTML element (including opening and closing tags)."
  (interactive)
  (let (type (pos (point)))
    (setq type (get-text-property pos 'tag-type))
    (if type
        (cond
         ((member type '(start void))
          (web-mode-tag-beginning)
          (set-mark (point))
          (web-mode-tag-match)
          (web-mode-tag-end)
          (exchange-point-and-mark))
         (t
          (web-mode-tag-match)
          (set-mark (point))
          (web-mode-tag-match)
          (web-mode-tag-end)
          (exchange-point-and-mark))
         );cond
      (web-mode-element-parent)
      (unless (= (point) pos) (web-mode-element-select))
      );if
    ))

(defun web-mode-element-delete ()
  "Delete the current HTML element."
  (interactive)
  (web-mode-element-select)
  (when mark-active
    (delete-region (region-beginning) (region-end))))

(defun web-mode-element-clone ()
  "Clone the current HTML element."
  (interactive)
  (let ((offset 0))
    (web-mode-element-select)
    (when mark-active
      (save-excursion
        (goto-char (region-beginning))
        (setq offset (current-column)))
      (kill-region (region-beginning) (region-end))
      (yank)
      (newline)
      (indent-line-to offset)
      (yank))))

(defun web-mode-element-rename ()
  "Rename the current HTML element."
  (interactive)
  (save-excursion
    (let (pos tag-name)
      (setq tag-name (read-from-minibuffer "Tag name? "))
      (when (and (> (length tag-name) 0)
                 (web-mode-element-beginning)
                 (looking-at "<\\([[:alnum:]]+\\)"))
        (setq pos (point))
        (unless (web-mode-is-void-element)
            (save-match-data
              (web-mode-tag-match)
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\)")
                  (replace-match (concat "</" tag-name))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag-name))
        (web-mode-scan-at-pos)
        ))))

(defun web-mode-current-trimmed-line ()
  "Line at point, trimmed."
  (web-mode-trim (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))

(defun web-mode-trim (string)
  "Remove white spaces in beginning and ending of STRING."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun web-mode-is-void-element (&optional tag)
  "Test if tag is a void tag."
  (if tag
      (car (member (downcase tag) web-mode-void-elements))
    (eq (get-text-property (point) 'tag-type) 'void)
    ))

(defun web-mode-fold-or-unfold ()
  "Toggle folding on an HTML element or a control block."
  (interactive)
  (web-mode-with-silent-modifications
   (save-excursion
     (let (beg-inside beg-outside end-inside end-outside overlay overlays regexp)
       (back-to-indentation)
       (setq overlays (overlays-at (point)))
       (cond
        ;; *** unfolding
        (overlays
         (setq overlay (car overlays))
         (setq beg-inside (overlay-start overlay)
               end-inside (overlay-end overlay))
         (remove-overlays beg-inside end-inside)
         (put-text-property beg-inside end-inside 'invisible nil)
         )
        ;; *** tag folding
        ((eq (get-text-property (point) 'tag-type) 'start)
         (setq beg-outside (point))
         (web-mode-tag-end)
         (setq beg-inside (point))
         (goto-char beg-outside)
         (when (web-mode-tag-match)
           (setq end-inside (point))
           (web-mode-tag-end)
           (setq end-outside (point)))
         )
        ;; *** block folding
        ((cdr (web-mode-is-active-block (point)))
         (setq beg-outside (point))
         (web-mode-block-end)
         (setq beg-inside (point))
         (goto-char beg-outside)
         (when (web-mode-tag-match)
           (setq end-inside (point))
           (web-mode-block-end)
           (setq end-outside (point)))
         )
        );cond
       (when end-outside
         ;;          (message "beg-out(%d) beg-in(%d) end-in(%d) end-out(%d)" beg-outside beg-inside end-inside end-outside)
         (setq overlay (make-overlay beg-outside end-outside))
         (overlay-put overlay 'face 'web-mode-folded-face)
         (put-text-property beg-inside end-inside 'invisible t)
         )
       ))))

(defun web-mode-toggle-comments ()
  "Toggle comments visbility"
  (interactive)
  (save-excursion
    (if web-mode-comments-invisible
        (remove-overlays))
    (setq web-mode-comments-invisible (null web-mode-comments-invisible))
    (let ((continue t)
          (pos (point-min))
          (visibility web-mode-comments-invisible)
          overlay end)
      (while continue
        (setq pos (next-single-property-change pos 'face))
        (if (null pos)
            (setq continue nil)
          (when (eq (get-text-property pos 'face) 'web-mode-comment-face)
            (setq end (next-single-property-change pos 'face))
            (put-text-property pos end 'invisible visibility)
            (when visibility
              (setq overlay (make-overlay pos end)))
            (goto-char pos)
            )
          )
        )
      );let
    ))

(defun web-mode-is-single-line-block (pos)
  "Is block at POS spread on a single line ?"
  (= (web-mode-line-number (web-mode-block-beginning-position pos))
     (web-mode-line-number (web-mode-block-end-position pos))))

(defun web-mode-comment-or-uncomment (&optional pos)
  "Comment or uncomment line(s), block or region at POS."
  (interactive)
  (unless pos (setq pos (if mark-active (region-beginning) (point))))
  (if (web-mode-is-comment)
      (web-mode-uncomment pos)
    (web-mode-comment pos))
  (web-mode-scan-region (point-min) (point-max)))

(defun web-mode-comment (pos)
  "Comment line(s) at point."
  (interactive)
  (save-excursion
    (let (ctx language sel beg end tmp block-side single-line-block)

      (setq block-side (get-text-property pos 'block-side))
      (setq single-line-block (web-mode-is-single-line-block pos))

      (cond

       ((and block-side
             (string= web-mode-engine "django")
             single-line-block)
        (web-mode-comment-django-block pos)
        )

       ((and block-side
             (string= web-mode-engine "dust")
             single-line-block)
        (web-mode-comment-dust-block pos)
        )

       ((and block-side
             (string= web-mode-engine "erb")
             single-line-block)
        (web-mode-comment-erb-block pos)
        )

       ((and block-side
             (string= web-mode-engine "aspx")
             single-line-block)
        (web-mode-comment-aspx-block pos)
        )

       ((and block-side
             (string= web-mode-engine "jsp")
             single-line-block)
        (web-mode-comment-jsp-block pos)
        )

       ((and block-side
             (string= web-mode-engine "go")
             single-line-block)
        (web-mode-comment-go-block pos)
        )

       ((and block-side
             (string= web-mode-engine "php")
             single-line-block)
        (web-mode-comment-php-block pos)
        )

       (t

        (setq ctx (web-mode-point-context
                   (if mark-active (region-beginning) (line-beginning-position))))
        ;;        (message "ctx=%S" ctx)
        (setq language (plist-get ctx :language))

        (if mark-active
            (setq beg (region-beginning)
                  end (region-end))
          (if (string= language "html")
              (progn
                (back-to-indentation)
                (web-mode-element-select))
            (end-of-line)
            (set-mark (line-beginning-position))
            );if
          (setq beg (region-beginning)
                end (region-end))
          ); if mark-active

        (when (> (point) (mark))
          (exchange-point-and-mark))

        (if (eq (char-before end) ?\n)
            (setq end (1- end)))

        ;;     (message "language=%S beg=%S end=%S" language beg end)
        (setq sel (web-mode-trim (buffer-substring-no-properties beg end)))
        ;;      (message "[language=%s] sel=%s" language sel)
        (delete-region beg end)
        (deactivate-mark)

        (cond

         ((string= language "html")

          (cond
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "django"))
            (web-mode-insert-and-indent (concat "{# " sel " #}"))
            )
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "erb"))
            (web-mode-insert-and-indent (concat "<%# " sel " %>"))
            )
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "aspx"))
            (web-mode-insert-and-indent (concat "<%-- " sel " --%>"))
            )
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "smarty"))
            (web-mode-insert-and-indent (concat "{* " sel " *}"))
            )
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "blade"))
            (web-mode-insert-and-indent (concat "{{-- " sel " --}}"))
            )
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "razor"))
            (web-mode-insert-and-indent (concat "@* " sel " *@"))
            )
           (t
            (web-mode-insert-and-indent (concat "<!-- " sel " -->"))
            )
           )

          )

         ((member language '("php" "javascript" "css"))
          (web-mode-insert-and-indent (concat "/* " sel " */")))

         (t
          (web-mode-insert-and-indent (concat "/* " sel " */")))

         );cond

        );t
       );cond

      )))

(defun web-mode-looking-at-pos (regexp pos)
  "Performs a looking-at at POS."
  (save-excursion
    (goto-char pos)
    (looking-at regexp)
    ))

(defun web-mode-insert-text-at-pos (text pos)
  "Insert TEXT at POS."
  (save-excursion
    (goto-char pos)
    (insert text)
    ))

(defun web-mode-remove-text-at-pos (n pos)
  "Remove N chars at POS."
  (delete-region pos (+ pos n))
  )

(defun web-mode-uncomment-erb-block (pos)
  "Uncomment an erb block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (+ beg 2))
    ))

(defun web-mode-comment-erb-block (pos)
  "Turn an erb block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" (+ beg 2))
    ))

(defun web-mode-uncomment-django-block (pos)
  "Uncomment a django block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (1- end))
    (web-mode-remove-text-at-pos 1 (1+ beg))
    ))

(defun web-mode-comment-django-block (pos)
  "Turn a django block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" end)
    (web-mode-insert-text-at-pos "#" (1+ beg))
    ))

(defun web-mode-uncomment-dust-block (pos)
  "Uncomment a dust block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (1- end))
    (web-mode-remove-text-at-pos 1 (1+ beg))
    ))

(defun web-mode-comment-dust-block (pos)
  "Turn a dust block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "!" end)
    (web-mode-insert-text-at-pos "!" (1+ beg))
    ))

(defun web-mode-comment-aspx-block (pos)
  "Turn a aspx block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" end)
    (web-mode-insert-text-at-pos "#" (1+ beg))
    ))

(defun web-mode-uncomment-aspx-block (pos)
  "Uncomment a aspx block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (1- end))
    (web-mode-remove-text-at-pos 1 (1+ beg))
    ))

(defun web-mode-uncomment-jsp-block (pos)
  "Uncomment a jsp block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 2 (+ beg 2))
    ))

(defun web-mode-comment-jsp-block (pos)
  "Turn a jsp block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "--" (+ beg 2))
    ))

(defun web-mode-uncomment-go-block (pos)
  "Uncomment a go block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (+ beg 2))
    ))

(defun web-mode-comment-go-block (pos)
  "Turn a go block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "/" (+ beg 2))
    ))

(defun web-mode-comment-php-block (pos)
  "Turn a php block into a comment block."
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "*/" (- end 1))
    (web-mode-insert-text-at-pos "/*" (+ beg (if (web-mode-looking-at-pos "<\\?php" beg) 5 3)))
    ))

(defun web-mode-uncomment (&optional pos)
  "Uncomment line(s) at point."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((beg pos)
        (end pos)
        (sub2 "")
        comment prop)

    (cond

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "django"))
        (web-mode-uncomment-django-block pos)
        )

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "dust"))
        (web-mode-uncomment-dust-block pos)
        )

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "erb"))
        (web-mode-uncomment-erb-block pos)
        )

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "aspx"))
      (web-mode-uncomment-aspx-block pos)
      )

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "jsp"))
      (web-mode-uncomment-jsp-block pos)
      )

     ((and (get-text-property pos 'block-side)
           (string= web-mode-engine "go"))
      (web-mode-uncomment-go-block pos)
      )

     (t

      (if (eq (get-text-property pos 'block-token) 'comment)
          (setq prop 'block-token)
        (setq prop 'part-token))

      (if (and (not (bobp))
               (eq (get-text-property pos prop) (get-text-property (- pos 1) prop)))
          (setq beg (previous-single-property-change pos prop)))

      (if (and (not (eobp))
               (eq (get-text-property pos prop) (get-text-property (+ pos 1) prop)))
          (setq end (next-single-property-change pos prop)))

      ;;    (message "beg(%d) end(%d)" beg end)

      (when (> (- end beg) 4)

        (setq comment (buffer-substring-no-properties beg end))
        ;;    (message "before[%s]" comment)

        (setq sub2 (substring comment 0 2))

        (cond

         ((member sub2 '("<!" "<%"))
          (setq comment (replace-regexp-in-string "\\(^<[!%]--[ ]?\\|[ ]?--[%]?>$\\)" "" comment)))

         ((string= sub2 "{#")
          (setq comment (replace-regexp-in-string "\\(^{#[ ]?\\|[ ]?#}$\\)" "" comment)))

         ((string= sub2 "/*")
          (setq comment (replace-regexp-in-string "\\(^/\\*[ ]?\\|[ ]?\\*/$\\)" "" comment)))

         ((string= sub2 "//")
          (setq comment (replace-regexp-in-string "\\(^//\\)" "" comment)))

         )

        ;;    (message "after[%s]" comment)

        (delete-region beg end)
        (web-mode-insert-and-indent comment)
        (goto-char beg)
;;        (back-to-indentation)

        );;when

      ))
    ))

(defun web-mode-snippet-names ()
  "Return list a snippet names."
  (interactive)
  (let (codes
        (counter 0)
        snippet
        (l (length web-mode-snippets)))
    (while (< counter l)
      (setq snippet (nth counter web-mode-snippets))
      (setq counter (1+ counter))
      (add-to-list 'codes (list (nth 0 snippet) counter)))
    ;;    (message "%S" codes)
    codes))

(defun web-mode-snippet-insert (code)
  "Insert snippet."
  (interactive
   (list (completing-read
          "Snippet: "
          (web-mode-snippet-names))))
  (let (beg
        (continue t)
        (counter 0)
        end
        sel
        snippet
        (l (length web-mode-snippets))
        pos)
    (when mark-active
      (setq sel (web-mode-trim
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end)))
    (while (and continue (< counter l))
      (setq snippet (nth counter web-mode-snippets))
      (when (string= (nth 0 snippet) code)
        (setq continue nil))
      (setq counter (1+ counter)))
    (when (and (null continue)
               (nth 1 snippet))
      (setq beg (point-at-bol))
      (insert (nth 1 snippet))
      (setq pos (point))
      (when sel
        (insert sel)
        (setq pos (point)))
      (if (nth 2 snippet) (insert (nth 2 snippet)))
      (setq end (point-at-eol))
      (goto-char pos)
      (indent-region beg end))
    ))

(defun web-mode-insert-and-indent (text)
  "Insert and indent text."
  (interactive)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)))

(defun web-mode-tag-match (&optional pos)
  "Match tag."
  (interactive)
  (unless pos (setq pos (point)))
  (let (init)
    (goto-char pos)
    (setq init (point))
    (when (> (current-indentation) (current-column))
      (back-to-indentation))
    (setq pos (point))
    (cond
     ((web-mode-is-comment-or-string)
      (goto-char init))
     ((and (get-text-property pos 'block-side)
           (web-mode-block-beginning)
           (looking-at-p web-mode-active-block-regexp))
      (funcall web-mode-engine-control-matcher))
     ((member (get-text-property pos 'tag-type) '(start end))
      (web-mode-tag-beginning)
      (web-mode-match-html-tag))
     (t
      (goto-char init))
     )
    ))

(defun web-mode-match-html-tag (&optional pos)
  "Fetch HTML block."
  (unless pos (setq pos (point)))
  (let (tag regexp)
    (setq tag (get-text-property pos 'tag-name))
    (setq regexp (concat "</?" tag))
    (if (eq (get-text-property pos 'tag-type) 'end)
        (web-mode-fetch-html-opening-tag regexp pos)
      (web-mode-fetch-html-closing-tag regexp pos))
    t))

(defun web-mode-fetch-html-opening-tag (regexp pos)
  "Fetch opening HTML block."
  (let ((counter 1) (n 0))
    (while (and (> counter 0) (re-search-backward regexp nil t))
      (when (get-text-property (point) 'tag-beg)
        (setq n (1+ n))
        (if (eq (get-text-property (point) 'tag-type) 'end)
            (setq counter (1+ counter))
          (setq counter (1- counter))))
      )
    (if (= n 0) (goto-char pos))
    ))

(defun web-mode-fetch-html-closing-tag (regexp pos)
  "Fetch closing HTML closing block."
  (let ((counter 1) (n 0))
    (web-mode-tag-end)
    (while (and (> counter 0) (re-search-forward regexp nil t))
      (when (get-text-property (match-beginning 0) 'tag-beg)
        (setq n (1+ n))
        (if (eq (get-text-property (point) 'tag-type) 'end)
            (setq counter (1- counter))
          (setq counter (1+ counter))))
      )
    (if (> n 0)
        (web-mode-tag-beginning)
      (goto-char pos))
    ))

(defun web-mode-match-php-block ()
  "Fetch PHP block."
  (let (regexp match)
    (cond

     ((looking-at-p "<\\?\\(php\\)?[ ]*}")
      (let (open)
        (search-forward "}")
        (backward-char)
        (setq open (web-mode-opening-paren-position))
        (when open
          (goto-char open)
          (web-mode-block-beginning)
          )
        ))

     ((looking-at-p "<\\?php.+{[ ]*\\?>")
      (let (close)
        (web-mode-block-end)
        (search-backward "{")
        (setq close (web-mode-closing-paren-position))
        (when close
          (goto-char close)
          (web-mode-block-beginning)
          )
        ))

     (t
      (looking-at web-mode-active-block-regexp)
      (setq match (match-string-no-properties 3))
      (setq regexp (concat "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?\\("
                         (if (member match '("else" "elseif")) "if" match)
                         "\\)"))
      (if (or (string= "end" (match-string-no-properties 2))
              (member match '("else" "elseif")))
          (web-mode-fetch-opening-php-block regexp)
        (web-mode-fetch-closing-php-block regexp))
      );t

     );cond
    t))

(defun web-mode-fetch-opening-php-block (regexp)
  "Fetch PHP opening block."
  (let ((counter 1))
    (while (and (> counter 0) (re-search-backward regexp nil t))
      (if (string= "end" (match-string-no-properties 2))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-php-block (regexp)
  "Fetch PHP closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (re-search-forward regexp nil t))
      (if (string= "end" (match-string-no-properties 2))
          (setq counter (1- counter))
        (setq counter (1+ counter))
        )
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-erb-block ()
  "Fetch ERB block."
  (let (regexp chunk)
    (setq chunk (buffer-substring-no-properties (+ (point) 3)
                                                (- (web-mode-block-end-position) 2)))
    (setq regexp web-mode-active-block-regexp)
    (if (string-match-p "else\\|end" chunk)
        (web-mode-fetch-opening-erb-block regexp)
      (web-mode-fetch-closing-erb-block regexp))
    t))

(defun web-mode-fetch-opening-erb-block (regexp)
  "Fetch erb opening block."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "else" match)
        )
       ((not (string= "end" match))
        (setq counter (1- counter)))
       (t
        (setq counter (1+ counter)))
       )
      )
    ))

(defun web-mode-fetch-closing-erb-block (regexp)
  "Fetch erb closing block."
  (let ((counter 1) match)
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "else" match)
        )
       ((not (string= "end" match))
        (setq counter (1+ counter)))
       (t
        (setq counter (1- counter)))
       )
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-blade-block ()
  "Fetch blade block."
  (let (beg end match regexp)
    (looking-at web-mode-active-block-regexp)
    (setq match (match-string-no-properties 2))
    (setq regexp (concat "@\\(end\\)?\\("
                         (if (member match '("else" "elseif")) "if" match)
                         "\\)"))
    (if (or (string= "end" (match-string-no-properties 1))
            (member match '("else" "elseif")))
        (web-mode-fetch-opening-blade-block regexp)
      (web-mode-fetch-closing-blade-block regexp))
    t))

(defun web-mode-fetch-opening-blade-block (regexp)
  "Fetch blade opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (string= "end" (match-string-no-properties 1))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-blade-block (regexp)
  "Fetch blade closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (string= "end" (match-string-no-properties 1))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (goto-char (match-beginning 0))
    ))

(defun web-mode-match-django-block ()
  "Fetch django block."
  (let (match regexp)
    (looking-at web-mode-active-block-regexp)
    (setq match (match-string-no-properties 2))
    (setq regexp (concat "{%[-]?[ ]+\\(end\\)?\\("
                         (if (member match '("else" "elseif" "elif")) "if" match)
                         "\\)"))
    (if (or (string= "end" (match-string-no-properties 1))
            (member match '("else" "elseif" "elif")))
        (web-mode-fetch-opening-django-block regexp)
      (web-mode-fetch-closing-django-block regexp))
    t))

(defun web-mode-fetch-opening-django-block (regexp)
  "Fetch django opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (string= "end" (match-string-no-properties 1))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-django-block (regexp)
  "Fetch django closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (string= "end" (match-string-no-properties 1))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-smarty-block ()
  "Fetch smarty block."
  (let (match regexp)
    (looking-at web-mode-active-block-regexp)
    (setq match (match-string-no-properties 1))
    (setq regexp (concat "{/?" (if (string= match "else") "if" match)))
    (if (or (char-equal ?\/ (aref (match-string-no-properties 0) 1))
            (string= match "else"))
        (web-mode-fetch-opening-smarty-block regexp)
      (web-mode-fetch-closing-smarty-block regexp))
    t))

(defun web-mode-fetch-opening-smarty-block (regexp)
  "Fetch smarty opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-smarty-block (regexp)
  "Fetch smarty closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-dust-block ()
  "Fetch dust block."
  (let (match regexp (continue t))
    (looking-at web-mode-active-block-regexp)
    (cond
     ((string= (match-string-no-properties 0) "{:else")
      (while continue
        (if (web-mode-block-previous)
            (when (cdr (web-mode-is-active-block (point)))
              (setq continue nil))
          (setq continue nil)
          )
        );while
      )
     (t
      (setq match (match-string-no-properties 1))
      (setq regexp (concat "{[#/:?@><+^]?" match))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (web-mode-fetch-opening-dust-block regexp)
        (web-mode-fetch-closing-dust-block regexp)))
     );cond
    t))

(defun web-mode-fetch-opening-dust-block (regexp)
  "Fetch dust opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-dust-block (regexp)
  "Fetch dust closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-closure-block ()
  "Fetch closure block."
  (let (match regexp (continue t))
    (looking-at web-mode-active-block-regexp)
    (cond
     ((member (match-string-no-properties 0) '("{else" "{elseif"))
      (while continue
        (if (web-mode-block-previous)
            (when (looking-at-p "{if")
              (setq continue nil))
          (setq continue nil)
          )
        );while
      )
     ((member (match-string-no-properties 0) '("{ifempty"))
      (while continue
        (if (web-mode-block-previous)
            (when (looking-at-p "{foreach")
              (setq continue nil))
          (setq continue nil)
          )
        );while
      )
     ((member (match-string-no-properties 0) '("{case" "{default"))
      (while continue
        (if (web-mode-block-previous)
            (when (looking-at-p "{switch")
              (setq continue nil))
          (setq continue nil)
          )
        );while
      )
     (t
      (setq match (match-string-no-properties 1))
      (setq regexp (concat "{/?" match))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (web-mode-fetch-opening-closure-block regexp)
        (web-mode-fetch-closing-closure-block regexp)))
     );cond
    t))

(defun web-mode-fetch-opening-closure-block (regexp)
  "Fetch closure opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-closure-block (regexp)
  "Fetch closure closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (web-mode-block-beginning)
    ))


(defun web-mode-match-velocity-block ()
  "Fetch velocity block."
  (let (regexp match)
    (looking-at web-mode-active-block-regexp)
    (setq match (match-string-no-properties 1))
    (setq regexp web-mode-active-block-regexp)
    (if (member match '("else" "elseif" "end"))
        (web-mode-fetch-opening-velocity-block regexp)
      (web-mode-fetch-closing-velocity-block regexp))
    t))

(defun web-mode-fetch-opening-velocity-block (regexp)
  "Fetch velocity opening block."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "end" match)
        (setq counter (1+ counter)))
       ((string= "else" match)
        )
       (t
        (setq counter (1- counter)))
       )
      )
    ))

(defun web-mode-fetch-closing-velocity-block (regexp)
  "Fetch velocity closing block."
  (let ((counter 1) match)
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "end" match)
        (setq counter (1- counter)))
       ((string= "else" match)
        )
       (t
        (setq counter (1+ counter)))
       )
      )
    (goto-char (match-beginning 0))
    ))

(defun web-mode-match-ctemplate-block ()
  "Fetch ctemplate block."
  (let (regexp)
    (looking-at web-mode-active-block-regexp)
    (setq regexp (concat "{{[#^/]" (match-string-no-properties 1)))
    (if (looking-at-p "{{/")
        (web-mode-fetch-opening-ctemplate-block regexp)
      (web-mode-fetch-closing-ctemplate-block regexp))
    t))

(defun web-mode-fetch-opening-ctemplate-block (regexp)
  "Fetch ctemplate opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 2))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-fetch-closing-ctemplate-block (regexp)
  "Fetch ctemplate closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (if (char-equal ?\/ (aref (match-string-no-properties 0) 2))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-go-block ()
  "Fetch go block."
  (let (regexp match)
    (looking-at web-mode-active-block-regexp)
    (setq match (match-string-no-properties 1))
    (setq regexp web-mode-active-block-regexp)
    (if (member match '("end" "else"))
        (web-mode-fetch-opening-go-block regexp)
      (web-mode-fetch-closing-go-block regexp))
    t))

(defun web-mode-fetch-opening-go-block (regexp)
  "Fetch go opening block."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "end" match)
        (setq counter (1+ counter)))
       ((string= "else" match)
        )
       (t
        (setq counter (1- counter)))
       )
      )
    ))

(defun web-mode-fetch-closing-go-block (regexp)
  "Fetch go closing block."
  (let ((counter 1) match)
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 1))
      (cond
       ((string= "end" match)
        (setq counter (1- counter))
        )
       ((string= "else" match)
        )
       (t
        (setq counter (1+ counter))
        )
       )
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-jsp-block ()
  "Fetch jsp block."
  (let (regexp)
    (looking-at web-mode-active-block-regexp)
    (setq regexp (concat "<\\(/?" (match-string-no-properties 1) "\\)\\>"))
    (if (char-equal ?\/ (aref (match-string-no-properties 0) 1))
        (web-mode-fetch-opening-jsp-block regexp)
      (web-mode-fetch-closing-jsp-block regexp))
    t))

(defun web-mode-fetch-opening-jsp-block (regexp)
  "Fetch jsp opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (cond
       ((char-equal ?\/ (aref (match-string-no-properties 1) 0))
        (setq counter (1+ counter)))
       (t
        (setq counter (1- counter)))
       )
      )
    ))

(defun web-mode-fetch-closing-jsp-block (regexp)
  "Fetch jsp closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (cond
       ((char-equal ?\/ (aref (match-string-no-properties 1) 0))
        (setq counter (1- counter)))
       (t
        (setq counter (1+ counter)))
       )
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-match-freemarker-block ()
  "Fetch freemarker block."
  (let (regexp match tag char)
    (looking-at "[<[]/?\\([[:alpha:]]+:[[:alpha:]]+\\|[@#][[:alpha:]._]+\\)")
    (setq match (match-string-no-properties 0)
          tag (match-string-no-properties 1)
          char (if (string= (substring (match-string-no-properties 0) 0 1) "<") "<" "\\["))

    (cond
     ((member tag '("#else" "#elseif"))
      (setq match (concat char "/#if")
            tag "#if")
      )
     ((string= tag "#break")
      (setq match (concat char "/#case")
            tag "#case"))
     );cond
    (setq regexp (concat char "\\(/?" tag "\\)\\>"))
;;    (message "tag=%S regexp=%S" tag regexp)
    (if (char-equal ?\/ (aref match 1))
        (web-mode-fetch-opening-freemarker-block regexp)
      (web-mode-fetch-closing-freemarker-block regexp))
    t))

(defun web-mode-fetch-opening-freemarker-block (regexp)
  "Fetch freemarker opening block."
  (let ((counter 1))
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (cond
       ((char-equal ?\/ (aref (match-string-no-properties 1) 0))
        (setq counter (1+ counter)))
       (t
        (setq counter (1- counter)))
       )
      )
    ))

(defun web-mode-fetch-closing-freemarker-block (regexp)
  "Fetch freemarker closing block."
  (let ((counter 1))
    (web-mode-block-end)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (cond
       ((char-equal ?\/ (aref (match-string-no-properties 1) 0))
        (setq counter (1- counter)))
       (t
        (setq counter (1+ counter)))
       )
      )
    (web-mode-block-beginning)
    ))

(defun web-mode-element-close ()
  "Close HTML element."
  (interactive)
  (let (jump epp ins tag)
    (setq epp (web-mode-element-parent-position))
    (when epp
      (setq tag (get-text-property epp 'tag-name))
      (cond
       ((looking-back "</")
        (setq ins tag))
       ((looking-back "<")
        (setq ins (concat "/" tag)))
       (t
        (setq ins (concat "</" tag)))
       )
      (unless (looking-at-p "[ ]*>")
        (setq ins (concat ins ">")))
      (insert ins)
      (save-excursion
        (search-backward "<")
        (setq jump (looking-back (concat "<" tag ">"))))
      (when jump
        (search-backward "<"))
      );when epp
    epp
    ))

(defun web-mode-on-after-change (beg end len)
  "Handles auto-pairing, auto-closing, and region-refresh after buffer alteration."

;;  (message "pos=%d, beg=%d, end=%d, len=%d, cur=%d" (point) beg end len (current-column))
;;  (backtrace)

  (setq web-mode-expand-initial-pos nil
        web-mode-expand-previous-state "")

  (let ((pos (point)) found chunk auto-closed)

    (if (not (= (point-max) (+ (buffer-size) 1)))

       (setq web-mode-is-narrowed t)

      ;;-- auto-closing and auto-pairing
      (when (and (> pos 3)
                 (not (get-text-property pos 'part-side))
                 (= len 0)
                 (= 1 (- end beg)))

        ;;-- auto-closing
        (when (and (> web-mode-tag-auto-close-style 0)
                   (setq chunk (buffer-substring-no-properties (- beg 1) end))
                   (or (and (= web-mode-tag-auto-close-style 2)
                            (string-match-p "[[:alnum:]]>" chunk))
                       (string= "</" chunk)))
          (when (web-mode-element-close)
            (setq auto-closed t
                  found t))
          )

        ;;-- auto-pairing
        (when (and (not web-mode-disable-auto-pairing)
                   (not found))
          (let ((i 0) expr after pos-end (l (length web-mode-auto-pairs)))
            (setq pos-end (if (> (+ end 10) (line-end-position))
                              (line-end-position)
                            (+ end 10)))
            (setq chunk (buffer-substring-no-properties (- beg 2) end)
                  after (buffer-substring-no-properties end pos-end))
            (while (and (< i l) (not found))
              (setq expr (elt web-mode-auto-pairs i))
              (when (string= (elt expr 0) chunk)
                (unless (string-match-p (elt expr 2) after)
                  (insert (elt expr 1))
                  (goto-char (+ pos (elt expr 3)))
                  (setq found t))
                );when
              (setq i (1+ i))
              );while
            );let
          );when

        );end auto-pairing auto-closing

      ;;-- region-refresh
      ;;      (save-match-data
      (web-mode-scan-region (or (web-mode-previous-tag-at-bol-pos beg)
                                (point-min))
                            (or (web-mode-next-tag-at-eol-pos end)
                                (point-max)))
      ;;        );save-match-data

      ;;-- auto-indentation
      (when (and (not web-mode-disable-auto-indentation)
                 (or auto-closed
                     (and (> end (point-min))
                          (get-text-property (1- end) 'tag-end)
                          (get-text-property (line-beginning-position) 'tag-beg))))
        (indent-for-tab-command)
        )

      );if narrowed
    ))

(defun web-mode-apostrophes-replace ()
  "Replace ' with ’."
  (interactive)
  (save-excursion
    (let ((min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-rsf-content "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" max)
        (replace-match "\\1’\\2")
        );while
      )))

(defun web-mode-entities-replace ()
  "Replace HTML entities ie. &eacute; becomes é"
  (interactive)
  (save-excursion
    (let (name pair (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-rsf-content "&\\([[:alpha:]]\\{2,8\\}\\);" max)
        (setq name (match-string 1))
        (setq pair (assoc name web-mode-html-entities))
;;        (message "pos=%S name=%S pair=%S" (point) name pair)
        (when pair
          (replace-match (cdr pair)))
        );while
      )))

(defun web-mode-xml-replace ()
  "Replace &, > and < in HTML content."
  (interactive)
  (save-excursion
    (let (expr (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-rsf-content "[&<>]" max)
        (replace-match (cdr (assq (char-before) web-mode-xml-chars)) t t))
      )))

(defun web-mode-quotes-replace ()
  "Replace dumb quotes."
  (interactive)
  (save-excursion
    (let (expr (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (setq expr (concat (car web-mode-smart-quotes) "\\2" (cdr web-mode-smart-quotes)))
      (while (web-mode-rsf-content "\\(\"\\)\\(.\\{1,200\\}\\)\\(\"\\)" max)
        (replace-match expr)
        );while
      )))

(defun web-mode-xpath (&optional pos)
  "HTML path"
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (let (path)
      (while (web-mode-element-parent (point))
        (setq path (cons (get-text-property (point) 'tag-name) path))
        )
      (message "/%s" (mapconcat 'identity path "/"))
      )))

;;-- position -----------------------------------------------------------------------

(defun web-mode-opening-paren-position (&optional pos limit)
  "Fetch opening paren."
  (save-restriction
    ;;    (unless paren (setq paren "("))
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let ((continue t)
          (n -1)
          block-side
          paren
          (pairs '((")" . "[)(]")
                   ("]" . "[\]\[]")
                   ("}" . "[}{]")))
          pt
          regexp)
      (setq paren (string (char-after)))
      ;;      (message "paren=%S" paren)
      (setq regexp (cdr (assoc paren pairs)))
      (if (null regexp) (setq continue nil))
      (setq block-side (get-text-property pos 'block-side))
      (while (and continue (re-search-backward regexp limit t))
        (unless (or (web-mode-is-comment-or-string)
                    (and block-side (not (get-text-property (point) 'block-side))))
          ;;          (message "pos=%S pt=%S" pos (point))
          (if (not (string= (string (char-after)) paren))
              (progn
                (setq n (1+ n))
                (if (= n 0)
                    (setq continue nil
                          pt (point))))
            (setq n (1- n)))
          ;;          (message "n=%S" n)
          );unless
        )
      pt
      )))

(defun web-mode-closing-paren-position (&optional pos limit)
  "Fetch opening paren."
  (save-excursion
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let ((continue t)
          paren
          (n 0)
          (pairs '(("(" . "[)(]")
                   ("[" . "[\]\[]")
                   ("{" . "[}{]")))
          pt
          block-side
          regexp)
      (setq paren (string (char-after)))
      (setq regexp (cdr (assoc paren pairs)))
      (if (null regexp) (setq continue nil))
      (setq block-side (get-text-property pos 'block-side))
      (while (and continue (re-search-forward regexp limit t))
        (unless (or (web-mode-is-comment-or-string)
                    (and block-side (not (get-text-property (point) 'block-side))))
          ;;          (message "char-before=%S pt=%S" (string (char-before)) (point))
          (if (string= (string (char-before)) paren)
              (setq n (1+ n))
            (setq n (1- n))
            (when (= n 0)
              (setq continue nil
                    pt (1- (point))))
            )
          ;;        (message "pt=%S char=%S n=%S" (point) (string (char-before)) n)
          )
        )
      ;;      (message "n=%S pt=%S" n pt)
      pt
      )))

(defun web-mode-opening-paren-block-position (pos limit)
  "Is opened code line."
  (save-excursion
    (goto-char pos)
    (let (c
          n
          pt
          (continue t)
          (pairs '((")" . "(")
                   ("]" . "[")
                   ("}" . "{")))
          (h (make-hash-table :test 'equal))
          (regexp "[\]\[)(}{]"))
      (while (and continue (re-search-backward regexp limit t))
        (unless (web-mode-is-comment-or-string)
          (setq c (string (char-after)))
          (cond
           ((member c '("(" "{" "["))
            (setq n (gethash c h 0))
            (if (= n 0)
                (setq continue nil
                      pt (point))
              (puthash c (1+ n) h)
              ))
           (t
            (setq c (cdr (assoc c pairs)))
            (setq n (gethash c h 0))
            (puthash c (1- n) h))
           );cond
          );unless
        );while
      ;;      (message "h=%S pt=%S" h pt)
      pt
      )))

(defun web-mode-previous-tag-at-bol-pos (pos)
  "Line beginning with and HTML tag. BOL is returned or nil."
  (save-excursion
    (goto-char pos)
    (setq pos nil)
    (let ((continue t))
      (back-to-indentation)
      (if (get-text-property (point) 'tag-beg)
          (setq pos (line-beginning-position))
        (while continue
          (forward-line -1)
          (setq pos (point))
          (when (bobp)
            (setq continue nil))
          (back-to-indentation)
          (if (get-text-property (point) 'tag-beg)
              (setq continue nil)
            (setq pos nil))
          );while
        );if
      pos)))

(defun web-mode-next-tag-at-eol-pos (pos)
  "Line ending with an HTML tag. EOL is returned or nil."
  (save-excursion
    (goto-char pos)
    (let ((continue t))
      (while continue
        (end-of-line)
        (setq pos (point))
        (when (eobp)
          (setq continue nil))
        (skip-chars-backward " ")
        (if (and (> (point) (point-min))
                 (get-text-property (1- (point)) 'tag-end))
            (setq continue nil)
          (setq pos nil))
        (if continue (forward-line))
        );while
      pos)))

(defun web-mode-tag-match-position (&optional pos)
  "Match tag position."
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-tag-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-tag-beginning-position (&optional pos)
  "Beginning position of the current tag. POINT is at <."
  (unless pos (setq pos (point)))
  (let (beg)
    (cond
     ((get-text-property pos 'tag-beg)
      (setq beg pos))
     ((get-text-property pos 'tag-name)
      (setq beg (1- (previous-single-property-change pos 'tag-beg)))
      (when (not (get-text-property beg 'tag-beg))
        (setq beg nil)))
     (t
      (setq beg nil))
     );cond
    beg))

(defun web-mode-tag-end-position (&optional pos)
  "End position of the current tag. POINT is at >."
  (unless pos (setq pos (point)))
  (let (end)
    (cond
     ((get-text-property pos 'tag-end)
      (setq end pos))
     ((get-text-property pos 'tag-name)
      (setq end (next-single-property-change pos 'tag-end))
      (when (not (get-text-property end 'tag-end))
        (setq end nil)))
     (t
      (setq end nil))
     );cond
    end))

(defun web-mode-element-beginning-position (&optional pos)
  "Beginning of element pos."
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (web-mode-element-parent-position)))
   ((eq (get-text-property pos 'tag-type) 'end)
    (setq pos (web-mode-tag-match-position pos))
    (setq pos (if (get-text-property pos 'tag-beg) pos nil)))
   ((member (get-text-property pos 'tag-type) '(start void))
    (setq pos (web-mode-tag-beginning-position pos)))
   (t
    (setq pos nil))
   );cond
  pos)

(defun web-mode-element-end-position (&optional pos)
  "End of element pos."
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (web-mode-element-parent-position pos))
    (when pos
      (setq pos (web-mode-tag-match-position pos))
      (when pos (setq pos (web-mode-tag-end-position pos)))
      )
    )
   ((member (get-text-property pos 'tag-type) '(end void))
    (setq pos (web-mode-tag-end-position pos))
    )
   ((member (get-text-property pos 'tag-type) '(start))
    (setq pos (web-mode-tag-match-position pos))
    (when pos (setq pos (web-mode-tag-end-position pos))))
   (t
    (setq pos nil))
   );cond
  pos)

(defun web-mode-element-child-position (&optional pos)
  "Child element pos."
  (save-excursion
    (let (child close)
      (unless pos (setq pos (point)))
      (goto-char pos)
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (web-mode-match-html-tag)
        (setq close (point))
        (goto-char pos)
        )
       ((eq (get-text-property pos 'tag-type) 'void)
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (web-mode-tag-beginning)
        (setq close (point))
        (web-mode-match-html-tag)
        )
       ((web-mode-element-parent-position pos)
        (setq pos (point))
        (web-mode-match-html-tag)
        (setq close (point))
        (goto-char pos)
        )
       );cond
      (when (and close
                 (web-mode-element-next)
                 (< (point) close))
        (setq child (point))
        )
      child
      )))

(defun web-mode-element-parent-position (&optional pos)
  "Parent element pos."
  (let (n
        tag-type
        tag-name
        (continue t)
        (h (make-hash-table :test 'equal)))
    (save-excursion
      (if pos (goto-char pos))
      (while (and continue (web-mode-tag-previous))
        (setq pos (point))
        (setq tag-type (get-text-property pos 'tag-type)
              tag-name (get-text-property pos 'tag-name))
        (setq n (gethash tag-name h 0))
        (when (member tag-type '(end start))
          (if (eq tag-type 'end)
              (puthash tag-name (1- n) h)
            (puthash tag-name (1+ n) h)
            (when (= n 0) (setq continue nil))
            );if
          );when
        );while
      );save-excursion
    (if (null continue) pos nil)
    ))

(defun web-mode-block-beginning-position (&optional pos)
  "web-mode-block-beginning-position"
  (unless pos (setq pos (point)))
;;  (message "web-mode-block-beginning-position=%S" pos)
  (cond
   ((or (and (get-text-property pos 'block-side)
             (= pos (point-min)))
        (get-text-property pos 'block-beg))
    )
   ((and (> pos (point-min))
         (get-text-property (1- pos) 'block-beg))
    (setq pos (1- pos))
    )
   ((get-text-property pos 'block-side)
    (setq pos (previous-single-property-change pos 'block-beg))
    (setq pos (if pos (1- pos) (point-min)))
;;    (setq pos (if pos pos (point-min)))
    )
   (t
    (setq pos nil))
   );cond
;;  (message "web-mode-block-beginning-position=%S" pos)
  pos)

(defun web-mode-block-end-position (&optional pos)
  "web-mode-block-end-position"
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-end)
    )
   ((get-text-property pos 'block-side)
    (setq pos (or (next-single-property-change pos 'block-end)
                  (point-max)))
    )
   (t
    (setq pos nil))
   );cond
  pos)

(defun web-mode-block-previous-position (&optional pos)
  "web-mode-block-previous-position"
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-side)
    (setq pos (web-mode-block-beginning-position pos))
    (when (and pos (> pos (point-min)))
      (setq pos (1- pos))
      (while (and (> pos (point-min))
                  (eq (char-after pos) ?\n))
        (setq pos (1- pos))
        )
      ;;            (message "pos=%S  <%c>" pos (char-after pos))
      (if (get-text-property pos 'block-side)
          (setq pos (web-mode-block-beginning-position pos))
        (setq pos (previous-single-property-change pos 'block-side))
        (when (and pos (> pos (point-min)))
          (setq pos (web-mode-block-beginning-position (1- pos))))
        );if
      );when
    )
   (t
    (setq pos (previous-single-property-change pos 'block-side))
    (when (and pos (> pos (point-min)))
      (setq pos (web-mode-block-beginning-position (1- pos))))
    )
   );conf
  pos)

(defun web-mode-block-next-position (&optional pos)
  "web-mode-block-next-position"
  (unless pos (setq pos (point)))
  (if (get-text-property pos 'block-side)
      (if (= pos (point-min))
          (set pos (point-min))
        (setq pos (web-mode-block-end-position pos))
        (when (and pos (> (point-max) pos))
          (setq pos (1+ pos))
          (if (not (get-text-property pos 'block-side))
              (setq pos (next-single-property-change pos 'block-side)))
          );when
        )
    (setq pos (next-single-property-change pos 'block-side)))
  pos)

;;--- /positions

;;--- nav

(defun web-mode-tag-beginning (&optional pos)
  "Fetch html tag beg."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-tag-beginning-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-tag-end (&optional pos)
  "Fetch html tag end."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-tag-end-position pos))
  (when pos
    (setq pos (1+ pos))
    (goto-char pos))
  pos)

;; TODO : utiliser property tag-beg
(defun web-mode-start-tag-previous (&optional regexp)
  "Fetch previous start tag."
  (interactive)
  (unless regexp (setq regexp web-mode-start-tag-regexp))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp nil t))
      (if (or (null ret)
              (get-text-property (point) 'tag-beg)
              (eq (get-text-property (point) 'tag-type) 'start))
          (setq continue nil))
      );while
    ret))

(defun web-mode-tag-previous (&optional regexp)
  "Fetch previous tag."
  (interactive)
  (unless regexp (setq regexp web-mode-tag-regexp))
;;  (unless regexp (setq regexp "</?[[:alpha:]]"))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp nil t))
      (if (or (null ret)
              (get-text-property (point) 'tag-beg))
          (setq continue nil))
      );while
    ret))

(defun web-mode-tag-next (&optional pos)
  "Fetch next tag. Might be HTML comment or server tag (ie. JSP)."
  (interactive)
  (unless pos (setq pos (point)))
  (when (get-text-property pos 'tag-beg)
    (setq pos (1+ pos)))
  (setq pos (next-single-property-change pos 'tag-beg))
  (when pos (goto-char pos))
  pos)

(defun web-mode-element-previous ()
  "Fetch previous element."
  (interactive)
  (web-mode-tag-previous "<[[:alpha:]]"))

(defun web-mode-element-next ()
  "Fetch next element."
  (interactive)
  (let (continue ret (pos (point)) (props '(start void)))
    (setq continue (not (eobp)))
    (while continue
      (setq ret (web-mode-tag-next))
      (when (or (null ret)
                (member (get-text-property (point) 'tag-type) props))
        (setq continue nil)
        )
      );while
    (unless ret (goto-char pos))
    ret))

(defun web-mode-element-sibling-next ()
  "Fetch next element."
  (interactive)
  (let (parent ret (pos (point)))
    (save-excursion
      (cond
       ((not (get-text-property pos 'tag-type))
        (when (and (web-mode-element-parent)
                   (web-mode-match-html-tag)
                   (web-mode-element-next))
          (setq ret (point))
          )
        )
       ((eq (get-text-property pos 'tag-type) 'start)
        (when (and (web-mode-match-html-tag)
                   (web-mode-element-next))
          (setq ret (point))
          )
        )
       (t
        (when (web-mode-element-next)
          (setq ret (point))
          )
        )
       );cond

      );save
    (if ret (goto-char ret))
    ))


(defun web-mode-element-beginning (&optional pos)
  "Move to beginning of element."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-element-beginning-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-element-end (&optional pos)
  "Move to end of element."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-element-end-position pos))
  (when pos
    (setq pos (1+ pos))
    (goto-char pos))
  pos)

(defun web-mode-element-parent (&optional pos)
  "Fetch parent element."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-element-parent-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-element-child (&optional pos)
  "Fetch child element."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-element-child-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-element-traverse ()
  "Traverse html dom tree."
  (interactive)
  (cond
   ((web-mode-element-child)
    )
   ((web-mode-element-sibling-next)
    )
   ((web-mode-element-parent)
    (unless (web-mode-element-sibling-next)
      (goto-char (point-min)))
    )
   );cond
  )

(defun web-mode-block-previous (&optional pos)
  "web-mode-prev-server-block"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-previous-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-block-next (&optional pos)
  "web-mode-next-server-block"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-next-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-block-beginning (&optional pos)
  "web-mode-block-beg"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-beginning-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-block-end (&optional pos)
  "web-mode-block-beg"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-end-position pos))
  (when pos
    (setq pos (1+ pos))
    (goto-char pos))
  pos)

;;--- /nav ----------------------------------------------------------------------

;;--- search

(defun web-mode-rsf-balanced (regexp-open regexp-close &optional limit noerror)
  "web-mode-rsf-balanced in client."
  (unless noerror (setq noerror t))
  (let ((continue t)
        (level 1)
        ret
        (regexp (concat regexp-open "\\|" regexp-close)))
;;    (message "regexp=%S" regexp)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
;;      (message "regexp=%S ret=%S pos=%S" regexp ret (point))
      (cond
       ((null ret)
        (setq continue nil)
        )
       (t
;;        (message "%S" (match-string-no-properties 0))
        (if (string-match-p regexp-open (match-string-no-properties 0))
            (setq level (1+ level))
          (setq level (1- level)))
        (when (< level 1)
          (setq continue nil)
          )
        );t
       );cond
      );while
    ret))

(defun web-mode-rsb-client (regexp &optional limit noerror)
  "re-search-backward in client."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (get-text-property (point) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun web-mode-rsf-client (regexp &optional limit noerror)
  "re-search-forward in client."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (get-text-property (match-beginning 0) 'block-side)))
          (setq continue nil))
      );while
    ret))

(defun web-mode-sf-client (expr &optional limit noerror)
  "search-forward in client."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (get-text-property (- (point) (length expr)) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun web-mode-rsb (regexp &optional limit noerror)
  "re-search-backward not in comment or string."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-rsf (regexp &optional limit noerror)
  "re-search-forward not in comment or string."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil))
      )
    ret))

(defun web-mode-sb (expr &optional limit noerror)
  "re-search-backward not in comment or string."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-sf (expr &optional limit noerror)
  "re-search-backward not in comment or string."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-rsb-html (regexp &optional limit noerror)
  "re-search-backward only in html."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-part-token-or-server)))
          (setq continue nil)))
    ret))

(defun web-mode-rsf-html (regexp &optional limit noerror)
  "re-search-forward only in html."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-part-token-or-server)))
          (setq continue nil)))
    ret))

(defun web-mode-rsf-content (regexp &optional limit noerror)
  "re-search-forward only in html content."
  (unless noerror (setq noerror t))
  (let ((continue t) ret beg end)
    (while continue
      (setq ret (re-search-forward regexp limit noerror)
            beg (if (null ret) (point) (match-beginning 0))
            end (if (null ret) (point) (match-end 0)))
;;      (message "pt=%S" pos)
      (if (or (null ret)
              (and (web-mode-is-html-text beg)
                   (web-mode-is-html-text end)))
          (setq continue nil)))
    ret))

(defun web-mode-is-html-tag (&optional pos)
  "Is point in an html tag."
  (unless pos (setq pos (point)))
  (member (get-text-property pos 'tag-type) '(start end void)))

(defun web-mode-is-comment-or-string-line ()
  "Detect if current line is in a comment or in a string."
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-comment-or-string)
            (setq counter (1+ counter))
          (when (not (char-equal ?\s (following-char)))
            (setq continue nil
                  counter 0))
          );if
        (forward-char)
        );while
      (> counter 0)
      )))

(defun web-mode-is-part-token-or-server (&optional pos)
  "Detect if POS is in a comment, a string or in server script."
  (unless pos (setq pos (point)))
  (or (get-text-property pos 'block-side)
      (not (null (member (get-text-property pos 'part-token) '(string comment))))))

(defun web-mode-is-part-token-line ()
  "Detect if current line has only client tokens (string/comment) or server blocks."
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-part-token-or-server)
            (setq counter (1+ counter))
          (when (not (char-equal ?\s (following-char)))
            (setq continue nil
                  counter 0))
          );if
        (forward-char)
        );while
      (> counter 0)
      )))

(defun web-mode-is-html-text (&optional pos)
  "Is point in a html text."
  (unless pos (setq pos (point)))
  (not (or (get-text-property pos 'part-side)
           (get-text-property pos 'tag-type)
           (get-text-property pos 'block-side)
           )))

(defun web-mode-is-comment-or-string (&optional pos)
  "Detect if point is in a comment or in a string."
  (unless pos (setq pos (point)))
  (or (memq (get-text-property pos 'block-token) '(string comment))
      (memq (get-text-property pos 'part-token) '(string comment))))

(defun web-mode-is-comment (&optional pos)
  "Detect if point is in a comment."
  (unless pos (setq pos (point)))
  (or (eq (get-text-property pos 'block-token) 'comment)
      (eq (get-text-property pos 'part-token) 'comment)))

;;--- end search

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (web-mode-with-silent-modifications
   (setq web-mode-time nil)
   (put-text-property (point-min) (point-max) 'invisible nil)
   (remove-overlays)
   (unload-feature 'web-mode)
   (setq web-mode-disable-css-colorization t)
   (web-mode)
   (if (fboundp 'web-mode-hook)
       (web-mode-hook))))

(defun web-mode-trace (msg)
  "Benchmark."
  (interactive)
  (when nil
    (when (null web-mode-time) (setq web-mode-time (current-time)))
    (setq sub (time-subtract (current-time) web-mode-time))
    (message "%18s: time elapsed = %Ss %9Sµs" msg (nth 1 sub) (nth 2 sub))
    ))

(provide 'web-mode)

;;; web-mode.el ends here




;; (defvar web-mode-php-control-regexp
;;   (concat "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?" (regexp-opt web-mode-php-controls t))
;;   "PHP control regexp")

;; (defvar web-mode-erb-control-regexp
;;   "<%[-]?[ ]+\\(.* do \\|for\\|unless\\|end\\|if\\|else\\)"
;;   "ERB control regexp")

;; (defvar web-mode-go-control-regexp
;;   (concat "{{[ ]*" (regexp-opt web-mode-go-controls t))
;;   "Go control regexp")


;; (defvar web-mode-blade-control-regexp
;;   (concat "@\\(end\\)?" (regexp-opt web-mode-blade-controls t))
;;   "Blade control regexp")

;; (defvar web-mode-django-control-regexp
;;   (concat "{%[-]?[ ]+\\(end\\)?" (regexp-opt web-mode-django-controls t))
;;   "Django controls regexp.")

;; (defvar web-mode-ctemplate-control-regexp
;;   "{{[#^/]\\([[:alnum:]_]+\\)"
;;   "Ctemplate control regexp.")

;; (defvar web-mode-dust-control-regexp
;;   "{[#/:?@><+^]\\([[:alpha:]_]+\\)"
;;   "Dust control regexp.")

;; (defvar web-mode-smarty-control-regexp
;;   (concat "{/?" (regexp-opt web-mode-smarty-controls t))
;;   "Smarty control regexp.")

;; (defvar web-mode-velocity-control-regexp
;;   (concat "#" (regexp-opt web-mode-velocity-controls t))
;;   "Velocity control regexp.")
