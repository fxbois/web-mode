;;; web-mode.el --- major mode for editing html templates
;;; -*- coding: utf-8 -*-

;; Copyright 2011-2014 François-Xavier Bois

;; Version: 8.0.23
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

;;better switch / case indentation
;;new lexer/highlighter : web-mode.el is now compatible with minor modes relying on font-locking
;;compatibility with  *.js.erb (javascript content type), *.css.erb (css content type)
;;engine compatibility : web2py (python), mako (python), mason (perl)
;;jshint compatibility : web-mode-jshint
;;alertnative delimiters can be defined for smarty (see web-mode-engines-alternate-delimiters)
;;delimiters highlighting is more robust (same loop than string/comment highlighting)

;;todo :
;;       C-n sur delimiter = on bascule sur open-delim ou close-delim
;;       essayer de réduire la zone à scanner / repeindre
;;       phphint
;;       colorer : <a href=" >

;;todo : Stickiness of Text Properties
;;todo : web-mode-engine-real-name
;;todo : finir filling
;;todo : screenshot : http://www.cockos.com/licecap/
;;todo : better default colors for tags & attrs
;;todo : passer les content-types en symboles
;;todo : tester shortcut A -> pour pomme
;;todo : commentaire d'une ligne ruby ou d'une ligne asp
;;todo : créer tag-token pour différentier de part-token : tag-token=attr,comment ???

(defconst web-mode-version "8.0.23"
  "Web Mode version.")

(defgroup web-mode nil
  "Major mode for editing web templates:
   HTML files embedding parts (CSS/JavaScript)
   and blocks (php, erb, django/twig, smarty, jsp, asp, etc.)"
  :group 'languages
  :prefix "web-"
  :link '(url-link :tag "Site" "http://web-mode.org")
  :link '(url-link :tag "Repository" "https://github.com/fxbois/web-mode"))

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

(defcustom web-mode-disable-auto-opening (not (display-graphic-p))
  "Disable html element auto-opening."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-current-element-highlight nil
  "Disable element highlight."
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

(defcustom web-mode-enable-string-interpolation t
  "Enable string interpolation fontification (php and erb)."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-heredoc-fontification t
  "Enable heredoc fontification. The identifier should contain JS, JAVASCRIPT or HTML."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-comment-keywords nil
  "Enable highlight of keywords like FIXME, TODO, etc. in comments."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-comment-style 1
  "Comment style : 1 = default, 2 = force server comments outside a block."
  :group 'web-mode
  :type '(choice (const :tag "default" 1)
                 (const :tag "force engine comments" 2)))

(defcustom web-mode-indent-style 2
  "Indentation style.
with value 2, HTML lines beginning text are also indented (do not forget side effects, e.g. content of a textarea)."
  :group 'web-mode
  :type '(choice (const :tag "default" 2)
                 (const :tag "text at the beginning of line is now indented" 1)))

(defcustom web-mode-tag-auto-close-style 1
  "Tag auto-close style:
0=no auto-closing
1=auto-close with </
2=auto-close with > and </."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-extra-auto-pairs '()
  "A list of additional snippets."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-snippets '()
  "A list of additional snippets."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-python-constants '()
  "A list of additional strings to treat as Python constants."
  :type 'list
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

(defcustom web-mode-extra-mason-keywords '()
  "A list of additional strings to treat as Mason keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-asp-constants '()
  "A list of additional strings to treat as ASP constants."
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

(defface web-mode-error-face
  '((t :background "red"))
  "Face for warning."
  :group 'web-mode-faces)

(defface web-mode-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for warning."
  :group 'web-mode-faces)

(defface web-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-block-delimiter-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for block delimiters."
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
  '((((class color) (min-colors 88) (background dark)) :foreground "Snow4")
    (((class color) (min-colors 88) (background light)) :foreground "grey15")
    (((class color) (min-colors 16) (background dark)) :foreground "Snow4")
    (((class color) (min-colors 16) (background light)) :foreground "grey15")
    (((class color) (min-colors 8)) :foreground "Snow4")
    (((type tty) (class mono)) :inverse-video t)
    (t :foreground "Snow4"))
  "Face for HTML tags."
  :group 'web-mode-faces)

(defface web-mode-html-tag-custom-face
  '((t :inherit web-mode-html-tag-face))
  "Face for HTML custom tags (e.g. <polymer-element>)."
  :group 'web-mode-faces)

(defface web-mode-html-tag-bracket-face
;;  '((t :inherit web-mode-html-tag-face))
  '((((class color) (min-colors 88) (background dark)) :foreground "Snow3")
    (((class color) (min-colors 88) (background light)) :foreground "grey14")
    (((class color) (min-colors 16) (background dark)) :foreground "Snow3")
    (((class color) (min-colors 16) (background light)) :foreground "grey14")
    (((class color) (min-colors 8)) :foreground "Snow3")
    (((type tty) (class mono)) :inverse-video t)
    (t :foreground "Snow3"))
  "Face for HTML tags angle brackets (< and >)."
  :group 'web-mode-faces)

(defface web-mode-html-attr-name-face
  '((((class color) (min-colors 88) (background dark)) :foreground "Snow3")
    (((class color) (min-colors 88) (background light)) :foreground "grey13")
    (((class color) (min-colors 16) (background dark)) :foreground "Snow3")
    (((class color) (min-colors 16) (background light)) :foreground "grey13")
    (((class color) (min-colors 8)) :foreground "Snow3")
    (((type tty) (class mono)) :inverse-video t)
    (t :foreground "Snow4"))
  "Face for HTML attribute names."
  :group 'web-mode-faces)

(defface web-mode-html-attr-custom-face
  '((t :inherit web-mode-html-attr-name-face))
  "Face for custom attribute names (e.g. data-*)."
  :group 'web-mode-faces)

(defface web-mode-html-attr-equal-face
  '((t :inherit web-mode-html-attr-name-face))
  "Face for the = character between name and value."
  :group 'web-mode-faces)

(defface web-mode-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for HTML attribute values."
  :group 'web-mode-faces)

(defface web-mode-block-attr-name-face
  '((t :foreground "#8fbc8f")) ;; inherit web-mode-html-attr-name-face))
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

(defface web-mode-function-call-face
  '((t :inherit font-lock-function-name-face))
  "Face for function calls."
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

(defface web-mode-json-comment-face
  '((t :inherit web-mode-comment-face))
  "Face for json comments."
  :group 'web-mode-faces)

(defface web-mode-javascript-comment-face
  '((t :inherit web-mode-comment-face))
  "Face for javascript comments."
  :group 'web-mode-faces)

(defface web-mode-css-comment-face
  '((t :inherit web-mode-comment-face))
  "Face for css comments."
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
     :background "black") ;""grey18")
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

(defface web-mode-current-element-highlight-face
  '((t :background "#000000"))
  "Overlay face for element highlight."
  :group 'web-mode-faces)

(defface web-mode-comment-keyword-face
  '((t :weight bold :box t))
  "Comment keywords."
  :group 'web-mode-faces)

(defvar font-lock-beg)
(defvar font-lock-end)

(defvar web-mode-cache '()
  "Cache computed values.")

(defvar web-mode-void-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr")
  "Void (self-closing) tags.")

(defvar web-mode-scan-properties
  (list 'tag-beg nil 'tag-end nil 'tag-name nil 'tag-type nil 'tag-attr nil 'tag-attr-end nil
        'part-side nil 'part-token nil
        'block-side nil 'block-controls nil 'block-token nil 'block-beg nil 'block-end nil)
  "Text properties used for tokens.")

(defvar web-mode-scan-properties2
  (list 'tag-beg nil 'tag-end nil 'tag-name nil 'tag-type nil 'tag-attr nil 'tag-attr-end nil
        'part-side nil 'part-token nil)
  "Text properties used for tokens.")

(defvar web-mode-large-embed-threshold 512
  "Threshold for large part/block.")

(defvar web-mode-has-any-large-part nil
  "Does the current buffer has large parts ?")

(defvar web-mode-has-any-large-block nil
  "Does the current buffer has large blocks ?")

(defvar web-mode-is-scratch nil
  "Is scratch buffer ?")

(defvar web-mode-time nil
  "For benchmarking")

(defvar web-mode-start-tag-overlay nil)

(defvar web-mode-end-tag-overlay nil)

(defvar web-mode-expand-initial-pos nil
  "First mark pos.")

(defvar web-mode-expand-previous-state ""
  "Last mark state.")

(defvar web-mode-tag-regexp "<\\(/?[[:alpha:]][[:alnum:]-]*\\)"
  "Regular expression for HTML/XML tag.")

(defvar web-mode-start-tag-regexp "<\\([[:alpha:]][[:alnum:]-]*\\)"
  "Regular expression for HTML/XML start tag.")

(defvar web-mode-whitespaces-regexp
  "^[ \t]\\{2,\\}$\\| \t\\|\t \\|[ \t]+$\\|^[ \n\t]+\\'\\|^[ \t]?[\n]\\{2,\\}"
  "Regular expression for whitespaces.")

(defvar web-mode-engine nil
  "Template engine")

(defvar web-mode-engine-font-lock-keywords nil
  "Font-lock keywords associated with the engine.")

(defvar web-mode-engines
  '(("angular"    . ("angular.js" "angularjs"))
    ("asp"        . ())
    ("aspx"       . ())
    ("blade"      . ("laravel"))
    ("closure"    . ("soy"))
    ("ctemplate"  . ("mustache" "handlebars" "hapax" "ngtemplate" "ember"
                     "kite" "meteor"))
    ("django"     . ("dtl" "twig" "swig" "jinja" "jinja2" "erlydtl" "liquid"))
    ("dust"       . ("dustjs"))
    ("erb"        . ("eruby" "erubis"))
    ("go"         . ("gtl"))
    ("jsp"        . ())
    ("mason"      . ("poet"))
    ("python"     . ())
    ("razor"      . ("play" "play2"))
    ("underscore" . ("underscorejs"))
    ("velocity"   . ("vtl" "cheetah"))
    ("web2py"     . ()))
  "Engine name aliases")

(defvar web-mode-content-types
  '(("css"        . "\\.css\\'\\|\\.css\\.erb\\'")
    ("javascript" . "\\.js\\'\\|\\.js\\.erb\\'")
    ("json"       . "\\.\\(json\\|jsonld\\)\\'")
    ("html"       . "."))
  "content types")

(defvar web-mode-engine-file-regexps
  '(("asp"              . "\\.asp\\'")
    ("aspx"             . "\\.as[cp]x\\'")
    ("angular"          . "angular")
    ("blade"            . "\\.blade")
    ("closure"          . "\\.soy\\'")
    ("ctemplate"        . "\\.\\(chtml\\)\\'")
    ("django"           . "\\.\\(djhtml\\|tmpl\\|dtl\\|liquid\\)\\'")
    ("django"           . "twig")
    ("dust"             . "\\.dust\\'")
    ("erb"              . "\\.\\(erb\\|rhtml\\)\\'")
    ("freemarker"       . "\\.ftl\\'")
    ("go"               . "\\.go\\(html\\|tmpl\\)\\'")
    ("handlebars"       . "\\(handlebars\\|.\\hbs\\'\\)")
    ("jsp"              . "\\.jsp\\'")
    ("mako"             . "\\.mako\\'")
    ("mason"            . "\\.mas\\'")
    ("mustache"         . "\\.mustache\\'")
    ("php"              . "\\.\\(php\\|ctp\\|psp\\|inc\\)\\'")
    ("python"           . "\\.pml\\'")
    ("razor"            . "play\\|scala\\|\\.razor\\'\\|\\.cshtml\\'\\|\\.vbhtml\\'")
    ("smarty"           . "\\.tpl\\'")
    ("template-toolkit" . "\\.tt.?\\'")
    ("underscore"       . "\\.underscore\\'")
    ("velocity"         . "\\.\\(vsl\\|vtl\\|vm\\)\\'")
    ("web2py"           . "web2py"))
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
  '(("egrave" . 232)
    ("eacute" . 233)
    ("ecirc"  . 234)
    ("euml"   . 235)
    ("agrave" . 224)
    ("aacute" . 225)
    ("aelig"  . 230)
    ("ccedil" . 231)
    ("times"  . 215)
    ("quot"   . 34)
    ("amp"    . 38)
    ("lt"     . 60)
    ("gt"     . 62)
    ("laquo"  . 171)
    ("raquo"  . 187)
    ("lsquo"  . 8249)
    ("rsquo"  . 8250)
    ("ldquo"  . 8220)
    ("rdquo"  . 8221)
    ("lsaquo" . 8249)
    ("rsaquo" . 8250)
    ("apos"   . 39)
    ("frac14" . 188)
    ("frac12" . 189)
    ("frac34" . 190)
    ("para"   . 182)
    ("middot" . 183)
    ("ndash"  . 8211)
    ("mdash"  . 8212)
    ("bull"   . 8226)
    ("hellip" . 8230)
    ("trade"  . 8482)
    ("larr"   . 8592)
    ("uarr"   . 8593)
    ("rarr"   . 8594)
    ("darr"   . 8595)
    ("harr"   . 8596)
    ("crarr"  . 8629)
    ("and"    . 8743)
    ("or"     . 8744)
    ("sdot"   . 8901)
    )
  "HTML entities")

;;(message "%S" web-mode-engines-alternate-delimiters)

(defvar web-mode-engines-alternate-delimiters
  (if (boundp 'web-mode-engines-alternate-delimiters) web-mode-engines-alternate-delimiters '())
  "Engine delimiters. Useful for engines that provide alternate delimiters.")

(defun web-mode-highlight-whitespaces (beg end)
  "Scan whitespaces."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward web-mode-whitespaces-regexp end t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face web-mode-whitespace-face))
      ) ;while
    ))

(defun web-mode-engine-delimiter-open (engine default)
  "alternative open delimiter"
;;  (setq engine (if (eq engine t) ))
  (let (delim)
    (setq delim (car (cdr (assoc engine web-mode-engines-alternate-delimiters))))
    (or delim default)
  ))

(defun web-mode-engine-delimiter-close (engine default)
  "alternative close delimiter"
  ;;  (setq engine (if (eq engine t) ))
  (let (delim)
    (setq delim (cdr (cdr (assoc engine web-mode-engines-alternate-delimiters))))
    (or delim default)
    ))

(defvar web-mode-jshint-errors 0
  "JSHint errors")

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

;;    http://webdesign.about.com/od/localization/l/blhtmlcodes-ascii.htm
(defvar web-mode-display-table
  (let ((table (make-display-table)))
    (aset table 9  (vector ?\xBB ?\t)) ;tab
    (aset table 10 (vector ?\xB6 ?\n)) ;line feed
    (aset table 32 (vector ?\xB7))
    table)
  "Display table.")

(defvar web-mode-hl-line-mode-flag nil
  "Is hl-line-mode enabled ?")

(defvar web-mode-django-control-blocks
  (regexp-opt
   '("assets" "autoescape" "block" "blocktrans" "cache" "call" "comment"
     "elif" "else" "elseif" "elsif" "embed" "empty" "filter" "foreach" "for"
     "ifchanged" "ifequal" "ifnotequal" "if"
     "macro" "draw" "random" "sandbox" "spaceless" "verbatim" "with")
   t)
  "Django controls.")

(defvar web-mode-auto-pairs nil
  "Auto-Pairs")

(defvar web-mode-closing-blocks nil
  "Snippets")

(defvar web-mode-engines-closing-blocks
  '(
    ("php"       . (("if"      . "<?php endif; ?>")
                    ("for"     . "<?php endfor; ?>")
                    ("foreach" . "<?php endforeach; ?>")
                    ("while"   . "<?php endwhile; ?>")))
    )
  "Closing blocks (see web-mode-block-close)"
  )

(defvar web-mode-engines-auto-pairs
  '(
    ("angular"    . (("{{ " " }}")))
    ("asp"        . (("<% " " %>")))
    ("aspx"       . (("<% " " %>")
                     ("<%=" "%>")
                     ("<%#" "%>")
                     ("<%$" "%>")
                     ("<%@" "%>")
                     ("<%:" "%>")
                     ("<%-" "- " " --%>")))
    ("blade"      . (("{{ " " }}")
                     ("{{-" "- " " --}}")))
    ("django"     . (("{{ " " }}")
                     ("{% " " %}")
                     ("{# " " #}")))
    ("erb"        . (("<% " " %>")
                     ("<%=" "%>")
                     ("<%#" "%>")))
    ("freemarker" . (("<% " " %>")
                     ("${ " " }")
                     ("[% " " %]")
                     ("[# " " #]")
                     ("[#-" "- " " --]")))
    ("jsp"        . (("<% " " %>")
                     ("<%-" "- " " %>")
                     ("<%=" "%>")
                     ("<%!" "%>")
                     ("<%@" "%>")
                     ("${ " " }")))
    ("mako"       . (("<% " " %>")
                     ("<%!" " " " %>")
                     ("${ " " }")))
    ("mason"      . (("<% " " %>")))
    ("php"        . (("<?p" "hp " " ?>")
                     ("<? " " ?>")
                     ("<?=" "?>")))
    ("underscore" . (("<% " " %>")))
    ("web2py"     . (("{{ " " }}")
                     ("{{=" "}}")))
    (nil          . (("<!-" "- " " -->")))

    )
  "Engines auto-pairs")

(defvar web-mode-snippets nil
  "Snippets")

(defvar web-mode-engines-snippets
  '(("erb" . (("if" . ("<% if " . " %>\n\n<% end %>"))))
    ("php" . (("if"      . ("<?php if ("      . "): ?>\n\n<?php endif; ?>"))
              ("while"   . ("<?php while ("   . "): ?>\n\n<?php endwhile; ?>"))
              ("for"     . ("<?php for ("     . " ; ; ): ?>\n\n<?php endfor; ?>"))
              ("foreach" . ("<?php foreach (" . " as ): ?>\n\n<?php endforeach; ?>"))
              ("switch"  . ("<?php switch ("  . "): ?>\n<?php case 1: ?>\n\n<?php break ;?>\n<?php case 2: ?>\n\n<?php break ;?>\n<?php endswitch;?>"))))
    ("django" . (("block"   . ("{% block "   . " %}\n\n{% endblock %}"))
                 ("comment" . ("{% comment " . " %}\n\n{% endcomment %}"))
                 ("cycle"   . ("{% cycle "   . " as  %}\n\n{% endcycle  %}"))
                 ("filter"  . ("{% filter "  . " %}\n\n{% endfilter %}"))
                 ("for"     . ("{% for "     . " in  %}\n\n{% endfor %}"))
                 ("if"      . ("{% if "      . " %}\n\n{% endif %}"))))
    (nil . (("html5" . ("<!doctype html>\n<html>\n<head>\n<title></title>\n<meta charset=\"utf-8\" />\n</head>\n<body>\n" . "\n</body>\n</html>"))
            ("table" . ("<table><tbody>\n<tr>\n<td>" . "</td>\n<td></td>\n</tr>\n</tbody></table>"))
            ("ul"    . ("<ul>\n<li>" . "</li>\n<li></li>\n</ul>"))))
    )
  "Engines snippets")

(defvar web-mode-block-regexps
  (list
   '("angular"          . "{{")
   '("asp"              . "<%")
   '("aspx"             . "<%")
   '("blade"            . "{{.\\|^[ \t]*@[[:alpha:]]")
   '("closure"          . "{.\\|/\\*\\| //")
   '("ctemplate"        . "[$]?{{.")
   '("django"           . "{[#{%]")
   '("dust"             . "{.")
   '("erb"              . "<%\\|^%.")
   '("freemarker"       . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]\\|</?[@#].\\|\\[/?[@#].")
   '("go"               . "{{.")
   '("jsp"              . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]")
   '("mako"             . "</?%\\|${\\|^[ \t]*% \\|^[ \t]*##")
;;   '("mason"            . "</&>\\|</%def\\|</%method\\|<%[[:alpha:]]+\\|<[%&]\\|^%.")
   '("mason"            . "</?[&%]\\|^%.")
   '("php"              . "<\\?")
   '("python"           . "<\\?")
   '("razor"            . "@.")
   (cons "smarty"       (concat (web-mode-engine-delimiter-open "smarty" "{") "[[:alpha:]#$/*\"]"))
   '("template-toolkit" . "\\[[%#]")
   '("underscore"       . "<%")
   '("velocity"         . "^[ \t]*#[[:alpha:]#*]\\|$[[:alpha:]!{]")
   '("web2py"           . "{{")
   )
  "Engine block regexps.")

(defvar web-mode-block-electric-chars
  (list
   (cons "blade"      '(?\{ ?\@))
   (cons "closure"    '(?\{ ?\/))
   (cons "ctemplate"  '(?\{ ?\$))
   (cons "django"     '(?\{))
   (cons "dust"       '(?\{))
   (cons "erb"        '(?\%))
   (cons "freemarker" '(?\[))
   (cons "go"         '(?\{))
   (cons "jsp"        '(?\$))
   (cons "razor"      '(?\@))
   (cons "smarty"     '(?\{))
   (cons "velocity"   '(?\# ?\$)))
  "Block electric chars.")

(defvar web-mode-block-regexp nil
  "Regular expression for identifying blocks.")

(defvar web-mode-electric-chars nil
  "electric chars")

(defvar web-mode-normalization-rules
  '(("tag-case"          . "lower-case")
    ("attr-case"         . "lower-case")
    ("special-chars"     . "unicode") ; "unicode" "entities"
    ("css-indentation"   . t)
    ("smart-apostrophes" . t)
    ("smart-quotes"      . t)
    ("whitespaces"       . t)
    ("indentation"       . t))
  "Normalization rules")

(defvar web-mode-comment-keywords
  (regexp-opt
   (append web-mode-extra-comment-keywords
           '("FIXME" "TODO" "BUG" "KLUDGE" "WORKAROUND"
             "OPTIMIZE" "HACK" "REFACTOR")))
  "Comment keywords.")

(defvar web-mode-python-constants
  (regexp-opt
   (append
    web-mode-extra-python-constants
    '("True" "False" "None" "__debug__" "NotImplemented" "Ellipsis")))
  "Python constants.")

(defvar web-mode-php-constants
  (regexp-opt
   (append
    web-mode-extra-php-constants
    '("TRUE" "FALSE" "NULL" "true" "false" "null"
      "STR_PAD_LEFT" "STR_PAD_RIGHT"
      "ENT_COMPAT" "ENT_QUOTES" "ENT_NOQUOTES" "ENT_IGNORE"
      "ENT_SUBSTITUTE" "ENT_DISALLOWED" "ENT_HTML401" "ENT_XML1"
      "ENT_XHTML" "ENT_HTML5"
      "LIBXML_NOBLANKS")))
  "PHP constants.")

(defvar web-mode-php-keywords
  (regexp-opt
   (append
    web-mode-extra-php-keywords
    '("and" "array" "as" "break"
      "callable" "case" "catch"  "catch all" "class" "const" "continue"
      "default" "die" "do" "echo" "else" "elseif" "empty"
      "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit" "extends"
      "finally" "for" "foreach" "function" "global" "goto"
      "if" "include" "include_once" "instanceof" "interface" "isset"
      "list" "next" "new" "or" "private" "protected" "public"
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
     '("charset" "import" "media" "page" "font-face"
       "namespace" "supports" "document"
       "keyframes" "-moz-keyframes" "-webkit-keyframes")))
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
   (append
    web-mode-extra-python-keywords
    '("and" "as" "assert" "break" "class" "continue" "def" "del"
      "elif" "else" "except" "finally" "for" "from" "global"
      "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
      "raise" "return" "try" "while" "with" "yield")))
  "Python keywords.")

(defvar web-mode-jsp-keywords
  (regexp-opt
   (append
    web-mode-extra-jsp-keywords
    '("case" "catch" "do" "else" "end" "false" "for" "function" "if" "in" "include"
      "new" "package" "page" "private" "protected" "public"
      "return" "tag" "taglib" "throw" "throws" "true" "try" "void" "while")))
  "JSP keywords.")

(defvar web-mode-erb-keywords
  (regexp-opt
   (append
    web-mode-extra-erb-keywords
    '("alias" "and" "begin" "break" "case" "class" "def" "defined?" "do"
      "elsif" "else" "end" "ensure" "fail" "for" "if" "in"
      "module" "next" "not" "or" "redo" "rescue" "retry" "return"
      "then" "super" "unless" "undef" "until" "when" "while" "yield"
      "__ENCODING__" "__FILE__" "__LINE__"
      )))
  "ERB keywords.")

(defvar web-mode-mason-keywords
  (regexp-opt
   (append
    web-mode-extra-mason-keywords
    '("and" "base" "close" "die" "each" "else" "elsif" "eval" "exists"
      "foreach" "grep" "if" "length" "local" "my" "next" "open" "or"
      "package" "pop" "ref" "return" "stat" "sub" "tie"
      "undef" "unless" "use" "while"
      )))
  "Mason keywords.")

(defvar web-mode-erb-builtins
  (regexp-opt
   '("__callee__" "__dir__" "__method__"
     "abort" "at_exit" "autoload" "autoload?"
     "binding" "block_given?" "caller" "catch"
     "eval" "exec" "exit" "exit!" "fail" "fork" "format"
     "lambda" "load" "loop" "open"
     "p" "print" "printf" "proc" "putc" "puts"
     "raise" "rand" "readline" "readlines" "require" "require_relative"
     "sleep" "spawn" "sprintf" "srand" "syscall" "system"
     "throw" "trap" "warn"
     "alias_method" "attr" "attr_accessor" "attr_reader" "attr_writer"
     "define_method" "extend" "include" "module_function"
     "prepend" "private" "protected" "public"
     "refine" "using"

     "error_message_on" "error_messages_for" "form" "input"
     "auto_discovery_link_tag" "image_tag" "javascript_include_tag"
     "stylesheet_link_tag" "image_path" "path_to_image"" "
     "javascript_path" "path_to_javascript" "register_javascript_expansion"
     "register_javascript_include_default" "register_stylesheet_expansion"
     "stylesheet_path" "path_to_stylesheet" "atom_feed" "entry" "updated"
     "benchmark" "cache" "capture" "content_for" "distance_of_time_in_words"
     "distance_of_time_in_words_to_now" "time_ago_in_words" "date_select"
     "datetime_select" "time_select" "select_date" "select_datetime"
     "select_day" "select_hour" "select_minute" "select_month" "select_second"
     "select_time" "select_year" "debug"
     "check_box" "fields_for" "file_field" "form_for" "hidden_field"
     "label" "password_field" "radio_button" "text_area" "text_field"
     "check_box_tag" "field_set_tag" "file_field_tag" "form_tag"
     "hidden_field_tag" "image_submit_tag" "label_tag" "password_field_tag"
     "radio_button_tag" "select_tag" "submit_tag" "text_area_tag"
     "text_field_tag"
     "collection_select" "country_options_for_select" "country_select"
     "option_groups_from_collection_for_select" "options_for_select"
     "options_from_collection_for_select" "select"
     "time_zone_options_for_select"
     "time_zone_select" "button_to_function" "define_javascript_functions"
     "escape_javascript" "javascript_tag" "link_to_function"" "
     "number_to_currency" "number_to_human_size" "number_to_percentage"
     "number_to_phone" "number_with_delimiter" "number_with_precision"
     "evaluate_remote_response" "form_remote_for" "form_remote_tag"
     "link_to_remote" "observe_field" "observe_field"
     "periodically_call_remote"
     "remote_form_for" "remote_function" "submit_to_remote" "update_page"
     "update_page_tag" "dom_class" "dom_id" "partial_path" "sanitize"
     "sanitize_css" "strip_links" "strip_tags"
     "cdata_section" "content_tag" "escape_once" "tag"
     "auto_link" "concat" "cycle" "excerpt" "highlight" "markdown" "pluralize"
     "reset_cycle" "simple_format" "textilize" "textilize_without_paragraph"
     "truncate" "word_wrap" "button_to" "current_page?" "link_to" "link_to_if"
     "link_to_unless" "link_to_unless_current" "mail_to" "url_for"
     "action_name" "atom_feed" "audio_path" "audio_tag"
     "content_tag_for" "controller" "controller_name" "action_name"
     "controller_path" "convert_to_model" "cookies" "csrf_meta_tag"
     "csrf_meta_tags" "headers"
     "current_cycle" "div_for" "email_field" "email_field_tag"
     "favicon_link_tag" "flash" "l" "button_tag"
     "grouped_collection_select" "grouped_options_for_select"
     "image_alt" "j" "javascript_cdata_section"
     "localize" "logger" "number_field"
     "number_field_tag" "number_to_human" "params" "path_to_audio"
     "path_to_video" "phone_field" "phone_field_tag" "provide"
     "range_field" "range_field_tag" "raw" "render" "request"
     "request_forgery_protection_token" "response" "safe_concat"
     "safe_join" "search_field" "search_field_tag"
     "session" "t" "telephone_field" "telephone_field_tag"
     "time_tag" "translate" "url_field" "url_field_tag"
     "url_options" "video_path" "video_tag"

     ))
  "ERB builtins.")

(defvar web-mode-asp-constants
  (regexp-opt
   (append
    web-mode-extra-asp-constants
    '("adAsyncExecute" "adAsyncFetch" "adAsyncFetchNonBlocking" "adCmdFile"
      "adCmdStoredProc" "adCmdTable" "adCmdTableDirect" "adCmdText" "adCmdUnknown"
      "adCmdUnspecified" "adExecuteNoRecords" "adExecuteRecord" "adExecuteStream"
      "adLockBatchOptimistic" "adLockOptimistic" "adLockPessimistic"
      "adLockReadOnly" "adLockUnspecified" "adOpenDynamic" "adOpenForwardOnly"
      "adOpenKeyset" "adOpenStatic" "adOpenUnspecified" "adOptionUnspecified"
      "Empty" "Nothing" "Null" "True" "False"
      "vbBack" "vbCr" "vbCrLf" "vbFormFeed" "vbLf" "vbNewLine" "vbNullChar"
      "vbNullString" "vbObjectError" "vbScript" "vbTab" "vbVerticalTab")))
  "ASP constants.")

(defvar web-mode-asp-keywords
  (regexp-opt
   (append
    web-mode-extra-asp-keywords
    '("Abs" "And" "Array" "Asc" "Atn"
      "CBool" "CByte" "CCur" "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr"
      "Call" "Case" "Chr" "Class" "Const" "Cos" "CreateObject"
      "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue"
      "Day" "Dim" "Do"
      "Each" "Else" "ElseIf" "End" "Erase" "Err" "Eval" "Exit" "Exp"
      "Explicit"
      "Filter" "Fix" "For" "FormatCurrency" "FormatDateTime"
      "FormatNumber" "FormatPercent" "Function"
      "GetLocale" "GetObject" "GetRef" "Hex" "Hour"
      "If" "In" "InStr" "InStrRev" "InputBox" "Int" "IsArray" "IsDate"
      "IsEmpty" "IsNull" "IsNumeric" "IsObject" "Join"
      "LBound" "LCase" "LTrim" "Language" "Left" "Len" "Let"
      "LoadPicture" "Log" "Loop"
      "Mid" "Minute" "Month" "MonthName" "MsgBox"
      "New" "Next" "Not" "Now"
      "Oct" "On" "Option" "Or" "Preserve" "Private" "Public"
      "RGB" "RTrim" "Redim" "Rem" "Replace" "Right" "Rnd" "Round"
      "ScriptEngine" "ScriptEngineBuildVersion"
      "ScriptEngineMajorVersion" "ScriptEngineMinorVersion"
      "Second" "Select" "Set" "SetLocale" "Sgn" "Sin" "Space" "Split"
      "Sqr" "StrComp" "StrReverse" "String" "Sub"
      "Tan" "Then" "Time" "TimeSerial" "TimeValue" "Timer" "To" "Trim"
      "TypeName"
      "UBound" "UCase" "Until" "VarType"
      "Weekday" "WeekdayName" "Wend" "With" "While" "Year")))
  "ASP keywords.")

(defvar web-mode-asp-types
  (regexp-opt
   (append
    web-mode-extra-asp-types
    '("Application" "ASPError" "Request" "Response" "Server" "Session")))
  "ASP types.")

(defvar web-mode-aspx-keywords
  (regexp-opt
   (append
    web-mode-extra-aspx-keywords
    '("case" "catch" "do" "else" "end" "for" "foreach" "function"
      "if" "in" "include" "new" "package" "page" "return"
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
       "debug" "do" "embed" "empty" "else" "elseif" "elsif" "elif"
       "endautoescape" "endblock" "endblocktrans" "endcomment"
       "endcache" "endcall" "endembed" "endfilter" "endfor" "endif"
       "endifchanged" "endifequal" "endifnotequal" "endmacro" "endrandom" "endraw"
       "endsandbox" "endset" "endspaceless" "endtrans" "endverbatim" "endwith"
       "extends" "filter" "firstof" "flush" "for" "from"
       "if" "ifchanged" "ifequal" "ifnotequal" "ignore" "import"
       "in" "include" "is" "load" "macro" "missing" "none" "not" "now" "or"
       "pluralize" "random" "raw" "regroup" "trans"
       "sandbox" "set" "spaceless" "ssi" "static" "templatetag" "trans"
       "use" "url" "var" "verbatim" "widthratio" "with"

       "assign" "capture" "endcapture" "case" "layout" "tablerow" "endtablerow" ;;liquid
       "unless" "endunless" ;; liquid

       )))
  "Django keywords.")

(defvar web-mode-django-types
  (eval-when-compile
    (regexp-opt
     '("null" "empty" "false" "true"
       ))))

(defvar web-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib"
       "Assembly" "Control" "Implements" "Import"
       "Master" "OutputCache" "Page" "Reference" "Register")))
  "Directives.")

(defvar web-mode-template-toolkit-keywords
  (regexp-opt
   '("block" "call" "case" "catch" "clear" "default" "do"
     "else" "elsif" "end" "filter" "final" "for"
     "foreach" "get" "if" "in" "include" "insert" "is" "last"
     "macro" "meta" "or" "perl" "process" "rawperl" "return"
     "set" "stop" "switch" "tags" "throw" "try"
     "unless" "use" "while" "wrapper"))
  "Template-toolkit keywords")

(defvar web-mode-javascript-keywords
  (regexp-opt
   (append
    web-mode-extra-javascript-keywords
    '("break" "case" "catch" "class" "const" "continue"
      "debugger" "default" "delete" "do" "else" "enum" "eval"
      "export" "extends" "finally" "for" "function" "if"
      "implements" "import" "in" "instanceof" "interface" "let"
      "new" "package" "private" "protected" "public"
      "return" "static" "super" "switch" "throw"
      "try" "typeof" "var" "void" "while" "with" "yield"
      )))
  "JavaScript keywords.")

(defvar web-mode-javascript-constants
  (regexp-opt
   '("false" "null" "undefined" "Infinity" "NaN" "true" "arguments" "this"))
  "JavaScript constants.")

(defvar web-mode-razor-keywords
  (regexp-opt
   (append
    web-mode-extra-razor-keywords
    '("false" "true" "foreach" "if" "else" "in" "var" "for" "display"
      ;;"main"
      ;; scala
      "match" "case"
      "Html")))
  "Razor keywords.")

(defvar web-mode-selector-font-lock-keywords
  (list
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>")
         '(1 'web-mode-css-at-rule-face))
   '("\\<\\(all\|braille\\|embossed\\|handheld\\|print\\|projection\\|screen\\|speech\\|tty\\|tv\\|and\\|or\\)\\>" 1 'web-mode-keyword-face)
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>")
         '(1 'web-mode-css-pseudo-class-face))
   '("[[:alnum:]-]+" 0 'web-mode-css-selector-face)
   '("\\[.*?\\]\\|(.*?)" 0 nil t t)
   '("url(\\(.+?\\))" 1 'web-mode-string-face)
   ))

(defvar web-mode-declaration-font-lock-keywords
  (list
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") '(1 'web-mode-css-at-rule-face))
   '("url(\\([^)]+\\)" 1 'web-mode-string-face)
   '("\\([[:alpha:]-]+\\)[ ]?:" 1 'web-mode-css-property-name-face)
   '("\\([[:alpha:]-]+\\)[ ]?(" 1 'web-mode-css-function-face)
   '("#[[:alnum:]]\\{1,6\\}" 0 'web-mode-css-color-face t t)
   '("![ ]?important" 0 'web-mode-css-priority-face t t)
   ))

(defvar web-mode-html-font-lock-keywords
  (list
   '("</?[[:alnum:]]+[ >]\\|>" 0 'web-mode-html-tag-face t)
   '(" \\([[:alnum:]-]+=\\)\\(\"[^\"]+\"\\)"
     (1 'web-mode-html-attr-name-face)
     (2 'web-mode-html-attr-value-face))
   ))

(defvar web-mode-javascript-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-javascript-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-javascript-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("\\<\\(new\\|instanceof\\) \\([[:alnum:]_.]+\\)\\>" 2 'web-mode-type-face)
   '("\\<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'web-mode-function-name-face)
   '("\\<function[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\<var[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
;;   '("\\<\\(var\\)\\>[ ]+"
;;     (1 'web-mode-keyword-face)
;;     ("\\([[:alnum:]_]+\\)\\([ ]*=[^,;]*\\)?[,; ]" nil nil (1 'web-mode-variable-name-face)))
   '("\\<\\(function\\)[ ]*("
     (1 'web-mode-keyword-face)
     ("\\([[:alnum:]_]+\\)\\([ ]*=[^,)]*\\)?[,)]" nil nil (1 'web-mode-variable-name-face)))
   '("\\([[:alnum:]_]+\\):" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-dust-font-lock-keywords
  (list
;;   '("/?}\\|{[#/:?@><+^]?" 0 'web-mode-preprocessor-face)
   '("{[#:/?@><+^]\\([[:alpha:]_]+\\)" 1 'web-mode-block-control-face)
   '(":\\([[:alpha:]]+\\)" 1 'web-mode-keyword-face)
   '("\\<\\([[:alpha:]_]+=\\)\\(\"[^\"]*\"\\|[[:alnum:]_]*\\)"
     (1 'web-mode-block-attr-name-face)
     (2 'web-mode-block-attr-value-face))
   '("\\\([[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-template-toolkit-font-lock-keywords
  (list
;;   '("\\[%[-+]?\\|[-+=]?%\\]" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-template-toolkit-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("\\\([[:alpha:]][[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("\\\([[:alpha:]][[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-smarty-font-lock-keywords
  (list
   ;; (cons (concat
   ;;        "[#]?"
   ;;        (web-mode-engine-delimiter-close "smarty" "}")
   ;;        "\\|"
   ;;        (web-mode-engine-delimiter-open "smarty" "{")
   ;;        "[/#]?")
   ;;       '(0 'web-mode-preprocessor-face))
   (cons (concat "[ ]\\(" web-mode-smarty-keywords "\\)[ ]")
         '(1 'web-mode-keyword-face))
   (cons (concat (web-mode-engine-delimiter-open "smarty" "{") "/?\\([[:alpha:]_]+\\)")
         '(1 'web-mode-block-control-face))
   '("\\([}{]\\)" 0 'web-mode-block-delimiter-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]+\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   '(" \\(\\sw+[ ]?=\\)" 1 'web-mode-param-name-face)
   '(" \\(\\sw+\\)[ }]" 1 'web-mode-param-name-face)
   '("|\\([[:alnum:]_]+\\)" 1 'web-mode-function-call-face)
   '("\\(->\\)\\(\\sw+\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("[.]\\([[:alnum:]_-]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[.]\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("#\\([[:alnum:]_]+\\)#" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-velocity-font-lock-keywords
  (list
   '("#\\([[:alpha:]]+\\)\\>"
     (1 'web-mode-block-control-face))
   (cons (concat "[ ]\\(" web-mode-velocity-keywords "\\)[ ]") '(1 'web-mode-keyword-face t t))
   '("#macro([ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-function-name-face)
   '("[.]\\([[:alnum:]_-]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\($[!]?[{]?\\)\\([[:alnum:]_-]+\\)[}]?" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-mako-tag-font-lock-keywords
  (list
   '("</?%\\([[:alpha:]:]+\\)" 1 'web-mode-block-control-face)
;;   '("</?%\\|/?>" 0 'web-mode-preprocessor-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-mako-block-font-lock-keywords
  (list
;;   '("<%!?\\|%>" 0 'web-mode-preprocessor-face)
;;   '("\\(%\\)" 1 'web-mode-preprocessor-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   (cons (concat "\\<\\(" web-mode-python-constants "\\)\\>") '(1 'web-mode-constant-face))
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(endfor\\|endif\\|endwhile\\)\\>") '(1 'web-mode-keyword-face))
   ))

(defvar web-mode-web2py-font-lock-keywords
  (list
;;   '("{{[=]?\\|}}" 0 'web-mode-preprocessor-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   (cons (concat "\\<\\(" web-mode-python-constants "\\)\\>") '(1 'web-mode-constant-face))
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(block\\|extend\\|super\\|end\\|include\\)\\>") '(1 'web-mode-keyword-face))
   ))

(defvar web-mode-django-expr-font-lock-keywords
  (list
;;   '("\\({{\\)[ ]?" 1 'web-mode-preprocessor-face)
;;   '("[ ]?\\(}}\\)" 1 'web-mode-preprocessor-face)
   '("|[ ]?\\([[:alpha:]_]+\\)\\>" 1 'web-mode-function-call-face)
   (cons (concat "\\<\\(" web-mode-django-types "\\)\\>") '(1 'web-mode-type-face))
   '("\\<\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-django-code-font-lock-keywords
  (list
;;   '("{%\\|%}" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-django-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-django-types "\\)\\>") '(1 'web-mode-type-face))
   '("|[ ]?\\([[:alpha:]_]+\\)\\>" 1 'web-mode-function-call-face)
;;   (cons (concat "|[ ]?\\(" web-mode-django-filters "\\)\\>") '(1 'web-mode-function-name-face t t))
   '("\\<\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-ctemplate-font-lock-keywords
  (list
;;   '("${{\\|{{[>#/{%^&]?\\|[}]?}}" 0 'web-mode-preprocessor-face)
   '("{{[#/>][ ]*\\([[:alnum:]_-]+\\)" 1 'web-mode-block-control-face)
   '("[[:alnum:]_]" 0 'web-mode-variable-name-face)
   '("[ ]+\\([[:alnum:]_]+=\\)" 1 'web-mode-param-name-face t t)
   '("[:=]\\([[:alpha:]_]+\\)" 1 'web-mode-function-call-face t t)
   ))

(defvar web-mode-razor-font-lock-keywords
  (list
;;   '("@" 0 'web-mode-preprocessor-face)
   '("@\\([[:alnum:]_.]+\\)[ ]*[({]" 1 'web-mode-block-control-face)
   (cons (concat "\\<\\(" web-mode-razor-keywords "\\)\\>") '(1 'web-mode-keyword-face))
;;   '("\\([[:alnum:]]+\\):" 1 'web-mode-symbol-face)
;;   '("@\\([[:alnum:]_.]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("@\\([[:alnum:]_.]+\\)" 1 'web-mode-variable-name-face)
;;   '("<\\([[:alnum:]_]+\\)>" 1 'web-mode-type-face)
;;   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
;;   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-closure-font-lock-keywords
  (list
;;   '("{/?\\|/?}" 0 'web-mode-preprocessor-face)
   '("{/?\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   '("{param[ ]+\\([[:alnum:]]+\\)" 1 'web-mode-symbol-face)
   '("\\<\\(true\\|false\\|null\\)\\>" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-closure-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("{\\(alias\\|call\\|delcall\\|delpackage\\|deltemplate\\|namespace\\|template\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-constant-face)
   '("\\(allowemptydefault\\|data\\|desc\\|meaning\\|autoescape\\|private\\|variant\\)=" 0 'web-mode-block-attr-name-face)
   '("|\\([[:alpha:]]+\\)" 1 'web-mode-function-call-face)
   '("\\<\\([[:alnum:]]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("$\\([[:alnum:]._]+\\)" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-go-font-lock-keywords
  (list
;;   '("{{\\|}}" 0 'web-mode-preprocessor-face)
   '("{{\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   (cons (concat "\\<\\(" web-mode-go-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-go-functions "\\)\\>")
         '(1 'web-mode-function-call-face))
   '("[$.]\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face t t)
   ))

(defvar web-mode-expression-font-lock-keywords
  (list
;;   '("<%\\$\\|%>" 0 'web-mode-preprocessor-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-angular-font-lock-keywords
  (list
;;   '("{{\\|}}" 0 'web-mode-preprocessor-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-underscore-font-lock-keywords
  (list
;;   '("<%[-=]?\\|%>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-javascript-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("\\<\\(_\.[[:alpha:]]+\\)(" 1 'web-mode-function-call-face)
   '("\\<new \\([[:alnum:]_.]+\\)\\>" 1 'web-mode-type-face)
   '("\\<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'web-mode-function-name-face)
   '("\\<\\(var\\)\\>[ ]+\\([[:alnum:]_]+\\)"
     (1 'web-mode-keyword-face)
     (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-asp-font-lock-keywords
  (list
;;   '("<%=?\\|%>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-asp-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-asp-types "\\)\\>")
         '(0 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-asp-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("\\(Class\\|new\\) \\([[:alnum:]_]+\\)" 2 'web-mode-type-face)
   '("Const \\([[:alnum:]_]+\\)" 1 'web-mode-constant-face)
   '("\\<dim\\>"
     (0 'web-mode-keyword-face)
     ("[[:alnum:]_]+" nil nil (0 'web-mode-variable-name-face)))
   '("\\<\\(public\\|private\\|sub\\|function\\)\\> \\([[:alnum:]_]+\\)[ ]*("
     2 'web-mode-function-name-face)
   '("\\<\\(public\\|private\\|dim\\)\\> \\([[:alnum:]_]+\\)"
     2 'web-mode-variable-name-face)
   ))

(defvar web-mode-aspx-font-lock-keywords
  (list
;;   '("<%[:=#]?\\|%>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-aspx-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   ))

(defvar web-mode-uel-font-lock-keywords
  (list
   '("[$#{]{\\|}" 0 'web-mode-preprocessor-face)
   '("\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("|[ ]*\\(trim\\|x\\|u\\)" 1 'web-mode-function-call-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-php-var-interpolation-font-lock-keywords
  (list
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   '("\".+\"\\|'.*'" 0 'web-mode-string-face)
   ))

(defvar web-mode-freemarker-square-font-lock-keywords
  (list
;;   '("\\[/?[#@]\\|/?>\\|/?\\]" 0 'web-mode-preprocessor-face)
   '("\\[/?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-freemarker-font-lock-keywords
  (list
;;   '("</?[#@]\\|/?>\\|/?>" 0 'web-mode-preprocessor-face)
   '("</?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

;;TODO : definir web-mode-block-attr-name-face et web-mode-block-attr-name-face
(defvar web-mode-jsp-tag-font-lock-keywords
  (list
;;   '("</?\\|/?>" 0 'web-mode-preprocessor-face)
   '("</?\\([[:alpha:]]+:[[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-jsp-font-lock-keywords
  (list
;;   '("-?%>\\|<%\\(!\\|=\\|#=\\)?" 0 'web-mode-preprocessor-face)
   '("\\(throws\\|new\\|extends\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
   ))

(defvar web-mode-directive-font-lock-keywords
  (list
;;   '("<%@\\|%>" 0 'web-mode-preprocessor-face)
   '("<%@[ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-erb-font-lock-keywords
  (list
;;   '("-?%>\\|^%\\|<%[=-]?" 0 'web-mode-preprocessor-face)
   '("[^:]\\(:[[:alnum:]_]+\\)" 1 'web-mode-symbol-face)
   '("\\([[:alnum:]_]+:\\)[ ]+" 1 'web-mode-symbol-face)
   (cons (concat "\\<\\(" web-mode-erb-builtins "\\)\\>") '(0 'web-mode-builtin-face))
   (cons (concat "\\<\\(" web-mode-erb-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\(self\\|true\\|false\\|nil\\)\\>" 0 'web-mode-variable-name-face)
   '("[@$]@?\\([[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   '("class[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-type-face)
   '("def[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\(?:\\_<\\|::\\)\\([A-Z]+[[:alnum:]_]+\\)" 1 (unless (eq (char-after) ?\() 'web-mode-type-face))
   '("/[^/]+/" 0 'web-mode-string-face)
   ))

(defvar web-mode-python-font-lock-keywords
  (list
;;   '("<\\?\\|\\?>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   ))

(defvar web-mode-mason-font-lock-keywords
  (list
;;   '("^\\(%\\)" 1 'web-mode-preprocessor-face)
;;   '("<%\\(def\\|method\\).*>" 0 'web-mode-preprocessor-face)
;;   '("</&>\\|</?%[[:alpha:]]+>\\|<[%&]|?\\|[%&]>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-mason-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("sub[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '(" | \\([hun]+\\) " 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\([@]\\)\\([[:alnum:]#_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("\\<\\([$%]\\)\\([[:alnum:]@#_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("{\\([[:alnum:]_]+\\)}" 1 'web-mode-variable-name-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("->[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-php-font-lock-keywords
  (list
;;   '("<\\?\\(php\\|=\\)?\\|\\?>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   (cons (concat "(\\<\\(" web-mode-php-types "\\)\\>") '(1 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") '(0 'web-mode-constant-face))
   '("function[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-constant-face)
   '("->[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\<\\(instanceof\\|class\\|extends\\|new\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-type-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-blade-font-lock-keywords
  (append
   (list
;;    '("{{\\|}}" 0 'web-mode-preprocessor-face)
    '("@\\([[:alpha:]_]+\\)"
      (1 'web-mode-block-control-face)))
   web-mode-php-font-lock-keywords))

(defvar web-mode-engines-font-lock-keywords
  '(("angular"          . web-mode-angular-font-lock-keywords)
    ("asp"              . web-mode-asp-font-lock-keywords)
    ("blade"            . web-mode-blade-font-lock-keywords)
    ("closure"          . web-mode-closure-font-lock-keywords)
    ("ctemplate"        . web-mode-ctemplate-font-lock-keywords)
    ("dust"             . web-mode-dust-font-lock-keywords)
    ("erb"              . web-mode-erb-font-lock-keywords)
    ("go"               . web-mode-go-font-lock-keywords)
    ("mason"            . web-mode-mason-font-lock-keywords)
    ("php"              . web-mode-php-font-lock-keywords)
    ("python"           . web-mode-python-font-lock-keywords)
    ("razor"            . web-mode-razor-font-lock-keywords)
    ("smarty"           . web-mode-smarty-font-lock-keywords)
    ("template-toolkit" . web-mode-template-toolkit-font-lock-keywords)
    ("underscore"       . web-mode-underscore-font-lock-keywords)
    ("web2py"           . web-mode-web2py-font-lock-keywords)
    ("velocity"         . web-mode-velocity-font-lock-keywords))
  "Engines font-lock keywords")

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)

    table)
  "Syntax table in use in web-mode buffers.")

(defvar web-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [menu-bar wm]             (cons "Web-Mode" (make-sparse-keymap)))
    (define-key map [menu-bar wm dom]         (cons "Dom" (make-sparse-keymap)))
    (define-key map [menu-bar wm blk]         (cons "Block" (make-sparse-keymap)))
    (define-key map [menu-bar wm tag]         (cons "Html Tag" (make-sparse-keymap)))
    (define-key map [menu-bar wm elt]         (cons "Html Element" (make-sparse-keymap)))

    (define-key map [menu-bar wm sep-1]       '(menu-item "--"))

    (define-key map [menu-bar wm dom dom-xpa] '(menu-item "XPath" web-mode-dom-xpath))
    (define-key map [menu-bar wm dom dom-tra] '(menu-item "Traverse" web-mode-dom-traverse))
    (define-key map [menu-bar wm dom dom-err] '(menu-item "Show error(s)" web-mode-dom-errors-show))
    (define-key map [menu-bar wm dom dom-ent] '(menu-item "Replace HTML entities" web-mode-dom-entities-replace))
    (define-key map [menu-bar wm dom dom-quo] '(menu-item "Replace dumb quotes" web-mode-dom-quotes-replace))
    (define-key map [menu-bar wm dom dom-apo] '(menu-item "Replace apostrophes" web-mode-dom-apostrophes-replace))
    (define-key map [menu-bar wm dom dom-nor] '(menu-item "Normalise" web-mode-dom-normalize))

    (define-key map [menu-bar wm blk blk-sel] '(menu-item "Select" web-mode-block-select))
    (define-key map [menu-bar wm blk blk-pre] '(menu-item "Previous" web-mode-block-previous))
    (define-key map [menu-bar wm blk blk-nex] '(menu-item "Next" web-mode-block-next))
    (define-key map [menu-bar wm blk blk-kil] '(menu-item "Kill" web-mode-block-kill))
    (define-key map [menu-bar wm blk blk-end] '(menu-item "End" web-mode-block-beginning))
    (define-key map [menu-bar wm blk blk-clo] '(menu-item "Close" web-mode-block-close))
    (define-key map [menu-bar wm blk blk-beg] '(menu-item "Beginning" web-mode-block-beginning))

    (define-key map [menu-bar wm tag tag-sel] '(menu-item "Select" web-mode-tag-select))
    (define-key map [menu-bar wm tag tag-pre] '(menu-item "Previous" web-mode-tag-previous))
    (define-key map [menu-bar wm tag tag-nex] '(menu-item "Next" web-mode-tag-next))
    (define-key map [menu-bar wm tag tag-mat] '(menu-item "Match" web-mode-tag-match))
    (define-key map [menu-bar wm tag tag-end] '(menu-item "End" web-mode-tag-end))
    (define-key map [menu-bar wm tag tag-beg] '(menu-item "Beginning" web-mode-tag-beginning))

    (define-key map [menu-bar wm elt elt-wra] '(menu-item "Wrap" web-mode-element-wrap))
    (define-key map [menu-bar wm elt elt-van] '(menu-item "Vanish" web-mode-element-vanish))
    (define-key map [menu-bar wm elt elt-exc] '(menu-item "Transpose" web-mode-element-transpose))
    (define-key map [menu-bar wm elt elt-sel] '(menu-item "Select" web-mode-element-select))
    (define-key map [menu-bar wm elt elt-ren] '(menu-item "Rename" web-mode-element-rename))
    (define-key map [menu-bar wm elt elt-pre] '(menu-item "Previous" web-mode-element-previous))
    (define-key map [menu-bar wm elt elt-par] '(menu-item "Parent" web-mode-element-parent))
    (define-key map [menu-bar wm elt elt-nex] '(menu-item "Next" web-mode-element-next))
    (define-key map [menu-bar wm elt elt-mut] '(menu-item "Mute blanks" web-mode-element-mute-blanks))
    (define-key map [menu-bar wm elt elt-del] '(menu-item "Kill" web-mode-element-kill))
    (define-key map [menu-bar wm elt elt-end] '(menu-item "End" web-mode-element-end))
    (define-key map [menu-bar wm elt elt-inn] '(menu-item "Content (select)" web-mode-element-content-select))
    (define-key map [menu-bar wm elt elt-clo] '(menu-item "Close" web-mode-element-close))
    (define-key map [menu-bar wm elt elt-dup] '(menu-item "Clone" web-mode-element-clone))
    (define-key map [menu-bar wm elt elt-cfo] '(menu-item "Children fold" web-mode-element-children-fold-or-unfold))
    (define-key map [menu-bar wm elt elt-chi] '(menu-item "Child" web-mode-element-child))
    (define-key map [menu-bar wm elt elt-beg] '(menu-item "Beginning" web-mode-element-beginning))

    (define-key map [menu-bar wm fol]         '(menu-item "Fold/Unfold" web-mode-fold-or-unfold))
    (define-key map [menu-bar wm ind]         '(menu-item "Indent buffer" web-mode-buffer-indent))
    (define-key map [menu-bar wm nav]         '(menu-item "Tag/Block navigation" web-mode-navigate))
    (define-key map [menu-bar wm exp]         '(menu-item "Mark and Expand" web-mode-mark-and-expand))
    (define-key map [menu-bar wm spa]         '(menu-item "Toggle whitespaces" web-mode-whitespaces-show))
    (define-key map [menu-bar wm sni]         '(menu-item "Insert snippet" web-mode-snippet-insert))

    ;;--------------------------------------------------------------------------
    ;; "C-c letter"  are reserved for users

    (define-key map (kbd "C-c C-b c") 'web-mode-block-close)
    (define-key map (kbd "C-c C-b b") 'web-mode-block-beginning)
    (define-key map (kbd "C-c C-b e") 'web-mode-block-end)
    (define-key map (kbd "C-c C-b k") 'web-mode-block-kill)
    (define-key map (kbd "C-c C-b n") 'web-mode-block-next)
    (define-key map (kbd "C-c C-b p") 'web-mode-block-previous)
    (define-key map (kbd "C-c C-b s") 'web-mode-block-select)

    (define-key map (kbd "C-c C-d a") 'web-mode-dom-apostrophes-replace)
    (define-key map (kbd "C-c C-d n") 'web-mode-dom-normalize)
    (define-key map (kbd "C-c C-d d") 'web-mode-dom-errors-show)
    (define-key map (kbd "C-c C-d e") 'web-mode-dom-entities-replace)
    (define-key map (kbd "C-c C-d q") 'web-mode-dom-quotes-replace)
    (define-key map (kbd "C-c C-d t") 'web-mode-dom-traverse)
    (define-key map (kbd "C-c C-d x") 'web-mode-dom-xpath)

    (define-key map (kbd "C-c C-e b") 'web-mode-element-beginning)
    (define-key map (kbd "C-c C-e c") 'web-mode-element-clone)
    (define-key map (kbd "C-c C-e d") 'web-mode-element-child)
    (define-key map (kbd "C-c C-e e") 'web-mode-element-end)
    (define-key map (kbd "C-c C-e f") 'web-mode-element-children-fold-or-unfold)
    (define-key map (kbd "C-c C-e i") 'web-mode-element-content-select)
    (define-key map (kbd "C-c C-e k") 'web-mode-element-kill)
    (define-key map (kbd "C-c C-e m") 'web-mode-element-mute-blanks)
    (define-key map (kbd "C-c C-e n") 'web-mode-element-next)
    (define-key map (kbd "C-c C-e p") 'web-mode-element-previous)
    (define-key map (kbd "C-c C-e r") 'web-mode-element-rename)
    (define-key map (kbd "C-c C-e s") 'web-mode-element-select)
    (define-key map (kbd "C-c C-e t") 'web-mode-element-transpose)
    (define-key map (kbd "C-c C-e u") 'web-mode-element-parent)
    (define-key map (kbd "C-c C-e v") 'web-mode-element-vanish)
    (define-key map (kbd "C-c C-e w") 'web-mode-element-wrap)

    (define-key map (kbd "C-c C-t b") 'web-mode-tag-beginning)
    (define-key map (kbd "C-c C-t e") 'web-mode-tag-end)
    (define-key map (kbd "C-c C-t m") 'web-mode-tag-match)
    (define-key map (kbd "C-c C-t n") 'web-mode-tag-next)
    (define-key map (kbd "C-c C-t p") 'web-mode-tag-previous)
    (define-key map (kbd "C-c C-t s") 'web-mode-tag-select)

    ;;--------------------------------------------------------------------------

;;    (define-key map (kbd "C-;")       'web-mode-comment-or-uncomment)
    (define-key map (kbd "M-;")       'web-mode-comment-or-uncomment)

    ;;C-c C-b : block
    ;;C-c C-d : dom
    ;;C-c C-e : element
    (define-key map (kbd "C-c C-f")   'web-mode-fold-or-unfold)
    (define-key map (kbd "C-c C-i")   'web-mode-buffer-indent)
    (define-key map (kbd "C-c C-j")   'web-mode-jshint)
    (define-key map (kbd "C-c C-m")   'web-mode-mark-and-expand)
    (define-key map (kbd "C-c C-n")   'web-mode-navigate)
    (define-key map (kbd "C-c C-s")   'web-mode-snippet-insert)
    ;;C-c C-t : tag
    (define-key map (kbd "C-c C-w")   'web-mode-whitespaces-show)

    ;; compatibility with nxml
    ;;(define-key map (kbd "M-C-u")     'web-mode-element-parent)
    ;;(define-key map (kbd "M-C-d")     'web-mode-element-child)
    ;;(define-key map (kbd "M-C-n")     'web-mode-element-next)
    ;;(define-key map (kbd "M-C-p")     'web-mode-element-previous)

    ;;(define-key map (kbd "C-c /")     'web-mode-element-close)
    ;;(define-key map (kbd "C-c <")     'web-mode-element-beginning)
    ;;(define-key map (kbd "C-c >")     'web-mode-element-end)

    ;;(define-key map (kbd "C-c C-c")   'web-mode-block-close)
    ;;(define-key map (kbd "C-c C-n")   'web-mode-tag-match)
    ;;(define-key map (kbd "C-c C-p")   'web-mode-element-parent)
    ;;(define-key map (kbd "C-c C-v")   'web-mode-dom-traverse)

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

  ) ;eval-and-compile

;;(defvar web-mode-buffer-highlighted nil)

(defvar web-mode-font-lock-keywords
  '(web-mode-font-lock-highlight))

(defun web-mode-font-lock-extend-region ()
  (save-excursion
    ;;    (message "before : font-lock-beg=%S - font-lock-end=%S" font-lock-beg font-lock-end)
    (if (string= web-mode-engine "razor")
        (setq font-lock-beg (point-min)
              font-lock-end (point-max))
      (setq font-lock-beg (or (web-mode-previous-tag-at-bol-pos font-lock-beg)
                              (point-min))
            font-lock-end (or (web-mode-next-tag-at-eol-pos font-lock-end)
                              (point-max))))
    nil))

(defun web-mode-font-lock-highlight (limit)
  "font-lock matcher"
;;  (message "web-mode-font-lock-highlight : point=%S limit=%S" (point) limit)
  (web-mode-highlight-region (point) limit)
  nil)

;;;###autoload
(define-derived-mode web-mode web-mode-prog-mode "Web"
  "Major mode for editing web templates (HTML documents with embedded parts and blocks)."

  (make-local-variable 'web-mode-auto-pairs)
  (make-local-variable 'web-mode-buffer-highlighted)
  (make-local-variable 'web-mode-comment-style)
  (make-local-variable 'web-mode-content-type)
  (make-local-variable 'web-mode-display-table)
  (make-local-variable 'web-mode-engine)
  (make-local-variable 'web-mode-block-regexps)
  (make-local-variable 'web-mode-enable-block-face)
  (make-local-variable 'web-mode-enable-part-face)
  (make-local-variable 'web-mode-engine-file-regexps)
  (make-local-variable 'web-mode-expand-initial-pos)
  (make-local-variable 'web-mode-expand-previous-state)
  (make-local-variable 'web-mode-has-any-large-block)
  (make-local-variable 'web-mode-has-any-large-part)
  (make-local-variable 'web-mode-hl-line-mode-flag)
  (make-local-variable 'web-mode-indent-style)
  (make-local-variable 'web-mode-is-narrowed)
  (make-local-variable 'web-mode-jshint-errors)
  (make-local-variable 'web-mode-block-regexp)
  (make-local-variable 'web-mode-start-tag-overlay)
  (make-local-variable 'web-mode-end-tag-overlay)
  (make-local-variable 'web-mode-time)

  (make-local-variable 'after-change-functions)
  (make-local-variable 'change-major-mode-hook)
  (make-local-variable 'fill-paragraph-function)
;;  (make-local-variable 'font-lock-beg)
  (make-local-variable 'font-lock-defaults)
;;  (make-local-variable 'font-lock-end)
  (make-local-variable 'font-lock-extend-region-functions)
  (make-local-variable 'font-lock-maximum-size)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'imenu-case-fold-search)
  (make-local-variable 'imenu-create-index-function)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'indent-line-function)

  (setq fill-paragraph-function 'web-mode-fill-paragraph
        font-lock-defaults '(web-mode-font-lock-keywords t)
        font-lock-extend-region-functions '(web-mode-font-lock-extend-region)
        font-lock-support-mode nil
        font-lock-maximum-size nil
        ;;        font-lock-fontify-buffer-function 'web-mode-scan-buffer
        ;;        font-lock-unfontify-buffer-function 'web-mode-scan-buffer
        imenu-case-fold-search t
        imenu-create-index-function 'web-mode-imenu-index
        indent-line-function 'web-mode-indent-line)

;;  (remove-hook 'after-change-functions 'font-lock-after-change-function t)

  (if (and (fboundp 'global-hl-line-mode)
           global-hl-line-mode)
      (setq web-mode-hl-line-mode-flag t))

  (when web-mode-enable-current-element-highlight
    (add-hook 'post-command-hook 'web-mode-highlight-current-element nil t))

  (add-hook 'after-change-functions 'web-mode-on-after-change t t)
  (add-hook 'change-major-mode-hook 'web-mode-on-exit nil t)

  (add-hook 'after-save-hook
            '(lambda ()
               (when web-mode-is-scratch
                 (web-mode-guess-engine-and-content-type)
                 (web-mode-scan-buffer)
;;                 (message "-->%S" (buffer-file-name))
                 )
               nil)
            t t)

  (cond
   ((boundp 'yas-after-exit-snippet-hook)
    (add-hook 'yas-after-exit-snippet-hook
              'web-mode-yasnippet-exit-hook
              t t))
   ((boundp 'yas/after-exit-snippet-hook)
    (add-hook 'yas/after-exit-snippet-hook
              'web-mode-yasnippet-exit-hook
              t t))
   )

  (when web-mode-enable-whitespaces
    (web-mode-whitespaces-on))

  (web-mode-guess-engine-and-content-type)
;;  (web-mode-scan-buffer)

  (web-mode-scan-region (point-min) (point-max))

  )

;; (add-hook 'web-mode-hook
;;           (lambda()
;;             (let (mode modes found)
;;               (setq modes '(esk-add-watchwords
;;                             esk-pretty-lambdas
;;                             fic-ext-mode
;;                             fic-mode
;;                             global-whitespace-mode
;;                             idle-highlight-mode
;;                             rainbow-mode
;;                             whitespace-mode))
;; ;;              (message "==> %S" column-number-mode)
;; ;;              (when (and (boundp 'global-whitespace-mode) global-whitespace-mode)
;; ;;                (message "==> %S" global-whitespace-mode))
;;               (dolist (mode modes)
;; ;;                (message "> %S (%S) has been disabled" mode (symbol-value mode))
;;                 (when (and (boundp mode) (symbol-value mode))
;;                   (message "=> %S (%S) has been disabled" mode (symbol-value mode))
;;                   (setq found t)
;;                   (funcall mode -1))
;;                 ) ;dolist
;;               (when found
;;                 (web-mode-scan-buffer)
;;                 )
;;               )))

(defun web-mode-yasnippet-exit-hook ()
  "Yasnippet exit hook"
  (when (and (boundp 'yas-snippet-beg) (boundp 'yas-snippet-end))
;;    (web-mode-highlight-region yas-snippet-beg yas-snippet-end)
    (indent-region yas-snippet-beg yas-snippet-end)))

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

(defun web-mode-set-engine (engine)
  "set engine"
  (interactive
   (list (completing-read
          "Engine: "
          (let (engines elt)
            (dolist (elt web-mode-engines)
              (setq engines (append engines (list (car elt)))))
            engines))))
  (setq web-mode-content-type "html"
        web-mode-engine engine)
  (web-mode-on-engine-setted)
  (web-mode-scan-buffer))

(defun web-mode-on-engine-setted ()
  "engine setted"
  (let (elt elts engines)
    (when (string= web-mode-engine "razor") (setq web-mode-enable-block-face t))
    (cond
     ((member web-mode-content-type '("css" "javascript" "json"))
      (setq web-mode-has-any-large-part t))
     ((member web-mode-content-type '("php"))
      (setq web-mode-has-any-large-block nil))
     ) ;cond

    (setq web-mode-electric-chars nil)
    (when (string= web-mode-content-type "html")
;;      (unless (string= web-mode-engine "none")
;;        (setq web-mode-active-block-regexp
;;              (cdr (assoc web-mode-engine web-mode-active-block-regexps)))
;;        (setq web-mode-close-block-regexp
;;              (cdr (assoc web-mode-engine web-mode-close-block-regexps)))
;;        (setq web-mode-engine-control-matcher
;;              (intern-soft (concat "web-mode-match-" web-mode-engine "-block")))
;;        )
      (setq web-mode-electric-chars
            (append '(?\<)
                    (cdr (assoc web-mode-engine web-mode-block-electric-chars)))
            )
      ) ;when

    (setq elt (assoc web-mode-engine web-mode-block-regexps))
    (if elt
        (setq web-mode-block-regexp (cdr elt))
      (setq web-mode-engine "none"))

    (unless (boundp 'web-mode-extra-auto-pairs)
      (setq web-mode-extra-auto-pairs nil))

    (setq web-mode-auto-pairs
          (append
           (cdr (assoc web-mode-engine web-mode-engines-auto-pairs))
           (cdr (assoc nil web-mode-engines-auto-pairs))
           (cdr (assoc web-mode-engine web-mode-extra-auto-pairs))
           (cdr (assoc nil web-mode-extra-auto-pairs))))

    (unless (boundp 'web-mode-extra-snippets)
      (setq web-mode-extra-snippets nil))

    (setq elts
          (append
           (cdr (assoc web-mode-engine web-mode-extra-snippets))
           (cdr (assoc nil             web-mode-extra-snippets))
           (cdr (assoc web-mode-engine web-mode-engines-snippets))
           (cdr (assoc nil             web-mode-engines-snippets))))

    (dolist (elt elts)
      (unless (assoc (car elt) web-mode-snippets)
        (setq web-mode-snippets (append (list elt) web-mode-snippets)))
      )

;;    (message "wms=%S" web-mode-snippets)

    (setq web-mode-closing-blocks (cdr (assoc web-mode-engine web-mode-engines-closing-blocks)))

    (setq web-mode-engine-font-lock-keywords
          (symbol-value (cdr (assoc web-mode-engine web-mode-engines-font-lock-keywords))))

;;    (message "%S" (symbol-value (cdr (assoc web-mode-engine web-mode-engines-font-lock-keywords))))

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
        (cond
         ((stringp (cdr elt))
          (when (string-match-p (cdr elt) buff-name)
            (setq web-mode-engine (car elt))))
         ((functionp (cdr elt))
          (when (funcall (cdr elt))
            (setq web-mode-engine (car elt))))
         ) ;cond
        ) ;dolist
      ) ;when
    (unless web-mode-engine
      (setq found nil)
      (dolist (elt web-mode-engine-file-regexps)
;;          (message "%S %S" (cdr elt) buff-name)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-engine (car elt)
                found t))
        )
      )
    (when web-mode-engine
      (setq found nil)
      (dolist (elt web-mode-engines)
        (when (and (not found) (member web-mode-engine (cdr elt)))
          (setq web-mode-engine (car elt)
                found t))
        )
      )
    (when (and (null found)
               (string-match-p "php" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))
      (setq web-mode-engine "php"
            found t)
      )

;;    (message "engine=%S" web-mode-engine)

    (web-mode-on-engine-setted)
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
  (web-mode-scan-region (point-min) (point-max))
  (font-lock-fontify-buffer))

;;(syntax-propertize)

(defun web-mode-scan-region (beg end &optional content-type)
  "Identify nodes/parts/blocks and syntactic symbols (strings/comments)."
  (interactive)
;;  (message "lexing buffer from %d to %d" beg end)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-modification-hooks t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (setq beg (if web-mode-is-narrowed 1 beg))
           (remove-text-properties beg end web-mode-scan-properties)
           (cond
            ((member web-mode-content-type '("javascript" "json" "css"))
             (web-mode-scan-blocks beg end)
             (web-mode-scan-part beg end)
             )
            ((string= web-mode-engine "none")
             (web-mode-scan-tags beg end)
             (web-mode-scan-parts beg end)
             )
            ;; ((and content-type (member content-type '("css")))
            ;;  (remove-text-properties beg end web-mode-scan-properties2)
            ;;  (web-mode-scan-blocks beg end)
            ;;  (web-mode-scan-part beg end)
            ;;  )
            ;; ((and content-type (member content-type '("asp")))
            ;;  (remove-text-properties beg end '(block-token nil font nil))
            ;;  (web-mode-scan-block beg end)
            ;;  )
            (t
             (web-mode-scan-blocks beg end)
             (web-mode-scan-tags beg end)
             (web-mode-scan-parts beg end)
             )
            ) ;cond
           (web-mode-trace "web-mode-scan-region")
           ))))))

(defun web-mode-highlight-region (beg end &optional content-type)
  "Identify code blocks (clientside and serverside) and syntactic symbols (strings/comments)."
  (interactive)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-modification-hooks t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (setq beg (if web-mode-is-narrowed 1 beg))
           (remove-text-properties beg end '(font-lock-face nil))
           (cond
            ((member web-mode-content-type '("javascript" "json" "css"))
             (web-mode-highlight-part beg end)
             (web-mode-highlight-blocks beg end))
            ((string= web-mode-engine "none")
             (web-mode-highlight-tags beg end)
             (web-mode-highlight-parts beg end))
            ;; ((and content-type (member content-type '("css")))
            ;;  (web-mode-highlight-part beg end)
            ;;  (web-mode-highlight-blocks beg end))
            ;; ((and content-type (member content-type '("asp")))
            ;;  (web-mode-highlight-block beg end)
            ;;  )
            (t
             (web-mode-highlight-tags beg end)
             (web-mode-highlight-parts beg end)
             (web-mode-highlight-blocks beg end))
            ) ;cond
           (when web-mode-enable-whitespaces
             (web-mode-highlight-whitespaces beg end))
           ))))))

(defun web-mode-scan-blocks (reg-beg reg-end)
  "Identifies blocks (with block-side, block-beg, block-end text properties)."
  (save-excursion

    (let ((i 0) open close closing-string start sub1 sub2 pos tagopen l tmp delim-open delim-close)

      (goto-char reg-beg)

      ;;     (message "%S: %Sx%S" (point) reg-beg reg-end)
      ;;     (message "regexp=%S" web-mode-block-regexp)
      (while (and (< i 1200)
                  (> reg-end (point))
                  web-mode-block-regexp
                  (re-search-forward web-mode-block-regexp reg-end t)
                  (not (eobp)))

        (setq i (1+ i)
              closing-string nil
              close nil
              tagopen (match-string 0)
              open (match-beginning 0)
              delim-open nil
              delim-close nil
              pos nil)
        (setq l (length tagopen))

        (when (member (string-to-char tagopen) '(?\s ?\t))
          (setq tagopen (replace-regexp-in-string "\\`[ \t]*" "" tagopen))
;;          (message "tagopen=%s (%S)" tagopen (point))
          (setq open (+ open (- l (length tagopen))))
          (setq l (length tagopen))
          )

        (setq sub1 (substring tagopen 0 1)
              sub2 (substring tagopen 0 (if (>= l 2) 2 1)))

        (cond

         ((string= web-mode-engine "php")
          (unless (member (char-after) '(?x ?X))
            (setq closing-string '("<\\?". "\\?>")))
          (cond
           ((eq (char-after) ?p)
            (setq delim-open "<?php"))
           ((eq (char-after) ?\=)
            (setq delim-open "<?="))
           (t
            (setq delim-open "<?"))
           ) ;cond
          (setq delim-close "?>")
          ) ;php

         ((string= web-mode-engine "django")
          (cond
           ((string= sub2 "{{")
            (setq closing-string '("{{" . "}}")
                  delim-open "{{"
                  delim-close "}}"))
           ((string= sub2 "{%")
            (setq closing-string "%}"
                  delim-open "{%[+]?"
                  delim-close "%}"))
           (t
            (setq closing-string "#}"))
           )
          ) ;django

         ((string= web-mode-engine "erb")
          (cond
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%[=-]?"
                  delim-close "[-]?%>")
            )
           (t
            (setq closing-string "EOL"
                  delim-open "%"))
           )
          ) ;erb

         ((string= web-mode-engine "mako")
          (cond
           ((member tagopen '("<% " "<%!"))
            (setq closing-string "%>"
                  delim-open "<%[!]?"
                  delim-close "%>"))
           ((member sub2 '("<%" "</"))
            (setq closing-string ">"
                  delim-open "</?%"
                  delim-close ">"))
           ((string= sub2 "${")
            (setq closing-string "}"
                  delim-open "${"
                  delim-close "}"))
           (t
            (setq closing-string "EOL"
                  delim-open "%"))
           )
          ) ;mako

         ((string= web-mode-engine "ctemplate")
          (cond
           ((string= tagopen "{{{")
            (setq closing-string "}}}"
                  delim-open "{{{"
                  delim-close "}}}")
            )
           ((string= sub2 "{{")
            (setq closing-string "}}"
                  delim-open "{{[>#/%^&]?"
                  delim-close "}}"))
           (t
            (setq closing-string "}}"
                  delim-open "${{"
                  delim-close "}}"))
           )
          ) ;ctemplate

         ((or (string= web-mode-engine "asp")
              (string= web-mode-engine "aspx"))
          (setq closing-string "%>"
                delim-open "<%[:=#]?"
                delim-close "%>")
          ) ;asp

         ((string= web-mode-engine "blade")
          (cond
           ((string= tagopen "{{-")
            (setq closing-string "--}}"))
           ((string= sub2 "{{")
            (setq closing-string "}}"
                  delim-open "{{"
                  delim-close "}}"))
           ((string= sub1 "@")
            (setq closing-string "EOL"
                  delim-open "@"))
           )
          ) ;blade

         ((string= web-mode-engine "smarty")
;;          (message "l=%S tagopen=%s" l tagopen)
;;          (setq l (length (web-mode-engine-delimiter-open web-mode-engine "{")))
;;          (setq tagopen (substring-no-properties tagopen 0 l))
          (cond
           ((string= tagopen (concat (web-mode-engine-delimiter-open web-mode-engine "{") "*"))
            (setq closing-string (concat "*" (web-mode-engine-delimiter-close web-mode-engine "}")))
            )
           ((string= tagopen (concat (web-mode-engine-delimiter-open web-mode-engine "{") "#"))
            (setq closing-string (concat "#" (web-mode-engine-delimiter-close web-mode-engine "}"))
                  delim-open (concat (web-mode-engine-delimiter-open web-mode-engine "{") "#")
                  delim-close (concat "#" (web-mode-engine-delimiter-close web-mode-engine "}")))
            )
           (t
            (setq closing-string (cons (web-mode-engine-delimiter-open web-mode-engine "{")
                                       (web-mode-engine-delimiter-close web-mode-engine "}"))
                  delim-open (concat (web-mode-engine-delimiter-open web-mode-engine "{") "/?")
                  delim-close (web-mode-engine-delimiter-close web-mode-engine "}"))
;;            (message "delim-open=%s delim-close=%s" delim-open delim-close)
            ) ;t
           ) ;cond
          ) ;smarty

         ((string= web-mode-engine "web2py")
          (setq closing-string "}}"
                delim-open "{{[=]?"
                delim-close "}}")
          ) ;web2py

         ((string= web-mode-engine "dust")
          (cond
           ((string= sub2 "{!")
            (setq closing-string "!}"))
           (t
            (setq closing-string "}"
                  delim-open "{[#/:?@><+^]?"
                  delim-close "/?}")
            )
           )
          ) ;dust

         ((string= web-mode-engine "closure")
          (cond
           ((string= sub2 "//")
            (setq closing-string "EOL")
            )
           ((string= sub2 "/*")
            (setq closing-string "*/")
            )
           (t
            (setq closing-string "}"
                  delim-open "{/?"
                  delim-close "/?}")
            )
           )
          ) ;closure

         ((string= web-mode-engine "ctemplate")
          (cond
           ((string= tagopen "{{{")
            (setq closing-string "}}}"))
           (t
            (setq closing-string "}}"))
           )
          ) ;ctemplate

         ((string= web-mode-engine "go")
          (setq closing-string "}}"
                delim-open "{{"
                delim-close "}}")
          ) ;go

         ((string= web-mode-engine "angular")
          (setq closing-string "}}"
                delim-open "{{"
                delim-close "}}")
          ) ;angular

         ((string= web-mode-engine "mason")
          (cond
           ((and (member sub2 '("<%" "</"))
                 ;;                 (progn (message "%S" (point)) t)
                 (looking-at "[[:alpha:]]+"))
            ;;(member tagopen '("<%def" "</%def" "<%method" "</%method"))
            (if (member (match-string-no-properties 0) '("def" "method"))
                (setq closing-string ">"
                      delim-open "<[^>]+>")
              (setq closing-string (concat "</%" (match-string-no-properties 0) ">")
                    delim-open "<[^>]+>"
                    delim-close "<[^>]+>")
              ) ;if
;;            (message "closing-string=%S" closing-string)
            )
           ((and (string= sub2 "<%")
                 (eq (char-after) ?\s))
            (setq closing-string "%>"
                  delim-open "<%"
                  delim-close "%>"))
           ;; ((member sub2 '("<%" "</"))
           ;;  (setq closing-string ">"
           ;;        delim-open "<%"
           ;;        delim-close ">"))
           ((string= tagopen "</&")
            (setq closing-string ">"
                  delim-open "</&"
                  delim-close ">")
            )
           ((string= sub2 "<&")
            (setq closing-string "&>"
                  delim-open "<&[|]?"
                  delim-close "&>"))
           (t
            (setq closing-string "EOL"
                  delim-open "%"))
           )
          ) ;mason

         ((string= web-mode-engine "jsp")
          (cond
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%\\([!=@]\\|#=\\)?"
                  delim-close "[-]?%>"))
           ((string= sub2 "${")
            (setq closing-string "}"
                  delim-open "${"
                  delim-close "}"))
           (t
            (setq closing-string ">"
                  delim-open "</?"
                  delim-close "/?>"))
           )
          ) ;jsp

         ((string= web-mode-engine "underscore")
          (setq closing-string "%>"
                delim-open "<%"
                delim-close "%>")
          ) ;underscore

         ((string= web-mode-engine "template-toolkit")
          (cond
           ((string= sub2 "[#")
            (setq closing-string "#]"))
           (t
            (setq closing-string "%]"
                  delim-open "\\[%[-+]?"
                  delim-close "[-=+]?%\\]"))
           )
          ) ;template-toolkit

         ((string= web-mode-engine "freemarker")
          (cond
           ((string= sub1 "<")
            (setq closing-string ">"
                  delim-open "</?[#@]?"
                  delim-close "/?>"))
           ((string= sub1 "[")
            (setq closing-string "]"
                  delim-open "\\[/?[#@]"
                  delim-close "/?\\]"))
           (t
            (setq closing-string "}"
                  delim-open "${"
                  delim-close "}"))
           )
          ) ;freemarker

         ((string= web-mode-engine "velocity")
          (cond
           ((string= sub2 "##")
            (setq closing-string "EOL"))
           ((string= sub2 "#*")
            (setq closing-string "*#"))
           (t
            (setq closing-string "EOV"
                  delim-open "#"))
           )
          ) ;velocity

         ((string= web-mode-engine "razor")
;;          (message "sub2=%S" sub2)
          (cond
           ((string= sub2 "@@")
            (forward-char 2)
            (setq closing-string nil))
           ((string= sub2 "@*")
            (setq closing-string "*@"))
           ((string= sub1 "@")
            (setq closing-string "EOR"
                  delim-open "@"))
           ((string= sub1 "}")
            (setq closing-string "EOR"))
           )
          ) ;razor

         ) ;cond

        (when closing-string

          (cond

           ((listp closing-string)
            (if (web-mode-rsf-balanced (car closing-string) (cdr closing-string) reg-end t)
                (setq close (match-end 0)
                      pos (point))
              (when (and (string= web-mode-engine "php")
                         (string= "<?" sub2))
                (setq close (point-max)
                      delim-close nil
                      pos (point-max)))
              ) ;if
            )

           ((and (string= web-mode-engine "smarty")
                 (string= closing-string (web-mode-engine-delimiter-close web-mode-engine "}")))
            (goto-char open)
            (setq tmp (web-mode-closing-delimiter-position
                       (web-mode-engine-delimiter-close web-mode-engine "}")
                       (point)
                       (line-end-position)))
            (if tmp
                (setq tmp (1+ tmp))
              (setq tmp (line-end-position)))
            (goto-char tmp)
            (setq close (point)
                  pos (point))
            )

           ((and (member web-mode-engine '("closure" "dust"))
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
            (setq close (if (> (point) reg-end) reg-end (point))
                  pos (if (> (point) reg-end) reg-end (point))) ;; (point))
            (goto-char pos)
;;            (message "pos=%S close=%S" pos close)
            )

           ((string= closing-string "EOV")
            (web-mode-velocity-skip-forward open)
            (setq close (point)
                  pos (point)))

           ((search-forward closing-string reg-end t)
            (setq close (match-end 0)
                  pos (point))

            )

           ) ;cond

          (when (and close (>= reg-end pos))
            ;;(message "pos(%S) : open(%S) close(%S)" pos open close)
            (put-text-property open close 'block-side t)
            (put-text-property open (1+ open) 'block-beg 0)
            (put-text-property (1- close) close 'block-end t)

            (when delim-open
              (web-mode-block-delimiters-set open close delim-open delim-close))
            (if (string= web-mode-engine "razor")
                (web-mode-razor-tag-exclude open close)
              (web-mode-scan-block open close))
            )

          (if pos (goto-char pos))

          ) ;when closing-string

        ) ;while

      (cond
       ((>= i 1200)
        (message "** strange loop (web-mode-scan-blocks) **"))
       ((string= web-mode-engine "razor")
        (web-mode-process-blocks reg-beg reg-end "scan"))
       ((string= web-mode-engine "django")
        (web-mode-scan-django-block-comments reg-beg reg-end))
       ((string= web-mode-engine "mako")
        (web-mode-scan-mako-block-comments reg-beg reg-end))
       ) ;cond

      )))

(defun web-mode-block-delimiters-set (reg-beg reg-end delim-open delim-close)
  "Set text-property 'block-token to 'delimiter on block delimiters (e.g. <?php ?>)"
;;  (message "reg-beg(%S) reg-end(%S) delim-open(%S) delim-close(%S)" reg-beg reg-end delim-open delim-close)
  (when (member web-mode-engine '("asp" "aspx" "closure" "ctemplate" "django" "dust"
                                  "erb" "freemarker" "jsp" "mako" "mason"
                                  "smarty" "template-toolkit" "web2py"))
    (save-excursion
      (goto-char reg-beg)
      (looking-at delim-open)
      (setq delim-open (match-string-no-properties 0))
      (goto-char reg-end)
      (looking-back delim-close reg-beg t)
      (setq delim-close (match-string-no-properties 0))
      ))
;;  (message "reg-beg(%S) reg-end(%S) delim-open(%S) delim-close(%S)" reg-beg reg-end delim-open delim-close)
  (put-text-property reg-beg (+ reg-beg (length delim-open)) 'block-token 'delimiter)
  (when delim-close
;;    (message "%S > %S" (- reg-end (length delim-close)) reg-end)
    (put-text-property (- reg-end (length delim-close)) reg-end 'block-token 'delimiter)
    ))

(defun web-mode-highlight-blocks (reg-beg reg-end)
  "Highlight blocks."
  (web-mode-process-blocks reg-beg reg-end "highlight"))

;;todo : passer en funcall
(defun web-mode-process-blocks (reg-beg reg-end type)
  "Process blocks. The scan relies on the 'block-beg and 'block-end text-properties."
  (let ((i 0) (continue t) (block-beg reg-beg) (block-end nil))
    (while continue
      (setq block-end nil
            i (1+ i))
      (unless (get-text-property block-beg 'block-beg)
        (setq block-beg (web-mode-block-next-position block-beg)))
      (when (and block-beg (< block-beg reg-end))
        (setq block-end (web-mode-block-end-position block-beg)))
      (cond
       ((or (null block-end) (> block-end reg-end) (> i 1200))
        (setq continue nil)
        (if (> i 1200) (message "*** invalid loop (web-mode-process-blocks) ***")))
       (t
        (setq block-end (1+ block-end))
        (cond
         ((string= type "scan")
          (web-mode-scan-block block-beg block-end))
         (t
          (web-mode-highlight-block block-beg block-end))
         )
        (setq block-beg block-end)
        )
       ) ;cond
      ) ;while
    ))

(defun web-mode-scan-parts (reg-beg reg-end)
  "Scan parts."
  (web-mode-process-parts reg-beg reg-end "scan"))

(defun web-mode-highlight-parts (reg-beg reg-end)
  "Highlight parts."
  (web-mode-process-parts reg-beg reg-end "highlight"))

;;todo : passer en funcall
(defun web-mode-process-parts (reg-beg reg-end type)
  "Process parts. The scan relies on the 'part-beg and 'part-end text-properties."
  (let ((i 0) (continue t) (part-beg reg-beg) (part-end nil))
    (while continue
      (setq part-end nil
            i (1+ i))
      (unless (get-text-property part-beg 'part-side)
        (setq part-beg (web-mode-part-next-position part-beg)))
      (when (and part-beg (< part-beg reg-end))
        (setq part-end (web-mode-part-end-position part-beg)))
      (cond
       ((or (null part-end) (> part-end reg-end) (> i 1200))
        (setq continue nil)
        (if (> i 1200) (message "*** invalid loop (web-mode-process-parts) ***")))
       (t
        (setq part-end (1+ part-end))
        (cond
         ((string= type "scan")
          (web-mode-scan-part part-beg part-end))
         (t
          (web-mode-highlight-part part-beg part-end))
         )
        (setq part-beg part-end)
        )
       ) ;cond
      ) ;while
    ))

(defun web-mode-scan-block (reg-beg reg-end)
  "Scan a block."
  (let (sub1 sub2 sub3 regexp props continue beg match char (flags 0))

;;    (message "reg-beg=%S reg-end=%S" reg-beg reg-end)
    ;;(remove-text-properties reg-beg reg-end web-mode-scan-properties)

    (when (and (not web-mode-has-any-large-block)
               (> (- reg-end reg-beg) web-mode-large-embed-threshold))
      (setq web-mode-has-any-large-block t))

    (goto-char reg-beg)

    (setq sub1 (buffer-substring-no-properties reg-beg (+ reg-beg 1)))
    (setq sub2 sub1
          sub3 sub1)
    (when (>= (point-max) (+ reg-beg 2))
      (setq sub2 (buffer-substring-no-properties reg-beg (+ reg-beg 2)))
      (setq sub3 sub2)
      (when (>= (point-max) (+ reg-beg 3))
        (setq sub3 (buffer-substring-no-properties reg-beg (+ reg-beg 3)))
        )
      )

    (cond

     ((string= web-mode-engine "php")
      (setq regexp "//\\|/\\*\\|\"\\|'\\|<<<['\"]?\\([[:alnum:]]+\\)['\"]?")
      (setq flags (logior flags 1))
      ) ;php

     ((string= web-mode-engine "django")
      (cond
       ((member sub2 '("{{" "{%"))
        (setq regexp "\"\\|'"))
       ((string= sub2 "{#")
        (setq props '(block-token comment)))
       )
      ) ;django

     ((string= web-mode-engine "ctemplate")
      (cond
       ((string= sub3 "{{%")
        (setq regexp "\"\\|'"))
       ((string= sub3 "{{!")
        (setq props '(block-token comment)))
       )
      ) ;ctemplate

     ((string= web-mode-engine "go")
      (cond
       ((string= sub3 "{{/")
        (setq props '(block-token comment)))
       ((string= sub2 "{{")
        (setq regexp "\"\\|'"))
       )
      ) ;go

     ((string= web-mode-engine "razor")
      (cond
       ((string= sub2 "@*")
        (setq props '(block-token comment)))
       (t
        (setq regexp "//\\|\"\\|'"))
       )
      ) ;razor

     ((string= web-mode-engine "mako")
      (setq regexp "\"\\|'\\|#")
      ) ;mako

     ((string= web-mode-engine "python")
      (setq regexp "\"\\|'\\|#")
      ) ;python

     ((string= web-mode-engine "web2py")
      (setq regexp "\"\\|'")
      ) ;web2py

     ((string= web-mode-engine "blade")
      (cond
       ((string= sub3 "{{-")
        (setq props '(block-token comment)))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;blade

     ((string= web-mode-engine "velocity")
      (cond
       ((member sub2 '("##" "#*"))
        (setq props '(block-token comment)))
       ((string= sub1 "$")
        (setq regexp "\"\\|'"))
       ((string= sub1 "#")
        (setq regexp "\"\\|'"))
       )
      ) ;velocity

     ((string= web-mode-engine "jsp")
      (cond
       ((string= sub3 "<%-")
        (setq props '(block-token comment)))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"))
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"))
       ((string= sub2 "<%")
        (setq regexp "//\\|/\\*\\|\"\\|'"))
       )
      ) ;jsp

     ((string= web-mode-engine "freemarker")
      (cond
       ((member sub3 '("<#-" "[#-"))
        (setq props '(block-token comment)))
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"))
       ((or (member sub2 '("<@" "[@" "<#" "[#"))
            (member sub3 '("</@" "[/@" "</#" "[/#")))
        (setq regexp "\""))
       )
      ) ;freemarker

     ((string= web-mode-engine "erb")
      (cond
       ((string= sub3 "<%#")
        (setq props '(block-token comment)))
       (t
        (setq regexp "\"\\|'\\|#\\|<<[-]?['\"]?\\([[:alnum:]_]+\\)['\"]?"))
       )
      ) ;erb

     ((string= web-mode-engine "mason")
      (setq regexp "\"\\|'\\|#")
      ) ;mason

     ((string= web-mode-engine "asp")
      (setq regexp "//\\|/\\*\\|\"\\|'")
      ) ;asp

     ((string= web-mode-engine "template-toolkit")
      (cond
       ((string= sub2 "[#")
        (setq props '(block-token comment)))
       (t
        (setq regexp "#\\|\"\\|'"))
       )
      ) ;template-toolkit

     ((string= web-mode-engine "underscore")
      (setq regexp "/\\*\\|\"\\|'")
      ) ;underscore

     ((string= web-mode-engine "angular")
      ) ;angular

     ((string= web-mode-engine "aspx")
      (cond
       ((string= sub3 "<%-")
        (setq props '(block-token comment)))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"))
       ((string= sub3 "<%$")
        (setq regexp "\"\\|'"))
       (t
        (setq regexp "//\\|/\\*\\|\"\\|'"))
       )
      ) ;aspx

     ((string= web-mode-engine "smarty")
      (cond
       ((string= sub2 (concat (web-mode-engine-delimiter-open web-mode-engine "{") "*"))
        (setq props '(block-token comment)))
       (t
        (setq regexp "\"\\|'")))
      ) ;smarty

     ((string= web-mode-engine "dust")
      (cond
       ((string= sub2 "{!")
        (setq props '(block-token comment)))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;dust

     ((string= web-mode-engine "closure")
      (cond
       ((member sub2 '("/*" "//"))
        (setq props '(block-token comment)))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;closure

     ) ;cond

    (when props (add-text-properties reg-beg reg-end props))

    (when regexp

      (goto-char reg-beg)

      (while (re-search-forward regexp reg-end t)

        (setq beg (match-beginning 0)
              match (match-string 0)
              continue t
              flags (logior flags 1))

        (setq char (aref match 0))

        (cond

         ((and (string= web-mode-engine "asp")
               (eq char ?\'))
          (setq props '(block-token comment))
          (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
          )

         ((eq char ?\')
          (setq props '(block-token string))
          (while (and continue (search-forward "'" reg-end t))
            (if (looking-back "\\\\+'" reg-beg t)
                (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
              (setq continue nil))
            )
          )

         ((eq char ?\")
;;          (message "pt=%S" (point))
          (setq props '(block-token string))
          (while (and continue (search-forward "\"" reg-end t))
            (if (looking-back "\\\\+\"" reg-beg t)
                (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
              (setq continue nil))
            )
          )

         ((string= match "//")
          (setq props '(block-token comment))
          (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
          )

         ((eq char ?\#)
          (setq props '(block-token comment))
          (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
          )

         ((string= match "/*")
          (setq props '(block-token comment))
          (search-forward "*/" reg-end t)
          )

         ((eq char ?\<)
          (when (and web-mode-enable-heredoc-fontification
                     (string-match-p "JS\\|JAVASCRIPT\\|HTM\\|CSS" (match-string 1)))
            (setq flags (logior flags 2))
;;            (message "%S flags=%S" (point) flags)
            )
          (setq props '(block-token string))
          (re-search-forward (concat "^[ ]*" (match-string 1)) reg-end t)
          )

         ((and (member web-mode-engine '("python" "erb"))
               (eq char ?\#))
          (setq props '(block-token comment))
          (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
          )

         ) ;cond

;;        (message "%S %S %S" beg (point) props)
        (add-text-properties beg (point) props)

        ) ;while

      (put-text-property reg-beg (1+ reg-beg) 'block-beg flags)

      ) ;when regexp

    (web-mode-block-controls-set reg-beg reg-end)

    ))

(defun web-mode-set-php-controls (reg-beg reg-end)
  "web-mode-set-php-controls"
  (goto-char reg-beg)
  (let (match controls
        (continue t)
        (regexp "endif\\|endforeach\\|endfor\\|endwhile\\|else\\|elsif\\|if\\|foreach\\|for\\|while"))
    (while continue
      (if (not (web-mode-rsf regexp reg-end))
          (setq continue nil)
        (setq match (match-string-no-properties 0))
        (cond
         ((and (>= (length match) 4)
               (string= match "else")
               (looking-at-p "[ ]*:"))
          (setq controls (append controls (list (cons 'inside "if"))))
          )
         ((and (>= (length match) 3)
               (string= (substring match 0 3) "end"))
          (setq controls (append controls (list (cons 'close (substring match 3)))))
          )
         ((and (progn (skip-chars-forward "[ ]") t)
               (eq (char-after) ?\()
               (web-mode-closing-paren reg-end)
               (looking-at-p ")[ ]*:"))
          (setq controls (append controls (list (cons 'open match))))
          )
         ) ; cond
        ) ;if
      ) ;while
    (when (and controls (> (length controls) 1))
      (setq controls (web-mode-block-controls-clean controls)))
    controls))

(defun web-mode-block-controls-clean (controls)
  "clean block controls"
  (when (and (eq (car (car controls)) 'open)
             (member (cons 'close (cdr (car controls))) controls))
    (setq controls nil)
    )
  controls)

(defun web-mode-block-controls-set (reg-beg reg-end)
  "Set block properties"
  (save-excursion
    (goto-char reg-beg)
    (let (controls pos type control)

      (cond

       ;;todo
       ;; nettoyer
       ;; <?php if (): echo $x; endif; ?>
       ;; ((open . "if") (close . "if"))
       ;; -> nil
       ((string= web-mode-engine "php")
        ;; (when (web-mode-block-starts-with "end\\(if\\|foreach\\|for\\|while\\)[ ]*;" reg-beg)
        ;;   (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
        ;; (when (web-mode-block-ends-with "\\(else[ ]*:\\|elseif[^)]+)[ ]*:\\)" reg-beg)
        ;;   (setq controls (append controls (list (cons 'inside "if")))))
        ;; (when (web-mode-block-starts-with "\\(if\\|foreach\\|for\\|while\\)[^)]+)[ ]*:" reg-beg)
        ;;   (setq controls (append controls (list (cons 'open (match-string-no-properties 1))))))
        (setq controls (web-mode-set-php-controls reg-beg reg-end))
        (when (web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
        (when (web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
        ) ; php

       ((string= web-mode-engine "erb")
        (cond
         ((web-mode-block-starts-with "else" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with ".* do \\|for\\|if\\|unless" reg-beg)
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ; erb

       ((string= web-mode-engine "django")
        (cond
         ((web-mode-block-starts-with "\\(else\\|elsif\\|elif\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "\\(empty\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "for")))))
         ((web-mode-block-starts-with "end\\([[:alpha:]]+\\)" reg-beg)
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((web-mode-block-starts-with web-mode-django-control-blocks reg-beg)
          (setq controls (append controls (list (cons 'open (match-string-no-properties 1))))))
         )
        ) ;django

       ((string= web-mode-engine "smarty")
        (cond
         ((and (eq (char-after (+ (length (web-mode-engine-delimiter-open web-mode-engine "{")) reg-beg)) ?\/)
               (web-mode-block-starts-with "\\([[:alpha:]]+\\)" reg-beg))
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((web-mode-block-starts-with "\\(else\\|elseif\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "\\(block\\|foreach\\|for\\|if\\|section\\|while\\)")
          (setq controls (append controls (list (cons 'open (match-string-no-properties 1))))))
         )
        ) ;smarty

       ((string= web-mode-engine "web2py")
        (cond
         ((web-mode-block-starts-with "def" reg-beg)
          (setq controls (append controls (list (cons 'open "def")))))
         ((web-mode-block-starts-with "return" reg-beg)
          (setq controls (append controls (list (cons 'close "def")))))
         ((web-mode-block-starts-with "block" reg-beg)
          (setq controls (append controls (list (cons 'open "block")))))
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "block")))))
         ((web-mode-block-starts-with "pass" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "\\(except\\|finally\\|els\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "\\(if\\|for\\|try\\|while\\)")
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ;web2py

       ((string= web-mode-engine "dust")
        (cond
         ((eq (char-after (1- reg-end)) ?\/)
          )
         ((eq (char-after (1+ reg-beg)) ?\:)
          (setq pos (web-mode-block-control-previous-position 'open reg-beg))
          (when pos
            (setq controls (append controls
                                   (list
                                    (cons 'inside
                                          (cdr (car (get-text-property pos 'block-controls))))))))
          )
         ((looking-at "{/\\([[:alpha:]]+\\)")
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((looking-at "{[#?@><+^]\\([[:alpha:]]+\\)")
          (setq controls (append controls (list (cons 'open (match-string-no-properties 1))))))
         ) ;cond
        ) ;dust

       ((member web-mode-engine '("asp" "aspx" "underscore"))
        (cond
         ((web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
         ((web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
         )
        ) ;asp aspx underscore

       ((string= web-mode-engine "mako")
        (cond
         ((looking-at "</?%\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
         )
         ((web-mode-block-starts-with "\\(else\\|elsif\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "end\\(if\\|for\\)" reg-beg)
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((web-mode-block-starts-with "if\\|for" reg-beg)
          (setq controls (append controls (list (cons 'open (match-string-no-properties 0))))))
         )
        ) ;mako

       ((string= web-mode-engine "mason")
        (cond
         ((looking-at "</?%\\(def\\|method\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ) ;mason
        )

       ((string= web-mode-engine "ctemplate")
        (cond
         ((looking-at "{{[#^/][ ]*\\([[:alpha:]]+\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 2) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         )
        ) ;ctemplate

       ((string= web-mode-engine "blade")
        (cond
         ((not (eq (char-after) ?\@))
          )
         ((web-mode-block-starts-with
           "\\(?:end\\)?\\(foreach\\|forelse\\|for\\|if\\|section\\|stop\\|unless\\|while\\)"
           reg-beg)
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 0) ?e) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ((web-mode-block-starts-with "stop" reg-beg)
          (setq controls (append controls (list (cons 'inside "section")))))
         ((web-mode-block-starts-with "else\\|elseif" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         )
        ) ;blade

       ((string= web-mode-engine "closure")
        (cond
         ((eq (char-after (1- reg-end)) ?\/)
          )
         ((looking-at "alias\\|namespace")
          )
         ((web-mode-block-starts-with "ifempty" reg-beg)
          (setq controls (append controls (list (cons 'inside "foreach")))))
         ((web-mode-block-starts-with "else\\|elseif" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "case\\|default" reg-beg)
          (setq controls (append controls (list (cons 'inside "switch")))))
         ((looking-at
           "{/?\\(call\\|deltemplate\\|for\\|foreach\\|if\\|let\\|literal\\|msg\\|param\\|switch\\|template\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         )
        ) ;closure

       ((string= web-mode-engine "go")
        (cond
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "else" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "range\\|with" reg-beg)
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ;go

       ((string= web-mode-engine "template-toolkit")
        (cond
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "els" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "if\\|foreach\\|filter" reg-beg)
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ;template-toolkit

       ((string= web-mode-engine "velocity")
        (cond
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "els" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "define\\|if\\|for\\|foreach\\|macro" reg-beg)
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ;velocity

       ((string= web-mode-engine "jsp")
        (cond
         ((eq (char-after (1- reg-end)) ?\/)
          )
         ((looking-at "</?\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (when (not (member control '("h:inputtext" "jsp:usebean" "jsp:forward" "struts:property")))
            (setq controls (append controls (list (cons type control)))))
          )
         (t
          (when (web-mode-block-starts-with "}" reg-beg)
            (setq controls (append controls (list (cons 'close "{")))))
          ;; todo : bug qd <% } else { %>
          (when (web-mode-block-ends-with "{" reg-beg)
            (setq controls (append controls (list (cons 'open "{")))))
          )
         )
        ) ;jsp

       ((string= web-mode-engine "freemarker")
        (cond
         ((eq (char-after (1- reg-end)) ?\/)
          )
         ((looking-at "[<[]#\\(break\\|case\\|default\\)")
          (setq controls (append controls (list (cons 'inside "switch"))))
          )
         ((looking-at "[<[]#els")
          (setq controls (append controls (list (cons 'inside "if"))))
          )
         ((looking-at "</?\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ((looking-at "[<[]/?[#@]\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         (t
          (when (web-mode-block-starts-with "}" reg-beg)
            (setq controls (append controls (list (cons 'close "{")))))
          ;; todo : bug qd <% } else { %>
          (when (web-mode-block-ends-with "{" reg-beg)
            (setq controls (append controls (list (cons 'open "{")))))
          )
         )
        ) ;freemarker

       ((string= web-mode-engine)
        (when (web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
        (when (web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
        ) ;razor

       ) ;cond

      (when controls
        (put-text-property reg-beg (1+ reg-beg) 'block-controls controls)
        )

      ;;    (message "(%S) controls=%S" reg-beg controls)

      )))

(defun web-mode-regexp-factory (key keywords)
  "web-mode-regexp-factory"
  (let (regexp)
    (setq regexp (plist-get web-mode-cache key))
    (unless regexp
      )
    regexp))

(defun web-mode-block-control-previous-position (type &optional pos)
  "web-mode-block-control-previous-open"
  (unless pos (setq pos (point)))
  (let ((continue t) controls)
    (while continue
      (setq pos (web-mode-block-previous-position pos))
;;      (message "pos=%S" pos)
      (if (null pos)
          (setq continue nil
                pos nil)
        (when (and (setq controls (get-text-property pos 'block-controls))
                   (eq (car (car controls)) type))
          (setq continue nil)
          ) ;when
        )
      ) ;while
    pos))

(defun web-mode-highlight-block (reg-beg reg-end)
  "Highlight block."
  (let (sub2 sub3 continue char keywords token-type face beg end flags (buffer (current-buffer)))
    ;;(message "reg-beg=%S reg-end=%S" reg-beg reg-end)
    (remove-text-properties reg-beg reg-end '(font-lock-face nil))

    (goto-char reg-beg)

    (when (null web-mode-engine-font-lock-keywords)
      (setq sub2 (buffer-substring-no-properties
                  reg-beg (+ reg-beg 2))
            sub3 (buffer-substring-no-properties
                  reg-beg (+ reg-beg (if (>= (point-max) (+ reg-beg 3)) 3 2))))
      )

    (cond

     ((eq (get-text-property reg-beg 'block-token) 'comment)
      (put-text-property reg-beg reg-end 'font-lock-face 'web-mode-comment-face)
      ) ;comment block

     (web-mode-engine-font-lock-keywords
      (setq keywords web-mode-engine-font-lock-keywords)
      )

     ((string= web-mode-engine "django")
      (cond
       ((string= sub2 "{{")
        (setq keywords web-mode-django-expr-font-lock-keywords))
       ((string= sub2 "{%")
        (setq keywords web-mode-django-code-font-lock-keywords))
       )) ;django

     ((string= web-mode-engine "mako")
      (cond
       ((member sub3 '("<% " "<%\n" "<%!"))
        (setq keywords web-mode-mako-block-font-lock-keywords))
       ((string= sub2 "% ")
        (setq keywords web-mode-mako-block-font-lock-keywords))
       ((member sub2 '("<%" "</"))
        (setq keywords web-mode-mako-tag-font-lock-keywords))
       ((member sub2 '("${"))
        (setq keywords web-mode-uel-font-lock-keywords))
       )) ;mako

     ((string= web-mode-engine "jsp")
      (cond
       ((string= sub3 "<%@")
        (setq keywords web-mode-directive-font-lock-keywords))
       ((member sub2 '("${" "#{"))
        (setq keywords web-mode-uel-font-lock-keywords))
       ((string= sub2 "<%")
        (setq keywords web-mode-jsp-font-lock-keywords))
       (t
        (setq keywords web-mode-jsp-tag-font-lock-keywords))
       )) ;jsp

     ((string= web-mode-engine "aspx")
      (cond
       ((string= sub3 "<%@")
        (setq keywords web-mode-directive-font-lock-keywords))
       ((string= sub3 "<%$")
        (setq keywords web-mode-expression-font-lock-keywords))
       (t
        (setq keywords web-mode-aspx-font-lock-keywords))
       )) ;aspx

     ((string= web-mode-engine "freemarker")
      (cond
       ((member sub2 '("${" "#{"))
        (setq keywords web-mode-uel-font-lock-keywords))
       ((or (member sub2 '("<@" "[@" "<#" "[#"))
            (member sub3 '("</@" "[/@" "</#" "[/#")))
        (setq keywords (if (eq ?\[ (aref sub2 0))
                           web-mode-freemarker-square-font-lock-keywords
                         web-mode-freemarker-font-lock-keywords)))
       (t
        (setq keywords web-mode-jsp-tag-font-lock-keywords))
       )) ;freemarker

     ) ;cond

    (when keywords
      (web-mode-fontify-region reg-beg reg-end keywords)
;;      (message "reg-beg(%S) reg-end(%S)" reg-beg reg-end)
      (setq flags (get-text-property reg-beg 'block-beg))
      (setq continue (not (null flags))) ;;(> (logand flags 1) 0))
      (setq end reg-beg)
      (while continue
;;        (message "end=%S %S" end (get-text-property end 'block-token))

        ;; (if (and (= reg-beg end)
        ;;          (eq (get-text-property end 'block-token) 'delimiter))
        ;;     (setq beg end)
        ;;   (setq beg (next-single-property-change end 'block-token))
        ;;   )

        (if (get-text-property end 'block-token)
            (setq beg end)
          (setq beg (next-single-property-change end 'block-token buffer reg-end)))

        (setq end nil)
        (when beg (setq char (char-after beg)))
        (if (and beg (< beg reg-end))
            (progn
              (setq token-type (get-text-property beg 'block-token))
              (setq face (cond
                          ((eq token-type 'string)  'web-mode-block-string-face)
                          ((eq token-type 'comment) 'web-mode-block-comment-face)
                          (t                        'web-mode-block-delimiter-face)))
              (setq end (next-single-property-change beg 'block-token buffer reg-end))
;;              (message "end=%S" end)
              (if (and end (<= end reg-end))
                  (progn
;;                    (message "%S > %S face(%S)" beg end face)
                    (remove-text-properties beg end '(face nil))
                    (put-text-property beg end 'font-lock-face face))
                (setq continue nil
                      end nil)
                ) ;if end
              ) ;progn beg
          (setq continue nil
                end nil)
          ) ;if beg
        (when (and beg end)
          (save-match-data
            (when (and web-mode-enable-heredoc-fontification
                       (eq char ?\<)
                       (> (- end beg) 8)
                       ;;                     (progn (message "ici%S" (buffer-substring-no-properties beg end)) t)
                     ;;                   (member web-mode-engine '("php"))
                       (> (logand flags 2) 0)
                       (string-match-p "JS\\|JAVASCRIPT\\|HTM\\|CSS" (buffer-substring-no-properties beg end)))
              (setq keywords (if (eq ?H (char-after (+ beg 3)))
                                 web-mode-html-font-lock-keywords
                               web-mode-javascript-font-lock-keywords))
;;              (remove-text-properties beg end '(font-lock-face nil))
              (web-mode-fontify-region beg end keywords)
            ))
;;          (message "%S %c %S beg=%S end=%S" web-mode-enable-string-interpolation char web-mode-engine beg end)
          (when (and web-mode-enable-string-interpolation
                     (member char '(?\" ?\<))
                     (member web-mode-engine '("php" "erb"))
                     (> (- end beg) 4))
            (web-mode-interpolate-string beg end))
          (when (and web-mode-enable-comment-keywords
                     (eq token-type 'comment)
                     (> (- end beg) 3))
            (web-mode-interpolate-comment beg end t)
            ) ;when
          ) ;when beg end
        ) ;while continue
      ) ;when keywords

    (when (and (member web-mode-engine '("jsp" "mako"))
               (> (- reg-end reg-beg) 12)
               (eq ?\< (char-after reg-beg)))
      (web-mode-interpolate-block-tag reg-beg reg-end))

    (when web-mode-enable-block-face
;;      (message "block-face %S %S" reg-beg reg-end)
      (font-lock-append-text-property reg-beg reg-end 'face 'web-mode-block-face))

    ))

(defun web-mode-scan-mako-block-comments (reg-beg reg-end)
  "Scan extra"
  (save-excursion
    (let (beg end)
      (goto-char reg-beg)
      (while (and (< (point) reg-end)
                  (re-search-forward "<%doc>" reg-end t))
        (goto-char (match-beginning 0))
        (setq beg (point))
        (when (re-search-forward "</%doc>" reg-end t)
          (setq end (point))
          (remove-text-properties beg end web-mode-scan-properties)
          (add-text-properties beg end '(block-side t block-token comment))
          (put-text-property beg (1+ beg) 'block-beg t)
          (put-text-property (1- end) end 'block-end t)
          ) ;when
        ) ;while
      )))

(defun web-mode-scan-django-block-comments (reg-beg reg-end)
  "Scan extra"
  (save-excursion
    (let (beg end)
      (goto-char reg-beg)
      (while (and (< (point) reg-end)
                  (re-search-forward "{% comment %}" reg-end t))
        (goto-char (match-beginning 0))
        (setq beg (point))
        (goto-char (1+ (match-beginning 0)))
        (when (re-search-forward "{% endcomment %}" reg-end t)
;;          (setq end (1- (point)))
          (setq end (point))
          (remove-text-properties beg end web-mode-scan-properties)
          (add-text-properties beg end '(block-side t block-token comment))
          (put-text-property beg (1+ beg) 'block-beg t)
          (put-text-property (1- end) end 'block-end t)
;;          (remove-text-properties beg end web-mode-scan-properties)
          ;; (add-text-properties
          ;;  beg end
          ;;  '(block-token comment font-lock-face web-mode-comment-face))
          ) ;when
        )
      )))

(defun web-mode-interpolate-block-tag (beg end)
  "Scan a block tag (jsp / mako) to fontify ${ } blocks"
  (save-excursion
    (goto-char (+ 4 beg))
    (setq end (1- end))
    (while (re-search-forward "${.*}" end t)
      (remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face nil))
      (web-mode-fontify-region (match-beginning 0) (match-end 0)
                               web-mode-uel-font-lock-keywords)
      )
    ))

;; todo : parsing plus compliqué: {$obj->values[3]->name}
(defun web-mode-interpolate-string (beg end)
  "Interpolate php/erb strings."
  (save-excursion
    (goto-char (1+ beg))
    (setq end (1- end))
    (cond
     ((string= web-mode-engine "php")
      (while (re-search-forward "$[[:alnum:]_]+\\(->[[:alnum:]_]+\\)*\\|{[ ]*$.+}" end t)
        ;;web-mode-php-var-interpolation-font-lock-keywords
;;        (message "la%S" (point))
        (remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face nil))
        (web-mode-fontify-region (match-beginning 0) (match-end 0)
                                 web-mode-php-var-interpolation-font-lock-keywords)

        ;; (put-text-property (match-beginning 0) (match-end 0)
        ;;                    'font-lock-face nil)
        ;; (put-text-property (1+ (match-beginning 0)) (match-end 0)
        ;;                    'font-lock-face 'web-mode-variable-name-face)

        ))
     ((string= web-mode-engine "erb")
      (while (re-search-forward "#{.*}" end t)
        (remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face nil))
        (put-text-property (match-beginning 0) (match-end 0)
                           'font-lock-face 'web-mode-variable-name-face)
        ))
     ) ;cond
    ))

(defun web-mode-interpolate-comment (beg end block-side)
  "Interpolate comment"
  (save-excursion
    (let (regexp)
      (goto-char beg)
      (setq regexp (concat "\\<\\(" web-mode-comment-keywords "\\)\\>"))
      (while (re-search-forward regexp end t)
        (font-lock-prepend-text-property (match-beginning 1) (match-end 1)
                                         'font-lock-face
                                         'web-mode-comment-keyword-face)
        )
      )))

;;todo :
;; tag-type : start / end / void / comment / cdata / doctype / declaration
;; tag-name uniquement sur les html tag

;; piste d'optim : associer tagname à 'tag-end

;; flags
;; (1)attrs (2)custom (4)slash-beg (8)slash-end (16)brackend-end

;; http://dev.w3.org/html5/html-author/#tags
;; http://www.w3schools.com/jsref/prop_node_nodetype.asp
;; start-tag, end-tag, tag-name, element (<a>xsx</a>, an element is delimited by tags), void-element
;; http://www.w3.org/TR/html-markup/syntax.html#syntax-elements
(defun web-mode-scan-tags (reg-beg reg-end)
  "Scan html nodes (tags/attrs/comments/doctype)."
  (save-excursion
    (let (part-beg part-end flags limit close-expr props tname tbeg tend tstop element-content-type attrs-end close-found is-tag slash-beg slash-end regexp regexp1 regexp2)

      (setq regexp1 "<\\(/?[[:alpha:]][[:alnum:]-]*\\|!--\\|!\\[CDATA\\[\\|!doctype\\|\?xml\\)"
            regexp2 "<\\(/?[[:alpha:]][[:alnum:]-]*\\|!--\\|!\\[CDATA\\[\\)")

      (setq regexp regexp1)

      (goto-char reg-beg)

      (while (web-mode-rsf-client regexp reg-end t)

        (setq flags 0
              tname (downcase (match-string-no-properties 1))
              tbeg (match-beginning 0)
              tend nil
              tstop (point)
              element-content-type nil
              limit reg-end
              part-beg nil
              part-end nil
              props nil
              close-expr ">"
              close-found nil
              is-tag nil
              slash-beg nil
              slash-end nil)

        (cond
         ((string= tname "!--")
          (setq close-expr "-->"
                props '(tag-type comment)))
         ((string= tname "?xml")
          (setq regexp regexp2
                close-expr "?>"
                props '(tag-type declaration)))
         ((string= tname "![cdata[")
          (setq close-expr "]]>"
                props '(tag-type cdata)))
         ((string= tname "!doctype")
          (setq regexp regexp2
                props '(tag-type doctype)))
         (t
          (setq is-tag t)
          (when (string-match-p "-" tname)
            (setq flags (logior flags 2)))
          (cond
           ((eq ?\/ (aref tname 0))
            (setq props (list 'tag-name (substring tname 1) 'tag-type 'end)
                  slash-beg t
                  flags (logior flags 4))
            (setq limit (if (> reg-end (line-end-position)) (line-end-position) reg-end))
            )
           ((web-mode-element-is-void tname)
            (setq props (list 'tag-name tname 'tag-type 'void))
            )
           (t
            (setq props (list 'tag-name tname 'tag-type 'start))
            )
           ) ;cond
          ) ;t
         ) ;cond

        (if (web-mode-sf-client close-expr limit t)
            (progn
              (setq attrs-end (- (point) (length close-expr))
                    tend (point)
                    flags (logior flags 16)
                    close-found t)
              (when (eq ?\/ (char-after (- (point) 2)))
                (setq attrs-end (1- attrs-end)
                      props (plist-put props 'tag-type 'void)
                      slash-end t
                      flags (logior flags 8)))
              ) ;progn
          (setq attrs-end (line-end-position)
                tend (line-end-position))
          ) ;if

        (cond
         ((string= tname "script")
          (let (script)
            (setq script (buffer-substring-no-properties tbeg tend))
            (cond
             ((string-match-p " type[ ]*=[ ]*[\"']text/\\(x-handlebars\\|html\\|ng-template\\)" script)
              (setq element-content-type "html"))
             ((string-match-p " type[ ]*=[ ]*[\"']application/\\(ld\\+json\\|json\\)" script)
              (setq element-content-type "json"))
             (t
              (setq element-content-type "javascript"))
             )
            ;;          (message "tag=%S : %S" tname element-content-type)
            )
          ) ;case script
         ((string= tname "style")
          (setq element-content-type "css")
          )
         )

        ;;        (message "tag=%S (%S > %S)\n%S" tname tbeg tend props)
        (add-text-properties tbeg tend props)

;;        (when is-tag

        (when (and is-tag
                   (not slash-beg)
                   (> (- attrs-end tstop) 2)
                   (> (web-mode-scan-attrs tstop attrs-end) 0))
          (setq flags (logior flags 1)))

        ;;            (progn
        (put-text-property tbeg (1+ tbeg) 'tag-beg flags)
        ;;              (when close-found
        (put-text-property (1- tend) tend 'tag-end t)
          ;;                )
          ;;              ) ;progn
          ;;          (add-text-properties tbeg (1+ tbeg) '(tag-beg t))
          ;;          (add-text-properties (1- tend) tend '(tag-end t))
;;          )

        (when (and is-tag close-found)

          (cond
           ((and (string= tname "script")
                 (member element-content-type '("javascript" "json")))
            (setq close-expr "</script>"))
           ((string= tname "style")
            (setq close-expr "</style>"))
           (t
            (setq close-expr nil))
           )

          ;; si <script type="document/html"> on ne fait pas la suite

          (when (and close-expr (web-mode-sf-client close-expr reg-end t))
            (setq part-beg (if (eq (char-after tend) ?\n) (1+ tend) tend)
                  part-end (match-beginning 0))
;;            (message "part-beg(%S) part-end(%S)" part-beg part-end)
            (if (>= (- part-end part-beg) 3)
                (progn
                  (put-text-property part-beg part-end
                                     'part-side
                                     (cond
                                      ((string= element-content-type "javascript") 'javascript)
                                      ((string= element-content-type "json") 'json)
                                      ((string= element-content-type "css") 'css)
                                      ))
;;                  (web-mode-scan-part part-beg part-end)
                  ) ;progn
;;              (remove-text-properties part-beg part-end web-mode-scan-properties2)
              ) ;if
            (goto-char part-end)
            ) ;when

          ) ;when

        ) ;while

      )))

(defun web-mode-highlight-tags (reg-beg reg-end)
  "web-mode-highlight-nodes"
  (let ((continue t))
    (goto-char reg-beg)
    (when (and (not (get-text-property (point) 'tag-beg))
               (not (web-mode-tag-next)))
      (setq continue nil))
    (when (and continue (>= (point) reg-end))
      (setq continue nil))
    (while continue
      (web-mode-highlight-tag)
      (when (or (not (web-mode-tag-next))
                (>= (point) reg-end))
        (setq continue nil))
      ) ;while
    ))

;; flags
;; (1)attrs (2)custom (4)slash-beg (8)slash-end (16)brackend-end
(defun web-mode-highlight-tag ()
  "web-mode-highlight-nodes"
  (let ((beg (point)) end name type face flags slash-beg slash-end bracket-end)

    (setq end (1+ (web-mode-tag-end-position beg))
          flags (get-text-property beg 'tag-beg)
          type (get-text-property beg 'tag-type)
          name (get-text-property beg 'tag-name))

    (cond

     ((eq type 'comment)
      (put-text-property beg end 'font-lock-face 'web-mode-comment-face)
;;      (message "web-mode-enable-comment-keywords=%S beg(%S) end(%S)" web-mode-enable-comment-keywords beg end)
      (when (and web-mode-enable-comment-keywords (> (- end beg) 5))
        (web-mode-interpolate-comment beg end nil))
      )

     ((eq type 'cdata)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))

     ((eq type 'doctype)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))

     ((eq type 'declaration)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))

     (name

      ;; todo : se passer des vars intermédiaires
      (setq face (if (> (logand flags 2) 0) 'web-mode-html-tag-custom-face 'web-mode-html-tag-face)
            slash-beg (> (logand flags 4) 0)
            slash-end (> (logand flags 8) 0)
            bracket-end (> (logand flags 16) 0))

      (put-text-property beg (+ beg (if slash-beg 2 1)) 'font-lock-face 'web-mode-html-tag-bracket-face)
      (put-text-property (+ beg (if slash-beg 2 1))
                         (+ beg (if slash-beg 2 1) (length name))
                         'font-lock-face face)
      (when (or slash-end bracket-end)
        (put-text-property (- end (if slash-end 2 1)) end 'font-lock-face 'web-mode-html-tag-bracket-face)
        ) ;when

      (when (> (logand flags 1) 0)
        (web-mode-highlight-attrs beg end)
        )

      ) ;name

     ) ;cond

    ))

;; todo : optimisation des zones reg-beg et reg-end
(defun web-mode-highlight-attrs (reg-beg reg-end)
  "Highlight attributes."
  (let ((continue t) (pos reg-beg) beg end flags offset face)
    (while continue
      (setq beg (next-single-property-change pos 'tag-attr))
      (if (and beg (< beg reg-end))
          (progn
            (setq flags (get-text-property beg 'tag-attr))
;;            (message "beg=%S flags=%S" beg flags)
            (setq face (if (> (logand flags 1) 0) 'web-mode-html-attr-custom-face 'web-mode-html-attr-name-face))
            (if (get-text-property beg 'tag-attr-end)
                (setq end beg)
              (setq end (next-single-property-change beg 'tag-attr-end)))
;;            (message "beg=%S end=%S" beg end)
            (if (and end (< end reg-end))
                (progn
                  (setq offset (get-text-property end 'tag-attr-end))
;;                  (message "offset=%S" offset)
                  (if (= offset 0)
                      (put-text-property beg (1+ end) 'font-lock-face face)
                    (put-text-property beg (+ beg offset) 'font-lock-face face)
                    (put-text-property (+ beg offset) (+ beg offset 1) 'font-lock-face 'web-mode-html-attr-equal-face)
                    (put-text-property (+ beg offset 1) (1+ end) 'font-lock-face 'web-mode-html-attr-value-face)
                    ) ;if offset
                  (setq pos (1+ end))
                  ) ;progn
              (setq continue nil)
              ) ;if end
            ) ;progn beg
        (setq continue nil)
        ) ;if beg
      ) ;while
    ))

;; http://www.w3.org/TR/html-markup/syntax.html#syntax-attributes
;; states:
;; nil(0) space(1) name(2) space-before(3) equal(4) space-after(5) value-uq(6) value-sq(7) value-dq(8)
(defun web-mode-scan-attrs (beg end)
  "Scan html attributes."
  (save-excursion
    ;;(message "beg(%S) end(%S)" beg end)
    (let (name-beg name-end val-beg (count 0) (state 0) (flags 0) (equal-offset 0) char pos escaped spaced)
      (goto-char (1- beg))

      (while (< (point) end)
        (forward-char)
        (setq pos (point)
              char (char-after))
        (setq spaced (eq char ?\s))

        (cond

         ((= pos end)
          (when name-beg
            (unless name-end (setq name-end (1- pos)))
;;            (message "name-end=%S" name-end)
            (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
            )
          (setq state 0
                flags 0
                equal-offset 0
                name-beg nil
                name-end nil
                val-beg nil)
          )

         ((get-text-property pos 'block-side)
          )

         ((and spaced (= state 0))
          (setq state 1)
          )

         ((and spaced (member state '(1 3 5)))
          )

         ((and spaced (= state 2))
          (setq name-end (1- pos)
                state 3)
          )

         ((and spaced (= state 4))
          (setq state 5)
          )

         ((and (= state 3)
;;               (progn (message "pt=%S state=%S char=%c" (point) state char) t)
               (or (and (>= char 65) (<= char 90)) ;A - Z
                   (and (>= char 97) (<= char 122)) ;a - z
                   ;; (= char 34) (= char 39) ; " '
                   ;; (and (>= char 48) (<= char 57)) ;0 - 9
                   ))
;;          (message "pos=%S name-beg=%S name-end=%S char=%c" pos name-beg name-end char)
;;          (message "%S %S - %S %S" ?a ?z ?A ?Z)
;;          (message "pos=%S %S" pos name-end)
          (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
          (setq state 2
                flags 0
                equal-offset 0
                name-beg pos
                name-end nil
                val-beg nil)
          )

         ((and (eq char ?\n) (not (member state '(7 8))))
          (if (= state 2) (setq name-end (1- pos)))
          (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
          (setq state 1
                flags 0
                equal-offset 0
                name-beg nil
                name-end nil
                val-beg nil)
          )

         ((or (and (eq ?\" char) (= state 8) (not escaped))
              (and (eq ?\' char) (= state 7) (not escaped))
              (and (member char '(?\s ?\n ?\>)) (= state 6)))
          (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
          (setq state (if (= state 6) 1 0)
                flags 0
                equal-offset 0
                name-beg nil
                name-end nil
                val-beg nil)
          )

         ((and (not spaced) (= state 1))
          (setq state 2)
          (setq name-beg pos)
          )

         ((and (eq ?\= char) (member state '(2 3)))
          (setq name-end pos)
;;          (message "name-beg=%S %S" name-beg pos)
          (setq equal-offset (- pos name-beg))
          (setq state 4)
          )

         ((and (eq ?\" char) (member state '(4 5)))
          (setq val-beg pos)
          (setq state 8)
;;          (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
          )

         ((and (eq ?\' char) (member state '(4 5)))
          (setq val-beg pos)
          (setq state 7)
;;          (setq count (+ count (web-mode-scan-attr state char name-beg name-end val-beg flags equal-offset)))
          )

         ((member state '(4 5))
          (setq val-beg pos)
          (setq state 6)
          )

         ((= state 1)
          (setq state 2)
          )

         ((and (= state 2) (eq ?\- char))
          (setq flags (logior flags 1))
          )

         ) ;cond

        ;;        (message "point(%S) end(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) flags(%S) equal-offset(%S)" pos end state char name-beg name-end val-beg flags equal-offset)

        (setq escaped (eq ?\\ char))

        ) ;while

      count)))

;; flags:
;; (1)custom

;; states:
;; (0)nil (1)space (2)name (3)space-before (4)equal (5)space-after (6)value-uq (7)value-sq (8)value-dq
(defun web-mode-scan-attr (state char name-beg name-end val-beg flags equal-offset)
  "propertize attr."
;;  (message "point(%S) state(%S) c(%c) name-beg(%S) name-end(%S) val-beg(%S) flags(%S) equal-offset(%S)"
;;           (point) state char name-beg name-end val-beg flags equal-offset)
;;  (message "flags=%S" flags)
  (cond
   ((and (= state 8) (not (eq ?\" char)))
    0)
   ((and (= state 7) (not (eq ?\' char)))
    0)
   ((= state 4)
    0)
   ((null name-beg)
    0)
   (t
    (let (val-end)
      (if (null val-beg)
          (setq val-end name-end)
        (setq val-end (point))
        (when (or (null char) (member char '(?\s ?\n ?\>)))
          (setq val-end (1- val-end)))
        ) ;if
        ;;      (put-text-property name-beg (1+ name-beg) 'tag-attr flags)
      (put-text-property name-beg (1+ val-end) 'tag-attr flags)
      (put-text-property val-end (1+ val-end) 'tag-attr-end equal-offset)
      ) ;let
    1) ;t
   ) ;cond
  )

(defun web-mode-scan-part (reg-beg reg-end)
  "Scan client part (e.g. javascript, json, css)."
  (save-excursion
    (let (token-re ch-before ch-at ch-next token-type start continue content-type)
;;      (message "reg-beg(%S) reg-end(%S) content-type(%S)" reg-beg reg-end content-type)

      (if (member web-mode-content-type '("javascript" "json" "css"))
          (setq content-type web-mode-content-type)
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side)))
;;        (message "content-type=%S" content-type)
        )

;;      (remove-text-properties reg-beg reg-end web-mode-scan-properties2)

      (goto-char reg-beg)

      (when (and (not web-mode-has-any-large-part)
                 (> (- reg-end reg-beg) web-mode-large-embed-threshold))
;;        (message "** large part detected [ %S - %S ] **" reg-beg reg-end)
        (setq web-mode-has-any-large-part t))

      (cond
       ((string= content-type "javascript")
        (setq token-re "/.\\|\"\\|'"))
       ((string= content-type "json")
        (setq token-re "//\\|/\\*\\|\"\\|'"))
       ((string= content-type "css")
        (setq token-re "/\\*\\|\"\\|'"))
       (t
        (setq token-re "/\\*\\|\"\\|'"))
       )

      (while (and token-re (web-mode-rsf-client token-re reg-end t))
        (setq start (match-beginning 0)
              token-type nil
              continue t)
        (setq ch-at (char-after start))
        (setq ch-next (or (char-after (1+ start)) ?\d))
        (setq ch-before (or (char-before start) ?\d))
        ;;        (message "beg=%S :%c%c%c" start ch-before ch-at ch-next)
        (cond

         ((eq ?\' ch-at)
          (unless (eq ?\\ ch-before)
            (while (and continue (search-forward "'" reg-end t))
              (setq continue (or (get-text-property (1- (point)) 'block-side)
                                 (eq ?\\ (char-before (1- (point))))))
              )
            (cond
             ((string= content-type "javascript")
              (setq token-type 'string))
             ((string= content-type "css")
              (setq token-type 'string))
             ((string= content-type "json")
              (setq token-type 'string))
             (t
              (setq token-type 'string))
             ) ;cond
            ) ;unless
          )

         ((eq ?\" ch-at)
          (unless (eq ?\\ ch-before)
            (while (and continue (search-forward "\"" reg-end t))
              (setq continue (or (get-text-property (1- (point)) 'block-side)
                                 (eq ?\\ (char-before (1- (point))))))
              ) ;while
            (cond
             ((string= content-type "json")
              (if (looking-at-p "[ ]*:")
                  (cond
                   ((eq ?\@ (char-after (1+ start)))
                    (setq token-type 'context))
                   (t
                    (setq token-type 'key))
                   )
                (setq token-type 'string))
              )
             (t
              (cond
               ((string= content-type "javascript")
                (setq token-type 'string))
               ((string= content-type "css")
                (setq token-type 'string))
               (t
                (setq token-type 'string))
               ) ;cond
              ) ;t
             ) ;cond
            ) ;unless
          )

         ((eq ?\/ ch-next)
          (unless (eq ?\\ ch-before)
            (setq token-type 'comment)
            (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
            )
          )

         ((eq ?\* ch-next)
          (unless (eq ?\\ ch-before)
            (setq token-type 'comment)
            (search-forward "*/" reg-end t)
            )
          )

         ((and (string= content-type "javascript")
               (eq ?\/ ch-at)
               (progn (backward-char) t)
               (looking-back "[(=][ ]*/")
               (looking-at-p ".+/")
;;               (not (eq ?\s ch-next))
               )
;;          (message "regexp literal at (%S)" (1- (point)))
          (while (and continue (search-forward "/" reg-end t))
            (setq continue (or (get-text-property (1- (point)) 'block-side)
                               (eq ?\\ (char-before (1- (point))))))
            )
          (setq token-type 'string)
          (skip-chars-forward "gimy")
          )

         ) ;cond

        (when (and (>= reg-end (point)) token-type)
          (put-text-property start (point) 'part-token token-type)
          )

        ) ;while

      )))

(defun web-mode-highlight-part (reg-beg reg-end)
  "Highlight part (e.g. javascript, json, css)."
  (save-excursion
    (let (char start continue token-type face beg end string-face comment-face content-type)

      (if (member web-mode-content-type '("javascript" "json" "css"))
          (setq content-type web-mode-content-type)
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side)))
        )

      (goto-char reg-beg)

      (cond
       ((string= content-type "javascript")
        (setq string-face 'web-mode-javascript-string-face
              comment-face 'web-mode-javascript-comment-face)
        (web-mode-fontify-region reg-beg reg-end web-mode-javascript-font-lock-keywords))
       ((string= content-type "json")
        (setq string-face 'web-mode-json-string-face
              comment-face 'web-mode-json-comment-face)
        (web-mode-fontify-region reg-beg reg-end web-mode-javascript-font-lock-keywords))
       ((string= content-type "css")
        (setq string-face 'web-mode-css-string-face
              comment-face 'web-mode-css-comment-face)
        (web-mode-highlight-css-rules reg-beg reg-end)
        )
       (t
        (setq string-face 'web-mode-part-string-face
              comment-face 'web-mode-part-comment-face)
        )
       )

      (setq continue t
            end reg-beg)
      (while continue
        (setq char (char-after beg))
        (setq beg (next-single-property-change end 'part-token)
              end nil)
        (if (and beg (< beg reg-end))
            (progn
              (setq token-type (get-text-property beg 'part-token))
              (setq face (cond
                          ((eq token-type 'context) 'web-mode-json-context-face)
                          ((eq token-type 'key) 'web-mode-json-key-face)
                          ((eq token-type 'string) string-face)
                          (t comment-face)))
              (setq end (next-single-property-change beg 'part-token))
              (if (and end (< end reg-end))
                  (progn
                    (remove-text-properties beg end '(face nil))
                    (put-text-property beg end 'font-lock-face face)
                    )
                (setq continue nil
                      end nil)
                ) ;if end
              ) ;progn beg
          (setq continue nil
                end nil)
          ) ;if beg

        (when (and beg end
                   web-mode-enable-comment-keywords
                   (eq token-type 'comment)
                   (> (- end beg) 3))
          (web-mode-interpolate-comment beg end t))

        ) ;while

      (when web-mode-enable-part-face
        (font-lock-append-text-property reg-beg reg-end 'face 'web-mode-part-face)
        ;;        (font-lock-prepend-text-property reg-beg reg-end 'font-lock-face 'web-mode-block-face)
        ;;        (font-lock-append-text-property reg-beg reg-end 'web-mode-part-face face)
        )

      )))

(defun web-mode-highlight-css-rules (part-beg part-end)
  "Scan CSS rules."
  (save-excursion
    (goto-char part-beg)
    (let (rule (continue t) (i 0) at-rule)
      (while continue
        (setq i (1+ i))
        (setq rule (web-mode-css-next-rule part-end))
        (cond
         ((> i 1000)
          (message "*** too much css rules ***")
          (setq continue nil))
         ((null rule)
          (setq continue nil)
          )
         ((and (setq at-rule (plist-get rule :at-rule))
               (not (member at-rule '("charset" "font-face" "import")))
               (plist-get rule :dec-end))
          (web-mode-highlight-css-rule (plist-get rule :sel-beg)
                                       (plist-get rule :sel-end)
                                       nil nil)
          (web-mode-highlight-css-rules (plist-get rule :dec-beg)
                                        (plist-get rule :dec-end))
          )
         (t
          (web-mode-highlight-css-rule (plist-get rule :sel-beg)
                                       (plist-get rule :sel-end)
                                       (plist-get rule :dec-beg)
                                       (plist-get rule :dec-end))
          )
         ) ;cond
        )
      )
    ))

(defun web-mode-highlight-css-rule (sel-beg sel-end dec-beg dec-end)
  "Fontify css rule."
  (save-excursion
;;    (message "sel-beg=%S sel-end=%S dec-beg=%S dec-end=%S" sel-beg sel-end dec-beg dec-end)
    (web-mode-fontify-region sel-beg sel-end
                             web-mode-selector-font-lock-keywords)
    (when (and dec-beg dec-end)
      (web-mode-fontify-region dec-beg dec-end
                               web-mode-declaration-font-lock-keywords)
      (goto-char dec-beg)
      (while (and (not web-mode-disable-css-colorization)
                  (re-search-forward "#[0-9a-fA-F]\\{6\\}\\|#[0-9a-fA-F]\\{3\\}\\|rgb([ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)\\(.*?\\))" dec-end t)
                  (< (point) dec-end))
        (web-mode-colorize (match-beginning 0) (match-end 0))
        )
      )))

;; css rule = selector(s) + declaration (properties)
(defun web-mode-css-next-rule (limit)
  "next rule"
  (let (at-rule sel-beg sel-end dec-beg dec-end chunk)
    (skip-chars-forward "\n\t ")
    (setq sel-beg (point))
    (when (and (< (point) limit)
               (web-mode-rsf-client "[{;]" limit t))
      (setq sel-end (1- (point)))
      (cond
       ((eq (char-before) ?\{)
        (setq dec-beg (point))
        (setq dec-end (web-mode-closing-paren-position (1- dec-beg) limit))
        (if dec-end
            (progn
              (goto-char dec-end)
              (forward-char))
          (setq dec-end limit)
          (goto-char limit))
        )
       (t
        )
       ) ;cond
      (setq chunk (buffer-substring-no-properties sel-beg sel-end))
      (when (string-match "@\\([[:alpha:]-]+\\)" chunk)
        (setq at-rule (match-string-no-properties 1 chunk))
;;        (message "%S at-rule=%S" chunk at-rule)
        )
      ) ;when
    (if (not sel-end)
        (progn (goto-char limit) nil)
      (list :at-rule at-rule
            :sel-beg sel-beg
            :sel-end sel-end
            :dec-beg dec-beg
            :dec-end dec-end)
      ) ;if
    ))

(defun web-mode-css-current-rule (pos min max)
  "current css rule"
  (save-excursion
    (let (beg end)
      (goto-char pos)
      (if (not (web-mode-sb-client "{" min t))
          (progn
            (setq beg min)
            (if (web-mode-sf-client ";" max t)
                (setq end (1+ (point)))
              (setq end max))
            )
        (setq beg (point))
        (setq end (web-mode-closing-paren-position beg max))
        (if end
            (setq end (1+ end))
          (setq end max)
          )
;;        (message "%S >>beg%S >>end%S" pos beg end)
        (if (> pos end)

            ;;selectors
            (progn
              (goto-char pos)
              (if (web-mode-rsb-client "[};]" min t)
                  (setq beg (1+ (point)))
                (setq beg min)
                )
              (goto-char pos)
              (if (web-mode-rsf-client "[{;]" max t)
                  (cond
                   ((eq (char-before) ?\;)
                    (setq end (point))
                    )
                   (t
                    (setq end (web-mode-closing-paren-position (1- (point)) max))
                    (if end
                        (setq end (1+ end))
                      (setq end max))
                    )
                   ) ;cond
                (setq end max)
                )
              ) ;progn selectors

          ;; declaration
          (goto-char beg)
          (if (web-mode-rsb-client "[}{;]" min t)
              (setq beg (1+ (point)))
            (setq beg min)
            )
          )
        )
;;      (message "beg(%S) end(%S)" beg end)
      (cons beg end)
      )))

(defun web-mode-velocity-skip-forward (pos)
  "find the end of a velocity block."
  (goto-char pos)
  (let (continue)
    (when (eq ?\# (char-after))
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
        ) ;while
      ) ;if
    ))

(defun web-mode-razor-tag-exclude (block-beg block-end)
  "Exclude HTML"
  (save-excursion
    (goto-char block-beg)
;;    (message "block-beg(%S) block-end(%S)" block-beg block-end)
    (let ((continue t) beg end tag-beg tag-end line line-end line-end-pos pos)

      (setq line (line-number-at-pos block-beg)
            line-end (line-number-at-pos block-end))

      (while (<= line line-end)
        (setq pos (point))
;;        (message "pos=%S line=%S" pos line)
        (setq line-end-pos (line-end-position))
        (if (>= line-end-pos block-end)
            (setq line-end-pos block-end)
;;          (setq line-end-pos (1+ line-end-pos))
          )
        (when (looking-at "[ \n]+")
;;          (message "ici %S %S" (match-beginning 0) (match-end 0))
          (remove-text-properties (match-beginning 0) (match-end 0) '(block-side))
          )

        (while (re-search-forward "\\([ \t]*\\(?:</?[[:alpha:]].*?>\\|<!--.*?-->\\)[ ]*\\)" line-end-pos t)
          (setq beg (match-beginning 0)
                end (match-end 0)
                tag-beg (match-beginning 1)
                tag-end (match-end 1))
;;          (message "line-end-pos(%S) beg(%S) end(%S)" line-end-pos beg end)
          (remove-text-properties beg end '(block-side))
          (put-text-property (1- tag-beg) tag-beg 'block-end t)
          (put-text-property tag-end (1+ tag-end) 'block-beg t)
          (when (and (looking-at "\\(.+\\)<")
                     (not (string-match-p "@" (match-string-no-properties 1))))
            (remove-text-properties (match-beginning 1) (match-end 1) '(block-side))
            )
          (when (string-match-p "@" (buffer-substring-no-properties beg end))
            (web-mode-scan-blocks beg end)
            )
          ) ;while
        (goto-char pos)
        (end-of-line)
        (when (looking-back "[ ]*")
          (remove-text-properties (match-beginning 0) (match-end 0) '(block-side))
          )
;;        (message "removing %S %S" (point) (1+ (point)))
        (when (get-text-property (point) 'block-side)
          (put-text-property (1- (point)) (point) 'block-end t)
          (remove-text-properties (point) (+ (point) 1) '(block-side))
          )
;;        (goto-char pos)
        (forward-line)
;;        (message "pos=%S" (point))
        (setq line (1+ line))
        )

      )))

(defun web-mode-razor-tag-exclude2 (block-beg block-end)
  "Exclude HTML"
  (save-excursion
    (goto-char block-beg)
;;    (message "block-beg(%S) block-end(%S)" block-beg block-end)
    (let ((continue t) beg end tag-beg tag-end)
      (while (re-search-forward "[ \t]*\\(</?[[:alpha:]].*?>\\|<!--.*?-->\\)[ \n]*" block-end t)
        (setq beg (match-beginning 0)
              end (match-end 0)
              tag-beg (match-beginning 1)
              tag-end (match-end 1))
;;        (message "beg(%S) end(%S)" beg end)
        (remove-text-properties beg end '(block-side))
        (put-text-property (1- tag-beg) tag-beg 'block-end t)
        (put-text-property tag-end (1+ tag-end) 'block-beg t)
        (when (and (looking-at "\\(.+\\)<")
                   (not (string-match-p "@" (match-string-no-properties 1))))
          (remove-text-properties (match-beginning 1) (match-end 1) '(block-side))
          )
        (when (string-match-p "@" (buffer-substring-no-properties beg end))
          (web-mode-scan-blocks beg end))
        ) ;while
      )))

(defun web-mode-razor-skip-forward (pos)
  "Find the end of a razor block."
  (goto-char pos)
  ;;            (message "pt=%S %c" (point) (char-after))
  (let ((continue t) tmp (i 0))
    (while continue
      (setq i (1+ i))
;;      (message "i=%S (%S)" i (point))
      (skip-chars-forward " =@a-zA-Z0-9_-")
      (cond
       ((> i 500)
        (message "*** invalid razor loop at (%S) ***" pos)
        (setq continue nil))
       ((looking-at-p "@[({]")
        (forward-char)
        (setq tmp (web-mode-closing-paren-position (point)))
        (when tmp
          (goto-char tmp))
        (forward-char)
        )
       ((and (not (eobp)) (eq ?\( (char-after)))
        (setq tmp (web-mode-closing-paren-position))
        (when tmp
          (goto-char tmp))
        (forward-char)
        )
       ((and (not (eobp)) (eq ?\. (char-after)))
        (forward-char))
       ((looking-at-p "[ \n]*{")
        (search-forward "{")
        (backward-char)
        (setq tmp (web-mode-closing-paren-position))
        (when tmp
          (goto-char tmp))
        (forward-char)
        )
       (t
        (setq continue nil))
       ) ;cond
      ) ;while
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
     ) ;cond
    ))

(defun web-mode-fontify-region (beg end keywords)
  "Font-lock region according to the keywords."
  (save-excursion
;;    (message "beg=%S end=%S" beg end)
    (let ((font-lock-keywords keywords)
          (font-lock-multiline nil)
          (font-lock-keywords-case-fold-search
           (member web-mode-engine '("asp" "template-toolkit")))
          (font-lock-keywords-only t)
          (font-lock-extend-region-functions nil))
;;      (remove-text-properties beg end '(font-lock-face nil))
;;      (message "%S" web-mode-mako-tag-font-lock-keywords)
      (when (listp font-lock-keywords)
        (font-lock-fontify-region beg end))
      ))
  ;; UGLY HACK / workaround (help needed)
  ;; (unless web-mode-buffer-highlighted
  ;;   (setq web-mode-buffer-highlighted t)
  ;;   (web-mode-fontify-region beg end keywords))
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
        ) ;comment - case

       ((web-mode-is-content)
;;        (message "prop(%S)" prop)
        (setq pair (web-mode-content-boundaries pos))
        (setq beg (car pair)
              end (cdr pair))
        )

       ) ;cond
;;      (message "beg(%S) end(%S)" beg end)
      (when (and beg end)
        (fill-region beg end)
;;        (indent-for-tab-command)
        )
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

;; verifier avec text-property-any si 'block-side
(defun web-mode-content-apply (&optional fun)
  "web-mode-content-apply"
  (interactive)
  (save-excursion
    (let (beg (i 0) (continue t))
      (goto-char (point-min))
      (when (get-text-property (point) 'tag-type)
        (web-mode-tag-end)
        (setq beg (point)))
      (while (and continue
                  (or (get-text-property (point) 'tag-beg)
                      (web-mode-tag-next)))
        (setq i (1+ i))
        (when (> i 2000)
          (setq continue nil))
        (when (and beg (> (point) beg))
          (message "content=%S > %S" beg (point)))
        (if (web-mode-tag-end)
            (setq beg (point))
          (setq continue nil))
        ) ;while
      )))

(defun web-mode-content-boundaries (&optional pos)
  "Text boundaries"
  (unless pos (setq pos (point)))
  (let (beg end)
    (setq beg (or (previous-property-change pos (current-buffer))
                  (point-max)))
    (setq end (or (next-property-change pos (current-buffer))
                  (point-min)))
    (while (and (< beg end) (member (char-after beg) '(?\s ?\n)))
      (setq beg (1+ beg)))
    (while (and (> end beg) (member (char-after (1- end)) '(?\s ?\n)))
      (setq end (1- end)))
    (message "beg(%S) end(%S)" beg end)
    (cons beg end)
    ))

(defun web-mode-coord-pos (line column)
  "Return pos at Line / Column pos"
  (save-excursion
    (when (stringp line) (setq line (string-to-number line)))
    (when (stringp column) (setq column (string-to-number column)))
;;    (message "%d %d" line column)
;;    (beginning-of-buffer)
;;    (forward-line line)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column (1- column))
    (point))
  )

(defun web-mode-jshint ()
  "Run JSHint on all the JavaScript parts."
  (interactive)
  (let (proc lines)
    (when (buffer-file-name)
      (setq proc (start-process
                  "jshint-proc"
                  nil
                  "jshint" "--extract=auto" (buffer-file-name)))
      (setq web-mode-jshint-errors 0)
      (set-process-filter proc
                          (lambda (proc output)
                            (let ((offset 0) overlay pos (old 0) msg)
                              (remove-overlays (point-min) (point-max) 'font-lock-face 'web-mode-error-face)
                              (while (string-match
                                      "line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)\\.$"
                                      output offset)
                                (setq web-mode-jshint-errors (1+ web-mode-jshint-errors))
                                (setq offset (match-end 0))
                                (setq pos (web-mode-coord-pos (match-string-no-properties 1 output)
                                                              (match-string-no-properties 2 output)))
                                (when (get-text-property pos 'tag-beg)
                                  (setq pos (1- pos)))
                                (when (not (= pos old))
                                  (setq old pos)
                                  (setq overlay (make-overlay pos (1+ pos)))
                                  (overlay-put overlay 'font-lock-face 'web-mode-error-face)
                                  )
                                (setq msg (or (overlay-get overlay 'help-echo)
                                               (concat "l="
                                                       (match-string-no-properties 1 output)
                                                       " c="
                                                       (match-string-no-properties 2 output)
                                                       )))
                                (overlay-put overlay 'help-echo
                                             (concat msg " ## " (match-string-no-properties 3 output)))
                                ) ;while
                              ))
                          )
      ) ;when
    ))

(defun web-mode-dom-errors-show ()
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
            (overlay-put overlay 'font-lock-face 'web-mode-warning-face)
;;            (message "invalid <%S> at %S" (nth 0 cell) (nth 1 cell))
            )
           ) ;cond
          ) ;while

        (dotimes (i i)
          (setq tags (cdr tags))
;;          (setq cell (nth i tags))
;;          (message "removing=%S" cell)
          )

        )
       ) ;cond
      (when (not (web-mode-tag-next))
        (setq continue nil))
      ) ;while
    (message "%S error(s) detected" errors)
    (if (> errors 0)
        (progn (goto-char first)
               (recenter))
      (goto-char ori)
      ) ;if
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
  (when web-mode-hl-line-mode-flag
    (global-hl-line-mode -1))
  (when web-mode-display-table
    (setq buffer-display-table web-mode-display-table))
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
;;  (put-text-property (point-min) (point-max) 'invisible nil)
;;  (remove-overlays)
  (web-mode-scan-buffer)
  (web-mode-buffer-indent))

(defun web-mode-buffer-change-tag-case (&optional type)
  "Change HTML tag case."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((continue t) f)
      (setq f (if (member type '("upper" "uppercase" "upper-case")) 'uppercase 'downcase))
      (when (and (not (get-text-property (point) 'tag-beg))
                 (not (web-mode-tag-next)))
        (setq continue nil))
      (while continue
        (skip-chars-forward "<!/")
        (if (looking-at "\\([[:alnum:]-]+\\)")
            (replace-match (funcall f (match-string 0)) t))
;;        (message "tag: %S (%S)"
;;                 (get-text-property (point) 'tag-name)
;;                 (point))
        (unless (web-mode-tag-next)
          (setq continue nil))
        ) ;while
      )))

(defun web-mode-buffer-change-attr-case (&optional type)
  "alter tag case"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((continue t) f)
      (setq f (if (member type '("upper" "uppercase" "upper-case")) 'uppercase 'downcase))
      (while continue
        (if (web-mode-attr-next)
            (when (looking-at "\\([[:alnum:]-]+\\)")
              (replace-match (funcall f (match-string 0)) t)
;;              (message "tag: %S (%S)" (match-string 0) (point))
              ) ;when
          (setq continue nil))
        ) ;while
      )))

;; todo : passer de règle en règle et mettre un \n à la fin
(defun web-mode-css-indent ()
  "Indent CSS parts"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((continue t) rule part-end)
      (while continue
        (if (web-mode-part-next)
            (when (eq (get-text-property (point) 'part-side) 'css)
              (setq part-end (web-mode-part-end-position))
              (while (setq rule (web-mode-css-next-rule part-end))
                (when (not (looking-at-p "[[:space:]]*\\($\\|<\\)"))
                  (newline)
                  (indent-for-tab-command)
                  (setq part-end (web-mode-part-end-position)))
                )
              )
          (setq continue nil)
          )
        )
      )))

;; tag-case=lower|upper-case , attr-case=lower|upper-case
;; special-chars=unicode|html-entities
;; smart-apostrophes=bool , smart-quotes=bool , indentation=bool
(defun web-mode-buffer-normalize ()
  "Normalize buffer"
  (interactive)
  (save-excursion
    (let ((rules web-mode-normalization-rules) elt)
      (when (setq elt (cdr (assoc "tag-case" rules)))
        (web-mode-buffer-change-tag-case elt))
      (when (setq elt (cdr (assoc "attr-case" rules)))
        (web-mode-buffer-change-attr-case elt))
      (when (setq elt (cdr (assoc "css-indentation" rules)))
        (web-mode-css-indent))
      (when (setq elt (cdr (assoc "smart-apostrophes" rules)))
        (web-mode-dom-apostrophes-replace))
      (when (setq elt (cdr (assoc "smart-quotes" rules)))
        (web-mode-dom-quotes-replace))
      (when (setq elt (cdr (assoc "special-chars" rules)))
        (if (string= elt "entities")
            (web-mode-dom-entities-encode)
          (web-mode-dom-entities-replace)))
      (when (setq elt (cdr (assoc "whitespaces" rules)))
        (goto-char (point-min))
        (while (not (eobp))
          (forward-line)
          (delete-blank-lines))
        (delete-trailing-whitespace)
        (untabify (point-min) (point-max)))
      (when (setq elt (cdr (assoc "indentation" rules)))
        (web-mode-buffer-indent))
      )))

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
        ) ;while
      (if (string= line "")
          (progn (goto-char pos) nil)
        (cons line (current-indentation)))
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
        (cons line (current-indentation)))
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

;; voir line-number-at-pos
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
              (eq (get-text-property i 'part-token input) 'comment)
              (eq (get-text-property i 'tag-type input) 'comment))
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        ) ;if
      ;;      (message "out=%s beg=%d" out beg)
      ) ;dotimes
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
      (if (or (not (get-text-property i 'block-side input))
              (member (get-text-property i 'block-token input) '(comment delimiter)))
          (when keep
            (setq out (concat out (substring input beg i))
                  beg 0
                  keep nil))
        (when (null keep)
          (setq beg i
                keep t))
        ) ;if
      ) ;dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (web-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

(defun web-mode-language-at-pos (&optional pos)
  "Return the language at pos."
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-side)
    web-mode-engine)
   ((get-text-property pos 'part-side)
    (symbol-name (get-text-property pos 'part-side)))
   (t
    web-mode-content-type)
   ) ;cond
  )

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
    :prev-line :prev-char :prev-props :prev-indentation"
  (save-excursion
    (let (ctx pos-min
              block-beg block-column first-char line type language indent-offset
              prev prev-line prev-char prev-props prev-indentation)

      (setq pos-min (point-min))
      (setq block-beg pos-min
            block-column 0
            type "live"
            language ""
            prev-line ""
            prev-char 0)
      (cond

       ((bobp)
        (setq language "html")
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

       ((or (and (member (get-text-property pos 'part-token) '(string context key))
                 (member (get-text-property (1- pos) 'part-token) '(string context key)))
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
         ((string= web-mode-engine "blade")
          (setq block-beg (+ block-beg 2)
                block-column (+ block-column 2)
                )
          )
         ((string= web-mode-engine "razor")
          (setq block-beg (+ block-beg 2)
;;                block-column (+ block-column 2)
                )
          )
         ((string= web-mode-engine "template-toolkit")
          (setq block-beg (+ block-beg 3)
                block-column (+ block-column 3))
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
         ) ;cond
        )

       ((get-text-property pos 'part-side)
;;        (setq block-beg (previous-single-property-change pos 'part-side))
;;        (message "pos-min=%S block-beg=%S part-beg=%S" pos-min block-beg (web-mode-part-beginning-position pos))
        (setq block-beg (web-mode-part-beginning-position pos))
        (setq block-beg (or block-beg pos-min))
        (goto-char block-beg)
        (search-backward "<" nil t)
        (setq block-column (current-column))
        (setq language (symbol-name (get-text-property pos 'part-side)))
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

       ) ;cond

      (goto-char pos)
      (setq line (web-mode-trim (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position))))
      (setq first-char (if (string= line "") 0 (aref line 0)))

      (when (or (member language '("php" "javascript" "razor"))
                (and (string= language "html") (not (eq ?\< first-char))))
        (cond
         ((member language '("html" "javascript"))
          (setq prev (web-mode-previous-usable-client-line))
          ;;          (message "prev-line=%S" prev-line)
          (when prev
            (setq prev-line (car prev)
                  prev-indentation (cdr prev))
            (setq prev-line (web-mode-clean-client-line prev-line))
            (setq prev-props (text-properties-at (1- (length prev-line)) prev-line)))
          )
         (t
          (setq prev (web-mode-previous-usable-server-line))
          (when prev
            (setq prev-line (car prev)
                  prev-indentation (cdr prev))
            (setq prev-line (web-mode-clean-server-line prev-line)))
            ;; (message "pl=%s" prev-line)
          )
         ) ;cond
        (when (>= (length prev-line) 1)
          (setq prev-char (aref prev-line (1- (length prev-line))))
          (setq prev-line (substring-no-properties prev-line))
          )
        )

;;      (if (string= language "json") (setq language "javascript"))

      (when (string= web-mode-content-type "html")
        (cond
         ((string= language "javascript")
          (setq block-column (+ block-column web-mode-script-padding))
;;          (message "block-column=%S" block-column)
          )
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
                      :prev-props prev-props
                      :prev-indentation prev-indentation))
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
        prev-props
        prev-indentation)

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
      (setq prev-indentation (plist-get ctx :prev-indentation))

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
        (cond
         ((and (string= (buffer-substring-no-properties (point) (+ (point) 2)) "/*")
                   (eq ?\* first-char))
          (setq offset (1+ offset)))
         ((and (string= web-mode-engine "django")
               (looking-back "{% comment %}"))
          (setq offset (- offset 12))
          )
         ((and (string= web-mode-engine "mako")
               (looking-back "<%doc%>"))
          (setq offset (- offset 6))
          )
         ) ;cond
        ) ;case comment

       ((and (or (web-mode-block-is-close pos)
                 (web-mode-block-is-inside pos))
             (web-mode-block-match))
        (setq offset (current-indentation))
        )

       ((member language '("asp" "aspx" "blade" "code" "django" "erb"
                           "freemarker" "javascript" "jsp" "mako" "mason"
                           "php" "python" "razor" "template-toolkit" "web2py"))

        (cond

         ;; ((and (string= language "javascript")
         ;;       (get-text-property pos 'block-beg))
         ;;  ;;          (message "ici")
         ;;  (if (or (web-mode-block-is-close pos)
         ;;          (web-mode-block-is-inside pos))
         ;;      (progn
         ;;        (web-mode-block-match)
         ;;        (setq offset (current-indentation)))
         ;;    (setq offset (web-mode-markup-indentation pos)))
         ;;  )

         ((string-match-p "^[?%]>" line)
          (if (web-mode-block-beginning pos)
              (setq offset (current-column)))
          )

         ((and (string= language "mason")
               (string-match-p "</%" line))
          (if (web-mode-block-beginning pos)
              (setq offset (current-column)))
          )

         ((and (string= language "web2py")
               (string-match-p "}}" line))
          (if (web-mode-block-beginning pos)
              (setq offset (current-column)))
          )

         ((and (string= language "razor")
               (string-match-p "^\\." line)
               (string-match-p "^\\." prev-line))
          (setq offset prev-indentation)
          )

         ((and (string= language "razor")
               (string-match-p "^}" line))
          (goto-char (web-mode-opening-paren-position (point)))
          (back-to-indentation)
          (setq offset (current-column))
          )

        ((and (string= language "razor")
              (string-match-p "^case " line)
              (string-match-p "^case " prev-line))
         (search-backward "case ")
         (setq offset (current-column))
         )

         ((and (string-match-p "^[=]?%]" line)
               (string= web-mode-engine "template-toolkit"))
          (if (web-mode-block-beginning pos)
              (setq offset (current-column)))
          )

         ((and (string= language "php") (string-match-p "^->" line))
          (when (web-mode-sb "->" block-beg)
            (setq offset (current-column)))
          )

         ((and (string= language "php") (string-match-p "\\.$" prev-line))
;;          (message "prev-line=%S" prev-line)
          (cond
           ((and (string-match-p "\\(=\\|echo \\)" prev-line)
;;                 (progn (message "%S" (point)) t)
                 (web-mode-rsb "\\(=\\|echo\\)[ ]+" block-beg))
            (goto-char (match-end 0))
            (setq offset (current-column))
            )
           ((string-match-p "^['\"$0-9]" prev-line)
            (setq offset prev-indentation)
            )
           (t
            (setq offset block-column)
            )
           )
          ;;(when (web-mode-sb "->" block-beg)
          ;;  (setq offset (current-column)))
          )

         ((and (string= language "php")
               (or (string-match-p "^else$" prev-line)
                   (string-match-p "^\\(if\\|for\\|foreach\\|while\\)[ ]*(.+)$" prev-line))
               (not (string-match-p "^{" line)))
          (setq offset (+ prev-indentation web-mode-code-indent-offset))
          )

         ((and (string= language "javascript") (eq ?\. first-char))
          (when (string-match-p "[[:alnum:][:blank:]]+\\.[[:alpha:]]" prev-line)
            (let ((new-offset (+ prev-indentation (search "." prev-line))))
              (setq offset new-offset)
              (message (number-to-string new-offset)))))

         ((and (member first-char '(?\? ?\. ?\:))
               (not (string= language "erb")))
          (web-mode-rsb "[^!=][=(]" block-beg)
          (setq offset (1+ (current-column)))
          (when (and (string= web-mode-engine "php")
                     (looking-at-p " =>"))
            (setq offset (1+ offset))
            )
          )

         ((and (member prev-char '(?\? ?\:))
               (not (string-match-p "^\\(case\\|default\\)[ :]" prev-line)))
          (web-mode-sb "?" block-beg)
          (when (looking-back ")[ ]*")
            (web-mode-sb ")" block-beg)
            (goto-char (web-mode-opening-paren-position (point)))
            )
          (web-mode-rsb "[=(]" block-beg)
          (if (eq (char-after) ?\=) (skip-chars-forward "= ") (skip-chars-forward "( "))
;;          (message "pt=%S" (point))
          (setq offset (current-column))
          )

         ((and (member prev-char '(?\. ?\+ ?\? ?\:))
               (not (string-match-p "^\\(case\\|default\\)[ :]" prev-line)))
          (web-mode-rsb "=\\|(" block-beg)
          (if (eq (char-after) ?\=) (skip-chars-forward "= ") (skip-chars-forward "( "))
          (setq offset (current-column))
          )

         ((string= language "erb")
          (setq offset (web-mode-ruby-indentation pos
                                                  line
                                                  block-column
                                                  indent-offset
                                                  block-beg))
          )

         ((member language '("mako" "web2py"))
          (setq offset (web-mode-python-indentation pos
                                                    line
                                                    block-column
                                                    indent-offset
                                                    block-beg))
          )

         ((string= language "asp")
          (setq offset (web-mode-asp-indentation pos
                                                 line
                                                 block-column
                                                 indent-offset
                                                 block-beg))
          )

         (t
          (setq offset (web-mode-bracket-indentation pos
                                                     block-column
                                                     indent-offset
                                                     language
                                                     block-beg))
          ) ;t

         )) ;end case script block

       ((string= language "css")
        (setq offset (web-mode-bracket-indentation pos
                                                   block-column
                                                   indent-offset
                                                   "css"
                                                   block-beg))
        ) ;case style

       (t ; case html block

        (cond

         ((and prev-props (plist-get prev-props 'tag-attr))
          (web-mode-tag-beginning)
          (let (skip)
            (setq skip (next-single-property-change (point) 'tag-attr))
            (when skip
              (goto-char skip)
              (setq offset (current-column))
              ))
          )

         ((and (string= web-mode-engine "mason")
               (string-match-p "^%" line))
          (setq offset 0)
          )

         ((and (string= web-mode-engine "razor")
;;               (get-text-property pos 'block-side)
               (string-match-p "^}" line))
;;          (message "ici")
          (goto-char (web-mode-opening-paren-position (point)))
          (back-to-indentation)
          (setq offset (current-column))
          )


         ((or (and (eq (get-text-property pos 'tag-type) 'end)
                   (web-mode-tag-match))
;;              (and (string= web-mode-engine "razor")
;;                   (string-match-p "^[ \t]*}" line)
;;                   (web-mode-block-match))
              ;; (and (get-text-property pos 'block-beg)
              ;;      (looking-at-p web-mode-close-block-regexp)
              ;;      (web-mode-block-match))
              )
          (setq offset (current-indentation))
          )

         ((or (eq (length line) 0)
              (= web-mode-indent-style 2)
              (get-text-property pos 'tag-beg)
              (get-text-property pos 'block-beg))
;;          (message "ici")
          (setq offset (web-mode-markup-indentation pos))
          )

         ) ;cond

        ) ;end case html block

       ) ;end switch language block

      ) ;save-excursion

    (when offset
      (let ((diff (- (current-column) (current-indentation))))
        (setq offset (max 0 offset))
        (indent-line-to offset)
        (if (> diff 0) (forward-char diff))

        (when (and (string= web-mode-engine "mason")
                   (= offset 0)
                   (string-match-p "^%" line))
          (web-mode-highlight-region (line-beginning-position) (line-end-position)))

        ) ;let
      ) ;when

    ))

(defun web-mode-block-is-control (pos)
  "web-mode-block-is-control"
  (save-excursion
    (let (control state controls pair)
      (goto-char pos)
      (setq controls (web-mode-block-controls pos))
      (setq pair (car controls))
      (cond
       ((eq (car pair) 'inside)
        )
       ((eq (car pair) 'open)
        (setq state t
              control (cdr pair)))
       ((eq (car pair) 'close)
        (setq state nil
              control (cdr pair)))
       ) ;cond
      ;;      (message "engine=%S control=%S state=%S" web-mode-engine control state)
      (if control (cons control state) nil)
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
                     (web-mode-block-is-control (point))
                     (not (looking-at-p "{% comment"))))
        (setq continue nil
              pos (point))
        )
      ) ;while
;;    (message "indent-origin=%S" pos)
    pos
    ))

(defun web-mode-markup-indentation (pos)
  "markup indentation"
  (save-excursion
    (goto-char pos)
    (let ((offset 0) beg ret)
;;      (if (or (member web-mode-content-type '("javascript" "css"))
;;              (get-text-property pos 'part-side))
;;          (setq beg (web-mode-part-beginning-position pos))
;;        (setq beg (web-mode-markup-indentation-origin)))
      (setq beg (web-mode-markup-indentation-origin))
      (when beg
        (goto-char beg)
        (setq ret (web-mode-element-is-opened beg pos))
        (cond
         ((null ret)
          (setq offset (current-indentation)))
         ((eq ret t)
          (setq offset (+ (current-indentation) web-mode-markup-indent-offset))
          )
         (t
          (setq offset ret))
         )
;;        (setq offset (+ (current-indentation)
;;                        (if (web-mode-element-is-opened beg pos)
;;                            web-mode-markup-indent-offset
;;                          0)))
        ) ;when
      offset
      )))

;; state=t <=> start tag
(defun web-mode-element-is-opened (pos limit)
  "Is there any HTML element without a closing tag ?"
  (interactive)
  (let (tag
        last-tag
        tag-pos block-pos
        state
        n
        ret
        (continue t)
        (buffer (current-buffer))
        (h (make-hash-table :test 'equal))
        (h2 (make-hash-table :test 'equal))
        ctrl)
    (while continue
      (setq ctrl nil
            last-tag nil)
      (when (or (and (get-text-property pos 'tag-beg)
                     (member (get-text-property pos 'tag-type) '(start end)))
                (and (get-text-property pos 'block-beg)
;;                     (progn (message "pos=%S" pos) t)
                     (setq ctrl (web-mode-block-is-control pos))))
;;        (message "ctrl=%S" ctrl)
        (if ctrl
            (setq tag (car ctrl)
                  state (cdr ctrl))
          (setq tag (get-text-property pos 'tag-name)
                state (eq (get-text-property pos 'tag-type) 'start))
          (if (null state) (setq last-tag (cons tag pos)))
          )
;;        (message "pos=%S tag=%S state=%S" pos tag state)
        (setq n (gethash tag h 0))
        (if (null state)
            (progn
              (when (> n 0) (puthash tag (1- n) h))
              (puthash tag (1- n) h2))
          (puthash tag (1+ n) h)
          (puthash tag (1+ n) h2))
        ) ;when
      (setq pos (1+ pos))
      (when (null tag-pos)
        (setq tag-pos (next-single-property-change pos 'tag-beg buffer limit)))
      (when (null block-pos)
        (setq block-pos (next-single-property-change pos 'block-beg buffer limit))
;;        (message "block-pos=%S limit=%S" block-pos limit)
        )
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
      ) ;while
;;    (message "hashtable=%S" h)
    (maphash (lambda (k v) (if (> v 0) (setq ret t))) h)
    (when (and (null ret)
               last-tag
               (> (hash-table-count h2) 1)
               (< (gethash (car last-tag) h2) 0))
;;      (message "last-tag=%S" last-tag)
      (save-excursion
        (goto-char (cdr last-tag))
        (web-mode-tag-match)
        (when (not (= (point) (cdr last-tag)))
          (setq n (point))
          (back-to-indentation)
          (if (= n (point)) (setq ret (current-indentation))))
        ))
    ret))

(defun web-mode-ruby-indentation (pos line initial-column language-offset limit)
  "Calc indent column."
  (interactive)
  (unless limit (setq limit nil))
  (let (h out prev-line prev-indentation ctx)
    (setq ctx (web-mode-count-opened-brackets pos "ruby" limit))
    (if (cddr ctx)
        (progn
;;          (message "ctx=%S" (car (cdr ctx)))
          (setq out (cadr ctx))
;;          (message "out=%S" out)
          )
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
        ) ;when
      ) ;if
    out
    ))

(defun web-mode-python-indentation (pos line initial-column language-offset limit)
  "Calc indent column."
  (interactive)
  (unless limit (setq limit nil))
  (let (h out prev-line prev-indentation ctx)
;;    (setq ctx (web-mode-count-opened-brackets pos limit))
;;    ;; (if (cddr ctx)
;;        (progn
;;          (message "ctx=%S" (car (cdr ctx)))
;;          (setq out (cadr ctx))
;;          (message "out=%S" out)
;;          )
    (setq h (web-mode-previous-line pos limit))
    ;;      (message "h=%S" h)
    (setq out initial-column)
    (when h
      (setq prev-line (car h))
      ;;(message "line=%S" prev-line)
      (setq prev-indentation (cdr h))
      (cond
       ((string-match-p "^\\(pass\\|else\\|elif\\|when\\)" line)
        (setq out (- prev-indentation language-offset))
        )
       ((string-match-p "\\(if\\|else\\|elif\\|for\\|while\\)" prev-line)
        (setq out (+ prev-indentation language-offset))
        )
       (t
        (setq out prev-indentation)
        )
       )
      ) ;when
    ;;      ) ;if
    out
    ))

(defun web-mode-asp-indentation (pos line initial-column language-offset limit)
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
       ;; ----------------------------------------------------------------------
       ;; unindent
       ((string-match-p "\\<\\(\\(end \\(if\\|function\\|class\\|sub\\|with\\)\\)\\|else\\|elseif\\|next\\|loop\\)\\>" line)
        (setq out (- prev-indentation language-offset)))
       ;; ----------------------------------------------------------------------
       ;; select case statement
       ((string-match-p "\\<\\(select case\\)\\>" line)
        (setq out (- prev-indentation 0)))
       ((string-match-p "\\<\\(end select\\)" line)
        (setq out (- prev-indentation (* 2 language-offset))))
       ((and (string-match-p "\\<\\(case\\)\\>" line) (not (string-match-p "\\<\\(select case\\)\\>" prev-line)))
        (setq out (- prev-indentation language-offset)))
       ;; ----------------------------------------------------------------------
       ;; do nothing
       ((string-match-p "\\<\\(\\(end \\(if\\|function\\|class\\|sub\\|select\\|with\\)\\)\\|loop\\( until\\| while\\)?\\)\\>" prev-line)
        (setq out (+ prev-indentation 0)))
       ;; indent
       ((string-match-p "\\<\\(\\(select \\)?case\\|else\\|elseif\\|unless\\|for\\|class\\|with\\|do\\( until\\| while\\)?\\|while\\|\\(public \\|private \\)?\\(function\\|sub\\|class\\)\\)\\>" prev-line)
        (setq out (+ prev-indentation language-offset)))
       ;; single line if statement
       ((string-match-p "\\<if\\>.*\\<then\\>[ \t]*[[:alpha:]]+" prev-line)
        (setq out (+ prev-indentation 0)))
       ;; normal if statement
       ((string-match-p "\\<\\if\\>" prev-line)
        (setq out (+ prev-indentation language-offset)))
       (t
        (setq out prev-indentation))
       )
      ) ;when
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
        ) ;if
      (setq line (buffer-substring-no-properties beg end))
      ;;      (message "line=%s" line)
      (cons line (current-indentation))
      )))

(defun web-mode-bracket-indentation (pos initial-column language-offset language &optional limit)
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

      (setq block-info (web-mode-count-opened-brackets pos language limit))
;;      (message "bi=%S" block-info)
      (setq col initial-column)
      (if (cddr block-info)
          (progn
            (setq col (car (cdr block-info)))
            )
        (setq n (car block-info))
        (setq col initial-column)
;;        (message "initial-col=%S n=%S col=%S" initial-column n col)
        (if (member first-char '(?\} ?\) ?\])) (setq n (1- n)))
        (setq col (+ initial-column (* n language-offset)))
        ) ;if
      (if (< col block-column) block-column col)
      )))

(defun web-mode-count-opened-blocks (pos &optional limit)
  "Count opened opened control blocks at POS."
  (save-excursion
    (goto-char pos)
    (let ((continue t) pair controls control type (counter 0))
      (when (null limit)
        (cond
         ((get-text-property pos 'part-side)
          (setq limit (web-mode-part-beginning-position pos)))
         ((get-text-property pos 'block-side)
          (setq limit (web-mode-block-beginning-position pos)))
         ) ;cond
        ) ;when
      (while continue
        (cond
         ((bobp)
          (setq continue nil))
         ((not (web-mode-block-previous))
          (setq continue nil)
          )
         ((null (get-text-property (point) 'block-controls))
          )
         (t
          (setq controls (get-text-property (point) 'block-controls))
          (setq pair (car controls))
          (cond
           ((eq (car pair) 'open)
            (setq counter (1+ counter)))
           ((eq (car pair) 'inside)
            )
           (t
            (setq counter (1- counter)))
           )
          ) ;t
         ) ;cond
        ) ;while
      (if (>= counter 0) counter 0)
      )))

;; return (opened-blocks . (col-num . arg-inline))
(defun web-mode-count-opened-brackets (pos language &optional limit)
  "Count opened brackets at POS."
  (interactive)
  (unless limit (setq limit nil))
  (save-excursion
    (goto-char pos)
    (let ((continue t)
          (match "")
;;          (case-num 0)
;;          (is-breaked nil)
;;          (case-found nil)
;;          (case-count 0)
          (switch-level 0)
          (queues (make-hash-table :test 'equal))
          (opened-blocks 0)
          (col-num 0)
;;          (regexp "[\]\[}{)(]\\|[ ;\t]\\(switch[ ]\\|break[ ;]\\|case[ :]\\|default[ :]\\)")
;;          (regexp "[\]\[}{)(]\\|[ ;\t]\\(switch[ ]\\)")
          (num-opened 0)
          regexp
          close-char n queue arg-inline arg-inline-checked char lines reg-end)

      (cond
       ((string= language "css")
        (setq regexp "[\]\[}{)(]"))
       (t
        (setq regexp "[\]\[}{)(]\\|[ ;\t]\\(switch[ ]\\)"))
       )

      (while (and continue (re-search-backward regexp limit t))
;;        (message "%S: %c" (point) (char-after))
        (unless (web-mode-is-comment-or-string)
          (setq match (match-string-no-properties 0))
;;          (message "match=%S" match)
          (when (> (length match) 1)
;;            (message "match=%S" (match-string-no-properties 0))
            (skip-chars-forward "[ \t]")
            (setq match (replace-regexp-in-string "\\`[ ;\t]*" "" (replace-regexp-in-string "[ ]*\\'" "" match)))
            )
          (setq char (aref match 0))
;;          (message "match:%S" match)

          (cond

           ((member char '(?\{ ?\( ?\[))
            (cond
             ((eq char ?\() (setq close-char ?\)))
             ((eq char ?\{) (setq close-char ?\}))
             ((eq char ?\[) (setq close-char ?\])))

            (setq queue (gethash char queues nil))
            (setq queue (push (cons (point) (web-mode-line-number)) queue))
            (puthash char queue queues)
;;            (message "%c queue=%S" char queue)

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
;;              (message "(%S) %c queue=%S" (point) char queue)
              )
             ((= n 0)
              (setq num-opened (1+ num-opened))
;;              (message "num-opened=%S %S" num-opened (point))
              )
             )

            (when (and (= num-opened 1) (null arg-inline-checked))
              (setq arg-inline-checked t)
              (when (not (web-mode-is-void-after (1+ (point)))) ;(not (looking-at-p ".[ ]*$"))
                (setq arg-inline t
                      col-num (1+ (current-column)))
                (when (looking-at ".[ ]+")
                  (setq col-num (+ col-num (1- (length (match-string-no-properties 0)))))
                  )
                )
              )

            ) ;case (?\{ ?\( ?\[)

           ((member char '(?\} ?\) ?\]))
            (setq queue (gethash char queues nil))
            (setq queue (push (point) queue))
            (puthash char queue queues)
            ;;            (message "%c queue=%S" char queue)
            )

           ((string= match "switch")
;;            (setq case-num 0)
;;            (setq is-breaked nil)
            ;;            (message "switch=%S" (match-end 1))
            (let (tmp)
              (when (null reg-end)
                (cond
                 ((member language '("css" "javascript"))
                  (setq reg-end (web-mode-part-end-position pos))
                  )
                 (t
                  (setq reg-end (web-mode-block-end-position pos))
                  )
                 )
                ) ;when
;;              (message "reg-end=%S" reg-end)
              (setq tmp (web-mode-bracket-block-end-position (point) reg-end))
              (when (and tmp (< pos tmp))
;;                (message "bol(%S) is inside switch(%S)" pos (point))
                (setq switch-level (1+ switch-level))
                )
              )
            )

;;            ((member match '("case" "default"))
;;             ;;            (if (null is-breaked)
;;             ;;                (setq is-breaked nil
;;             ;;                      case-count (1- case-count)
;;             ;;)
;;             ;;                (setq is-breaked nil
;;             ;;                      case-count (1- case-count))
;; ;;            (when (null is-breaked)
;; ;;              (setq is-breaked t
;;                     ;;                    case-found t
;; ;;                    case-count (1- case-count)
;; ;;                    )
;; ;;              ;;              ) ;if
;;             ;;  )

;; ;;            (when is-breaked

;;             (setq case-num (1+ case-num))

;;             (when (and (= case-num 1) (not is-breaked))
;; ;;              (message "faked break = %S" (point))
;;               (setq case-count (1- case-count))
;;               )

;; ;;            (setq is-breaked nil)

;;             (setq case-count (1+ case-count))
;;             ;;              )
;; ;;            (message "%S: pt%S %S" match (point) is-breaked)

;;             (cond
;;              ((not (looking-back "\\(break[ ]*;\\|{\\)[ \t\n]*" limit t))
;; ;;              (message "pas de break")
;;               (setq case-count (1- case-count))
;;               )
;;              ) ;cond

;;             )

;;            ((string= match "break")
;;             (setq is-breaked t
;;                   case-count (1- case-count))
;; ;;            (message "%S: pt%S %S" match (point) is-breaked)
;;             )

           ) ;cond

          ) ;unless
        ) ;while

      (unless arg-inline
;;        (message "%S" queues)
        (maphash
         (lambda (char queue)
           (when (member char '(?\{ ?\( ?\[))
;;             (message "%c => %S" char queue)
             (dolist (pair queue)
               (setq n (cdr pair))
               (unless (member n lines)
                 (push n lines))
               )
             ) ;when
           )
         queues)
;;        (message "lines=%S case-found=%S case-count=%S" lines case-found case-count)
        (setq opened-blocks (length lines))

;;         (when (and case-found (> case-count 0))
;;           (goto-char pos)
;;           (back-to-indentation)
;; ;;          (message "%S" (point))
;; ;;          (when (not (looking-at-p "case\\|}"))
;; ;;            (setq opened-blocks (1+ opened-blocks))
;;           (setq opened-blocks (+ opened-blocks case-count))
;; ;;            )
;;           ) ;when case-count

;;        (setq opened-blocks (+ opened-blocks case-count))

        (goto-char pos)
        (when (and (> switch-level 0)
                   (not (looking-at-p "\\(case[ ]\\|default[ :]\\)")))
          (setq opened-blocks (+ opened-blocks switch-level)))

        (when (member language '("css" "javascript"))
          ;;                 (setq tmp (web-mode-count-opened-blocks pos))
          ;;                   )
          ;;        (setq opened-blocks (+ opened-blocks tmp))
          (setq opened-blocks (+ opened-blocks (web-mode-count-opened-blocks pos))))

        ) ;unless

;;      (message "opened-blocks(%S) col-num(%S) arg-inline(%S)" opened-blocks col-num arg-inline)

;;      (message "pos=%S ob=%S" pos (web-mode-count-opened-blocks pos))

      (cons opened-blocks (cons col-num arg-inline))

      )))

(defun web-mode-bracket-block-end-position (pos limit)
  "web-mode-blk-end-pos"
  (save-excursion
    (goto-char pos)
    (setq pos nil)
;;    (message "limit=%S pos=%S" limit (point))
    (when (web-mode-sf "{" limit)
      (backward-char)
;;      (message "pos=%S" (point))
      (when (web-mode-closing-paren limit)
        (setq pos (point))
        )
      )
    )
  pos)

(defun web-mode-is-void-after (pos)
  "Only spaces or comment after pos"
  (save-excursion
    (goto-char pos)
;;    (message "pos=%S" pos)
    (let ((eol (line-end-position)) (continue t) c (ret t) part-side)
      (setq part-side (not (null (get-text-property pos 'part-side))))
      (while continue
        (setq c (char-after pos))
;;        (message "%S c='%c'" pos c)
        (cond
         ((member c '(?\s ?\n)) )
         ((and part-side (eq (get-text-property pos 'part-token) 'comment)) )
         ((and part-side (get-text-property pos 'block-side)) )
         ((and (not part-side) (eq (get-text-property pos 'block-token) 'comment)) )
         (t (setq ret nil
                  continue nil))
         )
        (when continue
          (setq pos (1+ pos))
          (when (>= pos eol) (setq continue nil)))
        ) ;while
      ret)))

(defun web-mode-count-char-in-string (char string)
  "Count char in string."
  (let ((n 0))
    (dotimes (i (length string))
      (if (eq (elt string i) char)
          (setq n (1+ n))))
    n))

;; (defun web-mode-scan-at-pos ()
;;   "web mode scan at point"
;;   (save-excursion
;;     (let (scan-beg scan-end (pos (point)))
;;       (cond
;;        ((web-mode-rsb-client "^[ ]*<")
;;         (setq scan-beg (point))
;;         (goto-char pos)
;;         (setq scan-end (if (web-mode-rsf-client "[[:alnum:] /\"]>[ ]*$") (point) (point-max)))
;;         ;;              (message "scan-end=%S" scan-end)
;;         ;;            (setq scan-end (point-max))
;;         )
;;        (t
;;         (setq scan-beg 1
;;               scan-end (point-max))
;;         )
;;        ) ;cond
;;       ;;(message "scan-region (%S) > (%S)" scan-beg scan-end)
;;       ;;          (setq scan-end (point-max))
;;       (web-mode-highlight-region scan-beg scan-end)
;;       ) ;save-excursion
;;     ))

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

     ((and (get-text-property pos 'block-side)
           (not (member web-mode-engine '(django go)))
           (setq boundaries (web-mode-in-code-block "{" "}" 'block-side))
           (not (string= web-mode-expand-previous-state "server-block")))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      ;;      (message "char=[%c]" (char-before (- (point) 1)))
      (if (eq ?\% (char-before (- (point) 1)))
          (setq web-mode-expand-previous-state "block-side")
        (setq web-mode-expand-previous-state "server-block"))
      (exchange-point-and-mark)
      )

     ((and (get-text-property pos 'block-side)
           (not (string= web-mode-expand-previous-state "block-side")))
      (when (eq (get-text-property pos 'block-side) (get-text-property (1- pos) 'block-side))
        (setq beg (or (previous-single-property-change pos 'block-side) (point-min))))
      (when (eq (get-text-property pos 'block-side) (get-text-property (1+ pos) 'block-side))
        (setq end (next-single-property-change pos 'block-side)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "block-side"))

     ((and (get-text-property pos 'part-token)
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

     ((and (get-text-property pos 'part-side)
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
     ((and (get-text-property pos 'tag-attr)
           (not (string= web-mode-expand-previous-state "html-attr")))

      (when (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token))
        (setq beg (previous-single-property-change pos 'part-token)))
      (when (eq (get-text-property pos 'part-token) (get-text-property (1+ pos) 'part-token))
        (setq end (next-single-property-change pos 'part-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "html-attr"))

     ((and mark-active (eq ?\< (char-after)))
      (web-mode-element-parent)
      (if (= reg-beg (region-beginning))
          (mark-whole-buffer)
        (web-mode-element-select))
      )

     (t
      (web-mode-element-select)
      ;;(mark-whole-buffer)
      )

     ) ;cond

;;    (message "after=%S" web-mode-expand-previous-state)

    ))

(defun web-mode-block-select ()
  "Select the current block."
  (interactive)
  (let (beg)
    (setq beg (web-mode-block-beginning-position (point)))
    (when beg
      (goto-char beg)
      (set-mark (point))
      (web-mode-block-end)
      (exchange-point-and-mark)
      )
    beg))

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
         ) ;cond
      (web-mode-element-parent)
      (unless (= (point) pos) (web-mode-element-select))
      ) ;if
    ))

(defun web-mode-element-is-collapsed (&optional pos)
  "Is the HTML element collapsed ?"
  (unless pos (setq pos (point)))
  (let (boundaries ret)
    (setq boundaries (web-mode-element-boundaries pos))
    (setq ret (and boundaries
                   (or (= (car (car boundaries)) (car (cdr boundaries)))
                       (= (cdr (car boundaries)) (1- (car (cdr boundaries)))))
                   ))
;;    (when ret (message "elt(%S) is collapsed" pos))
    ret))

(defun web-mode-element-transpose ()
  "Transpose two HTML elements."
  (interactive)
  (let (pos start1 end1 start2 end2)
    (save-excursion
      (setq pos (point))
      (cond
       ((get-text-property pos 'tag-type)
        (setq start1 (web-mode-element-beginning-position pos)
            end1 (1+ (web-mode-element-end-position pos)))
        )
       ((setq start1 (web-mode-element-parent-position pos))
        (setq end1 (1+ (web-mode-element-end-position pos)))
        )
       ) ;cond
      (when (and start1 end1 (> end1 0))
        (goto-char end1)
        (unless (get-text-property (point) 'tag-beg)
          (skip-chars-forward "\n\t "))
        (when (get-text-property (point) 'tag-beg)
          (setq start2 (web-mode-element-beginning-position (point))
                end2 (1+ (web-mode-element-end-position (point))))
          )
        )
;;      (message "start1(%S) end1(%S) start2(%S) end2(%S)"
;;               start1 end1 start2 end2)
      (transpose-regions start1 end1 start2 end2)
      ) ;save-excursion
    start2
    ))

(defun web-mode-element-children-fold-or-unfold (&optional pos)
  "Fold/Unfold all the children of the current HTML element."
  (interactive)
  (unless pos (setq pos (point)))
  (let (child children)
    (save-excursion
      (setq children (reverse (web-mode-element-children-position pos)))
      (dolist (child children)
;;        (message "child(%S)" child)
        (goto-char child)
        (web-mode-fold-or-unfold)
        )
      )))

(defun web-mode-element-mute-blanks ()
  "Mute blanks."
  (interactive)
  (let (pos parent beg end child children elt)
    (setq pos (point))
    (save-excursion
      (when (and (setq parent (web-mode-element-boundaries pos))
                 (setq child (web-mode-element-child-position (point))))
        (setq children (reverse (web-mode-element-children-position)))
;;        (message "%S %S" parent children)
        ;;        (setq end (car (cdr parent)))
        ;;        (message "end=%S" end)
        (goto-char (car (cdr parent)))
        (dolist (child children)
          (setq elt (web-mode-element-boundaries child))
          ;;          (message "child=%S elt=%S" child elt)
          (when (> (point) (1+ (cddr elt)))
            ;;(message "%S-->" (point))
            ;;(message "%S<!--" (1+ (cddr elt)))
            (when (and (not (eq (get-text-property (point) 'part-token) 'comment))
                       (not (eq (get-text-property (1+ (cddr elt)) 'part-token) 'comment)))
              (web-mode-insert-text-at-pos "-->" (point))
              (web-mode-insert-text-at-pos "<!--" (1+ (cddr elt))))
            )
          (goto-char child)
          )
        (when (and (> (point) (1+ (cdr (car parent))))
                   (not (eq (get-text-property (point) 'part-token) 'comment))
                   (not (eq (get-text-property (1+ (cdr (car parent))) 'part-token) 'comment)))
          (web-mode-insert-text-at-pos "-->" (point))
          (web-mode-insert-text-at-pos "<!--" (1+ (cdr (car parent)))))

        ) ;when
      )))

(defun web-mode-element-children-position (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t) (i 0) child children)
    (save-excursion
      (when (and (member (get-text-property pos 'tag-type) '(start end))
                 (setq child (web-mode-element-child-position pos)))
        (while continue
          (setq i (1+ i))
          (cond
           ((= i 1)
            (goto-char child)
            )
           ((web-mode-element-sibling-next)
            )
           (t
            (setq continue nil)
            )
           ) ;cond
          (when (> i 100)
            (message "** invalid loop **")
            (setq continue nil))
          (when continue
;;            (message "child(%S)" (point))
            (setq children (append children (list (point)))))
          ) ;while
        ) ;when
      ) ;save-excursion
    children
    ))

;; return ((start-tag-beg . start-tag-end) . (end-tag-beg end-tag-end))
;; car and cdr are the same with void elements
(defun web-mode-element-boundaries (&optional pos)
  "pos should be in a tag"
  (unless pos (setq pos (point)))
  (let (start-tag-beg start-tag-end end-tag-beg end-tag-end)
    (save-excursion
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (setq start-tag-beg (web-mode-tag-beginning-position pos)
              start-tag-end (web-mode-tag-end-position pos))
        (web-mode-tag-match pos)
;;        (message "pos=%S point=%S" pos (point))
        (setq end-tag-beg (point)
              end-tag-end (web-mode-tag-end-position (point)))
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (setq end-tag-beg (web-mode-tag-beginning-position pos)
              end-tag-end (web-mode-tag-end-position pos))
        (web-mode-tag-match pos)
        (setq start-tag-beg (point)
              start-tag-end (web-mode-tag-end-position (point)))
        )
       ((eq (get-text-property pos 'tag-type) 'void)
        (setq start-tag-beg (web-mode-tag-beginning-position pos)
              start-tag-end (web-mode-tag-end-position pos))
        (setq end-tag-beg start-tag-beg
              end-tag-end start-tag-end)
        )
       ) ;cond
      )
    (if (and start-tag-beg start-tag-end end-tag-beg end-tag-end)
        (cons (cons start-tag-beg start-tag-end) (cons end-tag-beg end-tag-end))
      nil)
    ))

(defun web-mode-element-wrap ()
  "Wrap current REGION with start and end tags."
  (interactive)
  (let (beg end pos tag sep)
    (save-excursion
      (setq tag (read-from-minibuffer "Tag name? "))
      (setq pos (point))
      (cond
       (mark-active
        (setq beg (region-beginning)
              end (region-end)))
       ((get-text-property pos 'tag-type)
        (setq beg (web-mode-element-beginning-position pos)
              end (1+ (web-mode-element-end-position pos)))
        )
       ((setq beg (web-mode-element-parent-position pos))
        (setq end (1+ (web-mode-element-end-position pos)))
        )
       )
      ;;      (message "beg(%S) end(%S)" beg end)
      (when (and beg end (> end 0))
        (setq sep (if (get-text-property beg 'tag-beg) "\n" ""))
        (web-mode-insert-text-at-pos (concat sep "</" tag ">") end)
        (web-mode-insert-text-at-pos (concat "<" tag ">" sep) beg)
        (when (string= sep "\n") (indent-region beg (+ end (* (+ 3 (length tag)) 2))))
        )
      ) ;save-excursion
    (if beg (goto-char beg))
    beg))

(defun web-mode-element-vanish ()
  "Vanish the current HTML element. The content of the element is kept."
  (interactive)
  (let (type (pos (point)) start-b start-e end-b end-e)
    (setq type (get-text-property pos 'tag-type))
    (when type
      (cond
       ((member type '(void))
        (web-mode-element-kill)
        (set-mark (point))
        (web-mode-tag-match)
        (web-mode-tag-end)
        (exchange-point-and-mark))
       ((member type '(start))
        (setq start-b (web-mode-tag-beginning-position)
              start-e (web-mode-tag-end-position))
        (when (web-mode-tag-match)
          (setq end-b (web-mode-tag-beginning-position)
                end-e (web-mode-tag-end-position)))
        )
       (t
        (setq end-b (web-mode-tag-beginning-position)
              end-e (web-mode-tag-end-position))
        (when (web-mode-tag-match)
          (setq start-b (web-mode-tag-beginning-position)
                start-e (web-mode-tag-end-position)))
        ) ;t
       ) ;cond
      (when (and start-b end-b)
        (goto-char end-b)
        (delete-region end-b (1+ end-e))
        (delete-blank-lines)
        (goto-char start-b)
        (delete-region start-b (1+ start-e))
        (delete-blank-lines)
        (web-mode-buffer-indent)
        )
;;        (message "start %S %S - end %S %S" start-b start-e end-b end-e))
      ) ;when
    ))

(defun web-mode-element-kill ()
  "Kill the current HTML element."
  (interactive)
  (web-mode-element-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

(defun web-mode-block-kill ()
  "Kill the current block."
  (interactive)
  (web-mode-block-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

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
                 (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)"))
        (setq pos (point))
;;        (message "match=%S" (match-string-no-properties 1))
        (unless (web-mode-element-is-void)
            (save-match-data
              (web-mode-tag-match)
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)")
                  (replace-match (concat "</" tag-name))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag-name))
;;        (web-mode-scan-at-pos)
        ))))

(defun web-mode-current-trimmed-line ()
  "Line at point, trimmed."
  (web-mode-trim (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))

(defun web-mode-trim (string)
  "Remove white spaces in beginning and ending of STRING."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;; on regarde le dernier
(defun web-mode-block-is-open (&optional pos)
  "web-mode-block-is-open"
  (unless pos (setq pos (point)))
  )

;; on regarde le premier
(defun web-mode-block-is-close (&optional pos)
  "web-mode-block-is-close"
  (unless pos (setq pos (point)))
  (eq (caar (web-mode-block-controls pos)) 'close)
  )

;; on regarde le premier
(defun web-mode-block-is-inside (&optional pos)
  "web-mode-block-is-inside"
  (unless pos (setq pos (point)))
  (eq (caar (web-mode-block-controls pos)) 'inside)
  )

(defun web-mode-block-controls (&optional pos)
  "web-mode-block-controls"
  (unless pos (setq pos (point)))
  (let (controls)
    (when (get-text-property pos 'block-side)
      (setq pos (web-mode-block-beginning-position pos))
      (setq controls (get-text-property pos 'block-controls)))
    controls))

(defun web-mode-element-is-void (&optional tag)
  "Test if tag is a void tag."
  (if tag
      (car (member (downcase tag) web-mode-void-elements))
    (eq (get-text-property (point) 'tag-type) 'void)
    ))

(defun web-mode-fold-or-unfold (&optional pos)
  "Toggle folding on an HTML element or a control block."
  (interactive)
  (web-mode-with-silent-modifications
   (save-excursion
     (if pos (goto-char pos))
     (let (beg-inside beg-outside end-inside end-outside overlay overlays regexp)
       (when (looking-back "^[\t ]*")
         (back-to-indentation))
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
        ((member (get-text-property (point) 'tag-type) '(start end))
         (when (not (web-mode-element-is-collapsed (point)))
           (web-mode-tag-beginning)
           (when (eq (get-text-property (point) 'tag-type) 'end)
             (web-mode-tag-match))
           (setq beg-outside (point))
           (web-mode-tag-end)
           (setq beg-inside (point))
           (goto-char beg-outside)
           (when (web-mode-tag-match)
             (setq end-inside (point))
             (web-mode-tag-end)
             (setq end-outside (point)))
           )
         )
        ;; *** block folding
        ((cdr (web-mode-block-is-control (point)))
         (setq beg-outside (point))
         (web-mode-block-end)
         (setq beg-inside (point))
         (goto-char beg-outside)
         (when (web-mode-tag-match)
           (setq end-inside (point))
           (web-mode-block-end)
           (setq end-outside (point)))
         )
        ) ;cond
       (when end-outside
         ;;(message "beg-out(%d) beg-in(%d) end-in(%d) end-out(%d)" beg-outside beg-inside end-inside end-outside)
         (setq overlay (make-overlay beg-outside end-outside))
         (overlay-put overlay 'font-lock-face 'web-mode-folded-face)
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
        (setq pos (next-single-property-change pos 'font-lock-face))
        (if (null pos)
            (setq continue nil)
          (when (eq (get-text-property pos 'font-lock-face) 'web-mode-comment-face)
            (setq end (next-single-property-change pos 'font-lock-face))
            (put-text-property pos end 'invisible visibility)
            (when visibility
              (setq overlay (make-overlay pos end)))
            (goto-char pos)
            )
          )
        )
      ) ;let
    ))

(defun web-mode-is-single-line-block (pos)
  "Is block at POS spread on a single line ?"
  (= (web-mode-line-number (web-mode-block-beginning-position pos))
     (web-mode-line-number (web-mode-block-end-position pos))))

(defun web-mode-comment-or-uncomment ()
  "Comment or uncomment line(s), block or region at POS."
  (interactive)
;;  (message "%S" (point))
  (save-excursion
    (unless mark-active
      (skip-chars-forward "[:space:]" (line-end-position)))
    (if (web-mode-is-comment)
	(web-mode-uncomment (point))
      (web-mode-comment (point))))
;;  (message "%S" (point))
;;  (web-mode-highlight-region (point-min) (point-max))
  )

(defun web-mode-comment (pos)
  "Comment line(s) at point."
  (interactive)
;;  (message "pt=%S" pos)
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
            (progn
              (setq beg (region-beginning)
                    end (region-end))
;;              (message "beg=%S end=%S" beg end)
              )
          (if (and (string= language "html")
                   (progn (back-to-indentation) t)
                   (get-text-property (point) 'tag-beg))
              ;;              (progn
              ;;                (back-to-indentation)
              (web-mode-element-select)
            ;;            )
            (end-of-line)
            (set-mark (line-beginning-position))
            ) ;if
          (setq beg (region-beginning)
                end (region-end))
          ) ;if mark-active

        (when (> (point) (mark))
          (exchange-point-and-mark))

        (if (eq (char-before end) ?\n)
            (setq end (1- end)))

;;        (message "language=%S beg=%S end=%S" language beg end)
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
            (web-mode-insert-and-indent (concat
                                         (web-mode-engine-delimiter-open web-mode-engine "{")
                                         "* "
                                         sel
                                         " *"
                                         (web-mode-engine-delimiter-close web-mode-engine "}")))
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

         ((member language '("erb"))
          (web-mode-insert-and-indent (replace-regexp-in-string "^" "#" sel)))

         ((member language '("asp"))
          (web-mode-insert-and-indent (replace-regexp-in-string "^" "''" sel)))

         (t
          (web-mode-insert-and-indent (concat "/* " sel " */")))

         ) ;cond

        ) ;t
       ) ;cond

      )
    ) ;save-excursion
;;  (message "%S" (point))
;;  (goto-char pos)
  )

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

(defun web-mode-remove-text-at-pos (n &optional pos)
  "Remove N chars at POS."
  (unless pos (setq pos (point)))
  (delete-region pos (+ pos n)))

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
    (web-mode-remove-text-at-pos 2 (1- end))
    (web-mode-remove-text-at-pos 2 beg)
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

      (cond
       ((eq (get-text-property pos 'block-token) 'comment)
        (setq prop 'block-token))
       ((eq (get-text-property pos 'tag-type) 'comment)
        (setq prop 'tag-type))
       ((eq (get-text-property pos 'part-token) 'comment)
        (setq prop 'part-token))
       )

      (if (and (not (bobp))
               (eq (get-text-property pos prop) (get-text-property (- pos 1) prop)))
          (setq beg (or (previous-single-property-change pos prop)
                        (point-min))))

      (if (and (not (eobp))
               (eq (get-text-property pos prop) (get-text-property (+ pos 1) prop)))
          (setq end (or (next-single-property-change pos prop)
                        (point-max))))

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

        ) ;when

      ))
    (indent-for-tab-command)
    ))

(defun web-mode-snippet-names ()
  "Return the snippet names."
  (interactive)
  (let (codes snippet)
    (dolist (snippet web-mode-snippets)
      (add-to-list 'codes (car snippet) t))
    codes))

(defun web-mode-snippet-insert (code)
  "Insert snippet."
  (interactive
   (list (completing-read
          "Snippet: " (web-mode-snippet-names))))
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
      (when (string= (car snippet) code)
        (setq continue nil))
      (setq counter (1+ counter)))
    (when snippet
      (setq snippet (cdr snippet))
      (setq beg (point-at-bol))
      (insert (car snippet))
;;      (message "insert[1] (%S)" (point))
      (setq pos (point))
      (when sel
        (insert sel)
        (setq pos (point)))
      (if (cdr snippet) (insert (cdr snippet)))
;;      (message "insert[2] (%S)" (point))
      (setq end (point-at-eol))
      (unless sel (goto-char pos))
      (indent-region beg end))
;;      (message "indent : beg(%S) end(%S)" beg end)
    ))

(defun web-mode-insert-and-indent (text)
  "Insert and indent text."
  (interactive)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)))

;; if on est dans un active block -> prioritaire si
;; on regarde si on est dans un tag
(defun web-mode-navigate (&optional pos)
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
     ((and (get-text-property pos 'block-side)
           (web-mode-block-beginning)
           (get-text-property (point) 'block-controls))
      (web-mode-block-match))
     ((member (get-text-property pos 'tag-type) '(start end))
      (web-mode-tag-beginning)
      (web-mode-tag-match))
     (t
      (goto-char init))
     )
    ))

(defun web-mode-block-match (&optional pos)
  "Block match"
  (unless pos (setq pos (point)))

  (let (init controls (counter 1) type control (continue t) pair)
    (setq init pos)
    (goto-char pos)
    (setq controls (get-text-property pos 'block-controls))
    (cond
     (controls
      (setq pair (car controls))
      (setq type (car pair))
      (when (eq type 'inside)
        (setq type 'close))
      (setq control (cdr pair))
      (while continue
        (cond
         ((or (and (eq type 'open) (not (web-mode-block-next)))
              (and (eq type 'close) (not (web-mode-block-previous))))
;;          (message "ici%S" (point))
          (setq continue nil)
          )
         ((null (get-text-property (point) 'block-controls))
          )
         (t
          (setq controls (get-text-property (point) 'block-controls))
          (setq pair (car controls))
;;          (message "%S-%S (%S) type=%S control=%S" type control (point) (car pair) (cdr pair))
          (when (string= control (cdr pair))
            (cond
             ((eq (car pair) type)
              (setq counter (1+ counter)))
             ((eq (car pair) 'inside)
              )
             (t
              (setq counter (1- counter)))
             )
            ) ;when
          (when (= counter 0)
            (setq continue nil))
          ) ;t
         ) ;cond
        ) ;while
      (if (= counter 0) (point) nil)
      ) ;controls
     ;; ((and (get-text-property pos 'block-side)
     ;;       (web-mode-block-beginning)
     ;;       web-mode-active-block-regexp
     ;;       ;;             (progn (message "web-mode-active-block-regexp=%S" web-mode-active-block-regexp) t)
     ;;       (looking-at-p web-mode-active-block-regexp)
     ;;       web-mode-engine-control-matcher)
     ;;  (funcall web-mode-engine-control-matcher)
     ;;  (point)
     ;;  )
     (t
      (goto-char init)
      nil)
     ) ;conf
    ))

(defun web-mode-tag-match (&optional pos)
  "Fetch HTML block."
  (unless pos (setq pos (point)))
  (let (regexp)
    (setq regexp (concat "</?" (get-text-property pos 'tag-name)))
    (if (eq (get-text-property pos 'tag-type) 'end)
        (web-mode-tag-fetch-opening regexp pos)
      (web-mode-tag-fetch-closing regexp pos))
    t))

(defun web-mode-tag-fetch-opening (regexp pos)
  "Fetch opening HTML block."
  (let ((counter 1) (n 0))
    (goto-char pos)
    (while (and (> counter 0) (re-search-backward regexp nil t))
      (when (get-text-property (point) 'tag-beg)
        (setq n (1+ n))
        (if (eq (get-text-property (point) 'tag-type) 'end)
            (setq counter (1+ counter))
          (setq counter (1- counter))))
      )
    (if (= n 0) (goto-char pos))
    ))

(defun web-mode-tag-fetch-closing (regexp pos)
  "Fetch closing HTML closing block."
  (let ((counter 1) (n 0))
;;    (message "regexp=%S pos=%S" regexp pos)
    (goto-char pos)
    (web-mode-tag-end)
    (while (and (> counter 0) (re-search-forward regexp nil t))
      (when (get-text-property (match-beginning 0) 'tag-beg)
        (setq n (1+ n))
        (if (eq (get-text-property (point) 'tag-type) 'end)
            (setq counter (1- counter))
          (setq counter (1+ counter))))
      )
;;    (message "n=%S" n)
    (if (> n 0)
        (web-mode-tag-beginning)
      (goto-char pos))
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
        (setq jump (and (eq (char-before) ?\>)
                        (string= (get-text-property (1- (point)) 'tag-name) tag)))
        (if jump (setq jump (point)))
;;        (setq jump (looking-back (concat "<" tag ">")))
        ) ;save-excursion
      (if jump (goto-char jump))
      ) ;when epp
    epp
    ))

(defun web-mode-tags-pos ()
  (save-excursion
    (let (start-beg start-end end-beg end-end (pos (point)))
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (setq start-beg (web-mode-tag-beginning-position pos)
              start-end (web-mode-tag-end-position pos))
        (when (web-mode-tag-match)
          (setq end-beg (point)
                end-end (web-mode-tag-end-position (point))))
;;        (message "%S %S %S %S" start-beg start-end end-beg end-end)
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (setq end-beg (web-mode-tag-beginning-position pos)
              end-end (web-mode-tag-end-position pos))
        (when (web-mode-tag-match)
          (setq start-beg (point)
                start-end (web-mode-tag-end-position (point))))
        )
       )
      (if (and start-beg end-beg)
          (cons (cons start-beg (1+ start-end))
                (cons end-beg (1+ end-end)))
        nil)
      )))

(defun web-mode-make-tag-overlays ()
  (unless web-mode-start-tag-overlay
    (setq web-mode-start-tag-overlay (make-overlay 1 1)
          web-mode-end-tag-overlay (make-overlay 1 1))
    (overlay-put web-mode-start-tag-overlay
                 'font-lock-face
                 'web-mode-current-element-highlight-face)
    (overlay-put web-mode-end-tag-overlay
                 'font-lock-face
                 'web-mode-current-element-highlight-face)))

(defun web-mode-delete-tag-overlays ()
  (when web-mode-start-tag-overlay
    (delete-overlay web-mode-start-tag-overlay)
    (delete-overlay web-mode-end-tag-overlay)))

(defun web-mode-highlight-current-element ()
  (let ((ctx (web-mode-tags-pos)))
    (if (null ctx)
        (web-mode-delete-tag-overlays)
      (web-mode-make-tag-overlays)
      (move-overlay web-mode-start-tag-overlay (caar ctx) (cdar ctx))
      (move-overlay web-mode-end-tag-overlay (cadr ctx) (cddr ctx)))
    ))

(defun web-mode-on-after-change (beg end len)
  "Handles auto-pairing, auto-closing, and region-refresh after buffer alteration."

;;  (message "pos=%d, beg=%d, end=%d, len=%d, cur=%d" (point) beg end len (current-column))
;;  (backtrace)

  (setq web-mode-expand-initial-pos nil
        web-mode-expand-previous-state "")

  (let ((pos (point)) self-insertion chunk auto-closed auto-opened atomic-insertion)

    (setq atomic-insertion (and (= len 0)
                                (= 1 (- end beg))))

    (if (not (= (point-max) (+ (buffer-size) 1)))

       (setq web-mode-is-narrowed t)

      ;;-- auto-closing and auto-pairing

      (when (and (> web-mode-jshint-errors 0)
                 (get-text-property pos 'part-side))
;;        (message "%S %S" beg (1+ end))
        (remove-overlays beg (1+ end) 'font-lock-face 'web-mode-error-face)
        )

      (when (and (> pos 3)
                 (not (get-text-property pos 'part-side))
                 atomic-insertion)

        (setq chunk (buffer-substring-no-properties (- beg 1) end))

        ;;-- auto-opening
        (when (and (not web-mode-disable-auto-opening)
                   (string= ">\n" chunk)
                   (not (eobp))
                   (eq (get-text-property (- beg 1) 'tag-type) 'start)
                   (eq (get-text-property end 'tag-type) 'end)
                   (string= (get-text-property (- beg 1) 'tag-name)
                            (get-text-property end 'tag-name)))
          (setq auto-opened t)
          (newline-and-indent)
          ;;(newline)
          ;;(indent-for-tab-command)
          (forward-line -1)
          (indent-for-tab-command)
          )

        ;;-- auto-closing
        (when (and (> web-mode-tag-auto-close-style 0)
                   (or (and (= web-mode-tag-auto-close-style 2)
                            (string-match-p "[[:alnum:]'\"]>" chunk))
                       (string= "</" chunk))
                   (not (get-text-property (- beg 1) 'block-side)))
          (when (web-mode-element-close)
            (setq auto-closed t
                  self-insertion t))
          )

        ;;-- auto-pairing
        (when (and (not web-mode-disable-auto-pairing)
                   (not self-insertion))
          (let ((i 0) expr p after pos-end (l (length web-mode-auto-pairs)))
            (setq pos-end (if (> (+ end 32) (line-end-position))
                              (line-end-position)
                            (+ end 10)))
            (setq chunk (buffer-substring-no-properties (- beg 2) end)
                  after (buffer-substring-no-properties end pos-end))
            (while (and (< i l) (not self-insertion))
              (setq expr (elt web-mode-auto-pairs i))
              (setq i (1+ i))
;;              (message "%S" expr)
;;              (when (string= (elt expr 0) chunk)
;;              (message "expr1=%S after=%S" (or (elt expr 2) (elt expr 1)) after)
              (when (and (string= (elt expr 0) chunk)
;;                         (progn (message "%S %S" (elt expr 1) after) t)
                         (not (string-match-p (or (elt expr 2) (elt expr 1)) after)))
                (setq self-insertion t)
                (insert (elt expr 1))
                (if (not (elt expr 2))
                    (goto-char pos)
                  (setq p (point))
                  (insert (elt expr 2))
                  (goto-char p))
                ) ;when
              ) ;while
            ) ;let
          ) ;when

        ) ;end auto-pairing auto-closing

      ;;-- region-refresh
      ;;      (save-match-data
      (cond

       ;; ((and nil (web-mode-is-content beg)
       ;;       atomic-insertion
       ;;       (not self-insertion)
       ;;       (not (member (char-before) web-mode-electric-chars)))
       ;;  (message "no invalidation %c" (char-before))
       ;;  )

       ((and nil ;;github:issue163
             web-mode-has-any-large-part
             atomic-insertion
             (not (member (char-before) '(?\} ?\n)))
             (not self-insertion)
             (or (member (get-text-property beg 'part-side)
                         '(css
                           ;;javascript json
                           ))
                 (member web-mode-content-type '("css"
                                                 ;;"javascript" "json"
                                                 ))))
        (if (or (string= web-mode-content-type "css")
                (eq (get-text-property beg 'part-side) 'css))
            (web-mode-invalidate-css-region beg end)
          (web-mode-invalidate-javascript-region beg end))
        )

       ((and nil
             web-mode-has-any-large-block
             atomic-insertion
             (not self-insertion)
             (get-text-property beg 'block-side)
             (member web-mode-engine '("asp")))
        (web-mode-invalidate-asp-region beg end)
        )

       (t

;;        (message "after change : pos(%d) beg(%d) end(%d) len(%d) cur(%d)" (point) beg end len (current-column))

        ;; (message "start(%S) end(%S)"
        ;;          (or (web-mode-previous-tag-at-bol-pos beg)
        ;;              (point-min))
        ;;          (or (web-mode-next-tag-at-eol-pos end)
        ;;              (point-max)))

        (if (string= web-mode-engine "razor")
            (web-mode-scan-region (point-min)
                                  (point-max))
          (web-mode-scan-region (or (web-mode-previous-tag-at-bol-pos beg)
                                    (point-min))
                                (or (web-mode-next-tag-at-eol-pos end)
                                    (point-max))))
        )
       )
      ;;        ) ;save-match-data

      ;;-- auto-indentation
      (when (and (not web-mode-disable-auto-indentation)
                 (not auto-opened)
                 (or auto-closed
                     (and (> end (point-min))
                          (get-text-property (1- end) 'tag-end)
                          (get-text-property (line-beginning-position) 'tag-beg))))
        (indent-for-tab-command)
        )

      (when (and (string= web-mode-engine "none")
                 (< (point) 16)
                 (eq (char-after 1) ?\#)
                 (string-match-p "php" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))))
        (web-mode-set-engine "php")
        )

      ) ;if narrowed
    ))

(defun web-mode-invalidate-css-region (pos-beg pos-end)
  "Invalidate css region (only when one char has been inserted)"
  (save-excursion
    (let (pair part-beg part-end)
      (if (string= web-mode-content-type "css")
          (setq part-beg (point-min)
                part-end (point-max))
        (setq part-beg (web-mode-part-beginning-position pos-beg)
              part-end (web-mode-part-end-position pos-beg)))
      (setq pair (web-mode-css-current-rule pos-beg part-beg part-end))
;;      (message "css region : %S > %S" (car pair) (cdr pair))
;;      (web-mode-highlight-region (car pair) (cdr pair) "css")
      )))

(defun web-mode-invalidate-javascript-region (pos-beg pos-end)
  "Invalidate javascript region (only when one char has been inserted)"
  (save-excursion
    (let (beg end part-beg part-end lang is-token-char)
      (goto-char pos-beg)
      (setq is-token-char (not (null (member (char-before) '(?\" ?\' ?\/ ?\< ?\*)))))
;;      (message "%S %c(%S)" pos-beg (char-before) is-token-char)
      (if (member web-mode-content-type '("javascript" "json"))
          (setq part-beg (point-min)
                part-end (point-max)
                lang web-mode-content-type)
        (setq part-beg (web-mode-part-beginning-position pos-beg)
              part-end (web-mode-part-end-position pos-beg)
              lang (symbol-name (get-text-property pos-beg 'part-side))))
      (cond
       ((and (not is-token-char)
             (get-text-property (1- pos-beg) 'part-token))
;;        (message "nothing")
        )
       ((and (not is-token-char)
             (progn (back-to-indentation) (setq beg (point)))
             (if (>= beg part-beg) beg part-beg)
             (progn (goto-char pos-end) (end-of-line) (setq end (point)))
             (if (<= end part-end) end part-end))
        ;;            (message "%S" (buffer-substring-no-properties beg end))
        (web-mode-scan-part beg end)
        )
       ;;        (message "%S" (buffer-substring-no-properties part-beg part-end))
       (t
        (web-mode-scan-part part-beg part-end))
       )
      )))

;; todo : gestion du remove-text-properties (ne pas toucher à pas mal de properties : block-beg, part-side etc.)
(defun web-mode-invalidate-asp-region (pos-beg pos-end)
  "Invalidate asp region."
  (save-excursion
    (let (beg end part-beg part-end)
      (setq part-beg (web-mode-block-beginning-position pos-beg)
            part-end (web-mode-block-end-position pos-beg))
      (if (and part-beg part-end
               (progn (goto-char pos-beg) t)
               (not (member (char-after) '(?\" ?\' ?\/)))
               (progn (back-to-indentation) t)
               (setq beg (point))
               (if (>= beg part-beg) beg part-beg)
               (progn (goto-char pos-end) t)
               (progn (end-of-line) t)
               (setq end (point))
               (if (<= end part-end) end part-end))
            (web-mode-highlight-region beg end "asp")
        (web-mode-highlight-region part-beg part-end "asp")
        )
      )))

(defun web-mode-dom-apostrophes-replace ()
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
        ) ;while
      )))

(defun web-mode-dom-entities-encode ()
  "Replace special chars with HTML entities (e.g. é becomes &eacute;)"
  (save-excursion
    (let (regexp ms pair elt (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (setq regexp "[")
      (dolist (pair web-mode-html-entities)
        (setq regexp (concat regexp (char-to-string (cdr pair))))
        )
      (setq regexp (concat regexp "]"))
      (while (web-mode-rsf-content regexp max)
        (setq elt (match-string-no-properties 0))
        (setq elt (aref elt 0))
        (setq elt (car (rassoc elt web-mode-html-entities)))
;;        (message "%S" elt)
        (replace-match (concat "&" elt ";"))
        ) ;while
      )))

;; ½ &frac12; &#189; &#x00BD;
(defun web-mode-dom-entities-replace ()
  "Replace HTML entities e.g. entities &eacute; &#233; &#x00E9; become é"
  (interactive)
  (save-excursion
    (let (ms pair elt (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-rsf-content "&\\([#]?[[:alnum:]]\\{2,8\\}\\);" max)
        (setq elt nil)
;;        (message "E=%S" (match-string 1))
        (setq ms (match-string-no-properties 1))
        (if (eq (aref ms 0) ?\#)
            (if (eq (aref ms 1) ?x)
                (progn
                  (setq elt (substring ms 2))
                  (setq elt (downcase elt))
                  (setq elt (string-to-number elt 16))
                  (setq elt (char-to-string elt))
                  )
              (setq elt (substring ms 1))
              (setq elt (char-to-string (string-to-number elt)))
              )
          (setq pair (assoc ms web-mode-html-entities))
          ;;        (message "pos=%S name=%S pair=%S" (point) name pair)
          (if pair (setq elt (cdr pair)))
          (if elt (setq elt (char-to-string elt)))
          ) ;if
        (if elt (replace-match elt))
        ) ;while
      )))

(defun web-mode-dom-xml-replace ()
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

(defun web-mode-dom-quotes-replace ()
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
        ) ;while
      )))

(defun web-mode-dom-xpath (&optional pos)
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

(defun web-mode-block-ends-with (regexp &optional pos)
  "Check if current block ends with regexp"
  (unless pos (setq pos (point)))
  (save-excursion
    (save-match-data
      (and (web-mode-block-end)
           (progn (backward-char) t)
           (web-mode-block-skip-chars-backward)
           (progn (forward-char) t)
           (looking-back regexp))
      )))

(defun web-mode-block-starts-with (regexp &optional pos)
  "Check if current block starts with regexp"
  (unless pos (setq pos (point)))
  (save-excursion
    (and (web-mode-block-beginning)
         (web-mode-block-skip-chars-forward)
         (looking-at regexp))
    ))

(defun web-mode-block-skip-chars-backward (&optional pos)
  "skip backward"
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (or (eq (char-after) ?\s)
                   (member (get-text-property (point) 'block-token) '(delimiter comment))))
          (backward-char)
        (setq continue nil))
      ) ;while
;;    (message "pt=%S" (point))
    (point)
    ))

(defun web-mode-block-skip-chars-forward (&optional pos)
  "skip forward"
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (or (eq (char-after) ?\s)
                   (member (get-text-property (point) 'block-token) '(delimiter comment))))
          (forward-char)
        (setq continue nil))
      ) ;while
;;    (message "pt=%S" (point))
    (point)
    ))

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
      (setq block-side (and (get-text-property pos 'block-side)
                            (not (string= web-mode-engine "razor"))))
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
          ) ;unless
        )
      pt
      )))

(defun web-mode-closing-delimiter-position (delimiter &optional pos limit)
  "Fetch closing delimiter."
  (save-excursion
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (setq pos nil)
    (let ((continue t))
      (while (and continue (re-search-forward delimiter limit t))
        (setq continue nil
              pos (1- (point)))
        ) ;while
      pos)))

(defun web-mode-closing-paren (limit)
  "web-mode-closing-paren"
  (let (pos)
    (setq pos (web-mode-closing-paren-position (point) limit))
    (if (or (null pos) (> pos limit))
        nil
      (goto-char pos)
      t)
    ))

(defun web-mode-closing-paren-position (&optional pos limit)
  "Fetch closing paren."
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
      (if (null regexp)
          (setq continue nil)
        (setq block-side (and (get-text-property pos 'block-side)
                              (not (string= web-mode-engine "razor")))))
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
        ) ;while
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
           ) ;cond
          ) ;unless
        ) ;while
      ;;      (message "h=%S pt=%S" h pt)
      pt
      )))

(defun web-mode-previous-tag-at-bol-pos (pos)
  "Line beginning with an HTML tag. BOL is returned or nil."
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
          ) ;while
        ) ;if
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
        ) ;while
      pos)))

(defun web-mode-tag-match-position (&optional pos)
  "Html tag match position."
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-tag-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-tag-match-position (&optional pos)
  "Match tag position."
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-tag-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-block-match-position (&optional pos)
  "Match block position."
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-block-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-tag-beginning-position (&optional pos)
  "Beginning position of the current tag. POINT is at <."
  (unless pos (setq pos (point)))
  (let (beg)
    (cond
     ((get-text-property pos 'tag-beg)
      (setq beg pos))
     ((and (> pos 1) (get-text-property (1- pos) 'tag-beg))
      (setq beg (1- pos)))
     ((get-text-property pos 'tag-type)
      (setq beg (1- (previous-single-property-change pos 'tag-beg)))
      (when (not (get-text-property beg 'tag-beg))
        (setq beg nil)))
     (t
      (setq beg nil))
     ) ;cond
    beg))

(defun web-mode-tag-end-position (&optional pos)
  "End position of the current tag. POINT is at >."
  (unless pos (setq pos (point)))
  (let (end)
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-end)
      (setq end pos))
     ((get-text-property pos 'tag-type)
      (setq end (next-single-property-change pos 'tag-end))
      (when (not (get-text-property end 'tag-end))
        (setq end nil)))
     (t
      (setq end nil))
     ) ;cond
    end))

(defun web-mode-part-end-position (&optional pos)
  "End position of the current part."
  (unless pos (setq pos (point)))
  (cond
   ((member web-mode-content-type '("css" "javascript" "json"))
    (setq pos (point-max)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   (t
    (setq pos (next-single-property-change pos 'tag-beg))
    (if (not (get-text-property pos 'tag-beg))
        (setq pos nil)
      (setq pos (1- pos)))
    )
   ) ;cond
  pos)

(defun web-mode-part-beginning-position (&optional pos)
  "Beginning of part"
  (unless pos (setq pos (point)))
  (cond
   ((member web-mode-content-type '("css" "javascript" "json"))
    (setq pos (point-min)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   (t
    (setq pos (previous-single-property-change pos 'tag-end))
    (when (or (= pos (point-min))
              (not (get-text-property (1- pos) 'tag-end)))
      (setq pos nil)))
   ) ;cond
  pos)

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
   ) ;cond
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
   ) ;cond
  pos)

(defun web-mode-element-child-position (&optional pos)
  "Child element pos."
  (save-excursion
    (let (child close)
      (unless pos (setq pos (point)))
      (goto-char pos)
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (web-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ((eq (get-text-property pos 'tag-type) 'void)
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (web-mode-tag-beginning)
        (setq close (point))
        (web-mode-tag-match)
        )
       ((web-mode-element-parent-position pos)
        (setq pos (point))
        (web-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ) ;cond
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
            ) ;if
          ) ;when
        ) ;while
      ) ;save-excursion
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
   ) ;cond
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
   ) ;cond
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
      ;;(message "pos=%S  <%c>" pos (char-after pos))
      (if (get-text-property pos 'block-side)
          (setq pos (web-mode-block-beginning-position pos))
        (setq pos (previous-single-property-change pos 'block-side))
        (cond
         ((and pos (get-text-property pos 'block-beg))
          )
         ((and pos (> pos (point-min)))
          (setq pos (web-mode-block-beginning-position (1- pos))))
         )
        ) ;if
      ) ;when
    ) ;block-side
   (t
    (setq pos (previous-single-property-change pos 'block-side))
    (when (and pos (> pos (point-min)))
      (setq pos (web-mode-block-beginning-position (1- pos))))
    )
   ) ;conf
  pos)

(defun web-mode-block-next-position (&optional pos)
  "web-mode-block-next-position"
  (unless pos (setq pos (point)))
  ;;    (message "start=%S" pos)
  (if (get-text-property pos 'block-side)
      ;;      (if (= pos (point-min))
      ;;          (setq pos (point-min))
      (progn
        (setq pos (web-mode-block-end-position pos))
        (when (and pos (> (point-max) pos))
          (setq pos (1+ pos))
          (if (not (get-text-property pos 'block-side))
              (setq pos (next-single-property-change pos 'block-side)))
          ) ;when
        )
    ;;       )
    (setq pos (next-single-property-change pos 'block-side))
    )
;;  (message "%S" (point))
  ;;    (when (or (null pos) (<= pos))
  pos
  )

(defun web-mode-part-next-position (&optional pos)
  "web-mode-part-next-position"
  (unless pos (setq pos (point)))
  (if (get-text-property pos 'part-side)
      (if (= pos (point-min))
          (set pos (point-min))
        (setq pos (web-mode-part-end-position pos))
        (when (and pos (> (point-max) pos))
          (setq pos (1+ pos))
          (if (not (get-text-property pos 'part-side))
              (setq pos (next-single-property-change pos 'part-side)))
          ) ;when
        )
    (setq pos (next-single-property-change pos 'part-side)))
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

(defun web-mode-tag-previous (&optional pos)
  "Fetch previous tag."
  (interactive)
  (unless pos (setq pos (point)))
  (if (bobp)
      (setq pos nil)
    (when (get-text-property pos 'tag-beg)
      (setq pos (1- pos)))
    (setq pos (previous-single-property-change pos 'tag-beg))
    (when pos
      (setq pos (1- pos))
      (goto-char pos))
    )
  pos)

(defun web-mode-tag-next (&optional pos)
  "Fetch next tag. Might be HTML comment or server tag (ie. JSP)."
  (interactive)
  (unless pos (setq pos (point)))
  (if (eobp)
      (setq pos nil)
    (when (get-text-property pos 'tag-beg)
      (setq pos (1+ pos)))
    (setq pos (next-single-property-change pos 'tag-beg))
    (when pos (goto-char pos)))
  pos)

(defun web-mode-skip-html-tag (&optional back bound context)
  "Skip html tag. (smartparens helper)"
  (interactive)
  (let ((pos (point)) mb me skipped back delim)

    (cond
     (back
      (unless (or (bobp)
                  (get-text-property (1- pos) 'tag-end))
        (when (web-mode-tag-previous)
          (web-mode-tag-end)
          )
        ) ;unless
      )
     (t
      (unless (or (eobp)
                  (get-text-property pos 'tag-beg))
        (web-mode-tag-next))
      )
     ) ;cond

    (cond
     ((get-text-property (point) 'tag-beg)
      (setq mb (point)
            me (1+ (web-mode-tag-end-position)))
      )
     ((and (not (bobp))
           (get-text-property (1- (point)) 'tag-end))
      (setq mb (point)
            me (web-mode-tag-beginning-position (1- (point))))
      )
     ) ;cond

    (if (and mb me)
        (progn
          (setq skipped (- (point) pos))
          (setq delim (buffer-substring-no-properties mb me))
          ;;    (message "%S" (list :mb mb :me me :skipped skipped :back back :delim delim))
          (list :mb mb :me me :skipped skipped :back back :delim delim)
          )
      nil)

    ))

(defun web-mode-get-html-tag (&optional back bound context)
  "Get html tag. (smartparens helper)"
  (interactive)
  (let (ctx beg end (pos (point)))
    (message "pos=%S" pos)
    (setq ctx (web-mode-skip-html-tag back bound context))
    (message "ctx=%S" ctx)
    (cond
     ((null ctx)
;;      (message "ici")
      )
     ((get-text-property (point) 'tag-beg)
      (setq beg (point)
            end (1+ (web-mode-tag-end-position)))
      )
     ((get-text-property (1- (point)) 'tag-end)
      (setq beg (web-mode-tag-end-position (1- (point)))
            end (point))
      )
     )
    (if (null ctx) nil
;;      (message "%S" (list :beg beg :end end :op "<" :cl ">" :prefix "" :suffix "" :from pos))
      (list :beg beg :end end :op "<" :cl ">" :prefix "" :suffix "" :from pos))
    ))

(defun web-mode-tag-get (&optional pos)
  "Tag get"
  (unless pos (setq pos (point)))
  (let (out)
    (cond
     ((get-text-property pos 'tag-name)
      (setq out (buffer-substring-no-properties (web-mode-tag-beginning-position)
                                                (web-mode-tag-end-position)))
      )
     (t
      (setq out "")
      )
     ) ;cond
    out
    ))

(defun web-mode-attr-next (&optional pos)
  "Fetch next attr."
  (interactive)
  (let ((continue t))
    (unless pos (setq pos (point)))
    (while continue
      (setq pos (next-single-property-change pos 'tag-attr))
;;      (message "pos=%S" pos)
      (cond
       ((null pos)
        (setq continue nil
              pos nil))
       ((get-text-property pos 'tag-attr)
        (setq continue nil)
        )
       )
      ) ;while
    (when pos (goto-char pos))
    pos))

(defun web-mode-element-previous ()
  "Fetch previous element."
  (interactive)
  (let (continue ret (pos (point)) (props '(start void)))
    (setq continue (not (bobp)))
    (while continue
      (setq ret (web-mode-tag-previous))
      (when (or (null ret)
                (member (get-text-property (point) 'tag-type) props))
        (setq continue nil)
        )
      ) ;while
    (unless ret (goto-char pos))
    ret))

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
      ) ;while
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
                   (web-mode-tag-match)
                   (web-mode-element-next))
          (setq ret (point))
          )
        )
       ((eq (get-text-property pos 'tag-type) 'start)
        (when (and (web-mode-tag-match)
                   (web-mode-element-next))
          (setq ret (point))
          )
        )
       (t
        (when (web-mode-element-next)
          (setq ret (point))
          )
        )
       ) ;cond

      ) ;save
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

(defun web-mode-dom-traverse ()
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
   ) ;cond
  )

(defun web-mode-block-close (&optional pos)
  "Close the first opened control block."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((continue t) ctx h ctrl n closing-block)
    (save-excursion
      (setq h (make-hash-table :test 'equal))
      (while (and continue (web-mode-block-previous))
        (when (setq ctx (web-mode-block-is-control (point)))
          (setq ctrl (car ctx))
          (setq n (gethash ctrl h 0))
          (if (cdr ctx)
              (puthash ctrl (1+ n) h)
            (puthash ctrl (1- n) h)
            )
          (when (> (gethash ctrl h) 0)
            (setq continue nil))
;;          (if ctx (message "(%S) %S : %S" (point) ctrl (gethash ctrl h)))
          )
        ) ;while
      ) ;save-excursion
    (when (and (null continue)
               (setq closing-block (web-mode-closing-block ctrl)))
      (insert closing-block)
      (indent-for-tab-command)
      )
    ))

(defun web-mode-closing-block (type)
  "Return the closing block corresponding to TYPE"
  (cond
   ((string= web-mode-engine "django")
    (concat "{% end" type " %}"))
   ((string= web-mode-engine "ctemplate")
    (concat "{{/" type "}}"))
   ((string= web-mode-engine "blade")
    (concat "@end" type))
   ((string= web-mode-engine "dust")
    (concat "{/" type "}"))
   ((string= web-mode-engine "underscore")
    "<% } %>")
   ((string= web-mode-engine "erb")
    "<% end %>")
   (t
    (cdr (assoc type web-mode-closing-blocks)))
   ) ;cond
  )

(defun web-mode-block-previous (&optional pos)
  "Move point to the beginning of the previous block."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-previous-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-block-next (&optional pos)
  "Move point to the beginning of the next block."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-next-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-part-next (&optional pos)
  "Move point to the beginning of the next part."
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-part-next-position pos))
  (when pos (goto-char pos))
  pos)

(defun web-mode-block-beginning (&optional pos)
  "Move point to the beginning of the current block."
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
        ) ;t
       ) ;cond
      ) ;while
    ret))

(defun web-mode-sb-client (regexp &optional limit noerror)
  "search-backward in client."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward regexp limit noerror))
      (if (or (null ret)
              (not (get-text-property (point) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun web-mode-rsb-client (regexp &optional limit noerror)
  "re-search-backward outside blocks."
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
  "re-search-forward outside blocks."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (get-text-property (match-beginning 0) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun web-mode-sf-client (expr &optional limit noerror)
  "search-forward outside blocks."
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
            end (if (null ret) (point) (1- (match-end 0))))
;;      (message "pt=%S" pos)
      (if (or (null ret)
              (and (web-mode-is-content beg)
                   (web-mode-is-content end)))
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
          (when (not (eq ?\s (following-char)))
            (setq continue nil
                  counter 0))
          ) ;if
        (forward-char)
        ) ;while
      (> counter 0)
      )))

(defun web-mode-is-part-token-or-server (&optional pos)
  "Detect if POS is in a comment, a string or in server script."
  (unless pos (setq pos (point)))
  (not (null (or (get-text-property pos 'block-side)
                 (get-text-property pos 'part-token))))
  )

(defun web-mode-is-part-token-line ()
  "Detect if current line has only client tokens (string/comment) or server blocks."
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-part-token-or-server)
            (setq counter (1+ counter))
          (when (not (eq ?\s (following-char)))
            (setq continue nil
                  counter 0))
          ) ;if
        (forward-char)
        ) ;while
      (> counter 0)
      )))

(defun web-mode-is-content (&optional pos)
  "Is pos in a html text."
  (unless pos (setq pos (point)))
  (not (or (get-text-property pos 'part-side)
           (get-text-property pos 'tag-type)
           (get-text-property pos 'block-side)
           )))

(defun web-mode-is-comment-or-string (&optional pos)
  "Detect if point is in a comment or in a string."
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (member (get-text-property pos 'block-token) '(comment string))
                 (get-text-property pos 'part-token))))
  )

(defun web-mode-is-comment (&optional pos)
  "Detect if point is in a comment."
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property pos 'part-token) 'comment))))
  )

;;--- end search

(defun web-mode-on-exit ()
  "Exit web-mode."
  (interactive)
  (web-mode-with-silent-modifications
   (put-text-property (point-min) (point-max) 'invisible nil)
   (remove-overlays)
   (remove-hook 'change-major-mode-hook 'web-mode-on-exit t)
   ))

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (web-mode-with-silent-modifications
   (setq web-mode-time nil)
   (put-text-property (point-min) (point-max) 'invisible nil)
   (remove-overlays)
   (unload-feature 'web-mode)
;;   (setq web-mode-disable-css-colorization t)
   (web-mode)
   (if (fboundp 'web-mode-hook)
       (web-mode-hook))))

(defun web-mode-trace (msg)
  "Benchmark."
  (interactive)
  (let (sub trace)
    (setq trace nil)
    (when trace
      (when (null web-mode-time) (setq web-mode-time (current-time)))
      (setq sub (time-subtract (current-time) web-mode-time))
      (message "%18s: time elapsed = %Ss %9Sµs" msg (nth 1 sub) (nth 2 sub))
      )))

(defun web-mode-debug ()
  "Display informations useful for debuging"
  (interactive)
  (let (modes)
    (message "\n")
    (message "--- WEB-MODE DEBUG BEG ---")
    (message "versions: emacs(%S.%S) web-mode(%S)"
             emacs-major-version emacs-minor-version web-mode-version)
    (message "vars: engine(%S) content-type(%S) file(%S)"
             web-mode-engine
             web-mode-content-type
             (or (buffer-file-name) (buffer-name)))
    (message "system: window(%S) config(%S)" window-system system-configuration)
    (message "colors: fg(%S) bg(%S) "
             (cdr (assoc 'foreground-color default-frame-alist))
             (cdr (assoc 'background-color default-frame-alist)))
    (message "modes: whitespace-mode(%S) global-whitespace-mode(%S) rainbow-mode(%S) idle-highlight-mode(%S) fic-mode(%S)"
             (if (boundp 'whitespace-mode) whitespace-mode nil)
             (if (boundp 'global-whitespace-mode) global-whitespace-mode nil)
             (if (boundp 'rainbow-mode) rainbow-mode nil)
             (if (boundp 'idle-highlight-mode) idle-highlight-mode nil)
             (if (boundp 'fic-mode) fic-mode nil)
             )
    (mapc (lambda (mode)
            (condition-case nil
                (if (and (symbolp mode) (symbol-value mode))
                    (add-to-list 'modes mode))
              (error nil))
            ) ;lambda
          minor-mode-list)
    (message "%S" modes)
    (message "--- WEB-MODE DEBUG END ---")
    (switch-to-buffer "*Messages*")
    (goto-char (point-max))
    (recenter)
  ))

;;; web-mode.el ends here

;; (defun web-mode-match-php-block ()
;;   "Fetch PHP block."
;;   (let (regexp match)
;;     (cond

;;      ((looking-at-p "<\\?\\(php\\)?[ ]*}")
;;       (let (open)
;;         (search-forward "}")
;;         (backward-char)
;;         (setq open (web-mode-opening-paren-position))
;;         (when open
;;           (goto-char open)
;;           (web-mode-block-beginning)
;;           )
;;         ))

;;      ((looking-at-p "<\\?php.+{[ ]*\\?>")
;;       (let (close)
;;         (web-mode-block-end)
;;         (search-backward "{")
;;         (setq close (web-mode-closing-paren-position))
;;         (when close
;;           (goto-char close)
;;           (web-mode-block-beginning)
;;           )
;;         ))

;;      (t
;;       (looking-at web-mode-active-block-regexp)
;;       (setq match (match-string-no-properties 3))
;;       (setq regexp (concat "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?\\("
;;                            (if (member match '("else" "elseif")) "if" match)
;;                            "\\)"))
;;       (if (or (string= "end" (match-string-no-properties 2))
;;               (member match '("else" "elseif")))
;;           (web-mode-fetch-opening-php-block regexp)
;;         (web-mode-fetch-closing-php-block regexp))
;;       ) ;t

;;      ) ;cond
;;     t))

;; (defun web-mode-fetch-opening-php-block (regexp)
;;   "Fetch PHP opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (re-search-backward regexp nil t))
;;       (when (web-mode-block-is-control (point))
;;         (if (string= "end" (match-string-no-properties 2))
;;             (setq counter (1+ counter))
;;           (setq counter (1- counter)))
;;         ) ;when
;;       )
;;     ))

;; (defun web-mode-fetch-closing-php-block (regexp)
;;   "Fetch PHP closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (re-search-forward regexp nil t))
;;       (when (web-mode-block-is-control (point))
;;         (if (string= "end" (match-string-no-properties 2))
;;             (setq counter (1- counter))
;;           (setq counter (1+ counter))
;;           ) ;if
;;         ) ;when
;;       ) ;while
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-django-block ()
;;   "Fetch django block."
;;   (let (match regexp)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 2))
;;     (setq regexp (concat "{%[-]?[ ]+\\(end\\)?\\("
;;                          (cond
;;                           ((member match '("else" "elseif" "elsif" "elif")) "if")
;;                           ((member match '("empty")) "for")
;;                           (t match))
;;                          "\\)"))
;; ;;    (message "%S" regexp)
;;     (if (or (string= "end" (match-string-no-properties 1))
;;             (member match '("else" "elseif" "elsif" "elif" "empty")))
;;         (web-mode-fetch-opening-django-block regexp)
;;       (web-mode-fetch-closing-django-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-django-block (regexp)
;;   "Fetch django opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (string= "end" (match-string-no-properties 1))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-django-block (regexp)
;;   "Fetch django closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (string= "end" (match-string-no-properties 1))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))


;; (defun web-mode-match-smarty-block ()
;;   "Fetch smarty block."
;;   (let (match regexp)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 1))
;;     (setq regexp (concat (web-mode-engine-delimiter-open web-mode-engine "{") "/?" (if (string= match "else") "if" match)))
;;     (if (or (eq ?\/ (aref (match-string-no-properties 0) (length (web-mode-engine-delimiter-open web-mode-engine "{"))))
;;             (string= match "else"))
;;         (web-mode-fetch-opening-smarty-block regexp)
;;       (web-mode-fetch-closing-smarty-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-smarty-block (regexp)
;;   "Fetch smarty opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) (length (web-mode-engine-delimiter-open web-mode-engine "{"))))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-smarty-block (regexp)
;;   "Fetch smarty closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) (length (web-mode-engine-delimiter-open web-mode-engine "{"))))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-web2py-block ()
;;   "Fetch web2py block."
;;   (let (regexp match)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 1))
;;     ;;    (message "match=%S" match)
;;     (cond
;;      ((member match '("def" "return"))
;;       (setq regexp "{{[ ]*\\(def\\|return\\)"))
;;      ((member match '("block" "end"))
;;       (setq regexp "{{[ ]*\\(block\\|end\\)"))
;;      (t
;;       (setq regexp web-mode-active-block-regexp))
;;      )
;;     (if (member match '("elif" "else" "except" "finally" "pass" "end" "return"))
;;         (web-mode-fetch-opening-web2py-block regexp)
;;       (web-mode-fetch-closing-web2py-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-web2py-block (regexp)
;;   "Fetch web2py opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((member match '("else" "elif"))
;;         )
;;        ((not (member match '("pass" "end" "return")))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-web2py-block (regexp)
;;   "Fetch web2py closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((member match '("else" "elif"))
;;         )
;;        ((not (member match '("pass" "end" "return")))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-dust-block ()
;;   "Fetch dust block."
;;   (let (match regexp (continue t))
;;     (looking-at web-mode-active-block-regexp)
;;     (cond
;;      ((string= (match-string-no-properties 0) "{:else")
;;       (while continue
;;         (if (web-mode-block-previous)
;;             (when (cdr (web-mode-block-is-control (point)))
;;               (setq continue nil))
;;           (setq continue nil)
;;           )
;;         ) ;while
;;       )
;;      (t
;;       (setq match (match-string-no-properties 1))
;;       (setq regexp (concat "{[#/:?@><+^][#/:?@><+^]?" match))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (web-mode-fetch-opening-dust-block regexp)
;;         (web-mode-fetch-closing-dust-block regexp)))
;;      ) ;cond
;;     t))

;; (defun web-mode-fetch-opening-dust-block (regexp)
;;   "Fetch dust opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-dust-block (regexp)
;;   "Fetch dust closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))


;; (defun web-mode-match-erb-block ()
;;   "Fetch ERB block."
;;   (let (regexp chunk)
;;     (setq chunk (buffer-substring-no-properties (+ (point) 3)
;;                                                 (- (web-mode-block-end-position) 2)))
;;     (setq regexp web-mode-active-block-regexp)
;;     (if (string-match-p "else\\|end" chunk)
;;         (web-mode-fetch-opening-erb-block regexp)
;;       (web-mode-fetch-closing-erb-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-erb-block (regexp)
;;   "Fetch erb opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "else" match)
;;         )
;;        ((not (string= "end" match))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-erb-block (regexp)
;;   "Fetch erb closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "else" match)
;;         )
;;        ((not (string= "end" match))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-underscore-block ()
;;   "Fetch underscore block."
;;   (let (open)
;;     (cond
;;      ((looking-at-p "<%[ ]*}")
;;       (search-forward "}")
;;       (backward-char)
;;       (setq open (web-mode-opening-paren-position))
;;       (when open
;;         (goto-char open)
;;         (web-mode-block-beginning)
;;         )
;;       )
;;      ((web-mode-block-ends-with "{")
;;       (web-mode-block-end)
;;       (search-backward "{")
;;       (setq open (web-mode-closing-paren-position))
;;       (when open
;;         (goto-char open)
;;         (web-mode-block-beginning)
;;         )
;;       )
;;      ) ;cond
;;     ))

;; (defun web-mode-match-mako-block ()
;;   "Fetch mako active block."
;;   (let (regexp match)
;;     (looking-at web-mode-active-block-regexp)
;; ;;    (message "regexp=%S" web-mode-active-block-regexp)
;; ;;    (message "%S %S" (match-string-no-properties 0) (match-string-no-properties 1))
;;     (cond
;;      ((match-string-no-properties 1)
;;       (setq match (match-string-no-properties 1))
;;       (setq regexp (concat "</?%" match))
;;       (if (eq (aref (match-string-no-properties 0) 1) ?\/)
;;           (web-mode-fetch-opening-mako-block regexp)
;;         (web-mode-fetch-closing-mako-block regexp))
;;       )
;;      (t
;;       (setq match (match-string-no-properties 3))
;;       (setq regexp (if (member match '("elif" "else")) "if" match))
;;       (setq regexp (concat "%[ ]+\\(end\\)?\\(" regexp "\\)"))
;; ;;      (message "regexp=%S" regexp)
;;       (if (or (match-string-no-properties 2)
;;               (member match '("elif" "else")))
;;           (web-mode-fetch-opening-mako-block regexp)
;;         (web-mode-fetch-closing-mako-block regexp))
;;       )
;;      ) ;cond
;;     t))

;; (defun web-mode-fetch-opening-mako-block (regexp)
;;   "Fetch mako opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 0))
;;       (cond
;;        ((or (eq (aref match 1) ?\/)
;;             (match-string-no-properties 1))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-mako-block (regexp)
;;   "Fetch mako closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 0))
;;       (cond
;;        ((or (eq (aref match 1) ?\/)
;;             (match-string-no-properties 1))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        ) ;cond
;;       ) ;while
;;     (backward-char)
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-mason-block ()
;;   "Fetch mason block."
;;   (let (match regexp)
;;     (looking-at web-mode-active-block-regexp)

;;     (setq match (match-string-no-properties 1))
;;     (setq regexp (concat "</?%" match))
;;     (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;         (web-mode-fetch-opening-mason-block regexp)
;;       (web-mode-fetch-closing-mason-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-mason-block (regexp)
;;   "Fetch mason opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-mason-block (regexp)
;;   "Fetch mason closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-ctemplate-block ()
;;   "Fetch ctemplate block."
;;   (let (regexp)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq regexp (concat "{{[#^/]" (match-string-no-properties 1)))
;;     (if (looking-at-p "{{/")
;;         (web-mode-fetch-opening-ctemplate-block regexp)
;;       (web-mode-fetch-closing-ctemplate-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-ctemplate-block (regexp)
;;   "Fetch ctemplate opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 2))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-ctemplate-block (regexp)
;;   "Fetch ctemplate closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 2))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-blade-block ()
;;   "Fetch blade block."
;;   (let (beg end match regexp)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 2))
;;     (setq regexp (cond
;;                   ((member match '("else" "elseif")) "@\\(end\\)?\\(if\\)")
;;                   ((string= match "stop") "@\\(section\\|stop\\)")
;;                   ((string= match "section") "@\\(endsection\\|stop\\|section\\)")
;;                   (t (concat "@\\(end\\)?\\(" match "\\)"))))
;;     (if (or (string= "end" (match-string-no-properties 1))
;;             (member match '("else" "elseif" "stop")))
;;         (web-mode-fetch-opening-blade-block regexp)
;;       (web-mode-fetch-closing-blade-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-blade-block (regexp)
;;   "Fetch blade opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (member (match-string-no-properties 1) '("end" "endsection" "stop"))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-blade-block (regexp)
;;   "Fetch blade closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (member (match-string-no-properties 1) '("end" "endsection" "stop"))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (goto-char (match-beginning 0))
;;     ))

;; (defun web-mode-match-closure-block ()
;;   "Fetch closure block."
;;   (let (match regexp (continue t))
;;     (looking-at web-mode-active-block-regexp)
;;     (cond
;;      ((member (match-string-no-properties 0) '("{else" "{elseif"))
;;       (while continue
;;         (if (web-mode-block-previous)
;;             (when (looking-at-p "{if")
;;               (setq continue nil))
;;           (setq continue nil)
;;           )
;;         ) ;while
;;       )
;;      ((member (match-string-no-properties 0) '("{ifempty"))
;;       (while continue
;;         (if (web-mode-block-previous)
;;             (when (looking-at-p "{foreach")
;;               (setq continue nil))
;;           (setq continue nil)
;;           )
;;         ) ;while
;;       )
;;      ((member (match-string-no-properties 0) '("{case" "{default"))
;;       (while continue
;;         (if (web-mode-block-previous)
;;             (when (looking-at-p "{switch")
;;               (setq continue nil))
;;           (setq continue nil)
;;           )
;;         ) ;while
;;       )
;;      (t
;;       (setq match (match-string-no-properties 1))
;;       (setq regexp (concat "{/?" match))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (web-mode-fetch-opening-closure-block regexp)
;;         (web-mode-fetch-closing-closure-block regexp)))
;;      ) ;cond
;;     t))

;; (defun web-mode-fetch-opening-closure-block (regexp)
;;   "Fetch closure opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1+ counter))
;;         (setq counter (1- counter)))
;;       )
;;     ))

;; (defun web-mode-fetch-closing-closure-block (regexp)
;;   "Fetch closure closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (setq counter (1- counter))
;;         (setq counter (1+ counter)))
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-go-block ()
;;   "Fetch go block."
;;   (let (regexp match)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 1))
;;     (setq regexp web-mode-active-block-regexp)
;;     (if (member match '("end" "else"))
;;         (web-mode-fetch-opening-go-block regexp)
;;       (web-mode-fetch-closing-go-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-go-block (regexp)
;;   "Fetch go opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "end" match)
;;         (setq counter (1+ counter)))
;;        ((string= "else" match)
;;         )
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-go-block (regexp)
;;   "Fetch go closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "end" match)
;;         (setq counter (1- counter))
;;         )
;;        ((string= "else" match)
;;         )
;;        (t
;;         (setq counter (1+ counter))
;;         )
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-template-toolkit-block ()
;;   "Fetch TEMPLATE-TOOLKIT block."
;;   (let (regexp chunk)
;;     (setq chunk (buffer-substring-no-properties (+ (point) 3)
;;                                                 (- (web-mode-block-end-position) 2)))
;;     (setq regexp web-mode-active-block-regexp)
;;     (if (string-match-p "else\\|end\\|END" chunk)
;;         (web-mode-fetch-opening-template-toolkit-block regexp)
;;       (web-mode-fetch-closing-template-toolkit-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-template-toolkit-block (regexp)
;;   "Fetch template-toolkit opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((member match '("else" "ELSE" "elsif" "ELSIF"))
;;         )
;;        ((not (member match '("end" "END")))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-template-toolkit-block (regexp)
;;   "Fetch template-toolkit closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((member match '("else" "ELSE" "elsif" "ELSIF"))
;;         )
;;        ((not (member match '("end" "END")))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-velocity-block ()
;;   "Fetch velocity block."
;;   (let (regexp match)
;;     (looking-at web-mode-active-block-regexp)
;;     (setq match (match-string-no-properties 1))
;;     (setq regexp web-mode-active-block-regexp)
;;     (if (member match '("else" "elseif" "end"))
;;         (web-mode-fetch-opening-velocity-block regexp)
;;       (web-mode-fetch-closing-velocity-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-velocity-block (regexp)
;;   "Fetch velocity opening block."
;;   (let ((counter 1) match)
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "end" match)
;;         (setq counter (1+ counter)))
;;        ((string= "else" match)
;;         )
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-velocity-block (regexp)
;;   "Fetch velocity closing block."
;;   (let ((counter 1) match)
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (setq match (match-string-no-properties 1))
;;       (cond
;;        ((string= "end" match)
;;         (setq counter (1- counter)))
;;        ((string= "else" match)
;;         )
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     (goto-char (match-beginning 0))
;;     ))

;; (defun web-mode-match-jsp-block ()
;;   "Fetch jsp block."
;;   (let (regexp)
;;     (cond
;;      ((looking-at-p "<% }")
;;       (let (open)
;;         (search-forward "}")
;;         (backward-char)
;;         (setq open (web-mode-opening-paren-position))
;;         (when open
;;           (goto-char open)
;;           (web-mode-block-beginning))
;;         ))
;;      (t
;;       (looking-at web-mode-active-block-regexp)
;;       (setq regexp (concat "<\\(/?" (match-string-no-properties 1) "\\)\\>"))
;;       (if (eq ?\/ (aref (match-string-no-properties 0) 1))
;;           (web-mode-fetch-opening-jsp-block regexp)
;;         (web-mode-fetch-closing-jsp-block regexp)))
;;      )
;;     t))

;; (defun web-mode-fetch-opening-jsp-block (regexp)
;;   "Fetch jsp opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (cond
;;        ((eq ?\/ (aref (match-string-no-properties 1) 0))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-jsp-block (regexp)
;;   "Fetch jsp closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (cond
;;        ((eq ?\/ (aref (match-string-no-properties 1) 0))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-freemarker-block ()
;;   "Fetch freemarker block."
;;   (let (regexp match tag char)
;;     (looking-at "[<[]/?\\([[:alpha:]]+:[[:alpha:]]+\\|[@#][[:alpha:]._]+\\)")
;;     (setq match (match-string-no-properties 0)
;;           tag (match-string-no-properties 1)
;;           char (if (string= (substring (match-string-no-properties 0) 0 1) "<") "<" "\\["))

;;     (cond
;;      ((member tag '("#else" "#elseif"))
;;       (setq match (concat char "/#if")
;;             tag "#if")
;;       )
;;      ((string= tag "#break")
;;       (setq match (concat char "/#case")
;;             tag "#case"))
;;      ) ;cond
;;     (setq regexp (concat char "\\(/?" tag "\\)\\>"))
;; ;;    (message "tag=%S regexp=%S" tag regexp)
;;     (if (eq ?\/ (aref match 1))
;;         (web-mode-fetch-opening-freemarker-block regexp)
;;       (web-mode-fetch-closing-freemarker-block regexp))
;;     t))

;; (defun web-mode-fetch-opening-freemarker-block (regexp)
;;   "Fetch freemarker opening block."
;;   (let ((counter 1))
;;     (while (and (> counter 0) (web-mode-rsb regexp nil t))
;;       (cond
;;        ((eq ?\/ (aref (match-string-no-properties 1) 0))
;;         (setq counter (1+ counter)))
;;        (t
;;         (setq counter (1- counter)))
;;        )
;;       )
;;     ))

;; (defun web-mode-fetch-closing-freemarker-block (regexp)
;;   "Fetch freemarker closing block."
;;   (let ((counter 1))
;;     (web-mode-block-end)
;;     (while (and (> counter 0) (web-mode-rsf regexp nil t))
;;       (cond
;;        ((eq ?\/ (aref (match-string-no-properties 1) 0))
;;         (setq counter (1- counter)))
;;        (t
;;         (setq counter (1+ counter)))
;;        )
;;       )
;;     (web-mode-block-beginning)
;;     ))

;; (defun web-mode-match-razor-block ()
;;   "Fetch razor block."
;;   (let (regexp pos)
;;     (cond
;;      ((looking-at-p "}")
;;       (setq pos (web-mode-opening-paren-position))
;;       (when pos
;;         (goto-char pos))
;;       (web-mode-block-beginning)
;;       )
;;      ((looking-at-p ".*{[ ]*$")
;;       (end-of-line)
;;       (search-backward "{")
;;       (setq pos (web-mode-closing-paren-position))
;;       (when pos
;;         (goto-char pos))
;;       )
;;      )
;;     t))

;; (defun web-mode-block-is-control (pos)
;;   "web-mode-block-is-control"
;;   (save-excursion
;;     (let (ctrl state controls pair)
;;       (goto-char pos)
;; ;;      (message "pos=%S" pos)

;;       (if t ;;(member web-mode-engine '("php" "django" "smarty"))
;;           (progn
;;             (setq controls (web-mode-block-controls pos))
;;             (setq pair (car controls))
;;             (cond
;;              ((eq (car pair) 'inside)
;;               (setq ctrl nil))
;;              ((eq (car pair) 'open)
;;               (setq state t
;;                     ctrl (cdr pair)))
;;              ((eq (car pair) 'close)
;;               (setq state nil
;;                     ctrl (cdr pair)))
;;              ) ;cond
;;             ) ;progn

;;         (when (looking-at web-mode-active-block-regexp)

;;           (cond



;;            ((string= web-mode-engine "php")
;;             ;;          (when (or (web-mode-block-starts-with "<\\?\\(php\\)?[ ]*\\(}\\|end\\)")
;;             (when (or (web-mode-block-starts-with "\\(}\\|end\\)")
;;                       (web-mode-block-ends-with "[{:]"))
;;               (setq ctrl (match-string-no-properties 3))
;;               (if (member ctrl '("else" "elseif"))
;;                   (setq ctrl nil)
;;                 (setq state (not (string= "end" (match-string-no-properties 2))))
;;                 )
;;               ); when
;;           ;;          (message "(%S) ctrl=%S state=%S" (point) ctrl state)
;;           )

;;          ((string= web-mode-engine "django")
;;           (setq ctrl (match-string-no-properties 2))
;;           (if (member ctrl '("else" "elseif" "elsif" "elif" "empty"))
;;               (setq ctrl nil)
;;             (setq state (not (string= "end" (match-string-no-properties 1))))
;;             )
;;           )

;;          ((string= web-mode-engine "web2py")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else" "elif" "except" "finally"))
;;               (setq ctrl nil)
;;             (setq state (not (member ctrl '("end" "pass" "return"))))
;;             )
;;           )

;;          ((string= web-mode-engine "mason")
;;           (setq ctrl (match-string-no-properties 1))
;;           (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 1))))
;;           )

;;          ((string= web-mode-engine "smarty")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else" "elseif"))
;;               (setq ctrl nil)
;;             (setq state (not (eq ?\/ (aref (match-string-no-properties 0)
;;                                            (length (web-mode-engine-delimiter-open web-mode-engine "{"))))))
;;             )
;;           )

;;          ((string= web-mode-engine "dust")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (or (member ctrl '("else"))
;;                   (eq ?\/ (char-after (1- (web-mode-block-end-position)))))
;;               (setq ctrl nil)
;;             (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 1))))
;;             )
;;           )

;;          ((string= web-mode-engine "closure")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (or (member ctrl '("else" "elseif" "case" "default"))
;;                   (eq ?\/ (char-after (1- (web-mode-block-end-position)))))
;;               (setq ctrl nil)
;;             (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 1))))
;;             )
;;           )

;;          ((string= web-mode-engine "ctemplate")
;;           (setq ctrl (match-string-no-properties 1))
;;           (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 2))))
;;           )

;;          ((string= web-mode-engine "mako")
;;           (cond
;;            ((eq (char-after pos) ?\<)
;;             (if (eq ?\/ (char-after (1- (web-mode-block-end-position))))
;;                 (setq ctrl nil)
;;               (setq ctrl (match-string-no-properties 1))
;;               (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 2))))))
;;            (t
;;             (if (member (match-string-no-properties 3) '("elif" "else"))
;;                 (setq ctrl nil)
;;               (setq ctrl (match-string-no-properties 3))
;;               (setq state (null (match-string-no-properties 2))))
;;             ;;              (message "%S ctrl=%S state=%S" pos ctrl state)
;;             ;;            (message "%S - %S" (match-string-no-properties 2) (match-string-no-properties 3))
;;             ) ;t
;;            ) ;cond
;;           )

;;          ((string= web-mode-engine "velocity")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else" "elseif"))
;;               (setq ctrl nil)
;;             (setq state (not (string= "end" (match-string-no-properties 1))))
;;             )
;;           )

;;          ((string= web-mode-engine "blade")
;;           (setq ctrl (match-string-no-properties 2))
;;           (cond
;;            ((string= ctrl "stop")
;;             (setq ctrl "section"))
;;            ((member ctrl '("else" "elseif"))
;;             (setq ctrl nil))
;;            (t
;;             (setq state (not (string= "end" (match-string-no-properties 1)))))
;;            )
;;           )

;;          ((string= web-mode-engine "go")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else"))
;;               (setq ctrl nil)
;;             (setq state (not (string= "end" ctrl)))
;;             )
;;           )

;;          ((string= web-mode-engine "erb")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else"))
;;               (setq ctrl nil)
;;             (setq state (not (string= "end" ctrl)))
;;             )
;;           )

;;          ((string= web-mode-engine "template-toolkit")
;;           (setq ctrl (match-string-no-properties 1))
;;           (if (member ctrl '("else"))
;;               (setq ctrl nil)
;;             (setq state (not (string= "end" ctrl)))
;;             )
;;           )

;;          ((string= web-mode-engine "jsp")
;;           (cond
;;            ((eq (aref (match-string-no-properties 0) 1) ?\%)
;;             (setq ctrl (match-string-no-properties 2)
;;                   state t)
;;             )
;;            (t
;;             (setq ctrl (match-string-no-properties 1))
;;             (if (or (member ctrl '("h:inputtext" "jsp:usebean" "jsp:forward" "struts:property"))
;;                     (eq ?\/ (char-after (1- (web-mode-block-end-position)))))
;;                 (setq ctrl nil)
;;               (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 1))))
;;               )
;;             )
;;            )
;;           )

;;          ((string= web-mode-engine "freemarker")
;;           (if (or (member (aref (match-string-no-properties 0) 1) '(?\@ ?\#))
;;                   (member (aref (match-string-no-properties 0) 2) '(?\@ ?\#)))
;;               (setq ctrl (match-string-no-properties 2))
;;             (setq ctrl (match-string-no-properties 1))
;;             )
;; ;;          (message "ctrl=%S" ctrl)
;;           (if (or (member ctrl '("include" "setting" "import" "global" "ftl"
;;                                  "nested" "return" "local" "flush" "break" "recover"))
;;                   (eq ?\/ (char-after (1- (web-mode-block-end-position)))))
;;               (setq ctrl nil)
;;             (setq state (not (eq ?\/ (aref (match-string-no-properties 0) 1))))
;;             )
;;           )

;;          ((string= web-mode-engine "underscore")
;;           (cond
;;            ((web-mode-block-ends-with "{")
;;             (setq ctrl "ctrl"
;;                   state t))
;;            ((looking-at-p "<%[ ]*}")
;;             (setq ctrl "ctrl"
;;                   state nil))
;;            )
;;           )

;;          ((string= web-mode-engine "razor")
;;           (cond
;;            ((web-mode-block-ends-with "{")
;;             (setq ctrl "ctrl"
;;                   state t))
;;            ((web-mode-block-starts-with "}")
;;             (setq ctrl "ctrl"
;;                   state nil))
;;            )
;;           )

;;          ) ;cond

;;         ) ;when
;;         ) ;if

;; ;;      (message "engine=%S ctrl=%S state=%S" web-mode-engine ctrl state)

;;       (if ctrl (cons ctrl state) nil)
;;       )))

;; (defvar web-mode-blade-active-blocks
;;   '("else" "elseif" "foreach" "forelse" "for" "if" "section" "stop" "unless" "while")
;;   "Blade controls.")

;; (defvar web-mode-closure-active-blocks
;;   '("call" "case" "default" "deltemplate" "else" "elseif" "for" "foreach"
;;     "if" "ifempty" "let" "literal" "msg" "param" "switch" "template")
;;   "Closure controls.")


;; (defvar web-mode-go-active-blocks
;;   '("else" "end" "if" "range" "with")
;;   "Go controls.")

;; (defvar web-mode-php-active-blocks
;;   '("declare" "else" "elseif" "for" "foreach" "if" "while")
;;   "PHP controls.")

;; (defvar web-mode-smarty-active-blocks
;;   '("block" "else" "elseif" "foreach" "for" "if" "section" "while")
;;   "Smarty controls.")

;; (defvar web-mode-velocity-active-blocks
;;   '("define" "else" "elseif" "end" "for" "foreach" "if" "macro")
;;   "Velocity controls.")

;; (defvar web-mode-web2py-active-blocks
;;   '("block" "def" "elif" "else" "end" "except" "finally" "for"
;;     "if" "pass" "return" "try" "while")
;;   "Web2py active controls")

;; ;; todo : est ce encore util ?
;; (defvar web-mode-active-block-regexps
;;   (list
;;    (cons "asp"        "----")
;;    (cons "aspx"       "----")
;;    (cons "angular"    "----")
;;    (cons "blade"      (concat "@\\(end\\)?" (regexp-opt web-mode-blade-active-blocks t)))
;;    (cons "closure"    (concat "{/?" (regexp-opt web-mode-closure-active-blocks t)))
;;    (cons "ctemplate"  "{{[#^/]\\([[:alnum:]_]+\\)")
;; ;;   (cons "django"     (concat "{%[-]?[ ]+\\(end\\)?" (regexp-opt web-mode-django-active-blocks t)))
;;    (cons "django"     (concat "{%[-]?[ ]+\\(end\\)?" web-mode-django-active-blocks))
;;    (cons "dust"       "{[#/:?@><+^]\\([[:alpha:]_]+\\)")
;;    (cons "erb"        "<%[-=]?[ ]+\\(.* do \\|for\\|unless\\|end\\|if\\|else\\)")
;;    (cons "freemarker" "</?\\([[:alpha:]]+:[[:alpha:]]+\\)\\|[[<]/?[@#]\\([[:alpha:]]+\\)")
;;    (cons "go"         (concat "{{[ ]*" (regexp-opt web-mode-go-active-blocks t)))
;;    (cons "jsp"        "</?\\([[:alpha:]]+:[[:alpha:]]+\\)\\|<%[ ]+\\(if\\|for\\|while\\|} else {\\)")
;;    (cons "mako"       "</?%\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)\\|%[ ]+\\(end\\)?\\(if\\|for\\|elif\\|else\\)")
;;    (cons "mason"      "</?%\\(method\\|def\\)")
;;    (cons "php"        (concat "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?" (regexp-opt web-mode-php-active-blocks t)))
;;    (cons "razor"      ".")
;; ;;   (cons "razor"      "@\\(main\\|if\\|for\\)")
;;    (cons "smarty"     (concat (web-mode-engine-delimiter-open "smarty" "{") "/?" (regexp-opt web-mode-smarty-active-blocks t)))
;;    (cons "template-toolkit" (concat "\\[% " (regexp-opt '("foreach" "if" "else" "elsif" "filter" "end") t)))
;;    (cons "underscore" "<%")
;;    (cons "velocity"   (concat "#" (regexp-opt web-mode-velocity-active-blocks t)))
;;    (cons "web2py"     (concat "{{[ ]*" (regexp-opt web-mode-web2py-active-blocks t)))
;;    )
;;   "Engine control regexps")

;; (defvar web-mode-close-block-regexps
;;   (list
;;    '("asp"              . "----")
;;    '("aspx"             . "----")
;;    '("blade"            . "@\\\(end\\|else\\|stop\\)")
;;    '("closure"          . "{\\(/\\|else\\|case\\|default\\|ifempty\\)")
;;    '("ctemplate"        . "{{/")
;;    '("django"           . "{%[-]?[ ]+\\(end\\|else\\|elseif\\|elsif\\|elif\\|empty\\)")
;;    '("dust"             . "{\\(/\\|:else\\)")
;;    '("erb"              . "<%[-]?[ ]+\\(end\\|else\\)")
;;    '("freemarker"       . "[<[]\\(/#\\|#els\\|#break\\)")
;;    '("go"               . "{{[ ]*\\(end\\|else\\)")
;;    '("jsp"              . "</\\|<% }")
;;    '("mako"             . "</%\\|%[ ]+\\(end\\|elif\\|else\\)")
;;    '("mason"            . "</%\\(method\\|def\\)")
;;    '("php"              . "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\|else\\|}\\)")
;;    '("razor"            . "}")
;;    (cons "smarty"      (concat (web-mode-engine-delimiter-open "smarty" "{") "\\(/\\|else\\)"))
;;    '("template-toolkit" . "\\[% \\(end\\|els\\)")
;;    '("underscore"       . "<%[ ]*}")
;;    '("velocity"         . "#\\(end\\|else\\)")
;;    '("web2py"           . "{{[ ]*\\(pass\\|elif\\|else\\|except\\|finally\\|return\\|end\\)")
;;    )
;;   "Close control blocks.")


;;(defvar web-mode-active-block-regexp nil
;;  "Engine control regexp")

;;(defvar web-mode-close-block-regexp nil
;;  "Engine end control regexp")

;;(defvar web-mode-engine-control-matcher nil
;;  "Engine control match")


(provide 'web-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
