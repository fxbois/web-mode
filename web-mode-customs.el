;;; web-mode-customs.el --- custom variables for web-mode.el
;;; -*- coding: utf-8 -*-

;; Copyright 2011-2015 François-Xavier Bois

;; Version: 11.1.10
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: languages
;; Homepage: http://web-mode.org
;; Repository: http://github.com/fxbois/web-mode
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;; =============================================================================
;; WEB-MODE is sponsored by Kernix: Great Digital Agency (Web & Mobile) in Paris
;; =============================================================================

;; Code goes here

(defcustom web-mode-script-padding 1
  "script element left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-style-padding 1
  "style element left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-block-padding 0
  "multi-line block (php, ruby, java, python, asp, etc.) left padding."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-attr-indent-offset nil
  "html attribute indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-markup-indent-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "html indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-css-indent-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "css indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-code-indent-offset
  (if (and (boundp 'standard-indent) standard-indent) standard-indent 2)
  "code (javascript, php, etc.) indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-sql-indent-offset 4
  "sql (inside strings) indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-enable-css-colorization (display-graphic-p)
  "in a css part, set background according to the color: #xxx, rgb(x,x,x)."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-indentation (display-graphic-p)
  "auto-indentation."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-closing (display-graphic-p)
  "auto-closing."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-pairing (display-graphic-p)
  "auto-pairing."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-opening (display-graphic-p)
  "html element auto-opening."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-quoting (display-graphic-p)
  "add double quotes after the character = in a tag."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-auto-expanding nil
  "e.g. s/ expands to <span>|</span>."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-control-block-indentation t
  "control blocks increase indentation."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-current-element-highlight nil
  "disable element highlight."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-current-column-highlight nil
  "show column for current element."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-whitespace-fontification nil
  "enable whitespaces."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-block-face nil
  "enable block face (useful for setting a background for example).
see web-mode-block-face."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-part-face nil
  "enable part face (useful for setting a background for example).
see web-mode-part-face."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-inlays nil
  "enable inlays (e.g. latex) highlighting."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-sexp-functions t
  "enable specific sexp functions."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-string-interpolation t
  "enable string interpolation fontification (php and erb)."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-heredoc-fontification t
  "enable heredoc fontification. the identifier should contain js, javascript, css or html."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-element-content-fontification nil
  "enable element content fontification. the content of an element can have a face associated."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-element-tag-fontification nil
  "enable tag name fontification."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-engine-detection nil
  "detect such directive -*- engine: engine -*- at the top of the file."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-enable-comment-keywords '()
  "enable highlight of keywords like fixme, todo, etc. in comments."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-comment-style 1
  "comment style : 1 = default, 2 = force server comments outside a block."
  :group 'web-mode
  :type '(choice (const :tag "default" 1)
                 (const :tag "force engine comments" 2)))

(defcustom web-mode-indent-style 2
  "indentation style."
  :group 'web-mode
  :type '(choice (const :tag "default (all lines are indented)" 2)
                 (const :tag "text at the beginning of line is not indented" 1)))

(defcustom web-mode-auto-close-style 1
  "auto-close style."
  :group 'web-mode
  :type '(choice (const :tag "auto-close on </" 1)
                 (const :tag "auto-close on > and </" 2)))

(defcustom web-mode-extra-expanders '()
  "a list of additional expanders."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-auto-pairs '()
  "a list of additional snippets."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-snippets '()
  "a list of additional snippets."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-builtins '()
  "a list of additional builtins."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-constants '()
  "a list of additional constants."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-keywords '()
  "a list of additional keywords."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-extra-types '()
  "a list of additional types."
  :type 'list
  :group 'web-mode)

(defcustom web-mode-tests-directory (concat default-directory "tests/")
  "directory containing all the unit tests."
  :type 'list
  :group 'web-mode)

(provide 'web-mode-customs)

;;; web-mode-customs.el ends here
