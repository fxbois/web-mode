;;; web-mode.el --- major mode for editing web templates
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

(require 'web-mode-customs)
(require 'web-mode-groups)
(require 'web-mode-faces)
(require 'web-mode-defuns)
(require 'web-mode-search)
(require 'web-mode-highlighting)
(require 'web-mode-indentation)
(require 'web-mode-position)
(require 'web-mode-testing)
(require 'web-mode-misc)
(require 'web-mode-minor)
(require 'web-mode-excursion)
(require 'web-mode-vars)

;;---- TODO --------------------------------------------------------------------

;; v12 : invert path and XX (web-mode-engines-alist,
;;       web-mode-content-types-alist) for more consistency

;;---- CONSTS ------------------------------------------------------------------

(defconst web-mode-version "11.1.10"
  "Web Mode version.")

;;---- COMPATIBILITY -----------------------------------------------------------

(eval-and-compile

  (defalias 'web-mode-prog-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

  ;; compatibility with emacs < 23.3
  (if (fboundp 'with-silent-modifications)
      (defalias 'web-mode-with-silent-modifications 'with-silent-modifications)
    (defmacro web-mode-with-silent-modifications (&rest body)
      `(let ((old-modified-p (buffer-modified-p))
             (inhibit-modification-hooks t)
             (buffer-undo-list t))
         (unwind-protect
             ,@body
           (set-buffer-modified-p old-modified-p)))))

  ;; compatibility with emacs < 24.3
  (defun web-mode-buffer-narrowed-p ()
    (if (fboundp 'buffer-narrowed-p)
        (buffer-narrowed-p)
      (/= (- (point-max) (point-min)) (buffer-size))))

  ;; compatibility with emacs 22
  (defun web-mode-string-match-p (regexp string &optional start)
    "Same as `string-match' except it does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start)))

  (unless (fboundp 'string-match-p)
    (fset 'string-match-p (symbol-function 'web-mode-string-match-p)))

  ;; compatibility with emacs < 24.3
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

  ) ;eval-and-compile

;;---- MAJOR MODE --------------------------------------------------------------

;;;###autoload
(define-derived-mode web-mode web-mode-prog-mode "Web"
  "Major mode for editing web templates."

  (make-local-variable 'web-mode-attr-indent-offset)
  (make-local-variable 'web-mode-auto-pairs)
  (make-local-variable 'web-mode-block-regexp)
  (make-local-variable 'web-mode-change-beg)
  (make-local-variable 'web-mode-change-end)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-column-overlays)
  (make-local-variable 'web-mode-comment-style)
  (make-local-variable 'web-mode-content-type)
  (make-local-variable 'web-mode-css-indent-offset)
  (make-local-variable 'web-mode-inhibit-fontification)
  (make-local-variable 'web-mode-display-table)
  (make-local-variable 'web-mode-enable-block-face)
  (make-local-variable 'web-mode-enable-inlays)
  (make-local-variable 'web-mode-enable-part-face)
  (make-local-variable 'web-mode-enable-sexp-functions)
  (make-local-variable 'web-mode-end-tag-overlay)
  (make-local-variable 'web-mode-engine)
  (make-local-variable 'web-mode-engine-attr-regexp)
  (make-local-variable 'web-mode-engine-file-regexps)
  (make-local-variable 'web-mode-engine-open-delimiter-regexps)
  (make-local-variable 'web-mode-engine-token-regexp)
  (make-local-variable 'web-mode-expand-initial-pos)
  (make-local-variable 'web-mode-expand-previous-state)
  (make-local-variable 'web-mode-indent-style)
  (make-local-variable 'web-mode-is-scratch)
  (make-local-variable 'web-mode-jshint-errors)
  (make-local-variable 'web-mode-last-enabled-feature)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-sql-indent-offset)
  (make-local-variable 'web-mode-start-tag-overlay)
  (make-local-variable 'web-mode-minor-engine)
  (make-local-variable 'web-mode-time)

  (make-local-variable 'web-mode-django-control-blocks)
  (make-local-variable 'web-mode-django-control-blocks-regexp)

  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start)
  (make-local-variable 'fill-paragraph-function)
  (make-local-variable 'font-lock-beg)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-extend-region-functions)
  (make-local-variable 'font-lock-end)
  (make-local-variable 'font-lock-support-mode)
  (make-local-variable 'font-lock-unfontify-region-function)
  (make-local-variable 'imenu-case-fold-search)
  (make-local-variable 'imenu-create-index-function)
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'parse-sexp-lookup-properties)
;;  (make-local-variable 'syntax-propertize-function)
  (make-local-variable 'yank-excluded-properties)

  ;; NOTE: required for block-code-beg|end
  ;;(make-local-variable 'text-property-default-nonsticky)
  ;;(add-to-list 'text-property-default-nonsticky '(block-token . t))
  ;;(message "%S" text-property-default-nonsticky)

  (setq comment-end "-->"
        comment-start "<!--"
        fill-paragraph-function 'web-mode-fill-paragraph
        font-lock-defaults '(web-mode-font-lock-keywords t)
        font-lock-extend-region-functions '(web-mode-extend-region)
        font-lock-support-mode nil
        font-lock-unfontify-region-function 'web-mode-unfontify-region
        imenu-case-fold-search t
        imenu-create-index-function 'web-mode-imenu-index
        indent-line-function 'web-mode-indent-line
        parse-sexp-lookup-properties t
;;        syntax-propertize-function nil
        yank-excluded-properties t)

  (add-hook 'after-change-functions 'web-mode-on-after-change nil t)
  (add-hook 'after-save-hook        'web-mode-on-after-save t t)
  (add-hook 'change-major-mode-hook 'web-mode-on-exit nil t)
  (add-hook 'post-command-hook      'web-mode-on-post-command nil t)

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

  (when web-mode-enable-whitespace-fontification
    (web-mode-whitespaces-on))

  (when (and (boundp 'indent-tabs-mode) indent-tabs-mode)
    (web-mode-use-tabs))

  (when web-mode-enable-sexp-functions
    (setq-local forward-sexp-function 'web-mode-forward-sexp))

  (web-mode-guess-engine-and-content-type)
  (setq web-mode-change-beg (point-min)
        web-mode-change-end (point-max))
  (when (> (point-max) 256000)
    (web-mode-buffer-highlight))

  (when (and (boundp 'hs-special-modes-alist)
             (not (assoc major-mode hs-special-modes-alist)))
    (add-to-list 'hs-special-modes-alist '(web-mode "{" "}" "/[*/]" web-mode-forward-sexp nil))
    ) ;when

  ;;(web-mode-trace "buffer loaded")

  )

;;---- ADVICES -----------------------------------------------------------------

(defadvice ac-start (before web-mode-set-up-ac-sources activate)
  "Set `ac-sources' based on current language before running auto-complete."
  (if (equal major-mode 'web-mode)
      (progn
        (run-hooks 'web-mode-before-auto-complete-hooks)
        (when web-mode-ac-sources-alist
          (let ((new-web-mode-ac-sources
                 (assoc (web-mode-language-at-pos)
                        web-mode-ac-sources-alist)))
            (setq ac-sources (cdr new-web-mode-ac-sources)))))))

(provide 'web-mode)

;;; web-mode.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
