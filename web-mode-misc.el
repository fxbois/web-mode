;;; web-mode-misc.el --- misc functionality for web-mode.el
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

(defun web-mode-set-engine (engine)
  "Set the engine for the current buffer."
  (interactive
   (list (completing-read
          "Engine: "
          (let (engines)
            (dolist (elt web-mode-engines)
              (setq engines (append engines (list (car elt)))))
            engines))))
  (setq web-mode-content-type "html"
        web-mode-engine (web-mode-engine-canonical-name engine)
        web-mode-minor-engine engine)
  (web-mode-on-engine-setted)
  (web-mode-buffer-highlight))

(defun web-mode-set-content-type (content-type)
  (setq web-mode-content-type content-type)
  (web-mode-buffer-highlight))

(defun web-mode-on-engine-setted ()
  (let (elt elts engines)

    (when (string= web-mode-engine "razor") (setq web-mode-enable-block-face t))
    (setq web-mode-engine-attr-regexp (cdr (assoc web-mode-engine web-mode-engine-attr-regexps)))
    (setq web-mode-engine-token-regexp (cdr (assoc web-mode-engine web-mode-engine-token-regexps)))

    ;;(message "%S %S" web-mode-engine-attr-regexp web-mode-engine)

    (when (null web-mode-minor-engine)
      (setq web-mode-minor-engine "none"))

    (setq elt (assoc web-mode-engine web-mode-engine-open-delimiter-regexps))
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

    (setq web-mode-engine-font-lock-keywords
          (symbol-value (cdr (assoc web-mode-engine web-mode-engines-font-lock-keywords))))

    (when (and (string= web-mode-minor-engine "jinja")
               (not (member "endtrans" web-mode-django-control-blocks)))
      (add-to-list 'web-mode-django-control-blocks "endtrans")
      (setq web-mode-django-control-blocks-regexp
            (regexp-opt web-mode-django-control-blocks t))
      )

;;    (message "%S" (symbol-value (cdr (assoc web-mode-engine web-mode-engines-font-lock-keywords))))

    ))

(defun web-mode-detect-engine ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "-\\*- engine:[ ]*\\([[:alnum:]-]+\\)[ ]*-\\*-" web-mode-chunk-length t)
      (setq web-mode-minor-engine (match-string-no-properties 1))
      (setq web-mode-engine (web-mode-engine-canonical-name web-mode-minor-engine)))
    web-mode-minor-engine))

(defun web-mode-guess-engine-and-content-type ()
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
        ) ;dolist
      ) ;when

    (unless web-mode-content-type
      (setq found nil)
      (dolist (elt web-mode-content-types)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-content-type (car elt)
                found t))
        ) ;dolist
      ) ;unless

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
        ;;(message "%S %S" (cdr elt) buff-name)
        (when (and (not found) (string-match-p (cdr elt) buff-name))
          (setq web-mode-engine (car elt)
                found t))
        )
      )

    (unless web-mode-engine
      (setq found nil)
      (dolist (elt web-mode-engines)
        ;;(message "%S %S" (car elt) buff-name)
        (when (and (not found) (string-match-p (car elt) buff-name))
          (setq web-mode-engine (car elt)
                found t))
        )
      )

    (when (and (or (null web-mode-engine) (string= web-mode-engine "none"))
               (string-match-p "php" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))
      (setq web-mode-engine "php"))

    (when (and (string= web-mode-content-type "javascript")
               (string-match-p "@jsx"
                               (buffer-substring-no-properties
                                (point-min)
                                (if (< (point-max) web-mode-chunk-length)
                                    (point-max)
                                  web-mode-chunk-length)
                                )))
      (setq web-mode-content-type "jsx"))

    (when web-mode-engine
      (setq web-mode-minor-engine web-mode-engine
            web-mode-engine (web-mode-engine-canonical-name web-mode-engine))
      )

    (when (and (or (null web-mode-engine)
                   (string= web-mode-engine "none"))
               web-mode-enable-engine-detection)
      (web-mode-detect-engine))

    (web-mode-on-engine-setted)

    ))

(defun web-mode-engine-canonical-name (name)
  (let (engine)
    (cond
     ((null name)
      nil)
     ((assoc name web-mode-engines)
      name)
     (t
      (dolist (elt web-mode-engines)
        (when (and (null engine) (member name (cdr elt)))
          (setq engine (car elt)))
        ) ;dolist
      engine)
     )))

(defun web-mode-on-after-save ()
  (when web-mode-is-scratch
    (web-mode-guess-engine-and-content-type)
    (web-mode-buffer-highlight))
  nil)

(defun web-mode-on-exit ()
  (web-mode-with-silent-modifications
   (put-text-property (point-min) (point-max) 'invisible nil)
   (remove-overlays)
   (remove-hook 'change-major-mode-hook 'web-mode-on-exit t)
   ))

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (web-mode-with-silent-modifications
    (put-text-property (point-min) (point-max) 'invisible nil)
    (remove-overlays)
    (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region)
    (load "web-mode.el")
    (setq web-mode-change-beg nil
          web-mode-change-end nil)
    (web-mode)
    ))

(defun web-mode-trace (msg)
  (let (sub)
    ;;      (when (null web-mode-time) (setq web-mode-time (current-time)))
    (setq sub (time-subtract (current-time) web-mode-time))
    (when nil
      (save-excursion
        (let ((n 0))
          (goto-char (point-min))
          (while (web-mode-tag-next)
            (setq n (1+ n))
            )
          (message "%S tags found" n)
          )))
    (message "%18s: time elapsed = %Ss %9Sµs" msg (nth 1 sub) (nth 2 sub))
    ))

(defun web-mode-reveal ()
  "Display text properties at point."
  (interactive)
  (let (symbols out)
    (setq out (format
               "[point=%S engine=%S minor=%S content-type=%S language-at-pos=%S]\n"
               (point)
               web-mode-engine
               web-mode-minor-engine
               web-mode-content-type
               (web-mode-language-at-pos (point))))
    (setq symbols (append web-mode-scan-properties '(font-lock-face face)))
    (dolist (symbol symbols)
      (when symbol
        (setq out (concat out (format "%s(%S) " (symbol-name symbol) (get-text-property (point) symbol)))))
      )
    (message "%s\n" out)
    ;;(message "syntax-class=%S" (syntax-class (syntax-after (point))))
    (message nil)))

(defun web-mode-debug ()
  "Display informations useful for debugging."
  (interactive)
  (let ((modes nil)
        (customs '(web-mode-enable-current-column-highlight web-mode-enable-current-element-highlight indent-tabs-mode))
        (ignore '(abbrev-mode auto-composition-mode auto-compression-mode auto-encryption-mode auto-insert-mode blink-cursor-mode column-number-mode delete-selection-mode display-time-mode electric-indent-mode file-name-shadow-mode font-lock-mode global-font-lock-mode global-hl-line-mode line-number-mode menu-bar-mode mouse-wheel-mode recentf-mode show-point-mode tool-bar-mode tooltip-mode transient-mark-mode)))
    (message "\n")
    (message "--- WEB-MODE DEBUG BEG ---")
    (message "versions: emacs(%S.%S) web-mode(%S)"
             emacs-major-version emacs-minor-version web-mode-version)
    (message "vars: engine(%S) minor(%S) content-type(%S) file(%S)"
             web-mode-engine
             web-mode-minor-engine
             web-mode-content-type
             (or (buffer-file-name) (buffer-name)))
    (message "system: window(%S) config(%S)" window-system system-configuration)
    (message "colors: fg(%S) bg(%S) "
             (cdr (assoc 'foreground-color default-frame-alist))
             (cdr (assoc 'background-color default-frame-alist)))
    (mapc (lambda (mode)
            (condition-case nil
                (if (and (symbolp mode) (symbol-value mode) (not (member mode ignore)))
                    (add-to-list 'modes mode))
              (error nil))
            ) ;lambda
          minor-mode-list)
    (message "minor modes: %S" modes)
    (message "vars:")
    (dolist (custom customs)
      (message (format "%s=%S " (symbol-name custom) (symbol-value custom))))
    (message "--- WEB-MODE DEBUG END ---")
    (switch-to-buffer "*Messages*")
    (goto-char (point-max))
    (recenter)
  ))

(provide 'web-mode-misc)

;;; web-mode-misc.el ends here
