;;; web-mode.el --- major mode for editing PHP Templates

;; Copyright (C) 2011, 2012 François-Xavier Bois

;; =========================================================================
;; This work is sponsored by KerniX : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; Version: 0.99
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: HTML PHP JavaScript CSS Template Web
;; URL: http://github.com/fxbois/web-mode
;;      http://web-mode.org

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

(eval-when-compile (require 'cl))

(defgroup web-mode nil
  "Major mode for editing PHP templates."
  :version "0.99"
  :group 'languages)

(defgroup web-mode-faces nil
  "Faces used in web-mode"
  :group 'web-mode
  :group 'faces)

(defconst web-mode-debug nil
  "t if in debug mode.")

(defcustom web-mode-html-offset 2
  "HTML indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-script-offset 2
  "PHP and JavaScript indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-autocompletes-flag t
  "Handle autocompletes."
  :type 'bool
  :group 'web-mode)

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in web-mode buffers.")

(define-derived-mode web-mode prog-mode "Web"
  "Major mode for editing mixed HTML Templates."

  ;; let syntax-table above
  (set-syntax-table web-mode-syntax-table)

  ;; (set (make-local-variable 'font-lock-defaults) '(web-mode-font-lock-keywords
  ;;                                                 t
  ;;                                                  t
  ;;                                                  (("_" . "w"))
  ;;                                                  nil))

  (set (make-local-variable 'font-lock-defaults) '(web-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-keywords-only) t)
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  (set (make-local-variable 'font-lock-syntax-table) nil)
  (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil)
  (set (make-local-variable 'font-lock-multiline) t)

  (add-to-list (make-local-variable 'font-lock-extend-region-functions) 
               'web-mode-font-lock-extend-region)

  (set (make-local-variable 'indent-line-function) 'web-mode-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  
;;  (message "%s" (string (char-syntax ?_)))

;;  (set (make-local-variable 'redisplay-preemption-period) 2)
  (set (make-local-variable 'require-final-newline) nil)

  (add-hook 'after-change-functions 'web-mode-on-after-change t t)

  (define-key web-mode-map (kbd "C-c C-c") '(lambda ()
                                              (interactive)
                                              (if (web-mode-in-block "script") (message "in script"))

;;                                              (message "elt at pt: %s" (web-mode-element-at-point))
                                              ))
  (define-key web-mode-map (kbd "C-c C-(") 'web-mode-fetch-opening-paren)
  (define-key web-mode-map (kbd "C-c C-d") 'web-mode-delete-element)
  (define-key web-mode-map (kbd "C-c C-i") 'web-mode-insert)
  (define-key web-mode-map (kbd "C-c C-j") 'web-mode-duplicate-element)
  (define-key web-mode-map (kbd "C-c C-n") 'web-mode-match-tag)
  (define-key web-mode-map (kbd "C-c C-p") 'web-mode-parent-element)
  (define-key web-mode-map (kbd "C-c C-r") 'web-mode-reload)
  (define-key web-mode-map (kbd "C-c C-s") 'web-mode-select-element)
  (define-key web-mode-map (kbd "C-c C-t") 'web-mode-insert-table)

  (define-key web-mode-map [menu-bar]
    (make-sparse-keymap))
  (define-key web-mode-map [menu-bar web]
    (cons "Web" (make-sparse-keymap)))
  (define-key web-mode-map [menu-bar web html]
    (cons "HTML" (make-sparse-keymap)))
  (define-key web-mode-map [menu-bar web html insert-table]
    '("Insert TABLE" . web-mode-insert-table))
  (define-key web-mode-map [menu-bar web sep1]
    '("--"))
  (define-key web-mode-map [menu-bar web debug]
    '("debug" . web-mode-debug))
)

(defvar web-mode-hook nil
  "List of functions to be executed with web-mode.")

(defface web-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-doctype-face
  '((t :foreground "Grey"))
  "Face for HTML doctype."
  :group 'web-mode-faces)

(defface web-mode-html-tag-face
  '((t :foreground "Snow4"))
  "Face for HTML tags."
  :group 'web-mode-faces)

(defface web-mode-html-attr-face
  '((t :foreground "Snow3"))
  "Face for HTML attributes."
  :group 'web-mode-faces)

(defface web-mode-css-rule-face
  '((t :foreground "Pink3"))
  "Face for CSS rules."
  :group 'web-mode-faces)

(defface web-mode-css-prop-face
  '((t :foreground "Pink1"))
  "Face for CSS props."
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

(defface web-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'web-mode-faces)

(defface web-mode-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for PHP constants."
  :group 'web-mode-faces)

(defface web-mode-type-face
  '((t :inherit font-lock-type-face))
  "Face for PHP/JS types."
  :group 'web-mode-faces)

(defface web-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for PHP/JS keywords."
  :group 'web-mode-faces)

(defun web-mode-replace-apos ()
  "Replace ' by ’."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" nil t)
      (replace-match "\\1’\\2"))))

(defun web-mode-is-comment-or-string (&optional pt)
  "Check if point is in a comment or in a string."
  (interactive)
  (unless pt
    (setq pt (point)))
  (let (face) 
    (setq face (get-text-property pt 'face))
    (or (eq 'web-mode-comment-face face)
        (eq 'web-mode-string-face face))))

;; TODO : - check every chars of the line
;;        - skip line with only PHP code => avoir une prop pour toute la zone php
(defun web-mode-previous-usable-line ()
  "Move cursor to previous non blank line."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      nil
    (progn
      (let ((continue 't) 
            line)
        (while (and continue
                    (forward-line -1))
          (setq line (web-mode-trim (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
          (if (or (bobp)
                  (not (string= line "")))
              (unless (and (web-mode-is-comment-or-string)
                           (web-mode-is-comment-or-string (1- (line-end-position))))
                (setq continue nil))
            ))
        ;;    (message (concat "previous:" line))
        (if (string= line "")
            nil
          line)))))

(defun web-mode-in-php-block ()
  "Detect if point is in a PHP block."
  (let ((pt (point))
        (ln (web-mode-current-line)))
    (save-excursion
      (and (search-backward "<?" 0 t)
           (not (string-match "\\?>" (buffer-substring-no-properties
                                      (point) pt)))
           ;;           (progn (message "lines: %d %d" ln (web-mode-current-line)) 't)
           (not (eq ln (web-mode-current-line)))
           ))))

(defun web-mode-in-block (type)
  "Detect if point is in a block"
  (let ((pt (point))
        (line-pt (web-mode-current-line))
        line-open line-close)
    (save-excursion
      (and (search-backward (concat "<" type) 0 t)
           (setq line-open (web-mode-current-line))
           (> line-pt line-open)
           (search-forward (concat "</" type ">") nil t)
           (< pt (point))
           (setq line-close (web-mode-current-line))
           (not (eq line-open line-close))
           (<= line-pt line-close)
           (beginning-of-line)
           (not (looking-at (concat "[ \t]*</" type)))
           ))
    );; let
)

;; (defun web-mode-in-style-block ()
;;   "Detect if point is in a style block."
;;   (let ((pt (point)))
;;     (save-excursion
;;       (and (search-backward "<style" 0 t)
;;            (not (string-match "</style>" (buffer-substring-no-properties
;;                                           (point) pt)))))))

;; (defun web-mode-in-script-block ()
;;   "Detect if point is in a script block."
;;   (let ((pt (point)))
;;     (save-excursion
;;       (and (search-backward "<script" 0 t)
;;            (not (string-match "</script>" (buffer-substring-no-properties
;;                                            (point) pt)))))))

(defun web-mode-current-line (&optional pt)
  "Return line number at point."
  (unless pt
    (setq pt (point)))
  (+ (count-lines (window-start) pt)
     (if (= (current-column) 0) 1 0))
  )

(defun web-mode-indent-line ()
  "Indent current line according to language."
  (interactive)
  (let (case-fold-search
        continue
        cur-column
        cur-first-char
        cur-indentation
        cur-line-number
        cur-line
        cur-point
        in-php-block
        in-script-block
        in-style-block
        line-number
        offset
        prev-last-char 
        prev-line 
        pt)
;;    (message "%S" (syntax-ppss))
    (save-excursion
      (setq cur-column (current-column)
            cur-line-number (web-mode-current-line)
            cur-indentation (current-indentation)
            cur-point (point))
      (cond
       ((web-mode-in-php-block)
        (setq in-php-block t))
       ((web-mode-in-block "script")
        (setq in-script-block t))
       ((web-mode-in-block "style")
        (setq in-style-block t))
       )
;;      (message "php(%S) script(%S) style(%S)" in-php-block in-script-block in-style-block)
      (setq cur-line (web-mode-trim (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
      (setq cur-first-char (if (string= cur-line "")
                               cur-line
                             (substring cur-line 0 1)))
      (setq prev-line (web-mode-previous-usable-line))
      (setq prev-last-char (if (null prev-line)
                               prev-line
                             (substring prev-line -1)))
      (if (null prev-line)
          (setq offset 0)
        (progn ;; not null prev-line
          
          (cond ;; switch language
           
           ((or in-php-block in-script-block) ;; php or script block
            
            (cond
             
             ((and (string-match "<\\?php" cur-line)
                   (string-match "\\?>" cur-line))
              (progn
                (message "do nothing")
                ))

             ((string= cur-first-char "}")
              (progn
                (end-of-line)
                (let (counter regexp)
                  (setq counter 1)
                  (setq regexp "[{}]")
                  (while (and (> counter 0)
                              (re-search-backward regexp nil t))
                    (progn
                      (if (string= (substring (match-string-no-properties 0) 0 1) "{")
                          (setq counter (- counter 1))
                        (setq counter (+ counter 1)))
                      )
                    ))
                (setq offset (current-indentation))
                ))

             ((and (string= cur-first-char "?")
                   (not (string= (substring cur-line 0 2) "?>")))
              (progn
                (end-of-line)
                (re-search-backward "[=(]")
                (setq offset (current-column))
                ))

             ((string= cur-first-char ":")
              (progn
                (setq offset (current-indentation))
                ))

             ((string= prev-last-char ",")
              (progn
                (end-of-line)
                (web-mode-fetch-opening-paren cur-point)
                (setq offset (+ (current-column) 1))
                ))

             ((string= prev-last-char ".")
              (progn
                (end-of-line)
                (search-backward "=")
                (forward-char)
                (forward-char)
                (setq offset (current-column))
                ))

             ((string= prev-last-char ";")
              (progn
                (end-of-line)
                (re-search-backward "[=(]")
                (setq offset (current-indentation))
                ))

             ((string= prev-last-char "{")
              (progn
                (setq offset (+ (current-indentation) web-mode-script-offset))
                ))

             ((string-match "\\(->[[:alnum:]_]+\\|)\\)$" prev-line)
              (progn
                (end-of-line)
                (search-backward ">")
                (setq offset (- (current-column) 1))
                ))

             (t
              (progn
;;                (message "php")
                ))

             )) ;; end case php or script bloc

           (in-style-block ;; style bloc
            
            (cond
             
             ((string-match "[{,]" cur-line)
              (progn
                (setq offset 0)
                ))
             
             ((string= prev-last-char ";")
              (progn
                (end-of-line)
                (re-search-backward "{")
                (skip-chars-forward "{ ")
                (setq offset (current-column))
                ))
             
             )) ;; end case style bloc


            (t ;; case html bloc

             (cond
              
              ((and (not (string= cur-first-char "<"))
                    (string-match "\\<[[:alpha:]-]+=\".*\"$" prev-line))
               (progn
                 (end-of-line)
                 (re-search-backward "<[[:alpha:]]")
                 (re-search-forward "<[[:alpha:]]+")
                 (skip-chars-forward " ")
                 (setq offset (current-column))
                 ))

              ((string-match "^<\\?php[ ]+\\(if\\|for\\|while\\|else\\|end\\)" cur-line)
               (progn
                 (setq offset 0)
                 ))

              ((and (string-match "^\\<[[:alpha:]-]+=\"" cur-line)
                    (re-search-backward "\\<[[:alpha:]-]+=\".*\"$" nil t))
               (progn 
                 (setq offset (current-column))
                 ))

              ((string-match "^</?\\(head\\|body\\|meta\\|link\\|title\\|style\\|script\\)" 
                             cur-line) 
               (progn
                 (setq offset 0)
                 ))

              ((string-match "^</" cur-line)
               (progn
                 (goto-char cur-point)
                 (beginning-of-line)
                 (re-search-forward "</")
                 (web-mode-match-tag)
                 (setq offset (current-indentation))
                 ))

              ((or (string= "" cur-line)
                   (string-match "^<[[:alpha:]]" cur-line))
               (progn
                 (end-of-line)
;;                 (message "%s" (web-mode-current-trimmed-line))
                 (setq continue t)
                 (while (and continue
                             (re-search-backward "^[ \t]*<\\([[:alpha:]]\\|/\\)" nil t))
;;                   (message "current-trimmed-line: %s" (web-mode-current-trimmed-line))
                   (when (and (not (web-mode-is-comment-or-string))
                              (not (looking-at "[ \t]*</?\\(script\\|style\\)")))
                     (setq continue nil)
                     (unless (string= (string (following-char)) "<")
                       (re-search-forward "<"))
;;                     (message "%s" (string (following-char)))
                     (setq offset (+ (current-indentation) 
;;                                     (if (and (web-mode-is-opened-element (web-mode-current-trimmed-line))
                                     (if (and (web-mode-is-opened-element (web-mode-element-at-point))
                                              (not (looking-at "<body")))
                                         web-mode-html-offset
                                       0)
                                     ))
                     ))
                 
                 ))

              (t
               (progn
                 ()
;;                 (message "..")
                 ))

              )) ;; end case html bloc

            ) ;; end switch language bloc

          )) ;; not null prev-line

      ) ;; save-excursion

    
    (when (and offset
               (not (eq cur-indentation offset))) 
      (setq offset (max 0 offset))  
      (indent-line-to offset)
      )
    
    (if (< (current-column) (current-indentation)) (back-to-indentation))
    
    ) ;; let
  )

(defun web-mode-fetch-opening-paren (&optional pt)
  "Fetch opening paren"
  (interactive)
  (unless pt 
    (setq pt (point)))
  (let ((continue t) 
        (n 0))
    (while (and continue
                (re-search-backward "[()]"))
      (unless (web-mode-is-comment-or-string)
        (if (string= (string (char-after)) "(")
            (progn 
              (setq n (1+ n))
              (if (> n 0)
                  (setq continue nil))
              )
          (setq n (1- n))))
      )
    );;let
  )

(defun web-mode-count-char-in-string (char &optional string)
  "count char in string"
;;  (setq c (elt c 0))
  (let ((i 0) (n 0) l)
    (setq l (length string))
    (while (< i l)
      (if (char-equal (elt string i) char)
          (setq n (1+ n)))
      (setq i (1+ i)))
;;    (message "%c %d" char n)
    n))

(defun web-mode-element-at-point ()
  "Return element at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((continue t)
          cont
          line l1 l2
          (pt (point)))
      (while continue
        (setq l1 (web-mode-current-line))
        (end-of-line)
        (setq cont t)
        (while cont
          (re-search-backward "<[[:alpha:]/]" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        (setq cont t)
        (while cont
          (re-search-forward "[[:alnum:] /\"']>" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        (setq l2 (web-mode-current-line))
        (if (eq l1 l2) (setq continue nil))
        )
      (end-of-line)
      (setq line (buffer-substring-no-properties pt (point)))
      (setq line (replace-regexp-in-string "[\r\n]" "" line))
;;      (message "elt at point: %s" line)
      line
      )))

(defun web-mode-select-element ()
  "Select the current HTML element"
  (interactive)
  (when (or (looking-at "<") 
            (re-search-backward "<[[:alpha:]]" nil t))
    (set-mark (point))
    (web-mode-match-tag)
    (search-forward ">"))
  )

(defun web-mode-delete-element ()
  "Delete the current HTML element"
  (interactive)
  (web-mode-select-element)
  (when mark-active
    (delete-region (region-beginning) (region-end)))
  )

(defun web-mode-duplicate-element ()
  "Duplicate the current HTML element"
  (interactive)
  (let ((offset 0))
    (web-mode-select-element)
    (when mark-active
      (save-excursion 
        (goto-char (region-beginning))
        (setq offset (current-column)))
      (kill-region (region-beginning) (region-end))
      (yank)
      (newline)
      (indent-line-to offset)
      (yank))
    )
  )

(defun web-mode-is-opened-element (&optional line)
  "Is there any HTML element without a closing tag ?"
  (interactive)
  (let ((deb 0)
        is-closing-tag
        tag
        n
        ret
        (h (make-hash-table :test 'equal)))
    (unless line
      (progn 
        (setq line (web-mode-element-at-point))
        ;;      (setq line (web-mode-current-trimmed-line))
        ;;      (if (% (web-mode-count-char-in-string ?>)) ())
        ))
;;    (message "line=%s" line)
    (while (string-match "<\\(/?[[:alpha:]]+\\)" line deb)
      (setq deb (match-end 0)
            tag (match-string 1 line)
            is-closing-tag (string= (substring tag 0 1) "/"))
      (if is-closing-tag (setq tag (substring tag 1)))
      (setq n (gethash tag h 0))
      (if (not (web-mode-is-void-html-element tag))
          (if is-closing-tag
              (if (> n 0) (puthash tag (1- n) h))
            (puthash tag (1+ n) h))
        ;;        (message (concat "void tag: " tag))
        )
      )
    ;;(message (number-to-string (gethash "strong" h)))
    ;;(message (number-to-string (hash-table-count h)))
    (maphash '(lambda (k v) (if (> v 0) (setq ret 't))) h)
;;    (if ret (message "line=%s: opened" line) (message "line=%s: closed" line))
    ret
    )
  )

(defun web-mode-parent-element ()
  "Fetch parent element."
  (interactive)
  (let (pt 
        is-closing-tag
        tag
        n
        (continue t)
        (h (make-hash-table :test 'equal)))
    (save-excursion
      (unless (string= (string (char-after)) "<")
        (progn
          (forward-char)
          (re-search-backward "<[[:alpha:]]" nil t)))
      (while (and continue
                  (re-search-backward "</?[[:alpha:]]" nil t))
        (forward-char)
        (setq is-closing-tag (string= (string (char-after)) "/"))
        (if (eq is-closing-tag t) (forward-char))
        (setq nb (skip-chars-forward "a-zA-Z"))
        (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
        (setq n (gethash tag h 0))
;;        (message "%s %d %d" tag n (point))
        (when (not (web-mode-is-void-html-element tag))
          (search-backward "<")
          (if (eq is-closing-tag t)
              (puthash tag (1- n) h)
            (progn
              (puthash tag (1+ n) h)
              (if (eq n 0)
                  (progn
                    (setq pt (point))
                    (setq continue nil)))
              )
            )
          ) ;; when
        ) ;; while
      ) ;; save-excursion
    (if (null continue) (goto-char pt))
    ) ;; let
  )

;;(what-cursor-position)
(defun web-mode-text-at-point ()
  "Text at point."
  (buffer-substring-no-properties (point) (line-end-position)))

(defun web-mode-current-trimmed-line ()
  "Line at point, trimmed."
  (web-mode-trim (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))))

(defun web-mode-trim (string)
  "Remove white spaces in beginning and ending of STRING."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string
    "[ \t\n]*\\'" "" string)))

(defun web-mode-is-void-html-element (tag)
  "Test is param is a void HTML tag."
  (find tag web-mode-void-html-elements :test 'equal))

(defconst web-mode-void-html-elements
  '("hr" "br" "col" "input" "link" "meta" "img")
  "Void (self-closing) HTML tags.")

(defconst web-mode-php-constants
  (regexp-opt
   (append (if (boundp 'web-mode-php-constants) web-mode-php-constants '())
           '("TRUE" "FALSE" "NULL" "true" "false" "null"
             "STR_PAD_LEFT" "STR_PAD_RIGHT")))
  "PHP constants.")

(defconst web-mode-php-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-php-keywords) web-mode-php-keywords '())
           '("array" "as" "break" "catch" "continue"
             "default" "die" "do"
             "echo" "else" "elseif"
             "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit"
             "for" "foreach"
             "if" "include" "instanceof"
             "next" "or" "require" "return" "switch"
             "when" "while")))
  "PHP keywords.")

(defconst web-mode-php-types
  (eval-when-compile
    (regexp-opt
     '("array" "bool" "boolean" "char" "const" "double" "float"
       "int" "integer" "long" "mixed" "object" "real" "string")))
  "PHP types.")

(defconst web-mode-js-keywords
  (eval-when-compile
    (regexp-opt
     '("function" "for" "if" "var" "while" "new" "try" "catch")))
  "JavaScript keywords.")

(defun web-mode-highlight-style-bloc (limit)
  "Highlight a <style> bloc."
  (let (beg
        end
        (font-lock-keywords web-mode-style-font-lock-keywords)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max)))
    (when (re-search-forward
           "<style>\\(.\\|\n\\)*?</style>"
           limit t)
      (setq beg (match-beginning 0)
            end (match-end 0))
      (font-lock-fontify-region beg end))))

(defun web-mode-highlight-script-bloc (limit)
  "Highlight a <script> bloc."
  (let (beg
        end
        (font-lock-keywords web-mode-script-font-lock-keywords)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max)))
    (when (re-search-forward
           "<script.*?>\\(.\\|\n\\)*?</script>"
           limit t)
      (setq beg (match-beginning 0)
            end (match-end 0))
      (font-lock-fontify-region beg end))))

(defun web-mode-highlight-php-bloc (limit)
  "Highlight a PHP bloc."
  (let (beg
        end
;;        font-lock-keywords
        (font-lock-keywords web-mode-php-font-lock-keywords)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max)))
;;    (message "%d %d" (point) limit)
    (when (re-search-forward
           "\\(<\\?php\\|<\\?=\\)\\(.\\|\n\\)*?\\?>"
           limit t)
      (setq beg (match-beginning 0)
            end (match-end 0))
;;      (message "%s" (buffer-substring-no-properties beg end))
      (font-lock-fontify-region beg end)

;;      (message "after %d" (point))
      ) ;; when

    ))

(defun web-mode-font-lock-extend-region ()
  "Extend region."
;;  (message "beg(%d) end(%d) max(%d)" font-lock-beg font-lock-end (point-max))
  (setq font-lock-end (point-max))
  )

(defconst web-mode-font-lock-keywords
  (list

   ;; html tag
   '("</?[[:alnum:]]\\{0,10\\}?\\(>\\|<\\|[ ]\\|/\\|$\\)" 0 'web-mode-html-tag-face t t)
;;   '("<[[:alnum:]]\\{0,10\\}" . 'web-mode-html-tag-face)
   '("/?>" 0 'web-mode-html-tag-face t t)

   ;; html attribute name
   '("\\<[[:alnum:]-]+=" 0 'web-mode-html-attr-face t t)
   
   ;; html attribute value
   '("[[:alnum:]]=\\(\"\\(.\\|\n\\)*?\"\\)" 1 'web-mode-string-face t t)

   '("<\\?[ph=]*" 0 'web-mode-preprocessor-face t t)
   '("\\?>" 0 'web-mode-preprocessor-face t t)
   '(web-mode-highlight-style-bloc)
   '(web-mode-highlight-script-bloc)
   '(web-mode-highlight-php-bloc)
   '("^<!D\\(.\\|\n\\)*?>" 0 'web-mode-doctype-face t t)
   '("^<\\?xml .+\\?>" 0 'web-mode-doctype-face t t)
   '("<!--\\(.\\|\n\\)*?-->" 0 'web-mode-comment-face t t)

   ))

(defconst web-mode-style-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("</?style>" 0 'web-mode-html-tag-face t t)
   '("^\\(.+?\\)\\({\\|,\\)" 1 'web-mode-css-rule-face)
   '("[[:alpha:]-]*?:" 0 'web-mode-css-prop-face)
   '("\\(\".*?\"\\|'.*?'\\)" 0 'web-mode-string-face t t)
   ))

(defconst web-mode-script-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("</?script.*?>" 0 'web-mode-html-tag-face t t)
   '(" \\(type\\)=" 0 'web-mode-html-attr-face t t)
   '("\\<\\([[:alnum:]_.]+\\)[ ]?(" 1 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-js-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))
   '("\\(\"\\(.\\|\n\\)*?\"\\|'\\(.\\|\n\\)*?'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]//.+" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-php-font-lock-keywords
  (list

   '(".*" 0 nil t t)

   '("<\\?\\(php\\|=\\)?" 0 'web-mode-preprocessor-face t t)
   '("\\?>" 0 'web-mode-preprocessor-face t t)

   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))

   (cons (concat "(\\s-*\\(" web-mode-php-types "\\)\\s-*)") 
         '(1 'web-mode-type-face))

   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") 
         '(0 'web-mode-constant-face))

   ;; func(
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)

   ;; Class::CONSTANT
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-constant-face)

   ;; ->variable
   '("->[ ]?\\(\\sw+\\)" 1 'web-mode-variable-name-face)

   ;; Class::
   '("\\<\\(\\sw+\\)[ ]?::" 1 'web-mode-type-face)
   ;; instanceof Class
   '("instanceof[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-type-face)

   ;; $var
   '("\\<$\\([[:alnum:]_]*\\)" 1 'web-mode-variable-name-face)

   '("\\(\"\\(.\\|\n\\)*?\"\\|'\\(.\\|\n\\)*?'\\)" 0 'web-mode-string-face t t)

   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   '("[^:\"]//.+" 0 'web-mode-comment-face t t)

   ))

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
   '("doctype" 
     "<!DOCTYPE html>\n")
   '("html5"
     "<!DOCTYPE html>\n<html>\n<head>\n<title></title>\n<meta charset=\"utf-8\" />\n</head>\n<body>\n" 
     "\n</body>\n</html>")
   )
  "Code snippets")

(defun web-mode-insert (code)
  "Prompt for snippet code."
  (interactive 
   (list (completing-read
          "Snippet: " 
          (web-mode-snippet-codes))))
  (web-mode-insert-snippet code))

(defun web-mode-snippet-codes ()
  "Snippet codes."
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

(defun web-mode-insert-snippet (code)
  "Insert snippet."
  (let (beg 
        (continue t) 
        (counter 0) 
        end 
        sel 
        snippet 
        (l (length web-mode-snippets)) 
        pt)
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
      (setq pt (point))
      (when sel 
        (insert sel)
        (setq pt (point)))
      (if (nth 2 snippet) (insert (nth 2 snippet)))
      (setq end (point-at-eol))
      (goto-char pt)
      (indent-region beg end))
    ))

(defun web-mode-insert-table ()
  "Insert HTML Table."
  (interactive)
  (web-mode-insert-snippet "table"))

(defun web-mode-insert-and-indent (text)
  "Insert and indent text."
  (interactive)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)))

(defun web-mode-match-tag ()
  "Match tag."
  (interactive)
  (let ((pt (point)))
    (when (not (web-mode-is-comment-or-string))
      (if (and (= (current-column) 0)
               (not (string= (string (char-after)) "<")))
          (progn
            (search-forward "<" nil t)
            (backward-char)
            ))
      (if (or (string= (string (char-after)) "<")
              (search-backward "<" 1 t))
          (if (string= (string (char-after (1+ (point)))) "?")
              (web-mode-match-php-tag)
            (web-mode-match-html-tag pt))))))

(defun web-mode-match-html-tag (pt)
  "Match HTML tag."
  (let (closing-tag nb tag)
    (forward-char)
    (setq closing-tag (string= (string (char-after)) "/"))
    (if (eq closing-tag t)
        (forward-char))
    (setq nb (skip-chars-forward "a-zA-Z"))
    (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
    (if (web-mode-is-void-html-element tag)
        (message "void tag")
      (if (eq closing-tag t)
          (web-mode-match-html-opening-tag tag pt)
        (web-mode-match-html-closing-tag tag pt)))))

(defun web-mode-match-html-closing-tag (tag pt)
  "Match closing HTML closing tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    (search-forward ">")
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0)
                (re-search-forward regexp nil t))
      (when (not (web-mode-is-comment-or-string))
        (setq n (1+ n))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
            (setq counter (- counter 1))
          (setq counter (+ counter 1))))
      )
    (if (> n 0)
        (search-backward "<" 1 t)
      (goto-char pt))
    ))

(defun web-mode-match-html-opening-tag (tag pt)
  "Match opening HTML tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    (search-backward "<")
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (when (not (web-mode-is-comment-or-string))
        (setq n (1+ n))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
            (setq counter (+ counter 1))
          (setq counter (- counter 1))))
      )
    (if (> n 0)
        ()
;;       (search-backward "<" 1 t)
      (goto-char pt))
    ))

(defun web-mode-match-php-tag ()
  "Match HTML tag."
  (let (beg end code regexp type)
    (forward-char)
    (setq beg (+ (point) 4))
    (search-forward ">")
    (setq end (- (point) 2))
    (setq code (buffer-substring-no-properties beg end))
    (if (string-match "for" code)
        (progn
          (if (string-match "foreach" code)
              (progn
                (setq regexp "<\\?php \\(foreach\\|endforeach\\)")
                (setq type   "foreach"))
            (progn
              (setq regexp "<\\?php \\(for\\|endfor\\)")
              (setq type   "for"))))
      (progn
        (setq regexp "<\\?php \\(if\\|else\\|elseif\\|endif\\)")
        (setq type   "if")))
    (if (string-match " end" code)
        (web-mode-match-opening-php-tag regexp type)
      (web-mode-match-closing-php-tag regexp type))))

(defun web-mode-match-opening-php-tag (regexp type)
  "Match PHP opening tag."
  (let ((counter 1) match)
    (search-backward "<")
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match " \\(if\\|for\\)" match)
          (progn 
            (setq counter (1- counter))
            )
        (progn
          (if (string-match " end\\(if\\|for\\)" match)
            (setq counter (1+ counter))))
        );; if
;;      (message "%s %d" (web-mode-current-trimmed-line) counter)
      );; while
    ))

(defun web-mode-match-closing-php-tag (regexp type)
  "Match PHP closing tag."
  (let ((counter 1) match)
    (while (and (> counter 0)
                (re-search-forward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match "<\\?php \\(if\\|for\\)" match)
          (setq counter (1+ counter))
        (progn
          (unless (and (> counter 1)
                       (string-match "else" match))
            (setq counter (1- counter)))
          )))
    (search-backward "<")))

(defun web-mode-debug ()
  (interactive)
  (what-cursor-position))

(defvar web-mode-autocompletes
  (list
   '("<?p" "hp  ?>" "\\?>" 3)
   '("<?=" "?>" "\\?>" 0)
   '("<!-" "-  -->" "--" 2)  
   '("<im" "g src=\"\" />" "src" 7))
  "Autocompletes")

(defun web-mode-on-after-change (beg end len)
  "Autocomplete"
;;  (message "beg=%d, end=%d, len=%d, cur=%d" beg end len (current-column))
  (let ((chunk "") 
        (pt (point))
        (cur-col (current-column))
        tag
        continue
        found
        (i 0)
        counter
        expr
        (l (length web-mode-autocompletes))
        pos-end
        after
        c)

    (when (and web-mode-autocompletes-flag
               (= len 0)
               (= 1 (- end beg)))
      (if (> (+ end 10) (line-end-position))
          (setq pos-end (line-end-position))
        (setq pos-end (+ end 10)))
      (setq after (buffer-substring-no-properties end pos-end))

      (when (and (>= cur-col 2)
                 (not found))
        (setq chunk (buffer-substring-no-properties (- beg 1) end))

        (if (string= "</" chunk)
            (progn
              (setq continue t)
              (setq counter 1)
              (while (and continue 
                          (re-search-backward "<\\(/?[[:alnum:]]+\\)" 0 t))
                (when (not (web-mode-is-comment-or-string))
                  (setq tag (substring (match-string-no-properties 0) 1))
                  (if (string= (substring tag 0 1) "/")
                      (setq counter (1+ counter))
                    (if (not (web-mode-is-void-html-element tag)) 
                        (setq counter (1- counter))
                      ))
                  (if (eq counter 0)
                      (progn
                        (setq continue nil)
                        (setq found t)
                        (goto-char pt)
                        (if (looking-at ">")
                            (progn
                              (insert tag)
                              (goto-char (+ (point) 1)))
                          (insert (concat tag ">")))
                        ))
                  ) ;; when
                ) ;; while
              (if continue (goto-char pt))
              ))
        );; when

      (when (and (>= cur-col 3)
                 (not found))
        (setq chunk (buffer-substring-no-properties (- beg 2) end))

        (while (and (< i l)
                    (not found))
          (setq expr (elt web-mode-autocompletes i))
;;          (message "%S" expr)
          (if (string= (elt expr 0) chunk)
              (unless (string-match (elt expr 2) after)
                (progn 
                   (insert (elt expr 1))
                   (goto-char (+ pt (elt expr 3)))
                   (setq found t))))
          (setq i (1+ i)))        
        ) ;; when

      (when (and (>= cur-col 6) 
                 (not found))

        (setq chunk (buffer-substring-no-properties (- beg 5) end))
        (cond
         ((string= "?php i" chunk)
          (unless (string-match "if" after)
            (progn
              (insert "f ():")
              (re-search-forward ">")
              (insert "\n<?php endif; ?>")
              (goto-char (+ pt 3)))))

         ((string= "?php f" chunk)
          (unless (string-match "for" after)
            (progn
              (insert "oreach ( as ):")
              (re-search-forward ">")
              (insert "\n<?php endforeach; ?>")
              (goto-char (+ pt 8)))))
        )
      ) ;; end when

    )))


(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (unload-feature 'web-mode)
  (web-mode)
  (web-mode-hook))

(provide 'web-mode)

;;; web-mode.el ends here

