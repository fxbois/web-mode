;;; web-mode.el --- major mode for editing PHP Templates

;; Copyright (C) 2011, 2012 François-Xavier Bois

;; =========================================================================
;; This work is sponsored by KerniX : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; Version: 1.03
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
  :version "1.03"
  :group 'languages)

(defgroup web-mode-faces nil
  "Faces for syntax highlighting."
  :group 'web-mode
  :group 'faces)

(defconst web-mode-debug nil
  "t if in debug mode.")

(defcustom web-mode-html-offset 2
  "HTML indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-css-offset 2
  "CSS indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-script-offset 2
  "General script indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-php-offset web-mode-script-offset
  "PHP indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-javascript-offset web-mode-script-offset
  "JavaScript indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-java-offset web-mode-script-offset
  "Java indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-autocompletes-flag (display-graphic-p)
  "Handle autocompletes."
  :type 'bool
  :group 'web-mode)

(defvar web-mode-file-type "html"
  "Buffer file type.")

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in web-mode buffers.")

(define-derived-mode web-mode prog-mode "Web"
  "Major mode for editing mixed HTML Templates."

  ;; syntax-table must be defined above
  (set-syntax-table web-mode-syntax-table)

  (make-local-variable 'font-lock-extend-region-functions)
  (make-local-variable 'font-lock-keywords)  
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-tabs-mode)  
  (make-local-variable 'require-final-newline)

  (cond

   ((string-match-p "\\.xml$" (buffer-file-name))
    (setq web-mode-file-type "xml")
    (setq font-lock-defaults '(web-mode-html-font-lock-keywords t t nil nil))
    (add-to-list 'font-lock-extend-region-functions 'web-mode-font-lock-extend-region)
    )

   ((string-match-p "\\.css$" (buffer-file-name))
    (setq web-mode-file-type "css")
    (setq web-mode-autocompletes-flag nil)
    (setq font-lock-defaults '(web-mode-css-font-lock-keywords t t nil nil))
;;    (add-to-list 'font-lock-extend-region-functions 'web-mode-font-lock-extend-css-region)
    )
   
   (t
    (setq web-mode-file-type "html")
    (setq font-lock-defaults '(web-mode-html-font-lock-keywords t t nil nil))
    (add-to-list 'font-lock-extend-region-functions 'web-mode-font-lock-extend-region)
    )

   )

  (setq font-lock-multiline t)
  (setq indent-line-function 'web-mode-indent-line)
  (setq indent-tabs-mode nil)
  (setq require-final-newline nil)

  (if (not (string= web-mode-file-type "css"))
      (add-hook 'after-change-functions 'web-mode-on-after-change t t)
    )

  (define-key web-mode-map (kbd "C-c C-(") 'web-mode-fetch-opening-paren)
  (define-key web-mode-map (kbd "C-c C-a") 'web-mode-indent-buffer)
  (define-key web-mode-map (kbd "C-c C-b") 'web-mode-beginning-of-element)
  (define-key web-mode-map (kbd "C-c C-d") 'web-mode-delete-element)
  (define-key web-mode-map (kbd "C-c C-i") 'web-mode-insert)
  (define-key web-mode-map (kbd "C-c C-j") 'web-mode-duplicate-element)
  (define-key web-mode-map (kbd "C-c C-n") 'web-mode-match-tag)
  (define-key web-mode-map (kbd "C-c C-p") 'web-mode-parent-html-element)
  (define-key web-mode-map (kbd "C-c C-r") 'web-mode-reload)
  (define-key web-mode-map (kbd "C-c C-s") 'web-mode-select-element)

  (define-key web-mode-map (kbd "C-$") 
    '(lambda ()
       (interactive)
       (if (web-mode-is-comment-or-string-line) (message "comment or string : yes") (message "comment or string : no"))
;;       (message (web-mode-previous-usable-line))
       ))
  
  ;; (define-key web-mode-map [menu-bar]
  ;;   (make-sparse-keymap))
  ;; (define-key web-mode-map [menu-bar web]
  ;;   (cons "Web" (make-sparse-keymap)))
  ;; (define-key web-mode-map [menu-bar web html]
  ;;   (cons "HTML" (make-sparse-keymap)))
  ;; (define-key web-mode-map [menu-bar web html insert-table]
  ;;   '("Insert TABLE" . web-mode-insert-table))
  ;; (define-key web-mode-map [menu-bar web sep1]
  ;;   '("--"))
  ;; (define-key web-mode-map [menu-bar web debug]
  ;;   '("debug" . web-mode-debug-point))

;;  (message "%S" (cdr web-mode-style-font-lock-keywords))
;;  (message "%S" (cdr web-mode-css-font-lock-keywords))

;;  (message "%S" (append 'web-mode-highlight-css-props web-mode-style-font-lock-keywords))
;;  (message "%S" (append (cdr web-mode-style-font-lock-keywords) '(web-mode-highlight-css-props)))

)

(defvar web-mode-hook nil
  "List of functions to be executed with web-mode.")

(defface web-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
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
  "Face for HTML attribute names."
  :group 'web-mode-faces)

(defface web-mode-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for HTML attribute values."
  :group 'web-mode-faces)

(defface web-mode-css-rule-face
  '((t :foreground "orchid3"))
  "Face for CSS rules."
  :group 'web-mode-faces)

(defface web-mode-css-pseudo-class-face
  '((t :foreground "plum2"))
  "Face for CSS pseudo-classes."
  :group 'web-mode-faces)

(defface web-mode-css-at-rule-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS at-rules."
  :group 'web-mode-faces)

(defface web-mode-css-prop-face
  '((t :foreground "Pink3"))
;;  '((t :foreground "Pink1"))
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

(defun web-mode-indent-buffer ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun web-mode-beginning-of-element ()
  "Fetch beginning of element"
  (interactive)
  (let (continue t)
    (while continue
      (re-search-backward "<[[:alpha:]]" nil t)
      (if (not (web-mode-is-comment-or-string)) (setq continue nil))
      )))

(defun web-mode-previous-usable-line ()
  "Move cursor to previous non blank/comment/string line and return this line (trimmed).
point is at the beginning of the line."
  (interactive)
  (let ((continue t) 
        (line "")
        (pt (point)))
    (beginning-of-line)
    (while (and continue
                (not (bobp))
                (forward-line -1))
      (if (not (web-mode-is-comment-or-string-line))
          (setq line (web-mode-trim (buffer-substring-no-properties
                                     (point) (line-end-position)))))
      (when (not (string= line "")) (setq continue nil))
      )
    (if (string= line "") 
        (progn
          (goto-char pt)
          nil) 
      line)
    ))

(defun web-mode-is-comment-or-string (&optional pt)
  "Detect if point is in a comment or in a string."
  (interactive)
  (unless pt
    (setq pt (point)))
  (let ((face (get-text-property pt 'face))) 
    (or (eq face 'web-mode-string-face)
        (eq face 'web-mode-comment-face))))

(defun web-mode-is-comment-or-string-line ()
  "Detect if current line is in a comment or in a string."
  (let ((continue t)
        (counter 0))  
    (save-excursion
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-comment-or-string)
            (setq counter (+ counter 1))
          (when (not (char-equal (following-char) ?\s))
            (setq continue nil
                  counter 0))
          );;if
        (forward-char)
        );;while
      (> counter 0)
      )))

(defun web-mode-in-php-block ()
  "Detect if point is in a PHP block."
  (let (line-current line-open line-close)
    (save-excursion
;;      (message "current(%d)" (web-mode-current-line-number))
;;      (end-of-line)
      (setq line-current (web-mode-current-line-number))
      (and (re-search-backward "<\\?[p=]" nil t)
           (setq line-open (web-mode-current-line-number))
           (search-forward "?>" nil t)
           (setq line-close (web-mode-current-line-number))
;;           (progn (message "current(%d) from(%d) to(%d)" line-current line-from line-to) 't)
           (not (eq line-open line-close))
           (>= line-close line-current)
           ))))

(defun web-mode-in-block (open close)
  "Detect if point is in a block delimited by open and close."
  (let (line-current line-open line-close)
    (save-excursion
      (setq line-current (web-mode-current-line-number))
      (and (search-backward open nil t)
           (setq line-open (web-mode-current-line-number))
           (search-forward close nil t)
           (setq line-close (web-mode-current-line-number))
           (not (eq line-open line-close))
           (>= line-close line-current)
           ))))

(defun web-mode-in-html-block (type)
  "Detect if point is in a block"
  (let ((pt (point))
        (line-current (web-mode-current-line-number))
        line-open line-close)
    (save-excursion
      (and (search-backward (concat "<" type) 0 t)
           (setq line-open (web-mode-current-line-number))
;;           (> line-current line-open)
           (search-forward (concat "</" type ">") nil t)
           (< pt (point))
           (setq line-close (web-mode-current-line-number))
           (not (eq line-open line-close))
           (<= line-current line-close)
           (progn (goto-char pt) (beginning-of-line) t)
           ;;           (message "-> %s" type)
           (not (looking-at (concat "[ \t]*</" type)))
           ))
    ))

(defun web-mode-current-line-number (&optional pt)
  "Return line number at point."
  (unless pt
    (setq pt (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pt)
                 (if (= (current-column) 0) 1 0)))
    ;;    (message "%d [%d] %s" pt out (web-mode-text-at-point))
    ret))

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
        in-jsp-block
        in-script-block
        in-style-block
        line-number
        local-offset
        offset
        prev-last-char 
        prev-line 
        pt)
;;    (message "%S" (syntax-ppss))
    (save-excursion
      (setq cur-column (current-column)
            cur-line-number (web-mode-current-line-number)
            cur-indentation (current-indentation)
            cur-point (point))
      (end-of-line)
      (if (string= web-mode-file-type "css")
          (progn
            (setq in-style-block t)
            (setq local-offset web-mode-css-offset)
            )
        (cond

         ((web-mode-in-php-block)
          (setq in-php-block t)
          (setq local-offset web-mode-php-offset))

         ((web-mode-in-html-block "script")
          (setq in-script-block t)
          (setq local-offset web-mode-javascript-offset))

         ((web-mode-in-html-block "style")
          (setq in-style-block t)
          (setq local-offset web-mode-css-offset))

         ((web-mode-in-block "<%" "%>")
          (setq in-jsp-block t)
          (setq local-offset web-mode-java-offset))

         (t
          (setq local-offset web-mode-html-offset))

         )
        );;if

;;      (message "php(%S) jsp(%S) script(%S) style(%S)" in-php-block in-jsp-block in-script-block in-style-block)
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
      (end-of-line)

;;          (message "prev-line(%s)" prev-line)
          
      (cond ;; switch language
       
       ((null prev-line)
        ;;          (message "nothing to check")
        (setq offset 0)
        )
       
       ((or in-php-block in-jsp-block in-script-block) 
        
        (cond
         
         ((string-match-p "^\\(<\\?php\\|<%\\|[?%]>\\)" cur-line)
          (setq offset 0)
          )

         ((string= cur-first-char "}")
          (web-mode-fetch-opening-paren "{")
          (setq offset (current-indentation))
          )
         
         ((string= cur-first-char "?")
          (rsb "[=(]")
          (setq offset (current-column))
          )
         
         ((string= cur-first-char ":")
          (setq offset (current-indentation))
          )
         
         ((string= prev-last-char ",")
          (web-mode-fetch-opening-paren "(" cur-point)
          (setq offset (+ (current-column) 1))
          )
         
         ((or (string= prev-last-char ".") 
              (string= prev-last-char "+")
              (string= prev-last-char "?") 
              (string= prev-last-char ":"))
          (rsb "[=(]")
          (skip-chars-forward "= (")
          (setq offset (current-column))
          )
         
         ((string= prev-last-char ";")
          ;;            (end-of-line)
          (if (string-match-p ")[ ]*;$" prev-line)
              (progn 
                (re-search-backward ")[ ]*;")
                (web-mode-fetch-opening-paren))
            (progn
              (re-search-backward "\\([=(]\\|^[[:blank:]]*var[ ]*\\)"))
            )
          (setq offset (current-indentation))
          )
        
         ((string= prev-last-char "{")
;;          (setq offset (+ (current-indentation) web-mode-script-offset))
          (setq offset (+ (current-indentation) local-offset))
          )
         
         ((and in-php-block
               (string-match-p "\\(->[ ]?[[:alnum:]_]+\\|)\\)$" prev-line))
          ;;            (end-of-line)
          (search-backward ">")
          (setq offset (- (current-column) 1))
          )
         
         (t
          ()
          ;;            (message "script block : unknown")
          )
         
         )) ;; end case script block
       
       (in-style-block ;; style block
        
        (goto-char cur-point)
        
        (cond
         
         ((or (string-match-p "\\(^}\\|{\\)" cur-line)
              (not (web-mode-in-block "{" "}")))
          (setq offset 0)
          )
         
         ((and (not (string= "" cur-line))
               (not (string-match-p "^[[:alpha:]-]+[ ]?:" cur-line)))
          (re-search-backward ":")
          (skip-chars-forward ":  ")
          (setq offset (current-column))            
          )
         
         (t
          (setq offset local-offset)
          )
         
         )) ;; end case style bloc
       
       (t ;; case html bloc
        
        (cond
         
         ((and (not (string= cur-first-char "<"))
               (string-match-p "\\<[[:alpha:]-]+=\".*?\"$" prev-line))
          (re-search-backward "<[[:alpha:]]")
          (re-search-forward "<[[:alpha:]]+")
          (skip-chars-forward " ")
          (setq offset (current-column))
          )
         
         ((and (string-match-p "^\\<[[:alpha:]-]+=\"" cur-line)
               (re-search-backward "\\<[[:alpha:]-]+=\"[^\"]*\"$" nil t))
          (setq offset (current-column))
          )
         
         ((and (string= web-mode-file-type "xml")
               (or (string-match-p "^</?\\(head\\|body\\|meta\\|link\\|title\\|style\\|script\\)" cur-line)
                   (string-match-p "^<\\?php \\(if\\|else\\|for\\|while\\|end\\)" cur-line)))
          (setq offset 0)
          )
         
         ((string-match-p "^</" cur-line)
          (goto-char cur-point)
          (beginning-of-line)
          (re-search-forward "</")
          (web-mode-match-tag)
          (setq offset (current-indentation))
          )
         
         ((or (string= "" cur-line)
              (string-match-p "^<[[:alpha:]]" cur-line))
          (setq continue t)
          (while (and continue
                      (re-search-backward "^[[:blank:]]*</?[[:alpha:]]" nil t))
            (when (and (not (web-mode-is-comment-or-string))
                       (not (looking-at-p "[[:blank:]]*</?\\(script\\|style\\)")))
              (setq continue nil)
              (unless (char-equal (following-char) ?<)
                ;;                (unless (string= (string (following-char)) "<")
                (re-search-forward "<"))
              (setq offset (+ (current-indentation) 
                              (if (and (web-mode-is-opened-element (web-mode-element-at-point))
                                       (not (looking-at-p "[[:blank:]]*<body")))
                                  local-offset
                                0)
                              ))
              );;when
            );;while
          )
         
         (t
          ()
          )
         
         )) ;; end case html bloc

       ) ;; end switch language bloc

      ;;          )) ;; not null prev-line

      ) ;; save-excursion

    
    (when (and offset
               (not (eq cur-indentation offset))) 
      (setq offset (max 0 offset))  
      (indent-line-to offset))
    
    (if (< (current-column) (current-indentation)) (back-to-indentation))
    
    ) ;; let
  )

(defun rsf (regexp &optional limit noerror)
  "re-search-forward not in comment or string."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil))
      )
    ret);;let
  )

(defun sf (expr &optional limit noerror)
  "search-forward not in comment or string."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret)
  )

(defun rsb (regexp &optional limit noerror)
  "re-search-backward not in comment or string."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil))
      
      ;;      (message (web-mode-current-trimmed-line))
      )
    ret);;let
  )

(defun web-mode-fetch-opening-paren (&optional paren pt)
  "Fetch opening paren."
  (interactive)
  (unless paren (setq paren "("))
  (unless pt (setq pt (point)))
;;  (message (web-mode-text-at-point))
  (let ((continue t) 
        (n 0)
        regexp)

    (cond

     ((string= paren "(")
      (setq regexp "[)(]"))

     ((string= paren "{")
      (setq regexp "[}{]"))

     ((string= paren "{")
      (setq regexp "[\]\[]"))

     );;cond

    (while (and continue
                (re-search-backward regexp nil t))
      (unless (web-mode-is-comment-or-string)
        (if (string= (string (char-after)) paren)
            (progn 
              (setq n (1+ n))
              (if (> n 0) (setq continue nil)))
          (setq n (1- n))))
      )
    );;let
;;  (message (web-mode-current-trimmed-line))
  )

(defun web-mode-count-char-in-string (char &optional string)
  "Count char in string."
  (let ((i 0) (n 0) l)
    (setq l (length string))
    (while (< i l)
      (if (char-equal (elt string i) char)
          (setq n (1+ n)))
      (setq i (1+ i)))
    n))

(defun web-mode-element-at-point ()
  "Return element at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((continue t)
          cont
          line l1 l2
          (pt (point)))
      (while continue
        (setq l1 (web-mode-current-line-number))
        (end-of-line)
        (setq cont t)
        (while cont
          (re-search-backward "<[[:alpha:]/]" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        (setq cont t)
        (while cont
          (re-search-forward "[[:alnum:] /\"']>" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        (setq l2 (web-mode-current-line-number))
        (if (eq l1 l2) (setq continue nil))
        )
      (end-of-line)
      (setq line (buffer-substring-no-properties pt (point)))
      (setq line (replace-regexp-in-string "[\r\n]" "" line))
;;      (message "elt at point: %s" line)
      line
      )))

(defun web-mode-select-element ()
  "Select the current HTML element."
  (interactive)
  (when (or (looking-at-p "<[[:alpha:]]") 
            (and (goto-char (+ 1 (point)))
                 (rsb "<[[:alpha:]]")))
    (set-mark (point))
    (web-mode-match-tag)
    (search-forward ">")))

(defun web-mode-delete-element ()
  "Delete the current HTML element"
  (interactive)
  (web-mode-select-element)
  (when mark-active
    (delete-region (region-beginning) (region-end))))

(defun web-mode-duplicate-element ()
  "Duplicate the current HTML element."
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
      (yank))))

(defun web-mode-is-opened-element (&optional line)
  "Is there any HTML element without a closing tag ?"
  (interactive)
  (let ((deb 0)
        is-closing-tag
        is-void-elemnt
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
;;    (while (string-match "<\\(/?[[:alpha:]:_]+\\)" line deb)
    (while (string-match web-mode-tag-regexp line deb)
      (setq deb (match-end 0)
            tag (match-string 1 line)
            is-closing-tag (string= (substring tag 0 1) "/"))
      (if is-closing-tag (setq tag (substring tag 1)))
      (setq n (gethash tag h 0))      
      (setq deb (string-match "/?>" line deb))
      (setq is-void-element (string= (substring (match-string 0 line) 0 1) "/"))
      
      (if (or is-void-element (web-mode-is-void-element tag))
          (progn
;;            (message "void tag: %s" tag)
            )
        (progn
          (if is-closing-tag
              (if (> n 0) (puthash tag (1- n) h))
            (puthash tag (1+ n) h))
          ))

      );; while
        
    ;;(message (number-to-string (gethash "strong" h)))
    ;;(message (number-to-string (hash-table-count h)))
    (maphash '(lambda (k v) (if (> v 0) (setq ret 't))) h)
    ;;    (if ret (message "line=%s: opened" line) (message "line=%s: closed" line))
    ret
    )
  )

(defun web-mode-parent-html-element ()
  "Fetch parent element."
  (interactive)
  (let (pt 
        is-closing-tag
        tag
        n
        (continue t)
        (h (make-hash-table :test 'equal)))
    (save-excursion
;;      (unless (string= (string (char-after)) "<")
;;        (progn
      ;;          (forward-char)
;;;;          (search-forward ">") ;; todo : verifier que l'on est pas dans une string
      ;;          (re-search-backward "<[[:alnum:]]+[ ><$]" nil t)))
      (while (and continue
;;                  (re-search-backward "</?[[:alnum:]]+[/ ><$]" nil t))
                  (rsb "</?[[:alnum:]]+[/ ><$]"))
        (message "ici")
        (forward-char)
;;        (setq is-closing-tag (string= (string (char-after)) "/"))
        (setq is-closing-tag (char-equal (char-after) ?/))
        (if (eq is-closing-tag t) (forward-char))
        (setq nb (skip-chars-forward "[:alnum:]"))
        (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
        (setq n (gethash tag h 0))
;;        (message "%s %d %d" tag n (point))
        (when (not (web-mode-is-void-element tag))
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

(defun web-mode-text-at-point (&optional pt)
  "Text at point."
  (unless pt
    (setq pt (point)))
  (buffer-substring-no-properties pt (line-end-position)))

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

(defun web-mode-is-void-element (&optional tag)
  "Test if tag is a void tag."
  (if tag
      (progn
        (find (downcase tag) web-mode-void-elements :test 'equal))
    (looking-at-p "<[^>]+/>")))

(defconst web-mode-void-elements
  '("hr" "br" "col" "input" "link" "meta" "img" 
    "tmpl_var" 
    "h:inputtext" 
    "#include" "#assign" "#import" "#else")
  "Void (self-closing) tags.")

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

(defconst web-mode-css-at-rules
  (eval-when-compile
    (regexp-opt
     '("charset" "import" "media" "page" "font-face" "namespace")))
  "CSS at-rules.")

(defconst web-mode-css-pseudo-classes
  (eval-when-compile
    (regexp-opt
     '("active" "after" "before" "checked" "disabled" "empty" "enabled" 
       "first" "first-child" "first-letter" "first-line" "first-of-type"
       "focus" "hover" "lang" "last-child" "last-of-type" "left" "link"
       "not" "nth-child" "nth-last-child" "nth-of-type"
       "only-child" "only-of-type"
       "right" "root" "selection" "target" "visited")))
  "CSS pseudo-classes (and pseudo-elements).")

(defconst web-mode-jsp-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-jsp-keywords) web-mode-jsp-keywords '())
           '("if" "else" "for" "while" "do" "case" "function"
             "new" "page" "include" "tag" "taglib" "package" "try" "catch"
             "throw" "throws"
             "return"
             ;;erb
             "in" "end"
             )))
  "JSP keywords.")

(defconst web-mode-js-keywords
  (eval-when-compile
    (regexp-opt
     '("function" "for" "if" "var" "while" "new" "try" "catch")))
  "JavaScript keywords.")

(defun web-mode-highlight-css-props (limit)
  "Highlight css props."
  (let ((font-lock-keywords web-mode-css-props-font-lock-keywords)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max))
        open close)

;;    (message "limit=%s" limit)
    (when (search-forward "{" limit t)
      (setq open (point))
      (search-forward "}" limit t)
      (setq close (- (point) 1))
      (font-lock-fontify-region open close)
      )

    ))

(defun web-mode-highlight-client-blocks (limit)
  "Highlight client blocks."
  (let ((font-lock-keywords nil)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max))
        open close closing-string chunk pt)

    (when (re-search-forward "<\\(style\\|script\\)[^>]*>" limit t)
      (setq open (point)
            chunk (substring (match-string 0) 0 3))

      (cond

       ((string= "<sc" chunk) 
        (setq font-lock-keywords web-mode-script-font-lock-keywords
              closing-string "</script>")
        )
       
       ((string= "<st" chunk) 
        (setq font-lock-keywords web-mode-style-font-lock-keywords
              closing-string "</style>")
        )
       
       );;cond

;;      (message "%s - %s" chunk closing-string)

      (when (and closing-string
                 (search-forward closing-string limit t))
        (setq close (match-beginning 0)
              pt (point))
;;        (message "%d %d" open close)
        (font-lock-fontify-region open close)



        (when (string= "<st" chunk)
;;          (message "%s" chunk)
          (goto-char open)
          (while (and (re-search-forward "{\\([^}]+\\)}" limit t)
                      (< (point) pt))
;;            (message "%s %d %d" (match-string 1) (match-beginning 1) (match-end 1))
            (setq font-lock-keywords web-mode-css-props-font-lock-keywords)
            (font-lock-fontify-region (match-beginning 1) (match-end 1))
            );;while
          (goto-char pt)
          )

        pt
        )

      );; when

    ))

(defun web-mode-highlight-server-blocks (limit)
  "Highlight server blocks."
  (let ((font-lock-keywords nil)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        (limit (point-max))
        open close closing-string chunk)
    (when (re-search-forward "\\(<\\?php\\|<\\?=\\|<%\\)" limit t)
      (setq open (point)
            chunk (substring (match-string 0) 0 2))
      (cond

       ((string= "<?" chunk) 
        (setq font-lock-keywords web-mode-php-font-lock-keywords
              closing-string "?>"))

       ((string= "<%" chunk) 
        (setq font-lock-keywords web-mode-jsp-font-lock-keywords
              closing-string "%>"))

       );;cond
      (when (and closing-string
                 (search-forward closing-string limit t))
        (setq close (match-beginning 0))
;;        (message "%s %d" (buffer-substring-no-properties open close) open)
        (font-lock-fontify-region open close)
        ))
    ))

(defun web-mode-font-lock-extend-css-region ()
 "Extend CSS region."
 (message "CSS beg(%d) end(%d) max(%d)" font-lock-beg font-lock-end (point-max))
 (save-excursion
;;   (when (search-backward "{" nil t)
;;     (beginning-of-line)
;;     (setq font-lock-beg (point))
;;     )
;;   (setq font-lock-end (point-max))
   )
 )

(defun web-mode-font-lock-extend-region ()
  "Extend HTML region."
  (message "beg(%d) end(%d) max(%d)" font-lock-beg font-lock-end (point-max))
  (save-excursion
    (goto-char font-lock-beg)
    (unless (looking-at-p "[ \t]*<")
      (when (re-search-backward "<[[:alpha:]%?]" nil t)
        (beginning-of-line)
        (setq font-lock-beg (point))
;;        (message "beg(%d) %s" font-lock-beg (web-mode-current-trimmed-line))
        )
      )

    ;; (goto-char font-lock-end)
    ;; (if (re-search-forward "[[:alpha:]]>[ ]*$" nil t)
    ;;     (progn
    ;;       (setq font-lock-end (point))
    ;;       )
    ;;   (setq font-lock-end (point-max))
    ;;   ) 
    ;; (message "end(%d) %s" font-lock-end (web-mode-current-trimmed-line))

    (setq font-lock-end (point-max))

    ) ;; save-rec
  )

(defconst web-mode-html-font-lock-keywords
  (list
;;   '("</?[[:alpha:]][[:alnum:]:_]\\{0,16\\}?\\(>\\|<\\|[ ]\\|/\\|$\\)"
   '("</?[[:alpha:]][[:alnum:]:_]*?\\(>\\|<\\|[ ]\\|/\\|$\\)"
     0 'web-mode-html-tag-face t t)
   '("\\(</?\\|/?>\\)"
     0 'web-mode-html-tag-face t t)
   '("[[:blank:]^>]\\([[:alnum:]_-]+=\\)\\(\"[^\"]*\"\\)?" 
     (1 'web-mode-html-attr-name-face t t)
     (2 'web-mode-html-attr-value-face t t))
   ;; Unified Expression Language
   '("\\([$#]{\\)\\([^}]+\\)\\(}\\)" 
     (1 'web-mode-preprocessor-face t t)
     (2 'web-mode-variable-name-face t t)
     (3 'web-mode-preprocessor-face t t))
   '(web-mode-highlight-client-blocks)
   '(web-mode-highlight-server-blocks)
   '("\\(<\\?[ph=]*\\|<%[!@=]?\\|[%?]>\\)" 0 'web-mode-preprocessor-face t t)
   '("\\`<\\(!D\\|\\?x\\)[^>]*>" 0 'web-mode-doctype-face t t)
   '("<[!#]--\\(.\\|\n\\)*?-->" 0 'web-mode-comment-face t t)
   '("<%\\(#\\|--\\)\\(.\\|\n\\)*?%>" 0 'web-mode-comment-face t t)
   '("</?\\([#@][[:alpha:]._]+\\|[[:alpha:]]+[:_][[:alpha:]]+\\)"
     (1 'web-mode-preprocessor-face t t))
   ))

(defconst web-mode-style-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("[^][#*~)(>,+{}@.:]" 0 'web-mode-css-rule-face)
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>") 
         '(1 'web-mode-css-pseudo-class-face t t))
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") 
         '(1 'web-mode-css-at-rule-face t t))   
   '("\\[\\(.+\\)\\]" (1 nil t t))
   '("(\\(.+\\))" (1 nil t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defvar web-mode-css-font-lock-keywords
  (append (cdr web-mode-style-font-lock-keywords) '(web-mode-highlight-css-props))
  )

(defconst web-mode-css-props-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("[[:alpha:]-]\\{3,\\}[ ]?:" 0 'web-mode-css-prop-face)
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
;;   '("(\\([^)]+\\))" (1 'web-mode-string-face t t))
   '("![ ]?important" 0 font-lock-builtin-face t t)
   '("#[[:alnum:]]\\{3,6\\}" 0 font-lock-builtin-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-script-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("\\<\\([[:alnum:]_.]+\\)[ ]?(" 1 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-js-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]//.+" 0 'web-mode-comment-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-jsp-font-lock-keywords
  (list
   '(".*" 0 nil t t)
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face t t)
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]//.+" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-php-font-lock-keywords
  (list

   '(".*" 0 nil t t)

   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))

   (cons (concat "(\\s-*\\(" web-mode-php-types "\\)\\s-*)") 
         '(1 'web-mode-type-face))

   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") 
         '(0 'web-mode-constant-face))

   ;; func(
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)

   ;; Class::CONSTANT
   '("[[:alnum:]_][ ]?::[ ]?\\(\\sw+\\)" 1 'web-mode-constant-face)

   ;; ->variable
   '("->[ ]?\\(\\sw+\\)" 1 'web-mode-variable-name-face)

   ;; Class::
   '("\\<\\(\\sw+\\)[ ]?::" 1 'web-mode-type-face)

   '("instanceof[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-type-face)

   ;; $var
   '("\\<$\\(\\sw*\\)" 1 'web-mode-variable-name-face)

   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)

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
  (let ((pt (point))
        tag)
    (when (not (web-mode-is-comment-or-string))
      (when (and (= (current-column) 0)
               (not (string= (string (char-after)) "<")))
        (search-forward "<" nil t)
        (backward-char))

      (when (or (char-equal (char-after) ?<)
                (search-backward "<" 1 t))
        
        (setq tag (buffer-substring-no-properties (+ 1 (point)) (+ 3 (point))))

        (cond
         
         ((string= tag "?p")
          (web-mode-match-php-tag)
          )

         (t
          (web-mode-match-html-tag pt)
          )
         
         );;cond
        
        );; when

      )))

(defun web-mode-match-html-tag (pt)
  "Match HTML tag."
  (let (closing-tag nb tag)
    (forward-char)
;;    (setq closing-tag (string= (string (char-after)) "/"))
    (setq closing-tag (char-equal (char-after) ?/))
    (if (eq closing-tag t)
        (forward-char))
    (setq nb (skip-chars-forward "a-z:A-Z"))
    (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
    (if (web-mode-is-void-element tag)
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


;; c:forEach c:forTokens c:if
(defun web-mode-match-jsp-tag ()
  "Match JSP tag."
  (let (beg end code regexp type)
    (message "jsp")
    )
  )

(defun web-mode-match-php-tag ()
  "Match PHP tag."
  (let (beg end code regexp type)
    (forward-char)
    (setq beg (+ (point) 4))
    (search-forward ">")
    (setq end (- (point) 2))
    (setq code (buffer-substring-no-properties beg end))
    (if (string-match-p "for" code)
        (progn
          (if (string-match-p "foreach" code)
              (progn
                (setq regexp "<\\?php \\(foreach\\|endforeach\\)")
                (setq type   "foreach"))
            (progn
              (setq regexp "<\\?php \\(for\\|endfor\\)")
              (setq type   "for"))))
      (progn
        (setq regexp "<\\?php \\(if\\|else\\|elseif\\|endif\\)")
        (setq type   "if")))
    (if (string-match-p " end" code)
        (web-mode-match-opening-php-tag regexp type)
      (web-mode-match-closing-php-tag regexp type))))

(defun web-mode-match-opening-php-tag (regexp type)
  "Match PHP opening tag."
  (let ((counter 1) match)
    (search-backward "<")
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p " \\(if\\|for\\)" match)
          (progn 
            (setq counter (1- counter))
            )
        (progn
          (if (string-match-p " end\\(if\\|for\\)" match)
            (setq counter (1+ counter))))
        );; if
;;      (message "%s %d" (web-mode-current-trimmed-line) counter)
      );; while
    ))

(defun web-mode-match-closing-php-tag (regexp type)
  "Match PHP closing tag."
  (let ((counter 1) 
        match)
    (while (and (> counter 0)
                (re-search-forward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "<\\?php \\(if\\|for\\)" match)
          (setq counter (1+ counter))
        (progn
          (unless (and (> counter 1)
                       (string-match-p "else" match))
            (setq counter (1- counter)))
          )))
    (search-backward "<")))

(defun web-mode-debug-point ()
  (interactive)
  (what-cursor-position))

(defvar web-mode-autocompletes
  (list
   '("<?p" "hp  ?>" "\\?>" 3)
   '("<?=" "?>" "\\?>" 0)
   '("<!-" "-  -->" "--" 2))
  "Autocompletes")

(defvar web-mode-tag-regexp "<\\(/?[[:alpha:]@#][[:alnum:]:_]*\\)"
  "Regular expression for HTML/XML regexp.")


(defun web-mode-on-after-change (beg end len)
  "Autocomplete"
;;  (message "beg=%d, end=%d, len=%d, cur=%d" beg end len (current-column))
  (let ((chunk "") 
;;        (case-fold-search t)
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

      (when (and (not found)
                 (>= cur-col 2)
                 (string= "</" (buffer-substring-no-properties (- beg 1) end)))
        (setq continue t
              counter 1)
        (while (and continue 
                    (re-search-backward web-mode-tag-regexp 0 t))
          (when (not (web-mode-is-comment-or-string))
            (setq tag (substring (match-string-no-properties 0) 1))
            (if (string= (substring tag 0 1) "/")
                (setq counter (1+ counter))
              (if (not (or (web-mode-is-void-element tag) 
                           (web-mode-is-void-element))) 
                  (progn
                    (setq counter (1- counter)))
                (message "tag %s is void" tag)
                ))
            (if (eq counter 0)
                (progn
                  (setq continue nil
                        found t)
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
        
        );; when

      (when (and (>= cur-col 3)
                 (not found))
        (setq chunk (buffer-substring-no-properties (- beg 2) end))

        (while (and (< i l)
                    (not found))
          (setq expr (elt web-mode-autocompletes i))
;;          (message "%S" expr)
          (if (string= (elt expr 0) chunk)
              (unless (string-match-p (elt expr 2) after)
                (progn 
                   (insert (elt expr 1))
                   (goto-char (+ pt (elt expr 3)))
                   (setq found t))))
          (setq i (1+ i)))        
        ) ;; when

    )

    ))

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (unload-feature 'web-mode)
  (web-mode)
  (if (fboundp 'web-mode-hook)
      (web-mode-hook))
  )

(provide 'web-mode)

;;; web-mode.el ends here





;; (defun web-mode-highlight-style-block (limit)
;;   "Highlight a <style> bloc."
;;   (let ((font-lock-keywords web-mode-style-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)
    
;;     (when (re-search-forward "<style[^>]*>" limit t)
;;       (setq open (point))
;;       (when (search-forward "</style>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

;;     ;; (when (re-search-forward
;;     ;;        "<style>\\(\\(:.\\|\n\\)+\\)</style>"
;;     ;;        limit t)
;;     ;;   (message "%d %d" (match-beginning 1) (match-end 1))
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))
    
;;     ))

;; (defun web-mode-highlight-script-block (limit)
;;   "Highlight a <script> bloc."
;;   (let ((font-lock-keywords web-mode-script-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "<script[^>]*>" limit t)
;;       (setq open (point))
;;       (when (search-forward "</script>" limit t)
;;         (setq close (match-beginning 0))
;; ;;        (message "%d %d" open close)
;;         (font-lock-fontify-region open close)
;;         )
;;       )
    
;;     ;; (when (re-search-forward
;;     ;;        "<script.*?>\\(.\\|\n\\)*?</script>"
;;     ;;        limit t)
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))
    
;; ))

;; (defun web-mode-highlight-php-block (limit)
;;   "Highlight a PHP bloc."
;;   (let ((font-lock-keywords web-mode-php-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "\\(<\\?php\\|<\\?=\\)" limit t)
;;       (setq open (point))
;;       (when (search-forward "?>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

    
;;     ;; (when (re-search-forward
;;     ;;        "\\(<\\?php\\|<\\?=\\)\\(.\\|\n\\)*?\\?>"
;;     ;;        limit t)
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))

;;     ))

;; (defun web-mode-highlight-jsp-block (limit)
;;   "Highlight a JSP bloc."
;;   (let ((font-lock-keywords web-mode-jsp-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "<%[ \n!@=]" limit t)
;;       (setq open (point))
;;       (when (search-forward "%>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

;; ;;    (when (re-search-forward "<%\\(.\\|\n\\)*?%>" limit t)
;; ;;      (font-lock-fontify-region (match-beginning 0) (match-end 0)))

;;     ))



;;  (set (make-local-variable 'font-lock-defaults) '(web-mode-font-lock-keywords))
;;  (set (make-local-variable 'font-lock-keywords-only) t)
;;  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
;;  (set (make-local-variable 'font-lock-syntax-table) nil)
;;  (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil)



;; (defun web-mode-in-jsp-block ()
;;   "Detect if point is in a JSP block."
;;   (let (line-current line-open line-close)
;;     (save-excursion
;;       (setq line-current (web-mode-current-line-number))
;;       (and (search-backward "<%" nil t)
;;            (setq line-open (web-mode-current-line-number))
;;            (search-forward "%>" nil t)
;;            (setq line-close (web-mode-current-line-number))
;;            (not (eq line-open line-close))
;;            (>= line-close line-current)
;;            ))))
