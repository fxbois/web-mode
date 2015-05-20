;;; web-mode-indentation.el --- indentation functionality for web-mode.el
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

(defun web-mode-point-context (pos)
  "POS should be at the beginning of the indentation."
  (save-excursion
    (let (curr-char curr-indentation curr-line
          language
          reg-beg reg-col
          prev-char prev-indentation prev-line
          token)

      (setq reg-beg (point-min)
            reg-col 0
            token "live"
            language ""
            prev-line ""
            prev-char 0)
      (cond

       ((bobp)
        (setq language "html")
        )

       ((string= web-mode-content-type "css")
        (setq language "css"
              curr-indentation web-mode-css-indent-offset))

       ((member web-mode-content-type '("javascript" "json"))
        (setq language "javascript"
              curr-indentation web-mode-code-indent-offset))

       ((member web-mode-content-type '("jsx"))
        (setq language "jsx"
              curr-indentation web-mode-code-indent-offset)
        (when (and (> pos (point-min))
                   (get-text-property pos 'part-expr)
                   (get-text-property (1- pos) 'part-expr))
          (setq language "javascript")
          (setq reg-beg (1+ (previous-single-property-change pos 'part-expr)))
          (goto-char reg-beg)
          (setq reg-col (current-column))
          )
        )

       ((string= web-mode-content-type "php")
        (setq language "php"
              curr-indentation web-mode-code-indent-offset))

       ((or (string= web-mode-content-type "xml"))
        (setq language "xml"
              curr-indentation web-mode-markup-indent-offset))

       ((and (get-text-property pos 'tag-beg)
             (get-text-property pos 'tag-name)
             (not (get-text-property pos 'part-side)))
        (setq language "html"
              curr-indentation web-mode-markup-indent-offset)
        )

       ((and (get-text-property pos 'block-side)
             (not (get-text-property pos 'block-beg)))

        (setq reg-beg (or (web-mode-block-beginning-position pos) (point-min)))
        ;;(message "reg-beg=%S" reg-beg)
        (goto-char reg-beg)
        (setq reg-col (current-column))
        (setq language web-mode-engine)
        (setq curr-indentation web-mode-code-indent-offset)

        (cond
         ((string= web-mode-engine "blade")
          (save-excursion
            (when (web-mode-rsf "{{[ ]*")
              (setq reg-col (current-column))))
          (setq reg-beg (+ reg-beg 2))
          ;;(message "beg=%S" reg-beg)
          ;;reg-col (+ reg-col 2)
          )
         ((string= web-mode-engine "razor")
          (setq reg-beg (+ reg-beg 2))
          )
         ;; tests/demo.chtml
         ((string= web-mode-engine "ctemplate")
          (save-excursion
            (when (web-mode-rsf "{{#?")
              (setq reg-col (current-column))))
          )
         ((string= web-mode-engine "dust")
          (save-excursion
            (when (web-mode-rsf "{@")
              (setq reg-col (current-column))))
          )
         ((string= web-mode-engine "template-toolkit")
          (setq reg-beg (+ reg-beg 3)
                reg-col (+ reg-col 3))
          )
         ((and (string= web-mode-engine "jsp")
               (web-mode-looking-at "<%@\\|<[[:alpha:]]" reg-beg))
          (save-excursion
            (goto-char reg-beg)
            (looking-at "<%@[ ]*[[:alpha:]]+[ ]+\\|</?[[:alpha:]]+:[[:alpha:]]+[ ]+")
            (goto-char (match-end 0))
            (setq reg-col (current-column))
            )
          )
         ((and (string= web-mode-engine "freemarker")
               (web-mode-looking-at "<@\\|<%@\\|<[[:alpha:]]" reg-beg))
          (save-excursion
            (goto-char reg-beg)
            (looking-at "<@[[:alpha:].]+[ ]+\\|<%@[ ]*[[:alpha:]]+[ ]+\\|<[[:alpha:]]+:[[:alpha:]]+[ ]+")
            (goto-char (match-end 0))
            (setq reg-col (current-column))
            )
          )
         ) ;cond
        ) ;block-side

       ((get-text-property pos 'part-side)
;;        (message "pos-min=%S reg-beg=%S part-beg=%S" pos-min reg-beg (web-mode-part-beginning-position pos))
        (setq reg-beg (web-mode-part-beginning-position pos))
;;        (message "reg-beg %S" reg-beg)
        (setq reg-beg (or reg-beg (point-min)))
        (goto-char reg-beg)
        (search-backward "<" nil t)
        (setq reg-col (current-column))
        (setq language (symbol-name (get-text-property pos 'part-side)))
        (cond
         ((string= language "css")
          (setq curr-indentation web-mode-css-indent-offset)
          )
         ((string= language "jsx")
          (setq curr-indentation web-mode-code-indent-offset)
          )
         (t
          (setq language "javascript"
                curr-indentation web-mode-code-indent-offset)
          )
         )
        ) ;part-side

       (t
        (setq language "html"
              curr-indentation web-mode-markup-indent-offset)
        )

       ) ;cond

      (cond
       ((or (and (> pos (point-min))
                 (eq (get-text-property pos 'part-token) 'comment)
                 (eq (get-text-property (1- pos) 'part-token) 'comment)
                 (progn
                   (setq reg-beg (previous-single-property-change pos 'part-token))
                   t))
            (and (> pos (point-min))
                 (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property (1- pos) 'block-token) 'comment)
                 (progn
                   (setq reg-beg (previous-single-property-change pos 'block-token))
                   t))
            (and (> pos (point-min))
                 (eq (get-text-property pos 'tag-type) 'comment)
                 (not (get-text-property pos 'tag-beg))
                 (progn
                   (setq reg-beg (web-mode-tag-beginning-position pos))
                   t))
            )
        (setq token "comment"))
       ((or (and (> pos (point-min))
                 (member (get-text-property pos 'part-token)
                         '(string context key))
                 (member (get-text-property (1- pos) 'part-token)
                         '(string context key)))
            (and (eq (get-text-property pos 'block-token) 'string)
                 (eq (get-text-property (1- pos) 'block-token) 'string)))
        (setq token "string"))
       )

      (goto-char pos)
      (setq curr-line (web-mode-trim
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
      (setq curr-char (if (string= curr-line "") 0 (aref curr-line 0)))

      (when (or (member language '("php" "blade" "javascript" "jsx" "razor"))
                (and (member language '("html" "xml"))
                     (not (eq ?\< curr-char))))
        (let (prev)
          (cond
           ((member language '("html" "xml" "javascript" "jsx"))
            (when (setq prev (web-mode-part-previous-live-line))
              (setq prev-line (car prev)
                    prev-indentation (cdr prev))
              (setq prev-line (web-mode-clean-part-line prev-line)))
            )
           ((setq prev (web-mode-block-previous-live-line))
            (setq prev-line (car prev)
                  prev-indentation (cdr prev))
            (setq prev-line (web-mode-clean-block-line prev-line)))
           ) ;cond

          ) ;let
        (when (>= (length prev-line) 1)
          (setq prev-char (aref prev-line (1- (length prev-line))))
          (setq prev-line (substring-no-properties prev-line))
          )
        )

      (cond
       ((not (member web-mode-content-type '("html" "xml")))
        )
       ((member language '("javascript" "jsx"))
        (setq reg-col (+ reg-col web-mode-script-padding)))
       ((string= language "css")
        (setq reg-col (+ reg-col web-mode-style-padding)))
       ((not (member language '("html" "xml" "razor")))
        (setq reg-col (+ reg-col web-mode-block-padding)))
       )

      (list :curr-char curr-char
            :curr-indentation curr-indentation
            :curr-line curr-line
            :language language
            :prev-char prev-char
            :prev-indentation prev-indentation
            :prev-line prev-line
            :reg-beg reg-beg
            :reg-col reg-col
            :token token)
      )))

(defun web-mode-indent-line ()

  (web-mode-propertize)

  (let ((offset nil)
        (char nil)
        (inhibit-modification-hooks t))

    (save-excursion
      (back-to-indentation)
      (setq char (char-after))
      (let* ((pos (point))
             (ctx (web-mode-point-context pos))
             (curr-char (plist-get ctx :curr-char))
             (curr-indentation (plist-get ctx :curr-indentation))
             (curr-line (plist-get ctx :curr-line))
             (language (plist-get ctx :language))
             (prev-char (plist-get ctx :prev-char))
             (prev-indentation (plist-get ctx :prev-indentation))
             (prev-line (plist-get ctx :prev-line))
             (reg-beg (plist-get ctx :reg-beg))
             (reg-col (plist-get ctx :reg-col))
             (token (plist-get ctx :token))
             (chars (list curr-char prev-char)))

        ;;(message "%S" ctx)
        ;;(message "%c %S %c" curr-char prev-line prev-char)

        (cond

         ((or (bobp) (= (line-number-at-pos pos) 1))
          (setq offset 0))

         ((string= token "string")
          (cond
           ((web-mode-block-token-starts-with (concat "[ \n]*" web-mode-sql-queries))
            (save-excursion
              (let (col)
                (web-mode-block-string-beginning)
                (skip-chars-forward "[ \"'\n]")
                (setq col (current-column))
                (goto-char pos)
                (if (looking-at-p "\\(SELECT\\|INSERT\\|DELETE\\|UPDATE\\|FROM\\|LEFT\\|JOIN\\|WHERE\\|GROUP BY\\|LIMIT\\|HAVING\\|\)\\)")
                    (setq offset col)
                  (setq offset (+ col web-mode-sql-indent-offset)))
                )
              ) ;save-excursion
            )
           (t
            (setq offset nil))
           ) ;cond
          ) ;case string

         ((string= token "comment")
          (if (eq (get-text-property pos 'tag-type) 'comment)
              (web-mode-tag-beginning)
            (goto-char (car
                        (web-mode-property-boundaries
                         (if (eq (get-text-property pos 'part-token) 'comment)
                             'part-token
                           'block-token)
                         pos))))
          (setq offset (current-column))
          (cond
           ((member (buffer-substring-no-properties (point) (+ (point) 2)) '("/*" "{*" "@*"))
            (cond
             ((eq ?\* curr-char)
              (setq offset (+ offset 1)))
             (t
              (setq offset (+ offset 3)))
             ) ;cond
            )
           ((and (string= web-mode-engine "django") (looking-back "{% comment %}"))
            (setq offset (- offset 12)))
           ((and (string= web-mode-engine "mako") (looking-back "<%doc%>"))
            (setq offset (- offset 6)))
           ((and (string= web-mode-engine "mason") (looking-back "<%doc%>"))
            (setq offset (- offset 6)))
           ) ;cond
          ) ;case comment

         ((and (string= web-mode-engine "mason")
               (string-match-p "^%" curr-line))
          (setq offset 0))

         ((and (get-text-property pos 'block-beg)
               (or (web-mode-block-is-close pos)
                   (web-mode-block-is-inside pos)))
          (when (web-mode-block-match)
            (setq offset (current-indentation))))

         ((eq (get-text-property pos 'block-token) 'delimiter-end)
          (when (web-mode-block-beginning)
            (setq reg-col (current-indentation)) ;; TODO: bad hack
            (setq offset (current-column))))

         ((and (get-text-property pos 'tag-beg)
               (eq (get-text-property pos 'tag-type) 'end))
          (when (web-mode-tag-match)
            (setq offset (current-indentation))))

         ((and (member language '("html" "xml" "jsx"))
               (get-text-property pos 'tag-type)
               (not (get-text-property pos 'tag-beg)))
          (cond
           ((and (get-text-property pos 'tag-attr)
                 (get-text-property (1- pos) 'tag-attr)
                 (web-mode-attribute-beginning)
                 (web-mode-dom-rsf "=[ ]*[\"']?" pos))
            (setq offset (current-column))
            )
           ((not (web-mode-tag-beginning))
            )
           (web-mode-attr-indent-offset
            (setq offset (+ (current-column) web-mode-attr-indent-offset)))
           ((string-match-p "^/>" curr-line)
            (setq offset (current-column)))
           (t
            (let ((skip (next-single-property-change (point) 'tag-attr)))
              (when skip
                (goto-char skip)
                (setq offset (current-column)))
              ) ;let
            ) ;t
           ))

         ((member language '("html" "xml"))
          (cond
           ((and (get-text-property pos 'tag-beg)
                 ;;(not (get-text-property pos 'part-side))
                 ;; Est ce que cette derniere ligne est utile ?
                 ;;(not (member web-mode-content-type '("jsx")))
                 )
            (setq offset (web-mode-markup-indentation pos))
            )
           ((and web-mode-pre-elements
                 (null (get-text-property pos 'block-side))
                 (null (get-text-property pos 'part-side))
                 (and (null (get-text-property pos 'tag-beg))
                      (save-excursion
                        (and (web-mode-element-parent)
                             (member (get-text-property (point) 'tag-name) web-mode-pre-elements))))
                 )
            (setq offset nil))
           ((or (eq (length curr-line) 0)
                (= web-mode-indent-style 2)
                (get-text-property pos 'tag-beg)
                (get-text-property pos 'reg-beg))
            (setq offset (web-mode-markup-indentation pos))
            )
           )
          )

         ((string= language "ctemplate")
          (setq offset reg-col))

         ((string= language "erb")
          (setq offset (web-mode-ruby-indentation pos
                                                  curr-line
                                                  reg-col
                                                  curr-indentation
                                                  reg-beg)))

         ((member language '("mako" "web2py"))
          (setq offset (web-mode-python-indentation pos
                                                    curr-line
                                                    reg-col
                                                    curr-indentation
                                                    reg-beg)))

         ((string= language "asp")
          (setq offset (web-mode-asp-indentation pos
                                                 curr-line
                                                 reg-col
                                                 curr-indentation
                                                 reg-beg)))

         ((member language '("lsp" "cl-emb"))
          (setq offset (web-mode-lisp-indentation pos ctx)))

         ((member curr-char '(?\} ?\) ?\]))
          (let ((ori (web-mode-opening-paren-position pos)))
            (cond
             ((null ori)
              (message "indent-line ** invalid closing bracket (%S) **" pos)
              (setq offset reg-col))
             ((and (looking-back ")[ ]*") ;; peut-on se passer du looking-back ?
                   (re-search-backward ")[ ]*" nil t)
                   (web-mode-block-opening-paren reg-beg))
              (back-to-indentation)
              (setq offset (current-indentation))
              )
             (t
              (goto-char ori)
              (back-to-indentation)
              (setq offset (current-indentation))
              ) ;t
             ) ;cond
            ) ;let
          )

         ((string= language "css")
          (setq offset (car (web-mode-css-indentation pos
                                                      reg-col
                                                      curr-indentation
                                                      language
                                                      reg-beg))))

         ((and (string= language "razor")
               (string-match-p "^\\." curr-line)
               (string-match-p "^\\." prev-line))
          (setq offset prev-indentation))

         ((and (string= language "razor")
               (string-match-p "^case " curr-line)
               (string-match-p "^case " prev-line))
          (search-backward "case ")
          (setq offset (current-column)))

         ;;((and (member language '("javascript" "jsx" "ejs"))
         ;;      (string-match-p "^@[[:alpha:]]+" prev-line))
         ;; (setq offset prev-indentation)
         ;; )

         ((and (member language '("javascript" "jsx" "ejs" "php"))
               (or (eq prev-char ?\))
                   (string-match-p "^else$" prev-line))
               (not (string-match-p "^\\([{.]\\|->\\)" curr-line)))
          (cond
           ((member language '("javascript" "jsx" "ejs"))
            (setq offset
                  (+ (car (web-mode-javascript-indentation pos
                                                           reg-col
                                                           curr-indentation
                                                           language
                                                           reg-beg))
                     web-mode-code-indent-offset))
            )
           (t
            (setq offset (+ prev-indentation web-mode-code-indent-offset))
            )
           )
          )

         ((and (member language '("javascript" "jsx" "ejs")) (member ?\. chars))
          (when (web-mode-javascript-calls-beginning pos reg-beg)
            (cond
             ((cdr (assoc "lineup-calls" web-mode-indentation-params))
              (search-forward ".")
              (setq offset (current-column))
              (when (eq curr-char ?\.)
                (goto-char pos)
                (looking-at "\\.[ \t\n]*")
                (setq offset (- offset (length (match-string-no-properties 0)))))
              )
             (t
              (setq offset (+ (current-indentation) web-mode-code-indent-offset))
              ) ;t
             ) ;cond
            ) ;when
          )

         ((and (member language '("javascript" "jsx" "ejs")) (member ?\+ chars))
          (cond
           ((not (web-mode-javascript-string-beginning pos reg-beg))
            )
           ((null (cdr (assoc "lineup-concats" web-mode-indentation-params)))
            (setq offset (+ (current-indentation) web-mode-code-indent-offset)))
           ((not (eq curr-char ?\+))
            (setq offset (current-column)))
           (t
            (setq offset (current-column))
            (goto-char pos)
            (looking-at "\\+[ \t\n]*")
            (setq offset (- offset (length (match-string-no-properties 0))))
            )
           )
          )

         ;; #446
         ((and (member language '("javascript" "jsx" "ejs" "php"))
               (or (string-match-p "[+-&|?:]$" prev-line)
                   (string-match-p "^[+-&|?:]" curr-line))
               (not (get-text-property pos 'part-element))
               (not (and (eq prev-char ?\:)
                         (string-match-p "^\\(case\\|default\\)" prev-line))))
          (when (funcall (if (member language '("javascript" "jsx" "ejs"))
                             'web-mode-javascript-statement-beginning
                           'web-mode-block-statement-beginning)
                         pos reg-beg)
            (setq offset (current-column))
            (when (member curr-char '(?\+ ?\- ?\& ?\| ?\? ?\:))
              (goto-char pos)
              (looking-at "\\(||\\|&&\\|[+-&|?:]\\)[ \t\n]*")
              (setq offset (- offset (length (match-string-no-properties 0)))))
            ) ;when
          )

         ((and (member language '("javascript" "jsx" "ejs"))
               (or (member ?\, chars)
                   (member prev-char '(?\( ?\[ ?\{))))
          (cond
           ((not (web-mode-javascript-args-beginning pos reg-beg))
            (message "no js args beg")
            )
           ((or (not (cdr (assoc "lineup-args" web-mode-indentation-params)))
                (looking-at-p "\n"))
            ;;(message "pos1=%S" pos)
            (setq offset (+ (current-indentation) web-mode-code-indent-offset)))
           ((not (eq curr-char ?\,))
            ;;(message "pos2=%S" pos)
            (setq offset (current-column)))
           (t
            ;;(message "pos3=%S" pos)
            (setq offset (current-column))
            (goto-char pos)
            (looking-at ",[ \t\n]*")
            (setq offset (- offset (length (match-string-no-properties 0)))))
           ))


         ((and (string= language "php") (string-match-p "^->" curr-line))
          (cond
           ((not (web-mode-block-calls-beginning pos reg-beg))
            )
           ((cdr (assoc "lineup-calls" web-mode-indentation-params))
            (search-forward "->")
            (setq offset (- (current-column) 2)))
           (t
            (setq offset (+ (current-indentation) web-mode-code-indent-offset)))
           ))

         ((member ?\, chars)
          (cond
           ((not (web-mode-block-args-beginning pos reg-beg))
            )
           ((cdr (assoc "lineup-args" web-mode-indentation-params))
            (setq offset (current-column))
            (when (eq curr-char ?\,)
              (goto-char pos)
              (looking-at ",[ \t\n]*")
              (setq offset (- offset (length (match-string-no-properties 0)))))
            )
           (t
            (setq offset (+ (current-indentation) web-mode-code-indent-offset)))
           ))

         ((and (string= language "php") (member ?\. chars))
          (cond
           ((not (web-mode-block-string-beginning pos reg-beg))
            )
           ((null (cdr (assoc "lineup-concats" web-mode-indentation-params)))
            (setq offset (+ (current-indentation) web-mode-code-indent-offset)))
           ((not (eq curr-char ?\.))
            (setq offset (current-column)))
           (t
            (setq offset (current-column))
            (goto-char pos)
            (looking-at "\\.[ \t\n]*")
            (setq offset (- offset (length (match-string-no-properties 0)))))
           ))

         ((and (string= language "jsx")
               (get-text-property pos 'part-element)
               (get-text-property (1- pos) 'part-element)
               (not (and (get-text-property pos 'part-expr)
                         (get-text-property (1- pos) 'part-expr))))
          (setq offset (web-mode-markup-indentation pos)))

         ((member language '("javascript" "jsx" "ejs"))
          (setq offset (car (web-mode-javascript-indentation pos
                                                             reg-col
                                                             curr-indentation
                                                             language
                                                             reg-beg))))

         (t
          ;;(message "point-context=%S" ctx)
          (setq offset (car (web-mode-bracket-indentation pos
                                                          reg-col
                                                          curr-indentation
                                                          language
                                                          reg-beg))))

         ) ;cond

        ;;(message "offset=%S" offset)
        (when (and offset reg-col (< offset reg-col)) (setq offset reg-col))
        ;;(message "offset=%S" offset)

        ) ;let
      ) ;save-excursion

    (when offset
      (let ((diff (- (current-column) (current-indentation))))
        (when (not (= offset (current-indentation)))
          (setq web-mode-change-beg (line-beginning-position)
                web-mode-change-end (+ web-mode-change-beg offset)))
        (setq offset (max 0 offset))
        (indent-line-to offset)
        (if (> diff 0) (move-to-column (+ (current-column) diff)))
        (when (and (string= web-mode-engine "mason")
                   (= offset 0)
                   (eq char ?\%))
          ;;(web-mode-highlight-region (line-beginning-position) (line-end-position))
          (save-excursion
            (font-lock-fontify-region (line-beginning-position) (line-end-position)))
          ) ;when
        ) ;let
      ) ;when

    ))

(defun web-mode-markup-indentation (pos)
  (save-excursion
    (goto-char pos)
    (let ((offset 0) beg ret)
      (when (setq beg (web-mode-markup-indentation-origin))
        (goto-char beg)
        (setq ret (web-mode-element-is-opened beg pos))
        (cond
         ((null ret)
          ;;(message "ind=%S col=%S" (current-indentation) (current-column))
          (setq offset (current-indentation)))
         ((eq ret t)
          (setq offset (+ (current-indentation) web-mode-markup-indent-offset)))
         (t
          (setq offset ret))
         ) ;cond
        ) ;when
      offset)))

(defun web-mode-css-indentation (pos initial-column language-offset language &optional limit)
  (let ((open-ctx (web-mode-bracket-up pos language limit)) offset)
    (cond
     ((or (null open-ctx) (null (plist-get open-ctx :pos)))
      (setq offset initial-column))
     (t
      (setq offset (+ (plist-get open-ctx :indentation) language-offset)))
     ) ;cond
    (cons (if (< offset initial-column) initial-column offset) open-ctx)
    ))

(defun web-mode-javascript-indentation (pos initial-column language-offset language &optional limit)
  (let ((open-ctx (web-mode-bracket-up pos language limit)) offset)
    ;;    (message "pos(%S) initial-column(%S) language-offset(%S) language(%S) limit(%S)" pos initial-column language-offset language limit)
    ;;(message "javascript-indentation: %S" open-ctx)
    (cond
     ((or (null open-ctx) (null (plist-get open-ctx :pos)))
      (setq offset initial-column))
     ((and (member language '("javascript" "jsx" "ejs"))
           (eq (plist-get open-ctx :char) ?\{)
           (web-mode-looking-back "switch[ ]*(.*)[ ]*" (plist-get open-ctx :pos))
           (not (looking-at-p "case\\|default")))
      (setq offset (+ (plist-get open-ctx :indentation) (* language-offset 2))))
     (t
      (setq offset (+ (plist-get open-ctx :indentation) language-offset)))
     ) ;cond
    (cons (if (< offset initial-column) initial-column offset) open-ctx)
    ))

(defun web-mode-bracket-indentation (pos initial-column language-offset language &optional limit)
  (save-excursion
    (let* ((ctx (web-mode-bracket-up pos language limit))
           (char (plist-get ctx :char))
           (pos (plist-get ctx :pos))
           (indentation (plist-get ctx :indentation)))
      ;;(message "pos(%S) initial-column(%S) language-offset(%S) language(%S) limit(%S)" pos initial-column language-offset language limit)
      ;;(message "bracket-up: %S, %c" ctx char)
      (cond
       ((null pos)
        (setq indentation initial-column))
       ((and (member language '("php"))
             (eq char ?\{)
             (web-mode-looking-back "switch[ ]*(.*)[ ]*" pos)
             (not (looking-at-p "case\\|default")))
        (setq indentation (+ indentation (* language-offset 2)))
        )
       ((and (member language '("php"))
             (eq char ?\{)
             (goto-char pos)
             (web-mode-looking-back "[)][ ]*" pos)
             (search-backward ")")
             (web-mode-block-opening-paren limit))
        (setq indentation (+ (current-indentation) language-offset))
        )
       (t
        (setq indentation (+ indentation language-offset))
        )
       )
      (cons (if (< indentation initial-column) initial-column indentation) ctx)
      )))

(defun web-mode-ruby-indentation (pos line initial-column language-offset limit)
  (unless limit (setq limit nil))
  (let (h offset prev-line prev-indentation open-ctx)
    (setq open-ctx (web-mode-bracket-up pos "ruby" limit))
    (if (plist-get open-ctx :pos)
        (setq offset (1+ (plist-get open-ctx :column)))
      (setq h (web-mode-previous-line pos limit))
      (setq offset initial-column)
      (when h
        (setq prev-line (car h))
        (setq prev-indentation (cdr h))
        (cond
         ((string-match-p "^\\(end\\|else\\|elsif\\|when\\)" line)
          (setq offset (- prev-indentation language-offset))
          )
         ((string-match-p "\\(when\\|if\\|else\\|elsif\\|unless\\|for\\|while\\|def\\|class\\)" prev-line)
          (setq offset (+ prev-indentation language-offset))
          )
         (t
          (setq offset prev-indentation)
          )
         )
        ) ;when
      ) ;if
    offset))

(defun web-mode-python-indentation (pos line initial-column language-offset limit)
  (unless limit (setq limit nil))
  (let (h out prev-line prev-indentation ctx)
    (setq h (web-mode-previous-line pos limit))
    (setq out initial-column)
    (when h
      (setq prev-line (car h))
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
       ) ;cond
      ) ;when
    ;;out
    (if (< out initial-column) initial-column out)
    ))

(defun web-mode-lisp-indentation (pos point-ctx)
  (let (offset open-ctx)
    (setq open-ctx (web-mode-bracket-up pos "lsp" (plist-get point-ctx :reg-beg)))
    ;;(message "point-ctx=%S" point-ctx)
    ;;(message "open-ctx=%S" open-ctx)
    (cond
     ((null (plist-get open-ctx :pos))
      (setq offset (plist-get point-ctx :reg-col)))
     ((member (plist-get point-ctx :curr-char) '(?\( ?\)))
      (if (web-mode-looking-at-p "((" (plist-get open-ctx :pos))
          (setq offset (+ (plist-get open-ctx :column) 1))
        (setq offset (+ (plist-get open-ctx :column) web-mode-code-indent-offset)))
      )
     (t
      (goto-char (plist-get open-ctx :pos))
      (forward-char)
      (web-mode-rsf "[[:alnum:]-:]+ ")
      (setq offset (current-column))
      )
     ) ;cond
    offset))

(defun web-mode-asp-indentation (pos line initial-column language-offset limit)
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
    out))

(defun web-mode-block-previous-live-line ()
  (save-excursion
    (let ((continue t) (line "") (pos (point)))
      (beginning-of-line)
      (while (and continue (not (bobp)) (forward-line -1))
        (when (not (web-mode-block-is-token-line))
          (setq line (web-mode-trim (buffer-substring (point) (line-end-position)))))
        (when (not (string= line ""))
          (setq continue nil))
        ) ;while
      (if (string= line "")
          (progn (goto-char pos) nil)
        (cons line (current-indentation)))
      )))

(defun web-mode-part-previous-live-line ()
  (save-excursion
    (let ((continue t) (line "") (pos (point)))
      (beginning-of-line)
      (while (and continue (not (bobp)) (forward-line -1))
        (if (not (web-mode-part-is-token-line))
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

(defun web-mode-clean-part-line (input)
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
      ) ;dotimes
    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (web-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

(defun web-mode-clean-block-line (input)
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))
    (dotimes (i n)
      (if (or (not (get-text-property i 'block-side input))
              (member (get-text-property i 'block-token input)
                      '(comment delimiter-beg delimiter-end)))
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

(defun web-mode-coord-position (line column)
  (save-excursion
    (when (stringp line) (setq line (string-to-number line)))
    (when (stringp column) (setq column (string-to-number column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column (1- column))
    (point)))

(defun web-mode-column-at-pos (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun web-mode-is-single-line-block (pos)
  (= (web-mode-line-number (web-mode-block-beginning-position pos))
     (web-mode-line-number (web-mode-block-end-position pos))))

(defun web-mode-line-number (&optional pos)
  (unless pos (setq pos (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pos)
                 (if (= (web-mode-column-at-pos pos) 0) 1 0)))))

(defun web-mode-block-is-control (pos)
  (save-excursion
    (let (control state controls pair)
      (goto-char pos)
      (setq controls (web-mode-block-controls-get pos))
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

(defun web-mode-block-is-opening-control (pos)
  (save-excursion
    (let (controls pair)
      (goto-char pos)
      (if (and (setq controls (web-mode-block-controls-get pos))
               (= (length controls) 1)
               (setq pair (car controls))
               (eq (car pair) 'open))
          (cdr pair)
        nil)
      )))

(defun web-mode-markup-indentation-origin ()
  (let* ((continue (not (bobp)))
         (pos (point))
         (part-side (not (null (get-text-property pos 'part-side)))) ;part-side at the origin
         (types '(start end void)))
    (while continue
      (forward-line -1)
      (back-to-indentation)
      (setq pos (point))
      (setq continue (not (or (bobp)
                              (and (null part-side)
                                   (null (get-text-property pos 'part-side))
                                   (get-text-property pos 'tag-beg)
                                   (member (get-text-property pos 'tag-type) types)
                                   (null (get-text-property (1- pos) 'invisible)))
                              (and part-side
                                   (get-text-property pos 'part-side)
                                   (get-text-property pos 'tag-beg)
                                   (member (get-text-property pos 'tag-type) types)
                                   (null (get-text-property (1- pos) 'invisible)))
                              (and (get-text-property pos 'block-beg)
                                   (not (get-text-property pos 'tag-type))
                                   (web-mode-block-is-control pos)
                                   (not (looking-at-p "{% comment"))))))
      ) ;while
    ;;(message "indent-origin=%S" pos)
    pos))

;;TODO : prendre en compte part-token
;; state=t <=> start tag
(defun web-mode-element-is-opened (pos limit)
  (let (tag
        last-end-tag
        tag-pos block-pos
        state
        n
        ret
        (continue t)
        controls
        control
        (buffer (current-buffer))
        (h (make-hash-table :test 'equal))
        (h2 (make-hash-table :test 'equal)))

;;    (message "pos-ori=%S limit=%S" pos limit)

    (while continue
      (setq control nil
            controls nil
            last-end-tag nil
            tag nil)

      (cond
       ((get-text-property pos 'tag-beg)
        (when (member (get-text-property pos 'tag-type) '(start end))
          (setq tag (get-text-property pos 'tag-name)
                state (eq (get-text-property pos 'tag-type) 'start))
          (if (null state) (setq last-end-tag (cons tag pos)))
          (setq n (gethash tag h 0))
          (if (null state)
              (progn
                (when (> n 0) (puthash tag (1- n) h))
                (puthash tag (1- n) h2))
            (puthash tag (1+ n) h)
            (puthash tag (1+ n) h2))
          ) ;when
        (when (setq pos (web-mode-tag-end-position pos))
          (setq tag-pos nil)
          (when (and block-pos (> pos block-pos))
            (setq block-pos nil))
          ) ;when
        )
       ((and web-mode-enable-control-block-indentation
             (get-text-property pos 'block-beg))
        (when (setq controls (web-mode-block-controls-get pos))
          (dolist (control controls)
            (setq tag (cdr control))
            (setq n (gethash tag h 0))
            (cond
             ((eq (car control) 'inside)
              )
             ((eq (car control) 'open)
              (puthash tag (1+ n) h))
             ((> n 0)
              (puthash tag (1- n) h))
             ) ;cond
            ) ;dolist
          )
        (when (setq pos (web-mode-block-end-position pos))
          (setq block-pos nil)
          (when (and tag-pos (> pos tag-pos))
            (setq tag-pos nil))
          )
        )
       ) ;cond

;;      (message "tag=%S end-pos=%S" tag pos)

      (when (and pos (< pos limit))
        (when (or (null tag-pos) (>= pos tag-pos))
          (setq tag-pos (web-mode-tag-next-position pos limit))
;;          (message "from=%S tag-next-pos=%S" pos tag-pos)
          )
        (when (or (null block-pos) (>= pos block-pos))
          (setq block-pos (web-mode-block-next-position pos limit))
;;          (message "from=%S block-next-pos=%S" pos block-pos)
          )
        )

      (cond
       ((null pos)
        )
       ((and (null tag-pos)
             (null block-pos))
        (setq pos nil))
       ((and tag-pos block-pos)
        (if (< tag-pos block-pos)
            (progn
              (setq pos tag-pos)
              (setq tag-pos nil))
          (setq pos block-pos)
          (setq block-pos nil))
        )
       ((null tag-pos)
        (setq pos block-pos)
        (setq block-pos nil))
       (t
        (setq pos tag-pos)
        (setq tag-pos nil))
       )

      (when (or (null pos)
                (>= pos limit))
        (setq continue nil))
      ) ;while

;;    (message "hashtable=%S" h)
    (maphash (lambda (k v) (if (> v 0) (setq ret t))) h)

    (when (and (null ret)
               last-end-tag
               (> (hash-table-count h2) 1)
               (< (gethash (car last-end-tag) h2) 0))
;;      (message "last-end-tag=%S" last-end-tag)
      (save-excursion
        (goto-char (cdr last-end-tag))
        (web-mode-tag-match)
        (when (not (= (point) (cdr last-end-tag)))
          (setq n (point))
          (back-to-indentation)
          (if (= n (point)) (setq ret (current-indentation))))
        ))

    ret))

(defun web-mode-previous-line (pos limit)
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
      (cons line (current-indentation))
      )))

(defun web-mode-bracket-up (pos language &optional limit)
  (unless limit (setq limit nil))
  ;;(message "pos(%S) language(%S) limit(%S)" pos language limit)
  (save-excursion
    (goto-char pos)
    (let ((continue t)
          (regexp "[\]\[}{)(]")
          (key nil)
          (char nil)
          (n 0)
          (queue nil)
          (searcher (if (get-text-property pos 'block-side) 'web-mode-block-rsb 'web-mode-part-rsb)))
      (while (and continue (funcall searcher regexp limit))
        (setq char (aref (match-string-no-properties 0) 0))
        (setq key (cond ((eq char ?\)) ?\()
                        ((eq char ?\}) ?\{)
                        ((eq char ?\]) ?\[)
                        (t             char)))
        (setq n (or (plist-get queue key) 0))
        (setq n (if (member char '(?\( ?\{ ?\[)) (1+ n) (1- n)))
        (setq queue (plist-put queue key n))
        (setq continue (< n 1))
        ;;(message "pos=%S char=%c key=%c n=%S queue=%S" (point) char key n queue)
        ) ;while
      (list :pos (if (> n 0) (point) nil)
            :char char
            :column (current-column)
            :indentation (current-indentation))
      ) ;let
    ))

(defun web-mode-count-char-in-string (char string)
  (let ((n 0))
    (dotimes (i (length string))
      (if (eq (elt string i) char)
          (setq n (1+ n))))
    n))

(defun web-mode-mark-and-expand ()
  "Mark and expand."
  (interactive)
  (web-mode-mark (point)))

(defun web-mode-mark (pos)
  (let ((beg pos) (end pos) prop reg-beg boundaries)

    (if mark-active
        (setq reg-beg (region-beginning))
      (setq web-mode-expand-initial-pos (point)))

    ;; (message "regs=%S %S %S %S" (region-beginning) (region-end) (point-min) (point-max))
    ;; (message "before=%S" web-mode-expand-previous-state)

    (cond

     ((and mark-active
           (= (region-beginning) (point-min))
           (or (= (region-end) (point-max))
               (= (1+ (region-end)) (point-max))))
      (deactivate-mark)
      (goto-char (or web-mode-expand-initial-pos (point-min)))
      (setq web-mode-expand-previous-state nil)
      (when (and web-mode-expand-initial-pos
                 (or (< web-mode-expand-initial-pos (window-start))
                     (> web-mode-expand-initial-pos (window-end))))
        (recenter))
      )

     ((string= web-mode-expand-previous-state "elt-content")
      (web-mode-element-parent)
      ;;(message "pos=%S" (point))
      (web-mode-element-select)
      (setq web-mode-expand-previous-state "html-parent"))

     ((and (member (get-text-property pos 'block-token) '(comment string))
           (not (member web-mode-expand-previous-state '("block-token" "block-body" "block-side"))))
      (when (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token))
        (setq beg (or (previous-single-property-change pos 'block-token) (point-min))))
      (when (eq (get-text-property pos 'block-token) (get-text-property (1+ pos) 'block-token))
        (setq end (next-single-property-change pos 'block-token)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "block-token"))

     ((and (get-text-property pos 'block-side)
           (not (member web-mode-expand-previous-state '("block-body" "block-side")))
           (not (member web-mode-engine '(django go)))
           (setq boundaries (web-mode-in-code-block "{" "}" 'block-side)))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "block-body"))

     ((and (get-text-property pos 'block-side)
           (not (member web-mode-expand-previous-state '("block-side"))))
      (set-mark (web-mode-block-beginning-position pos))
      (goto-char (1+ (web-mode-block-end-position pos)))
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
      (setq web-mode-expand-previous-state "client-part"))

     ((and (get-text-property pos 'part-side)
           (not (string= web-mode-expand-previous-state "part-side")))
      (when (eq (get-text-property pos 'part-side) (get-text-property (1- pos) 'part-side))
        (setq beg (previous-single-property-change pos 'part-side)))
      (when (eq (get-text-property pos 'part-side) (get-text-property (1+ pos) 'part-side))
        (setq end (next-single-property-change pos 'part-side)))
      (when (eq (char-after beg) ?\n)
        (setq beg (1+ beg)))
      (set-mark beg)
      (goto-char end)
      (when (looking-back "^[ \t]+")
        (beginning-of-line))
      (exchange-point-and-mark)
      (setq web-mode-expand-previous-state "part-side"))

     ((and (get-text-property pos 'tag-attr)
           (not (member web-mode-expand-previous-state '("html-attr" "html-tag"))))
      (web-mode-attribute-select pos)
      (setq web-mode-expand-previous-state "html-attr"))

     ((and (eq (get-text-property pos 'tag-type) 'comment)
           (not (member web-mode-expand-previous-state '("html-tag" "html-comment" "html-elt" "html-parent"))))
      (web-mode-tag-select)
      (setq web-mode-expand-previous-state "html-comment"))

     ((and (get-text-property pos 'tag-name)
           (not (member web-mode-expand-previous-state '("html-tag" "html-elt" "html-parent"))))
      (web-mode-tag-select)
      (setq web-mode-expand-previous-state "html-tag"))

     ((and (get-text-property pos 'tag-beg)
           (string= web-mode-expand-previous-state "html-tag"))
      (web-mode-element-select)
      (setq web-mode-expand-previous-state "html-elt"))

     (t
      ;; (web-mode-element-parent)
      ;; (if (and reg-beg (= reg-beg (region-beginning)))
      ;;     (progn
      ;;       (push-mark (point))
      ;;       (push-mark (point-max) nil t)
      ;;       (goto-char (point-min))
      ;;       (setq web-mode-expand-previous-state "mark-whole"))
      ;;   (web-mode-element-select)
      ;;   (setq web-mode-expand-previous-state "html-parent")
      ;;   )
      (cond
       ((not (web-mode-element-parent))
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char (point-min))
        (setq web-mode-expand-previous-state "mark-whole"))
       ((not (= (web-mode-tag-end-position (point)) (1- beg)))
        (web-mode-element-content-select)
        (setq web-mode-expand-previous-state "elt-content"))
       (t
        (web-mode-element-select)
        (setq web-mode-expand-previous-state "html-parent"))
       )
      ) ;t

     ) ;cond

    ;;(message "w=%S" (window-end))
    ;;(message "after=%S" web-mode-expand-previous-state)

    ))

(defun web-mode-block-kill ()
  "Kill the current block."
  (interactive)
  (web-mode-block-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

(defun web-mode-block-select ()
  "Select the current block."
  (interactive)
  (let (beg)
    (when (setq beg (web-mode-block-beginning-position (point)))
      (goto-char beg)
      (set-mark (point))
      (web-mode-block-end)
      (exchange-point-and-mark))
    beg))

(defun web-mode-tag-select ()
  "Select the current html tag."
  (interactive)
  (let (beg)
    (when (setq beg (web-mode-tag-beginning-position (point)))
      (goto-char beg)
      (set-mark (point))
      (web-mode-tag-end)
      (exchange-point-and-mark))
    beg))

(defun web-mode-element-content-select ()
  "Select the content of a html element."
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
  "Select the current html element (including opening and closing tags)."
  (interactive)
  (let* ((pos (point))
         (type (get-text-property pos 'tag-type)))
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
  (unless pos (setq pos (point)))
  (let (boundaries)
    (and (setq boundaries (web-mode-element-boundaries pos))
         (or (= (car (car boundaries)) (car (cdr boundaries)))
             (= (cdr (car boundaries)) (1- (car (cdr boundaries)))))
         )))

(defun web-mode-element-transpose ()
  "Transpose two html elements."
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
    start2))

(defun web-mode-element-children-fold-or-unfold (&optional pos)
  "Fold/Unfold all the children of the current html element."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (dolist (child (reverse (web-mode-element-children pos)))
      (goto-char child)
      (web-mode-fold-or-unfold))
    ))

(defun web-mode-element-mute-blanks ()
  "Mute blanks."
  (interactive)
  (let (pos parent beg end children elt)
    (setq pos (point))
    (save-excursion
      (when (and (setq parent (web-mode-element-boundaries pos))
                 (web-mode-element-child-position (point)))
        (setq children (reverse (web-mode-element-children)))
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

(defun web-mode-element-children (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t) (i 0) child children)
    (save-excursion
      (when (and (member (get-text-property pos 'tag-type) '(start end))
                 (setq child (web-mode-element-child-position pos)))
        (while continue
          (cond
           ((> (setq i (1+ i)) 100)
            (setq continue nil)
            (message "element-children ** warning **"))
           ((= i 1)
            (goto-char child))
           ((web-mode-element-sibling-next)
            )
           (t
            (setq continue nil))
           ) ;cond
          (when continue
            (setq children (append children (list (point)))))
          ) ;while
        ) ;when
      ) ;save-excursion
    children))

(defun web-mode-element-boundaries (&optional pos)
  "Return ((start-tag-beg . start-tag-end) . (end-tag-beg . end-tag-end))
First level car and cdr are the same with void elements.
Pos should be in a tag."
  (unless pos (setq pos (point)))
  (let (start-tag-beg start-tag-end end-tag-beg end-tag-end)
    (cond
     ((eq (get-text-property pos 'tag-type) 'start)
      (setq start-tag-beg (web-mode-tag-beginning-position pos)
            start-tag-end (web-mode-tag-end-position pos))
      (when (setq pos (web-mode-tag-match-position pos))
        (setq end-tag-beg pos
              end-tag-end (web-mode-tag-end-position pos)))
      )
     ((eq (get-text-property pos 'tag-type) 'end)
      (setq end-tag-beg (web-mode-tag-beginning-position pos)
            end-tag-end (web-mode-tag-end-position pos))
      (when (setq pos (web-mode-tag-match-position pos))
        (setq start-tag-beg pos
              start-tag-end (web-mode-tag-end-position pos)))
      )
     ((eq (get-text-property pos 'tag-type) 'void)
      (setq start-tag-beg (web-mode-tag-beginning-position pos)
            start-tag-end (web-mode-tag-end-position pos))
      (setq end-tag-beg start-tag-beg
            end-tag-end start-tag-end)
      )
     ) ;cond
    (if (and start-tag-beg start-tag-end end-tag-beg end-tag-end)
        (cons (cons start-tag-beg start-tag-end) (cons end-tag-beg end-tag-end))
      nil)
    ))

(defun web-mode-surround ()
  "Surround each line of the current REGION with a start/end tag."
  (interactive)
  (when mark-active
    (let (beg end line-beg line-end pos tag tag-start tag-end)
      (save-excursion
        (setq tag (read-from-minibuffer "Tag name? ")
              tag-start (concat "<" tag ">")
              tag-end (concat "</" tag ">")
              pos (point)
              beg (region-beginning)
              end (region-end)
              line-beg (web-mode-line-number beg)
              line-end (web-mode-line-number end))
        (goto-char end)
        (unless (bolp)
          (insert tag-end)
          (back-to-indentation)
          (when (> beg (point))
            (goto-char beg))
          (insert tag-start))
        (while (> line-end line-beg)
          (forward-line -1)
          (setq line-end (1- line-end))
          (unless (looking-at-p "[[:space:]]*$")
            (end-of-line)
            (insert tag-end)
            (back-to-indentation)
            (when (> beg (point))
              (goto-char beg))
            (insert tag-start))
          ) ;while
        (deactivate-mark)
        ))))

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
              end (1+ (web-mode-element-end-position pos))))
       ((setq beg (web-mode-element-parent-position pos))
        (setq end (1+ (web-mode-element-end-position pos))))
       )
      ;;      (message "beg(%S) end(%S)" beg end)
      (when (and beg end (> end 0))
        (setq sep (if (get-text-property beg 'tag-beg) "\n" ""))
        (web-mode-insert-text-at-pos (concat sep "</" tag ">") end)
        (web-mode-insert-text-at-pos (concat "<" tag ">" sep) beg)
        (when (string= sep "\n") (indent-region beg (+ end (* (+ 3 (length tag)) 2))))
        )
      ) ;save-excursion
    (web-mode-go beg)))

(defun web-mode-element-vanish ()
  "Vanish the current html element. The content of the element is kept."
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
  "Kill the current html element."
  (interactive)
  (web-mode-element-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

(defun web-mode-element-clone ()
  "Clone the current html element."
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

(defun web-mode-element-insert ()
  "Insert an html element."
  (interactive)
  (let (tag-name)
    (cond
     ((and (get-text-property (point) 'tag-type)
           (not (get-text-property (point) 'tag-beg)))
      (message "element-insert ** invalid context **"))
     ((not (and (setq tag-name (read-from-minibuffer "Tag name? "))
                (> (length tag-name) 0)))
      (message "element-insert ** failure **"))
     ((web-mode-element-is-void tag-name)
      (insert (concat "<" tag-name "/>"))
      )
     (mark-active
      (let ((beg (region-beginning)) (end (region-end)))
        (deactivate-mark)
        (goto-char end)
        (insert "</" tag-name ">")
        (goto-char beg)
        (insert "<" tag-name ">")
        )
      )
     (t
      (insert (concat "<" tag-name ">" "</" tag-name ">"))
      (web-mode-sb "</")
      )
     ) ;cond
    ))

(defun web-mode-element-rename ()
  "Rename the current html element."
  (interactive)
  (save-excursion
    (let (pos tag-name)
      (setq tag-name (read-from-minibuffer "New tag name? "))
      (when (and (> (length tag-name) 0)
                 (web-mode-element-beginning)
                 (looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)"))
        (setq pos (point))
        (unless (web-mode-element-is-void)
            (save-match-data
              (web-mode-tag-match)
              (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)")
                  (replace-match (concat "</" tag-name))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag-name))
        ))))

(defun web-mode-current-trimmed-line ()
  (web-mode-trim (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))

(defun web-mode-trim (string)
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun web-mode-block-is-token-line ()
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (back-to-indentation)
      (while (and continue (not (eolp)))
        (cond
         ((get-text-property (point) 'block-token)
          (setq counter (1+ counter)))
         ((not (eq ?\s (following-char)))
          (setq continue nil
                counter 0))
         ) ;cond
        (forward-char)
        ) ;while
      (> counter 0)
      )))

(defun web-mode-part-is-token-line ()
  (save-excursion
    (let ((continue t)
          (counter 0))
      (beginning-of-line)
      (setq continue (not (eolp)))
      (while continue
        (forward-char)
        (cond
         ((eolp)
          (setq continue nil))
         ((or (get-text-property (point) 'block-side)
              (member (get-text-property (point) 'part-token) '(comment string)))
          (setq counter (1+ counter)))
         ((eq ?\s (following-char))
          )
         (t
          (setq continue nil
                counter 0))
         )
        ) ;while
      (> counter 0))))

(defun web-mode-is-content (&optional pos)
  (unless pos (setq pos (point)))
  (not (or (get-text-property pos 'part-side)
           (get-text-property pos 'tag-type)
           (get-text-property pos 'block-side)
           )))

(defun web-mode-is-comment-or-string (&optional pos)
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (member (get-text-property pos 'block-token) '(comment string))
                 (member (get-text-property pos 'part-token) '(comment string))))))

;; on regarde le dernier
(defun web-mode-block-is-open (&optional pos)
  (unless pos (setq pos (point))))

;; on regarde le premier
(defun web-mode-block-is-close (&optional pos)
  (unless pos (setq pos (point)))
  (and (get-text-property pos 'block-side)
       (eq (caar (web-mode-block-controls-get pos)) 'close)))

;; on regarde le premier
(defun web-mode-block-is-inside (&optional pos)
  (unless pos (setq pos (point)))
  (and (get-text-property pos 'block-side)
       (eq (caar (web-mode-block-controls-get pos)) 'inside)))

(defun web-mode-element-is-void (&optional tag)
  (cond
   ((and tag (member tag '("div" "li" "a" "p")))
    nil)
   (tag
    (car (member (downcase tag) web-mode-void-elements)))
   (t
    (eq (get-text-property (point) 'tag-type) 'void))
   ))

(defun web-mode-toggle-current-element-highlight ()
  "Toggle highlighting of the current html element."
  (interactive)
  (if web-mode-enable-current-element-highlight
      (progn
        (web-mode-delete-tag-overlays)
        (setq web-mode-enable-current-element-highlight nil))
    (setq web-mode-enable-current-element-highlight t)
    ))

(defun web-mode-fold-or-unfold (&optional pos)
  "Toggle folding on an html element or a control block."
  (interactive)
  (web-mode-propertize)
  (web-mode-with-silent-modifications
   (save-excursion
     (if pos (goto-char pos))
     (let (beg-inside beg-outside end-inside end-outside overlay overlays regexp)
       (when (looking-back "^[\t ]*")
         (back-to-indentation))
       (setq overlays (overlays-at (point)))
       (dolist (elt overlays)
         (when (and (not overlay)
                    (eq (overlay-get elt 'font-lock-face) 'web-mode-folded-face))
           (setq overlay elt)))
       (cond
        ;; *** unfolding
        (overlay
         ;;(setq overlay (car overlays))
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
         (setq beg-outside (web-mode-block-beginning-position (point)))
         (setq beg-inside (1+ (web-mode-block-end-position (point))))
         (when (web-mode-block-match)
           (setq end-inside (point))
           (setq end-outside (1+ (web-mode-block-end-position (point)))))
         ) ;block-control
        ) ;cond
       (when (and beg-inside beg-outside end-inside end-outside)
         ;;(message "beg-out(%d) beg-in(%d) end-in(%d) end-out(%d)" beg-outside beg-inside end-inside end-outside)
         (setq overlay (make-overlay beg-outside end-outside))
         (overlay-put overlay 'font-lock-face 'web-mode-folded-face)
         (put-text-property beg-inside end-inside 'invisible t)
         )
       ))))

(defun web-mode-toggle-comments ()
  "Toggle comments visbility."
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

(defun web-mode-comment-or-uncomment-region (beg end)
  (interactive)
  (save-excursion
    (push-mark end)
    (goto-char beg)
    (setq mark-active t)
    (web-mode-comment-or-uncomment)
    (pop-mark)))

(defun web-mode-comment-or-uncomment ()
  "Comment or uncomment line(s), block or region at POS."
  (interactive)
  ;; TODO : if mark is at eol, mark--
  (if (looking-at-p "[[:space:]]*$")
      (web-mode-comment-insert)
    (when (and (use-region-p) (eq (point) (region-end)))
      (if (bolp) (backward-char))
      (exchange-point-and-mark))
    (skip-chars-forward "[:space:]" (line-end-position))
    (cond
     ((or (eq (get-text-property (point) 'tag-type) 'comment)
          (eq (get-text-property (point) 'block-token) 'comment)
          (eq (get-text-property (point) 'part-token) 'comment))
      (web-mode-uncomment (point)))
     (t
      (web-mode-comment (point)))
     )
    ) ;if
  )

(defun web-mode-comment-insert ()
  (cond
   ((get-text-property (point) 'block-side)
    (insert "/*  */")
    (search-backward " */"))
   ((get-text-property (point) 'part-side)
    (insert "/*  */")
    (search-backward " */"))
   (t
    (insert "<!--  -->")
    (search-backward " -->"))
   )
  )

(defun web-mode-comment (pos)
    (let (ctx language sel beg end tmp block-side single-line-block pos-after content)

      (setq pos-after pos)

      (setq block-side (get-text-property pos 'block-side))
      (setq single-line-block (web-mode-is-single-line-block pos))

      (cond

       ((and block-side
             (intern-soft (concat "web-mode-comment-" web-mode-engine "-block"))
             single-line-block)
        (funcall (intern (concat "web-mode-comment-" web-mode-engine "-block")) pos))

       (t
        (setq ctx (web-mode-point-context
                   (if mark-active (region-beginning) (line-beginning-position))))
        (setq language (plist-get ctx :language))
        (cond
         (mark-active
          )
         ((and (member language '("html" "xml"))
               (get-text-property (progn (back-to-indentation) (point)) 'tag-beg))
          (web-mode-element-select))
         (t
          (end-of-line)
          (set-mark (line-beginning-position)))
         ) ;cond

        (setq beg (region-beginning)
              end (region-end))

        (when (> (point) (mark))
          (exchange-point-and-mark))

        (if (and (eq (char-before end) ?\n)
                 (not (eq (char-after end) ?\n)))
            (setq end (1- end)))

;;        (setq sel (web-mode-trim (buffer-substring-no-properties beg end)))
        (setq sel (buffer-substring-no-properties beg end))

        ;;(web-mode-with-silent-modifications
        ;; (delete-region beg end))
        ;;(deactivate-mark)

        (cond

         ((member language '("html" "xml"))
          (cond
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "django"))
            (setq content (concat "{# " sel " #}")))
           ((and (= web-mode-comment-style 2) (member web-mode-engine '("ejs" "erb")))
            (setq content (concat "<%# " sel " %>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "aspx"))
            (setq content (concat "<%-- " sel " --%>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "smarty"))
            (setq content (concat "{* " sel " *}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "blade"))
            (setq content (concat "{{-- " sel " --}}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "razor"))
            (setq content (concat "@* " sel " *@")))
           (t
            (setq content (concat "<!-- " sel " -->"))
            (when (< (length sel) 1)
              (search-backward " -->")
              (setq pos-after nil))
            ))
          ) ;case html

         ((member language '("php" "javascript" "java" "jsx"))
          (let (alt)
            (setq alt (cdr (assoc language web-mode-comment-formats)))
            (if (and alt (not (string= alt "/*")))
                (setq content (replace-regexp-in-string "^[ ]*" alt sel))
              ;;(message "before")
              (setq content (concat "/* " sel " */"))
              ;;(message "after")
              ) ;if
            )
          )

         ((member language '("erb"))
          (setq content (replace-regexp-in-string "^[ ]*" "#" sel)))

         ((member language '("asp"))
          (setq content (replace-regexp-in-string "^[ ]*" "''" sel)))

         (t
          (setq content (concat "/* " sel " */")))

         ) ;cond

        ) ;t
       ) ;cond

      ;;(web-mode-with-silent-modifications
      (delete-region beg end)
      (deactivate-mark)
      ;;)
      ;;(web-mode-insert-and-indent content)
      (let (beg end)
        (setq beg (point-at-bol))
        (insert content)
        (setq end (point-at-eol))
        (indent-region beg end)
        )

      ;;(when content (web-mode-insert-and-indent content))

      (when pos-after (goto-char pos-after))

      ;;      (message "END")

      ))

(defun web-mode-comment-boundaries (&optional pos)
  (interactive)
  (unless pos (setq pos (point)))
  (let ((beg pos) (end pos) prop)
    (save-excursion
      (goto-char pos)
      (setq prop
            (cond
             ((eq (get-text-property pos 'block-token) 'comment) 'block-token)
             ((eq (get-text-property pos 'tag-type) 'comment) 'tag-type)
             ((eq (get-text-property pos 'part-token) 'comment) 'part-token)
             (t nil)
             ))
      (if (null prop)
          (setq beg nil
                end nil)
        (when (and (not (bobp))
                   (eq (get-text-property pos prop) (get-text-property (1- pos) prop)))
          (setq beg (or (previous-single-property-change pos prop) (point-min))))
        (when (and (not (eobp))
                   (eq (get-text-property pos prop) (get-text-property (1+ pos) prop)))
          (setq end (or (next-single-property-change pos prop) (point-max)))))
      (when (and beg (string= (buffer-substring-no-properties beg (+ beg 2)) "//"))
        (goto-char end)
        (while (and (looking-at-p "\n[ ]*//")
                    (not (eobp)))
          (search-forward "//")
          (backward-char 2)
          ;;(message "%S" (point))
          (setq end (next-single-property-change (point) prop))
          (goto-char end)
          ;;(message "%S" (point))
          ) ;while
        ) ;when
      (when end (setq end (1- end)))
      ) ; save-excursion
    ;;(message "beg=%S end=%S" beg end)
    (if (and beg end) (cons beg end) nil)
    ))

(defun web-mode-uncomment (pos)
  (let ((beg pos) (end pos) (sub2 "") comment boundaries)
    (save-excursion
      (cond
       ((and (get-text-property pos 'block-side)
             (intern-soft (concat "web-mode-uncomment-" web-mode-engine "-block")))
        (funcall (intern (concat "web-mode-uncomment-" web-mode-engine "-block")) pos))
       ((and (setq boundaries (web-mode-comment-boundaries pos))
             (setq beg (car boundaries))
             (setq end (1+ (cdr boundaries)))
             (> (- end beg) 4))
        ;;(message "beg(%S) end(%S)" beg end)
        (setq comment (buffer-substring-no-properties beg end))
        (setq sub2 (substring comment 0 2))
        (cond
         ((member sub2 '("<!" "<%"))
          (setq comment (replace-regexp-in-string "\\(^<[!%]--[ ]?\\|[ ]?--[%]?>$\\)" "" comment)))
         ((string= sub2 "{#")
          (setq comment (replace-regexp-in-string "\\(^{#[ ]?\\|[ ]?#}$\\)" "" comment)))
         ((string= sub2 "/*")
          (setq comment (replace-regexp-in-string "\\(^/\\*[ ]?\\|[ ]?\\*/$\\)" "" comment)))
         ((string= sub2 "//")
          ;;(setq comment (replace-regexp-in-string "\\(^//\\)" "" comment))
          (setq comment (replace-regexp-in-string "\\(//\\)" "" comment)))
         ) ;cond
        (delete-region beg end)
        (web-mode-insert-and-indent comment)
        (goto-char beg)
        )
       ) ;cond
      (indent-according-to-mode))))

(defun web-mode-comment-ejs-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "//" (+ beg 2))))

(defun web-mode-comment-erb-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" (+ beg 2))))

(defun web-mode-comment-django-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" end)
    (web-mode-insert-text-at-pos "#" (1+ beg))))

(defun web-mode-comment-dust-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "!" end)
    (web-mode-insert-text-at-pos "!" (1+ beg))))

(defun web-mode-comment-aspx-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "#" end)
    (web-mode-insert-text-at-pos "#" (1+ beg))))

(defun web-mode-comment-jsp-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "--" (+ beg 2))))

(defun web-mode-comment-go-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "/" (+ beg 2))))

(defun web-mode-comment-php-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-insert-text-at-pos "*/" (- end 1))
    (web-mode-insert-text-at-pos "/*" (+ beg (if (web-mode-looking-at "<\\?php" beg) 5 3)))))

(defun web-mode-uncomment-ejs-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (+ beg 2))))

(defun web-mode-uncomment-erb-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (+ beg 2))))

(defun web-mode-uncomment-django-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 2 (1- end))
    (web-mode-remove-text-at-pos 2 beg)))

(defun web-mode-uncomment-dust-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (1- end))
    (web-mode-remove-text-at-pos 1 (1+ beg))))

(defun web-mode-uncomment-aspx-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (1- end))
    (web-mode-remove-text-at-pos 1 (1+ beg))))

(defun web-mode-uncomment-jsp-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 2 (+ beg 2))))

(defun web-mode-uncomment-go-block (pos)
  (let (beg end)
    (setq beg (web-mode-block-beginning-position pos)
          end (web-mode-block-end-position pos))
    (web-mode-remove-text-at-pos 1 (+ beg 2))))

(defun web-mode-snippet-names ()
  (let (codes)
    (dolist (snippet web-mode-snippets)
      (add-to-list 'codes (car snippet) t))
    codes))

(defun web-mode-snippet-insert (code)
  "Insert a snippet."
  (interactive
   (list (completing-read "Snippet: " (web-mode-snippet-names))))
  (let (beg
        (continue t)
        (counter 0)
        end
        sel
        snippet
        (l (length web-mode-snippets))
        pos)
    (when mark-active
      (setq sel (web-mode-trim (buffer-substring-no-properties
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
      (insert snippet)
      (setq pos (point)
            end (point))
      (when (string-match-p "|" snippet)
        (search-backward "|")
        (delete-char 1)
        (setq pos (point)
              end (1- end)))
      (when sel
        (insert sel)
        (setq pos (point)
              end (+ end (length sel))))
      (goto-char end)
      (setq end (point-at-eol))
      (unless sel (goto-char pos))
      (indent-region beg end))
    ))

(defun web-mode-looking-at (regexp pos)
  (save-excursion
    (goto-char pos)
    (looking-at regexp)))

(defun web-mode-looking-at-p (regexp pos)
  (save-excursion
    (goto-char pos)
    (looking-at-p regexp)))

(defun web-mode-looking-back (regexp pos)
  (save-excursion
    (goto-char pos)
    (looking-back regexp)))

(defun web-mode-insert-text-at-pos (text pos)
  (let ((mem web-mode-enable-auto-pairing))
    (setq web-mode-enable-auto-pairing nil)
    (save-excursion
      (goto-char pos)
      (insert text)
      (setq web-mode-enable-auto-pairing mem)
      )))

(defun web-mode-remove-text-at-pos (n &optional pos)
  (unless pos (setq pos (point)))
  (delete-region pos (+ pos n)))

(defun web-mode-insert-and-indent (text)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)
    ))

(defun web-mode-navigate (&optional pos)
  "Move point to the matching opening/closing tag/block."
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
           (web-mode-block-controls-get (point)))
      (web-mode-block-match))
     ((member (get-text-property pos 'tag-type) '(start end))
      (web-mode-tag-beginning)
      (web-mode-tag-match))
     (t
      (goto-char init))
     )
    ))

(defun web-mode-block-match (&optional pos)
  (unless pos (setq pos (point)))
  (let (pos-ori controls control (counter 1) type (continue t) pair)
    (setq pos-ori pos)
    (goto-char pos)
    (setq controls (web-mode-block-controls-get pos))
;;    (message "controls=%S" controls)
    (cond
     (controls
      (setq pair (car controls))
      (setq control (cdr pair))
      (setq type (car pair))
      (when (eq type 'inside) (setq type 'close))
      (while continue
        (cond
         ((and (> pos-ori 1) (bobp))
          (setq continue nil))
         ((or (and (eq type 'open) (not (web-mode-block-next)))
              (and (eq type 'close) (not (web-mode-block-previous))))
          (setq continue nil)
          )
         ((null (setq controls (web-mode-block-controls-get (point))))
          )
         (t
          ;; TODO : est il nécessaire de faire un reverse sur controls si on doit matcher backward
          (dolist (pair controls)
            (cond
             ((not (string= (cdr pair) control))
              )
             ((eq (car pair) 'inside)
              )
             ((eq (car pair) type)
              (setq counter (1+ counter)))
             (t
              (setq counter (1- counter)))
             )
            ) ;dolist
          (when (= counter 0)
            (setq continue nil))
          ) ;t
         ) ;cond
        ) ;while
      (if (= counter 0) (point) nil)
      ) ;controls
     (t
      (goto-char pos-ori)
      nil
      ) ;controls = nul
     ) ;conf
    ))

(defun web-mode-tag-match (&optional pos)
  (unless pos (setq pos (point)))
  (let (regexp)
    (setq regexp (concat "</?" (get-text-property pos 'tag-name)))
    (when (member (get-text-property pos 'tag-type) '(start end))
      (web-mode-tag-beginning)
      (setq pos (point)))
    (if (eq (get-text-property pos 'tag-type) 'end)
        (web-mode-tag-fetch-opening regexp pos)
      (web-mode-tag-fetch-closing regexp pos))
    t))

(defun web-mode-tag-fetch-opening (regexp pos)
  (let ((counter 1) (n 0) (type nil))
    (goto-char pos)
    (while (and (> counter 0) (re-search-backward regexp nil t))
      (when (and (get-text-property (point) 'tag-beg)
                 (member (get-text-property (point) 'tag-type) '(start end)))
        (setq n (1+ n))
        (cond
         ((eq (get-text-property (point) 'tag-type) 'end)
          (setq counter (1+ counter)))
         (t
          (setq counter (1- counter))
          )
         )
        )
      )
    (if (= n 0) (goto-char pos))
    ))

(defun web-mode-tag-fetch-closing (regexp pos)
  (let ((counter 1) (n 0))
    (goto-char pos)
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

(defun web-mode-element-tag-name (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (if (and (web-mode-tag-beginning)
             (looking-at "<\\(/?[[:alpha:]][[:alnum:]-]*\\)"))
        (match-string-no-properties 1)
      nil)))

(defun web-mode-element-close ()
  "Close html element."
  (interactive)
  (let (jump epp ins tag)
    (setq epp (web-mode-element-parent-position))
    (when epp
;;      (setq tag (get-text-property epp 'tag-name))
      (setq tag (web-mode-element-tag-name epp))
      (cond
       ((null tag)
        (setq epp nil))
       ((looking-back "</")
        (setq ins tag))
       ((looking-back "<")
        (setq ins (concat "/" tag)))
       (t
        ;; auto-close-style = 2
;;        (message "%S %c" (point) (char-after))
        (when (and (looking-at-p "[[:alpha:]]") (> (length tag) 4))
          (dolist (elt '("div" "span" "strong" "pre" "li"))
            (when (and (string-match-p (concat "^" elt) tag) (not (string= tag elt)))
              (setq tag elt)
              (put-text-property epp (point) 'tag-name tag))
            )
          ) ;when
        (if (web-mode-element-is-void (get-text-property (point) 'tag-name))
            (setq ins nil
                  epp nil)
          (setq ins (concat "</" tag)))
        )
       ) ;cond
      (when ins
        ;;        (message "ins=%S" ins)
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
        ) ;when not ins
      ) ;when epp
    epp
    ))

(defun web-mode-detect-content-type ()
  (cond
   ((and (string= web-mode-engine "none")
         (< (point) 16)
         (eq (char-after 1) ?\#)
         (string-match-p "php" (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))
    (web-mode-set-engine "php"))
   ((and (string= web-mode-content-type "javascript")
         (< (point) web-mode-chunk-length)
         (eq (char-after (point-min)) ?\/)
         (string-match-p "@jsx" (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
    (web-mode-set-content-type "jsx"))
   ))

(defun web-mode-on-after-change (beg end len)
;;  (message "after-change: pos=%d, beg=%d, end=%d, len=%d, ocmd=%S, cmd=%S" (point) beg end len this-original-command this-command)
  ;;  (backtrace)
;;  (message "this-command=%S" this-command)
  (when (eq this-original-command 'yank)
    (setq web-mode-inhibit-fontification t))
  (when (or (null web-mode-change-beg) (< beg web-mode-change-beg))
    (setq web-mode-change-beg beg))
  (when (or (null web-mode-change-end) (> end web-mode-change-end))
    (setq web-mode-change-end end))
  )

(defun web-mode-complete ()
  "Autocomple at point."
  (interactive)
  (let ((pos (point))
        (char (char-before))
        (chunk (buffer-substring-no-properties (- (point) 2) (point)))
        (auto-closed   nil)
        (auto-expanded nil)
        (auto-paired   nil)
        (auto-quoted   nil)
        expanders)

    ;;-- auto-closing
    (when (and web-mode-enable-auto-closing
               (>= pos 4)
               (or (string= "</" chunk)
                   (and (= web-mode-auto-close-style 2)
                        (not (get-text-property pos 'part-side))
                        (string-match-p "[[:alnum:]'\"]>" chunk)))
               (not (get-text-property (- pos 2) 'block-side))
               (web-mode-element-close))
      (setq auto-closed t))

    ;;-- auto-pairing
    (when (and web-mode-enable-auto-pairing
               (>= pos 4)
               (not auto-closed))
      (let ((i 0) expr after pos-end (l (length web-mode-auto-pairs)))
        (setq pos-end (if (> (+ pos 32) (line-end-position))
                          (line-end-position)
                        (+ pos 10)))
        (setq chunk (buffer-substring-no-properties (- pos 3) pos)
              after (buffer-substring-no-properties pos pos-end))
        (while (and (< i l) (not auto-paired))
          (setq expr (elt web-mode-auto-pairs i)
                i (1+ i))
          ;;(message "chunk=%S expr=%S after=%S" chunk expr after)
          (when (and (string= (car expr) chunk)
                     (not (string-match-p (regexp-quote (cdr expr)) after)))
            (setq auto-paired t)
            (insert (cdr expr))
            (if (string-match-p "|" (cdr expr))
                (progn
                  (search-backward "|")
                  (delete-char 1))
              (goto-char pos))
            ) ;when
          ) ;while
        ) ;let
      )

    ;;-- auto-expanding
    (when (and web-mode-enable-auto-expanding
               (not auto-closed)
               (not auto-paired)
               (eq char ?\/)
               (not (get-text-property (1- pos) 'tag-type))
               (not (get-text-property (1- pos) 'part-side))
               (not (get-text-property (1- pos) 'block-side))
               (looking-back "\\(^\\|[[:punct:][:space:]>]\\)./"))
      (setq expanders (append web-mode-expanders web-mode-extra-expanders))
      (let ((i 0) pair (l (length expanders)))
        (setq chunk (buffer-substring-no-properties (- pos 2) pos))
        ;;(message "%S" chunk)
        (while (and (< i l) (not auto-expanded))
          (setq pair (elt expanders i)
                i (1+ i))
          (when (string= (car pair) chunk)
            (setq auto-expanded t)
            (delete-char -2)
            (insert (cdr pair))
            (when (string-match-p "|" (cdr pair))
              (search-backward "|")
              (delete-char 1))
            ) ;when
          ) ;while
        ) ; let
      )

    ;;-- auto-quoting
    (when (and web-mode-enable-auto-quoting
               (>= pos 4)
               (not (get-text-property pos 'block-side))
               (not auto-closed)
               (not auto-paired)
               (not auto-expanded)
               (get-text-property (- pos 2) 'tag-attr)
               ;;(not (get-text-property pos 'part-element))
               )
      (cond
       ((and (eq char ?\=)
             (not (looking-at-p "[ ]*[\"']")))
        (insert "\"\"")
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\")
             (not (looking-at-p "[ ]*[\"]")))
        (insert "\"")
        (backward-char)
        (setq auto-quoted t))
       ((and (eq char ?\")
             (eq (char-after) ?\"))
        (if (looking-back "=\"\"")
            (progn
              (delete-char 1)
              (backward-char))
          (delete-char 1)
          ;;(message "%c" (char-after))
          (if (eq (char-after) ?\s)
              (forward-char)
            (insert " "))
          )
        )
       )
      )

    ;;--
    (cond
     ((or auto-closed auto-paired auto-expanded auto-quoted)
      (when (and web-mode-change-end
                 (>= (line-end-position) web-mode-change-end))
        (setq web-mode-change-end (line-end-position)))
      (list :auto-closed auto-closed
            :auto-paired auto-paired
            :auto-expanded auto-expanded
            :auto-quoted auto-quoted))
     (t
      nil)
     )

    ))

(defun web-mode-on-post-command ()
  (let (ctx n char)

    ;;(message "this-command=%S (%S)" this-command web-mode-expand-previous-state)
    ;;(message "%S: %S %S" this-command web-mode-change-beg web-mode-change-end)

    (when (and web-mode-expand-previous-state
               (not (member this-command '(web-mode-mark-and-expand
                                           er/expand-region))))
      (when (eq this-command 'keyboard-quit)
        (goto-char web-mode-expand-initial-pos))
      (deactivate-mark)
      (setq web-mode-expand-previous-state nil
            web-mode-expand-initial-pos nil))

    (when (member this-command '(yank))
      (setq web-mode-inhibit-fontification nil)
      ;;(web-mode-font-lock-highlight web-mode-change-end)
      (when (and web-mode-change-beg web-mode-change-end)
        (save-excursion
          (font-lock-fontify-region web-mode-change-beg web-mode-change-end)
          ))
      )

    (when (< (point) 16)
      (web-mode-detect-content-type))

    (when (and web-mode-enable-engine-detection
               (or (null web-mode-engine) (string= web-mode-engine "none"))
               (< (point) web-mode-chunk-length)
               (web-mode-detect-engine))
      (web-mode-on-engine-setted)
      (web-mode-buffer-highlight))

    (when (> (point) 1)
      (setq char (char-before)))

    (cond

     ((null char)
      )

     ((and (>= (point) 3)
           (member this-command '(self-insert-command))
           (not (member (get-text-property (point) 'part-token) '(comment string))))
      (setq ctx (web-mode-complete)))

     ((and web-mode-enable-auto-opening
           (member this-command '(newline electric-newline-and-maybe-indent))
           (or (and (not (eobp))
                    (eq (char-after) ?\<)
                    (eq (get-text-property (point) 'tag-type) 'end)
                    (looking-back ">\n[ \t]*")
                    (setq n (length (match-string-no-properties 0)))
                    (eq (get-text-property (- (point) n) 'tag-type) 'start)
                    (string= (get-text-property (- (point) n) 'tag-name)
                             (get-text-property (point) 'tag-name))
                    )
               (and (get-text-property (1- (point)) 'block-side)
                    (string= web-mode-engine "php")
                    (looking-back "<\\?php[ ]*\n")
                    (looking-at-p "[ ]*\\?>"))))
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode)
      )
     ) ;cond

    (when (and web-mode-enable-auto-indentation
               (member this-command '(self-insert-command))
               (or (and ctx
                        (or (plist-get ctx :auto-closed)
                            (plist-get ctx :auto-expanded)))
                   (and (> (point) (point-min))
                        (get-text-property (1- (point)) 'tag-end)
                        (get-text-property (line-beginning-position) 'tag-beg))))
      (indent-according-to-mode)
      (when (and web-mode-change-end (> web-mode-change-end (point-max)))
        (message "post-command: enlarge web-mode-change-end")
        (setq web-mode-change-end (point-max))
        )
      ) ; when auto-indent

    (when web-mode-enable-current-element-highlight
      (web-mode-highlight-current-element))

    (when (and web-mode-enable-current-column-highlight
               (not (web-mode-buffer-narrowed-p)))
      (web-mode-column-show))

    ;;(message "post-command (%S) (%S)" web-mode-change-end web-mode-change-end)

    ))

(defun web-mode-dom-apostrophes-replace ()
  "Replace char(') with char(’) in the html contents of the buffer."
  (interactive)
  (save-excursion
    (let ((min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-content-rsf "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" max)
        (replace-match "\\1’\\2"))
      )))

(defun web-mode-dom-entities-encode ()
  (save-excursion
    (let (regexp ms elt (min (point-min)) (max (point-max)))
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
      (while (web-mode-content-rsf regexp max)
        (setq elt (match-string-no-properties 0))
        (setq elt (aref elt 0))
        (setq elt (car (rassoc elt web-mode-html-entities)))
        (replace-match (concat "&" elt ";"))
        ) ;while
      )))

(defun web-mode-dom-entities-replace ()
  "Replace html entities (e.g. &eacute; &#233; or &#x00E9; become é)"
  (interactive)
  (save-excursion
    (let (ms pair elt (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-content-rsf "&\\([#]?[[:alnum:]]\\{2,8\\}\\);" max)
        (setq elt nil)
        (setq ms (match-string-no-properties 1))
        (cond
         ((not (eq (aref ms 0) ?\#))
          (and (setq pair (assoc ms web-mode-html-entities))
               (setq elt (cdr pair))
               (setq elt (char-to-string elt))))
         ((eq (aref ms 1) ?x)
          (setq elt (substring ms 2))
          (setq elt (downcase elt))
          (setq elt (string-to-number elt 16))
          (setq elt (char-to-string elt)))
         (t
          (setq elt (substring ms 1))
          (setq elt (char-to-string (string-to-number elt))))
         ) ;cond
        (when elt (replace-match elt))
        ) ;while
      )))

(defun web-mode-dom-xml-replace ()
  "Replace &, > and < in html content."
  (interactive)
  (save-excursion
    (let (expr (min (point-min)) (max (point-max)))
      (when mark-active
        (setq min (region-beginning)
              max (region-end))
        (deactivate-mark))
      (goto-char min)
      (while (web-mode-content-rsf "[&<>]" max)
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
      (while (web-mode-content-rsf "\\(\"\\)\\(.\\{1,200\\}\\)\\(\"\\)" max)
        (replace-match expr)
        ) ;while
      )))

(defun web-mode-dom-xpath (&optional pos)
  "Display html path."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (let (path)
      (while (web-mode-element-parent)
        (setq path (cons (get-text-property (point) 'tag-name) path))
        )
      (message "/%s" (mapconcat 'identity path "/"))
      )))

(defun web-mode-block-ends-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (save-match-data
      (if (stringp regexp)
          (and (web-mode-block-end)
               (progn (backward-char) t)
               (web-mode-block-skip-blank-backward)
               (progn (forward-char) t)
               (looking-back regexp))
        (let ((pair regexp)
              (block-beg (web-mode-block-beginning-position pos))
              (block-end (web-mode-block-end-position pos)))
          (and (web-mode-block-end)
               (web-mode-block-sb (car pair) block-beg)
               (not (web-mode-sf (cdr pair) block-end)))
          ) ;let
        ) ;if
      )))

(defun web-mode-block-token-starts-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (and (goto-char pos)
         (web-mode-block-token-beginning)
         (skip-chars-forward "[\"']")
         (looking-at regexp))
    ))

(defun web-mode-block-starts-with (regexp &optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (and (web-mode-block-beginning)
         (web-mode-block-skip-blank-forward)
         (looking-at regexp))
    ))

(defun web-mode-block-skip-blank-backward (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (not (bobp))
               (or (member (char-after) '(?\s ?\n))
                   (member (get-text-property (point) 'block-token)
                           '(delimiter-beg delimiter-end comment))))
          (backward-char)
        (setq continue nil))
      ) ;while
    (point)))

(defun web-mode-block-skip-blank-forward (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (goto-char pos)
    (while continue
      (if (and (get-text-property (point) 'block-side)
               (or (member (char-after) '(?\s ?\n ?\t))
                   (member (get-text-property (point) 'block-token)
                           '(delimiter-beg delimiter-end comment))))
          (forward-char)
        (setq continue nil))
      ) ;while
;;    (message "pt=%S" (point))
    (point)))

(defun web-mode-tag-attributes-sort (&optional pos)
  "Sort the attributes inside the current html tag."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (let (attrs (continue t) min max tag-beg tag-end attr attr-name attr-beg attr-end indent indentation sorter ins)
      (if (not (member (get-text-property pos 'tag-type) '(start void)))
          nil
        (setq tag-beg (web-mode-tag-beginning-position pos)
              tag-end (web-mode-tag-end-position))
;;        (message "%S %S" tag-beg tag-end)
        (goto-char tag-beg)
        (while continue
          (if (or (not (web-mode-attribute-next))
                  (>= (point) tag-end))
              (setq continue nil)
            ;;(message "attr=%S" (point))
            (setq attr-beg (web-mode-attribute-beginning-position)
                  attr-end (1+ (web-mode-attribute-end-position)))
            (when (null min)
              (setq min attr-beg))
            (setq max attr-end)
            (goto-char attr-beg)
            (setq attr (buffer-substring-no-properties attr-beg attr-end))
            (if (string-match "^\\([[:alnum:]-]+\\)=" attr)
                (setq attr-name (match-string-no-properties 1 attr))
              (setq attr-name attr))
            (setq indent (looking-back "^[ \t]*"))
            (setq attrs (append attrs (list (list attr-beg attr-end attr-name attr indent))))
            ) ;if
          ) ;while
        ) ;if in tag
      (when attrs
        (setq sorter (function
                      (lambda (elt1 elt2)
                        (string< (nth 2 elt1) (nth 2 elt2))
                        )))
        (setq attrs (sort attrs sorter))
        (delete-region (1- min) max)
        (setq ins "")
        (dolist (elt attrs)
          (if (and (nth 4 elt) (> (length ins) 1))
              (setq ins (concat ins "\n"))
            (setq ins (concat ins " ")))
          (setq ins (concat ins (nth 3 elt)))
          )
        (goto-char (1- min))
        (insert ins)
        (web-mode-tag-beginning)
        (setq min (line-beginning-position))
        (web-mode-tag-end)
        (setq max (line-end-position))
        (indent-region min max)
        )
      ;;(message "attrs=%S" attrs)
      )))

(defun web-mode-attribute-insert ()
  "Insert an attribute inside current tag."
  (interactive)
  (let (attr attr-name attr-value)
    (cond
     ((not (eq (get-text-property (point) 'tag-type) 'start))
      (message "attribute-insert ** invalid context **"))
     ((not (and (setq attr-name (read-from-minibuffer "Attribute name? "))
                (> (length attr-name) 0)))
      (message "attribute-insert ** failure **"))
     (t
      (setq attr (concat " " attr-name))
      (when (setq attr-value (read-from-minibuffer "Attribute value? "))
        (setq attr (concat attr "=\"" attr-value "\"")))
      (web-mode-tag-end)
      (re-search-backward "/?>")
      (insert attr)
      )
     ) ;cond
    ))

(defun web-mode-attribute-transpose (&optional pos)
  "Transpose the current html attribute."
  (interactive)
  (unless pos (setq pos (point)))
  (let (ret attr-beg attr-end next-beg next-end tag-end)
    (when (and (get-text-property pos 'tag-attr)
               (setq next-beg (web-mode-attribute-next-position pos))
               (setq next-end (web-mode-attribute-end-position next-beg))
               (setq tag-end (web-mode-tag-end-position pos))
               (> tag-end next-end))
      (setq attr-beg (web-mode-attribute-beginning-position pos)
            attr-end (web-mode-attribute-end-position pos))
      ;;      (message "%S %S - %S %S" attr-beg attr-end next-beg next-end)
      (transpose-regions attr-beg (1+ attr-end) next-beg (1+ next-end))
      )))

(defun web-mode-attribute-select (&optional pos)
  "Select the current html attribute."
  (interactive)
  (unless pos (setq pos (point)))
  (if (null (get-text-property pos 'tag-attr))
      nil
    (goto-char pos)
    (web-mode-attribute-beginning)
    (set-mark (point))
    (web-mode-attribute-end)
    (point)
    ))

(defun web-mode-attribute-kill ()
  "Kill the current html attribute."
  (interactive)
  (web-mode-attribute-select)
  (when mark-active
    (kill-region (region-beginning) (region-end))))

(defun web-mode-block-close (&optional pos)
  "Close the first unclosed control block."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((continue t)
        (h (make-hash-table :test 'equal)) ctx ctrl n closing-block)
    (save-excursion
      (while (and continue (web-mode-block-previous))
        (when (setq ctx (web-mode-block-is-control (point)))
          (setq ctrl (car ctx))
          (setq n (gethash ctrl h 0))
          (if (cdr ctx)
              (puthash ctrl (1+ n) h)
            (puthash ctrl (1- n) h))
          (when (> (gethash ctrl h) 0)
            (setq continue nil))
          )
        ) ;while
      ) ;save-excursion
    (when (and (null continue)
               (setq closing-block (web-mode-closing-block ctrl)))
      (insert closing-block)
      (indent-according-to-mode)
      ;;      (indent-for-tab-command)
      )
    ))

(defun web-mode-closing-block (type)
  (cond
   ((string= web-mode-engine "php")       (concat "<?php end" type "; ?>"))
   ((string= web-mode-engine "django")    (concat "{% end" type " %}"))
   ((string= web-mode-engine "ctemplate") (concat "{{/" type "}}"))
   ((string= web-mode-engine "blade")
    (if (string= type "section")
        (concat "@show")
      (concat "@end" type)))
   ((string= web-mode-engine "dust")      (concat "{/" type "}"))
   ((string= web-mode-engine "mako")      (concat "% end" type))
   ((string= web-mode-engine "closure")   (concat "{/" type "}"))
   ((string= web-mode-engine "smarty")    (concat "{/" type "}"))
   ((string= web-mode-engine "underscore")        "<% } %>")
   ((string= web-mode-engine "lsp")               "<% ) %>")
   ((string= web-mode-engine "erb")               "<% } %>")
   ((string= web-mode-engine "erb")               "<% end %>")
   ((string= web-mode-engine "go")                "{{end}}")
   ((string= web-mode-engine "velocity")          "#end")
   ((string= web-mode-engine "template-toolkit")  "[% end %]")
   ((member web-mode-engine '("asp" "jsp"))
    (if (string-match-p ":" type) (concat "</" type ">") "<% } %>")
    )
   (t nil)
   ;;(t (cdr (assoc type web-mode-closing-blocks)))
   ) ;cond
  )

(provide 'web-mode-indentation)

;;; web-mode-indentation.el ends here
