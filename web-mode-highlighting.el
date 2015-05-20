;;; web-mode-highlighting.el --- highlighting functionality for web-mode.el
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

(defun web-mode-font-lock-highlight (limit)
  ;;(message "font-lock-highlight: point(%S) limit(%S) change-beg(%S) change-end(%S)" (point) limit web-mode-change-beg web-mode-change-end)
  (cond
   (web-mode-inhibit-fontification
    nil)
   (t
    (web-mode-highlight-region (point) limit)
    nil)
   ))

(defun web-mode-buffer-highlight ()
  (interactive)
  ;;(setq web-mode-change-beg (point-min)
  ;;      web-mode-change-end (point-max))
  ;;  (web-mode-font-lock-highlight (point-max))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (font-lock-fontify-buffer)))

(defun web-mode-extend-region ()
  ;;(message "extend-region: flb(%S) fle(%S) wmcb(%S) wmce(%S)" font-lock-beg font-lock-end web-mode-change-beg web-mode-change-end)
  ;;  (setq font-lock-beg web-mode-change-beg
  ;;        font-lock-end web-mode-change-end)
  (cond
   (web-mode-inhibit-fontification
    nil)
   (t ;;(and web-mode-change-beg web-mode-change-end)
    (when (or (null web-mode-change-beg) (< font-lock-beg web-mode-change-beg))
      ;;(message "font-lock-beg(%S) < web-mode-change-beg(%S)" font-lock-beg web-mode-change-beg)
      (setq web-mode-change-beg font-lock-beg))
    (when (or (null web-mode-change-end) (> font-lock-end web-mode-change-end))
      ;;(message "font-lock-end(%S) > web-mode-change-end(%S)" font-lock-end web-mode-change-end)
      (setq web-mode-change-end font-lock-end))
    (let ((region (web-mode-propertize web-mode-change-beg web-mode-change-end)))
      (when region
        ;;(message "region: %S" region)
        (setq font-lock-beg (car region)
              font-lock-end (cdr region)
              ;;web-mode-change-beg (car region)
              ;;web-mode-change-end (cdr region)
              )
        ) ;when
      ) ;let
    nil) ;t
   ))

(defun web-mode-unfontify-region (beg end)
  ;;(message "unfontify: %S %S" beg end)
  )

(defun web-mode-highlight-region (&optional beg end) ;; content-type)
;;  (message "highlight-region: beg(%S) end(%S) ct(%S)" beg end content-type)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let (
               (buffer-undo-list t)
               ;;(inhibit-modification-hooks t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end '(font-lock-face face))
           (cond
            ((and (get-text-property beg 'block-side)
                  (not (get-text-property beg 'block-beg)))
             (web-mode-block-highlight beg end))
            ((or (member web-mode-content-type web-mode-part-content-types)
                 ;;(member content-type web-mode-part-content-types)
                 (get-text-property beg 'part-side))
             (web-mode-part-highlight beg end)
             (web-mode-process-blocks beg end 'web-mode-block-highlight))
            ((string= web-mode-engine "none")
             (web-mode-highlight-tags beg end)
             (web-mode-process-parts beg end 'web-mode-part-highlight))
            (t
             (web-mode-highlight-tags beg end)
             (web-mode-process-parts beg end 'web-mode-part-highlight)
             (web-mode-process-blocks beg end 'web-mode-block-highlight))
            ) ;cond
           (when web-mode-enable-element-content-fontification
             (web-mode-highlight-elements beg end))
           (when web-mode-enable-whitespace-fontification
             (web-mode-highlight-whitespaces beg end))
           ;;(message "%S %S" font-lock-keywords font-lock-keywords-alist)
           ))))))

(defun web-mode-highlight-tags (reg-beg reg-end)
  (let ((continue t))
    (goto-char reg-beg)
    (when (and (not (get-text-property (point) 'tag-beg))
               (not (web-mode-tag-next)))
      (setq continue nil))
    (when (and continue (>= (point) reg-end))
      (setq continue nil))
    (while continue
      (web-mode-tag-highlight)
      (when (or (not (web-mode-tag-next))
                (>= (point) reg-end))
        (setq continue nil))
      ) ;while
    (when web-mode-enable-inlays
      (when (null web-mode-inlay-regexp)
        (setq web-mode-inlay-regexp (regexp-opt '("\\[" "\\(" "\\begin{align}"))))
      (let (beg end expr)
        (goto-char reg-beg)
        (while (web-mode-dom-rsf web-mode-inlay-regexp reg-end)
          (setq beg (match-beginning 0)
                end nil
                expr (substring (match-string-no-properties 0) 0 2))
          (setq expr (cond
                      ((string= expr "\\[") "\\]")
                      ((string= expr "\\(") "\\)")
                      (t "\\end{align}")))
          (when (and (web-mode-dom-sf expr reg-end)
                     (setq end (match-end 0))
                     (not (text-property-any beg end 'tag-end t)))
            (font-lock-append-text-property beg end 'font-lock-face 'web-mode-inlay-face)
            ) ;when
          ) ;while
        ) ;let
      ) ;when
    ))

(defun web-mode-tag-highlight (&optional beg end)
  (unless beg (setq beg (point)))
  (unless end (setq end (1+ (web-mode-tag-end-position beg))))
  (let (name type face flags slash-beg slash-end bracket-end)
    (setq flags (get-text-property beg 'tag-beg)
          type (get-text-property beg 'tag-type)
          name (get-text-property beg 'tag-name))
    (cond
     ((eq type 'comment)
      (put-text-property beg end 'font-lock-face 'web-mode-comment-face)
      (when (and web-mode-enable-comment-keywords (> (- end beg) 5))
        (web-mode-interpolate-comment beg end nil)))
     ((eq type 'cdata)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     ((eq type 'doctype)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     ((eq type 'declaration)
      (put-text-property beg end 'font-lock-face 'web-mode-doctype-face))
     (name
      (setq face (cond
                  ((and web-mode-enable-element-tag-fontification
                        (setq face (cdr (assoc name web-mode-element-tag-faces))))
                   face)
                  ((> (logand flags 2) 0) 'web-mode-html-tag-custom-face)
                  (t                      'web-mode-html-tag-face))
            slash-beg (> (logand flags 4) 0)
            slash-end (> (logand flags 8) 0)
            bracket-end (> (logand flags 16) 0))
      (put-text-property beg
                         (+ beg (if slash-beg 2 1))
                         'font-lock-face 'web-mode-html-tag-bracket-face)
      (put-text-property (+ beg (if slash-beg 2 1))
                         (+ beg (if slash-beg 2 1) (length name))
                         'font-lock-face face)
      (when (or slash-end bracket-end)
        (put-text-property (- end (if slash-end 2 1)) end 'font-lock-face 'web-mode-html-tag-bracket-face)
        ) ;when
      (when (> (logand flags 1) 0)
        (web-mode-highlight-attrs beg end))
      ) ;case name
     ) ;cond
    ))

(defun web-mode-highlight-attrs (reg-beg reg-end)
  (let ((continue t) (pos reg-beg) beg end flags offset face)
    (while continue
      (setq beg (next-single-property-change pos 'tag-attr))
      (if (and beg (< beg reg-end))
          (progn
            (setq flags (or (get-text-property beg 'tag-attr) 0))
            (setq face (cond
                        ((= (logand flags 1) 1) 'web-mode-html-attr-custom-face)
                        ((= (logand flags 2) 2) 'web-mode-html-attr-engine-face)
                        (t                      'web-mode-html-attr-name-face)))
            (if (get-text-property beg 'tag-attr-end)
                (setq end beg)
              (setq end (next-single-property-change beg 'tag-attr-end)))
            (if (and end (< end reg-end))
                (progn
                  (setq offset (get-text-property end 'tag-attr-end))
                  (if (= offset 0)
                      (put-text-property beg (1+ end) 'font-lock-face face)
                    (put-text-property beg (+ beg offset) 'font-lock-face face)
                    (put-text-property (+ beg offset) (+ beg offset 1)
                                       'font-lock-face
                                       'web-mode-html-attr-equal-face)
                    (put-text-property (+ beg offset 1) (1+ end)
                                       'font-lock-face
                                       (if (get-text-property (+ beg offset 1) 'part-expr)
                                           nil
                                         'web-mode-html-attr-value-face))
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

(defun web-mode-block-highlight (reg-beg reg-end)
  (let (sub1 sub2 sub3 continue char keywords token-type face beg end (buffer (current-buffer)))
    ;;(message "reg-beg=%S reg-end=%S" reg-beg reg-end)

    ;; NOTE: required for block inside tag attr
    (remove-list-of-text-properties reg-beg reg-end '(font-lock-face))

    (goto-char reg-beg)

    (when (null web-mode-engine-font-lock-keywords)
      (setq sub1 (buffer-substring-no-properties
                  reg-beg (+ reg-beg 1))
            sub2 (buffer-substring-no-properties
                  reg-beg (+ reg-beg 2))
            sub3 (buffer-substring-no-properties
                  reg-beg (+ reg-beg (if (>= (point-max) (+ reg-beg 3)) 3 2))))
      )

    (cond

     ((and (get-text-property reg-beg 'block-beg)
           (eq (get-text-property reg-beg 'block-token) 'comment))
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
       ((eq (aref sub2 0) ?\%)
        (setq keywords web-mode-mako-block-font-lock-keywords))
       ((member sub2 '("<%" "</"))
        (setq keywords web-mode-mako-tag-font-lock-keywords))
       ((member sub2 '("${"))
        (setq keywords web-mode-uel-font-lock-keywords))
       )) ;mako

     ((string= web-mode-engine "mason")
      ;;(message "%S %S" sub2 sub3)
      (cond
       ((member sub3 '("<% " "<%\n" "<&|"))
        (setq keywords web-mode-mason-code-font-lock-keywords))
       ((eq (aref sub2 0) ?\%)
        (setq keywords web-mode-mason-code-font-lock-keywords))
       ((and (or (string= sub2 "<%") (string= sub3 "</%"))
             (not (member sub3 '("<%c" "<%i" "<%p"))))
        (setq keywords web-mode-mason-block-font-lock-keywords))
       (t
        (setq keywords web-mode-mason-code-font-lock-keywords))
       )) ;mason

     ((string= web-mode-engine "jsp")
      (cond
       ((string= sub3 "<%@")
        (setq keywords web-mode-directive-font-lock-keywords))
       ((member sub2 '("${" "#{"))
        (setq keywords web-mode-uel-font-lock-keywords))
       ((string= sub2 "<%")
        (setq keywords web-mode-jsp-font-lock-keywords))
       (t
        (setq keywords web-mode-engine-tag-font-lock-keywords))
       )) ;jsp

     ((string= web-mode-engine "asp")
      (cond
       ((or (string= sub2 "<%")
            (not (string= sub1 "<")))
        (setq keywords web-mode-asp-font-lock-keywords))
       (t
        (setq keywords web-mode-engine-tag-font-lock-keywords))
       )) ;asp

     ((string= web-mode-engine "clip")
      (setq keywords web-mode-engine-tag-font-lock-keywords)
      ) ;clip

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
        (setq keywords web-mode-engine-tag-font-lock-keywords))
       )) ;freemarker

     ) ;cond

    (when keywords
      (web-mode-fontify-region reg-beg reg-end keywords)
      (setq continue t)
      (setq end reg-beg)
      (while continue
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
                    ;;(message "%S > %S face(%S)" beg end face)
                    (remove-list-of-text-properties beg end '(face))
                    (put-text-property beg end 'font-lock-face face)
                    )
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
                       (string-match-p "JS\\|JAVASCRIPT\\|HTM\\|CSS" (buffer-substring-no-properties beg end)))
              (setq keywords (if (eq ?H (char-after (+ beg 3)))
                                 web-mode-html-font-lock-keywords
                               web-mode-javascript-font-lock-keywords))
              (web-mode-fontify-region beg end keywords)
            ))
;;          (message "%S %c %S beg=%S end=%S" web-mode-enable-string-interpolation char web-mode-engine beg end)
          (when (and web-mode-enable-string-interpolation
                     (member char '(?\" ?\<))
                     (member web-mode-engine '("php" "erb"))
                     (> (- end beg) 4))
            (web-mode-interpolate-block-string beg end)
            ) ;when
          (when (and web-mode-enable-comment-keywords
                     (eq token-type 'comment)
                     (> (- end beg) 3))
            (web-mode-interpolate-comment beg end t)
            ) ;when
          (when (and (eq token-type 'string)
                     (> (- end beg) 6)
                     (web-mode-looking-at-p (concat "[ \n]*" web-mode-sql-queries) (1+ beg)))
            (web-mode-interpolate-sql-string beg end)
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

(defun web-mode-part-highlight (reg-beg reg-end)
  (save-excursion
    (let (start continue token-type face beg end string-face comment-face content-type)
;;      (message "part-highlight: reg-beg(%S) reg-end(%S)" reg-beg reg-end)
      (if (member web-mode-content-type web-mode-part-content-types)
          (setq content-type web-mode-content-type)
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side))))
      (goto-char reg-beg)
      (cond
       ((member content-type '("javascript" "jsx"))
        (setq string-face 'web-mode-javascript-string-face
              comment-face 'web-mode-javascript-comment-face)
        (web-mode-fontify-region reg-beg reg-end web-mode-javascript-font-lock-keywords)
        )
       ((string= content-type "json")
        (setq string-face 'web-mode-json-string-face
              comment-face 'web-mode-json-comment-face)
        (web-mode-fontify-region reg-beg reg-end web-mode-javascript-font-lock-keywords))
       ((string= content-type "css")
        (setq string-face 'web-mode-css-string-face
              comment-face 'web-mode-css-comment-face)
        (web-mode-css-rules-highlight reg-beg reg-end)
        )
       (t
        (setq string-face 'web-mode-part-string-face
              comment-face 'web-mode-part-comment-face)
        )
       )
      (when (string= content-type "jsx") (web-mode-highlight-tags reg-beg reg-end))
      (setq continue t
            end reg-beg)
      (while continue
        (if (and (= reg-beg end)
                 (get-text-property end 'part-token))
            (setq beg reg-beg)
          (setq beg (next-single-property-change end 'part-token)))
        (setq end nil)
        (if (and beg (< beg reg-end))
            (progn
              (setq token-type (get-text-property beg 'part-token))
              (setq face (cond
                          ((eq token-type 'string)  string-face)
                          ((eq token-type 'comment) comment-face)
                          ((eq token-type 'context) 'web-mode-json-context-face)
                          ((eq token-type 'key)     'web-mode-json-key-face)
                          (t                        nil)))
              (setq end (or (next-single-property-change beg 'part-token) (point-max)))
              (if (<= end reg-end)
                  (cond
                   (face
                    (remove-list-of-text-properties beg end '(face))
                    (put-text-property beg end 'font-lock-face face)
                    (when (and web-mode-enable-string-interpolation
                               (string= content-type "javascript")
                               (>= (- end beg) 6))
                      (web-mode-interpolate-javascript-string beg end))
                    ) ;face
                   ) ;cond
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
      (when (and (string= web-mode-content-type "html") web-mode-enable-part-face)
        (font-lock-append-text-property reg-beg reg-end 'face
                                        (if (string= content-type "javascript")
                                            'web-mode-script-face
                                          'web-mode-style-face))
        )
      (when (string= content-type "jsx")
        (goto-char reg-beg)
        ;;(web-mode-highlight-tags reg-beg reg-end)
        (setq continue t
              end reg-beg)
        (while continue
          (setq beg (next-single-property-change end 'part-expr)
                end nil)
;;          (message "beg=%S reg-end=%S" beg reg-end)
          (if (and beg (< beg reg-end))
              (progn
                (setq end (next-single-property-change beg 'part-expr))
;;                (message "bounds %S %S"  beg end)
                (if (and end (< end reg-end))
                    (progn
;;                      (remove-text-properties beg end '(face nil))
                      (put-text-property beg (1+ beg) 'font-lock-face
                                         'web-mode-block-delimiter-face)
                      (put-text-property (1- end) end 'font-lock-face
                                         'web-mode-block-delimiter-face)
                      (web-mode-fontify-region (1+ beg) (1- end)
                                               web-mode-javascript-font-lock-keywords)
                      ) ;progn
                  (setq continue nil
                        end nil))
                ) ;progn
            (setq continue nil
                  end nil))
          ) ;while
        ) ;when jsx
      )))

(defun web-mode-css-rules-highlight (part-beg part-end)
  (save-excursion
    (goto-char part-beg)
    (let (rule (continue t) (i 0) (at-rule nil))
      (while continue
        (setq rule (web-mode-css-rule-next part-end))
        ;;(message "rule=%S" rule)
        (cond
         ((> (setq i (1+ i)) 1000)
          (message "css-rules-highlight ** too much rules **")
          (setq continue nil))
         ((null rule)
          (setq continue nil))
         ((and (setq at-rule (plist-get rule :at-rule))
               (not (member at-rule '("charset" "font-face" "import")))
               (plist-get rule :dec-end))
          (web-mode-css-rule-highlight (plist-get rule :sel-beg)
                                       (plist-get rule :sel-end)
                                       nil nil)
          (web-mode-css-rules-highlight (plist-get rule :dec-beg)
                                        (plist-get rule :dec-end)))
         (t
          (web-mode-css-rule-highlight (plist-get rule :sel-beg)
                                       (plist-get rule :sel-end)
                                       (plist-get rule :dec-beg)
                                       (plist-get rule :dec-end)))
         ) ;cond
        ) ;while
      ) ;let
    ))

(defun web-mode-css-rule-highlight (sel-beg sel-end dec-beg dec-end)
  (save-excursion
;;    (message "sel-beg=%S sel-end=%S dec-beg=%S dec-end=%S" sel-beg sel-end dec-beg dec-end)
    (web-mode-fontify-region sel-beg sel-end
                             web-mode-selector-font-lock-keywords)
    (when (and dec-beg dec-end)
      (web-mode-fontify-region dec-beg dec-end
                               web-mode-declaration-font-lock-keywords)
      (goto-char dec-beg)
      (while (and web-mode-enable-css-colorization
                  (re-search-forward "#[0-9a-fA-F]\\{6\\}\\|#[0-9a-fA-F]\\{3\\}\\|rgb([ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)\\(.*?\\))" dec-end t)
                  (< (point) dec-end))
        (web-mode-colorize (match-beginning 0) (match-end 0))
        )
      )))

(defun web-mode-fontify-region (beg end keywords)
;;  (message "beg=%S end=%S" beg end);; (symbol-name keywords))
  (save-excursion
    (let ((font-lock-keywords keywords)
          (font-lock-multiline nil)
          (font-lock-keywords-case-fold-search
           (member web-mode-engine '("asp" "template-toolkit")))
          (font-lock-keywords-only t)
          (font-lock-extend-region-functions nil))
      ;;      (message "%S" keywords)
      (when (listp font-lock-keywords)
        (font-lock-fontify-region beg end)
        )
      )
    ))

(defun web-mode-colorize-foreground (color)
  (let* ((values (x-color-values color))
	 (r (car values))
	 (g (cadr values))
	 (b (car (cdr (cdr values)))))
    (if (> 128.0 (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256))
	"white" "black")))

(defun web-mode-colorize (beg end)
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

(defun web-mode-interpolate-block-tag (beg end)
  (save-excursion
    (goto-char (+ 4 beg))
    (setq end (1- end))
    (while (re-search-forward "${.*?}" end t)
      (remove-list-of-text-properties (match-beginning 0) (match-end 0) '(face))
      (web-mode-fontify-region (match-beginning 0) (match-end 0)
                               web-mode-uel-font-lock-keywords))
    ))

(defun web-mode-interpolate-javascript-string (beg end)
  (save-excursion
    (goto-char (1+ beg))
    (setq end (1- end))
    (while (re-search-forward "${.*?}" end t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         'web-mode-variable-name-face)
      )
    ))

;; todo : parsing plus compliqué: {$obj->values[3]->name}
(defun web-mode-interpolate-block-string (beg end)
  (save-excursion
    (goto-char (1+ beg))
    (setq end (1- end))
    (cond
     ((string= web-mode-engine "php")
      (while (re-search-forward "$[[:alnum:]_]+\\(->[[:alnum:]_]+\\)*\\|{[ ]*$.+?}" end t)
;;        (message "%S > %S" (match-beginning 0) (match-end 0))
        (remove-list-of-text-properties (match-beginning 0) (match-end 0) '(font-lock-face))
        (web-mode-fontify-region (match-beginning 0) (match-end 0)
                                 web-mode-php-var-interpolation-font-lock-keywords)
        ))
     ((string= web-mode-engine "erb")
      (while (re-search-forward "#{.*?}" end t)
        (remove-list-of-text-properties (match-beginning 0) (match-end 0) '(font-lock-face))
        (put-text-property (match-beginning 0) (match-end 0)
                           'font-lock-face 'web-mode-variable-name-face)
        ))
     ) ;cond
    ))

(defun web-mode-interpolate-comment (beg end block-side)
  (save-excursion
    (let ((regexp (concat "\\<\\(" web-mode-comment-keywords "\\)\\>")))
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (font-lock-prepend-text-property (match-beginning 1) (match-end 1)
                                         'font-lock-face
                                         'web-mode-comment-keyword-face)
        ) ;while
      )))

(defun web-mode-interpolate-sql-string (beg end)
  (save-excursion
    (let ((regexp (concat "\\<\\(" web-mode-sql-keywords "\\)\\>")))
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (font-lock-prepend-text-property (match-beginning 1) (match-end 1)
                                         'font-lock-face
                                         'web-mode-sql-keyword-face)
        ) ;while
      )))

(defun web-mode-fill-paragraph (&optional justify)
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
          )
        ) ;comment - case

       ((web-mode-is-content)
        (setq pair (web-mode-content-boundaries pos))
        (setq beg (car pair)
              end (cdr pair))
        )

       ) ;cond
;;      (message "beg(%S) end(%S)" beg end)
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

;; verifier avec text-property-any si 'block-side
(defun web-mode-content-apply (&optional fun)
  (save-excursion
    (let ((beg nil) (i 0) (continue t))
      (goto-char (point-min))
      (when (get-text-property (point) 'tag-type)
        (web-mode-tag-end)
        (setq beg (point)))
      (while (and continue
                  (or (get-text-property (point) 'tag-beg)
                      (web-mode-tag-next)))
        (when (> (setq i (1+ i)) 2000)
          (message "content-apply ** warning **")
          (setq continue nil))
        (when (and beg (> (point) beg))
          (message "content-apply ** beg(%S) > pt(%S) **" beg (point)))
        (if (web-mode-tag-end)
            (setq beg (point))
          (setq continue nil))
        ) ;while
      )))

(defun web-mode-content-boundaries (&optional pos)
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
;;    (message "beg(%S) end(%S)" beg end)
    (cons beg end)
    ))

(defun web-mode-engine-syntax-check ()
  (interactive)
  (let ((proc nil)
        (errors nil)
        (file (concat temporary-file-directory "emacs-web-mode-tmp")))
    (write-region (point-min) (point-max) file)
    (cond
     ;;       ((null (buffer-file-name))
     ;;        )
     ((string= web-mode-engine "php")
      (setq proc (start-process "php-proc" nil "php" "-l" file))
      (set-process-filter proc
                          (lambda (proc output)
                            (cond
                             ((string-match-p "No syntax errors" output)
                              (message "No syntax errors")
                              )
                             (t
;;                              (setq output (replace-regexp-in-string temporary-file-directory "" output))
;;                              (message output)
                              (message "Syntax error")
                              (setq errors t))
                             ) ;cond
;;                            (delete-file file)
                            ) ;lambda
                          )
      ) ;php
     (t
      (message "no syntax checker found")
      ) ;t
     ) ;cond
    errors))

(defun web-mode-jshint ()
  "Run JSHint on all the JavaScript parts."
  (interactive)
  (let (proc lines)
    (when (buffer-file-name)
      (setq proc (start-process
                  "jshint-proc"
                  nil
                  (or (executable-find "jshint") "/usr/local/bin/jshint")
                  "--extract=auto"
                  (buffer-file-name)))
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
                                (setq pos (web-mode-coord-position
                                           (match-string-no-properties 1 output)
                                           (match-string-no-properties 2 output)))
                                (when (get-text-property pos 'tag-beg)
                                  (setq pos (1- pos)))
                                (when (not (= pos old))
                                  (setq old pos)
                                  (setq overlay (make-overlay pos (1+ pos)))
                                  (overlay-put overlay 'font-lock-face 'web-mode-error-face)
                                  )
                                (setq msg (or (overlay-get overlay 'help-echo)
                                               (concat "line="
                                                       (match-string-no-properties 1 output)
                                                       " column="
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
          (setq tags (cdr tags)))

        )
       ) ;cond
      (when (not (web-mode-tag-next))
        (setq continue nil))
      ) ;while
    (message "%S error(s) detected" errors)
    (if (< errors 1)
        (goto-char ori)
      (goto-char first)
      (recenter))
    ;;    (message "%S" tags)
    ))

(defun web-mode-highlight-elements (beg end)
  (save-excursion
    (goto-char beg)
    (let ((continue (or (get-text-property (point) 'tag-beg) (web-mode-tag-next)))
          (i 0) (ctx nil) (face nil))
      (while continue
        (cond
         ((> (setq i (1+ i)) 1000)
          (message "highlight-elements ** too much tags **")
          (setq continue nil))
         ((> (point) end)
          (setq continue nil))
         ((not (get-text-property (point) 'tag-beg))
          (setq continue nil))
         ((eq (get-text-property (point) 'tag-type) 'start)
          (when (and (setq ctx (web-mode-element-boundaries (point)))
                     (<= (car (cdr ctx)) end)
                     (setq face (cdr (assoc (get-text-property (point) 'tag-name) web-mode-element-content-faces))))
            (font-lock-prepend-text-property (1+ (cdr (car ctx))) (car (cdr ctx))
                                             'font-lock-face face))
          )
         ) ;cond
        (when (not (web-mode-tag-next))
          (setq continue nil))
        ) ;while
      )))

(defun web-mode-enable (feature)
  "Enable one feature."
  (interactive
   (list (completing-read
          "Feature: "
          (let (features)
            (dolist (elt web-mode-features)
              (setq features (append features (list (car elt)))))
            features))))
  (when (and (or (not feature) (< (length feature) 1)) web-mode-last-enabled-feature)
    (setq feature web-mode-last-enabled-feature))
  (when feature
    (setq web-mode-last-enabled-feature feature)
    (setq feature (cdr (assoc feature web-mode-features)))
    (cond
     ((eq feature 'web-mode-enable-current-column-highlight)
      (web-mode-column-show))
     ((eq feature 'web-mode-enable-current-element-highlight)
      (when (not web-mode-enable-current-element-highlight)
        (web-mode-toggle-current-element-highlight))
      )
     ((eq feature 'web-mode-enable-whitespace-fontification)
      (web-mode-whitespaces-on))
     (t
      (set feature t)
      (web-mode-buffer-highlight))
     )
    ) ;when
  )

(defun web-mode-disable (feature)
  "Disable one feature."
  (interactive
   (list (completing-read
          "Feature: "
          (let (features)
            (dolist (elt web-mode-features)
              (setq features (append features (list (car elt)))))
            features))))
  (when (and (or (not feature) (< (length feature) 1)) web-mode-last-enabled-feature)
    (setq feature web-mode-last-enabled-feature))
  (when feature
    (setq feature (cdr (assoc feature web-mode-features)))
    (cond
     ((eq feature 'web-mode-enable-current-column-highlight)
      (web-mode-column-hide))
     ((eq feature 'web-mode-enable-current-element-highlight)
      (when web-mode-enable-current-element-highlight
        (web-mode-toggle-current-element-highlight))
      )
     ((eq feature 'web-mode-enable-whitespace-fontification)
      (web-mode-whitespaces-off))
     (t
      (set feature nil)
      (web-mode-buffer-highlight))
     )
    ) ;when
  )

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

(defun web-mode-column-overlay-factory (index)
  (let (overlay)
    (when (null web-mode-column-overlays)
      (dotimes (i 100)
        (setq overlay (make-overlay 1 1))
        (overlay-put overlay 'font-lock-face 'web-mode-current-column-highlight-face)
        (setq web-mode-column-overlays (append web-mode-column-overlays (list overlay)))
        )
      )
    (setq overlay (nth index web-mode-column-overlays))
    (when (null overlay)
;;      (message "new overlay(%S)" index)
      (setq overlay (make-overlay 1 1))
      (overlay-put overlay 'font-lock-face 'web-mode-current-column-highlight-face)
      (setq web-mode-column-overlays (append web-mode-column-overlays (list overlay)))
      ) ;when
    overlay))

(defun web-mode-column-hide ()
  (setq web-mode-enable-current-column-highlight nil)
  (remove-overlays (point-min) (point-max)
                   'font-lock-face
                   'web-mode-current-column-highlight-face))

(defun web-mode-column-show ()
  (let ((index 0) overlay diff column line-to line-from)
    (web-mode-column-hide)
    (setq web-mode-enable-current-column-highlight t)
    (save-excursion
      (back-to-indentation)
      (setq column (current-column)
            line-to (web-mode-line-number))
      (when (and (get-text-property (point) 'tag-beg)
                 (member (get-text-property (point) 'tag-type) '(start end))
                 (web-mode-tag-match)
                 (setq line-from (web-mode-line-number))
                 (not (= line-from line-to)))
        (when (> line-from line-to)
          (let (tmp)
            (setq tmp line-from)
            (setq line-from line-to)
            (setq line-to tmp))
          ) ;when
        ;;(message "column(%S) line-from(%S) line-to(%S)" column line-from line-to)
        (goto-char (point-min))
        (when (> line-from 1)
          (forward-line (1- line-from)))
        (while (<= line-from line-to)
          (setq overlay (web-mode-column-overlay-factory index))
          (setq diff (- (line-end-position) (point)))
          (cond
           ((or (and (= column 0) (= diff 0))
                (> column diff))
            (end-of-line)
            (move-overlay overlay (point) (point))
            (overlay-put overlay
                         'after-string
                         (concat
                          (if (> column diff) (make-string (- column diff) ?\s) "")
                          (propertize " "
                                      'font-lock-face
                                      'web-mode-current-column-highlight-face)
                          ) ;concat
                         )
            )
           (t
            (move-to-column column)
            (overlay-put overlay 'after-string nil)
            (move-overlay overlay (point) (1+ (point)))
            )
           ) ;cond
          (setq line-from (1+ line-from))
          (forward-line)
          (setq index (1+ index))
          ) ;while
        ) ;when
      ) ;save-excursion
    ) ;let
  )

(defun web-mode-highlight-current-element ()
  (let ((ctx (web-mode-element-boundaries)))
    ;;    (message "%S" ctx)
    (if (null ctx)
        (web-mode-delete-tag-overlays)
      (web-mode-make-tag-overlays)
      (move-overlay web-mode-start-tag-overlay (caar ctx) (1+ (cdar ctx)))
      (move-overlay web-mode-end-tag-overlay (cadr ctx) (1+ (cddr ctx))))
    ))

(defun web-mode-highlight-whitespaces (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward web-mode-whitespaces-regexp end t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face web-mode-whitespace-face))
      ) ;while
    ))

(defun web-mode-whitespaces-show ()
  "Toggle whitespaces."
  (interactive)
  (if web-mode-enable-whitespace-fontification
      (web-mode-whitespaces-off)
    (web-mode-whitespaces-on)))

(defun web-mode-whitespaces-on ()
  "Show whitespaces."
  (interactive)
  (when web-mode-display-table
    (setq buffer-display-table web-mode-display-table))
  (setq web-mode-enable-whitespace-fontification t))

(defun web-mode-whitespaces-off ()
  (setq buffer-display-table nil)
  (setq web-mode-enable-whitespace-fontification nil))

(defun web-mode-use-tabs ()
  "Tweaks vars to be compatible with TAB indentation."
  (let (offset)
    (setq web-mode-block-padding 0)
    (setq web-mode-script-padding 0)
    (setq web-mode-style-padding 0)
    (setq offset
          (cond
           ((and (boundp 'tab-width) tab-width) tab-width)
           ((and (boundp 'standard-indent) standard-indent) standard-indent)
           (t 4)))
;;    (message "offset(%S)" offset)
    (setq web-mode-attr-indent-offset offset)
    (setq web-mode-code-indent-offset offset)
    (setq web-mode-css-indent-offset offset)
    (setq web-mode-markup-indent-offset offset)
    (setq web-mode-sql-indent-offset offset)
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    ))

(defun web-mode-buffer-indent ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun web-mode-buffer-change-tag-case (&optional type)
  "Change html tag case."
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
  "Change case of html attribute names."
  (interactive)
  (unless type (setq type "downcase"))
  (save-excursion
    (goto-char (point-min))
    (let ((continue t)
          (fun (if (eq (aref (downcase type) 0) ?u) 'uppercase 'downcase)))
      (while continue
        (cond
         ((not (web-mode-attribute-next))
          (setq continue nil))
         ((looking-at "\\([[:alnum:]-]+\\)")
          (replace-match (funcall fun (match-string 0)) t)
          )
         ) ;cond
        ) ;while
      )))

;; todo : passer de règle en règle et mettre un \n à la fin
(defun web-mode-css-indent ()
  (save-excursion
    (goto-char (point-min))
    (let ((continue t) rule part-end)
      (while continue
        (cond
         ((not (web-mode-part-next))
          (setq continue nil))
         ((eq (get-text-property (point) 'part-side) 'css)
          (setq part-end (web-mode-part-end-position))
          (while (setq rule (web-mode-css-rule-next part-end))
            (when (not (looking-at-p "[[:space:]]*\\($\\|<\\)"))
              (newline)
              (indent-according-to-mode)
              (setq part-end (web-mode-part-end-position)))
            )
          )
         ) ;cond
        )
      )))

;; tag-case=lower|upper-case , attr-case=lower|upper-case
;; special-chars=unicode|html-entities
;; smart-apostrophes=bool , smart-quotes=bool , indentation=bool
(defun web-mode-dom-normalize ()
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

(provide 'web-mode-highlighting)

;;; web-mode-highlighting.el ends here
