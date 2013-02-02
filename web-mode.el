;;; web-mode.el --- major mode for editing HTML templates

;; Copyright (C) 2011-2013 François-Xavier Bois

;; =========================================================================
;; This work is sponsored by KerniX : Digital Agency (Web & Mobile) in Paris
;; =========================================================================
;; Version: 4.0
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: Web Template HTML PHP JavaScript CSS Js
;;           JSP ASP ERB Twig Jinja Mustache
;;           FreeMarker Django Velocity Cheetah Smarty
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

(defgroup web-mode nil
  "Major mode for editing web templates.
`web-mode' is compatible with many template engines: php, jsp, aspx, erb, django/twig/jinja2, CTemplate/Mustache/Hapax.
HTML files can embed various kinds of blocks: javascript / css / code."
  :version "4.0"
  :group 'languages)

(defgroup web-mode-faces nil
  "Faces for syntax highlighting."
  :group 'web-mode
  :group 'faces)

(defcustom web-mode-markup-indent-offset 2
  "HTML indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-css-indent-offset 2
  "CSS indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-code-indent-offset 2
  "General code indentation level."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-disable-css-colorization (not (display-graphic-p))
  "In a style block, do not set background according to the color: #xxx, rgb(x,x,x)."
  :type 'boolean
  :group 'web-mode)

(defcustom web-mode-disable-autocompletion (not (display-graphic-p))
  "Disable autocompletion."
  :type 'bool
  :group 'web-mode)

(defcustom web-mode-indent-style 1
  "Indentation style (1=standard, 2=leftish).
With the value 2 blocks like <?php for (): ?> stay on the left (no indentation)."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-tag-autocomplete-style 1
  "Tag autocomplete style:
0=no autocomplete
1=autocomplete with </
2=autocomplete with > and </."
  :type 'integer
  :group 'web-mode)

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
  "Face for HTML attribute names (including =)."
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

(defconst web-mode-void-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"
    "tmpl_var" "h:inputtext" "jsp:usebean"
    "#include" "#assign" "#import" "#else")
  "Void (self-closing) tags.")

(defconst web-mode-text-properties
  '(client-tag-name nil client-tag-type nil server-tag-name nil server-tag-type nil client-language nil server-engine nil client-side nil server-side nil client-type nil server-type nil client-pos nil server-pos nil face nil)
  "Text properties used for fontification and indentation.")

(defvar web-mode-expand-first-pos nil
  "First mark pos.")

(defvar web-mode-expand-last-type ""
  "Last mark type.")

(defface web-mode-folded-face
  '((t :underline t))
  "Overlay face for folded."
  :group 'web-mode-faces)

(defvar web-mode-tag-regexp "<\\(/?[[:alpha:]@#][[:alnum:]:_]*\\)"
  "Regular expression for HTML/XML tag.")

(defvar web-mode-server-blocks-regexp nil
  "Regular expression for identifying server blocks.")

(defvar web-mode-engine nil
  "Template engine")

(defvar web-mode-engine-families
  '(("django"    . ("twig" "jinja" "jinja2"))
    ("erb"       . ("eruby" "ember" "erubis"))
    ("velocity"  . ("cheetah"))
    ("ctemplate" . ("mustache" "handlebars" "hapax" "ngtemplate")))
  "Engine name aliases")

(defvar web-mode-file-type ""
  "Buffer file type.")

(defvar web-mode-comments-invisible nil
  "Comments visbility.")

(defvar web-mode-is-narrowed nil
  "Buffer has been narrowed.")

(defvar web-mode-block-beg nil
  "Beg of current block.")

(defvar web-mode-hook nil
  "List of functions to be executed with web-mode.")

(defvar web-mode-buffer-highlighted nil
  "Is buffer highlighted.")

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in web-mode buffers.")

(defvar web-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-;") 'web-mode-comment-uncomment)
    (define-key keymap (kbd "C-;")     'web-mode-comment-uncomment)
    (define-key keymap (kbd "C-c C-a") 'web-mode-indent-buffer)
    (define-key keymap (kbd "C-c C-b") 'web-mode-beginning-of-element)
    (define-key keymap (kbd "C-c C-b") 'web-mode-beginning-of-element)
    (define-key keymap (kbd "C-c C-c") 'web-mode-next-server-block)
    (define-key keymap (kbd "C-c C-e") 'web-mode-select-element-content)
    (define-key keymap (kbd "C-c C-f") 'web-mode-toggle-folding)
    (define-key keymap (kbd "C-c C-i") 'web-mode-insert)
    (define-key keymap (kbd "C-c C-j") 'web-mode-duplicate-element)
    (define-key keymap (kbd "C-c C-l") 'web-mode-previous-element)
    (define-key keymap (kbd "C-c C-m") 'web-mode-mark-and-expand)
    (define-key keymap (kbd "C-c C-n") 'web-mode-match-tag)
    (define-key keymap (kbd "C-c C-p") 'web-mode-parent-element)
    (define-key keymap (kbd "C-c C-r") 'web-mode-rename-element)
    (define-key keymap (kbd "C-c C-s") 'web-mode-select-element)
    keymap)
  "Keymap for `web-mode'.")

(eval-and-compile

  (defalias 'web-mode-prog-mode (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

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

  )

;;;###autoload
(define-derived-mode web-mode web-mode-prog-mode "Web"
  "Major mode for editing mixed HTML Templates."

  (let (bfn elt l i)

    ;;    (make-local-variable 'font-lock-extend-region-functions)
    ;;    (make-local-variable 'font-lock-keywords-case-fold-search)
    ;;    (make-local-variable 'font-lock-keywords-only)
    ;;    (make-local-variable 'font-lock-lock-defaults)

    (make-local-variable 'after-change-functions)
    (make-local-variable 'font-lock-fontify-buffer-function)
    (make-local-variable 'font-lock-keywords)
    (make-local-variable 'font-lock-multiline)
    (make-local-variable 'font-lock-unfontify-buffer-function)
    (make-local-variable 'indent-line-function)
    (make-local-variable 'indent-tabs-mode)
    (make-local-variable 'require-final-newline)

    (make-local-variable 'web-mode-block-beg)
    (make-local-variable 'web-mode-buffer-highlighted)
    (make-local-variable 'web-mode-disable-autocompletion)
    (make-local-variable 'web-mode-disable-css-colorization)
    (make-local-variable 'web-mode-engine)
    ;;    (make-local-variable 'web-mode-engine-families)
    (make-local-variable 'web-mode-expand-first-pos)
    (make-local-variable 'web-mode-expand-last-type)
    (make-local-variable 'web-mode-file-type)
    (make-local-variable 'web-mode-indent-context)
    (make-local-variable 'web-mode-indent-style)
    (make-local-variable 'web-mode-is-narrowed)
    (make-local-variable 'web-mode-server-blocks-regexp)

    (setq bfn (file-name-nondirectory (buffer-file-name)))

    ;;  (make-local-variable 'font-lock-extend-after-change-region-function)
    ;;  (setq font-lock-extend-after-change-region-function 'web-mode-extend-after-change-region)

    (cond
     ((string-match-p "\\.xml\\'" bfn)
      (setq web-mode-file-type "xml"))
     ((string-match-p "\\.css\\'" bfn)
      (setq web-mode-file-type "css"))
     (t
      (setq web-mode-file-type "html"))
     )

    (when (boundp 'web-mode-engines-alist)
      (dolist (elt web-mode-engines-alist)
        (when (string-match-p (car elt) bfn)
          (setq web-mode-engine (cdr elt)))
        );dolist
      );when

    (unless web-mode-engine
      (cond
       ((string-match-p "\\.\\(erb\\|rhtml\\)\\'" bfn)
        (setq web-mode-engine "erb"))
       ((string-match-p "\\.tpl\\'" bfn)
        (setq web-mode-engine "smarty"))
       ((string-match-p "\\.jsp\\'" bfn)
        (setq web-mode-engine "jsp"))
       ((string-match-p "\\.\\(php\\|ctp\\|psp\\)\\'" bfn)
        (setq web-mode-engine "php"))
       ((string-match-p "\\.as[cp]x?\\'" bfn)
        (setq web-mode-engine "asp"))
       ((string-match-p "\\.\\(djhtml\\|tmpl\\|twig\\)\\'" bfn)
        (setq web-mode-engine "django"))
       ((string-match-p "\\.ftl\\'" bfn)
        (setq web-mode-engine "freemarker"))
       ((string-match-p "\\.mustache\\'" bfn)
        (setq web-mode-engine "mustache"))
       ((string-match-p "\\.\\(handlebars\\|hbs\\)\\'" bfn)
        (setq web-mode-engine "handlebars"))
       ((string-match-p "\\.\\(vsl\\|vm\\)\\'" bfn)
        (setq web-mode-engine "velocity"))
       );cond
      );when

    (when web-mode-engine
      (dolist (elt web-mode-engine-families)
        (when (member web-mode-engine (cdr elt))
          (setq web-mode-engine (car elt)))
        );dolist
      );when

    (cond
     ((string= web-mode-engine "php")
      (setq web-mode-server-blocks-regexp "<\\?"))
     ((string= web-mode-engine "velocity")
      (setq web-mode-server-blocks-regexp "^[ \t]*#[[:alpha:]#*]\\|$[[:alpha:]!{]"))
     ((string= web-mode-engine "django")
      (setq web-mode-server-blocks-regexp "{[#{%]"))
     ((string= web-mode-engine "ctemplate")
      (setq web-mode-server-blocks-regexp "{{."))
     ((string= web-mode-engine "freemarker")
      (setq web-mode-server-blocks-regexp "[<[]/?[#@][-]?\\|${"))
     ((string= web-mode-engine "smarty")
      ;;      (setq web-mode-server-blocks-regexp "{[^ ]"))
      (setq web-mode-server-blocks-regexp "{[[:alpha:]#$/*\"]"))
     ((string= web-mode-engine "asp")
      (setq web-mode-server-blocks-regexp "<%."))
     ;;      (setq web-mode-server-blocks-regexp "{[[:alpha:]*$#]"))
     (t
      (setq web-mode-server-blocks-regexp "<\\?\\|<%[#-!@]?\\|[<[]/?[#@][-]?\\|[$#]{\\|{[#{%]\\|^%."))
     )

    ;;    (message "engine=%S regexp=%S" web-mode-engine web-mode-server-blocks-regexp)

    (setq font-lock-fontify-buffer-function 'web-mode-scan-buffer
          ;;          font-lock-keywords-only t
          font-lock-unfontify-buffer-function 'web-mode-scan-buffer
          indent-line-function 'web-mode-indent-line
          indent-tabs-mode nil
          require-final-newline nil)

    (remove-hook 'after-change-functions 'font-lock-after-change-function t)

    (add-hook 'after-change-functions 'web-mode-on-after-change t t)

    (web-mode-scan-buffer)

    ))

(defun web-mode-scan-buffer ()
  "Scan entine buffer."
  (interactive)
  (web-mode-scan-region (point-min) (point-max)))

(defun web-mode-scan-region (beg end &optional verbose)
  "Identify code blocks (client/server) and syntactic symbols (strings/comments)."
  (interactive)
  ;;  (message "scanning buffer from %d to %d" beg end)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-match-data
       (let ((inhibit-modification-hooks t)
             (inhibit-point-motion-hooks t)
             (inhibit-quit t))
         (setq beg (if web-mode-is-narrowed 1 beg))
         (remove-text-properties beg end web-mode-text-properties)
         (cond
          ((string= web-mode-file-type "css")
           (web-mode-scan-client-block beg end "style"))
          (t
           (web-mode-mark-server-boundaries beg end)
           (web-mode-scan-client beg end)
           (web-mode-scan-server beg end))
          );cond
         )))))

(defun web-mode-mark-server-boundaries (beg end)
  "Identifies server blocks."
  (save-excursion

    (let (open close closing-string continue start sub2 sub3 pos tagopen l tmp)

      (goto-char beg)

      ;;      (message "%S: %Sx%S" (point) beg end)
      ;;      (message "regexp=%S" web-mode-server-blocks-regexp)
      (while (and (> end (point))
                  (re-search-forward web-mode-server-blocks-regexp end t))

        (setq close nil
              tagopen (match-string 0)
              open (match-beginning 0)
              pos nil)

        (when (or (char-equal ?\s (string-to-char tagopen))
                  (char-equal ?\t (string-to-char tagopen)))
          (setq l (length tagopen))
          (setq tagopen (replace-regexp-in-string "\\`[ \t]*" "" tagopen))
          (setq open (+ open (- l (length tagopen))))
          )

        (setq sub2 (substring tagopen 0 2))

        (cond

         ((string= web-mode-engine "smarty")

          (cond
           ((string= sub2 "{*")
            (setq closing-string "*}"))

           ((char-equal ?{ (string-to-char sub2))
            (setq closing-string "}"))
           )

          );smarty

         ((string= web-mode-engine "ctemplate")

          (cond
           ((string= sub3 "{{{")
            (setq closing-string "}}}"))

           (t
            (setq closing-string "}}"))
           )

          );smarty

         (t

          (cond

           ((string= "#*" sub2)
            (setq closing-string "*#"))

           ((char-equal ?\# (string-to-char sub2))
            (setq closing-string "EOL"))

           ((char-equal ?$ (string-to-char sub2))
            (setq closing-string "EOV"))

           ((char-equal ?% (string-to-char sub2))
            (setq closing-string "EOL"))

           ((string= "<?" sub2)
            (unless (looking-at-p "xml ")
              (setq closing-string "?>")
              ))

           ((string= "<%-" tagopen)
            (setq closing-string "--%>"))

           ((string= "<%#" tagopen)
            (setq closing-string "%>"))

           ((string= "[#-" tagopen)
            (setq closing-string "--]"))

           ((string= "<#-" tagopen)
            (setq closing-string "-->"))

           ((or (string= "[#" sub2) (string= "[@" sub2) (string= "[/" sub2))
            (setq closing-string "]"))

           ((or (string= "<#" sub2) (string= "<@" sub2) (string= "</" sub2))
            (setq closing-string ">"))

           ((string= "<%@" tagopen)
            (setq closing-string "%>"))

           ((string= "<%" sub2)
            (setq closing-string "%>"))

           ((member sub2 '("${" "#{"))
            (setq closing-string "}"))

           ((string= "{{" sub2)
            (setq closing-string "}}"))

           ((string= "{%" sub2)
            (setq closing-string "%}"))

           ((string= "{#" sub2)
            (setq closing-string "#}"))

           );cond

          );t

         );cond

        (when closing-string

          (cond

           ((and (string= web-mode-engine "smarty")
                 (string= closing-string "}"))
            (goto-char open)
            (setq tmp (web-mode-fetch-closing-paren-pos (point) (line-end-position)))
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

           ((string= closing-string "EOV")
            (goto-char open)
            ;;            (message "pt=%S %c" (point) (char-after))
            (when (char-equal ?$ (char-after))
              ;;              (message "pt=%S" (point))
              (forward-char))
            (when (char-equal ?! (char-after))
              ;;              (message "pt=%S" (point))
              (forward-char))
            (if (char-equal ?{ (char-after))
                (search-forward "}")
              (setq continue t)
              (while continue
                (skip-chars-forward "a-zA-Z0-9_-")
                (when (char-equal ?\( (char-after))
                  (search-forward ")")
                  )
                (if (char-equal ?\. (char-after))
                    (forward-char)
                  (setq continue nil))
                );while
              );if
            (setq close (point)
                  pos (point)))

           ((search-forward closing-string end t)
            ;;            (message "cs: %S" closing-string)
            (setq close (match-end 0)
                  pos (point)))

           ((string= "<?" sub2)
            (setq close (point-max)
                  pos (point-max)))

           )

          (when close
            ;;            (message "open(%S) close(%S)" open close)
            ;;            (add-text-properties open (+ open 1) '(server-pos beg))
            (add-text-properties open close '(server-side t))
            (add-text-properties (- close 1) close '(server-pos end))
            )

          (if pos (goto-char pos))

          );when

        );while

      )))

(defun web-mode-scan-server (beg end)
  "Identifies server blocks."
  (save-excursion

    (let ((block-beg beg)
          (block-end nil)
          (continue t)
          (pm (point-max)))

      (goto-char beg)

      (unless (get-text-property beg 'server-side)
        ;;        (setq block-beg (next-single-property-change beg 'server-side))
        (setq block-beg (web-mode-next-server-block-pos beg))
        (if (or (null block-beg) (>= block-beg end))
            (setq continue nil))
        )

      ;; (while continue
      ;;   (setq block-end (or (next-single-property-change block-beg 'server-pos) pm))
      ;;   (if (or (not block-end) (> block-end end) (> block-end (- pm 2)))
      ;;       (setq continue nil)
      ;;     (web-mode-scan-server-block block-beg (+ block-end 1))
      ;;     (if (get-text-property (+ block-end 1) 'server-side)
      ;;         (setq block-beg (+ block-end 1))
      ;;       (setq block-beg (next-single-property-change (+ block-end 1) 'server-side)))
      ;;     (if (or (not block-beg) (> block-beg end))
      ;;         (setq continue nil))
      ;;     );if
      ;;   );while

      (while continue
        (setq block-end (web-mode-server-block-end-pos block-beg))
        (if (or (null block-end) (> block-end end) (> block-end (- pm 2)))
            (setq continue nil)
          (setq block-end (1+ block-end))
          (web-mode-scan-server-block block-beg block-end)
          (if (get-text-property block-end 'server-side)
              (setq block-beg block-end)
            (setq block-beg (web-mode-next-server-block-pos block-end)))
          (if (or (null block-beg) (> block-beg end))
              (setq continue nil))
          );if
        );while

      )))

(defun web-mode-scan-server-block (beg end)
  "Scan server block."
  (let (sub1 sub2 sub3 regexp props start ms continue fc keywords tag)

    (goto-char beg)

    (setq sub1 (buffer-substring-no-properties beg (+ beg 1))
          sub2 (buffer-substring-no-properties beg (+ beg 2)))
    (setq sub3 sub2)
    (if (>= (point-max) (+ beg 3))
        (setq sub3 (buffer-substring-no-properties beg (+ beg 3))))

    ;;    (message "beg(%S) end(%S) sub3(%S)" beg end sub3)

    (cond

     ((string= web-mode-engine "ctemplate")
      (cond
       ((string= sub3 "{{!")
        (setq props '(server-type comment face web-mode-comment-face)))
       ((string= sub3 "{{%")
        (setq regexp "\"\\|'"
              props '(server-engine ctemplate face nil)
              keywords web-mode-ctemplate-font-lock-keywords))
       (t
        (setq props '(server-engine ctemplate face nil)
              keywords web-mode-ctemplate-font-lock-keywords))
       )
      );ctemplate

     ((string= sub2 "<?")
      (setq regexp "//\\|/\\*\\|\"\\|'\\|<<<['\"]?\\([[:alnum:]]+\\)['\"]?"
            props '(server-engine php face nil)
            keywords web-mode-php-font-lock-keywords))

     ((member sub3 '("<%-" "<#-" "[#-"))
      ;;     ((or (string= "<%-" sub3) (string= "<#-" sub3) (string= "[#-" sub3))
      (setq props '(server-type comment face web-mode-comment-face)))

     ((member sub2 '("<#" "<@" "</" "[#" "[@" "[/"))
      ;;     ((or (string= "<#" sub2) (string= "<@" sub2) (string= "</" sub2)
      ;;          (string= "[#" sub2) (string= "[@" sub2) (string= "[/" sub2))
      (setq regexp "\"\\|'"
            keywords web-mode-freemarker-font-lock-keywords
            props '(server-engine freemarker face nil))
      (looking-at "[<[]/?\\([#@][[:alnum:]._]+\\)")
      (setq tag (match-string-no-properties 1))
      (setq props (plist-put props 'server-tag-name tag))
      (cond
       ((char-equal (char-after (+ beg 1)) ?/)
        (setq props (plist-put props 'server-tag-type 'end))
        )
       ((char-equal (char-after (- end 2)) ?/)
        (setq props (plist-put props 'server-tag-type 'void))
        )
       (t
        (if (web-mode-is-void-element (match-string 1))
            (setq props (plist-put props 'server-tag-type 'void))
          (setq props (plist-put props 'server-tag-type 'start))
          )
        )
       );cond
      );or

     ((string= sub3 "<%@")
      (setq regexp "/\\*"
            props '(server-engine directive face nil)
            keywords web-mode-directive-font-lock-keywords))

     ((string= sub3 "<%$")
      (setq regexp "\"\\|'"
            props '(face nil)
            keywords web-mode-expression-font-lock-keywords))

     ((and (string= sub3 "<%#")
           (not (string= web-mode-engine "asp")))
      (setq props '(server-type comment face web-mode-comment-face))
      )

     ((or (string= sub2 "<%") (string= sub1 "%"))
      (setq regexp "//\\|/\\*\\|\"\\|'")
      (cond
       ((or (string= sub1 "%") (string= web-mode-engine "erb"))
        (setq props '(server-engine erb face nil)
              keywords web-mode-jsp-font-lock-keywords)
        )
       ((string= web-mode-engine "asp")
        (setq props '(server-engine asp face nil)
              keywords web-mode-asp-font-lock-keywords)
        )
       (t
        (setq props '(server-engine jsp face nil)
              keywords web-mode-jsp-font-lock-keywords)
        )
       )
      )

     ((string= sub2 "{*")
      (setq props '(server-type comment face web-mode-comment-face))
      )

     ((and (string= sub1 "{") (string= web-mode-engine "smarty"))
      (setq regexp "\"\\|'"
            props '(server-engine smarty face nil)
            keywords web-mode-smarty-font-lock-keywords)
      )

     ((and (string= sub1 "$") (string= web-mode-engine "velocity"))
      (setq regexp "\"\\|'"
            props '(server-engine velocity face nil)
            keywords web-mode-velocity-font-lock-keywords)
      )


     ((member sub2 '("${" "#{"))
      (setq regexp "\"\\|'"
            props '(server-engine jsp face nil)
            keywords web-mode-uel-font-lock-keywords)
      )

     ((string= sub2 "{{")
      (setq regexp "\"\\|'"
            props '(server-engine django face nil)
            keywords web-mode-uel-font-lock-keywords)
      )

     ((string= sub2 "{%")
      (setq regexp "//\\|/\\*\\|\"\\|'"
            props '(server-engine django face nil)
            keywords web-mode-django-font-lock-keywords)
      )

     ((string= sub2 "{#")
      (setq props '(server-type comment face web-mode-comment-face))
      )

     ((member sub2 '("##" "#*"))
      (setq props '(server-type comment face web-mode-comment-face))
      )


     ((string= sub1 "#")
      (setq regexp "\"\\|'"
            props '(server-engine velocity face nil)
            keywords web-mode-velocity-font-lock-keywords)
      )

     );cond

    (add-text-properties beg end props)

    (when keywords (web-mode-fontify-region beg end keywords))

    (when regexp
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (setq start (match-beginning 0)
              ms (match-string 0)
              continue t)
        (setq fc (substring ms 0 1))
        (cond

         ((string= fc "'")
          (setq props '(server-type string face web-mode-string-face))
          (while (and continue (search-forward "'" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= fc "\"")
          (setq props '(server-type string face web-mode-string-face))
          (while (and continue (search-forward "\"" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= ms "//")
          (setq props '(server-type comment face web-mode-comment-face))
          (goto-char (if (< end (line-end-position)) end (line-end-position)))
          )

         ((string= ms "/*")
          (setq props '(server-type comment face web-mode-comment-face))
          (search-forward "*/" end t)
          )

         ((string= fc "<")
          ;;                (message "tag=%S" (match-string 1))
          (setq props '(server-type string face web-mode-string-face))
          (re-search-forward (concat "^" (match-string 1)) end t)
          )

         );;cond

        ;;        (message "elt=%S" (buffer-substring start (point)))
        (add-text-properties start (point) props)

        );while

      );when

    ))

;; start-tag, end-tag, tag-name, element (<a>xsx</a>, an element is delimited by tags), void-element
;; http://www.w3.org/TR/html-markup/syntax.html#syntax-elements
;;<#include "toto">
(defun web-mode-scan-client (beg end)
  "Scan client side blocks (JS / CSS / HTML Comments) and identifies strings and comments."
  (save-excursion
    (let (open limit close ms regexp props closing-string start tag-name tag-beg tag-end tag-fc tag-stop attrs-end close-found prop-type prop-name)

      (goto-char beg)

      (while (web-mode-rsf-client "<\\(!--\\|!doctype\\|/?[[:alnum:]]+[:_]?[[:alnum:]]*\\|\?xml\\)" end t)
        (setq tag-name (downcase (match-string 1))
              tag-beg (match-beginning 0)
              tag-end nil
              tag-type nil
              tag-stop (point)
              tag-fc (substring (match-string 0) 0 1)
              prop-name 'client-tag-name
              prop-type 'client-tag-type
              open nil
              limit end
              close nil
              pos nil
              markup-face nil
              props nil
              regexp nil
              closing-string nil
              close-found nil)

        (cond
         ((string= tag-name "!--")
          (setq regexp "-->"))
         ((string= tag-name "!doctype")
          (setq regexp ">"))
         ((string= tag-name "?xml")
          ;;          (setq regexp "\?>"))
          (setq regexp "?>"))
         (t
          (cond
           ((string-match-p "[:_]" tag-name)
            (setq props '(face web-mode-preprocessor-face)
                  prop-name 'server-tag-name
                  prop-type 'server-tag-type))
           (t
            (setq props '(face web-mode-html-tag-face)))
           )
          (cond
           ((char-equal (string-to-char tag-name) ?/)
            (setq props (plist-put props prop-name (substring tag-name 1)))
            (setq props (plist-put props prop-type 'end))
            (setq regexp ">")
            (setq limit (if (> end (line-end-position)) (line-end-position) end))
            )
           ((web-mode-is-void-element tag-name)
            (setq props (plist-put props prop-name tag-name))
            (setq props (plist-put props prop-type 'void))
            (setq regexp "/?>")
            ;;            (setq regexp ">")
            )
           (t
            (setq props (plist-put props prop-name tag-name))
            (setq props (plist-put props prop-type 'start))
            (setq regexp "/?>")
            ;;            (setq regexp ">")
            )
           );cond

          ;;          (add-text-properties tag-beg tag-stop props)

          );t
         );cond

        (if (web-mode-rsf-client regexp limit t)
            (progn
              (setq attrs-end (match-beginning 0)
                    tag-end (point)
                    close-found t)
              ;;              (message "attrs-end=%S" attrs-end)
              ;;              (message "close found , tag=%S (%d > %d)" tag-name tag-beg tag-end)

              (when (char-equal (string-to-char (match-string 0)) ?/)
                ;;                (put-text-property tag-beg tag-stop 'markup-type 'void)
                (setq props (plist-put props prop-type 'void))
                )

              ;;              (setq props (plist-put props 'client-tag-name nil))
              ;;              (setq props (plist-put props 'markup-type 'close))
              ;;              (add-text-properties attrs-end tag-end props)

              );progn

          (setq attrs-end (line-end-position)
                tag-end (line-end-position))
          ;;          (message "close not found , tag=%S (%S > %S)" tag-name tag-beg tag-end)

          );if


        (add-text-properties tag-beg tag-end props)

        (cond

         ((or (string= tag-name "!doctype") (string= tag-name "?xml"))
          (add-text-properties tag-beg tag-end '(face web-mode-doctype-face)))

         ((string= tag-name "!--")
          (add-text-properties tag-beg tag-end '(client-side t client-type comment face web-mode-comment-face)))

         (close-found
          (when (and (not (string= tag-fc "/"))
                     (> (- attrs-end tag-stop) 3))
            (web-mode-scan-attrs tag-stop attrs-end)
            )
          (cond
           ((string= tag-name "script")
            (setq closing-string "</script>"))
           ((string= tag-name "style")
            (setq closing-string "</style>"))
           )
          (when (and closing-string (web-mode-sf-client closing-string end t))
            (setq open tag-end
                  close (match-beginning 0))
            ;;            (message "open(%S) close(%S) tag(%S)" open close tag)
            (web-mode-scan-client-block open close tag-name)
            ;;            (message "%S" (buffer-substring open close))
            (goto-char close)
            ); when
          ); close-found
         ); cond tag

        ); while

      )))

(defun web-mode-scan-client-block (beg end tag-name)
  "Scan client block."
  (save-excursion
    (let (regexp props fc start continue ms keywords rules-beg rules-end props-beg props-end)

      (cond
       ((string= tag-name "script")
        (setq regexp "//\\|/\\*\\|\"\\|'"
              keywords web-mode-script-font-lock-keywords
              props '(client-language js client-side t)))
       ((string= tag-name "style")
        (setq regexp "/\\*\\|\"\\|'"
              props '(client-language css client-side t)))
       )

      (add-text-properties beg end props)

      (when keywords
        (web-mode-fontify-region beg end keywords))

      (when (string= tag-name "style")
        (goto-char beg)
        (setq rules-beg (if (= beg 1) 1 (+ beg 1)))
        (while (and rules-beg
                    (search-forward "{" end t)
                    (< (point) end))
          (setq rules-end (- (point) 1))
          (setq props-beg (point))
          ;;          (message "rules-beg(%S) rules-end(%S)" rules-beg rules-end)
          ;;          (message "%S" font-lock-keywords)
          (web-mode-fontify-region rules-beg rules-end web-mode-css-rules-font-lock-keywords)
          (goto-char props-beg)
          (setq rules-beg nil)
          (when (and (search-forward "}" end t)
                     (< (point) end))
            (setq props-end (- (point) 1))
            (setq rules-beg (point))
            ;;            (setq font-lock-keywords web-mode-css-props-font-lock-keywords)
            ;;            (message "props-beg(%S) props-end(%S)" props-beg props-end)
            (when (> (- props-end props-beg) 2)
              ;;              (font-lock-fontify-region props-beg props-end)
              (web-mode-fontify-region props-beg props-end web-mode-css-props-font-lock-keywords)
              )
            (goto-char rules-beg)
            )
          );while
        (goto-char beg)
        (while (and (not web-mode-disable-css-colorization)
                    (re-search-forward "#[0-9a-fA-F]\\{6\\}\\|#[0-9a-fA-F]\\{3\\}\\|rgb([ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)[ ]*,[ ]*\\([[:digit:]]\\{1,3\\}\\)\\(.*?\\))" end t)
                    (< (point) end))
          (web-mode-colorize (match-beginning 0) (match-end 0))
          );while
        );when

      (goto-char beg)

      (while (and regexp (web-mode-rsf-client regexp end t))
        (setq start (match-beginning 0)
              ms (match-string 0)
              continue t)
        ;;            (message "beg=%S match=%S" start ms)
        (setq fc (substring ms 0 1))
        (cond

         ((string= fc "'")
          (setq props '(client-type string face web-mode-string-face))
          (while (and continue (search-forward "'" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= fc "\"")
          (setq props '(client-type string face web-mode-string-face))
          (while (and continue (search-forward "\"" end t))
            (setq continue (char-equal ?\\ (char-before (- (point) 1))))
            )
          )

         ((string= ms "//")
          (setq props '(client-type comment face web-mode-comment-face))
          (goto-char (if (< end (line-end-position)) end (line-end-position)))
          )

         ((string= ms "/*")
          (setq props '(client-type comment face web-mode-comment-face))
          (search-forward "*/" end t)
          )

         );cond

        ;;        (message "elt=%s" (buffer-substring-no-properties start (point)))
        (add-text-properties start (point) props)

        );while

      )))

;; http://www.w3.org/TR/html-markup/syntax.html#syntax-attributes
;; states: "nil" "space" "name" "space-before" "equal" "space-after" "value-uq" "value-sq" "value-dq"
(defun web-mode-scan-attrs (beg end)
  "Scan and fontify html attributes."
  (save-excursion
    ;;    (message "beg(%S) end(%S)" beg end)
    (let (name-beg name-end val-beg val-end (state "nil") c pos prev)
      (goto-char (- beg 1))
      ;;      (setq end (1- end))
      (while (< (point) end)
        (forward-char)
        (setq pos (point))
        (setq c (buffer-substring-no-properties pos (+ pos 1)))

        (cond

         ((= (point) end)
          (web-mode-propertize-attr state c name-beg name-end val-beg)
          (setq state "nil"
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((get-text-property pos 'server-side)
          )

         ((and (string= c " ")
               (string= state "nil"))
          (setq state "space")
          )

         ((and (string= c " ")
               (member state '("space-before" "space-after" "space"))
               ;;               (or (string= state "space-before")
               ;;                   (string= state "space-after")
               ;;                   (string= state "space"))
               )
          )

         ((and (string= c " ")
               (string= state "name"))
          (setq state "space-before")
          )

         ((and (string= c " ")
               (string= state "equal"))
          (setq state "space-after")
          )

         ((and (string= c "\n")
               (not (member state '("value-sq" "value-dq"))))
          (web-mode-propertize-attr state c name-beg name-end val-beg)
          (setq state "space"
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((or (and (string= c "\"") (string= state "value-dq") (not (string= prev "\\")))
              (and (string= c "'") (string= state "value-sq") (not (string= prev "\\")))
              (and (or (string= c " ") (string= c "\n") (string= c ">")) (string= state "value-uq")))
          (web-mode-propertize-attr state c name-beg name-end val-beg)
          (setq state (if (string= state "value-uq") "space" "nil")
                name-beg nil
                name-end nil
                val-beg nil
                val-end nil)
          )

         ((and (not (string= c " "))
               (string= state "space"))
          ;;          (message "pos(%S)" (point))
          (setq state "name")
          (setq name-beg (point))
          )

         ((and (string= c "=")
               (member state '("space-before" "name")))
          (setq name-end (point))
          (setq state "equal")
          )

         ((and (string= c "\"")
               (member state '("space-after" "equal")))
          (setq val-beg (point))
          (setq state "value-dq")
          )

         ((and (string= c "'")
               (member state '("space-after" "equal")))
          (setq val-beg (point))
          (setq state "value-sq")
          )

         ((member state '("space-after" "equal"))
          (setq val-beg (point))
          (setq state "value-uq")
          )

         ((string= state "space")
          (setq state "name")
          )

         );;cond

        ;;        (message "point(%S) end(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) val-end(%S)" pos end state c name-beg name-end val-beg val-end)

        (setq prev c)

        );;while

      )))

(defun web-mode-propertize-attr (state c name-beg name-end val-beg &optional val-end)
  "propertize attr."
  (unless val-end (setq val-end (point)))
  ;;  (message "point(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) val-end(%S)" (point) state c name-beg name-end val-beg val-end)
  (cond

   ((and (string= state "value-dq")
         (not (string= c "\"")))
    )

   ((and (string= state "value-sq")
         (not (string= c "'")))
    )

   ((string= state "equal")
    )

   ((null name-beg)
    )

   (t

    (if (or (and (string= state "value-dq") (string= c "\""))
            (and (string= state "value-sq") (string= c "'")))
        (add-text-properties name-beg (+ (point) 1) '(client-type attr face web-mode-html-attr-name-face))
      (add-text-properties name-beg (point) '(client-type attr face web-mode-html-attr-name-face))
      )


    ;;    (add-text-properties name-beg (point) '(client-type attr face web-mode-html-attr-name-face))
    (when (and val-beg val-end)
      (setq val-end (if (string= c ">") val-end (+ val-end 1)))
      (add-text-properties val-beg val-end '(face web-mode-html-attr-value-face)))
    );t

   );cond

  )

(defun web-mode-colorize (beg end)
  "Colorize CSS colors."
  (let (str plist len)
    (setq str (buffer-substring-no-properties beg end))
    (setq len (length str))
    (cond
     ((string= (substring str 0 1) "#")
      (setq plist (list :background str))
      (put-text-property beg end 'face plist)
      )
     ((string= (substring str 0 4) "rgb(")
      (setq str (format "#%02X%02X%02X"
                        (string-to-number (match-string-no-properties 1))
                        (string-to-number (match-string-no-properties 2))
                        (string-to-number (match-string-no-properties 3))))
      (setq plist (list :background str))
      (put-text-property beg end 'face plist)
      )
     )
    ))

(defun web-mode-fontify-region (beg end keywords)
  "Highlight block."
  (save-excursion
    (let ((font-lock-keywords keywords)
          (font-lock-multiline nil)
          (font-lock-keywords-case-fold-search nil)
          (font-lock-keywords-only t)
          (font-lock-extend-region-functions nil)
          )
      (font-lock-fontify-region beg end)
      ))
  ;; workaround
  (unless web-mode-buffer-highlighted
    (setq web-mode-buffer-highlighted t)
    (web-mode-fontify-region beg end keywords))
  )

(defun web-mode-replace-apos ()
  "Replace ' with ’."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" nil t)
      (when (and (null (get-text-property (point) 'face))
                 (not (get-text-property (point) 'server-side))
                 (not (get-text-property (point) 'client-side)))
        (replace-match "\\1’\\2")))))

(defun web-mode-indent-buffer ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


(defun web-mode-previous-usable-server-line ()
  "Move cursor to previous non blank/comment/string line and return this line (trimmed).
point is at the beginning of the line."
  (interactive)
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
      )
    (if (string= line "")
        (progn
          (goto-char pos)
          nil)
      line)
    ))

(defun web-mode-previous-usable-client-line ()
  "Move cursor to previous non blank/comment/string line and return this line (trimmed).
point is at the beginning of the line."
  (interactive)
  (let ((continue t)
        (line "")
        (pos (point)))
    (beginning-of-line)
    (while (and continue
                (not (bobp))
                (forward-line -1))
      (if (not (web-mode-is-csss-line))
          (setq line (web-mode-trim (buffer-substring (point) (line-end-position)))))
      (when (not (string= line "")) (setq continue nil))
      )
    (if (string= line "")
        (progn
          (goto-char pos)
          nil)
      line)
    ))

(defun web-mode-is-comment-or-string-line ()
  "Detect if current line is in a comment or in a string."
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-comment-or-string)
            (setq counter (+ counter 1))
          (when (not (char-equal (following-char) ?\s))
            (setq continue nil
                  counter 0))
          );if
        (forward-char)
        );while
      (> counter 0)
      )))

(defun web-mode-is-csss-line ()
  "Detect if current line is in a comment or in a string."
  (save-excursion
    (let ((continue t) (counter 0))
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-csss)
            (setq counter (+ counter 1))
          (when (not (char-equal (following-char) ?\s))
            (setq continue nil
                  counter 0))
          );if
        (forward-char)
        );while
      (> counter 0)
      )))

(defun web-mode-is-csss (&optional pos)
  "Detect if point is in a comment, a string or in server script."
  (unless pos (setq pos (point)))
  (or (get-text-property pos 'server-side)
      (get-text-property pos 'server-tag-name)
      (not (null (member (get-text-property pos 'client-type) '(string comment))))))

(defun web-mode-is-comment-or-string (&optional pos)
  "Detect if point is in a comment or in a string."
  (interactive)
  (unless pos (setq pos (point)))
  (or (memq (get-text-property pos 'server-type) '(string comment))
      (memq (get-text-property pos 'client-type) '(string comment))))

(defun web-mode-is-comment (&optional pos)
  "Detect if point is in a comment."
  (interactive)
  (unless pos (setq pos (point)))
  (or (eq (get-text-property pos 'server-type) 'comment)
      (eq (get-text-property pos 'client-type) 'comment)))

(defun web-mode-is-line-in-block (open close)
  "Detect if point is in a block delimited by open and close."
  (save-excursion
    (let (line-current line-open line-close)
      (setq line-current (web-mode-current-line-number))
      (and (web-mode-sb open nil t)
           (setq web-mode-block-beg (+ (point) (length open)))
           (setq line-open (web-mode-current-line-number))
           (web-mode-sf close nil t)
           (setq line-close (web-mode-current-line-number))
           (not (eq line-open line-close))
           (>= line-close line-current)
           ))))

(defun web-mode-in-code-block (open close &optional prop)
  "Detect if point is in a block delimited by open and close."
  (save-excursion
    (let ((pos (point)) pos-open pos-close start end ret)
      (when prop
        (setq start pos
              end pos)
        (when (eq (get-text-property pos prop) (get-text-property (- pos 1) prop))
          (setq start (or (previous-single-property-change pos prop) (point-min))))
        (when (eq (get-text-property pos prop) (get-text-property (+ pos 1) prop))
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

(defun web-mode-in-server-block (language)
  "Detect if point is in a server block."
  ;;(web-mode-scan-init)
  (save-excursion
    ;;    (progn (message "language=%S" language) t)
    (let ((pos (point)))
      (and (not (bobp))
           (eq (get-text-property pos 'server-engine) language)
           (eq (get-text-property (- pos 1) 'server-engine) language)
           (not (looking-at-p "\\?>\\|%>"))
           (progn
             (setq web-mode-block-beg (or (previous-single-property-change pos 'server-engine)
                                          (point-min)))
             t)
           )
      )))

(defun web-mode-in-client-block (language)
  "Detect if point is in a client (CSS/JS) block."
  (save-excursion
    (let ((pos (point)))
      (and (not (bobp))
           (eq (get-text-property pos 'client-language) language)
           (eq (get-text-property (- pos 1) 'client-language) language)
           (progn
             (setq web-mode-block-beg (or (previous-single-property-change pos 'client-language)
                                          (point-min)))
             t)
           )
      )))

(defun web-mode-current-line-number (&optional pos)
  "Return line number at point."
  (unless pos (setq pos (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pos)
                 (if (= (current-column) 0) 1 0)))
    ;;    (message "%d [%d] %s" pos out (web-mode-text-at-point))
    ret))

(defun web-mode-clean-client-line (input)
  "Remove comments and server scripts."
  (let ((out "")
        (beg 0)
        (keep t)
        (n (length input)))

    (dotimes (i n)
      (if (or (get-text-property i 'server-side input)
              (get-text-property i 'server-tag-name input)
              (eq (get-text-property i 'client-type input) 'comment))
          (if keep
              (setq out (concat out (substring input beg i))
                    beg 0
                    keep nil)
              (setq beg i
                    keep t))
        );if
      );dotimes

    (if (> beg 0) (setq out (concat out (substring input beg n))))
    (setq out (if (= (length out) 0) input out))
    (web-mode-trim out)
    ;;    (message "%S [%s] > [%s]" beg input out)
    ))

;; todo : see web-mode-clean-client-line
(defun web-mode-clean-server-line (input)
  "Remove comments."
  (let ((out ""))
    (dotimes (i (length input))
      (unless (eq (get-text-property i 'server-type input) 'comment)
        (setq out (concat out (substring-no-properties input i (1+ i))))))

    (web-mode-trim out)
    ))

(defvar web-mode-indent-context
  '(prev-line nil prev-last-char nil cur-point nil cur-line nil cur-first-char language nil)
  "Intent context")

;; todo: use indent-context
;; todo: less vars
(defun web-mode-indent-line ()
  "Indent current line according to language."
  (interactive)
  (let ((inhibit-modification-hooks t)
        continue
        counter
        cur-line-beg-pos
        cur-column
        cur-first-char
        cur-indentation
        cur-line-number
        cur-line
        in-comment-block
        in-directive-block
        ;;        in-django-block
        ;;        in-ctemplate-block
        in-php-block
        in-asp-block
        in-jsp-block
        in-js-block
        in-style-block
        in-html-block
        line-number
        (local-indent-offset web-mode-code-indent-offset)
        n
        offset
        pos
        prev-indentation
        prev-last-char
        prev-line
        props
        tmp)

    (save-excursion
      (setq cur-line-beg-pos (line-beginning-position)
            cur-column (current-column)
            cur-line-number (web-mode-current-line-number)
            cur-indentation (current-indentation)
            pos (point)
            web-mode-block-beg nil)

      (back-to-indentation)

      (cond

       ((string= web-mode-file-type "css")
        (setq in-style-block t
              web-mode-block-beg (point-min)
              local-indent-offset web-mode-css-indent-offset))

       ((or (string= web-mode-file-type "xml"))
        (setq in-html-block t
              local-indent-offset web-mode-markup-indent-offset))

       ((web-mode-in-server-block 'php)
        (setq in-php-block t))

       ((web-mode-in-server-block 'jsp)
        (setq in-jsp-block t))

       ((web-mode-in-server-block 'directive)
        (setq in-directive-block t))

       ((web-mode-in-server-block 'asp)
        (setq in-asp-block t))

       ((web-mode-in-client-block 'js)
        (setq in-js-block t))

       ((web-mode-in-client-block 'css)
        (setq in-style-block t
              local-indent-offset web-mode-css-indent-offset))

       ;;       ((eq (get-text-property (point) 'server-engine) 'django)
       ;;        (setq in-django-block t)
       ;;        )

       ;;       ((eq (get-text-property (point) 'server-engine) 'ctemplate)
       ;;        (setq in-ctemplate-block t)
       ;;        )

       ((eq (get-text-property (point) 'server-type) 'comment)
        (setq in-comment-block t)
        )

       (t
        (setq in-html-block t
              local-indent-offset web-mode-markup-indent-offset))

       ) ;;cond

      ;;      (message "php(%S) jsp(%S) js(%S) css(%S) directive(%S) asp(%S) html(%S) comment(%S)" in-php-block in-jsp-block in-js-block in-style-block in-directive-block in-asp-block in-html-block in-comment-block)

      ;;      (message "block limit = %S" web-mode-block-beg)

      (setq cur-line (web-mode-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
      (setq cur-first-char (if (string= cur-line "") cur-line (substring cur-line 0 1)))

      (if (or in-php-block in-asp-block in-jsp-block in-directive-block)
          (setq prev-line (web-mode-previous-usable-server-line))
        (setq prev-line (web-mode-previous-usable-client-line)))

      ;;      (message "prev-line=[%s]" prev-line)

      (when prev-line

        (setq prev-indentation (current-indentation))

        (cond

         ((or in-html-block in-js-block in-style-block)
          (setq prev-line (web-mode-clean-client-line prev-line))
          ;;          (message "prev=[%s] %S" prev-line (length prev-line))
          (setq props (text-properties-at (1- (length prev-line)) prev-line))
          ;;          (message "props=%S" props)
          )

         (t
          (setq prev-line (web-mode-clean-server-line prev-line))
          )

         )

        ;;        (message "prev=%s" prev-line)
        (when (>= (length prev-line) 1)
          (setq prev-last-char (substring prev-line -1)))

        );; unless

      (end-of-line)

      ;;      (message "block-limit:%d" web-mode-block-beg)

      (cond ;; switch language

       ((null prev-line)
        (setq offset 0)
        )

       ((or in-php-block in-jsp-block in-asp-block in-js-block)
        ;;        (message "prev=%S" prev-last-char)
        (cond

         ((and (or in-jsp-block in-asp-block)
               (string-match-p "<%" prev-line))
          ;;                   (string-match-p "<%$" prev-line)))
          (web-mode-sb "<%")
          (setq offset (current-column))
          )

         ((and in-js-block
               (string= cur-first-char "."))
          ;;          (web-mode-rsb "\\." web-mode-block-beg)
          ;;          (setq offset prev-indentation)
          )

         ((member cur-first-char '("}" ")"))
          (goto-char pos)
          (back-to-indentation)
          (setq tmp (web-mode-fetch-opening-paren-pos (point) web-mode-block-beg))
          (when tmp
            (goto-char tmp)
            (setq offset (current-indentation)))
          )

         ((member cur-first-char '("]"))
          (goto-char pos)
          (back-to-indentation)
          (setq tmp (web-mode-fetch-opening-paren-pos (point) web-mode-block-beg))
          (when tmp
            (goto-char tmp)
            (setq offset (current-column)))
          )

         ((string= cur-first-char "?")
          (web-mode-rsb "[=(]" web-mode-block-beg)
          (setq offset (current-column))
          )

         ((string= cur-first-char ":")
          (setq offset (current-indentation))
          )

         ((string= prev-last-char ",")
          (goto-char pos)
          (back-to-indentation)
          ;;          (message "prev-line=%s" prev-line)
          ;; todo : ne pas regarder dans des strings ou comment
          (cond
           ((and in-js-block (string-match-p "var " prev-line))
            (web-mode-sb "var " web-mode-block-beg)
            (setq offset (+ (current-column) 4))
            )
           ((string-match-p "[({\[]" prev-line)
            (setq tmp (web-mode-fetch-opening-paren-block-pos (point) web-mode-block-beg))
            ;;            (message "tmp=%S" tmp)
            ;;            (web-mode-rsb "[({\[]" web-mode-block-beg)
            (when tmp
              (goto-char tmp)
              (setq offset (+ (current-column) 1)))
            )
           ((not (string-match-p "[({\[]" prev-line))
            (setq offset prev-indentation)
            )
           ;;           (t
           ;;            (when (web-mode-rsb "[({\[]" web-mode-block-beg)
           ;;              ;;              (message "%S" (point))
           ;;              ;;              (web-mode-fetch-opening-paren (string (char-after)) pos web-mode-block-beg)
           ;;              (setq offset (+ (current-column) 1))))
           );cond
          )

         ((member prev-last-char '("." "+" "?" ":"))
          (web-mode-rsb "[=(]" web-mode-block-beg)
          (skip-chars-forward "= (")
          (setq offset (current-column))
          )

         ((string= prev-last-char "}")
          (setq offset (current-indentation))
          )

         ((string= prev-last-char ")")
          (setq offset (current-indentation))
          )


         ((string= prev-last-char ";")
          (setq n (web-mode-count-opened-blocks-at-point web-mode-block-beg))
          ;;          (message "n=%S block-beg=%S" n web-mode-block-beg)
          (goto-char web-mode-block-beg)
          (if in-js-block (search-backward "<"))
          (setq offset (current-indentation))
          (if (> n 0) (setq offset (+ offset (* n local-indent-offset))))

          ;; (if (string-match-p ")[ ]*;$" prev-line)
          ;;     (progn
          ;;       (re-search-backward ")[ ]*;" web-mode-block-beg)
          ;;       (web-mode-fetch-opening-paren "(" (point) web-mode-block-beg))
          ;;   (re-search-backward "\\([=(]\\|^[[:blank:]]*var[ ]*\\)" web-mode-block-beg t))
          ;; (setq offset (current-indentation))
          )

         ((member prev-last-char '("{" "[" "("))
          (setq offset (+ (current-indentation) local-indent-offset))
          )

         ((and in-php-block (string-match-p "\\(->[ ]?[[:alnum:]_]+\\|)\\)$" prev-line))
          (web-mode-sb ">" web-mode-block-beg)
          (setq offset (- (current-column) 1))
          )

         ((and in-js-block (string-match-p "<script" prev-line))
          (web-mode-sb "<script")
          (setq offset (current-column))
          )

         ((and in-php-block (string-match-p "<\\?" prev-line))
          (web-mode-sb "<?")
          (setq offset (current-column))
          )

         (t
          ()
          )

         )) ;; end case script block

       (in-directive-block

        (cond

         (t
          (goto-char pos)
          (re-search-backward "@ " nil t)
          (re-search-forward "@ [[:alpha:]]+ " nil t)
          (setq offset (current-column)))
         )

        ) ;; directive

       (in-style-block

        (goto-char pos)

        (cond

         ((or (string-match-p "\\(^}\\|{\\)" cur-line)
              (not (web-mode-is-line-in-block "{" "}")))
          (if (or (= web-mode-indent-style 2)
                  (string= web-mode-file-type "css"))
              (setq offset 0)
            (web-mode-sb "<style")
            (setq offset (current-column)))
          )

         ((and (not (string= "" cur-line))
               (not (string-match-p "^[[:alpha:]-]+[ ]?:" cur-line)))

          (re-search-backward ":"  web-mode-block-beg)
          (skip-chars-forward ":  ")
          (setq offset (current-column))
          )

         (t
          (if (or (= web-mode-indent-style 2)
                  (string= web-mode-file-type "css"))
              (setq offset local-indent-offset)
            (web-mode-sb "<style" nil t)
            (setq offset (+ (current-column) local-indent-offset)))
          )

         )

        ); end case style block

       (in-comment-block

        (setq offset prev-indentation)

        ); end comment block

       (t ; case html block

        (cond

         ((and props (eq (plist-get props 'client-type) 'attr))
          (web-mode-tag-beg)
          (re-search-forward "<[[:alpha:]_:]+")
          (skip-chars-forward " ")
          (setq offset (current-column)))

         ((and (= web-mode-indent-style 2)
               (string-match-p "^\\(<\\?php\\|</?[@#]\\|<%\\|[?%]>\\)" cur-line))
          (setq offset 0)
          )

         ;; toto : utiliser block-beg
         ((and (> web-mode-indent-style 2)
               (string-match-p "^\\(\\?>\\|%>\\)" cur-line))
          (goto-char pos)
          (web-mode-rsb "<[%?]")
          (setq offset (current-column)) ;; prev-indent ?
          )

         ;;          ((and prev-indentation
         ;;                (string-match-p "^<\\?php[ ]+\\(end\\|else\\)" cur-line))
         ;;           (goto-char pos)
         ;;           (back-to-indentation)
         ;;           (web-mode-match-tag)
         ;; ;;          (message "pos(%S)" (point))
         ;;           (setq offset (current-indentation))
         ;;           )

         ;; ((and (string= web-mode-engine "smarty")
         ;;       (char-equal (string-to-char cur-line) ?{))
         ;;  (if (string-match-p "^{/" cur-line)
         ;;      (progn
         ;;        (goto-char pos)
         ;;        (back-to-indentation)
         ;;        (web-mode-match-tag)
         ;;        (setq offset (current-indentation)))
         ;;    (setq offset prev-indentation))
         ;;  )

         ((or (string-match-p "^</" cur-line)
              (string-match-p "^<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\|else\\)" cur-line)
              (and (string= web-mode-engine "django")
                   (string-match-p "^{%[-]?[ ]*end" cur-line))
              (and (string= web-mode-engine "smarty")
                   (string-match-p "^{/" cur-line))
              (and (string= web-mode-engine "ctemplate")
                   (string-match-p "^{{/" cur-line))
              (and (string= web-mode-engine "velocity")
                   (string-match-p "^#end" cur-line))
              )
          (goto-char pos)
          (back-to-indentation)
          (web-mode-match-tag)
          (setq offset (current-indentation))
          )

         ;; ((and (string= web-mode-engine "velocity")
         ;;       (char-equal (string-to-char cur-line) ?#))
         ;;  (if (string-match-p "^#end" cur-line)
         ;;      (progn
         ;;        (goto-char pos)
         ;;        (back-to-indentation)
         ;;        (web-mode-match-tag)
         ;;        (setq offset (current-indentation)))
         ;;    (setq offset prev-indentation))
         ;;  )

         ;; ((string-match-p "^</" cur-line)
         ;;  (goto-char pos)
         ;;  (back-to-indentation)
         ;;  (web-mode-match-tag)
         ;;  (setq offset (current-indentation))
         ;;  )

         ;;todo : il faut remonter les lignes une à une jusqu'a en trouver une qui commence par <alpha
         ;;       attention il peut y avoir du "server" ou commentaire au début
         ((or (eq (length cur-line) 0)
              (char-equal (string-to-char cur-line) ?<)
              (and (string= web-mode-engine "ctemplate")
                   (char-equal (string-to-char cur-line) ?{))
              (and (string= web-mode-engine "smarty")
                   (char-equal (string-to-char cur-line) ?{))
              (and (string= web-mode-engine "django")
                   (char-equal (string-to-char cur-line) ?{))
              (and (string= web-mode-engine "velocity")
                   (char-equal (string-to-char cur-line) ?#))
              )
          ;;          (message "ici")
          (setq continue t
                counter 0)
          (while (and continue (re-search-backward "^[[:blank:]]*</?[[:alpha:]]" nil t))
            (back-to-indentation)
            (when (and (web-mode-is-html-tag)
                       (not (member (get-text-property (point) 'client-tag-name) '("style" "script"))))
              (setq counter (1+ counter)
                    continue nil
                    offset (+ (current-indentation)
                              (if (web-mode-is-opened-element (buffer-substring (point) cur-line-beg-pos))
                                  local-indent-offset
                                0)
                              ))
              );when
            );while
          (if (eq counter 0) (setq offset 0))
          )

         (t
          ;;          (message "nothing")
          ()
          )

         )) ;; end case html block

       ) ;; end switch language block

      ) ;; save-excursion

    (when (and offset (not (eq cur-indentation offset)))
      (setq offset (max 0 offset))
      (indent-line-to offset))

    (if (< (current-column) (current-indentation)) (back-to-indentation))

    ) ;; let
  )

(defun web-mode-is-html-tag ()
  "Is point a the beginning of an html tag."
  (member (get-text-property (point) 'client-tag-type) '(start end void))
  )

(defun web-mode-count-opened-blocks-at-point (&optional limit)
  "Is it an open block."
  (interactive)
  (unless limit (setq limit nil))
  (let ((continue t) (n 0) (regexp "[}{]"))
    (while (and continue (re-search-backward regexp limit t))
      (unless (web-mode-is-comment-or-string)
        (if (string= (string (char-after)) "{")
            (setq n (1+ n))
          (setq n (1- n))))
      );while
    ;;    (message "opened-blocks(%S)" n)
    n))


(defun web-mode-count-char-in-string (char &optional string)
  "Count char in string."
  (let ((n 0))
    (dotimes (i (length string))
      (if (char-equal (elt string i) char)
          (setq n (1+ n))))
    n))

(defun web-mode-element-at-point ()
  "Return element at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((continue t)
          cont
          line l1 l2
          (pos (point)))
      (while continue
        (setq l1 (web-mode-current-line-number))
        (end-of-line)
        (setq cont t)
        (while cont
          (re-search-backward "<[[:alpha:]/]" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        ;;          (setq cont (web-mode-is-csss)))
        (setq cont t)
        (while cont
          (re-search-forward "[[:alnum:] /\"']>" nil t)
          (setq cont (web-mode-is-comment-or-string)))
        ;;          (setq cont (web-mode-is-csss)))
        ;;        (message "point=%d" (point))
        (setq l2 (web-mode-current-line-number))
        (if (eq l1 l2) (setq continue nil))
        )
      (end-of-line)
      ;;      (setq line (buffer-substring-no-properties pos (point)))
      (setq line (buffer-substring pos (point)))
      (setq line (replace-regexp-in-string "[\r\n]" "" line))
      (setq line (replace-regexp-in-string "[ ]+" " " line))
      ;;      (message "elt at point: %s" line)
      line
      )))

(defun web-mode-rename-element ()
  "Rename the current HTML element."
  (interactive)
  (save-excursion
    (let (pos tag-name)
      (setq tag-name (read-from-minibuffer "Tag name? "))
      (when (and (> (length tag-name) 0)
                 (web-mode-beginning-of-element)
                 (looking-at "<\\([[:alpha:]]+\\)"))
        (setq pos (point))
        (unless (web-mode-is-void-element)
            (save-match-data
              (web-mode-match-tag)
              (if (looking-at "</[ ]*\\([[:alpha:]]+\\)")
                  (replace-match (concat "</" tag-name))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag-name))
        ))))

(defun web-mode-mark-and-expand ()
  "Mark and expand."
  (interactive)
  (web-mode-mark (point)))

(defun web-mode-mark (pos)
  "Mark at point."

  (let ((beg pos) (end pos) prop reg-beg boundaries)

    (if mark-active
        (setq reg-beg (region-beginning))
      (setq web-mode-expand-first-pos (point)))

    ;;    (message "regs=%S %S %S %S" (region-beginning) (region-end) (point-min) (point-max))

    ;;    (message "before=%S" web-mode-expand-last-type)

    (cond

     ((and mark-active
           (= (region-beginning) (point-min))
           (or (= (region-end) (point-max)) (= (+ (region-end) 1) (point-max))))
      (deactivate-mark)
      (goto-char (or web-mode-expand-first-pos (point-min)))
      (recenter)
      )

     ((and (eq (get-text-property pos 'server-type) 'comment)
           (not (string= web-mode-expand-last-type "server-comment")))

      (when (eq (get-text-property pos 'server-type) (get-text-property (- pos 1) 'server-type))
        (setq beg (or (previous-single-property-change pos 'server-type) (point-min))))
      (when (eq (get-text-property pos 'server-type) (get-text-property (+ pos 1) 'server-type))
        (setq end (next-single-property-change pos 'server-type)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "server-comment"))

     ((and (eq (get-text-property pos 'server-type) 'string)
           (not (string= web-mode-expand-last-type "server-string")))

      (when (eq (get-text-property pos 'server-type) (get-text-property (- pos 1) 'server-type))
        (setq beg (or (previous-single-property-change pos 'server-type) (point-min))))
      (when (eq (get-text-property pos 'server-type) (get-text-property (+ pos 1) 'server-type))
        (setq end (next-single-property-change pos 'server-type)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "server-string"))

     ((and (eq (get-text-property pos 'server-side) t)
           (not (eq (get-text-property pos 'server-engine) 'django))
           (setq boundaries (web-mode-in-code-block "{" "}" 'server-side))
           (not (string= web-mode-expand-last-type "server-block")))

      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      ;;      (message "char=[%c]" (char-before (- (point) 1)))
      (if (char-equal (char-before (- (point) 1)) ?%)
          (setq web-mode-expand-last-type "server-side")
        (setq web-mode-expand-last-type "server-block"))
      (exchange-point-and-mark)
      )

     ((and (eq (get-text-property pos 'server-side) t)
           (not (string= web-mode-expand-last-type "server-side")))

      (when (eq (get-text-property pos 'server-side) (get-text-property (- pos 1) 'server-side))
        (setq beg (or (previous-single-property-change pos 'server-side) (point-min))))
      (when (eq (get-text-property pos 'server-side) (get-text-property (+ pos 1) 'server-side))
        (setq end (next-single-property-change pos 'server-side)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "server-side"))

     ((and (eq (get-text-property pos 'client-type) 'comment)
           (not (string= web-mode-expand-last-type "client-comment")))

      (when (eq (get-text-property pos 'client-type) (get-text-property (- pos 1) 'client-type))
        (setq beg (previous-single-property-change pos 'client-type)))
      (when (eq (get-text-property pos 'client-type) (get-text-property (+ pos 1) 'client-type))
        (setq end (next-single-property-change pos 'client-type)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "client-comment"))

     ((and (eq (get-text-property pos 'client-type) 'string)
           (not (string= web-mode-expand-last-type "client-string")))

      (when (eq (get-text-property pos 'client-type) (get-text-property (- pos 1) 'client-type))
        (setq beg (previous-single-property-change pos 'client-type)))
      (when (eq (get-text-property pos 'client-type) (get-text-property (+ pos 1) 'client-type))
        (setq end (next-single-property-change pos 'client-type)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "client-string"))

     ((and (eq (get-text-property pos 'client-side) t)
           (setq boundaries (web-mode-in-code-block "{" "}" 'client-side))
           (not (string= web-mode-expand-last-type "client-block")))
      (set-mark (car boundaries))
      (goto-char (cdr boundaries))
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "client-block")
      )

     ((and (eq (get-text-property pos 'client-side) t)
           (not (string= web-mode-expand-last-type "client-side")))

      (when (eq (get-text-property pos 'client-side) (get-text-property (- pos 1) 'client-side))
        (setq beg (previous-single-property-change pos 'client-side)))
      (when (eq (get-text-property pos 'client-side) (get-text-property (+ pos 1) 'client-side))
        (setq end (next-single-property-change pos 'client-side)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "client-side"))

     ;;     ((and (eq (get-text-property pos 'markup-type) 'attr)
     ((and (eq (get-text-property pos 'client-type) 'attr)
           (not (string= web-mode-expand-last-type "html-attr")))

      ;; todo: tester que le car précédent n'est pas un
      (when (eq (get-text-property pos 'client-type) (get-text-property (- pos 1) 'client-type))
        (setq beg (previous-single-property-change pos 'client-type)))
      (when (eq (get-text-property pos 'client-type) (get-text-property (+ pos 1) 'client-type))
        (setq end (next-single-property-change pos 'client-type)))
      (set-mark beg)
      (goto-char end)
      (exchange-point-and-mark)
      (setq web-mode-expand-last-type "html-attr"))

     ((and mark-active
           (char-equal (char-after) ?<))

      (web-mode-parent-element)
      (if (= reg-beg (region-beginning))
          (mark-whole-buffer)
        (web-mode-select-element))
      )

     (t
      (web-mode-select-element)
      ;;(mark-whole-buffer)
      )

     ) ;;cond

    ;;    (message "after=%S" web-mode-expand-last-type)

    ))

(defun web-mode-select-element-content ()
  "Select the inner content HTML element."
  (interactive)
  (let (pos beg end)
    (web-mode-select-element)
    (when mark-active
      (setq pos (point))
      (deactivate-mark)
      (web-mode-match-tag)
      (setq end (point))
      (goto-char pos)
      (web-mode-tag-end)
      (set-mark (point))
      (goto-char end)
      (exchange-point-and-mark)
      )))

(defun web-mode-select-element ()
  "Select the current HTML element."
  (interactive)
  (let ((pos (point)))
    (if (web-mode-is-tag)
        (cond
         ((member (get-text-property (point) 'client-tag-type) '(start void))
          (web-mode-tag-beg)
          (set-mark (point))
          (web-mode-match-tag)
          (web-mode-sf-client ">")
          (exchange-point-and-mark))
         (t
          (web-mode-match-tag)
          (set-mark (point))
          (web-mode-match-tag)
          (web-mode-sf-client ">")
          (exchange-point-and-mark))
         );cond
      (web-mode-parent-element)
      (unless (= (point) pos) (web-mode-select-element))
      );if
    ))

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
        is-void-element
        tag
        n
        ret
        (h (make-hash-table :test 'equal)))
    (unless line (setq line (web-mode-element-at-point)))
    ;;    (message "line=%s" line)
    ;;    (message "-- web-mode-is-opened-element")
    (setq line (web-mode-clean-client-line line))
    (setq line (substring-no-properties line))
    ;;    (message "*** clean-line=%s" line)
    (while (string-match web-mode-tag-regexp line deb)
      (setq deb (match-end 0)
            tag (match-string 1 line)
            is-closing-tag (string= (substring tag 0 1) "/"))
      ;;      (message "tag=%s" tag)
      (if is-closing-tag (setq tag (substring tag 1)))
      (setq n (gethash tag h 0))
      (setq deb (string-match "/?>" line deb))
      ;;      (setq deb (string-match "[^%?]?>" line deb))
      (setq is-void-element (string= (substring (match-string 0 line) 0 1) "/"))
      ;;      (message "ms=%s" (match-string 0 line))
      (if (or is-void-element (web-mode-is-void-element tag))
          (progn
            ;;            (message "void tag: %s" tag)
            )
        (if is-closing-tag
            (if (> n 0) (puthash tag (1- n) h))
          (puthash tag (1+ n) h))
        )

      );; while

    ;;(message (number-to-string (hash-table-count h)))
    (maphash (lambda (k v) (if (> v 0) (setq ret 't))) h)
    ;;    (if ret (message "line=%s: opened" line) (message "line=%s: closed" line))
    ret
    )
  )

;;-- nav -----------------------------------------------------------------------

(defun web-mode-fetch-opening-paren-pos (&optional pos limit)
  "Fetch opening paren."
  (interactive)
  (save-restriction
    ;;    (unless paren (setq paren "("))
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let ((continue t)
          (n -1)
          paren
          (pairs '((")" . "[)(]")
                   ("]" . "[\]\[]")
                   ("}" . "[}{]")))
          pt
          regexp)

      (setq paren (string (char-after)))
      ;;      (message "parent=%S" paren)
      (setq regexp (cdr (assoc paren pairs)))
      (if (null regexp) (setq continue nil))

      ;; (cond

      ;;  ((string= paren "(")
      ;;   (setq regexp "[)(]"))

      ;;  ((string= paren "{")
      ;;   (setq regexp "[}{]"))

      ;;  ((string= paren "[")
      ;;   (setq regexp "[\]\[]"))

      ;;  );cond

      (while (and continue (re-search-backward regexp limit t))
        (unless (web-mode-is-comment-or-string)
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

(defun web-mode-fetch-closing-paren-pos (&optional pos limit)
  "Fetch opening paren."
  (interactive)
  ;;  (unless paren (setq paren ")"))
  ;;  (message (web-mode-text-at-point))
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
          regexp)

      (setq paren (string (char-after)))
      (setq regexp (cdr (assoc paren pairs)))
      (if (null regexp) (setq continue nil))
      ;;      (message "paren=%S regexp=%S" paren regexp)

      ;; (cond

      ;;  ((string= paren ")")
      ;;   (setq regexp "[)(]"))

      ;;  ((string= paren "}")
      ;;   (setq regexp "[}{]"))

      ;;  ((string= paren "]")
      ;;   (setq regexp "[\]\[]"))

      ;;  );;cond

      (while (and continue (re-search-forward regexp limit t))
        (unless (web-mode-is-comment-or-string)
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

(defun web-mode-fetch-opening-paren-block-pos (pos limit)
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

(defun web-mode-tag-beg ()
  "Fetch html tag beg."
  (interactive)
  (let ((continue t) ret)
    (while continue
      (setq ret t)
      (if (not (looking-at-p "</?[[:alpha:]]"))
          (setq ret (re-search-backward "</?[[:alpha:]]" nil t)))
      (if (or (null ret)
              (member (get-text-property (point) 'client-tag-type) '(start end void)))
          (setq continue nil)))
    ret))

(defun web-mode-tag-end ()
  "Fetch html tag end."
  (interactive)
  (let ((continue t) ret prop)
    (setq prop (if (get-text-property (point) 'server-tag-type)
                   'server-tag-type
                 'client-tag-type))
    (while continue
      (setq ret (web-mode-sf-client ">" nil t))
      (if (or (null ret) (get-text-property (- (point) 1) prop))
          (setq continue nil)))
    ret))

(defun web-mode-previous-element ()
  "Fetch previous element."
  (interactive)
  (web-mode-rsb-html "</?[[:alpha:]]"))

(defun web-mode-next-element ()
  "Fetch next element."
  (interactive)
  (if (char-equal (following-char) ?<) (forward-char))
  (if (web-mode-rsf-html "</?[[:alpha:]]")
      (search-backward "<")))

(defun web-mode-parent-element ()
  "Fetch parent element."
  (interactive)
  (let (pos tag-type tag-name n (continue t) (h (make-hash-table :test 'equal)))
    (save-excursion
      (while (and continue (web-mode-previous-element))
        (setq pos (point))
        (setq tag-type (get-text-property pos 'client-tag-type)
              tag-name (get-text-property pos 'client-tag-name))
        (setq n (gethash tag-name h 0))
        (when (member tag-type '(end start))
          (if (eq tag-type 'end)
              (puthash tag-name (1- n) h)
            (puthash tag-name (1+ n) h)
            (if (eq n 0) (setq continue nil))
            ) ; if
          ) ; when
        ) ; while
      ) ; save-excursion
    (if (null continue) (goto-char pos))
    ))

(defun web-mode-beginning-of-element ()
  "Fetch beginning of element."
  (interactive)
  (let ((continue t)
        (pos nil))
    (save-excursion
      (if (char-equal (char-before) ?<) (backward-char))
      (if (and (looking-at-p "<[[:alpha:]]")
               (not (web-mode-is-csss)))
          (setq pos (point))
        (while continue
          (when (re-search-backward "<[[:alpha:]]" nil t)
            (setq continue (web-mode-is-csss))
            (unless continue (setq pos (point))))
          )))
    (if pos (goto-char pos))
    ))

(defun web-mode-server-block-beg-pos (&optional pos)
  "web-mode-server-block-beg-pos"
  (unless pos (setq pos (point)))
  (let ((end pos)
        start)
    (cond
     ((get-text-property pos 'server-side)
      (unless (or (eq pos (point-min))
                  (not (get-text-property (1- pos) 'server-side)))
        (setq start (or (previous-single-property-change pos 'server-side)
                        (point-min)))
        (setq end (text-property-any start pos 'server-pos 'end))
        (setq pos (if end (1+ end) start))
        ))
     (t
      (setq pos nil))
     );cond
    ;;    (message "web-mode-server-block-beg-pos=%S" pos)
    pos
    ))

(defun web-mode-prev-server-block-pos (&optional pos)
  "web-mode-prev-server-block-pos"
  (interactive)
  (unless pos (setq pos (point)))
  (cond

   ((get-text-property pos 'server-side)
    (setq pos (web-mode-server-block-beg-pos pos))
    (when (and pos (> pos (point-min)))
      (setq pos (1- pos))
      (while (and (> pos (point-min))
                  (eq (char-after pos) ?\n))
        (setq pos (1- pos))
        )
      ;;            (message "pos=%S  <%c>" pos (char-after pos))
      (if (get-text-property pos 'server-side)
          (progn
            (setq pos (web-mode-server-block-beg-pos pos))
            )
        (setq pos (previous-single-property-change pos 'server-side))
        (when (and pos (> pos (point-min)))
          (setq pos (web-mode-server-block-beg-pos (1- pos))))
        );if
      );when
    )

   (t
    (setq pos (previous-single-property-change pos 'server-side))
    (when (and pos (> pos (point-min)))
      (setq pos (web-mode-server-block-beg-pos (1- pos))))
    )

   );conf
  pos
  )

(defun web-mode-prev-server-block (&optional pos)
  "web-mode-prev-server-block"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-prev-server-block-pos pos))
  (if pos (goto-char pos)))

(defun web-mode-server-block-end-pos (&optional pos)
  "web-mode-server-block-end-pos"
  (unless pos (setq pos (point)))
  (cond
   ((eq (get-text-property pos 'server-pos) 'end)
    )
   ((get-text-property pos 'server-side)
    (setq pos (or (next-single-property-change pos 'server-pos)
                  (point-max))))
   (t
    (setq pos nil))
   );cond
  pos)

(defun web-mode-next-server-block-pos (&optional pos)
  "web-mode-next-server-block-pos"
  (unless pos (setq pos (point)))
  (if (get-text-property pos 'server-side)
      (if (= pos (point-min))
          (set pos (point-min))
        (setq pos (web-mode-server-block-end-pos pos))
        (when (and pos (> (point-max) pos))
          (setq pos (1+ pos))
          (if (not (get-text-property pos 'server-side))
              (setq pos (next-single-property-change pos 'server-side)))
          );when
        )
    (setq pos (next-single-property-change pos 'server-side)))
  pos)

(defun web-mode-next-server-block (&optional pos)
  "web-mode-next-server-block"
  (interactive)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-next-server-block-pos pos))
  (if pos (goto-char pos)))

;; todo : faire attention à server-pos 'end
(defun web-mode-goto-block-beg (&optional pos)
  "Block type beg"
  (interactive)
  (unless pos (setq pos (point)))
  (unless (bobp)
    (when (string= (get-text-property pos 'server-engine)
                   (get-text-property (- pos 1) 'server-engine))
      (setq pos (or (previous-single-property-change pos 'server-engine) (point-min)))
      (goto-char pos))
    );unless
  t)


;;-- /nav ----------------------------------------------------------------------

(defun web-mode-text-at-point (&optional pos)
  "Text at point."
  (buffer-substring-no-properties (or pos (point)) (line-end-position)))

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
    (eq (get-text-property (point) 'client-tag-type) 'void)
    ;;    (or (looking-at-p (concat "<\\(" (regexp-opt web-mode-void-elements) "\\)"))
    ;;        (looking-at-p "<[^>]+/>"))
    ))

(defun web-mode-is-tag ()
  "Is point in a client tag."
  (member (get-text-property (point) 'client-tag-type) '(start end void))
  )

(defun web-mode-is-start-tag ()
  "Is point a the beggining of start element."
  (eq (get-text-property (point) 'client-tag-type) 'start)
  )

(defun web-mode-is-end-tag ()
  "Is point a the beggining of start element."
  (eq (get-text-property (point) 'client-tag-type) 'close)
  )

(defconst web-mode-php-constants
  (regexp-opt
   (append (if (boundp 'web-mode-extra-php-constants)
               web-mode-extra-php-constants '())
           '("TRUE" "FALSE" "NULL" "true" "false" "null"
             "STR_PAD_LEFT" "STR_PAD_RIGHT")))
  "PHP constants.")

(defconst web-mode-php-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-php-keywords)
               web-mode-extra-php-keywords '())
           '("array" "as" "break" "callable" "catch" "class" "const" "continue"
             "default" "die" "do"
             "echo" "else" "elseif" "empty"
             "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit" "extends"
             "for" "foreach" "function"
             "if" "include" "instanceof" "interface" "isset" "list"
             "next" "or" "require" "return" "switch" "try" "unset"
             "var" "when" "while")))
  "PHP keywords.")

(defconst web-mode-php-types
  (eval-when-compile
    (regexp-opt
     '("array" "bool" "boolean" "char" "const" "double" "float"
       "int" "integer" "long" "mixed" "object" "real" "string"
       "Exception")))
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
   (append (if (boundp 'web-mode-extra-jsp-keywords)
               web-mode-extra-jsp-keywords '())
           '("case" "catch" "do" "else" "end" "false" "for" "function"
             "if" "in" "include" "new"
             "package" "page" "private" "protected" "public"
             "return" "tag" "taglib" "throw" "throws" "true" "try"
             "void" "while")))
  "JSP keywords.")

(defconst web-mode-asp-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-asp-keywords)
               web-mode-extra-asp-keywords '())
           '("case" "catch" "do" "else" "end" "for" "function"
             "if" "in" "include"
             "new" "package" "page" "return"
             "tag" "throw" "throws" "try" "while")))
  "ASP keywords.")

(defconst web-mode-smarty-directives
  (eval-when-compile
    (regexp-opt
     '("if" "include" "html_options")))
  "Smarty directives.")

(defconst web-mode-velocity-directives
  (eval-when-compile
    (regexp-opt
     '("else" "elseif" "end" "foreach" "if" "in" "include" "macro" "parse"
       "set" "stop")))
  "Velocity directives.")

(defconst web-mode-freemarker-keywords
  (eval-when-compile
    (regexp-opt
     '("as"))))

(defconst web-mode-django-keywords
  (eval-when-compile
    (regexp-opt
     '("as" "autoescape" "block" "break" "cache" "call" "context" "continue"
       "do" "embed" "else" "elseif" "elif"
       "endautoescape" "endblock" "endcache" "endcall" "endembed" "endfilter"
       "endfor" "endif" "endmacro" "endrandom" "endraw"
       "endsandbox" "endspaceless" "endtrans" "endwith"
       "extends" "false" "filter" "flush" "for" "from"
       "if" "ignore" "import" "in" "include" "is"
       "macro" "missing" "none" "not" "pluralize" "random" "raw" "trans" "true"
       "sandbox" "set" "spaceless" "use" "var" "with")))
  "Django keywords.")

(defconst web-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib"
       "Assembly" "Control" "Implements" "Import"
       "Master" "OutputCache" "Page" "Reference" "Register")))
  "Directives.")

(defconst web-mode-js-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-js-keywords)
               web-mode-extra-js-keywords '())
           '("catch" "false" "for" "function" "if" "in" "instanceof"
             "new" "null" "return"
             "this" "typeof" "true" "try" "undefined" "var" "while")))
  "JavaScript keywords.")

(defconst web-mode-directive-font-lock-keywords
  (list
   '("<%@\\|%>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\(" web-mode-directives "\\)[ ]+") '(1 'web-mode-keyword-face t t))
   '("[[:space:]^]\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-html-attr-name-face t t)
     (2 'web-mode-html-attr-value-face t t))
   ))

(defconst web-mode-freemarker-font-lock-keywords
  (list
   '("[<[]/?[#@][[:alpha:]_.]*\\|/?>\\|/?]" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>") '(1 'web-mode-keyword-face t t))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   ))

(defconst web-mode-smarty-font-lock-keywords
  (list
   '("[}{]" 0 'web-mode-preprocessor-face)
   ;;   '("\\`{" 0 'web-mode-preprocessor-face)
   ;;   '("}\\'" 0 'web-mode-preprocessor-face)
   '("{\\(/?[[:alpha:]_]+\\)" (1 'web-mode-keyword-face))
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

(defconst web-mode-velocity-font-lock-keywords
  (list
   ;;   '("#" 0 'web-mode-preprocessor-face)
   ;;   (cons (concat "\\<\\(" web-mode-velocity-directives "\\)\\>") '(1 'web-mode-keyword-face))
   (cons (concat "\\([#]\\)\\(" web-mode-velocity-directives "\\)\\>")
         '((1 'web-mode-preprocessor-face)
           (2 'web-mode-keyword-face)))
   '("[.]\\([[:alnum:]_-]+\\)[ ]?("
     (1 'web-mode-function-name-face))
   '("[.]\\([[:alnum:]_-]+\\)"
     (1 'web-mode-variable-name-face))
   '("\\<\\($[!]?[{]?\\)\\([[:alnum:]_-]+\\)[}]?" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defconst web-mode-django-font-lock-keywords
  (list
   '("{%\\|%}" 0 'web-mode-preprocessor-face)
   (cons (concat "[% ]\\(" web-mode-django-keywords "\\)[ %]") '(1 'web-mode-keyword-face t t))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   ))

(defconst web-mode-ctemplate-font-lock-keywords
  (list
   '("{{[>#/{%^&]?\\|[}]?}}" 0 'web-mode-preprocessor-face)
   '("{{>[ ]*\\([[:alnum:]_]+\\)" 1 'web-mode-keyword-face)
   '("[[:alnum:]_]" 0 'web-mode-variable-name-face)
   '("[ ]+\\([[:alnum:]_]+=\\)" 1 'web-mode-param-name-face t t)
   '("[:=]\\([[:alpha:]_]+\\)" 1 'web-mode-function-name-face t t)
   ))

;;comment:Unified Expression Language
(defconst web-mode-uel-font-lock-keywords
  (list
   '("[$#{]{\\|}" 0 'web-mode-preprocessor-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defconst web-mode-expression-font-lock-keywords
  (list
   '("<%\\$\\|%>" 0 'web-mode-preprocessor-face)
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defconst web-mode-css-rules-font-lock-keywords
  (list
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>") '(1 'web-mode-css-pseudo-class-face))
   '("[[:alnum:]-]+" 0 'web-mode-css-rule-face)
   ))

(defconst web-mode-css-props-font-lock-keywords
  (list
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") '(1 'web-mode-css-at-rule-face))
   '("[[:alpha:]-]\\{3,\\}[ ]?:" 0 'web-mode-css-prop-face)
   '("#[[:alnum:]]\\{3,6\\}\\|![ ]?important" 0 font-lock-builtin-face t t)
   ))

(defconst web-mode-script-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-js-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("\\([[:alnum:]]+\\):" 1 'web-mode-variable-name-face)
   ))

(defconst web-mode-asp-font-lock-keywords
  (list
   '("<%[=#:]?\\|%>" 0 'web-mode-preprocessor-face)
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-asp-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   ))

;; todo : specific keywords for erb
(defconst web-mode-jsp-font-lock-keywords
  (list
   '("%>\\|^%\\|<%\\(!\\|=\\|#=\\)?" 0 'web-mode-preprocessor-face)
   '("\\(throws\\|new\\|extends\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
   ))

(defconst web-mode-php-font-lock-keywords
  (list
   '("<\\?\\(php\\|=\\)?\\|\\?>" 0 'web-mode-preprocessor-face)
   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") '(0 'web-mode-keyword-face))
   (cons (concat "(\\<\\(" web-mode-php-types "\\)\\>") '(1 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") '(0 'web-mode-constant-face))
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("[[:alnum:]_][ ]?::[ ]?\\(\\sw+\\)" 1 'web-mode-constant-face)
   '("->[ ]?\\(\\sw+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\(\\sw+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\<\\(instanceof\\|class\\|new\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-type-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
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

;; todo: <?php ?>
(defun web-mode-toggle-folding ()
  "Toggle folding on a block."
  (interactive)
  (web-mode-with-silent-modifications
   (save-excursion
     (let (beg-inside beg-outside end-inside end-outside overlay overlays regexp)
       (back-to-indentation)
       (setq overlays (overlays-at (point)))
       (if overlays
           (progn
             ;; *** unfolding
             (setq overlay (car overlays))
             (setq beg-inside (overlay-start overlay)
                   end-inside (overlay-end overlay))
             (remove-overlays beg-inside end-inside)
             (put-text-property beg-inside end-inside 'invisible nil))
         ;; *** folding
         (when (or (web-mode-is-start-tag)
                   (looking-at-p "<\\?php[ ]+\\(if\\|while\\|for\\)")
                   (looking-at-p "{%[-]?[ ]+\\(if\\|while\\|for\\)")
                   (looking-at-p "{{[#^]")
                   (looking-at-p "#\\(define\\|if\\|for\\|macro\\)"))
           (setq beg-outside (point))
           (cond
            ((looking-at-p "<\\?")
             (setq regexp "\\?>"))
            ((looking-at-p "{%")
             (setq regexp "%}"))
            ((looking-at-p "{{[#^]")
             (setq regexp "}}"))
            ((looking-at-p "#")
             (setq regexp "$"))
            (t
             (setq regexp ">"))
            )
           (web-mode-rsf regexp)
           (setq beg-inside (point))
           (goto-char beg-outside)
           (web-mode-match-tag)
           (setq end-inside (point))
           (web-mode-rsf regexp)
           (setq end-outside (point))
           ;;          (message "beg-out(%d) beg-in(%d) end-in(%d) end-out(%d)" beg-outside beg-inside end-inside end-outside)
           (setq overlay (make-overlay beg-outside end-outside))
           (overlay-put overlay 'face 'web-mode-folded-face)
           (put-text-property beg-inside end-inside 'invisible t)
           ); when
         ); if
       ); let
     )))


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
    )
  )

(defun web-mode-line-type (&optional pos)
  "Line type."
  (save-excursion
    (let (type)
      (if pos (goto-char pos))

      (back-to-indentation)
      (cond

       ((web-mode-in-server-block 'php)
        (setq type "php"))

       ((web-mode-in-server-block 'jsp)
        (setq type "java"))

       ((web-mode-in-server-block 'directive)
        (setq type "html"))

       ((web-mode-in-server-block 'asp)
        (setq type "asp"))

       ((web-mode-in-client-block 'js)
        (setq type "script"))

       ((web-mode-in-client-block 'css)
        (setq type "style"))

       (t
        (setq type "html"))

       );; cond

      type

      )))

(defun web-mode-comment-uncomment (&optional pos)
  "Comment or uncomment line(s) at point."
  (interactive)
  (unless pos (setq pos (point)))
  (if (web-mode-is-comment)
      (web-mode-uncomment pos)
    (web-mode-comment pos))
  (web-mode-scan-region (point-min) (point-max)))

(defun web-mode-comment (&optional pos)
  "Comment line(s) at point."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (let (type sel beg end)

      (if mark-active
          (progn
            (setq beg (region-beginning)
                  end (region-end))
            (setq type (web-mode-line-type beg))
            ;;            (message "(%d)->(%d)" (region-beginning) (region-end))
            )
        (setq type (web-mode-line-type (line-beginning-position)))
        ;;        (message "type=%S" type)
        (if (string= type "html")
            (progn
              (back-to-indentation)
              (web-mode-select-element))
          (end-of-line)
          (set-mark (line-beginning-position))
          );if
        (setq beg (region-beginning)
              end (region-end))
        ); if

      ;;      (message "type=%s" type)

      (setq sel (web-mode-trim (buffer-substring-no-properties beg end)))
      ;;      (message "[type=%s] sel=%s" type sel)
      (delete-region beg end)
      (deactivate-mark)

      (cond

       ((string= type "html")

        (web-mode-insert-and-indent (concat "<!-- " sel " -->"))
        )

       ((or (string= type "php") (string= type "script") (string= type "style"))
        (web-mode-insert-and-indent (concat "/* " sel " */"))
        )

       (t
        (web-mode-insert-and-indent (concat "/* " sel " */")))

       ))))

(defun web-mode-uncomment (&optional pos)
  "Uncomment line(s) at point."
  (interactive)
  ;;  (web-mode-scan-init)
  (unless pos (setq pos (point)))
  (let ((beg pos)
        (end pos)
        (sub2 "")
        comment prop)

    (if (eq (get-text-property pos 'server-type) 'comment)
        (setq prop 'server-type)
      (setq prop 'client-type))

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
      (back-to-indentation)

      );;when

    ))

(defun web-mode-insert-snippet (code)
  "Insert snippet."
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

;; todo : avec tag-type ... différenciation pour freemarker
(defun web-mode-match-tag ()
  "Match tag."
  (interactive)
  (let ((init (point)) pos)

    (when (> (current-indentation) (current-column))
      (back-to-indentation))

    (setq pos (point))

    (cond

     ((or (web-mode-is-comment-or-string)
          (eq (get-text-property pos 'server-engine) 'directive))
      (goto-char init))

     ((and (eq (get-text-property pos 'server-engine) 'php)
           (web-mode-goto-block-beg)
           (looking-at-p "<\\?\\(php[ ]+\\|[ ]*\\)?\\(end\\)?\\(for\\|if\\|else\\|while\\)"))
      (web-mode-match-php-tag))

     ((and (eq (get-text-property pos 'server-engine) 'smarty)
           (web-mode-goto-block-beg)
           (looking-at-p (concat "{/?" (regexp-opt web-mode-smarty-controls))))
      (web-mode-match-smarty-tag))

     ((and (eq (get-text-property pos 'server-engine) 'velocity)
           (web-mode-goto-block-beg)
           (looking-at-p (concat "#" (regexp-opt web-mode-velocity-controls))))
      (web-mode-match-velocity-tag))

     ((and (eq (get-text-property pos 'server-engine) 'django)
           (web-mode-goto-block-beg)
           (looking-at-p (concat "{%[-]?[ ]*\\(end\\)?" (regexp-opt web-mode-django-controls))))
      (web-mode-match-django-tag))

     ((and (eq (get-text-property pos 'server-engine) 'ctemplate)
           (web-mode-goto-block-beg)
           (looking-at-p "{{[#^/]"))
      (web-mode-match-ctemplate-tag))

     ((and (search-forward ">")
           (web-mode-rsb web-mode-tag-regexp nil t))
      (if (or (web-mode-is-void-element)
              (eq (get-text-property (point) 'server-tag-type) 'void))
          (goto-char init)
        (web-mode-match-html-tag))
      )

     ); cond

    ))

(defun web-mode-match-html-tag (&optional pos)
  "Match HTML tag."
  (unless pos (setq pos (point)))
  (let (closing-tag
        tag)
    (setq tag (or (get-text-property pos 'server-tag-name)
                  (get-text-property pos 'client-tag-name)))
    (setq closing-tag (or (eq (get-text-property pos 'server-tag-type) 'end)
                          (eq (get-text-property pos 'client-tag-type) 'end)))
    (if (eq closing-tag t)
        (web-mode-match-html-opening-tag tag pos)
      (web-mode-match-html-closing-tag tag pos))))

(defun web-mode-match-html-closing-tag (tag pos)
  "Match closing HTML closing tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    ;;    (search-forward ">")
    (web-mode-tag-end)
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0) (re-search-forward regexp nil t))
      ;;      (when (not (web-mode-is-comment-or-string))
      (unless (web-mode-is-comment-or-string)
        (setq n (1+ n))
        ;; (message "[%s] point=%d line=%d"
        ;;          (match-string-no-properties 0)
        ;;          (point)
        ;;          (web-mode-current-line-number))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
            ;;        (if (eq (length (match-string-no-properties 0)) 2)
            (setq counter (- counter 1))
          (setq counter (+ counter 1))))
      )
    (if (> n 0)
        (search-backward "<" 1 t)
      (goto-char pos))
    ))

(defun web-mode-match-html-opening-tag (tag pos)
  "Match opening HTML tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    ;;    (search-backward "<")
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (unless (web-mode-is-comment-or-string)
        (setq n (1+ n))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
            (setq counter (+ counter 1))
          (setq counter (- counter 1))))
      )
    (if (> n 0)
        ()
      ;;       (search-backward "<" 1 t)
      (goto-char pos))
    ))

;; todo : ajouter while + passer en cond
(defun web-mode-match-php-tag ()
  "Match PHP tag."
  (let (beg end code regexp type)
    ;;    (forward-char)
    (setq beg (+ (point) 2))
    (search-forward ">")
    (setq end (- (point) 2))
    (setq code (buffer-substring-no-properties beg end))
    ;;    (message "code %S" code)

    (cond

     ((string-match-p "if\\|else" code)
      (setq regexp "<\\?\\(php[ ]+\\|[ ]*\\)?\\(if\\|else\\|elseif\\|endif\\)"
            type   "if")
      )

     ((string-match-p "foreach" code)
      (setq regexp "<\\?\\(php[ ]+\\|[ ]*\\)?\\(foreach\\|endforeach\\)"
            type   "foreach")
      )

     ((string-match-p "for" code)
      (setq regexp "<\\?\\(php[ ]+\\|[ ]*\\)?\\(for\\|endfor\\)"
            type   "foreach")
      )

     (t
      (setq regexp "<\\?\\(php[ ]+\\|[ ]*\\)?\\(while\\|endwhile\\)"
            type   "foreach")
      )

     )

    (if (string-match-p "end\\(if\\|for\\|while\\)" code)
        (web-mode-match-opening-php-tag regexp type)
      (web-mode-match-closing-php-tag regexp type))))

(defun web-mode-match-opening-php-tag (regexp type)
  "Match PHP opening tag."
  (let ((counter 1) match)
    (search-backward "<")
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "<\\?\\(php[ ]+\\|[ ]*\\)?\\(if\\|for\\|while\\)" match)
          (setq counter (1- counter))
        (if (string-match-p "<\\?\\(php[ ]+\\|[ ]*\\)?end\\(if\\|for\\|while\\)" match)
            (setq counter (1+ counter)))
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
      (if (string-match-p "<\\?\\(php[ ]+\\|[ ]*\\)?\\(if\\|for\\|while\\)" match)
          (setq counter (1+ counter))
        (unless (and (> counter 1)
                     (string-match-p "else" match))
          (setq counter (1- counter)))
        ))
    (search-backward "<")))

(defconst web-mode-django-controls
  '("autoescape" "block" "cache" "call" "embed" "filter" "foreach" "for" "if"
    "macro" "draw" "random" "sandbox" "spaceless" "trans" "with")
  "Django controls.")

(defun web-mode-match-django-tag ()
  "Match django tag."
  (let (beg end chunk regexp)
    (setq beg (+ (point) 2))
    (search-forward "%}")
    (setq end (- (point) 2))
    (setq chunk (buffer-substring-no-properties beg end))
    ;;    (message "chunk=%S" chunk)
    (dolist (control web-mode-django-controls)
      (when (string-match-p control chunk)
        (setq regexp (concat "{%[-]?[ ]*\\(" control "\\|end" control "\\)"))
        ;;        (message "regexp=%S" regexp)
        )
      )
    ;; (cond
    ;;  ((string-match-p "foreach" chunk)
    ;;   (setq regexp "{%[-]?[ ]+\\(foreach\\|endforeach\\)"))
    ;;  ((string-match-p "for" chunk)
    ;;   (setq regexp "{%[-]?[ ]+\\(for\\|endfor\\)[ ]+"))
    ;;  ((string-match-p "if" chunk)
    ;;   (setq regexp "{%[-]?[ ]+\\(if\\|endif\\)"))
    ;;  ((string-match-p "block" chunk)
    ;;   (setq regexp "{%[-]?[ ]+\\(block\\|endblock\\)"))
    ;;  )
    (if (string-match-p "end" chunk)
        (web-mode-match-opening-django-tag regexp)
      (web-mode-match-closing-django-tag regexp))))

(defun web-mode-match-opening-django-tag (regexp)
  "Match django opening tag."
  ;;  (message "opening : regexp=%S" regexp)
  (let ((counter 1) match)
    (search-backward "{%")
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 0))
      ;;      (if (string-match-p "[ ]\\(autoescape\\|block\\|cache\\|call\\|embed\\|filter\\|for\\|foreach\\|if\\|macro\\|draw\\|sandbox\\|spaceless\\|trans\\|with\\)" match)
      ;;      (if (string-match-p (concat (regexp-opt web-mode-django-controls)) match)
      (if (not (string-match-p "end" match))
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    ))

(defun web-mode-match-closing-django-tag (regexp)
  "Match django closing tag."
  ;;  (message "closing : pt=%S regexp=%S" (point) regexp)
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 0))
      ;;      (if (string-match-p (concat (regexp-opt web-mode-django-controls)) match)
      (if (not (string-match-p "end" match))
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    (search-backward "{%")
    ))

(defconst web-mode-smarty-controls
  '("block" "foreach" "for" "if" "section" "while")
  "Smarty controls.")

(defun web-mode-match-smarty-tag ()
  "Match smarty tag."
  (let (beg end chunk regexp)
    (setq beg (+ (point) 1))
    (search-forward "}")
    (setq end (- (point) 1))
    (setq chunk (buffer-substring-no-properties beg end))
    (dolist (control web-mode-smarty-controls)
      (when (string-match-p control chunk)
        (setq regexp (concat "{/?\\(" control "\\)")))
      )
    (if (string-match-p "/" chunk)
        (web-mode-match-opening-smarty-tag regexp)
      (web-mode-match-closing-smarty-tag regexp))))

(defun web-mode-match-opening-smarty-tag (regexp)
  "Match smarty opening tag."
  (let ((counter 1) match)
    (search-backward "{")
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p (concat "/" (regexp-opt web-mode-smarty-controls)) match)
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-match-closing-smarty-tag (regexp)
  "Match smarty closing tag."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p (concat "/" (regexp-opt web-mode-smarty-controls)) match)
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (search-backward "{")
    ))

(defconst web-mode-velocity-controls
  '("define" "foreach" "for" "if" "macro"
    "end")
  "Velocity controls.")

(defun web-mode-match-velocity-tag ()
  "Match velocity tag."
  (let (regexp)
    (setq regexp (concat "#" (regexp-opt web-mode-velocity-controls)))
    (if (looking-at-p "#end")
        (web-mode-match-opening-velocity-tag regexp)
      (web-mode-match-closing-velocity-tag regexp))))

(defun web-mode-match-opening-velocity-tag (regexp)
  "Match velocity opening tag."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "end" match)
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-match-closing-velocity-tag (regexp)
  "Match velocity closing tag."
  (let ((counter 1) match)
    (forward-char)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "end" match)
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (search-backward "#")
    ))

(defun web-mode-match-ctemplate-tag ()
  "Match ctemplate tag."
  (let (regexp)
    (looking-at "{{[#^/]\\([[:alnum:]_]+\\)")
    (setq regexp (concat "{{[#^/]" (match-string-no-properties 1)))
    ;;    (message "pt=%S regexp=%S" (point) regexp)
    (if (looking-at-p "{{/")
        (web-mode-match-opening-ctemplate-tag regexp)
      (web-mode-match-closing-ctemplate-tag regexp))))

(defun web-mode-match-opening-ctemplate-tag (regexp)
  "Match ctemplate opening tag."
  (let ((counter 1) match)
    (while (and (> counter 0) (web-mode-rsb regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "/" match)
          (setq counter (1+ counter))
        (setq counter (1- counter)))
      )
    ))

(defun web-mode-match-closing-ctemplate-tag (regexp)
  "Match ctemplate closing tag."
  (let ((counter 1) match)
    ;;    (forward-char)
    (while (and (> counter 0) (web-mode-rsf regexp nil t))
      (setq match (match-string-no-properties 0))
      ;;      (message "match=%S" match)
      (if (string-match-p "/" match)
          (setq counter (1- counter))
        (setq counter (1+ counter)))
      )
    (search-backward "{{")
    ))

(defun web-mode-debug-point ()
  (interactive)
  (what-cursor-position))

(defvar web-mode-autocompletes
  (list
   '("<?p" "hp  ?>" "\\?>" 3)
   '("<? " "?>" "\\?>" 0)
   '("<?=" "?>" "\\?>" 0)
   '("<!-" "-  -->" "--" 2)
   '("<%-" "-  --%>" "--" 2)
   '("<%@" "  %>" "%>" 1)
   '("{{ " " }}" "}}" 0)
   '("{% " " %}" "%}" 0))
  "Autocompletes")

(defun web-mode-on-after-change (beg end len)
  "Autocomplete"
  ;;  (message "beg=%d, end=%d, len=%d, cur=%d" beg end len (current-column))

  ;;  (backtrace)

  (setq web-mode-expand-last-type "")

  (let ((chunk "")
        (pos (point))
        (cur-col (current-column))
        tag
        continue
        found
        nb
        (i 0)
        counter
        expr
        (l (length web-mode-autocompletes))
        pos-end
        after
        jump-pos
        scan-beg scan-end
        sub2
        c)

    (if (not (= (point-max) (+ (buffer-size) 1)))
        (setq web-mode-is-narrowed t)
        (progn
          (when (and (not web-mode-disable-autocompletion)
                   (> pos 2)
                   (= len 0)
                   (= 1 (- end beg)))

          (if (> (+ end 10) (line-end-position))
              (setq pos-end (line-end-position))
            (setq pos-end (+ end 10)))
          (setq after (buffer-substring-no-properties end pos-end))

          (setq sub2 (buffer-substring-no-properties (- beg 1) end))

          (if (and (= web-mode-tag-autocomplete-style 2)
                   (not found)
                   (string-match-p "[[:alnum:]]>" sub2)
                   (not (get-text-property pos 'server-side))
                   (not (get-text-property pos 'client-side))
                   (web-mode-rsb-html "<[[:alpha:]]")
                   (not (web-mode-is-void-element)))
              (progn
                (forward-char)
                (setq nb (skip-chars-forward "a-z:A-Z0-9"))
                (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
                (goto-char pos)
                (insert (concat "</" tag ">"))
                (goto-char pos)
                (setq found t))
            (goto-char pos))

          (when (and (>= web-mode-tag-autocomplete-style 1)
                     (not found)
                     (not (get-text-property pos 'server-side))
                     (not (get-text-property pos 'client-side))
                     (>= cur-col 2)
                     (string= "</" sub2))
            ;;            (message "%d %S %S" pos (get-text-property pos 'server-type) (get-text-property pos 'client-type))
            (when (string= "></" (buffer-substring-no-properties (- beg 2) end))
              (setq jump-pos (- beg 1)))

            (setq continue t
                  counter 1)
            (while (and continue
                        ;;                    (re-search-backward web-mode-tag-regexp 0 t))
                        (web-mode-rsb-html web-mode-tag-regexp))
              ;;          (when (not (web-mode-is-comment-or-string))
              ;;          (when (not (web-mode-is-csss))
              (setq tag (substring (match-string-no-properties 0) 1))
              ;;            (message "tag=%S" tag)
              (if (string= (substring tag 0 1) "/")
                  (setq counter (1+ counter))
                (if (not (web-mode-is-void-element))
                    (setq counter (1- counter))
                  (message "tag %s is void" tag)
                  ))
              (when (eq counter 0)
                (setq continue nil
                      found t)
                (goto-char pos)
                (if (looking-at ">")
                    (progn
                      (insert tag)
                      (goto-char (+ (point) 1)))
                  (insert (concat tag ">")))
                )
              ;;            ) ;; when
              ) ;; while
            (if jump-pos (goto-char jump-pos)
              (if continue (goto-char pos)))

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
                    (insert (elt expr 1))
                    (goto-char (+ pos (elt expr 3)))
                    (setq found t)))
              (setq i (1+ i)))
            ) ;; when

          )

        (save-excursion
          (when (not (= len (- end beg)))
            (cond
             ((or (> (- end beg) 1) (> len 1))
              (setq scan-beg 1
                    scan-end (point-max))
              )

             ((web-mode-rsb-client "^[ ]*<")
              (setq scan-beg (point))
              (goto-char pos)
              (setq scan-end (if (web-mode-rsf-client ">[ ]*$") (point) (point-max)))
              ;;            (setq scan-end (point-max))
              )
             (t
              (setq scan-beg 1
                    scan-end (point-max))
              )
             );cond
            ;;          (message "scan-region (%S) > (%S)" scan-beg scan-end)
            ;;          (setq scan-end (point-max))
            (web-mode-scan-region scan-beg scan-end)
            ))

        ))))

(defun web-mode-rsb-client (regexp &optional limit noerror)
  "re-search-backward in client."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (get-text-property (point) 'server-side)))
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
              (not (get-text-property (match-beginning 0) 'server-side)))
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
              (not (get-text-property (- (point) (length expr)) 'server-side)))
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
              (not (web-mode-is-csss)))
          (setq continue nil)))
    ret))

(defun web-mode-rsf-html (regexp &optional limit noerror)
  "re-search-forward only in html."
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-csss)))
          (setq continue nil)))
    ret))

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (unload-feature 'web-mode)
  (web-mode)
  (if (fboundp 'web-mode-hook)
      (web-mode-hook)))

(provide 'web-mode)

;;; web-mode.el ends here
