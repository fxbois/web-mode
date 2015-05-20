;;; web-mode-defuns.el --- function definitions for web-mode.el
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

(defun web-mode-scan-region (beg end &optional content-type)
  "Identify nodes/parts/blocks and syntactic symbols (strings/comments)."
  ;;(message "scan-region: beg(%d) end(%d) content-type(%S)" beg end content-type)
  (web-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end web-mode-scan-properties)
           (cond
            ((and content-type (string= content-type "php"))
;;             (web-mode-block-scan beg end)
             )
            ((and content-type
                  (member content-type web-mode-part-content-types))
             (put-text-property beg end 'part-side
                                (cond
                                 ((string= content-type "javascript") 'javascript)
                                 ((string= content-type "json") 'json)
                                 ((string= content-type "jsx") 'jsx)
                                 ((string= content-type "css") 'css)
                                 ))
             (web-mode-scan-blocks beg end)
             (web-mode-part-scan beg end content-type))
            ((member web-mode-content-type web-mode-part-content-types)
             (web-mode-scan-blocks beg end)
             (web-mode-part-scan beg end))
            ((string= web-mode-engine "none")
             (web-mode-scan-elements beg end)
             (web-mode-process-parts beg end 'web-mode-part-scan))
            (t
             (web-mode-scan-blocks beg end)
             (web-mode-scan-elements beg end)
             (web-mode-process-parts beg end 'web-mode-part-scan))
            ) ;cond
           (cons beg end)
           ))))))

(defun web-mode-scan-blocks (reg-beg reg-end)
  "Identifies blocks (with block-side, block-beg, block-end text properties)."
  (save-excursion

    (let ((i 0) open close closing-string start sub1 sub2 pos tagopen tmp delim-open delim-close part-beg part-end tagclose)

      (goto-char reg-beg)

      ;;(message "%S: %Sx%S" (point) reg-beg reg-end)
      ;;(message "regexp=%S" web-mode-block-regexp)
      (while (and (< i 2000)
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

        (let ((l (length tagopen)))
          (when (member (string-to-char tagopen) '(?\s ?\t))
            (setq tagopen (replace-regexp-in-string "\\`[ \t]*" "" tagopen))
            (setq open (+ open (- l (length tagopen))))
            (setq l (length tagopen))
            )
          (setq sub1 (substring tagopen 0 1)
                sub2 (substring tagopen 0 (if (>= l 2) 2 1)))
          )

        (cond

         ((string= web-mode-engine "php")
          (unless (member (char-after) '(?x ?X))
            (setq closing-string '("<\\?". "\\?>")))
          (cond
           ((looking-at-p "<?php")
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
                  delim-open "{%[+-]?"
                  delim-close "[-]?%}"))
           (t
            (setq closing-string "#}"))
           )
          ) ;django

         ((string= web-mode-engine "ejs")
          (setq closing-string "%>"
                delim-open "<%[=-]?"
                delim-close "[-]?%>")
          ) ;ejs

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

         ((string= web-mode-engine "lsp")
          (setq closing-string "%>"
                delim-open "<%[%#]?"
                delim-close "%>")
          ) ;lsp

         ((string= web-mode-engine "mako")
          (cond
           ((and (string= tagopen "<%")
                 (member (char-after) '(?\s ?\n ?\!)))
            (setq closing-string "%>"
                  delim-open "<%[!]?"
                  delim-close "%>"))
           ((member sub2 '("<%" "</"))
            (setq closing-string ">"
                  delim-open "</?%"
                  delim-close "/?>"))
           ((string= sub2 "${")
            (setq closing-string "}"
                  delim-open "${"
                  delim-close "}"))
           (t
            (setq closing-string "EOL"
                  delim-open "%"))
           )
          ) ;mako

         ((string= web-mode-engine "cl-emb")
          (cond
           ((string= tagopen "<%#")
            (setq closing-string "#%>"))
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%[=%]?"
                  delim-close "%>"))
           )
          ) ;cl-emb

         ((string= web-mode-engine "elixir")
          (cond
           ((string= tagopen "<%#")
            (setq closing-string "%>"))
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%[=%]?"
                  delim-close "%>"))
           )
          ) ;elixir

         ((string= web-mode-engine "mojolicious")
          (cond
           ((string= tagopen "<%#")
            (setq closing-string "%>"))
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%\\(==\\|[=%]\\)?"
                  delim-close "%>"))
           ((string= sub2 "%#")
            (setq closing-string "EOL"))
           (t
            (setq closing-string "EOL"
                  delim-open "%\\(==\\|[=%]\\)?"))
           )
          ) ;mojolicious

         ((string= web-mode-engine "ctemplate")
          ;;(message "ctemplate")
          (cond
           ((member tagopen '("{{{" "{{~"))
            (setq closing-string "}~?}}"
                  delim-open "{{~?{"
                  delim-close "}~?}}")
            )
           ((string= tagopen "{~{")
            (setq closing-string "}~?}"
                  delim-open "{~{"
                  delim-close "}~?}")
            )
           ((string= tagopen "{{!")
            (setq closing-string (if (looking-at-p "--") "--}}" "}}"))
            )
           ((string= sub2 "{{")
            (setq closing-string "}~?}"
                  delim-open "{{[>#/%^&]?"
                  delim-close "}~?}"))
           (t
            (setq closing-string "}}"
                  delim-open "${{"
                  delim-close "}}"))
           )
          ) ;ctemplate

         ((string= web-mode-engine "aspx")
          (setq closing-string "%>"
                delim-open "<%[:=#@$]?"
                delim-close "%>")
          ) ;aspx

         ((string= web-mode-engine "asp")
          (cond
           ((string= sub2 "<%")
            (setq closing-string "%>"
                  delim-open "<%[:=#@$]?"
                  delim-close "%>"))
           (t
            (setq closing-string ">"
                  delim-open "</?"
                  delim-close "/?>"))
           )
          ) ;asp

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

         ((string= web-mode-engine "clip")
          (setq closing-string ">"
                delim-open "</?"
                delim-close "/?>")
          ) ;clip

         ((string= web-mode-engine "blade")
          (cond
           ((string= tagopen "{{-")
            (setq closing-string "--}}"))
           ((string= tagopen "{!!")
            (setq closing-string "!!}"
                  delim-open "{!!"
                  delim-close "!!}"))
           ((string= tagopen "@{{")
            (setq closing-string nil))
           ((string= tagopen "{{{")
            (setq closing-string "}}}"
                  delim-open "{{{"
                  delim-close "}}}"))
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
          (cond
           ((string= tagopen "{*")
            (setq closing-string "*}")
            )
           ((string= tagopen "{#")
            (setq closing-string "#}"
                  delim-open "{#"
                  delim-close "#}")
            )
           (t
            (setq closing-string (cons "{" "}")
                  delim-open "{/?"
                  delim-close "}")
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
            (setq closing-string '("{". "}") ;;closing-string "}"
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
                 (looking-at "[[:alpha:]]+"))
            (if (member (match-string-no-properties 0) '("after" "around" "augment" "before" "def" "filter" "method" "override"))
                (setq closing-string ">"
                      delim-open "<[/]?%"
                      delim-close ">")
              (setq closing-string (concat "</%" (match-string-no-properties 0) ">")
                    delim-open "<[^>]+>"
                    delim-close "<[^>]+>")
              ) ;if
            )
           ((and (string= sub2 "<%")
                 (eq (char-after) ?\s))
            (setq closing-string "%>"
                  delim-open "<%"
                  delim-close "%>"))
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

         ((string= web-mode-engine "underscore")
          (setq closing-string "%>"
                delim-open "<%"
                delim-close "%>")
          ) ;underscore

         ((string= web-mode-engine "template-toolkit")
          (cond
           ((string= tagopen "[%#")
            (setq closing-string "%]"))
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
                  delim-open "</?[#@]"
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
            (save-excursion
              (let (paren-pos)
                (setq paren-pos (web-mode-opening-paren-position (1- (point))))
                (if (and paren-pos (get-text-property paren-pos 'block-side))
                    (setq closing-string "EOR")
                  (setq closing-string nil)
                  ) ;if
                ) ;let
              ) ;let
            ) ; case }
           ) ;cond
          ) ;razor

         ) ;cond

        (when closing-string

          (cond

           ((listp closing-string)
            (cond
             ((web-mode-rsf-balanced (car closing-string) (cdr closing-string) reg-end t)
              (setq close (match-end 0)
                    pos (point))
              )
             ((and (string= web-mode-engine "php")
                   (string= "<?" sub2))

              (if (or (text-property-not-all (1+ open) (point-max) 'tag-beg nil)
                      (text-property-not-all (1+ open) (point-max) 'block-beg nil)
                      (looking-at-p "[ \t\n]*<"))

                    (setq close nil
                          delim-close nil
                          pos (point))

                  ;; (setq close (line-end-position)
                  ;;       delim-close nil
                  ;;       pos (line-end-position))

                (setq close (point-max)
                      delim-close nil
                      pos (point-max))
                ) ;if
              ) ;case
             ) ;cond
            ) ;case listp

           ((and (string= web-mode-engine "smarty")
                 (string= closing-string "}"))
            (goto-char open)
            (setq tmp (web-mode-closing-delimiter-position
                       "}"
                       (point)
                       (line-end-position)))
            (if tmp
                (setq tmp (1+ tmp))
              (setq tmp (line-end-position)))
            (goto-char tmp)
            (setq close (point)
                  pos (point))
            )

           ((and (member web-mode-engine '("closure"))
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
                  pos (if (> (point) reg-end) reg-end (point)))
            (goto-char pos))

           ((string= closing-string "EOV")
            (web-mode-velocity-skip-forward open)
            (setq close (point)
                  pos (point)))

           ((and (member web-mode-engine '("ctemplate"))
                 (re-search-forward closing-string reg-end t))
            (setq close (match-end 0)
                  pos (point)))

           ((search-forward closing-string reg-end t)
            (setq close (match-end 0)
                  pos (point)))
           ) ;cond

          (when (and close (>= reg-end pos))
            ;;(message "pos(%S) : open(%S) close(%S)" pos open close)
            (put-text-property open (1+ open) 'block-beg 0)
            (put-text-property open (1+ open) 'block-controls 0)
            (if nil ;;(= close (point-max))
                (progn
                  (put-text-property open (1+ close) 'block-side t)
                  (put-text-property close (1+ close) 'block-end t))
              (put-text-property open close 'block-side t)
              (put-text-property (1- close) close 'block-end t)
              )
            (when delim-open
              (web-mode-block-delimiters-set open close delim-open delim-close))
            (web-mode-block-scan open close)
            ;;(message "tagopen=%s %S" tagopen (point))
            (when (and (string= web-mode-engine "erb")
                       (looking-at-p "<%= javascript_tag do %>"))
              (setq tagopen "<%= javascript_tag do %>")
              )
            (when (and (member tagopen '("<r:script" "<r:style"
                                         "<c:js" "<c:css"
                                         "<%= javascript_tag do %>"))
                       (setq part-beg close)
                       (setq tagclose
                             (cond
                              ((string= tagopen "<r:script") "</r:script")
                              ((string= tagopen "<r:style") "</r:style")
                              ((string= tagopen "<c:js") "</c:js")
                              ((string= tagopen "<c:css") "</c:css")
                              ((string= tagopen "<%= javascript_tag do %>") "<% end %>")
                              ))
                       (web-mode-sf tagclose) ;; reg-end)
                       (setq part-end (match-beginning 0))
                       (> part-end part-beg))
              (put-text-property part-beg part-end
                                 'part-side
                                 (cond
                                  ((member tagopen '("<r:style" "<c:css")) 'css)
                                  (t 'javascript)))
              ;;                (goto-char part-end)
              (setq pos part-beg
                    part-beg nil
                    part-end nil)
              ) ;when
            ) ;when close

          (if pos (goto-char pos))

          ) ;when closing-string

        ) ;while

      (cond
       ((>= i 2000)
        (message "scan-blocks ** warning (%S) **" i))
       ((string= web-mode-engine "razor")
        (web-mode-process-blocks reg-beg reg-end 'web-mode-block-scan))
       ((string= web-mode-engine "django")
        (web-mode-scan-engine-comments reg-beg reg-end
                                       "{% comment %}" "{% endcomment %}"))
       ((string= web-mode-engine "mako")
        (web-mode-scan-engine-comments reg-beg reg-end
                                       "<%doc>" "</%doc>"))
       ((string= web-mode-engine "mason")
        (web-mode-scan-engine-comments reg-beg reg-end
                                       "<%doc>" "</%doc>"))
       ) ;cond

      )))

(defun web-mode-block-delimiters-set (reg-beg reg-end delim-open delim-close)
  "Set text-property 'block-token to 'delimiter-(beg|end) on block delimiters (e.g. <?php ?>)"
  ;;(message "reg-beg(%S) reg-end(%S) delim-open(%S) delim-close(%S)" reg-beg reg-end delim-open delim-close)
  (when (member web-mode-engine
                '("asp" "aspx" "cl-emb" "clip" "closure" "ctemplate" "django" "dust"
                  "elixir" "ejs" "erb" "freemarker" "jsp" "lsp" "mako" "mason" "mojolicious"
                  "smarty" "template-toolkit" "web2py"))
    (save-excursion
      (when delim-open
        (goto-char reg-beg)
        (looking-at delim-open)
        (setq delim-open (match-string-no-properties 0)))
      (when delim-close
        (goto-char reg-end)
        (looking-back delim-close reg-beg t)
        (setq delim-close (match-string-no-properties 0)))
      ))
  (when delim-open
    (put-text-property reg-beg (+ reg-beg (length delim-open))
                       'block-token 'delimiter-beg))
  (when delim-close
    (put-text-property (- reg-end (length delim-close)) reg-end
                       'block-token 'delimiter-end))
  )

(defun web-mode-process-blocks (reg-beg reg-end func)
  (let ((i 0) (continue t) (block-beg reg-beg) (block-end nil))
    (while continue
      (setq block-end nil)
      (unless (get-text-property block-beg 'block-beg)
        (setq block-beg (web-mode-block-next-position block-beg)))
      (when (and block-beg (< block-beg reg-end))
        (setq block-end (web-mode-block-end-position block-beg)))
      (cond
       ((> (setq i (1+ i)) 2000)
        (message "process-blocks ** warning (%S) **" (point))
        (setq continue nil))
       ((or (null block-end) (> block-end reg-end))
        (setq continue nil))
       (t
        (setq block-end (1+ block-end))
        (funcall func block-beg block-end)
        (setq block-beg block-end)
        ) ;t
       ) ;cond
      ) ;while
    ))

(defun web-mode-process-parts (reg-beg reg-end func)
  (let ((i 0) (continue t) (part-beg reg-beg) (part-end nil))
    (while continue
      (setq part-end nil)
      (unless (get-text-property part-beg 'part-side)
        (setq part-beg (web-mode-part-next-position part-beg)))
      (when (and part-beg (< part-beg reg-end))
        (setq part-end (web-mode-part-end-position part-beg)))
      (cond
       ((> (setq i (1+ i)) 100)
        (message "process-parts ** warning (%S) **" (point))
        (setq continue nil))
       ((or (null part-end) (> part-end reg-end))
        (setq continue nil))
       (t
        (setq part-end (1+ part-end))
        (funcall func part-beg part-end)
        (setq part-beg part-end)
        )
       ) ;cond
      ) ;while
    ))

(defun web-mode-block-scan (block-beg block-end)
  (let (sub1 sub2 sub3 regexp token-type)

    ;;(message "block-beg=%S block-end=%S" block-beg block-end)
    ;;(remove-text-properties block-beg block-end web-mode-scan-properties)

    (goto-char block-beg)

    (cond
     ((>= (point-max) (+ block-beg 3))
      (setq sub3 (buffer-substring-no-properties block-beg (+ block-beg 3))
            sub2 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      )
     ((>= (point-max) (+ block-beg 2))
      (setq sub3 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub2 (buffer-substring-no-properties block-beg (+ block-beg 2))
            sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      )
     (t
      (setq sub1 (buffer-substring-no-properties block-beg (+ block-beg 1)))
      (setq sub2 sub1
            sub3 sub1)
      )
     )

    (cond

     ((member web-mode-engine '("php" "lsp" "python" "web2py" "mason"))
      (setq regexp web-mode-engine-token-regexp))

     ((string= web-mode-engine "mako")
      (cond
       ((string= sub2 "##")
        (setq token-type 'comment)
        )
       (t
        (setq regexp web-mode-engine-token-regexp))
       )
      ) ;mako

     ((string= web-mode-engine "django")
      (cond
       ((member sub2 '("{{" "{%"))
        (setq regexp "\"\\|'"))
       ((string= sub2 "{#")
        (setq token-type 'comment))
       )
      ) ;django

     ((string= web-mode-engine "ctemplate")
      (cond
       ((string= sub3 "{{!")
        (setq token-type 'comment)
        )
       ((member sub2 '("{{"))
        ;;(setq regexp "\"\\|'")
        )
       )
      ) ;ctemplate

     ((string= web-mode-engine "go")
      (cond
       ((string= sub3 "{{/")
        (setq token-type 'comment))
       ((string= sub2 "{{")
        (setq regexp "\"\\|'"))
       )
      ) ;go

     ((string= web-mode-engine "razor")
      (cond
       ((string= sub2 "@*")
        (setq token-type 'comment))
       (t
        (setq regexp "//\\|@\\*\\|\"\\|'"))
       )
      ) ;razor

     ((string= web-mode-engine "blade")
      (cond
       ((string= sub3 "{{-")
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;blade

     ((string= web-mode-engine "cl-emb")
      (cond
       ((string= sub3 "<%#")
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;cl-emb

     ((string= web-mode-engine "elixir")
      (cond
       ((string= sub3 "<%#")
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;elixir

     ((string= web-mode-engine "mojolicious")
      (cond
       ((or (string= sub2 "%#") (string= sub3 "<%#"))
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;mojolicious

     ((string= web-mode-engine "velocity")
      (cond
       ((member sub2 '("##" "#*"))
        (setq token-type 'comment))
       ((member sub1 '("$" "#"))
        (setq regexp "\"\\|'"))
       )
      ) ;velocity

     ((string= web-mode-engine "jsp")
      (cond
       ((string= sub3 "<%-")
        (setq token-type 'comment))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"))
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"))
       ((string= sub2 "<%")
        (setq regexp "//\\|/\\*\\|\"\\|'"))
       )
      ) ;jsp

     ((string= web-mode-engine "clip")
      (setq regexp nil)
      ) ;clip

     ((and (string= web-mode-engine "asp")
           (string= sub2 "<%"))
      (setq regexp "//\\|/\\*\\|\"\\|'")
      ) ; asp

     ((string= web-mode-engine "aspx")
      (cond
       ((string= sub3 "<%-")
        (setq token-type 'comment))
       ((string= sub3 "<%@")
        (setq regexp "/\\*"))
       ((string= sub3 "<%$")
        (setq regexp "\"\\|'"))
       (t
        (setq regexp "//\\|/\\*\\|\"\\|'"))
       )
      ) ;aspx

     ((string= web-mode-engine "freemarker")
      (cond
       ((member sub3 '("<#-" "[#-"))
        (setq token-type 'comment))
       ((member sub2 '("${" "#{"))
        (setq regexp "\"\\|'"))
       ((or (member sub2 '("<@" "[@" "<#" "[#"))
            (member sub3 '("</@" "[/@" "</#" "[/#")))
        (setq regexp "\"\\|'"))
       )
      ) ;freemarker

     ((member web-mode-engine '("ejs" "erb"))
      (cond
       ((string= sub3 "<%#")
        (setq token-type 'comment))
       (t
        (setq regexp web-mode-engine-token-regexp))
       )
      ) ;erb

     ((string= web-mode-engine "template-toolkit")
      (cond
       ((string= sub3 "[%#")
        (setq token-type 'comment))
       (t
        (setq regexp "#\\|\"\\|'"))
       )
      ) ;template-toolkit

     ((string= web-mode-engine "underscore")
      (setq regexp "/\\*\\|\"\\|'")
      ) ;underscore

     ((string= web-mode-engine "angular")
      ) ;angular

     ((string= web-mode-engine "smarty")
      (cond
       ((string= sub2 "{*")
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'")))
      ) ;smarty

     ((string= web-mode-engine "dust")
      (cond
       ((string= sub2 "{!")
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;dust

     ((string= web-mode-engine "closure")
      (cond
       ((member sub2 '("/*" "//"))
        (setq token-type 'comment))
       (t
        (setq regexp "\"\\|'"))
       )
      ) ;closure

     ) ;cond

    (cond
     (token-type
      (put-text-property block-beg block-end 'block-token token-type))
     ((and regexp
           (> (- block-end block-beg) 6))
      (web-mode-block-tokenize
       (web-mode-block-code-beginning-position block-beg)
       (web-mode-block-code-end-position block-beg)
       regexp)
      )
     ) ;cond

    ))

(defun web-mode-block-tokenize (reg-beg reg-end &optional regexp)
  (unless regexp (setq regexp web-mode-engine-token-regexp))
  ;;(message "tokenize: reg-beg(%S) reg-end(%S) regexp(%S)" reg-beg reg-end regexp)
  ;;(message "tokenize: reg-beg(%S) reg-end(%S) command(%S)" reg-beg reg-end this-command)
  ;;(message "%S>%S : %S" reg-beg reg-end (buffer-substring-no-properties reg-beg reg-end))
  (save-excursion
    (let ((pos reg-beg) beg end char match continue (flags 0) token-type token-end)

      (remove-list-of-text-properties reg-beg reg-end '(block-token))

      ;; TODO : vérifier la cohérence
      (put-text-property reg-beg reg-end 'block-side t)

      (goto-char reg-beg)

      (when (> reg-beg reg-end)
        (message "block-tokenize ** reg-beg(%S) reg-end(%S) **" reg-beg reg-end))

      (while (and (< reg-beg reg-end) (re-search-forward regexp reg-end t))
        (setq beg (match-beginning 0)
              match (match-string 0)
              continue t
              token-type 'comment
              token-end (if (< reg-end (line-end-position)) reg-end (line-end-position))
              char (aref match 0))
        (cond

         ((and (string= web-mode-engine "asp")
               (eq char ?\'))
          (goto-char token-end))

         ((eq char ?\')
          (setq token-type 'string)
          (while (and continue (search-forward "'" reg-end t))
            (if (looking-back "\\\\+'" reg-beg t)
                (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
              (setq continue nil))))

         ((eq char ?\")
          (setq token-type 'string)
          (while (and continue (search-forward "\"" reg-end t))
            (if (looking-back "\\\\+\"" reg-beg t)
                (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0))
              (setq continue nil))))

         ((string= match "//")
          (goto-char token-end))

         ((eq char ?\;)
          (goto-char token-end))

         ((string= match "#|")
          (unless (search-forward "|#" reg-end t)
            (goto-char token-end)))

         ((eq char ?\#)
          (goto-char token-end))

         ((string= match "/*")
          (unless (search-forward "*/" reg-end t)
            ;;(message "*/ found: %S [%c] [%c] %S" (point) (char-before) (char-after) reg-end)
            (goto-char token-end))
          ;;(unless (search-forward "*/" reg-end t)
          ;;  (goto-char token-end))
          ;;(message "*/ %S" (point))
          )

         ((string= match "@*")
          (unless (search-forward "*@" reg-end t)
            (goto-char token-end)))

         ((eq char ?\<)
          (setq token-type 'string)
          (re-search-forward (concat "^[ ]*" (match-string 1)) reg-end t))

         (t
          (message "block-tokenize ** token end (%S) **" beg)
          (setq token-type nil))

         ) ;cond

        ;;(when (eq token-type 'comment) (message "comment: %S %S" beg (point)))

        (put-text-property beg (point) 'block-token token-type)

        (when (eq token-type 'comment)
          (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "<"))
          (if (or (< (point) (line-end-position)) (= (point) (point-max)))
              (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">")) ;#445 #480
            (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ">")) ;#377
            )
          )

        ) ;while

      (web-mode-block-controls-unset pos)

      )))

(defun web-mode-set-php-controls (reg-beg reg-end)
  (goto-char reg-beg)
  (let (match controls
        (continue t)
        (regexp "endif\\|endforeach\\|endfor\\|endwhile\\|elseif\\|else\\|if\\|foreach\\|for\\|while"))
    (while continue
      (if (not (web-mode-block-rsf regexp reg-end))
          (setq continue nil)
        (setq match (match-string-no-properties 0))
;;        (message "%S %S" match (point))
        (cond
         ((and (member match '("else" "elseif"))
               (looking-at-p "[ ]*[:(]"))
          (setq controls (append controls (list (cons 'inside "if"))))
          )
         ((and (>= (length match) 3)
               (string= (substring match 0 3) "end"))
          (setq controls (append controls (list (cons 'close (substring match 3)))))
          )
         ((and (progn (skip-chars-forward "[ ]") t)
               (eq (char-after) ?\()
               (web-mode-closing-paren reg-end)
;;               (progn (message "ixi%S" (point)))
               (looking-at-p ")[ ]*:"))
          (setq controls (append controls (list (cons 'open match))))
          )
         ) ; cond
        ) ;if
      ) ;while
    ;;(message "%S-%S %S" reg-beg reg-end controls)
    (when (and controls (> (length controls) 1))
      (setq controls (web-mode-block-controls-reduce controls)))
    controls))

;; todo : clean
;; <?php if (): echo $x; endif; ?>
;; ((open . "if") (close . "if"))
;; -> nil
(defun web-mode-block-controls-reduce (controls)
  (when (and (eq (car (car controls)) 'open)
             (member (cons 'close (cdr (car controls))) controls))
    (setq controls nil))
  controls)

(defun web-mode-block-controls-unset (pos)
  (cond
   ((null (get-text-property pos 'block-side))
    (message "block-controls-unset ** invalid value (%S) **" pos))
   ((or (get-text-property pos 'block-beg)
        (setq pos (web-mode-block-beginning-position pos)))
    (put-text-property pos (1+ pos) 'block-controls 0))
   (t
    (message "block-controls-unset ** failure (%S) **" (point)))
   ))

(defun web-mode-block-controls-get (pos)
  (web-mode-with-silent-modifications
   (let ((controls nil))
     (cond
      ((null (get-text-property pos 'block-side))
       (message "block-controls-get ** invalid value (%S) **" pos))
      ((or (get-text-property pos 'block-beg)
           (setq pos (web-mode-block-beginning-position pos)))
       (setq controls (get-text-property pos 'block-controls))
       (when (integerp controls)
         (web-mode-block-controls-set pos (web-mode-block-end-position pos))
         (setq controls (get-text-property pos 'block-controls))
         ;;        (message "controls=%S" controls)
         )
       )
      (t
       (message "block-controls-get ** failure (%S) **" (point)))
      ) ;cond
     controls)))

(defun web-mode-block-controls-set (reg-beg reg-end)
  (save-excursion
    (goto-char reg-beg)
    (let (controls pos type control)

      (cond

       ((null web-mode-engine)
        (message "block-controls-set ** unknown engine (%S) **" web-mode-engine)
        )

       ((string= web-mode-engine "php")
        (setq controls (web-mode-set-php-controls reg-beg reg-end))
        (when (web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
        (when (web-mode-block-ends-with (cons "{" "}") reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
        ) ; php

       ((string= web-mode-engine "ejs")
        (cond
         ((web-mode-block-ends-with "}[ ]*else[ ]*{" reg-beg)
          (setq controls (append controls (list (cons 'inside "{")))))
         ((web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
         ((web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
         )
        ) ; ejs

       ((string= web-mode-engine "erb")
        (cond
         ((web-mode-block-starts-with "else\\|elsif\\|when" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((and (web-mode-block-starts-with "\\(.* do\\|for\\|if\\|unless\\|case\\)\\>" reg-beg)
               (not (web-mode-block-ends-with "end" reg-end)))
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ; erb

       ((string= web-mode-engine "django")
        (when (eq (char-after (1+ reg-beg)) ?\%)
          (cond
           ((and (string= web-mode-minor-engine "jinja") ;#504
                 (web-mode-block-starts-with "else\\>" reg-beg))
            (let ((continue t)
                  (pos reg-beg)
                  (ctrl nil))
              (while continue
                (cond
                 ((null (setq pos (web-mode-block-control-previous-position 'open pos)))
                  (setq continue nil))
                 ((member (setq ctrl (cdr (car (get-text-property pos 'block-controls)))) '("if" "ifequal" "ifnotequal" "for"))
                  (setq continue nil)
                  )
                 ) ;cond
                )
              (setq controls (append controls (list (cons 'inside (or ctrl "if")))))
              )
            )
           ((web-mode-block-starts-with "\\(else\\|els?if\\)" reg-beg)
            (let ((continue t)
                  (pos reg-beg)
                  (ctrl nil))
              (while continue
                (cond
                 ((null (setq pos (web-mode-block-control-previous-position 'open pos)))
                  (setq continue nil))
                 ((member (setq ctrl (cdr (car (get-text-property pos 'block-controls)))) '("if" "ifequal" "ifnotequal"))
                  (setq continue nil)
                  )
                 ) ;cond
                ) ;while
              (setq controls (append controls (list (cons 'inside (or ctrl "if")))))
              ) ;let
            ) ;case else
           ((web-mode-block-starts-with "\\(empty\\)" reg-beg)
            (setq controls (append controls (list (cons 'inside "for")))))
           ((web-mode-block-starts-with "end\\([[:alpha:]]+\\)" reg-beg)
            (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
           ((web-mode-block-starts-with (concat web-mode-django-control-blocks-regexp "\\>") reg-beg)
            (let (control)
              (setq control (match-string-no-properties 1))
              ;;(message "%S %S %S" control (concat "end" control) web-mode-django-control-blocks)
              (when (member (concat "end" control) web-mode-django-control-blocks)
                (setq controls (append controls (list (cons 'open control)))))
              ) ;let
            ) ;case
           ) ;cond
          ) ;when
        ) ;django

       ((string= web-mode-engine "smarty")
        (cond
         ((and (eq (char-after (1+ reg-beg)) ?\/)
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
                                          (cdr (car (web-mode-block-controls-get pos))))))))
          )
         ((looking-at "{/\\([[:alpha:].]+\\)")
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((looking-at "{[#?@><+^]\\([[:alpha:].]+\\)")
          (setq controls (append controls (list (cons 'open (match-string-no-properties 1))))))
         )
        ) ;dust

       ((member web-mode-engine '("mojolicious"))
        (cond
         ((web-mode-block-ends-with "begin" reg-beg)
          (setq controls (append controls (list (cons 'open "begin")))))
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "begin")))))
         ((web-mode-block-starts-with "}[ ]*else[ ]*{" reg-beg)
          (setq controls (append controls (list (cons 'inside "{")))))
         ((web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
         ((web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
         )
        ) ;mojolicious

       ((member web-mode-engine '("aspx" "underscore"))
        (cond
         ((web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
         ((web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
         )
        ) ;aspx underscore

       ((member web-mode-engine '("jsp" "asp" "clip"))
        (cond
         ((eq (char-after (1- reg-end)) ?\/)
          )
         ((looking-at "</?\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)\\|[[:alpha:]]+Template\\)")
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
        ) ;jsp asp

       ((string= web-mode-engine "mako")
        (cond
         ((looking-at "</?%\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)")
          (cond
           ((eq (char-after (- (web-mode-block-end-position reg-beg) 1)) ?\/)
            )
           (t
            (setq control (match-string-no-properties 1)
                  type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
            (setq controls (append controls (list (cons type control)))))
           )
          )
         ((web-mode-block-starts-with "\\(else\\|elif\\)" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "end\\(if\\|for\\)" reg-beg)
          (setq controls (append controls (list (cons 'close (match-string-no-properties 1))))))
         ((and (web-mode-block-starts-with "if\\|for" reg-beg)
               (web-mode-block-ends-with ":" reg-beg))
          (setq controls (append controls (list (cons 'open (match-string-no-properties 0))))))
         )
        ) ;mako

       ((string= web-mode-engine "mason")
        (cond
         ((looking-at "</?%\\(after\\|around\\|augment\\|before\\|def\\|filter\\|method\\|override\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ) ;mason
        )

       ((string= web-mode-engine "ctemplate")
        (cond
         ((looking-at-p "{{else")
          (setq controls (append controls (list (cons 'inside "if")))))
         ((looking-at "{{[#^/][ ]*\\([[:alpha:]-]+\\)")
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
           "\\(?:end\\)?\\(foreach\\|forelse\\|for\\|if\\|section\\|unless\\|while\\)"
           reg-beg)
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 0) ?e) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ((web-mode-block-starts-with "stop\\|show\\|overwrite" reg-beg)
          (setq controls (append controls (list (cons 'close "section")))))
         ((web-mode-block-starts-with "else\\|elseif" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "empty" reg-beg)
          (setq controls (append controls (list (cons 'inside "forelse")))))
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
         ((web-mode-block-starts-with "end\\>" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "else\\>" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "\\(range\\|with\\|if\\)\\>" reg-beg)
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

       ((string= web-mode-engine "cl-emb")
        (cond
         ((web-mode-block-starts-with "@else" reg-beg)
          (setq controls (append controls (list (cons 'inside "if")))))
         ((web-mode-block-starts-with "@\\(?:end\\)?\\(if\\|unless\\|repeat\\|loop\\|with\\|genloop\\)" reg-beg)
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?e) 'close 'open))
          (setq controls (append controls (list (cons type control)))))
         )
        ) ;cl-emb

       ((string= web-mode-engine "elixir")
        (cond
         ((web-mode-block-starts-with "end" reg-beg)
          (setq controls (append controls (list (cons 'close "ctrl")))))
         ((web-mode-block-starts-with "else" reg-beg)
          (setq controls (append controls (list (cons 'inside "ctrl")))))
         ((web-mode-block-starts-with "if\\|for\\|while" reg-beg)
          (setq controls (append controls (list (cons 'open "ctrl")))))
         )
        ) ;elixir

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

       ((string= web-mode-engine "freemarker")
        (cond
         ((looking-at "<#\\(import\\|assign\\|return\\|local\\)")
          )
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
         ((looking-at "</?\\(@\\)")
          (setq control (match-string-no-properties 1)
                type (if (eq (aref (match-string-no-properties 0) 1) ?\/) 'close 'open))
          (setq controls (append controls (list (cons type control))))
          )
         ((looking-at "[<[]/?#\\([[:alpha:]]+\\(?:[:][[:alpha:]]+\\)?\\)")
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

       ((string= web-mode-engine "razor")
        (when (web-mode-block-starts-with "}" reg-beg)
          (setq controls (append controls (list (cons 'close "{")))))
        (when (web-mode-block-ends-with "{" reg-beg)
          (setq controls (append controls (list (cons 'open "{")))))
        ) ;razor

       ((string= web-mode-engine "lsp")
        (when (web-mode-block-starts-with ")" reg-beg)
          (setq controls (append controls (list (cons 'close "(")))))
        (when (web-mode-block-is-opened-sexp reg-beg reg-end)
          (setq controls (append controls (list (cons 'open "(")))))
        ) ;lsp

       ) ;cond engine

      (put-text-property reg-beg (1+ reg-beg) 'block-controls controls)
      ;;      (message "(%S) controls=%S" reg-beg controls)

      )))

(defun web-mode-block-is-opened-sexp (reg-beg reg-end)
  (let ((n 0))
    (save-excursion
      (goto-char reg-beg)
      (while (web-mode-block-rsf "[()]" reg-end)
        (if (eq (char-before) ?\() (setq n (1+ n)) (setq n (1- n)))))
    (> n 0)))

(defvar web-mode-regexp1 "<\\(/?[[:alpha:]][[:alnum:]-]*\\|!--\\|!\\[CDATA\\[\\|!doctype\\|!DOCTYPE\\|\?xml\\)")

(defvar web-mode-regexp2 "<\\(/?[[:alpha:]][[:alnum:]-]*\\|!--\\|!\\[CDATA\\[\\)")

(defun web-mode-scan-elements (reg-beg reg-end)
  (save-excursion
    (let (part-beg part-end flags limit close-expr props tname tbeg tend element-content-type (regexp web-mode-regexp1) part-close-tag char)

      (goto-char reg-beg)

      (while (web-mode-dom-rsf regexp reg-end)

        (setq flags 0
              tname (downcase (match-string-no-properties 1))
              char (aref tname 0)
              tbeg (match-beginning 0)
              tend nil
              element-content-type nil
              limit reg-end
              part-beg nil
              part-end nil
              props nil
              close-expr nil
              part-close-tag nil)

        (cond
         ((not (member char '(?\! ?\?)))
          (when (string-match-p "-" tname)
            (setq flags (logior flags 2)))
          (cond
           ((eq char ?\/)
            (setq props (list 'tag-name (substring tname 1) 'tag-type 'end)
                  flags (logior flags 4)
                  limit (if (> reg-end (line-end-position)) (line-end-position) reg-end))
            )
           ((web-mode-element-is-void tname)
            (setq props (list 'tag-name tname 'tag-type 'void)))
           (t
            (setq props (list 'tag-name tname 'tag-type 'start)))
           ) ;cond
          )
         ((and (eq char ?\!) (eq (aref tname 1) ?\-))
          (setq close-expr "-->"
                props '(tag-type comment)))
         ((string= tname "?xml")
          (setq regexp web-mode-regexp2
                close-expr "?>"
                props '(tag-type declaration)))
         ((string= tname "![cdata[")
          (setq close-expr "]]>"
                props '(tag-type cdata)))
         ((string= tname "!doctype")
          (setq regexp web-mode-regexp2
                props '(tag-type doctype)))
         ) ;cond

        (cond
         ((and (null close-expr) (eq (char-after) ?\>))
          (setq flags (logior flags 16)
                tend (1+ (point))))
         ((null close-expr)
          (setq flags (logior flags (web-mode-attr-skip reg-end)))
          (when (> (logand flags 8) 0)
            (setq props (plist-put props 'tag-type 'void)))
          (setq tend (point)))
         ((web-mode-dom-sf close-expr limit t)
          (setq tend (point)))
         (t
          (setq tend (line-end-position)))
         )

        (cond
         ((string= tname "style")
          (setq element-content-type "css"
                part-close-tag "</style>"))
         ((string= tname "script")
          (let (script)
            (setq script (buffer-substring-no-properties tbeg tend)
                  part-close-tag "</script>")
            (cond
             ((string-match-p " type[ ]*=[ ]*[\"']text/jsx" script)
              (setq element-content-type "jsx"))
             ((string-match-p " type[ ]*=[ ]*[\"']text/\\(x-handlebars\\|html\\|ng-template\\|template\\)" script)
              (setq element-content-type "html"
                    part-close-tag nil))
             ((string-match-p " type[ ]*=[ ]*[\"']application/\\(ld\\+json\\|json\\)" script)
              (setq element-content-type "json"))
             (t
              (setq element-content-type "javascript"))
             ) ;cond
            ) ;let
          ) ;script
         )

        (add-text-properties tbeg tend props)
        (put-text-property tbeg (1+ tbeg) 'tag-beg flags)
        (put-text-property (1- tend) tend 'tag-end t)

        (when (and part-close-tag
                   (web-mode-dom-sf part-close-tag reg-end t)
                   (setq part-beg tend)
                   (setq part-end (match-beginning 0))
                   (> part-end part-beg))
          (put-text-property part-beg part-end 'part-side
                             (intern element-content-type web-mode-obarray))
          (setq tend part-end)
          ) ;when

        (goto-char tend)

        ) ;while

      )))

;; tag flags
;; (1)attrs (2)custom (4)slash-beg (8)slash-end (16)bracket-end

;; attr flags
;; (1)custom-attr (2)engine-attr

;; attr states
;; (0)nil (1)space (2)name (3)space-before (4)equal (5)space-after
;; (6)value-uq (7)value-sq (8)value-dq (9)value-bq : jsx attr={}

(defun web-mode-attr-skip (limit)

  (let ((tag-flags 0) (attr-flags 0) (continue t) (attrs 0) (counter 0) (brace-depth 0)
        (pos-ori (point)) (state 0) (equal-offset 0) (go-back nil)
        name-beg name-end val-beg char pos escaped spaced quoted)

    (while continue

      (setq pos (point)
            char (char-after)
            spaced (eq char ?\s))

      (when quoted (setq quoted (1+ quoted)))

      (cond

       ((>= pos limit)
        (setq continue nil)
        (setq go-back t)
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        )

       ((or (and (= state 8) (not (member char '(?\" ?\\))))
            (and (= state 7) (not (member char '(?\' ?\\))))
            (and (= state 9) (not (member char '(?} ?\\))))
            )
        (when (and (= state 9) (eq char ?\{))
          (setq brace-depth (1+ brace-depth)))
        )

       ((and (= state 9) (eq char ?\}) (> brace-depth 1))
        (setq brace-depth (1- brace-depth)))

       ((get-text-property pos 'block-side)
        (when (= state 2)
          (setq name-end pos))
        )

       ((or (and (= state 8) (eq ?\" char) (not escaped))
            (and (= state 7) (eq ?\' char) (not escaped))
            (and (= state 9) (eq ?\} char) (= brace-depth 1))
            )
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 0
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (member state '(4 5)) (member char '(?\' ?\" ?\{)))
        (setq val-beg pos)
        (setq quoted 1)
        (setq state (cond ((eq ?\' char) 7)
                          ((eq ?\" char) 8)
                          (t             9)))
        (when (= state 9)
          (setq brace-depth 1))
        )

       ((and (eq ?\= char) (member state '(2 3)))
        (setq equal-offset (- pos name-beg))
        (setq state 4)
        )

       ((and spaced (= state 0))
        (setq state 1)
        )

       ((and (eq char ?\<) (not (member state '(7 8 9))))
        (setq continue nil)
        (setq go-back t)
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        )

       ((and (eq char ?\>) (not (member state '(7 8 9))))
        (setq tag-flags (logior tag-flags 16))
        (when (eq (char-before) ?\/)
          (setq tag-flags (logior tag-flags 8))
          )
        (setq continue nil)
        (when name-beg
          (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset))))
        )

       ((and spaced (member state '(1 3 5)))
        )

       ((and spaced (= state 2))
        (setq state 3)
        )

       ((and (eq char ?\/) (member state '(4 5)))
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (eq char ?\/) (member state '(0 1)))
        )

       ((and spaced (= state 4))
        (setq state 5)
        )

       ((and (= state 3)
             (or (and (>= char 97) (<= char 122)) ;a - z
                 (and (>= char 65) (<= char 90)) ;A - Z
                 (eq char ?\-)))
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 2
              attr-flags 0
              equal-offset 0
              name-beg pos
              name-end pos
              val-beg nil)
        )

       ((and (eq char ?\n) (not (member state '(7 8 9))))
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (= state 6) (member char '(?\s ?\n ?\/)))
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and quoted (= quoted 2) (member char '(?\s ?\n ?\>)))
        (when (eq char ?\>)
          (setq tag-flags (logior tag-flags 16))
          (setq continue nil))
        (setq state 6)
        (setq attrs (+ attrs (web-mode-attr-scan state char name-beg name-end val-beg attr-flags equal-offset)))
        (setq state 1
              attr-flags 0
              equal-offset 0
              name-beg nil
              name-end nil
              val-beg nil)
        )

       ((and (not spaced) (= state 1))
        (setq state 2)
        (setq name-beg pos
              name-end pos)
        )

       ((member state '(4 5))
        (setq val-beg pos)
        (setq state 6)
        )

       ((= state 1)
        (setq state 2)
        )

       ((= state 2)
        (setq name-end pos)
        (when (and (= attr-flags 0) (member char '(?\- ?\:))) ;;(= (logand attr-flags 1) 1)
          (let (attr)
            (setq attr (buffer-substring-no-properties name-beg (1+ name-end)))
            (cond
             ((member attr '("http-equiv"))
              (setq attr-flags (1- attr-flags))
              )
             ((and web-mode-engine-attr-regexp
                   (string-match-p web-mode-engine-attr-regexp attr))
              ;;(message "%S: %S" pos web-mode-engine-attr-regexp)
              (setq attr-flags (logior attr-flags 2))
              ;;(setq attr-flags (1- attr-flags))
              )
             ((and (eq char ?\-) (not (string= attr "http-")))
              (setq attr-flags (logior attr-flags 1)))
             ) ;cond
            ) ;let
          ) ;when attr-flags = 1
        ) ;state=2

       ) ;cond

      ;;(message "point(%S) end(%S) state(%S) c(%S) name-beg(%S) name-end(%S) val-beg(%S) attr-flags(%S) equal-offset(%S)" pos end state char name-beg name-end val-beg attr-flags equal-offset)

      (when (and quoted (>= quoted 2))
        (setq quoted nil))

      (setq escaped (eq ?\\ char))
      (when (null go-back)
        (forward-char))

      (when (> (setq counter (1+ counter)) 3200)
        (message "attr-skip ** too much attr ** pos-ori(%S) limit(%S)" pos-ori limit)
        (setq continue nil))

      ) ;while

    (when (> attrs 0)
      (setq tag-flags (logior tag-flags 1)))

    tag-flags))

(defun web-mode-attr-scan (state char name-beg name-end val-beg flags equal-offset)
;;  (message "point(%S) state(%S) c(%c) name-beg(%S) name-end(%S) val-beg(%S) flags(%S) equal-offset(%S)"
;;           (point) state char name-beg name-end val-beg flags equal-offset)
  (if (null flags) (setq flags 0))
  (cond
   ((null name-beg)
;;    (message "name-beg is null (%S)" (point))
    0)
   ((or (and (= state 8) (not (eq ?\" char)))
        (and (= state 7) (not (eq ?\' char))))
    (put-text-property name-beg val-beg 'tag-attr flags)
    (put-text-property (1- val-beg) val-beg 'tag-attr-end equal-offset)
    1)
   ((and (member state '(4 5)) (null val-beg))
    (put-text-property name-beg (+ name-beg equal-offset 1) 'tag-attr flags)
    (put-text-property (+ name-beg equal-offset) (+ name-beg equal-offset 1) 'tag-attr-end equal-offset)
    1)
   (t
    (let (val-end)
      (if (null val-beg)
          (setq val-end name-end)
        (setq val-end (point))
        (when (or (null char) (member char '(?\s ?\n ?\> ?\/)))
          (setq val-end (1- val-end))
;;          (message "val-end=%S" val-end)
          )
        ) ;if
      (put-text-property name-beg (1+ val-end) 'tag-attr flags)
      (put-text-property val-end (1+ val-end) 'tag-attr-end equal-offset)
      ) ;let
    1) ;t
   (t
    0)
   ) ;cond
  )

(defun web-mode-part-scan (reg-beg reg-end &optional content-type)
  (save-excursion
    (let (token-re ch-before ch-at ch-next token-type beg end continue)

      (cond
       (content-type
        )
       ((member web-mode-content-type web-mode-part-content-types)
        (setq content-type web-mode-content-type))
       (t
        (setq content-type (symbol-name (get-text-property reg-beg 'part-side))))
       ) ;cond

      (goto-char reg-beg)

      (cond
       ((member content-type '("javascript" "json"))
        (setq token-re "//\\|/\\*\\|\"\\|'\\|`"))
       ((member content-type '("jsx"))
        (setq token-re "//\\|/\\*\\|\"\\|'\\|`\\|</?[[:alpha:]]"))
       ((string= content-type "css")
        (setq token-re "/\\*"))
       (t
        (setq token-re "/\\*\\|\"\\|'"))
       )

      (while (and token-re (< (point) reg-end) (web-mode-dom-rsf token-re reg-end t))
        (setq beg (match-beginning 0)
              end nil
              token-type nil
              continue t
              ch-at (char-after beg)
              ch-next (or (char-after (1+ beg)) ?\d)
              ch-before (or (char-before beg) ?\d))
        (cond

         ((eq ?\' ch-at)
          (while (and continue (search-forward "'" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             ((looking-back "\\\\+'" reg-beg t)
              (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0)))
             (t
              (setq continue nil))
             )
            ) ;while
          (setq token-type 'string))

         ((eq ?\` ch-at)
          (while (and continue (search-forward "`" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             ((looking-back "\\\\+`" reg-beg t)
              (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0)))
             (t
              (setq continue nil))
             )
            ) ;while
          (setq token-type 'string))

         ((eq ?\" ch-at)
          (while (and continue (search-forward "\"" reg-end t))
            (cond
             ((get-text-property (1- (point)) 'block-side)
              (setq continue t))
             ((looking-back "\\\\+\"" reg-beg t)
              (setq continue (= (mod (- (point) (match-beginning 0)) 2) 0)))
             (t
              (setq continue nil))
             )
            )
          (cond
           ((string= content-type "json")
            (if (looking-at-p "[ ]*:")
                (cond
                 ((eq ?\@ (char-after (1+ beg)))
                  (setq token-type 'context))
                 (t
                  (setq token-type 'key))
                 )
              (setq token-type 'string))
            )
           (t
            (setq token-type 'string))
           ) ;cond
          )

         ((eq ?\< ch-at)
          (when (web-mode-jsx-skip-forward reg-end)
            (setq end (point))
            (put-text-property beg end 'part-element t)
            (web-mode-scan-elements beg end)
            (web-mode-scan-expr-literal beg end)
            (goto-char beg)
            (let (token-beg token-end)
              (while (web-mode-part-sf "/*" end t)
                (goto-char (match-beginning 0))
                (setq token-beg (point))
                (if (not (web-mode-part-sf "*/" end t))
                    (goto-char end)
                  (setq token-end (point))
                  (put-text-property token-beg token-end 'part-token 'comment)
                  ) ;if
                ) ;while
              ) ;let
            (goto-char end)

            ) ;when
          )

         ((eq ?\/ ch-next)
          (unless (eq ?\\ ch-before)
            (setq token-type 'comment)
            (goto-char (if (< reg-end (line-end-position)) reg-end (line-end-position)))
            )
          )

         ((eq ?\* ch-next)
          (cond
           ((and (member content-type '("javascript" "jsx"))
                 (looking-back "[(=][ ]*..")
                 (looking-at-p "[^*]*/[gimy]*"))
            (setq token-type 'string)
            (re-search-forward "/[gimy]*" reg-end t))
           ;; ((unless (eq ?\\ ch-before))
           ;;  (message "la%S" (point))
           ;;  (setq token-type 'comment)
           ;;  (search-forward "*/" reg-end t)
           ;;  ) ;unless
           ((search-forward "*/" reg-end t)
            (setq token-type 'comment))
           (t
            (forward-char))
           ) ;cond
          )

         ((and (member content-type '("javascript" "jsx"))
               (eq ?\/ ch-at)
               (progn (or (bobp) (backward-char)) t)
               (looking-back "[(=][ ]*/")
               (looking-at-p ".+/"))
          (while (and continue (search-forward "/" reg-end t))
            (setq continue (or (get-text-property (1- (point)) 'block-side)
                               (eq ?\\ (char-before (1- (point))))))
            )
          (setq token-type 'string)
          (skip-chars-forward "gimy")
          )

         ) ;cond

        (when (and beg (>= reg-end (point)) token-type)
          (put-text-property beg (point) 'part-token token-type)
          (when (eq token-type 'comment)
            (put-text-property beg (1+ beg) 'syntax-table (string-to-syntax "<"))
            (when (< (point) (point-max))
              (if (< (point) (line-end-position))
                  (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">")) ;#445
                (put-text-property (point) (1+ (point)) 'syntax-table (string-to-syntax ">")) ;#377
                )
              )
            )
          )

        (when (> (point) reg-end)
          (message "reg-beg(%S) reg-end(%S) token-type(%S) point(%S)"
                   reg-beg reg-end token-type (point)))

        ) ;while

      )))

(defun web-mode-velocity-skip-forward (pos)
  (goto-char pos)
  (let ((continue t) (i 0))
    (when (eq ?\# (char-after))
      (forward-char))
    (when (member (char-after) '(?\$ ?\@))
      (forward-char))
    (when (member (char-after) '(?\!))
      (forward-char))
    (if (member (char-after) '(?\{))
        (search-forward "}")
      (setq continue t)
      (while continue
        (skip-chars-forward "a-zA-Z0-9_-")
        (when (> (setq i (1+ i)) 500)
          (message "velocity-skip-forward ** warning (%S) **" pos)
          (setq continue nil))
        (when (member (char-after) '(?\())
          (search-forward ")" nil t))
        (if (member (char-after) '(?\.))
            (forward-char)
          (setq continue nil))
        ) ;while
      ) ;if
    ))

(defun web-mode-razor-skip-forward (pos)
  (goto-char pos)
  (let ((continue t) (i 0))
    (while continue
      (skip-chars-forward " =@a-zA-Z0-9_-")
      (cond
       ((> (setq i (1+ i)) 500)
        (message "razor-skip-forward ** warning **")
        (setq continue nil))
       ((and (eq (char-after) ?\*)
             (eq (char-before) ?@))
        (when (not (search-forward "*@" nil t))
          (setq continue nil))
        )
       ((looking-at-p "@[({]")
        (forward-char)
        (when (setq pos (web-mode-closing-paren-position (point)))
          (goto-char pos))
        (forward-char)
        )
       ((and (not (eobp)) (eq ?\( (char-after)))
        (if (looking-at-p "[ \n]*<")
            (setq continue nil)
          (when (setq pos (web-mode-closing-paren-position))
            (goto-char pos))
          (forward-char)
          ) ;if
        )
       ((and (not (eobp)) (eq ?\. (char-after)))
        (forward-char))
       ((looking-at-p "[ \n]*{")
        (search-forward "{")
        (if (looking-at-p "[ \n]*<")
            (setq continue nil)
          (backward-char)
          (when (setq pos (web-mode-closing-paren-position))
            (goto-char pos))
          (forward-char)
          ) ;if
        )
       ((looking-at-p "}")
        (forward-char))
       (t
        (setq continue nil))
       ) ;cond
      ) ;while
    ))

(defun web-mode-jsx-skip-forward (reg-end)
  (let ((continue t) (pos nil) (i 0))
    (save-excursion
      (while continue
        (cond
         ((> (setq i (1+ i)) 100)
          (message "jsx-skip-forward ** warning **")
          (setq continue nil))
         ((not (web-mode-dom-rsf ">\\([ \t\n]*[;,)']\\)\\|{" reg-end))
          (setq continue nil)
          (when (string= web-mode-content-type "jsx")
            (setq pos (point-max)))
          )
         ((eq (char-before) ?\{)
          (backward-char)
          (if (web-mode-closing-paren reg-end)
              (forward-char)
            ;;(setq continue nil)
            (forward-char)
            ) ;if
          )
         (t
          (setq continue nil)
          (setq pos (match-beginning 1))
          ) ;t
         ) ;cond
        ) ;while
      ) ;save-excursion
    (when pos (goto-char pos))
    ;;(message "jsx-skip: %S" pos)
    pos))

(defun web-mode-scan-expr-literal (reg-beg reg-end)
  (let ((continue t) beg end)
    (save-excursion
      (goto-char reg-beg)
;;      (message "reg-beg=%S reg-end=%S" reg-beg reg-end)
      (while (and continue (search-forward "{" reg-end t))
        (backward-char)
        (setq beg (point)
              end (web-mode-closing-paren reg-end))
        (if (not end)
            (setq continue nil)
          (setq end (1+ end))
          ;; NOTE: keeping { and } as part-token is useful for indentation
          (put-text-property (1+ beg) (1- end) 'part-token nil)
          (put-text-property beg end 'part-expr t)
          (web-mode-part-scan (1+ beg) (1- end) "javascript")
          )
        )
      )))

;; css rule = selector(s) + declaration (properties)
(defun web-mode-css-rule-next (limit)
  (let (at-rule sel-beg sel-end dec-beg dec-end chunk)
    (skip-chars-forward "\n\t ")
    (setq sel-beg (point))
    (when (and (< (point) limit)
               (web-mode-part-rsf "[{;]" limit))
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

(defun web-mode-css-rule-current (&optional pos part-beg part-end)
  "Current CSS rule boundaries."
  (unless pos (setq pos (point)))
  (unless part-beg (setq part-beg (web-mode-part-beginning-position pos)))
  (unless part-end (setq part-end (web-mode-part-end-position pos)))
  (save-excursion
    (let (beg end)
      (goto-char pos)
      (if (not (web-mode-part-sb "{" part-beg))
          (progn
            (setq beg part-beg)
            (if (web-mode-part-sf ";" part-end)
                (setq end (1+ (point)))
              (setq end part-end))
            ) ;progn
        (setq beg (point))
        (setq end (web-mode-closing-paren-position beg part-end))
        (if end
            (setq end (1+ end))
          (setq end (line-end-position)))
;;        (message "%S >>beg%S >>end%S" pos beg end)
        (if (> pos end)

            ;;selectors
            (progn
              (goto-char pos)
              (if (web-mode-part-rsb "[};]" part-beg)
                  (setq beg (1+ (point)))
                (setq beg part-beg)
                ) ;if
              (goto-char pos)
              (if (web-mode-part-rsf "[{;]" part-end)
                  (cond
                   ((eq (char-before) ?\;)
                    (setq end (point))
                    )
                   (t
                    (setq end (web-mode-closing-paren-position (1- (point)) part-end))
                    (if end
                        (setq end (1+ end))
                      (setq end part-end))
                    )
                   ) ;cond
                (setq end part-end)
                )
              ) ;progn selectors

          ;; declaration
          (goto-char beg)
          (if (web-mode-part-rsb "[}{;]" part-beg)
              (setq beg (1+ (point)))
            (setq beg part-beg)
            ) ;if
          ) ; if > pos end
        )
;;      (message "beg(%S) end(%S)" beg end)
      (when (eq (char-after beg) ?\n)
        (setq beg (1+ beg)))
      (cons beg end)
      )))

(defun web-mode-scan-engine-comments (reg-beg reg-end tag-start tag-end)
  "Scan engine comments (mako, django)."
  (save-excursion
    (let (beg end (continue t))
      (goto-char reg-beg)
      (while (and continue
                  (< (point) reg-end)
                  (re-search-forward tag-start reg-end t))
        (goto-char (match-beginning 0))
        (setq beg (point))
        (if (not (re-search-forward tag-end reg-end t))
            (setq continue nil)
          (setq end (point))
          (remove-list-of-text-properties beg end web-mode-scan-properties)
          (add-text-properties beg end '(block-side t block-token comment))
          (put-text-property beg (1+ beg) 'block-beg 0)
          (put-text-property (1- end) end 'block-end t)
          ) ;if
        ) ;while
      )))

(defun web-mode-propertize (&optional beg end)

  (unless beg (setq beg web-mode-change-beg))
  (unless end (setq end web-mode-change-end))

;;  (message "propertize: beg(%S) end(%S)" web-mode-change-beg web-mode-change-end)

  (when (and end (> end (point-max)))
    (setq end (point-max)))

;;  (remove-text-properties beg end '(font-lock-face nil))

  (setq web-mode-change-beg nil
        web-mode-change-end nil)
  (cond

   ((or (null beg) (null end))
    ;;      (message "nothing todo")
    nil)

   ((and (member web-mode-engine '("php" "asp"))
         (get-text-property beg 'block-side)
         (get-text-property end 'block-side)
         (> beg (point-min))
         (not (eq (get-text-property (1- beg) 'block-token) 'delimiter-beg))
         (not (eq (get-text-property end 'block-token) 'delimiter-end)))
    ;;(message "invalidate block")
    (web-mode-invalidate-block-region beg end))

   ((and (or (member web-mode-content-type '("css" "jsx" "javascript"))
             (and (get-text-property beg 'part-side)
                  (get-text-property end 'part-side)
                  (> beg (point-min))
                  (get-text-property (1- beg) 'part-side)
                  (get-text-property end 'part-side))
             ))
    ;; (not (looking-back "\\*/\\|</"))
    ;; (progn
    ;;   (setq chunk (buffer-substring-no-properties beg end))
    ;;   (not (string-match-p "\\*/\\|</" chunk))
    ;;   )
    ;;(message "invalidate part (%S > %S)" beg end)
    (web-mode-invalidate-part-region beg end))

   (t
    ;;(message "%S %S" beg end)
    (web-mode-invalidate-region beg end))

   ) ;cond

  )

;; NOTE: il est important d'identifier des caractères en fin de ligne
;; web-mode-block-tokenize travaille en effet sur les fins de lignes pour
;; les commentaires de type //
(defun web-mode-invalidate-block-region (pos-beg pos-end)
  ;;  (message "pos-beg(%S) pos-end(%S)" pos-beg pos-end)
  (save-excursion
    (let (beg end code-beg code-end)
      ;;(message "invalidate-block-region: pos-beg(%S)=%S" pos-beg (get-text-property pos 'block-side))
      ;;(message "code-beg(%S) code-end(%S) pos-beg(%S) pos-end(%S)" code-beg code-end pos-beg pos-end)
      (cond
       ((not (and (setq code-beg (web-mode-block-code-beginning-position pos-beg))
                  (setq code-end (web-mode-block-code-end-position pos-beg))
                  (>= pos-beg code-beg)
                  (<= pos-end code-end)
                  (> code-end code-beg)))
        (web-mode-invalidate-region pos-beg pos-end))
       ((member web-mode-engine '("asp"))
        (goto-char pos-beg)
        (forward-line -1)
        (setq beg (line-beginning-position))
        (when (> code-beg beg)
          (setq beg code-beg))
        (goto-char pos-beg)
        (forward-line)
        (setq end (line-end-position))
        (when (< code-end end)
          (setq end code-end))
        ;; ?? pas de (web-mode-block-tokenize beg end) ?
        (cons beg end)
        ) ; asp
       (t
        (goto-char pos-beg)
        (if (web-mode-block-rsb "[;{}(][ ]*\n" code-beg)
            (setq beg (match-end 0))
          (setq beg code-beg))
        (goto-char pos-end)
        (if (web-mode-block-rsf "[;{})][ ]*\n" code-end)
            (setq end (1- (match-end 0)))
          (setq end code-end))
        (web-mode-block-tokenize beg end)
        ;;(message "beg(%S) end(%S)" beg end)
        (cons beg end)
        )
       ) ;cond
      )))

(defun web-mode-invalidate-part-region (pos-beg pos-end)
  (save-excursion
    (let (beg end part-beg part-end language)
      (if (member web-mode-content-type '("css" "javascript" "json" "jsx"))
          (setq language web-mode-content-type)
        (setq language (symbol-name (get-text-property pos-beg 'part-side))))
      (setq part-beg (web-mode-part-beginning-position pos-beg)
            part-end (web-mode-part-end-position pos-beg))
      ;;(message "language(%S) pos-beg(%S) pos-end(%S) part-beg(%S) part-end(%S)"
      ;;       language pos-beg pos-end part-beg part-end)
      (goto-char pos-beg)
      (cond
       ((not (and part-beg part-end
                  (>= pos-beg part-beg)
                  (<= pos-end part-end)
                  (> part-end part-beg)))
        (web-mode-invalidate-region pos-beg pos-end))
       ((member language '("javascript" "json" "jsx"))
        (if (web-mode-javascript-rsb "[;{}(][ ]*\n" part-beg)
            (setq beg (match-end 0))
          (setq beg part-beg))
        (goto-char pos-end)
        (if (web-mode-javascript-rsf "[;{})][ ]*\n" part-end)
            (setq end (match-end 0))
          (setq end part-end))
        (web-mode-scan-region beg end language))
       ((string= language "css")
        (let (rule1 rule2)
          (setq rule1 (web-mode-css-rule-current pos-beg))
          (setq rule2 rule1)
          (when (> pos-end (cdr rule1))
            (setq rule2 (web-mode-css-rule-current pos-end)))
          (setq beg (car rule1)
                end (cdr rule2))
          )
        (web-mode-scan-region beg end language))
       (t
        (setq beg part-beg
              end part-end)
        (web-mode-scan-region beg end language))
       ) ;cond
      )))

(defun web-mode-invalidate-region (reg-beg reg-end)
  (setq reg-beg (web-mode-invalidate-region-beginning-position reg-beg)
        reg-end (web-mode-invalidate-region-end-position reg-end))
  ;;(message "invalidate-region: reg-beg(%S) reg-end(%S)" reg-beg reg-end)
  (web-mode-scan-region reg-beg reg-end))

(defun web-mode-invalidate-region-beginning-position (pos)
  (save-excursion
    (goto-char pos)
    (when (and (bolp) (not (bobp)))
      (backward-char))
    (beginning-of-line)
    ;;(message "pos=%S %S" (point) (text-properties-at (point)))
    (setq pos (point-min))
    (let ((continue (not (bobp))))
      (while continue
        (cond
         ((bobp)
          (setq continue nil))
         ;; NOTE: Going back to the previous start tag is necessary
         ;; when inserting a part end tag (e.g. </script>).
         ;; Indeed, parts must be identified asap.
         ((and (progn (back-to-indentation) t)
               (get-text-property (point) 'tag-beg)
               (eq (get-text-property (point) 'tag-type) 'start))
          (setq pos (point)
                continue nil))
         (t
          (forward-line -1))
         ) ;cond
        ) ;while
      ;;(message "pos=%S" pos)
      pos)))

(defun web-mode-invalidate-region-end-position (pos)
  (save-excursion
    (goto-char pos)
    (setq pos (point-max))
    (let ((continue (not (eobp))))
      (while continue
        (end-of-line)
        (cond
         ((eobp)
          (setq continue nil))
         ((and (not (get-text-property (point) 'tag-type))
               (not (get-text-property (point) 'part-side))
               (not (get-text-property (point) 'block-side)))
          (setq pos (point)
                continue nil))
         (t
          (forward-line))
         ) ;cond
        ) ;while
      pos)))

(defun web-mode-buffer-scan ()
  "Scan entine buffer."
  (interactive)
  (web-mode-scan-region (point-min) (point-max)))

(provide 'web-mode-defuns)

;;; web-mode-defuns.el ends here
