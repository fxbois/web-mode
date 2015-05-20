;;; web-mode-vars.el --- variable definitions for web-mode.el
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

(defvar font-lock-beg)
(defvar font-lock-end)

(defvar web-mode-auto-pairs nil)
(defvar web-mode-block-regexp nil)
(defvar web-mode-chunk-length 64)
(defvar web-mode-column-overlays nil)
(defvar web-mode-comments-invisible nil)
(defvar web-mode-content-type "")
(defvar web-mode-inhibit-fontification nil)
(defvar web-mode-end-tag-overlay nil)
(defvar web-mode-engine nil)
(defvar web-mode-engine-attr-regexp nil)
(defvar web-mode-engine-font-lock-keywords nil)
(defvar web-mode-engine-token-regexp nil)
(defvar web-mode-expand-initial-pos nil)
(defvar web-mode-expand-previous-state "")
(defvar web-mode-font-lock-keywords '(web-mode-font-lock-highlight))
(defvar web-mode-change-beg nil)
(defvar web-mode-change-end nil)
(defvar web-mode-hook nil)
(defvar web-mode-inlay-regexp nil)
(defvar web-mode-is-scratch nil)
(defvar web-mode-jshint-errors 0)
(defvar web-mode-obarray nil)
(defvar web-mode-snippets nil)
(defvar web-mode-start-tag-overlay nil)
(defvar web-mode-minor-engine nil)
(defvar web-mode-time (current-time))

(defvar web-mode-pre-elements '("code" "pre" "textarea"))

(defvar web-mode-void-elements
  '("area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"))

(defvar web-mode-part-content-types '("css" "javascript" "json" "jsx"))

(defvar web-mode-javascript-languages '("javascript" "jsx" "ejs"))

;; NOTE: without 'syntax-table forward-word fails (bug#377)
(defvar web-mode-scan-properties
  (list 'tag-beg 'tag-end 'tag-name 'tag-type 'tag-attr 'tag-attr-end
        'part-side 'part-token 'part-element 'part-expr
        'block-side 'block-token 'block-controls 'block-beg 'block-end
        'syntax-table)
  "Text properties used for code regions/tokens and html nodes.")

(defvar web-mode-start-tag-regexp "<\\([[:alpha:]][[:alnum:]-]*\\)"
  "Regular expression for HTML/XML start tag.")

(defvar web-mode-whitespaces-regexp
  "^[ \t]\\{2,\\}$\\| \t\\|\t \\|[ \t]+$\\|^[ \n\t]+\\'\\|^[ \t]?[\n]\\{2,\\}"
  "Regular expression for whitespaces.")

(defvar web-mode-imenu-regexp-list
  '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">")
    ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 "id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">"))
  "Regexps to match imenu items (see http://web-mode.org/doc/imenu.txt)")

(defvar web-mode-indentation-params
  '(("lineup-args"    . t)
    ("lineup-calls"   . t)
    ("lineup-concats" . t)
    ))

(defvar web-mode-engines
  '(("angular"          . ("angularjs" "angular.js"))
    ("asp"              . ())
    ("aspx"             . ())
    ("blade"            . ("laravel"))
    ("cl-emb"           . ())
    ("clip"             . ())
    ("closure"          . ("soy"))
    ("ctemplate"        . ("mustache" "handlebars" "hapax" "ngtemplate" "ember"
                           "kite" "meteor" "blaze" "ractive"))
    ("django"           . ("dtl" "twig" "swig" "jinja" "erlydtl" "liquid"
                           "clabango" "selmer" "nunjucks"))
    ("dust"             . ("dustjs"))
    ("ejs"              . ())
    ("elixir"           . ())
    ("erb"              . ("eruby" "erubis"))
    ("freemarker"       . ())
    ("go"               . ("gtl"))
    ("jsp"              . ("grails"))
    ("mako"             . ())
    ("mason"            . ("poet"))
    ("lsp"              . ("lisp"))
    ("mojolicious"      . ())
    ("php"              . ())
    ("python"           . ())
    ("razor"            . ("play" "play2"))
    ("template-toolkit" . ())
    ("smarty"           . ())
    ("thymeleaf"        . ())
    ("underscore"       . ("underscore.js"))
    ("velocity"         . ("vtl" "cheetah" "ssp"))
    ("web2py"           . ()))
  "Engine name aliases")

(defvar web-mode-content-types
  '(("css"        . "\\.\\(s?css\\|css\\.erb\\)\\'")
    ("javascript" . "\\.\\(js\\|js\\.erb\\)\\'")
    ("json"       . "\\.\\(api\\|json\\|jsonld\\)\\'")
    ("jsx"        . "\\.jsx\\'")
    ("xml"        . "\\.xml\\'")
    ("html"       . "."))
  "content types")

(defvar web-mode-engine-attr-regexps
  '(("angular"   . "ng-")
    ("thymeleaf" . "th:"))
  "Engine custom attributes")

(defvar web-mode-last-enabled-feature nil)

(defvar web-mode-features
  '(("css-colorization"          . web-mode-enable-css-colorization)
    ("element-highlight"         . web-mode-enable-current-element-highlight)
    ("column-highlight"          . web-mode-enable-current-column-highlight)
    ("whitespace-fontification"  . web-mode-enable-whitespace-fontification)
    ("element-tag-fontification" . web-mode-enable-element-tag-fontification)
    ("block-face"                . web-mode-enable-block-face)
    ("part-face"                 . web-mode-enable-part-face)))

(defvar web-mode-comment-formats
  '(("java"       . "/*")
    ("javascript" . "/*")
    ("php"        . "/*")
    ))

(defvar web-mode-engine-file-regexps
  '(("asp"              . "\\.asp\\'")
    ("aspx"             . "\\.as[cp]x\\'")
    ("blade"            . "\\.blade\\.php\\'")
    ("cl-emb"           . "\\.clemb\\'")
    ("clip"             . "\\.ctml\\'")
    ("closure"          . "\\.soy\\'")
    ("ctemplate"        . "\\.\\(chtml\\|mustache\\)\\'")
    ("django"           . "\\.\\(djhtml\\|tmpl\\|dtl\\|liquid\\|j2\\)\\'")
    ("dust"             . "\\.dust\\'")
    ("elixir"           . "\\.eex\\'")
    ("ejs"              . "\\.ejs\\'")
    ("erb"              . "\\.\\(erb\\|rhtml\\|erb\\.html\\)\\'")
    ("freemarker"       . "\\.ftl\\'")
    ("go"               . "\\.go\\(html\\|tmpl\\)\\'")
    ("handlebars"       . "\\.\\(hb\\.html\\|hbs\\)\\'")
    ("jinja"            . "\\.jinja\\'")
    ("jsp"              . "\\.[gj]sp\\'")
    ("lsp"              . "\\.lsp\\'")
    ("mako"             . "\\.mako?\\'")
    ("mason"            . "\\.mas\\'")
    ("mojolicious"      . "\\.epl?\\'")
    ("php"              . "\\.\\(p[hs]p\\|ctp\\|inc\\)\\'")
    ("python"           . "\\.pml\\'")
    ("razor"            . "\\.\\(cs\\|vb\\)html\\'")
    ("smarty"           . "\\.tpl\\'")
    ("template-toolkit" . "\\.tt.?\\'")
    ("thymeleaf"        . "\\.thtml\\'")
    ("velocity"         . "\\.v\\(sl\\|tl\\|m\\)\\'")

    ("django"           . "[st]wig")
    ("razor"            . "scala")

    )
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
  '(("AElig" . 198) ("Aacute" . 193) ("Acirc" . 194) ("Agrave" . 192)
    ("Alpha" . 913) ("Aring" . 197) ("Atilde" . 195) ("Auml" . 196)
    ("Beta" . 914)
    ("Ccedil" . 199) ("Chi" . 935)
    ("Dagger" . 8225) ("Delta" . 916)
    ("ETH" . 208) ("Eacute" . 201) ("Ecirc" . 202) ("Egrave" . 200)
    ("Epsilon" . 917) ("Eta" . 919) ("Euml" . 203)
    ("Gamma" . 915)
    ("Iacute" . 205) ("Icirc" . 206) ("Igrave" . 204) ("Iota" . 921)
    ("Iuml" . 207)
    ("Kappa" . 922)
    ("Lambda" . 923)
    ("Mu" . 924)
    ("Ntilde" . 209) ("Nu" . 925)
    ("OElig" . 338) ("Oacute" . 211) ("Ocirc" . 212) ("Ograve" . 210)
    ("Omega" . 937) ("Omicron" . 927) ("Oslash" . 216) ("Otilde" . 213)
    ("Ouml" . 214)
    ("Phi" . 934) ("Pi" . 928) ("Prime" . 8243) ("Psi" . 936)
    ("Rho" . 929)
    ("Scaron" . 352) ("Sigma" . 931)
    ("THORN" . 222) ("Tau" . 932) ("Theta" . 920)
    ("UArr" . 8657) ("Uacute" . 218) ("Uacute" . 250) ("Ucirc" . 219)
    ("Ugrave" . 217)  ("Upsih" . 978)
    ("Upsilon" . 933) ("Uuml" . 220) ("Uuml" . 252)
    ("Xi" . 926)
    ("Yacute" . 221) ("Yuml" . 376)
    ("Zeta" . 918)
    ("aacute" . 225) ("acirc" . 226) ("acute" . 180) ("aelig" . 230)
    ("agrave" . 224) ("alefsym" . 8501) ("alpha" . 945) ("amp" . 38)
    ("ang" . 8736) ("apos" . 39) ("aring" . 229) ("asymp" . 8776)
    ("atilde" . 227) ("auml" . 228)
    ("bdquo" . 8222) ("beta" . 946) ("brvbar" . 166) ("bull" . 8226)
    ("cap" . 8745) ("ccedil" . 231) ("cedil" . 184) ("cent" . 162)
    ("chi" . 967) ("circ" . 710) ("clubs" . 9827) ("cong" . 8773)
    ("copy" . 169) ("crarr"  . 8629) ("cup" . 8746) ("curren" . 164)
    ("dArr" . 8659) ("dagger" . 8224) ("darr" . 8595) ("deg" . 176)
    ("delta" . 948) ("diams" . 9830) ("divide" . 247)
    ("eacute" . 233) ("ecirc"  . 234) ("egrave" . 232) ("empty" . 8709)
    ("emsp" . 8195) ("ensp" . 8194) ("epsilon" . 949) ("equiv" . 8801)
    ("eta" . 951) ("eth" . 240) ("euml" . 235) ("euro" . 8364) ("exist" . 8707)
    ("fnof" . 402) ("forall" . 8704) ("frac12" . 189) ("frac14" . 188)
    ("frac34" . 190) ("frasl" . 8260)
    ("gamma" . 947) ("ge" . 8805) ("gt" . 62)
    ("hArr" . 8660) ("harr" . 8596) ("hearts" . 9829) ("hellip" . 8230)
    ("iacute" . 237) ("icirc" . 238) ("iexcl" . 161) ("igrave" . 236)
    ("image" . 8465) ("infin" . 8734) ("int" . 8747) ("iota" . 953)
    ("iquest" . 191) ("isin" . 8712) ("iuml" . 239)
    ("kappa" . 954)
    ("lArr" . 8656) ("lambda" . 955) ("lang" . 9001) ("laquo" . 171)
    ("larr" . 8592) ("lceil" . 8968) ("ldquo" . 8220) ("le" . 8804)
    ("lfloor" . 8970) ("lowast" . 8727) ("loz" . 9674) ("lrm" . 8206)
    ("lsaquo" . 8249) ("lsquo" . 8249) ("lt" . 60)
    ("macr" . 175) ("mdash" . 8212) ("micro" . 181) ("middot" . 183)
    ("minus" . 8722) ("mu" . 956)
    ("nabla" . 8711) ("nbsp" . 160) ("ndash" . 8211) ("ne" . 8800)
    ("ni" . 8715) ("not" . 172) ("notin" . 8713) ("nsub" . 8836)
    ("ntilde" . 241) ("nu" . 957) ("oacute" . 243) ("ocirc" . 244)
    ("oelig" . 339) ("ograve" . 242) ("oline" . 8254) ("omega" . 969)
    ("omicron" . 959) ("oplus" . 8853) ("or" . 8744) ("ordf" . 170)
    ("ordm" . 186) ("oslash" . 248) ("otilde" . 245) ("otimes" . 8855)
    ("ouml" . 246)
    ("para" . 182) ("part" . 8706) ("permil" . 8240) ("perp" . 8869)
    ("phi" . 966) ("pi" . 960) ("piv" . 982) ("plusmn" . 177) ("pound" . 163)
    ("prime" . 8242) ("prod" . 8719) ("prop" . 8733) ("psi" . 968)
    ("quot" . 34)
    ("rArr" . 8658) ("radic" . 8730) ("rang" . 9002) ("raquo" . 187)
    ("rarr" . 8594) ("rceil" . 8969) ("rdquo" . 8221) ("real" . 8476)
    ("reg" . 174) ("rfloor" . 8971) ("rho" . 961) ("rlm" . 8207)
    ("rsaquo" . 8250) ("rsquo" . 8250) ("sbquo" . 8218)
    ("scaron" . 353) ("sdot" . 8901) ("sect" . 167) ("shy" . 173)
    ("sigma" . 963) ("sigmaf" . 962) ("sim" . 8764) ("spades" . 9824)
    ("sub" . 8834) ("sube" . 8838) ("sum" . 8721) ("sup" . 8835)
    ("sup1" . 185) ("sup2" . 178) ("sup3" . 179) ("supe" . 8839)
    ("szlig" . 223)
    ("tau" . 964) ("there4" . 8756) ("theta" . 952) ("thetasym" . 977)
    ("thinsp" . 8201) ("thorn" . 254) ("tilde" . 732) ("times" . 215)
    ("trade" . 8482)
    ("uarr" . 8593) ("ucirc" . 251) ("ugrave" . 249) ("uml" . 168)
    ("upsilon" . 965)
    ("weierp" . 8472)
    ("xi" . 958)
    ("yacute" . 253) ("yen" . 165) ("yuml" . 255)
    ("zeta" . 950) ("zwj" . 8205) ("zwnj" . 8204)))

;; http://webdesign.about.com/od/localization/l/blhtmlcodes-ascii.htm
(defvar web-mode-display-table
  (let ((table (make-display-table)))
    (aset table 9  (vector ?\xBB ?\t)) ;tab
    (aset table 10 (vector ?\xB6 ?\n)) ;line feed
    (aset table 32 (vector ?\xB7))
    table)
  "Display table used when switching to the whitespace visualization.")

(defvar web-mode-expanders
  '(("a/" . "<a href=\"|\"></a>")
    ("b/" . "<table><tbody><tr><td>|</td><td></td></tr></tbody></table>")
    ("c/" . "<div class=\"|\"></div>")
    ("d/" . "<div>|</div>")
    ("e/" . "<em>|</em>")
    ("f/" . "<form>|</form>")
    ("g/" . "<strong>|</strong>")
    ("h/" . "<h1>|</h1>")
    ("i/" . "<img src=\"|\" />")
    ("j/" . "<script>|</script>")
    ("l/" . "<li>|</li>")
    ("m/" . "<main>|</main>")
    ("n/" . "<input type=\"|\" />")
    ("p/" . "<p>|</p>")
    ("q/" . "<quote>|</quote>")
    ("s/" . "<span>|</span>")
    ("t/" . "<td>|</td>")
    ("u/" . "<ul><li>|</li><li></li></ul>")
    ("x/" . "<textarea>|</textarea>")
    ("2/" . "<h2>|</h2>")
    ("3/" . "<h3>|</h3>")
    ("?/" . "<?php | ?>")))

(defvar web-mode-engines-auto-pairs
  '(("angular"          . (("{{ " . " }}")))
    ("asp"              . (("<% " . " %>")))
    ("aspx"             . (("<% " . " %>")
                           ("<%=" . "%>")
                           ("<%#" . "%>")
                           ("<%$" . "%>")
                           ("<%@" . "%>")
                           ("<%:" . "%>")
                           ("<%-" . "- | --%>")))
    ("blade"            . (("{{{" . " | }}}")
                           ("{{ " . " }}")
                           ("{!!" . " | !!}")
                           ("@{{" . " | }}")
                           ("{{-" . "- | --}}")))
    ("cl-emb"           . (("<% " . " %>")
                           ("<%=" . " | %>")
                           ("<%#" . " | %>")))
    ("ctemplate"        . (("{{ " . "| }}")
                           ("{{{" . " | }}}")
                           ("{~{" . " | }}")
                           ("{{~" . "{ | }}}")
                           ("{{!" . "-- | --}}")
                           ("{{/" . "}}")
                           ("{{#" . "}}")))
    ("django"           . (("{{ " . " }}")
                           ("{% " . " %}")
                           ("{%-" . " | %}")
                           ("{# " . " #}")))
    ("elixir"           . (("<% " . " %>")
                           ("<%=" . " | %>")
                           ("<%%" . " | %>")
                           ("<%#" . " | %>")))
    ("ejs"              . (("<% " . " %>")
                           ("<%=" . "%>")
                           ("<%#" . "%>")
                           ("<%-" . "%>")))
    ("erb"              . (("<% " . " %>")
                           ("<%=" . "%>")
                           ("<%#" . "%>")
                           ("<%-" . "%>")))
    ("freemarker"       . (("<% " . " %>")
                           ("${ " . " }")
                           ("[% " . " %]")
                           ("[# " . " #]")
                           ("[#-" . "- | --]")))
    ("jsp"              . (("<% " . " %>")
                           ("<%-" . "- | %>")
                           ("<%=" . "%>")
                           ("<%!" . "%>")
                           ("<%@" . "%>")
                           ("${ " . " }")))
    ("lsp"              . (("<% " . " %>")
                           ("<%%" . " | %>")
                           ("<%#" . " | %>")))
    ("mako"             . (("<% " . " %>")
                           ("<%!" . " | %>")
                           ("${ " . " }")))
    ("mason"            . (("<% " . " %>")
                           ("<& " . " &>")))
    ("mojolicious"      . (("<% " . " %>")
                           ("<%=" . " | %>")
                           ("<%%" . " | %>")
                           ("<%#" . " | %>")))
    ("php"              . (("<?p" . "hp | ?>")
                           ("<? " . " ?>")
                           ("<?=" . "?>")))
    ("template-toolkit" . (("[% " . " %]")
                           ("[%-" . " | %]")
                           ("[%#" . " | %]")))
    ("underscore"       . (("<% " . " %>")))
    ("web2py"           . (("{{ " . " }}")
                           ("{{=" . "}}")))
    (nil                . (("<!-" . "- | -->")))
    ))

(defvar web-mode-engines-snippets
  '(("ejs" . (("for"     . "<% for (|) { %>\n\n<% } %>")
              ("if"      . "<% if (|) { %>\n\n<% } %>")
              ))
    ("erb" . (("each"    . "<% |.each do  %>\n\n<% end %>")
              ("if"      . "<% if | %>\n\n<% end %>")
              ("when"    . "<% when | %>\n\n<% end %>")
              ("unless"  . "<% unless | %>\n\n<% end %>")
              ))
    ("php" . (("if"      . "<?php if (|): ?>\n\n<?php endif; ?>")
              ("while"   . "<?php while (|): ?>\n\n<?php endwhile; ?>")
              ("for"     . "<?php for (| ; ; ): ?>\n\n<?php endfor; ?>")
              ("foreach" . "<?php foreach (| as ): ?>\n\n<?php endforeach; ?>")
              ("each"    . "<?php foreach (| as ): ?>\n\n<?php endforeach; ?>")
              ("switch"  . "<?php switch (|): ?>\n<?php case 1: ?>\n\n<?php break ;?>\n<?php case 2: ?>\n\n<?php break ;?>\n<?php endswitch;?>")
              ))
    ("django" . (("block"      . "{% block | %}\n\n{% endblock %}")
                 ("comment"    . "{% comment | %}\n\n{% endcomment %}")
                 ("cycle"      . "{% cycle | as  %}\n\n{% endcycle  %}")
                 ("filter"     . "{% filter | %}\n\n{% endfilter %}")
                 ("for"        . "{% for | in  %}\n\n{% endfor %}")
                 ("if"         . "{% if | %}\n\n{% endif %}")
                 ("ifequal"    . "{% ifequal | %}\n\n{% endifequal %}")
                 ("ifnotequal" . "{% ifnotequal | %}\n\n{% endifnotequal %}")
                 ("safe"       . "{% safe | %}\n\n{% endsafe %}")
                 ))
    ("template-toolkit" . (("if"      . "[% IF | %]\n\n[% END %]")
                           ))
    (nil . (("html5" . "<!doctype html>\n<html>\n<head>\n<title></title>\n<meta charset=\"utf-8\" />\n</head>\n<body>\n|\n</body>\n</html>")
            ("table" . "<table><tbody>\n<tr>\n<td>|</td>\n<td></td>\n</tr>\n</tbody></table>")
            ("ul"    . "<ul>\n<li>|</li>\n<li></li>\n</ul>")
            ))
    ))

(defvar web-mode-engine-token-regexps
  (list
   '("asp"         . "//\\|/\\*\\|\"\\|'")
   '("ejs"         . "//\\|/\\*\\|\"\\|'")
   '("erb"         . "\"\\|'\\|#\\|<<[-]?['\"]?\\([[:alnum:]_]+\\)['\"]?")
   '("lsp"         . "\"\\|#|\\|;")
   '("mako"        . "\"\\|'\\|#")
   '("mason"       . "\"\\|'\\|#")
   '("mojolicious" . "\"\\|'")
   '("php"         . "//\\|/\\*\\|#\\|\"\\|'\\|<<<['\"]?\\([[:alnum:]]+\\)['\"]?")
   '("python"      . "\"\\|'\\|#")
   '("web2py"      . "\"\\|'"))
  "Engine regexps used to identify tokens (strings / comments) in blocks.")

(defvar web-mode-engine-open-delimiter-regexps
  (list
   '("angular"          . "{{")
   '("asp"              . "<%\\|</?[[:alpha:]]+:[[:alpha:]]+\\|</?[[:alpha:]]+Template")
   '("aspx"             . "<%.")
   '("blade"            . "{{.\\|{!!\\|@{{\\|^[ \t]*@[[:alpha:]]")
   '("cl-emb"           . "<%")
   '("closure"          . "{.\\|/\\*\\| //")
   '("clip"             . "</?c:[[:alpha:]-]+")
   '("ctemplate"        . "[$]?{[{~].")
   '("django"           . "{[#{%]")
   '("dust"             . "{.")
   '("elixir"           . "<%.")
   '("ejs"              . "<%")
   '("erb"              . "<%\\|^%.")
   '("freemarker"       . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]\\|</?[@#]\\|\\[/?[@#].")
   '("go"               . "{{.")
   '("jsp"              . "<%\\|${\\|</?[[:alpha:]]+:[[:alpha:]]+")
   '("lsp"              . "<%")
   '("mako"             . "</?%\\|${\\|^[ \t]*%.\\|^[ \t]*##")
   '("mason"            . "</?[&%]\\|^%.")
   '("mojolicious"      . "<%\\|^[ \t]*%.")
   '("php"              . "<\\?")
   '("python"           . "<\\?")
   '("razor"            . "@.\\|^[ \t]*}")
   '("smarty"           . "{[[:alpha:]#$/*\"]")
   '("template-toolkit" . "\\[%.")
   '("underscore"       . "<%")
   '("velocity"         . "^[ \t]*#[[:alpha:]#*]\\|$[[:alpha:]!{]")
   '("web2py"           . "{{"))
  "Engine regexps used to identify blocks.")

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

(defvar web-mode-element-tag-faces
  (list
   '("h1"     . web-mode-underline-face)
   '("h2"     . web-mode-underline-face)
   '("h3"     . web-mode-underline-face)
   '("h4"     . web-mode-underline-face)
   '("title"  . web-mode-underline-face)
   '("em"     . web-mode-italic-face)
   '("strong" . web-mode-bold-face)
   ))

(defvar web-mode-element-content-faces
  (list
   '("h1"     . web-mode-underline-face)
   '("h2"     . web-mode-underline-face)
   '("h3"     . web-mode-underline-face)
   '("h4"     . web-mode-underline-face)
   '("title"  . web-mode-underline-face)
   '("em"     . web-mode-italic-face)
   '("strong" . web-mode-bold-face)
   ))

(defvar web-mode-comment-keywords
  (regexp-opt
   (append
    (cdr (assoc "comment" web-mode-extra-keywords))
    '("FIXME" "TODO" "BUG" "KLUDGE" "WORKAROUND" "OPTIMIZE" "HACK" "REFACTOR" "REVIEW"))))

(defvar web-mode-sql-queries
  (regexp-opt
   '("SELECT" "INSERT" "UPDATE" "DELETE")))

(defvar web-mode-sql-keywords
  (regexp-opt
   (append
    (cdr (assoc "sql" web-mode-extra-keywords))
    '("SELECT" "INSERT" "UPDATE" "DELETE"
      "FROM" "WHERE" "GROUP BY" "LIMIT" "HAVING" "JOIN" "LEFT" "INNER"
      "FULL" "OUTER" "VALUES" "ORDER BY" "SEPARATOR" "ASC" "DESC"
      "AND" "OR" "ON"))))

(defvar web-mode-python-constants
  (regexp-opt
   (append
    (cdr (assoc "python" web-mode-extra-constants))
    '("True" "False" "None" "__debug__" "NotImplemented" "Ellipsis"))))

(defvar web-mode-erlang-constants
  (regexp-opt
   (append
    (cdr (assoc "erlang" web-mode-extra-constants))
    '("true" "false"))))

(defvar web-mode-erlang-keywords
  (regexp-opt
   (append
    (cdr (assoc "erlang" web-mode-extra-keywords))
    '("else" "if" "do" "end"))))

(defvar web-mode-cl-emb-constants
  (regexp-opt
   '("nil" "t" "raw" "escape")))

(defvar web-mode-cl-emb-keywords
  (regexp-opt
   '("if" "else" "endif" "unless" "endunless" "var" "repeat"
     "endrepeat" "loop" "endloop" "include" "call" "with"
     "endwith" "set" "genloop" "endgenloop" "insert")))

(defvar web-mode-lsp-constants
  (regexp-opt
   '("nil" "t")))

(defvar web-mode-lsp-keywords
  (regexp-opt
   '("dolist" "let" "while" "cond" "when" "progn" "if"
     "dotimes" "unless" "lambda"
     "loop" "for" "and" "or" "in" "do" "defun")))

(defvar web-mode-php-constants
  (regexp-opt
   (append
    (cdr (assoc "php" web-mode-extra-constants))
    '("TRUE" "FALSE" "NULL" "true" "false" "null"
      "STR_PAD_LEFT" "STR_PAD_RIGHT"
      "ENT_COMPAT" "ENT_QUOTES" "ENT_NOQUOTES" "ENT_IGNORE"
      "ENT_SUBSTITUTE" "ENT_DISALLOWED" "ENT_HTML401" "ENT_XML1"
      "ENT_XHTML" "ENT_HTML5" "JSON_PRETTY_PRINT"
      "LIBXML_NOBLANKS"))))

(defvar web-mode-php-keywords
  (regexp-opt
   (append
    (cdr (assoc "php" web-mode-extra-keywords))
    '("and" "array" "as" "break"
      "callable" "case" "catch"  "catch all" "class" "const" "continue"
      "default" "die" "do" "echo" "else" "elseif" "empty"
      "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit" "extends"
      "finally" "for" "foreach" "function" "global" "goto"
      "if" "include" "include_once" "instanceof" "interface" "isset"
      "list" "next" "new" "or" "private" "protected" "public"
      "require" "require_once" "return" "static" "switch" "try" "throw"
      "unset" "use" "var" "when" "while" "xor" "yield"))))

(defvar web-mode-php-types
  (eval-when-compile
    (regexp-opt
     '("array" "bool" "boolean" "char" "const" "double" "float"
       "int" "integer" "long" "mixed" "object" "real" "string"))))

(defvar web-mode-css-at-rules
  (eval-when-compile
    (regexp-opt
     '("charset" "import" "media" "page" "font-face"
       "namespace" "supports" "document"
       "keyframes" "-moz-keyframes" "-webkit-keyframes"
       "mixin"))))

(defvar web-mode-css-pseudo-classes
  (eval-when-compile
    (regexp-opt
     '("active" "after" "before" "checked" "disabled" "empty" "enabled"
       "first" "first-child" "first-letter" "first-line" "first-of-type" "focus"
       "hover" "lang" "last-child" "last-of-type" "left" "link"
       "not" "nth-child" "nth-last-child" "nth-last-of-type" "nth-of-type"
       "only-child" "only-of-type"
       "right" "root" "selection" "target" "visited"))))

(defvar web-mode-python-keywords
  (regexp-opt
   (append
    (cdr (assoc "python" web-mode-extra-keywords))
    '("and" "as" "assert" "break" "class" "continue" "def" "del"
      "elif" "else" "except" "finally" "for" "from" "global"
      "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
      "raise" "return" "try" "while" "with" "yield"))))

(defvar web-mode-jsp-keywords
  (regexp-opt
   (append
    (cdr (assoc "jsp" web-mode-extra-keywords))
    '("case" "catch" "do" "else" "end" "false" "for" "function"
      "if" "in" "include"
      "new" "package" "page" "private" "protected" "public"
      "return" "tag" "taglib" "throw" "throws" "true" "try" "void" "while"))))

(defvar web-mode-erb-keywords
  (regexp-opt
   (append
    (cdr (assoc "erb" web-mode-extra-keywords))
    '("alias" "and" "begin" "break" "case" "class" "def" "defined?" "do"
      "elsif" "else" "end" "ensure" "fail" "for" "if" "in"
      "module" "next" "not" "or" "redo" "rescue" "retry" "return"
      "then" "super" "unless" "undef" "until" "when" "while" "yield"
      "__ENCODING__" "__FILE__" "__LINE__"))))

(defvar web-mode-mason-keywords
  (regexp-opt
   (append
    (cdr (assoc "mason" web-mode-extra-keywords))
    '("and" "base" "close" "die" "each" "else" "elsif" "eval" "exists"
      "foreach" "grep" "if" "length" "local" "my" "next" "open" "or"
      "package" "pop" "ref" "return" "stat" "sub" "tie"
      "undef" "unless" "use" "while"))))

(defvar web-mode-erb-builtins
  (regexp-opt
   (append
    (cdr (assoc "erb" web-mode-extra-builtins))

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

      ))))

(defvar web-mode-asp-constants
  (regexp-opt
   (append
    (cdr (assoc "asp" web-mode-extra-constants))
    '("adAsyncExecute" "adAsyncFetch" "adAsyncFetchNonBlocking" "adCmdFile"
      "adCmdStoredProc" "adCmdTable" "adCmdTableDirect" "adCmdText" "adCmdUnknown"
      "adCmdUnspecified" "adExecuteNoRecords" "adExecuteRecord" "adExecuteStream"
      "adLockBatchOptimistic" "adLockOptimistic" "adLockPessimistic"
      "adLockReadOnly" "adLockUnspecified" "adOpenDynamic" "adOpenForwardOnly"
      "adOpenKeyset" "adOpenStatic" "adOpenUnspecified" "adOptionUnspecified"
      "Empty" "Nothing" "Null" "True" "False"
      "vbBack" "vbCr" "vbCrLf" "vbFormFeed" "vbLf" "vbNewLine" "vbNullChar"
      "vbNullString" "vbObjectError" "vbScript" "vbTab" "vbVerticalTab"))))

(defvar web-mode-asp-keywords
  (regexp-opt
   (append
    (cdr (assoc "asp" web-mode-extra-keywords))
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
      "Weekday" "WeekdayName" "Wend" "With" "While" "Year"))))

(defvar web-mode-asp-types
  (regexp-opt
   (append
    (cdr (assoc "asp" web-mode-extra-types))
    '("Application" "ASPError" "Request" "Response" "Server" "Session"))))

(defvar web-mode-aspx-keywords
  (regexp-opt
   (append
    (cdr (assoc "aspx" web-mode-extra-keywords))
    '("case" "catch" "do" "else" "end" "for" "foreach" "function"
      "if" "in" "include" "new" "package" "page" "return"
      "tag" "throw" "throws" "try" "while"))))

(defvar web-mode-smarty-keywords
  (regexp-opt '("as")))

(defvar web-mode-velocity-keywords
  (eval-when-compile
    (regexp-opt '("in"))))

(defvar web-mode-freemarker-keywords
  (eval-when-compile
    (regexp-opt '("as" "list"))))

(defvar web-mode-go-keywords
  (eval-when-compile
    (regexp-opt
     '("define" "else" "end" "if" "pipeline" "range" "template" "with"))))

(defvar web-mode-go-functions
  (eval-when-compile
    (regexp-opt
     '("and" "call" "html" "index" "js" "len" "not" "or"
       "print" "printf" "println" "urlquery"))))

(defvar web-mode-closure-keywords
  (eval-when-compile
    (regexp-opt '("in" "and" "not" "or"))))

(defvar web-mode-django-control-blocks
  '("assets" "autoescape"
    "block" "blocktrans"
    "cache" "call" "capture" "comment"
    "draw"
    "embed"
    "filter" "for" "foreach" "form"
    "if" "ifchanged" "ifequal" "ifnotequal"
    "macro"
    "random" "raw"
    "safe" "sandbox" "spaceless"
    "tablerow"
    "unless"
    "verbatim"
    "with"

    "endassets" "endautoescape"
    "endblock" "endblocktrans"
    "endcache" "endcall" "endcapture" "endcomment"
    "draw"
    "endembed"
    "endfilter" "endfor" "endforeach" "endform"
    "endif" "endifchanged" "endifequal" "endifnotequal"
    "endmacro"
    "endrandom" "endraw"
    "endsafe" "endsandbox"  "endspaceless"
    "endtablerow"
    "endunless"
    "endverbatim"
    "endwith"

    ;; "set" "endset" ;#504

    "csrf_token" "cycle" "debug"
    "elif" "else" "elseif" "elsif" "empty" "extends"
    "firstof" "include" "load" "lorem" "now" "regroup" "ssi"
    "trans" "templatetag" "url" "widthratio"

    ))

(defvar web-mode-django-control-blocks-regexp
  (regexp-opt web-mode-django-control-blocks t))

(defvar web-mode-django-keywords
  (eval-when-compile
    (regexp-opt
     '("and" "as" "assign"
       "break"
       "cache" "call" "case" "context" "continue"
       "do"
       "flush" "from"
       "ignore" "import" "in" "is"
       "layout" "load"
       "missing"
       "none" "not"
       "or"
       "pluralize"
       "random"
       "set" ;#504
       "unless" "use"
       "var"
       ))))

(defvar web-mode-django-types
  (eval-when-compile
    (regexp-opt '("null" "false" "true"))))

(defvar web-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib"
       "Assembly" "Control" "Implements" "Import"
       "Master" "OutputCache" "Page" "Reference" "Register"))))

(defvar web-mode-template-toolkit-keywords
  (regexp-opt
   '("block" "call" "case" "catch" "clear" "default" "do"
     "else" "elsif" "end" "filter" "final" "for"
     "foreach" "get" "if" "in" "include" "insert" "is" "last"
     "macro" "meta" "or" "perl" "process" "rawperl" "return"
     "set" "stop" "switch" "tags" "throw" "try"
     "unless" "use" "while" "wrapper")))

(defvar web-mode-perl-keywords
  (regexp-opt
   '("__DATA__" "__END__" "__FILE__" "__LINE__" "__PACKAGE__"
     "and" "cmp" "continue" "CORE" "do" "else" "elsif" "eq" "exp"
     "for" "foreach" "ge" "gt" "if" "le" "lock" "lt" "m" "ne" "no"
     "or" "package" "q" "qq" "qr" "qw" "qx" "s" "sub"
     "tr" "unless" "until" "while" "xor" "y"
     "my" "use" "print" "say")))

(defvar web-mode-javascript-keywords
  (regexp-opt
   (append
    (cdr (assoc "javascript" web-mode-extra-keywords))
    '("break" "case" "catch" "class" "const" "continue"
      "debugger" "default" "delete" "do" "else" "enum" "eval"
      "export" "extends" "finally" "for" "function" "if"
      "implements" "import" "in" "instanceof" "interface" "let"
      "new" "package" "private" "protected" "public"
      "return" "static" "super" "switch" "throw"
      "try" "typeof" "var" "void" "while" "with" "yield"))))

(defvar web-mode-javascript-constants
  (regexp-opt
   '("false" "null" "undefined" "Infinity" "NaN" "true" "arguments" "this")))

(defvar web-mode-razor-keywords
  (regexp-opt
   (append
    (cdr (assoc "razor" web-mode-extra-keywords))
    '("false" "true" "foreach" "if" "else" "in" "var" "for" "display"
      "match" "case"
      "Html"))))

(defvar web-mode-selector-font-lock-keywords
  (list
   '("$[[:alnum:]-]+" 0 'web-mode-css-variable-face)
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>")
         '(0 'web-mode-css-at-rule-face))
   '("\\<\\(all\|braille\\|embossed\\|handheld\\|print\\|projection\\|screen\\|speech\\|tty\\|tv\\|and\\|or\\)\\>"
     1 'web-mode-keyword-face)
   '("[^,]+" 0 'web-mode-css-selector-face)
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\(([^)]*)\\)?")
         '(0 'web-mode-css-pseudo-class-face t t))
   ))

(defvar web-mode-declaration-font-lock-keywords
  (list
   '("--[[:alnum:]-]+" 0 'web-mode-css-variable-face)
   '("$[[:alnum:]-]+" 0 'web-mode-css-variable-face)
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>")
         '(1 'web-mode-css-at-rule-face))
   '("\\([[:alpha:]-]+\\)[ ]?:" 0 'web-mode-css-property-name-face)
   '("\\([[:alpha:]-]+\\)[ ]?(" 1 'web-mode-css-function-face)
   '("#[[:alnum:]]\\{1,6\\}" 0 'web-mode-css-color-face t t)
   '("![ ]?important" 0 'web-mode-css-priority-face t t)
   '("\\([^,]+\\)[ ]+{" 1 'web-mode-css-selector-face)
   '("'[^']*'\\|\"[^\"]*\"" 0 'web-mode-string-face t t)
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
   '("@\\([[:alnum:]_]+\\)\\>" 0 'web-mode-keyword-face)
   (cons (concat "\\<\\(" web-mode-javascript-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-javascript-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("\\<\\(new\\|instanceof\\|class\\) \\([[:alnum:]_.]+\\)\\>" 2 'web-mode-type-face)
   '("\\<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'web-mode-function-name-face)
   '("\\<function[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:]_]+\\)([^)]*)[ ]*{" 1 'web-mode-function-name-face)
   '("\\<\\(var\\|let\\|const\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-variable-name-face)
   '("\\<\\(function\\)[ ]*("
     (1 'web-mode-keyword-face)
     ("\\([[:alnum:]_]+\\)\\([ ]*=[^,)]*\\)?[,)]" nil nil (1 'web-mode-variable-name-face)))
   '("\\([[:alnum:]_]+\\):" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-html-tag-font-lock-keywords
  (list
   '("\\(</?\\)\\([[:alnum:]]+\\)"
     (1 'web-mode-html-tag-bracket-face)
     (2 'web-mode-html-tag-face))
   '("\"[^\"]*\"" 0 'web-mode-html-attr-value-face)
   '("\\([[:alnum:]]+\\)" 1 'web-mode-html-attr-name-face)
   '("/?>" 0 'web-mode-html-tag-bracket-face)
  ))

(defvar web-mode-dust-font-lock-keywords
  (list
   '("{[#:/?@><+^]\\([[:alpha:]_.]+\\)" 1 'web-mode-block-control-face)
   '(":\\([[:alpha:]]+\\)" 1 'web-mode-keyword-face)
   '("\\<\\([[:alnum:]_]+=\\)\\(\"[^\"]*\"\\|[[:alnum:]_]*\\)"
     (1 'web-mode-block-attr-name-face)
     (2 'web-mode-block-attr-value-face))
   '("\\\([[:alnum:]_.]+\\)" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-template-toolkit-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-template-toolkit-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("\\\([[:alpha:]][[:alnum:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("\\\([[:alpha:]][[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-smarty-font-lock-keywords
  (list
   (cons (concat "[ ]\\(" web-mode-smarty-keywords "\\)[ ]")
         '(1 'web-mode-keyword-face))
   '("{/?\\([[:alpha:]_]+\\)" 1 'web-mode-block-control-face)
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
   '("#\\([[:alpha:]_]+\\)\\>" (1 'web-mode-block-control-face))
   (cons (concat "[ ]\\(" web-mode-velocity-keywords "\\)[ ]") '(1 'web-mode-keyword-face t t))
   '("#macro([ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-function-name-face)
   '("[.]\\([[:alnum:]_-]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\($[!]?[{]?\\)\\([[:alnum:]_-]+\\)[}]?" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-mako-tag-font-lock-keywords
  (list
   '("</?%\\([[:alpha:]:]+\\)" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-mako-block-font-lock-keywords
  (list
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   (cons (concat "\\<\\(" web-mode-python-constants "\\)\\>")
         '(1 'web-mode-constant-face))
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(endfor\\|endif\\|endwhile\\)\\>")
         '(1 'web-mode-keyword-face))
   ))

(defvar web-mode-web2py-font-lock-keywords
  (list
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   (cons (concat "\\<\\(" web-mode-python-constants "\\)\\>")
         '(1 'web-mode-constant-face))
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(block\\|extend\\|super\\|end\\|include\\)\\>")
         '(1 'web-mode-keyword-face))
   ))

(defvar web-mode-django-expr-font-lock-keywords
  (list
   '("|[ ]?\\([[:alpha:]_]+\\)\\>" 1 'web-mode-filter-face)
   (cons (concat "\\<\\(" web-mode-django-types "\\)\\>")
         '(1 'web-mode-type-face))
   '("\\<\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_]+" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-django-code-font-lock-keywords
  (list
   (cons (concat "{%[ ]*\\(" web-mode-django-control-blocks-regexp "\\)\\>")
         '(1 'web-mode-block-control-face))
   '("{%[ ]*\\(end[[:alpha:]]+\\)\\>" 1 'web-mode-block-control-face) ;#504
   (cons (concat "\\<\\(" web-mode-django-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-django-types "\\)\\>")
         '(1 'web-mode-type-face))
   '("|[ ]?\\([[:alpha:]_]+\\)\\>" 1 'web-mode-function-call-face)
   '("\\<\\([[:alpha:]_]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_.]+" 0 'web-mode-variable-name-face)
   '("[[:alnum:]_]+\\([.][[:alnum:]_]+\\)+" 0 'web-mode-variable-name-face t t)
   ))

(defvar web-mode-ctemplate-font-lock-keywords
  (list
   '("{[~]?{[#/>]?[ ]*\\([[:alnum:]_-]+\\)" 1 'web-mode-block-control-face)
   '("[ \t]+\\([[:alnum:]_]+\\)=\\([[:alnum:]_.]+\\|\"[^\"]+\"\\)"
     (1 'web-mode-block-attr-name-face)
     (2 'web-mode-block-attr-value-face))
   '("\"[^\"]+\"" 0 'web-mode-block-string-face)
   ))

(defvar web-mode-razor-font-lock-keywords
  (list
   '("@\\([[:alnum:]_.]+\\)[ ]*[({]" 1 'web-mode-block-control-face)
   (cons (concat "\\<\\(" web-mode-razor-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("@\\([[:alnum:]_.]+\\)" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-closure-font-lock-keywords
  (list
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
   '("{{[ ]*\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   (cons (concat "\\<\\(" web-mode-go-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-go-functions "\\)\\>")
         '(1 'web-mode-function-call-face))
   '("[$.]\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face t t)
   ))

(defvar web-mode-expression-font-lock-keywords
  (list
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-angular-font-lock-keywords
  (list
   '("[[:alpha:]_]" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-underscore-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-javascript-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("\\<\\(_\.[[:alpha:]]+\\)(" 1 'web-mode-function-call-face)
   '("\\<new \\([[:alnum:]_.]+\\)\\>" 1 'web-mode-type-face)
   '("\\<\\([[:alnum:]_]+\\):[ ]*function[ ]*(" 1 'web-mode-function-name-face)
   '("\\<\\(var\\)\\>[ ]+\\([[:alnum:]_]+\\)"
     (1 'web-mode-keyword-face)
     (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-engine-tag-font-lock-keywords
  (list
   '("</?\\([[:alpha:]]+\\(?:Template\\|:[[:alpha:]-]+\\)\\)" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]-]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   '("\\<\\([[:alpha:]-]+=\\)\\('[^']*\'\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-jsp-font-lock-keywords
  (list
   '("\\(throws\\|new\\|extends\\)[ ]+\\([[:alnum:].]+\\)" 2 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[{[:alpha:]]+" 1 'web-mode-type-face)
   ))

(defvar web-mode-asp-font-lock-keywords
  (list
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
   (cons (concat "\\<\\(" web-mode-aspx-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
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
   '("\\[/?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>")
         '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-freemarker-font-lock-keywords
  (list
   '("</?[#@]\\([[:alpha:]_.]*\\)" 1 'web-mode-block-control-face)
   '("#\\(macro\\|function\\) \\([[:alpha:]]+\\)" 2 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-freemarker-keywords "\\)\\>") '(1 'web-mode-keyword-face))
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alpha:]]\\([[:alnum:]_]+\\)?" 0 'web-mode-variable-name-face)
   ))

(defvar web-mode-directive-font-lock-keywords
  (list
   '("<%@[ ]*\\([[:alpha:]]+\\)[ ]+" 1 'web-mode-block-control-face)
   '("\\<\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)"
     (1 'web-mode-block-attr-name-face t t)
     (2 'web-mode-block-attr-value-face t t))
   ))

(defvar web-mode-erb-font-lock-keywords
  (list
   '("[^:]\\(:[[:alnum:]_]+\\)" 1 'web-mode-symbol-face)
   '("\\([[:alnum:]_]+:\\)[ ]+" 1 'web-mode-symbol-face)
   (cons (concat "\\<\\(" web-mode-erb-builtins "\\)\\>")
         '(0 'web-mode-builtin-face))
   (cons (concat "\\<\\(" web-mode-erb-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("\\<\\(self\\|true\\|false\\|nil\\)\\>" 0 'web-mode-variable-name-face)
   '("[@$]@?\\([[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   '("class[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-type-face)
   '("def[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\(?:\\_<\\|::\\)\\([A-Z]+[[:alnum:]_]+\\)" 1 (unless (eq (char-after) ?\() 'web-mode-type-face))
   '("/[^/]+/" 0 'web-mode-string-face)
   ))

(defvar web-mode-ejs-font-lock-keywords
  web-mode-javascript-font-lock-keywords)

(defvar web-mode-python-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-python-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   ))

(defvar web-mode-erlang-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-erlang-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-erlang-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("@\\([[:alnum:]_]+\\)" 0 'web-mode-variable-name-face)
   '("[ ]\\(:[[:alnum:]-_]+\\)" 1 'web-mode-symbol-face)
   ))

(defvar web-mode-mason-code-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-mason-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("sub[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
;;   '(" | \\([hun]+\\) " 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\([@]\\)\\([[:alnum:]#_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("\\<\\([$%]\\)\\([[:alnum:]@#_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   '("{\\([[:alnum:]_]+\\)}" 1 'web-mode-variable-name-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("->[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("\\(?:method\\|def\\) \\([[:alnum:]._]+\\)" 1 'web-mode-function-name-face)
   '("|[ ]*\\([[:alnum:],]+\\)[ ]*%>" 1 'web-mode-filter-face)
   ))

(defvar web-mode-mason-block-font-lock-keywords
  (list
   '("<[/]?%\\([[:alpha:]]+\\)" 1 'web-mode-block-control-face)
   '("[[:alpha:]]" 0 'web-mode-block-attr-value-face)
   ))

(defvar web-mode-mojolicious-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-perl-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   '("\\<\\(begin\\|end\\)\\>" 1 'web-mode-constant-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-lsp-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-lsp-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-lsp-constants "\\)\\>")
         '(1 'web-mode-constant-face))
   '("[ ]\\(:[[:alnum:]-_]+\\)" 1 'web-mode-symbol-face)
   '("(defun \\([[:alnum:]-:]+\\)" 1 'web-mode-function-name-face)
   '("(defvar \\([[:alnum:]-:]+\\)" 1 'web-mode-variable-name-face)
   ))

(defvar web-mode-cl-emb-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-cl-emb-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "\\<\\(" web-mode-cl-emb-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("\\(@\\)" 1 'web-mode-function-call-face)
   (list (concat "\\(@" web-mode-cl-emb-keywords "\\)[ ]+\\([[:alnum:]_]+\\)")
         '(1 'web-mode-keyword-face)
         '(2 'web-mode-variable-name-face))
   ))

(defvar web-mode-php-font-lock-keywords
  (list
   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>")
         '(0 'web-mode-keyword-face))
   (cons (concat "(\\<\\(" web-mode-php-types "\\)\\>")
         '(1 'web-mode-type-face))
   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>")
         '(0 'web-mode-constant-face))
   '("function[ ]+\\([[:alnum:]_]+\\)" 1 'web-mode-function-name-face)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-call-face)
   '("[[:alnum:]_][ ]?::[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-constant-face)
   '("->[ ]?\\([[:alnum:]_]+\\)" 1 'web-mode-variable-name-face)
   '("\\<\\([[:alnum:]_]+\\)[ ]?::" 1 'web-mode-type-face)
   '("\\<\\(instanceof\\|class\\|extends\\|new\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-type-face)
   '("\\<\\([$]\\)\\([[:alnum:]_]*\\)" (1 nil) (2 'web-mode-variable-name-face))
   ))

(defvar web-mode-latex-font-lock-keywords
  (list
   '("[[:alnum:]_]+" 0 'web-mode-function-name-face t t)
   ))

(defvar web-mode-blade-font-lock-keywords
  (append
   (list
    '("@\\([[:alpha:]_]+\\)" (1 'web-mode-block-control-face)))
   web-mode-php-font-lock-keywords))

(defvar web-mode-engines-font-lock-keywords
  '(("angular"          . web-mode-angular-font-lock-keywords)
;;    ("asp"              . web-mode-asp-font-lock-keywords)
    ("blade"            . web-mode-blade-font-lock-keywords)
    ("cl-emb"           . web-mode-cl-emb-font-lock-keywords)
    ("closure"          . web-mode-closure-font-lock-keywords)
    ("ctemplate"        . web-mode-ctemplate-font-lock-keywords)
    ("dust"             . web-mode-dust-font-lock-keywords)
    ("elixir"           . web-mode-erlang-font-lock-keywords)
    ("ejs"              . web-mode-ejs-font-lock-keywords)
    ("erb"              . web-mode-erb-font-lock-keywords)
    ("go"               . web-mode-go-font-lock-keywords)
    ("lsp"              . web-mode-lsp-font-lock-keywords)
;;    ("mason"            . web-mode-mason-font-lock-keywords)
    ("mojolicious"      . web-mode-mojolicious-font-lock-keywords)
    ("php"              . web-mode-php-font-lock-keywords)
    ("python"           . web-mode-python-font-lock-keywords)
    ("razor"            . web-mode-razor-font-lock-keywords)
    ("smarty"           . web-mode-smarty-font-lock-keywords)
    ("template-toolkit" . web-mode-template-toolkit-font-lock-keywords)
    ("underscore"       . web-mode-underscore-font-lock-keywords)
    ("web2py"           . web-mode-web2py-font-lock-keywords)
    ("velocity"         . web-mode-velocity-font-lock-keywords))
  "Engines font-lock keywords")

(defvar web-mode-before-auto-complete-hooks nil
  "List of functions to run before triggering the auto-complete library.

Auto-complete sources will sometimes need some tweaking to work
nicely with web-mode. This hook gives users the chance to adjust
the environment as needed for ac-sources, right before they're used.")

(defvar web-mode-ac-sources-alist nil
  "alist mapping language names to ac-sources for that language.")

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table used to reveal whitespaces.")

(defvar web-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [menu-bar wm]             (cons "Web-Mode" (make-sparse-keymap)))
    (define-key map [menu-bar wm dom]         (cons "Dom" (make-sparse-keymap)))
    (define-key map [menu-bar wm blk]         (cons "Block" (make-sparse-keymap)))
    (define-key map [menu-bar wm attr]        (cons "Html Attr" (make-sparse-keymap)))
    (define-key map [menu-bar wm tag]         (cons "Html Tag" (make-sparse-keymap)))
    (define-key map [menu-bar wm elt]         (cons "Html Element" (make-sparse-keymap)))

    (define-key map [menu-bar wm sep-1]       '(menu-item "--"))

    (define-key map [menu-bar wm dom dom-xpa] '(menu-item "XPath" web-mode-dom-xpath))
    (define-key map [menu-bar wm dom dom-tra] '(menu-item "Traverse" web-mode-dom-traverse))
    (define-key map [menu-bar wm dom dom-err] '(menu-item "Show error(s)" web-mode-dom-errors-show))
    (define-key map [menu-bar wm dom dom-ent] '(menu-item "Replace html entities" web-mode-dom-entities-replace))
    (define-key map [menu-bar wm dom dom-quo] '(menu-item "Replace dumb quotes" web-mode-dom-quotes-replace))
    (define-key map [menu-bar wm dom dom-apo] '(menu-item "Replace apostrophes" web-mode-dom-apostrophes-replace))
    (define-key map [menu-bar wm dom dom-nor] '(menu-item "Normalise" web-mode-dom-normalize))

    (define-key map [menu-bar wm blk blk-sel] '(menu-item "Select" web-mode-block-select))
    (define-key map [menu-bar wm blk blk-pre] '(menu-item "Previous" web-mode-block-previous))
    (define-key map [menu-bar wm blk blk-nex] '(menu-item "Next" web-mode-block-next))
    (define-key map [menu-bar wm blk blk-kil] '(menu-item "Kill" web-mode-block-kill))
    (define-key map [menu-bar wm blk blk-end] '(menu-item "End" web-mode-block-end))
    (define-key map [menu-bar wm blk blk-clo] '(menu-item "Close" web-mode-block-close))
    (define-key map [menu-bar wm blk blk-beg] '(menu-item "Beginning" web-mode-block-beginning))

    (define-key map [menu-bar wm attr attr-ins] '(menu-item "Insert" web-mode-attribute-insert))
    (define-key map [menu-bar wm attr attr-end] '(menu-item "End" web-mode-attribute-end))
    (define-key map [menu-bar wm attr attr-beg] '(menu-item "Beginning" web-mode-attribute-beginning))
    (define-key map [menu-bar wm attr attr-sel] '(menu-item "Select" web-mode-attribute-select))
    (define-key map [menu-bar wm attr attr-kil] '(menu-item "Kill" web-mode-attribute-kill))
    (define-key map [menu-bar wm attr attr-nex] '(menu-item "Next" web-mode-attribute-next))
    (define-key map [menu-bar wm attr attr-pre] '(menu-item "Previous" web-mode-attribute-previous))
    (define-key map [menu-bar wm attr attr-tra] '(menu-item "Transpose" web-mode-attribute-transpose))

    (define-key map [menu-bar wm tag tag-beg] '(menu-item "Sort Attributes" web-mode-tag-attributes-sort))
    (define-key map [menu-bar wm tag tag-sel] '(menu-item "Select" web-mode-tag-select))
    (define-key map [menu-bar wm tag tag-pre] '(menu-item "Previous" web-mode-tag-previous))
    (define-key map [menu-bar wm tag tag-nex] '(menu-item "Next" web-mode-tag-next))
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
    (define-key map [menu-bar wm elt elt-ins] '(menu-item "Insert" web-mode-element-insert))
    (define-key map [menu-bar wm elt elt-dup] '(menu-item "Clone" web-mode-element-clone))
    (define-key map [menu-bar wm elt elt-cfo] '(menu-item "Children fold" web-mode-element-children-fold-or-unfold))
    (define-key map [menu-bar wm elt elt-chi] '(menu-item "Child" web-mode-element-child))
    (define-key map [menu-bar wm elt elt-beg] '(menu-item "Beginning" web-mode-element-beginning))

    (define-key map [menu-bar wm fol]         '(menu-item "Fold/Unfold" web-mode-fold-or-unfold))
    (define-key map [menu-bar wm hig]         '(menu-item "Highlight buffer" web-mode-buffer-highlight))
    (define-key map [menu-bar wm ind]         '(menu-item "Indent buffer" web-mode-buffer-indent))
    (define-key map [menu-bar wm nav]         '(menu-item "Tag/Block navigation" web-mode-navigate))
    (define-key map [menu-bar wm exp]         '(menu-item "Mark and Expand" web-mode-mark-and-expand))
    (define-key map [menu-bar wm spa]         '(menu-item "Toggle whitespaces" web-mode-whitespaces-show))
    (define-key map [menu-bar wm sni]         '(menu-item "Insert snippet" web-mode-snippet-insert))

    ;;--------------------------------------------------------------------------
    ;; "C-c <LETTER>" are reserved for users

    (define-key map (kbd "C-c C-a b") 'web-mode-attribute-beginning)
    (define-key map (kbd "C-c C-a e") 'web-mode-attribute-end)
    (define-key map (kbd "C-c C-a i") 'web-mode-attribute-insert)
    (define-key map (kbd "C-c C-a n") 'web-mode-attribute-next)
    (define-key map (kbd "C-c C-a s") 'web-mode-attribute-select)
    (define-key map (kbd "C-c C-a k") 'web-mode-attribute-kill)
    (define-key map (kbd "C-c C-a p") 'web-mode-attribute-previous)
    (define-key map (kbd "C-c C-a t") 'web-mode-attribute-transpose)

    (define-key map (kbd "C-c C-b b") 'web-mode-block-beginning)
    (define-key map (kbd "C-c C-b c") 'web-mode-block-close)
    (define-key map (kbd "C-c C-b e") 'web-mode-block-end)
    (define-key map (kbd "C-c C-b k") 'web-mode-block-kill)
    (define-key map (kbd "C-c C-b n") 'web-mode-block-next)
    (define-key map (kbd "C-c C-b p") 'web-mode-block-previous)
    (define-key map (kbd "C-c C-b s") 'web-mode-block-select)

    (define-key map (kbd "C-c C-d a") 'web-mode-dom-apostrophes-replace)
    (define-key map (kbd "C-c C-d d") 'web-mode-dom-errors-show)
    (define-key map (kbd "C-c C-d e") 'web-mode-dom-entities-replace)
    (define-key map (kbd "C-c C-d n") 'web-mode-dom-normalize)
    (define-key map (kbd "C-c C-d q") 'web-mode-dom-quotes-replace)
    (define-key map (kbd "C-c C-d t") 'web-mode-dom-traverse)
    (define-key map (kbd "C-c C-d x") 'web-mode-dom-xpath)

    (define-key map (kbd "C-c C-e /") 'web-mode-element-close)
    (define-key map (kbd "C-c C-e a") 'web-mode-element-content-select)
    (define-key map (kbd "C-c C-e b") 'web-mode-element-beginning)
    (define-key map (kbd "C-c C-e c") 'web-mode-element-clone)
    (define-key map (kbd "C-c C-e d") 'web-mode-element-child)
    (define-key map (kbd "C-c C-e e") 'web-mode-element-end)
    (define-key map (kbd "C-c C-e f") 'web-mode-element-children-fold-or-unfold)
    (define-key map (kbd "C-c C-e i") 'web-mode-element-insert)
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

    (define-key map (kbd "C-c C-t a") 'web-mode-tag-attributes-sort)
    (define-key map (kbd "C-c C-t b") 'web-mode-tag-beginning)
    (define-key map (kbd "C-c C-t e") 'web-mode-tag-end)
    (define-key map (kbd "C-c C-t m") 'web-mode-tag-match)
    (define-key map (kbd "C-c C-t n") 'web-mode-tag-next)
    (define-key map (kbd "C-c C-t p") 'web-mode-tag-previous)
    (define-key map (kbd "C-c C-t s") 'web-mode-tag-select)

    ;;--------------------------------------------------------------------------

    (define-key map (kbd "M-;")       'web-mode-comment-or-uncomment)

    ;;C-c C-a : attribute
    ;;C-c C-b : block
    ;;C-c C-d : dom
    ;;C-c C-e : element
    (define-key map (kbd "C-c C-f")   'web-mode-fold-or-unfold)
    (define-key map (kbd "C-c C-h")   'web-mode-buffer-highlight)
    (define-key map (kbd "C-c C-i")   'web-mode-buffer-indent)
    (define-key map (kbd "C-c C-j")   'web-mode-jshint)
    (define-key map (kbd "C-c C-m")   'web-mode-mark-and-expand)
    (define-key map (kbd "C-c C-n")   'web-mode-navigate)
    (define-key map (kbd "C-c C-r")   'web-mode-reload)
    (define-key map (kbd "C-c C-s")   'web-mode-snippet-insert)
    ;;C-c C-t : tag
    (define-key map (kbd "C-c C-w")   'web-mode-whitespaces-show)

    map)
  "Keymap for `web-mode'.")

(provide 'web-mode-vars)

;;; web-mode-vars.el ends here
