<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html><head>
<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
<!-- Created by htmlize-1.16 in css mode. -->

  
    <title>Couleurs.el</title>
    <style type="text/css">
    <!--
      body {
        color: #000000;
        background-color: #ffffff;
      }
      .comment {
        /* font-lock-comment-face */
        color: #cd5c5c;
        font-weight: bold;
      }
      .string {
        /* font-lock-string-face */
        color: #838b8b;
        font-weight: bold;
        font-style: italic;
      }
      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
    -->
    </style>
  </head><body>
    <pre><span class="comment">;;##########################################################
;; definition de l'environnement couleur.
;; nom de couleur voir fichier /var/X11R6/lib/rgb.txt
</span>
<span class="comment">;; font-lock
;;(modify-face FACE FOREGROUND BACKGROUND STIPPLE BOLD-P ITALIC-P UNDERLINE-P &amp;optional INVERSE-P FRAME)
</span>(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

<span class="comment">;; pour le preprocesseur
</span>(modify-face font-lock-builtin-face <span class="string">"DarkKhaki"</span>  nil nil t nil nil)
<span class="comment">;; commentaires
</span>(modify-face font-lock-comment-face <span class="string">"indianRed"</span> nil nil t nil nil)      
<span class="comment">;; constantes
</span>(modify-face font-lock-constant-face <span class="string">"violetRed"</span>  nil nil t nil nil)
<span class="comment">;; ??????
</span>(modify-face font-lock-doc-face <span class="string">"yellow"</span> nil nil nil nil nil)
<span class="comment">;; noms de fonctions
</span>(modify-face font-lock-function-name-face <span class="string">"darkorange"</span> nil nil t nil nil)
<span class="comment">;; mots reserves
</span>(modify-face font-lock-keyword-face <span class="string">"MediumOrchid1"</span>  nil nil t nil nil)
<span class="comment">;; chaines de caracteres
</span>(modify-face font-lock-string-face  <span class="string">"azure4"</span>  nil nil t t nil)
<span class="comment">;; types
</span>(modify-face font-lock-type-face <span class="string">"LightSeaGreen"</span> nil nil t nil nil)
<span class="comment">;; variables 
</span>(modify-face font-lock-variable-name-face  <span class="string">"DodgerBlue"</span> nil nil t nil nil)
<span class="comment">;; ??????
</span>(modify-face font-lock-warning-face <span class="string">"red"</span> nil nil t nil nil)
</pre>
  </body></html>