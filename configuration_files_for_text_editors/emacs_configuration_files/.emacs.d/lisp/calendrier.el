<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN">
<html><head>
<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">
<!-- Created by htmlize-1.16 in css mode. -->

  
    <title>calendrier.html</title>
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
      .function-name {
        /* font-lock-function-name-face */
        color: #ff8c00;
        font-weight: bold;
      }
      .keyword {
        /* font-lock-keyword-face */
        color: #e066ff;
        font-weight: bold;
      }
      .string {
        /* font-lock-string-face */
        color: #838b8b;
        font-weight: bold;
        font-style: italic;
      }
      .variable-name {
        /* font-lock-variable-name-face */
        color: #1e90ff;
        font-weight: bold;
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
;; Gestion du calendrier 
</span>
<span class="comment">;; on efface toutes les autres jours feries
</span>(setq general-holidays nil)
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq oriental-holidays nil)
(setq solar-holidays nil)

<span class="comment">;; Gestion des fetes de Paques
</span>(<span class="keyword">defun</span> <span class="function-name">feries-paques</span> ()
  <span class="string">"Liste des jours de vacances  relatifs a paques."</span>
  (<span class="keyword">let*</span> ((century (1+ (/ displayed-year 100)))
         (shifted-epact        <span class="comment">;; Age of moon for April 5...
</span>          (% (+ 14 (* 11 (% displayed-year 19))<span class="comment">;;     ...by Nicaean rule
</span>                (-           <span class="comment">;; ...corrected for the Gregorian century rule
</span>                 (/ (* 3 century) 4))
                (/    <span class="comment">;; ...corrected for Metonic cycle inaccuracy.
</span>                 (+ 5 (* 8 century)) 25)
                (* 30 century))<span class="comment">;;              Keeps value positive.
</span>             30))
         (adjusted-epact       <span class="comment">;;  Adjust for 29.5 day month.
</span>          (<span class="keyword">if</span> (or (= shifted-epact 0)
                  (and (= shifted-epact 1) (&lt; 10 (% displayed-year 19))))
              (1+ shifted-epact)
            shifted-epact))
         (paschal-moon       <span class="comment">;; Day after the full moon on or after March 21.
</span>          (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
             adjusted-epact))
         (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
         (day-list
          (list
           (list (calendar-gregorian-from-absolute abs-easter)
                 <span class="string">"Pâques"</span>)
           (list (calendar-gregorian-from-absolute (+ abs-easter 1))
                 <span class="string">"Lundi de Pâques"</span>)
           (list (calendar-gregorian-from-absolute (+ abs-easter 39))
                 <span class="string">"Jeudi de l'ascension"</span>)
           (list (calendar-gregorian-from-absolute (+ abs-easter 49))
                 <span class="string">"Pentecôte"</span>)
           (list (calendar-gregorian-from-absolute (+ abs-easter 50))
                 <span class="string">"Lundi de Pentecôte"</span>)))
         (output-list
          (filter-visible-calendar-holidays day-list)))
    output-list))

<span class="comment">;; Les vacances francaises
;; Je mets aussi d'autres jours remarquables la-dedans.
</span>(setq local-holidays
      '((holiday-fixed 1 1 <span class="string">"Nouvel an"</span>)
        (holiday-fixed 5 1 <span class="string">"Fête du travail"</span>)
        (holiday-fixed 5 8 <span class="string">"Victoire 1945"</span>)
        (feries-paques)
        (holiday-fixed 7 14 <span class="string">"Fête nationale"</span>)
        (holiday-fixed 8 15 <span class="string">"Assomption"</span>)
        (holiday-fixed 11 11 <span class="string">"Armistice 1918"</span>)
        (holiday-fixed 11 1 <span class="string">"Toussaint"</span>)
        (holiday-fixed 12 25 <span class="string">"Noël"</span>)
        <span class="comment">;; fetes a date variable
</span>        <span class="comment">;; (holiday-float 5 0 2 "Fête des mères")
</span>        (holiday-float 6 0 3 <span class="string">"Fête des pères"</span>)
        ))

<span class="comment">;; afficher les fetes
</span>(setq calendar-holiday-marker 'bold)
(setq mark-holidays-in-calendar t)

<span class="comment">;; afficher le jour d'aujourd'hui
</span>(setq today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-today-marker 'highlight)

<span class="comment">;; les mois et les jours en francais
</span>(<span class="keyword">defvar</span> <span class="variable-name">calendar-day-name-array</span>
  [<span class="string">"dim"</span> <span class="string">"lun"</span> <span class="string">"mar"</span> <span class="string">"mer"</span> <span class="string">"jeu"</span> <span class="string">"ven"</span> <span class="string">"sam"</span>])
(<span class="keyword">defvar</span> <span class="variable-name">calendar-month-name-array</span>
  [<span class="string">"janvier"</span> <span class="string">"février"</span> <span class="string">"mars"</span> <span class="string">"avril"</span> <span class="string">"mai"</span> <span class="string">"juin"</span>
   <span class="string">"juillet"</span> <span class="string">"août"</span> <span class="string">"septembre"</span> <span class="string">"octobre"</span> <span class="string">"novembre"</span> <span class="string">"décembre"</span>])

</pre>
  </body></html>