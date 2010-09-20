(TeX-add-style-hook "article"
 (lambda ()
    (LaTeX-add-bibitems
     "Clerc07"
     "Fischer96"
     "Forrest86"
     "Guignet93"
     "Lefebvre70"
     "Lemarchand08"
     "Stiglitz07")
    (TeX-run-style-hooks
     "geometry"
     "includeheadfoot"
     "hyperref"
     "graphicx"
     "xcolor"
     "amsmath"
     "textcomp"
     "longtable"
     "url"
     "babel"
     "frenchb"
     "xltxtra"
     "latex2e"
     "scrartcl11"
     "scrartcl"
     "a4paper"
     "11pt")))

