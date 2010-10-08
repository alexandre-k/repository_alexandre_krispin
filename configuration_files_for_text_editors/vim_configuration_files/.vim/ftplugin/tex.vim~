"--------------------------------------------------------------------------
"			General settings
"--------------------------------------------------------------------------
"let g:autoclose = 1
"let g:Tex_SmartKeyQuote = 1

setlocal efm+=%E%f:%l:\ %m

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_CompileRule_pdf='/usr/local/texlive/2010/bin/i386-linux/xelatex $*'

let g:Tex_ViewRule_dvi = 'xdvi'
let g:Tex_ViewRule_ps  = 'gv'
let g:Tex_ViewRule_pdf = 'evince'


"---------------------------------
"Enhanced Vim formatting of LaTeX files
"---------------------------------
"map \gq ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\\|\\documentclass\\|\\usepackage\\|\\paragraph\\item\)?1<CR>gq//+1<CR>
"omap lp ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\)?1<CR>//-1<CR>.<CR>

""To format LaTeX files,


"---------------------
"Dictionaries
autocmd Filetype tex,latex :set dictionary=~/.vim/ftplugin/latex-suite/dictionaries/dictionary,/home/linux/.vim/spell

"----------------------------------------------------------------------
"			Abbreviations
"----------------------------------------------------------------------
iab ds dans
iab bcp beaucoup
iab qqn quelqu'un
iab qqc quelque chose
iab pol politique
iab leurope l'Europe
iab tv télévision
iab ceca CECA
iab ue Union Européenne
iab CE Commission Européenne
iab AL Amérique Latine
iab letat l'État
iab socio sociologie
iab éco économie
iab math mathématique
iab xelatex \XeLaTeX
iab latex \LaTeX
iab ak Alexandre Krispin


"---------------------------------------------------------------------------
"				MAPPING
"---------------------------------------------------------------------------

"--------------------------------
"fixing of é letter :
imap <buffer> <leader>it <Plug>Tex_InsertItemOnThisLine
imap <C-b> <Plug>Tex_MathBF
imap <C-c> <Plug>Tex_MathCal
imap <C-l> <Plug>Tex_LeftRight

"---------------------------------------
"Another shortcut for placeholders.
"The default one is ctrl+j, really 
"annoying. Pressing shift+tab is easier
imap <S-tab> <Plug>IMAP_JumpForward

"---------------------------------------
"useful imaps
imap <buffer> ,ja {\ja 
"the above imap is intended to work with 
"my setup for japanese in my templates compiled
"with XeLaTeX. For more details, look at my
"blog : http://alexkrispin.wordpress.com/
imap <buffer> ... \ldots

"---------------------------------
"autoclose brackets
"Actually you can get a similar
"behavior with snippets : just type (,
"and then press the tab key. In case
"you prefer snippets, comment the 
"following 3 lines
imap <buffer> { {}
imap <buffer> ( ()
imap <buffer> [ []

"---------------------------------------
"With XeLaTeX, these imaps are no longer
"required since you compile with
"unicode. In case you use pdfLaTeX to 
"compile, enable the following :
"imap <buffer> « \og
"imap <buffer> » \fg
"imap <buffer> € \EUR
"imap <buffer>~ $\sim\ 
"imap <buffer> " \textquotedblleft 
"imap <buffer> ¢ \textquotedblright\ 

"--------------------------------------
"If you have a french keyboard, this setting
"will let you easily put, let say `bf.
"Instead of inserting `bf in normal mode
"you will just have to insert ,bf (therefore,
"no need of the Alt Gr key)
vmap <buffer> <silent> , `

