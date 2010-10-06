"--------------------------------------------------------------------------
"																General settings
"--------------------------------------------------------------------------
let g:autoclose = 1
let g:Tex_SmartKeyQuote = 0

setlocal efm+=%E%f:%l:\ %m

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_CompileRule_pdf='/usr/local/texlive/2010/bin/i386-linux/xelatex $*'

let g:Tex_ViewRule_dvi = 'xdvi'
let g:Tex_ViewRule_ps  = 'gv'
let g:Tex_ViewRule_pdf = 'evince'


"---------------------------------
"Enhanced Vim formatting of LaTeX files
"---------------------------------
map \gq ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\\|\\documentclass\\|\\usepackage\\|\\paragraph\\item\)?1<CR>gq//+1<CR>
omap lp ?^$\\|^\s*\(\\begin\\|\\end\\|\\label\)?1<CR>//-1<CR>.<CR>

" To format LaTeX files,
setlocal comments+=:%,:\\begin{,:\\end{,:\\title{,:\\author{,:\\subtitle{,:\\part{,:\\chapter{,:\\section{,:\\subsection{,:\\subsubsection{,:\\paragraph,:\\subparagraph{,:\\usepackage{,:\\documenclass{,:\\usepackage[,:\\item[,:\\item
"-------
"using tex.vim in ~/.vim/indent/tex.vim
"let g:tex_indent_items = 1

"---------------------
"Dictionaries
autocmd Filetype tex,latex :set dictionary=~/.vim/ftplugin/latex-suite/dictionaries/dictionary,/home/linux/.vim/spell

"--------------------------------------------
"Fixing LaTeX-Suite's jump to error feature
"--------------------------------------------
let g:Tex_CompileRule_dvi = 'latex -interaction=nonstopmode -file-line-error-style $*'

"----------------------------------------------------------------------
"			Abbreviations
"----------------------------------------------------------------------
ab ds dans
ab bcp beaucoup
ab letat l'État
ab socio sociologie
ab éco économie
ab math mathématique
ab xelatex \XeLaTeX
ab latex \LaTeX
ab ak Alexandre Krispin


"---------------------------------------------------------------------------
"														MAPPING
"---------------------------------------------------------------------------

let Tex_PromptedEnvironments  = ""
"imap <silent> <buffer> g:Tex_PromptedEnvironments <S-F5>
"--------------------------------
"Change the mapleader from \ to ,
let mapleader=";"

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


"-----------------------------------------------------------------------------
"										MiSCELLANOUS
"-----------------------------------------------------------------------------

"-----------------------------------------------------
" From http://linuxwisdom.blogspot.com/search/label/vi
"-----------------------------------------------------
" Set the warning messages to ignore.
let g:Tex_IgnoredWarnings =
\"Underfull\n".
\"Overfull\n".
\"specifier changed to\n".
\"You have requested\n".
\"Missing number, treated as zero.\n".
\"There were undefined references\n".
\"Citation %.%# undefined\n".
\'LaTeX Font Warning:'"
" This number N says that latex-suite should ignore the first N of the above.
let g:Tex_IgnoreLevel = 8



"----------------------------------
"Special indentation for \footnotes
"----------------------------------
" Vim indent file
" mpg-customized
"
" Language: LaTeX
" Was:      http://www.unet.univie.ac.at/~a9925098/vim/indent/tex.vim
"

if exists("b:did_indent") | finish
endif
let b:did_indent = 1

setlocal indentexpr=GetTeXIndent()
setlocal nolisp
setlocal nosmartindent
setlocal autoindent
setlocal indentkeys+=},=\\item,=\\bibitem,=\\else,=\\fi,=\\or,=\\]

" Only define the function once
if exists("*GetTeXIndent") | finish
endif

function GetTeXIndent()

  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " At the start of the file use zero indent.
  if lnum == 0
    return 0
  endif

  let ind = indent(lnum)
  let line = getline(lnum)             " last line
  let cline = getline(v:lnum)          " current line

  " Remove the commented part of the lines
  let line = substitute(line, '%.*$', '', '')
  let cline = substitute(cline, '%.*$', '', '')

  " Add a 'shiftwidth' after beginning of environments.
  " Don't add it for \begin{document}, \begin{verbatim}, etc.
  if line =~ '\\begin{'  && line !~ 'verbatim'
        \ && line !~ 'document' && line !~ 'lstlisting'

    let  ind += &sw

  endif

  " Subtract a 'shiftwidth' when an environment ends
  if cline =~ '\\end{' && cline !~ 'verbatim'
        \&& cline !~ 'document' && cline !~ 'lstlisting'

    let  ind -= &sw

    " Remove another sw for item-environments
    if cline =~ 'itemize\|description\|enum\|thebibliography'
      let  ind -= &sw
    endif

  endif

  " Same for short display math environment \[ ... \]
  if line =~ '\\\['
    let ind += &sw
  endif

  if cline =~ '\\\]'
    let ind -= &sw
  endif

  " Special treatment for 'item'
  " ----------------------------

  " '\item' or '\bibitem' itself:
  if cline =~ '^\s*\\\(bib\)\=item' && line !~ '^\s*\\begin{'
    let  ind -= &sw
  endif

  " lines following to '\item' are intented once again:
  if line =~ '^\s*\\\(bib\)\=item'
    let  ind += &sw
  endif

  " Special treatment for 'if' constructs
  " -------------------------------------

  if line =~ '^\s*\(\\if\|\\expandafter\\if\|\\else\>\|\\or\>\)'
              \ && line !~ '\\fi\>'
    let ind += &sw
  endif

  if cline =~ '^\s*\(\\else\>\|\\or\>\|\\fi\>\)'
    let ind -= &sw
  endif

  " { and } (must be done at end, since it modifies line)
  " -------

  let line = substitute(line, '\\{\|\\}', '', 'g')
  let ind += &sw * strlen(substitute(line, '[^{]', '', 'g'))
              \ - &sw * strlen(substitute(line, '[^}]', '', 'g'))

  return ind

endfunction

" vim: set sw=2 ts=2:

