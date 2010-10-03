"-------------------------------------------------------------------------
"		Basic settings for LaTeXsuite
"http://vim-latex.sourceforge.net/documentation/latex-suite/recommended-settings.html
"-------------------------------------------------------------------------
" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
"set shellslash

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_CompileRule_pdf='/usr/local/texlive/2010/bin/i386-linux/xelatex $*'
"let g:Tex_ViewRule_pdf='/usr/bin/open -a /Applications/Preview.app'

"To jump from a section to another one
map <silent> ]s :/\\\(sub\)\{,2}section\s*{<CR> :noh<CR>
map <silent> [s :?\\\(sub\)\{,2}section\s*{<CR> :noh<CR>


"------------------------------------------------------------------------
"				General settings
"------------------------------------------------------------------------
let b:loaded_tex_autoclose = 1
let g:autoclose = 1
set backspace=indent,eol,start "allow backspacing over everything in insert mode
set textwidth=80
set title                " change the terminal's title
syntax on		"switch on syntax highlight
set undolevels=1000	"number of undos
set history=1000"	"sets how many lines of history VIM has to remember
set complete+=k         " enable dictionary completion
set clipboard+=unnamed  " yank and copy to X clipboard
set showmatch		"show pairs of brackets
set hlsearch        " When there is a previous search pattern, highlight all
                    " its matches.
set incsearch       " While typing a search command, show immediately where the
                    " so far typed pattern matches.
set ignorecase      " Ignore case in search patterns.
set cursorline   	"highlight current line
set cursorcolumn 	"highlight current column
set wildmenu
set wildignore=*.o,*~,*.cmo,*.cmi,*.a,*.cmx,*.cmxa,*.lo,*.log,*.aux,*.dvi,*.aut,*.aux,*.bbl,*.blg,*.dvi,*.fff,*.out,*.pdf,*.ps,*.toc,*.ttt
set number		"to show line number
"set nocompatible	"seems that setting Vim not to be compatible with Vi disables the setting bellow, whichwrap.
"Therefore I disable it
set whichwrap=b,s,<,>,[,]	" Allow specified keys that move the cursor left/right
			" to move to the previous/next line when the cursor is on the first/last character
			" in the line. b (backspace), s (space) and the arrows.  set nostartofline
			" Do not place the cursor at the start of the line when using Page up/down.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.out,.toc "for file names completion
set hi=2000		" remember last 2000 typed commands
set autoread	" Set to auto read when a file is changed from the outside
set enc=utf-8
set hidden	" It hides buffers instead of closing them. This means that you
		" can have unwritten changes to a file and open a new file using :e, without being
		" forced to write or undo your changes first. Also, undo buffers and marks are
		" preserved while the buffer is open.
set visualbell           " don't beep
set noerrorbells         " don't beep
set list		" Vim can highlight whitespaces for you in a convenient way:	
set listchars=tab:>.,trail:.,extends:#,nbsp:.
set laststatus=2	"status bar
set statusline=%n:\ %f%m%r%h%w\ [%Y,%{&fileencoding},%{&fileformat}]\ [%l-%L,%v][%p%%]\ [%{strftime(\"%l:%M:%S\ \%p,\ %a\ %b\ %d,\ %Y\")}]

"--------------------------------
" Backups
"--------------------------------
set backup
set backupcopy=auto
set backupskip=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*test*,*temp*,*tmp*,*tst*,*~,*bak


"-----------------------------------
"nice colors and fonts
"-----------------------------------
colorscheme herald
colors herald
"set guifont=DejaVu\ Sans\ Mono\ 10
set guifont=Inconsolata\ 12 " very nice, but leaves terrible artefacts with national (mostly russian) characters
"set guifont=Terminus\ 10 " wonderful, yet smallish, and sucks at larger sizes
"----------------------------------
" GUI Options
"----------------------------------
"if has("gui_running")
"	" GUI cursor: no blinking
	set guicursor+=a:blinkon0
"
"	" no toolbar
"	set guioptions-=T
"	" no autoselect 
"	set guioptions-=a
"
"	" Use console messages instead of GUI dialogs
"	set guioptions+=c
"endif



"-----------------------------------------------------------------------------
"				formatting
"-----------------------------------------------------------------------------
set formatoptions=tcroqn " see help
"set formatoptions=c,q,r,t " This is a sequence of letters which describes how
                    " automatic formatting is to be done.
                    "
                    " letter    meaning when present in 'formatoptions'
                    " ------    ---------------------------------------
                    " c         Auto-wrap comments using textwidth, inserting
                    "           the current comment leader automatically.
                    " q         Allow formatting of comments with "gq".
                    " r         Automatically insert the current comment leader
                    "           after hitting <Enter> in Insert mode. 
                    " t         Auto-wrap text using textwidth (does not apply
                    "           to comments)
"-----------------------------------------------------------------
set smartindent      " turn on smart indenting
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set smarttab      " insert tabs on the start of a line according to
                  "    shiftwidth, not tabstop


"-----------------------------------------------------------------
" indentation automatique (à la Emacs)
vnoremap <C-F>   =$
vnoremap <tab>   =
nnoremap <tab>   =$
nnoremap <C-tab> mzvip=`z
"------------------------------------------------------------------
setlocal indentkeys+=},=\\item,=\\bibitem,=\\else,=\\fi,=\\or,=\\]
"------------------------------------------------------------------
" shortcut for formatting paragraph
map	<C-J>	gqap
imap	<C-J>	<C-O>gqap
vmap	<C-J>	gq



"------------------------------------------------------------------
"			Others
"------------------------------------------------------------------

"--------------------------------------------------------
" Go back where I left off
    autocmd BufReadPost * call RestoreCursorPos()
    autocmd BufWinEnter * call OpenFoldOnRestore()

" Restore my cursor position
function! RestoreCursorPos()
    if expand("<afile>:p:h") !=? $TEMP
        if line("'\"") > 1 && line("'\"") <= line("$")
            let line_num = line("'\"")
            let b:doopenfold = 1
            if (foldlevel(line_num) > foldlevel(line_num - 1))
                let line_num = line_num - 1
                let b:doopenfold = 2
            endif
            execute line_num
        endif
    endif
endfunction

" Open the fold if restoring cursor position
function! OpenFoldOnRestore()
    if exists("b:doopenfold")
        execute "normal zv"
        if(b:doopenfold > 1)
            execute "+".1
        endif
        unlet b:doopenfold
    endif
endfunction
"-----------------------------------------------------------



"----------------------------------------------------------------------
"			Abbreviations
"----------------------------------------------------------------------
ab ds dans
ab bcp beaucoup
ab letat l'État
ab socio sociologie
ab éco économie
ab math mathématique
ab btcqb baise tout ce qui bouge


"------------------------------------------------------------------------
"				mapping
"------------------------------------------------------------------------

"---------------------------------
"Ctrl+Insert to copy into clipboard 
"Shift+Insert to paste from clipboard
"Shift+Delete to cut into clipboard
"Ctrl+a to select all
"----------------------------
nmap <S-Insert> "+gP
imap <S-Insert> <ESC><S-Insert>i
vmap <C-Insert> "+y 
map <C-a> ggVG
map <S-Delete> "+x


"----------------------------
"Mapping to desactivate highligting
"of search results
"----------------------------
nnoremap <silent> <C-N> :noh<CR>

"----------------------------
"Mapping to explorer of recent file,
"Most recently used files
"----------------------------
nnoremap <silent> <F5> :Mru<CR>
"----------------------------
"Mapping to activate file explorer 
"of the specified directory
"----------------------------
nnoremap <silent> <F4> :NERDTree /home/freeman/<CR>

"-----------------------------
"If you want to write in a non western
"language, you will have to disable
"the autocompletion popup.
"-----------------------------
"In normal mode, F8 enable it
nmap <F8> :AcpEnable<CR>
"And F9 disable it
nmap <F9> :AcpDisable<CR>

"----------------------------
"Saving
"---------------------------
map <F2> :w<CR>
imap <F2> <ESC>:w<CR>

"----------------------------
"Exit
"----------------------------
"exit
map <F3> :q<CR>
imap <F3> <ESC>:q<CR>
"exit all
map <S-F3> :qall<CR>
imap <S-F3> <ESC>:qall<CR>
"force Exit
map <A-F3> :qall!<CR>
imap <A-F3> <ESC>:qall!<CR>



"------------------------------
" Terminal
"------------------------------
map <S-F2> :!gnome-terminal &<CR><CR>
imap <S-F2> <ESC>:!gnome-terminal &<CR><CR>

"-------------------------------
" Graphical file manager
"--------------------------------
map <F2>t :!nautilus & <CR><CR>
map <F2>t <ESC>:!nautilus &<CR><CR>


"--------------------------------
" firefox like shorcuts
"--------------------------------
map <C-t>     :tabnew<cr>
map <C-left>  :tabnext<cr>
map <C-right> :tabprevious<cr>
map <C-o> :e

"--------------------------------
" Quickly edit/reload the vimrc file
"-------------------------------
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>


"--------------------------------
"For qwerty keyboards : instead of 
"pressing "shif"+";", you will just
"have to press ;, and say w to save.
"-------------------------------
nnoremap ; :

"--------------------------------
" Use Q for formatting the current paragraph (or selection)
"--------------------------------
vmap Q gq
nmap Q gqap

"--------------------------------
"If you like long lines with line wrapping enabled
"--------------------------------
nnoremap j gj
nnoremap k gk

"--------------------------------
"Tired of clearing highlighted searches ?
"--------------------------------
nmap <silent> ,/ :nohlsearch<CR>

"--------------------------------
"when you forgot to sudo before editing a file that requires root privileges
"(typically /etc/hosts). This lets you use w!! to do that after you opened the
"file already
"--------------------------------
cmap w!! w !sudo tee % >/dev/null

"---------------------------------------------------------------------------
" 日本語入力に関する設定:
"
if has('multi_byte_ime') || has('xim')
  " IME ON時のカーソルの色を設定(設定例:紫)
  highlight CursorIM guibg=Purple guifg=NONE
  " 挿入モード・検索モードでのデフォルトのIME状態設定
  set iminsert=0 imsearch=0
  if has('xim') && has('GUI_GTK')
    " XIMの入力開始キーを設定:
    " 下記の s-space はShift+Spaceの意味でkinput2+canna用設定
    "set imactivatekey=s-space
  endif
  " 挿入モードでのIME状態を記憶させない場合、次行のコメントを解除
  "inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
endif


