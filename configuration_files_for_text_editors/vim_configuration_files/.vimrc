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

"To jump from a section to another one
map <silent> ]s :/\\\(sub\)\{,2}section\s*{<CR> :noh<CR>
map <silent> [s :?\\\(sub\)\{,2}section\s*{<CR> :noh<CR>


"------------------------------------------------------------------------
"				GENERAL SETTINGS
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
set smartcase       " Override the 'ignorecase' option if
set wrapscan                          " end of search has been achieved!
set magic		"regexp version magic
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
set ruler
set rulerformat=%25(%n%m%r:\ %Y\ [%l,%v]\ %p%%%)
" toujours afficher le mode courant
set showcmd		 "show the command being typed

"-----------------------------------------
" change cursor colour depending upon mode
if exists('&t_SI')
    let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
    let &t_EI = "\<Esc>]12;grey80\x7"
endif


"--------------------------------
" Backups
"--------------------------------
set nobackup
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


"------------------
"formatting
"------------------
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
"---------------
set smartindent      " turn on smart indenting
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set smarttab      " insert tabs on the start of a line according to
                  "    shiftwidth, not tabstop
set comments+=:%,:\\begin{,:\\end{,:\\title{,:\\author{,:\\subtitle{,:\\part{,:\\chapter{,:\\section{,:\\subsection{,:\\subsubsection{,:\\paragraph,:\\subparagraph{,:\\usepackage{,:\\documenclass{,:\\usepackage[,:\\item[,:\\item
"setlocal indentkeys+=},=\\item,=\\bibitem,=\\else,=\\fi,=\\or,=\\]

"--------------------------
" indentation automatique (à la Emacs)
"vnoremap <C-J>   =$
"vnoremap <tab>   =
"nnoremap <tab>   =$
"nnoremap <C-tab> mzvip=`z

"let g:tex_indent_items = 1

"----------------------------
"TAB BAR
"----------------------------
" set up tab labels with tab number, buffer name, number of windows
function! GuiTabLabel()
    let label = ''
    let bufnrlist = tabpagebuflist(v:lnum)
 
    " Add '+' if one of the buffers in the tab page is modified
    for bufnr in bufnrlist
    if getbufvar(bufnr, "&modified")
        let label = '+'
        break
    endif
    endfor
 
    " Append the tab number
    let label .= tabpagenr().': '
 
    " Append the buffer name
    let name = bufname(bufnrlist[tabpagewinnr(v:lnum) - 1])
    if name == ''
        " give a name to no-name documents
        if &buftype=='quickfix'
            let name = '[Quickfix List]'
        else
            let name = '[Not yet saved]'
        endif
    else
        " get only the file name
        let name = fnamemodify(name,":t")
    endif
    let label .= name
 
    " Append the number of windows in the tab page
    let wincount = tabpagewinnr(v:lnum, '$')
    return label . '  [' . wincount . ']'
endfunction
 
" set up tab tooltips with every buffer name
function! GuiTabToolTip()
    let tip = ''
    let bufnrlist = tabpagebuflist(v:lnum)
 
    for bufnr in bufnrlist
        " separate buffer entries
        if tip!=''
            let tip .= ' | '
        endif
 
        " Add name of buffer
        let name=bufname(bufnr)
        if name == ''
            " give a name to no name documents
            if getbufvar(bufnr,'&buftype')=='quickfix'
                let name = '[Quickfix List]'
            else
                let name = '[Not yet saved]'
            endif
        endif
        let tip.=name
 
        " add modified/modifiable flags
        if getbufvar(bufnr, "&modified")
            let tip .= ' [+]'
        endif
        if getbufvar(bufnr, "&modifiable")==0
            let tip .= ' [-]'
        endif
    endfor
 
    return tip
endfunction
 
set guitablabel=%!GuiTabLabel()
set guitabtooltip=%!GuiTabToolTip()

"--------------
"spelling...
"--------------
"to enable spell checking by default, uncomment the following line,
"set spell
" automatic spell checking in your language for .txt et .tex. Replace "fr" by your default
" language, "en" if english :

"augroup filetypedetect
  "au BufNewFile,BufRead *.txt setlocal spell spelllang=fr
  "au BufNewFile,BufRead *.tex setlocal spell spelllang=fr
"augroup END

 "------------------------------------
" painless spell checking
" for French, you'll need
" wget http://ftp.vim.org/pub/vim/runtime/spell/fr.utf-8.sug
" wget http://ftp.vim.org/pub/vim/runtime/spell/fr.utf-8.spl
" which you may move into ~/.vim/spell
"-------------------------------------
function s:spell_fr()
    if !exists("s:spell_check") || s:spell_check == 0
        echo "Spell checking activated (french)"
        let s:spell_check = 1
        setlocal spell spelllang=fr
    else
        echo "Spell checking canceled"
        let s:spell_check = 0
        setlocal spell spelllang=
    endif
endfunction
" for English
function s:spell_en()
    if !exists("s:spell_check") || s:spell_check == 0
        echo "Spell checking activated (english)"
        let s:spell_check = 1
        setlocal spell spelllang=en
    else
        echo "Spell checking canceled"
        let s:spell_check = 0
        setlocal spell spelllang=
    endif
endfunction

"See mapping for spell checking in the relevant section, l. 361

"--------Another trick for spell checking is the following line :
"uncomment if you want to use it, type ",C" if you want to enable it,
"and replace aspell by any other dictionary you use (ispell, hunspell)
"map ,C :w<CR>:!aspell -c %<CR>:e %<CR>"

"---------------------------
"For tags, but doesn't work
let tlist_tex_settings   = 'latex;s:sections;g:graphics;l:labels'
let tlist_make_settings  = 'make;m:makros;t:targets'

"------------------------------------------------------------------------
"				MAPPING
"------------------------------------------------------------------------


"----------------------------
"Saving
"---------------------------
map <F2> :w<CR>
imap <F2> <ESC>:w<CR>

"------------------------------
" Terminal
"------------------------------
map <S-F2> :!gnome-terminal &<CR><CR>
imap <S-F2> <ESC>:!gnome-terminal &<CR><CR>

"-------------------------------
" Graphical file manager
"--------------------------------
map <A-F2> :!nautilus & <CR><CR>
map <A-F2> <ESC>:!nautilus &<CR><CR>

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

"----------------------------
"Mapping to activate file explorer 
"of the specified directory
"----------------------------
"To display NERDTree
nnoremap <silent> <A-F4> :NERDTree /home/freeman/<CR>
" To display VimExplorer
nmap <silent> <F4> :VE %:p:h<CR>

"----------------------------
"Mapping to explorer of recent file,
"Most recently used files
"----------------------------
map <F8> :Mru<CR>
imap <F8> <ESC>:Mru<CR>

"-------------
"SelectBuf
"-------------
nmap <unique> <silent> <F10> <Plug>SelectBuf
noremap <silent> <Plug>SelBufHelpKey <A-F10>

"-----------------------------
"Compiling and viewing its .tex file
"with XeLaTeX and evince (set in tex.vim)
"-----------------------------
"Compile and start viewer
map <F11> ;ll ;lv<CR>
imap <F11> <ESC>;ll ;lv<CR>
"Compile only
map <A-F11> ;ll<CR>
imap <A-F11> ;ll<CR>

"--------------------------
"To enable spell checking for french :
noremap  <F12>  :call <SID>spell_fr()<CR>
inoremap <F12>  :call <SID>spell_fr()<CR>
vnoremap <F12>  :call <SID>spell_fr()<CR>
" and for english :
noremap  <A-F12> :call <SID>spell_en()<CR>
inoremap <A-F12> :call <SID>spell_en()<CR>
vnoremap <A-F12> :call <SID>spell_en()<CR>

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
" unmap arrows/pgdn/pgup so you learn to use hjkl
map <Left> \
map <Right> \
map <Up> \
map <Down> \
map <PageUp> \
map <PageDown> \
 
imap <Left> <nop>
imap <Right> <nop>
imap <Up> <nop>
imap <Down> <nop>
imap <PageUp> <nop>
imap <PageDown> <nop>
"--------------------------------
"Tired of clearing highlighted searches ?
"--------------------------------
nmap <silent> ,/ :nohlsearch<CR>

"--------------------------------
"Will search the word in firefox where
"the cursor is when typing g in visual mode
"big thanks to http://la.firme.perso.esil.univmed.fr/website/article.php3?id_article=70 
vmap f :<C-U>!firefox "http://www.google.fr/search?hl=fr&q=<cword>&btnG=Recherche+Google&meta=" &gt;& /dev/null<CR><CR>
"A similar behaviour but for Wikipedia
vmap w :<C-U>!firefox "http://en.wikipedia.org/wiki/<cword>" >& /dev/null<CR><CR>

"--------------------------------
"For qwerty keyboards : instead of 
"pressing "shift"+";", you will just
"have to press ;, and say w to save.
"-------------------------------
"nnoremap ; :

"--------------------------------
"when you forgot to sudo before editing a file that requires root privileges
"(typically /etc/hosts). This lets you use w!! to do that after you opened the
"file already
"--------------------------------
cmap w!! w !sudo tee % >/dev/null

"------------------------------------------------------------------
"			MiSCELLANOUS
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

"-------------------------
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

" 日本語入力用のkeymapの設定例 (コメントアウト)
"if has('keymap')
"  " ローマ字仮名のkeymap
"  "silent! set keymap=japanese
set iminsert=0 imsearch=0 " 入力時の初期状態 = IME OFF
"endif

