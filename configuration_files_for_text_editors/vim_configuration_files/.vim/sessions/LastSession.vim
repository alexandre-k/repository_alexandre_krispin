let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <C-Tab> 	
imap <silent> <Plug>IMAP_JumpBack =IMAP_Jumpfunc('b', 0)
imap <silent> <Plug>IMAP_JumpForward =IMAP_Jumpfunc('', 0)
imap <S-Tab> 
inoremap <expr> <PageUp> pumvisible() ? "\<PageUp>\\" : "\<PageUp>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\\" : "\<PageDown>"
inoremap <expr> <Up> pumvisible() ? "\" : "\<Up>"
inoremap <expr> <Down> pumvisible() ? "\" : "\<Down>"
cmap <F4> :bd!
imap <F4> :bd!
imap <S-F2> :!xterm &
imap <F2> :w
map! <S-Insert> <MiddleMouse>
vnoremap  =$
noremap  h
nnoremap 	 =$
vmap 	 >gv
noremap <NL> j
noremap  k
noremap  l
map  :e
map  :tabnew
map  :BottomExplorerWindow
map  :FirstExplorerWindow
map  :WMToggle
vmap [% [%m'gv``
nmap <silent> \sl <Plug>SpellChangeLanguage
nmap <silent> \s? <Plug>SpellProposeAlternatives
nmap <silent> \sA <Plug>SpellAutoEnable
nmap <silent> \ss <Plug>SpellCheck
nmap <silent> \cv <Plug>VCSVimDiff
nmap <silent> \cu <Plug>VCSUpdate
nmap <silent> \cU <Plug>VCSUnlock
nmap <silent> \cs <Plug>VCSStatus
nmap <silent> \cr <Plug>VCSReview
nmap <silent> \cq <Plug>VCSRevert
nmap <silent> \cn <Plug>VCSAnnotate
nmap <silent> \cN <Plug>VCSSplitAnnotate
nmap <silent> \cl <Plug>VCSLog
nmap <silent> \cL <Plug>VCSLock
nmap <silent> \ci <Plug>VCSInfo
nmap <silent> \cg <Plug>VCSGotoOriginal
nmap <silent> \cG <Plug>VCSClearAndGotoOriginal
nmap <silent> \cd <Plug>VCSDiff
nmap <silent> \cD <Plug>VCSDelete
nmap <silent> \cc <Plug>VCSCommit
nmap <silent> \ca <Plug>VCSAdd
map \mbt <Plug>TMiniBufExplorer
map \mbu <Plug>UMiniBufExplorer
map \mbc <Plug>CMiniBufExplorer
map \mbe <Plug>MiniBufExplorer
nmap \caL <Plug>CalendarH
nmap \cal <Plug>CalendarV
nmap \ihn :IHN
nmap \is :IHS:A
nmap \ih :IHS
vmap ]% ]%m'gv``
vmap a% [%v]%
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <RightMouse> <LeftMouse>:SpellMenuAlternatives<RightMouse>
nnoremap <silent> <Plug>SpellMenuAlternatives :SpellMenuAlternatives
nnoremap <silent> <Plug>SpellProposeAlternatives :SpellProposeAlternatives
nnoremap <silent> <Plug>SpellAutoDisable :SpellAutoDisable
nnoremap <silent> <Plug>SpellAutoEnable :SpellAutoEnable
nnoremap <silent> <Plug>SpellChangeLanguage :SpellChangeLanguage
nnoremap <silent> <Plug>SpellExit :SpellExit
nnoremap <silent> <Plug>SpellCheckLine :SpellCheckLine
nnoremap <silent> <Plug>SpellCheck :SpellCheck
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <Plug>CVSWatchRemove :CVSWatch remove
nnoremap <silent> <Plug>CVSWatchOn :CVSWatch on
nnoremap <silent> <Plug>CVSWatchOff :CVSWatch off
nnoremap <silent> <Plug>CVSWatchAdd :CVSWatch add
nnoremap <silent> <Plug>CVSWatchers :CVSWatchers
nnoremap <silent> <Plug>CVSUnedit :CVSUnedit
nnoremap <silent> <Plug>CVSEditors :CVSEditors
nnoremap <silent> <Plug>CVSEdit :CVSEdit
nnoremap <silent> <Plug>VCSVimDiff :VCSVimDiff
nnoremap <silent> <Plug>VCSUpdate :VCSUpdate
nnoremap <silent> <Plug>VCSUnlock :VCSUnlock
nnoremap <silent> <Plug>VCSStatus :VCSStatus
nnoremap <silent> <Plug>VCSSplitAnnotate :VCSAnnotate!
nnoremap <silent> <Plug>VCSReview :VCSReview
nnoremap <silent> <Plug>VCSRevert :VCSRevert
nnoremap <silent> <Plug>VCSLog :VCSLog
nnoremap <silent> <Plug>VCSLock :VCSLock
nnoremap <silent> <Plug>VCSInfo :VCSInfo
nnoremap <silent> <Plug>VCSClearAndGotoOriginal :VCSGotoOriginal!
nnoremap <silent> <Plug>VCSGotoOriginal :VCSGotoOriginal
nnoremap <silent> <Plug>VCSDiff :VCSDiff
nnoremap <silent> <Plug>VCSDelete :VCSDelete
nnoremap <silent> <Plug>VCSCommit :VCSCommit
nnoremap <silent> <Plug>VCSAnnotate :VCSAnnotate
nnoremap <silent> <Plug>VCSAdd :VCSAdd
vmap <silent> <Plug>IMAP_JumpBack `<i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>IMAP_JumpBack i=IMAP_Jumpfunc('b', 0)
nmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
nnoremap <silent> <Plug>CalendarH :cal Calendar(1)
nnoremap <silent> <Plug>CalendarV :cal Calendar(0)
vmap <S-Left> h
nmap <S-Left> v
omap <S-Left> v
vmap <S-Right> l
nmap <S-Right> v
omap <S-Right> v
vmap <S-Down> j
nmap <S-Down> vj
omap <S-Down> vj
vmap <S-Up> k
nmap <S-Up> vk
omap <S-Up> vk
map <F4> :bd!
map <C-Right> :tabprevious
map <C-Left> :tabnext
map <F2>t :!pcmanfm &
map <S-F2> :!xterm &
map <silent> <F2> :if &guioptions =~# 'T' |set guioptions-=T |set guioptions-=m |else |set guioptions+=T |set guioptions+=m |endif
vmap <S-Tab> <gv
nnoremap <C-Tab> mzvip=`z
map <S-Insert> <MiddleMouse>
imap 	 
imap <NL> <Plug>IMAP_JumpForward
inoremap <expr>  pumvisible() ? "\" : "\"
inoremap <expr>  pumvisible() ? "\" : "\"
imap \ihn :IHN
imap \is :IHS:A
imap \ih :IHS
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autoread
set backspace=indent,eol,start
set backup
set backupskip=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*test*,*temp*,*tmp*,*tst*,*~,*bak
set clipboard=autoselect,exclude:cons\\|linux,unnamed
set complete=.,w,b,u,t,i,k
set completeopt=menu,preview,menuone
set diffopt=filler,iwhite
set expandtab
set fileencodings=ucs-bom,utf-8,cp936,big5,euc-jp,euc-kr,gb18030,latin1
set guifont=DejaVu\ Sans\ Mono\ 10
set guitablabel=%!GuiTabLabel()
set guitabtooltip=%!GuiTabToolTip()
set helplang=fr
set hidden
set hlsearch
set ignorecase
set laststatus=2
set lazyredraw
set matchpairs=(:),{:},[:],<:>
set mouse=a
set mousefocus
set mousemodel=popup_setpos
set ruler
set rulerformat=%25(%n%m%r:\ %Y\ [%l,%v]\ %p%%%)
set shiftwidth=4
set showmatch
set showtabline=2
set smartcase
set smartindent
set smarttab
set softtabstop=4
set splitbelow
set nostartofline
set statusline=%n:\ %f%m%r%h%w\ [%Y,%{&fileencoding},%{&fileformat}]\ [%l-%L,%v][%p%%]
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.out,.toc
set tabstop=4
set termencoding=utf-8
set textwidth=80
set title
set undolevels=200
set updatetime=1
set wildignore=*.o,*~,*.cmo,*.cmi,*.a,*.cmx,*.cmxa,*.lo,*.log,*.aux,*.dvi,*.aut,*.aux,*.bbl,*.blg,*.dvi,*.fff,*.out,*.pdf,*.ps,*.toc,*.ttt
set wildmenu
set window=27
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
silent! argdel *
set splitbelow splitright
set nosplitright
wincmd t
set winheight=1 winwidth=1
argglobal
enew
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i,k
setlocal completefunc=
setlocal nocopyindent
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != ''
setlocal filetype=
endif
set foldcolumn=4
setlocal foldcolumn=4
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
set foldlevel=4
setlocal foldlevel=4
setlocal foldmarker={{{,}}}
set foldmethod=indent
setlocal foldmethod=indent
set foldminlines=2
setlocal foldminlines=2
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=tcq
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=0
setlocal imsearch=0
setlocal include=
setlocal includeexpr=
setlocal indentexpr=
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e,},=\\item,=\\bibitem,=\\else,=\\fi,=\\or,=\\]
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255
setlocal keywordprg=
set linebreak
setlocal linebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:],<:>
setlocal modeline
setlocal modifiable
setlocal nrformats=octal,hex
set number
setlocal number
setlocal numberwidth=4
setlocal omnifunc=syntaxcomplete#Complete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal smartindent
setlocal softtabstop=4
set spell
setlocal spell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != ''
setlocal syntax=
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=80
setlocal thesaurus=
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
