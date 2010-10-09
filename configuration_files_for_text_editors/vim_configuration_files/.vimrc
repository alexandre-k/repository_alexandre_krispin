"~/.vimrc
"=======================================================================
"          FILE:  Configuration file for GVim.
" 
"         USAGE:  To be copied in your home.
" 
"   DESCRIPTION:  It will load automatically all settings put in different
"   files. These files are located in ~/.vim/vimrc/ Change or copy it according
"   to your needs.
" 
"       OPTIONS:  ---
"  REQUIREMENTS:  you should have GVim >= 7.2
"          BUGS:  ---
"         NOTES:  In case of problem, see here : http://alexkrispin.wordpress.com/
"        AUTHOR: Alexandre Krispin, k.m.alexandre@gmail.com
"       COMPANY:  ---
"       CREATED:  I don't remember the date...
"      REVISION:   2010年 10月  9日 土曜日 01:35:48 CEST
"=======================================================================

" This source imports all of my general settings. Indent, hilight, etc.
source ~/.vim/vimrc/vimrc_general

"This source imports all of my miscellanous settings. Tab bar, etc
source ~/.vim/vimrc/vimrc_miscellaneous

" This source imports all of my plugin settings and bindings.
source ~/.vim/vimrc/vimrc_mapping_and_plugins

"---------------------------------------------------------------------
"				spell checking
"---------------------------------------------------------------------
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

"--------------------------
"To enable spell checking for french :
noremap  <F12>  :call <SID>spell_fr()<CR>
inoremap <F12>  :call <SID>spell_fr()<CR>
vnoremap <F12>  :call <SID>spell_fr()<CR>
" and for english :
noremap  <A-F12> :call <SID>spell_en()<CR>
inoremap <A-F12> :call <SID>spell_en()<CR>
vnoremap <A-F12> :call <SID>spell_en()<CR>

"--------Another trick for spell checking is the following line :
"uncomment if you want to use it, type ",C" if you want to enable it,
"and replace aspell by any other dictionary you use (ispell, hunspell)
"map ,C :w<CR>:!aspell -c %<CR>:e %<CR>"
