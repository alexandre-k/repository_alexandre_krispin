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

"This source imports all of my miscellanous settings. Tab bar, spell checking,
"etc
source ~/.vim/vimrc/vimrc_miscellaneous

" This source imports all of my plugin settings and bindings.
source ~/.vim/vimrc/vimrc_mapping_and_plugins
