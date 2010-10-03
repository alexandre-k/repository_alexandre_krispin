" Vim color file
" Maintainer:  Damien Gombault <desintegr@gmail.com>
" WWW:         http://desintegr.googlecode.com/svn/config/vim/colors/oxygen.vim
" Last Change: 2008 Oct 28
" Version:     0.01

set background=light

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "oxygen"


" Main highlight groups
hi Normal         guifg=#2e3436   guibg=#ffffff
hi Cursor                         guibg=#555753
"hi CursorIM
"hi CursorColumn
"hi CursorLine
hi Directory      guifg=#2c72c7
hi DiffAdd                        guibg=#d8e8c2
hi DiffChange                     guibg=#fff6c8
hi DiffDelete     guifg=#f9ccca   guibg=#f9ccca
hi DiffText                       guibg=#ffeb55   gui=none
hi ErrorMsg       guifg=#f9ccca   guibg=#e85752   gui=none
hi VertSplit      guifg=bg        guibg=#babdb6
hi Folded         guifg=#babdb6   guibg=bg
hi FoldColumn     guifg=#e85752   guibg=bg
hi SignColum      guifg=#e85752   guibg=bg
hi IncSearch      guifg=fg        guibg=#ffeb55   gui=none
hi LineNr         guifg=#babdb6   guibg=bg
hi MatchParen     guifg=bg        guibg=#2c72c7   gui=bold
hi ModeMsg        guifg=#e85752   guibg=#f9ccca   gui=none
hi MoreMsg        guifg=#37a42c   guibg=#d8e8c2   gui=none
hi NonText        guifg=#d3d7cf
hi Pmenu          guifg=#888a85   guibg=#eeeeec
hi PmenuSel       guifg=fg        guibg=#d3d7cf
hi PmenuSbar                      guibg=#d3d7cf
hi PmenuThumb     guifg=#888a85
hi Question       guifg=#37a42c   guibg=#d8e8c2   gui=none
hi Search         guifg=fg        guibg=#fff6c8
hi SpecialKey     guifg=#d3d7cf
hi SpellBad       guisp=#f08682
hi SpellCap       guisp=#b1d28f
"hi SpellLocal
hi SpellRare      guisp=#8e79b9
hi StatusLine     guifg=#555753   guibg=#eeeeec   gui=none
hi StatusLineNC   guifg=#babdb6   guibg=bg        gui=none
hi TabLine        guifg=#babdb6   guibg=bg        gui=none
hi TabLineFill                    guibg=bg        gui=none
hi TabLineSel     guifg=#555753   guibg=bg        gui=none
hi Title          guifg=#e85752                   gui=none
hi Visual         guifg=bg        guibg=#555753
"hi VisualNOS
hi WarningMsg     guifg=#ec7331   guibg=#fcd9b0   gui=none
"hi WildMenu

"hi Menu
"hi ScrollBar
"hi Tooltip

" Color groups
hi Brown          guifg=#8f6b32
hi Red            guifg=#e85752
hi Rose           guifg=#e85290
hi Purple         guifg=#b14f9a
hi Violet         guifg=#644a9b
hi Blue           guifg=#2c72c7
hi Cyan           guifg=#00a7b3
hi Green          guifg=#00b377
hi Olive          guifg=#77b753
hi Yellow         guifg=#ffeb55
hi Orange         guifg=#f29b68
hi Grey           guifg=#888a85
hi White          guifg=#ffffff

hi BlueU          guifg=#2c72c7                   gui=underline

hi RedR           guifg=#eeeeec   guibg=#e85752
hi YellowR        guifg=fg        guibg=#ffeb55   gui=bold


" Syntax highligh groups
hi! link Comment        Grey
"
hi! link Constant       Purple
hi! link String         Orange
hi! link Character      Orange
"hi Number
"hi Boolean
"hi Float
"
hi! link Identifier     Olive
"hi Function
"
hi! link Statement      Red
"hi Conditional
"hi Repeat
"hi Label
hi! link Operator       Olive
"hi Keyword
"hi Exception
"
hi! link PreProc        Orange
"hi Include
"hi Define
"hi Macro
"hi PreCondit
"
hi! link Type           Blue
hi! link StorageClass   Red
"hi Structure
"hi Typedef
"
hi! link Special        Grey
"hi SpecialChar
hi! link Tag            Olive
"hi Delimiter
"hi SpecialComment
"hi Debug
"
hi! link Underlined     BlueU
"hi Ignore
hi! link Error          RedR
hi! link Todo           YellowR
