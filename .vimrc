set nocompatible              " be iMproved, required
filetype off                  " required

"Plugins-----------------------------------------------------------------------
call plug#begin('~/.vim/plugged')

Plug 'SirVer/ultisnips'
Plug 'davidhalter/jedi-vim'
Plug 'ervandew/supertab'
Plug 'Rip-Rip/clang_complete', {'do' : 'make install'}
Plug 'Chiel92/vim-autoformat'
Plug 'Shougo/neocomplete.vim'
Plug 'Yggdroot/indentLine'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/vim-easy-align'
Plug 'mhinz/vim-startify'
Plug 'osyo-manga/vim-over', {'on': 'OverCommandLine'}
Plug 'scrooloose/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'wlangstroth/vim-racket'

" Syntastic--------------------------------------------------------------------
function! Installjshint(info)
    if a:info.status == 'installed' || a:info.force
        !npm install -g jshint
    endif
endfunction
Plug 'scrooloose/syntastic', { 'do': function('Installjshint') }

" YouCompleteMe----------------------------------------------------------------
"function! BuildYCM(info)
"if a:info.status == 'installed' || a:info.force
"!./install.sh
"endif
"endfunction
"Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }

" Command-t--------------------------------------------------------------------
Plug 'wincent/command-t', {
            \   'do': 'cd ruby/command-t && ruby extconf.rb && make'
            \ }

Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'dietsche/vim-lastplace'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'flazz/vim-colorschemes'
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'junegunn/goyo.vim'
Plug 'lervag/vimtex'
Plug 'scrooloose/nerdcommenter'
Plug 'tmhedberg/SimpylFold'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

call plug#end()

"------------------------------------------------------------------------------
" General Settings-------------------------------------------------------------
"------------------------------------------------------------------------------

filetype plugin indent on
syntax on
set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus,autoselect
set cmdheight=1
set completeopt+=menuone,longest
set completeopt-=preview
set efm+=\ (%l)\ error:\ %m
set encoding=utf-8
set expandtab
set foldlevel=99
set foldmethod=indent
set formatoptions=qrn1t
set hidden
set history=1000
set ignorecase
set incsearch
set laststatus=2
set pumheight=15
set ruler
set scrolloff=3
set shell=/bin/bash
set shiftwidth=4
set showcmd
set showmode
set smarttab
set softtabstop=4
set spelllang=en
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
set tabstop=4
set textwidth=79
set ttyfast
set undofile
set undolevels=500
set visualbell
set wrap

" Autocomplete Menus {{{
if has("wildmenu")
    set wildignore+=*.a,*.o
    set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png
    set wildignore+=.DS_Store,.git,.hg,.svn
    set wildignore+=*~,*.swp,*.tmp
    set wildmenu
    if has("wildignorecase")
        set wildignorecase
    endif
    set wildmode=longest:full,full
    set wildcharm=<tab>
endif

"------------------------------------------------------------------------------
" Asthetics--------------------------------------------------------------------
" -----------------------------------------------------------------------------
set t_Co=256
set background=dark
colorscheme molokai

" Highlights / Colors {{{
function! s:apply_highlights()
    " No tildes for empty lines
    hi clear NonText | hi NonText
                \
                \ ctermfg=darkgrey guifg=darkgrey
                \

    " Keep colors in visual
    hi clear Visual | hi Visual
                \
                \ ctermfg=white guifg=white
                \ ctermbg=red guibg=red

    " Matching delimiter color
    hi clear MatchParen | hi MatchParen
                \ cterm=bold
                \ ctermfg=172
                \ ctermbg=NONE

    " Terms
    hi clear Search | hi Search
                \
                \ ctermfg=black  guifg=black
                \ ctermbg=yellow guibg=yellow
    hi clear Todo | hi Todo
                \ cterm=bold  gui=bold
                \ ctermfg=red guifg=red
                \ ctermbg=NONE  guibg=NONE
    hi clear Error | hi Error
                \ cterm=reverse gui=reverse
                \ ctermfg=red   guifg=red
                \ ctermbg=black guibg=black
    hi clear SpellBad | hi SpellBad
                \ term=underline cterm=underline gui=underline
                \ ctermfg=red    guifg=red
                \
    hi clear SpellCap | hi SpellCap
                \ term=underline cterm=underline gui=underline
                \
                \
    hi clear SpellLocal | hi SpellLocal
                \ term=underline cterm=underline gui=underline
                \
                \
    hi clear SpellRare | hi SpellRare
                \ term=underline cterm=underline gui=underline
                \
                \

    hi clear VertSplit | hi VertSplit
                \
                \ ctermfg=white guifg=white
                \ ctermbg=black guibg=black

    " Status line
    hi clear StatusLine | hi StatusLine
                \ term=bold     cterm=bold  gui=bold
                \ ctermfg=white guifg=white
                \ ctermbg=black guibg=black
    hi clear StatusLineNC | hi StatusLineNC
                \ term=bold        cterm=bold     gui=bold
                \ ctermfg=darkgrey guifg=darkgrey
                \ ctermbg=black    guibg=black

    " Folds
    hi clear Folded | hi Folded
                \
                \ ctermfg=blue guifg=blue
                \
    hi clear FoldColumn | hi FoldColumn
                \
                \ ctermbg=NONE guibg=NONE
                \

    " Diffs
    hi clear DiffAdd | hi DiffAdd
                \
                \ ctermfg=green guifg=green
                \
    hi clear DiffChange | hi DiffChange
                \
                \ ctermfg=yellow guifg=yellow
                \
    hi clear DiffDelete | hi DiffDelete
                \
                \ ctermfg=red guifg=red
                \

    " Completion menu
    hi clear Pmenu | hi Pmenu
                \
                \ ctermfg=white guifg=black
                \ ctermbg=black guibg=blue
    hi clear PmenuSel | hi PmenuSel
                \ term=bold     cterm=bold  gui=bold
                \ ctermfg=white guifg=white
                \ ctermbg=blue  guibg=blue
    hi clear PmenuSbar | hi PmenuSbar
                \
                \
                \ ctermbg=white guibg=white
    hi clear PmenuThumb | hi PmenuThumb
                \
                \
                \ ctermbg=red guibg=blue
endfunction

if !has('gui_running')
    call s:apply_highlights()
    augroup CUSTOM_COLORS
        au!
        au ColorScheme * call s:apply_highlights()
    augroup END
endif

" Highlight tabs and spaces----------------------------------------------------
highlight SpecialKey ctermfg=9
set list

" General Key Bindings---------------------------------------------------------
"------------------------------------------------------------------------------
" Map Leader to comma
let mapleader = ","

" Indent
noremap > >>
noremap < <<

nnoremap <silent> <bslash> <esc>:%s/
vnoremap <silent> <bslash> <esc>gv:s/

" Q - Replay macro
noremap Q @q

" Unmap the arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
"inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

" Recenter the keys
nmap G Gzz
nmap N Nzz
nmap n nzz
nmap { {zz
nmap } }zz

" abbrv email
iabbr email mutaabhinav@gmail.com

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" change <ESC> to jj
inoremap jj <ESC>

" Quit Buffer
nnoremap <leader>k :q<CR>

" Split and switch over
nnoremap <leader>w <C-w>v<C-w>

" Maxmimise buffer
nnoremap <leader>o <C-w><C-o>

" Open files
nnoremap <leader>e :e
nnoremap <leader>3 :vsplit 

" Switch between two buffers, Change between two buffers
nmap <C-Tab> :bn<CR>
nmap <C-S-Tab> :bp<CR>
nnoremap <leader>h <c-w>t<c-w>H
nnoremap <left> <c-w><left>
nnoremap <up> <c-w><up>
nnoremap <down> <c-w><down>
nnoremap <right> <c-w><right>

" Indentations settings
vnoremap < <gv  " Select indenetation
vnoremap > >gv  " Same as above

" Enable folding with sapcebar
nnoremap <space> za

" For local replace
nnoremap gr gd[{V%::s/<C-R>///gc<left><left><left>

" For global replace
nnoremap gR gD:%s/<C-R>///gc<left><left><left>

" Make return select in completion
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
    "return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
    " For no inserting <CR> key.
    return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction

" -----------------------------------------------------------------------------
" Text settings----------------------------------------------------------------
" -----------------------------------------------------------------------------
" Text file format options:
"   n - Recognize numbered lists
"   2 - Paragraph-style indents
augroup FORMAT_OPTIONS_TXT
    au!
    au BufNewFile,BufRead *.txt,*.md,*.tex setlocal fo+=n2
augroup END

" Enable spell check for text files
augroup SPELLCHECK
    au!
    au BufNewFile,BufRead *.txt,*.tex setlocal spell
    au Filetype markdown,help setlocal spell
augroup END

"------------------------------------------------------------------------------
" Python-----------------------------------------------------------------------
"------------------------------------------------------------------------------
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
let python_highlight_all=1

" Jedi vim--------------------------------------------------------------------
let g:jedi#use_splits_not_buffers = "right"
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#show_call_signatures = "1"
let g:jedi#goto_command = "\e,"
let g:jedi#goto_assignments_command = "<leader>g"
let g:jedi#goto_definitions_command = ""
let g:jedi#documentation_command = "\e."
let g:jedi#usages_command = "<leader>n"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "<leader>r"

"------------------------------------------------------------------------------
" Haskell----------------------------------------------------------------------
"------------------------------------------------------------------------------
au FileType haskell setl sw=2 sts=2 et omnifunc=necoghc#omnifunc

" Ghc-mod----------------------------------------------------------------------
let g:haskellmode_completion_ghc = 1

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

"------------------------------------------------------------------------------
" LaTeX -----------------------------------------------------------------------
"------------------------------------------------------------------------------
" Change auto detect of .tex to plaintex
let g:tex_flavor = 'latex'

" IndentLine-------------------------------------------------------------------
let g:indentLine_char = '¦'

" Syntastic--------------------------------------------------------------------
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

nnoremap <leader>s :SyntasticToggleMode<CR>

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_python_checkers = ['pyflakes']
let g:syntastic_enable_signs = 1
let g:syntastic_enable_balloons = 0
let g:syntastic_enable_highlighting = 0

" Autoformat-------------------------------------------------------------------
let g:formatdef_hindent = '"hindent --indent-size 2"'
let g:formatters_haskell = ['hindent']
let g:formatdef_yapf = '"yapf"'
let g:formatters_python = ['yapf']
let g:formatterpath = [
            \ '/home/abhinav/.local/bin',
            \ '/home/abhinav/.cargo/bin',
            \ '/home/abhinav/anaconda3/bin/']

noremap <F3> :Autoformat<CR>

fun! AutoFormatExceptPython()
    " Separate filetypes using "\|"
    if &ft =~ 'snippets\|python\|vim'
        return
    endif
    Autoformat
endfun

au BufWrite * call AutoFormatExceptPython()

" AutoPairs--------------------------------------------------------------------
let g:AutoPairsFlyMode=0
let g:AutoPairsShortcutBackInsert = '<C-c><C-a>'
let g:AutoPairsShortcutJump = '%'

" EasyAlign--------------------------------------------------------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Stratify---------------------------------------------------------------------
let g:startify_list_order = ['files', 'dir', 'bookmarks', 'sessions',
            \ 'commands']
let g:startify_relative_path = 0
let g:startify_files_number = 8
let g:startify_change_to_dir = 1

" Vim Polyglot-----------------------------------------------------------------
"let g:polyglot_disabled = ['haskell', 'python']

" Vim-Over---------------------------------------------------------------------
nnoremap <silent> <bslash> <esc>:OverCommandLine<cr>%s/
vnoremap <silent> <bslash> <esc>gv:OverCommandLine<cr>s/

" Nerd Tree--------------------------------------------------------------------
nnoremap <leader>n :NERDTreeToggle<CR>

" Ultisnips-------------------------------------------------------------------
fun! s:my_tab_expand()
    return pumvisible() ? "\<C-k>" : <tab>
endfun

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsEditSplit="vertical"

" Super Tab-------------------------------------------------------------------
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = '<c-x><c-o>'
let g:SuperTabCompletionContexts = ['s:ContextText', 's:ContextDiscover']
let g:SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']
let g:SuperTabContextDiscoverDiscovery =
            \ [ "&omnifunc:<c-x><c-o>","&completefunc:<c-x><c-u>"]
let g:SuperTabLongestHighlight = 0
let g:SuperTabNoCompleteAfter = ['^', ',', '\s']
let g:SuperTabLongestEnhanced = 1
let g:SuperTabClosePreviewOnPopupClose = 1
let g:SuperTabRetainCompletionDuration = 'insert'

" Neocomplete------------------------------------------------------------------
let g:neocomplete#max_list=15
let g:neocomplete#enable_auto_select=0
let g:neocomplete#enable_auto_delimiter=0
let g:neocomplete#enable_auto_close_preview=1
let g:neocomplete#disable_auto_complete = 1

execute "set <M-n>=\en"
inoremap <M-n> <Down>
"inoremap <M-n>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" -----------------------------------------------------------------------------
" C/C++ -----------------------------------------------------------------------
" -----------------------------------------------------------------------------
let g:clang_library_path='/home/abhinav/.clang/lib/libclang.so.3.9'
