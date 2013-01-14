"=====================================
" GENERAL SETTINGS
"=====================================
    " drop vi shit
    set nocompatible

    " do not making backup before editing file
    set nobackup

    " do not using swapfile
    set noswapfile

    " number of lines that are remembered
    set history=100

    " automatically reload file, when it's changed outside of Vim
    set autoread

    " Maximum number of changes that can be undone
    set undolevels=100

    "  list of character encodings considered when starting to edit an existing file
    set fileencodings=utf-8

    call pathogen#infect()
    filetype plugin indent on

    " share clipboard among instances
    set clipboard=unnamed

    " prevents some security exploits having to do with modelines in files
    set modelines=0

    set ttyfast

    " enable neocomplcache
    let g:neocomplcache_enable_at_startup = 1
    let g:neocomplcache_force_overwrite_completefunc = 1

"=====================================
" FUNCTIONS
"=====================================
    " chmod +х to scripts
    function ModeChange()
        if getline(1) =~ '^#!'
            silent !chmod a+x <afile>
        endif
    endfunction

"=====================================
" APPEARANCE SETTINGS
"=====================================
    syntax on

    " Syntax coloring lines that are too long just slows down the world
    set synmaxcol=2048

    set background=dark

    " Show the current mode
    set showmode

    set statusline=#%n\ %f\ %y\ %{'['.(&fenc!=''?&fenc:&enc).':'.']'}\ %m\ %r\ %=%l,%c/%L

    " Show the current command in the lower right corner
    set showcmd
    " Show status line for all files
    set laststatus=2
    " Show matching brackets
    set showmatch
    " Do case insensitive matching
    set ignorecase
    " Ignore case when the pattern contains lowercase letters only.
    set smartcase

    set cursorline

    colorscheme neverland-darker
    " autocmd BufEnter *.hs colorscheme elflord

    " enable autocomplete
    set wildmenu
    set wildmode=list:longest,full

    " make vim message not to annoy
    set shortmess=aoOIT

    " always report about changed lines
    set report=0

    " Don't update the display while executing macros
    set lazyredraw

    " Nice-looking vertical separator
    set fillchars=vert:│

"=====================================
" MISC SETTINGS
"=====================================
    set tabstop=4
    set shiftwidth=4
    set softtabstop=4
    set expandtab
    set autoindent

    " matched string is highlighted.
    set incsearch

    " lines to scroll when cursor leaves screen
    set scrolljump=10
    " lines before screen edge to scroll
    set scrolloff=1000

    " Don't searches wrap around the end of the file
    set nowrapscan
    " enable wrap
    set wrap
    " wrap backspace, space, h, l, <-, ->, [ and ] keys
    set whichwrap=b,s,<,>,[,],l,h
    " set word-wrap, not symbol-wrap
    set linebreak

    set colorcolumn=80

    " don’t worry, I’m using two spaces like a sane person (http://stevelosh.com/blog/2012/10/why-i-two-space/)
    set cpo+=J

" some Haskell properties
    hi hsNiceOperator ctermfg=none ctermbg=black
    autocmd FileType haskell setlocal expandtab shiftwidth=2 softtabstop=2
    " use ghc functionality for haskell files
    autocmd Bufenter *.hs compiler ghc
    " configure browser for haskell_doc.vim
    let g:haddock_browser = "firefox-bin"
    " syntax rules
    let hs_highlight_delimiters = 1
    let hs_highlight_boolean = 1
    let hs_highlight_debug = 1
    let hs_highlight_more_types = 1
    let hs_highlight_types = 1

    let g:syntastic_mode_map = { 'mode': 'active',
    \ 'active_filetypes': [],
    \ 'passive_filetypes': ['haskell'] }

    " Use ruby syntax for capfiles
    autocmd Bufenter *.[cC]apfile,[cC]apfile setfiletype ruby
    autocmd FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2

    " set XML style
    " let g:xml_syntax_folding=1
    autocmd FileType xml setlocal expandtab shiftwidth=2 softtabstop=2 foldmethod=syntax

    " ingore whitespaces (vimdiff)
    set diffopt+=iwhite " ignore whitespaces

    " highlight trailing spaces
    set list!
    set listchars=tab:»»,trail:∘
    " disable matches in help buffers
    autocmd BufEnter,FileType help call clearmatches()

    " Automatically chmod +x
    autocmd BufWritePost * call ModeChange()

    " preserve undo actions even after file has closed
    set undolevels=1000
    set undofile

    " vim-latex settings
    set grepprg=grep\ -nH\ $*
    let g:tex_flavor='latex'

    " save after losing focus
    autocmd FocusLost * :wa

    " ghc-mod customs
    let g:ghcmod_ghc_options = ['-Wall','-fno-warn-missing-signatures']
    autocmd bufwritepost *.hs :GhcModCheck

"=====================================
" GVIM SETTINGS
"=====================================
    set guifont=Terminus\ 8
    set guioptions=ac

"=====================================
" COMMANDS
"=====================================
    command -nargs=+ Ghc !ghc -e <q-args>

"=====================================
" KEY MAPPING SETTINGS
"=====================================
    " System default for mappings is now the "," character
    let mapleader = ","

    " remove search highlighting
    nmap <Leader>nh :nohlsearch<CR>

    " set paste mode
    set pastetoggle=<F3>

    " remove trailing whitespaces
    noremap <Leader>rw :%s/ \+$//c<CR>

    noremap <space> <C-d>
    noremap zz :q!<CR>

    noremap k gk
    noremap j gj

    " perd tree
    nmap <silent> <c-u> :Unite file buffer<CR>

    " save as root with w!!
    cmap w!! w !sudo tee % > /dev/null

    " Edit the vimrc file
    nmap <silent> <Leader>sv :so $MYVIMRC<CR>

    " Close current buffer
    nmap <Leader>d :bd<CR>

    " Close all buffers except current
    nmap <Leader>a :BufOnly<CR>

    " disable arrow keys in insert and normal modes
    inoremap <up> <nop>
    inoremap <down> <nop>
    inoremap <left> <nop>
    inoremap <right> <nop>
    nnoremap <up> <nop>
    nnoremap <down> <nop>
    nnoremap <left> <nop>
    nnoremap <right> <nop>

    " leave insert mode by jj
    inoremap jj <ESC>

    " Haskell maps
    autocmd FileType haskell nnoremap <buffer> <F3> :GhcModLint<CR>
    autocmd FileType haskell nnoremap <buffer> <F4> :GhcModCheck<CR>
    autocmd FileType haskell nnoremap <buffer> <F5> :HdevtoolsType<CR>
    autocmd FileType haskell nnoremap <buffer> <F6> :HdevtoolsClear<CR>
    autocmd FileType haskell nnoremap <Leader>sh :%!stylish-haskell<CR>
