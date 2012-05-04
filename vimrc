"=====================================
" GENERAL SETTINGS
"=====================================
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

"=====================================
" APPEARANCE SETTINGS
"=====================================
    syntax on

    set background=dark

    " Ignore case when the pattern contains lowercase letters only.
    set smartcase

    " always show current cursor position
    set ruler
    set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%)
    " Show the current mode
    set showmode
    set statusline=%F\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]
    " Show the current command in the lower right corner
    set showcmd
    " Show status line for all files
    set laststatus=2
    " Show matching brackets
    set showmatch
    " Do case insensitive matching
    set ignorecase

    " line numbers
    set number
    set numberwidth=3
    highlight LineNr ctermfg=yellow ctermbg=black

    colorscheme moria
    " autocmd BufEnter *.hs colorscheme elflord

"=====================================
" MISC SETTINGS
"=====================================
    set tabstop=4
    set shiftwidth=4
    set softtabstop=4
    set expandtab
    set autoindent

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

    " set g default option to any s///
    set gdefault

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

    " Use ruby syntax for capfiles
    autocmd Bufenter Capfile setfiletype ruby
    autocmd FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2

    " set XML style
    " let g:xml_syntax_folding=1
    autocmd FileType xml setlocal expandtab shiftwidth=2 softtabstop=2 foldmethod=syntax

    " ingore whitespaces (vimdiff)
    set diffopt+=iwhite " ignore whitespaces

    " highlight trailing spaces
    set list!
    set listchars=tab:»»,trail:·
    " disable matches in help buffers
    autocmd BufEnter,FileType help call clearmatches()

    " Automatically chmod +x Shell and Python scripts
    autocmd BufWritePost *.sh !chmod +x %
    autocmd BufWritePost *.py !chmod +x %

    " enable filetype detection, plus loading of filetype plugins
    source ~/.vim/plugin/matchit.vim
    filetype plugin on

    " preserve undo actions even after file has closed
    set undolevels=1000
    set undofile

    " vim-latex settings
    set grepprg=grep\ -nH\ $*
    let g:tex_flavor='latex'
    filetype indent on

"=====================================
" COMMANDS
"=====================================
    command -nargs=+ Ghc !ghc -e <q-args>

"=====================================
" KEY MAPPING SETTINGS
"=====================================
    nmap <F2> :nohlsearch<CR>
    set pastetoggle=<F3>

    " remove trailing whitespaces
    noremap <F4> :%s/ \+$//gc

    noremap <space> <C-d>
    noremap zz :q!<CR>

    noremap k <c-y>
    noremap j <c-e>

    " switching between buffers
    noremap <Esc>[5^ :bnext<CR>
    noremap <C-PageUp> :bnext<CR>
    noremap <Esc>[6^ :bprevious<CR>
    noremap <C-PageDown> :bprevious<CR>

    " exec Ghc
    noremap <C-g> :Ghc
