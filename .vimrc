" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Tabstops are 4 spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent

set nobackup
set noswapfile

" Ignore case when the pattern contains lowercase letters only.
set smartcase

" Maximum number of changes that can be undone
set undolevels=100

"  This is a list of character encodings considered when starting to edit an existing file
set fileencodings=utf-8,cp1251,koi8-r,cp866

" Use visual bell instead of beeping
set visualbell

" Don't searches wrap around the end of the file
set nowrapscan

" FUSK NO ARROWS AGAIN
set whichwrap=b,s,<,>,[,],l,h

" Set syntax folding method
set fdm=syntax

set foldlevel=100000

set statusline=%F\ %m\ %r\ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

" Show status line for all files
set laststatus=2

" Show the current command in the lower right corner
set showcmd

" Show the current mode
set showmode


" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set showcmd		" Show (partial) command in status line.
"set showmatch		" Show matching brackets.
"set ignorecase		" Do case insensitive matching
"set smartcase		" Do smart case matching
set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
set hlsearch
"set linebreak

nmap <F2> :nohlsearch<CR>
set pastetoggle=<F3>
nmap <F4> :exec 'windo '.line('.').'d' <cr>

" Encoding change menu
set wildmenu
set wcm=<Tab>
menu Encoding.utf-8 :e ++enc=utf8 <CR>
menu Encoding.windows-1251 :e ++enc=cp1251<CR>
menu Encoding.koi8-r :e ++enc=koi8-r<CR>
menu Encoding.cp866 :e ++enc=cp866<CR>
map <F6> :emenu Encoding.<TAB>

" Source a global configuration file if available
" XXX Deprecated, please move your changes here in /etc/vim/vimrc
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

" highlight trailing spaces
" au BufNewFile,BufRead * let b:mtrailingws=matchadd('ErrorMsg', '\s\+$', -1)
" highlight tabs between spaces
au BufNewFile,BufRead * let b:mtabbeforesp=matchadd('ErrorMsg', '\v(\t+)\ze( +)', -1)
au BufNewFile,BufRead * let b:mtabaftersp=matchadd('ErrorMsg', '\v( +)\zs(\t+)', -1)
" disable matches in help buffers
au BufEnter,FileType help call clearmatches()

" hightlight over 80 symbols
" :match ErrorMsg '\%>80v.\+'
" :au BufWinEnter * let w:m1=matchadd('Search', '\%<81v.\%>77v', -1)
" :au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" colorscheme blackboard 
colorscheme railscasts

" Смена кодировок
" http://www.opennet.ru/base/rus/vim_rus_text.txt.html


" <F8> переоткрывает файл в разных кодировках через :e ++enc=кодировка
map <F8> :execute RotateEnc()<CR>
vmap <F8> <C-C><F8>
imap <F8> <C-O><F8>
let b:encindex=0
function! RotateEnc()
let y = -1
while y == -1
let encstring = "#koi8-r#cp1251#8bit-cp866#utf-8#ucs-2le#"
let x = match(encstring,"#",b:encindex)
let y = match(encstring,"#",x+1)
let b:encindex = x+1
if y == -1
let b:encindex = 0
else
let str = strpart(encstring,x+1,y-x-1)
return ":e ++enc=".str
endif
endwhile
endfunction

" <Shift+F8> тоже что и <F8>, но предварительно меняет внутреннюю
" кодировку vim на равную кодировке файла. Это нужно когда vim умничает и
" команда :e ++enc=кодировка для него не указ. Минус этого метода в том,
" что когда внутренняя кодировка равна 8bit-cp866, то vim некоторые
" русские буквы показывает неверно, но именно показывает, поскольку если
" конвертировать, то ничего не портится (сравнить можно с результатом
" работы <F8>).
map <S-F8> :execute ForceRotateEnc()<CR>
vmap <S-F8> <C-C><S-F8>
imap <S-F8> <C-O><S-F8>
let b:encindex=0
function! ForceRotateEnc()
let y = -1
while y == -1
let encstring = "#koi8-r#cp1251#8bit-cp866#utf-8#ucs-2le#"
let x = match(encstring,"#",b:encindex)
let y = match(encstring,"#",x+1)
let b:encindex = x+1
if y == -1
let b:encindex = 0
else
let str = strpart(encstring,x+1,y-x-1)
:execute "set encoding=".str
return ":e ++enc=".str
endif
endwhile
endfunction

" <Ctrl+F8> меняет кодировку файла, то есть после его сохранения он будет
" конвертирован
map <C-F8> :execute RotateFEnc()<CR>
vmap <C-F8> <C-C><C-F8>
imap <C-F8> <C-O><C-F8>
let b:fencindex=0
function! RotateFEnc()
let y = -1
while y == -1
let encstring = "#koi8-r#cp1251#8bit-cp866#utf-8#ucs-2le#"
let x = match(encstring,"#",b:fencindex)
let y = match(encstring,"#",x+1)
let b:fencindex = x+1
if y == -1
let b:fencindex = 0
else
let str = strpart(encstring,x+1,y-x-1)
return ":set fenc=".str
endif
endwhile
endfunction

:source ~/.vim/plugin/matchit.vim 

" Automatically chmod +x Shell and Python scripts
au BufWritePost   *.sh             !chmod +x %
au BufWritePost   *.py             !chmod +x %
