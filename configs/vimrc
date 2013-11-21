" auto indent
"set ai
"set smartindent
set cindent
" show line number
set number
" expande tab
set expandtab
set tabstop=4
set shiftwidth=4
" no folding code
set nofoldenable
" highlight search
set hlsearch
" enbale syntax highlight
syntax on
" align function arguments
set cino+=(0

" filter the unwanted files
let NERDTreeIgnore=['\.o$', '\.lo$', '\.la$', 'tags', 'cscope.*']

" auto launch NERDTree
function! AutoNERDTree()
    if 0 == argc()
        NERDTree
    end
endfunction

autocmd VimEnter * call AutoNERDTree()

" ident 2 spaces when doing ruby
"autocmd FileType rb setlocal shiftwidth=2 tabstop=2

" shotcut for taglist
nnoremap <C-l> :TlistToggle<cr>
vnoremap <C-l> :TlistToggle<cr>

" auto update tags
function! UpdateTags()
    " only update the tags when we've created one
    if !filereadable("tags")
        return
    endif

    " use the path relative to pwd
    let _file = expand("%")
    let _cmd = 'ctags --append "' . _file . '"'
    let _ret = system(_cmd)
    unlet _cmd
    unlet _file
    unlet _ret
endfunction

autocmd BufWritePost *.cpp,*.h,*.c call UpdateTags()

" easy copy to/paste from system clipboard
nnoremap <C-y> "+y
vnoremap <C-y> "+y
nnoremap <C-p> "+gP
vnoremap <C-p> "+gp

" show function names in c
function! ShowFuncName()
    let lnum = line(".")
    let col = col(".")
    echohl ModeMsg
    echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bW'))
    echohl None
    call search("\\%" . lnum . "l" . "\\%" . col . "c")
endfunction
map ,f :call ShowFuncName() <CR>

" highlight unwanted spaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" shotcuts for fuzzyfinder
map ff <esc>:FufFile **/<cr>
map ft <esc>:FufTag<cr>
"map <silent> <c-\> :FufTag! <c-r>=expand('<cword>')<cr><cr>

" hight lines longer than 80 characters
if exists('+colorcolumn')
  set colorcolumn=80
else
  au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
endif

" show current function name
fun! ShowFuncName()
  let lnum = line(".")
  let col = col(".")
  echohl ModeMsg
  echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bW'))
  echohl None
  call search("\\%" . lnum . "l" . "\\%" . col . "c")
endfun
map F :call ShowFuncName() <CR>

" make hjkl movements accessible from insert mode via the <Alt> modifier key
inoremap <A-h> <C-o>h
inoremap <A-j> <C-o>j
inoremap <A-k> <C-o>k
inoremap <A-l> <C-o>l
" quick input closing bracket
inoremap {<CR> {<CR>}<ESC>O
