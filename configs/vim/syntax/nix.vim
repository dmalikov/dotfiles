" Vim syntax file
" Language:    Nix expressions
"	       http://www.nixos.org/nix
" Maintainer:  Andres Loeh <mail@andres-loeh.de>
" Last update: Fri, 06-Jun-2008
" Filenames:   .nix

syntax case match

syntax keyword nixConditional if then else
syntax keyword nixAssert      assert
syntax keyword nixLet         let in
syntax keyword nixScope       with inherit
syntax keyword nixRec         rec

highlight link nixConditional Conditional
highlight link nixAssert      Exception
highlight link nixLet         Keyword
highlight link nixScope       Keyword
highlight link nixRec         Repeat

syntax match nixOperator "?\|//\|++\|:\|\."
syntax match nixDelim /[\[\]]/ 

highlight link nixOperator    Operator
highlight link nixDelim       Delimiter

" syntax match nixIdentifier /\<[a-zA-Z_][a-zA-Z0-9_']*\>/
syntax match nixComment    /#.*/
syntax region nixComment start="/\*" end="\*/"

highlight link nixIdentifier  Identifier
highlight link nixComment     Comment

syntax region nixString start=/"/ skip=/\\"/ end=/"/ contains=nixEscape
syntax region nixString start=/''/ end=/''/
syntax match nixURI  /[a-zA-Z][a-zA-Z0-9\+-\.]*:[a-zA-Z0-9%/\?:@&=\+\$,_\.!~\*'-]\+/
syntax match nixPath "[a-zA-Z0-9._\+-]*\(/[a-zA-Z0-9._\+-]\+\)\+"
syntax region nixEscape start=/\${/ end=/}/ contained contains=ALLBUT,nixEscape,nixDelim
syntax region nixBlock  start=/\([^$]\|^\){/ end=/}/ contained contains=ALL

highlight link nixString      String
highlight link nixURI         Constant
highlight link nixPath        Constant

let b:current_syntax = "nix"

" eof
