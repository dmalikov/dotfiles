syn cluster hsRegions add=hsImport,hsLineComment,hsBlockComment,hsPragma
syn cluster hsRegions add=cPreCondit,cCppOut,cCppOut2,cCppSkip
syn cluster hsRegions add=cIncluded,cDefine,cPreProc,cComment,cCppString

syn match tab display "\t" containedin=@hsRegions
hi link tab Error
syn match trailingWhite display "[[:space:]]\+$" containedin=@hsRegions
hi link trailingWhite Error

" match unicode symbols as original ones
syn match hsVarSym /→/
syn match hsVarSym /←/
syn match hsVarSym /⇒/
syn match hsVarSym /∷/

setlocal omnifunc=necoghc#omnifunc
