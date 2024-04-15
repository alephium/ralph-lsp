" Vim syntax file
" Language: Ralph

syn keyword   ralphConditional if else
syn keyword   ralphRepeat for while

syn keyword   ralphStucture    Interface TxScript Contract AssetScript enum event extends skipwhite skipempty nextgroup=ralphInstanceDeclaration
syn keyword   ralphKeyword     fn nextgroup=ralphFunctionName skipwhite skipempty
syn keyword   ralphKeyword     pub nextgroup=ralphPubScope skipwhite skipempty
syn keyword   ralphKeyword     return implements 
syn keyword   ralphKeyword     let mut const nextgroup=ralphNameDefinition skipwhite skipempty
syn keyword   ralphStorage     Abstract

syn match ralphInstanceDeclaration /\<[_\.A-Za-z0-9$]\+\>/ contained 
hi link ralphInstanceDeclaration Special

syn match ralphNameDefinition /\<[_A-Za-z0-9$]\+\>/ contained 
hi link ralphNameDefinition Function

syn match ralphFunction /[A-Za-z0-9]\+!/ 
hi link ralphFunction Function

syn match     ralphFunctionName    "\%(r#\)\=\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn keyword   ralphType        Bool I256 U256 Address ByteVec
syn keyword   ralphBoolean     true false

syn match     ralphOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"

syn match     ralphArrowCharacter display "->"

syn region ralphCommentLine                                                  start="//"                      end="$" 

hi def link ralphNumber        Number
hi def link ralphBoolean       Boolean
hi def link ralphEnum          ralphType
hi def link ralphEnumVariant   ralphConstant
hi def link ralphConstant      Constant
hi def link ralphOperator      Operator
hi def link ralphStucture      Structure
hi def link ralphKeyword       Keyword
hi def link ralphRepeat        Conditional
hi def link ralphConditional   Conditional
hi def link ralphFunctionName  Function
hi def link ralphCommentLine   Comment
hi def link ralphType          Type
hi def link ralphStorage       StorageClass

let b:current_syntax = "ralph"
