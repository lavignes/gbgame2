syn clear
syn case ignore

syn match gbasmIdentifier "[a-z_\.][a-z0-9_\.]*"
syn match gbasmGlobalLabel "[a-z_][a-z0-9_\.]*::\?"
syn match gbasmLocalLabel "\.[a-z_][a-z0-9_]*::\?"

syn keyword gbasmRegister a b c d e h l af bc de hl sp
syn keyword gbasmConditions z c nc nz

syn match gbasmOperator display "\%(+\|-\|/\|*\|\^\|\~\|&\||\|!\|>\|<\|%\|=\)=\?"
syn match gbasmOperator display "&&\|||\|<<\|>>\|\~>"

syn keyword gbasmOpcode ld ldd ldi ldh push pop add adc sub sbc and or xor cp inc dec swap
syn keyword gbasmOpcode daa cpl ccf scf nop halt stop di ei rlca rla rrca rra rlc rl rrc rr
syn keyword gbasmOpcode sla sra srl bit set res jp jr call rst ret reti

syn match gbasmDirective "\\byte"
syn match gbasmDirective "\\word"
syn match gbasmDirective "\\section"
syn match gbasmDirective "\\include"
syn match gbasmDirective "\\if"
syn match gbasmDirective "\\ifdef"
syn match gbasmDirective "\\ifndef"
syn match gbasmDirective "\\end"
syn match gbasmDirective "\\space"
syn match gbasmDirective "\\macro"
syn match gbasmDirective "\\tag"
syn match gbasmDirective "\\len"
syn match gbasmDirective "\\loop"
syn match gbasmDirective "\\fail"
syn match gbasmDirective "\\warn"
syn match gbasmDirective "\\struct"
syn match gbasmDirective "\\create"
syn match gbasmDirective "\\[0-9]\+"
syn match gbasmDirective "\\narg"
syn match gbasmDirective "\\shift"
syn match gbasmDirective "\\uniq"
syn match gbasmDirective "\\join"
syn match gbasmDirective "\\break"

syn match gbasmComment ";.*" contains=gbasmTodo
syn match gbasmDocComment ";;.*" contains=gbasmTodo
syn keyword gbasmTodo contained todo fixme xxx warning danger note notice bug
syn match gbasmEscape contained "\\."
syn region gbasmString start=+"+ end=+"+ contains=gbasmEscape
syn region gbasmChar start=+'+ end=+'+ contains=gbasmEscape

syn match gbasmNumber "[0-9][0-9_]*"
syn match gbasmNumber "\$[0-9a-fA-F_]\+"
syn match gbasmNumber "%[01_]\+"

syn case match

hi def link gbasmComment      Comment
hi def link gbasmDocComment   SpecialComment
hi def link gbasmNumber       Number
hi def link gbasmString	      String
hi def link gbasmChar         Character
hi def link gbasmIdentifier   Identifier
hi def link gbasmRegister     SpecialChar
hi def link gbasmConditions   SpecialChar
hi def link gbasmOpcode       Keyword
hi def link gbasmEscape       SpecialChar
hi def link gbasmDirective    PreProc
hi def link gbasmGlobalLabel  Function
hi def link gbasmLocalLabel   Function
hi def link gbasmTodo         Todo

let b:current_syntax = "gbasm"
set ts=4
set sw=4
set et

