#lang brag
at : /"(" /"." at-content at* /")"
@at-content : cyclenode | treelabel | generalization
cyclenode : /"!CY" NUMBER
treelabel : [NUMBER /"."] selectionless-abstract-conjunction [abstract-substitution knowledge]
          | NUMBER /"." abstract-conjunction-selection [precedence-list] [abstract-substitution knowledge]
selectionless-abstract-conjunction : /"□"
                                   | nonempty-selectionless-abstract-conjunction
@nonempty-selectionless-abstract-conjunction : abstract-conjunct (/"," abstract-conjunct)*
@abstract-conjunct : abstract-atom
                   | multi-abstraction
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-term : abstract-function | abstract-variable | abstract-list
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-variable : abstract-a-variable | abstract-g-variable
abstract-a-variable : AVAR-A
abstract-g-variable : AVAR-G
abstract-list : /"[" [abstract-term abstract-term-tail ["|" (abstract-list | abstract-variable)]] /"]" # can't cut comma or separator (need to distinguish during expansion)
@abstract-term-tail : ("," abstract-term)*
multi-abstraction : /"multi" /"(" parameterized-abstract-conjunction /"," BOOLEAN /"," init /"," consecutive /"," final  /")"
parameterized-abstract-conjunction : /"(" parameterized-abstract-atom (/"," parameterized-abstract-atom)* /")"
parameterized-abstract-atom : SYMBOL [/"(" parameterized-abstract-term (/"," parameterized-abstract-term)* /")"]
@parameterized-abstract-term : parameterized-abstract-function | parameterized-abstract-variable | parameterized-abstract-list
parameterized-abstract-function : SYMBOL [/"(" parameterized-abstract-term (/"," parameterized-abstract-term)* /")"]
@parameterized-abstract-variable : parameterized-abstract-a-variable | parameterized-abstract-g-variable
parameterized-abstract-a-variable : /"a" /"<" NUMBER /"," (NUMBER|"i+1"|"i"|"L") /"," NUMBER /">"
parameterized-abstract-g-variable : /"g" /"<" NUMBER /"," (NUMBER|"i+1"|"i"|"L") /"," NUMBER /">"
parameterized-abstract-list : /"[" [parameterized-abstract-term parameterized-abstract-term-tail ["|" (parameterized-abstract-list | parameterized-abstract-variable)]] /"]"
@parameterized-abstract-term-tail : ("," parameterized-abstract-term)*
init : /"{" [init-pair (/"," init-pair)*] /"}"
init-pair : parameterized-abstract-variable /"/" abstract-term
consecutive : /"{" [consecutive-pair (/"," consecutive-pair)*] /"}"
consecutive-pair : parameterized-abstract-variable /"/" parameterized-abstract-term
final : /"{" [final-pair (/"," final-pair)*] /"}"
final-pair : parameterized-abstract-variable /"/" abstract-variable
abstract-conjunction-selection : [selectionless-abstract-conjunction /","] /"*" abstract-conjunct /"*" [/"," selectionless-abstract-conjunction]
precedence-list : /"[" [precedence (/"," precedence)*] /"]"
precedence : abstract-atom /"<" abstract-atom
abstract-substitution : /"{" [abstract-substitution-pair (/"," abstract-substitution-pair)*] /"}"
abstract-substitution-pair : abstract-variable /"/" abstract-term
knowledge : rule | fullai-rule
@rule : fact | clause
fact : atom /"."
clause : atom /":-" conjunction /"."
conjunction : atom (/"," atom)*
atom : SYMBOL [/"(" term (/"," term)* /")"]
@term : variable | function | lplist
variable : VARIABLE-IDENTIFIER
function : SYMBOL [/"(" term (/"," term)* /")"]
         | NUMBER
lplist : /"[" [term term-tail ["|" (lplist | variable)]] /"]"
@term-tail : ("," term)*
fullai-rule : abstract-atom /"->" abstract-substitution
generalization : /"!GEN" selectionless-abstract-conjunction generalized-index-ranges
               | /"!GEN" NUMBER /"." abstract-conjunction-selection [precedence-list] generalized-index-ranges
generalized-index-ranges : /"(" generalized-index-range+ /")"
generalized-index-range : /"(" NUMBER NUMBER /")"