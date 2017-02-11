#lang brag
at : /"(" at-content /")"

cyclenode : /"!CY" NUMBER
treelabel : selectionless-abstract-conjunction [substitution knowledge]
          | NUMBER /"." abstract-conjunction-selection [precedence-list] [substitution knowledge]
selectionless-abstract-conjunction : /"□"
                                   | nonempty-selectionless-abstract-conjunction
@nonempty-selectionless-abstract-conjunction : abstract-conjunct (/"," abstract-conjunct)*
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-term : abstract-function | abstract-variable | abstract-list
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-variable : abstract-a-variable | abstract-g-variable
abstract-a-variable : AVAR-A
abstract-g-variable : AVAR-G
abstract-list : /"[" [abstract-term abstract-term-tail ["|" (abstract-list | abstract-variable)]] /"]" # can't cut comma or separator (need to distinguish during expansion)
@abstract-term-tail : ("," abstract-term)*
@abstract-conjunct : abstract-atom
                   | multi-abstraction
multi-abstraction : /"multi" /"(" parameterized-abstract-conjunction /"," BOOLEAN /"," init /"," consecutive /"," final  /")"
parameterized-abstract-conjunction : /"(" parameterized-abstract-atom (/"," parameterized-abstract-atom)* /")"
abstract-conjunction-selection : [selectionless-abstract-conjunction /","] selected-abstract-conjunct [/"," selectionless-abstract-conjunction]
selected-abstract-conjunct : /"*" abstract-conjunct /"*"
precedence-list : /"[" [precedence (/"," precedence)*] /"]"
precedence : abstract-atom /"<" abstract-atom
substitution : /"{" [substitution-pair (/"," substitution-pair)*] /"}"
substitution-pair : abstract-variable /"/" abstract-term
@knowledge : rule | fullai-rule
fullai-rule : abstract-atom /"->" substitution
@rule : fact | clause
fact : atom /"."
clause : atom /":-" conjunction
conjunction : atom (/"," atom)*
atom : SYMBOL [/"(" term (/"," term)* /")"]
@term : variable | function | list
variable : VARIABLE-IDENTIFIER
function : SYMBOL [/"(" term (/"," term)* /")"]
         | NUMBER
list : /"[" [term term-tail ["|" (list | variable)]] /"]"
@term-tail : ("," term)*


parameterized-abstract-atom : SYMBOL
init : /"{" /"}"
consecutive : /"{" /"}"
final : /"{" /"}"
@at-content : cyclenode | treelabel