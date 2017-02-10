#lang brag
at : /"(" at-content /")"

cyclenode : /"!CY" NUMBER
treelabel : selectionless-abstract-conjunction
          | NUMBER /"." abstract-conjunction-selection [precedence-list] substitution knowledge
/selectionless-abstract-conjunction : /"□"
                                    | nonempty-selectionless-abstract-conjunction
@nonempty-selectionless-abstract-conjunction : abstract-conjunct (/"," abstract-conjunct)*
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-term : abstract-function | abstract-variable | abstract-list
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-variable : abstract-a-variable | abstract-g-variable
abstract-a-variable : AVAR-A
abstract-g-variable : AVAR-G
abstract-list : /"[" [abstract-term term-tail ["|" (abstract-list | abstract-variable)]] /"]" # can't cut comma or separator (need to distinguish during expansion)
@term-tail : ("," abstract-term)*
@abstract-conjunct : abstract-atom
                   | multi-abstraction
multi-abstraction : /"multi" /"(" parameterized-abstract-conjunction /"," BOOLEAN /"," init /"," consecutive /"," final  /")"
parameterized-abstract-conjunction : /"(" parameterized-abstract-atom (/"," parameterized-abstract-atom)* /")"






parameterized-abstract-atom : SYMBOL
init : /"{" /"}"
consecutive : /"{" /"}"
final : /"{" /"}"
abstract-conjunction-selection : [selectionless-abstract-conjunction] /"*" abstract-conjunct /"*" [selectionless-abstract-conjunction]
precedence-list : /"[" /"]"
substitution : /"{" /"}"
knowledge : rule
@rule : fact
fact : atom /"."
atom : SYMBOL

@at-content : cyclenode | treelabel