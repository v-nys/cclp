#lang brag
at : /"(" at-content /")"
@at-content : cyclenode | treelabel

cyclenode : /"!CY" NUMBER
treelabel : selectionless-abstract-conjunction
          | NUMBER /"." abstract-conjunction-selection [precedence-list] substitution knowledge
selectionless-abstract-conjunction : /"□"
                                   | nonempty-selectionless-abstract-conjunction
@nonempty-selectionless-abstract-conjunction : abstract-conjunct (/"," abstract-conjunct)*
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-variable : abstract-a-variable | abstract-g-variable
abstract-a-variable : AVAR-A
abstract-g-variable : AVAR-G




@abstract-term : abstract-function | abstract-variable
@abstract-conjunct : abstract-atom
abstract-conjunction-selection : [selectionless-abstract-conjunction] /"*" abstract-conjunct /"*" [selectionless-abstract-conjunction]
precedence-list : /"[" /"]"
substitution : /"{" /"}"
knowledge : rule
@rule : fact
fact : atom /"."
atom : SYMBOL