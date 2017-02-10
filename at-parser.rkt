#lang brag
at : /"(" at-content /")"
@at-content : cyclenode | treelabel

cyclenode : /"!CY" NUMBER
treelabel : selectionless-conjunction
selectionless-conjunction : /"□"
                          | nonempty-selectionless-conjunction
@nonempty-selectionless-conjunction : abstract-atom (/"," abstract-atom)*
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-term : abstract-function
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]