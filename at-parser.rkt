#lang brag
at : /"(" at-content /")"
@at-content : cyclenode | treelabel

cyclenode : /"!CY" NUMBER
treelabel : selectionless-conjunction
selectionless-conjunction : /"□"
                          | nonempty-selectionless-conjunction
@nonempty-selectionless-conjunction : abstract-atom (/"," abstract-atom)*
abstract-atom : SYMBOL