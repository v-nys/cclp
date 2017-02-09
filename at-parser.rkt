#lang brag
at : "(" (cyclenode | generalization | label-edges-origin | (label-edges-origin-sel at*)) ")"

cyclenode : "!CY" NUMBER

generalization : "!GEN" (acon-without-selection | (bare-label-sel at*))
acon-without-selection : "□" | nonempty-acon-without-selection
nonempty-acon-without-selection : abstract-conjunct ("," abstract-conjunct)*
abstract-conjunct : abstract-atom | multi-abstraction
abstract-atom : SYMBOL ["(" abstract-term ("," abstract-term)* ")"]
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : SYMBOL NUMBER
abstract-function-term : (SYMBOL ["(" abstract-term ("," abstract-term)* ")"]) | NUMBER
abstract-lplist : "[" [abstract-term ("," abstract-term)* ["|" (abstract-lplist | abstract-variable)]] "]"
multi-abstraction : "multi" "(" parameterized-conjunction "," boolean "," init-set "," consecutive-set "," final-set ")"

# parameterized-conjunction = collect(g<1,i,1>,a<1,i,1>),append(a<1,i,2>,a<1,i,1>,a<1,i,3>)
# init-set = {g<1,1,1>=g8,a<1,1,1>=a8,a<1,1,2>=a7,a<1,1,3>=a5}
# consecutive-set = {a<1,i+1,2>=a<1,i,3>}
# final-set = {g<1,L,1>=g4,a<1,L,1>=a4,a<1,L,2>=a3,a<1,L,3>=a1}

parameterized-conjunction : parameterized-atom ["," parameterized-atom]*
parameterized-atom : SYMBOL ["(" parameterized-term ("," parameterized-term)* ")"]
parameterized-term : parameterized-variable | parameterized-function-term | parameterized-lplist

boolean : "#t" | "#f"


at-label : [NUMBER "."] (acon-with-selection | acon-without-selection) # this currently assumes a treelabel, not widening, case-split, or loop
acon-with-selection : [nonempty-acon-without-selection ","] "*" abstract-atom "*" ["," nonempty-acon-without-selection]



graph-edges : "[" [precedence ("," precedence)*] "]"
precedence : abstract-atom "<" abstract-atom

substitution : "{" [substitution-pair ("," substitution-pair)*] "}"
substitution-pair : abstract-variable "/" abstract-term

knowledge : (rule | fullai-rule) "."
rule : (atom ":-" conjunction) | atom
atom : SYMBOL ["(" term ("," term)* ")"]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL ["(" term ("," term)* ")"]) | NUMBER
lplist : "[" [term ("," term)* ["|" (lplist | variable)]] "]"
conjunction : atom ("," atom)*
fullai-rule : abstract-atom "->" substitution

# this is tricky: cannot distinguish between widening and case split based on current tokens
# expander uses a trick to make this work, but it's sketchy
widening-edges : VARIABLE-IDENTIFIER [NUMBER "."] (acon-with-selection | acon-without-selection) [graph-edges]
case-split-edges : VARIABLE-IDENTIFIER [NUMBER "."] (acon-with-selection | acon-without-selection) [graph-edges]