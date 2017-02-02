#lang brag

# note: I am not very flexible about whitespace (e.g. after most commas) - this is just to facilitate writing data, so I should stick to one style anyway

top : [WS] at
at : OPEN-PAREN (label-edges-origin | widening-edges | cyclenode) [WS subtrees] CLOSE-PAREN  # keep label, edges and origin together as they are the all stored in the node label
label-edges-origin : at-label [WS graph-edges] [WS substitution WS knowledge]

at-label : [NUMBER PERIOD] acon-with-selection | acon-without-selection # this currently assumes a treelabel, not widening, case-split, or loop
acon-with-selection : [nonempty-acon-without-selection COMMA] ASTERISK abstract-atom ASTERISK [COMMA nonempty-acon-without-selection]
nonempty-acon-without-selection : abstract-atom (COMMA abstract-atom)*
abstract-atom : SYMBOL [OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN]
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : SYMBOL NUMBER
abstract-function-term : (SYMBOL [OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN]) | NUMBER
abstract-lplist : OPEN-RECTANGULAR-PAREN [abstract-term (COMMA abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-RECTANGULAR-PAREN
acon-without-selection : EMPTY-GOAL | nonempty-acon-without-selection

graph-edges : OPEN-RECTANGULAR-PAREN [precedence (COMMA WS precedence)*] CLOSE-RECTANGULAR-PAREN
precedence : abstract-atom WS LT WS abstract-atom

substitution : OPEN-CURLY-PAREN [substitution-pair (COMMA WS substitution-pair)*] CLOSE-CURLY-PAREN
substitution-pair : abstract-variable SLASH abstract-term

knowledge : (rule | fullai-rule) PERIOD
rule : (atom WS IMPLIES WS conjunction) | atom
atom : SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | NUMBER
lplist : OPEN-RECTANGULAR-PAREN [term (COMMA term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-RECTANGULAR-PAREN
conjunction : atom (COMMA atom)*
fullai-rule : abstract-atom WS LEADS-TO WS substitution

widening-edges : VARIABLE-IDENTIFIER WS [NUMBER PERIOD] (acon-with-selection | acon-without-selection) [WS graph-edges]

cyclenode : VARIABLE-IDENTIFIER WS NUMBER

subtrees : at (WS at)*