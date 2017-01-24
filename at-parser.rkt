#lang brag

top : WS* at
at : OPEN-PAREN at-label WS+ graph-stack (WS+ substitution WS+ knowledge WS+ at)* CLOSE-PAREN

at-label : [NUMBER PERIOD] acon-with-potential-selection
acon-with-potential-selection : acon-with-selection | acon-without-selection
acon-with-selection : [nonempty-acon-without-selection COMMA WS*] ASTERISK abstract-atom ASTERISK [COMMA WS* nonempty-acon-without-selection]
nonempty-acon-without-selection : abstract-atom (COMMA WS* abstract-atom)*
abstract-atom : abstract-atom-with-args | abstract-atom-without-args
abstract-atom-with-args : SYMBOL OPEN-PAREN abstract-term (COMMA WS* abstract-term)* CLOSE-PAREN
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : SYMBOL NUMBER
abstract-function-term : (SYMBOL [OPEN-PAREN abstract-term (COMMA WS* abstract-term)* CLOSE-PAREN]) | abstract-number-term
abstract-number-term : abstract-number
abstract-number : NUMBER
abstract-lplist : OPEN-RECTANGULAR-PAREN [abstract-term (COMMA WS* abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-RECTANGULAR-PAREN
abstract-atom-without-args : SYMBOL
acon-without-selection : EMPTY-GOAL | nonempty-acon-without-selection

graph-stack : OPEN-RECTANGULAR-PAREN [graph (WS* AMPERSAND WS* graph)*] CLOSE-RECTANGULAR-PAREN
graph : QUESTION-MARK | precedence (COMMA WS* precedence)*
precedence : abstract-atom WS* GT WS* abstract-atom

substitution : OPEN-CURLY-PAREN [substitution-pair (COMMA WS* substitution-pair)*] CLOSE-CURLY-PAREN
substitution-pair : abstract-variable SLASH abstract-term

knowledge : (rule | fullai-rule) PERIOD
rule : (atom WS* IMPLIES WS* conjunction) | atom
atom : SYMBOL [OPEN-PAREN term (COMMA WS* term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL [OPEN-PAREN term (COMMA WS* term)* CLOSE-PAREN]) | number-term
number-term : NUMBER
lplist : OPEN-RECTANGULAR-PAREN [term (COMMA WS* term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-RECTANGULAR-PAREN
conjunction : atom (COMMA WS* atom)*

fullai-rule : fullai-rule-with-body | fullai-rule-without-body
fullai-rule-with-body : abstract-atom-with-args WS* LEADS-TO WS* substitution
fullai-rule-without-body : abstract-atom-with-args