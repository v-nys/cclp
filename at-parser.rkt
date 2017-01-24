#lang brag

top : opt-ws at
at : OPEN-PAREN label-stack-opt-origin ws-prefixed-subtrees CLOSE-PAREN
label-stack-opt-origin : at-label mandatory-ws graph-stack mandatory-ws opt-origin
mandatory-ws : WS+
opt-origin : [substitution mandatory-ws knowledge]
ws-prefixed-subtrees : [mandatory-ws at (mandatory-ws at)*]
opt-ws : WS*

at-label : [NUMBER PERIOD] acon-with-potential-selection
acon-with-potential-selection : acon-with-selection | acon-without-selection
acon-with-selection : [nonempty-acon-without-selection COMMA opt-ws] ASTERISK abstract-atom ASTERISK [COMMA opt-ws nonempty-acon-without-selection]
nonempty-acon-without-selection : abstract-atom (COMMA opt-ws abstract-atom)*
abstract-atom : abstract-atom-with-args | abstract-atom-without-args
abstract-atom-with-args : SYMBOL OPEN-PAREN abstract-term (COMMA opt-ws abstract-term)* CLOSE-PAREN
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : SYMBOL NUMBER
abstract-function-term : (SYMBOL [OPEN-PAREN abstract-term (COMMA opt-ws abstract-term)* CLOSE-PAREN]) | abstract-number-term
abstract-number-term : abstract-number
abstract-number : NUMBER
abstract-lplist : OPEN-RECTANGULAR-PAREN [abstract-term (COMMA opt-ws abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-RECTANGULAR-PAREN
abstract-atom-without-args : SYMBOL
acon-without-selection : EMPTY-GOAL | nonempty-acon-without-selection

graph-stack : OPEN-RECTANGULAR-PAREN [graph (opt-ws AMPERSAND opt-ws graph)*] CLOSE-RECTANGULAR-PAREN
graph : QUESTION-MARK | precedence (COMMA opt-ws precedence)*
precedence : abstract-atom opt-ws GT opt-ws abstract-atom

substitution : OPEN-CURLY-PAREN [substitution-pair (COMMA opt-ws substitution-pair)*] CLOSE-CURLY-PAREN
substitution-pair : abstract-variable SLASH abstract-term

knowledge : (rule | fullai-rule) PERIOD
rule : (atom opt-ws IMPLIES opt-ws conjunction) | atom
atom : SYMBOL [OPEN-PAREN term (COMMA opt-ws term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL [OPEN-PAREN term (COMMA opt-ws term)* CLOSE-PAREN]) | number-term
number-term : NUMBER
lplist : OPEN-RECTANGULAR-PAREN [term (COMMA opt-ws term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-RECTANGULAR-PAREN
conjunction : atom (COMMA opt-ws atom)*

fullai-rule : fullai-rule-with-body | fullai-rule-without-body
fullai-rule-with-body : abstract-atom-with-args opt-ws LEADS-TO opt-ws substitution
fullai-rule-without-body : abstract-atom-with-args