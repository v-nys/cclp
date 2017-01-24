#lang brag

at : OPEN-PAREN at-label graph-stack (substitution knowledge at)* CLOSE-PAREN

at-label : [NUMBER PERIOD] acon-with-potential-selection
acon-with-potential-selection : acon-with-selection | acon-without-selection
acon-with-selection : [nonempty-acon-without-selection] ASTERISK abstract-atom ASTERISK [nonempty-acon-without-selection]
nonempty-acon-without-selection : abstract-atom (COMMA abstract-atom)*
abstract-atom : abstract-atom-with-args | abstract-atom-without-args
abstract-atom-with-args : SYMBOL OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN
abstract-term : abstract-variable | abstract-function-term | abstract-lplist
abstract-variable : AVAR-SYMBOL-A NUMBER | AVAR-SYMBOL-G NUMBER
abstract-function-term : (SYMBOL [OPEN-PAREN abstract-term (COMMA abstract-term)* CLOSE-PAREN]) | abstract-number-term
abstract-number-term : abstract-number
abstract-number : NUMBER
abstract-lplist : OPEN-RECTANGULAR-PAREN [abstract-term (COMMA abstract-term)* [LIST-SEPARATOR (abstract-lplist | abstract-variable)]] CLOSE-RECTANGULAR-PAREN
abstract-atom-without-args : SYMBOL
acon-without-selection : EMPTY-GOAL | nonempty-acon-without-selection

graph-stack : OPEN-RECTANGULAR-PAREN [graph (AMPERSAND graph)*] CLOSE-RECTANGULAR-PAREN
graph : QUESTION-MARK | precedence (COMMA precedence)*
precedence : abstract-atom GT abstract-atom

substitution : substitution-pair (COMMA substitution-pair)*
substitution-pair : abstract-variable SLASH abstract-term

knowledge : rule | fullai-rule
rule : (atom IMPLIES conjunction) | atom
atom : SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]
term : variable | function-term | lplist
variable : VARIABLE-IDENTIFIER
function-term : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | number-term
number-term : NUMBER
lplist : OPEN-LIST-PAREN [term (COMMA term)* [LIST-SEPARATOR (lplist | variable)]] CLOSE-LIST-PAREN
conjunction : atom (COMMA atom)*

fullai-rule : fullai-rule-with-body | fullai-rule-without-body
fullai-rule-with-body : abstract-atom-with-args LEADS-TO substitution PERIOD
fullai-rule-without-body : abstract-atom-with-args PERIOD