#lang brag
abstractlp-program : clause*
clause : (atom | rule) PERIOD
fact : atom
atom : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN]) | (term ARITHMETIC-OP term)
term : VARIABLE-IDENTIFIER | function-term | list
function-term : (SYMBOL [OPEN-PAREN term (COMMA term)* CLOSE-PAREN])
list : OPEN-LIST-PAREN [term (COMMA term)* [LIST-SEPARATOR (list | VARIABLE-IDENTIFIER)]] CLOSE-LIST-PAREN
rule : atom IMPLIES conjunction
conjunction : atom (COMMA atom)*