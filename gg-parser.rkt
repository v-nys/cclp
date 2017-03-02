#lang brag
gg : nodes-section edges-section
nodes-section : /"NODES" node-line*
node-line : NUMBER abstract-atom [generation-range]
          | NUMBER multi-abstraction [generation-range]
abstract-atom : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-term : abstract-function | abstract-variable | abstract-list
abstract-function : SYMBOL [/"(" abstract-term (/"," abstract-term)* /")"]
@abstract-variable : abstract-a-variable | abstract-g-variable
abstract-a-variable : AVAR-A
abstract-g-variable : AVAR-G
abstract-list : /"[" [abstract-term abstract-term-tail ["|" (abstract-list | abstract-variable)]] /"]" # can't cut comma or separator (need to distinguish during expansion)
@abstract-term-tail : ("," abstract-term)*
generation-range : NUMBER BOOLEAN | recursion-depth NUMBER | recursion-depth /":" recursion-depth NUMBER
@recursion-depth : NUMBER | SYMBOL | symbol-sum
symbol-sum : SYMBOL /"+" NUMBER
multi-abstraction : /"multi" /"(" parameterized-abstract-conjunction /"," BOOLEAN /"," init /"," consecutive /"," final  /")"
parameterized-abstract-conjunction : /"(" parameterized-abstract-atom (/"," parameterized-abstract-atom)* /")"
parameterized-abstract-atom : SYMBOL [/"(" parameterized-abstract-term (/"," parameterized-abstract-term)* /")"]
@parameterized-abstract-term : parameterized-abstract-function | parameterized-abstract-variable | parameterized-abstract-list
parameterized-abstract-function : SYMBOL [/"(" parameterized-abstract-term (/"," parameterized-abstract-term)* /")"]
@parameterized-abstract-variable : parameterized-abstract-a-variable | parameterized-abstract-g-variable
parameterized-abstract-a-variable : /"a" /"<" NUMBER /"," (NUMBER|"i+1"|"i"|"L") /"," NUMBER /">"
parameterized-abstract-g-variable : /"g" /"<" NUMBER /"," (NUMBER|"i+1"|"i"|"L") /"," NUMBER /">"
parameterized-abstract-list : /"[" [parameterized-abstract-term parameterized-abstract-term-tail ["|" (parameterized-abstract-list | parameterized-abstract-variable)]] /"]"
@parameterized-abstract-term-tail : ("," parameterized-abstract-term)*
init : /"{" [init-pair (/"," init-pair)*] /"}"
init-pair : parameterized-abstract-variable /"/" abstract-term
consecutive : /"{" [consecutive-pair (/"," consecutive-pair)*] /"}"
consecutive-pair : parameterized-abstract-variable /"/" parameterized-abstract-term
final : /"{" [final-pair (/"," final-pair)*] /"}"
final-pair : parameterized-abstract-variable /"/" abstract-variable
edges-section : /"EDGES" edge-line*
edge-line : NUMBER /"->" NUMBER (/"," NUMBER)* /"."
