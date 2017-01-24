#lang brag

at : OPEN-PAREN at-label graph-stack (substitution knowledge at)* CLOSE-PAREN

graph-stack : OPEN-RECTANGULAR-PAREN [graph (AMPERSAND graph)*] CLOSE-RECTANGULAR-PAREN
