#lang parenlog

; eerste vraag: wat is de juiste manier om data voor te stellen?
; in Prolog werkt dit:
; member(X,[X|Y]).
; member(X,[A|B]) :- member(X,B).


(member X (cons X Y))
(:- (member X (cons A B))
    (member X B))

(? (member X (cons Y Z)))
(? (member X Y))
; (? (member X (cons X Y))) -> loops indefinitely for some reason
; (? (member 1 (cons X Y))) -> also doesn't work... only Prolog atoms and variables are allowed as arguments?
(? (member a (cons X Y)))
(? (member a (cons b Y)))
(? (member a (cons a Y))) ; oddly, this is not a problem, whereas (member X (cons X Y)) is... -> should check source code, maybe suggest a patch

(not_a_member X ())
(:- (not_a_member X (cons A B))
    (,(compose not equal?) X A)
    (not_a_member X B))

(? (not_a_member a (cons b (cons c ())))) ; should yield true
(? (not_a_member a (cons b (cons a ())))) ; should yield false

(before a b)
(before a c)
; (before b a) ; introduces loop
(before b d)
(before c b)
(before c e)
(before d e)

(:- (reaches_without_encountering X Y Path)
    (before X Y)
    (not_a_member Y Path))
(:- (reaches_without_encountering X Z Path)
    (before X Y)
    (not_a_member Y Path)
    (reaches_without_encountering Y Z (cons Y Path)))
(:- (reaches_loopfree X Y)
    (reaches_without_encountering X Y (cons X ())))

(? (reaches_loopfree a c))

(:- (violates_partial_order)
    (reaches_loopfree X Y)
    (reaches_loopfree Y X)
    (,(compose not equal?) X Y)) ; note: in actual code, we need to check for renaming!
(? (violates_partial_order)) ; should be the case when there is a loop

