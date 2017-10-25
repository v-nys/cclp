#lang at-exp racket
(require
  scribble/srcdoc
  (for-doc scribble/manual)
  graph
  math/number-theory
  anaphoric
  cclp/abstract-multi-domain
  cclp/clustering-structs
  cclp/gen-graph-structs
  cclp/genealogical-graph)

(define (partition-set proc st)
  (for/fold ([true-set (set)]
             [false-set (set)])
            ([elem st])
    (if (proc elem)
        (values (set-add true-set elem) false-set)
        (values true-set (set-add false-set elem)))))
(module+ test
  (require rackunit)
  (let-values ([(odd-set even-set)
                (partition-set odd? (set 1 2 3 4 5 6 7))])
    (check-equal? odd-set (set 1 3 5 7))
    (check-equal? even-set (set 2 4 6))))

(define (assign-prime-factor-ids g)
  (define root (first (tsort g)))
  ;; this is a custom BFS procedure
  ;; graph has BFS, but doesn't support sorting neighbors
  ;; note mapping and ->parents both just map between ID's
  (define (aux g q acc)
    (match acc
      [(list mapping previous-prime ->parents)
       (if
        (null? q)
        mapping
        (let ([gn (first q)])
          (if (hash-has-key? mapping (gen-node-id gn)) ; skip already processed multis that are child of several nodes
              (aux g (cdr q) acc)
              (let* ([ch
                      (sort
                       (get-neighbors g gn)
                       (λ (gn1 gn2)
                         (< (gen-node-id gn1) (gen-node-id gn2))))]
                     [new-prime (next-prime previous-prime)]
                     [encoding
                      (apply *
                             new-prime
                             (map
                              (curry hash-ref mapping)
                              (hash-ref ->parents (gen-node-id gn) (list))))])
                (aux g
                     (append (cdr q) ch)
                     (list
                      (hash-set mapping (gen-node-id gn) encoding)
                      new-prime
                      (foldl
                       (λ (c h)
                         (hash-update h (gen-node-id c) (curry cons (gen-node-id gn)) (list)))
                       (hash-remove ->parents (gen-node-id gn))
                       ch)))))))]))
  (aux g (list root) (list #hasheq() 1 #hasheqv())))
(module+ test
  (let*-values
      ([(node-constructor) (λ (id) (gen-node (abstract-atom 'foo empty) id (gen 0 #f) #f #t))]
       [(node1 node2 node3 node4 node5 node6 node8)
        (apply values (map node-constructor '(1 2 3 4 5 6 8)))]
       [(node7) (gen-node (multi (list) #t (init empty) (consecutive empty) (final empty)) 7 (gen-range 1 2 1 #t) #f #t)]
       [(graph)
        (unweighted-graph/directed
         `((,node1 ,node2)
           (,node1 ,node3)
           (,node1 ,node4)
           (,node1 ,node5)
           (,node2 ,node6)
           (,node3 ,node7)
           (,node4 ,node7)
           (,node5 ,node8)))])
    (check-equal?
     (assign-prime-factor-ids graph)
     #hasheq((1 . 2)
             (2 . 6)
             (3 . 10)
             (4 . 14)
             (5 . 22)
             (6 . 78)
             (7 . 2380)
             (8 . 418)))))
(provide
 (proc-doc/names
  assign-prime-factor-ids
  (-> graph? hash?)
  (g)
  @{Maps the identifiers in @racket[g] to prime encoded identifiers so that a prime encoded identifier immediately provides useful information on a node's relevant ancestry.}))

(define (cluster gen-nodes id->encoding unfolded-multi-encodings)
  (define gcd-all
    (apply
     gcd
     (map
      (λ (gn)
        (hash-ref id->encoding (gen-node-id gn)))
      gen-nodes)))
  (define
    greater-pairwise-gcds
    (remove
     gcd-all
     (sort
      (set->list
       (for*/set
           ([gn1 gen-nodes]
            [gn2 gen-nodes]) ; purposefully including gn1!
         (gcd
          (hash-ref id->encoding (gen-node-id gn1))
          (hash-ref id->encoding (gen-node-id gn2)))))
      <)))
  (define partitions
    (group-by
     (λ (gn)
       (findf
        (λ (d)
          (divides?
           d
           (hash-ref
            id->encoding
            (gen-node-id gn))))
        greater-pairwise-gcds))
     gen-nodes))
  (match partitions
    [(list (list single-gn))
     #:when (multi? (gen-node-conjunct single-gn))
     (let* ([gn-encoding (hash-ref id->encoding (gen-node-id single-gn))])
       (clustering
        single-gn
        gn-encoding))]
    [_
     #:when
     (and
      (not (member gcd-all unfolded-multi-encodings))
      (findf (λ (me) (and (< me gcd-all) (divides? me gcd-all))) unfolded-multi-encodings))
     (clustering
      (for/set ([p partitions])
        (cluster p id->encoding (filter (λ (me) (not (divides? me gcd-all))) (remove (findf (λ (me) (and (< me gcd-all) (divides? me gcd-all))) unfolded-multi-encodings) unfolded-multi-encodings))))
      (findf (λ (me) (and (< me gcd-all) (divides? me gcd-all))) unfolded-multi-encodings))]
    [(list (list single-gn))
     (let* ([gn-encoding (hash-ref id->encoding (gen-node-id single-gn))])
       (clustering
        single-gn
        gn-encoding))]
    [_
     (clustering
      (for/set ([p partitions])
        (cluster p id->encoding (filter (λ (me) (not (divides? me gcd-all))) unfolded-multi-encodings)))
      gcd-all)]))
(module+ test
  (let* ([gn1 (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 1 #f #f #t)]
         [gn2 (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 2 #f #f #t)]
         [gn3 (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 3 #f #f #t)]
         [gn4 (gen-node (abstract-atom 'filter (list (g 4) (a 3) (a 4))) 4 #f #f #t)]
         [gn5 (gen-node (abstract-atom 'sift (list (a 4) (a 5))) 5 #f #f #t)]
         [gn6 (gen-node (abstract-atom 'length (list (a 5) (abstract-function 'cons (list (g 5) (a 5))))) 6 #f #f #t)]
         [gns (list gn1 gn2 gn3 gn4 gn5 gn6)]
         [->encodings
          #hasheq((1 . 22)
                  (2 . 102)
                  (3 . 570)
                  (4 . 4830)
                  (5 . 6090)
                  (6 . 26))])
    (check-equal?
     (cluster gns ->encodings (list))
     (clustering
      (set
       (clustering gn1 22)
       (clustering gn6 26)
       (clustering
        (set
         (clustering gn2 102)
         (clustering
          (set
           (clustering gn3 570)
           (clustering
            (set
             (clustering gn4 4830)
             (clustering gn5 6090))
            210))
          30))
        6))
      2)))
  (let ([gn1
         (gen-node (abstract-atom 'filter (list (g 57) (abstract-function 'nil (list)) (a 77))) 118 #f #f #f)]
        [gn2
         ; don't need the actual contents, just writing out more or less accurately to see where this occurs in prog
         (gen-node (multi (list) #t (init (list)) (consecutive (list)) (final (list))) 119 #f #f #f)]
        [gn3
         (gen-node (abstract-atom 'filter (list (g 28) (a 38) (a 40))) 120 #f #f #f)]
        [gn4
         (gen-node (abstract-atom 'sift (list (a 40) (a 42))) 121 #f #f #f)]
        [gn5
         (gen-node (abstract-atom 'length (list (a 42) (g 32))) 122 #f #f #f)]
        [id->encoding
         #hasheq((1 . 2)
                 (2 . 6)
                 (3 . 10)
                 (4 . 14)
                 (5 . 66)
                 (6 . 78)
                 (7 . 170)
                 (8 . 266)
                 (9 . 1794)
                 (10 . 4930)
                 (11 . 8246)
                 (12 . 66378)
                 (13 . 202130)
                 (14 . 211990)
                 (15 . 387562)
                 (16 . 3518034)
                 (17 . 11925670)
                 (18 . 12931390)
                 (19 . 25966654)
                 (20 . 27516902)
                 (21 . 256816482)
                 (22 . 942127930)
                 (23 . 1073305370)
                 (24 . 2449004278)
                 (25 . 24911198754)
                 (26 . 25938464682)
                 (27 . 97039176790)
                 (28 . 114843674590)
                 (29 . 266941466302)
                 (30 . 2931046509066)
                 (31 . 12323975452330)
                 (32 . 15044521371290)
                 (33 . 36570980883374)
                 (34 . 407415464760174)
                 (35 . 1836272342397170)
                 (36 . 1860920293301830)
                 (37 . 2361989855292530)
                 (38 . 5961069883989962)
                 (39 . 68038382614949058)
                 (40 . 321939210741216590)
                 (41 . 422796184097362870)
                 (42 . 1078953649002183122)
                 (43 . 12995331079455270078)
                 (44 . 62134267673054801870)
                 (45 . 83290848267180485390)
                 (46 . 84136440635375211130)
                 (47 . 227659219939460638742) (48 . 2897958830718525227394) (49 . 14104478761783440024490) (50 . 19073604253184331154310) (51 . 19603790668042424193290) (52 . 54410553565531092659338) (53 . 54865872005410013936822) (54 . 727387666510349832075894) (55 . 3624851041778344086293930) (56 . 5016357918587479093583530) (57 . 5273419689703412107995010) (58 . 14868651313466113776878762) (59 . 201486383623366903485022638) (60 . 204395934289408302813326214) (61 . 1025832844823271376421182190) (62 . 1469792870146131374419974290) (63 . 1618939844738947517154468070) (64 . 4624150558487961384609294982) (65 . 63975927432584798780571104982) (66 . 325189011808977026325514754230) (67 . 486501440018369484933011489990) (68 . 545582727677025313281055739590) (69 . 1604580243795322600459425358754) (70 . 22327598673972094774419315638718) (71 . 114791721168568890292906708243190) (72 . 116742855239422752450859796768570) (73 . 178546028486741600970415216826330) (74 . 203502357423530441853833790867070) (75 . 608135912398427265574122210967766) (76 . 8551470292131312298602597889628994) (77 . 45412970688135450703384460942973730) (78 . 70882773309236415585254841080053010) (79 . 81604445326835707183387350137695070) (80 . 248727588170956751619815984285816294) (81 . 3583066052403019853114488515754548486) (82 . 19118860659705024746124858056991940330) (83 . 30550475296280895117244836505502847310) (84 . 30692240842899367948415346187662953330) (85 . 35824351498480875453507046710448135730) (86 . 110186321559733840967578481038616618242) (87 . 1608796657528955914048405343573792270214) (88 . 8737319321485196308979060132045316730810) (89 . 14149123028576608624219474592512621485130) (90 . 16586674743796645334973762626937486842990) (91 . 51457012168395703731859150645033960719014) (92 . 770613598956369882829186159571846497432506) (93 . 4255074509563290602472802284306069247904470) (94 . 6947219407031114834491762024923697149198830) (95 . 8276750697154526022151907550841805934652010) (96 . 8343097396129712603491802601349555882023970) (97 . 26191619193713413199516307678322286005978126) (98 . 401489685056268708954005989136932025162335626) (99 . 15460369638458382150367559621853236780858903987069650655182322154530495600932526027762300) (100 . 4477722127160598577984181985005417010646737410) (101 . 4563674275682952794110016022938207067467111590) (102 . 14588731890898371152130583376825513305329816182) (103 . 226038692686679283141105371884092730166394957438) (104 . 8796950324282819443559141424834491728308716368642631222798741305927851996930607309796748700) (105 . 2556779334608701788028967913438093113079287061110) (106 . 2633240057069063762201479245235345477928523387430)
                 (107 . 8563585619957343866300652442196576310228602098834)
                 (108 . 8651118011302734093213435942457529390060580995926)
                 (109 . 135397176919320890601522117758571545369670579505362)
                 (110 . 5286967144893974485579043996325529528713538537554221364902043524862639050155294993187845968700)
                 (111 . 1551965056107481985333583523456922519639127246093770)
                 (112 . 1614176154983336086229506777329266777970184836494590)
                 (113 . 5337739812973786935512689976496295633667378474486342)
                 (114 . 3272632662689370206573428233725502778273680354746063024874364941889973572046127600783276654625300)
                 (115 . 979289950403821132745491203301318109892289292285168870)
                 (116 . 1034686915344318431273113844268060004678888480193032190)
                 (117 . 3432166699742144999534659654887118092448124359094717906)
                 (118 . 2117393332760022523653008067220400297543071189520702777093714117402812901113844557706779995542569100)
                 (119 . 2137029128736158744892448636622753314212713271649179155242960307054152742546121323311479655470320900)
                 (120 . 645352077316118126479278702975568634419018643615926285330)
                 (121 . 683928051042594483071528251061187663092745285407594277590)
                 (122 . 2309848188926463584686825947739030476217587693670745150738))])
    (check-equal?
     (cluster (list gn1 gn2 gn3 gn4 gn5) id->encoding (list))
     (clustering
      (set
       (clustering
        gn5
        2309848188926463584686825947739030476217587693670745150738)
       (clustering
        (set
         (clustering
          (set
           (clustering
            gn4
            683928051042594483071528251061187663092745285407594277590)
           (clustering
            gn3
            645352077316118126479278702975568634419018643615926285330))
          16586674743796645334973762626937486842990)
         (clustering
          (set
           (clustering
            gn1
            2117393332760022523653008067220400297543071189520702777093714117402812901113844557706779995542569100)
           (clustering
            gn2
            2137029128736158744892448636622753314212713271649179155242960307054152742546121323311479655470320900))
          3272632662689370206573428233725502778273680354746063024874364941889973572046127600783276654625300))
        422796184097362870))
      2))))
(provide
 (proc-doc/names
  cluster
  (-> (non-empty-listof gen-node?) hash? (listof exact-positive-integer?) clustering?)
  (gen-nodes id->encoding multi-encodings)
  @{Clusters elements in a list of @racket[gen-node?] structures. Multi ancestors are always indicated in the clustering, provided that their encoding is in @racket[multi-encodings], unless their only entry is another multi node.}))

(define (flatten-clustering c)
  (match c
    [(clustering (and (? gen-node?) gn) gcd)
     (set gn)]
    [(clustering subclusters gcd)
     (for/fold ([acc (set)])
               ([sc (list->set (set-map subclusters flatten-clustering))])
       (set-union acc sc))]))
(module+ test
  (let ([gn1 (gen-node (abstract-atom 'foo (list)) 1 (gen 0 #f) #f #f)]
        [gn2 (gen-node (abstract-atom 'bar (list)) 2 (gen 0 #f) #f #f)]
        [gn3 (gen-node (abstract-atom 'baz (list)) 3 (gen 0 #f) #f #f)]
        [gn4 (gen-node (abstract-atom 'quux (list)) 4 (gen 0 #f) #f #f)])
    (check-equal?
     (flatten-clustering
      (clustering
       (set
        (clustering
         (set
          (clustering
           gn1
           1)
          (clustering
           gn2
           2))
         5)
        (clustering
         gn3
         3)
        (clustering
         gn4
         4))
       6))
     (set gn1 gn2 gn3 gn4))))
(provide
 (proc-doc/names
  flatten-clustering
  (-> clustering? (set/c gen-node?))
  (c)
  @{Transforms a hierarchical @racket[clustering?] into a flat @racket[set?] of @racket[gen-node?]s.}))

(define (annotate-cluster encoding->id id->conjunct cluster #:established [established #f])
  (define (some-gen-node c)
    (match c
      [(clustering (and (? gen-node?) gn) _gcd) gn]
      [(clustering subclusters _gcd)
       (some-gen-node (set-first subclusters))]))
  ;; can't just curry due to kw
  (define rec
    (λ (c #:established [established #f])
      (annotate-cluster encoding->id id->conjunct c #:established established)))
  (define (multi-cluster? c)
    (match c
      [(clustering (gen-node conjunct _ _ _ _) _)
       (multi? conjunct)]
      [(clustering _ gcd)
       (multi? (hash-ref id->conjunct (hash-ref encoding->id gcd)))]))  
  (match cluster
    [(clustering (and (gen-node gnc _ _ _ _) gn) gcd)
     (let ([last (if (abstract-atom? gnc) ; note: not necessarily syntactically last!
                     (if established
                         (gen-number established)
                         0)
                     (gensym))]
           [asc? (and (multi? gnc) (multi-ascending? gnc))])
       (cons
        (clustering
         (struct-copy
          gen-node
          gn
          [range
           (if (abstract-atom? gnc)
               (or established (gen 0 #f))
               (gen-range (if asc? (gen-number established) last) (if asc? last (gen-number established)) (gen-origin established) asc?))])
         gcd)
        last))]
    [(clustering subclusters gcd)
     (cond
       [(and
         established
         (renames-with-corresponding-args?
          (hash-ref id->conjunct (hash-ref encoding->id gcd))
          (hash-ref id->conjunct (gen-origin established)))
         (> (set-count subclusters) 1))
        (let-values ([(multi-clusters non-multi-clusters)
                      (partition
                       multi-cluster?
                       (set->list subclusters))])
          ;; may be able to consolidate these two cases with something like map-accumulate
          (match multi-clusters
            [(list)
             (let* ([next-gen-number (gen-add1 (gen-number established))]
                    [next-established (gen next-gen-number (gen-origin established))])
               (cons
                (clustering
                 (list->set (set-map subclusters (λ (sc) (car (rec sc #:established next-established)))))
                 gcd)
                next-gen-number))]
            [(list mc)
             (match-let ([(cons annotated-mc last-symbol)
                          (rec mc #:established (gen (add1 (gen-number established)) (gen-origin established)))])
               (cons
                (clustering
                 (foldl
                  (λ (nmc acc)
                    (set-add acc (car (rec nmc #:established (gen last-symbol (gen-origin established))))))
                  (set annotated-mc)
                  non-multi-clusters)
                 gcd)
                last-symbol))]))]
       [established
        (let-values
            ([(multi-clusters non-multi-clusters)
              (partition
               multi-cluster?
               (set->list subclusters))])
          (match multi-clusters
            [(list)
             (cons
              (clustering
               (list->set
                (set-map
                 subclusters
                 (λ (sc)
                   (car (rec sc #:established established)))))
               gcd)
              (gen-number established))]
            [(list mc)
             #:when (and (multi? (hash-ref id->conjunct (hash-ref encoding->id (clustering-gcd cluster)))) (multi-ascending? (hash-ref id->conjunct (hash-ref encoding->id (clustering-gcd cluster)))))
             (match-let
                 ([annotated-nmcs
                   (list->set
                    (map
                     (λ (nmc)
                       (car (rec nmc #:established established)))
                     non-multi-clusters))]
                  [(cons annotated-mc last-sym-or-number)
                   (rec mc #:established (gen (gen-add1 (gen-number established)) (gen-origin established)))])
               (cons (clustering (set-add annotated-nmcs annotated-mc) gcd) last-sym-or-number))]
            
            [(list mc)
             #:when (multi? (hash-ref id->conjunct (hash-ref encoding->id (clustering-gcd cluster))))
             (match-let*
                 ([(cons annotated-mc last-sym-or-number)
                   (rec mc #:established established)]
                  [annotated-nmcs
                   (list->set
                    (map
                     (λ (nmc)
                       (car (rec nmc #:established (gen (gen-add1 last-sym-or-number) (gen-origin established)))))
                     non-multi-clusters))])
               (cons (clustering (set-add annotated-nmcs annotated-mc) gcd) (gen-add1 last-sym-or-number)))][(list mc) (error "this should not happen")]
            [(list _ _)
             ;; relying on syntax here
             ;; could also check which contains the smaller (greater than the greatest prime factor in the parent) but would be slower
             (let* ([gcd-id (hash-ref encoding->id gcd)]
                    [sorted-mcs (sort multi-clusters (λ (c1 c2) (< (gen-node-id (some-gen-node c1)) (gen-node-id (some-gen-node c2)))))])
               (cond
                 ;; no non-multi clusters here
                 [(multi-ascending? (hash-ref id->conjunct gcd-id))
                  (match-let
                      ([(cons amc1 last1)
                        (rec (first sorted-mcs) #:established established)])
                    (match-let
                        ([(cons amc2 last2)
                          (rec (second sorted-mcs) #:established (gen (gen-add1 last1) (gen-origin established)))])
                      (cons
                       (clustering
                        (set amc1 amc2)
                        gcd)
                       last2)))]
                 [else (error "deal with this later")]))]))]
       [else
        (let* ([gcd-id (hash-ref encoding->id gcd)]
               [is-rta?
                (or
                 (and
                  (> (set-count subclusters) 1)
                  (ormap
                   (match-lambda
                     [(clustering ssc sc-gcd)
                      (renames-with-corresponding-args?
                       (hash-ref id->conjunct gcd-id)
                       (hash-ref id->conjunct (hash-ref encoding->id sc-gcd)))])
                   (set->list subclusters)))
                 (and (abstract-atom? (hash-ref id->conjunct gcd-id))
                      (aif (findf multi-cluster? (set->list subclusters))
                           (equal? (multi-rta (hash-ref id->conjunct (hash-ref encoding->id (clustering-gcd it)))) gcd-id)
                           #f)))])
          (if is-rta?
              (let-values ([(multi-clusters non-multi-clusters)
                            (partition
                             multi-cluster?
                             (set->list subclusters))])
                (match multi-clusters
                  [(list)
                   (cons
                    (clustering
                     (list->set
                      (map
                       (λ (nmc)
                         (car (rec nmc #:established (gen 1 gcd-id))))
                       non-multi-clusters))
                     gcd)
                    1)]
                  [(list mc)
                   (match-let ([(cons annotated-mc last-symbol)
                                (rec mc #:established (gen 1 gcd-id))])
                     (cons
                      (clustering
                       (set-add
                        (list->set
                         (map
                          (λ (nmc)
                            (car (rec nmc #:established (gen last-symbol gcd-id))))
                          non-multi-clusters))
                        annotated-mc)
                       gcd)
                      last-symbol))]))
              (cons (clustering (list->set (set-map subclusters (compose car rec))) gcd) 0)))])]))
(provide
 (proc-doc/names
  annotate-cluster
  (->* (hash? hash? clustering?) (#:established (or/c #f gen?)) (cons/c clustering? (or/c exact-nonnegative-integer? symbol? symsum?)))
  ((encoding->id id->conjunct cluster ) ((established #f)))
  @{Transforms a hierarchical @racket[clustering?] containing @racket[gen-node?] structs without generations into a @racket[clustering?] where the @racket[(set/c gen-node?)] have been assigned generations and pairs the result with the generation number that should be used for sibling clusters.}))

(module+ test
  (let* ([gn1 (gen-node (abstract-atom 'integers (list (g 1) (a 1))) 1 #f #f #t)]
         [gn2 (gen-node (abstract-atom 'filter (list (g 2) (a 1) (a 2))) 2 #f #f #t)]
         [gn3 (gen-node (abstract-atom 'filter (list (g 3) (a 2) (a 3))) 3 #f #f #t)]
         [gn4 (gen-node (abstract-atom 'filter (list (g 4) (a 3) (a 4))) 4 #f #f #t)]
         [gn5 (gen-node (abstract-atom 'sift (list (a 4) (a 5))) 5 #f #f #t)]
         [gn6 (gen-node (abstract-atom 'length (list (a 5) (abstract-function 'cons (list (g 5) (a 5))))) 6 #f #f #t)]
         [ann-gn1 (struct-copy gen-node gn1 [range (gen 0 #f)])]
         [ann-gn2 (struct-copy gen-node gn2 [range (gen 1 7)])]
         [ann-gn3 (struct-copy gen-node gn3 [range (gen 2 7)])]
         [ann-gn4 (struct-copy gen-node gn4 [range (gen 3 7)])]
         [ann-gn5 (struct-copy gen-node gn5 [range (gen 3 7)])]
         [ann-gn6 (struct-copy gen-node gn6 [range (gen 0 #f)])]
         [gns (list gn1 gn2 gn3 gn4 gn5 gn6)]
         [encoding->id
          #hasheq((22 . 1)
                  (102 . 2)
                  (570 . 3)
                  (4830 . 4)
                  (6090 . 5)
                  (26 . 6)
                  ;; these are the encodings of the three ancestor sifts
                  (6 . 7)     ; sift @ 0 filters
                  (30 . 8)    ; sift @ 1 filter
                  (210 . 9)   ; sift @ 2 filters
                  (2 . 10))]
         [id->conjunct
          (hasheq
           1 (gen-node-conjunct gn1)
           2 (gen-node-conjunct gn2)
           3 (gen-node-conjunct gn3)
           4 (gen-node-conjunct gn4)
           5 (gen-node-conjunct gn5)
           6 (gen-node-conjunct gn6)
           7 (abstract-atom 'sift (list (abstract-function 'cons (list (g 6) (a 6))) (a 7)))
           8 (abstract-atom 'sift (list (abstract-function 'cons (list (g 7) (a 8))) (a 9)))
           9 (abstract-atom 'sift (list (abstract-function 'cons (list (g 8) (a 10))) (a 11)))
           10 (abstract-atom 'primes (list (g 9) (a 12))))]
         [clusters
          (clustering
           (set
            (clustering gn1 22)
            (clustering gn6 26)
            (clustering
             (set
              (clustering gn2 102)
              (clustering
               (set
                (clustering gn3 570)
                (clustering
                 (set
                  (clustering gn4 4830)
                  (clustering gn5 6090))
                 210))
               30))
             6))
           2)])
    (check-equal?
     (flatten-clustering (car (annotate-cluster encoding->id id->conjunct clusters)))
     (set ann-gn1 ann-gn2 ann-gn3 ann-gn4 ann-gn5 ann-gn6))))