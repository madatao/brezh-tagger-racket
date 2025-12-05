#lang racket
(require racket/match)

;; tag contient ADJ ou ADV ?
(define (adj/adv-tag? t)
  (or (string-contains? t "ADJ")
      (string-contains? t "ADV")))

;; finit par "oc'h" ? (comparatif)
(define (comparatif-suffix? tok)
  (and (regexp-match #rx"oc'h$" tok) #t))
;A. « Ken ker all eo. »
;("ken") ("ker") ("all") ("eo") (".")

;; On veut approx :
;; "SCJ" "ADJ" "ADV" "VERB" ...
;; 
;; racket
;; Copier le code
;; A : "ken X all VERBE"
(define (rule-A-ken-ker-all tokens tags)
  (letrec ([loop
            (lambda (toks ts)
              (match (list toks ts)
                ;; fin
                [(list '() _) '()]
                [(list _ '()) '()]

                ;; "ken" X "all" VERB ...
                [(list (list "ken" x "all" v more-toks ...)
                       (list t0 t1 t2 t3 more-tags ...))
                 #:when (string-contains? t3 "VERB")
                 (append '("SCJ" "ADJ" "ADV" "VERB")
                         (loop more-toks more-tags))]

                ;; sinon on avance
                [(list (cons _ more-toks)
                       (cons t0 more-tags))
                 (cons t0
                       (loop more-toks more-tags))]))])
    (loop tokens tags)))
;; B. « Ken melen hag an aour. »
;; ("ken") ("melen") ("hag") ("an" "aour") (".")
;; ("ADV_N_SCJ") ("N") ("SCJ") ("DET:art-d" "N") ("PUNCT")
;; 
;; On veut :
;; "SCJ" "ADJ" "SCJ" "DET" "N" ...
;; 
;; racket
;; Copier le code
;; ;; B : "ken X hag DET N"
(define (rule-B-ken-X-hag tokens tags)
  (letrec ([loop
            (lambda (toks ts)
              (match (list toks ts)

                [(list '() _) '()]
                [(list _ '()) '()]

                ;; "ken" X "hag" DET N ...
                [(list (list "ken" x "hag" det n more-toks ...)
                       (list t0 t1 t2 t3 t4 more-tags ...))
                 #:when (and (string-contains? t2 "SCJ")
                             (string-contains? t3 "DET")
                             (string-contains? t4 "N"))
                 (append '("SCJ" "ADJ" "SCJ" "DET" "N")
                         (loop more-toks more-tags))]

                [(list (cons _ more-toks)
                       (cons t0 more-tags))
                 (cons t0
                       (loop more-toks more-tags))]))])
    (loop tokens tags)))
;; C & D. « Ken ker-se eo ? »
;; "ken" "xxx-yy" "VERBE" → "SCJ" "ADJ" VERB ...
;; 
;; Cas général :
;; si on a ken + mot avec un tiret + verbe ensuite →
;; "SCJ" "ADJ" "VERB" ...
;; 
;; racket
;; Copier le code
;; ;; C/D : "ken" X-qqch VERBE
(define (rule-C-ken-hyphen tokens tags)
  (letrec ([loop
            (lambda (toks ts)
              (match (list toks ts)

                [(list '() _) '()]
                [(list _ '()) '()]

                ;; "ken" x(verbe composé) VERB ...
                [(list (list "ken" x v more-toks ...)
                       (list t0 t1 t2 more-tags ...))
                 #:when (and (string-contains? x "-")
                             (string-contains? t2 "VERB"))
                 (append (list "SCJ" "ADJ" t2)
                         (loop more-toks more-tags))]

                ;; sinon on avance
                [(list (cons _ more-toks)
                       (cons t0 more-tags))
                 (cons t0
                       (loop more-toks more-tags))]))])
    (loop tokens tags)))
;; Exemple :
;; 
;; racket
;; Copier le code
;; (define toksC '("ken" "ker-se" "eo" "?"))
;; (define tagsC '("ADV_N_SCJ" "UNKN" "VERB" "PUNCT"))
;; ;; => '("SCJ" "ADJ" "VERB" "PUNCT")


;; E. « N'eo ket ken tev al levr-mañ. »
;; Tokens :
;; 
;; racket
;; Copier le code
;; ("n'eo" "ket" "ken" "tev" "al" "levr-mañ" ".")
;; ("UNKN" "ADV" "ADV_N_SCJ" "ADJ" "DET" "UNKN" "PUNCT")
;; E1. Fusion / retag de la négation verbale
;; On veut au moins :
;; 
;; "n'eo" → "PART-V-NEG"
;; 
;; "ket" reste "ADV"
;; 
;; racket
;; Copier le code
;; ;; E1 : "n'eo ket ..."
(define (rule-E1-neg-neo-ket tokens tags)
  (letrec ([loop
            (lambda (toks ts)
              (match (list toks ts)

                [(list '() _) '()]
                [(list _ '()) '()]

                ;; "n'eo" "ket" ...
                [(list (list "n'eo" "ket" more-toks ...)
                       (list t0 t1 more-tags ...))
                 #:when (string-contains? t1 "ADV")
                 (append '("PART-V-NEG" "ADV")
                         (loop more-toks more-tags))]

                [(list (cons _ more-toks)
                       (cons t0 more-tags))
                 (cons t0
                       (loop more-toks more-tags))]))])
    (loop tokens tags)))



(define (rule-E2-det-unkn->N tokens tags)
  (let loop ([toks tokens]
             [ts   tags]
             [prev-tag ""])
    (cond
      [(null? toks) '()]
      [else
       (define t0 (car ts))   ; <- t0 est supposé être une string
       (define new-tag
         (if (and (string=? t0 "UNKN")
                  (string-contains? prev-tag "DET"))
             "N"
             t0))
       (cons new-tag
             (loop (cdr toks) (cdr ts) new-tag))])))

(define (rule-F1-comp-oc-h tokens tags)
  (let loop ([toks tokens]
             [ts   tags])
    (cond
      [(null? toks) '()]
      [else
       (define tok (car toks))
       (define tag (car ts))
       (define new-tag
         (if (and (string=? tag "UNKN")
                  (comparatif-suffix? tok))
             "ADJ:comp"
             tag))
       (cons new-tag
             (loop (cdr toks) (cdr ts)))])))

;; Règle : "ken" avec tag "ADV_N_SCJ" -> "SCJ" dans les contextes simples
(define (rule-ken-advscj->scj tokens tags)
  (let loop ([toks tokens]
             [ts   tags])
    (cond
      [(null? toks) '()]
      [else
       (define tok (car toks))
       (define tag (car ts))

       (define new-tag
         (if (and (string=? tok "ken")
                  (string=? tag "ADV_N_SCJ"))
             "SCJ"
             tag))
       (cons new-tag
             (loop (cdr toks) (cdr ts)))])))




(define (rule-E2-da-bet-eur tokens tags)
  (let loop ([toks  tokens]
             [ts    tags]
             [prev1 #f]    ; token précédent
             [prev2 #f])   ; token d'avant
    (cond
      [(null? toks)
       '()]
      [else
       (define tok0 (car toks)) ; token courant
       (define tag0 (car ts))   ; balise courante

       (define new-tag
         (cond
           ;; 1) "bet" après "da" => PRO
           [(and (string=? tok0 "bet")
                 (string=? prev1 "da"))
            "PRO"]

           ;; 2) "eur" après "da bet" => N
           [(and (string=? tok0 "eur")
                 (string=? prev1 "bet")
                 (string=? prev2 "da"))
            "N"]

           ;; sinon : on laisse la balise telle quelle
           [else tag0]))

       (cons new-tag
             (loop (cdr toks)
                   (cdr ts)
                   tok0      ; nouveau prev1
                   prev1))]))) ; nouveau prev2


(define (rule-E3-init-adj tokens tags)
  (let loop ([toks tokens]
             [ts   tags]
             [pos  0])
    (cond
      [(null? toks)
       '()]
      [else
       (define tok0      (car toks))          ; token courant
       (define tag0      (car ts))            ; tag courant
       (define next-tok  (if (null? (cdr toks)) #f (cadr toks)))
       (define next-tag  (if (null? (cdr ts))   #f (cadr ts)))

       (define new-tag
         (cond
           ;; Cas 1 : "prest" ADJ_N au début, suivi de "eo" => ADJ
           [(and (= pos 0)
                 (string=? tag0 "ADJ_N")
                 (string=? next-tok "eo"))
            "ADJ"]

           ;; Cas 2 : "mat" UNKN au début, suivi d'un PART-V => ADJ
           [(and (= pos 0)
                 (string=? tag0 "UNKN")
                 (string=? next-tag "PART-V"))
            "ADJ"]

           ;; sinon, on garde la balise telle quelle
           [else tag0]))

       (cons new-tag
             (loop (cdr toks)
                   (cdr ts)
                   (add1 pos)))])))


(define (rule-E4-det-art-i-goan tokens tags)
  (let loop ([ts tags])
    (cond
      ;; plus assez d'éléments pour matcher 3 tags
      [(or (null? ts) (null? (cdr ts)) (null? (cddr ts)))
       ts]

      ;; motif DET:art-i_PART-V ADJ_N ADJ
      [(and (string=? (car ts)  "DET:art-i_PART-V")
            (string=? (cadr ts) "ADJ_N")
            (string=? (caddr ts) "ADJ"))
       (append (list "DET:art-i" "N" "ADJ")
               (cdddr ts))]

      ;; sinon, on garde le tag courant et on continue
      [else
       (cons (car ts)
             (loop (cdr ts)))])))


(define (rule-E2-part-v-N->DET:poss tokens tags)
  (let loop ([ts tags])
    (cond
      ;; plus de tags du tout
      [(null? ts)
       '()]

      ;; un seul tag restant : on ne peut plus faire PART-V N
      [(null? (cdr ts))
       (list (car ts))]

      [else
       (define tag0 (car ts))
       (define tag1 (cadr ts))

       (define new-tag0
         (if (and (string=? tag0 "PART-V")
                  (string=? tag1 "N"))
             "DET:poss"
             tag0))

       (cons new-tag0
             (loop (cdr ts)))])))


;; Règle : "DET:poss-N_VERB_AUX:Bezañ" 
(define (rule-ma->scj tokens tags)
  (let loop ([ts tags])
    (cond
      ;; Fin de liste
      [(null? ts)
       '()]

      ;; Plus assez d’éléments pour faire un couple (car + cadr)
      [(null? (cdr ts))
       (list (car ts))]

      ;; Cas où on a : DET:poss  N_VERB_AUX:Bezañ
      [(and (string=? (car ts)  "DET:poss")
            (string=? (cadr ts) "N_VERB_AUX:Bezañ"))
       (cons "SCJ"
             (cons "AUX:Bezañ"
                   (loop (cddr ts))))]

      ;; Sinon, on recopie le tag courant et on continue
      [else
       (cons (car ts)
             (loop (cdr ts)))])))

;2️⃣ "DET:art-i_PART-V" "N" → "DET:art-i" "N"

;Même style que ta règle rule-E4 :

(define (rule-det-art-i-part-v->det-art-i tokens tags)
  (let loop ([ts tags])
    (cond
      ;; plus de tags
      [(null? ts)
       '()]

      ;; un seul tag restant : on ne peut plus matcher 2 tags
      [(null? (cdr ts))
       (list (car ts))]

      ;; motif DET:art-i_PART-V N
      [(and (string=? (car ts)  "DET:art-i_PART-V")
            (string=? (cadr ts) "N"))
       (append (list "DET:art-i" "N")
               (cddr ts))]

      ;; sinon, on garde le tag courant et on continue
      [else
       (cons (car ts)
             (loop (cdr ts)))])))

;; 1️⃣ SCJ → CJC si balise gauche = balise droite
;; 
;; Ici on a besoin de regarder la balise précédente et la suivante.
;; On garde donc ton schéma let loop mais avec un paramètre supplémentaire prev :

(define (rule-SCJ->CJC tokens tags)
  (let loop ([prev #f] [ts tags])
    (cond
      ;; plus de tags
      [(null? ts)
       '()]

      ;; un seul tag restant : pas de voisin droit, donc pas de règle
      [(null? (cdr ts))
       (list (car ts))]

      [else
       (define cur  (car ts))
       (define next (cadr ts))

       (define new-cur
         (if (and prev
                  (string=? cur "SCJ")
                  (string=? prev next))
             "CJC"
             cur))

       (cons new-cur
             (loop cur (cdr ts)))])))


;    prev = balise précédente

;cur = balise courante

;next = balise suivante
;Si prev = next et cur = "SCJ", on remplace par "CJC".

;"N_VERB" "PART-V" "VERB" en position 0 → "VERB" "PART-V" "VERB"

;Puisque tu veux uniquement en position 0, on peut faire un test direct sur le début de la liste, sans boucle :
          
(define (rule-N_VERB-initial->VERB tokens tags)
  (cond
    ;; au moins 3 tags et motif en tête
    [(and (pair? tags)
          (pair? (cdr tags))
          (pair? (cddr tags))
          (string=? (car tags)   "N_VERB")
          (string=? (cadr tags)  "PART-V")
          (string=? (caddr tags) "VERB"))
     (append (list "VERB" "PART-V" "VERB")
             (cdddr tags))]

    ;; sinon, on ne touche à rien
    [else
     tags]))

(define (stage3-apply tokens tags)
  (let* ([t1  (rule-A-ken-ker-all               tokens tags)]
         [t2  (rule-B-ken-X-hag                 tokens t1)]
         [t3  (rule-C-ken-hyphen                tokens t2)]
         [t4  (rule-E1-neg-neo-ket              tokens t3)]
         [t5  (rule-ken-advscj->scj             tokens t4)]
         [t6  (rule-F1-comp-oc-h                tokens t5)]
         [t7  (rule-E2-det-unkn->N              tokens t6)]
         [t8  (rule-E2-da-bet-eur               tokens t7)]
         [t9  (rule-E3-init-adj                 tokens t8)]
         [t10 (rule-E4-det-art-i-goan           tokens t9)]
         [t11 (rule-E2-part-v-N->DET:poss       tokens t10)]
         [t12 (rule-ma->scj                     tokens t11)]
         [t13 (rule-SCJ->CJC                    tokens t12)]
         [t14 (rule-det-art-i-part-v->det-art-i tokens t13)]
         [t15 (rule-N_VERB-initial->VERB        tokens t14)])
    t15))


(provide(all-defined-out))


;besoin d'une petite regle de match  qui generalise
;si bal-gauche est la meme que balise droite alors "SCJ" => "CJC"
;; et deux autres regles pour l'exemple suivant avec "N_VERB" en position 0
;; 
;; ("N_VERB" "PART-V" "VERB" "DET:art-i_PART-V" "N" "ADJ" "PREP+DET" "N" "PUNCT")
;; ("kinnig" "a" "raint" "ur" "bourmenadenn" "dibar" "d’ar" "weladennerien" ".")
;; 
;; "DET:art-i_PART-V"  "N" =>   "DET:art-i" "N"
;; ("N_VERB" "PART-V" "VERB" pos 0  => ("VERB" "PART-V" "VERB" 