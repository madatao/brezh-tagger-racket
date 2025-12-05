#lang racket


(require "fonctions.rkt") 
 (require "new-tag1.rkt")

;; ============================================================
;; Helpers généraux
;; ============================================================

;; Renvoie #t si le token tok a une balise dont le début correspond à prefix
(define (has-bal-prefix? tok prefix)
  
  (define bal (valeur-cle 'bal (cadr tok)))
  (and (string? bal)
       (regexp-match (regexp (string-append "^" prefix)) bal)))

(define (noun? tok) (has-bal-prefix? tok "N"))
(define (adv?  tok) (has-bal-prefix? tok "ADV"))
(define (adj?  tok) (has-bal-prefix? tok "ADJ"))




;; renvoie #t si la liste de balises contient tag

;**********************************************
(define (nom->verb-token tok)
  (define form  (car tok))   ; "pren"
  (define feats (cadr tok))  ; ((prov . dict*) (bal . "N_"))
  (define res   (nom->verb form suf-inf$))

  ;; si nom->verb n’a rien trouvé, il renvoie juste form (string)
  (cond
    [(string? res)
     #f] ; pas de verbe, on ne fait rien

    ;; sinon, on attend : (lemm ((bal . "VERB") (lemm . ...) (dess . ...) (inf . ...)))
    [(and (pair? res)
          (pair? (cdr res))
          (list? (cdr res)))
     (define lemm  (car res))
     (define feats-verb (cadr res)) ; ((bal . "VERB") (lemm . ...) (dess . ...) (inf . ...))
     ;; on pose prov=stage1 + les features renvoyées par nom->verb
     (list lemm
           (cons (cons 'prov 'stage1)
                 feats-verb))]

    [else
     #f]))

;; ============================================================
;; Règle 1 : particule verbale PART-V + nom -> verbe
;; ============================================================
; tu peux ajouter une micro-règle :

;PART-V (attendu = "VERB") + VERB →
;juste confirmer la particule : on enlève attendu, on met prov . stage1.

;Code minimal (on réutilise fixer-particule-verbale) :

(define (regle-part-v+verb cur nxt)
  (define feats0 (cadr cur))
  (define bal0   (valeur-cle 'bal feats0))

  (define feats1 (cadr nxt))
  (define bal1   (valeur-cle 'bal feats1))

  (cond
    [(and (string? bal0)
          (string=? bal0 "PART-V")
          (string? bal1)
          (regexp-match #rx"^VERB" bal1))
     ;; on ne change que la particule
     (list (fixer-particule-verbale cur))]
    [else
     #f]))
;; nettoie la particule "e" (ou autre PART-V) quand elle a trouvé son verbe :
;; - enlève 'attendu
;; - met (prov . stage1)
(define (fixer-particule-verbale tok)
  (define orth  (car tok))
  (define feats (cadr tok))
  (define feats-sans-attendu
    (filter (lambda (p) (not (eq? (car p) 'attendu)))
            feats))
  (list orth
        (cons (cons 'prov 'stage1)
              (filter (lambda (p) (not (eq? (car p) 'prov)))
                      feats-sans-attendu))))

;; règle : PART-V + N_  => PART-V corrigé + nom->verb(N)
;; cur : ("e"   ((prov . stage0) (attendu . "VERB") (bal . "PART-V")))
;; nxt : ("pren"((prov . dict*)  (bal . "N_")))
;;
;; renvoie :
;;   '(( "e"   (...) ) ( "prenañ" (...) ))
;; ou #f si non applicable
(define (regle-part-v+nom->verb cur nxt)
  (define tok0   (car cur))
  (define feats0 (cadr cur))
  (define bal0   (valeur-cle 'bal feats0))

  (define tok1   (car nxt))
  (define feats1 (cadr nxt))
  (define bal1   (valeur-cle 'bal feats1))

  (cond
    [(and (string? bal0)
          (string=? bal0 "PART-V")
          (string? bal1)
          (regexp-match #rx"^N" bal1)) ; N, N_, N:...
     (define maybe-verb (nom->verb-token nxt))
     (if maybe-verb
         (list (fixer-particule-verbale cur)
               maybe-verb)
         #f)]
    [else #f]))

;; ============================================================
;; Règle 2 : "ma" + N -> DET:poss (et non DET:poss_SCJ)
;; ============================================================

;; corrige "ma" DET:poss_SCJ -> DET:poss, prov -> stage1
;; tok : ("ma" ((prov . mut*) (bal . "DET:poss_SCJ") (m$ . "S/M")))
;; ->  ("ma" ((prov . stage1) (bal . "DET:poss") (m$ . "S/M")))
(define (fixer-ma-det-poss tok)
  (define orth  (car tok))
  (define feats (cadr tok))
  (define new-feats
    (map (lambda (p)
           (cond
             [(eq? (car p) 'prov) (cons 'prov 'stage1)]
             [(eq? (car p) 'bal)  (cons 'bal "DET:poss")]
             [else p]))
         feats))
  (list orth new-feats))

;; règle : "ma" DET:poss_SCJ + N -> "ma" DET:poss
;; renvoie '(nouveau-ma) ou #f
(define (regle-ma-det-poss cur nxt)
  (define tok0   (car cur))
  (define feats0 (cadr cur))
  (define bal0   (valeur-cle 'bal feats0))

  (define tok1   (car nxt))
  (define feats1 (cadr nxt))
  (define bal1   (valeur-cle 'bal feats1))

  (cond
    [(and (string=? tok0 "ma")
          (string? bal0)
          (regexp-match #rx"DET:poss" bal0) ; contient DET:poss
          (string? bal1)
          (regexp-match #rx"^N" bal1))      ; bal1 = N, N_, N:...
     (list (fixer-ma-det-poss cur))]
    [else
     #f]))

;; ADJ_N_ après VERB/ADV -> ADJ
;; ADJ_N_ dans contexte VERB/ADV -> ADV
(define (fixer-adj_n_-en-adv tok)
  (define orth  (car tok))
  (define feats (cadr tok))
  (define new-feats
    (map (lambda (p)
           (cond
             [(eq? (car p) 'prov) (cons 'prov 'stage1)]
             [(eq? (car p) 'bal)  (cons 'bal "ADV")]
             [else p]))
         feats))
  (list orth new-feats))


;; prev : token précédent
;; cur  : token courant
;; si prev est VERB ou ADV_ et cur a bal = ADJ_N_ -> on corrige cur
;; renvoie le nouveau token ou #f
(define (regle-adj_n_-apres-verbe-ou-adv prev cur)
  (define bal-prev (valeur-cle 'bal (cadr prev)))
  (define bal-cur  (valeur-cle 'bal (cadr cur)))
  (if (and (string? bal-prev)
           (or (regexp-match #rx"^VERB" bal-prev)
               (regexp-match #rx"^ADV"  bal-prev))
           (string? bal-cur)
           (regexp-match #rx"^ADJ_N" bal-cur))
      (fixer-adj_n_-en-adv cur)
      #f))

;; "da" ADJ_ADV_PREP + VERB -> PREP
(define (fixer-da-en-prep tok)
  (define orth  (car tok))
  (define feats (cadr tok))
  (define new-feats
    (map (lambda (p)
           (cond
             [(eq? (car p) 'prov) (cons 'prov 'stage1)]
             [(eq? (car p) 'bal)  (cons 'bal "PREP")]
             [else p]))
         feats))
  (list orth new-feats))

;; UNKN dans un contexte adverbial -> ADV
(define (fixer-unkn-en-adv tok)
  (define orth  (car tok))
  (define feats (if (and (pair? tok) (pair? (cdr tok)))
                    (cadr tok)
                    '()))
  (list orth
        (list (cons 'prov 'stage1)
              (cons 'bal  "ADV"))))
;b) Règle da avant verbe

(define (regle-da-avant-verb cur nxt)
  (define tok0   (car cur))
  (define bal0   (valeur-cle 'bal (cadr cur)))
  (define bal1   (and nxt (valeur-cle 'bal (cadr nxt))))
  (cond
    [(and (string=? tok0 "da")
          (string? bal0)
          (string=? bal0 "ADJ_ADV_PREP")
          (string? bal1)
          (regexp-match #rx"^VERB" bal1))
     (list (fixer-da-en-prep cur))]
    [else
     #f]))

;a) Fixeur général : mettre un token en VERB
(define (fixer-token-en-verb tok)
  (define orth  (car tok))
  (define feats (cadr tok))
  ;; On enlève les anciens prov/bal et on met prov=stage1, bal=VERB
  (define feats-sans-prov/bal
    (filter (lambda (p)
              (let ([k (car p)])
                (and (not (eq? k 'prov))
                     (not (eq? k 'bal)))))
            feats))
  (list orth
        (cons (cons 'prov 'stage1)
              (cons (cons 'bal "VERB")
                    feats-sans-prov/bal))))

;b) Règle PART-V force VERB
(define (regle-part-v-force-verb prev cur)
  (define bal-prev (valeur-cle 'bal (cadr prev)))
  (define bal-cur  (valeur-cle 'bal (cadr cur)))
  (if (and (string? bal-prev)
           (string=? bal-prev "PART-V")
           (string? bal-cur)
           (or (regexp-match #rx"VERB" bal-cur)       ; VERB_, N_VERB_, etc.
               (regexp-match #rx"ADV_INTJ" bal-cur))) ; ton "vo"
      (fixer-token-en-verb cur)
      #f))

;; si prev est VERB et cur est UNKN (ou sans balise),
;; on force cur en ADV
(define (regle-unkn-apres-verb prev cur)
  (define bal-prev (valeur-cle 'bal (cadr prev)))
  (define bal-cur  (valeur-cle 'bal (cadr cur)))
  (if (and (string? bal-prev)
           (regexp-match #rx"^VERB" bal-prev)
           (or (not bal-cur)
               (string=? bal-cur "UNKN")))
      (fixer-unkn-en-adv cur)
      #f))

;; ============================================================
;; Orchestrateur de règles
;; ============================================================

;; cur, nxt, nxt2 : tokens
;; renvoie :
;;   - une liste de 2 tokens (cas PART-V + N -> VERB)
;;   - une liste de 1 token (cas ma + N -> ma corrigé)
;;   - ou #f si aucune règle ne s'applique
(define (appliquer-regles cur nxt nxt2)
  (or (regle-part-v+nom->verb cur nxt)
      (regle-part-v+verb      cur nxt)
      (regle-ma-det-poss      cur nxt)
      (regle-da-avant-verb    cur nxt)  ; <-- nouvelle règle ici
      #f))

;; ============================================================
;; Moteur stage1
;; ============================================================

;; l : liste de tokens
;; chaque token est de la forme ("mot" ((bal . "...") ...))



(define (stage1 l)
  (let loop ((rest l)
             (out '()))
    (cond
      [(null? rest)
       (reverse out)]

      [else
       (define cur  (car rest))
       (define nxt  (and (pair? (cdr rest))  (cadr rest)))
       (define nxt2 (and (pair? (cdr rest))
                         (pair? (cddr rest))
                         (caddr rest)))
       (define prev (and (pair? out) (car out))) ; dernier token déjà traité

       (cond
         ;; Cas avec au moins 2 tokens : on tente d'abord les règles "vers la droite"
         [(and cur nxt)
          (define res (appliquer-regles cur nxt nxt2))
          (cond
            ;; règle qui renvoie 2 tokens (ex : e + pren)
            [(and res (list? res) (> (length res) 1))
             (loop (cddr rest)
                   (append (reverse res) out))]

            ;; règle qui renvoie 1 token (ex : ma + N, da + VERB)
            [(and res (list? res) (= (length res) 1))
             (loop (cdr rest)
                   (cons (car res) out))]

            ;; aucune règle "vers la droite" : on tente les règles "vers la gauche"
            [else
             (define new-cur
               (and prev
                    (or (regle-adj_n_-apres-verbe-ou-adv prev cur)
                        (regle-part-v-force-verb          prev cur)
                        (regle-unkn-apres-verb            prev cur))))
             (if new-cur
                 (loop (cdr rest)
                       (cons new-cur out))
                 (loop (cdr rest)
                       (cons cur out)))])]

         ;; Cas où il ne reste qu'un seul token (pas de nxt) :
         ;; pas de règles "vers la droite", mais on peut encore regarder à gauche
         [else
          (define new-cur
            (and prev
                 (or (regle-adj_n_-apres-verbe-ou-adv prev cur)
                     (regle-part-v-force-verb          prev cur)
                     (regle-unkn-apres-verb            prev cur))))
          (if new-cur
              (loop (cdr rest)
                    (cons new-cur out))
              (loop (cdr rest)
                    (cons cur out)))])])))

;(regle-n-adv-ambig->pp prev2 prev cur)





(define ex1
  '(
    ("Ne" ((prov . stage1) (bal . "PART-V") (m$ . "A")))
   ("oa" ((prov . stage1) (bal . "VERB") (vb . "bezañ") (tps . "imp") (pers . "Sg3")))
   ("ket" ((prov . dict*) (bal . "ADV_")))
   ("ma" ((prov . stage1) (bal . "DET:poss") (m$ . "S/M")))
   ("zad" ((prov . dict*) (Mut . S) (bal . "N_")))
   ("o" ((prov . stage1) (bal . "PART-V")))
   ("prenañ" ((prov . stage1) (bal . "VERB")))
   ("ar" ((prov . mut*) (bal . "DET:art-d") (m$ . "A")))
   ("gazetenn" ((prov . dict*) (Mut . A) (bal . "N_") (g . "f")))
   ("." ((bal . "PUNCT")))))

(define (select-bal-principale ex1);on doit mettre le premier element en minuscule
(cond((null? ex1 )ex1)
     ((cons (list (car (car ex1))(acons 'bal (norm-bal(valeur-cle 'bal (cadar ex1)))'()))(select-bal-principale (cdr ex1))))))



(provide(all-defined-out))
