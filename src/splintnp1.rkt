#lang racket
(require racket/list)

(require "helpers.rkt")
(require "new-tag1.rkt")
(require "stage1.rkt")
(require "rulesnp.rkt") 
(require "npvp.rkt")

;; ----------------------------
;; 1. Phrase d'exemple = tok+bal
;; ----------------------------

(define a1-1
  (select-bal-principale
   (stage1 (stage0 (Str->m
                    "Bemdez a pren ma zad ar gazetenn er stal.")))))

;; a1-1 a la forme :
;; '(("Bemdez" ((bal . "ADV")))
;;   ("a"      ((bal . "PART-V")))
;;   ("pren"   ((bal . "VERB")))
;;   ...
;;   ("."      ((bal . "PUNCT"))))


;; ----------------------------
;; 2. Flags syntaxiques
;; ----------------------------

(define flag-np '("DET:art-i" "DET:art-d" "DET:poss" "NBR" "N" "ADJ"))
(define flag-vp '("AUX" "VERB" "PART-V"))
(define flag-s  '("ADV" "PUNCT" "INTJ" "CCJ" "SCJ" "PROPN" "PRO" "PREP"))
(define flag-x  '("PREP+DET"))

(define (flag-syn x)
  (cond [(member x flag-np) 'n]
        [(member x flag-vp) 'v]
        [(member x flag-x)  'x]
        [else               's]))


;; ----------------------------
;; 3. Fonctions d'extraction
;; ----------------------------

;; extrait les balises "bal" depuis a1-1
(define (extr-balises l)
  (cond [(null? l) l]
        [else
         (cons (valeur-cle 'bal (last (car l)))
               (extr-balises (cdr l)))]))

;; extrait les tokens depuis a1-1
(define (extr-tok l)
  (cond [(null? l) l]
        [else
         (cons (caar l)
               (extr-tok (cdr l)))]))


;; ----------------------------
;; 4. Adaptateur vers tes règles PREP+DET
;; ----------------------------

;; tags : liste plate de balises
;;   '("DET:art-d" "N" "ADV" "PREP+DET" "N" ...)
;; → on applique analyse-S (qui attend une liste de chunks)
;; → on récupère à nouveau une liste plate
(define (preprocess-tags-with-prep+det tags)
  (define chunks (list tags))        ; on met toute la phrase dans 1 chunk
  (define new-chunks (analyse-S chunks))
  (apply append new-chunks))


;; ----------------------------
;; 5. Np-vp-s1 + Np-splint-automate + restruct-satz + copier-structure
;;    (tes fonctions existantes)
;; ----------------------------

;; (je ne modifie pas ton code ici, je le recopie tel quel)

(define (Np-vp-s1 bal) ; s'applique sur balises uniquement
  (let* ((lg (length bal))
         (tp-flag (list (car bal)))
         (flag-p "")
         (flag-c "")
         (tp-bal (list (car bal)))
         (TP-bal '())
         (it-b "")
         (index 0))
    (letrec
        ((loop
          (lambda ()
            (if (= index lg)
                (cdr (reverse (cons it-b TP-bal)))
                (begin
                  (set! flag-p (car tp-flag))
                  (set! it-b (list-ref bal index))
                  (set! flag-c (flag-syn it-b))
                  (cond
                    [(equal? flag-p flag-c)
                     (set! tp-bal (cons it-b tp-bal))
                     (set! tp-flag (cons flag-c tp-flag))
                     (set! index (+ index 1))
                     (loop)]
                    [(equal? flag-c 'x)
                     (set! TP-bal (cons (reverse tp-bal) TP-bal))
                     (set! tp-bal '())
                     (set! tp-flag (cons 'n tp-flag))
                     (set! tp-bal (cons it-b tp-bal))
                     (set! index (+ index 1))
                     (loop)]
                    [else
                     (set! TP-bal (cons (reverse tp-bal) TP-bal))
                     (set! tp-flag (cons flag-c tp-flag))
                     (set! tp-bal '())
                     (set! tp-bal (cons it-b tp-bal))
                     (set! index (+ index 1))
                     (loop)]))))))
      (loop))))

;; ton automate NP
(define det-tags
  '("DET:art-d" "DET" "DET:poss" "NBR"))

(define (Np-splint-automate tags)
  (let loop ((state 'out)
             (rest tags)
             (current '())
             (result '()))
    (cond
      [(null? rest)
       (reverse
        (if (null? current)
            result
            (cons (reverse current) result)))]
      [else
       (define t    (car rest))
       (define more (cdr rest))
       (case state
         [(out)
          (cond
            [(member t det-tags)
             (loop 'in more (list t) result)]
            [(string=? t "PREP+DET")
             (loop 'in more (list t) result)]
            [(and (string=? t "PART-V")
                  (pair? more)
                  (string=? (car more) "VERB"))
             (loop 'out
                   (cdr more)
                   '()
                   (cons '("PART-V" "VERB") result))]
            [else
             (loop 'out more '() (cons (list t) result))])]
         [(in)
          (cond
            [(or (string=? t "N")
                 (string=? t "ADJ"))
             (loop 'in more (cons t current) result)]
            [else
             (loop 'out
                   rest
                   '()
                   (cons (reverse current) result))])])])))

(define (restruct-satz satz)
  (cond
    [(null? satz) '()]
    [(string? (car satz))
     (cons (list (car satz))
           (restruct-satz (cdr satz)))]
    [(member "N" (car satz))
     (append (Np-splint-automate (car satz))
             (restruct-satz (cdr satz)))]
    [else
     (cons (car satz)
           (restruct-satz (cdr satz)))]))

(define (copier-structure s1 s2)
  (let loop ((s1 s1)
             (s2 s2)
             (acc '()))
    (if (or (null? s1) (null? s2))
        (reverse acc)
        (let* ((elem-c   (car s1))
               (lg-elem-c (length elem-c))
               (groupe   (take s2 lg-elem-c)))
          (loop (cdr s1)
                (drop s2 lg-elem-c)
                (cons groupe acc))))))


;; ----------------------------
;; 6. Pipeline complet BRANCHÉ
;; ----------------------------

(define tok+bal    a1-1)                 ; 👈 alias clair
(define tokens-lst (extr-tok tok+bal))
(define tags-lst-0 (extr-balises tok+bal))         ; tags bruts
(define tags-lst-1 (preprocess-tags-with-prep+det  ; 👈 tes règles PREP+DET
                     tags-lst-0))
(define tags-lst-2 (Np-splint-automate tags-lst-1))
(define chunks     (restruct-satz (Np-vp-s1 tags-lst-2)))
(define tokens*    (copier-structure tags-lst-2 tokens-lst))

;; (define tok+bal   '())
;; (define tokens-lst '())
;; (define tags-lst   '())

(provide(all-defined-out))