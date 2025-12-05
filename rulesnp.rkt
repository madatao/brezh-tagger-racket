#lang racket

;; ----------------------------
;; 1. Exemple de données
;; ----------------------------

(define ex-S
  '(("N" "DET:art-d" "N")
    ("PART-V" "VERB")
    ("ADV")
    ("DET:art-d" "N" "PREP+DET" "N" "N" "N" "DET:art-d" "N")
    ("PUNCT")))

;; ----------------------------
;; 2. Règle : PREP+DET ne peut
;;    être suivi que d'un seul N
;; ----------------------------
;;
;; Si on rencontre ... PREP+DET N X ...
;; où X existe encore (pas fin de liste),
;; on coupe après PREP+DET N :
;;   [avant ...] [PREP+DET N] [reste...]
;;
;; Sinon, on ne touche pas au groupe.

(define (rule-prep+det chunk)
  ;; chunk : '("DET:art-d" "N" "PREP+DET" "N" "N" "N" "DET:art-d" "N")
  (define (loop lst current acc)
    (cond
      ;; fin de liste : on range le dernier morceau
      [(null? lst)
       (reverse
        (if (null? current)
            acc
            (cons (reverse current) acc)))]

      ;; cas où la règle s'applique :
      ;; ... PREP+DET N X ...
      [(and (equal? (car lst) "PREP+DET")
            (pair? (cdr lst))      ; il y a un N après
            (pair? (cddr lst)))    ; il y a encore qqch après le N
       (define new-acc
         (if (null? current)
             acc
             (cons (reverse current) acc)))
       ;; on ajoute le groupe ("PREP+DET" "N")
       (loop (cddr lst)                ; on reprend après ce N
             '()
             (cons (list "PREP+DET" (cadr lst))
                   new-acc))]

      ;; sinon, on continue à accumuler
      [else
       (loop (cdr lst)
             (cons (car lst) current)
             acc)]))
  (loop chunk '() '()))

;; ----------------------------
;; 3. Liste de règles
;;    (facile à étendre)
;; ----------------------------

(define rules
  (list rule-prep+det))
;; Plus tard :
;; (define rules (list rule-prep+det rule-GN rule-GV ...))

;; ----------------------------
;; 4. Application des règles
;; ----------------------------

;; Appliquer toutes les règles à un seul groupe
(define (apply-rules-to-chunk chunk)
  (foldl (lambda (rule chunks)
           ;; chunks est une liste de sous-groupes
           ;; chaque règle peut encore les découper
           (apply append (map rule chunks)))
         (list chunk)
         rules))

;; Analyse d'une phrase entière (liste de groupes)
(define (analyse-S S)
  (apply append (map apply-rules-to-chunk S)))
(provide(all-defined-out))
;; ----------------------------
;; 5. Test
;; ----------------------------

;(module+ test
 ; (displayln (analyse-S ex-S)))
;; Si tu exécutes (analyse-S ex-S), tu obtiens :
;; 
;; racket
;; Copier le code
;; '(("N" "DET:art-d" "N")
;;   ("PART-V" "VERB")
;;   ("ADV")
;;   ("DET:art-d" "N")
;;   ("PREP+DET" "N")
;;   ("N" "N" "DET:art-d" "N")
;;   ("PUNCT"))
;; C’est exactement ce que tu voulais afficher :
;; 
;; racket
;; Copier le code
;; (("N" "DET:art-d" "N")
;;  ("PART-V" "VERB")
;;  ("ADV")
;;  ("DET:art-d" "N")
;;  ("PREP+DET" "N")
;;  ("N" "N" "DET:art-d" "N")
;;  ("PUNCT"))
;; Comment ajouter d’autres règles ensuite ?
;; Tu écris simplement une nouvelle fonction, par exemple :
;; 
;; racket
;; Copier le code
;; (define (rule-quelque-chose chunk)
;;   ;; retourne (list chunk) si rien à faire
;;   ;; ou plusieurs sous-chunks si tu coupes
;;   ...)
;; Puis tu ajoutes dans rules :
;; 
;; racket
;; Copier le code
;; (define rules
;;   (list rule-prep+det
;;         rule-quelque-chose))



