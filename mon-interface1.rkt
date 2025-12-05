#lang racket/gui
(require "helpers.rkt")
(require "new-tag1.rkt")
(require "stage1.rkt")

(require "splintnp1.rkt")
(require "rulesnp.rkt")
(require "truk-pattern.rkt")

(define Analyse$ "")(define Token$ "")(define Balise$ "") (define Np-vp0 "")

;Merc’h ar mestr-skol a bren bemdez ar gazetenn er stal merc’h roue ar vro.

(require srfi/1 srfi/13)
(require (only-in mzlib/string read-from-string-all expr->string))

(require racket/match)

(define style-propn (make-object style-delta% 'change-size 21))
(send style-propn set-delta-foreground "light-blue")

(define style-noun (make-object style-delta% 'change-size 23))
(send style-noun set-delta-foreground "blue")

(define style-verb (make-object style-delta% 'change-size 21))
(send style-verb set-delta-foreground "orange")

(define style-adj (make-object style-delta% 'change-size 21))
(send style-adj set-delta-foreground "orange")

(define style-pro (make-object style-delta% 'change-size 21))
(send style-pro set-delta-foreground "purple")

(define style-punct (make-object style-delta% 'change-size 21))
(send style-punct set-delta-foreground "gray")

(define style-default (make-object style-delta% 'change-size 21))
(send style-default set-delta-foreground "black")

(define ft16$ (make-object font% 16 'modern))
(define ft17$ (make-object font% 17 'modern))

;; largeur d'un texte en pixels
(define (text-width dc s)
  (define-values (w h descent external-leading)
    (send dc get-text-extent s))
  w)

(current-directory)
;; ;; ============================================
;; ;; FRAME PRINCIPALE
;; ;; ============================================
;; 
(define haupt-frame
  (new frame%
       (label "Brezhoneg NLP Claude Talarmiñ Version experimentale 00  26.11.2024")
       (height 500)  (width 1800)
       (stretchable-height #t)
       (x 0)
       (y 0)))

(send haupt-frame show #t)
;; 
(define hp0        (new horizontal-panel% (parent haupt-frame)))
(define menu-bar   (new menu-bar% (parent haupt-frame)))
(define menu-fichier (new menu% (label "Fichier") (parent menu-bar)))
;; 
;; Champ d'entrée texte
(define input-text
  (new text-field%
       (parent hp0)
       (label "")
       (init-value "entrer ou coller le texte brute à analyser")
       (font ft16$)
       (stretchable-width #t)
       (min-width 1800)
       (min-height 200)
       (style '(multiple vertical-label)) 
       ))


;; Menus standard pour l'éditeur
(define edit-menu (new menu% (label "Edit") (parent menu-bar)))
(define font-menu (new menu% (label "Font") (parent menu-bar)))
(append-editor-operation-menu-items edit-menu #f)
(append-editor-font-menu-items font-menu)

;; Messages d'info (si besoin)
(define message1
  (new message%
       (parent haupt-frame)
       (label "")
       (font ft16$)
       (color "green")
       (auto-resize #t)))



;============================================
;; PANELS
;; ============================================

(define vp0  (new vertical-panel%    (parent haupt-frame)))
(define hp1  (new horizontal-panel%  (parent vp0)));affichage des tokens
(define hp2  (new horizontal-panel%  (parent vp0)));affichage des balises
(define hp3  (new horizontal-panel%  (parent vp0)));afficher analyse



(define reference-field
  (new text-field%
       (parent vp0)
       (label "")
       (init-value "reference")
       (font ft16$)
       (stretchable-width #t)
       (style '(multiple vertical-label))
       (min-width 1800)
       (min-height 200)))



(define token-field
  (new text-field%
       (parent vp0)
       (label "")
       (init-value "tokens")
       (font ft16$)
       (stretchable-width #t)
       (style '(multiple vertical-label))
       (min-width 1800)
       (min-height 200)))

(define balise-field
  (new text-field%
       (parent vp0)
       (label "")
       (init-value "balises")
       (font ft16$)
       (stretchable-width #t)
       (style '(multiple vertical-label))
       (min-width 1800)
       (min-height 200)))


(define match-field
  (new text-field%
       (parent vp0)
       (label "")
       (init-value "pattern-matching")
       (font ft16$)
       (stretchable-width #t)
       (style '(multiple vertical-label))
       (min-width 1800)
       (min-height 200)))

(define parall-field
  (new text-field%
       (parent vp0)
       (label "")
       (init-value "text//")
       (font ft16$)
       (stretchable-width #t)
       (style '(multiple vertical-label))
       (min-width 1800)
       (min-height 200)))

(define hp4 (new horizontal-panel%  (parent vp0)))

(define hp5(new horizontal-panel%  (parent vp0)))




(define satz-nr 0)




(define pos-B 0) ;position dans une liste de listes
(define Lg-txt1 0) ; nbre de liste de Text1
(define Lg-txt2 0) ; nbre de liste de Text1


(define Text1 "")
(define Text2 "")
(define Seq-c "")
(define Seq-c2 "")
(define tok+bal "")
(define tokens-lst"")
(define tags-lst-0 "")
(define tags-lst-1 "")
(define tags-lst-2 "")
(define tokens-lst-0"")
(define tokens-lst1 "")
(define tokens-lst2 "")
(define tokens* "")

(define (list-string->string lst)
  (cond
    [(empty? lst) ""]
    [(empty? (rest lst)) (first lst)]
    [else (string-append (first lst)
                         " "
                         (list-string->string (rest lst)))]))


(define bt2
  (new button% (label "lfd//=▶")  (parent hp4)
       (enabled #f);
       (callback (λ (b e)
                   (cond((member pos-B '(9))
                         (set! pos-B (+ 10 pos-B)))
                  
              
            ((< pos-B Lg-txt1 )
                              (begin(set! Seq-c(list-ref Text1 pos-B ))
                                (set! Seq-c2(list-ref Text2 pos-B ))
                                    ( print 'pos-b) ( print pos-B)(newline)
                                
    
          ;; 2. Pipeline stage0 + stage1
         (set! tok+bal (stage1 (stage0  Seq-c)))
          ;(set! tok+bal (stage0 (Str->m Seq-c)))

          ;; 3. Tokens bruts
          (set! tokens-lst (extr-tok tok+bal))

     
          ;; 4. Sélection des balises principales (feld5)
          (set! truc1 (select-bal-principale tok+bal))

          ;; 5. Extraction de la liste des tags à partir de truc1
          (set! tags-lst
                (map (λ (entry)
                       (valeur-cle 'bal (cadr entry)))
                     truc1))

          ;; 6. Affichages de debug
          (send input-text set-value (expr->string (map string->symbol Seq-c)));(expr->string 
          (send reference-field set-value (expr->string tok+bal))
          (send token-field     set-value (expr->string tokens-lst))
          (send balise-field    set-value (expr->string tags-lst))

               (set! tags-lst (stage3-apply tokens-lst tags-lst))
            (send match-field    set-value (expr->string tags-lst))


      
 
          (send parall-field  set-value (expr->string (map string->symbol  Seq-c2)));(expr->string 

          (set! pos-B(+ 1 pos-B)) ))

          )))))
   
;;  
(define bt2-1
  (new button% (label "lfd-mono =▶")  (parent hp4)
       (enabled #f);
       (callback (λ (b e)
                  ( print 'kiki)
          
                
              (cond ((< pos-B Lg-txt1 )
                              (begin(set! Seq-c(list-ref Text1 pos-B ))
         (set! tok+bal (stage1 (stage0  Seq-c)))
 
          ;; 3. Tokens bruts
          (set! tokens-lst (extr-tok tok+bal))

          ;; 4. Sélection des balises principales (feld5)
          (set! truc1 (select-bal-principale tok+bal))

          ;; 5. Extraction de la liste des tags à partir de truc1
          (set! tags-lst
                (map (λ (entry)
                       (valeur-cle 'bal (cadr entry)))
                     truc1))
    ;; 6. Affichages de debug
          (send input-text set-value (expr->string (map string->symbol Seq-c)));(expr->string 
          (send reference-field set-value (expr->string tok+bal))
          (send token-field     set-value (expr->string tokens-lst))
          (send balise-field    set-value (expr->string tags-lst))
       (set! tags-lst (stage3-apply tokens-lst tags-lst))
            (send match-field    set-value (expr->string tags-lst))


          (set! pos-B(+ 1 pos-B)) ))

          )))))
 
 
;(stage3-apply tokens tags)


(define truc1 "")
(define tags-lst "")
(define truc3 "")

;Ma zad a bren bemdez ar gazetenn er stal.


(define bt3
  (new button%
       (label "direct=▶")
       (parent hp4)
       (enabled #t)
       (callback
        (λ (b e)
          ;; 1. Récupérer et normaliser le texte
          (set! Seq-c
                (string-downcase (send input-text get-value)))

          ;; 2. Pipeline stage0 + stage1
         (set! tok+bal (stage1 (stage0 (Str->m Seq-c))))
          ;(set! tok+bal (stage0 (Str->m Seq-c)))

          ;; 3. Tokens bruts
          (set! tokens-lst (extr-tok tok+bal))

          ;; 4. Sélection des balises principales (feld5)
          (set! truc1 (select-bal-principale tok+bal))

          ;; 5. Extraction de la liste des tags à partir de truc1
          (set! tags-lst
                (map (λ (entry)
                       (valeur-cle 'bal (cadr entry)))
                     truc1))

          ;; 6. Affichages de debug
          (send reference-field set-value (expr->string tok+bal))
          (send token-field     set-value (expr->string tokens-lst))
          (send balise-field    set-value (expr->string tags-lst))
         (set! tags-lst (stage3-apply tokens-lst tags-lst))
            (send match-field    set-value (expr->string tags-lst))

          ))))
   
#| 
             ;; 7. Application du stage3 et affichage du résultat
             (send match-field set-value)))))
                (expr->string (stage3-apply/safe tokens-lst tags-lst))))))) |#

(define bt4
  (new button%	 
       (label "Stz-nbr=▶")
       (enabled #f);
       (parent hp4)))

                                                   

                    



(define position-field
  (new text-field%
       (parent hp4)
       (label "position")
       (init-value "0")
       (font ft16$)
       [min-width 50]	 
       [min-height 45]
       (style '(single vertical-label))
    
       ))

(define corpus-taille
  (new text-field%
       (parent hp4)
       (label "nbr Satz")
       (init-value "0")
       (font ft16$)
       [min-width 50]	 
       [min-height 45]
       (style '(single vertical-label))
    
       ))



(define position-text
  (new text-field%
       (parent hp4)
       
       (label "aller à:")
       (init-value "0")
       (font ft16$)
       [min-width 50]	 
       [min-height 45]
       (style '(single vertical-label))
       ))

(define bt5
  (new button% (label "<-")  (parent hp4);les boutons av ar
       (callback (λ (b e)
                   (cond ((> pos-B 0 )
                          (set! pos-B(- pos-B 1))
                          (send  position-field set-value (number->string pos-B))))))))
;           (send  position-field  set-value  (expr->string  (number->string pos-B)))
                   
(define combo-textes-mono
  (new combo-field%
       (parent  hp4)
       (label "Textes monolangue :")(enabled #f)
       (font ft16$)
       (choices '("SYNT1-bzh" ))
       ;(style'(vertical-label)
       (init-value "Chalm")
       (style '(vertical-label))
       ;(font ft16$)
       (callback
        (lambda (cb e)
          (current-directory "/Users/gwallamzer/Desktop/zz/mono/")
          (set! Text1 (send combo-textes-mono get-value))
          (set! Text1 (lire-fichier Text1))
          (set! Lg-txt1 (length Text1))
          (send bt2 enable #f)
          (send bt3 enable #f)
          (send bt2-1 enable #t)
 
          ;pas d'adaptation

          
          ))))


(define txt-paralleles '("SEQ-BZH" "SEQ-FR"))

(define combo-textes-parall
  (new combo-field%
       (parent  hp4)
       (label "Textes + //:")(enabled #f)
       (font ft16$)
       (choices '("SEQ-BZH" ))
       (style '( vertical-label))
       (init-value "Ofis corpus")
  
       ;(font ft16$)
       (callback
        (lambda (cb e)
          (current-directory "/Users/gwallamzer/Desktop/zz/parallele/")
          (set! Textc (send combo-textes-parall get-value))
          (set! Text1 (lire-fichier Textc))
          (set! Lg-txt1 (length Text1))
          (send corpus-taille set-value (number->string  Lg-txt1))
          (send bt2 enable #t)
          (send bt2-1 enable #f)

          (set! Text2 (lire-fichier (cadr(member Textc txt-paralleles))))
          (set! Lg-txt2 (length Text2))
          ))))

(define Textc (send combo-textes-parall get-value))

(define modus '("direct" "fichier"))
(send bt3 enable #t)

(define toggle-btn
  (new button%    
       (label (car modus))
       (parent hp4)
       (callback
        (lambda (btn event)
          (set! modus (reverse modus))
          (send  toggle-btn set-label (car modus))
          (if (equal? (car modus) "direct")
              (begin
                (send bt2 enable #t)
                (send bt3 enable #t)
                (send bt4 enable #f)
                (send combo-textes-parall enable #f)
                (send combo-textes-mono enable #f))                     
              (begin
                (send bt3 enable #f)
                ; (send bt2 enable #t)
                (send combo-textes-parall enable #t)
                (send combo-textes-mono enable #t))
              )))))




(define bt10
  (new button%	 
       (label "dictionaires")
       (enabled #t);
       (parent hp5)))
(define bt11
  (new button%	 
       (label "table de codification")
       (enabled #t);
       (parent hp5)))

(define bt12
  (new button%	 
       (label "Module Statistique")
       (enabled #t);
       (parent hp5)))

(define bt13
  (new button%	 
       (label "mode graphique")
       (enabled #t);
       (parent hp5)))
(define bt14
  (new button%	 
       (label "écouter texte")
       (enabled #t);
       (parent hp5)))
;(send input-text set-value (map string->symbol ss))
