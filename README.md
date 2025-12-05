# brezh-tagger-racket
Breton Rule-Based Morphosyntactic Tagger in Racket
📘 README.md — VERSION FINALE
# Tager Morpho-Syntaxek Brezhonek e Racket  
### Breton Rule-Based Morphosyntactic Tagger in Racket  
### Tagger morpho-syntaxique breton en Racket

![Language](https://img.shields.io/badge/Language-Breton-blue)
![Racket](https://img.shields.io/badge/Racket-%3E%3D8.0-red)
![License](https://img.shields.io/badge/License-MIT-green)
![Status](https://img.shields.io/badge/Status-Active-brightgreen)

---


       ___             _                
      | _ ) ___ _ __ | |_ ___ _ _ _ __  
      | _ \/ -_) '  \|  _/ -_) '_| '  \ 
      |___/\___|_|_|_|\__\___|_| |_|_|_|

    Breton Rule-Based Morphosyntactic Tagger


---

# 🟩 1. **BREZHONEG**

## 📌 Deskrivadur

Ar raktres-mañ zo un **tager morfo-syntaxek evit ar brezhoneg**, savet e **Racket**, diazezet war :
- reolennoù morfologel,
- reolennoù syntaksel (clauses a Horn),
- reolennoù diboell (*pattern matching*),
- reizhiad demutat brezhoneg (A, M, S, R).

Ret eo digeriñ ar restr bennañ :

`mon-interface1.rkt`

A gevreo gant :



helpers.rkt
new-tag1.rkt
stage1.rkt
splintnp1.rkt
rulesnp.rkt
truk-pattern.rkt


---

## 🧰 3 Mod Implij

### 1️⃣ Enankañ ur frazenn dre zorn  
Aes da arnodañ ur ger, ur frazenn, pe un tamm frazenn.

### 2️⃣ Mod "restr"  
Gant skouerioù :
- E. Chalm (Grammaire),
- Ar Prosez (Kafka / Cornillet),
- pe ho korpus deoc'h.

### 3️⃣ Mod testennoù parzhioù  
Gant :
- Korpus an Ofis: >30 000 frazenn  
- Geriadur: ~50 000 enank  

---

## 🧩 Strukturiad ar strollad tagger



TOKENIZING → DEMUTATION → MORPHOLOGY → SYNTAX RULES → OUTPUT


### Skema ar pipeline


+-------------+
|  TOKENIZE   |
+-------------+
        |
        v
+----------------+
|  DEMUTATION    |  (A / M / S / R)
+----------------+
        |
        v
+----------------------+
|  MORPHOLOGY RULES    |
+----------------------+
        |
        v
+----------------------+
|  SYNTAX RULES (NP)   |
+----------------------+
        |
        v
+----------------------+
|   FINAL TAG OUTPUT   |
+----------------------+


---

## 🎯 Palioù  
- Reiñ un doare skoazellet da zeskiñ yezhoniezh ar brezhoneg  
- Kinnig ur tagger diazezet war reolennoù, hep stlennadoù bras  
- Servijout d'an dud a fell dezho NLP e brezhoneg  
- Lakaat ar yezh da vevañ er XXIvet kantved

---

## 🤝 Trugarez  
D'an holl a fell dezho **kenderc'hel al labour**.  
Plijadur ganeoc'h !

---

# 🟦 2. **ENGLISH**

## 📌 Description

This repository contains a **Breton rule-based morpho-syntactic tagger**, implemented in **Racket**, using:

- morphological rules  
- syntactic Horn clauses  
- pattern matching  
- full Breton demutation system (A, M, S, R mutations)

Main entry point :

`mon-interface1.rkt`

Supporting modules:



helpers.rkt
new-tag1.rkt
stage1.rkt
splintnp1.rkt
rulesnp.rkt
truk-pattern.rkt


---

## 🧰 3 Operating Modes

### 1️⃣ Direct keyboard input  
Test any Breton word or sentence quickly.

### 2️⃣ File mode  
Examples:
- E. Chalm grammar  
- Kafka (*Der Prozess*, Cornillet translation)  
- Any external corpus

### 3️⃣ Parallel text mode  
Compatible with:
- OFIS Breton corpus (>30,000 sentences)  
- 50,000-entry dictionary  

---

## 🧩 Tagger Architecture



TOKENIZER → DEMUTATION → MORPHOLOGY → SYNTAX RULES → OUTPUT


### Pipeline Diagram



TOKENIZE
↓
DEMUTATION (A/M/S/R)
↓
MORPHOLOGY RULES
↓
SYNTAX RULES (NP/VG)
↓
FINAL TAGGED OUTPUT


---

## 🎯 Goals
- Provide a **transparent**, rule-based NLP model in Racket  
- Support Breton NLP without large corpora  
- Offer a pedagogical model for linguists/nlp learners  
- Contribute to language revitalization through technology  

---

## 🤝 Acknowledgments  
For all who wish to **continue this work**.  
Enjoy exploring it!

---

# 🟥 3. **FRANÇAIS**

## 📌 Description

Ce dépôt contient un **tagger morpho-syntaxique breton**, basé entièrement sur des **règles linguistiques** et développé en **Racket**.  
Il implémente :

- des règles morphologiques  
- des clauses de Horn  
- du pattern matching  
- un système complet de démution (A / M / S / R)

Fichier principal :

`mon-interface1.rkt`

Modules associés :



helpers.rkt
new-tag1.rkt
stage1.rkt
splintnp1.rkt
rulesnp.rkt
truk-pattern.rkt


---

## 🧰 3 Modes d’utilisation

### 1️⃣ Entrée directe  
Écrire une phrase en breton dans le champ.

### 2️⃣ Mode « fichier »  
Avec :
- exemples de la grammaire de E. Chalm  
- extraits du *Procès* (Kafka / Cornillet)  
- tout corpus à vous

### 3️⃣ Mode « textes parallèles »  
Avec :
- corpus OFIS (30 000+ phrases)  
- dictionnaire 50 000 entrées  

---

## 🧩 Architecture du Tagger



TOKENISEUR → DÉMUTATION → MORPHOLOGIE → RÈGLES SYNTAXIQUES → SORTIE


### Pipeline



TOKENISATION
↓
DÉMUTATION
↓
ANALYSE MORPHOLOGIQUE
↓
GROUPEMENTS NP / VG
↓
RÈGLES SYNTACTIQUES
↓
SORTIE FINALE


---

## 🎯 Objectifs  
- Montrer un pipeline NLP **à base de règles**  
- Faciliter la recherche en technologie linguistique bretonne  
- Donner un modèle pédagogique clair  
- Permettre de moderniser le traitement automatique du breton  

---

## 🤝 Remerciements  
À celles et ceux qui voudront **poursuivre le travail**.  
Bonne exploration !

---

# 🔧 Example Output



Input:
"Ar plac'h vihan a yeas d'ar skol."

Output (simplified):
'((NP (Ar DET) (plac'h N:f) (bihan ADJ))
(VG (a PART) (yeas V:3S:PAST))
(PP (d'ar PREP+DET) (skol N:f)))


---

# 📜 License  
This project is released under the **MIT License**.
