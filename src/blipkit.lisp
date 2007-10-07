
(defpackage #:blipkit
  (:use #:common-lisp #:cl-prolog))

(in-package :blipkit)

(defunctor file-search-path/2 "file_search_path/2")
(defunctor use-module/1 "use_module/1")

(defunctor bio/1 "bio/1")
(defunctor load-bioresource/1 "load_bioresource/1")
(defunctor load-biofile/2 "load_biofile/2")

(defunctor format/2 "format/2")
(defunctor revcomp/2 "revcomp/2")
(defunctor translate-dna/2 "translate_dna/2")

(defunctor class/2 "class/2")
(defunctor subclass/2 "subclass/2")

;; (setf *current-prolog* (start-prolog :swi-prolog
;;                                     "/usr/local/bin/pl" "-nosignals"))

(setf *current-prolog* (start-prolog :swi-client-prolog
                                     "localhost" 4321))

(setf *current-module* (find-module "user"))

(<- '(file-search-path/2 bio "/home/keith/dev/prolog/bio/blip"))
(<- '(file-search-path/2 ontology "/home/keith/dev/ontology"))


(call '(use-module/1 (bio/1 blipkit)))
(call '(use-module/1 (bio/1 blipkit_ontol)))
(call '(use-module/1 (bio/1 blipkit_fasta)))
(call '(use-module/1 (bio/1 blipkit_sb)))
(call '(use-module/1 (bio/1 blipkit_goa)))
(call '(use-module/1 (bio/1 io)))

(call '(use-module/1 (bio/1 ontol_db)))
(call '(use-module/1 (bio/1 sb_db)))

(call '(use-module/1 (bio/1 bioseq)))

(query '(revcomp/2 TCAG ?seq1))
(query '(translate-dna/2 ATG ?seq1))

(call '(load-biofile/2 obo "/home/keith/dev/ontology/go.obo"))
(call '(load-biofile/2 obo "/home/keith/dev/ontology/so.obo"))

(defunctor class/2 "class/2")
(defunctor subclass/2 "subclass/2")

(defunctor parentT/2 "parentT/2")
(defunctor parentRT/2 "parentRT/2")

; (query '(findall/3 ?x (parentT/2 "GO:0003832" ?x) ?y))
; (query '(findall/3 ?x (parentRT/2 "GO:0003832" ?x) ?y))

(defunctor association/3 "association/3")
;; parentRT('GO:0003832', Parent), goa_db:association(ID, Parent, FeatureID).  