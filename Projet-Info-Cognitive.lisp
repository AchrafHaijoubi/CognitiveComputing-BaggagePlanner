;=============================================;
; Haijoubi Achraf                             ;
; Zouhair Ayman                               ;
; Sebti Zineb                                 ;
; Bour Fatima-Ezzahra                         ;
;=============================================;

(clear-all)

;;; Fonction pour placer les valises dans le coffre

(defun place-valises(n-times &optional (draw-valises nil))

   (setf moyenne 0)

   (dotimes (i n-times)

      ;;(format t "run-blocks iteration: ~d~%" i) ;; for debug only

      (setf compteur 1)
      (setf dim-unite-carre-coffre 36) ;; 6 x 6
      (setf max-valises-hors-coffre 2) ;; Plus que 2 valises en dehors du coffre semble trop.
      (setf not-win t)
      (setf res nil)
      (setf state nil)
      (setf memory-retention nil)
      (setf *valises* (create-valises)); Creation des valises

      (while not-win ; appeler le modèle tant qu'il n'a pas win
         (setf (slot-value (nth 0 *valises*) 'couche) 0)      
         (setf (slot-value (nth 1 *valises*) 'couche) 0)     
         (setf (slot-value (nth 2 *valises*) 'couche) 0)

         (when (>= (length *valises*) 4) (setf (slot-value (nth 3 *valises*) 'couche) 0))
         (when (>= (length *valises*) 5) (setf (slot-value (nth 4 *valises*) 'couche) 0))
         (when (>= (length *valises*) 6) (setf (slot-value (nth 5 *valises*) 'couche) 0))

         (let* ((choix-model (show-model-valises *valises* res state))) ;; Montre les valises au modèle et enregistre la key pressée par le model

            (when (or (null (nth 0 choix-model)) (null (nth 1 choix-model)) (equal "none" (nth 0 choix-model)) (equal "none" (nth 1 choix-model)))
               (print "***** Model Error *****")
               (return 0)
            )

            (progn

               (when (and (equal memory-retention nil) (equal (nth 0 choix-model) "f")) ;; pour conserver la logique de l'ancien model
                  (setf compteur (+ compteur 1))
                  (setf memory-retention t)
               )
               (setf (slot-value (nth 0 *valises*) 'couche) (parse-integer (nth  1 choix-model)))
               (setf (slot-value (nth 1 *valises*) 'couche) (parse-integer (nth  2 choix-model)))
               (setf (slot-value (nth 2 *valises*) 'couche) (parse-integer (nth  3 choix-model)))
               
               (when (>= (length *valises*) 4) (setf (slot-value (nth 3 *valises*) 'couche) (parse-integer (nth 4 choix-model))))
               (when (>= (length *valises*) 5) (setf (slot-value (nth 4 *valises*) 'couche) (parse-integer (nth 5 choix-model))))
               (when (>= (length *valises*) 6) (setf (slot-value (nth 5 *valises*) 'couche) (parse-integer (nth 6 choix-model))))
            )

            (setf res "win") ;; De base on win
            (setf poids-tot-couche-1 0)
            (setf poids-tot-couche-2 0)
            (setf unite-carre-tot-couche-1 0)
            (setf unite-carre-tot-couche-2 0)
            (setf valises-count-couche-0 0)

            (loop for valise in *valises* do ;; boucle sur les valises choisi par le modèle

               (case (slot-value valise 'couche) 
                  (1 (progn 
                        (setf poids-tot-couche-1 (+ poids-tot-couche-1 (slot-value valise 'poids))) ; Addition du poids de la valise
                        (setf unite-carre-tot-couche-1 (+ unite-carre-tot-couche-1 (* (slot-value valise 'x) (slot-value valise 'y))))
                  ))
                  (2 (progn 
                        (setf poids-tot-couche-2 (+ poids-tot-couche-2 (slot-value valise 'poids))) ; Addition du poids de la valise
                        (setf unite-carre-tot-couche-2 (+ unite-carre-tot-couche-2 (* (slot-value valise 'x) (slot-value valise 'y))))
                  ))
                  (0 (progn 
                        (setf valises-count-couche-0 (+ valises-count-couche-0 1)) ; compte le nombre de valises qui ne seront pas dans le coffre
                  ))
               )
            ) ; Adition du poids de la valise
            
            (if (or (> poids-tot-couche-2 poids-tot-couche-1) (> unite-carre-tot-couche-1 dim-unite-carre-coffre) (> unite-carre-tot-couche-2 dim-unite-carre-coffre) (> valises-count-couche-0 max-valises-hors-coffre))
               (progn
                  (if (equal state "weight-problem")
                     ;; TRUE: il y avait deja un state
                     (setf state "weight-problem-2")
                     ;; FALSE
                     (setf state "weight-problem")
                  )
                  (setf res "lose") ; Si les valises en couche 2 sont plus lourdes -> lose
               )
               (progn 
                  (setf not-win nil)
                  (progn 
                     (setf state "final")
                     (show-model-result res state)
                  )
               )
            )
            
         
            (when draw-valises
               (setf valises-to-draw nil)

               (print-valise (nth 0 *valises*))
               (push (nth 0 *valises*) valises-to-draw)

               (print-valise (nth 1 *valises*))
               (push (nth 1 *valises*) valises-to-draw)

               (print-valise (nth 2 *valises*))
               (push (nth 2 *valises*) valises-to-draw)


               (when (>= (length *valises*) 4)
                  (progn 
                     (print-valise (nth 3 *valises*))
                     (push (nth 3 *valises*) valises-to-draw)
                  )
               )

               (when (>= (length *valises*) 5)
                  (progn 
                     (print-valise (nth 4 *valises*))
                     (push (nth 4 *valises*) valises-to-draw)
                  )
               )

               (when (>= (length *valises*) 6)
                  (progn 
                     (print-valise (nth 5 *valises*))
                     (push (nth 5 *valises*) valises-to-draw)
                  )
               )

               (draw-niveau1-niveau2-HorsCoffre *valises*)

            )

         )
      )

      (setf moyenne (+ moyenne compteur))
   )

   (/ (/ moyenne n-times) 3.0)
)


;;afficher valises
(defun show-model-valises(valises &optional res state)

   (let* 
      ( (cat1 (slot-value (nth 0 valises) 'categorie)) 
        (cat2 (slot-value (nth 1 valises) 'categorie))
        (cat3 (slot-value (nth 2 valises) 'categorie))

        (cat4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'categorie) "none"))
        (cat5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'categorie) "none"))
        (cat6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'categorie) "none"))

        (poids1 (slot-value (nth 0 valises) 'poids))
        (poids2 (slot-value (nth 1 valises) 'poids))
        (poids3 (slot-value (nth 2 valises) 'poids))

        (poids4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'poids) "none"))
        (poids5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'poids) "none"))
        (poids6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'poids) "none"))

        (niv1 (slot-value (nth 0 valises) 'couche))
        (niv2 (slot-value (nth 1 valises) 'couche))
        (niv3 (slot-value (nth 2 valises) 'couche))

        (niv4 (if (>= (length *valises*) 4) (slot-value (nth 3 valises) 'couche) "none"))
        (niv5 (if (>= (length *valises*) 5) (slot-value (nth 4 valises) 'couche) "none"))
        (niv6 (if (>= (length *valises*) 6) (slot-value (nth 5 valises) 'couche) "none"))
      )

      (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
         (mod-focus-fct `(c1 ,cat1  c2 ,cat2  c3 ,cat3  c4 , cat4  c5 , cat5  c6 , cat6  
                          p1 ,poids1  p2 ,poids2  p3 ,poids3  p4 , poids4  p5 , poids5  p6 , poids6
                          n1 ,niv1  n2 ,niv2  n3 ,niv3  n4 , niv4  n5 , niv5  n6 , niv6
                              result , res
                              state , state
                           )
         )
         (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                              `((isa arrange-state c1 , cat1  c2 , cat2  c3 , cat3  c4 , cat4  c5 , cat5  c6 , cat6
                                                   p1 , poids1  p2 , poids2  p3 , poids3  p4 , poids4  p5 , poids5  p6 , poids6 
                                                   n1 , niv1  n2 , niv2  n3 , niv3  n4 , niv4  n5 , niv5  n6 , niv6
                                    result , res
                                    state , state))))
         )
      )
   )
   
   (setf *model-action-list* nil) ;; En théorie on doit le remettre à nil si on recommence
   (run-full-time 10)
   (reverse *model-action-list*)
)

(defun show-model-result(res state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal

      (mod-focus-fct `(result ,res
                           state ,state)
      )

      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `((isa arrange-state result ,res
                                 state ,state))))
      )

   )
   
   (run-full-time 3) ;; 10
)

(defun run-blocks (blocks block-size) 
   (setf sum 0)
   (dotimes (i blocks)
      (setf sum (+ sum (place-valises block-size)))
   )
   (/ sum blocks)
)

(defun show-learning (n &optional (graph t))
   (let ((points))

      (dotimes (i n)
         (push (run-blocks 1 100) points)) ;; ici pour des blocs de 100
      (setf points (reverse points))
      (when graph
         (draw-graph points)
      )
   )
) 

(defun draw-graph (points)
   (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
      (allow-event-manager w)
      (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")
      (dotimes (i 11)
         (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (* (- 1 (* i .1)) 3)) :window "Data")
         (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data")
      )
    
      (let ((x 50))
         (mapcar  (lambda (a b) (add-line-to-exp-window (list x (floor (- 410 (* a 400))))
                                                   (list (incf x 25) (floor (- 410 (* b 400))))
                                                      :color 'blue :window "Data")
                  )
                  (butlast points) (cdr points)
         )
      )
      (allow-event-manager w)
   )
)

(defvar *model-action-list* nil) ; La variable que le model devra remplir (liste de valise)

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
   (if (eq win (current-device))
      (progn
         (unless (string= key "s")
            (push (string key) *model-action-list*)
         )
      )
      (unless *human-action*
         (setf *human-action* (string key))
      )
   )
)

;;; Classe valise
(defclass valise()
   (
      (poids :accessor valise-poids)
      (categorie :accessor valise-categorie)
      (couche :accessor valise-couche)
      (x :accessor valise-x)
      (y :accessor valise-y)
   )
)

;;; permet d'afficher les infos d'une valise
(defgeneric print-valise (valise))
(defmethod print-valise ((la-valise valise))
   (format t "La valise pese: ~d, est de categorie ~d, mesure ~dx~d et est positionnee a la couche ~d~%" (slot-value la-valise 'poids) (slot-value la-valise 'categorie) (slot-value la-valise 'x) (slot-value la-valise 'y) (slot-value la-valise 'couche)))

;;; permet de dessiner une valise
(defgeneric draw-valise (valise))
(defmethod draw-valise ((la-valise valise))
   (format t " ")
   (dotimes (i (slot-value la-valise 'x))
      (format t "__")
   )
   (format t "~%")
   (dotimes (i (slot-value la-valise 'y))
      (format t "|")
      
      (dotimes (i (slot-value la-valise 'x))
         (format t "__")
      )
      (format t "|~%")
   )
)

(defun draw-esthetique (valises)
   (setf nb 0)
   (setf grandevalise (car valises))

   (loop for valise in valises
      do (if (= (slot-value valise 'categorie) 1)
         (setf nb (+ nb 1))
         (setf grandevalise valise))
   )

   (if (>= nb 2)
      (progn 
         (draw2little)
         (draw-valise grandevalise)
      )
      (progn
         (loop for valise in valises do
            (progn (draw-valise valise) (format t "~%"))
         )
      )
   )
)

(defun draw-niveau1-niveau2-HorsCoffre (valises)
   (setf valises-niveau1 nil)
   (setf valises-niveau2 nil)
   (setf valises-HorsCoffre nil)

   (loop for valise in valises do
      (case (slot-value valise 'couche)
         (1 (push valise valises-niveau1))
         (2 (push valise valises-niveau2))
         (0 (push valise valises-HorsCoffre))
      )
   )

   (progn 
      (format t "Niveau 1:~%")
      (when (>= (length valises-niveau1) 1)
         (draw-esthetique valises-niveau1)
      )
      (format t "~%Niveau 2:~%")
      (when (>= (length valises-niveau2) 1)
         (draw-esthetique valises-niveau2)
      )
      (format t "Valises non placees :~%")
      (when (>= (length valises-HorsCoffre) 1)
         (draw-esthetique valises-HorsCoffre)
      )
   )

)

(defun draw2little()
   (format t " ______  ______~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
   (format t "|______||______|~%")
)

;; fonction qui cree les valises de nombre entre 3 et 6

(defun create-valises()
  ;; Génère un nombre aléatoire de valises entre 3 et 6
  (let* ((nb-valises (+ 3 (act-r-random 4))) ; Nombre aléatoire entre 3 et 6
         (valise-list (loop repeat nb-valises collect (make-instance 'valise)))) ; Crée les valises
    ;; Configure chaque valise
    (loop for valise in valise-list
       do (progn
            (setf (slot-value valise 'poids) (1+ (act-r-random 5))) ; poids aléatoire
            (setf (slot-value valise 'categorie) (1+ (act-r-random 3))) ; catégorie aléatoire
            (setf (slot-value valise 'couche) 1) ; par défaut, couche 1
            ;; Dimensions selon la catégorie
            (case (slot-value valise 'categorie)
              (1 (progn (setf (slot-value valise 'x) 3) (setf (slot-value valise 'y) 3)))
              (2 (progn (setf (slot-value valise 'x) 6) (setf (slot-value valise 'y) 2)))
              (3 (progn (setf (slot-value valise 'x) 6) (setf (slot-value valise 'y) 3))))))

    ;; Trie les valises par poids décroissant puis par catégorie croissante
    (sort-by-poids-first valise-list)))



;;fonction de calcule d'espace occupé

(defun espace-occupe (cat)
   (setf espace "error-calcul-espace")
   (setf catt cat)
   (when (equal cat "none") (setf catt 0))
   (case catt
      (1 (setf espace 9))
      (2 (setf espace 12))
      (3 (setf espace 18))
      (0 (setf espace 0))
   )
   espace
)




(defun compare-valises (x y)
  "Compare deux valises en fonction de leur poids et, en cas d'égalité, par catégorie."
  (cond
    ((/= (valise-poids x) (valise-poids y))
     (> (valise-poids x) (valise-poids y)))
    (t
     (< (valise-categorie x) (valise-categorie y)))))
;; fonction qui retourne une liste ordonnee des valises

(defun sort-by-poids-first (vlist)
  "Trie les valises par poids décroissant, puis par catégorie croissante en cas d'égalité."
  (stable-sort vlist #'compare-valises))


;;; Modèle ACT-R : 

(define-model baggage-organization
(sgp :v t)
(sgp :esc nil :ans 0.4 :bll 0.5  :ncnar nil :rt 0 :pas nil :show-focus t :trace-detail low)

(install-device (open-exp-window "" :visible nil))

(chunk-type arrange-state  c1 c2 c3 c4 c5 c6  p1 p2 p3 p4 p5 p6  n1 n2 n3 n4 n5 n6  encoding-status (memory-retention "f") sum1 sum2 result state)
(chunk-type learned-info  c1 c2 c3 c4 c5 c6  p1 p2 p3 p4 p5 p6  n1 n2 n3 n4 n5 n6)
(declare-buffer-usage goal arrange-state :all)


(add-dm)

(p start
   =goal>
        isa arrange-state
        state nil
        memory-retention "f"

        c1  =cat1
        c2  =cat2
        c3  =cat3
        c4  =cat4
        c5  =cat5
        c6  =cat6

        p1  =poids1
        p2  =poids2
        p3  =poids3
        p4  =poids4
        p5  =poids5
        p6  =poids6
   ==>
   +retrieval>
        isa learned-info
        c1  =cat1
        c2  =cat2
        c3  =cat3
        c4  =cat4
        c5  =cat5
        c6  =cat6

        p1  =poids1
        p2  =poids2
        p3  =poids3
        p4  =poids4
        p5  =poids5
        p6  =poids6

      - n1 nil
      - n2 nil
      - n3 nil
      - n4 nil
      - n5 nil
      - n6 nil
   =goal>
        state remembering-phase
)

(p remember-organization
    =goal>
       isa arrange-state
       state remembering-phase
    =retrieval>
       isa learned-info

       n1 =niv1
       n2 =niv2
       n3 =niv3
       n4 =niv4
       n5 =niv5
       n6 =niv6
    ==>
    =goal>
       state encoding-phase
       encoding-status finalization-phase
       result "win"
       memory-retention "t"

       n1 =niv1
       n2 =niv2
       n3 =niv3
       n4 =niv4
       n5 =niv5
       n6 =niv6
)

(p doesnt-remember-organization
    =goal>
       isa arrange-state
       state remembering-phase
    ?retrieval>
       buffer  failure
    ==>
     =goal>
        state org-stage-1
)


(p espace-initial
   =goal>
      state org-stage-1

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
      
   ==>
   !bind! =sum (+ (espace-occupe =cat1) (espace-occupe =cat2) (espace-occupe =cat3))
   =goal>
      state org-stage-2
      sum1 =sum
      n1 1
      n2 1
      n3 1
)


(p cas-depassement-niveau-1
   =goal>
      state org-stage-2

      sum1 =sum
   >  sum1 36

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
   ==>
   !bind! =ssum (- =sum (espace-occupe =cat3))
   !bind! =ssum2 (espace-occupe =cat3)
   =goal>
      state org-stage-3
      n3 2
      sum1 =ssum
      sum2 =ssum2
)


(p cas-non-depassement-niveau-1
   =goal>
      state org-stage-2

      sum1 =sum
   <= sum1 36

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
   ==>
   =goal>
      state org-stage-3
      sum1 =sum
      sum2 0
      n3 1
)



(p espace-libre
   =goal>
      state org-stage-3

      sum2 =sum2

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
   ==>
   !bind! =ssum2 (+ =sum2 (espace-occupe =cat4) (espace-occupe =cat5))
   =goal>
      state org-stage-4
      sum2 =ssum2
      n4 2
      n5 2
      n6 2
)


(p cas-depassement-niveau-2
   =goal>
      state org-stage-4

      sum2 =sum2
   >  sum2 36

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
   ==>
   !bind! =ssum2 (- =sum2 (espace-occupe =cat5))
   =goal>
      state encoding-phase
      encoding-status comparing_weight
      sum2 =ssum2
      n5 0
      n6 0
)



(p cas-non-depassement-niveau-2
   =goal>
      state org-stage-4

      sum2 =sum2
   <= sum2 36

      c1 =cat1
      c2 =cat2
      c3 =cat3
      c4 =cat4
      c5 =cat5
      c6 =cat6
   ==>
   =goal>
      state encoding-phase
      encoding-status comparing_weight
      sum2 =sum2
      n5 2
      n6 0
)


(p encode-remember
   =goal>
         isa arrange-state
         state encoding-phase
         memory-retention =rem
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat1
      +manual>
         cmd press-key
         key =rem
)

(p encoder-cat1
      =goal>
         isa arrange-state
         state write-cat1
         n1 =niv1
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat2
      +manual>
         cmd press-key
         key =niv1
)

(p encoder-cat2
      =goal>
         isa arrange-state
         state write-cat2
         n2 =niv2
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat3
      +manual>
         cmd press-key
         key =niv2
)

(p encoder-cat3
      =goal>
         isa arrange-state
         state write-cat3
         n3 =niv3
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat4
      +manual>
         cmd press-key
         key =niv3
)

(p encoder-cat4
      =goal>
         isa arrange-state
         state write-cat4
         n4 =niv4
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat5
      +manual>
         cmd press-key
         key =niv4
)

(p encoder-cat5
      =goal>
         isa arrange-state
         state write-cat5
         n5 =niv5
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state write-cat6
      +manual>
         cmd press-key
         key =niv5
)

(p encoder-cat6
      =goal>
         isa arrange-state
         state write-cat6
         encoding-status =next-state
         n6 =niv6
      ?manual>
         state free
   ==>
      =goal>
         isa arrange-state
         state =next-state
      +manual>
         cmd press-key
         key =niv6
)

(p memorize
    =goal>
        state "final"
        result "win"
        c1  =cat1
        c2  =cat2
        c3  =cat3
        c4  =cat4
        c5  =cat5
        c6  =cat6

        p1  =poids1
        p2  =poids2
        p3  =poids3
        p4  =poids4
        p5  =poids5
        p6  =poids6

        n1 =niv1
        n2 =niv2
        n3 =niv3
        n4 =niv4
        n5 =niv5
        n6 =niv6
    ?imaginal>
        state free    
    ==>
    =goal>
        state finalization-phase
    +imaginal>
        c1  =cat1
        c2  =cat2
        c3  =cat3
        c4  =cat4
        c5  =cat5
        c6  =cat6

        p1  =poids1
        p2  =poids2
        p3  =poids3
        p4  =poids4
        p5  =poids5
        p6  =poids6

        n1 =niv1
        n2 =niv2
        n3 =niv3
        n4 =niv4
        n5 =niv5
        n6 =niv6
)

(p show-organization
   =goal>
      state "none"
      result "win"
      n1 =niv1
      n2 =niv2
      n3 =niv3
      n4 =niv4
      n5 =niv5
      n6 =niv6
   ?manual>
      state free
    ==>
)

(p clear-new-imaginal-chunk
    ?imaginal>
        state free
        buffer full
    ==>
    -imaginal>
)

)