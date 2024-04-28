         CLIPS (6.30 3/17/15)
CLIPS> (deftemplate Symptom
   (slot name)
)

(deftemplate Disease
   (slot name)
   (multislot symptoms)
   (slot treatment)
)

(deffacts Symptoms
   (Symptom (name yellow-leaves))
   (Symptom (name brown-spots))
   (Symptom (name wilting))
)

(deffacts Diseases
   (Disease (name Fusarium-wilt)
            (symptoms yellow-leaves wilting)
            (treatment "Use fungicides and remove infected plants."))
   (Disease (name Leaf-spot)
            (symptoms yellow-leaves brown-spots)
            (treatment "Use fungicides and remove infected leaves."))
   (Disease (name Root-rot)
            (symptoms wilting brown-spots)
            (treatment "Improve drainage and avoid overwatering."))
)

(defrule Diagnose
   (declare (salience -10))
   =>
   (printout t "Enter the symptoms (separated by spaces): ")
   (bind ?symptoms (read))
   (bind ?disease (find-disease ?symptoms))
   (if ?disease then
      (printout t "The plant may have " ?disease " disease." crlf)
      (printout t "Treatment: " (slot-value ?disease treatment) crlf)
   else
      (printout t "Unable to diagnose the disease." crlf)
   )
)

(defun find-disease (?symptoms)
   (loop-for-count (?diseaseIdx 1) to (fact-count)
      (bind ?disease (fact ?diseaseIdx))
      (if (and (eq (type ?disease) disease)
               (eq ?symptoms (intersection ?symptoms (slot-value ?disease symptoms))))
         then
            (return ?disease)
   )
   (return FALSE)
)
CLIPS> 
