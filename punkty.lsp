(vl-load-com)
;;;
(defun C:punkty (/ el_line layer)
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq util (vla-get-Utility doc))
	(setq layers (vla-get-Layers doc))
	(setq mspace (vla-get-ModelSpace doc))
	(setq pspace (vla-get-PaperSpace doc))
	
	(setq layer (getvar "clayer"))

	(vlax-for obj mspace
		(if (and
				(equal "AcDbPolyline" (vla-get-objectname obj))
				(equal 1 (LM:get-color obj))
				;(or (equal "DASHED" (LM:get-linetype obj))
				;	(equal "DASHED2" (LM:get-linetype obj))
				;)
			)
		(progn
			;(setq cord (vla-get-coordinates obj))
			;(setq cordvar (vlax-variant-value (vla-get-coordinates obj)))
			(setq cordlist (vlax-safearray->list (vlax-variant-value (vla-get-coordinates obj))))
			(setq cordlen (length cordlist))
			
			(print cordlen)
			;(print (vlax-safearray-get-element cordvar 10))
			;(print (vlax-safearray-get-l-bound cordvar 0))
				
			  

		)
		;(progn
			
		;)
		)
	
	
	)
	
	
 )