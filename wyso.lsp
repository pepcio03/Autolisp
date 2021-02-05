(defun C:wyso ( / )
	(setq mspace (vla-get-modelspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	(setq pspace (vla-get-paperspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	
	
	(cd:SYS_UndoBegin)
		
	(vlax-for obj mspace
		(if (and
				;(equal "AcDbPolyline" (vla-get-objectname obj))
				;(equal "0 proj przy" (vla-get-layer obj))	
				(equal "daneo" (LM:effectivename obj))	
				;(equal 256 (vla-get-Color obj))	
				
			)
			(progn
				(setq e_model obj)
				(setq list_att (cd:BLK_GetAttsVLA e_model))
			)
		)
		
		(if (and
				(equal "AcDbPolyline" (vla-get-objectname obj))
				(or
					;(equal "0" (vla-get-layer obj))	
					;(equal "0 proj przy" (vla-get-layer obj))	
					(equal "0 proj rura" (vla-get-layer obj))	
				)
				;(equal "dane" (LM:effectivename obj))	
				(equal 5 (vla-get-Color obj))
			)
			(progn
				(setq i_rura (+ i_rura 1))
				(setq el_rura (append el_rura (list obj)))
				;(princ el_rura )
			)
		)
		(if (and
				(equal "AcDbPolyline" (vla-get-objectname obj))
				(equal "0 proj przy" (vla-get-layer obj))	
				(equal "0 proj kabel" (vla-get-layer obj))	
				;(equal "dane" (LM:effectivename obj))	
				(or
					(equal 256 (vla-get-Color obj))	
					(equal 1 (vla-get-Color obj))	
				)
			)
			(progn
				;(setq i_rura (+ i_rura 1))
				;(setq el_kabel (append el_kabel (list obj)))
				(setq el_kabel obj)
				;(princ el_rura )
			)
		)
	)
	
	
	
	(vlax-for obj pspace
		(if (and 
			;(/= "Model" (cdr (assoc 410 ed)))
			(equal "daneo" (LM:effectivename obj))
			)
			(progn
				(foreach % list_att
					(setq h (car %)
						  v (cdr %)
					)
					(cd:BLK_SetAttValueVLA obj h v)
				)
			)
		)
		
	)
	
	(print " ")
	(CD:SYS_UndoEnd)
	(command "_regen" nil)
	;(setvar "CMDECHO" 1)
	nil
)