(defun C:zmianatxt ( / )
	(cd:SYS_UndoBegin)
	
	(defun *error* (errmsg)
   ; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		
		(cd:SYS_UndoEnd)
		nil
	;)
	); end *error*
	;(print "ok")
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq mspace (vla-get-modelspace doc))
	(setq pspace (vla-get-paperspace doc))
	(setq alllayouts (vla-get-layouts doc))

	(vlax-for obj pspace
		(if (or
				(equal "AcDbMText" (vla-get-ObjectName obj))
				(equal "AcDbText" (vla-get-ObjectName obj))
			)
		(progn
			(vla-put-stylename obj "Arial")	
		)
		)
	)
	(command "_regen" nil)
	(cd:SYS_UndoEnd)
)
	;(setq g 80)
	
	;; dla amelinium
	;(setq Rga (* R20 (/ (+ 234.5 g) 254.5)))
	;; dla miedz
	;(setq Rgm (* R20 (/ (+ 228 g) 248)))
	
