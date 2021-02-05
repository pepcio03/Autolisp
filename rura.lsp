(defun c:rura (/ rys kiel v p pt ptlist tmp myobj a e obj lobj eobj thexdata rodzajflag rodzaj)
	(setq kabele (list
			;Ensto AsXS
			'("AsXS4x25" 1.2 0.09)
			'("AsXS4x35" 0.868 0.087)
			'("AsXS4x50" 0.641 0.085)
			'("AsXS4x70" 0.443 0.083)
			'("AsXS4x95" 0.320 0.082)
			'("AsXS4x120" 0.253 0.08)
			
			;Eltrim AL !!!!!!!!!!!!! brak reaktancji 
			'("AL4x16" 1.7772 0.3)
			'("AL4x25" 1.1459 0.3)
			'("AL4x35" 0.8186 0.3)
			'("AL4x50" 0.5776 0.3)
			'("AL4x70" 0.4090 0.3)
			'("AL4x95" 0.3033 0.3)
			'("AL4x120" 0.2457 0.3)
			'("AL4x150" 0.1943 0.3)
			'("AL4x185" 0.1563 0.3)
			'("AL4x240" 0.1196 0.3)
			'("AL4x300" 0.0966 0.3)
			'("AL4x887" 0.0327 0.3)
			
			;Nkt Cables YAKXs !!!!!!!!!!!!!!!!!!!!!! sprawdzic reaktancje
			'("YAKXs4x16" "50" "75")
			'("YAKXs5x16" "50" "75")
			'("YAKXs4x25" "50" "75")
			'("YAKXs5x25" "50" "75")
			'("YAKXs4x35" "50" "75")
			'("YAKXs4x50" "50" "75")
			'("YAKXs4x70" "75" "75")
			'("YAKXs4x95" "75" "110")
			'("YAKXs4x120" "75" "110")
			'("YAKXs4x150" "110" "160")
			'("YAKXs4x240" "110" "160")
			'("3x1xXRUHAKXs 50/25" "160" "160")			
			'("3x1xXRUHAKXs 70/25" "160" "160")
			'("3x1xXRUHAKXs 120/50" "160" "160")
	))
	(setq lst
       '(
			(
				"YAKXs4x16" 
				"YAKXs5x16" 
				"YAKXs4x25" 
				"YAKXs5x25" 
				"YAKXs4x35"
				"YAKXs4x50" 
				"YAKXs4x70" 
				"YAKXs4x95" 
				"YAKXs4x120" 
				"YAKXs4x150" 
				"YAKXs4x240"
				"3x1xXRUHAKXs 50/25"				
				"3x1xXRUHAKXs 70/25"
				"3x1xXRUHAKXs 120/50"
			)
        )
    )
	
	(defun *error* (errmsg)
		(if (= 'file (type des))
            (close des)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
           (progn
			(princ (strcat "\nError: " msg))
			(cd:SYS_UndoEnd)
        ))
     
		
		(cd:SYS_UndoEnd)
		(princ)
		nil

	); end *error*
	
	
	
	;(if (not (null *Label:Reactor* )) 
	;(progn
		;(LM:label:reactor nil )
	;)
	;)
	
	
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq mspace (vla-get-modelspace doc))
	(setq pspace (vla-get-paperspace doc))
	
	(defun cd:kieli(p1 p2 k / p)
		(setq ang1 (- (angle p1 p2) (* pi (/ k 180.0)))
		  y1 (* (/ v 100.0) (sin ang1))
		  x1 (* (/ v 100.0) (cos ang1))
		  rx1 (rtos x1 2 10 )
	      ry1 (rtos y1 2 10 )
		  p (list (- (car p1) (atof rx1)) (- (cadr p1) (atof ry1)))
		)
		p
	)
	(cd:SYS_UndoBegin)
	(initget "0 1 2 3")
	(setq tkiel
	  (cond ((getkword (strcat "\nWprowadz strone po³o¿enia kielicha [Bez(0)/Pocz¹tek(1)/Obie(2)/Koniec(3)]: <"
			  (setq tkiel (cond ( tkiel ) ( "2" )))
			  ">: ")))( tkiel )))
	(setq kiel (atof tkiel))
	(initget "30 50 100 200 300")
	(setq vv
	  (cond
		(
		  (getkword
			(strcat "\nWprowadz wielkoœæ rury np. SRS(30)/DVR(50): <"
			  (setq vv (cond ( vv ) ( "50" ))) ">: ")
		  )
		)
		( vv )
	  )
	)
	(setq v (atof vv))
	
	(initget "Automatyczne Manualne")
	(setq trys
	  (cond ((getkword (strcat "\nRysowanie [Automatyczne/Manualne]: <"
			  (setq trys (cond ( trys ) ( "Automatyczne" )))
			  ">: ")))( trys )))
	
	(setq rys trys)
	(setq pt (getpoint "\nWprowadz punkt startowy: "))
	
	(setq i 1)
	(setq ptlist (cons (list (car pt)(cadr pt)) ptlist))

	(while 
			(and 
				(if (equal rys "Automatyczne") 
					(< i 2) 
					(>= i 1)
				)
				(if (equal rys "Automatyczne") 
					(setq pt (getpoint "\nWprowadz punkt koñcowy lub [Enter]: " pt)) 
					(setq pt (getpoint "\nWprowadz kolejny punkt lub zakoñcz [Enter]: " pt))
				)
				
			)
		(setq ptlist (cons (list (car pt)(cadr pt)) ptlist))
		(setq i (1+ i))
	)
	(setq lobj (LM:rura ptlist kiel v rys))
	
	(cond 
		( (= kiel 0) (print (+ (abs (- disp1 disp2)))) )
		( (= kiel 1) (print (+ (* 1 (/ v 100.0)) (abs (- disp1 disp2)))) )
		( (= kiel 2) (print (+ (* 2 (/ v 100.0)) (abs (- disp1 disp2)))) )
		( (= kiel 3) (print (+ (* 1 (/ v 100.0)) (abs (- disp1 disp2)))) )
		(t nil)
	)
	
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (car lobj)) "rura" (list (vla-get-Handle (cadr lobj))))
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (car lobj)) "kabel" (list (vla-get-Handle (caddr lobj))))
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (cadr lobj)) "rura" (list (vla-get-Handle (car lobj))))
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (cadr lobj)) "kabel" (list (vla-get-Handle (caddr lobj))))
	(setq eobj (vlax-vla-object->ename (caddr lobj)))
	;(print eobj)
	;(print (vla-get-Handle (car lobj)))
	;(print (setq thexdata (cd:XDT_GetXData eobj "rura")))
	(cd:XDT_UpdateXData eobj "rura" (list (vla-get-Handle (car lobj)) (vla-get-Handle (cadr lobj))))
	(print "Okej Xdata" )
	(setq thexdata (cd:XDT_GetXData eobj "Rodzaj"))
	(setq rodzaj "YAKXs4x35")
	(if (or 
			(equal thexdata nil)
			(equal (cdr (nth 1 thexdata)) "")
		)
	(progn
		(setq rodzajflag 0)
		;(setq rodzaj "35")
		;(while (= rodzajflag 0)
		(if
		(and
			(setq dcl (vl-filename-mktemp "tmp.dcl"))
			(setq des (open dcl "w"))
			(foreach str
			   '(
					"lbx : list_box"
					"{"
					"    alignment = centered;"
					"    fixed_width = true;"
					"    fixed_height = true;"
					"    width = 20;"
					"    height = 15;"
					"}"
					"test : dialog"
					"{"
					"    label = \"Wybierz rodzaj kabla\";"
					"    spacer;"
					"    : column"
					"    {" 
					"        : lbx { key = \"lb0\"; label = \"Rodzaj kabla\"; }"
					"    }"
					"    spacer;"
					"    ok_cancel;"
					"}"
				)
				(write-line str des)
			)
			(not (setq des (close des)))
			(< 0 (setq dch (load_dialog dcl)))
			(new_dialog "test" dch)
		)
		(progn           
			(setq rtn '(0))
			(LM:dcld:action '("lb0") 'lst 'rtn)
			(if (= 1 (start_dialog))
				(progn
					(setq rodzaj (car (LM:dcld:getitems rtn lst)))
				)
				(progn
					(princ "\n*Cancel*")
				)
			)
		)
		)
		;(print rodzaj)
		;(*error* nil)
		;(princ)
		;)
		(cd:XDT_PutXData eobj "Rodzaj" (list (cons 1000 rodzaj)))
	)
	(progn
		(setq rodzaj (cdr (cadr thexdata)))
	)
	)
	
	;(if (not (null *Label:Reactor* )) 
	;(progn
	;	(LM:label:reactor t )
	;)
	;)
	
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (car lobj)) "rodzajrury" (list (if (equal v 30) (cadr (assoc rodzaj kabele)) (caddr (assoc rodzaj kabele)))))
	;(cd:XDT_UpdateXData (vlax-vla-object->ename (cadr lobj)) "rodzajrury" (list (if (equal v 30) (cadr (assoc rodzaj kabele)) (caddr (assoc rodzaj kabele)))))
	
	
	(print "Okej Xdata rodzaj" )
	(cd:SYS_UndoEnd)
	nil	
)
