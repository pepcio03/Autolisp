(vl-load-com)

(defun C:oswiecmniepanie ( / 

	;---------- Local function  -----------------;
	LM:effectivename  
	cd:addtext
	DCL_WriteToList
	DCL_numerslupa
	DCL_przedrostek 
	DCL_zarostek 
	DCL_faza 
	DCL_opiskabla 
	DCL_rysowanie 
	
	;----------- local Variables -----------------;
	
	przedrostek faza i_oprawa el_oprawa pt_oprawa sum_oprawa el_kabel pt_kabel sum_kabel zestaw podzial temp_zestaw temp_spierwy spierwy sort_zestaw sort_zestaw pierwy 
	ostatni t tt len1 len2 len1all len2all el_srs el_dvr numer acadObj doc mspace pspace  )
	;(setq diagnoza :vlax-false)
	(setq diagnoza :vlax-true)
	(defun *error* (errmsg)
    ;(if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		(vla-put-activeLayer doc objlayer)
		(print errmsg)
		(print "nazwaopis")
		(print nazwaopis)
		(print "przedrostek")
		(print przedrostek)
		(print "numerslupa")
		(print numerslupa)
		(print "faza")
		(print faza)
		
		(cd:SYS_UndoEnd)
		nil
	;)
	); end *error*
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq mspace (vla-get-modelspace doc))
	(setq pspace (vla-get-paperspace doc))
	
	(defun LM:effectivename ( obj )
		(vlax-get-property obj
			(if (vlax-property-available-p obj 'effectivename)
				'effectivename
				'name
			)
		)
	)
	
	(defun cd:addtext (point text height style)
		(setq textObj (vla-AddMText mspace (vlax-3D-point point) 5 text))  
		(vla-put-height textObj height)
		(vla-put-stylename textObj style)
		textObj
	)
	(setq pi 3.1415)
	
	(setq i_oprawa 0)
	(setq el_oprawa (list))
	(setq pt_oprawa (list))
	(setq sum_oprawa (list))
	(setq el_kabel (list))
	(setq pt_kabel (list))
	(setq sum_kabel (list))
	(setq zestaw (list))
	(setq podzial (list))
	(setq temp_zestaw (list))
	(setq temp_spierwy (list))
	(setq spierwy (list))
	(setq sort_zestaw (list))
	(setq pierwy nil)
	(setq ostatni nil)
	(setq t nil)
	(setq tt nil)
	(setq opiskablaAsXS " ")
	(setq opiskablaYAKXs " ")
	(setq rysowanie "Poziome")
	(setq faza 1)
	(setq opiskabla 0)
	(setq zarostek "")
	(setq przedrostek "")
	(setq nazwaopis "")
	(setq len1 nil)
	(setq len2 nil)
	(setq len1all 0)
	(setq len2all 0)
	(setq el_srs 0)
	(setq el_dvr 0)
	(setq numer "1")
	(setq numerslupa 1)
	(setq layer (getvar "clayer"))
	(setq objlayer (vla-get-activeLayer doc))
	;(print (strcat "Layer set:" ( itoa (LM:LayerSet "0 schemat" 2 "CONTINUOUS" acLnWt030))))
	(print (LM:LayerSet "0 schemat" acRed "CONTINUOUS" acLnWt030))
	
	(vlax-for obj mspace
		(if (and
				(equal "AcDbPolyline" (vla-get-objectname obj)) 
				(or
					(equal "0 proj kabel" (vla-get-layer obj))
					(equal layer (vla-get-layer obj))
				)
				(/= "0 schemat" (vla-get-layer obj))
			)
		(progn
			(setq objKabel obj
				pointstart (vlax-curve-getstartpoint objKabel)
				pointend (vlax-curve-getendpoint objKabel)
				pointstart (list (car pointstart) (cadr pointstart))
				pointend (list (car pointend) (cadr pointend)))
				
			(setq sum_kabel (append sum_kabel (list (list objKabel pointstart pointend))))
		)
		)
		(if (and
				(equal "AcDbBlockReference" (vla-get-objectname obj))
				(or
					(equal "oprawa" (LM:effectivename obj))
					(equal "napow" (LM:effectivename obj))
					(equal "zapas" (LM:effectivename obj))
					(equal "istslup" (LM:effectivename obj))
					(equal "projslup" (LM:effectivename obj))
					(equal "istnapow" (LM:effectivename obj))
				)
				(/= "0 schemat" (vla-get-layer obj))
			)
			(progn
				(setq objOprawa obj)
				(setq point (vlax-safearray->list ( vlax-variant-value ( vla-get-InsertionPoint objOprawa) ) ))
				(setq point (list (car point) (cadr point)))
				(setq sum_oprawa (append sum_oprawa (list (list obj point))))
			)
		)
	)
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "SUM_Oprawa")
		(print sum_oprawa)
		(print "SUM_Kabel")
		(print sum_kabel)
	)
	)
	(foreach % sum_oprawa
		(foreach %% sum_kabel			
			(if (or 
					(<= (distance (nth 1 %) (nth 1 %%)) 0.42 )
					(<= (distance (nth 1 %) (nth 2 %%)) 0.42 )
				)
			(progn
				(setq zestaw (append zestaw (list (list (nth 0 %) (nth 0 %%)))))
			)
			)
		)
	)
	(sssetfirst nil nil )	
	(print "\n Zaznacz pierwszy element:")
	(while (not (setq pierwy (car (cd:SSX_Convert (ssget ":S"
					(setq sstest (list
							(cons -4 "<OR")
							;(cons -4 "<AND") (cons 2 "o") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "oprawa") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "napow") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "zapas") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "istslup") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "projslup") (cons -4 "AND>")
							(cons -4 "<AND") (cons 2 "istnapow") (cons -4 "AND>")
							(cons -4 "OR>")
					))
					) 1))))
		
		(alert (strcat "Pud³o !!! Zaznacz pierszy element"))
	)
	
	(print "\n Zaznacz ostatni element:")
	(while (not (setq ostatni (car (cd:SSX_Convert (ssget ":S"
				  sstest
					) 1))))
	(alert (strcat "Pud³o !!! Zaznacz ostatni element"))
	)
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "pierwy")
		(print pierwy)
		(print "ostatni")
		(print ostatni)
		(print "Zestaw")
		(foreach % zestaw (princ %) (princ "\n"))
	)
	)
	(foreach % zestaw
		(setq i 0)
		(foreach %% zestaw
			(if (and  ; Usuwa z dublowane dwie oprawy (jeden s³up dwie oprawy)
					(< (distance (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint (car %)))) (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint (car %%)))))  0.2)
					(/= (car %) (car %%)) 
				)
			(progn
				;(print "remove zestaw")
				(setq zestaw (vl-remove % zestaw))
			)
			)
			(if (equal (nth 0 %) (nth 0 %%))  
				(setq i (1+ i))
			)
		)
		(if (and 
				(equal pierwy (nth 0 %))
				(>= i 1)
			)
		(progn
			(setq spierwy (append spierwy (list %)))
			;(print "OK")
		)
		)
		
		(if (>= i 3)
			(setq podzial (append podzial (list %)))
		)
	)
	
	(foreach % podzial
		(setq i 0)
		(foreach %% podzial
			(if (and
					(<= (distance (vlax-safearray->list ( vlax-variant-value ( vla-get-InsertionPoint (car %)) ) ) (vlax-safearray->list ( vlax-variant-value ( vla-get-InsertionPoint (car %%)) ) )) 0.5 )
					(/= (nth 0 %) (nth 0 %%)) 
					;(equal (nth 1 %) (nth 1 %%)) 
				)
				(setq i (1+ i))
			)
		)
		(if (>= i 2)
		(progn
			(foreach %% podzial
				(setq podzial (vl-remove % podzial))
				(setq zestaw (vl-remove % zestaw))
			)
			(setq i 0)
		)
		)
	)
	
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "spierwy\n")
		(foreach % spierwy (princ %) (princ "\n"))
		(print "podzial\n")
		(foreach % podzial (princ %) (princ "\n"))
	)
	)
	
	(setq temp_zestaw zestaw)
	(setq temp_spierwy spierwy)
	(foreach % temp_spierwy (setq temp_zestaw (vl-remove % temp_zestaw)))
	(setq t (car temp_spierwy))
	(setq temp_spierwy (vl-remove t temp_spierwy))
	(setq sort_zestaw (append sort_zestaw (list t)))
	(setq j 0)
	(setq i 0)
	(setq trigger 1)
	(if (equal diagnoza :vlax-true)
	(progn
		(print "temp_zestaw\n")
		(foreach % temp_zestaw (princ %) (princ "\n"))
	)
	)
	(while (< (setq j (1+ j)) 400)
		(foreach % temp_zestaw
			(setq len1 (length sort_zestaw)) ; Sprawdzenie wielkosci listy
			(if (and						; Jeœli nie tedy droga zmien obiekt startowy 
					(= len1 0)
					(/= (length temp_spierwy) 0)
					(/= ostatni (nth 0 (last sort_zestaw)))
				)
			(progn 
				(setq i 0)
				(setq trigger 1) 
				(setq t (car temp_spierwy)) 
				(setq temp_spierwy (vl-remove t temp_spierwy))
				(setq sort_zestaw (append sort_zestaw (list t)))
				(if (equal diagnoza :vlax-true) 
				(progn
					(print "Change")
					(print t)
					(print "temp_zestaw\n")
					(foreach % temp_zestaw (princ %) (princ "\n"))
				))
			)
			)
			(setq t (last sort_zestaw))
			(if (and
					;(/= len1 0)
					(/= ostatni (nth 0 (last sort_zestaw))) 
					(equal (nth trigger t) (nth trigger %))  
					;(/= i -1)
				)
			(progn
				(setq tt %)
				(setq sort_zestaw (append sort_zestaw (list tt)))
				(setq temp_zestaw (vl-remove tt temp_zestaw))
				(if (equal diagnoza :vlax-true) 
				(progn
					(print "SORT_FOR_zestaw") 
					(print )
					(foreach % sort_zestaw (princ %) (princ "\n"))
					(print "temp_zestaw_FOR_zestaw")
					(print )
					(foreach % temp_zestaw (princ %) (princ "\n"))
				)
				)
				(setq trigger (LM:trigger trigger))
				(setq i 0)
				
			)
			(progn 
				(setq i (1+ i))
			)
			)
			(if (and
					(> i (* 2 (length temp_zestaw)))
					(/= ostatni (nth 0 t))
					;(/= ostatni (nth 0 t))
					(> len1 0)
				)
			(progn
				(setq i 0 )
				(if (equal diagnoza :vlax-true) 
				(progn
					(print "--------------SORT_FOR_zestaw_Remove------------------\n")
					(print j)
					(foreach % sort_zestaw (princ %) (princ "\n"))
				)
				)
				(setq sort_zestaw (vl-remove t sort_zestaw))
				(setq t (last sort_zestaw))
				(if (member t podzial)
				(progn				
					(setq podzial (vl-remove t podzial))
					
					(setq sort_zestaw (vl-remove t sort_zestaw))
					(setq t (last sort_zestaw))
					;(setq sort_zestaw (append sort_zestaw (list t)))
					
					(if (equal diagnoza :vlax-true) 
					(progn
						(print "Change podzial")
						(print t)
						(print "temp_zestaw\n")
						(foreach % temp_zestaw (princ %) (princ "\n"))
						(print "sort_zestaw\n")
						(foreach % sort_zestaw (princ %) (princ "\n"))
						
					))
				)
				)
				(setq t (last sort_zestaw))
				;(setq trigger (LM:trigger trigger))
				(setq trigger 0)
			)
			)
		)
		(if (equal diagnoza :vlax-true) 
		(progn
		;	(grread )
		;	(print "klik")
		)
		)
	)
	
	;;
	
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "----------------KONIEC-------------")
		;(foreach % temp_zestaw (princ %) (princ "\n"))
	))
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "temp!_zestaw")
		(foreach % temp_zestaw (princ %) (princ "\n"))
	))
	
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "podzial!")
		(foreach % podzial (princ %) (princ "\n"))
	))
	
	(if (equal diagnoza :vlax-true) 
	(progn
		(print "SORT!_zestaw")
		(foreach % sort_zestaw (princ %) (princ "\n"))
	))
	(initget 1)
	(setq ps (getpoint "\n Punkt rysowania: "))
	
	(cd:SYS_UndoBegin)
	;(initget "Poziome Pionowe")
	;(setq rysowanie (cd:USR_GetKeyWord "\nRysowanie:" '( "Poziome" "Pionowe") "Poziome"))
	;(initget 5)
	;(setq numerslupa (getint "\n Pocz¹tkowy numer s³upa: "))
	;(setq przedrostek (getstring "\n Przedrostek : "))
	;(setq zarostek (getstring "\n Zarostek : "))
	;(setq faza (atoi (cd:USR_GetKeyWord "\nFaza:" '("0" "1" "2" "3") "1")))
	
    (defun DCL_WriteToList (Wartosci Nazwa / War )
    (start_list Nazwa )
    (foreach War Wartosci (add_list War ))
	(end_list)
    )
	
	(defun DCL_numerslupa ( / )
	(if (not(> (atoi $value) 0)) 
		(progn
			(set_tile $key "")
		)
		(progn
			(set_tile $key $value)
			(setq numerslupa (atoi $value))
		)
	)
	)
	(defun DCL_przedrostek ( / )
		;(set_tile $key $value)
		(setq przedrostek $value)
	)
	(defun DCL_zarostek ( / )
		;(set_tile $key $value)
		(setq zarostek $value)
	
	)	
	(defun DCL_faza ( / )
		;(set_tile $key $value)
		(setq faza (atoi $value))
	)
	(defun DCL_opiskabla ( / )
		;(set_tile $key $value)
		(setq opiskabla (atoi $value))
	)
	(defun DCL_rysowanie ( / )
		;(set_tile $key $value)
		(cond
			((equal $value "0")
				(setq rysowanie "Poziome")
			) 
			((equal $value "1")
				(setq rysowanie "Pionowe")
			)
		)
	)
	
	(if
		(and
			(setq dcl (vl-filename-mktemp "tmp.dcl"))
			(setq des (open dcl "w"))
			(foreach str
			   '(
					"test : dialog"
					"{"
					"    label = \"Wybierz rodzaj kabla\";"
					"    spacer;"
					"    : column"
					"    {" 
					"	 	 : edit_box { key = \"numerslupa\"; label = \"Podaj numer s³upa: \";action = \"(DCL_numerslupa)\"; value = \"\"; edit_limit = 7; edit_width = 7; }"
					"	 	 : edit_box { key = \"przedrostek\"; label = \"Podaj przedrostek do numeru: \";action = \"(DCL_przedrostek)\"; value = \"\"; edit_limit = 7; edit_width = 7; }"
					"	 	 : edit_box { key = \"zarostek\"; label = \"Podaj zarostek do numeru: \";action = \"(DCL_zarostek)\"; value = \"\"; edit_limit = 7; edit_width = 7; }"
					"	 	 : popup_list { key = \"faza\"; edit_width = 4;action = \"(DCL_faza)\";alignment = \"left\"; value = \"1\"; label = \"Faza:\"; }"
					"	 	 : popup_list { key = \"rysowanie\"; edit_width = 10;action = \"(DCL_rysowanie)\";alignment = \"left\"; value = \"Poziome\"; label = \"Rysowanie:\"; }"
					"    : row"
					"    {"
					"	 	 : text { edit_width = 10;alignment = \"left\"; label = \"Opis rodzaju kabla:\"; }"
					"		 : toggle { key = \"opiskabla\"; edit_width = 10;action = \"(DCL_opiskabla)\";alignment = \"right\"; value = \"0\"; label = \"\"; }"
					"    }"
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
			;(setq rtn '(0))
			;(LM:dcld:action '("lb0") 'lst 'rtn)
			(DCL_WriteToList (list "0" "1" "2" "3") "faza" )
			(DCL_WriteToList (list "Poziome" "Pionowe") "rysowanie" )
			(if (= 1 (start_dialog))
				(progn
				;	(setq rodzaj (car (LM:dcld:getitems rtn lst)))
				)
				(progn
					(princ "\n*Cancel*")
				)
			)
		)
		)
	;;Kosmetyka
	;(if (assoc (car sort_zestaw) spierwy)
	;(progn				
		;(setq podzial (vl-remove t podzial))
		;(setq sort_zestaw (vl-remove t sort_zestaw))
		;(setq t (last sort_zestaw))
		;(setq sort_zestaw (append sort_zestaw (list t)))			
	;)
	;)
	(setq sort_zestaw (append sort_zestaw (list (last sort_zestaw))))
	(setq i 0)
	(setq j 0)
	(if (equal rysowanie "Poziome")
	(progn
		(setq sort_zestaw (cons (car sort_zestaw) sort_zestaw))
	)
	(progn 
		(setq ps0 (polar ps 0 (* i 11)))
		(setq ps1 (polar ps0 (/(* 7 pi ) 4) 3))
		(setq ps2 (polar ps1 (/(* 3 pi ) 2) (* (1+ i) 11)))
		(setq tpoly (cd:ACX_AddLWPolyline mspace (list ps0 ps1) nil))
		(setq tpoly1 (cd:ACX_AddLWPolyline mspace (list ps1 ps2) nil))
		(vla-put-Color tpoly acRed)
		(vla-put-Color tpoly1 acRed)
		(vla-put-Lineweight tpoly 30)
		(vla-put-Lineweight tpoly1 30)
	)
	)
	
	(while (< j (length sort_zestaw))
			(setq t (nth j sort_zestaw))
		(if (or
			(and
				(equal (nth 0 (nth j sort_zestaw)) (nth 0 (nth (1+ j) sort_zestaw)))
				(/= (nth 1 (nth j sort_zestaw)) (nth 1 (nth (1+ j) sort_zestaw)))
			)
			(and
				(equal j 0)
				(equal (nth 1 (nth j sort_zestaw))(nth 1 (nth (1+ j) sort_zestaw)))
				(equal (nth 1 (nth (1+ j) sort_zestaw))(nth 1 (nth (+ j 2) sort_zestaw)))
				(equal (nth 0 (nth j sort_zestaw))(nth 0 (nth (1+ j) sort_zestaw)))				
			)
			(and
				(>= j (- (length sort_zestaw) 2))
				(equal (nth 1 (nth j sort_zestaw)) (nth 1 (nth (1- j) sort_zestaw)))
				(equal (nth 1 (nth (1- j) sort_zestaw))(nth 1 (nth (+ j 1) sort_zestaw)))
				(equal (nth 0 (nth j sort_zestaw)) (nth 0 (nth (1+ j) sort_zestaw)))
				
			)
			)
		(progn	
			
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "oprawa") (setq nazwa "oprawa" nazwaopis "proj. s³up" acColor acRed))
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "napow") (setq nazwa "napow" nazwaopis (strcat "{\\C0;istn. s³up}" "\n" "proj. oprawa") acColor acRed))
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "zapas") (setq nazwa "zapas" nazwaopis "proj. zapas" acColor acRed))
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "istslup") (setq nazwa "istslup" nazwaopis "istn. s³up" acColor acByBlock))
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "projslup") (setq nazwa "projslup" nazwaopis "proj. s³up" acColor acRed))
			(if (equal (LM:effectivename (nth 0 (nth j sort_zestaw))) "istnapow") (setq nazwa "istnapow" nazwaopis (strcat "istn. s³up" "\n" "istn. oprawa") acColor acByBlock))
			
			(if (equal rysowanie "Poziome")
			(progn
				(if (equal nazwa "napow") (setq blockisnt (cd:BLK_InsertBlock (polar ps 0 (* i 11)) "istslup" '(1 1 1) pi nil)))
				;(if (equal blockisnt nil) (cd:BLK_InsertBlock (polar ps 0 (* i 11)) "Z:\AZART projektowanie\PROJEKTY\Szablony CAD\paleta\oswietlenie\istslup.dwg" '(1 1 1) pi T))
				;(if (equal blockisnt nil) (print "test"))
				
				(cd:BLK_InsertBlock (polar ps 0 (* i 11)) nazwa '(1 1 1) pi nil)
				(setq textObj (vla-AddMText mspace (vlax-3D-point (list (- (car (polar ps 0 (* i 11))) 2.5) (- (cadr (polar ps 0 (* i 11))) 3) 0.0)) 6 (strcat nazwaopis "\n" "nr " przedrostek (rtos numerslupa  2 0) zarostek "\n" "L" (if (= faza 0) "1" (itoa faza)))))
			)
			(progn
				(if (equal nazwa "napow") (cd:BLK_InsertBlock (polar ps1 (/ (* 3 pi ) 2) (* (1+ i) 11)) "istslup" '(1 1 1) (/ (* 3 pi ) 2) nil)) 
				(cd:BLK_InsertBlock (polar ps1 (/ (* 3 pi ) 2) (* (1+ i) 11)) nazwa '(1 1 1) (/ (* 3 pi ) 2) nil)
				(setq textObj (vla-AddMText mspace (vlax-3D-point (list (+ (car (polar ps1 (/(* 3 pi ) 2) (* (1+ i) 11))) 3) (+ (cadr (polar ps1 (/(* 3 pi ) 2) (* (1+ i) 11))) 1) 0.0)) 6 (strcat nazwaopis "\n" "nr " przedrostek (rtos numerslupa 2 0) zarostek "\n" "L" (if (= faza 0) "1" (itoa faza)))))
			)
			)
	
			(vla-put-height textObj 0.7)
			(vla-put-Color textObj acColor)
			(vla-put-AttachmentPoint textObj 2)
			(vla-put-stylename textObj "Arial")
			(cond
				((and (/= faza 0) (< faza 3)) (setq faza (1+ faza)))
				((>= faza 3) (setq faza 1))
				((= faza 0) (setq faza 0))
			)
			(setq i (1+ i))
			(setq numerslupa (+ numerslupa 1))
		)
		)
		
		(if (or
			(and
				(equal (nth 1 (nth j sort_zestaw)) (nth 1 (nth (1+ j) sort_zestaw)))
				(/= (nth 0 (nth j sort_zestaw)) (nth 0 (nth (1+ j) sort_zestaw)))
			)
			(and
				(equal j 0)
				(equal (nth 0 (nth j sort_zestaw))(nth 0 (nth (1+ j) sort_zestaw)))
				(equal (nth 0 (nth (1+ j) sort_zestaw))(nth 0 (nth (+ j 2) sort_zestaw)))
				(equal (nth 1 (nth j sort_zestaw))(nth 1 (nth (1+ j) sort_zestaw)))				
			)
			(and
				(>= j (- (length sort_zestaw) 2))
				(equal (nth 0 (nth j sort_zestaw)) (nth 0 (nth (1- j) sort_zestaw)))
				(equal (nth 0 (nth (1- j) sort_zestaw))(nth 0 (nth (+ j 1) sort_zestaw)))
				(equal (nth 1 (nth j sort_zestaw)) (nth 1 (nth (1+ j) sort_zestaw)))
			)
			)
		(progn	
			(setq len1 (vla-get-length (nth 1 (nth j sort_zestaw))))
			;(if (equal nazwa "oprawa")
			;	(setq len2 (+ (* len1 1.04) 4))
			;	(setq len2 (+ (* len1 1.03) 2))
			;)
			(if (equal "CONTINUOUS" (LM:get-linetype (nth 1 (nth j sort_zestaw))))
				(setq len2 (+ len1 2))
				(setq len2 (+ (* len1 1.04) 4))
			)
			(if (equal opiskabla 1)
			(progn
				(setq opiskablaAsXS "AsXSn 2x25mm\U+00B2 \n")
				(setq opiskablaYAKXs "YAKXs 5x16mm\U+00B2 \n")
			)
			(progn
				(setq opiskablaAsXS "\n")
				(setq opiskablaYAKXs "\n")
			)
			)
			(if (equal rysowanie "Poziome")
			(progn
				(setq lwpoly (cd:ACX_AddLWPolyline mspace (list (polar ps 0 (* (1- i) 11)) (polar ps 0 (* i 11))) nil))
				(if (equal "CONTINUOUS" (LM:get-linetype (nth 1 (nth j sort_zestaw))))
					(progn 
						(setq textObj (vla-AddMText mspace (vlax-3D-point (polar (polar ps 4.71 0.25) 0 (+ (* (1- i) 11) 4))) 8 (strcat opiskablaAsXS "L=" (rtos len1 2 0) "/" (rtos len2 2 0) "m" )))
						(vla-put-Linetype lwpoly "CONTINUOUS")
					)
					(progn
						(setq textObj (vla-AddMText mspace (vlax-3D-point (polar (polar ps 4.71 0.25) 0 (+ (* (1- i) 11) 4))) 8 (strcat opiskablaYAKXs "L=" (rtos len1 2 0) "/" (rtos len2 2 0) "m" )))
						(vla-put-Linetype lwpoly "DASHED")
					)
				)
			)
			(progn
				(setq lwpoly (cd:ACX_AddLWPolyline mspace (list (polar ps1 (/ (* 3 pi ) 2) (* i 11)) (polar ps1 (/(* 3 pi ) 2) (* (1+ i) 11))) nil))
				(if (equal "CONTINUOUS" (LM:get-linetype (nth 1 (nth j sort_zestaw))))
					(progn
						(setq textObj (vla-AddMText mspace (vlax-3D-point (polar ps1 (/ (* 3 pi ) 2) (+ (* i 11) 4))) 8 (strcat opiskablaAsXS "L=" (rtos len1 2 0) "/" (rtos len2 2 0) "m" )))
						(vla-put-Linetype lwpoly "CONTINUOUS")
					)
					(progn
						(setq textObj (vla-AddMText mspace (vlax-3D-point (polar ps1 (/ (* 3 pi ) 2) (+ (* i 11) 4))) 8 (strcat opiskablaYAKXs "L=" (rtos len1 2 0) "/" (rtos len2 2 0) "m" )))
						(vla-put-Linetype lwpoly "DASHED")
					)
				)
			)
			)
			(setq len1all (+ len1 len1all))
			(setq len2all (+ len2 len2all))
			
			
			(vla-put-Color lwpoly acRed)
			;(vla-put-Linetype lwpoly "DASHED")
			(vla-put-Lineweight lwpoly 30)
			(vla-put-height textObj 0.7)
			
			(if (equal rysowanie "Poziome")
				(progn
					(vla-put-AttachmentPoint textObj 5)
					(vla-put-InsertionPoint textObj (vlax-3D-point (polar (polar ps 5.5 0) 0 (+ (* (1- i) 11) 5.5))))
				)
				(progn
					(vla-put-AttachmentPoint textObj 4)
					(vla-put-InsertionPoint textObj (vlax-3D-point (polar ps1 (/ (* 3 pi ) 2) (+ (* i 11) 4))))
					(vla-put-AttachmentPoint textObj 5)
				)
			)
			
			(vla-put-Color textObj acRed)
			(vla-put-stylename textObj "Arial")
		)
		)
		(setq j (+ j 1))
	)
	(print (strcat "Trasa: " (rtos len1all 2 0) "/ £¹czna: " (rtos len2all 2 0)))
	(vla-put-activeLayer doc objlayer)
	(cd:SYS_UndoEnd)
)
