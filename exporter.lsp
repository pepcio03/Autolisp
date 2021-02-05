(print "Wczytuje plik EXPO")
(defun C:expodxf2000 (/ istn ssElem layer SciezkaPliku)
	(cd:SYS_UndoBegin)
	(setq layer (getvar "clayer"))
	(setq SciezkaPliku (strcat (getvar "Dwgprefix") (setq filenamebase (vl-filename-base (getvar "dwgname"))) "_trasa.dxf"))
	(if (findfile SciezkaPliku )
		(setq istn "TAK")
		(setq istn "")
	)
	(command "_.dxfout"
		SciezkaPliku
		;(getfiled "Save R12 Dxf As" (getvar "Dwgprefix") "dxf" 1)
		"_Objects"  (setq ssElem 
						(ssget
							(list
									(cons -4 "<OR")
										(cons -4 "<AND") 
											(cons 0 "LWPOLYLINE") 
											(cons -4 "<OR")
												(cons 8 layer) 
												(cons 8 "0 proj. SN") 
												(cons 8 "0 proj. nN") 
											(cons -4 "OR>")
											(cons -4 "<NOT") 
												(cons 62 5) 
											(cons -4 "NOT>") 
										(cons -4 "AND>")
											(cons -4 "<AND") 
												(cons 0 "LINE") 
											(cons -4 "<OR")
												(cons 8 layer) 
												(cons 8 "0 proj. SN") 
												(cons 8 "0 proj. nN") 
											(cons -4 "OR>")
											(cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
										(cons -4 "<AND")
											(cons 0 "CIRCLE") 
											(cons -4 "<OR")
												(cons 8 layer) 
												(cons 8 "0 proj. SN") 
												(cons 8 "0 proj. nN") 
											(cons -4 "OR>")
										(cons -4 "AND>")
										;(cons -4 "<AND") (cons 8 layer) (cons -4 "AND>")
										(cons -4 "<AND") 
											(cons 2 "oprawa") 
										(cons -4 "AND>")
										(cons -4 "<AND") 
											(cons 2 "zapas") 
										(cons -4 "AND>")
									(cons -4 "OR>")
							)
						)
					)
		"" ; completes selection
		"_Version" "2000"
		"16"
		istn ; non-default number or other option if desired, with followup line(s) if necessary
	)
	(princ "2000 DXF Created")
	(cd:SYS_UndoEnd)
	(princ)
)

(defun C:expodxfMAPY_przylacza (/ nazwa istn ssElem layer SciezkaPliku)
	(setq layer (getvar "clayer"))
	;(setq SciezkaPliku (strcat (getvar "Dwgprefix") (setq filenamebase (vl-filename-base (getvar "dwgname"))) "_trasa.dxf"))
	(setq nazwa (getstring T "Nazwa: "))
	(if (wcmatch nazwa "*/*")
		(setq nazwa (vl-string-subst "na" "/" nazwa))
		; (setq v $value)		
	)
	(setq SciezkaPliku (strcat "Z:\\AZART projektowanie\\PROJEKTY\\TEMP\\mapy_zakresy\\" nazwa ".dxf"))
	(if (findfile SciezkaPliku )
		(setq istn "TAK")
		(setq istn "")
	)
	(command "_.dxfout"
		SciezkaPliku
		;(getfiled "Save R12 Dxf As" (getvar "Dwgprefix") "dxf" 1)
		"_Objects"  (setq ssElem 
						(ssget
							(list
								(cons -4 "<OR")
								(cons -4 "<AND") (cons 0 "LWPOLYLINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
								(cons -4 "<AND") (cons 0 "LINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
								(cons -4 "<AND") (cons 0 "CIRCLE") (cons 8 layer) (cons -4 "AND>")
								;(cons -4 "<AND") (cons 8 layer) (cons -4 "AND>")
								(cons -4 "OR>")
							)
						)
					)
		"" ; completes selection
		"_Version" "2000"
		"16"
		istn ; non-default number or other option if desired, with followup line(s) if necessary
	)
	(princ "2000 DXF Created")
	(princ)
)

;(print "EXPOOOOOOOOOOOOOOOOOOOOOOOO")
(defun C:expoklajn_przylacza ( / acadObj doc mspace pspace e_model istn obreb dzialka ssElem layer SciezkaPliku)
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	;(setq util (vla-get-Utility doc))
	;(setq layers (vla-get-Layers doc))
	(setq mspace (vla-get-ModelSpace doc))
	(setq pspace (vla-get-PaperSpace doc))
	(print "ok")
	(vlax-for obj mspace
		(if ;(and
				;(equal "AcDbPolyline" (vla-get-objectname obj))
				;(equal "0 proj przy" (vla-get-layer obj))	
				(equal "dane" (LM:effectivename obj))	
				;(equal 256 (vla-get-Color obj))	
				
			;)
			(progn
				(setq e_model obj)
				;(setq list_att (cd:BLK_GetAttsVLA e_model))
			)
		)
	)
	(print "ok")
	(setq obreb (cd:BLK_GetAttValueVLA e_model "OBREB"))
	(setq dzialka (cd:BLK_GetAttValueVLA e_model "DZIALKA"))
	(while (wcmatch dzialka "*/*")
		(setq dzialka (vl-string-subst "na" "/" dzialka))
	)
	;(print dzialka)
	;(setq layer (getvar "clayer"))
	(setq layer (getvar "clayer"))
	;(setq SciezkaPliku (strcat (getvar "Dwgprefix") (setq filenamebase (vl-filename-base (getvar "dwgname"))) "_trasa.dxf"))
	(setq SciezkaPliku (strcat (getvar "Dwgprefix") obreb "_" dzialka ".dxf"))
	(if (findfile SciezkaPliku )
		(setq istn "TAK")
		(setq istn "")
	)
	;(wcmatch (vla-get-Layer obj) "211-Nr dzia@ki" )  
	(command "_.dxfout"
		SciezkaPliku
		;(getfiled "Save R12 Dxf As" (getvar "Dwgprefix") "dxf" 1)
		"_Objects"  (setq ssElem 
						(ssget
							(list
									(cons -4 "<OR")
									(cons -4 "<AND") (cons 0 "LWPOLYLINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
									(cons -4 "<AND") (cons 0 "LINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
									(cons -4 "<AND") (cons 0 "CIRCLE") (cons 8 layer) (cons -4 "AND>")
									;(cons -4 "<AND") (cons 8 layer) (cons -4 "AND>")
									(cons -4 "<AND") (cons 2 "oprawa") (cons -4 "AND>")
									(cons -4 "<AND") (cons 2 "zapas") (cons -4 "AND>")
									(cons -4 "OR>")
							)
						)
					)
		"" ; completes selection
		"_Version" "2000"
		"16"
		istn ; non-default number or other option if desired, with followup line(s) if necessary
	)
	(princ "2000 DXF Created")
	(princ)
)

(defun c:expozamianaoswietlenie (/ inst thepoint layer test p c w h p1 p2 p3 p4 ss vp layers pspace mspace util doc acadObj objKabel textObj) 

	(defun *error* ( msg )
		(print msg)
		;(if (wcmatch msg "ObjectIdToObject")
		(princ "\n*error* program test .")
		;)
	)

	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))
	(setq util (vla-get-Utility doc))
	(setq layers (vla-get-Layers doc))
	(setq mspace (vla-get-ModelSpace doc))
	(setq pspace (vla-get-PaperSpace doc))
	;(setq radius 0.25)

	(vlax-for obj mspace 
		(if (or
			
			(equal (LM:effectivename obj) "oprawa")
			(equal (LM:effectivename obj) "zapas")
			)
		(progn
			(setq inst (vla-get-insertionpoint obj))
			(setq thepoint (vla-AddCircle mspace inst 0.25))
			(vla-delete obj)
		)
		)
	)


	(princ)
	
)

; (defun C:expo1 ( / acadObj modelSpace layer SciezkaPliku ssets ssElem doc docc docco)
	; (vl-load-com)

	; (setq acadObj (vlax-get-acad-object))
    ; (setq doc (vla-get-ActiveDocument acadObj))
    ; (setq modelSpace (vla-get-ModelSpace doc))
	
	
	; (defun *error* (errmsg)
   ; ; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		
		; (setvar "CMDECHO" 1)
		; nil
	; ;)
	; ); end *error*
	; (setq layer (getvar "clayer"))
	; (setq SciezkaPliku (strcat (getvar "Dwgprefix") (setq filenamebase (vl-filename-base (getvar "dwgname")))))
	; (setq ssElem 
			; (ssget
			    ; (list
						; (cons -4 "<OR")
						; (cons -4 "<AND") (cons 0 "LWPOLYLINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 0 "LINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 0 "CIRCLE") (cons 8 layer) (cons -4 "AND>")
						; ;(cons -4 "<AND") (cons 8 layer) (cons -4 "AND>")
						; (cons -4 "<AND") (cons 2 "oprawa") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 2 "zapas") (cons -4 "AND>")
						; (cons -4 "OR>")
				; )
			; )
	; )
	; (print ssElem)
	; (if (findfile (setq SciezkaPlikuDxf (strcat SciezkaPliku "_trasa.dxf")))
		; (setq istn "TAK")
		; (setq istn "")
	; )
	
	; (vl-cmdf "_copyclip" ssElem "")
	
	; ;(vl-cmdf "_qnew" SciezkaPlikuDxf "")
	
	
	; ;(if (findfile (setq SciezkaPlikuDxf (strcat SciezkaPliku "_trasa.dxf")))
	; ;	(vl-cmdf "_.-wblock" SciezkaPlikuDxf "TAK" "" "_non" '(0.0 0.0) ssElem "")
	; ;	(vl-cmdf "_.-wblock" SciezkaPlikuDxf "" "_non" '(0.0 0.0) ssElem "")
	; ;)
	
	
	; ;(vl-cmdf "_oops" "")
	
	; ;(setq docc (vla-get-documents (vlax-get-acad-object)))
	; ;(setq docco (vla-open docc SciezkaPlikuDxf ))
	; ;(setq modelSpace2 (vla-get-ModelSpace docco))
	
	; ;(vlax-for obj modelSpace2 
; ;		(if (or
; ;			
; ;			(equal (LM:effectivename obj) "oprawa")
; ;			(equal (LM:effectivename obj) "zapas")
; ;			)
; ;		(progn
; ;			(setq inst (vla-get-insertionpoint obj))
; ;			(setq thepoint (vla-AddCircle modelSpace2 inst 0.25))
; ;			;(vla-delete obj)
; ;		)
; ;		)
; ;	)
	
	; (setvar "CMDECHO" 1)
; )

; (defun C:expooswietlenie ( / acadObj modelSpace layer SciezkaPliku ssets ssElem doc docc docco)
	; (vl-load-com)

	; (setq acadObj (vlax-get-acad-object))
    ; (setq doc (vla-get-ActiveDocument acadObj))
    ; (setq modelSpace (vla-get-ModelSpace doc))
	
	; (defun *error* (errmsg)
   ; ; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		
		; (setvar "CMDECHO" 1)
		; nil
	; ; )
	; ); end *error*
	; (setq layer (getvar "clayer"))
	; (setq SciezkaPliku (strcat (getvar "Dwgprefix") (setq filenamebase (vl-filename-base (getvar "dwgname")))))
	; (setq ssElem 
			; (ssget
			    ; (list
						; (cons -4 "<OR")
						; (cons -4 "<AND") (cons 0 "LWPOLYLINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 0 "LINE") (cons 8 layer) (cons -4 "<NOT") (cons 62 5) (cons -4 "NOT>") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 0 "CIRCLE") (cons 8 layer) (cons -4 "AND>")
						; ; (cons -4 "<AND") (cons 8 layer) (cons -4 "AND>")
						; (cons -4 "<AND") (cons 2 "oprawa") (cons -4 "AND>")
						; (cons -4 "<AND") (cons 2 "zapas") (cons -4 "AND>")
						; (cons -4 "OR>")
				; )
			; )
	; )
	; (print ssElem)
	; ; (setq ssets (vla-get-activeselectionset doc))
	; ; (setq I 0)
    ; ; (if (= (vla-get-Count ssets) 0)
       ; ; (print "The selection set is empty")
       ; ; (while (>= (1- (vla-get-Count ssets)) I)
           ; ; (print  (strcat "The selection set contains: " (vla-get-ObjectName (vla-Item ssets I))))

           ; ; (setq I (1+ I))
       ; ; )
    ; ; )
	; ; (print "OK")
    ; ; (print (getenv  "DefaultFormatForSav" ))
	; ; (setenv  "DefaultFormatForSav" "25")
	; ; (print (getenv  "DefaultFormatForSavexpoe" ))
	
	; ; (print SciezkaPliku)
	; ; (setvar "CMDECHO" 0)
	
	; (if (findfile (setq SciezkaPlikuDxf (strcat SciezkaPliku "_trasa.dxf")))
		; (vl-cmdf "_.-wblock" SciezkaPlikuDxf "TAK" "" "_non" '(0.0 0.0) ssElem "")
		; (vl-cmdf "_.-wblock" SciezkaPlikuDxf "" "_non" '(0.0 0.0) ssElem "")
	; )
	
	; ; (print SciezkaPlikuDxf)
	; ; (print filenamebase)
	; (vl-cmdf "_oops" "")
	; ; (vl-cmdf "_open" SciezkaPlikuDxf)
	
	; (setq docc (vla-get-documents (vlax-get-acad-object)))
	; (setq docco (vla-open docc SciezkaPlikuDxf ))
	; (setq modelSpace2 (vla-get-ModelSpace docco))
	
	; (vlax-for obj modelSpace2 
		; (if (or
			
			; (equal (LM:effectivename obj) "oprawa")
			; (equal (LM:effectivename obj) "zapas")
			; )
		; (progn
			; (setq inst (vla-get-insertionpoint obj))
			; (setq thepoint (vla-AddCircle modelSpace2 inst 0.25))
			; ; (vla-delete obj)
		; )
		; )
	; )
	; ; (vla-put-SaveAsType
		; ; (vla-get-OpenSave
			; ; (vla-get-Preferences (vlax-get-acad-object))
		; ; )
		; ; 25
	; ; )
  
	; ; (print (cd:DWG_GetOpenDocs))
	; ; (setq filedoc (cdr (assoc (strcat filenamebase "_trasa.dxf") (cd:DWG_GetOpenDocs))))
	
	; ; (setq activeDoc (vla-put-ActiveDocument filedoc))
	
	; ; (setq modelSpace (vla-get-ModelSpace filedoc))
	; ; (vla-put-ActiveSpace filedoc modelSpace)
	
	; ; (setq ssElem1 (list))
	; ; (setq ssElemList (cd:SSX_Convert ssElem 1) )
	; ; (print "OK1")
	; ; (foreach obj ssElemList
		; ; (print obj)
		; ; (setq nameObject (vla-get-ObjectName obj))
		; ; (if (or
				; ; (equal "AcDbPolyline" nameObject)
				; ; (equal "AcDbCircle" nameObject)
			; ; )
		; ; (progn
			; ; (if (equal "AcDbBlockReference" (vla-get-ObjectName obj))
			; ; (progn
				; ; (setq obj1 (vla-explode obj))
				; ; (setq ssElem1 (cons obj1 ssElemList))
			; ; )
			; ; (progn
				; ; (setq ssElem1 (cons obj1 ssElemList))
			; ; )
			; ; )
		; ; )
		; ; )
	; ; )
	; ; (print "OK2")
	; ; (print ssElem1)
	

	; ; (vla-activate docco)
	; ; (setq acadObj (vlax-get-acad-object))
    ; ; (setq docco (vla-get-ActiveDocument acadObj))
	; ; (vlax-invoke-method docco 'SaveAs  (strcat SciezkaPliku ".dxf") ac2000_dxf) 
	; ; (vla-close docco)
	; ; (vla-saveas docco ac2000_dxf (strcat SciezkaPliku ".dxf"))
	; ; (vla-export doc SciezkaPliku "dxf" ssets)
	; ; (vla-Wblock doc SciezkaPliku ssets)
	; ; (vla-delete ssets)
	; ; (setenv  "DefaultFormatForSave" "48")
	; (setvar "CMDECHO" 1)
; )

(print "Wczytano plik EXPO")
