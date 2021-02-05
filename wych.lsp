
(defun C:wych ( / )
	(LM:wych)
	(princ)
)
(defun LM:wych ( / alert_text e_rodzajkabla e_textproj  e_rurySRS e_ruryDVR e_list_SKP el_liniarozgr e_list_dz i_rura el_rura el_kabel el_srs el_dvr e_zestawienie e_umowa li j e_model e_obliczenia e_uprawnienia doc mspace pspace )
	
	; (defun *error* ( msg )
        ; (print msg)
		; ; (if (wcmatch msg "ObjectIdToObject")
		; (princ "\n*error* program wych .")
		; (print i_rura)
		; (print el_rura)
		; (print el_kabel)
		; (print el_srs)
		; (print el_dvr)
		; (print e_zestawienie)
		; (print e_umowa )
		; (print (nth j el_rura))
        ; )
    ; )
	(cd:SYS_UndoBegin)
	(setvar "CMDECHO" 0)
	(setvar "PSLTSCALE" 0)
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq mspace (vla-get-modelspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	(setq pspace (vla-get-paperspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	
	(setq listaarkuszy (list))
	
	(setq listaarkuszy (mapcar 'car (cd:DWG_LayoutsList)))
	(setq nazwaarkusza (vla-get-name (vla-get-activelayout (vla-get-ActiveDocument (vlax-get-acad-object)))))
	;(print nazwaarkusza)
	(setq nazwyprojektu
	(list 
		"1¯N_ZL-2_OU" "1¯N_ZL-1_WU" "1¯N_ZL-1_OU" "1¯N_SKP_WU" "1¯N_SKP_OU" "1¯N_RSA_ZL-1_WU" "1¯N_RSA_ZL-1_OU" "1¯N_RSA_P_ZL-1_OU" "1¯N_P_ZL-2_OU" "1¯N_P_ZL-1_OU" "1¯N_P_SKP_OU" "1ZL_ZL_WU" "1ZL_ZL_OU" "1ZL_P_ZL_OU" "1ZAPAS_SKP" "1ST_RSA_SK+P_OU" "1SK_ZL-1_WU" "1SK_ZL-1_OU" "1SK_SKP_WU" "1SK_SKP_OU" "1SK_P_ZL-1_OU" "1SK_P_SKP_OU" "1MUF_SKP" "1MUF_SK_ZL-1 WU" "1E_ZL-2_OU" "1E_ZL-1_WU" "1E_ZL-1_OU" "1E_SKP_WU" "1E_SKP_OU" "1E_P_ZL-2_OU" "1E_P_ZL-1_OU" "1E_P_SKP_OU" 
	)
	)
	(defun vl-positions ( x l / i )
		(setq i -1)
		(vl-remove nil (mapcar '(lambda ( y ) (setq i (1+ i)) (if (= x y) i)) l))
	)
	
	(defun cd:rur( el_rura1 el_rura2 / ret)
		(setq dis (distance (vlax-curve-getstartpoint el_rura1)  (vlax-curve-getstartpoint el_rura2)))
		(setq ret "err")
		(setq dis (fix (* 100 dis)))
		;(princ dis)
		(if (or (= dis 120) (= dis 50))
		(progn
			(setq ret "dvr")
		)
		) 
		(if (or (= dis 72) (= dis 30))
		(progn
			(setq ret "srs")
		)
		)
		ret
	)
	
	
	(setq i_rura 0)
	(setq i_rura_srs 0)
	(setq el_rura (list))
	(setq el_slup (list))
	(setq el_kabel nil)
	(setq el_podlacz nil)
	(setq el_liniarozgr nil)
	(setq el_srs 0)
	(setq el_dvr 0)
	(setq e_zestawienie nil)
	(setq e_obliczenia nil)
	(setq e_uprawnienia nil)
	(setq e_umowa nil)
	(setq e_pelno nil)
	(setq e_SKP nil)
	(setq e_list_dz (list))
	(setq e_rodzajkabla nil)
	(setq e_rurySRS nil)
	(setq e_ruryDVR nil)
	(setq e_textproj nil)
	(setq e_list_SKP (list))
	(setq alert_text " ")
	
	(setq SciezkaPliku (getvar "Dwgprefix"))
	
	(setq listklocki (list 
		(list "KLOCKI 10" "ZRB/0419/2017")
		(list "KLOCKI 11" "ZRB/0243/2018")
		(list "KLOCKI 2019" "ZRB/0115/2019")
		(list "KLOCKI 2020" "ZRB/1040/2019")
		(list "KLOCKI 2020 II" "ZRB/0114/2020")
		(list "KLOCKI 2020 III" "ZRB/0290/2020")
		(list "KLOCKI 2020 IV" "ZRB/0545/2020")
		(list "KLOCKI 2020 V" "ZRB/0715/2020")

	)
	)
	(setq listpelno (list 
		;(list "KLOCKI 10" "ZRB/0419/2017")
		;(list "KLOCKI 11" "ZRB/0243/2018")
		;(list "KLOCKI 2019" "ZRB/0115/2019")
		;(list "KLOCKI 2020" "ZRB/1040/2019")
		(list "KLOCKI 2020 II" "13.03.2020")
		(list "KLOCKI 2020 III" "14.05.2020")
		(list "KLOCKI 2020 IV" "20.08.2020")
		(list "KLOCKI 2020 V" "30.11.2020")
	)
	)
	
	(vlax-for obj mspace
		(if (and
				;(equal "AcDbPolyline" (vla-get-objectname obj))
				;(equal "0 proj przy" (vla-get-layer obj))	
				(equal "dane" (LM:effectivename obj))	
				;(equal 256 (vla-get-Color obj))	
				
			)
			(progn
				(setq e_model obj)
				(setq list_att (cd:BLK_GetAttsVLA e_model))
			)
		)
		(if (and
				;(equal "Model" (cdr (assoc 410 ed)))
				(equal "AcDbPolyline" (vla-get-objectname obj))
				(or 
					(equal "0" (vla-get-layer obj))	
					(equal "0 proj przy" (vla-get-layer obj))	
					(equal "0 proj rury" (vla-get-layer obj))	
				)
				(/= 0.0 (vla-get-Length obj))	
				;(equal "dane" (LM:effectivename obj))	
				(equal 5 (LM:get-Color obj))	
			)
			(progn
				(setq i_rura (+ i_rura 1))
				(setq el_rura (append el_rura (list obj)))
				;(princ el_rura )
			)
		)
		(if (and
				;(equal "Model" (cdr (assoc 410 ed)))
				(equal "AcDbPolyline" (vla-get-objectname obj))
				(equal "0 proj przy" (vla-get-layer obj))
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
			; (print (vla-get-length el_kabel))
			;(princ el_kabel )
		)
		)
		
		(if 
			(equal "obliczenia" (LM:effectivename obj))
		(progn
			(setq e_obliczenia obj)
			(foreach % list_att
				(setq h (car %)
					  v (cdr %)
				)
				(cd:BLK_SetAttValueVLA e_obliczenia h v)
			)
		)
		)
		(if 
			(equal "zestawienie" (LM:effectivename obj))
		(progn
			(setq e_zestawienie obj)
		)
		)
		(if (and
				(or
					(equal "AcDb2dPolyline" (vla-get-objectname obj))
					(equal "AcDbPolyline" (vla-get-objectname obj))
				)
				(equal "0 linia rozgraniczajaca" (vla-get-layer obj))
				(or
					(equal 84 (vla-get-color obj))
					(equal 256 (vla-get-Color obj))
				)
			)
			(progn
				(setq el_liniarozgr obj)
			)
		)
		(if (and
				(equal "AcDbMText" (vla-get-objectname obj))
				(or
					(equal "Dzialki" (vla-get-layer obj))
					(equal "DZIALKI" (vla-get-layer obj))
					(equal "211-Nr dzia³ki" (vla-get-layer obj))
				)
				(equal :VLAX-TRUE (vla-get-BackgroundFill obj))	
				;(equal "daneo" (LM:effectivename obj))	
				;(equal 256 (vla-get-Color obj))	
				
			)
			(progn
				(setq e_list_dz (cons (vla-get-TextString obj) e_list_dz))
			)
		)	
		(if (and
				(equal "AcDbBlockReference" (vla-get-objectname obj))
				(or
					(equal "SK" (LM:effectivename obj))
					(equal "ZP" (LM:effectivename obj))
				)
			)
			(progn
				(setq e_list_SKP (cons (LM:effectivename obj) e_list_SKP))
			)
		)	
		
	)
		
	(vlax-for obj pspace
		(if (and 
			;(/= "Model" (cdr (assoc 410 ed)))
			(equal "dane" (LM:effectivename obj))
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
		(if 
			(equal "obliczenia" (LM:effectivename obj))
		(progn
			(setq e_obliczenia obj)
			(foreach % list_att
				(setq h (car %)
					  v (cdr %)
				)
				(cd:BLK_SetAttValueVLA e_obliczenia h v)
			)
		)
		)
		(if 
			(equal "zestawienie" (LM:effectivename obj))
		(progn
			(setq e_zestawienie obj)
		)
		)
		(if (or
				(equal "uprawnienia_P" (LM:effectivename obj))
				(equal "uprawnienia_M" (LM:effectivename obj))
			)
		(progn
			(setq e_uprawnienia obj)
		)
		)	
		(if (and
				(equal "AcDbMText" (vla-get-ObjectName obj))
				(vl-string-search "Umowa" (vla-get-TextString obj))
			)
		(progn
			(setq e_umowa obj)
		)
		)
		(if (and
				(equal "AcDbMText" (vla-get-ObjectName obj))
				(vl-string-search "13.03.2020" (vla-get-TextString obj))
			)
		(progn
			(setq e_pelno obj)
		)
		)
		
		(if (and
				(equal "AcDbMText" (vla-get-ObjectName obj))
				(or
					(vl-string-search "0,8 m" (vla-get-TextString obj))
					(vl-string-search "0.8 m" (vla-get-TextString obj))
					(vl-string-search "0,8m" (vla-get-TextString obj))
					(vl-string-search "0.8m" (vla-get-TextString obj))
				)
		)
		(progn
			(setq e_textproj obj)
		)
		)
		
		(if (and
				(equal "AcDbMLeader" (vla-get-ObjectName obj))
				(vl-string-search "(??)" (vla-get-TextString obj))
			)
		(progn
			(setq e_SKP obj)
		)
		)
		(if ;(and
				(equal "AcDbMText" (vla-get-ObjectName obj))
				
			;)
		(progn
			(cond
				((vl-string-search "Rura os³onowa  DVR 75" (vla-get-TextString obj))
					(setq e_ruryDVR obj)
				)
				((vl-string-search "Rura os³onowa  SRS 50" (vla-get-TextString obj))
					(setq e_rurySRS obj)
				)
			)
		)
		)
		(if ;(and
				(or
					(equal "AcDbMText" (vla-get-ObjectName obj))
					(equal "AcDbMLeader" (vla-get-ObjectName obj))
				)
			;)
		(progn
			(cond
				((vl-string-search "YAKXs 4x25" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x25")
				)
				((vl-string-search "YAKXs 4x35" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x35")
				)
				((vl-string-search "YAKXs 4x50" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x50")
				)
				((vl-string-search "YAKXs 4x70" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x70")
				)
				((vl-string-search "YAKXs 4x95" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x95")
				)
				((vl-string-search "YAKXs 4x120" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x120")
				)
				((vl-string-search "YAKXs 4x240" (vla-get-TextString obj))
					(setq e_rodzajkabla "YAKXs 4x240")
				)
			)
		)
		)
		
	)
		;(print e_rurySRS)
		;(print e_ruryDVR)
		;(print e_rodzajkabla)
	;(print "el_rura_test")
	(setq i_rura_srs i_rura)
	(if (/= el_rura nil)
	(progn
		(setq j 0)
		(while (< j i_rura)
			;(princ j)
			;(princ (cd:rur (nth j el_rura) (nth (1+ j) el_rura)))
			(if (and
					(/= nil (nth j el_rura))
					(/= nil (nth (1+ j) el_rura))
				)
			(progn
				(if (equal "dvr" (cd:rur (nth j el_rura) (nth (1+ j) el_rura)))
				(progn
					;(princ "dvr")
					
					(setq len1 (vla-get-length (nth j el_rura)))
					(setq len2 (vla-get-length (nth (1+ j) el_rura)))
					(if (> len1 len2)
						(setq el_dvr (+ el_dvr (cd:Round len1 1)))
						(setq el_dvr (+ el_dvr (cd:Round len2 1)))
					)
					(setq i_rura_srs (- i_rura_srs 1))
				)
				)
				(if (equal "srs" (cd:rur (nth j el_rura) (nth (1+ j) el_rura)))
				(progn
					;(princ "srs")
					(setq len1 (vla-get-length (nth j el_rura)))
					(setq len2 (vla-get-length (nth (1+ j) el_rura)))
					(if (> len1 len2)
						(setq el_srs (+ el_srs (cd:Round len1 1)))
						(setq el_srs (+ el_srs (cd:Round len2 1)))
					)
					;(setq el_srs (+ el_srs (cd:Round (vla-get-length (nth j el_rura)) 1)))
				)
				)
			)
			(progn
				(setq alert_text (strcat alert_text  "\n" "Mo¿liwy problem z rurami"))
			)
			)
		(setq j (+ 2 j))
		)
		;(print el_srs)
		;(print el_dvr)
		;(setq el_srs (cd:RoundUp el_srs 1))
		;(setq el_dvr (cd:RoundUp el_dvr 1))
		;(setq el_srs (cd:Round el_srs 1))
		;(setq el_dvr (cd:Round el_dvr 1))
	)
	)
	;(print "e_zestawienie_test")
	;(print e_zestawienie)
	(if e_zestawienie
	(progn
		(setq dl_trasy (atof (cd:BLK_GetAttValueVLA e_model "dl_trasy"))) ; pobiera dlugosc trasy 
		(cd:BLK_SetAttValueVLA e_zestawienie "folia" (rtos (* 1.03 (- dl_trasy el_srs)) 2 0)) ; wpisuje w blok e_zestawienie wartosc folia
		(cd:BLK_SetAttValueVLA e_zestawienie "oznaczniki" (rtos (+ 2 i_rura (/ dl_trasy 9 )) 2 0)) ; wpisuje w blok e_zestawienie wartosc oznaczniki
		;(print "OK4")
		(setq tempPiasek (* 0.082 (- dl_trasy el_srs el_dvr))) ; obliczanie ilosci piasku 
		(if (> tempPiasek 0) 
			(setq tempPiasek (+ tempPiasek 0.3))
			(setq tempPiasek 0.3)
		)
		
		(cd:BLK_SetAttValueVLA e_zestawienie "piasek" (rtos tempPiasek 2 2))
		(cd:BLK_SetAttValueVLA e_zestawienie "olkit" (rtos (* i_rura_srs 2) 2 0))
		
		(if (< dl_trasy (+ el_dvr el_srs)) 
			(setq el_dvr (- dl_trasy el_srs))
		)
		
		(cd:BLK_SetAttValueVLA e_zestawienie "dvr" (rtos el_dvr 2 0))
		(cd:BLK_SetAttValueVLA e_zestawienie "srs" (rtos el_srs 2 0))
	)
	)

	; (print "el_kabel_Test")
	; (print el_kabel)
	(print "Debug_004")
	; (print (vla-get-length el_kabel))
	(if el_kabel
	(progn
		;(print "el_kabel_1")
		(setq el_kabel_round (cd:Round (vla-get-length el_kabel) 1))
		; (print el_kabel_round)		
		(setq points nil )
		(setq p (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates el_kabel))))
		(setq i -2)
		(while (< (setq i (+ i 2)) (length p))	
			(setq n1 (nth i p))
			(setq n2 (nth (1+ i) p))
			(if (and n1 n2)
				(setq points (cons (list n1 n2) points ))
			)
			(if (> i 100)(print (length p)))
		)
		
		(vlax-for obj mspace
		(if (and
				;(equal "Model" (cdr (assoc 410 ed)))
				;(equal "AcDbPolyline" (vla-get-objectname obj))
				(or
					(and
						(equal "0 proj przy" (vla-get-layer obj))
						(equal "mufa" (LM:effectivename obj))
					)
					; (and
						; (equal "0 proj przy" (vla-get-layer obj))
						; (equal "SK" (LM:effectivename obj))
					; )
					(and
						(equal "Uz_Energia" (vla-get-layer obj))
						(or
							(equal "AcDb3dPolyline" (vla-get-ObjectName obj))
							(equal "1763" (LM:effectivename obj))
						)
					)
					(and
						(equal "Linie_napowietrzne" (vla-get-layer obj))
						(equal "AcDbBlockReference" (vla-get-ObjectName obj))
					)
					(equal "s³up (punkt)" (vla-get-layer obj))
					(equal "s³up ³¹czony (linia)" (vla-get-layer obj))
					(equal "s³up ³¹czony (powierzchnia) - symbol" (vla-get-layer obj))
					(equal "510-Podpora jednos³upowa symb.-SEPOW" (vla-get-layer obj))
					(equal "szafa kablowa (punkt)" (vla-get-layer obj))
					(equal "szafa elektroenergetyczna (punkt)" (vla-get-layer obj))
					(equal "szafa elektroenergetyczna (powierzchnia)" (vla-get-layer obj))
					(equal "w³az - siec elektroenergetyczna (powierzchnia)" (vla-get-layer obj))
					(equal "s³upowa stacja transformatorowa (punkt)" (vla-get-layer obj))
					(equal "stacja transformatorowa (powierzchnia)" (vla-get-layer obj))
				)
				; (or
					; (equal 256 (vla-get-color obj))
					; (equal 1 (vla-get-Color obj))
				; )
			)
			(progn
				(setq intPoints nil)
				(setq test nil)
				(setq intPoints (vla-IntersectWith el_kabel obj acExtendNone))	
				;(setq intPoints (vla-IntersectWith el_kabel obj acExtendOtherEntity ))	
				(setq test (vlax-safearray->list (vlax-variant-value intPoints)))
				;(print "Test1")
				;(print (LM:effectivename obj))
				(if test 
				(progn
					;(print "Test2")
					;(print obj)
					(setq el_podlacz obj)
					;(print obj)
					;(print (vla-get-layer obj))
					;(print (LM:effectivename obj))
					;(print test)
				)
				)
			)
		)
		)
		
		(if el_podlacz
		(progn
			;(print (vla-get-Layer el_podlacz))
			(cond 
				((or
					(equal "s³up (punkt)" (vla-get-Layer el_podlacz))
					(equal "s³up ³¹czony (linia)" (vla-get-Layer el_podlacz))
					(equal "s³up ³¹czony (powierzchnia) - symbol"  (vla-get-Layer el_podlacz))
					(equal "510-Podpora jednos³upowa symb.-SEPOW" (vla-get-Layer el_podlacz))
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 13) 1))
				)
				((or
					(equal "szafa kablowa (punkt)" (vla-get-Layer el_podlacz))
					(equal "szafa elektroenergetyczna (punkt)" (vla-get-Layer el_podlacz))
					(equal "szafa elektroenergetyczna (powierzchnia)" (vla-get-Layer el_podlacz))
					(equal "w³az - siec elektroenergetyczna (powierzchnia)" (vla-get-Layer el_podlacz))
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 4) 1))
				)
				(
					(equal "s³upowa stacja transformatorowa (punkt)" (vla-get-Layer el_podlacz))
					
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 8) 1))
				)
				((and
					(equal "0 proj przy" (vla-get-Layer el_podlacz))
					(equal "mufa" (LM:effectivename el_podlacz))
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 4) 1))
				)
				((and
					(equal "Uz_Energia" (vla-get-Layer el_podlacz))
					(or
						(equal "AcDb3dPolyline" (vla-get-ObjectName el_podlacz))
						(equal "1763" (LM:effectivename el_podlacz))
					)
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 4) 1))
				)
				((and
					(equal "Linie_napowietrzne" (vla-get-layer el_podlacz))
					(equal "AcDbBlockReference" (vla-get-ObjectName el_podlacz))
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 13) 1))
				)
				((and
					(equal "stacja transformatorowa (powierzchnia)" (vla-get-layer el_podlacz))
					(equal "AcDb2dPolyline" (vla-get-ObjectName el_podlacz))
				 )
					(setq el_calkowita_round (cd:Round (+ (* 1.04 el_kabel_round) 6) 1))
				)
				(t 
					;(setq el_calkowita_round (atoi (cd:BLK_GetAttValueVLA e_model "dl_laczna")))
					(print "Nie mo¿na dopasowaæ sk¹d jest miejsce przy³¹czenia")
				)
			
			)
		)
		)
		; (print "el_podlacz")
		; (print el_podlacz)
		; (print el_kabel_round)
		; (print el_calkowita_round)
		(if el_kabel_round
			(if (equal el_kabel_round (atoi (cd:BLK_GetAttValueVLA e_model "dl_trasy")))
				(print (strcat "D³ugoœæ trasy: " (rtos el_kabel_round 2 0) " m"))
				(setq alert_text (strcat alert_text  "\n" "Ró¿ne wartoœci trasy !!!: " "odczytana z modelu: " (rtos el_kabel_round 2 0) " m , " "odczytane z bloku \"dane\": " (rtos (atoi (cd:BLK_GetAttValueVLA e_model "dl_trasy")) 2 0) " m" ))
			)
		)
		(if el_calkowita_round
			(if (equal el_calkowita_round (atoi (cd:BLK_GetAttValueVLA e_model "dl_laczna")))
				(print (strcat "D³ugoœæ ³¹czna: " (rtos el_calkowita_round 2 0) " m"))
				(setq alert_text (strcat alert_text  "\n" "Ró¿na wartoœæ ³¹czna kabla !!!: " "odczytana z modelu: " (rtos el_calkowita_round 2 0) " m , " "odczytane z bloku \"dane\": " (rtos (atoi (cd:BLK_GetAttValueVLA e_model "dl_laczna")) 2 0) " m" ))
			)
		)
		;(print "el_podlacz END")
		;(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		;(setvar "TILEMODE" 1)  ;<- na zak³adkê z przestrzeni¹ papieru
		;(vla-put-ActiveSpace doc 1) ;<- to samo co TILEM0DE
		;(if (= (vla-get-MSpace doc) :vlax-true) (vla-put-Mspace doc 1))
		
		;(print points)
		;(setq sear (ssget "_F" points (list (cons -4  "<OR") (cons 8 "0 proj przy") (cons 8 "s³up (punkt)") (cons 8 "s³up ³¹czony (linia)") (cons 8 "szafa kablowa (punkt)") (cons 8 "szafa elektroenergetyczna (punkt)") (cons -4  "OR>"))))
		;(setq sear (cd:SSX_Convert sear 1))
		;(foreach obj sear
		;	(print obj)
			
		;)
		;(if (= (vla-get-MSpace doc) :vlax-false) (vla-put-Mspace doc 0)) ;<- odpowiednik _PSpace
		;(print "el_kabel_end")
	)
	)
	
	(if (and (not e_uprawnienia) (or (wcmatch nazwaarkusza "1*") (wcmatch nazwaarkusza "0_Profil") ) (or (vl-position "0_Tytulowa_P" listaarkuszy) (vl-position "0_Tytulowa_M" listaarkuszy)))
	(progn
		(cond 
			((and (vl-position "0_Tytulowa_P" listaarkuszy) (vl-position "0_Tytulowa_M" listaarkuszy))
				(setq alert_text (strcat alert_text  "\n" "Dwie strony tytu³owe zdecydujcie sie Grande projektande albo wstaw rêcznie :D"))
			)
			((vl-position "0_Tytulowa_P" listaarkuszy)
				(setq blkObj (cd:BLK_InsertBlock (list 346.5 34.9 0.0) "Z:\\AZART projektowanie\\PROJEKTY\\Szablony CAD\\paleta\\opisy\\uprawnienia_P" nil nil T))
				(vla-put-layer blkObj "0")
			)
			((vl-position "0_Tytulowa_M" listaarkuszy)
				(setq blkObj (cd:BLK_InsertBlock (list  346.5 34.9 0.0) "Z:\\AZART projektowanie\\PROJEKTY\\Szablony CAD\\paleta\\opisy\\uprawnienia_M" nil nil T))
				(vla-put-layer blkObj "0")
			)
			
		)
	)
	)
	
	(if (wcmatch nazwaarkusza "Umowa*")
	(progn
		(foreach obj1 listpelno 
			(if (vl-string-search (car obj1) SciezkaPliku)
				(setq pelno (cadr obj1)) 
			)
		)
		;(print pelno)
		(if e_pelno
			(vla-put-TextString e_pelno (vl-string-subst pelno "13.03.2020" (vla-get-TextString e_pelno)))
		)
	)
	)
	;(print "leci ")
	(if (or (wcmatch nazwaarkusza "Umowa*") (wcmatch nazwaarkusza "ZgodaMapa") (wcmatch nazwaarkusza "Trasa"))
	(progn
		(setq text_SKP "????")
			
		;(print e_list_SKP)
		(cond
			((and (not (vl-position "SK" e_list_SKP)) (vl-position "ZP" e_list_SKP))
				(setq text_SKP " ")
			)
			((and (vl-position "SK" e_list_SKP) (vl-position "ZP" e_list_SKP) (equal (vl-list-length (vl-positions "ZP" e_list_SKP)) 1))
				(setq text_SKP "(SK+P)")
			)
			((and (vl-position "SK" e_list_SKP) (vl-position "ZP" e_list_SKP) (equal (vl-list-length (vl-positions "ZP" e_list_SKP)) 2))
				(setq text_SKP "(SK+2P)")
			)
		)
		(if e_SKP
			(vla-put-TextString e_SKP (vl-string-subst text_SKP "(??)" (vla-get-TextString e_SKP)))
		)
	)
	)
	
	(setq kable (list
			'("YAKXs 4x16" "50" "75")
			'("YAKXs 5x16" "50" "75")
			'("YAKXs 4x25" "50" "75")
			'("YAKXs 5x25" "50" "75")
			'("YAKXs 4x35" "50" "75")
			'("YAKXs 4x50" "50" "75")
			'("YAKXs 4x70" "75" "75")
			'("YAKXs 4x95" "75" "110")
			'("YAKXs 4x120" "75" "110")
			'("YAKXs 4x150" "110" "160")
			'("YAKXs 4x240" "110" "160")
			'("3x1xXRUHAKXs 50/25" "110" "160")
			'("3x1xXRUHAKXs 70/25" "110" "160")
			'("3x1xXRUHAKXs 120/50" "160" "160")
	))
	
	(if (and e_rurySRS e_ruryDVR e_rodzajkabla (wcmatch nazwaarkusza "1*"))
	(progn
		;(print e_rurySRS)
		;(print e_ruryDVR)
		;(print e_rodzajkabla)
		(vla-put-textString e_rurySRS (strcat "Rura os³onowa  SRS " 
			(if e_rodzajkabla (nth 1 (assoc e_rodzajkabla kable)) "50" )))
		(vla-put-StyleName e_rurySRS "Arial")
		
		(vla-put-textString e_ruryDVR (strcat "Rura os³onowa  DVR " 
			(if e_rodzajkabla  (nth 2 (assoc e_rodzajkabla kable)) "75" )))
		(vla-put-StyleName e_ruryDVR "Arial")
		
	)
	)
	
	

	
	;(print "Uprawnienia list dz")
	(if (not el_liniarozgr)
	(progn
		(setq alert_text (strcat alert_text  "\n"  "Nie ma wrysowanej linii rozgraniczajacej"))
	)
	)
	;(print "Uprawnienia rozgr")
	(if e_umowa
	(progn
		(foreach obj1 listklocki 
			(if (vl-string-search (car obj1) SciezkaPliku)
				(setq umowa (cadr obj1)) 
			)
		)
		;(print e_umowa)
		(vla-put-TextString e_umowa (vl-string-subst umowa "ZRB/........................" (vla-get-TextString e_umowa)))
	)
	)
	
	(if (and  e_textproj (wcmatch nazwaarkusza "1*") (vl-position "Powiat" listaarkuszy) )
	(progn
		;(command "_regen" nil)
		;(print e_textproj)
		;(print (vla-get-TextString e_textproj))
		;(vla-put-TextString e_textproj (vl-string-subst  "1,1 m" "0,8 m" (vla-get-TextString e_textproj)))
		; (vla-put-StyleName e_textproj "Arial")
		(setq alert_text (strcat alert_text "\n" "Powiat!! Zmieñ g³êbokoœæ zakopania na 1,1 m" "\n"))
	)
	)
	
	(if (/= alert_text " ")
	(progn
		(alert alert_text )
	)
	)
	
	;(print e_list_dz)
	;(print (car e_list_dz))
	; (if e_list_dz
	; (progn
		; (setq dzpodmiot (cd:BLK_GetAttValueVLA e_model "DZIALKA"))
		; (setq dzprzyl (cd:BLK_GetAttValueVLA e_model "NR_DZ_PRZYLACZENIA"))
		; ;(print e_list_dz )
		; ;(print dzpodmiot )
		; ;(print dzprzyl )
		; ;(print (vl-list-length e_list_dz) )
		; ;(print (vl-position dzpodmiot e_list_dz))
		; (cond 
		; ((= (vl-list-length e_list_dz) 1)
			; (setq drogowa e_list_dz)
		; )
		; ((= (vl-list-length e_list_dz) 2)
			; (setq drogowa (vl-remove (nth (vl-position dzpodmiot e_list_dz) e_list_dz) e_list_dz))
		; )
		; ((and (= (vl-list-length e_list_dz) 3) (not (= dzpodmiot dzprzyl)))
			; (setq e_list_dz (vl-remove (nth (vl-position dzpodmiot e_list_dz) e_list_dz) e_list_dz))
			; (setq drogowa (vl-remove (nth (vl-position dzprzyl e_list_dz) e_list_dz) e_list_dz))
		; )
		; )
		
		; ;(print (strcat "Drogowa: ? " (mapcar 'strcat drogowa)))
		; (print (strcat "Drogowa: ? " (car drogowa)))
		
	; )
	; )
	
	(princ)
	
	(command "_regen" nil)
	(setvar "CMDECHO" 1)
	(CD:SYS_UndoEnd)
	nil
)