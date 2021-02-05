(vl-load-com)
(defun c:prof()
	(setq oldlayer (getvar "clayer"))
	(setq olddimstyle (getvar "dimstyle"))
	(setq lastOSMODE (getvar "OSMODE") )
	(princ lastOSMODE)
	(princ oldlayer)
	(setvar "OSMODE" 0)
	(setvar "clayer" "0")
	
	(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
	(setq mspace (vla-get-modelspace thisdrawing))
	;(setq acadObj (vlax-get-acad-object))
    ;(setq doc (vla-get-ActiveDocument acadObj))
	
	(defun *error* (errmsg)
   ; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		(setvar "OSMODE" lastOSMODE)
		(setvar "clayer" oldlayer)
		(cd:SYS_UndoEnd)
		nil
	;)
	); end *error*
  
   	(defun cd:pline(p1 p2)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
	)
	(defun cd:plineD(p1 p2)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Color myobj acRed)
		;(vla-put-Linetype myobj "DASHED")
	)
	(defun cd:pwymiar(p1 p2 wysokosc nazwa)
		;; Define the dimension
		(setq point1 (vlax-3d-point p1)
			  point2 (vlax-3d-point p2)
			  location (vlax-3d-point (+(car p1) 0.40) (+(cadr p1) wysokosc) 0.0))
		
		(setq dimObj (vla-AddDimAligned mspace point1 point2 location))
		;(princ (vlax-dump-object dimObj))
		(vla-put-TextOverride dimObj nazwa)
		(vla-put-StyleName dimObj "P50")
		(vla-Update dimObj)
	)
	(defun cd:pwymiarstyl()
		(setq flagDim (tblsearch "dimstyle" "P50")) ;looking for dimstyle
		(setq flagSty (tblsearch "style" "Arial")) ;looking for dimstyle
		(command "_dimstyle" nil)
		(if (not flagSty)
			(command "-styl" "Arial" "arial" "" "" "" "" "" "")
		)
		;(command "-dimstyle" "w" olddimstyle)
		(setvar "CMDECHO" 0)
		(setvar "DIMADEC" 0)
		(setvar "DIMALTD" 2)
		(setvar "DIMALTF" 25.4)
		(setvar "DIMALTRND" 0)
		(setvar "DIMALTTD" 2)
		(setvar "DIMALTTZ" 0)
		(setvar "DIMALTU" 2)
		(setvar "DIMALTZ" 0)
		(setvar "DIMDEC" 2)
		(setvar "DIMAPOST" "")
		(setvar "DIMASSOC" 2)
		(setvar "DIMASZ" 0.2)
		(setvar "DIMATFIT" 3)
		(setvar "DIMAUNIT" 0)
		(setvar "DIMAZIN" 0)
		(setvar "DIMCEN" 0.0781)
		(setvar "DIMCLRD" 0)
		(setvar "DIMCLRE" 0)
		(setvar "DIMCLRT" 0)
		(setvar "DIMDLE" 0)
		(setvar "DIMDLI" 0.0938)
		;(setvar "DIMDSEP" ".")
		(setvar "DIMEXE" 0.0469)
		(setvar "DIMEXO" 0.0469)
		(setvar "DIMFIT" 3)
		(setvar "DIMFRAC" 2)
		(setvar "DIMGAP" 0.1)
		(setvar "DIMJUST" 0)
		(setvar "DIMBLK" "_ARCHTICK")
		(setvar "DIMLFAC" 1)
		(setvar "DIMLUNIT" 4)
		(setvar "DIMLWD" 5)
		(setvar "DIMLWE" 5)
		(setvar "DIMPOST" "")
		(setvar "DIMRND" 0.01)
		(setvar "DIMSCALE" (getvar "ltscale"))
		(setvar "DIMTAD" 0)
		(setvar "DIMTDEC" 4)
		(setvar "DIMTFAC" 1)
		(setvar "DIMTM" 0)
		(setvar "DIMTMOVE" 0)
		(setvar "DIMTOLJ" 1)
		(setvar "DIMTP" 0)
		(setvar "DIMTSZ" 0)
		(setvar "DIMTVP" 0)
		(setvar "DIMTXSTY" "Arial")
		(setvar "DIMTXT" 0.1)
		(setvar "DIMTZIN" 0)
		(setvar "DIMUNIT" 2)
		(setvar "DIMZIN" 0)
		;(setvar "DIMLDRBLK" my_leaderblock)
		
		(if (not flagDim) 
			(command "-dimstyle" "Z" "P50" )
			(command "-dimstyle" "Z" "P50" "T")
		)
		(command "-dimstyle" "w" olddimstyle)
		(command "-styl" "Standard" "" "" "" "" "" "" "")
	)
	
	(defun cd:phatch(obj / patternName)
		(setq patternName "ANSI31"
		patternType 0
		bAssociativity :vlax-true)
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity acHatchObject))
		;(vlax-put-property HatchObj "PatternScale" scale)
	)
	
	(defun cd:plinedr(p1 p2)
		(setq p3 (list (car p2) (- (cadr p2) 0.3) 0))
		(setq p4 (list (car p1) (- (cadr p1) 0.3) 0))
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (cons p3 ptlist))
		(setq ptlist (cons p4 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Closed myobj :vlax-true)      ;; Open Polyline
		;; Define the hatch
		(setq patternName "ANSI31"
			patternType 0
			bAssociativity :vlax-true)
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity acHatchObject))
		(setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
		(vlax-safearray-put-element outerLoop 0 myobj)
		(vla-AppendOuterLoop hatchObj outerLoop)
		(vla-Evaluate hatchObj)
		;(vla-Regen doc :vlax-true)
		;(vla-Regen thisdrawing :vlax-true)
	)
	
	(defun cd:prtos(p / pr )
		(setq pr (list (rtos (car p) 2 10) (rtos (cadr p) 2 10) 0.0))
	)
	(defun cd:prura(p1 p2)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Color myobj acBlue)
		(setq os1 (vla-offset myobj 0.15))
		(setq os2 (vla-offset myobj -0.15))
		(vla-erase myobj)
	)
	(defun cd:pkropka(p1 color wymiar)
		;; Define the hatch
		(setq patternName "SOLID"
			patternType 0
			bAssociativity :vlax-true)
		(setq odlt (distance pdzialki1 p1))
		;; Create the associative Hatch object in model space
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity ))
		;;Create the outer boundary for the hatch (a circle)
		(setq center (vlax-3d-point (+(car pdl1) odlt) (-(cadr pdl1) (atof wymiar)) 0.0))
		(setq radius 0.1112)
		(setq circle (vla-AddCircle mspace center radius))
		(vla-put-Color circle color)
		(vla-put-Color hatchObj color)
		(setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
		(vlax-safearray-put-element outerLoop 0 circle)
		;; Append the outerboundary to the hatch object, and display the hatch
		(vla-AppendOuterLoop hatchObj outerLoop)
		(vla-Evaluate hatchObj)
		(cd:pwymiar (list (+(car pdl1) odlt) (-(cadr pdl1) (atof wymiar)) 0.0) (list (+(car pdl1) odlt) (cadr pdl1)  0.0) 0.3 wymiar)
		(cd:pline (list (+(car pdl1) odlt) (cadr pdl1) 0.0) (list (+(car pdl1) odlt) (cadr ps) 0.0))
		(setq pwymlist (append (list (+(car pdl1) odlt)) pwymlist))
		
	)
	(cd:pwymiarstyl)
	(cd:SYS_UndoBegin)
	(setq ps (getpoint "Punkt rysowania:\n"))
	
	;(setq pdzialki1 (LM:getpoint "Pierwszy punkt dzialki:\n"))
	;(setq pdzialki2 (getpoint "Drugi punkt dzialki:\n"))
	;(setq pkabla1 (getpoint "Pierwszy punkt kabla:\n"))
	;(setq pkabla2 (getpoint "Drugi punkt kabla:\n"))
	;(setq pdrogi1 (getpoint "Pierwszy punkt drogi:\n"))
	;(setq pdrogi2 (getpoint "Drugi punkt drogi:\n"))
	;(setq prura1 (getpoint "Pierwszy punkt rury:\n"))
	;(setq prura2 (getpoint "Drugi punkt rury:\n"))
	
	(defun LM:getpoint ( i text / entsellast testpt pt sel entsel ensellast objsel )
		(print text)
		(setq ret (list))
		(setq ensellast nil)
		(setq testpt t)
		(setq j 0)
		(while testpt
			(setq pt (grread t 15 0))
			(if (equal (car pt) 5)
			(progn
				(setq sel (nentselp (cadr pt)))
				(setq ensel (car sel))
				(if (and ensel (/= ensellast ensel))
				(progn
					(setq objsel (vlax-ename->vla-object ensel))
					(print (strcat text "Warstwa: < " (vla-get-layer objsel) " >"))
					(setq ensellast ensel)
				)
				)
			)
			)
			(if (equal (car pt) 3)
			(progn
				(setq sel (nentselp (cadr pt)))
				(setq ensel (car sel))
				(if (= i 1)  
					(setq ret (cadr pt))
					(setq ret (append (list (cadr pt)) ret))
				)
				(setq j (1+ j))
			)
			)
			(if (or 
					(>= j i)
					(and (equal (car pt) 2) (equal (cadr pt) 13))
				)
				(setq testpt nil)
			)
		)
		ret
	)
	
	(setq pdzialki1 (LM:getpoint 1 "Pierwszy punkt dzialki: "))
	(setq pdzialki2 (LM:getpoint 1 "Drugi punkt dzialki: "))
	(setq pkabla1 (LM:getpoint  1 "Pierwszy punkt kabla: "))
	(setq pkabla2 (LM:getpoint 1 "Drugi punkt kabla: "))
	(setq pdrogi1 (LM:getpoint 1 "Pierwszy punkt drogi: "))
	(setq pdrogi2 (LM:getpoint 1 "Drugi punkt drogi: "))
	(setq prura1 (LM:getpoint 1 "Pierwszy punkt rury: "))
	(setq prura2 (LM:getpoint 1 "Drugi punkt rury: "))
	
	(setq pwymlist nil)
		
	(setq ptelelist nil)
	(setq ptelelist (LM:getpoint 10 "Punkt kolizji telekomunikacja: " ))
	
	(setq pgazlist nil)
	(setq pgazlist (LM:getpoint 10 "Punkt kolizji gazu: " ))
	
	(setq pwodalist nil)
	(setq pwodalist (LM:getpoint 10 "Punkt kolizji woda: " ))
	
	(setq pkanlist nil)
	(setq pkanlist (LM:getpoint 10 "Punkt kolizji kanalizacja: " ))
	
	(setq penelist nil)
	(setq penelist (LM:getpoint 10 "Punkt kolizji kabla: " ))
	
	;(setq ptelelist nil)
	;(setq ptelelist (LM:getpoint 10 "Punkt kolizji telekomunikacja:\n" ))
	;(while (setq ptele1 (getpoint "Punkt kolizji telekomunikacja:\n" ))
	;	(setq ptelelist (append (list ptele1) ptelelist))
	;)
	;(setq pgazlist nil)
	;(while (setq pgaz1 (getpoint "Punkt kolizji gazu:\n" ))
	;	(setq pgazlist (append (list pgaz1) pgazlist))
	;)
	;(setq pwodalist nil)
	;(while (setq pwoda1 (getpoint "Punkt kolizji woda:\n" ))
;		(setq pwodalist (append (list pwoda1) pwodalist))
;	)
;	(setq pkanlist nil)
;	(while (setq pkan1 (getpoint "Punkt kolizji kanalizacja:\n" ))
;		(setq pkanlist (append (list pkan1) pkanlist))
;	)
;	(setq penelist nil)
;	(while (setq pene1 (getpoint "Punkt kolizji kabla:\n" ))
;		(setq penelist (append (list pene1) penelist))
;	)
	;(setq ps (cd:prtos ps))
	;(setq pdzialki1 (cd:prtos ps))
    ;(setq pdzialki2 (cd:prtos ps))
    ;(setq pdrogi1 (cd:prtos ps))
    ;(setq pdrogi2 (cd:prtos ps))
	;(princ ps)
	(setq odl (distance pdzialki1 pdzialki2)) 
	(setq odldr (distance pdrogi1 pdrogi2)) 
	(setq odlka1 (distance pdzialki1 pkabla1)) 
	(setq odlka2 (distance pkabla2 pdzialki2)) 
	(setq odlzi1 (distance pdzialki1 pdrogi1)) 
	(setq odlzi2 (distance pdrogi2 pdzialki2))
	(setq odlr1 (distance prura1 pdrogi1)) 
	(setq odlr2 (distance pdrogi2 prura2)) 
	
	;(princ odl1)
	(setq pd1 (list (+(car ps) 2)  (cadr ps) 0.0))
	(setq pd2 (list (+(+(car ps) odl) 2) (cadr ps) 0.0))
	(setq pdr1 (list (+(car pd1) odlzi1) (cadr ps) 0.0))
	(setq pdr2 (list (+(car pdr1) odldr) (cadr ps) 0.0))
	(setq pdl1 (list (car pd1) (+(cadr ps) 4) 0.0))
	(setq pdl2 (list (car pd2) (+(cadr ps) 4) 0.0))
	(setq pdrl1 (list (car pdr1) (+(cadr pdr1) 4) 0.0))
	(setq pdrl2 (list (car pdr2) (+(cadr pdr2) 4) 0.0))
	(setq pdk1 (list (+(car pdl1) odlka1) (-(cadr pdl1) 1.2) 0.0))
	(setq pdk2 (list (-(car pdl2) odlka2) (- (cadr pdl2) 1.2) 0.0))
	(setq pdkru1 (list (-(car pdr1) odlr1) (cadr pdk1) 0.0))
	(setq pdkru2 (list (+(car pdr2) odlr2) (cadr pdk2) 0.0))
	
	(setq textStringOdl "Odleglosc [m]"
          textStringTitle "PRZEKROJ POPRZECZNY DROGI W MIEJSCU PRZEJSCIA KABLA SKALA 1:50"
          textStringDzialka "Dzialka nr:???")
   

    (setq textObj (vla-AddText mspace textStringOdl (vlax-3d-point (+(car ps) 0.1) (+(cadr ps) 0.1) 0.0) 0.15)) 
	(vla-put-stylename textObj "Arial")
	(setq textObj (vla-AddText mspace textStringTitle (vlax-3d-point (+(car ps) 1.55) (+(cadr ps) 6.5) 0.0) 0.15))  
	(vla-put-stylename textObj "Arial")
	(setq textObj (vla-AddText mspace textStringDzialka (vlax-3d-point (+(car ps) 0.7) (+(cadr ps) 1) ) 0.15))  
	(vla-put-stylename textObj "Arial")
	(cd:pline ps pd2)
	;(cd:pline ps pdzialki1 nil)
	(cd:pline (list (car ps) (+ (cadr ps) 0.6)  0.0) (list (+ (+ (car ps) odl) 2) (+ (cadr ps) 0.6) 0.0))
	(cd:pline ps (list (car ps) (+ (cadr ps) 0.6)  0.0))
	(cd:pline pd1 pdl1)
	(setq pwymlist (append (list (car pd1)) pwymlist))
	(cd:pline pd2 pdl2)
	(setq pwymlist (append (list (car pd2)) pwymlist))
	(cd:pline pdr1 pdrl1)
	(setq pwymlist (append (list (car pdr1)) pwymlist))
	(cd:pline pdr2 pdrl2)
	(setq pwymlist (append (list (car pdr2)) pwymlist))
	(cd:plinedr pdrl1 pdrl2 )
	(cd:pwymiar pdl1 pdrl1 2 "pas zieleni" )
	(cd:pwymiar pdrl1 pdrl2 2 "droga" )
	(cd:pwymiar pdrl2 pdl2 2 "pas zieleni" )
	(cd:pline pdl1 pdl2)
	(cd:plineD pdk1 pdk2)
	(cd:pwymiar pdrl1 (list (car pdrl1) (- (cadr pdrl1) 1.2) 0.0) 1 "1.2")
	(cd:prura pdkru1 pdkru2)
	
	(foreach n ptelelist
		(cd:pkropka n acMagenta "0.6")
	)
	(foreach n pgazlist
		(cd:pkropka n acYellow "0.8")
	)
	(foreach n pwodalist
		(cd:pkropka n acBlue "1.6")
	)
	(foreach n pkanlist
		(cd:pkropka n 26 "1.7")
	)
	(foreach n penelist
		(cd:pkropka n acRed "0.8")
	)
	
	(setq pwymlist (vl-sort pwymlist '<)) 
	
	(setq i 0)
	(while (<= i (-(vl-list-length pwymlist) 2))
		(setq p1 (list (nth i pwymlist) (cadr ps) 0.0)
			  p2 (list (nth (1+ i) pwymlist)(cadr ps) 0.0))
		(cd:pwymiar p1 p2 0.3 "")
		(setq i (1+ i))
	)
	(cd:SYS_UndoEnd)
	(setvar "OSMODE" lastOSMODE)
	(setvar "clayer" oldlayer)
	
)



;;-------------------------------------------------------------------------------------------------------------------------;;
;;	W sumie nowy skrypt na profil 		
;;-------------------------------------------------------------------------------------------------------------------------;;
(defun c:profilosupero( / rysuj ok1 ok2 ps pkabla1 pkabla2 odl odlzi1 odlzz pdzialki1 pdzialki2 odldr pdrogi1 pdrogi2 odlka1 odlka2 odlzi1  
	odlzi2 odlr1 prura1 odlr2 pzakres1 pzakres2 prura2 ptelelist pgazlist pwodalist pkanlist penelist )
	
	(setq oldlayer (getvar "clayer"))
	(setq olddimstyle (getvar "dimstyle"))
	(setq lastOSMODE (getvar "OSMODE") )
	(print lastOSMODE)
	(print oldlayer)
	(setvar "OSMODE" 512)
	(setvar "clayer" "0")
	
	(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
	(setq mspace (vla-get-modelspace thisdrawing))
	;(setq acadObj (vlax-get-acad-object))
    ;(setq doc (vla-get-ActiveDocument acadObj))
	
	(setq gora (/ pi 2))
	(setq dol (/ (* pi 3) 2))
	(setq lewo pi )
	(setq prawo 0)
	
	(defun *error* (errmsg)
   ; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
		(cd:SYS_UndoEnd)
		(setvar "OSMODE" lastOSMODE)
		(setvar "clayer" oldlayer)
		(vla-delete textObj)
		(redraw)
		nil
	;)
	); end *error*
  
   	(defun cd:pline(p1 p2)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		myobj
	)
	(defun cd:plineD(p1 p2)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Color myobj acRed)
		;(vla-put-Linetype myobj "DASHED")
	)
	(defun cd:pwymiar(p1 p2 wysokosc nazwa)
		;; Define the dimension
		(setq point1 (vlax-3d-point p1)
			  point2 (vlax-3d-point p2)
			  location (vlax-3d-point (+(car p1) 0.40) (+(cadr p1) wysokosc) 0.0))
		
		(setq dimObj (vla-AddDimAligned mspace point1 point2 location))
		;(princ (vlax-dump-object dimObj))
		(vla-put-TextInside dimObj :vlax-true)
		(if (<= (distance p1 p2) 0.4)
		(progn
			(vla-put-TextRotation dimObj 1.5708)
		)
		)
		(vla-put-TextOverride dimObj nazwa)
		(vla-put-StyleName dimObj "P50")
		(vla-put-TextStyle dimObj "Arial")
		(vla-Update dimObj)
	)
	(defun cd:pwymiarstyl()
		
		(LM:TextSet "Arial" "Arial")
		(LM:DimSet "P50")
		(setq flagDim (tblsearch "dimstyle" "P50")) ;looking for dimstyle
		;(setq flagSty (tblsearch "style" "Arial")) ;looking for dimstyle
		(command "_dimstyle" nil)
		;(if (not flagSty)
		;	(command "-styl" "Arial" "arial" "" "" "" "" "" )
		;)
		
		;(command "-dimstyle" "w" olddimstyle)
		(setvar "CMDECHO" 0)
		(setvar "DIMADEC" 0)
		(setvar "DIMALTD" 2)
		(setvar "DIMALTF" 25.4)
		(setvar "DIMALTRND" 0)
		(setvar "DIMALTTD" 2)
		(setvar "DIMALTTZ" 0)
		(setvar "DIMALTU" 2)
		(setvar "DIMALTZ" 0)
		(setvar "DIMDEC" 2)
		(setvar "DIMAPOST" "")
		(setvar "DIMASSOC" 2)
		(setvar "DIMASZ" 0.2)
		(setvar "DIMATFIT" 3)
		(setvar "DIMAUNIT" 0)
		(setvar "DIMAZIN" 0)
		(setvar "DIMCEN" 0.0781)
		(setvar "DIMCLRD" 0)
		(setvar "DIMCLRE" 0)
		(setvar "DIMCLRT" 0)
		(setvar "DIMDLE" 0)
		(setvar "DIMDLI" 0.0938)
		;(setvar "DIMDSEP" ".")
		(setvar "DIMEXE" 0.0469)
		(setvar "DIMEXO" 0.0469)
		(setvar "DIMFIT" 3)
		(setvar "DIMFRAC" 2)
		(setvar "DIMGAP" 0.1)
		(setvar "DIMJUST" 0)
		(setvar "DIMBLK" "_ARCHTICK")
		(setvar "DIMLFAC" 1)
		(setvar "DIMLUNIT" 4)
		(setvar "DIMLWD" 5)
		(setvar "DIMLWE" 5)
		(setvar "DIMPOST" "")
		(setvar "DIMRND" 0.01)
		(setvar "DIMSCALE" (getvar "ltscale"))
		(setvar "DIMTAD" 0)
		(setvar "DIMTDEC" 4)
		(setvar "DIMTFAC" 1)
		(setvar "DIMTM" 0)
		(setvar "DIMTMOVE" 0)
		(setvar "DIMTOLJ" 1)
		(setvar "DIMTP" 0)
		(setvar "DIMTSZ" 0)
		(setvar "DIMTVP" 0)
		(setvar "DIMTXSTY" "Arial")
		(setvar "DIMTXT" 0.1)
		(setvar "DIMTZIN" 0)
		(setvar "DIMUNIT" 2)
		(setvar "DIMZIN" 0)
		;(setvar "DIMLDRBLK" my_leaderblock)
		;(print "flagDim")
		;(print flagDim)
		
		(if (not flagDim) 
			(command "_.dimstyle" "Z" "P50" )
			(command "_.dimstyle" "Z" "P50" "T")
		)
		;(command "-dimstyle" "w" olddimstyle)
		;(command "-styl" "Standard" "" "" "" "" "" "" "")
	)
	
	(defun cd:phatch(obj / patternName)
		(setq patternName "ANSI31"
		patternType 0
		bAssociativity :vlax-true)
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity acHatchObject))
		(setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
		(vlax-safearray-put-element outerLoop 0 obj)
		(vla-AppendOuterLoop hatchObj outerLoop)
		(vla-Evaluate hatchObj)
		;(vlax-put-property HatchObj "PatternScale" scale)
	)
	
	(defun cd:plinedr(p1 p2)
		;(setq p3 (list (car p2) (- (cadr p2) 0.3) 0))
		(setq p3 (polar p2 dol 0.3))
		;(setq p4 (list (car p1) (- (cadr p1) 0.3) 0))
		(setq p4 (polar p1 dol 0.3))
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (cons p3 ptlist))
		(setq ptlist (cons p4 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Closed myobj :vlax-true)      ;; Open Polyline
		;; Define the hatch
		;(print "go Hatch")
		(setq patternName "ANSI31"
			patternType 1
			bAssociativity :vlax-true)
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity ))
		(vla-put-PatternScale hatchObj 0.1)
		(vla-put-PatternType hatchObj 1)
		(vlax-invoke hatchObj 'AppendOuterLoop (list myobj))
		(vla-evaluate hatchObj )
	)
	
	(defun cd:prtos(p / pr )
		(setq pr (list (rtos (car p) 2 10) (rtos (cadr p) 2 10) 0.0))
	)
	(defun cd:prura(p1 p2 d)
		(setq ptlist nil)
		(setq ptlist (cons p1 ptlist))
		(setq ptlist (cons p2 ptlist))
		(setq ptlist (apply 'append ptlist))
		(if (= (rem (length ptlist) 3) 0)
			(progn (setq
				tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1)))
					)
			  (vlax-safearray-fill tmp ptlist)
			  (setq myobj (vla-addPolyline mspace tmp))
			)
		)
		(vla-put-Color myobj acBlue)
		(setq os1 (vla-offset myobj d))
		(setq os2 (vla-offset myobj (- 0 d)))
		(vla-erase myobj)
	)
	
	(defun cd:prow(p1 p2)
		(setq odlro1 (distance punktzero p1)) 
		(setq odlro2 (distance punktzero p2))
		(setq pdkro1 (polar pzzl1 prawo odlro1))
		(setq pdkro2 (polar pzzl1 prawo odlro2))
		(setq szerrowu (distance p1 p2)) 
		(setq pdkru3 (polar (polar pdkro1 dol 0.7) prawo (/ szerrowu 2)))
		(command "_arc" pdkro1 pdkru3 pdkro2 "")
	)
	
	(defun cd:pkropka(p1 color wymiar / pdl1 patternName patternType bAssociativity odlt hatchObj center radius circle outerLoop )
		;; Define the hatch
		(setq patternName "SOLID"
			patternType 1
			bAssociativity :vlax-true)
		(setq odlt (distance punktzero p1))
		(setq pdl1 (polar ps gora 4))
		(setq pdl1 (polar pdl1 prawo 2))
		;(setq pdl1 (polar pdl1 gora 4))
		;; Create the associative Hatch object in model space
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity ))
		;;Create the outer boundary for the hatch (a circle)
		(setq center (vlax-3d-point (+(car pdl1) odlt) (-(cadr pdl1) (atof wymiar)) 0.0))
		(setq radius 0.1112)
		(setq circle (vla-AddCircle mspace center radius))
		(vla-put-Color circle color)
		(vla-put-Color hatchObj color)
		(setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
		(vlax-safearray-put-element outerLoop 0 circle)
		;; Append the outerboundary to the hatch object, and display the hatch
		(vla-AppendOuterLoop hatchObj outerLoop)
		(vla-Evaluate hatchObj)
		(cd:pwymiar (list (+(car pdl1) odlt) (-(cadr pdl1) (atof wymiar)) 0.0) (list (+(car pdl1) odlt) (cadr pdl1)  0.0) 0.3 wymiar)
		(cd:pline (list (+(car pdl1) odlt) (cadr pdl1) 0.0) (list (+(car pdl1) odlt) (cadr ps) 0.0))
		(setq pwymlist (append (list (+(car pdl1) odlt)) pwymlist))
	)
	(defun DTR (a) (* PI (/ a 180.0)))
	
	(cd:pwymiarstyl)
	
	(defun LM:getpoint ( i text / ptf ret entsellast testpt pt sel entsel ensel ensellast objsel objlay)
		(print text)
		(setq ret (list))
		(setq ensellast nil)
		(setq testpt t)
		(setq j 0)
		;///////
		;(setq textObj (vla-AddText (GetActiveSpace) " " (vlax-3d-point (getvar 'VIEWCTR)) (/ (getvar 'VIEWSIZE) 50.0)))
		(setq textObj (vla-AddMText mspace (vlax-3d-point (getvar 'VIEWCTR)) 1 " " ))
		(vla-put-BackgroundFill textObj :vlax-true)
		
		(setq osf (LM:grsnap:snapfunction)
			  osm (getvar 'osmode)
		)
		(setq k 0)
		(while testpt
			(setq pt (grread t 15 0) code (car pt) data (cadr pt) vs (getvar 'VIEWSIZE))
			(redraw)
			(if (equal (car pt) 5)
			(progn
				
				(osf (cadr pt) osm)
				(setq sel (nentselp (cadr pt)))
				(setq ensel (car sel))
				
				(vla-put-Width textObj (/ vs 1.4))
				(vla-put-Height textObj (/ vs 50.0))
				(setq data (polar (polar data (DTR 270.0) (/ vs 30)) (DTR 0.0) (/ vs 30)) )
				(vla-put-InsertionPoint textObj (vlax-3d-point data))
				(vla-Update textObj)
				
				(if (> k 40)
				(progn
					(vla-put-TextString textObj " ")
					(setq k 0)
				)
				(progn
					(setq k (1+ k))
				)
				)
				
				(if (and ensel (/= ensellast ensel))
				(progn
					;(setq getensel (entget ensel))
					;(setq eentsel (cdr (assoc 330 getensel)))
					;(print getensel)
					(setq objsel (vlax-ename->vla-object ensel))
					;(setq objsel (vlax-ename->vla-object eentsel))
					;(print "ok1")
					(if (vlax-property-available-p objsel "Layer")
					(progn
						
						(vla-put-TextString textObj (strcat "Warstwa: < " (vla-get-layer objsel) " >"))
						(print (strcat text "  Aktualna warstwa: < " (setq lay (vla-get-layer objsel)) " >"))
						(setq ensellast ensel)
					)
					);end if
					;(print "ok3")
				)
				);end if
			)
			);end if
			
			;(if (equal (car pt) 5)
			;(progn
			;	(setq ptf (trans (osf (cadr pt) osm) 1 0))
			;	(setq ptf (list (car ptf)(cadr ptf) 0.0))
				;(setq sel (nentselp (cadr pt)))
			;	(setq sel (nentselp ptf))
			;	(setq ensel (car sel))
			;	(if (and ensel (/= ensellast ensel))
			;	(progn
			;		(setq objsel (vlax-ename->vla-object ensel))
			;		(if (vlax-property-available-p objlay "Layer")
			;		(progn
			;			(setq objlay (vla-get-layer objsel))
			;			(print (strcat text " || Warstwa: < " objlay " >"))
			;			(setq ensellast ensel)
			;		)
			;		)
			;	)
			;	)
			;)
			;);end if
			(if (equal (car pt) 3)
			(progn
				;(print "okejj")
				(setq ptf (cadr pt))
				;(setq sel (nentselp ptf))
				;(setq ensel (car sel))
				(cond 
					((= i 1)  
						(setq ret ptf)
					)
					((= i 2)
						(setq ret (append (list ret) ret))
						(setq ret (append (list ptf) ret))
					)
					((> i 2)
						(setq ret (append (list ptf) ret))
					)
				)
				;(print ptf )
				;(print ret )
				(setq j (1+ j))
			)
			);end if
			(if (or 
					(>= j i)
					;(equal (car pt) 2)
					(and 
						(equal (car pt) 2)
						(equal (cadr pt) 13)
					)
				)
			(progn
				;(print "exitaaaaaaaaaaaaaaaaa")
				(setq testpt nil)
			)
			);end if
		);end while
		
		(vla-delete textObj)

		(print ret)
		ret
	)
	
	(defun DCL_dodatki (/)
		(setq WYdodatki (nth (atoi $value) listdodatki))
		(print WYdodatki)
	)
	(defun DCL_rodzajrury (/)
		(setq WYrodzajrury (nth (atoi $value) listrodzajrury))
		(print WYrodzajrury)
	)
	(setq WYdodatki "Ogrodzenie")
	(setq WYrodzajrury "SRS")
	
	(cd:SYS_UndoBegin)
	(setq pwymlist nil)
	(setq ptelelist nil)
	(setq pgazlist nil)
	(setq pwodalist nil)
	(setq pkanlist nil)
	(setq penelist nil)
	(setq pdzialki1 nil)
	(setq pdzialki2 nil)
	(setq pzakres1 nil)
	(setq pzakres2 nil)
	(setq pdodatki (list))
	(setq prurylist (list))
	(setq prowlist (list))
	(setq ok1 "")
	
	(defun punkt_ok ( / )
		(if ps
			(set_tile "Punkt_rysowania_ok" (strcat (get_tile "Punkt_rysowania_ok") "  OK"))
			(set_tile "Punkt_rysowania_ok" "Podaj punkt rysowania")
		)
		(if (and pdzialki1 pdzialki2)
			(set_tile "Punkt_dzialki_ok" (strcat (get_tile "Punkt_dzialki_ok" ) "  OK"))
			(set_tile "Punkt_dzialki_ok" "Podaj punkty dzia³ki")
		)
		(if (and pzakres1 pzakres2)
			(set_tile "Punkt_zakres_ok" (strcat (get_tile "Punkt_zakres_ok" ) "  OK"))
			(set_tile "Punkt_zakres_ok" "Podaj zakres rysowania")
		)
		(if (and pkabla1 pkabla2)
			(set_tile "Punkt_kabla_ok" (strcat (get_tile "Punkt_kabla_ok" ) "  OK"))
			(set_tile "Punkt_kabla_ok" "Podaj punkty kabla")
		)
		(if (and prurylist)
			(set_tile "Punkt_rury_ok" (strcat (get_tile "Punkt_rury_ok" ) "  OK"))
			(set_tile "Punkt_rury_ok" "Podaj punkty rury")
		)
		(if (and pdrogi1 pdrogi2)
			(set_tile "Punkt_drogi_ok" (strcat (get_tile "Punkt_drogi_ok") "  OK"))
			(set_tile "Punkt_drogi_ok" "Podaj punkty drogi")
		)
		(if (and prowlist)
			(set_tile "Punkt_rowu_ok" (strcat (get_tile "Punkt_rowu_ok") "  OK"))
			(set_tile "Punkt_rowu_ok" "Podaj punkty rowu")
		)
		(if (or penelist ptelelist pgazlist pwodalist pkanlist)
			(set_tile "Punkt_media_ok" (strcat (get_tile "Punkt_media_ok" ) "  OK"))
			(set_tile "Punkt_media_ok" "Podaj media")
		)
	)
	
	(if
	(and
		(setq dcl (vl-filename-mktemp "tmp.dcl"))
		(setq des (open dcl "w"))
		(foreach str
		   (list 
				"test : dialog"
				"{"
				"    label = \"Wybierz dane do profilu \";"
				"    spacer;"
				"    : column"
				"    {" 
				"		 : row"
				"   	 {" 
				"	 	 	: text { key =\"Punkt_rysowania_ok\"; label = \"Podaj punkt rysowania\"; value = \"Podaj punkt rysowania\"; } "
				" 	 		: button { key =\"Punkt_rysowania\"; label = \"Wybierz\"; value = \"0\"; } "
				"		 }"
				"		 : row"
				"   	 {" 
				"			: text { key =\"Punkt_dzialki_ok\"; label = \"Podaj punkty dzia³ki\"; value = \"Podaj punkty dzia³ki \"; } "
				" 	 		: button { key =\"Punkt_dzialki\"; label = \"Wybierz\"; value = \"0\"; } "
				"		 }"
				"		 : row"
				"   	 {" 
				"		 	: text { key =\"Punkt_zakres_ok\"; label = \"Podaj zakres rysowania\"; value = \"Podaj zakres rysowania\"; } "
				" 	 		: button { key =\"Punkt_zakres\"; label = \"Wybierz\"; value = \"0\"; } "
				"		 }"
				"		 : row"
				"   	 {" 
				"	 	 	: text { key =\"Punkt_kabla_ok\"; label = \"Podaj punkty kabla\"; value = \"Podaj punkty kabla\"; } "
				"	 	 	: button { key =\"Punkt_kabla\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"
				"		 : row"
				"   	 {" 
				"			: text { key =\"Punkt_rury_ok\"; label = \"Podaj punkty rury\"; value = \"Podaj punkty rury\"; } "
				"	 	 	: button { key =\"Punkt_rury_czysc\";edit_width = 5; label = \"Czysc\"; value = \"0\"; } "
				"			: popup_list { key = \"rodzaj_rury\";edit_width = 6; action = \"(DCL_rodzajrury)\";alignment = \"left\"; value = \"0\"; label = \"\"; }"
				"	 	 	: button { key =\"Punkt_rury\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"	
				"		 : row"
				"   	 {" 
				"	 	 	: text { key =\"Punkt_drogi_ok\"; label = \"Podaj punkty drogi\"; value = \"Podaj punkty drogi\"; } "
				"	 	 	: button { key =\"Punkt_drogi\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"
				"		 : row"
				"   	 {" 
				"	 	 	: text { key =\"Punkt_rowu_ok\"; label = \"Podaj punkty rowu\"; value = \"Podaj punkty rowu\"; } "
				"	 	 	: button { key =\"Punkt_rowu_czysc\";edit_width = 5; label = \"Czysc\"; value = \"0\"; } "
				"	 	 	: button { key =\"Punkt_rowu\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"	
				"		 : row"
				"   	 {" 
				"	 	 	: text { key =\"Punkt_media_ok\"; label = \"Podaj media\"; value = \"Podaj media\"; } "
				"	 	 	: button { key =\"Punkt_media_czysc\";edit_width = 5; label = \"Czysc\"; value = \"0\"; } "
				"	 	 	: button { key =\"Punkt_media\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"	
				"		 : row"
				"   	 {" 
				"	 	 	: text { label = \"Dodatki\"; value = \"\"; } "
				"	 	 	: button { key =\"Punkt_dodatki_czysc\";edit_width = 5; label = \"Czysc\"; value = \"0\"; } "
				"			: popup_list { key = \"dodatki\";edit_width = 15; action = \"(DCL_dodatki)\";alignment = \"left\"; value = \"0\"; label = \"\"; }"
				"	 	 	: button { key =\"Punkt_dodatki\"; label = \"Wybierz\"; value = \"0\"; } "
				"    	 }"	
				"    }"
				"	 :column
						{ width = 30; fixed_width = true; alignment = right;"
				"		:row{"
				"			: button { label = \"Dalej\"; mnemonic = \"D\"; key = \"OK\"; width = 20; fixed_width = true; }"
				"			: button { label = \"Anuluj\"; mnemonic = \"C\"; key = \"Anuluj\"; is_cancel = true; width = 20; fixed_width = true; }"
				"		}"
				"	}"
				"   spacer;"
				;"    ok_cancel;"
				;"	  ok_only;"
				"}"
			)
			(write-line str des)
		)
		(not (setq des (close des)))
		(< 0 (setq dch (load_dialog dcl)))
		(new_dialog "test" dch)
		;(punkt_ok)
	)
	(progn
		(setq what_next 1)
		(while (> what_next 0)
			(LM:DCL_WriteToList (setq listdodatki (list "Ogrodzenie" "Slup" "ZL"  )) "dodatki" )
			(LM:DCL_WriteToList (setq listrodzajrury (list "SRS" "DVK" )) "rodzaj_rury" )
			
			(action_tile "OK" "(done_dialog 1)")
			(action_tile "Anuluj" "(done_dialog 2)")
			(action_tile "Punkt_rysowania" "(done_dialog 11)")
			(action_tile "Punkt_dzialki" "(done_dialog 12)")
			(action_tile "Punkt_kabla" "(done_dialog 13)")
			(action_tile "Punkt_drogi" "(done_dialog 14)")
			(action_tile "Punkt_rury" "(done_dialog 15)")
			(action_tile "Punkt_media" "(done_dialog 16)")
			(action_tile "Punkt_dodatki" "(done_dialog 17)")
			(action_tile "Punkt_zakres" "(done_dialog 18)")
			(action_tile "Punkt_rowu" "(done_dialog 19)")
			(action_tile "Punkt_rowu_czysc" "(done_dialog 21)")
			(action_tile "Punkt_rury_czysc" "(done_dialog 22)")
			(action_tile "Punkt_media_czysc" "(done_dialog 23)")
			(action_tile "Punkt_dodatki_czysc" "(done_dialog 24)")
			
			(setq what_next (start_dialog))
			(if(not(new_dialog "test" dch))(exit))
			(punkt_ok)
			(print what_next)
			(cond 
			((= 11 what_next)
				(term_dialog)
				(setq ps (getpoint "Punkt rysowania:\n"))
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 12 what_next)
				(term_dialog)
				(setq pdzialki1 (LM:getpoint 1 "Pierwszy punkt dzialki: "))
				(setq pdzialki2 (LM:getpoint 1 "Drugi punkt dzialki: "))
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 18 what_next)
				(term_dialog)
				
				(setq pzakres1 (LM:getpoint 1 "Pierwszy punkt zakresu: "))
				(setq pzakres2 (LM:getpoint 1 "Drugi punkt zakresu: "))
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 13 what_next)
				(term_dialog)
				(setq pkabla1 (LM:getpoint  1 "Pierwszy punkt kabla: "))
				(setq pkabla2 (LM:getpoint 1 "Drugi punkt kabla: "))
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 14 what_next)
				(term_dialog)
				(setq pdrogi1 (LM:getpoint 1 "Pierwszy punkt drogi: "))
				(setq pdrogi2 (LM:getpoint 1 "Drugi punkt drogi: "))
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 15 what_next)
				(term_dialog)
				;(setq prurylist nil)
				;(if (equal WYrodzajrury "SRS")
				;(progn
				;	(setq prura1 (LM:getpoint 1 (strcat "Pierwszy punkt rury " WYrodzajrury ": "))
				;	(setq prura2 (LM:getpoint 1 (strcat "Drugi punkt rury " WYrodzajrury ": "))
				(setq prurylist (cons (list WYrodzajrury 
									  (LM:getpoint 1 (strcat "Pierwszy punkt rury " WYrodzajrury ": "))
									  (LM:getpoint 1 (strcat "Drugi punkt rury " WYrodzajrury ": "))) 
									   prurylist))
									   
				;)
				;)
				(print prurylist)
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 19 what_next)
				(term_dialog)
				;(setq prowlist nil)
				(setq prowlist (cons (list  (LM:getpoint 1 (strcat "Pierwszy punkt rowu: "))
											(LM:getpoint 1 (strcat "Drugi punkt rowu: "))) prowlist))

				(print prowlist)
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 16 what_next)
				(term_dialog)
				
				
				(setq ptelelist (LM:getpoint 10 "Punkt kolizji telekomunikacja: " ))
				(setq pgazlist (LM:getpoint 10 "Punkt kolizji gazu: " ))
				(setq pwodalist (LM:getpoint 10 "Punkt kolizji woda: " ))
				(setq pkanlist (LM:getpoint 10 "Punkt kolizji kanalizacja: " ))
				(setq penelist (LM:getpoint 10 "Punkt kolizji kabla: " ))
				;(dialogg)
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 17 what_next)
				(term_dialog)
				(setq pdodatek nil)
				(setq pdodatek (LM:getpoint 10 "Punkt dodatku: " ))
				(setq pdodatki (append (list (list WYdodatki pdodatek)) pdodatki))
				(print pdodatki)
				
				(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 1 what_next)
				(term_dialog)
				
				
				(if (not ps)
				(progn
					(alert  "Podaj gdzie ma powstaæ ten twór :)")
					(new_dialog "test" dch)
					(punkt_ok)
				)
				(progn
					(if (and (and (not pdzialki1) (not pdzialki2)) (and (not pzakres1) (not pzakres2)))
					(progn
						(alert  "Podaj chocia¿ punkt dzia³ki/zakres :(")
						(new_dialog "test" dch)
						(punkt_ok)
					)
					(progn
						(if (and (not pkabla1) (not pkabla2))
						(progn
							(alert  "Podaj punkty kabla ")
							(new_dialog "test" dch)
							(punkt_ok)
						)
						(progn
							(if (not prurylist)
							(progn
								(alert  "Podaj punkty rury ")
								(new_dialog "test" dch)
								(punkt_ok)
							)
							(progn
								(if (and (not pdrogi1) (not pdrogi2))
								(progn
									(alert  "Podaj punkty drogi ")
									(new_dialog "test" dch)
									(punkt_ok)
								)
								(progn
									(setq rysuj t)
								)
								)
							)
							)
						)
						)
					)
					)
				)
				)
				
			)
			((= 2 what_next)
				;(unload_dialog "test")
				(setq rysuj nil)
				
				(term_dialog)
				(princ "\n*Cancel*")
			)
			((= 21 what_next)
				;(term_dialog)
				(setq prowlist (list))
				;(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 22 what_next)
				;(term_dialog)
				(setq prurylist (list))
				;(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 23 what_next)
				(setq ptelelist nil)
				(setq pgazlist nil)
				(setq pwodalist nil)
				(setq pkanlist nil)
				(setq penelist nil)
				;(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 24 what_next)
				(setq pdodatek nil)
				(setq pdodatki nil)
				;(new_dialog "test" dch)
				(punkt_ok)
			)
			((= 0 what_next)
				;(unload_dialog "test")
				;(setq rysuj nil)
				(term_dialog)
				(cd:SYS_UndoEnd)
				(setvar "OSMODE" lastOSMODE)
				(setvar "clayer" oldlayer)
				(redraw)
				(princ "\n*Cancel*")
			)
			)
			(command "regen")
			;(setq what_next 0)
		)
	)
	)
	(if rysuj 
	(progn
		(print "OKEJ0")
		
		(if (and pdzialki1 pdzialki2)
		(progn
			(setq punktzero pdzialki1)
			(setq punktend pdzialki2)
			(setq odld (distance pdzialki1 pdzialki2)) 
			(setq odlzz (distance pdzialki1 pdzialki2)) 
			(setq pd1 (polar ps prawo 2))
			(setq pzz1 (polar ps prawo 2))
			(setq pd2 (polar pd1 prawo odld))
			(setq pzz2 (polar pd1 prawo odld))
			(setq pdl1 (polar pd1 gora 4))
			(setq pzzl1 (polar pd1 gora 4))
			(setq pdl2 (polar pd2 gora 4))	
			(setq pzzl2 (polar pd2 gora 4))	
		)
		)
		
		(if (and pzakres1 pzakres2)
		(progn
			(setq punktzero pzakres1)
			(setq punktend pzakres2)
			(setq odlz (distance pzakres1 pzakres2)) 
			(setq odlzz (distance pzakres1 pzakres2)) 
			(setq pz1 (polar ps prawo 2))
			(setq pzz1 (polar ps prawo 2))
			(setq pz2 (polar pz1 prawo odlz))
			(setq pzz2 (polar pz1 prawo odlz))
			(setq pzl1 (polar pz1 gora 4))
			(setq pzzl1 (polar pz1 gora 4))
			(setq pzl2 (polar pz2 gora 4))	
			(setq pzzl2 (polar pz2 gora 4))	
			
			(if (and pdzialki1 pdzialki2)
			(progn	
				(setq odld1 (distance punktzero pdzialki1 )) 
				(setq odld2 (distance punktzero pdzialki2 ))
				(setq pd1 (polar pzz1 prawo odld1))
				(setq pd2 (polar pzz1 prawo odld2))
				(setq pdl1 (polar pd1 gora 4))
				(setq pdl2 (polar pd2 gora 4))	
			)
			)
		)
		(progn
			
		
		)
		)
		(if (and pdrogi1 pdrogi2)
		(progn
			(setq odlzi1 (distance punktzero pdrogi1 )) 
			(setq odlzi2 (distance punktzero pdrogi2 ))
			
			(setq odlzi1test (distance punktend pdrogi1 )) 
			(setq odlzi2test (distance punktend pdrogi2 ))
			
			(setq odltest (distance punktzero punktend ))
			
			(if (and pdrogi1 pdrogi2)
			(progn
				(setq pdr1 (polar pzz1 prawo odlzi1))
				(setq pdr2 (polar pzz1 prawo odlzi2))
				(setq pdrl1 (polar pdr1 gora 4))
				(setq pdrl2 (polar pdr2 gora 4))
			)
			)
		)
		)
		(if (and pkabla1 pkabla2)
		(progn
			(setq odlka1 (distance punktzero pkabla1)) 
			(setq odlka2 (distance punktzero pkabla2))
			(setq pdk1 (polar (polar pzzl1 dol 1.2) prawo odlka1))
			(setq pdk2 (polar (polar pzzl1 dol 1.2) prawo odlka2))
		)
		)
		
		(print "OKEJ1")
		
		(setq textStringOdl "Odleglosc [m]"
			  textStringTitle "PRZEKROJ POPRZECZNY DROGI W MIEJSCU PRZEJSCIA KABLA SKALA 1:50"
			  textStringDzialka "Dzialka nr:???")
	   

		(setq textObj (vla-AddText mspace textStringOdl (vlax-3d-point (+(car ps) 0.1) (+(cadr ps) 0.1) 0.0) 0.15)) 
		(vla-put-stylename textObj "Arial")
		(setq textObj (vla-AddText mspace textStringTitle (vlax-3d-point (+(car ps) 1.55) (+(cadr ps) 6.5) 0.0) 0.15))  
		(vla-put-stylename textObj "Arial")
		(setq textObj (vla-AddText mspace textStringDzialka (vlax-3d-point (+(car ps) 0.7) (+(cadr ps) 1) ) 0.15))  
		(vla-put-stylename textObj "Arial")
		(cd:pline ps pzz2)
		(cd:pline (polar ps gora 0.6) (polar (polar ps gora 0.6) prawo (+ odlzz 2)))
		(cd:pline ps (polar ps gora 0.6))
		(cd:pline pzz1 pzzl1)
		
		(if (and pdzialki1 pdzialki2)
		(progn	
			(setq pwymlist (append (list (car pd1)) pwymlist))
			(cd:pline pd1 pdl1)
			(setq pwymlist (append (list (car pd2)) pwymlist))
			(cd:pline pd2 pdl2)
		)
		)
		
		(setq pwymlist (append (list (car pzz1)) pwymlist))
		(cd:pline pzz2 pzzl2)
		(setq pwymlist (append (list (car pzz2)) pwymlist))
		(cd:pline pdr1 pdrl1)
		(setq pwymlist (append (list (car pdr1)) pwymlist))
		(cd:pline pdr2 pdrl2)
		(setq pwymlist (append (list (car pdr2)) pwymlist))
		(cd:plinedr pdrl1 pdrl2 )
		
		(if (and pdzialki1 pdzialki2)
		(progn	
			(cd:pwymiar pdl1 pdrl1 1.5 "pas zieleni" )
			(cd:pwymiar pdrl2 pdl2 1.5 "pas zieleni" )
		)
		)
		
		(cd:pwymiar pdrl1 pdrl2 1.5 "droga" )
		(if (and pzakres1 pzakres2)
		(progn
			(cd:pwymiar pzzl1 pdl1 2 "Dzialka nr: " )
			(cd:pwymiar pdl1 pdl2 2 "Dzialka nr: " )
			(cd:pwymiar pdl2 pzzl2  2 "Dzialka nr: " )
		)
		)
		
		(cd:pline pzzl1 pzzl2)
		(cd:plineD pdk1 pdk2)
		(cd:pwymiar pdrl1 (polar pdrl1 dol 1.2) 1 "1.2")
		
		(foreach n prurylist
			(if (and (setq prura1 (cadr n)) (setq prura2 (caddr n)))
			(progn

				(setq odlr1 (distance punktzero prura1)) 
				(setq odlr2 (distance punktzero prura2)) 
				(setq pdkru1 (polar (polar pzzl1 dol 1.2) prawo odlr1))
				(setq pdkru2 (polar (polar pzzl1 dol 1.2) prawo odlr2))
			)
			)
			(cond 
				((equal (car n) "SRS")
					(setq d 0.15)
				)
				((equal (car n) "DVK")
					(setq d 0.25)
				)
				(T
					(setq d 0.15)
				)
			)
			(cd:prura pdkru1 pdkru2 d)
		)
		
		(print "OKEJ rury")
		
		(foreach n prowlist
			(if (and (setq prow1 (car n)) (setq prow2 (cadr n)))
			(progn
					(cd:prow prow1 prow2)
			)
			)
		)
		
		(print "OKEJ row")
		
		(foreach n ptelelist
			(cd:pkropka n acMagenta "0.6")
		)
		(foreach n pgazlist
			(cd:pkropka n acYellow "0.8")
		)
		(foreach n pwodalist
			(cd:pkropka n acBlue "1.6")
		)
		(foreach n pkanlist
			(cd:pkropka n 26 "1.7")
		)
		(foreach n penelist
			(cd:pkropka n acRed "0.8")
		)
		
		(setq pwymlist (vl-sort pwymlist '<)) 
		
		(setq i 0)
		(while (<= i (-(vl-list-length pwymlist) 2))
			(setq p1 (list (nth i pwymlist) (cadr ps) 0.0)
				  p2 (list (nth (1+ i) pwymlist)(cadr ps) 0.0))
			(cd:pwymiar p1 p2 0.3 "")
			(setq i (1+ i))
		)
		
		(foreach n pdodatki
			(setq rodzaj (strcase (car n) T))
			(setq p (cadr n))
			
			(cond
			((or (equal "ogrodzenie" rodzaj) (equal "slup" rodzaj))
				(foreach m p
					(setq odld (distance punktzero m )) 
					(setq pdd (polar (polar pzz1 gora 4) prawo odld))
					(setq blockisnt (cd:BLK_InsertBlock pdd (strcat "Z:\AZART projektowanie\PROJEKTY\Szablony CAD\paleta\dodatki\profil\\" rodzaj ) '(1 1 1) 0 T))
				)
			)
			((equal "zl" rodzaj)
				(foreach m p
					(setq odld (distance punktzero m )) 
					(setq odlzl1 (distance pkabla1 m )) 
					(setq odlzl2 (distance pkabla2 m )) 
					(setq pdd (polar (polar pzzl1 dol 1.2) prawo odld))
					(setq pddmir (polar pdd dol 0.5))
					(setq blockisnt (cd:BLK_InsertBlock pdd (strcat "Z:\AZART projektowanie\PROJEKTY\Szablony CAD\paleta\dodatki\profil\\" rodzaj ) '(1 1 1) 0 T))
					(if (< (- odld odlzl1)(- odld odlzl2))
					(progn	
						(vla-mirror blockisnt (vlax-3d-point pdd) (vlax-3d-point pddmir))
						(vla-remove blockisnt)
					)
					)
				)
			)
			)
		)
	
		
	)
	)
	(cd:SYS_UndoEnd)
	(setvar "OSMODE" lastOSMODE)
	(setvar "clayer" oldlayer)
	(redraw)
)		

(print "Profil OKEJ wczytany")