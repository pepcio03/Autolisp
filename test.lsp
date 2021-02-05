(vl-load-com)
(print "Wczytuje plik TEST")
;;-----------------------=={  Dynamic Information Tool  }==----------------------;;
;;                                                                               ;;
;;  Information about an object is displayed upon the user moving the cursor     ;;
;;  over the object. The cursor size may be altered using the +/- keys, and the  ;;
;;  program mode may be changed using the TAB key.                               ;;
;;                                                                               ;;
;;  Click or Hit Enter to Exit the Program                                       ;;
;;  +/- Increase or Decrease Cursor Pick Box respectively.                       ;;
;;                                                                               ;;
;;  Hit 'TAB' to switch modes:-                                                  ;;
;;  DINFO Mode    ~    Information about an object is displayed.                 ;;
;;  LAYISO Mode   ~    Isolate a Layer by clicking an object on that layer,      ;;
;;                     Shift-click to turn on all layers.                        ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;  FUNCTION SYNTAX:  DINFO                                                      ;;
;;-------------------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011 - www.lee-mac.com                         ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.0    -    09-05-2009                                               ;;
;;                                                                               ;;
;;  First Release                                                                ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.1    -    04-04-2010                                               ;;
;;                                                                               ;;
;;  Complete program upgrade.                                                    ;;
;;  Removed Object Editing functionality (unnecessary).                          ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.2    -    06-04-2010                                               ;;
;;                                                                               ;;
;;  Added additional properties to list.                                         ;;
;;  Accounted for Radian measurements.                                           ;;
;;  Added 'Belongs to' Field.                                                    ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.3    -    07-04-2010                                               ;;
;;                                                                               ;;
;;  Fixed Dynamic Block SelectionSet error.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.4    -    07-04-2010                                               ;;
;;                                                                               ;;
;;  Added Dynamic Block Flag.                                                    ;;
;;-------------------------------------------------------------------------------;;
;;  Version 1.5    -    06-01-2011                                               ;;
;;                                                                               ;;
;;  Updated code formatting.                                                     ;;
;;  Fixed bug pertaining to Polylines within Blocks/XRefs.                       ;;
;;-------------------------------------------------------------------------------;;

(defun c:DInfo

  ;...............................................................................;

  (   /  ;;         --=={ Local Functions }==--          ;

         *error*
         _Display
         dxf
         _GetColour
         _GetLW
         _GetName
         _GetScale   
         PurgeLayer
         _PutDXF
         ss->lst
         _Stringify
         _Text
         _Update

         ;;         --=={ Local Variables }==--          ;

         -pi/4
         5pi/4
         alignment
         attachment
         celst
         cent
         cobj
         code
         data
         doc
         e
         express
         gr
         inter
         layers
         mode
         modelst
         msg
         msglst
         on
         owner
         pi/4
         r
         rad
         spc
         ss
         telst
         tent
         vs
         x

         ;;         --=={ Global Variables }==--         ;
   
         ; DInfo:Mode    -- Global: Function Number
         ; DInfo:cRad    -- Global: Cursor Aperture

  )

  (vl-load-com)
  

 
  ;;--------------------------------------------------------------------------------;;
  ;;                           --=={  Preliminaries  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq spc
    (vlax-get-property
      (setq doc
        (vla-get-ActiveDocument (vlax-get-acad-object))
      )
      (if (= 1 (getvar 'CVPORT)) 'PaperSpace 'ModelSpace)
    )
  )
  
  (vlax-for l (vla-get-layers doc)
    (setq layers (cons l layers))
    (if (eq :vlax-true (vla-get-Layeron l))
      (setq on (cons l on))
    )
  )  
  
  (setq Express
    (and (vl-position "acetutil.arx" (arx))
      (not
        (vl-catch-all-error-p
          (vl-catch-all-apply
            (function (lambda nil (acet-sys-shift-down)))
          )
        )
      )
    )
  )

  (setq ModeLst '("DINFO" "LAYISO"))

  (or DInfo:Mode (setq DInfo:Mode 0))
  (or DInfo:cRad (setq DInfo:cRad 25.0))

  (setq Alignment
    (list
      (cons acAlignmentLeft               "Left"          )
      (cons acAlignmentCenter             "Center"        )
      (cons acAlignmentRight              "Right"         )
      (cons acAlignmentAligned            "Aligned"       )
      (cons acAlignmentMiddle             "Middle"        )
      (cons acAlignmentFit                "Fit"           )
      (cons acAlignmentTopLeft            "Top-Left"      )
      (cons acAlignmentTopCenter          "Top-Center"    )
      (cons acAlignmentTopRight           "Top-Right"     )
      (cons acAlignmentMiddleLeft         "Middle-Left"   )
      (cons acAlignmentMiddleCenter       "Middle-Center" )
      (cons acAlignmentMiddleRight        "Middle-Right"  )
      (cons acAlignmentBottomLeft         "Bottom-Left"   )
      (cons acAlignmentBottomCenter       "Bottom-Center" )
      (cons acAlignmentBottomRight        "Bottom-Right"  )
    )
  )

  (setq Attachment
    (list
      (cons acAttachmentPointTopLeft      "Top-Left"      )
      (cons acAttachmentPointTopCenter    "Top-Center"    )
      (cons acAttachmentPointTopRight     "Top-Right"     )
      (cons acAttachmentPointMiddleLeft   "Middle-Left"   )
      (cons acAttachmentPointMiddleCenter "Middle-Center" )
      (cons acAttachmentPointMiddleRight  "Middle-Right"  )
      (cons acAttachmentPointBottomLeft   "Bottom-Left"   )
      (cons acAttachmentPointBottomCenter "Bottom-Center" )
      (cons acAttachmentPointBottomRight  "Bottom-Right"  )
    )
  )  

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Local Functions  }==--                       ;;
  ;;-------------------------------------------------------------------------------;;

  
  ;;  --=={  Error Handler  }==--
  
  (defun *error* ( msg )
    (if cEnt (entdel cEnt))
    (if tEnt (entdel tEnt))

    (if on (mapcar '(lambda ( x ) (vla-put-layeron x :vlax-true)) on))

    (PurgeLayer "LMAC_DINFO")
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  ;.................................................................................;

  (defun _PutDXF ( e x r )
    (entmod (subst (cons x r) (assoc x e) e))
  )

  ;.................................................................................;

  (defun _Update ( e )
    (entupd (cdr (assoc -1 e)))
  )

  ;.................................................................................;

  (defun _Text ( tx ) (strcat "{\\fArial|b1|i0|c0|p34;" tx "}"))

  ;.................................................................................;

  (defun ss->lst ( ss / i ent lst )
    (setq i -1)
    (while (setq ent (ssname ss (setq i (1+ i))))
      (setq lst (cons (vlax-ename->vla-object ent) lst))
    )
    lst
  )

  ;.................................................................................;

  (defun _GetColour ( e / c )    
    (if (setq c (cdr (assoc 62 e)))      
      (cond
        (
          (cdr
            (assoc c
             '(
                (0 . "ByBlock")
                (1 . "Red")
                (2 . "Yellow")
                (3 . "Green")
                (4 . "Cyan")
                (5 . "Blue")
                (6 . "Magenta")
                (7 . "White")
              )
            )
          )
        )
        ( (itoa c) )
      )
      "ByLayer"
    )
  )

  ;.................................................................................;

  (defun _GetScale ( e )
    (vl-princ-to-string
      (mapcar
        (function
          (lambda ( x )
            (rtos (dxf x e) (getvar 'LUNITS) 2)
          )
        )
       '(41 42 43)
      )
    )
  )

  ;.................................................................................;

  (defun _GetName ( obj )
    (if (vlax-property-available-p obj 'EffectiveName)
      (vla-get-EffectiveName obj)
      (vla-get-name obj)
    )
  )

  ;.................................................................................;
  
  (defun _GetLW ( e / w )
    (if (setq w (cdr (assoc 370 e)))      
      (cond
        (
          (cdr
            (assoc w
             '(
                (-1 . "ByLayer")
                (-2 . "ByBlock")
                (-3 . "Default")
              )
            )
          )
        )
        ( (strcat (rtos (/ w 100.) 2 2) "mm") )
      )      
      "ByLayer"
    )
  )

  ;.................................................................................;

  (defun _Stringify ( data / typ )
    
    (setq data
      (cond
        ( (eq :vlax-true  data) "Yes" )
        ( (eq :vlax-false data) "No"  )
        ( data )
      )
    )
    
    (cond
      ( (eq 'STR (setq typ (type data)))

        data
      )
      ( (eq 'INT typ)

        (itoa data)
      )
      ( (eq 'REAL typ)

        (rtos data)
      )
      ( (vl-princ-to-string data) )
    )
  )

  ;.................................................................................;

  (defun _Display ( Cir Tx ss mode / cObj cSs iObj aStr iLst tStr cnt tStr )
    (setq cObj (vlax-ename->vla-object Cir) aStr "")
    
    (cond
      (
        (or (not ss) (= (sslength ss) 0))
      )
      ( (setq iObj
          (vl-some
            (function
              (lambda ( obj )
                (if (vlax-invoke obj 'IntersectWith cObj acExtendNone) obj)
              )
            )
            (ss->lst ss)
          )
        )

        (setq iLst (entget (vlax-vla-object->ename iObj)))
        (vla-put-Color cObj acRed)

        (cond
          (
            (zerop mode)

            (cond
              (
                (eq "INSERT" (dxf 0 iLst))

                (if (setq cSs (ssget "_X" (list (cons 0 "INSERT") (cons 2 (_GetName iObj)))))
                  (setq cnt (itoa (sslength cSs)))
                )

                (if (= 1 (dxf 66 iLst))
                  (progn
                    (setq aStr "\nATTRIBUTES: \n{\\fArial|b0|i0|c0|p34;")

                    (foreach x (vlax-invoke iObj 'GetAttributes)
                      (setq aStr (strcat aStr (vla-get-TagString x)  ":  " (vla-get-TextString x) "\n"))
                    )
                    (setq aStr (strcat aStr "}"))
                  )
                )

                (setq tStr
                  (strcat
                    "{\\C4;"         (dxf 0 iLst) "}"
                    "\nNAME:  "      (_GetName iObj)
                    (if cnt (strcat "\nINSTANCES:  " cnt) "")
                    "\nLAYER:  "     (dxf 8 iLst)
                    "\nCOLOR:  "     (_GetColour iLst)
                    "\nLINETYPE:  "  (vlax-get-property iObj 'Linetype)
                    "\nLINEWEIGHT:  "(_GetLW iLst)
                    "\nROTATION:  "  (angtos (dxf 50 iLst))
                    "\nSCALE:  "     (_GetScale iLst)
                    "\nDYNAMIC: "    (_Stringify (vlax-get-property iObj 'isDynamicBlock))
                    "\nXREF:  "      (_Stringify (vlax-get-property (vla-item (vla-get-Blocks doc) (_GetName iObj)) 'ISXREF))
                    aStr
                  )
                )
              )
              (t
                (setq tStr (strcat "{\\C4;" (dxf 0 iLst) "}"))

                (foreach prop
                 '(
                    LAYER
                    COLOR
                    LINETYPE
                    LINEWEIGHT
                    ALIGNMENT
                    ARCLENGTH
                    AREA
                    ATTACHMENTPOINT
                    CENTER
                    CIRCUMFERENCE
                    CLOSED
                    CUSTOMSCALE
                    DEGREE
                    DIAMETER
                    DISPLAYLOCKED
                    ELEVATION
                    HEIGHT
                    LENGTH
                    MEASUREMENT
                    OBLIQUEANGLE
                    RADIUS
                    ROTATION
                    SCALEFACTOR
                    STYLENAME
                    TEXTOVERRIDE
                    TEXTSTRING
                    TOTALANGLE
                    WIDTH
                  )

                  (setq tStr
                    (strcat tStr
                      (if
                        (and
                          (vlax-property-available-p iObj prop)
                          (not (eq "" (vlax-get iObj prop)))
                        )

                        (strcat "\n"
                          (strcase (vl-princ-to-string prop)) ":  "

                          (cond
                            (
                              (eq prop 'COLOR)

                              (_GetColour iLst)
                            )
                            (
                              (vl-position prop '(DISPLAYLOCKED CLOSED))

                              (_Stringify (vlax-get-property iObj prop))
                            )
                            (
                              (eq prop 'ALIGNMENT)

                              (cdr (assoc (vlax-get-property iObj prop) Alignment))
                            )
                            (
                              (eq prop 'ATTACHMENTPOINT)

                              (cdr (assoc (vlax-get-property iObj prop) Attachment))
                            )
                            (
                              (or
                                (and
                                  (eq 'MEASUREMENT prop)
                                  (vl-position (vla-get-ObjectName iObj) '("AcDb2LineAngularDimension" "AcDb3PointAngularDimension"))
                                )
                                (and
                                  (eq 'TOTALANGLE prop)
                                  (eq "AcDbArc" (vla-get-ObjectName iObj))
                                )
                                (eq 'ROTATION prop)
                              )

                              (angtos (vlax-get iObj prop))
                            )
                            (
                              (eq prop 'LINEWEIGHT)

                              (_GetLW iLst)
                            )
                            (
                              (_Stringify (vlax-get iObj prop))
                            )
                          )
                        )

                        ""
                      )
                    )
                  )
                )
              )
            )

            (_Update
              (_PutDxf
                (_PutDxf (entget tx) 62 251) 1 (_Text tStr)
              )
            )
            t
          )
        )
      )
      (t )
    )
    
    iObj
  )

  ;.................................................................................;

  (defun dxf ( code lst ) (cdr (assoc code lst)))

  ;.................................................................................;

  (defun PurgeLayer ( layer )
    (if
      (not
        (vl-catch-all-error-p
          (setq layer
            (vl-catch-all-apply 'vla-item
              (list
                (vla-get-layers
                  (vla-get-ActiveDocument (vlax-get-acad-object))
                )
                layer
              )
            )
          )
        )
      )      
      (vl-catch-all-apply 'vla-delete (list layer))
    )
  )

  ;.................................................................................;

  (defun RedrawSS ( ss mode )
    (
      (lambda ( i )
        (while (setq e (ssname ss (setq i (1+ i))))
          (redraw e mode)
        )
      )
      -1
    )
  )

  ;;-------------------------------------------------------------------------------;;
  ;;                           --=={  Main Function  }==--                         ;;
  ;;-------------------------------------------------------------------------------;;

  (setq cEnt
    (entmakex
      (list
        (cons 0 "CIRCLE")
        (cons 8 "LMAC_DINFO")
        (cons 10 (getvar 'VIEWCTR))
        (cons 40 (setq rad (/ (getvar 'VIEWSIZE) (float DInfo:cRad))))
        (cons 62 3)
      )
    )
    cELst (entget cEnt)
  )

  (setq tEnt
    (entmakex
      (list
        (cons 0 "MTEXT")                              
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbMText")
        (cons 8   "LMAC_DINFO")
        (cons 1  (_Text (nth DInfo:Mode ModeLst)))
        (cons 10 (getvar 'VIEWCTR))
        (cons 40 (/ (getvar 'VIEWSIZE) 60.0))
        (cons 50 0.0)
        (cons 62 71)
        (cons 71 1)
        (cons 90 3)
        (cons 63 256)
        (cons 45 1.2)
      )
    )
    tElst (entget tEnt) -pi/4 (/ pi -4.) pi/4 (/ pi 4.) 5pi/4 (/ (* 5 pi) 4.)
  )

  (setq msgLst
   '("\n[TAB Mode] [+/- Cursor Size] Move Cursor Over Objects to Retrieve Information..."
     "\n[TAB Mode] [+/- Cursor Size] Click Object to Isolate Layer, Shift+Click to Turn on All Layers..."
    )
  )
  
  (princ (setq msg (nth DInfo:Mode msgLst)))
  
  (while
    (progn
      (setq gr (grread 't 15 1) code (car gr) data (cadr gr) vs (getvar 'VIEWSIZE))

      (cond
        (
          (and (= 5 code) (listp data))

          (setq r (sqrt (* 2. rad rad)))

          (setq cEnt
            (_Update
              (setq cELst
                (_PutDxf (_PutDxf cELst 10 data) 40 (setq rad (/ vs (float DInfo:cRad))))
              )
            )
          )

          (setq tEnt
            (_Update
              (setq tELst
                (_PutDxf
                  (_PutDxf tELst 10 (polar (polar data -pi/4 rad) 0 (/ vs 90.0))) 40 (/ vs 60.0)
                )
              )
            )
          )

          (if (setq ss (ssget "_C" (polar data  pi/4 r) (polar data 5pi/4 r)))
            (progn

              (ssdel cEnt ss)
              (ssdel tEnt ss)

              (setq Inter (_Display cEnt tEnt ss DInfo:Mode))
            )
          )

          t
        )
        (
          (= 2 code)

          (cond
            (
              (vl-position data '(43 61))

              (if (> DInfo:cRad 1.0)
                (setq cEnt
                  (_Update
                    (setq cELst
                      (_PutDxf cELst 40
                        (setq rad (/ vs (float (setq DInfo:cRad (1- DInfo:cRad)))))
                      )
                    )
                  )
                )

                (princ (strcat "\n** Maximum Cursor Size Reached **" msg))
              )
            )
            (
              (= 45 data)

              (setq cEnt
                (_Update
                  (setq cELst
                    (_PutDxf cELst 40
                      (setq rad (/ vs (float (setq DInfo:cRad (1+ DInfo:cRad)))))
                    )
                  )
                )
              )
            )
            (
              (= 122 data)

              (setq DInfo:Mode (rem (1+ DInfo:Mode) 2))

              (setq tEnt
                (_Update
                  (setq tELst
                    (_PutDxf tELst 1 (_Text (nth DInfo:Mode ModeLst)))
                  )
                )
              )

              (princ (setq msg (nth DInfo:Mode msgLst)))
            )
            (
              (vl-position data '(13 32)) nil
            )
            ( t )
          )
        )
        (
          (and (= 3 code) (listp data) (= 1 DInfo:Mode))

          (if (and Express (acet-sys-shift-down))
            (mapcar
              (function
                (lambda ( x ) (vla-put-layeron x :vlax-true))
              )
              on
            )

            (if (and Inter (not (eq "LMAC_DINFO" (vla-get-layer Inter))))

              (mapcar
                (function
                  (lambda ( x )
                    (if (not (eq (strcase (vla-get-layer Inter)) (strcase (vla-get-name x))))
                      (vla-put-layeron x :vlax-false)
                    )
                  )
                )
                layers
              )
            )
          )

          t
        )
      )
    )
  )

  (mapcar 'entdel (list tEnt cEnt))
  (PurgeLayer "LMAC_DINFO")
  
  (princ)
)

;.................................................................................;

(princ)
(princ "\n:: DInfo.lsp | Version 1.5 | © Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"DInfo\" to Invoke ::")
(princ)

;;-------------------------------------------------------------------------------;;
;;                             End of Program Code                               ;;
;;-------------------------------------------------------------------------------;;



; =============================================================== ;
; snapang.lsp                                                     ;
; Ustawia zmienna SNAPANG do wskazanego obiektu                   ;
; by kojacek 2010,2018                                            ;
; =============================================================== ;
(defun C:ustawkursor (/ s e n d _rtd _gap)
  (defun _rtd (r)(* 180.0 (/ r pi)))
  (defun _gap (es / s p)
    (if
      (setq s (jk:ENT_GetLWPolySeg (car es)(cadr es)))
      (progn
        (setq p (jk:ENT_GetLWPolySegPoints (car es) s))
        (angle (car p)(cadr p))
      )
    )
  )
  (cd:SYS_UndoBegin)
  (if
    (setq s (entsel "\nSelect object: ") e (car s))
    (progn
      (setq d (entget e) n (cdr (assoc 0 d)))
      (setvar "SNAPANG"
        (cond
          ( (= "LINE" n)
            (angle (cdr (assoc 10 d))(cdr (assoc 11 d)))
          )
          ( (= "LWPOLYLINE" n)(_gap s))
          ( (member n
              '("TEXT" "MTEXT" "INSERT" "MINSERT"
                "ATTRIB" "ATTDEF"
              )
            )
            (cdr (assoc 50 d))
          )
          ( (member n '("RAY" "XLINE"))
            (angle '(0.0 0.0 0.0)(cdr (assoc 11 d)))
          )
          (t 0.0)
        )
      )
    )
    (setvar "SNAPANG" 0.0)
  )
  (cd:SYS_UndoEnd)
  (princ
    (strcat "\nSNAPANG = "
      (vl-princ-to-string (_rtd (getvar "SNAPANG")))
    )
  )
  (princ)
)
; =============================================================== ;
; zwraca numer segmentu obiektu LWPOLYLINE                        ;
;   en - ename lub vla-object LWPOLYLINE                          ;
;   pt - punkt wskazania                                          ;
; =============================================================== ;
(defun jk:ENT_GetLWPolySeg (en pt)
  (fix
    (vlax-curve-getParamAtPoint
      en
      (vlax-curve-getClosestPointTo en pt)
    )
  )
)
; =============================================================== ;
; Zwraca liste wspolrzednych segmentu SE dla LWPOLYLINE EN        ;
; =============================================================== ;
(defun jk:ENT_GetLWPolySegPoints (en se / v)
  (setq v (jk:ENT_LWPolySegs en nil))
  (if
    (< se v)
    (list
      (vlax-curve-getPointAtParam en se)
      (vlax-curve-getPointAtParam en (1+ se))
    )
  )
)
; =============================================================== ;
; zwraca ilosc segmentow LWPOLYLINE (gdy Mode=Nil) lub liste nume-;
; row segmentow: (0 1 2 ... N)                                    ;
; =============================================================== ;
(defun jk:ENT_LWPolySegs (en mode / v i l)
  (setq v (cdr (assoc 90 (entget en)))
        v (if
            (vlax-curve-isClosed en)
            v
            (1- v)
          )
  )
  (if Mode
    (progn
      (setq i 0)
      (while
        (< (length l)(1- v))
        (setq i (1+ i))
        (setq l (append (list i) l))
      )
      (cons 0 (reverse l))
    )
    v
  )
)

; =============================================================== ;
(princ)

; ------------------------------------------------------------------ ;
; Polecenie DELBKG dla wskazanych MTEXT-ow usuwa maske tla.          ;
; by kojacek - 2019                                                  ;
; ------------------------------------------------------------------ ;
(defun C:DELBKG (/ s l)
  (if
    (setq s (ssget '((0 . "MTEXT")(-4 . ">")(90 . 0))))
    (progn
      (setq l (cd:SSX_Convert s 0))
      (cd:SYS_UndoBegin)
      (foreach % l
        (setpropertyvalue % "BackgroundFill" 0)
      )
      (cd:SYS_UndoEnd)
    )
    (princ "\nNot selected objects.")
  )
  (princ)
)

; ------------------------------------------------------------------ ;
(princ)

(defun c:testlayer (/)
	;(print "okej 1")
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))
	(setq util (vla-get-Utility doc))
	(setq layers (vla-get-Layers doc))
	(setq layouts (vla-get-Layouts doc))
	(setq mspace (vla-get-ModelSpace doc))
	(setq pspace (vla-get-PaperSpace doc))
	(setq layer (getvar "clayer"))
	
	(vlax-for obj layouts
		;(print (vla-get-name obj))
		;(print (LM:dump obj))
		(setq pspace (vla-get-block obj)
          index 0)
		;(print pspace)
		(vlax-for obj1 pspace
			(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-get-name (list obj1))))
			(progn
				(if (equal (vla-get-name obj1) "Z$W@#498643CD")
					(print (vla-get-name obj))
				)
			)
			)
		)
	)
	
	;(vlax-for obj pspace
	;	(if (equal (vla-get-layer obj) layer)
	;		(print "ok")
	;	)
	;)

)

(defun c:googlemaps (/ acadObj doc mspace pspace p p1 p2 p3 p4 c w h vp obj url http str googlemaps )
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq mspace (vla-get-ModelSpace doc))  	
    (setq pspace (vla-get-PaperSpace doc))  	
	(setq p (getpoint "Podaj punkt na mapie:"))
	
	(if (and 
			(/= "Model" (getvar "CTAB")) 
			(= 0 (getvar "TILEMODE"))
			(= 1 (getvar "CVPORT"))
		)
	(progn	
		;print "PAPER STATE")
		(vlax-for obj pspace 
			(if ;(and
					(equal (vla-get-objectname obj) "AcDbViewport")
				;	(equal vp nil)
				;)
			(progn
				(setq c (vlax-safearray->list (vlax-variant-value (vla-get-center obj))))
				(setq w (/ (vla-get-Width obj) 2))
				(setq h (/ (vla-get-Height obj) 2))
				(setq   p1 (- (car c) w)
						p2 (+ (cadr c) h)
						p3 (+ (car c) w)
						p4 (- (cadr c) h)
				)
				(if (and
					(> (car p) p1)
					(< (cadr p) p2)
					(< (car p) p3)
					(> (cadr p) p4)
					)
				(progn
					(setq vp obj)
				)			
				)
			)
			)
		)
		;(vla-delete vp)
		(setq p (PCS2WCS p vp))
		(setq p (list (car p) (cadr p) 0.0))
	)
	)
	
	(setq x (car p))
	(setq y (cadr p))
	;(setq url (strcat "https://epsg.io/trans?x=" (rtos x 2 5) "&y=" (rtos y 2 5) "&s_srs=2179&t_srs=2180"))
	(setq url (strcat "https://epsg.io/trans?x=" (rtos x 2 5) "&y=" (rtos y 2 5) "&s_srs=2179&t_srs=4326"))
	(setq http (vlax-get-or-create-object "MSXML2.XMLHTTP.3.0"))
	(vlax-invoke-method http 'open "GET" url :vlax-false)
	(vlax-invoke-method http 'send)
	(setq str (vlax-get-property http 'responsetext))
	(print str)
	
	;2180
	;(setq x (substr str 8 9))
	;(setq y (substr str 26 9))
	;4326
	;(setq x (substr str 8 11))
	;(setq y (substr str 28 11))
	(setq y (substr str 7 9))
	(setq x (substr str 25 9))
	
	(setq googlemaps (strcat "https://www.google.pl/maps/@" x "," y ",20z"))
	(vl-cmdf "_browser" googlemaps "")
	
)

(defun c:KW ( / acadObj doc mspace pspace nazwaarkusza kw url a b c )
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq mspace (vla-get-ModelSpace doc)) 
    (setq pspace (vla-get-PaperSpace doc)) 
	
	(setq nazwaarkusza (vla-get-name (vla-get-activelayout doc)))
	(setq kw nil)
	(vlax-for obj pspace
		(if (and 
			;(/= "Model" (cdr (assoc 410 ed)))
			(equal "dane" (LM:effectivename obj))
			)
			(progn
				(setq kw (cd:BLK_GetAttValueVLA obj "KW"))
			)
		)
	)
	(print "PSpace ojk")
	(print kw)
	(cond
		((or (not kw) (vl-string-search "-" kw ) (vl-string-search  " " kw) )
			(setq url (strcat "https://przegladarka-ekw.ms.gov.pl/eukw_prz/KsiegiWieczyste/wyszukiwanieKW" ))
		)
		((and (wcmatch nazwaarkusza "Umowa*") (vl-string-search "/" kw)) 
			
				(setq a (substr kw 1 4))
				(setq b (substr kw 6 8))
				(setq c (substr kw 15 ))
				(setq url (strcat "https://przegladarka-ekw.ms.gov.pl/eukw_prz/KsiegiWieczyste/wyszukiwanieKW?&kodWydzialu=" a "&numerKW=" b "&cyfraKontrolna=" c ))
				
		)
	)
	;(setq url (strcat "https://ekw.ms.gov.pl/eukw_ogol/menu.do" ))
	
	;(setq url (strcat "https://przegladarka-ekw.ms.gov.pl/eukw_prz/KsiegiWieczyste/wyszukiwanieKW?komunikaty=true&kontakt=true&okienkoSerwisowe=false" ))
	
	
	
	(vl-cmdf "_browser" url "")
	
)


(defun c:emapa (/ acadObj doc mspace pspace p p1 p2 p3 p4 c w h vp obj p x y url http str emapa)
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq mspace (vla-get-ModelSpace doc))  	
    (setq pspace (vla-get-PaperSpace doc))  	
	(setq p (getpoint "Podaj punkt na mapie:"))
	
	(if (and 
			(/= "Model" (getvar "CTAB")) 
			(= 0 (getvar "TILEMODE"))
			(= 1 (getvar "CVPORT"))
		)
	(progn	
		;print "PAPER STATE")
		(vlax-for obj pspace 
			(if ;(and
					(equal (vla-get-objectname obj) "AcDbViewport")
				;	(equal vp nil)
				;)
			(progn
				(setq c (vlax-safearray->list (vlax-variant-value (vla-get-center obj))))
				(setq w (/ (vla-get-Width obj) 2))
				(setq h (/ (vla-get-Height obj) 2))
				(setq   p1 (- (car c) w)
						p2 (+ (cadr c) h)
						p3 (+ (car c) w)
						p4 (- (cadr c) h)
				)
				(if (and
					(> (car p) p1)
					(< (cadr p) p2)
					(< (car p) p3)
					(> (cadr p) p4)
					)
				(progn
					(setq vp obj)
				)			
				)
			)
			)
		)
		;(vla-delete vp)
		(setq p (PCS2WCS p vp))
		(setq p (list (car p) (cadr p) 0.0))
	)
	)
	
	(setq x (car p))
	(setq y (cadr p))
	;(setq url (strcat "https://epsg.io/trans?x=" (rtos x 2 5) "&y=" (rtos y 2 5) "&s_srs=2179&t_srs=2180"))
	(setq url (strcat "https://epsg.io/trans?x=" (rtos x 2 5) "&y=" (rtos y 2 5) "&s_srs=2179&t_srs=2180"))
	(setq http (vlax-get-or-create-object "MSXML2.XMLHTTP.3.0"))
	(vlax-invoke-method http 'open "GET" url :vlax-false)
	(vlax-invoke-method http 'send)
	(setq str (vlax-get-property http 'responsetext))
	(print str)
	;2180
	;(setq y (substr str 8 9))
	;(setq x (substr str 26 9))
	;2180
	(setq x (substr str 7 9))
	(setq y (substr str 23 9))
	;4326
	;(setq x (substr str 8 11))
	;(setq y (substr str 28 11))
	(print x)
	(print y)
	
	(setq emapa (strcat "http://polska.e-mapa.net/?x=" x "&" "y=" y ))
	(vl-cmdf "_browser" emapa "")
	
)

(defun c:daneedit (/ acadObj doc mspace pspace p p1 p2 p3 p4 c w h vp obj p x y url http str emapa)
	
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    (setq mspace (vla-get-ModelSpace doc))  	
    (setq pspace (vla-get-PaperSpace doc))  	
	
	(vlax-for obj mspace 
		(if (and
				(equal (vla-get-ObjectName obj) "AcDbBlockReference")
				(equal (vla-get-Name obj) "dane")
				
			)
		(progn
			(setq obj1 obj)
		)
		)
	)
	(vl-cmdf "_eattedit" (vlax-vla-object->ename obj1) "")
	
)

(defun C:sluzebnosc ( / layer osmode dist1 disp1 disp2 test eos1 objlayer ptlist1 trys plist1 i pt vv v rys flag arclist war pt1 pt2 ptlist tmp myobj a e p o flag obj ptlistarc arclist)
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(defun *error* ( msg / )
		;(if (not (null msg ) )         
		;(progn 
			(vla-put-activeLayer doc objlayer)
			(princ "\n Error rura " ) 
			(print msg)
			(cd:SYS_UndoEnd)
		;;)
	)
	
	(setq layer (getvar "clayer"))
	(setq osmode (getvar "OSMODE"))
	(setq objlayer (vla-get-activeLayer doc))
	;(print (strcat "Layer set:" ( itoa (LM:LayerSet "0 schemat" 2 "CONTINUOUS" acLnWt030))))
	(print (LM:LayerSet "0 SLUZEBNOSC" acBlue "CONTINUOUS" acLnWt020))
	(defun GetActiveSpace ( / ) 
		(LM:activespace 'doc 'spc)
		spc
	)
	
	(defun cd:phatch(obj / patternName hatchObj outerLoop)
		(setq patternName "ANSI31"
		patternType 0
		bAssociativity :vlax-true)
		(setq hatchObj (vla-AddHatch mspace patternType patternName bAssociativity acHatchObject))
		(vlax-put-property hatchObj "PatternScale" 0.5)
		(vlax-put-property hatchObj "PatternSpace" 0.5)
		(setq outerLoop (vlax-make-safearray vlax-vbObject '(0 . 0)))
		(vlax-safearray-put-element outerLoop 0 obj)
		(vla-AppendOuterLoop hatchObj outerLoop)
		(vla-Evaluate hatchObj)
	)

	
	(defun LM:sgettext (pplist1 / )
		(setq ssobj (ssget "_WP" pplist1 '( (-4 . "<OR")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>") (8 . "211-Nr dzia³ki"))))
		(if ssobj
		(progn
			(foreach obj1 (cd:SSX_Convert ssobj 1)
				(setq textstring1 (vla-get-textString obj1))
			)
		)
		(progn
			(setq ssobj (ssget "_CP" pplist1 '((-4 . "<OR")(0 . "TEXT")(0 . "MTEXT")(-4 . "OR>")(8 . "211-Nr dzia³ki"))))
			(if ssobj
			(progn
				(foreach obj1 (cd:SSX_Convert ssobj 1)
					(setq textstring1 (vla-get-textString obj1))
				)
			)
			)
		)
		)
		textstring1
	)
	
    (defun LM:selectpolygon ( olist / pplist pP pE )
		(setq pplist (list))
		(setq pP (vlax-curve-getStartPoint (car olist)))
		(setq pE (vlax-curve-getEndPoint (car olist)))
		(setq pplist (cons pP pplist))
		(setq pplist (cons pE pplist))
		(setq olist (vl-remove (car olist) olist))
		(while (< 0 (length olist))
			(foreach obj olist
				(setq pP (vlax-curve-getStartPoint obj))
				(setq pE (vlax-curve-getEndPoint obj))
				(cond
				((equal (car pplist) pP)	
					(setq pplist (cons pE pplist))
					(setq olist (vl-remove obj olist))
				)
				((equal (car pplist) pE)
					(setq pplist (cons pP pplist))
					(setq olist (vl-remove obj olist))
				)
				((equal (last pplist) pP)
					(setq pplist (reverse (cons pE (reverse pplist))))
					(setq olist (vl-remove obj olist))
				)
				((equal (last pplist) pE)
					(setq pplist (reverse (cons pP (reverse pplist))))
					(setq olist (vl-remove obj olist))
				)
				)
			)
		)
		pplist
	)
	
	(defun LM:Unique ( l )
		(if l (cons (car l) (LM:Unique (vl-remove (car l) (cdr l)))))
	)
	
	(defun LM:selectnext (Point / p1 p2 p3 p4 c ret ss)
		(setq c 0.1)
		(setq p1 (polar Point (* 0.25 pi) c))
		(setq p2 (polar Point (* 0.75 pi) c))
		(setq p3 (polar Point (* 1.25 pi) c))
		(setq p4 (polar Point (* 1.75 pi) c))
		(setq ss (ssget "_CP" (list p1 p2 p3 p4) '((0 . "POLYLINE") (8 . "219-Linia granicy"))))
		(setq ret (cd:SSX_Convert ss 1))
		ret
	)
	
	(defun LM:selectall (obj strona / i j objlist test koniec objtemp pE pP pPN pEN pPP pP1 pE1 angpP angpP1 angpP1t deltaang tempobj1  tempdeltaang1 fdeg )
		(setq fdeg 6.28318531)
		(setq test nil)
		(setq koniec nil)
		(setq objtemp obj)
		(setq objlist (list))
		(setq objlist (cons obj objlist))
		(setq i 0)
		(setq j 0)
		
		(while (not test)
			;(print i)
			(cond 
				((= i 0)
					(setq objtemp obj)
					(if koniec
					(progn
						(setq pE (vlax-curve-getStartPoint objtemp))
						(setq pP (vlax-curve-getEndPoint objtemp))
					)
					(progn
						(setq pP (vlax-curve-getStartPoint objtemp))
						(setq pE (vlax-curve-getEndPoint objtemp))
					)
					)
				)
				((>= i 1)
					(setq objtemp tempobj1)
					(setq pPN (vlax-curve-getStartPoint objtemp))
					(setq pEN (vlax-curve-getEndPoint objtemp))
					(if (> (distance pP pPN) 0.1)
					(progn
						(setq 	pE pEN 
								pP pPN)
					);progn
					(progn
						(setq 	pE pPN 
								pP pEN)
					);progn
					);if
				)
			)
			(setq pPP (LM:selectnext pP))
			(setq angpP (angle pP pE))

			(if strona
				(setq tempdeltaang1 0)
				(setq tempdeltaang1 20)
			)
			(foreach obj1 pPP
				(if (/= objtemp obj1)
				(progn
					(setq pP1 (vlax-curve-getStartPoint obj1))
					(setq pE1 (vlax-curve-getEndPoint obj1))
					(if (> (distance pP pP1) 0.1)
					(progn
						(setq Temp pE1 
								pE1 pP1 
								pP1 Temp)
					);progn
					);if
					(setq angpP1 (angle pP1 pE1))
					(setq angpP1t angpP1)
					(setq deltaang (- angpP1 angpP))
					
					(if (minusp deltaang)
						(setq angpP1t (+ fdeg deltaang))
						(setq angpP1t deltaang)
					);if
					(if strona
					(progn
						(if (<= (abs tempdeltaang1) (abs angpP1t))
						(progn
							(setq tempdeltaang1 angpP1t)
							;(setq tempdeltaanga1 angpP1)
							(setq tempobj1 obj1)
							
						);progn
						);if
					)
					(progn
						(if (>= (abs tempdeltaang1) (abs angpP1t))
						(progn
							(setq tempdeltaang1 angpP1t)
							;(setq tempdeltaanga1 angpP1)
							(setq tempobj1 obj1)
							
						);progn
						);if
					)
					)
					
				);progn
				);if
			);foreach
			(if (or
					(> i 100)
					(vl-position tempobj1 objlist)
				)
			(progn
				(setq test t)
			)
			(progn
				(setq objlist (cons tempobj1 objlist))
			)
			)
			(setq i (1+ i))
			(if (equal (length pPP) 1)
			(progn
				(setq i 0)
				(setq koniec t)
				(cond
					((equal strona nil)
						(setq strona t)
					)
					((equal strona t)
						(setq strona nil)
					)
				)
				(setq test nil)
				(setq j (1+ j))
			)
			)
			(if (>= j 2 )
				(setq test t)
			)
		);while
		objlist
	);(LM:selectall obj wprawo)
	
	(defun LM:zbierzpunkty (/)
		
		(setq pt (getpoint "\nWprowadz punkt startowy: "))
		(setq i 1)
		(setq ptlist1 (cons (list (car pt)(cadr pt)) ptlist1))
		(while 
				(and 
					(if (equal trys "Automatyczne")
						(< i 2)
						(>= i 1)
					)
					(if (equal trys "Automatyczne")
						(setq pt (getpoint "\nWprowadz punkt koñcowy lub [Enter]: " pt))
						(setq pt (getpoint "\nWprowadz kolejny punkt lub zakoñcz [Enter]: " pt))
					)
				)
			(setq ptlist1 (cons (list (car pt)(cadr pt)) ptlist1))
			(setq i (1+ i))
		)
	)
	
	
	(defun LM:rysuj (/)
		(if (equal (length (car ptlist1)) 2)
		(progn
			(setq ptlist ptlist1)
			;(setq ptlistarc ptlist1)
		)
		(progn
			(foreach % ptlist1
				(setq ptlist (cons (list (car %)(cadr %)) ptlist))
			)
			;(setq  ptlistarc ptlist)
		)
		)

		(setq i (length ptlist))
		(setq ptlist (apply 'append ptlist))
		
		(if (= (rem (length ptlist) 2) 0)
		(progn
			(setq tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1))))
			(vlax-safearray-fill tmp ptlist)
			(setq myobj (vla-AddLightWeightPolyline (GetActiveSpace) tmp))
			;(vla-put-Color myobj acBlue)
			;(vla-put-Linetype myobj "Continuous")

			(vla-offset myobj (/ v 200.0))
			(setq eos1 (entlast))
			(setq os1 (vlax-ename->vla-object eos1))
			(vla-offset myobj (* (/ v 200.0) -1))
			(setq eos2 (entlast))
			(setq os2 (vlax-ename->vla-object eos2))
			(vla-delete myobj)
			(setq os1p1 (vlax-curve-getstartpoint os1)
				  os1p2 (vlax-curve-getendpoint os1)
				  os2p1 (vlax-curve-getstartpoint os2)
				  os2p2 (vlax-curve-getendpoint os2))
				  
			;(vla-put-Lineweight os1 acLnWt035)
			;(vla-put-Lineweight os2 acLnWt035)
			(setq obj1 (cd:ACX_AddLWPolyline (GetActiveSpace) (list os1p1 os2p1) nil))
			(setq obj2 (cd:ACX_AddLWPolyline (GetActiveSpace) (list os1p2 os2p2) nil))
			(setq eobj1 (vlax-vla-object->ename obj1))
			(setq eobj2 (vlax-vla-object->ename obj2))
			(vl-cmdf "_.join" eos1 eos2 eobj1 eobj2 "")
			(setq o (entlast))
			(setq oo (vlax-ename->vla-object o))
			(vla-put-Closed oo :vlax-true)
			(cd:phatch oo)
		)
		(princ "\nerror: Polilinia nie moze byc stworzona")
		)
	
	)
	
	;(setq pt1 (getpoint "\nWprowadz punkt startowy: "))
	;(setq pt2 (getpoint "\nWprowadz punkt koñcowy: "))
	(setq arclist (list))
	(setq flag 0)
	(setq war 0)
	
	(setq trys (cd:USR_GetKeyWord "\nRysowanie: " '("Automatyczne" "Manualne" "Pole" "Tabelka") "Automatyczne"))
	
	(cond 
		((equal trys "Automatyczne")
			(setq vv (cd:USR_GetKeyWord "\nWprowadz szerokosc sluzebnosci pasa" '("25" "50" "100" "200") "50") )
			(setq v (atof vv))
			(LM:zbierzpunkty)
			;(setq pt1 (car pt))
			;(setq pt2 (cadr pt)) 
			;(setq ptlist1 (list (list (car pt1)(cadr pt1))  (list (car pt2)(cadr pt2))))
	
			(setq p (car ptlist1))
			(if (and 
					(setq o (osnap p "_nea"))
					(setq e (car (nentselp o)))

				)
			(progn
				(setq o (osnap p "_nea"))
				(setq e (car (nentselp o)))
				(setq obj (vlax-ename->vla-object e))
			)
			(progn
				(setq p (cadr ptlist1))
				(setq o (osnap p "_nea"))
				(setq e (car (nentselp o)))
				(setq obj (vlax-ename->vla-object e))
			)
			)
			
			(setq ptlist1 (subst (vlax-curve-getClosestPointTo obj (car ptlist1)) (car ptlist1) ptlist1))
			(if (equal (length ptlist1) 2)
				(setq ptlist1 (subst (vlax-curve-getClosestPointTo obj (cadr ptlist1)) (cadr ptlist1) ptlist1))
			)
			(setq test (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obj))))
			(if (= 1 (length ptlist1))
				(setq ptlist1 (list (list (car test) (cadr test)) (list (cadr (reverse test)) (car (reverse test)))))
			)
			(setq i 0)
			(setq p1 (car ptlist1))
			(setq p2 (cadr ptlist1))
			(while (< i (length test))
				(setq test1 (list (nth i test) (nth (+ 1 i) test)))
				(setq dist1 (vlax-curve-getDistAtPoint obj test1))
				(setq disp1 (vlax-curve-getDistAtPoint obj p1))
				(setq disp2 (vlax-curve-getDistAtPoint obj p2))
				(if (and 
						(> dist1 disp1 )
						(< dist1 disp2 )
					)
				(progn
					(setq ptlist1 (subst test1 (setq olditem (last ptlist1)) ptlist1))
					(setq ptlist1 (append ptlist1 (list olditem)))
					
				)
				)
				(if (and 
						(< dist1 disp1 )
						(> dist1 disp2 )
					)
				(progn
					(setq ptlist1 (vl-remove (setq olditem (car ptlist1)) ptlist1))
					(setq ptlist1 (cons test1 ptlist1))
					(setq ptlist1 (cons olditem ptlist1))
				)
				)
			(setq i (+ i 2))
			)
			(LM:rysuj)
		)
		((equal trys "Manualne")
			(setq vv (cd:USR_GetKeyWord "\nWprowadz szerokosc sluzebnosci" '("25" "50" "100" "200") "50") )
			(setq v (atof vv))
			(LM:zbierzpunkty)
			(LM:rysuj)
		)
		
		((equal trys "Pole")
			(LM:zbierzpunkty)
			(setq p (car ptlist1))
			(if (and 
					(setq o (osnap p "_nea"))
					(setq e (car (nentselp o)))
				)
			(progn
				(setq o (osnap p "_nea"))
				(setq e (car (nentselp o)))
				(setq obj (vlax-ename->vla-object e))
			)
			(progn
				(setq p (last ptlist1))
				(setq o (osnap p "_nea"))
				(setq e (car (nentselp o)))
				(setq obj (vlax-ename->vla-object e))
			)
			)
			(print obj)
			
			(if (equal (length (car ptlist1)) 2)
			(progn
				(setq ptlist ptlist1)
				;(setq ptlistarc ptlist1)
			)
			(progn
				(foreach % ptlist1
					(setq ptlist (cons (list (car %)(cadr %)) ptlist))
				)
				;(setq  ptlistarc ptlist)
			)
			)
			(setq i (length ptlist))
			(setq ptlist (apply 'append ptlist))
			
			(if (= (rem (length ptlist) 2) 0)
			(progn
				(setq tmp (vlax-make-safearray vlax-vbDouble (cons 0 (- (length ptlist) 1))))
				(vlax-safearray-fill tmp ptlist)
				(setq myobj (vla-AddLightWeightPolyline (GetActiveSpace) tmp))
				(vla-put-Closed myobj :vlax-true)
				
				(cd:phatch myobj)
			)
			(progn 
				(princ "\nerror: Polilinia nie moze byc stworzona")
			)
			)
		)
		((equal trys "Zrob sie samo :D")
			(alert "Naprawde siê komuœ pojebawszy")
		)
		((equal trys "Tabelka")
			(setq dzialeczka (getstring "Podaj dzia³kê:"))
			
			(setq sget (car (cd:SSX_Convert (ssget "_X" (list (cons 1 dzialeczka) (cons -4  "<OR") (cons 0  "TEXT") (cons 0  "MTEXT") (cons -4  "OR>") (cons -4  "<OR") (cons 8 "Dzialki") (cons 8 "211-Nr dzia³ki") (cons -4  "OR>"))) 1)))
			(setq inssget (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint sget))))
			;(print inssget)
			(setq sear nil)
			(setq i 0)
			(setq ang 0)
			(while (not sear) 
				(setq i (+ i 0.01))
				(setq ang (+ ang 0.01))
				(setq p (polar inssget ang i))
				(setq sear (ssget "_F" (list inssget p) (list (cons 0  "POLYLINE") (cons -4  "<OR") (cons 8 "Dzialki") (cons 8 "219-Linia granicy") (cons -4  "OR>"))))
				(if (> i 314) (setq sear t))
			)
			(setq sear (car (cd:SSX_Convert sear 1)))
			(setq olist1 (LM:selectall sear t))
			(setq olist2 (LM:selectall sear nil))
			(setq olist1 (vl-remove nil olist1))
			(setq olist2 (vl-remove nil olist2))
			(setq pplist1 (LM:selectpolygon olist1))
			(setq pplist2 (LM:selectpolygon olist2))
			(cond 
				((equal dzialeczka (LM:sgettext pplist1))
					(setq pplist pplist1)
				)
				((equal dzialeczka (LM:sgettext pplist2))
					(setq pplist pplist2)
				)
				(t
					(alert "ERRROROROROROORR XD")
				)
			)
			;(print pplist)
			(setq obrysdzialki (cd:ACX_AddLWPolyline (cd:ACX_ASpace) pplist :vlax-true))  
			(setq areacala (vla-get-area obrysdzialki))
			;(print areacala)
			(vl-cmdf "_offset" "0.2" (vlax-vla-object->ename obrysdzialki) inssget "")  
			(setq obrysdzialkiOffset (vlax-ename->vla-object (entlast)))
			;(vla-delete obrysdzialki)
			; (print (vla-get-Coordinates obrysdzialkiOffset))
			(setq cord (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates obrysdzialkiOffset))))
			;(print cord)
			(setq i 0)
			(setq ptlist1 (list))
			
			(while (< i (length cord))
					(setq test1 (list (nth i cord) (nth (+ 1 i) cord)))
					(setq ptlist1 (cons test1 ptlist1))
					
				(setq i (+ i 2))
			)
			(vla-delete obrysdzialki)
			(vla-delete obrysdzialkiOffset)
			(print ptlist1)
			(setq ss (ssget "_CP" ptlist1 '( (8 . "0 SLUZEBNOSC")(-4 . "<OR")(0 . "LWPOLYLINE")(0 . "POLYLINE")(-4 . "OR>"))))
			(setq ssobj (cd:SSX_Convert ss 1))
			(print ssobj)
			(setq areasluzebnosc 0)
			(foreach o ssobj
				(setq areasluzebnosc (+ areasluzebnosc (vla-get-area o)))
			)
			;(vl-cmdf "_AREASUM" ss "")
			
			(print "Arena cala (Metry): ")
			; (print (/ areacala 100.0))
			(print areacala)
			(print "Arena sluzebnosc (Metry): ")
			; (print (/ areasluzebnosc 100.0))
			(print areasluzebnosc)
			
			(princ)
		)
		(T
			(print "B³¹d")
		)
	)
	
	(cd:SYS_UndoEnd)
	(vla-put-activeLayer doc objlayer)

)

(defun c:klon ( / file sh App Params)
	;(startapp "zwcad"  (strcat (getvar "dwgprefix")(getvar "dwgname")))
	;(vl-cmdf "_DELAY" "5000")
	;(startapp "explorer" (strcat (getvar "dwgprefix")(getvar "dwgname")))
	(setq file (strcat (getvar "dwgprefix")(getvar "dwgname")))
	(setq sh (vlax-create-object "Shell.Application"))
	(setq App "C:\\Program Files\\ZWSOFT\\ZWCAD 2018\\ZWCAD.exe" )
	(setq Params (strcat "/company ZWSoft /product ZWCAD /language \"pl-PL\" \"" file "\""))
	(vlax-invoke-method sh 'ShellExecute App Params )
	(vlax-release-object sh)

)

(defun c:obl ( / file sh App Params)
	;(startapp "zwcad"  (strcat (getvar "dwgprefix")(getvar "dwgname")))
	;(vl-cmdf "_DELAY" "5000")
	;(startapp "explorer" (strcat (getvar "dwgprefix")(getvar "dwgname")))
	(setq dwgprefix (getvar "dwgprefix"))
	(setq dwgname (getvar "dwgname"))
	; (print (strcat dwgprefix "obl.tbl"))
	; (print (vl-file-systime (strcat dwgprefix "obl.obl")))
	(setq findok (findfile (strcat dwgprefix "obl.tbl")))
	;(setq obel "Z:\\AZART projektowanie\\PROJEKTY\\KLOCKI 2020 IV\\#MATERIA£Y\\obl.tbl")
	(setq obel "Z:\\AZART projektowanie\\PROJEKTY\\KLOCKI 2020 IV\\#MATERIA£Y\\obl.tbl")
	(if findok
	(progn
		;(setq obel "Z:\\AZART projektowanie\\PROJEKTY\\KLOCKI 2020 IV\\#MATERIA£Y\\obl.tbl")
	
		(setq sh (vlax-create-object "Shell.Application"))
		;(setq App "C:\\Program Files (x86)\\obl2012\\obl2012.exe" )
		(setq App "C:\Program Files (x86)\oblX\\oblXu.exe" )
		(setq Params (strcat dwgprefix "obl.tbl"))
		(vlax-invoke-method sh 'ShellExecute App Params )
		(vlax-release-object sh)
	)
	(progn
		(alert "Uwaga!! \nNowy plik obelusa")
		(vl-file-copy obel (strcat dwgprefix "obl.tbl"))
		(setq sh (vlax-create-object "Shell.Application"))
		;(setq App "C:\\Program Files (x86)\\obl2012\\obl2012.exe" )
		(setq App "C:\Program Files (x86)\oblX\\oblXu.exe" )
		(setq Params (strcat dwgprefix "obl.tbl"))
		(vlax-invoke-method sh 'ShellExecute App Params )
		(vlax-release-object sh)
	)
	)
	

)

(defun C:szukajarkusz ( / mspace pspace li idodac dcl des dch str)
	(setq mspace (vla-get-modelspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	(setq pspace (vla-get-paperspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	
	
	;(cd:SYS_UndoBegin)
	
	(setq li (list))
	(setq li (mapcar 'car (cd:DWG_LayoutsList)))
	(setq lli (list))
	;(print nazwaarkusza)
	
	(defun DCL_Wybierz( / )
		;(set_tile $key $value)
		
		(setq idodac (atoi $value))
		(if (not (equal idodac -1)) 
		(progn
			(term_dialog)
			;(unload_dialog "test")
			(princ "\n*Cancel*")
			(setq pli (nth idodac li))
			(print pli)
			(command "ARKUSZ" "U" pli )
		)
		)
	);DCL_dodac
	
	(defun DCL_Sortuj( / )
		;(set_tile $key $value)
		(print "Sort ok")
		(setq li (sort li '<))
		(LM:DCL_WriteToList li "li" )
		
	);DCL_dodac
	(defun DCL_Szukajka( / v xli)
		;(set_tile $key $value)
		(print $value)
		(if (wcmatch $value "*/*")
			(setq v (vl-string-subst "-" "/" $value))
			(setq v $value)
					
		)
		(setq li (list))
		(if (equal v "")
		(progn
			(setq li (mapcar 'car (cd:DWG_LayoutsList)))
			(LM:DCL_WriteToList li "li" )
		)
		(progn
			(foreach xli (mapcar 'car (cd:DWG_LayoutsList)) 
				(if (wcmatch (strcase xli) (strcat "*" (strcase v ) "*"))
				(progn
					(setq li (cons xli li))
				)
				)
			)
			(LM:DCL_WriteToList li "li" )
		)
		)
		
		;(setq idodac (atoi $value))
		;(term_dialog)
		;(unload_dialog "test")
		;(princ "\n*Cancel*")
		;(setq pli (nth idodac li))
		;(command "ARKUSZ" "U" pli )
		
	);DCL_dodac
	
	
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
				"    label = \"Lista Arkuszy\";"
				"    spacer;"
				"    : column"
				"    {" 
				"		 : text {label = \"Podaj tekst do szukajki(ENTER): \";}"
				"        : edit_box {action = \"(DCL_Szukajka)\"; value = \"\"; }"
				"        : lbx { key = \"li\"; action = \"(DCL_Wybierz)\"; label = \"Podaj arkusz\"; }"
				"    	: row"
				"	 	{"
				"		 	 : button { label = \"Sortuj\"; key = \"Sortuj\";  action = \"(DCL_Sortuj)\"; }	"
				"			 : button { label = \"Anuluj\"; key = \"Anuluj\"; is_cancel = true ;}	"
				"    	}"
				"    }"
				"    spacer;"
				;"    ok_cancel;"
				"}"
			)
			(write-line str des)
		)
		(not (setq des (close des)))
		(< 0 (setq dch (load_dialog dcl)))
		(new_dialog "test" dch)
	)
	(progn           
		(LM:DCL_WriteToList li "li" )
		
		(cond 
			((= 1 (start_dialog))
				(term_dialog)
				(princ "\n*Cancel*")
			)
			((= 0 what_next)
				(term_dialog)
				;(unload_dialog "test")
				(princ "\n*Cancel*")
			)
		);cond
	
	)
	)
	
	;(CD:SYS_UndoEnd)
	;(command "_regen" nil)
	;(setvar "CMDECHO" 1)
	nil

)

(defun C:szukajdzialki ( / acadObj mspace pspace li idodac dcl des dch str)
	
	(setq acadObj (vlax-get-acad-object))
	(setq mspace (vla-get-modelspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	(setq pspace (vla-get-paperspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	
	
	;(cd:SYS_UndoBegin)
	
	(setq li (list))
	;(foreach x (cd:DWG_LayoutsList)
	;	(setq li (cons (car x) li))
	;)
	(print "OK1")
	(defun cd:DWG_D ( / d end)
		(setq d (cd:SSX_Convert (ssget "_A" (list (cons -4  "<OR") (cons 8 "Dzialki") (cons 8 "211-Nr dzia³ki") (cons -4  "OR>"))) 1))
		(setq end '())
		(foreach s (reverse d)
			(if (vlax-property-available-p s "TextString" )
				(setq end (cons (list (vla-get-TextString s) (vlax-safearray->list ( vlax-variant-value ( vla-get-InsertionPoint s)))) end))
			)
		)
		;(print d)
		;(setq end (mapcar 'vla-get-TextString d))
		end
	)
	
	
	;(print "OK11")
	(setq liall (cd:DWG_D))
	(setq li (mapcar 'car liall))
	
	;(print "OK2")
	
	;(print li)
	;(print (mapcar 'car li) )
	
	(defun DCL_Wybierz( / )
		;(set_tile $key $value)
		
		(setq idodac (atoi $value))
		(if (not (equal idodac -1)) 
		(progn
			(term_dialog)
			;(unload_dialog "test")
			(princ "\n*Cancel*")
			(setq pli (nth idodac li))
			(print "li")
			(print li)
			(print "idodac")
			(print idodac)
			(print "pli")
			(print pli)
			; (print "liall")
			; (print liall)
			(setq wyst -1)
			(setq i 0)
			
			(while (<= i idodac)
				(if (equal pli (nth i li))
					(setq wyst (1+ wyst))
				)
				(setq i (1+ i))
			)
			
			(setq wierz nil )
			(while
				(setq w (assoc pli liall)) ; odczytujemy pierwszy w
				(setq wierz (append wierz (list (cdr w))))        ; dodajemy go do listy wynikowej
				(setq liall (cdr (member w liall) ) )    ; "obcinamy" pocz¹tek listy za wyszukany element, by kolejny by³ wyszukany jako pierwszy.
			)
			(print wierz)

			; (setq zcenter (vlax-3d-point (cadr (assoc pli liall)))
			(setq zcenter (vlax-3d-point (car (nth wyst wierz)))
				   magnification 20)
			(vla-ZoomCenter acadObj zcenter magnification)
			;(command "ARKUSZ" "U" pli )
		)
		)
	);DCL_dodac
	
	(defun DCL_Sortuj( / )
		;(set_tile $key $value)
		(print "Sort ok")
		(setq li (sort li '<))
		(LM:DCL_WriteToList li "li" )
		
	);DCL_dodac
	
	(defun DCL_Szukajka( / v)
		;(set_tile $key $value)
		(print $value)
		;(if (wcmatch $value "*/*")
		;	(setq v (vl-string-subst "-" "/" $value))
			(setq v $value)		
		;)
		(setq li '())
		(if (equal v "")
		(progn
			(setq li (mapcar 'car (cd:DWG_D)))
			(LM:DCL_WriteToList li "li" )
		)
		(progn
			(foreach xli (mapcar 'car (cd:DWG_D)) 
				(if (wcmatch (strcase xli) (strcat "*" (strcase v ) "*"))
				(progn
					(setq li (cons xli li))
				)
				)
			)
			(LM:DCL_WriteToList li "li" )
		)
		)
		
		;(setq idodac (atoi $value))
		;(term_dialog)
		;(unload_dialog "test")
		;(princ "\n*Cancel*")
		;(setq pli (nth idodac li))
		;(command "ARKUSZ" "U" pli )
		
	);DCL_dodac
	
	
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
				"    label = \"Lista Arkuszy\";"
				"    spacer;"
				"    : column"
				"    {" 
				"		 : text {label = \"Podaj tekst do szukajki(ENTER): \";}"
				"        : edit_box {action = \"(DCL_Szukajka)\"; value = \"\"; }"
				"        : lbx { key = \"li\"; action = \"(DCL_Wybierz)\"; label = \"Podaj arkusz\"; }"
				"    	: row"
				"	 	{"
				"		 	 : button { label = \"Sortuj\"; key = \"Sortuj\";  action = \"(DCL_Sortuj)\"; }	"
				"			 : button { label = \"Anuluj\"; key = \"Anuluj\"; is_cancel = true ;}	"
				"    	}"
				"    }"
				"    spacer;"
				;"    ok_cancel;"
				"}"
			)
			(write-line str des)
		)
		(not (setq des (close des)))
		(< 0 (setq dch (load_dialog dcl)))
		(new_dialog "test" dch)
	)
	(progn           
		(LM:DCL_WriteToList li "li" )
		
		(cond 
			((= 1 (start_dialog))
				(term_dialog)
				(princ "\n*Cancel*")
			)
			((= 0 what_next)
				(term_dialog)
				;(unload_dialog "test")
				(princ "\n*Cancel*")
			)
		);cond
	
	)
	)
	
	;(CD:SYS_UndoEnd)
	;(command "_regen" nil)
	;(setvar "CMDECHO" 1)
	nil

)

(defun c:zaokraglijpolilinie ( / acadObj doc mspace pspace e rad cmd tempvar typ sspoli) 

	;;;======================== VARIANTS & SAFEARRAYS ========================;;;
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq mspace (vla-get-modelspace doc))
	(setq pspace (vla-get-paperspace doc))
	
	(setq rad (atof (cd:USR_GetKeyWord "\nWartosc zaokraglenia" '("0" "0.25" "0.5" "0.75" "1") "0.5")))
	(setq sspoli (cd:SSX_Convert (ssget ":S" (list (cons -4 "<AND") (cons -4 "<OR") (cons 0 "LWPOLYLINE") (cons -4 "OR>") (cons 8 "0*") (cons -4 "AND>") )) 0))
		
	(setq cmd (cond (command-s)(vl-cmdf)(command))) 
	(setvar 'errno 0)
	(setq tempvar (getvar 'filletrad))
	
	; (if (and (zerop (getvar 'errno)) (setq e (car (entsel "\nZaznacz obiekt <exit>: "))))
	(if (and (zerop (getvar 'errno)) (setq e (car sspoli)))
	(cond
		( (or (not e) (= 7 (getvar 'errno))) (setvar 'errno 0) )
		( (wcmatch (setq typ (cdr (assoc 0 (entget e)))) "*POLYLINE")
			(setvar 'filletrad rad)
			(cmd "_.fillet" "_polyline" e)
		)
		( (= typ "LINE") 
			(setvar 'filletrad rad)
			(cmd "fillet" e "\\") )	   
		( (alert "Invalid object.") )
	)
	)
	(setvar 'filletrad tempvar)
	(princ)
	
)

(defun C:poprawic ( /)
	(alert (strcat "* Poprawiæ skrypt zestaw oswietlelnie nie liczy podwójnych opraw (mo¿na liczyæ z opraw w schemacie\n"
	
	" *skrypt tworz¹cy szafy kablowe\n"
	" *skrypt drukuj¹cy mapy zasadnicze !!\n"
	"obl skrypt/skrot"
	"itd"
	))
	
)

(defun C:warstwyliniakablowaniskie ( / )
	(setvar "PSLTSCALE" 0)
	
	(vla-put-Width (vlax-ename->vla-object (tblobjname "STYLE" "Standard")) 1.0)
	(LM:loadlinetypes '("DASHED(xref)" "DASHEDX2") nil)
	(print (LM:LayerSet "0 proj nN" acRed "DASHEDX2" acLnWt030))
	(print (LM:LayerSet "0 proj slupy" acRed "Continuous" acLnWt030))
	(LM:flat)
	(vla-regen (cd:ACX_ADoc) 1)
)
(defun C:warstwyliniakablowa ( / )
	(setvar "PSLTSCALE" 0)
	
	(vla-put-Width (vlax-ename->vla-object (tblobjname "STYLE" "Standard")) 1.0)
	(LM:loadlinetypes '("DASHED(xref)" "DASHEDX2") nil)
	(print (LM:LayerSet "0 proj nN" acBlue "DASHEDX2" acLnWt030))
	(print (LM:LayerSet "0 proj SN" acRed "DASHEDX2" acLnWt030))
	(print (LM:LayerSet "0 proj slupy" acRed "DASHEDX2" acLnWt030))
	(LM:flat)
	(vla-regen (cd:ACX_ADoc) 1)
)

(defun C:warstwyprzylacza ( / )
	(setvar "PSLTSCALE" 0)
	
	(vla-put-Width (vlax-ename->vla-object (tblobjname "STYLE" "Standard")) 1.0)
	(vla-put-Color (vlax-ename->vla-object (tblobjname "LAYER" "0")) 7)
	(LM:loadlinetypes '("DASHED(xref)" "DASHEDX2" "DASHED") nil)
	(print (LM:LayerSet "0 proj przy" acRed "DASHED" acLnWt030))
	(LM:flat)
	(vla-regen (cd:ACX_ADoc) 1)
)

(defun C:warstwyoswietlenie ( / )
	(setvar "PSLTSCALE" 0)
	
	(vla-put-Width (vlax-ename->vla-object (tblobjname "STYLE" "Standard")) 1.0)
	(vla-put-Color (vlax-ename->vla-object (tblobjname "LAYER" "0")) 7)
	(LM:loadlinetypes '("DASHED(xref)" "DASHEDX2" "DASHED") nil)
	(print (LM:LayerSet "0 proj slupy" acRed "DASHED" acLnWt030))
	(print (LM:LayerSet "0 proj kabel" acRed "DASHED" acLnWt030))
	(LM:flat)
	(vla-regen (cd:ACX_ADoc) 1)
)

(defun c:flat ( / ) (LM:flat))

(defun LM:flat ( / doc org blk elv obj )
   (setq doc (vla-get-activedocument (vlax-get-acad-object))
         org (vlax-3D-point 0 0 0)
   )
   (vlax-for blk (vla-get-blocks doc)
       (if (= :vlax-false (vla-get-isxref blk))
           (vlax-for obj blk
               (if (vlax-write-enabled-p obj)
                   (foreach elv '(1e99 -1e99) (vla-move obj org (vlax-3D-point 0 0 elv)))
               )
           )
       )
   )
   (vla-regen doc acallviewports)
   (princ)
   
)

(defun c:wyczernij ( / doc org )
   (setq doc (vla-get-activedocument (vlax-get-acad-object))
         org (vlax-3D-point 0 0 0)
   )
   (vlax-for blk (vla-get-blocks doc)
       (if (= :vlax-false (vla-get-isxref blk))
           (vlax-for obj blk
               (if (vlax-write-enabled-p obj)
					(if (and
						(not (equal (vla-get-layer obj) "0"))
						(not (wcmatch (vla-get-layer obj) "0 *" ))
						)
						(vla-put-color obj 0)
					)
               )
           )
       )
   )
   (vla-regen doc acallviewports)
   (princ)
   
)

(defun c:Drukuj_przy³¹cze ( / acadObj doc util layers mspace  pspace drukarkaHP drukarkaKonica sh Params hp konica testt itm lst App  BACKGROUNDPLT )
	(print "Start")
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))
	(setq util (vla-get-Utility doc))
	(setq layers (vla-get-Layers doc))
	(setq mspace (vla-get-ModelSpace doc))
	(setq pspace (vla-get-PaperSpace doc))
	;(setq drukarkaHP "HP Universal Printing PCL 6")
	(defun LM:RemoveNth ( n l )
		(if (and l (< 0 n))
			(cons (car l) (LM:RemoveNth (1- n) (cdr l)))
			(cdr l)
		)
	)

	(setq drukarkaHP "HP P2015")
	(setq drukarkaKonica "KONICA MINOLTA")
	
	(setq BACKGROUNDPLT (getvar 'BACKGROUNDPLOT))
	(setvar "BACKGROUNDPLOT" 0)
	;(setvar 'TRAYTIMEOUT 1)

	(setq sh (vlax-create-object "Shell.Application"))
	(print "Sh")
	;(setq pdf (findfile "Z:\AZART projektowanie\PROJEKTY\Uprawnienia\Piotr Kwiatkowki - WYPELNIONY KOMPLETNIE.pdf")
	(setq ParamsP (strcat "/t \"Z:\AZART projektowanie\PROJEKTY\Uprawnienia\Piotr Kwiatkowski - WYPELNIONY KOMPLETNIE.pdf\" \"HP P2015\""))
	(setq ParamsM (strcat "/t \"Z:\AZART projektowanie\PROJEKTY\Uprawnienia\Marek Banaszak - WYPELNIONY KOMPLETNIE.pdf\" \"HP P2015\""))
	;(setq Params (strcat "/t \"" pdf "\" \"" drukarkaHP "\""))
	
	
	(setq hp
	(list 
		"SK"
		"SK+P(555)"
		"SK+P(420)"
		"ZL-1"
		"ZL-2"
		"ZL-1_RBK"
		"ZK+1P"
		"SK+8P"
		"0_BIOZ_napow"
		"0_BIOZ_kabel"
		"przekladniki"
		
	)
	)
	(setq konica
	(list 
		"SK+2P"
		"PP"
		"SK+1PP_(Dobra)"
		"SK+4P"
		"1PP"
		"0_Tytulowa_M"
		"0_Tytulowa_P"
		"0_Odbior"
		"0_Profil"
		"0_sch_mont_PP"
		"1MUF_SK_SKP_OU"  "1SK_SKP_OU" "1MUF_ZP" "1MUF_SK_PP_WU" "1¯N_ZL-2_OU" "1¯N_ZL-1_WU" "1¯N_ZL-1_OU" "1¯N_SKP_WU" "1¯N_SKP_OU" "1¯N_RSA_ZL-1_WU" "1¯N_RSA_ZL-1_OU" "1¯N_RSA_P_ZL-1_OU" "1¯N_P_ZL-2_OU" "1¯N_P_ZL-1_OU" "1¯N_P_SKP_OU" "1ZL_ZL_WU" "1ZL_ZL_OU" "1ZL_P_ZL_OU" "1ZAPAS_SKP" "1ST_RSA_SK+P_OU" "1SK_ZL-1_WU" "1SK_ZL-1_OU" "1SK_SKP_WU" "1SK_SKP_OU" "1SK_P_ZL-1_OU" "1SK_P_SKP_OU" "1MUF_SKP" "1MUF_SK_ZL-1 WU" "1E_ZL-2_OU" "1E_ZL-1_WU" "1E_ZL-1_OU" "1E_SKP_WU" "1E_SKP_OU" "1E_P_ZL-2_OU" "1E_P_ZL-1_OU" "1E_P_SKP_OU" 
		
	)
	)
	(print "Filtbox")
	(setq lst (LM:FiltListBox "Zaznacz arkusze do druku" (append (layoutlist) '("Uprawnienia")) T ))
	
	(if lst
	(progn
		(setq liczbakopii 
			(cd:DCL_StdEditBoxDialog
			  (list 2
				(list
				  (cons 1 "Wprowadz liczbe rzeczywista")
				  (cons 2 "Liczba nie moze byc zerem")
				  (cons 8 "Spacje niedozwolone")
				  (cons 16 "To nie jest liczba")
				  (cons 32 "Liczba jest za mala")
				  (cons 64 "Liczba jest za duza")
				)
				"2" 1 10 2 2                                                                       
			  )
			  "Kopie" "Wprowadz liczbe kopii: (1-10)" 40 13 (list "&Ok" "&Anuluj")
			  T nil
			)
		)
		
		(repeat (atoi liczbakopii)
			(if (setq o (vl-position "Uprawnienia" lst ))
			(progn
			(print "Print Uprawnienia")
				(cond 
					((setq App (findfile "C:\Program Files (x86)\Foxit Software\Foxit Reader\FoxitReader.exe"))
						(print "Foxit Reader")
						;(vlax-invoke-method sh 'ShellExecute App ParamsP)
					)
					((setq App (findfile "C:\Program Files (x86)\Foxit Software\Foxit PhantomPDF\FoxitPhantomPDF.exe"))
						(print "Foxit Phantom")
						;(vlax-invoke-method sh 'ShellExecute App ParamsP)
					)
				)
				(cond 
					((or (vl-position "0_Tytulowa_M" lst )(vl-position "0_Tytulowa_M" (layoutlist) ))
						(vlax-invoke-method sh 'ShellExecute App ParamsM)
						(print "Druk Uprawnien M")
					)
					((or (vl-position "0_Tytulowa_P" lst )(vl-position "0_Tytulowa_P" (layoutlist) ))
						(vlax-invoke-method sh 'ShellExecute App ParamsP)
						(print "Druk Uprawnien P")
					)
				)
				
			)
			)
			(if o (setq lst1 (LM:RemoveNth o lst)) (setq lst1 lst) )
			(foreach itm lst1 ;; For every 'itm' in the list given by 'lst'

				(setvar 'LTSCALE 1)
				(setvar 'PSLTSCALE 0)
				(setvar 'CTab itm)
				
				(setq pspace (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object))))
	
				(vlax-for obj pspace 
					(if (equal "dane" (LM:effectivename obj)) 
						(progn 
							(setq testt (cd:BLK_GetAttValueVLA obj "PODMIOT") )
						)
					
					)
				)
				(if (equal testt "Imie Nazwisko")
					(LM:wych)
				)
				
				(cond 
					((vl-position itm hp)
						(COMMAND  "_PLOT" "N" "" "" drukarkaHP "" "N" "T")
						(princ (strcat "\nDrukowanie arkusza \"" itm " z drukarki " drukarkaHP "\" "  )) 
					)
					((vl-position itm konica)
						(COMMAND  "_PLOT" "N" "" "" drukarkaKonica "" "N" "T")
						(princ (strcat "\nDrukowanie arkusza \"" itm " z drukarki " drukarkaKonica "\" "  )) 
					)
				)
			) ;; end foreach
		);end repeat
	)
	)
	;(if (= (vla-get-MSpace doc) :vlax-true) (vla-put-Mspace doc 1))
	(vlax-release-object sh)
	(if (= (getvar "tilemode") 0) (setvar "tilemode" 1)) ; Go back to MSPACE

	(princ)
)

; List Layouts in popup list
; Select Layouts to plot

(defun c:PDFCreator ( / itm lst BACKGROUNDPLT )

	(setq BACKGROUNDPLT (getvar 'BACKGROUNDPLOT))
	(setvar "BACKGROUNDPLOT" 0)
	;(setvar 'TRAYTIMEOUT 1)

	(setq lst (LM:FiltListBox "Zaznacz arkusze do druku" (layoutlist) T ))

	(foreach itm lst ;; For every 'itm' in the list given by 'lst'

	   (setvar 'LTSCALE 1)
	   (setvar 'PSLTSCALE 0)
	   (setvar 'CTab itm)
	   (COMMAND  "_PLOT" "N" "" "" "PDFCreator" "" "N" "T")
	   (princ (strcat "\nDrukowanie arkusza \"" itm "\" "  ))

	) ;; end foreach
	;(if (= (vla-get-MSpace doc) :vlax-true) (vla-put-Mspace doc 1))
	(if (= (getvar "tilemode") 0) (setvar "tilemode" 1)) ; Go back to MSPACE
   
	(princ)
)

(defun c:MapaKonica ( / itm lst BACKGROUNDPLT )

	(setq BACKGROUNDPLT (getvar 'BACKGROUNDPLOT))
	(setvar "BACKGROUNDPLOT" 0)
	;(setvar 'TRAYTIMEOUT 1)
	(setvar 'LTSCALE 1)
	(setvar 'PSLTSCALE 0)
	
	(COMMAND  "_PLOT" "T" "" "KONICA MINOLTA*" "A4" "Milimetry" "Pionowo" "N" "Okno" (setq pt (getpoint)) (getcorner pt) "1=0.5" "œrodek" "N" "" "T" "Jak" "N" "N" "T")
	
	;(if (= (vla-get-MSpace doc) :vlax-true) (vla-put-Mspace doc 1))
	;(if (= (getvar "tilemode") 0) (setvar "tilemode" 1)) ; Go back to MSPACE
   
	(princ)
)

(defun c:PDFCreator_wniosek ( / itm lst BACKGROUNDPLT )

	(setq BACKGROUNDPLT (getvar 'BACKGROUNDPLOT))
	(setvar "BACKGROUNDPLOT" 0)
	;(setvar 'TRAYTIMEOUT 1)

	;(setq lst (LM:FiltListBox "Zaznacz arkusze do druku" (layoutlist) T ))

	(setq sh (vlax-create-object "Shell.Application"))
	(print "Sh")
	;(setq pdf (findfile "Z:\AZART projektowanie\PROJEKTY\Uprawnienia\Piotr Kwiatkowki - WYPELNIONY KOMPLETNIE.pdf")
	(setq ParamsP (strcat "/t \"Z:\AZART projektowanie\PROJEKTY\Uprawnienia\Piotr Kwiatkowski - WYPELNIONY KOMPLETNIE.pdf\" \"HP P2015\""))
	;(setq Params (strcat "/t \"" pdf "\" \"" drukarkaHP "\""))
	
	(foreach itm lst ;; For every 'itm' in the list given by 'lst'

	   (setvar 'LTSCALE 1)
	   (setvar 'PSLTSCALE 0)
	   (setvar 'CTab itm)
	   (COMMAND  "_PLOT" "N" "" "" "PDFCreator" "" "N" "T")
	   (princ (strcat "\nDrukowanie arkusza \"" itm "\" "  ))

	) ;; end foreach
	;(if (= (vla-get-MSpace doc) :vlax-true) (vla-put-Mspace doc 1))
	(if (= (getvar "tilemode") 0) (setvar "tilemode" 1)) ; Go back to MSPACE
   
	(princ)
)
;;------------------=={ Filtered List Box }==-----------------;;
;;                                                            ;;
;;  Displays a list box interface from which the user may     ;;
;;  select one or more items. Includes an edit box filter     ;;
;;  to enable the user to filter the displayed list of items. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg - List box dialog title                               ;;
;;  lst - List of strings to display in the list box          ;;
;;  mtp - Boolean flag to determine whether the user may      ;;
;;        select multiple items (T=Allow Multiple)            ;;
;;------------------------------------------------------------;;
;;  Returns:  List of selected items, else nil.               ;;
;;------------------------------------------------------------;;

(defun LM:FiltListBox ( msg lst mtp / _addlist dch dcl des rtn tmp )

   (defun _addlist ( key lst )
       (start_list key)
       (foreach x lst (add_list x))
       (end_list)
       lst
   )

   (if
       (and
           (setq dcl (vl-filename-mktemp nil nil ".dcl"))
           (setq des (open dcl "w"))
           (write-line
               (strcat
                   "filtlistbox : dialog { label = \"" msg "\"; spacer;"
                   ": list_box { key = \"lst\"; width = 50; fixed_width = true; height = 15; fixed_height = true; allow_accept = true; "
                   "multiple_select = " (if mtp "true" "false") "; }"
                   ": edit_box { key = \"flt\"; width = 50; fixed_width = true; label = \"Filter:\"; }"
                   "spacer; ok_cancel; }"
               )
               des
           )
           (not (close des))
           (< 0 (setq dch (load_dialog dcl)))
           (new_dialog "filtlistbox" dch)
       )
       (progn
           (_addlist "lst" (setq tmp lst))
           (set_tile "lst" (setq rtn "0"))
           (set_tile "flt" "*")
           (action_tile "lst" "(setq rtn $value)")
           (action_tile "flt"
               (vl-prin1-to-string
                  '(progn
                       (setq flt (strcat "*" (strcase $value) "*"))
                       (_addlist "lst" (setq tmp (vl-remove-if-not '(lambda ( x ) (wcmatch (strcase x) flt)) lst)))
                       (set_tile "lst" (if (< (atoi rtn) (length tmp)) rtn (setq rtn "0")))
                   )
               )
           )
           (setq rtn
               (if (= 1 (start_dialog))
                   (mapcar '(lambda ( x ) (nth x tmp)) (read (strcat "(" rtn ")")))
               )
           )
       )
   )
   (if (< 0 dch)
       (setq dch (unload_dialog dch))
   )
   (if (and (= 'str (type dcl)) (findfile dcl))
       (vl-file-delete dcl)
   )
   rtn
)

(defun C:szukajpodmiotu ( / mspace pspace li idodac dcl des dch str dane danew dzialki obreby)
	(setq mspace (vla-get-modelspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	(setq pspace (vla-get-paperspace 
                (vla-get-activedocument 
                   (vlax-get-acad-object))))
	
	
	(defun *error* (errmsg)
		; (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
			; (setvar "OSMODE" lastOSMODE)
			; (setvar "clayer" oldlayer)
			; (vla-delete poly)
			(print errmsg)
			(print d )	
			(print o )
			(print i)			
			; (print ip1 )
			; (print lobjk )
			; (print lobjt )
			; (print lobjl )
			; (print kol1 )
			; (print kol2 )
			; (print i )
			; (print j )
			; (print (vlax-dump-object obj))
			; (print (nth j listobjlinie))
			; (vla-ZoomCenter acadObj (vlax-3D-point (list (car test) (cadr test) 0.0)) 10)
			; (vla-put-activeLayer doc objlayer)
			(cd:SYS_UndoEnd)
			; (setvar "CMDECHO" 1)
		; )
	); end *error*
	; (cd:SYS_UndoBegin)
	
	; (foreach x (cd:DWG_LayoutsList)
		; (setq li (cons (car x) li))
	; )
	
	(defun DCL_Wybierz( / idodac pli pos wyniki)
		; (set_tile $key $value)
		(setq idodac (atoi $value))
		
		; (print idodac)
		(if (not (equal idodac -1)) 
		(progn
			(setq pli (nth idodac li))
			; (print pli)
			; (print libak)
			(setq pos (vl-position pli libak))
			
			; (term_dialog)
			(cond
				((equal (nth pos cosiedzieje) 0 )
					(setq zebrane "---- Nie napisana, Nie zebrana ----")
				)
				((equal (nth pos cosiedzieje) 1 )
					(setq zebrane "---- Napisana, Nie zebrana ----")
				)
				((equal (nth pos cosiedzieje) 2 )
					(setq zebrane "---- Napisana, Zebrana ----")
				)
				((equal (nth pos cosiedzieje) 3 )
					(setq zebrane "---- Œ³u¿ebnoœæ ----")
				)
				(T
				(setq zebrane "Jakis b³¹d")
				)
			)
			
			(setq wyniki (strcat (nth pos dane) "\n\n Tak¿ê no: " zebrane "\n\nUwagi:\n" (nth pos uwagi)))
			(alert wyniki)
		)
		)
		; (command "ARKUSZ" "U" pli )
	
	);DCL_Wybierz
	
	(defun DCL_Sortuj( / )
		; (set_tile $key $value)
		(print "Sort ok")
		; (setq li (sort li '<))
		; (LM:DCL_WriteToList li "li" )
		
	);DCL_Sortuj
	
	(defun DCL_Szukajka( / )
		; (set_tile $key $value)
		; (print $value)
		(setq li (list))
		(if (equal $value "")
		(progn
			(setq li libak)
			(LM:DCL_WriteToList li "li" )
		)
		(progn
			(foreach xli libak
				(if (wcmatch (strcase xli) (strcat "*" (strcase $value ) "*"))
				(progn
					(setq li (cons xli li))
				)
				)
			)
			(LM:DCL_WriteToList li "li" )
		)
		)
		
		; (setq idodac (atoi $value))
		; (term_dialog)
		; (unload_dialog "test")
		; (princ "\n*Cancel*")
		; (setq pli (nth idodac li))
		; (command "ARKUSZ" "U" pli )
		
	);DCL_dodac
	(setq dir "Z:\\AZART projektowanie\\PROJEKTY\\JANÓW\\ROBOCZY\\Wypisy\\excel\\")
	(setq obreb1 "Janów.xlsm")
	
	; (setq dzialki (LM:getCellsFunction (strcat dir obreb1) "Sheet1" (strcat "D" "2" ":D" "200")))
	; (setq obreby (LM:getCellsFunction (strcat dir obreb1) "Sheet1" (strcat "E" "2" ":E" "200")))
	; (setq dane (LM:getCellsFunction (strcat dir obreb1) "Sheet1" (strcat "G" "2" ":G" "200")))
	(setq danew (LM:getCellsFunction (strcat dir obreb1) "Sheet1" (strcat "B" "2" ":H" "200")))
	; (setq cosiedzieje (LM:getCellsFunction (strcat dir obreb1) "Sheet1" (strcat "B" "2" ":D" "200")))

	; (setq dzialki (vl-remove nil dzialki))
	; (setq obreby (vl-remove nil obreby))
	(setq danew (vl-remove nil danew))
	; (setq cosiedzieje (vl-remove nil cosiedzieje))
	; (print cosiedzieje)
	; (print danew)
	; (print "danew")
	; (print (length danew))
	(setq li (list))
	(setq dzialki (list))
	(setq obreby (list))
	(setq dane (list))
	(setq uwagi (list))
	(setq cosiedzieje (list))
	
	(setq i 0)
	(while (< i (length danew))
		; (setq test1 (nth (+ i 0) dane))
		
		(setq dzialki (cons (setq d (nth (+ i 2) danew)) dzialki))
		(setq obreby (cons (setq o (nth (+ i 3) danew)) obreby))
		(setq dane (cons (nth (+ i 5) danew) dane))
		(setq uwagi (cons (nth (+ i 6) danew) uwagi))
		(setq cosiedzieje (cons (nth i danew) cosiedzieje))
		
		; (print d)
		; (print o)
		(if (numberp  d) (setq d (rtos d 2 0)))
		
		(setq li (cons (strcat d " " o) li))
		(setq i (+ i 7))
	)
	
	(setq li (reverse li))
	(setq dzialki (reverse dzialki))
	(setq obreby (reverse obreby))
	(setq dane (reverse dane))
	(setq cosiedzieje (reverse cosiedzieje))
	(setq uwagi (reverse uwagi))
	
	; (print li)
	(setq libak li)
	
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
					"    width = 35;"
					"    height = 15;"
					"}"
					"test : dialog"
					"{"
					"    label = \"Lista Arkuszy\";"
					"    spacer;"
					"    : column"
					"    {" 
					"		 : text {label = \"Podaj tekst do szukajki(ENTER): \";}"
					"        : edit_box {action = \"(DCL_Szukajka)\"; value = \"\"; }"
					"        : lbx { key = \"li\"; action = \"(DCL_Wybierz)\"; label = \"Podaj obrêb\"; }"
					"    	: row"
					"	 	{"
					"		 	 : button { label = \"Sortuj\"; key = \"Sortuj\";  action = \"(DCL_Sortuj)\"; }	"
					"			 : button { label = \"Anuluj\"; key = \"Anuluj\"; is_cancel = true ;}	"
					"    	}"
					"    }"
					"    spacer;"
					; "    ok_cancel;"
					"}"
				)
				(write-line str des)
			)
			(not (setq des (close des)))
			(< 0 (setq dch (load_dialog dcl)))
			(new_dialog "test" dch)
		)
	(progn           
		(LM:DCL_WriteToList li "li" )
		
		(cond 
			((= 1 (start_dialog))
				(term_dialog)
				(princ "\n*Cancel*")
			)
			((= 0 what_next)
				(term_dialog)
				; (unload_dialog "test")
				(princ "\n*Cancel*")
			)
		);cond
	)
	)
	
	; (CD:SYS_UndoEnd)
	; (command "_regen" nil)
	; (setvar "CMDECHO" 1)
	nil
)

(defun C:szukajmapy ( / acadObj mspace dir dirlist liall pspace li idodac dcl des dch str)
	
	(cd:SYS_UndoBegin)
	; (print (LM:directoryfiles (getvar 'dwgprefix) "*.dxf" nil))
	(setq files (LM:directoryfiles "Z:\\AZART projektowanie\\PROJEKTY\\KLOCKI 2020 IV\\#MATERIA£Y\\#MAPY\\dwg\\" "*.dwg" nil))
	(setq files2 (LM:directoryfiles "Z:\\AZART projektowanie\\PROJEKTY\\KLOCKI 2020 V\\#MATERIA£Y\\#MAPY\\" "*.dwg" nil))
	; Z:\AZART projektowanie\PROJEKTY\KLOCKI 2020 V\#MATERIA£Y\#MAPY
	; (print (append files files2))
	(setq fileopen '())
	(setq qi (getstring "Dzia³ka: "))
	(foreach file (append files files2)
	
		(setq dbx (vla-getInterfaceObject (vlax-get-acad-object) (strcat "ObjectDBX.ZxDbDocument." (substr (getvar "ACADVER") 1 2))))
		(vla-open dbx file)
		(setq ms (vla-get-modelspace dbx))
		
	
		(vlax-for obj ms
			(if (and
					(vlax-property-available-p obj "Layer" )
					;(equal (vla-get-layer obj) "Dzialki")
					(equal (vla-get-layer obj) "211-Nr dzia³ki")
				)
			(progn
				(if (and
						(vlax-property-available-p obj "TextString" )
						(equal (vla-get-textString obj) qi)
					)
					(setq fileopen (cons file fileopen))
				)
			)
			)
			
		)
		
		(vlax-release-object dbx)
		
	)
	(foreach file fileopen
		(print fileopen)
		(vla-open (vla-get-documents (vlax-get-acad-object)) file)
	)
	
	(CD:SYS_UndoEnd)
	;(command "_regen" nil)
	;(setvar "CMDECHO" 1)
	; nil

)

(defun C:infraster ( / acadObj doc mspace pspace obj l_obiekt p0 p1 p2 p3 p4 layname layobj iw ih)

	(cd:SYS_UndoBegin)
	;(print "GOGO1")
	(setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
	(setq mspace (vla-get-modelspace doc))
	(setq pspace (vla-get-paperspace doc))
	;(setq oldlayer (getvar "clayer"))
	
	(setq l_obiekt (list))
	
	(vlax-for obj mspace
		(if (and
					(equal "AcDbRasterImage" (vla-get-EntityName obj))
					(wcmatch (strcase (vla-get-layer obj)) "M*")
			)
		(progn				
			(setq l_obiekt (cons obj l_obiekt))	
		)
		)
	)	
	; (print l_obiekt)
	(setq p0 (getpoint))
	

	(foreach obj l_obiekt
		(setq iw (vla-get-ImageWidth obj))
		(setq ih (vla-get-ImageHeight obj))
		(setq p1 (vla-get-Origin obj))
		(setq p1 (vlax-safearray->list (vlax-variant-value p1)))
		(setq p2 (polar p1 (LM:DtR 0.0) iw))
		(setq p3 (polar p2 (LM:DtR 90.0) ih))
		(setq p4 (polar p1 (LM:DtR 90.0) ih))
		(if 
			(and 
				(> (car p0) (car p1))
				(> (cadr p0) (cadr p1))
				(< (car p0) (car p3))
				(< (cadr p0) (cadr p3))
			)
			(progn
				;(print obj)
				(setq layname (vla-get-layer obj))
				(setq layobj (tblobjname "layer" layname))
				(vla-put-LayerOn (vlax-ename->vla-object layobj) :vlax-true)
			)
		
		)
	)
	
	(cd:SYS_UndoEnd)
)



; (defun C:listlay ( / mspace pspace li idodac dcl des dch str)
	; (setq mspace (vla-get-modelspace 
                ; (vla-get-activedocument 
                   ; (vlax-get-acad-object))))
	
	; (setq pspace (vla-get-paperspace 
                ; (vla-get-activedocument 
                   ; (vlax-get-acad-object))))
	
	
	
	
	; ; (cd:SYS_UndoBegin)
	
	; (setq li (list))
	; ; (foreach x (cd:DWG_LayoutsList)
		; ; (setq li (cons (car x) li))
	; ; )
	; (setq li (mapcar 'car (cd:DWG_LayoutsList)))
	
	; (defun DCL_Wybierz( / )
		; ; (set_tile $key $value)
		; (setq idodac (atoi $value))
		; (term_dialog)
		; ; (unload_dialog "test")
		; (princ "\n*Cancel*")
		; (setq pli (nth idodac li))
		; (command "ARKUSZ" "U" pli )
		
	; );DCL_dodac
	; (defun DCL_Sortuj( / )
		; ; (set_tile $key $value)
		; (print "Sort ok")
		; (setq li (sort li '<))
		; (LM:DCL_WriteToList li "li" )
		
	; );DCL_dodac
	; (defun DCL_Szukajka( / )
		; ; (set_tile $key $value)
		; (print $value)
		; (setq li (list))
		; (if (equal $value "")
		; (progn
			; (setq li (mapcar 'car (cd:DWG_LayoutsList)))
			; (LM:DCL_WriteToList li "li" )
		; )
		; (progn
			; (foreach xli (mapcar 'car (cd:DWG_LayoutsList)) 
				; (if (wcmatch (strcase xli) (strcat "*" (strcase $value ) "*"))
				; (progn
					; (setq li (cons xli li))
				; )
				; )
			; )
			; (LM:DCL_WriteToList li "li" )
		; )
		; )
		
		; ; (setq idodac (atoi $value))
		; ; (term_dialog)
		; ; (unload_dialog "test")
		; ; (princ "\n*Cancel*")
		; ; (setq pli (nth idodac li))
		; ; (command "ARKUSZ" "U" pli )
		
	; );DCL_dodac
	
	
	; (if
		; (and
			; (setq dcl (vl-filename-mktemp "tmp.dcl"))
			; (setq des (open dcl "w"))
			; (foreach str
			   ; '(
					; "lbx : list_box"
					; "{"
					; "    alignment = centered;"
					; "    fixed_width = true;"
					; "    fixed_height = true;"
					; "    width = 20;"
					; "    height = 15;"
					; "}"
					; "test : dialog"
					; "{"
					; "    label = \"Lista Arkuszy\";"
					; "    spacer;"
					; "    : column"
					; "    {" 
					; "		 : text {label = \"Podaj tekst do szukajki(ENTER): \";}"
					; "        : edit_box {action = \"(DCL_Szukajka)\"; value = \"\"; }"
					; "        : lbx { key = \"li\"; action = \"(DCL_Wybierz)\"; label = \"Podaj arkusz\"; }"
					; "    	: row"
					; "	 	{"
					; "		 	 : button { label = \"Sortuj\"; key = \"Sortuj\";  action = \"(DCL_Sortuj)\"; }	"
					; "			 : button { label = \"Anuluj\"; key = \"Anuluj\"; is_cancel = true ;}	"
					; "    	}"
					; "    }"
					; "    spacer;"
					; ; "    ok_cancel;"
					; "}"
				; )
				; (write-line str des)
			; )
			; (not (setq des (close des)))
			; (< 0 (setq dch (load_dialog dcl)))
			; (new_dialog "test" dch)
		; )
	; (progn           
		; (LM:DCL_WriteToList li "li" )
		
		; (cond 
			; ((= 1 (start_dialog))
				; (term_dialog)
				; (princ "\n*Cancel*")
			; )
			; ((= 0 what_next)
				; (term_dialog)
				; ; (unload_dialog "test")
				; (princ "\n*Cancel*")
			; )
		; );cond
	
	; )
	; )
	
	; ; (CD:SYS_UndoEnd)
	; ; (command "_regen" nil)
	; ; (setvar "CMDECHO" 1)
	; nil
; )

; (defun c:kawka (/ layer test p c w h p1 p2 p3 p4 ss vp layers pspace mspace util doc acadObj objKabel textObj) 	
	; (defun *error* ( msg )
       ; (print msg)
		; ;(if (wcmatch msg "ObjectIdToObject")
		; (princ "\n*error* program test .")
			
      ; ;)
	
   ; )
	; (setq k0 ""
			; k1 ""
			; k2 "")
	; (setq grandeprojektande (list "El Prezident Marek Banaszek +30 " "Piotr K +30 " "Piotr K -30 " ))
	; (setq grandeprojektande (list "El Prezident Marek Banaszek +30 " "Marek Banaszek +30 " "Prezident Marek Banaszek +30 " ))
	; (setq grandeprojektande (list "Kapeæ " "robi kawe " "za kare do konca tygodnia " ))
	; (setq kto 0)
	; (setq dat (getvar "date"))
	; (setq fdat (fix dat))
	; (setq pon 2458414)
	; (setq ponKto 1)
	
	; ;(print fdat)
	
	; ;(setq j (rem pon 7))
	; (setq dzien (- fdat 2458414))
	; (setq i (/ (- fdat 2458414) 7))
	; (setq dzien2 (- dzien (* 2 i)))
	
	; ;(setq j (rem fdat 7))
	
	; ;(print j)
	; (setq kto (rem dzien2 3))
	
	; (if (= kto 0) (setq k0 "--> " )(setq k0 "     " ))
	; (if (= kto 1) (setq k1 "--> " )(setq k1 "     " ))
	; (if (= kto 2) (setq k2 "--> " )(setq k2 "     " ))
	
	; (alert 
		; (strcat k0 (nth 0 grandeprojektande) "\n"
				; k1 (nth 1 grandeprojektande) "\n"
				; k2 (nth 2 grandeprojektande))
	; )
	; (princ)
	
; )

(print "Wczytano plik TEST")
