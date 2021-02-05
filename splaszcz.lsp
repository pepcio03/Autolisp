(vl-load-com)
;;;
(defun C:splaszcz (/)
	(cd:SYS_UndoBegin)
	(command "_MOVE" "_all" "" "0,0,1e99" "")
	(command "_MOVE" "_all" "" "0,0,-1e99" "")
	(cd:SYS_UndoEnd)
)