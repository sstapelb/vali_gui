;---------------------------------------------------------------------------------------------------
;routine:		vector
;description:	gives an uniform spaced n elementry vector of values between
;				x0 and x1
;author:		peter albert
;---------------------------------------------------------------------------------------------------
; function vector, x0, x1, n
; 	return, (lindgen(n) * (x1 - x0)) / (n - 1l) + x0
; end
function vector,x0,x1,pts,logarithmic=logarithmic

	sv1=size(x0)
	sv2=size(x1)

	if sv1[0] ne 0 then message,'Min and Max must be scalars.'

	if sv1[1] eq 7 then begin
		x0  = float(x0)
		x1  = float(x1)
		sv1 = size(x0)
		sv2 = size(x1)
; 		message,'String vectors not supported.'
	endif
	if sv1[1] eq 8  then message,'Vectors of structures not supported.'
	if sv1[1] eq 10 then message,'Vectors of Pointers not supported.'
	if sv1[1] eq 11 then message,'Vectors of Objects not supported.'
	v1=x0
	v2=x1
; 	pts=fix(pts)
	if pts eq 1 then result=v1 else begin
	if keyword_set(logarithmic) then begin
		if (v1 le 0) or (v2 le 0) then  $
		message,'Min and Max must be positive for logarithmic spacing.'
		v1=alog10(v1)
		v2=alog10(v2)
	endif

	case sv1[1] of
; 		1  : result =    bindgen(pts)*(byte(v2)-byte(v1))/byte(pts-1)+byte(v1)
		1  : result =    byte(findgen(pts)*(float(v2)-float(v1))/float(pts-1))
		2  : result =     indgen(pts)*(v2-v1)/(pts-1)+v1
		3  : result =    lindgen(pts)*(v2-v1)/(pts-1)+v1
		4  : result =    findgen(pts)*(v2-v1)/(pts-1)+v1
		5  : result =    dindgen(pts)*(v2-v1)/(pts-1)+v1
		6  : result =    cindgen(pts)*(v2-v1)/(pts-1)+v1
		9  : result =   dcindgen(pts)*(v2-v1)/(pts-1)+v1
		12 : result =    uindgen(pts)*(v2-v1)/(pts-1)+v1
		13 : result =   ulindgen(pts)*(v2-v1)/(pts-1)+v1
		14 : result =  l64indgen(pts)*(v2-v1)/(pts-1)+v1
		15 : result = ul64indgen(pts)*(v2-v1)/(pts-1)+v1
		else :	message,'Vector: unsupported datatype.'
	endcase
	endelse
	if keyword_set(logarithmic) then result=(10.d)^result
	return,[result]
end
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
;                                            EXAMPLES
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
pro vector_ex01
	print, vector(2., 3., 11)
end
;---------------------------------------------------------------------------------------------------
