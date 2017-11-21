FUNCTION TO_HEX, D, NCHAR
;+
; NAME:
;       TO_HEX
; PURPOSE:
;       Translate a non-negative decimal integer to a hexadecimal string
; CALLING SEQUENCE:
;       HEX = TO_HEX( D, [ NCHAR ] )
; INPUTS:
;       D - non-negative decimal integer, scalar or vector.  If input as a
;           string, (e.g. '32') then all leading blanks are removed.
;
; OPTIONAL INPUT:
;       NCHAR - number of characters in the output hexadecimal string.
;               If not supplied, then the hex string will contain no 
;               leading zeros.
;
; OUTPUT:
;       HEX - hexadecimal translation of input integer, string
;
; EXAMPLES:
;       IDL> A = TO_HEX([11,16])    ==>   A = ['B','10']
;       IDL> A = TO_HEX(100,3) ==>   A = '064'
;
; METHOD:
;       The hexadecimal format code '(Z)' is used to convert.  No parameter
;       checking is done.
; PROCEDURES CALLED:
;       None.
; REVISION HISTORY:
;       Written   W. Landsman         November, 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use FSTRING() for more than 1024 values      March 2000 
;       Assume since  V5.4, omit FSTRING() call      April 2006
;-

  if N_elements(nchar) EQ 0 then format = '(Z)' else begin
      ch = strtrim( nchar, 2 ) 
      format = '(Z' + ch + '.' + ch + ')'
  endelse

  return, strtrim( string(d, FORM = format), 2)

  end
;-------------------------------------------------------------------------------------------------------------------------
;checks magic number of image files , jpg,jpeg,png
;based on is_compressed.pro from rene preusker
function is_image,file,hex=hex

	error=0
	catch,error
	dum=bytarr(12)
	if error eq 0 then begin
		openr,lun,file,/get_lun
		readu,lun,dum
		free_lun,lun
		hex =	to_hex(dum,2)
		bmp	= 	(total(hex[0:1] eq ['42','4D'])           eq 2.)
		jpg	= 	(total(hex[0:2] eq ['FF','D8','FF'])      eq 3.)
		png	= 	(total(hex[0:3] eq ['89','50','4E','47']) eq 4.)
		gif	= 	(total(hex[0:3] eq ['47','49','46','38']) eq 4.)
		pic	= 	(total(hex[0:3] eq ['00','00','00','00']) eq 4.)			; pict = IBM Storyboard bitmap file? PIC
		tif	= 	(total(hex[0:3] eq ['49','49','2A','00']) eq 4.) or  $ 	; little endian
				(total(hex[0:3] eq ['4D','4D','00','2A']) eq 4.) 			; big endian
		ps	= 	(total(hex[0:3] eq ['25','21','50','53']) eq 4.)
		eps	= 	(total(hex[0:3] eq ['C5','D0','D3','C6']) eq 4.)
		pdf	=	(total(hex[0:3] eq ['25','50','44','46']) eq 4.)
		ret = 	bmp or jpg or png or gif or pic or tif or ps or eps or pdf
	endif else begin
		hex='00'
		ret=0
	endelse

	return,ret
end
