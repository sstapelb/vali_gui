function is_compressed,file,bzip2=bzip2,gzip=gzip
;-------------------------------------------------------------
;+
; NAME:
;       is_compressed
; PURPOSE:
;       checks if file is an existing file, readable and compressed 
;
;	If the use_magic_number keyword is set, it looks for the 
;	magic number of gzip files which is abit faster  
;
; CATEGORY:In/Output, strings
;
; CALLING SEQUENCE:
;
;	isfile=is_compressed("/full/or/relativ/path")
;
; MODIFICATION HISTORY:
;       Written  R. Preusker, Nov, 2003.
;
; Copyright (C) 2003, Freie Universitaet Berlin
; This software may be used, copied, or redistributed as long
; as it is not sold and this copyright notice is reproduced on
; each copy made.  This routine is provided as is without any
; express or implied warranties whatsoever.
;-

	error=0
	catch,error
	dum=bytarr(12)
	if error eq 0 then begin
		openr,lun,file,/get_lun
		readu,lun,dum
		free_lun,lun
		bz = (total(dum[0:2] eq [66b,90b,104b]) eq 3.)
		gz = (total(dum[0:1] eq     [31b,139b]) eq 2.)
		if keyword_set(bzip2) then ret = bz else $
		if keyword_set(gzip)  then ret = gz else $
		ret = (bz or gz)
	endif else begin
		ret=0
	endelse

	return, ret
end
