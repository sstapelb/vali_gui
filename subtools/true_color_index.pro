function true_color_index,r,g,b,bw=bw
;-------------------------------------------------------------
;+
; NAME:
;       true_color_index
; PURPOSE:
;	returns the 1dim index of a colored line (in plot) 
;	of a true color display. The color is defined as a
;	r,g,b triple where r,g,b are integers between 0 and 255   
;
; CATEGORY:
;	Plotting
;
; CALLING SEQUENCE:
;	index=true_color_index(r,g,b)
;
; EXAMPLE:
;	yellow_index=true_color_index(255,255,0)
;	plot,[0,1],color=yellow_index   ; <- plot yellow line
;
; TRUE_COLOR ON PS:
;	Unfortunatly the the color treatment of
;	true color on PS is not consistent with 
;	the treatment of true color on X/WIN.
;	On X/WIN the color of lines or letters 
;	is a long integer between 0 and 16777215
; 	allowing "real" true colors. On postscript
;	(even if opened with bits=8) the color_index
;	of lines and letters is an integer between
;	0 and 255. true_color_index allows
;	a consistent treatment:
;	
;	idl> plot,[0,1],color=true_color_index(255,255,0)
;
;	creates a yellow plot on PS and X/WIN.
;	
;	BUT: FOR IT,  IT MUST MODIFY THE COLORTABLE!
; 	So you cannot use it together with the predefined
;	color tabels if idl. I prefer this method, allowing
;	me explicitly defining the colors I want.
;	Forget "idl> device,decomposed=0" ! 
;
; 	The expence for sqeezing  the full color space into 
;	256 indicees is a reduced variability in grey values
; 	In fact there are only 6 different greys (including 
;       black and white) this can be changed with the bw keyword
;
; MODIFICATION HISTORY:
;       Written by Rene Preusker, 2000
;	+ added PS workaround  (R.Preusker April 2000)
;
; Copyright (C) 2000, Freie Universitaet Berlin
; This software may be used, copied, or redistributed as long
; as it is not sold and this copyright notice is reproduced on
; each copy made.  This routine is provided as is without any
; express or implied warranties whatsoever.
;
;-


if (!d.flags and 1) eq 1 then begin ; device has scalable pixel size
	   	

	if not keyword_set(bw) then begin
		rta=bytscl(REBIN(byte(indgen(256) mod 6),256))
		gta=bytscl(REBIN(byte((indgen(256)/6) mod 6),256))
		bta=bytscl(REBIN(byte((indgen(256)/36) mod 6),256)) 
		tvlct,rta,gta,bta

		rr=(round(r/51) >0 )< 5.	
		gg=(round(g/51) >0 )< 5.	
		bb=(round(b/51) >0 )< 5.	

		in=round(rr+6*(gg+6*bb))
		;help,in
		return,in
		
	endif else begin
		
		tvlct,indgen(256),indgen(256),indgen(256)
		return,round((r+g+b)/3.)<255
		
	endelse
		
endif else return,long((r<255))+256l*(long((g<255))+256l*long((b<255)))	
			
			
end
