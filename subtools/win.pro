pro win , i,xsize=xsize,ysize=ysize,_extra=_extra,fullscreen=fullscreen
;-------------------------------------------------------------
;+
; NAME:
;       win
; PURPOSE:
;       win is a combination of wset and window
;	if the window i already exists wset is used
;	otherwise the window will be created.
; 	if device is postscript, nothing happens
;
; CATEGORY: 
;	Graphics
;
; CALLING SEQUENCE:
;	win,1
;
; KEYWORDS:  
;	all window/wset keywords
;
; MODIFICATION HISTORY:
;       Written by R. Preusker, Jan, 2001.
;
; Copyright (C) 2001, Freie Universitaet Berlin
; This software may be used, copied, or redistributed as long
; as it is not sold and this copyright notice is reproduced on
; each copy made.  This routine is provided as is without any
; express or implied warranties whatsoever.
;
; rene@amor.met.fu-berlin.de
;-
; stapel 2012 
; 	- added fullscreen keyword

if STRLOWCASE(!d.name) eq "ps" then return
if not (keyword_set (xsize)) then xsize=720
if not (keyword_set (ysize)) then ysize=450
if keyword_set(fullscreen) then begin
	dum = GET_SCREEN_SIZE()
	xsize=dum[0]
	ysize=dum[1]
endif
if (i eq 0) and (!d.window eq -1) then window,i,xsize=xsize,ysize=ysize,_extra=_extra

catch, errorstat

if errorstat ne 0 then	window,i,xsize=xsize,ysize=ysize,_extra=_extra else wset,i

end	
