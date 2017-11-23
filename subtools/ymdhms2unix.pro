; NAME:
;	ymdhms2unix.pro
; PURPOSE:
;	Converts time (year, month, day, hour, minute, seconds) into unixseconds
;
; CALLING SEQUENCE:
;	result = ymdhms2unix(year, month, day, hour, minute, seconds)
;
; MODIFICATION HISTORY:
;	Written by S. Stapelberg, 2007.
;	stapel 2009	result is now rounded. Fixes problems when
; 			julday creates seconds like 14.99999999 instead of 15
;	stapel 2013	introduce keyword ystart , choose startyear other than 1970 (unixseconds)
 
function ymdhms2unix, year, month, day, hour, minute, seconds, ystart = ystart

	case n_params() of
		0l	: return,  long(systime(1))
		1l	: begin
				if size(year[0],/type) eq 7 then begin
					dd = strmid(year,[0,4,6,8,10,12],[4,2,2,2,2,2])
					juldate = julday(dd[1,*] > 1,dd[2,*] > 1,dd[0,*],dd[3,*],dd[4,*],dd[5,*])
				endif else juldate = julday(1,1,year,0,0,0)
			  end
		2l	: juldate = julday(month,1,year,0,0,0)
		3l	: juldate = julday(month,day,year,0,0,0)
		4l	: juldate = julday(month,day,year,hour,0,0)
		5l	: juldate = julday(month,day,year,hour,minute,0)
		6l	: juldate = julday(month,day,year,hour,minute,seconds)
		else	: return,-1l
	endcase

	return, round( ( juldate - julday(1,1,(keyword_set(ystart) ? ystart : 1970),0,0,0) ) * 86400, /l64)

end