; NAME:
;	unix2ymdhms.pro
; PURPOSE:
;	Converts unixseconds into time with year, month, day, hour, minute and second
;
; CALLING SEQUENCE:
;	result = unix2ymdhms(unixseconds)
; 
; 	result will be a string with year+'/'+month+'/'+day+'  '+hour+':'+minu+':'+sec
; 
; 	keywords : 
; 		array  ... returns [year,month,day,hour,minute,seconds] instead of string 
; 		stu_mi ... returns hour+':'+minu string (used, e.g. in predict_sat.pro)
;		ystart ... year where time in seconds started, e.g. unixsecondes startet at 1970 (default) also possible ystart=1993 ;DAI 
;
; MODIFICATION HISTORY:
;	Written by S. Stapelberg, 2007.
;	stapel 2009	seconds are now rounded. Fixes problems when
; 			julday creates seconds like 14.99999999 instead of 15
; 
function unix2ymdhms, unix_seconds, stu_mi = stu_mi, array = array, ystart = ystart

	unix_secs = keyword_set(unix_seconds) ? long64(unix_seconds) : long64(0)

	; allowed range of julday

	if not between(unix_secs[0],-210961368000,157722624359993) then return,-1l

	julianDate = julday(1,1,(keyword_set(ystart) ? ystart : 1970),0,0,0) + ( double(unix_secs) / 86400d )
	caldat, julianDate, month, day, year, hour, minute, seconds

	year  = string(year ,format='(i4.4)')
	month = string(month,format='(i2.2)')
	day   = string(day  ,format='(i2.2)')
	hour  = string(hour ,format='(i2.2)')
	minu  = string(minute mod 60,format='(i2.2)')
	sec   = string(round(seconds) mod 60,format='(i2.2)')

	if keyword_set(array)  then return, transpose([[year],[month],[day],[hour],[minu],[sec]])
	if keyword_set(stu_mi) then return, hour+':'+minu

	return, year+'/'+month+'/'+day+'  '+hour+':'+minu+':'+sec

end