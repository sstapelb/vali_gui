;---------------------------------------------------------------------------------------------------
;routine:		msg_to_geo
;description:	converts x, y msg coordinates to lon, lat coordinates
;				bases on eumetsat document: doc no.: cgms 03; issue: 2.6; date: 19990812; chapter 4.4
;				note:  idl/fub counting(default): 0,0 = left,bottom; eumetsat counting: 1,1 = right,bottom)
;author: 		max reuter
;
; Stapel 11/2011
;Nach Änderung der lfac und cfac auf -13644183 für nicht HRV 
;und kompletter Umstellung der Berechnung auf double precision floating point 
;konnten die Daten aus Madison (fast) komplett reproduziert werden.
;Spricht dafuer das Madison richtig ist und wir jetzt auch. To be validated!
;minmax(FUB - Madison) :
;vorher 
;lon :  [-2.5991111   , 2.5991111]    ; 108042 pixel haben eine Differenz grösser als 0.1°
;lat  : [ -0.88948098 , 0.88948098]   ; 8838     pixel haben eine Differenz grösser als 0.1°
;
;nach Änderung der lfac/cfac
;lon :  [-0.24470689  , 0.24470689]   ; 56 pixel haben eine Differenz grösser als 0.1°
;lat :  [-0.060058986 , 0.060058986]  ; keine Pixel haben eine Differenz grösser als 0.1°
;
;nach Umstellung auf double
;lon :  [-0.0040235913, 0.0040235913] ; keine Pixel haben eine Differenz grösser als 0.1°  
;lat :  [-0.0013813477, 0.0013813477] ; keine Pixel haben eine Differenz grösser als 0.1°
;---------------------------------------------------------------------------------------------------

function msg_to_geo, col, lin, hrv = hrv		, $
	scale_params		= scale_params		, $
	sub_sat_lon 		= sub_sat_lon		, $
	eumetsat_counting	= eumetsat_counting	, $
	structure_in		= structure_in

	!except = 0 ;report no math errors as nan values will be fetched at the end of the procedure (not nice but ugly)

	if keyword_set(structure_in) then begin
		col = structure_in.column
		lin = structure_in.line
	endif

	;line and column count starts at 0 (idl style) not at 1 (eumetsat style)
	;constats
	; EUM/MSG/SPE/057 Issue 6, 21 June 2006
	; MSG pixel sampling: delta = 251.53d/3d [µrad] for non HRV ,delta = 251.53d/9d [µrad] for HRV 
	; cfac/lfac = -( 2^16 / (delta / 1000000) ) * !dtor [deg^-1]
	; print, - ( 2d^16 / ((251.53d/3d)/1000000d) ) * (!dpi/180d), f='(f20.8)'
	; lfac/cfac (HRV Kanal): -40927010, in allen xmlfiles bisher -40927014 !?!
	sub_lon	= keyword_set(sub_sat_lon)  ? sub_sat_lon     : 0d
	coff	= keyword_set(scale_params) ? scale_params[0] : keyword_set(hrv) ? 5568d:1856d
	loff	= keyword_set(scale_params) ? scale_params[1] : keyword_set(hrv) ? 5568d:1856d
	cfac	= keyword_set(scale_params) ? scale_params[2] : keyword_set(hrv) ? -40927014d:-13642337d ;-13644183. geaendert auf Zahl in xml files (stapel 11/2011)
	lfac	= keyword_set(scale_params) ? scale_params[3] : keyword_set(hrv) ? -40927014d:-13642337d ;-13644183. 

	;convert to idl counting starting from 0 in the south/west
	;(eumetsat: starting from 1 in the south/east)
	if not keyword_set(eumetsat_counting) then loff = loff - 1
	;inverse scaling function (section4.4.4)
; 	x		= (col - coff) / cfac * 65536. * !dtor; * (1. + 2.715 / 1000.)
; 	y		= (lin - loff) / lfac * 65536. * !dtor
	x	= (col - coff) / cfac * 65536d * (!dpi/180d)
	y	= (lin - loff) / lfac * 65536d * (!dpi/180d)

	;convert to idl counting starting from 0 in the south/west
	;(eumetsat: starting from 1 in the south/east)
	if not keyword_set(eumetsat_counting) then x = -temporary(x)

	;normalized geostationary inverse projection (section4.4.3.2)
	cosx	= cos(x)
	siny	= sin(y)
	dum1	= (1d + .00680297763d * siny^2)		; 1.00680297763 := req^2/rpol^2
	dum2	= cosx * cos(y)
	sn	= (42164d * dum2 - sqrt((42164d * dum2)^2d - dum1 * 1737121856d)) / temporary(dum1) ; sathöhe+ req
	s1	= 42164d - sn * temporary(dum2)
	s2	= sn * sin(temporary(x)) * cos(temporary(y))

	lat 	= atan(- 1.00680297763d * temporary(sn) * temporary(siny) / sqrt(s1^2d + s2^2d)) / (!dpi/180d)
	lon 	= atan(temporary(s2) / temporary(s1)) / (!dpi/180d) + sub_lon

	;set no data at points behind the disk and where an math error was detected
	crap_idx	= where(lnot(finite(lon)) or lnot(finite(lat)), crap_anz)
	if crap_anz gt 0 then lon[crap_idx] = -999d
	if crap_anz gt 0 then lat[crap_idx] = -999d

	;return the result
	return, {lon: lon, lat: lat}
end
;---------------------------------------------------------------------------------------------------
;EXAMPLES
;---------------------------------------------------------------------------------------------------
pro msg_to_geo_ex01
	o	= obj_new('msg_data_cl')
	c	= indgen(3712) # (intarr(3712) + 1)
	l	= indgen(3712) ## (intarr(3712) + 1)
	o -> set_date, 2006, 08, 01, 12, 00
	r	= o -> get_product('rgb')
	stop
	x0	= 1500
	y0	= 2600
	x1	= 2900
	y1	= 3300
	c0	= c[x0: x1, y0: y1]
	l0	= l[x0: x1, y0: y1]
	r0	= reform(r[0, x0: x1, y0: y1])
	dum = msg_to_geo(c0, l0)
	lo	= dum.lon
	la	= dum.lat
	dum = 0b
	map_image, r0, la, lo, /box
	obj_destroy, o
	stop
end
;---------------------------------------------------------------------------------------------------













