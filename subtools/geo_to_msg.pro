;---------------------------------------------------------------------------------------------------
;routine:		geo_to_msg
;description:	converts lat, lon to msg x, y coordinates (see msg_to_geo)
;				bases on eumetsat document: doc no.: cgms 03; issue: 2.6; date: 19990812; chapter 4.4
;				keywords:	height: height [km] of target above earth elipsoid (optional)
;				note:  idl/fub counting(default): 0,0 = left,bottom; eumetsat counting: 1,1 = right,bottom)
;author: 		max reuter
;---------------------------------------------------------------------------------------------------
function geo_to_msg, lon, lat, hrv = hrv		, $
	 scale_params		= scale_params		, $
	 sub_sat_lon 		= sub_sat_lon		, $
	 unique_projection	= unique_projection 	, $ ;obsolete keyword, now only unique projections are allowed
	 no_round		= no_round		, $
	 eumetsat_counting	= eumetsat_counting	, $
	 height			= height		, $
	 structure_in		= structure_in

	!except = 0 ;report no math errors as nan values will be fetched at the end of the procedure (not nice but ugly)

	if keyword_set(structure_in) then begin
		lon = structure_in.lon
		lat = structure_in.lat
	endif

	;constats
	sub_lon	= keyword_set(sub_sat_lon)  ? sub_sat_lon     : 0d
	coff	= keyword_set(scale_params) ? scale_params[0] : keyword_set(hrv) ? 5568d:1856d
	loff	= keyword_set(scale_params) ? scale_params[1] : keyword_set(hrv) ? 5568d:1856d
	cfac	= keyword_set(scale_params) ? scale_params[2] : keyword_set(hrv) ? -40927014d:-13642337d ;-13644183. geaendert auf Zahl in xml files (stapel 11/2011)
	lfac	= keyword_set(scale_params) ? scale_params[3] : keyword_set(hrv) ? -40927014d:-13642337d ;-13644183. 

	;normalized geostationary projection (section4.4.3.2)
	c_lat	= atan(0.993243d * tan(lat * (!dpi/180d)))
	rl	= 6356.5838d / sqrt(1d - 0.00675701d * cos(c_lat)^2)
	if keyword_set(height) then rl += height
	r1	= 42164d - rl * cos(c_lat) * cos((lon - sub_lon) * !dtor) 
	r2	= - rl * cos(c_lat) * sin((lon - sub_lon) * !dtor)
	r3	= rl * sin(c_lat)
	rn	= sqrt(r1^2 + r2^2 + r3^2)
	x	= atan(- r2 / r1) / !dtor
	y	= asin(- r3 / rn) / !dtor

	;convert to idl counting starting from 0 in the south/west
	;(eumetsat: starting from 1 in the south/east)
	if not keyword_set(eumetsat_counting) then begin
		x		= -temporary(x)
		loff	= loff - 1
	endif

	;scaling fuction (section4.4.4)
	col		= coff + (x / 65536d * cfac)
	lin		= loff + (y / 65536d * lfac)
	if not keyword_set(no_round) then begin
		col		= fix(round(temporary(col)))
		lin		= fix(round(temporary(lin)))
	endif

	;set no data at points behind the disk and where an math error was detected
	crap_idx	= where((rn gt 42164d) or lnot(finite(col)) or lnot(finite(lin)), crap_anz)
	if crap_anz gt 0 then col[crap_idx] = -1
	if crap_anz gt 0 then lin[crap_idx] = -1
	
	;return the result
	return, {column:col, line:lin}
end
