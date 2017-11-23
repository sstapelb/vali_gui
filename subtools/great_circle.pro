; lon1,lat1,lon2,lat2 input in degree
; great circle  in m or rad für eine Kugel von rene
; oder mit keyword wgs84 mit wgs84 referenzsystem für die Erde
function great_circle, lon1, lat1, lon2, lat2, rad = rad, sign = sign, wgs84 = wgs84, track = track

	if n_params() ne 4 then begin
		print, 'Syntax: Result = great_circle(lon1,lat1,lon2,lat2,rad=rad,sign=sign,wgs84=wgs84)'
		return,-1l
	endif

; 	rr  = 6378388.d0 ; äquatorradius des älteren Hayford-Ellipsoids von 1924
	rr  = 6378137.d0 ; äquatorradius große halbachse Geodätisches Referenzsystem 1980 (auch wgs84)
	; input umrechnen in rad
	lat1_r = double(lat1) * (!dpi/180d)
	lon1_r = double(lon1) * (!dpi/180d)
	lat2_r = double(lat2) * (!dpi/180d)
	lon2_r = double(lon2) * (!dpi/180d)

	!except = 0 ; nans werden gecheckt

	sign = keyword_set(sign) ? ((lon1_r-lon2_r) / abs(lon1_r-lon2_r)) : 1.d0

	; check nan's
	check  = where(~finite(sign), nan)
	if nan gt 0 then sign[check] = 1.d0

;	World Geodetic System 1984
;	Abstand zwischen zwei Standorten auf der Erde angeblich auf 50 Meter genau!
	if keyword_set(wgs84) then begin
		ab = 1d / 298.257223563d ; Abplattung der Erde
		f  = (lat1_r + lat2_r) / 2d
		g  = (lat1_r - lat2_r) / 2d
		l  = (lon1_r - lon2_r) / 2d
		s  = sin(g)^2 * cos(l)^2 + cos(f)^2 * sin(l)^2
		c  = cos(g)^2 * cos(l)^2 + sin(f)^2 * sin(l)^2
		w  = atan(sqrt(s/c))
		d  = 2 * w * rr
		r  = sqrt(s * c) / w
		h1 = (3d * r - 1d) / (2d * c)
		h2 = (3d * r + 1d) / (2d * s)
		dist = d * ( 1d + ab * h1 * sin(f)^2 * cos(g)^2 - ab * h2 * cos(f)^2 * sin(g)^2 )
		; check nan's
		check  = where(~finite(dist), nan)
		if nan gt 0 then dist[check] = 0.d0
	endif else dist = acos(sin(lat1_r)*sin(lat2_r) + cos(lat1_r)*cos(lat2_r)*cos(lon1_r-lon2_r)) * rr

	if keyword_set(rad) or keyword_set(track) then begin
		x = dist / rr
		if not keyword_set(track) then return, x
		kurswinkel = acos( (sin(lat2_r)-sin(lat1_r) * cos(x) ) / cos(lat1_r) * sin(x) )
		lat_nord   = ( acos( sin(abs(kurswinkel))*cos(lat1_r)) )
		lon_nord   = ( ((lon1_r-lon2_r) / abs(lon1_r-lon2_r)) * abs( acos( tan(lat1_r)/tan(lat_nord))) )
		x = [lon1_r, lon_nord, lon2_r] * (180d/!dpi)
		y = [lat1_r, lat_nord, lat2_r] * (180d/!dpi)
		track_lon = vector(lon1_r* (180d/!dpi),lon2_r * (180d/!dpi),100.)
		track_lat = interpol(y,x,track_lon,/quadr)
		return, {distance : sign * dist, track_lon : track_lon, track_lat : track_lat}
	endif

	return, sign * dist

end

