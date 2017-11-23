@./bw_to_color.pro
@./make_color_bar.pro
;----------------------------------------------------
; View2d by Max Reuter
;Stefans version 
;anders ist
;	- vertikale color_bar und cool_color sind hier default
;		- keine colorbar erwünscht; color_bar = 0 setzen
;		- keine farben erwünscht  ; keyword bw setzen
;	- default horizontal bar_size von 0.25 auf 0.15 gesetzt
;	- alle bar keywords haben hier ein underscore, ist einfach konsistenter, finde ich.
;	- brewer colortables sind über keyword ladbar
;	- map_set kann zusätzlich geladen werden um ländergrenzen und küstenlinien,etc anzuzeigen
;		- funktioniert nur bei globalen, regulären Gittern (equal angle), und bei bekannten anderen wie CLAAS , Cloud_CCI L3U Europe 
;		- MSG geht wie immer über MAX's coast_line() und meridians() zusammen mit keyword "MSG"

;andere keywords 
; 	bw 			- anstatt cool_color
;	data_idx 	- zusätzlich zu no_data_idx
;	bar_l_eq	- erzeugt "kleinergleich" Zeichen an colorbar (setzt l_eq keyword bei make_color_bar.pro)
;	bar_g_eq	- erzeugt "größergleich" Zeichen an colorbar (setzt g_eq keyword bei make_color_bar.pro)
;	bar_tickname- setzt tickname für color bar
;	brewer		- nimm brewer color_table
; 	geo plots (wie map_image) funktioniert nur für reguläre gitter (lon:[-180,180],lat[-90,90]) oder msg, benutzt map_set
;		continents- setzt kontinent linien
;		countries - setzt Grenzen und küstenlienen
;		box_axes  - "box-style" Achsen
; 		coasts	  - markiert zusätzlich seen und Inseln (continents oder countries muss hierfür auch gesetzt sein)
; 		rivers	  - markiert zusätzlich flüsse (continents oder countries muss hierfür auch gesetzt sein)
; 		USA		  - markiert zusätzlich "States" Grenzen von USA und CANADA (continents oder countries muss hierfür auch gesetzt sein)
; 		limit	  - setzt das Gebiet (wie map_image: [lat1,lon1,lat2,lon2])
;		lowres_bounderies - per default werden die hoch aufgelösten Grenzen gesetzt, das kann hiermit verhindert werden (deaktiviert automatisch coasts keyword)
;		_extra	  - theoretisch sind alle map_set,map_grid und map_continent keywords durchschleifbar
;------------------------------------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;----------------------------------------------------
function adv_keyword_set, keyword
	return, (size(keyword,/type) < 1)
end
;----------------------------------------------------
function lnot, x
        return, (not x) and 1b
end
;------------------------------------------------------------------------------------------
function is_valid_idx, index

	if n_elements(index) gt 0 then begin
		if n_elements(index) eq 1 and index[0] eq -1 then return, 0b
		return, 1b
	endif

	return, 0b
    
end
;------------------------------------------------------------------------------------------
; by M. Reuter ; adapted to regular grids by stapel
function coast_line, lon, lat, border = border, rivers = rivers, lakes = lakes, msg = msg, found = found, hires = hires

	; at the moment only regular grids
	proz = 0.8
	x_0  = 0 & x_1 = (size(lon,/dim)-1)[0]
	y_0  = 0 & y_1 = (size(lon,/dim)-1)[1]
	scale   = 1
	found = 1
	
	slon  = min(lon[where(lon ne -999.)],max=elon)
	slat  = min(lat[where(lat ne -999.)],max=elat)

	gridx = float(x_1)/float(abs(slon-elon))
	gridy = float(y_1)/float(abs(slat-elat))

	;filename bauen
	res = keyword_set(hires) ? 'high' : 'low'
	b_ndx_file = FILEPATH('b'+res+'.ndx', SUBDIR=['resource', 'maps',res])
	b_dat_file = FILEPATH('b'+res+'.dat', SUBDIR=['resource', 'maps',res])
	p_ndx_file = FILEPATH('p'+res+'.ndx', SUBDIR=['resource', 'maps',res])
	p_dat_file = FILEPATH('p'+res+'.dat', SUBDIR=['resource', 'maps',res])
	c_ndx_file = FILEPATH('c'+res+'.ndx', SUBDIR=['resource', 'maps',res])
	c_dat_file = FILEPATH('c'+res+'.dat', SUBDIR=['resource', 'maps',res])
	r_ndx_file = FILEPATH('r'+res+'.ndx', SUBDIR=['resource', 'maps',res])
	r_dat_file = FILEPATH('r'+res+'.dat', SUBDIR=['resource', 'maps',res])

	if file_test(b_ndx_file) and file_test(p_dat_file) and $
	   file_test(r_ndx_file) and file_test(c_ndx_file) then begin
		filez_ndx = [p_ndx_file]
		if keyword_set(border) then filez_ndx = [b_ndx_file,filez_ndx]
		if keyword_set(rivers) then filez_ndx = [r_ndx_file,filez_ndx]
		if keyword_set(lakes)  then filez_ndx = [c_ndx_file,filez_ndx]
	endif else begin
		print, res +'res map information not found! Try ',(keyword_set(hires) ? 'low' : 'high')
		found = 0
		return, -1
	endelse

	;einlesen
	dum_vector	= 0b

	for i = 0l, n_elements(filez_ndx) - 1l do begin
		openr, lun, filez_ndx[i], /xdr, /get_lun
		nsegs	= 0l
		readu, lun, nsegs
		ndx = replicate({fptr:0l, npts:0l, maxlat:0., minlat:0., maxlon:0., minlon:0.}, nsegs)
		readu, lun, ndx
		free_lun, lun,/force
		file_dat	= file_name_info(filez_ndx[i], /path, /name) + '.dat'
		openr, lun, file_dat, /xdr, /stream
		for j = 01, n_elements(ndx) - 1l do begin
			point_lun, lun, ndx[j].fptr
			xy	= fltarr(2, ndx[j].npts)
			readu, lun, xy
			if keyword_set(msg) then begin
				dum     = geo_to_msg(reform(xy[1, *]), reform(xy[0, *]), /unique_projection, /no_round,hrv=hrv)
				x       = dum.column
				y       = dum.line
			endif else begin
				x= fix((reform(xy[1, *])+(-1.0)*slon)*gridx)
				idx=where(x lt 0,idxcnt)
				if idxcnt gt 0 then x[idx] = x_1 - x[idx]
				y= fix((reform(xy[0, *])+(-1.0)*slat)*gridy)
			endelse
			ok		= ((x ge x_0) and (y ge y_0) and (x le x_1) and (y le y_1))
			ok_idx		= where(ok, ok_anz)
			if ok_anz le 1 then continue
			x[ok_idx]	= (x[ok_idx] - x_0) * scale
			y[ok_idx]	= (y[ok_idx] - y_0) * scale

			; zusatzliche split_idxe finden
			ok_x = x[ok_idx]
			ok_y = y[ok_idx]
			n_x  = n_elements(ok_x)
			dum = ok_x[1:n_x-1]-ok_x[0:n_x-2]
			split_x =where(abs(dum) gt proz * x_1 ) ; ?? Prozent von max x, testweise
			n_y =n_elements(ok_y)
			dum = ok_y[1:n_y-1]-ok_y[0:n_y-2]
			split_y =where(abs(dum) gt proz * y_1 )
			split_idx = [-1l, split_x, split_y, uniq(ok_idx - lindgen(n_elements(ok_idx)))]
			split_idx = split_idx[ree(split_idx,/sort)]

			for k = 0l, n_elements(split_idx) - 2l do begin
				von		= split_idx[k] + 1l
				bis		= split_idx[k+1l]
				plot_idx	= ok_idx[von:bis]
				if n_elements(plot_idx) gt 2 then begin
					dum_data   = reform(transpose([[[x[plot_idx]]],[[y[plot_idx]]]]))
					dum_vector = is_eq(dum_vector, 0b) ? ptr_new(dum_data) : $
					[dum_vector, ptr_new(dum_data)]
				endif
			endfor
		endfor
		free_lun, lun,/force
	endfor
	if is_eq(dum_vector, 0b) then begin
		found = 0
		return, -1
	endif

	return, dum_vector
end
;------------------------------------------------------------------------------------------
; by M. Reuter ; adapted to regular grids by stapel
function meridians, lon, lat, interval = interval, msg= msg, found = found, grid_res = grid_res

	scale = 1
	found = 1
	; at the moment only regular grids allowed
	if keyword_set(lon) and keyword_set(lat) then begin
		x_0   = 0 & x_1 = (size(lon,/dim)-1)[0]
		y_0   = 0 & y_1 = (size(lon,/dim)-1)[1]
		slon  = min(lon[where(lon ne -999.)],max=elon)
		slat  = min(lat[where(lat ne -999.)],max=elat)
		gridx = float(x_1)/float(abs(slon-elon))
		gridy = float(y_1)/float(abs(slat-elat))
	endif else if keyword_set(grid_res) then begin
		x_0   = 0 & x_1 = 360./float(grid_res) -1.
		y_0   = 0 & y_1 = 180./float(grid_res) -1.
		slon  = -180. & elon = 180.
		slat  = - 90. & elat =  90.
		gridx = 1./grid_res
		gridy = 1./grid_res
	endif else begin
		print,'Need Lon/lat or regular grid size Info!'
		found = 0
		return,-1
	endelse

	dum_vector	= 0b
	resolution	= 1000
	interval	= keyword_set(interval) ? interval : 10.
	anz		= (180.*2) / interval + 1.
	lo		= findgen(anz) / (anz - 1) * (2*180.) - (2*90.)
	la		= findgen(anz) / (anz - 1) * (2*180.) - (2*90.)
	for i = 0l, n_elements(lo) - 1l do begin
		;erstellen der meridiane
		m_la		= fltarr(2, resolution)
		m_la[1, *]	= la[i]
		m_la[0, *]	= vector(slat,elat,resolution)
		m_lo		= fltarr(2, resolution)
		m_lo[0, *]	= lo[i]
		m_lo[1, *]	= vector(slon,elon,resolution)
		if keyword_set(msg) then begin
			dum_lo          = geo_to_msg(reform(m_lo[1, *]),reform(m_lo[0, *]),/unique_projection,/no_round,hrv=hrv)
			x_lo            = dum_lo.column
			y_lo            = dum_lo.line
			dum_la          = geo_to_msg(reform(m_la[1, *]),reform(m_la[0, *]),/unique_projection,/no_round,hrv=hrv)
			x_la            = dum_la.column
			y_la            = dum_la.line
		endif else begin
; 			x_lo= fix((reform(m_lo[1, *])+(-1.0)*slon)*gridx)
; 			y_lo= fix((reform(m_lo[0, *])+(-1.0)*slat)*gridy)
; 			x_la= fix((reform(m_la[1, *])+(-1.0)*slon)*gridx)
; 			y_la= fix((reform(m_la[0, *])+(-1.0)*slat)*gridy)
			x_lo= fix((reform(m_lo[1, *])+(-1.0)*slon)*gridx)
			y_lo= fix((reform(m_lo[0, *])+(-1.0)*slon)*gridx)
			x_la= fix((reform(m_la[1, *])+(-1.0)*slat)*gridy)
			y_la= fix((reform(m_la[0, *])+(-1.0)*slat)*gridy)
		endelse
		x		= x_lo
		y		= y_lo
		ok		= ((x ge x_0) and (y ge y_0) and (x le x_1) and (y le y_1))
		ok_idx	= where(ok, ok_anz)
		if ok_anz gt 0 then begin
			x[ok_idx]	= (x[ok_idx] - x_0) * scale
			y[ok_idx]	= (y[ok_idx] - y_0) * scale
			split_idx	= [-1l, uniq(ok_idx - lindgen(n_elements(ok_idx)))]
			for k = 0l, n_elements(split_idx) - 2l do begin
				von			= split_idx[k] + 1l
				bis			= split_idx[k+1l]
				plot_idx	= ok_idx[von:bis]
				if n_elements(plot_idx) gt 2 then begin
					dum_data	= $
					{data:reform(transpose([[[x[plot_idx]]],[[y[plot_idx]]]])), $
					type:'lat', ang:round(m_la[1, 0] * 100.)/100.}
					dum_vector	= is_eq(dum_vector, 0b) ? ptr_new(dum_data) : $
					[dum_vector, ptr_new(dum_data)]
				endif
			endfor
		endif

		x		= x_la
		y		= y_la
		ok		= ((x ge x_0) and (y ge y_0) and (x le x_1) and (y le y_1))
		ok_idx		= where(ok, ok_anz)
		if ok_anz gt 0 then begin
			x[ok_idx]	= (x[ok_idx] - x_0) * scale
			y[ok_idx]	= (y[ok_idx] - y_0) * scale
			split_idx	= [-1l, uniq(ok_idx - lindgen(n_elements(ok_idx)))]
			for k = 0l, n_elements(split_idx) - 2l do begin
				von			= split_idx[k] + 1l
				bis			= split_idx[k+1l]
				plot_idx	= ok_idx[von:bis]
				if n_elements(plot_idx) gt 2 then begin
					dum_data	= $
					{data:reform(transpose([[[x[plot_idx]]],[[y[plot_idx]]]])),$
					type:'lon', ang:round(m_lo[0, 0] * 100.)/100.}
					dum_vector	= is_eq(dum_vector, 0b) ? ptr_new(dum_data) :$
					[dum_vector, ptr_new(dum_data)]
				endif
			endfor
		endif
	endfor
	if is_eq(dum_vector, 0b) then begin
		found = 0
		return, -1
	endif

	return, dum_vector
end
;------------------------------------------------------------------------------------------
;for the coast lines etc, map_set will be used (only on known regular grid sizes!)
;map_set, keywords can be parsed via _extra keywords
;countries = countries,lakes = lakes,coasts = coasts,p0lon = p0lon,p0lat = p0lat, hires = hires, etc
;------------------------------------------------------------------------------------------
pro view2d, bild, aspect = aspect, _extra = _extra, bw = bw, color_bar = color_bar, mini = mini, maxi = maxi		, $
		col_table = col_table, dont_scale = dont_scale, col_center = col_center, position = position				, $
		bar_title = bar_title, bar_format = bar_format, bar_nlev = bar_nlev, bar_size = bar_size					, $
		bar_dist = bar_dist, bar_discontinous = bar_discontinous, bar_g_eq = bar_g_eq, bar_l_eq = bar_l_eq			, $
		no_data_idx = no_data_idx, no_data_value = no_data_value, no_data_col = no_data_col, no_axes = no_axes		, $
		full_screen = full_screen, logarithmic = logarithmic, space_value = space_value, space_col = space_col		, $
		coast_vec = coast_vec, coast_color = coast_color, coast_thick = coast_thick, coast_style = coast_style		, $
		coast_idx = coast_idx, boxed = boxed, msg = msg, space_idx = space_idx, meridian_vec = meridian_vec			, $
		meridian_col = meridian_col, meridian_format = meridian_format, meridian_thick = meridian_thick				, $
		meridian_style = meridian_style, meridian_nonumber = meridian_nonumber,meridian_interval = meridian_interval, $
		noerase = noerase, range = range, data_idx = data_idx,coast_psym = coast_psym, bar_position = bar_position	, $
		bar_tickname = bar_tickname, brewer = brewer,continents = continents, countries=countries,limit = limit		, $
		box_axes = box_axes, lowres_bounderies = lowres_bounderies

	bar_nlev		= keyword_set(bar_nlev)				? bar_nlev		: 4
	bar_dist		= keyword_set(bar_dist)				? bar_dist		: 0.
	bar_format  	= keyword_set(bar_format) 			? bar_format	:'(f10.2)'
	col_table		= adv_keyword_set(col_table)		? col_table 	: 2
	coast_style		= keyword_set(coast_style)			? coast_style	: 0
	meridian_style	= adv_keyword_set(meridian_style)	? meridian_style: 1
	color_bar		= adv_keyword_set(color_bar) 		? color_bar		: 1
	bw 				= keyword_set(bw)					? bw			: 0
	_extra			= keyword_set(_extra) 				? _extra		: {dummy:'extra'}
	; we want high resolution coasts and bounderies
	if ~keyword_set(lowres_bounderies) and ~is_tag(_extra,'hires') then _extra = create_struct(_extra,{hires:1})

	if adv_keyword_set(bild) then a = reform(bild) else begin & print, 'nothing to view2d -> return' & return & endelse
	;check dimensions, switch [3,x,y] -> [x,y,3]
	s_img = size(a)
	true  = 0
	if (s_img[0] lt 2) or (s_img[0] gt 3) then begin 
		print, 'Image have to be 2D or 3D'
		return
	endif else if s_img[0] eq 3 then begin
		true = 1
		if (s_img[1] ne 3) and (s_img[3] ne 3) then begin
			print, 'at least one dim has to be of size 3'
			return
		endif
		if s_img[1] eq 3 then a = transpose(a, [1, 2, 0])
	endif

	if keyword_set(continents) or keyword_set(limit) or keyword_set(box_axes) or keyword_set(countries) then begin
		grid_res = get_grid_res(a, claas=claas, cci_l3u_eu=cci_l3u_eu, cci_l3u_af=cci_l3u_af) ; check if "a" is of regular (equal angle) grid size
		p0lon = is_tag(_extra,'P0lon') ? _extra.p0lon : 0.
		p0lat = 0.; so far we do nothing if p0lat is set.
		if grid_res ne 0 or cci_l3u_eu or cci_l3u_af or keyword_set(msg) or keyword_set(claas) then begin
			si  = size(a,/dim)
			if keyword_set(msg) and ~keyword_set(claas) then begin
				xrange = [0,si[0]-1] & yrange=[0,si[1]-1]
				msg_x = (indgen((xrange[1]-xrange[0])+1)+xrange[0]) #  (intarr((yrange[1]-yrange[0])+1)+1)
				msg_y = (indgen((yrange[1]-yrange[0])+1)+yrange[0]) ## (intarr((xrange[1]-xrange[0])+1)+1)
				dum = msg_to_geo(temporary(msg_x),temporary(msg_y),scale_params=scale_params,sub_sat_lon=sub_sat_lon)
				lon = dum.lon
				lat = dum.lat
				msg = 1
				free, dum
				found_geo = 1
				slon  = min(lon[where(lon ne -999.)],max=elon)
				slat  = min(lat[where(lat ne -999.)],max=elat)
				meridian_interval = keyword_set(meridian_interval) ? meridian_interval : 30
			endif else begin
				slon  =-180. & elon = 180.
				slat  = -90. & elat =  90.
				found_geo = 0
				coast_vec = keyword_set(continents) or keyword_set(countries)
				if keyword_set(limit) then box_axes = 1
				meridian_vec = keyword_set(box_axes)
				use_map_set  = keyword_set(coast_vec) or keyword_set(meridian_vec)
				if keyword_set(claas) then begin
					grid_res = 0.05
					slon     =  -90. & elon = 90.
					slat     =  -90. & elat = 90.
					p0lon    =    0.
					limit    = keyword_set(limit) ? limit : [slat,slon,elat,elon]
					meridian_interval = keyword_set(meridian_interval) ? meridian_interval : 30
				endif
				if keyword_set(cci_l3u_eu) then begin
					grid_res = 0.02
					slon     =  -15. & elon = 45.
					slat     =   35. & elon = 75.
					p0lon    =   15.
					limit    = keyword_set(limit) ? limit : [slat,slon,elat,elon]
				endif
				if keyword_set(cci_l3u_af) then begin
					grid_res = 0.01
					slon     =  -20. & elon = 30.
					slat     =    0. & elat = 20.
					p0lon    =    5.
					limit    = keyword_set(limit) ? limit : [slat,slon,elat,elon]
				endif
				;we have to shift the array if p0lon is set
				if p0lon ne 0 then begin
					if keyword_set(limit) then begin
						;what about limit???? What we do now is:
						;We do not shift and we set p0lon to center of limit to avoid inconsistencies
						p0lon = total(limit[[1,3]])/2.
					endif else begin
						a=shift(a,fix(p0lon/(-360.) * si[0]),0)
					endelse
				endif
			endelse
			if keyword_set(limit) then begin
				if n_elements(limit) eq 8 then limit = [[6,1,2,5]]; reshape map_set 8 elements limit vector to map_set 4 element limit vector
				if n_elements(limit) eq 4 then begin
					limit = [limit[0] > slat, limit[1] > slon, limit[2] < elat, limit[3] < elon] ; make sure limit bounds are not greater than array
					if is_valid_idx(no_data_idx) or is_valid_idx(data_idx) then dum = float(reform(a[*,*,0])*0)
					meridian_interval = keyword_set(meridian_interval) ? meridian_interval : 10
					xx  = 0 > fix((limit[[1,3]]-slon)*1./grid_res-1) < (si[0]-1)
					yy  = 0 > fix((limit[[0,2]]-slat)*1./grid_res-1) < (si[1]-1)
					a   = a[xx[0]:xx[1],yy[0]:yy[1],*]
					if found_geo then begin
						lon = lon[xx[0]:xx[1],yy[0]:yy[1]]
						lat = lat[xx[0]:xx[1],yy[0]:yy[1]]
					endif
					if is_valid_idx(no_data_idx) or is_valid_idx(data_idx) then begin
						if is_valid_idx(no_data_idx) then begin
							dum[no_data_idx] = 123.
							dum = dum[xx[0]:xx[1],yy[0]:yy[1]]
							no_data_idx = where(dum eq 123.)
						endif else begin
							dum[data_idx] = 123.
							dum = dum[xx[0]:xx[1],yy[0]:yy[1]]
							no_data_idx = where(dum ne 123.)
							free, data_idx
						endelse
						free,dum
					endif
				endif
			endif else begin
				xx  = [0,si[0]]
				yy  = [0,si[1]]
				meridian_interval = keyword_set(meridian_interval) ? meridian_interval : 30
			endelse
			if found_geo then begin
				rivers = is_tag(_extra,'coasts')
				lakes  = is_tag(_extra,'lakes')
				hires  = ~keyword_set(lowres_bounderies)
				coast_vec = coast_line(lon,lat,border=countries,lakes=lakes,rivers=rivers,msg=msg,found=found,hires=hires)
				if found eq 0 then free, coast_vec
				meridian_vec = meridians(lon,lat,interval=meridian_interval,msg=msg,found=found)
				if found eq 0 then free, meridian_vec
			endif
			if keyword_set(box_axes) then begin
				meridian_nonumber = 1
				_extra = create_struct(_extra,{xtickformat:"(A1)",ytickformat:"(A1)",xticklen:0.0001,yticklen:0.0001})
			endif
			if ~is_tag(_extra,'LONDEL') then _extra = create_struct(_extra,{londel:meridian_interval})
			if ~is_tag(_extra,'LATDEL') then _extra = create_struct(_extra,{latdel:meridian_interval})
		endif
	endif

	if color_bar eq 1 then begin
		;vertical colorbar
		if stregex(strjoin(tag_names(_extra)) , 'XMARGIN', /fold,/bool) then xmargin = _extra.xmargin else $
		xmargin = keyword_set(boxed) or keyword_set(no_axes) ? [6,3] : [6,3] ; default = [7,3]
		if stregex(strjoin(tag_names(_extra)) , 'YMARGIN', /fold,/bool) then ymargin = _extra.ymargin else $
		ymargin = keyword_set(boxed) or keyword_set(no_axes) ? [2,2] : [3,3] ; default = [4,2]
		if stregex(strjoin(tag_names(_extra)) , 'XTIT', /fold,/bool) and ~keyword_set(no_axes) then ymargin += [2,0]
		if keyword_set(bar_title) then xmargin += [2,0]
	endif
 	if color_bar eq 2 then begin
		;horizontal colorbar
		if stregex(strjoin(tag_names(_extra)) , 'XMARGIN', /fold,/bool) then xmargin = _extra.xmargin else $
		xmargin = keyword_set(boxed) or keyword_set(no_axes) ? [3,3] : [10,4] ; default = [7,3]
		if stregex(strjoin(tag_names(_extra)) , 'YTIT', /fold,/bool) then xmargin += [4,0]
		if stregex(strjoin(tag_names(_extra)) , 'YMARGIN', /fold,/bool) then ymargin = _extra.ymargin else $
		ymargin = keyword_set(boxed) or keyword_set(no_axes) ? [2,2] : [6,2] ; default = [4,2]
		if stregex(strjoin(tag_names(_extra)) , 'XTIT', /fold,/bool) then ymargin += [4,0]
		if keyword_set(bar_title) then ymargin += [2,0]
	endif
	if keyword_set(msg) then begin
		no_axes	= 1
		aspect	= 1
	endif
	if keyword_set(full_screen) then begin
		no_axes = 1
		color_bar = 0
	endif
	if color_bar eq 0 then begin
		; no color bar
		; just like true color images
		if stregex(strjoin(tag_names(_extra)) , 'XMARGIN', /fold,/bool) then xmargin = _extra.xmargin else $
		xmargin = keyword_set(boxed) or keyword_set(no_axes) ? [6,3] : [7,3] ; default = [7,3]
		if stregex(strjoin(tag_names(_extra)) , 'YTIT', /fold,/bool) then xmargin += [4,0]
		if stregex(strjoin(tag_names(_extra)) , 'YMARGIN', /fold,/bool) then ymargin = _extra.ymargin else $
		ymargin = keyword_set(boxed) or keyword_set(no_axes) ? [2,2] : [3,2]
		if stregex(strjoin(tag_names(_extra)) , 'XTIT', /fold,/bool) then ymargin += [4,0]
	endif
	if is_tag(_extra,'TITLE') then begin
		ymargin += [0,1]
		;same with ytitle,xtitle
		if keyword_set(box_axes) then begin
			ymargin += [0,1]
			_extra.title = '!C!C!C!C!C'+_extra.title
		endif
	endif

	;	testen ob bild 2 oder 3 dimensionen hat	=> true_dim & bildgroessen (six, siy) & bar (0: nix, 1:vertikal, 2:horizontal) & mini, maxi
	s_img = size(a)
	if (s_img[0] lt 2) or (s_img[0] gt 3) then begin & print, 'Image have to be 2D or 3D' & return & endif
	if s_img[0] eq 2 then true = 0 else true = 1
	if true then begin
		true_dim = where(s_img[1:3] eq 3, dum)
		true_dim = true_dim[0] + 1
		if dum lt 1 then begin & print, 'at least one dim have to got size of 3' & return & endif
		case true_dim of
			1:begin & six = float(s_img[2]) & siy = float(s_img[3]) & end
			2:begin & six = float(s_img[1]) & siy = float(s_img[3]) & end
			3:begin & six = float(s_img[1]) & siy = float(s_img[2]) & end
		endcase
		if is_valid_idx(data_idx) then begin
			dum = bytarr(six, siy)
			if data_idx[0] ne -1 then dum[data_idx] = 1
			no_data_idx = where(dum ne 1)
		endif
	endif else begin
		six = float(s_img[1])
		siy = float(s_img[2])
		true_dim = 0
		if is_valid_idx(data_idx) then begin
			dum = bytarr(six, siy)
			if data_idx[0] ne -1 then dum[data_idx] = 1
			no_data_idx = where(dum ne 1)
		endif
		if adv_keyword_set(no_data_value) then no_data_idx = where(a eq no_data_value[0])
		if is_valid_idx(no_data_idx) then begin
			data_idx = bytarr(six, siy)
			if no_data_idx[0] ne -1 then data_idx[no_data_idx] = 1
			data_idx = where(data_idx ne 1)
			if data_idx[0] eq -1 then begin & print, 'nothing to view2d -> return' & return & endif
			fin_idx = where(finite(a[data_idx]), fin_cou, complement = nan_idx, ncomplement = nan_cou)
			if fin_cou le 0 then begin & print, 'nothing to view2d -> return' & return & endif
			data_idx = data_idx[fin_idx]
			if nan_cou gt 0 then no_data_idx = [no_data_idx, data_idx[nan_idx]]
		endif else begin
			fin_idx = where(finite(a), fin_cou, complement = nan_idx, ncomplement = nan_cou)
			if fin_cou le 0 then begin & print, 'nothing to view2d -> return' & return & endif
			if nan_cou gt 0 then begin
				no_data_idx	= nan_idx
				data_idx	= fin_idx
			endif
		endelse

		mini = adv_keyword_set(mini) ? make_array(1,type=size(a,/type),val=mini[0]) : (is_valid_idx(no_data_idx) ? min(a[data_idx]) : min(a))
		maxi = adv_keyword_set(maxi) ? make_array(1,type=size(a,/type),val=maxi[0]) : (is_valid_idx(no_data_idx) ? max(a[data_idx]) : max(a))
		mini = keyword_set(logarithmic) ? alog10(mini) : mini
		maxi = keyword_set(logarithmic) ? alog10(maxi) : maxi
		; nur um sicher zu gehen das bw_to_color keinen Unsinn macht !!!! 
		mini = mini[0]
		maxi = maxi[0]
	endelse

	bar = (keyword_set(color_bar) and true_dim eq 0) ? color_bar : 0

	plot, [0,0], [1,1], /nodata, xstyle=4, ystyle = 4, noerase = noerase, xmargin = xmargin, ymargin = ymargin;,_extra=_extra

	case bar of
		0:		begin & xscale = 1.	& yscale = 1.							& end
		1:		begin & xscale = 1. - (keyword_set(bar_size) ? bar_size : 0.25)	 & yscale = 1.		& end
  		2:		begin & xscale = 1.	& yscale = 1. - (keyword_set(bar_size) ? bar_size : 0.15)	& end
		else:		begin & xscale = 1.	& yscale = 1.							& end
	endcase
	px = (keyword_set(full_screen) ? [0., 1.] : (!x.window)) * !d.x_vsize		; Get size of window in device units
	py = (keyword_set(full_screen) ? [0., 1.] : (!y.window)) * !d.y_vsize		; Get size of window in device units
	if keyword_set(position) then begin
		px[0] = px[0] + position[0] * (px[1] - px[0])
		px[1] = px[0] + position[2] * (px[1] - px[0])
		py[0] = py[0] + position[1] * (py[1] - py[0])
		py[1] = py[0] + position[3] * (py[1] - py[0])
	endif
	swx = (px[1]-px[0])	* xscale						; Size in x in device units
	swy = (py[1]-py[0])	* yscale						; Size in Y
	aspi = six / siy									; Image aspect ratio
	aspw = swx / swy									; Window aspect ratio
	f = aspi / aspw										; Ratio of aspect ratios
	if keyword_set(aspect) then if f ge 1.0 then swy = swy / f else swx = swx * f

;	bild in richtige 3d form bringen
	case true_dim of
		0:	begin
				dum = a
; 				dum = keyword_set(logarithmic) ? alog10(a) : a
				if keyword_set(logarithmic) then begin
					logidx = where(a le 0.,logcnt)
					if logcnt gt 0 then dum[logidx] = 1
					dum = alog10(dum)
				endif
				if keyword_set(bw) then begin
					im = ((maxi - mini) ne 0) ? byte((255l * ((mini > dum < maxi) - mini) / (maxi - mini))) : dum * 0
					im = [[[(im)]], [[(im)]], [[(im)]]]
				endif else begin
					im = bw_to_color(dum, mini = mini, maxi = maxi, col_table = col_table, dont_scale = dont_scale, col_center = col_center, brewer = brewer)
				endelse
				if keyword_set(logarithmic) then begin
					if logcnt gt 0 then begin
						im1 = float(reform(im[*,*,0]))
						im2 = float(reform(im[*,*,1]))
						im3 = float(reform(im[*,*,2]))
						im1[logidx]=!values.f_nan
						im2[logidx]=!values.f_nan
						im3[logidx]=!values.f_nan
						im = [[[(im1)]], [[(im2)]], [[(im3)]]]
					endif
				endif
			end
		1:	im = transpose(a, [1, 2, 0])
		2:	im = transpose(a, [0, 2, 1])
		3:	im = a
	endcase
	if (true_dim gt 0l) and lnot(keyword_set(dont_scale)) then im = bytscl(im)
	if is_valid_idx(no_data_idx) then begin
		for i = 0, 2 do begin
			dum_img = im[*, *, i]
			dum_img[no_data_idx] = adv_keyword_set(no_data_col) ? no_data_col[i < (n_elements(no_data_col) - 1)]:127
			im[*, *, i] = dum_img
		endfor
	endif
	if is_valid_idx(coast_idx) then begin
		for i = 0, 2 do begin
			dum_img = im[*, *, i]
			dum_img[coast_idx] = adv_keyword_set(coast_col) ? coast_col[i < (n_elements(coast_col) - 1)]:0
			im[*, *, i] = dum_img
		endfor
	endif
	if adv_keyword_set(space_value) then space_idx = where(a eq space_value)
	if is_valid_idx(space_idx) then begin
		for i = 0, 2 do begin
			dum_img = im[*, *, i]
			dum_img[space_idx] = adv_keyword_set(space_col)?space_col[i < (n_elements(space_col) - 1)]:255
			im[*, *, i] = dum_img
		endfor
	endif

;	bild reinmalen
	if (strlowcase(!d.name) ne 'win') and (strlowcase(!d.name) ne 'x') and (strlowcase(!d.name) ne 'z') then begin	;Scalable pixels?
;		Postscriptausgabe
		tv, im, px[0] + swx / xscale * (1. - xscale), py[0] + swy / yscale * (1. - yscale), $
			xsize = swx, ysize = swy, /device, true = 3
	endif else begin
;		Bildschirmausgabe
		dum_img = bytarr(swx, swy, 3)
		for i = 0, 2 do dum_img[*, *, i] = congrid(im[*, *, i], swx, swy)
		tv, dum_img, $
			floor(px[0] + swx /  xscale * (1. - xscale)), floor(py[0] + swy/yscale * (1. - yscale)), true = 3
	endelse

;	Colorbar reinmalen
	case bar of
	1:	begin
			bar_pos = keyword_set(bar_position) ? bar_position : $
				[(px[0]+swx/xscale * (1. - xscale - bar_dist - 0.20))/!d.x_vsize, py[0]/!d.y_vsize	 , $
				 (px[0]+swx/xscale * (1. - xscale - bar_dist - 0.18))/!d.x_vsize, (py[0]+swy)/!d.y_vsize]
			if not stregex(strjoin(tag_names(_extra)) , 'YTIT', /fold,/bool) then bar_pos += [.04,0,.04,0]
			make_color_bar, mini, maxi, bar_nlev, /noerase, logarithmic = logarithmic, continous = lnot(keyword_set(bar_discontinous)) 	, $
				pos = bar_pos, orient = 1, bw = bw, bar_title = bar_title, col_table = col_table, dont_scale = dont_scale		, $
				col_center = col_center, bar_format = bar_format, _extra = _extra, g_eq = bar_g_eq, l_eq = bar_l_eq			, $
				bar_tickname=bar_tickname, brewer = brewer, charsize = charsize
		end
	2:	begin
			bar_pos = keyword_set(bar_position) ? bar_position : $
				[ px[0]/!d.x_vsize, py[0]/!d.y_vsize, (px[0]+swx/xscale)/!d.x_vsize , $
				 (py[0]+swy/yscale*(1 - yscale - 0.1))/!d.y_vsize]
				bar_pos += [((bar_pos[2]-bar_pos[0]) *(.05)),-.01,((bar_pos[2]-bar_pos[0]) *(-.05)),-.01]
				if stregex(strjoin(tag_names(_extra)) , 'XTIT', /fold,/bool) then bar_pos += [0,-.04,0,-.04]
			make_color_bar, mini, maxi, bar_nlev, /noerase, logarithmic = logarithmic, continous = lnot(keyword_set(bar_discontinous))	, $
				pos = bar_pos, orient = 0, bw = bw, bar_title = bar_title, col_table = col_table, dont_scale = dont_scale		, $
				col_center = col_center, bar_format = bar_format, _extra = _extra, g_eq = bar_g_eq, l_eq = bar_l_eq			, $
				bar_tickname=bar_tickname, brewer = brewer, charsize = charsize, charthick = charthick
		end
	else:
	endcase

;	Achsen reinmalen
	x_0 = 0.
	x_1 = six
	y_0 = 0.
	y_1 = siy
	if keyword_set(range) then begin
		if n_elements(range) eq 4 then begin
			x_0 = range[0]
			x_1 = range[1]
			y_0 = range[2]
			y_1 = range[3]
		endif
	endif
	pos = [	px[0] + swx/xscale * (1. - xscale)	, $
		py[0] + swy/yscale * (1. - yscale)	, $
		px[0] + swx/xscale			, $
		py[0] + swy/yscale			]
	if lnot(keyword_set(no_axes) or keyword_set(boxed)) then begin
		plot,[x_0, x_1], [y_0, y_1], xrange = [x_0, x_1], yrange = [y_0, y_1], /noerase, /nodata, pos = pos, /dev, $
			xstyle = 1, ystyle = 1, xticklen = 0.02, yticklen = 0.02, xgridstyle = 0, ygridstyle = 0, _extra = _extra
	endif else begin
 		plot,[x_0, x_1], [y_0, y_1], xrange = [x_0, x_1], yrange = [y_0, y_1], /noerase, /nodata, pos = pos, /dev, $
 			xstyle = keyword_set(boxed) ? 1:5, ystyle = keyword_set(boxed) ? 1:5, xticks = 1, yticks = 1     , $
			xtickname = [' ',' '], ytickname = [' ',' '], _extra = _extra
	endelse
	
	if keyword_set(use_map_set) then begin
		;We need mapping coordinates to draw coasts or meridians, map_set will do this for us
		pos = [	(px[0]+swx/xscale * (1. - xscale))/!d.x_vsize, py[0]/!d.y_vsize	 , $
					(px[0]+swx/xscale)/!d.x_vsize,(py[0]+swy)/!d.y_vsize]
		rot = 0.
		limit = keyword_set(limit) ? limit : [-90,p0lon-180.,90,p0lon+180.]
		map_set,p0lat[0],p0lon[0],rot,pos=pos,/noerase,/noborder,limit=limit
	endif

	if keyword_set(coast_vec) then begin
		if ~keyword_set(use_map_set) then begin
			;coast_line reinmalen
			if ptr_valid(coast_vec[0]) then begin
				for i = 0l, n_elements(coast_vec) -1l do begin
					oplot, (*coast_vec[i])[0, *] + .5, (*coast_vec[i])[1, *] + .5, $
					color = coast_col, thick = coast_thick, linestyle = coast_style, psym = coast_psym
				endfor
			endif else begin
				print,'Coast vector must be of type pointer!'
			endelse
		endif else begin
			map_continents, hires=hi_res, continents=continents,countries=countries,usa=usa,rivers=rivers,coasts=coasts,color=coast_color,$
			mlinethick = coast_thick, mlinestyle = coast_style,_extra=_extra
			if is_tag(_extra,'USA') then begin
				; if USA state borders are wanted we include Canada as well! basta!
				canada_shape 	= FILEPATH('canadaprovince.shp', SUBDIR=['resource', 'maps','shape'])
				cgDrawShapes, canada_shape, color = coast_color, thick = coast_thick, linestyle = coast_style
			endif
		endelse
	endif

	;meridiane reinmalen
	if keyword_set(meridian_vec) then begin
		if ~keyword_set(use_map_set) then begin
			meridian_format = keyword_set(meridian_format) ? meridian_format : '(i4)'
			;lage und typ der meridiane bestimmen
			dum_mx		= fltarr(n_elements(meridian_vec))
			dum_my		= fltarr(n_elements(meridian_vec))
			dum_type	= bytarr(n_elements(meridian_vec))
			dum_ang		= strarr(n_elements(meridian_vec))
			dum_crap	= bytarr(n_elements(meridian_vec))
			for i = 0l, n_elements(meridian_vec) -1l do begin
				dum_type[i]	= ((*meridian_vec[i]).type eq 'lon')
				dum_ang[i]	= string((*meridian_vec[i]).ang, format = meridian_format)
				if (dum_type[i] eq 1) then begin
					if ( (strcompress(dum_ang[i],/rem) eq '-180') or (strcompress(dum_ang[i],/rem) eq '180' ) ) then dum_ang[i] = ''
				endif else begin
					if ( (strcompress(dum_ang[i],/rem) eq '-90' ) or (strcompress(dum_ang[i],/rem) eq '90'  ) ) then dum_ang[i] = ''
				endelse
				dum_x		= ((*meridian_vec[i]).data)[0, *]
				dum_y		= ((*meridian_vec[i]).data)[1, *]
				if dum_type[i] eq 0 then begin
					;finde dichtesten pkt auf y achse in bildmitte
					dum		= min(abs(dum_x - (x_1 - x_0) / 2.), pos_min)
					dum_mx[i]	= dum_x[pos_min]
					dum_my[i]	= dum_y[pos_min]
				endif else begin
					dum		= min(abs(dum_y - (y_1 - y_0) / 2.), pos_min)
					dum_mx[i]	= dum_x[pos_min]
					dum_my[i]	= dum_y[pos_min]
				endelse
				dum_crap[i]	= (pos_min eq 0) or (pos_min eq (n_elements(dum_x) - 1l))
			endfor
			for i = 0l, n_elements(meridian_vec) -1l do begin
				oplot, ((*meridian_vec[i]).data)[0, *], ((*meridian_vec[i]).data)[1, *], color = meridian_col, $
				linestyle = meridian_style, thick = meridian_thick
				if not keyword_set(meridian_nonumber) then begin
					if not dum_crap[i] then begin
						if dum_type[i] 	then	xyouts, dum_mx[i], dum_my[i] + 0, dum_ang[i], color = meridian_col, alignment = .5 $
								else	xyouts, dum_mx[i] + 0, dum_my[i], dum_ang[i], color = meridian_col, alignment = .5
					endif
				endif
			endfor
		endif else begin
			;meridian_number nonumber ,etc
			map_grid, label = 1,color=meridian_col,glinestyle=meridian_style,glinethick=meridian_thick,box_axes=box_axes,_extra=_extra
		endelse
	endif

	return
end
