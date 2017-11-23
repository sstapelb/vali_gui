FUNCTION clean_up_viaz,viaz,viel_in,meris=meris,lat = lat 
; From Rene Preusker 
; this function cleans some artefacts in MERIS and AATSR 
; viewing azimut, that comes from tie-point interpolation 
; (which is not aplicable for non-steady (at Nadir) functions!) 
; the solution is a secondorder polynom left and right 
; from the Nadir point.  
; 
; Inputs: 
;-------------- 
; viaz        viewing azimuth in degree 
; viel      viewing elevation (90° = nadir for AATSR) 
;        or viewing zenith (0° = nadir for MERIS) 
;        booth in degree 
; meris     keyword that switches between AATSR and MERIS 
; 
;Output:     
;-------------- 
; viaz        viewing azimuth 
;        with a nice discontinuity at Nadir 
 
 
	if keyword_set(meris) then begin 
		viel=90.-viel_in 
		lim=85 
	endif else begin 
		viel=viel_in 
		lim=85 
	endelse 

	si =size(viaz) 
	out=viaz 
	if si[0] eq 1 then nlines = 0 else nlines = si[2]
	for i=0,nlines-1 do begin  ; going over all lines
		nidx=where(viel[*,i] eq 0,ncount) 
		dum=min(abs(1.-cosd(90.-viel[*,i])),mitte) 
		; mitte is the index of the point closest to Nadir  
	
		;Correction for left side of orbit 
		;bla=viaz[0:mitte,i]-shift(viaz[0:mitte,i],1) 
		;find_jump = where(abs(bla[1:*]) ge 50.,count_jump) ;around North Pole 
	
		vielleft = viel[0:mitte,i]  
	
		;find_neg = where(vielleft lt 0 
		idx=where(vielleft lt lim and vielleft gt 1,count) 
		;if count_jump eq 2 then begin 
		;   find_idx = where(idx lt find_jump[0] or idx gt find_jump[1],count_idx) 
		;   if count_idx ge 1 then idx = idx[find_idx] 
		;endif 
	
		if count gt 2 then begin 
			;06.11.2015 sstapelb changed "if lat ge 70." to "if min(lat[*,i]) ge 70."
			if min(lat[*,i]) ge 70. then par=poly_fit(viel[idx,i],abs(viaz[idx,i]),2) else par=poly_fit(viel[idx,i],viaz[idx,i],2) 
			out[0:mitte,i]=par[0]+par[1]*viel[0:mitte,i]+par[2]*(viel[0:mitte,i])^2 
			;if count_jump eq 2 then out[find_jump[0]:find_jump[1],i] = viaz[find_jump[0]:find_jump[1],i]     
		endif 
		;Correction for right side of orbit 
		;bla=viaz[mitte:*,i]-shift(viaz[mitte:*,i],1) 
		;find_jump = where(abs(bla[1:*]) ge 50.,count_jump) ;around North Pole 
	
		vielright = viel[mitte:*,i]
		idx=mitte+where(vielright lt lim and vielright gt 1,count) 
		;if count_jump eq 2 then begin 
		;find_idx = where(idx lt find_jump[0] or idx gt find_jump[1],count_idx) 
		;if count_idx ge 1 then idx = idx[find_idx] 
		;endif 
	
		if count gt 2 then begin 
			par=poly_fit(viel[idx,i],viaz[idx,i],2) 
			out[mitte:*,i]=par[0]+par[1]*viel[mitte:*,i]+par[2]*(viel[mitte:*,i])^2 
			;if count_jump eq 2 then out[find_jump[0]:find_jump[1],i] = viaz[find_jump[0]:find_jump[1],i]     
		endif
		if ncount gt 0 then out[nidx,i]=0. 
	endfor 
	return,out 
END 

function read_aatsr_data, file, view=view,found=found,main_only=main_only


; file='/cmsaf/cmsaf-cld6/esa_cci_cloud_data/AATSR/2008/07/01/ATS_TOA_1PUUPA20080701_094113_000065272070_00007_33127_6370.nc'
	view = keyword_set(view) ? view : 'nadir' ; or 'fward'

	read_ncdf,file,'reflec_'+view+'_0670',rad1,fillv,found=found
	if found then begin
		idx = where(rad1 lt 0,idxcnt)
		if idxcnt gt 0 then rad1[idx]=-999.
	endif
	read_ncdf,file,'reflec_'+view+'_0870',rad2,fillv,found=found
	if found then begin
		idx = where(rad2 lt 0,idxcnt)
		if idxcnt gt 0 then rad2[idx]=-999.
	endif
	read_ncdf,file,'reflec_'+view+'_1600' ,rad3a,fillv,found=found
	if found then begin
		idx = where(rad3a lt 0,idxcnt)
		if idxcnt gt 0 then rad3a[idx]=-999.
	endif
	read_ncdf,file,'btemp_'+view+'_0370' ,rad3b,fillv,found=found
	if found then begin
		idx = where(rad3b lt 0,idxcnt)
		if idxcnt gt 0 then rad3b[idx]=-999.
	endif
	read_ncdf,file,'btemp_'+view+'_1100' ,rad4,fillv,found=found
	if found then begin
		idx = where(rad4 lt 0,idxcnt)
		if idxcnt gt 0 then rad4[idx]=-999.
	endif
	read_ncdf,file,'btemp_'+view+'_1200' ,rad5_uc,fillv,found=found
	if found then begin
		idx = where(rad5_uc lt 0,idxcnt)
		if idxcnt gt 0 then rad5_uc[idx]=-999.
	endif
	read_ncdf,file,'lon' ,lon,fillv,found=found
	if found then begin
		idx = where(lon eq fillv[0],idxcnt)
		if idxcnt gt 0 then lon[idx]=-999.
	endif
; 	read_ncdf,file,'lon_corr_'+view ,lon_c,fillv,found=found
; 	if found then begin
; 		idx = where(lon_c eq fillv[0],idxcnt)
; 		if idxcnt gt 0 then lon_c[idx]=-999.
; 	endif
	read_ncdf,file,'lat' ,lat,fillv,found=found
	if found then begin
		idx = where(lat eq fillv[0],idxcnt)
		if idxcnt gt 0 then lat[idx]=-999.
	endif
; 	read_ncdf,file,'lat_corr_'+view ,lat_c,fillv,found=found
; 	if found then begin
; 		idx = where(lat_c eq fillv[0],idxcnt)
; 		if idxcnt gt 0 then lat_c[idx]=-999.
; 	endif
	read_ncdf,file,'view_elev_'+view ,view_elev,fillv,found=found
	if found then begin
		idx = where(view_elev eq fillv[0],idxcnt)
		if idxcnt gt 0 then view_elev[idx]=-999.
	endif
	read_ncdf,file,'sun_elev_'+view ,sunza,fillv,found=found
	if found then begin
		idx = where(sunza eq fillv[0],idxcnt)
		if idxcnt gt 0 then sunza[idx]=-999.
	endif
	read_ncdf,file,'view_azimuth_'+view,satazi_uc,fillv,found=found
	if found then begin
		idx_satazi = where(satazi_uc eq fillv[0],idxcnt_satazi)
		if idxcnt_satazi gt 0 then satazi_uc[idx_satazi]=-999.
	endif
	read_ncdf,file,'sun_azimuth_'+view,sunazi,fillv,found=found
	if found then begin
		idx = where(sunazi eq fillv[0],idxcnt)
		if idxcnt gt 0 then sunazi[idx]=-999.
	endif

	; convert elevation angles into zenith angles
	idx = where(view_elev eq -999.,idx_cnt)
	satza = 90 - view_elev
	if idx_cnt gt 0 then satza[idx] = -999.
	idx = where(sunza eq -999.,idx_cnt)
	sunza = 90 - sunza
	if idx_cnt gt 0 then sunza[idx] = -999.

	; correct lon/lat
; 	idx = where(lon_uc eq -999. or lon_c eq -999.,idxcnt)
; 	lon = lon_uc + lon_c
; 	if idxcnt gt 0 then lon[idx] = -999.
; 	idx = where(lat_uc eq -999. or lat_c eq -999.,idxcnt)
; 	lat = lat_uc + lat_c
; 	if idxcnt gt 0 then lat[idx] = -999.
; 
; 	idx = where(lon lt -180.,idxcnt)
; 	if idxcnt gt 0 then lon[idx] += 360.
; 	idx = where(lon gt  180.,idxcnt)
; 	if idxcnt gt 0 then lon[idx] -= 360.

	;correct 12.0 µm channel (non linearity correction)
	rad5 = correct_aatsr_ch7(rad5_uc)
	if keyword_set(main_only) then free, rad5_uc

	;Correct artifical problems arising around nadir point for satellite azimuth angles for AATSR
	satazi = -180 > clean_up_viaz(satazi_uc,temporary(view_elev),lat = lat) < 180
	if idxcnt_satazi gt 0 then satazi[idx_satazi]=-999.
	if keyword_set(main_only) then free, satazi_uc

	;relative azimuth
	nd_idx = where(sunazi eq -999. or satazi eq -999.,nd_cnt)
	relazi = abs  (sunazi - satazi)
	idx_gt = where(relazi gt 180.,cnt_gt,complement=idx_le,ncomplement=cnt_le )
	if cnt_gt gt 0. then relazi[idx_gt] -= 180.
	if cnt_le gt 0. then relazi[idx_le] = 180. - relazi[idx_le]
	if nd_cnt gt 0. then relazi[nd_idx] = -999.
	; calculate glint_angle
	if ~keyword_set(main_only) then begin
		nd_idx = where(sunza eq -999. or satza eq -999. or relazi eq -999.,nd_cnt)
		glint_angle = acosd( (-1.0 > ( cosd(sunza) * cosd(satza) + sind(sunza) * sind(satza) * cosd(relazi) < 1.0 )))
		if nd_cnt gt 0. then glint_angle[nd_idx] = -999.

		; calculate glint_angle (uncorrected satazi)
		nd_idx = where(sunazi eq -999. or satazi_uc eq -999.,nd_cnt)
		relazi_uc = abs(sunazi - satazi_uc)
		idx_gt = where(relazi_uc gt 180.,cnt_gt,complement=idx_le,ncomplement=cnt_le )
		if cnt_gt gt 0. then relazi_uc[idx_gt] -= 180.
		if cnt_le gt 0. then relazi_uc[idx_le] = 180. - relazi_uc[idx_le]
		if nd_cnt gt 0. then relazi_uc[nd_idx] = -999.
		nd_idx = where(sunza eq -999. or satza eq -999. or relazi_uc eq -999.,nd_cnt)
		glint_angle_uc = acosd( (-1.0 > ( cosd(sunza) * cosd(satza) + sind(sunza) * sind(satza) * cosd(relazi_uc) < 1.0 )))
		if nd_cnt gt 0. then glint_angle_uc[nd_idx] = -999.

		illum = byte(sunza * 0 -127)
		illum[where(sunza ge 0 and sunza lt 80)] = 1
		illum[where(between(sunza,80,90))] = 2
		illum[where(sunza gt 90)] = 3
	endif

	si = size(lon,/dim)
	start_date = get_ncdf_data_by_name(file,'start_date',/global_attribute)
	  dum  = strsplit(start_date,/ext) 
	  dum1 = strsplit(dum[0],/ext,'-') 
	  dum2 = strsplit(dum[1],/ext,':')
	  bin_date = bin_date(strjoin(['??',dum1[1],dum1[0],dum[1],dum1[2]],' '))
	  startu = double(ymdhms2unix(bin_date[0],bin_date[1],bin_date[2],bin_date[3],bin_date[4]))+double(dum2[2])
	stop_date  = get_ncdf_data_by_name(file,'stop_date',/global_attribute)
	  dum  = strsplit(stop_date,/ext)
	  dum1 = strsplit(dum[0],/ext,'-')
	  dum2 = strsplit(dum[1],/ext,':')
	  bin_date = bin_date(strjoin(['??',dum1[1],dum1[0],dum[1],dum1[2]],' '))
	  endeu = double(ymdhms2unix(bin_date[0],bin_date[1],bin_date[2],bin_date[3],bin_date[4]))+double(dum2[2])

	date  = strjoin(unix2ymdhms(startu,/arr))
	ltime = vector(startu[0],endeu[0],double(si[1]))
	time  = (fltarr(si[0])+1) # ltime
	free, ltime

	if keyword_set(main_only) then begin
		out = {	time:time,lon:lon,lat:lat,satza:satza,sunza:sunza,relazi:relazi, $
				rad1:rad1,rad2:rad2,rad3a:rad3a,rad3b:rad3b,rad4:rad4,rad5:rad5}
	endif else begin
		out = {	date:date, start_date:start_date,stop_date:stop_date,time:time,lon:lon,lat:lat,$
				rad1:rad1,rad2:rad2,rad3a:rad3a,rad3b:rad3b,rad4:rad4,rad5:rad5,rad5_uc:rad5_uc,satza:satza,sunza:sunza,$
				relazi:relazi,relazi_uc:relazi_uc,sunazi:sunazi,satazi:satazi,satazi_uc:satazi_uc,illum:illum,$
				glint_angle:glint_angle,glint_angle_uc:glint_angle_uc}
	endelse

	return,out

end