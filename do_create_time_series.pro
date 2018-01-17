pro do_create_time_series

end

; ----------------------------------------------------------------------------------------------------------------------------------------------
; do jobs
; ----------------------------------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_create_all_compare_time_series
	starttime = systime(1)
	mem_cur   = memory(/current)

; 	period   = ['2003-2011'] ; else !default_ts_period (vali_set_path.pro) will be taken

	cli      = 'cci3'; this is the climatology that will be directly compared to the reference
	ref      = ['cci','pmx','gac2'];['cci3','myd2','mod2','gac2','pmx','myd','gac','mod','cal','era','cla'];,'cci']; reference
	sat      = ['noaaam','noaapm','noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaa16','noaa17','noaa18','noaa19', $
		        'metopa','metopb','aqua','terra','aatme','aatsr','avhrrs','modises','allsat']
	data     = ['cfc','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high'];,'ctp','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high','ctt','cot','cot_liq','cot_ice',$
; 				'cer','cer_liq','cer_ice','cth','lwp','iwp','cwp','cph','cph_day','iwp_allsky','lwp_allsky','cwp_allsky']

	if is_defined(period) then begin
		print,''
		print,'### Period Keyword is set! The new period will be '+period+'. Do you want to do this?'
		stop
	endif

	set_coverage, cov

	for k=0,n_elements(ref)-1 do begin
		for i= 0,n_elements(sat)-1 do begin
			for l=0,n_elements(data)-1 do begin
				do_it = 1
				if strmid(ref[k],0,3) eq 'myd' and sat_ampm(sat[i]) eq 'am' then do_it = 0
				if strmid(ref[k],0,3) eq 'mod' and sat_ampm(sat[i]) eq 'pm' then do_it = 0
				if strmid(ref[k],0,3) eq 'myd' and total(sat[i] eq ['noaa7','noaa9','noaa11','noaa12','noaa14','noaaam']) then do_it = 0
				if strmid(ref[k],0,3) eq 'mod' and total(sat[i] eq ['noaa7','noaa9','noaa11','noaa12','noaapm']) then do_it = 0
				if ref[k] eq 'cal' and total(sat[i] eq ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaaam']) then do_it = 0
				if strmid(ref[k],0,3) eq 'era' and ~total(sat[i] eq ['terra','aqua','noaapm']) then do_it = 0
				if do_it then create_cci_vs_gac_or_aqua_time_series, data[l], cli, ref[k], sat[i], cov, period = period
			endfor
		endfor
	endfor

	caldat, systime(/utc, /julian), mo, da, ye, ho, mi, se
	dat_str	= string(da, mo, ye, ho, mi, format = '(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2,"[UTC] / ")')
	print, dat_str + 'create_all_compare_time_series -> '+string((systime(1)-starttime)/3600.,f='("Duration        : ", f7.3, " hrs")')
	print, dat_str + 'create_all_compare_time_series -> '+string(float(memory(/highwater)-mem_cur)/1024.^3,f='("Memory required : ", f7.3, " GiB")')

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_create_all_single_time_series

	starttime = systime(1)
	mem_cur   = memory(/current)
; 	period    = ['2003-2011'] ; pvir

	data      = ['cfc','ctp','cph','cot_liq','cot_ice','cer_liq','cer_ice','lwp','iwp','iwp_allsky','lwp_allsky','ctp2',$ 
				'cfc_day','cfc_night','cfc_twl','cfc_low','cfc_mid','cfc_high','cph_day','ctt','ctt2',$
				'cer','cot','cth','cth2','cwp','cwp_allsky','sal']; etc.
	; PVIR vars
; 	data      = ['cfc','ctp','cph','cot_liq','cot_ice','cer_liq','cer_ice','lwp','iwp','iwp_allsky','lwp_allsky'] 

	; coll6 only
; 	data     = [	'iwp_16','lwp_16','cot_16_liq','cer_16_liq','cot_16_ice','cer_16_ice', $
; 					'iwp_37','lwp_37','cot_37_liq','cer_37_liq','cot_37_ice','cer_37_ice'];, $
; 					'cwp_16','cwp_37','cot_16','cer_16','cot_37','cer_37',$
; 					'iwp_16_allsky','lwp_16_allsky','cwp_16_allsky','iwp_37_allsky','lwp_37_allsky','cwp_37_allsky']
	; calipso only
; 	data = [ 'ctp_mean_all', 'cfc_allclouds', 'ctp_mean_liq', 'ctp_mean_ice', 'ctp_mean_th_ice', 'ctp_mean_sc_liq',  'cfc_allclouds_max', $
; 		 'cfc_cloudsgt01', 'cfc_cloudsgt02', 'cfc_cloudsgt03', 'cfc_allclouds_day', 'cfc_allclouds_night']
	; gac2 only
; 	data = ['refl1','refl2','rad3b','rad4','rad5','refl3a']
	; cci only
; 	data = ['nobs_cloudy','nretr_cloudy','nretr_cloudy_day','nobs_cloudy_day','nretr_cloudy_day_liq','nretr_cloudy_day_ice','nobs','nobs_day'];, $
; 			'cla_vis006','cla_vis008','cla_vis006_liq','cla_vis006_ice','cla_vis008_liq','cla_vis008_ice']

	;sensors and algorithmen
	avh_list = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaa16','noaa17','noaa18','noaa19','metopa','metopb','noaaam','noaapm']
	; cci
	cci_list = ['cci-'+avh_list,'cci-aqua','cci-terra','cci-aatsr','cci-atsr2','cci-aatme','cci-atsrs','cci-avhrrs','cci-modises','cci-allsat']
	; cci3
	cci3_list = ['cci3-'+avh_list,'cci3-aatsr','cci3-atsr2','cci3-avhrrs','cci3-allsat']
	; gac
	gac_list = ['gac-allsat','gac-'+avh_list]
	; gac2
	gac2_list = ['gac2-'+avh_list,'gac2-allsat']
	; pmx
	pmx_list = ['pmx-'+avh_list]
	; coll6
	coll6_list = ['myd2-','mod2-']
	; coll5
	coll5_list = ['myd-','mod-']
	; era-interim
	era_list = ['era-','era2-']
	; calipso
	cal_list = ['cal-']
	; claas
	cla_list = ['cla-']
	; isccp list ; isp- = Gewex AMPM (avhrrs,allsat)
	isp_list = ['isp-','isp-noaapm','isp-noaaam','isp_old-']

	; combine all you need
	algon_list = ['pmx-noaapm']

	if is_defined(period) then begin
		print,''
		print,'### Period Keyword is set! The new period will be '+period+'. Do you want to do this?'
		stop
	endif

	set_coverage, cov

	for i= 0,n_elements(algon_list)-1 do begin
		for l=0,n_elements(data)-1 do begin
			do_it = 1
			if do_it then create_time_series, data[l], algon_list[i], cov, period = period
		endfor
	endfor

	caldat, systime(/utc, /julian), mo, da, ye, ho, mi, se
	dat_str	= string(da, mo, ye, ho, mi, format = '(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2,"[UTC] / ")')
	print, dat_str + 'create_all_single_time_series -> '+string((systime(1)-starttime)/3600.,f='("Duration        : ", f7.3, " hrs")')
	print, dat_str + 'create_all_single_time_series -> '+string(float(memory(/highwater)-mem_cur)/1024.^3,f='("Memory required : ", f7.3, " GiB")')

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_hist_cloud_type_time_series, compare_to = compare_to

; 	period      = '2003-2011' ; pvir

	avh_list = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaa16','noaa17','noaa18','noaa19','metopa','metopb','noaaam','noaapm']
	; cci
	cci_list = ['cci-'+avh_list,'cci-aqua','cci-terra','cci-aatsr','cci-aatme','cci-avhrrs','cci-modises','cci-allsat']
	; cci3
	cci3_list = ['cci3-'+avh_list,'cci3-aatsr','cci3-atsr2','cci3-avhrrs','cci3-allsat']
; 	; gac
 	gac_list = ['gac-'+avh_list,'gac-allsat']
	; gac2
	gac2_list = ['gac2-'+avh_list,'gac2-allsat']
; 	; pmx
 	pmx_list = ['pmx-'+avh_list]
; 	; coll6
 	coll6_list = ['myd2-','mod2-']
; 	; coll5
 	coll5_list = ['myd-','mod-']
; 	; era-interim
 	era_list = ['era-','era2-']

	; combine all you need
	algon_list = 'era2-'

	;read default variable
	vali_set_defaults
	per     = keyword_set(period) ? period : !default_ts_period
	yspl    = fix(strsplit(per,'-',/ext))
	years   = string(indgen(yspl[1]-yspl[0]+1)+yspl[0],f='(i4.4)')
	months  = string(indgen(12)+1,f='(i2.2)')
	datestr = per

	for k = 0,n_elements(algon_list) -1 do begin
		dum = strsplit(algon_list[k],'-',/ext)
		ref = dum[0]
		vs  = ''
		if keyword_set(compare_to) then begin
			dalg = algo2ref(compare_to)
			if dalg eq 'unknown' then begin
				free, dalg
			endif else begin
				dsat = sat
				if strmid(ref,0,3) eq 'era' then dsat = 'noaapm'
				if strmid(ref,0,3) eq 'myd' then dsat = 'aqua'
				if strmid(ref,0,3) eq 'mod' then dsat = 'terra'
				vs = '_vs_'+dalg
			endelse
		endif
		apx = ''
; 		if ref eq 'cci' then dirname='/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C' else free,dirname
		sat = n_elements(dum) eq 2 ? dum[1] : ''
		datum = strarr(3,n_elements(years)*n_elements(months))
		count = 0
		for i = 0,n_elements(years) -1 do begin
			yyyy = years[i]
			print,'Reading '+sat_name(ref,sat)+' '+yyyy
			for j = 0,n_elements(months) -1 do begin
				mm = months[j]
				if is_defined(dalg) then begin
					ff = get_filename(yyyy,mm,sat=dsat,algo=dalg,level='l3c',found=ccifound,/silent)
				endif else ccifound=1
				if ccifound eq 1 then begin
					found_all = 0
					if strmid(ref,0,3) eq 'myd' or strmid(ref,0,3) eq 'mod' then begin
						tmp_all = get_data(yyyy,mm,sat=sat,algo=ref,data='hist2d_cot_ctp',found=found_all,/silent,dirname=dirname,$
								  /no_recursive)
						found_liq = 0
						found_ice = 0
					endif else begin
						tmp_liq = get_data(yyyy,mm,sat=sat,algo=ref,data='hist2d_cot_ctp_liq',found = found_liq,/silent,dirname=dirname,$
								   /no_recursive)
						tmp_ice = get_data(yyyy,mm,sat=sat,algo=ref,data='hist2d_cot_ctp_ice',found = found_ice,/silent,dirname=dirname,$
								   /no_recursive)
						if found_liq and found_ice then begin & tmp_all = (tmp_ice>0)+(tmp_liq>0) & found_all = 1 & end
					endelse
				endif else begin
					found_all = 0
					found_liq = 0
					found_ice = 0
				endelse
				if found_all then if n_elements(final_array_all) eq 0 then final_array_all = (tmp_all>0) else final_array_all += (tmp_all>0)
				if found_liq then if n_elements(final_array_liq) eq 0 then final_array_liq = (tmp_liq>0) else final_array_liq += (tmp_liq>0)
				if found_ice then if n_elements(final_array_ice) eq 0 then final_array_ice = (tmp_ice>0) else final_array_ice += (tmp_ice>0)
				if found_all then datum[0,count]=yyyy+mm
				if found_liq then datum[1,count]=yyyy+mm
				if found_ice then datum[2,count]=yyyy+mm
				count++
			endfor
		endfor

		if n_elements(final_array_all) ne 0 then begin
			idx = where(datum[0,*] ne '',idxcnt)
			actual_date = idxcnt gt 0 ? datum[0,min(idx)]+'-'+datum[0,max(idx)] : datestr
			print,'Save -> '+!SAVS_DIR +'time_series/hist2d/hist2d_cot_ctp_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			save_var,{actual_date:actual_date,bild:final_array_all},chmod = '664',$
			!SAVS_DIR + 'time_series/hist2d/hist2d_cot_ctp_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			free,final_array_all
			free,actual_date
		endif
		idx = where(datum[1,*] ne '',idxcnt)
		actual_date = idxcnt gt 0 ? datum[1,min(idx)]+'-'+datum[1,max(idx)] : datestr
		if n_elements(final_array_liq) ne 0 then begin
			print,'Save -> '+!SAVS_DIR +'time_series/hist2d/hist2d_cot_ctp_liq_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			save_var,{actual_date:actual_date,bild:final_array_liq},chmod = '664',$
			!SAVS_DIR + 'time_series/hist2d/hist2d_cot_ctp_liq_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			free,final_array_liq
			free,actual_date
		endif
		idx = where(datum[2,*] ne '',idxcnt)
		actual_date = idxcnt gt 0 ? datum[2,min(idx)]+'-'+datum[2,max(idx)] : datestr
		if n_elements(final_array_ice) ne 0 then begin
			print,'Save -> '+!SAVS_DIR +'time_series/hist2d/hist2d_cot_ctp_ice_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			save_var,{actual_date:actual_date,bild:final_array_ice},chmod = '664',$
			!SAVS_DIR + 'time_series/hist2d/hist2d_cot_ctp_ice_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
			free,final_array_ice
			free,actual_date
		endif
	endfor
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_1d_hist_time_series, compare_to = compare_to

; 	period      = '2003-2011' ; pvir

	avh_list = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaa16','noaa17','noaa18','noaa19','metopa','metopb','noaaam','noaapm']
	; cci
	cci_list = ['cci-'+avh_list,'cci-aqua','cci-terra','cci-aatsr','cci-aatme','cci-avhrrs','cci-modises','cci-allsat']
	; cci3
	cci3_list = ['cci3-'+avh_list,'cci3-aatsr','cci3-atsr2','cci3-avhrrs','cci3-allsat']
	; gac2
	gac2_list = ['gac2-'+avh_list,'gac2-allsat']
	; coll6
	coll6_list = ['myd2-','mod2-']
	; coll5
	coll5_list = ['myd-','mod-']
	; era-interim
	era_list = ['era-','era2-']

	prod_list  = ['ctp','ctt','cwp','cot','cer']

	algon_list = 'isp-noaapm'

	vali_set_defaults
	per    = keyword_set(period) ? period : !default_ts_period
	yspl   = fix(strsplit(per,'-',/ext))
	years  = string(indgen(yspl[1]-yspl[0]+1)+yspl[0],f='(i4.4)')
	months  = string(indgen(12)+1,f='(i2.2)')
	datestr = per

	for k = 0,n_elements(algon_list) -1 do begin
		dum = strsplit(algon_list[k],'-',/ext)
		ref = dum[0]
		vs  = ''
		if keyword_set(compare_to) then begin
			dalg = algo2ref(compare_to)
			if dalg eq 'unknown' then begin
				free, dalg
			endif else begin
				dsat = sat
				if strmid(ref,0,3) eq 'era' then dsat = 'noaapm'
				if strmid(ref,0,3) eq 'myd' then dsat = 'aqua'
				if strmid(ref,0,3) eq 'mod' then dsat = 'terra'
				vs = '_vs_'+dalg
			endelse
		endif
		apx = ''
; 		if ref eq 'cci' then dirname='/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C' else free,dirname
		sat = n_elements(dum) eq 2 ? dum[1] : ''
		for p = 0,n_elements(prod_list) -1 do begin
			dat = prod_list[p]
			datum = strarr(3,n_elements(years)*n_elements(months))
			count = 0
			for i = 0,n_elements(years) -1 do begin
				yyyy = years[i]
				print,'Reading hist1d_'+dat+' '+sat_name(ref,sat)+' '+yyyy
				for j = 0,n_elements(months) -1 do begin
					mm = months[j]
					if is_defined(dalg) then begin
						ff =get_filename(yyyy,mm,sat=dsat,algo=dalg,level='l3c',found=ccifound,/silent)
					endif else ccifound=1
					if ccifound eq 1 then begin
						tmp = get_data(	yyyy,mm,sat=sat,algo=ref,data='hist1d_'+dat,found = found,/silent,dirname=dirname, $
								/print_file,/no_recursive)
						if found then if n_elements(final_array) eq 0 then final_array = (tmp>0) else final_array += (tmp>0)
						if found then datum[0,count]=yyyy+mm
						tmp = get_data(	yyyy,mm,sat=sat,algo=ref,data='hist1d_'+dat+'_liq',found = found,/silent,dirname=dirname,$
								/no_recursive)
						if found then if n_elements(final_array_liq) eq 0 then final_array_liq = (tmp>0) else final_array_liq += (tmp>0)
						if found then datum[1,count]=yyyy+mm
						tmp = get_data(	yyyy,mm,sat=sat,algo=ref,data='hist1d_'+dat+'_ice',found = found,/silent,dirname=dirname,$
								/no_recursive)
						if found then if n_elements(final_array_ice) eq 0 then final_array_ice = (tmp>0) else final_array_ice += (tmp>0)
						if found then datum[2,count]=yyyy+mm
					endif
					count++
				endfor
			endfor
			if n_elements(final_array) ne 0 then begin
				idx = where(datum[0,*] ne '',idxcnt)
				actual_date = idxcnt gt 0 ? datum[0,min(idx)]+'-'+datum[0,max(idx)] : datestr
				print,'Save -> '+!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				save_var,{actual_date:actual_date,bild:final_array},chmod = '664',$
				!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				free,final_array
				free,actual_date
			endif
			if n_elements(final_array_liq) ne 0 then begin
				idx = where(datum[1,*] ne '',idxcnt)
				actual_date = idxcnt gt 0 ? datum[0,min(idx)]+'-'+datum[0,max(idx)] : datestr
				print,'Save -> '+!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_liq_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				save_var,{actual_date:actual_date,bild:final_array_liq},chmod = '664',$
				!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_liq_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				free,final_array_liq
				free,actual_date
			endif
			if n_elements(final_array_ice) ne 0 then begin
				idx = where(datum[2,*] ne '',idxcnt)
				actual_date = idxcnt gt 0 ? datum[0,min(idx)]+'-'+datum[0,max(idx)] : datestr
				print,'Save -> '+!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_ice_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				save_var,{actual_date:actual_date,bild:final_array_ice},chmod = '664',$
				!SAVS_DIR + 'time_series/hist1d/hist1d_'+dat+'_ice_'+datestr+'_'+ref+apx+vs+'_'+sat+'.sav'
				free,final_array_ice
				free,actual_date
			endif
		endfor
	endfor
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_create_hovmoeller

	plot_l3
	coverage   = ['midlat_trop','full','southern_hemisphere','northern_hemisphere','antarctica','midlat_south','tropic','midlat_north','arctic']
	data       = ['cfc','cfc_twl','cfc_low','cfc_mid','cfc_high','cfc_day','cfc_night','ctp','cot','cot_liq','cot_ice','ctt',$
		      'cth','cer','cer_liq','cer_ice','cwp','iwp','lwp','cph','cph_day','iwp_allsky','lwp_allsky','cwp_allsky','sal',$
		      'hist1d_'+['ctp','cot','ctt','cer','cwp']]
	;GAC" only
; 	data = ['rad4,rad5','rad4','rad5','rad3b','refl1','refl2']
	avh_list   = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa15','noaa16','noaa17','noaa18','noaa19','metopa','metopb','noaaam','noaapm']
	; cci
	cci_list   = ['cci-'+avh_list,'cci-aqua','cci-terra','cci-aatsr','cci-aatme','cci-avhrrs','cci-modises','cci-allsat']
	; cci3
	cci3_list = ['cci3-'+avh_list,'cci3-aatsr','cci3-atsr2','cci3-avhrrs','cci3-allsat']
	; gac
	gac_list   = ['gac-'+avh_list,'gac-allsat']
	; gac2
	gac2_list  = ['gac2-'+avh_list,'gac2-allsat']
	; pmx
	pmx_list   = ['pmx-'+avh_list]
	; coll6
	coll6_list = ['myd2-','mod2-']
	; coll5
 	coll5_list = ['myd-','mod-']
	; era-interim
	era_list = ['era-','era2-']

	; combine all you need
	algon_list = ['cci3-noaapm']
	data = ['cfc','cfc_day','cfc_night','cfc_twl','cfc_low','cfc_mid','cfc_high']

	vali_set_defaults
	per         = keyword_set(period) ? period : !default_ts_period
	yspl        = fix(strsplit(per,'-',/ext))
	years       = string(indgen(yspl[1]-yspl[0]+1)+yspl[0],f='(i4.4)')
	months      = string(indgen(12)+1   ,f='(i2.2)')
	datestr     = per
	nyears      = n_elements(years)
	nmonths     = n_elements(months)
	lat_res     = 1.
	dum_bin_val = -1
	dum_border  = -1
	for k = 0,n_elements(algon_list) -1 do begin
		dum = strsplit(algon_list[k],'-',/ext)
		ref = dum[0]
		sat    = n_elements(dum) eq 2 ? dum[1] : ''
		apx = ''
		case ref of
			'cci'		: begin & grid = 0.50 & free,dirname & end;dirname='/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C' & end
			'cci3'		: begin & grid = 0.50 & free,dirname & end
			'era'		: begin & grid = 0.50 & free,dirname & end
			'era2'		: begin & grid = 0.50 & free,dirname & end
			'gac'		: begin & grid = 0.25 & free,dirname & end
			'gac2'		: begin & grid = 0.25 & free,dirname & end
			else		: begin & grid = 1.   & free,dirname & end
		endcase

		make_geo,lon,lat,grid = grid
		dem = get_coverage(grid=grid,cov = 'land', index = idx_land, complement = idx_sea)
		for dd = 0,n_elements(data)-1 do begin
			dum_bin_val = -1
			dum_border  = -1
			wbins = 1
			first = 1
			matrix_all  = fltarr(180./lat_res,nyears*nmonths)-999.
			matrix_land = fltarr(180./lat_res,nyears*nmonths)-999.
			matrix_sea  = fltarr(180./lat_res,nyears*nmonths)-999.
			counti = 0l
			found_var = 0l
			for yy=0,nyears-1 do begin
				print,'Create Hovmoeller: '+years[yy]+' '+data[dd]+' '+ref+' '+sat
				for mm=0,nmonths-1 do begin
					dum = 	get_data(years[yy],months[mm],file=dum_file,data=data[dd],sat=sat,algo=ref,found=found,no_data_value=no_data_value,$
						dirname=dirname,/make_compare,/silent,/print_file,/no_recursive,var_dim_names=var_dim_names)
					if (ref eq 'cci' or ref eq 'cci3') and found then begin
						num = get_ncdf_data_by_name(dum_file,'number_of_processed_orbits',/global)
						if num lt 100 then begin
							print,'File has only '+string(num)+' Orbits, and will be skipped! ',dum_file
							found = 0
						endif
					endif
					if found then begin
						if stregex(data[dd],'hist1d',/bool,/fold) then begin
							if first then begin
								nbin        = (size(dum,/dim))[2]
								matrix_all  = fltarr(nbin,nyears*nmonths,9) -999.
								matrix_land = fltarr(nbin,nyears*nmonths,9) -999.
								matrix_sea  = fltarr(nbin,nyears*nmonths,9) -999.
								first = 0
							endif
							for cc = 0,n_elements(coverage)-1 do begin
								all  = get_coverage(lon,lat,dem=dem,coverage=coverage[cc])
								land = get_coverage(lon,lat,dem=dem,coverage=coverage[cc]+'_land')
								sea  = get_coverage(lon,lat,dem=dem,coverage=coverage[cc]+'_sea')
								for nb = 0, nbin -1 do begin
									matrix_all [nb,counti,cc] = total(dum[*,*,nb]>0l * all,/nan)
									matrix_land[nb,counti,cc] = total(dum[*,*,nb]>0l * land,/nan)
									matrix_sea [nb,counti,cc] = total(dum[*,*,nb]>0l * sea,/nan)
								endfor
								matrix_all [*,counti,cc] = matrix_all [*,counti,cc] / total(matrix_all [*,counti,cc],/nan) *100.
								matrix_land[*,counti,cc] = matrix_land[*,counti,cc] / total(matrix_land[*,counti,cc],/nan) *100.
								matrix_sea [*,counti,cc] = matrix_sea [*,counti,cc] / total(matrix_sea [*,counti,cc],/nan) *100.
								if wbins and keyword_set(var_dim_names) then begin
									dum_bin_val = get_ncdf_data_by_name(dum_file,var_dim_names[2],found=found)
									dum_border  = get_ncdf_data_by_name(dum_file,strreplace(var_dim_names[2],'_centre','_border'),found=found_border)
									wbins = 0
								endif
							endfor
						endif else begin
							matrix_all[*,counti]  = zonal_average(dum,lat,fillvalue=no_data_value,/mean,lat_res=lat_res)
							matrix_land[*,counti] = zonal_average(dum[idx_land],lat[idx_land],fillvalue=no_data_value,/mean,lat_res=lat_res)
							matrix_sea[*,counti]  = zonal_average(dum[idx_sea],lat[idx_sea],fillvalue=no_data_value,/mean,lat_res=lat_res)
						endelse
						found_var++
					endif
					free,dum_file
					counti++
				endfor
			endfor
			if found_var gt 0 then begin
				print,'Saved -> '+!SAVS_DIR + 'time_series/hovmoeller/'+data[dd]+'_hovmoeller_'+datestr+'_'+ref+apx+'_'+sat+'.sav'
				save_var,{all:matrix_all,land:matrix_land,sea:matrix_sea,bin_vals:dum_bin_val,bin_borders:dum_border} ,$
				!SAVS_DIR + 'time_series/hovmoeller/'+data[dd]+'_hovmoeller_'+datestr+'_'+ref+apx+'_'+sat+'.sav',chmod = '664'
			endif
			free,matrix_all
			free,matrix_land
			free,matrix_sea
		endfor
	endfor

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro do_all_time_series

	do_create_all_single_time_series
	do_create_all_compare_time_series
	do_1d_hist_time_series, compare_to = 'cci3'
	do_hist_cloud_type_time_series, compare_to = 'cci3'
	do_hist_cloud_type_time_series
	do_1d_hist_time_series
	do_create_hovmoeller
	do_hist_cloud_type_time_series
	do_1d_hist_time_series
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------------------------------------------------------
