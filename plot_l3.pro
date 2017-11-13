@vali_pre_compile.pro

pro plot_l3, save_as = save_as, white_bg = white_bg, reference = reference

	vali_set_path
	vali_set_charsize, white_bg = white_bg, save_as = save_as
	vali_set_plot_colors, white_bg = white_bg, save_as = save_as, reference = reference
	vali_set_defaults

	symball,/filled

end
;------------------------------------------------------------------------------------------
pro plot_only_color_bar,minvalue,maxvalue, data, unit, logarithmic=logarithmic, bar_horizontal=bar_horizontal,ctable = ctable, other = other, $
			bar_format = bar_format, save_as = save_as, discrete = discrete,notitle=notitle, $
			n_lev=n_lev,g_eq=g_eq,l_eq =l_eq, flip_colours = flip_colours,rainbow = rainbow, bar_tickname=bar_tickname, $
			bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow, brewer = brewer, greyscale = greyscale

; 	case strlowcase(strmid(data,0,3)) of
; 		'cot'	: title = 'Cloud Optical Thickness'
; 		'cth'	: title = 'Cloud Top Height'+ textoidl(unit)
; 		'cwp'	: title = 'Cloud Water Path'+ textoidl(unit)
; 		'iwp'	: title = 'Cloud Ice Water Path'+ textoidl(unit)
; 		'lwp'	: title = 'Cloud Liquid Water Path'+ textoidl(unit)
; 		'ctp'	: title = 'Cloud Top Pressure'+ textoidl(unit)
; 		'ctt'	: title = 'Cloud Top Temperature'+ textoidl(unit)
; 		'cfc'	: title = 'Cloud Fraction'
; 		'cc_'	: title = 'Cloud Fraction'
; 		'cph'	: title = 'Liquid Cloud Fraction'
; 		'cty'	: title = 'Cloud Type'
; 		else	: title = strupcase(data)
; 	endcase

	title = full_varname(data) + unit
	
	ncolors= 252
	bottom = 2
	void_color = 150
	oob_factor = 0.2
	oob_low    = 2
	oob_high   = 254

	leq  = keyword_set(l_eq) 	? l_eq : 0
	geq  = keyword_set(g_eq) 	? g_eq : 0
	disc = keyword_set(discrete)	? discrete :0
	bart = keyword_set(bar_tickname)? bar_tickname : 0
	mini = keyword_set(logarithmic) ? alog10(minvalue[0]) : float(minvalue[0])
	maxi = keyword_set(logarithmic) ? alog10(maxvalue[0]) : float(maxvalue[0])

	set_colors,rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply
	map_image_colors, rainbow = rainbow, extended_rainbow = extended_rainbow, greyscale = greyscale, elevation = elevation, bwr = bwr, ctable = ctable, $
			discrete = disc, flip_colours = flip_colours, brewer = brewer, void_color = void_color,ncolors=ncolors,bottom=bottom,mini=mini,maxi=maxi;, $
; 			panoply = panoply

	extra = {title:title,format:bar_format,l_eq:leq,g_eq:geq,discrete:disc,bar_tickname:bart}

	if keyword_set(save_as) then begin
		save_dum = (keyword_set(bar_horizontal) ? 'horizontal_':'vertical_')+strcompress(data,/rem)+$
			   (keyword_set(logarithmic) ? '_logarithmic':'')+'_color_bar'
		save_as  = !SAVE_DIR + 	'plot_l2/'+strreplace(save_dum,[' ',':',';',',','(',')','.','[',']','/','\','!'],['_','','','','','','','','','','',''],/noregex)+'.eps'
	endif

	plot, [mini,mini], [maxi,maxi], /nodata, xstyle=4, ystyle = 4

	if keyword_set(bar_horizontal) then begin
		start_save,save_as,size=[32,4]
			df_colorbar,minrange=mini,maxrange=maxi,divisions=n_lev,bot=bottom,ncolors=ncolors-1,$
			_extra=extra, $
			POSITION=[0.1, 0.3, 0.9, 0.6],$
			charsize = 2.5,$
 			logarithmic=logarithmic,/no_noerase,$
 			oob_factor=oob_factor,oob_low=oob_low,oob_high=oob_high
		end_save,save_as
	endif else begin
		start_save,save_as,size=[6,20]
			df_colorbar,minrange=mini,maxrange=maxi,divisions=n_lev,bot=bottom,ncolors=ncolors-1,/vertical , $
			_extra=extra,$
 			POSITION=[0.6, 0.1, 0.85, 0.9],$
			charsize = 2.5,logarithmic=logarithmic,/no_noerase,/left,$
			oob_factor=oob_factor,oob_low=oob_low,oob_high=oob_high
		end_save,save_as
	endelse

end
;------------------------------------------------------------------------------------------
pro plot_2d_rel_hist, bild1, name1, bild2=bild2, name2=name2, col_tab=col_tab, brewer=brewer, mini = mini, maxi = maxi, save_as=save_as, $
			difference = difference,notitle=notitle, appendix = appendix,nobar=nobar

	apx = keyword_set(appendix) ? appendix : ''
	xtickname = ['0.3','1.3','3.6','9.4','23','60','100'] ; tau
	ytickname = reverse(['10','180','310','440','560','680','800','1100']) ; plev
	if keyword_set(bild2) then begin
		if ~keyword_set(difference) then !p.multi=[0,2,1]
		name2 = keyword_set(name2) ? name2 : 'Algon2'
		if keyword_set(save_as) then begin
			ext = (file_name_info(save_as,/ext))
			save_d = ext ne '' ? file_name_info(save_as[0],/name,/path)+'_hist2d'+'.'+ext : $
				save_as[0]+'_Diff_'+strcompress(name1[0],/rem)+'_-_'+strcompress(name2[0],/rem)+'.eps'
			save_1 = ext ne '' ? file_name_info(save_as[0],/name,/path)+'_'+strcompress(name1,/rem)+'_hist2d'+'.'+ext : $
				save_as[0]+'_'+strcompress(name1,/rem)+'.eps'
			save_2 = ext ne '' ? file_name_info(save_as[0],/name,/path)+'_'+strcompress(name2,/rem)+'_hist2d'+'.'+ext : $
				save_as[0]+'_'+strcompress(name2,/rem)+'.eps'
			pmulti_bck = !p.multi

			!p.multi=0
		endif
	endif else begin
		if keyword_set(save_as) then begin
			ext = (file_name_info(save_as,/ext))
			save_1 = ext ne '' ? (keyword_set(bild2) ? file_name_info(save_as[0],/name,/path)+'_'+strcompress(name1,/rem)+'_hist2d'+'.'+ext : save_as[0]) : $
				 save_as[0]+'_'+strcompress(name1,/rem)+'.eps'
		endif
	endelse

	if keyword_set(difference) and keyword_set(bild2) then begin
;		!p.multi=0
		if keyword_set(save_as) then start_save, save_d, thick = thick, size=[40,40] else thick = 2
			view2d,bild1-bild2,xtickname=xtickname,ytickname=ytickname,xticks=n_elements(xtickname)-1,yticks=n_elements(ytickname)-1, $
			xtitle=apx+'Cloud Optical Thickness',ytitle=apx+'Cloud Top Pressure',bar_title= 'Relative Occurrence [%]', $
			title='Diff. '+name1+' - '+name2, mini = mini, maxi = maxi, color=~keyword_set(nobar), $
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize, $
			xmargin = [8,9], ymargin = [keyword_set(save_as)?6:4,5], bar_format=('(f5.1)'),col_tab=col_tab,brewer=brewer
			; plot cloud type boxes
			oplot,[2,2],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,[4,4],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,!x.crange,[2,2],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,!x.crange,[4,4],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			; aussen ränder
			oplot,[!x.crange[0],!x.crange[0]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
			oplot,[!x.crange[1],!x.crange[1]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
			oplot,!x.crange,[!y.crange[0],!y.crange[0]],thick=(keyword_set(save_as) ? 8:2)
			oplot,!x.crange,[!y.crange[1],!y.crange[1]],thick=(keyword_set(save_as) ? 8:2)
		if keyword_set(save_as) then end_save, save_d
	endif else begin
		if keyword_set(save_as) then start_save, save_1, thick = thick, size=[40,40] else thick = 2
		view2d,bild1,xtickname=xtickname,ytickname=ytickname,xticks=n_elements(xtickname)-1,yticks=n_elements(ytickname)-1, $
			xtitle=apx+'Cloud Optical Thickness',ytitle=apx+'Cloud Top Pressure',bar_title= 'Relative Occurrence [%]', title=name1+apx, $
			mini = mini, maxi = maxi, color=~keyword_set(nobar),$
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize, $
			xmargin = [8,9], ymargin = [keyword_set(save_as)?6:4,5], bar_format=('(f5.1)'),col_tab=col_tab,brewer=brewer
			; plot cloud type boxes
			oplot,[2,2],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,[4,4],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,!x.crange,[2,2],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			oplot,!x.crange,[4,4],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
			; aussen ränder
			oplot,[!x.crange[0],!x.crange[0]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
			oplot,[!x.crange[1],!x.crange[1]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
			oplot,!x.crange,[!y.crange[0],!y.crange[0]],thick=(keyword_set(save_as) ? 8:2)
			oplot,!x.crange,[!y.crange[1],!y.crange[1]],thick=(keyword_set(save_as) ? 8:2)
		if keyword_set(save_as) then end_save, save_1
		if keyword_set(bild2) then begin
			if keyword_set(save_as) then start_save, save_2, thick = thick, size=[40,40] else thick = 2
				view2d,bild2,xtickname=xtickname,ytickname=ytickname,xticks=n_elements(xtickname)-1,yticks=n_elements(ytickname)-1, $
				xtitle=apx+'Cloud Optical Thickness',ytitle=apx+'Cloud Top Pressure',bar_title= 'Relative Occurrence [%]', title=name2+apx, $
				mini = mini, maxi = maxi, color=~keyword_set(nobar),$
; 				charsize = (keyword_set(save_as) ? 3. : 1.5),charthick = (keyword_set(save_as) ? 2. : 1), $
				xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize, $
				xmargin = [8,9], ymargin = [keyword_set(save_as)?6:4,5], bar_format=('(f5.1)'),col_tab=col_tab,brewer=brewer
				; plot cloud type boxes
				oplot,[2,2],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
				oplot,[4,4],!y.crange,thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
				oplot,!x.crange,[2,2],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
				oplot,!x.crange,[4,4],thick=(keyword_set(save_as) ? 8:2),col=cgcolor('White')
				; aussen ränder
				oplot,[!x.crange[0],!x.crange[0]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
				oplot,[!x.crange[1],!x.crange[1]],!y.crange,thick=(keyword_set(save_as) ? 8:2)
				oplot,!x.crange,[!y.crange[0],!y.crange[0]],thick=(keyword_set(save_as) ? 8:2)
				oplot,!x.crange,[!y.crange[1],!y.crange[1]],thick=(keyword_set(save_as) ? 8:2)
			if keyword_set(save_as) then end_save, save_2
		endif
	endelse

end
;------------------------------------------------------------------------------------------
pro plot_taylor_diagram, year,month,day,file1=file1,file2=file2,varname=varname,sat1=sat1,algo=algo,save_as=save_as,win_nr=win_nr,$
			 reference=reference, verbose = verbose,level=level,mini=mini,maxi=maxi,limit=limit,unit=unit,$
			 other=other,antarctic=antarctic,arctic=arctic,time_series=time_series,notitle=notitle,sat2=sat2

	datum = keyword_set(time_series) ? 'time_series' : string(year,f='(i4.4)')+string(month,f='(i2.2)')+string(day,f='(i2.2)')
	ts    = keyword_set(time_series)

	algon1 = sat_name(algo,sat1,year=(ts ? 0:year), month = (ts ? 0:month) ,level=level)
	algon2 = sat_name(reference,sat2,year=(ts ? 0:year), month = (ts ? 0:month) ,level=level)

	if keyword_set(save_as) then begin
		if strcompress(save_as,/rem) eq '1' then begin
			save_as = !SAVE_DIR + datum+'_'+strupcase(varname)+'_taylor_diagram_'+algon1+'_vs_'+algon2+ '.eps'
		endif
	endif

	coverage = ['','antarctica','midlat_south','tropic','midlat_north','arctic','midlat_trop','land','sea']
	ncov     = n_elements(coverage)
	apx      = ''

	if ts then begin
		period       = !DEFAULT_TS_PERIOD
		corr_arr     = fltarr(ncov) -999.
		bias_arr     = fltarr(ncov) -999.
		pbias_arr    = fltarr(ncov) -999.
		rmse_arr     = fltarr(ncov) -999.
		stdd_arr_cci = fltarr(ncov) -999.
		stdd_arr_gac = fltarr(ncov) -999.
		max_bild_gac = fltarr(ncov) -999.
		for ii = 0, ncov-1 do begin
			d = get_available_time_series( 	algo, varname, sat1, coverage = coverage[ii], reference = reference, unit = unit, $
							period = period, sav_file = sav_file, found = found, trend = trend, tr_corr = tr_corr, $
							anomalies = anomalies, no_trend_found = no_trend_found)
			if not found then begin
				if ~no_trend_found then ok = dialog_message("plot_taylor_diagram: Sav File not found or not readable! "+sav_file)
				return
			endif
			corr_arr[ii]     = d.OVERALL_STATS.LATITUDE_WEIGHTED.CORRELATION
			bias_arr[ii]     = d.OVERALL_STATS.LATITUDE_WEIGHTED.BIAS
 			pbias_arr[ii]    = d.OVERALL_STATS.LATITUDE_WEIGHTED.SUM_DIFF / d.OVERALL_STATS.LATITUDE_WEIGHTED.SUM2 * 100.
			rmse_arr[ii]     = d.OVERALL_STATS.LATITUDE_WEIGHTED.RMSE
			stdd_arr_cci[ii] = d.OVERALL_STATS.LATITUDE_WEIGHTED.STDDEV
			stdd_arr_gac[ii] = d.OVERALL_STATS.LATITUDE_WEIGHTED.STDDEV2
			max_bild_gac[ii] = d.OVERALL_STATS.MAXVALUE
			datum            = d.actual_date
		endfor
	endif else begin
		bild_cci  = get_data(year,month,day,file=file1,sat = sat1,algo=algo,data=varname,level=level,no_data_value=fillv_b_cci,$
			   found=found,unit=unit,verbose= verbose,finfo=cci_info,dim3=dim3,/print_filename)
		if ~found then begin
			print,'plot_taylor_diagram: No data found!'
			return
		endif

		bild_gac = get_data(year,month,day,file=file2,sat = sat2,data=varname,algo=reference,level=level,no_data_value=fillv_b_gac,$
			   unit=unit2,found=found,verbose= verbose,finfo=ref_info,print_filename=2)
		if ~found then begin
			print,'plot_taylor_diagram: No reference data found!'
			return
		endif
;  		if is_the_same(algo,reference,sat=sat) then begin
 		if strmatch(algon1,algon2) then begin
			if ref_info.mtime gt cci_info.mtime then apx = ' (new)'
			if cci_info.mtime gt ref_info.mtime then apx = ' (old)'
		endif else new = -1

		bring_to_same_grid_and_unit,varname,bild_cci,bild_gac,fillv_b_cci,fillv_b_gac,file1=file1,file2=file2, $
			   algo,ref2algo(reference),unit,unit2,level=level,lon,lat,grid_res_out,verbose= verbose

		; all coverages
		corr_arr     = fltarr(ncov) -999.
		bias_arr     = fltarr(ncov) -999.
		pbias_arr    = fltarr(ncov) -999.
		stdd_arr_cci = fltarr(ncov) -999.
		stdd_arr_gac = fltarr(ncov) -999.
		max_bild_gac = fltarr(ncov) -999.
		for i = 0, ncov -1 do begin
			area = get_coverage(lon, lat, coverage = coverage[i])
			idx  = where(bild_cci ne fillv_b_cci and bild_gac ne fillv_b_gac and area,idx_cnt)
			if idx_cnt gt 0 then begin
				corr_arr[i]     = gcorrelate(bild_cci[idx],bild_gac[idx],lat[idx])
				bias_arr[i]     = gbias(bild_cci[idx],bild_gac[idx],lat[idx])
				pbias_arr[i]    = pbias(bild_cci[idx],bild_gac[idx],lat[idx])
				stdd_arr_cci[i] = gstddev(bild_cci[idx],lat[idx])
				stdd_arr_gac[i] = gstddev(bild_gac[idx],lat[idx])
				max_bild_gac[i] = max(bild_gac[idx])
			endif
		endfor
	endelse

	test = where(corr_arr eq -999. or bias_arr eq -999. or stdd_arr_cci eq -999. or stdd_arr_gac eq -999. or max_bild_gac eq -999.,test_cnt)
	if test_cnt gt 0 then begin
		print,'Some Arrays contain fillvalues. Please Check!'
		stop
	endif

; 	labels = ['All','Ant','MLS','Tro','MLN','Arc','PM6','Land','Sea']
; 	C_SYMBOL = (["Red","Green","Blue","Goldenrod","Magenta","PUR7","Dark Red","YGB4","Spring Green"])

	neg = where(corr_arr lt 0.,neg_cnt)
	labels = ['G','A','B','T','N','C','P','L','S']
	if neg_cnt gt 0 then labels[neg]=labels[neg]+'(-)'
	C_SYMBOL = "CG"+strcompress([2,5,9,4,6,1,8,7,3],/rem)
	stddev = stdd_arr_cci/stdd_arr_gac
	; Bias positive psym=5,negative psym=11,zero psym=16
	bias_perc2 = bias_arr/max_bild_gac*100.
	bias_perc = pbias_arr
	rms       = sqrt(( stdd_arr_cci^2. + stdd_arr_gac^2. -2*stdd_arr_cci*stdd_arr_gac*corr_arr) ) ; nach taylor 
	rms_norm  = rms / stdd_arr_gac
	symsize = fltarr(9)
	idx = where(between(abs(bias_perc),0,5,/not_include_upper),idx_cnt)
	if idx_cnt gt 0 then symsize[idx] = 2.;2
	idx = where(between(abs(bias_perc),5,10,/not_include_upper),idx_cnt)
	if idx_cnt gt 0 then symsize[idx] = 3.;3
	idx = where(between(abs(bias_perc),10,20,/not_include_upper),idx_cnt)
	if idx_cnt gt 0 then symsize[idx] = 4.;4
	idx = where(abs(bias_perc) ge 20,idx_cnt)
	if idx_cnt gt 0 then symsize[idx] = 5.;5
	print,'------------------------------------------------------------------------------------------------------------------------------------------'
	print,'Areas              :          ',strjoin(labels,'            ')
	print,'------------------------------------------------------------------------------------------------------------------------------------------'
	print,'MaxV2 (reference)  : ',max_bild_gac
	print,'Std1  (observed)   : ',stdd_arr_cci
	print,'Std2  (reference)  : ',stdd_arr_gac
	print,'RMSD               : ',rms
	print,'------------------------------------------------------------------------------------------------------------------------------------------'
	print,'Stddev (Std1/Std2) : ',stddev
	print,'Correlation        : ',corr_arr
;  	print,'BIAS / MaxV2 [%]   : ',bias_perc2
; 	print,'BIAS               : ',bias_arr
	print,'BIAS  [%]          : ',bias_perc
	print,'RMS (RMSD/Std2)    : ',rms_norm
	print,'------------------------------------------------------------------------------------------------------------------------------------------'
	psym = replicate(17,9) ; 29: hütchen hoch
	idx = where(bias_arr lt 0, idx_cnt)
	if idx_cnt gt 0 then psym[idx]=18 ; 30: hütchen runter
	idx = where(bias_arr eq 0, idx_cnt)
	if idx_cnt gt 0 then psym[idx]=16

;Still a lot to do to make x and yrange work!! 
; xrange=[0.5,1.5]
; yrange=[0.5,1.5]

	start_save,save_as,size=[40,25]
		cgTaylorDiagram, stddev, abs(corr_arr),  REF_STDDEV=1,LABELS=labels,observation=algon2+ apx,xrange=xrange,yrange=yrange, $
; 		STDDEV_MAX=(1.3 > rnd(max(stddev),/up,0.5)),c_symbol=c_symbol,symsize=symsize,symbol=psym,output=ouput;,cgcharsize=3.
		STDDEV_MAX=(0.0 > rnd(max(stddev),/up,0.1)),c_symbol=c_symbol,symsize=symsize,symbol=psym,output=ouput;,cgcharsize=3.

		labels=[strupcase(varname)+' '+unit,'  ',algon1,'  ',$
		'G: Global','A: Antarctica','B: MidLat-S','T: Tropics','N: MidLAt-N','C: Arctic',textoidl('P: \pm60\circ Latitude'),'L: Land','S: Sea']
		legend,labels,thick=replicate(2,13),psym=[0,0,28,0,replicate(16,9)],numsym=1,color=[-1,-1,-1,-1,cgcolor(c_symbol)], $
		charsize=(keyword_set(save_as) ? 2.5:1.5),pos=[0.8,0.7,1.1,1.05]
; 		legend, ['  Bias  ',' 0 - 5 %',' 5 -10 %','10 -20 %'],thick=replicate(1,4),psym=[1,5,5,5],numsym=1,symsize=[2,2,3,4],pos=[0.05,0.75,0.15,0.98]
; 		legend, ['  ','  ','  ','  '],thick=replicate(1,4),psym=[47,11,11,11],numsym=1,symsize=[2,2,3,4],pos=[0.04,0.75,0.05,0.98],charsize=2
		legend, ['  Bias  ',' 0 - 5 %',' 5 -10 %','10 -20 %',textoidl('  \geq 20 %')],thick=replicate(1,5),psym=[1,5,5,5,5],numsym=1, $
		symsize=[2,2,3,4,5],pos=[0.05,0.75,0.15,0.98]
		legend, ['  ','  ','  ','  ','  '],thick=replicate(1,5),psym=[47,11,11,11,11],numsym=1,symsize=[2,2,3,4,5], $
		pos=[0.04,0.75,0.05,0.98],charsize=(keyword_set(save_as) ? 1.5:.5)
		;draw box around bias
; 		x0 = 0.13 & x1 = 0.23
; 		y0 = 0.71  & y1 = 0.91
; 		xbox=[x0,x1,x1,x0,x0]
; 		ybox=[y0,y0,y1,y1,y0]
; 		plots,xbox,ybox,color=fgc,/norm
; 		plots,[x0,x1],[0.86,0.86],color=fgc,/norm

	end_save,save_as
end
;------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
pro compare_cci_with_clara, year, month, day, data = data, sat = sat, mini = mini	, $
			maxi = maxi, zoom=zoom, limit=limit, win_nr = win_nr, save_dir = save_dir, $
			overwrite_sav = overwrite_sav,verbose = verbose, land = land, sea = sea	, $
			only_clara=only_clara, ccifile=ccifile, reference=reference,orbit=orbit	, $
			coverage=coverage, show_values=show_values,other = other, ctable=ctable	, $
			p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
			mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,msg=msg	, $
			robinson=robinson, hammer = hammer, goode = goode,globe = globe			, $
			histogramm = histogramm,difference = difference, level=level,error=error, $
			ztext = ztext, algo1=algo1,zonal_only=zonal_only, nobar=nobar			, $
			stereographic = stereographic, out=out, hist_cloud_type=hist_cloud_type	, $
			logarithmic = logarithmic,timeseries=timeseries,dim3=dim3				, $
			white_bg=white_bg,dirname2=dirname2,magnify=magnify,oplots=oplots		, $
			wtext = wtext,countries=countries,notitle=notitle,shape_file=shape_file	, $
			no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box,$
			version1=version1,version2=version2

	if n_params() lt 1 then begin
		print,"Syntax: compare_cci_with_clara, year, month, day, /data, /sat, /mini, /maxi, /zoom, /limit, /win_nr, /save_as"
		print,"Set year,month for monthly means, set year,month,day for daily means!"
		return
	endif

	if not keyword_set(data) then begin
		print,"Syntax: compare_cci_with_clara, year, month, day, /data, /sat, /mini, /maxi, /zoom, /limit, /win_nr, /save_as"
		print,"Tell me what data to compare, e.g. ['cfc','cot','cph','iwp','lwp','ctp','ctt','cth']"
		return
	endif

	if keyword_set(shape_file) then begin
		use_shape_file = file_test(shape_file)
	endif else use_shape_file = 0
	ts         = keyword_set(timeseries)
	limit_test = keyword_set(limit)
	if limit_test then limit_bck = limit
	cov      = keyword_set(coverage)  ? strlowcase(coverage)  : 'full'
	ref      = keyword_set(reference) ? strlowcase(reference) : ''
	dat      = strsplit(strlowcase(data[0]),',',/ext)
	if n_elements(dat) eq 1 then dat = [dat,dat]
	hct      = keyword_set(hist_cloud_type) ? strlowcase(hist_cloud_type) : ''
	win_nr   = adv_keyword_set(win_nr) ? win_nr : 1
	sat      = keyword_set(sat) ? strlowcase(sat) : 'noaa18'
	yyyy     = string(year,f='(i4.4)')
	mm       = keyword_set(month) ? string(month,f='(i2.2)') : ''
	dd       = keyword_set(day)   ? string(day,f='(i2.2)') : ''
	fidx     = keyword_set(orbit) ? orbit : 0 
	algo1    = keyword_set(algo1) ? strlowcase(algo1) : ''
	datum    = yyyy+(mm eq '??' ? '' : mm)+dd
	datum2   = datum
	opl      = keyword_set(oplots) ? fix(oplots) : 0
	wbg      = keyword_set(white_bg)
	sav      = keyword_set(save_dir)
	savbg    = keyword_set(save_dir) or keyword_set(white_bg)

	case strmid(ref,0,3) of
		'gac': 	begin
					satgac = sat eq 'avhrrs' ? 'allsat' : sat
					if total(sat eq ['aatme','aatsr','atsrs']) then satgac = 'noaaam'
				end
		else: 	satgac = sat
	endcase
	algo2     = ref2algo(ref,sat=satgac)
	algon_cci = sat_name(algo1,sat   , year=(ts ? 0:year), month=(ts ? 0:month),version=version1,level=level)
	algon_gac = sat_name(algo2,satgac, year=(ts ? 0:year), month=(ts ? 0:month),version=version2,level=level)

	if keyword_set(land) and keyword_set(sea) then begin
		land = 0
		sea  = 0
	endif

	if ~keyword_set(level) then level = keyword_set(day) ? 'l3u' : 'l3c'
	bar_discontinous = 0

	vollername1 = full_varname(dat[0],/universal)
	vollername2 = full_varname(dat[1],/universal)
	unc1 		= stregex(vollername1,'Uncertainty',/fold,/bool)
	unc2 		= stregex(vollername2,'Uncertainty',/fold,/bool)
; 	dtn      	= [appendix(dat[0]),appendix(dat[1])]; should be obsolete now, full_varname should do it.
	dtn 		= ['','']

	case strmid(dat[0],0,3) of
		'cot'	: begin & histv = [0.1,0.,50.]   & end
		'cth'	: begin & histv = [0.1,0.,16.]   & end
		'cwp'	: begin & histv = [10.,0.,1500.] & end
		'iwp'	: begin & histv = [10.,0.,1500.] & end
		'lwp'	: begin & histv = [10.,0.,1500.] & end
		'ctp'	: begin & histv = [10.,0.,1100.] & end
		'ctt'	: begin & histv = [1.,150.,320.] & end
		'cfc'	: begin & histv = [0.01,0.,1.]   & end
		'cma'	: begin & histv = [1.,0.,1.]     & bar_discontinous = 1 & end
		'cph'	: begin
				if stregex(vollername1,'Cloud Phase',/fold,/bool) and ~unc1 then begin
					histv = [1.,0.,2.] 
					bar_discontinous = 1
				endif else begin
					histv = unc1 ? [1.,0.,100.] : [0.01,0.,1.] 
				endelse
			  end
		'ref'	: begin & histv = [1.,0.,100.]   & end
		'cer'	: begin & histv = [1.,0.,100.]   & end
		'cty'	: begin & histv = [1.,0.,5.]     & bar_discontinous = 1 & end
		'cld'	: begin & histv = [1.,0.,5.]     & bar_discontinous = 1 & end
 		else		: begin 
					histv = adv_keyword_set(maxi) and adv_keyword_set(mini) ? $
						[(10.^(strlen(strcompress(string(maxi,f='(i)'),/rem)))/1000.) < 10.,mini,maxi] : [1,0,100.]
				  end
	endcase

	;-------------
	if ~ts then begin
		ccifile = keyword_set(ccifile) ? ccifile : get_filename(yyyy,mm,dd,sat=sat,algo=algo1,level=level,data=dat[0])
		if ~file_test(ccifile) then begin
			print,'No file found'
			return
		endif
	endif

	if strmatch(algon_cci,algon_gac) then begin
		if strmatch(dat[0],dat[1]) then begin
			if ~ts then begin
				mtime1 = (file_info(ccifile)).mtime
				file2  = get_filename(yyyy,mm,dd,sat=sat,algo=algo2,level=level,data=dat[1],found=found_f2,/silent)
				mtime2 = found_f2 ? (file_info(file2)).mtime : mtime1
				if mtime1 gt mtime2 then begin
					algon_cci = 'NEW_'+algon_cci
					algon_gac = 'OLD_'+algon_gac
				endif else if mtime2 gt mtime1 then begin
					algon_cci = 'OLD_'+algon_cci
					algon_gac = 'NEW_'+algon_gac
				endif else begin
					algon_cci = 'F1_'+algon_cci
					algon_gac = 'F2_'+algon_gac
				endelse
			endif else begin
				algon_cci = 'F1_'+algon_cci
				algon_gac = 'F2_'+algon_gac
			endelse
		endif
	endif

	if keyword_set(save_dir) then begin
		if keyword_set(zoom) then begin
			print, "[map_image::ERROR] : method zoom not defined for postscript mode! Use keyword limit instead!"
			return
		endif
		save_dir = save_dir eq '1' ? !SAVE_DIR +(strlowcase(sat) eq 'aatme' ? '/aatme/':'/compare_cci/') : save_dir+'/'
		save_dum = save_dir+'Compare_CCI__L3'+(keyword_set(dd)?'U':'C')+'_'+yyyy+(mm eq '??' ? '':mm)+dd+ $
		'_'+(keyword_set(limit) ? '_limit_'+strjoin(strcompress(string(limit,f='(f6.1)'),/rem),'_') : '')+$
		(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')
		save_as1 = save_dum + '_' + algon_cci+'_'+strupcase(dat[0]) + '.eps'
		figure_title1 = keyword_set(notitle) ? '' : algon_cci + ' ' + vollername1 + ' ' + dd + mm + yyyy
		save_as2 = save_dum + '_' + algon_gac+'_'+strupcase(dat[1]) + '.eps'
		figure_title2 = keyword_set(notitle) ? '' : algon_gac + ' ' + vollername2 + ' ' + dd + mm + yyyy
		save_as3 = save_dum + '_'+dat[0]+'_'+algon_cci + '_' +dat[1]+'_'+ algon_gac + '_2dhist.eps'
		save_as4 = save_dum + '_'+dat[0]+'_'+algon_cci + '_' +dat[1]+'_' + algon_gac + '_zonalmean.eps'
		save_as5 = save_dum + '_'+dat[0]+'_'+algon_cci + '_' +dat[1]+'_' + algon_gac + '_hist1d_1d.eps'
		save_as_diff = save_dum + '_'+dat[0]+'_'+algon_cci + '_' +dat[1]+'_' + algon_gac + '_diff2d.eps'
		figure_title_diff = keyword_set(notitle) ? '' : 'Diff ' + algon_cci + ' - ' + algon_gac + ' ' + vollername1 + ' ' + dd + mm + yyyy
	endif else if win_nr ne -1 then win, win_nr, xs=1200, ys=800,title=strjoin(dat,',')

	if keyword_set(save_dir) then !p.multi = 0

	if is_h1d(dat[0]) and ts then begin
		datum  = !DEFAULT_TS_PERIOD
		struc1 = get_histo_time_series(algo1, dat, sat, period = datum, longname = longname, unit = unit, sav_file = sav_file, found = found, compare=algo1)
		if not found then begin
			ok = dialog_message('compare_cci: sav_file1 not found! '+sav_file)
			return
		endif
		struc2 = get_histo_time_series(algo2, dat, satgac, period = struc1.period, longname = longname, unit = unit2, sav_file = sav_file, found = found,compare=algo1)
		if not found then begin
			ok = dialog_message('compare_cci: sav_file2 not found! '+sav_file)
			return
		endif
; 		datum = strmatch(struc1.actual_date,struc2.actual_date) ? struc1.actual_date : 'F1-'+struc1.actual_date+' F2-'+struc2.actual_date
		datum  = struc1.actual_date
		datum2 = struc2.actual_date
		bild_cci = struc1.bild
		bild_gac = struc2.bild
		fillvalue1 = 0
		fillvalue2 = 0
		longname = 'Histogram of '+strreplace(dat,'hist1d_','')
		unit = ''
		unit2 = ''
		free, struc1
		free, struc2
	endif else begin
		; cci l3u files
		bild_cci = get_data(yyyy,mm,dd,file=ccifile[0], data = dat[0],sat=sat, no_data_value = fillvalue1, longname = longname, unit = unit, found = found,$
		verbose = verbose, level=level, algo = algo1,join_nodes=join_nodes,error=error,dim3=dim3,var_dim_names=var_dim_names_cci,/print_filename,$
		flag_meanings = flag_meanings1)
		if not found then begin
			ok = dialog_message('compare_cci: Data '+dat[0]+' not found in '+level+' '+algo1+' '+sat+' file. Right satellite?, product name? e.g. cc_mask_asc')
			return
		endif
		free, join_nodes
		; ref l3u files
		select = strlowcase(algo2) eq 'select'
		bild_gac = get_data(file = gac_nc_file, yyyy,mm,dd, data=dat[1],sat=satgac,algo=algo2,no_data_value=fillvalue2,level=level,dirname=dirname2, $
			   longname=longname, unit=unit2, found=found, verbose=verbose,join_nodes=join_nodes,orbit=orbit,error=error,dim3=dim3,$
			   var_dim_names=var_dim_names_gac,print_filename=2,flag_meanings = flag_meanings2)
		if not found then begin
			if ~file_test(gac_nc_file) then return
			ok = dialog_message('compare_cci: Data '+dat[1]+' not found in '+level+' '+algo2+' file. Right product name? e.g. cc_mask_asc')
			return
		endif else if select then begin
			if algo2 eq 'era-i' then get_era_info, gac_nc_file, algo = algo2, /set_as_sysvar
			algon_gac = sat_name(algo2,satgac, year=(ts ? 0:year), month=(ts ? 0:month),version=version,level=level)
			datum2    = yyyy+(mm eq '??' ? '' : mm)+dd
		endif

		free, join_nodes
	endelse

	;-------------------------histo1d_compare-------------------------------------
	ndims = size(bild_cci,/n_dim)
	if is_h1d(dat[0]) then begin
		if hct eq '1d' then begin

			;this is a poor mans solution
			;make sure both algos see the same and mark areas where one of both is not seeing anything
			;e.g compare to claas
			si_cci = size(bild_cci,/dim)
			ndv_cci_array = total((bild_cci eq fillvalue1),3) eq product(si_cci[2]) ; no observations on cci grid
			si_gac = size(bild_gac,/dim)
			ndv_gac_array = total((bild_gac eq fillvalue2),3) eq product(si_gac[2])
			;make it available for each other
			ndv_cci_array_gac = congrid(ndv_cci_array,si_gac[0],si_gac[1]) ; no observations on cci grid brought to gac grid
			ndv_gac_array_cci = congrid(ndv_gac_array,si_cci[0],si_cci[1]) ; no observations on gac grid brought to cci grid
			fillv_index_cci = where(ndv_cci_array eq 1 or ndv_gac_array_cci eq 1,idxcnt)
			fillv_index_gac = where(ndv_gac_array eq 1 or ndv_cci_array_gac eq 1,idxcnt)
			;-------
			bild_cci = get_1d_rel_hist_from_1d_hist( bild_cci, dat[0], algo = algo1, limit=limit, land=land, sea=sea, arctic=arctic, $
						antarctic=antarctic,xtickname=xtickname,ytitle=ytitle,hist_name=data_name,found=found,file=ccifile[0],$
						var_dim_names=var_dim_names_cci,bin_val=bin_val,fillv_index=fillv_index_cci,shape_file=shape_file)
			if ~found then return

			bild_gac = get_1d_rel_hist_from_1d_hist( bild_gac, dat[1], algo = algo2, limit=limit, land=land, sea=sea, arctic=arctic, $
						antarctic=antarctic,xtickname=xtickname, ytitle=ytitle, hist_name = data_name, found=found, file=gac_nc_file, $
						var_dim_names=var_dim_names_gac,fillv_index=fillv_index_gac,shape_file=shape_file)
			if ~found then return
			apx  = 'Liq + Ice'
			if is_h1d(dat[0],/ice)    then apx = 'Ice ' 
			if is_h1d(dat[0],/liquid) then apx = 'Liquid '
			if is_h1d(dat[0],/ratio)  then apx = 'Liq/(Liquid+Ice)'
			zwi1 = (algo1 eq 'coll6' and stregex(dat[0],'ctt',/fold,/bool)) ? ' (Day only) ' : ' '
			zwi2 = (algo2 eq 'coll6' and stregex(dat[1],'ctt',/fold,/bool)) ? ' (Day only) ' : ' '
			start_save, save_as5, thick = thick
				if savbg then thick = 4
				if opl eq 0 then begin
					yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini[0],maxi[0]] : [0,max([bild_gac,bild_cci])]
					plot,[0,0],[1,1],yr=yrange,xr=[0,n_elements(bild_gac)-1],xticks=n_elements(xtickname)-1, xtickname = xtickname, $
					xtitle=data_name,ytitle=ytitle,xminor=2,/ys, $
					charthick = !p_charthick , $
					xcharsize = !p_xcharsize , $
					ycharsize = !p_ycharsize , $
					xmargin   = [12,4] + ((wbg and ~sav) ? [6,3]:0), ymargin = [6,2] + ((wbg and ~sav) ? [3,1]:0)
; 					charsize = (savbg ? 2.5 : 1.5),$
; 					charthick = (savbg ? 2. : 1)
					idx = where(bild_cci ge 0,idx_cnt)
					if idx_cnt gt 0 then oplot,idx,bild_cci[idx],thick=thick,psym=-8,color=cgcolor(!compare_col1) ,linestyle=linestyle,symsize=2
					idx = where(bild_gac ge 0,idx_cnt)
					if idx_cnt gt 0 then oplot,idx,bild_gac[idx],thick=thick,psym=-8,color=cgcolor(!compare_col2) ,linestyle=linestyle,symsize=2
					if keyword_set(show_values) then begin
						legend,'Coverage '+(keyword_set(coverage) ? coverage : 'Global'),spos='top',charsize=(savbg ? 2.:1.5),numsym=1,$
						color=cgcolor(!compare_col1)
					endif else begin
						legend,[algon_cci+zwi1+apx,algon_gac+zwi2+apx],psym=[8,8],numsym=1,$
						color=[cgcolor(!compare_col1) , cgcolor(!compare_col2) ] , $
						thick=[thick,thick],spos='top', charsize=!l_charsize, charthick=!p_charthick
					endelse
				endif else begin
					psym      = (keyword_set(show_values) and (opl eq 1) ? -8 : -1*(opl*2))
					linestyle = (keyword_set(show_values) and (opl eq 1) ? 0:opl+1)
					idx = where(bild_cci ge 0,idx_cnt)
					if idx_cnt gt 0 then oplot,idx,bild_cci[idx],thick=thick,psym=psym,color=cgcolor(!compare_col1) ,linestyle=linestyle,symsize=2
					idx = where(bild_gac ge 0,idx_cnt)
					if idx_cnt gt 0 then oplot,idx,bild_gac[idx],thick=thick,psym=psym,color=cgcolor(!compare_col2) ,linestyle=linestyle,symsize=2
					legend,algon_cci+zwi1+apx,psym=psym,color=cgcolor(!compare_col1) ,thick=thick,spos='tl',charsize=!l_charsize , $
					ystretch=opl+2,linestyle=linestyle,charthick=!p_charthick
					legend,algon_gac+zwi2+apx,psym=psym,color=cgcolor(!compare_col2) ,thick=thick,spos='tr',charsize=!l_charsize , $
					ystretch=opl+2,linestyle=linestyle,charthick=!p_charthick
				endelse
			end_save,save_as5
			return
		endif else if ndims eq 4 then begin
			if is_h1d(dat[0],/ratio) then begin
				bild_cci = reform(float(bild_cci[*,*,*,0])/float((bild_cci[*,*,*,0]>0)+(bild_cci[*,*,*,1]>0)>1)) *100.
				bild_gac = reform(float(bild_gac[*,*,*,0])/float((bild_gac[*,*,*,0]>0)+(bild_gac[*,*,*,1]>0)>1)) *100.
				ndims = size(reform(bild_gac),/n_dim)
				unit = textoidl(' [%]')
				unit2 = textoidl(' [%]')
			endif else begin
				ok=dialog_message('Image has 4 Dimensions dont know what to do with it!')
				return
			endelse
		endif
		if ndims eq 3 then begin
			if keyword_set(dim3) then begin
				if is_number(dim3) then mo = fix(dim3) else mo = 0
			endif else mo = 0
			sdum_cci = total(bild_cci>0,3)
			sdum_gac = total(bild_gac>0,3)
			bild_cci = reform(bild_cci[*,*,mo])
			bild_gac = reform(bild_gac[*,*,mo])
			ndims    = size(bild_cci,/n_dim)
		endif
		longname = longname+' (bin # '+string(mo,f='(i2)')+')'
		if ndims eq 2 then begin
		if get_grid_res(bild_cci) ne get_grid_res(bild_gac) then begin
				grid = max([get_grid_res(bild_cci),get_grid_res(bild_gac)])
				if get_grid_res(bild_cci) lt get_grid_res(bild_gac) then begin
					if rebinable(get_grid_res(bild_cci),grid) then begin
						dum  = grid_down_globe(bild_cci,grid,no_data_value = fillvalue1,found=found,/total)
						sdum = grid_down_globe(sdum_cci,grid,no_data_value = fillvalue1,found=found1,/total)
						if found and found1 then begin
							bild_cci = dum/(sdum>1) * 100.
							idx = where(sdum eq 0,idxcnt)
							if idxcnt gt 0 then bild_cci[idx] = fillvalue1
							make_geo,lon,lat,grid = grid
						endif
					endif else begin
						make_geo,lond,latd,grid = get_grid_res(bild_cci)
						dum  = sat2global(lond,latd,bild_cci,grid=grid,no_data_value = fillvalue1,found=found)
						sdum = sat2global(lond,latd,sdum_cci,grid=grid,no_data_value = fillvalue1,found=found1)
						if found and found1 then begin
							bild_cci = dum.sum/(sdum.sum>1) * 100.
							idx = where(sdum.sum eq 0,idxcnt)
							if idxcnt gt 0 then bild_cci[idx] = fillvalue1
							lon      = dum.lon
							lat      = dum.lat
						endif
					endelse
					bild_gac = bild_gac/(sdum_gac>1) * 100.
					idx = where(sdum_gac eq 0,idxcnt)
					if idxcnt gt 0 then bild_gac[idx] = fillvalue2
				endif else begin
					if rebinable(get_grid_res(bild_gac),grid) then begin
						dum  = grid_down_globe(bild_gac,grid,no_data_value = fillvalue2,found=found,/total)
						sdum = grid_down_globe(sdum_gac,grid,no_data_value = fillvalue2,found=found1,/total)
						if found and found1 then begin
							bild_gac = dum/(sdum>1) * 100.
							idx = where(sdum eq 0,idxcnt)
							if idxcnt gt 0 then bild_gac[idx] = fillvalue1
							make_geo,lon,lat,grid = grid
						endif
					endif else begin
						make_geo,lon,lat,grid = get_grid_res(bild_gac)
						dum  = sat2global(lon,lat,bild_gac,grid=grid,no_data_value = fillvalue2,found=found)
						sdum = sat2global(lon,lat,sdum_gac,grid=grid,no_data_value = fillvalue2,found=found1)
						if found and found1 then begin
							bild_gac = dum.sum/(sdum.sum>1) * 100.
							idx = where(sdum.sum eq 0,idxcnt)
							if idxcnt gt 0 then bild_gac[idx] = fillvalue2
							lon      = dum.lon
							lat      = dum.lat
						endif
					endelse
					bild_cci = bild_cci/(sdum_cci>1) * 100.
					idx = where(sdum_cci eq 0,idxcnt)
					if idxcnt gt 0 then bild_cci[idx] = fillvalue1
				endelse
			endif else begin
				make_geo,lon,lat,grid=get_grid_res(bild_cci)
				bild_gac = bild_gac/(sdum_gac>1) * 100.
				idx = where(sdum_gac eq 0,idxcnt)
				if idxcnt gt 0 then bild_gac[idx] = fillvalue2
				bild_cci = bild_cci/(sdum_cci>1) * 100.
				idx = where(sdum_cci eq 0,idxcnt)
				if idxcnt gt 0 then bild_cci[idx] = fillvalue1
			endelse
			no_gridding = 1
			unit = textoidl(' [%]')
			unit2 = textoidl(' [%]')
		endif
 	endif
	;---------------------------------------------------------------------------------------------------------------------------------------

	if ndims eq 3 then begin
		if (size(bild_cci,/dim))[2] eq 12 then begin
			mo = keyword_set(month) ? fix(month)-1 : 0
			bild_cci = reform(bild_cci[*,*,mo])
			bild_gac = reform(bild_gac[*,*,mo])
			ndims    = size(bild_cci,/n_dim)
		endif else if (size(bild_cci,/dim))[2] eq 3 then begin
			; assuming this is a true color image
			print, 'Image is treated as a True color image!'
			t_c_i = 1
		endif else begin
			if keyword_set(dim3) then begin
				mo = is_number(dim3) ? fix(dim3) : 0
			endif else mo = 0
			bild_cci = reform(bild_cci[*,*,mo])
			bild_gac = reform(bild_gac[*,*,mo])
			ndims = size(bild_cci,/n_dim)
		endelse
	endif

	minvalue = histv[1]
	maxvalue = histv[2]
	if ~keyword_set(no_gridding) then bring_to_same_grid_and_unit,dat[0],bild_cci,bild_gac,fillvalue1,fillvalue2,file1=ccifile,file2=gac_nc_file	, $
					algo1,algo2,unit,unit2,level=level,lon,lat,grid_res_out,verbose = verbose,flag_meanings1=flag_meanings1, $
					flag_meanings2=flag_meanings2

	lat_res = 1. > get_grid_res(bild_cci)
	n_lev1 =6
	n_lev2 =6
	if keyword_set(difference) then begin
		free, flag_meanings1
		free, flag_meanings2
	endif
	if keyword_set(flag_meanings1) then begin
		bar_tickname1 = [flag_meanings1]
		discrete1 = findgen(n_elements(flag_meanings1)+1)+min(bild_cci[where(bild_cci ne fillvalue1[0])])
		n_lev1 = n_elements(flag_meanings1)
		g_eq = 0
		l_eq = 0
	endif
	if keyword_set(flag_meanings2) then begin
		bar_tickname2 = [flag_meanings2]
		discrete2 = findgen(n_elements(flag_meanings2)+1)+min(bild_gac[where(bild_gac ne fillvalue1[0])])
		n_lev2 = n_elements(flag_meanings2)
		g_eq = 0
		l_eq = 0
	endif

	ls = ( keyword_set(land) or keyword_set(sea)) ? 1:0
	if ls then begin
; 		dem = get_dem(lon,lat,grid_res=get_grid_res(bild_cci))
		dem = get_coverage(lon, lat, /land)
		if keyword_set(sea)  then begin
			void_index1 = where(bild_cci eq fillvalue1 or dem ne 0)
			void_index2 = where(bild_gac eq fillvalue2 or dem ne 0)
		endif else begin
			void_index1 = where(bild_cci eq fillvalue1 or dem eq 0)
			void_index2 = where(bild_gac eq fillvalue2 or dem eq 0)
		endelse
	endif else begin
		void_index1 = where(bild_cci eq fillvalue1)
		void_index2 = where(bild_gac eq fillvalue2)
	endelse
	if total(strcompress(unit[0],/rem) eq ['[]','[[]]']) then unit = ''
	if total(strcompress(unit2[0],/rem) eq ['[]','[[]]']) then unit2 = ''
	;-------------------- plot with map_image ------------------------------------------------------------------------------------
	set_colors,rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply
	set_proj  ,globe = globe, limit = limit, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat,nobar=nobar,no_label=no_label, $
		   Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson,no_grid=no_grid	, $
		   ortho=ortho,iso=iso,bar_horizontal=bar_horizontal,grid=grid,londel=londel,latdel=latdel,label=label,noborder=noborder,stereographic=stereographic		, $
		   no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify,msg=msg, $
		   maxvalue = adv_keyword_set(maxi) ? maxi[0]:maxvalue, bar_format=bar_format,lambert=lambert,$
		   lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_box=no_box,position = position,horizon=horizon

	if use_shape_file then begin
		print,'Using shape_file, ',shape_file
		de  = get_coverage(lon,lat,shape_file=shape_file)
		idx = where(de eq 0,idxcnt)
		if idxcnt gt 0 then begin
			bild_cci[idx] = fillvalue1
			bild_gac[idx] = fillvalue2
			lon[idx] = !values.f_nan
			lat[idx] = !values.f_nan
		endif
		if limit_test eq 0 then limit = set_limits(lon,lat, p0lon=p0lon, p0lat=p0lat, /four_elements,bounds=2,verbose=verbose)
		rotate_globe = 0
	endif

	if keyword_set(position) and keyword_set(nobar) then begin
		if nobar eq 99 and ~keyword_set(difference) and ~keyword_set(zonal_only) then begin
			position1 = [0.,0.5,0.5,1.]
			position  = [0.5,0.5,1.,1.]
		endif
	endif

	if ~keyword_set(difference) and ~keyword_set(zonal_only) then begin
		figure_title1 = keyword_set(notitle) ? '' : datum + ' ' + vollername1
		start_save, save_as1, thick = thick, size = [32, 20]
			m = obj_new("map_image",bild_cci,lat,lon,void_index=void_index1,box_axes=box_axes,n_lev=n_lev1				, $
						min=(float(size(mini,/type)) ? mini : minvalue),max=(float(size(maxi,/type)) ? maxi : maxvalue)	, $
						format=bar_format, title= keyword_set(title)? title : 						  $
						algon_cci+(strmatch(dat[0],dat[1]) ? '' : ' '+vollername1+dtn[0]+unit)						, $
						charthick = !m_charthick, countries=countries,usa=countries,horizon=horizon,  $
						charsize  = !m_charsize , bar_tickname=bar_tickname1, panoply = panoply						, $
						limit = limit, figure_title = figure_title1,rainbow = rainbow, logarithmic=logarithmic		, $
						ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel,lambert=lambert		, $
						noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar,g_eq=g_eq,l_eq=l_eq	, $
						p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine,discrete=discrete1	, $
						lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position1, $
						mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson		, $
						stereographic=stereographic,label=label	,no_continents=no_continents,no_grid=no_grid, $
						bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow,magnify=magnify		, $
						brewer = brewer, greyscale = greyscale,flip_colours = flip_colours,ctable=ctable,debug=verbose)
			if use_shape_file and keyword_set(countries) then cgDrawShapes, shape_file
			obj_destroy, m
		end_save, save_as1
	endif

	if keyword_set(difference) then begin
		if ls then begin
			if keyword_set(sea)  then void_index2 = where(bild_cci eq fillvalue1 or bild_gac eq fillvalue2 or dem ne 0,complement=cidx)
			if keyword_set(land) then void_index2 = where(bild_cci eq fillvalue1 or bild_gac eq fillvalue2 or dem eq 0,complement=cidx)
		endif else void_index2 = where(bild_cci eq fillvalue1 or bild_gac eq fillvalue2,complement=cidx)
		bild_gac   = float(bild_cci)-float(bild_gac)
		if keyword_set(save_dir) then save_as2 = save_as_diff
		if ~keyword_set(notitle) then begin
			if strmatch(dat[0],dat[1]) then begin
				figure_title2 = 'Difference '+ algon_cci + ' - ' + algon_gac
				title2 = vollername2+dtn[1]+unit2
			endif else begin
				figure_title2 = 'Difference '+ algon_cci + ' '+strupcase(dat[0])+' - ' + algon_gac+ ' '+strupcase(dat[1])
				title2 = strjoin(dat,' - ')
			endelse
		endif else begin
			figure_title2 = ''
			title2 = vollername2+dtn[1]+unit2
		endelse
		out = {bild:bild_gac,lon:lon,lat:lat,unit:unit}
		if cidx[0] ne -1 then begin
			qw_cnt = n_elements(cidx)
			qw     = cidx
			if limit_test or keyword_set(antarctic) or keyword_set(arctic) then begin
				if keyword_set(antarctic) then qw = where(between(lon[cidx],-180,180) and between(lat[cidx],-90,-60),qw_cnt) else $
				if keyword_set(arctic)    then qw = where(between(lon[cidx],-180,180) and between(lat[cidx], 60, 90),qw_cnt) else $
				qw = where(between(lon[cidx],limit[1],limit[3]) and between(lat[cidx],limit[0],limit[2]),qw_cnt)
			endif
			if qw_cnt gt 0 then print,'Glob. Mean '+algon_cci+' - '+algon_gac+' : ',string(gmean((bild_gac[cidx])[qw],(lat[cidx])[qw]),f='(f11.4)')
		endif
	endif else begin
		title2 = keyword_set(title)? title : algon_gac+(strmatch(dat[0],dat[1]) ? '' : ' '+vollername2+dtn[1]+unit2)
		figure_title2 = keyword_set(notitle) ? '' : datum2 + ' ' + vollername2
	endelse

	if ~keyword_set(zonal_only) then begin
		start_save, save_as2, thick = thick, size = [32, 20]

			rotate_globe = is_defined(rotate_globe) ? rotate_globe :	keyword_set(globe) and ~keyword_set(save_as) and $
							~keyword_set(zoom) and !p.multi[0] le 0 and keyword_set(wtext) and ~keyword_set(antarctic) and $
							~keyword_set(arctic) and opl eq 0 and keyword_set(difference)

			m = obj_new("map_image",bild_gac,lat,lon,void_index=void_index2,box_axes=box_axes,n_lev=n_lev2	, $
						min=(float(size(mini,/type)) ? mini : minvalue)				, $
						max=(float(size(maxi,/type)) ? maxi : maxvalue)				, $
						title= title2, format=bar_format,countries=countries,usa=countries	, $
						charthick = !m_charthick	, panoply = panoply			, $
						charsize  = !m_charsize , bar_tickname=bar_tickname2,horizon=horizon,  $
						limit = limit, figure_title = figure_title2, rainbow = rainbow		, $
						ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel	, $
						lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position, $
						noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar, $
						p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine, $
						mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,discrete=discrete2	, $
						robinson=robinson,stereographic=stereographic, logarithmic=logarithmic,g_eq=g_eq,l_eq=l_eq, $
						label=label,lambert=lambert, no_continents=no_continents,no_grid=no_grid,$
						bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow,magnify=magnify	, $
						brewer = brewer, greyscale = greyscale ,flip_colours = flip_colours,ctable=ctable,debug=verbose)
			if keyword_set(zoom) then begin
				get_new_corners = 1
				m -> zoom,get_new_corners = get_new_corners,/print_new, ztext = ztext,magnify=magnify
				if win_nr ne -1 then win, win_nr
				if ~keyword_set(difference) then !p.multi = [2,2,2]
			endif
		if use_shape_file and keyword_set(countries) then cgDrawShapes, shape_file
		if ~rotate_globe then begin
			obj_destroy, m
			end_save, save_as2
		endif

		if rotate_globe then begin

			xyouts,0.32,0.95,figure_title2,/norm,charsize=2

			!mouse.button = 1
			bitset = total(dat[0] eq ['QCFLAG']) ? 1 : 0
			value = '                                   '
			bild_dum = size(bild_gac,/type) eq 1 ? fix(bild_gac) : bild_gac
			si = size(bild_dum,/dim)
			while (!mouse.button ne 4) do begin
				cursor, x, y, /change,/device
				cursor, lo, la, /change,/data

				if (total(finite([lo,la])) eq 2) then begin
					qw = where(between(lon,lo-1.,lo+1.) and between(lat,la-1.,la+1.),count)
						if count gt 0 then begin
							idx=qw[where((abs(lon[qw]-lo)+abs(lat[qw] -la)) eq min(abs(lon[qw]-lo)+abs(lat[qw] -la))) ]
							dum_string = '['+strjoin(string([lo,la],f='(f6.1)'),',')+'] '
							werte      = bitset ? '['+strjoin(strcompress( where( (byte(bild_dum[idx[0]]) and 2^(indgen(9))) ne 0),/rem ),',')+']' : $
							strcompress(bild_dum[idx[0]],/rem)
							widget_control,wtext,set_value='[lon,lat] '+dum_string+' '+werte
						endif else widget_control,wtext,set_value=value
				endif
				if !mouse.button eq 1 then begin
					inf = (total(finite([lo,la])) eq 0.)
					x_r = x/float(!d.x_vsize)
					y_r = y/float(!d.y_vsize)
					do_it = 1
					if between(x_r,.00,.25) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon -30) mod 360)	else $
					if between(x_r,.75,1.0) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon +30) mod 360 )	else $
					if between(x_r,.25,.75) and between(y_r,.75,1.0) and inf then p0lat = ( 90 < round(p0lat +30) > (-90) )	else $
					if between(x_r,.25,.75) and between(y_r,.00,.25) and inf then p0lat = ( 90 < round(p0lat -30) > (-90) )	else do_it = 0

					limit = p0lat ge 0 ? 	[0.,p0lon-90.,90.-p0lat,p0lon+180.,0.,p0lon+90.,p0lat-90.,p0lon] : $
								[0.,p0lon-90.,p0lat+90.,p0lon,0.,p0lon+90.,-90.-p0lat,p0lon+180.]
					if do_it then m -> project,limit=limit,p0lon=p0lon,p0lat=p0lat, void_index=void_index
					xyouts,0.32,0.95,figure_title2,/norm,charsize=2
				endif
			endwhile
			obj_destroy, m
			widget_control,wtext,set_value='             Pixel Values'
		endif

	endif
	;-----------------------------------------------------------------------------------------------------------------------------

	if keyword_set(difference) then goto,ende

; 	if ~keyword_set(save_dir) and ~keyword_set(notitle) then xyouts, 0.4, 0.98, datum+' '+(strmatch(dat[0],dat[1]) ? vollername2+dtn[1]+unit2:''), /norm

	if limit_test or keyword_set(antarctic) or keyword_set(arctic) then begin
		if keyword_set(antarctic) then qw = where(between(lon,-180,180) and between(lat,-90,-60),qw_cnt) else $
		if keyword_set(arctic)    then qw = where(between(lon,-180,180) and between(lat, 60, 90),qw_cnt) else $
		qw = where(between(lon,limit[1],limit[3]) and between(lat,limit[0],limit[2]),qw_cnt)
		if qw_cnt gt 0 then begin
			bild_cci =  bild_cci[qw]
			bild_gac =  bild_gac[qw]
			lon      =  lon[qw]
			lat      =  lat[qw]
			if ls then dem = dem[qw]
		endif else begin
			print,'found no points within limits!'
			return
		endelse
	endif

	if get_grid_res(bild_cci) ne get_grid_res(bild_gac) then begin
		ok = dialog_message('compare_cci: CCI and reference does not have the same grid size!! Skip comparing!')
		return
	endif

	; hist_2d plots
	idx  = where(bild_cci ne fillvalue1 and bild_gac ne fillvalue2, chk_idx)
	if keyword_set(land) then idx  = where(bild_cci ne fillvalue1 and bild_gac ne fillvalue2 and dem ne 0, chk_idx)
	if keyword_set(sea)  then idx  = where(bild_cci ne fillvalue1 and bild_gac ne fillvalue2 and dem eq 0, chk_idx)

	ed = strmatch(datum,datum2)

	if ~keyword_set(zonal_only) then begin
		bin   = histv[0]
		min_a = histv[1]
		max_a = histv[2]
		cc    = 5
		bar_format='(i)'
		bar_title= 'nr of occurrence'
		aa = fltarr(10,10)
		aa[0] = 1
		if chk_idx gt 0 then begin
			regr = linfit(bild_cci[idx],bild_gac[idx], YFIT=yfit)
			aa = hist_2d(bild_cci[idx],bild_gac[idx],bin1=bin,bin2=bin,max1=max_a,max2=max_a,min1=min_a,min2=min_a)
			if bar_discontinous then begin
				aa = aa/total(aa)*100. ; prozente
				bar_title= 'nr of occurrence [%]'
				bar_format='(f5.2)'
			endif else begin
				aa = float(aa)/max(float(aa)) *100.
				bar_title= 'norm. to max. occurrence'
				bar_format='(f7.3)'
			endelse
		endif

		start_save, save_as3, thick = thick, size = [32, 20]
			if (min(aa) eq max(aa)) and (max(aa) eq 0) then aa[0]=1
			if bar_discontinous and n_elements(aa) le 9 then bar_tickname = string(aa[sort(aa)],f=bar_format)
; 			xtickv = vector(0,(size(aa,/dim))[0]-1,cc+1)
; 			ytickv = vector(0,(size(aa,/dim))[1]-1,cc+1)
			xtickname = strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f3.1)':'(i)')),/rem)
			ytickname = strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f3.1)':'(i)')),/rem)
			xmargin=[7,5]
			ymargin=[3,2]+(keyword_set(save_dir) ? [0,2]:0)
			if keyword_set(flag_meanings1) then begin
				strdum = strarr(n_elements(flag_meanings1)*2+1)+' '
				for i = 0,n_elements(flag_meanings1)-1 do strdum[i*2+1] = flag_meanings1[i]
				xtickname = strdum
				xticklen=0.00001
				xoplot_lines = 1
				bar_nlev = n_elements(aa)< 9.
			endif
			if keyword_set(flag_meanings2) then begin
				strdum = strarr(n_elements(flag_meanings2)*2+1)+' '
				for i = 0,n_elements(flag_meanings2)-1 do strdum[i*2+1] = flag_meanings2[i]
				ytickname = strdum
				yticklen=0.00001
				yoplot_lines = 1
				bar_nlev = n_elements(aa)< 9.
			endif

			view2d,aa,no_data_val=0,$
			xtitle=(ed ? '': datum +' ') + algon_cci+(strmatch(dat[0],dat[1]) ? '':' '+vollername1+dtn[0]+unit +(ed ? '': ' ' +datum )),$
			ytitle=(ed ? '': datum2+' ') + algon_gac+(strmatch(dat[0],dat[1]) ? '':' '+vollername2+dtn[1]+unit2+(ed ? '': ' ' +datum2)),$
			bar_title=bar_title, bar_discontinous = bar_discontinous, $
			xticks = n_elements(xtickname)-1, xtickv = xtickv,yticks = n_elements(ytickname)-1, ytickv = ytickv, $
			xtickname=xtickname,ytickname=ytickname, bar_format=bar_format, bar_nlev = bar_nlev, $
			log=~bar_discontinous,col_table=col_tab,orientation=-90,xticklen=xticklen,yticklen=yticklen, $
			title = (keyword_set(notitle) ? '':(strmatch(dat[0],dat[1]) ? vollername1+dtn[0]+unit:'')+' (Binsize='+string(bin,f='(f6.3)')+')'), $
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, $
			bar_tickname = bar_tickname,xmargin=xmargin,ymargin=ymargin
			if chk_idx gt 0 and bar_discontinous eq 0 then begin
				oplot,!x.crange,regr[1]*!x.crange+regr[0]/bin,linestyle=2,thick=thick
				oplot,!x.crange,!y.crange,thick=thick
			endif else begin
				si=size(aa,/dim)
				if keyword_set(xoplot_lines) then for i=0,si[0] do oplot,[i,i],!y.crange,thick=2
				if keyword_set(yoplot_lines) then for i=0,si[1] do oplot,!x.crange,[i,i],thick=2
				for i = 0, si[0]-1 do begin & $
					for j = 0,si[1]-1 do begin & $
						xyouts,i+0.5,j+0.5,strcompress(string(aa[i,j],f='(f6.2)'),/rem)+'%',align=0.5 & $
					endfor & $
				endfor
			endelse
		end_save, save_as3
	endif
	; zonal median
	dum_bildc = bild_cci
	dum_bildg = bild_gac
	if ls then begin
		dum_idx  = keyword_set(land) ? where(dem eq 0, chk_idx) : where(dem ne 0, chk_idx)
		if chk_idx gt 0 then begin
			dum_bildc[dum_idx] = fillvalue1
			dum_bildg[dum_idx] = fillvalue2
		endif
	endif

	if keyword_set(save_dir) then begin
		charthick = !p_charthick ;1.5
		xcharsize = !p_xcharsize ;1.7 
		ycharsize = !p_ycharsize ;1.7
		lcharsize = !l_charsize  ;2.5
		xmargin   = [14,6]
		ymargin   = [ 7,3]
	endif else if keyword_set(white_bg) and keyword_set(zonal_only) then begin
		charthick = !p_charthick ;3.0
		xcharsize = !p_xcharsize ;2.5
		ycharsize = !p_ycharsize ;2.5
		lcharsize = !l_charsize  ;3.0
		xmargin   = [20,6]
		ymargin   =  [8,3]
	endif else begin
		charthick = 1.2
		xcharsize = 1.2 
		ycharsize = 1.2
		lcharsize = 1.5
		xmargin   =[10,3]
		ymargin   = [5,2]
	endelse

	; cci
	medi_c = zonal_average(dum_bildc,lat,fillvalue=fillvalue1,lat_zon=lat1dc,/mean,/nan,lat_res=lat_res);,/median)
	; gac
	medi_g = zonal_average(dum_bildg,lat,fillvalue=fillvalue2,lat_zon=lat1dg,/mean,/nan,lat_res=lat_res);,/median)

	savbg = keyword_set(save_dir) or keyword_set(white_bg)

; 	DEVICE, SET_FONT='-adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1'

	start_save, save_as4, thick = thick, size = [32, 20]
		title = keyword_set(notitle) ? '' : $
		'bias: '+string(gbias(bild_cci[idx],bild_gac[idx],lat[idx]),f='(f7.2)')+ $
		' ; rmse: '+string(grmse(bild_cci[idx],bild_gac[idx],lat[idx]),f='(f6.2)') + $
		' ; stdd: '+string(sqrt(grmse(bild_cci[idx],bild_gac[idx],lat[idx])^2 - gbias(bild_cci[idx],bild_gac[idx],lat[idx])^2),f='(f6.2)')+unit
		yr = [(adv_keyword_set(mini) ? mini : (unit eq ' [K]' ? 200:0)),(adv_keyword_set(maxi) ? maxi : max([medi_c,medi_g])*1.05)] 
		plot,[0,0],[1,1],xr=[-90,90],xs=3,xticks=6,xtickname=['-90','-60','-30','0','30','60','90'], $
		xtitle='latitude [degrees]',ytitle=vollername1+(strmatch(dat[0],dat[1]) ?dtn[0]:'')+unit,yr=yr,title=title, ylog=logarithmic,$
		charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize, xmargin=xmargin,ymargin=ymargin

		str_pholder = strjoin(replicate(' ',max([strlen(algon_cci),strlen(algon_gac)])))
		print,'-----------'+data+'-'+cov+'--------------'
		if stregex(dat[0],'npoints',/fold,/bool) then begin
			print,'Total         '+string(algon_cci,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(total(bild_cci[idx]),f='(f15.1)')
			print,'Total         '+string(algon_gac,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(total(bild_gac[idx]),f='(f15.1)')
		endif
		print,'Glob. Mean (F1) '+string(algon_cci,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' : ',string(gmean(bild_cci[idx],lat[idx]),f='(f11.4)')
		print,'Glob. Mean (F2) '+string(algon_gac,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' : ',string(gmean(bild_gac[idx],lat[idx]),f='(f11.4)')
		print,'Glob. BIAS      '+str_pholder        +' : ',string(gbias(bild_cci[idx],bild_gac[idx],lat[idx]),f='(f11.4)')
		print,'Glob. RMSE      '+str_pholder        +' : ',string(grmse(bild_cci[idx],bild_gac[idx],lat[idx]),f='(f11.4)')
		print,'Glob. BC-RMSE   '+str_pholder        +' : ',string(sqrt(grmse(bild_cci[idx],bild_gac[idx],lat[idx])^2 - $
							           gbias(bild_cci[idx],bild_gac[idx],lat[idx])^2),f='(f11.4)')
		print,'Correlation(^2) '+str_pholder        +' : ',string(gcorrelate(bild_cci[idx],bild_gac[idx],lat[idx]),f='(f11.4)') +' ('+$
								   strcompress(string(gcorrelate(bild_cci[idx],bild_gac[idx],lat[idx])^2.,f='(f11.4)'),/rem)+')'
		print,'----------------------------------'
		thick = savbg ? thick + 2 : thick
		oplot,lat1dc,medi_c,thick=thick,col=cgcolor(!compare_col1)
		oplot,lat1dg,medi_g,thick=thick,col=cgColor(!compare_col2)
 		lnames = strmatch(dat[0],dat[1]) ? ['',''] : strupcase(' '+[dat[0],dat[1]]) 
		legend,[(ed ? '': datum +' ') +algon_cci,(ed ? '': datum2 +' ')+algon_gac]+lnames,$
		thick=replicate(thick,2),spos=(keyword_set(show_values)?'bot':'top'),charsize=lcharsize, $
		color=[cgcolor(!compare_col1), cgColor(!compare_col2)]
	end_save, save_as4

	ende :

	if keyword_set(get_new_corners) then begin
		compare_cci_with_clara, year, month, day, data = data, sat = sat, mini = mini, maxi = maxi , $
				limit=get_new_corners, win_nr = win_nr, save_dir = save_dir     	   , $
				overwrite_sav = overwrite_sav,verbose = verbose, land = land, sea = sea    , $
				only_clara=only_clara, ccifile=ccifile, reference=reference,orbit=orbit    , $
				coverage=coverage, show_values = show_values, other = other, ctable=ctable , $
				difference = difference
	endif

	if limit_test then limit=limit_bck else free,limit

end

pro plot_l2, year, month, day ,sat = sat, data = data, mini = mini, maxi = maxi, limit=limit	, $
			grid_down_to = grid_down_to, browse = browse, zoom=zoom	, $
			_extra=_extra, win_nr = win_nr, save_as= save_as, g_eq = g_eq		, $
			l_eq = l_eq, logarithmic=logarithmic, land = land, sea = sea, globe = globe		, $
			iso = iso, file = file, magnify=magnify, verbose = verbose, out = out	, $
			hist_cloud_type = hist_cloud_type, hist_phase=hist_phase, algoname = algoname	, $ 
			show_values = show_values, level = level, orbit = orbit,lon = lon	, $
			lat = lat, unit = unit, bild = bild, timeseries = timeseries		, $
			p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
			mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal		, $
			hammer = hammer, goode = goode, robinson = robinson, stereographic = stereographic 			, $
			ctable = ctable, other = other, discrete = discrete, prefix=prefix	, $
			nobar=nobar,cov = cov,wtext = wtext,ztext = ztext,msg_proj=msg_proj	, $
			oplots=oplots,error=error,white_bg=white_bg,dim3=dim3,rot=rot,datum=datum, $
			obj_out=obj_out,addtext=addtext,countries=countries,notitle=notitle, $
			shape_file=shape_file,no_continents=no_continents,no_grid=no_grid,$
			no_label=no_label,no_box=no_box,version=version,ts_extras=ts_extras

	mem_cur   = memory(/current)
	starttime = systime(1)

	if keyword_set(save_as) then !p.multi= 0
	ts = keyword_set(timeseries)
	opl  = keyword_set(oplots) ? fix(oplots) : 0
	if keyword_set(shape_file) then begin
		use_shape_file = file_test(shape_file)
	endif else use_shape_file = 0
	; ---defaults-------------------------------------------------------------------------
	limit_test = keyword_set(limit)
	if limit_test then limit_bck = limit
	pref1 =''
 	win_nr    = adv_keyword_set(win_nr) ? win_nr : 1
	found_dem = 0
	if keyword_set(land) and keyword_set(sea) then begin
		land = 0
		sea  = 0
	endif
	if keyword_set(limit) then begin
		if size(limit,/n_ele) lt 4 then limit = [32,-30,52,-12]
	endif
	prefix = keyword_set(prefix)? strcompress(prefix+' ') : ''
	hct    = keyword_set(hist_cloud_type) ? strlowcase(hist_cloud_type) : ''
	if keyword_set(addtext) then begin
		germany = stregex(addtext,'germany',/fold,/bool)
	endif
	adt    = keyword_set(addtext) ? ' - '+strupcase(addtext[0]) : ''

	; next ones only valid if not keyword_set(file)
; 	sat      = keyword_set(sat)     ? sat    : 'noaa18'
	year     = keyword_set(year)    ? year   : '';'2008'
	month    = keyword_set(month)   ? month  : '';'06'
	day      = keyword_set(day)     ? day    : ''
; 	algo     = keyword_set(algo)    ? strlowcase(algo)   : 'esacci'
	algo     = ref2algo(algoname)
	level    = keyword_set(level)   ? level  : ''
	fidx     = keyword_set(orbit)   ? orbit  : 0
	dat      = get_product_name(data,algo=algo,level=level,/upper)
	histo2d  = is_jch(dat)
	histo1d  = is_h1d(dat)
	save_dir = !SAVE_DIR + '/plot_l2/'

	datum    = ts ? '200801-200812' : keyword_set(datum) ? datum : ''
	; ------------------------------------------------------------------------------------
	set_colors,rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply

	algon = sat_name(algo,sat, year=(ts ? 0 :year), month=(ts ? 0:month),level=level,version=version)
	if stregex(dat,'usgs_',/fold,/bool) then begin
		algon = 'USGS'
		datum = ''
	endif

	if ~ts then begin
		if not keyword_set(file) then begin
			file = get_filename(year,month,day,data=dat,sat=sat,found=found,algo=algo,level=level,orbit=orbit)
			if not found then return
		endif else begin
			if n_elements(file) eq 1 then fidx = 0
			if strlen(fidx) eq 4 then fidx = 0
		endelse
	endif else fidx = 0

	if keyword_set(browse) then begin
		ncdf_browser, file[fidx]
		return
	endif

	if ts then begin
		if histo1d or histo2d then begin
			datum = !DEFAULT_TS_PERIOD
			struc = get_histo_time_series(algo, dat, sat, period = datum, longname = longname, unit = unit, sav_file = sav_file, found = found)
			if not found then begin
				ok = dialog_message('plot_l2: sav_file not found! '+sav_file)
				return
			endif
			bild  = struc.bild
			fillvalue = -999.
			minvalue = 0
			maxvalue = max(bild)
			datum = struc.actual_date
			free,struc
		endif else begin
			datum = !DEFAULT_TS_PERIOD
			d = get_available_time_series( 	algo, dat, sat, coverage = cov, reference = reference, period = datum, $
							sav_file = sfile, longname = longname, unit = unit, found = found, $
							trend = trend, tr_corr = tr_corr, anomalies = anomalies, uncertainty = uncertainty, $
							stddev = stddev, no_trend_found = no_trend_found, pvir=pvir, season=season,ts_extras=ts_extras)

			algon = sat_name(algo,sat,version=version)

			if not found then begin
				trace = SCOPE_TRACEBACK( )
				if ~no_trend_found then ok = dialog_message('plot_l2: Sav_file not found: '+sfile)
				print,'Trace Back:'
				for i = 0,n_elements(trace)-1 do print,trace[i]
				return
			endif

			if pvir then save_dir += 'PVIR/'
			if trend then begin
				bild = d.TREND.MEAN 
				longname = longname + ' - trend per decade'
			endif else if anomalies then begin
				bild = d.TREND.ANOM_MEAN
				longname = longname + ' - Anomalies'
			endif else if uncertainty then begin
				bild = d.UNC 
				longname = longname + ' - Uncertainty'
			endif else if stddev then begin
				bild = d.STD 
				longname = longname + ' - Standard Deviation'
			endif else if season then begin
				bild = reform(d.TREND.SEASONAL_MEAN2D[*,*,fix(month)-1])
				longname = longname + ' - Seasonal Mean '+theMonths(month)
			endif else bild = d.MEAN

			datum  = d.actual_date
			fillvalue = -999.
			mima=minmax(bild,no=fillvalue)
			minvalue = mima[0]
			maxvalue = mima[1]
		endelse
	endif else begin
		make_compareable = 1
		bild = get_data(year,month,day,file=file[fidx],data=dat,algo=algo,no_data_value=fillvalue,level=level,sat=sat,dim3=dim3,/print_filename, $
		minvalue=minvalue,maxvalue=maxvalue,longname=longname,unit=unit,found=found,verbose=verbose,error=error,make_compareable=make_compareable,$
		var_dim_names=var_dim_names,flag_meanings=flag_meanings)
		if is_string(bild) then begin
			ok = dialog_message('plot_l2: Dont know what to do with an Array of type string')
			return
		endif
		if size(bild,/type) eq 8 then begin
			ok = dialog_message('plot_l2: Cannot handle data of type structure! Data: '+dat)
			return
		endif
		if ~found then begin
			if level eq 'l3u' then level = level+'(l2b)'
			ok = file_test(file[fidx]) ? dialog_message('plot_l2: Data '+dat+' not found in '+level+' '+algon+'! File: '+file[fidx]) : $
			dialog_message('plot_l2: File not found!')
			return
		endif
	endelse

	ndims   = size(reform(bild),/n_dim)
	img_res = get_grid_res(bild)
	t_c_i = 0

	;------------------------------------------------------------------------------------------------------------------------------
	if keyword_set(save_as) then begin
		save_dum=save_as
		if keyword_set(globe) then begin
			p0lon_string = keyword_set(p0lon) ? strcompress(fix(p0lon),/rem) : '0'
			p0lat_string = keyword_set(p0lat) ? strcompress(fix(p0lat),/rem) : '0'
			worldview_string = '_worldview_'+p0lon_string+'_'+p0lat_string
		endif
		if (keyword_set(limit) and ~keyword_set(globe)) then begin
			limit_string = '_limit_'+strjoin(strcompress(string(limit,f='(f6.1)'),/rem),'_')
		endif

		if strcompress(save_as,/rem) eq '1' then begin
			save_dum = save_dir+prefix+datum+'_'+algon+'_'				+ $
			file_basename(file[fidx],is_hdf(file[fidx]) ? '.hdf':'.nc')+'_'+dat	+ $
			(keyword_set(hist_cloud_type) ? '_'+strupcase(hct) : '')		+ $
			(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')		+ $
			(keyword_set(limit) and ~keyword_set(globe) ? limit_string:'')		+ $
			(keyword_set(globe) ? worldview_string:'')				+ $
			(keyword_set(mollweide) ? '_proj_mollweide':'')				+ $
			(keyword_set(goode) ? '_proj_goode':'')					+ $
			(keyword_set(hammer) ? '_proj_hammer':'')				+ $
			(keyword_set(stereographic) ? '_proj_stereographic':'')			+ $
			(keyword_set(aitoff) ? '_proj_aitoff':'')				+ $
			(keyword_set(sinusoidal) ? '_proj_sinusoidal':'')			+ $
			(keyword_set(robinson) ? '_proj_robinson':'')				+ $
			(keyword_set(msg_proj) ? '_proj_satellite':'')				+ $
			(keyword_set(logarithmic) ? '_log_plot':'')
			save_dum = strreplace(save_dum,[']','[',' ',':',';',',','(',')','.'],['','','_','','','','','',''],/noregex)
			save_dum = save_dum+'.eps'
		endif
		if ((keyword_set(mollweide) or keyword_set(goode) or keyword_set(hammer) or keyword_set(stereographic) or $
		    keyword_set(aitoff) or keyword_set(sinusoidal) or keyword_set(robinson) ) and ~keyword_set(limit)) or $
		    keyword_set(notitle) then title = full_varname(dat) + unit
	endif else if win_nr ne -1 then win, win_nr,title=dat
	;------------------------------------------------------------------------------------------------------------------------------

	if histo1d and keyword_set(hct) then begin
		if strmid(hct,0,2) eq '1d' then begin
			bild = get_1d_rel_hist_from_1d_hist( bild, dat, algo = algo, limit=limit, land=land, sea=sea, arctic=arctic, antarctic=antarctic,$
							     xtickname=xtickname, ytitle=ytitle, hist_name = data_name, file=file[fidx], $
							     var_dim_names=var_dim_names, bin_val=bin_val, found=found,shape_file=shape_file)
			if ~found then return
			apx = 'Liq + Ice'
			if is_h1d(dat,/ice)    then apx = datum+' Ice ' 
			if is_h1d(dat,/liquid) then apx = datum+' Liquid '
			if is_h1d(dat,/ratio)  then apx = datum
			hori  = stregex(get_product_name(dat,algo ='cci'),'ctp',/fold,/bool)
			zwi   = (algo eq 'coll6' and stregex(dat,'ctt',/fold,/bool)) ? ' (Day only) ' : ' '
			sav   = keyword_set(save_as)
			wbg   = keyword_set(white_bg)
			savbg = sav or wbg 
			start_save, save_dum, thick = thick,size=[36,16]
				if savbg then thick = 4
				if savbg then symsize = 2
				if opl eq 0 then begin
					if (hori eq 0) then begin
						yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,max(bild)]
						plot,[0,0],[1,1],yr=yrange,xr=[0,n_elements(bild)-1],xticks=n_elements(xtickname)-1,$
						xtickname=xtickname,xtitle=data_name,ytitle=ytitle,xminor=2, title=keyword_set(notitle)?'':datum, $
						charthick = !p_charthick , $
						xcharsize = !p_xcharsize , $
						ycharsize = !p_ycharsize , $
						xmargin   = [12,4] + ((wbg and ~sav) ? [6,3]:0), ymargin = [6,2] + ((wbg and ~sav) ? [3,1]:0)
						idx = where(bild ge 0,idx_cnt)
						if idx_cnt gt 0 then oplot,idx,bild[idx],thick=thick,psym=-8,symsize=symsize
						legend,[algon+zwi+apx+adt],psym=[8],numsym=1,color=[-1],thick=2,spos='top', charsize= !l_charsize
					endif
					if ( hori eq 1) then begin
						yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,max(bild)]
						plot,[0,0],[1,1],xr=yrange,yr=[0,n_elements(bild)-1],yticks=n_elements(xtickname)-1,$
						ytickname=reverse(xtickname),ytitle=data_name,xtitle=ytitle,yminor=2, title=keyword_set(notitle)?'':datum, $
						charthick = !p_charthick , $
						xcharsize = !p_xcharsize , $
						ycharsize = !p_ycharsize , $
						xmargin   = [12,4] + ((wbg and ~sav) ? [6,3]:0), ymargin = [6,2] + ((wbg and ~sav) ? [3,1]:0)
						idx = where(bild ge 0,idx_cnt)
						if idx_cnt gt 0 then oplot,reverse(bild[idx]),idx,thick=thick,psym=-8,symsize=symsize
						legend,[algon+zwi+apx+adt],psym=[8],numsym=1,color=[-1],thick=2,spos='top', charsize= !l_charsize
					endif
				endif else begin
					define_oplots, opl, cols, spos, linestyle, psym, ystretch, error = error
					idx = where(bild ge 0,idx_cnt)
					if idx_cnt gt 0 then begin
						if hori eq 0 then oplot,idx,bild[idx],thick=thick,psym=-8,color=cgcolor(cols),symsize=symsize,linestyle=linestyle
						if hori eq 1 then oplot,reverse(bild[idx]),idx,thick=thick,psym=-8,color=cgcolor(cols),symsize=symsize,linestyle=linestyle
					endif
					legend,algon+zwi+apx+adt,thick=thick,color=cgcolor(cols), $
					spos=spos,ystretch=ystretch,charsize= !l_charsize ,$
					linestyle=linestyle,psym=-8;,numsym=1
				endelse
			end_save,save_dum
			return
		endif
	endif

	if ndims eq 4 and ~histo2d then begin
		if is_h1d(dat,/ratio) then begin
			idx = where(bild[*,*,*,0] le 0 and bild[*,*,*,1] le 0,idxcnt)
			bild = reform(float(bild[*,*,*,0]>0)/float((bild[*,*,*,0]>0)+(bild[*,*,*,1]>0)>1)) *100.
			if idxcnt gt 0 then bild[idx] = float(fillvalue)
			ndims = size(reform(bild),/n_dim)
			unit = textoidl(' [%]')
		endif else begin
			ok=dialog_message('Image has 4 Dimensions dont know what to do with it!')
			return
		endelse
	endif

	if ndims eq 3 then begin
		if ~histo1d and (size(bild,/dim))[2] eq 12 then begin
			mo = keyword_set(month) ? fix(month)-1 : 0
			bild = reform(bild[*,*,mo])
			ndims = size(bild,/n_dim)
		endif else if ~histo1d and (size(bild,/dim))[2] eq 3 then begin
			; assuming this is a true color image
			print, 'Image is treated as a True color image!'
			t_c_i = 1
		endif else begin
			if adv_keyword_set(dim3) then begin
				mo = is_number(dim3) ? dim3 : 0
				if histo1d and keyword_set(var_dim_names) then begin
					Bin_centres = get_ncdf_data_by_name(file[fidx],var_dim_names[2])
					pref1 = '[Bin Centre '+strcompress(string(Bin_centres[dim3],f='(f20.2)'),/rem)+'] '
				endif
			endif else mo = 0
			if mo ge (size(bild,/dim))[2] then begin
				ok = dialog_message('plot_l2: Index has to be lower than '+strcompress((size(bild,/dim))[2],/rem))
				return
			endif
			if histo1d then begin
				idx  = where(reform(bild[*,*,mo]) eq fillvalue,idxcnt)
				bild = ( reform(bild[*,*,mo]) / (total(bild > 0,3,/nan) > 1.) ) * 100.
				if idxcnt gt 0 then bild[idx] = fillvalue
				unit = textoidl(' [%]')
			endif else bild = reform(bild[*,*,mo])
			ndims = size(bild,/n_dim)
		endelse
	endif
	if ndims le 1 then begin
		if ndims eq 1 then begin
			print, 'plot_l2: Image needs at least 2 dimensions to run properly!'
			plot,bild,psym=-8,ytitle=dat,xtitle='X-Value',thick=2,symsize=2
		endif else begin
			ok = dialog_message('plot_l2: Image needs at least 2 dimensions to run properly!')
		endelse
		return
	endif

	msg = (sat eq 'msg' or algo eq 'claas') and total(size(bild,/dim)) eq total([3712.,3712.])
	if stregex(file[fidx],'secondary',/bool,/fold) then begin
		datum    = stregex(strreplace(file_basename(file[fidx]),'T',''),'[0-9]{12}',/ext)
		geo_file = file_search(file_dirname(file[fidx]),datum+'*'+strupcase(algo+'*'+level+'*'+sat)+'*.nc',count = found)
		if found ne 1 then geo_file = file[fidx] else print,'Plot_L2: Use Geofile: ',geo_file[0]
	endif else geo_file = file[fidx]

	; if algo is Unknown then READ LON/LAT FROM FILE ONLY, otherwise
	; if bild is of known grid size lon,lat will be calculated, this is usually faster than reading from file
	make_geo, file = geo_file, lon,lat, verbose = verbose, dimension = size(bild,/dim), $
	found = found_geo, msg=msg,/ pick_file,algo=algo,grid=(algo eq 'unknown' ? 0 : get_grid_res(bild[*,*,0,0,0]))

	if keyword_set(rot) then begin
		if size(reform(bild),/n_dim) eq 2 then begin
			bild = rotate(bild,fix(rot>0))
		endif
	endif

	ls = ( keyword_set(land) or keyword_set(sea) and ndims ne 1)
	if ls then begin
		found_ls = 0
		if not get_grid_res(bild[*,*,0,0,0]) and level eq 'l2' then begin
			dem = get_data(year,month,day,file=file[fidx],data='lsflag',algo=algo,no_data_value=fillvalue,level=level,dim3=dim3, $
			minvalue=minvalue,maxvalue=maxvalue,longname=longname,unit=unit,found=found_ls,verbose=verbose)
		endif
 		if found_ls eq 0 then dem = get_coverage(lon, lat, /land)
	endif

	if (~found_geo and ~histo2d) then begin
		if ndims gt 3 then return
		no_data_idx = 	ls and found_dem ? ( keyword_set(land) ? where(dem eq 0 and bild eq fillvalue[0]) : $
						where(dem ne 0 and bild eq fillvalue[0]) ) : where(bild eq fillvalue[0])
		start_save, save_dum, thick = thick
			view2d,bild,no_data_idx=no_data_idx,n_lev=n_lev,maxi=adv_keyword_set(maxi) ? maxi:max(bild),mini=adv_keyword_set(mini)?mini:min(bild[where(bild ne fillvalue[0])]), $
			title = keyword_set(notitle) ? '' : keyword_set(save_as) ? '' : algon+' '+longname, $
			g_eq =g_eq, l_eq =l_eq, color=~keyword_set(nobar),$
			bar_title = ((keyword_set(hist_cloud_type) ? strupcase(hct)+' ' : '')+(dat[0] eq 'PHASE' ? 'CPH':dat[0])+(total(dat[0] eq ['COT','PHASE']) ? '' : unit))+adt, $
			bar_format='(f7.1)', brewer=brewer,col_tab=col_tab, meridian_vec = meridian_vec, $
			coast_vec = coast_vec,boxed=boxed,$
; 			charthick = keyword_set(save_as) ? 2. : 1.5,charsize = (keyword_set(save_as) ? 2. : 1.2), $
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize
		end_save, save_dum
		print,'No Lon/LAT Info found! -> View2D only!'
		return
	endif
	if histo2d then begin
		apx = stregex(data,'ice',/fold,/bool) ? 'Ice ' : (stregex(data,'liq',/fold,/bool) ? 'Liquid ' : '')
		nv_cnt = 0
		hct = keyword_set(hist_cloud_type) ? strlowcase(hist_cloud_type) : 'cu'
		if strmid(hct,0,2) eq '1d' then begin
			plot_ctp = 0
			plot_cot = 0
			sav = keyword_set(save_as) or keyword_set(white_bg)
			symsize = sav ? 2 :1
			if keyword_set(save_as) then !p.multi=0
			if strlowcase(hct) eq '1d_cot' then plot_cot = 1
			if strlowcase(hct) eq '1d_ctp' then plot_ctp = 1
			if strlowcase(hct) eq '1d'     then begin
				plot_cot = 1
				plot_ctp = 1
				if keyword_set(opl) then begin
					ok=dialog_message('Oplot not possible. Try "1d_cot" or "1d_ctp" instead of 1d.')
					return
				endif
			end
			yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,50]
			if is_jch(dat,/ratio) then begin
				liq = get_1d_hist_from_jch(bild[*,*,*,*,0],algo,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,found=found1,shape_file=shape_file)
				ice = get_1d_hist_from_jch(bild[*,*,*,*,1],algo,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,found=found,shape_file=shape_file)
				if found and found1 then begin
					histos = {	cot:(reform(float(liq.cot)/float((liq.cot>0)+(ice.cot>0)>1)) *100.),$
							ctp:(reform(float(liq.ctp)/float((liq.ctp>0)+(ice.ctp>0)>1)) *100.)}
					ytitle='Liquid Fraction [%]'
				endif else found=0
			endif else begin
				histos = get_1d_hist_from_jch(bild,algo,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
							lon=lon, lat=lat, dem=dem, land=land, sea=sea,found=found,shape_file=shape_file)
				ytitle='Relative Occurrence [%]'
			endelse
			if found then begin
				; CTP
				if plot_ctp then begin
					start_save, strreplace(save_dum,'.eps','_CTP.eps'), thick = thick, size = [32,16]
						if sav then thick = 5
						if opl eq 0 then begin
							if (1 eq 2) then begin
								plot,[0,0],[1,1],yr=yrange,xr=[0,6],xticks=7,xtickname=xtickname.ctp, $
								xtitle=apx+'Cloud Top Pressure [hPa]',ytitle=ytitle,xminor=2,$
								charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
								xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
								ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
								xmargin   = [12,4],ymargin = [6,2]
								oplot,is_jch(dat,/ratio) ? histos.ctp : histos.ctp/total(histos.ctp)*100.,thick=thick,$
								psym=-8,symsize=symsize
								legend,[algon+' '+apx+adt],psym=[8],numsym=1,color=[-1],thick=thick,spos='top', $
								charsize=(sav ? !l_charsize : 1.5)
							endif else begin
								plot,[0,0],[1,1],xr=yrange,yr=[0,6],yticks=7,ytickname=reverse(xtickname.ctp), $
								ytitle=apx+'Cloud Top Pressure [hPa]',xtitle=ytitle,yminor=2,$
								charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
								xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
								ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
								xmargin   = [12,4],ymargin = [6,2]
								oplot,is_jch(dat,/ratio) ? reverse(histos.ctp) : reverse(histos.ctp/total(histos.ctp)*100.),$
								findgen(n_elements(histos.ctp)),thick=thick,psym=-8,symsize=symsize
								legend,[algon+' '+apx+adt],psym=[8],numsym=1,color=[-1],thick=thick,spos='top', $
								charsize=(sav ? !l_charsize : 1.5)
							endelse
						endif else begin
							define_oplots, opl, cols, spos, linestyle, psym, ystretch, error=error
							if (1 eq 2) then begin
								oplot,is_jch(dat,/ratio) ? histos.ctp : histos.ctp/total(histos.ctp)*100.,$
								thick=thick,psym=-8,color=cgcolor(cols),symsize=symsize,linestyle=linestyle
							endif else begin
								oplot,is_jch(dat,/ratio) ? reverse(histos.ctp) : reverse(histos.ctp/total(histos.ctp)*100.),$
								findgen(n_elements(histos.ctp)),thick=thick,psym=-8,$
								color=cgcolor(cols),symsize=symsize,linestyle=linestyle
							endelse
							legend,algon+' '+apx+adt,thick=thick,color=cgcolor(cols),$
							spos=spos,ystretch=ystretch,charsize=(sav ? !l_charsize : 1.5),$
							linestyle=linestyle,psym=-8;,numsym=1
						endelse
					end_save, strreplace(save_dum,'.eps','_CTP.eps')
				endif
				; COT
				if plot_cot then begin
					start_save, strreplace(save_dum,'.eps','_COT.eps'), thick = thick, size = [32,16]
						if sav then thick = 5
						if opl eq 0 then begin
							plot,[0,0],[1,1],yr=yrange,xr=[0,5],xticks=6,xtickname=xtickname.cot, $
							xtitle=apx+'Cloud optical Thickness',ytitle=ytitle,xminor=2,$
							charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
							xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
							ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
							xmargin   = [12,4],ymargin = [6,2]
							oplot,histos.cot/total(histos.cot)*100.,thick=thick,psym=-8,symsize=symsize
							legend,[algon+' '+apx+adt],psym=[8],numsym=1,color=[-1],thick=thick,spos='top',$
							charsize=(sav ? !l_charsize : 1.5)
						endif else begin
							define_oplots, opl, cols, spos, linestyle, psym, ystretch, error=error
							oplot,histos.cot/total(histos.cot)*100.,thick=thick,psym=-8,color=cgcolor(cols),$
							symsize=symsize,linestyle=linestyle
							legend,algon+' '+apx+adt,thick=thick,color=cgcolor(cols),$
							spos=spos,ystretch=ystretch,charsize=(sav ? !l_charsize : 1.5),$
							linestyle=linestyle,psym=-8;,numsym=1
						endelse
					end_save, strreplace(save_dum,'.eps','_COT.eps')
				endif
			endif
			return
		endif
		si = size(bild,/dim)
		if hct eq 'overview' then begin
			plot_isccp_uebersicht, short=save_dum, save_as = save_dum, win_nr = win_nr
			return
		endif
		if hct eq 'max' then begin
			if is_jch(dat,/ratio) then begin
				ok = dialog_message('plot_l2: '+hct+' not possible with hist2d_ratio!')
				return
			endif
			bild = get_hct_maxtype( bild, algo,fillvalue=fillvalue, htypes=htypes)
			bar_tickname= [htypes]
			discrete=findgen(10)
			ctable = 13
			n_lev=9
			mini = 0
			minvalue = 0
			maxi = 9
			maxvalue = 9
			title = keyword_set(notitle) ? '' : 'ISCCP Cloud type'
			longname ='most frequent Cloud Type'
			g_eq = 0
			l_eq = 0
			if ls then begin
				if keyword_set(land) then void_index = where(bild eq -1 or dem eq 0,complement = nv_idx,ncomplement=nv_cnt)
				if keyword_set(sea)  then void_index = where(bild eq -1 or dem ne 0,complement = nv_idx,ncomplement=nv_cnt)
			endif else void_index = where(bild eq -1,complement = nv_idx,ncomplement=nv_cnt)
		endif else if hct eq 'hist2d' or hct eq 'hist_2d' then begin
			if is_jch(dat,/ratio) then begin
				ok = dialog_message('plot_l2: '+hct+' not possible with hist2d_ratio!')
				return
			endif
			if keyword_set(prefix) then begin
				dum_h = get_hct_data('high',reform(bild[*,*,*,*,0]),algo,found=found) 		; coll5 wird in get_hct_data gedreht 
				dum_m = get_hct_data('mid' ,reform(bild[*,*,*,*,0]),algo,found=found) 		; coll5 wird in get_hct_data gedreht 
				dum_l = get_hct_data('low' ,reform(bild[*,*,*,*,0]),algo,sdum=sdum,found=found) ; coll5 wird in get_hct_data gedreht 
				rat_h = get_hct_ratio(dum_h,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,lat=lat,dem=dem,$
						land=land,sea=sea,shape_file=shape_file,/tex)
				rat_m = get_hct_ratio(dum_m,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,lat=lat,dem=dem,$
						land=land,sea=sea,shape_file=shape_file,/tex)
				rat_l = get_hct_ratio(dum_l,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,lat=lat,dem=dem,$
						land=land,sea=sea,shape_file=shape_file,/tex)
				print, prefix+year+month+' '+algon+' high/mid/low Ratio  : '+string(rat_h,f='(f4.1)')+'\%/' $
											   +string(rat_m,f='(f4.1)')+'\%/'+string(rat_l,f='(f4.1)')+'\%'
				free, dum_h
				free, dum_m
				free, dum_l
				free, sdum
				if prefix eq 'File3 (ref): ' then prefix = ''
			endif
			bild = get_2d_rel_hist_from_jch(bild, algo, dem = dem, land = land, sea = sea, limit = limit, antarctic = antarctic, arctic = arctic, $
							lon = lon, lat = lat, fillvalue = fillvalue, found = found, shape_file = shape_file)
			if not found then return
			plot_2d_rel_hist, bild, algon, col_tab=col_tab, brewer=brewer, mini=mini, maxi=maxi, save_as=save_dum, appendix = apx,nobar=nobar
			return
		endif else begin
			if is_jch(dat,/ratio) then begin
				liq = get_hct_data(hct,bild[*,*,*,*,0],algo,sdum=sdum,found=found)
				ice = get_hct_data(hct,bild[*,*,*,*,1],algo,sdum=sdum,found=found)
				void_index = where(liq le 0 and ice le 0,idxcnt)
				bild = reform(float(liq)/float((liq>0)+(ice>0)>1)) *100.
				if idxcnt gt 0 then bild[void_index] = float(fillvalue)
				title = keyword_set(notitle) ? '' : 'Liquid Fraction '+(keyword_set(hct) ? 'of '+strupcase(hct)+' Clouds [%]':'')
			endif else begin
				; relative or not relative sis is se question.
				relative = 1
				bild = get_hct_data(hct,reform(bild[*,*,*,*,0]),algo,sdum=sdum,found=found,relative=relative)
				minvalue = 0
				maxvalue = relative ? 100. : max(bild)
				rat = get_hct_ratio(bild,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,$
						    lat=lat,dem=dem,land=land,sea=sea,void_index=void_index,relative=relative, shape_file = shape_file)
				print,strupcase(hct)+'/all Ratio '+algon+'  : '+rat
				if relative then title = keyword_set(notitle) ? '' : strupcase(hct)+' Clouds Fraction [%]'
			endelse
		endelse
	endif else begin
		; this might be obsolete
		if keyword_set(grid_down_to) then begin
			dum = sat2global(lon, lat, bild, no_data_value = fillvalue, grid_res = 1./grid_down_to, verbose = verbose,found=found)
			if found then begin
				lon  = dum.lon
				lat  = dum.lat
				bild = dum.mean
			endif else begin
				ok=dialog_message('plot_l2: Cannot plot anything. No valid Points found!')
				return
			endelse
		endif

		good_idx = where(lon ne -999 and lat ne -999 and finite(bild), gidx_cnt, ncomplement = bidx_cnt)
		if gidx_cnt eq 0 then begin
			ok=dialog_message('plot_l2: Cannot plot anything. No valid Points found!')
			return
		endif else if bidx_cnt ne 0 then begin
			lon = lon[good_idx]
			lat = lat[good_idx]
			bild = bild[good_idx]
			if ls and found_dem then dem = dem[good_idx]
		endif

		if stregex(dat,'nobs',/fold,/bool) or stregex(dat,'nretr',/fold,/bool) then begin
			if ls then begin
				if keyword_set(land) then void_index = where(bild eq fillvalue[0] or bild eq 0 or dem eq 0,complement = nv_idx,ncomplement=nv_cnt)
				if keyword_set(sea)  then void_index = where(bild eq fillvalue[0] or bild eq 0 or dem ne 0,complement = nv_idx,ncomplement=nv_cnt)
			endif else void_index = where(bild eq fillvalue[0] or bild eq 0,complement = nv_idx,ncomplement=nv_cnt)
			if adv_keyword_set(maxi) then if float(maxi[0]) lt (max(floor(bild[where(bild ne fillvalue[0])]*1000.))/1000.) then g_eq = 1
			if adv_keyword_set(mini) then if min(bild[where(bild ne fillvalue[0]) and bild ne 0]) lt mini[0] then l_eq = 1
		endif else begin
			; true_color_images only land / sea
			if t_c_i then begin
				if ls then begin
					if keyword_set(land) then void_index = where(dem eq 0,complement = nv_idx,ncomplement=nv_cnt)
					if keyword_set(sea)  then void_index = where(dem ne 0,complement = nv_idx,ncomplement=nv_cnt)
				endif else void_index = where(reform(bild[*,*,0]) lt 0,complement = nv_idx,ncomplement=nv_cnt) ; dummy
			endif else begin
				if ls then begin
					if keyword_set(land) then void_index = where(bild eq fillvalue[0] or dem eq 0,complement = nv_idx,ncomplement=nv_cnt)
					if keyword_set(sea)  then void_index = where(bild eq fillvalue[0] or dem ne 0,complement = nv_idx,ncomplement=nv_cnt)
				endif else void_index = where(bild eq fillvalue[0],complement = nv_idx,ncomplement=nv_cnt)
				if adv_keyword_set(maxi) then if float(maxi[0]) lt (max(floor(bild[where(bild ne fillvalue[0])]*1000.))/1000.) then g_eq = 1
				if adv_keyword_set(mini) then if min(bild[where(bild ne fillvalue[0])]) lt mini[0] then l_eq = 1
			endelse
		endelse

		n_lev=6
		if keyword_set(flag_meanings) then begin
			bar_tickname = [flag_meanings]
			for i = 0,n_elements(bar_tickname) -1 do begin
				; do only one line break in the middle
				dum = strsplit(bar_tickname[i],'!C',/ext)
				if n_elements(dum) gt 2 then begin
					middle= median(findgen(n_elements(dum)))
					bar_tickname[i] = strjoin(dum[0:middle],' ')+'!C'+strjoin(dum[middle+1:*],' ')
				endif
			endfor
			mini 	= minvalue[0]; this always! overwrites mini input
			maxi 	= maxvalue[0]; this always! overwrites maxi input
			discrete= findgen(n_elements(flag_meanings)+1) + mini[0]
			n_lev	= n_elements(flag_meanings)
			g_eq	= 0
			l_eq	= 0
		endif
		;otherwise title and figue_title are ambigous
		if ((keyword_set(mollweide) or keyword_set(goode) or keyword_set(hammer) or keyword_set(stereographic) or $
		    keyword_set(aitoff) or keyword_set(sinusoidal) or keyword_set(robinson) ) and ~keyword_set(limit)) or $
		    keyword_set(notitle) then title = full_varname(dat) + unit
	endelse

	set_proj, globe = globe, limit = limit, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat,lambert=lambert	, $
		  Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson	, $
		  enhanced_robinson = enhanced_robinson, ortho=ortho,iso=iso,stereographic=stereographic, bar_horizontal=bar_horizontal,grid=grid,$
		  londel=londel,latdel=latdel,label=label,noborder=noborder,no_label=no_label,no_grid=no_grid, $
		  no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify,nobar=nobar,msg=msg_proj,$
		  lonlab=lonlab,latlab=latlab,latalign=latalign,lonalign=lonalign,lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,$
		  maxvalue = adv_keyword_set(maxi) ? maxi[0]:maxvalue, bar_format=bar_format,no_box=no_box,position = position,horizon=horizon	

	if keyword_set(discrete) then begin
		if n_elements(discrete) eq 1 then discrete=findgen(n_lev+1)
	endif
	
	if minvalue eq maxvalue then maxvalue = minvalue +1
	if adv_keyword_set(maxi) and adv_keyword_set(mini) then begin
		if maxi eq mini then maxi = mini +1
	endif
	if total(strcompress(unit[0],/rem) eq ['[]','[[]]']) then unit = ''
; 	if strupcase(hct) eq 'HCB' or strupcase(hct) eq 'VCB' then begin
	if between(nobar,4,5) then begin
		plot_only_color_bar,(adv_keyword_set(mini) ? mini[0]:minvalue),$
		(adv_keyword_set(maxi) ? maxi[0]:maxvalue), dat, unit, logarithmic=logarithmic, $
			bar_horizontal = (nobar eq 4), ctable = ctable, other = other, save_as = save_as,bar_format=bar_format,$
			n_lev=n_lev,g_eq=g_eq,l_eq =l_eq, flip_colours = flip_colours,rainbow = rainbow,discrete =discrete, bar_tickname=bar_tickname,$
			bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow, brewer = brewer, greyscale = greyscale
		return
	endif

	br_start = strpos(longname,'(')
	br_end   = strpos(longname,')')
	if br_start ge 0 and br_end ge 0 then begin
		print, 'Cutting longname: ',longname
		longname = strreplace(longname,strmid(longname,br_start,br_end-br_start+1),'') 
	endif
	if longname eq 'long_name unknown' then longname = ''
	if strlen(longname) gt 80 then longname = strmid(longname,0,80)+'...'

	figure_title = keyword_set(notitle) ? '' : prefix + datum+' '+algon+' '+longname
	rotate_globe = keyword_set(globe) and ~keyword_set(save_as) and ~keyword_set(zoom) and !p.multi[0] le 0 and keyword_set(wtext) and $
			~keyword_set(antarctic) and ~keyword_set(arctic) and opl eq 0 and total(!p.multi) le 2

	if use_shape_file then begin
		print,'Using shape_file, ',shape_file
		de  = get_coverage(lon,lat,shape_file=shape_file)
		idx = where(de eq 0,idxcnt)
		if idxcnt gt 0 then begin
			lon[idx] = !values.f_nan
			lat[idx] = !values.f_nan
		endif

		if limit_test eq 0 then limit = set_limits(lon,lat, p0lon=p0lon, p0lat=p0lat,/four_elements,bounds=2,verbose=verbose)

		rotate_globe = 0
	endif

	device, get_decomposed = decomp

; 	if opl le 1 or ~obj_valid(obj_out) then begin
	if ~obj_valid(obj_out) then begin
		if opl eq 0 then start_save, save_dum, thick = thick,size=[48,30]
			m = obj_new("map_image",bild, lat, lon, void_index=void_index, void_color = void_color,n_lev=n_lev	, $
				max = adv_keyword_set(maxi) ? maxi[0]:maxvalue, min = adv_keyword_set(mini) ? mini[0]:minvalue, format=bar_format, $
				magnify=magnify, figure_title = figure_title,box_axes=box_axes, $
				no_grid = no_grid, no_continents = no_continents ,horizon=horizon,$
				charthick = !m_charthick, charsize  = !m_charsize ,  $
				title= keyword_set(title) ? title : pref1+((keyword_set(hist_cloud_type) ? strupcase(hct)+' ' : '')+$ 
				get_product_name(dat[0],algo=algo,/upper)+(total(dat[0] eq ['COT','PHASE']) ? '' : unit))+adt , g_eq =g_eq, l_eq =l_eq, $
				countries=countries,usa=countries,rainbow = rainbow, flip_colours = flip_colours, logarithmic=logarithmic,$
				bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow, brewer = brewer, greyscale = greyscale, $
				limit = limit, ctable=ctable,discrete=discrete, bar_tickname=bar_tickname ,lambert=lambert,$
				ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel,noborder=noborder, panoply=panoply, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position, $
				no_draw_border=no_draw_border, no_color_bar=no_color_bar, p0lon= p0lon, p0lat = p0lat, iso = iso , $
				lonlab=lonlab,latlab=latlab,latalign=latalign,lonalign=lonalign,$
				goodeshomolosine = goodeshomolosine, mollweide=mollweide,hammer=hammer,aitoff=aitoff,stereographic=stereographic, $
				label=label, sinusoidal=sinusoidal,robinson=robinson,debug=verbose)
			if keyword_set(zoom) and not keyword_set(save_as) then begin
				if win_nr ne -1 then m -> zoom, win = win_nr,/print_new else m -> zoom,/print_new,ztext=ztext, $
				figure_title = figure_title,void_index=void_index,magnify=magnify
			endif
		if opl eq 0 then end_save, save_dum
		if arg_present(obj_out) then begin
			obj_out = m
			; within map_image, decompose is set to 0 for non true color images, the cleanup resets this then,
			; since we dont destroy the map object we have to reset it manually to avoid crazy colors
			if ~rotate_globe then device, decompose = decomp
		endif else begin
			if ~rotate_globe then obj_destroy, m
		endelse
	endif else begin
		if void_index[0] ne -1 then begin
			lon[void_index] = !values.f_nan
			lat[void_index] = !values.f_nan
; 			if get_product_name(dat,algo='pmx') eq 'cloud_mask' then begin
				idx = where(bild eq 0,idxcnt)
				if idxcnt gt 0 then begin
					lon[idx] = !values.f_nan
					lat[idx] = !values.f_nan
				endif
; 			endif
			void_index = -1
		endif
		obj_out -> project, image = bild, lon = lon, lat = lat,/no_erase, no_color_bar=1, void_index=void_index,n_lev=n_lev, $
			max = adv_keyword_set(maxi) ? maxi[0]:maxvalue, min = adv_keyword_set(mini) ? mini[0]:minvalue,$
			limit=limit, figure_title = figure_title
		device, decompose = decomp
	endelse

	if use_shape_file and keyword_set(countries) then cgDrawShapes, shape_file

	if rotate_globe then begin

		!mouse.button = 1
		bitset = total(dat[0] eq ['QCFLAG']) ? 1 : 0
		value = '                                   '
		bild_dum = size(bild,/type) eq 1 ? fix(bild) : bild
		si = size(bild_dum,/dim)
		while (!mouse.button ne 4) do begin
			cursor, x, y, /change,/device
			cursor, lo, la, /change,/data

			if (total(finite([lo,la])) eq 2) then begin
				qw = where(between(lon,lo-1.,lo+1.) and between(lat,la-1.,la+1.),count)
					if count gt 0 then begin
						idx=qw[where((abs(lon[qw]-lo)+abs(lat[qw] -la)) eq min(abs(lon[qw]-lo)+abs(lat[qw] -la))) ]
						dum_string = '['+strjoin(string([lo,la],f='(f6.1)'),',')+'] '
						werte      = bitset ? '['+strjoin(strcompress( where( (byte(bild_dum[idx[0]]) and 2^(indgen(9))) ne 0),/rem ),',')+']' : $
						strcompress(bild_dum[idx[0]],/rem)
						widget_control,wtext,set_value='[lon,lat] '+dum_string+' '+werte
					endif else widget_control,wtext,set_value=value
			endif
			if !mouse.button eq 1 then begin
				inf = (total(finite([lo,la])) eq 0.)
				x_r = x/float(!d.x_vsize)
				y_r = y/float(!d.y_vsize)
				do_it = 1
				if between(x_r,.00,.25) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon -30) mod 360)	else $
				if between(x_r,.75,1.0) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon +30) mod 360 )	else $
				if between(x_r,.25,.75) and between(y_r,.75,1.0) and inf then p0lat = ( 90 < round(p0lat +30) > (-90) )	else $
				if between(x_r,.25,.75) and between(y_r,.00,.25) and inf then p0lat = ( 90 < round(p0lat -30) > (-90) )	else do_it = 0

				limit = p0lat ge 0 ? 	[0.,p0lon-90.,90.-p0lat,p0lon+180.,0.,p0lon+90.,p0lat-90.,p0lon] : $
							[0.,p0lon-90.,p0lat+90.,p0lon,0.,p0lon+90.,-90.-p0lat,p0lon+180.]
				if do_it then m -> project,limit=limit,p0lon=p0lon,p0lat=p0lat,void_index=void_index
			endif
		endwhile
		if arg_present(obj_out) then begin
			; within map_image, decompose is set to 0 for non true color images, the cleanup resets this then,
			; since we dont destroy the map object we have to reset it manually to avoid crazy colors
 			device, decompose = decomp
 		endif else begin
			obj_destroy, m
		endelse
		widget_control,wtext,set_value='             Pixel Values'
	endif

	if nv_cnt gt 0 then begin
		lim = ''
		if limit_test then begin
			if n_elements(limit) eq 4 then lim = ' - Limit: ['+strjoin(string(limit,f='(f6.1)'),',')+']'
		endif
		if keyword_set(cov) then lim = ' - '+strupcase(cov)
		if keyword_set(limit) then begin
			dumlimit = limit
			if keyword_set(antarctic) then dumlimit = [-90.0,-180,-60.0,180]
			if keyword_set(arctic) then dumlimit = [ 60.0,-180, 90.0,180]
			if n_elements(dumlimit) eq 4 then begin
				dumidx=where(between(lon[nv_idx],dumlimit[1],dumlimit[3]) and between(lat[nv_idx],dumlimit[0],dumlimit[2]),dumcnt)
				print,'-----------'+dat+lim+'--------------'
				print,'Global Mean '+algon+'  :',string(dumcnt eq 0 ? fillvalue[0] : gmean(bild[nv_idx[dumidx]],lat[nv_idx[dumidx]]),f='(f12.4)')
				print,'------------------------------------'
			endif else begin
				print,'-----------'+dat+lim+'--------------'
				print,'Global Mean '+algon+'  :',string(gmean(bild[nv_idx],lat[nv_idx]),f='(f12.4)')
				print,'------------------------------------'
			endelse
		endif else begin
			print,'-----------'+dat+lim+'--------------'
			print,'Global Mean '+algon+'  :',string(gmean(bild[nv_idx],lat[nv_idx]),f='(f12.4)')
			print,'------------------------------------'
		endelse
	endif
	
	if keyword_set(verbose) then begin
		caldat, systime(/utc, /julian), mo, da, ye, ho, mi, se
		dat_str	= string(da, mo, ye, ho, mi, format = '(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2," [UTC] / ")')
		print, dat_str + 'Plot_L2 -> '+string(systime(1)-starttime,f='("Duration        : ", f7.3, " Sec")')
		print, dat_str + 'Plot_L2 -> '+string(float(memory(/highwater)-mem_cur)/1024.^3,f='("Memory required : ", f7.3, " GiB")')
	endif

	out = {bild:bild,lon:lon,lat:lat,unit:unit,fillvalue:fillvalue,longname:longname}

	if keyword_set(show_values) then show_pixel_value, bild, lon,lat, data=dat[0], unit=unit
	if limit_test then limit=limit_bck else free,limit

end
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
pro plot_l2_save_serie, year, month, day ,sat = sat, limit=limit, logarithmic=logarithmic, land = land, sea = sea	, $
			iso = iso, file = file, magnify=magnify, verbose = verbose	, $
			hist_cloud_type = hist_cloud_type, hist_phase=hist_phase, algoname = algoname	, $
			show_values = show_values, level = level, orbit = orbit, timeseries = timeseries	, $
			p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
			mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal		, $
			hammer = hammer, goode = goode, robinson = robinson, stereographic = stereographic 			, $
			ctable = ctable, other = other, discrete = discrete, prefix=prefix	, $
			nobar=nobar,cov = cov,ztext = ztext,msg_proj=msg_proj	, $
			oplots=oplots,error=error,dim3=dim3,rot=rot,datum=datum, $
			addtext=addtext,countries=countries,notitle=notitle,save_as=save_as,$
			no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box

	pug  = 0
	pvir = 1

	if pug then begin
		if level eq 'l3c' then begin
			varnames = ['cfc','cph','cot','cer','ctp','lwp','iwp','cla_vis006','hist2d_cot_ctp','hist2d_cot_ctp']
			minv	 = [    0,    0,    0,    3,  200,    0,    1,          0,                0,               0]
			maxv	 = [    1,    1,   40,   60,  900,  500, 2000,          1,                8,             100]
			hct      = [   '',   '',   '',   '',   '',   '',   '',         '',         'hist2d',           'cu' ]
		endif else if level eq 'l3u' then begin
			varnames = ['cmask','cph','cot','cer','ctp','cwp','cla_vis006']
			minv	 = [      0,    0,    0,  0.1,   10,    0,           0]
			maxv	 = [      1,    2,   30,   70, 1050,  300,           1]
			hct      = [     '',   '',   '',   '',   '',   '',          '']
			varnames += (sat_ampm(sat) eq 'am' ? '_desc' : '_asc')
		endif else begin
			ok = dialog_message('Dont know what to do! Stop!')
			stop
		endelse
		robinson  = 1
	endif

	if pvir then begin
		if level eq 'l3c' then begin
			varnames1 = ['cfc','ctp','cot_liq','cot_ice','cer_liq','cer_ice','lwp','iwp','cph']+'_pvir'
			minv1	  = [    0,  200,        0,        0,        0,        0,    0,    0,   0]
			maxv1	  = [    1,  900,       40,       40,       40,       60,  500,  800,   1]
			varnames2 = ['cfc','ctp','cot_liq','cot_ice','cer_liq','cer_ice','lwp','iwp','cph']+'_std_pvir'
			minv2	  = [    0,    0,        0,        0,        0,        0,    0,    0,   0]
			maxv2	  = [  0.5,  250,       30,       30,       20,       20,  300,  600, 0.5]
			varnames  = [varnames1,varnames2]
			minv      = [minv1,minv2]
			maxv      = [maxv1,maxv2]
			magnify   = total(algo2ref(algoname,sat=sat) eq ['myd2','mod2','pmx','isp']) ? 1 : -1
			robinson  = 1
			nobar     = 1
			timeseries= 1
			hct       = strarr(n_elements(varnames))
		endif else begin
			ok = dialog_message('Dont know what to do! Stop!')
			stop
		endelse
	endif

	print,'Will now create '+string(n_elements(varnames),f='(i3)')+' Figures! Using magnify = '+strcompress(magnify)

	for dd = 0,n_elements(varnames)-1 do begin

		if stregex(varnames[dd],'cmask_',/fold,/bool) then begin
			ctable   = '-49'
			free, other
		endif else if (stregex(varnames[dd],'cph_asc',/fold,/bool) or stregex(varnames[dd],'cph_desc',/fold,/bool)) then begin
			free, ctable
			other  = 'rainbow'
		endif else begin
			free, ctable
			other  = 'extended_rainbow'
		endelse

		plot_l2, year, month, day ,sat = sat, data = varnames[dd], mini = minv[dd], maxi = maxv[dd]	, $
			limit=limit, save_as=save_as, logarithmic=logarithmic, land = land, sea = sea			, $
			iso = iso, file = file, magnify=magnify, verbose = verbose, win_nr =-1					, $
			hist_cloud_type = hct[dd], hist_phase=hist_phase, algoname = algoname					, $ 
			show_values = show_values, level = level, orbit = orbit, timeseries = timeseries		, $
			p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic					, $
			mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal							, $
			hammer = hammer, goode = goode, robinson = robinson, stereographic = stereographic 		, $
			ctable = ctable, other = other, prefix=prefix											, $
			nobar=nobar,cov = cov,wtext = wtext,ztext = ztext,msg_proj=msg_proj						, $
			oplots=oplots,error=error,white_bg=white_bg,dim3=dim3,rot=rot,datum=datum				, $
			obj_out=obj_out,addtext=addtext,countries=countries,notitle=notitle						, $
			no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box

	endfor

end
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
pro compare_l2, file1, file2, data1=data1, data2=data2, mini=mini, maxi=maxi, bin=bin	, $
		save_as=save_as, win_nr=win_nr, check_quality=check_quality, g_eq=g_eq	, $
		l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom		, $
		limit=limit, show_values=show_values, out = out, sat1=sat1, sat2=sat2	, $
		algo2=algo2, algo1=algo1, year = year	, htypes = htypes		, $
		month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
		p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
		mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
		robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
		diff_only=diff_only,hist_only=hist_only,level=level,maps_only=maps_only	, $
		ctable = ctable, other = other,zonal_only = zonal_only, stereographic = stereographic			, $
		box_only = box_only, coverage = coverage, nobar = nobar, ztext = ztext	, $
		msg=msg, logarithmic=logarithmic,timeseries=timeseries,dim3=dim3	, $
		addtext=addtext,rot=rot,magnify=magnify,wtext=wtext,countries=countries,notitle=notitle,$
		no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box,$
		version1=version1,version2=version2

	mem_cur   = memory(/current)
	starttime = systime(1)
	win_nr    = adv_keyword_set(win_nr) ? win_nr : 1
	ts        = keyword_set(timeseries)
	dat1      = get_product_name(data1,algo=algo1)
	dat2      = keyword_set(data2) ? get_product_name(data2,algo=algo2) : get_product_name(data1,algo=algo2)
	satn1     = sat_name(algo1,sat1, year=(ts ? 0:year), month=(ts ? 0:month),level=level,version=version1)
	satn2     = sat_name(algo2,sat2, year=(ts ? 0:year), month=(ts ? 0:month),level=level,version=version2)
	if adv_keyword_set(mini) then mini = strsplit(mini,',',/ext)
	if adv_keyword_set(maxi) then maxi = strsplit(maxi,',',/ext)
	histo1d  = is_h1d(dat1) or is_h1d(dat2)
	hct = keyword_set(htypes) ? strlowcase(htypes[0]) : ''

	fg_col = !p.color eq 0 ? 'black' : 'white' ; schwarz oder weiss mehr hintergrund haben wir nicht! Geht!

	set_colors, rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply

	if keyword_set(addtext) then begin
		at = strsplit(addtext,',',/ext)
		if n_elements(at) eq 2 then begin
			f1str  = 'F1 ('+at[0]+'): '
			f2str  = 'F2 ('+at[1]+'): '
		endif else begin
			f1str  = 'F1 ('+addtext[0]+'): '
			f2str  = 'F2 ('+addtext[0]+'): '
		endelse
		no_time_string = 1
	endif else begin
		f1str  = 'File1: '
		f2str  = 'File2: '
		if dat1 ne dat2 then begin
			f1str  = strreplace(f1str,':','')+strupcase(dat1)+': '
			f2str  = strreplace(f2str,':','')+strupcase(dat2)+': '
		endif
	endelse
	annex  = ''
	is_new = 0.
	if satn1 eq satn2 and satn1 ne '' then begin
		; wann wurde das letzte mal in datei geschrieben?
		mtime1 = (file_info(file1)).mtime
		mtime2 = (file_info(file2)).mtime
		if mtime1 gt mtime2 then begin
			if keyword_set(verbose) then print,'File1 with modification time: '+unix2ymdhms(mtime1)+' is newer than File2: '+unix2ymdhms(mtime2)
			if ~keyword_set(no_time_string) then f1str  = strreplace(f1str,':','')+' (new): '
			if ~keyword_set(no_time_string) then f2str  = strreplace(f2str,':','')+' (old): '
			is_new = 1.
		endif else if mtime2 gt mtime1 then begin
			if keyword_set(verbose) then print,'File2 with modification time: '+unix2ymdhms(mtime2)+' is newer than File1: '+unix2ymdhms(mtime1)
			if ~keyword_set(no_time_string) then f1str  = strreplace(f1str,':','')+' (old): '
			if ~keyword_set(no_time_string) then f2str  = strreplace(f2str,':','')+' (new): '
			is_new = 2.
		endif else if keyword_set(verbose) then print,'modification time of File1: '+unix2ymdhms(mtime1)+' and File2: '+unix2ymdhms(mtime2)+' are equal!'
	endif

	found = 0

	if ~(keyword_set(hist_only) or keyword_set(diff_only) or keyword_set(zonal_only) or keyword_set(box_only) or (histo1d and hct eq '1d')) then begin
; 		if keyword_set(maps_only)  then !p.multi = [0,2,1] else !p.multi=[0,1,2]
		!p.multi=keyword_set(globe) ? [0,2,1] : [0,1,2]
	endif
	save_dir = !SAVE_DIR + '/diffs/'

	if keyword_set(land) and keyword_set(sea) then begin
		land = 0
		sea  = 0
	endif
	ls = ( keyword_set(land) or keyword_set(sea))

	if ts then begin
		if dat1 ne dat2 then begin
			d  = get_available_time_series( algo1, dat1, sat1, coverage = coverage, period = !DEFAULT_TS_PERIOD, sav_file = sav_file, found = found, $
							trend = trend, tr_corr = tr_corr, anomalies = anomalies, uncertainty = uncertainty, $
							stddev = stddev, no_trend_found = no_trend_found)
			if ~found then begin
				if ~no_trend_found then ok = dialog_message("compare_l2: Sav File not found! "+sav_file)
				return
			endif
			bild1  = d.mean
			unit1  = d.unit
			datum1 = d.actual_date
			d2 = get_available_time_series( algo2, dat2, sat2, coverage = coverage, period = !DEFAULT_TS_PERIOD, sav_file = sav_file, found = found2, $
							trend = trend, tr_corr = tr_corr, anomalies = anomalies, uncertainty = uncertainty, $
							stddev = stddev, no_trend_found = no_trend_found)
			if ~found2 then begin
				if ~no_trend_found then ok = dialog_message("compare_l2: Sav File not found! "+sav_file)
				return
			endif
			bild2  = d2.mean
			unit2  = d2.unit
			datum2 = d2.actual_date
		endif else begin
			d = get_available_time_series( 	algo1, dat1, sat1, coverage = coverage, reference = algo2, period = !DEFAULT_TS_PERIOD, sav_file = sav_file, $
							trend = trend, tr_corr = tr_corr, anomalies = anomalies, uncertainty = uncertainty, found = found, $
							stddev = stddev, no_trend_found = no_trend_found)
			if not found then begin
				if ~no_trend_found then ok = dialog_message((strmatch(algo1,algo2,/fold) ? "compare_l2: Algo1 and Algo2 are the same! " : "compare_l2: Sav File not found! ")+sav_file)
				return
			endif
			longname = d.longname
			if trend then begin
				bild1 = d.TREND.MEAN 
				bild2 = d.TREND.MEAN2 
				longname = longname + ' - trend per decade'
			endif else if anomalies then begin
				bild1 = d.TREND.ANOM_MEAN
				bild2 = d.TREND.ANOM_MEAN2
				longname = longname + ' - Anomalies'
			endif else if uncertainty then begin
				bild1 = d.UNC 
				bild2 = d.UNC2 
				longname = longname + ' - Uncertainty'
			endif else if stddev then begin
				bild1 = d.STD 
				bild2 = d.STD2 
				longname = longname + ' - Standard Deviation'
			endif else begin
				bild1 = d.MEAN
				bild2 = d.MEAN2
			endelse
			unit1 = d.unit
			unit2 = d.unit
			datum1=d.actual_date
			datum2=d.actual_date
		endelse
		fillvalue1 = -999.
		fillvalue2 = -999.
		datum=d.actual_date
	endif else begin
		if keyword_set(check_quality) then qcflag1 = fix(get_data(file=file1,data='qcflag', verbose = verbose))
		bild1 = get_data(year, month, day,file=file1,data=dat1,sat=sat1,algo=algo1,no_data_value=fillvalue1,dim3=dim3,unit=unit1,$
			found=found,verbose=verbose,orbit=orbit,var_dim_names=var_dim_names1,/print_filename, flag_meanings=flag_meanings1)
		if not found then begin
			ok=dialog_message('compare_l2: Could not read data! '+file1+' '+dat1)
			return
		endif
		if keyword_set(check_quality) then qcflag2 = fix(get_data(file=file2,data='qcflag',verbose = verbose))
		bild2 = get_data(year, month, day,file=file2,data=dat2,sat=sat2,algo=algo2,dim3=dim3,no_data_value=fillvalue2,unit=unit2,$
			found=found,verbose=verbose,orbit=orbit,level=level,print_filename=2, flag_meanings=flag_meanings2)
		if not found then begin
			ok=dialog_message('compare_l2: Could not read data! '+file2+' '+dat2)
			return
		endif
	endelse
	dat = dat1

	;change byte into integer to avoid problems when making differences
	if size(bild1,/type) eq 1 then bild1 = fix(bild1)
	if size(bild2,/type) eq 1 then bild2 = fix(bild2)

	if keyword_set(rot) then begin
		if size(reform(bild1),/n_dim) eq 2 then begin
			bild1 = rotate(bild1,fix(rot>0))
		endif
		if size(reform(bild2),/n_dim) eq 2 then begin
			bild2 = rotate(bild2,fix(rot>0))
		endif
	endif

	if keyword_set(save_as) and ~keyword_set(zoom) then begin

		if save_as eq '1' then begin 
			save_as = save_dir+'Diff_'+file_basename(file1,is_hdf(file1) ? '.hdf':'.nc')+'_-_'+$
			file_basename(file2,is_hdf(file1) ? '.hdf':'.nc')+$
			'_'+dat+(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')+$
			(keyword_set(limit) ? '_limit_'+strjoin(strcompress(string(limit,f='(f6.1)'),/rem),'_') : '')+$
			(keyword_set(maps_only) ? '_2D_maps':'')+$
			(keyword_set(hist_only) ? '_histos_only':'')+$
			(keyword_set(box_only) ? '_box_only':'')+$
			(keyword_set(zonal_only) ? '_zonal_only':'')+$
			(keyword_set(diff_only) ? '_diff_only':'')+$
			(keyword_set(htypes)    ?  '_JCH_'+strupcase(htypes[0]):'')+'.eps'
		endif
	endif else if win_nr ne -1 then win, win_nr, xs=1200, ys=800,title=dat

	if is_jch(dat) then begin
		if total(strlowcase(htypes[0]) eq ['1d','1d_cot','1d_ctp','max']) then begin
			print,'we should not be here. Check!'
			stop
		endif
		apx = is_jch(dat,/ice) ? 'Ice ' : (is_jch(dat,/liq) ? 'Liquid ' : '')
		if total(strlowcase(htypes[0]) eq ['hist2d','hist_2d']) then begin
			if is_jch(dat,/ratio) then begin
				ok = dialog_message('plot_l2: '+htypes[0]+' not possible with hist2d_ratio!')
				return
			endif
			bild1 = get_2d_rel_hist_from_jch(bild1, algo1, dem = dem, land = land, sea = sea, limit = limit, antarctic = antarctic, arctic = arctic, $
							lon = lon, lat = lat, fillvalue = fillvalue1, found = found1)
			bild2 = get_2d_rel_hist_from_jch(bild2, algo2, dem = dem, land = land, sea = sea, limit = limit, antarctic = antarctic, arctic = arctic, $
							lon = lon, lat = lat, fillvalue = fillvalue2, found = found2)
			plot_2d_rel_hist, bild1, f1str+satn1, bild2=bild2, name2=f2str+satn2, col_tab=col_tab, brewer=brewer, $
					mini=mini, maxi=maxi, save_as=save_as, difference = diff_only, appendix = apx,nobar=nobar
			bild=(bild1-bild2)
			idx = where(bild1 eq fillvalue1 or bild eq fillvalue2,idxcnt)
			if idxcnt gt 0 then bild[idx]=fillvalue2
			out  = {lon:-999, lat:-999, bild:bild,unit:'%'}
			return
		endif else begin
			if htypes[0] eq '' then htypes[0] = 'cu'
			if (get_grid_res(bild1[*,*,0,0,0]) ne get_grid_res(bild2[*,*,0,0,0])) then $
			grid_res_out = max([get_grid_res(bild1[*,*,0,0,0]),get_grid_res(bild2[*,*,0,0,0])],/nan) 

			if is_jch(dat,/ratio) then begin
				liq = get_hct_data(htypes[0],bild1[*,*,*,*,0],algo1,sdum=sdum1,found=found1,grid=grid_res_out)
				ice = get_hct_data(htypes[0],bild1[*,*,*,*,1],algo1,sdum=sdum1,found=found1,grid=grid_res_out)
				bild1 = reform(float(liq)/float((liq>0)+(ice>0)>1)) *100.
				didx = where(liq le 0 and ice le 0,didxcnt)
				if didxcnt gt 0 then bild1[didx] = fillvalue1
				liq = get_hct_data(htypes[0],bild2[*,*,*,*,0],algo2,sdum=sdum2,found=found2,grid=grid_res_out)
				ice = get_hct_data(htypes[0],bild2[*,*,*,*,1],algo2,sdum=sdum2,found=found2,grid=grid_res_out)
				bild2 = reform(float(liq)/float((liq>0)+(ice>0)>1)) *100.
				didx = where(liq le 0 and ice le 0,didxcnt)
				if didxcnt gt 0 then bild2[didx] = fillvalue2
				cbar_title = 'Liquid Fraction '+(keyword_set(htypes[0]) ? 'of '+strupcase(htypes[0])+' Clouds [%]':'')
				if found1 and found2 then begin
					grid_res_out = get_grid_res(bild2)
					make_geo,lon,lat,grid=grid_res_out
					unit1 = textoidl(' [%]')
					unit2 = textoidl(' [%]')
					no_gridding = 1 ; is already regridded and on same unit
				endif
			endif else begin
				bild1 = get_hct_data(htypes[0],bild1,algo1,found=found1,/relative,sdum=sdum1,grid = grid_res_out)
				bild2 = get_hct_data(htypes[0],bild2,algo2,found=found2,/relative,sdum=sdum2,grid = grid_res_out)
				if found1 and found2 then begin
					grid_res_out = get_grid_res(bild2)
					make_geo,lon,lat,grid=grid_res_out
					ratio1 = get_hct_ratio(bild1,sdum1,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,lat=lat,dem=dem,land=land,sea=sea,/relative)
					ratio2 = get_hct_ratio(bild2,sdum2,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon,lat=lat,dem=dem,land=land,sea=sea,/relative)

					print, f1str+satn1+' Ratio '+htypes[0]+'/all: '+ratio1
					print, f2str+satn2+' Ratio '+htypes[0]+'/all: '+ratio2
					idx = where(sdum1 eq 0,idxcnt)
					if idxcnt gt 0 then bild1[idx] = fillvalue1
					idx = where(sdum2 eq 0,idxcnt)
					if idxcnt gt 0 then bild2[idx] = fillvalue2
					unit1 = textoidl(' [%]')
					unit2 = textoidl(' [%]')
					no_gridding = 1 ; is already regridded and on same unit
				endif
			endelse
		endelse
		if not found1 then begin
			ok=dialog_message('compare_l2: Could not read data! '+file1+' '+dat1+' '+htypes[0])
			return
		endif
		if not found2 then begin
			ok=dialog_message('compare_l2: Could not read data! '+file2+' '+dat2+' '+htypes[0])
			return
		endif
	endif

	if histo1d then begin
		if strmid(hct,0,2) eq '1d' then begin
			bild1 = get_1d_rel_hist_from_1d_hist( bild1, dat1, algo=algo1, limit=limit, land=land, sea=sea, arctic=arctic, antarctic=antarctic,$
					xtickname=xtickname,ytitle=ytitle,hist_name=data_name,found=found1,file=file1,var_dim_names=var_dim_names1, $
					bin_val=bin_val,shape_file=shape_file)
			bild2 = get_1d_rel_hist_from_1d_hist( bild2, dat2, algo=algo2, limit=limit, land=land, sea=sea, arctic=arctic, antarctic=antarctic,$
					found=found2,shape_file=shape_file)
			if found1 and found2 and (n_elements(bild1) eq n_elements(bild2)) then begin
				start_save, save_as, thick = thick
					yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,50]
					apx = is_h1d(dat,/ice) ? 'Ice ' : (is_h1d(dat,/liquid) ? 'Liquid ' : '')
					zwi1 = (algo1 eq 'coll6' and stregex(dat,'ctt',/fold,/bool)) ? ' (Day only)' : ''
					zwi2 = (algo2 eq 'coll6' and stregex(dat,'ctt',/fold,/bool)) ? ' (Day only)' : ''
					plot,[0,0],[1,1],yr=yrange,xr=[0,n_elements(bild1)-1],xticks=n_elements(xtickname)-1, xtickname = xtickname, $
					xtitle=apx+data_name,ytitle=ytitle,xminor=2,charsize = (keyword_set(save_as) ? 2.5 : 1.5),$
					charthick = (keyword_set(save_as) ? 2. : 1)
					didx = where(bild1 ge 0,didx_cnt)
; 					if didx_cnt gt 0 then oplot,didx,bild1[didx],thick=thick,psym=-8
					if didx_cnt gt 0 then oplot,didx,bild1[didx],thick=thick,col=cgcolor(!compare_col1) ,psym=-8
					didx = where(bild2 ge 0,didx_cnt)
; 					if didx_cnt gt 0 then oplot,didx,bild2[didx],thick=thick,psym=-8,col=cgcolor('Red')
					if didx_cnt gt 0 then oplot,didx,bild2[didx],thick=thick,psym=-8,col=cgcolor(!compare_col2)
; 					legend,[f1str+satn1+zwi1,f2str+satn2+zwi2],psym=[8,8],numsym=1,color=[-1,cgcolor('Red')],thick=[thick,thick],$
					legend,[f1str+satn1+zwi1,f2str+satn2+zwi2],psym=[8,8],numsym=1,color=[cgcolor(!compare_col1) ,cgcolor(!compare_col2) ],thick=[thick,thick],$
					spos=keyword_set(show_value) ? 'bot':'top', charsize=(keyword_set(save_as) ? 2.:1.5)
				end_save,save_as
				return
			endif else begin
				if ~found1 then ok=dialog_message('Compare_l2: Could not create Hist1D data vector for '+satn1) else $
				if ~found2 then ok=dialog_message('Compare_l2: Could not create Hist1D data vector for '+satn2) else $
				print,'Compare_l2: Something went wrong with plotting Hist_1D (hct = 1d) data!'
				return
			endelse
		endif else begin
			bild1 = reform(bild1[*,*,(keyword_set(dim3) ? dim3:0)])/(total(bild1,3) > 1.) *100.
			bild2 = reform(bild2[*,*,(keyword_set(dim3) ? dim3:0)])/(total(bild2,3) > 1.) *100.
			unit1 = textoidl(' [%]')
			unit2 = textoidl(' [%]')
		endelse
		; wenn bis hier gekommen dann checken ob beide gleiche auflösungen haben
		ggres1 = get_grid_res(bild1)
		ggres2 = get_grid_res(bild2)
		grid_res_out = ggres1 eq ggres2 ? ggres1 : max([ggres1,ggres2])
		make_geo,lon,lat,grid=grid_res_out
		if ggres1 ne ggres2 then begin
			if ggres1 lt ggres2 then begin
				if rebinable(ggres1,grid_res_out) then begin
					bild1 = grid_down_globe(bild1,grid_res_out,no_data_value=fillvalue1,found=found,/total)
				endif else begin
					make_geo,lond,latd,grid=ggres1
					dum = sat2global(lond,latd,bild1,no_data_value=fillvalue1,grid_res=grid_res_out,found=found)
					if found then bild1 =dum.sum
				endelse
			endif else begin
				if rebinable(ggres2,grid_res_out) then begin
					bild2 = grid_down_globe(bild2,grid_res_out,no_data_value=fillvalue2,found=found,/total)
				endif else begin
					make_geo,lond,latd,grid=ggres2
					dum = sat2global(lond,latd,bild2,no_data_value=fillvalue2,grid_res=grid_res_out,found=found)
					if found then bild2 =dum.sum
				endelse
			endelse
		endif
		no_gridding = 1
		fillvalue1 = 0
		fillvalue2 = 0
	endif

	if ~keyword_set(no_gridding) then bring_to_same_grid_and_unit,dat,bild1,bild2,fillvalue1,fillvalue2,file1=file1,file2=file2, $
							algo1,algo2,unit1,unit2,level=level,lon,lat,grid_res_out,verbose = verbose, $
							flag_meanings1=flag_meanings1,flag_meanings2=flag_meanings2,no_unit=ts

	n_lev1 =6
	n_lev2 =6

	if ~keyword_set(maps_only) then begin
		free, flag_meanings1
		free, flag_meanings2
	endif
	if keyword_set(flag_meanings1) then begin
		bar_tickname1 = [flag_meanings1]
		discrete1 = findgen(n_elements(flag_meanings1)+1)+min(bild1[where(bild1 ne fillvalue1[0])])
		n_lev1 = n_elements(flag_meanings1)
		g_eq = 0
		l_eq = 0
	endif
	if keyword_set(flag_meanings2) then begin
		bar_tickname2 = [flag_meanings2]
		discrete2 = findgen(n_elements(flag_meanings2)+1)+min(bild2[where(bild2 ne fillvalue2[0])])
		n_lev2 = n_elements(flag_meanings2)
		g_eq = 0
		l_eq = 0
	endif

	;bereite out struktur vor, für show pixel value
	bild=(bild1-bild2)
	idx = where(bild1 eq fillvalue1 or bild eq fillvalue2,idxcnt)
	if idxcnt gt 0 then bild[idx]=fillvalue2
	out  = {lon:lon, lat:lat, bild:bild,unit:unit1}

	; binsize für histogramme
	bin = (10.^(strlen(strcompress(string(max(bild1),f='(i)'),/rem)))/1000.) < 10.
	if stregex(dat,'ctp',/bool,/fold) then bin = 10.
	if stregex(dat,'cmask',/bool,/fold) then bin = 1
	if is_jch(dat) then bin = 1

; 	if ls then dem = get_dem(lon,lat,grid_res=get_grid_res(lon))
	if ls then dem = get_coverage(lon, lat, /land)

	lat_res = 1. > get_grid_res(lon)

	good_idx = where(lon ne -999 and lat ne -999, gidx_cnt)
	if gidx_cnt eq 0 then begin
		print,'no valid Lon Lat points!'
		return
	endif
	lon   = lon[good_idx]
	lat   = lat[good_idx]
	bild1 = bild1[good_idx]
	bild2 = bild2[good_idx]
	if ls then dem = dem[good_idx]

	if keyword_set(check_quality) then begin
		; alles okay
		chk_cnt = 0
		if strlowcase(check_quality) eq 'all' then begin 
			chk_idx = where(qcflag1[good_idx] eq 0 and qcflag2[good_idx] eq 0, chk_cnt)
		endif else if strlowcase(check_quality) eq 'convergence' then begin
			chk_idx = where(((qcflag1[good_idx] and 2^6) eq 2^6) eq 0 and ((qcflag2[good_idx] and 2^6) eq 2^6) eq 0, chk_cnt)
		endif else if strlowcase(check_quality) eq 'cost' then begin
			chk_idx = where(((qcflag1[good_idx] and 2^7) eq 2^7) eq 0 and ((qcflag2[good_idx] and 2^7) eq 2^7) eq 0, chk_cnt)
		endif else begin
			; cot state variable error not! out of bounds
			if dat eq 'cot'   then chk_idx = where(((qcflag1[good_idx] and 2^1) eq 2^1) eq 0 and ((qcflag2[good_idx] and 2^1) eq 2^1) eq 0, chk_cnt)
			; ref state variable error not! out of bounds
			if dat eq 'ref'   then chk_idx = where(((qcflag1[good_idx] and 2^2) eq 2^2) eq 0 and ((qcflag2[good_idx] and 2^2) eq 2^2) eq 0, chk_cnt)
			; ctp state variable error not! out of bounds
			if dat eq 'ctp'   then chk_idx = where(((qcflag1[good_idx] and 2^3) eq 2^3) eq 0 and ((qcflag2[good_idx] and 2^3) eq 2^3) eq 0, chk_cnt)
			; ctt state variable error not! out of bounds
			if dat eq 'ctt'   then chk_idx = where(((qcflag1[good_idx] and 2^4) eq 2^4) eq 0 and ((qcflag2[good_idx] and 2^4) eq 2^4) eq 0, chk_cnt)
			; stemp state variable error not! out of bounds
			if dat eq 'stemp' then chk_idx = where(((qcflag1[good_idx] and 2^5) eq 2^5) eq 0 and ((qcflag2[good_idx] and 2^5) eq 2^5) eq 0, chk_cnt)
		endelse
		if chk_cnt gt 0 then begin
			lon   = lon[chk_idx]
			lat   = lat[chk_idx]
			bild1 = bild1[chk_idx]
			bild2 = bild2[chk_idx]
		endif
	endif

	if keyword_set(limit) then begin
		qw = where(between(lon,limit[1],limit[3]) and between(lat,limit[0],limit[2]))
		bild1 = bild1[qw]
		bild2 = bild2[qw]
		lon   = lon[qw]
		lat   = lat[qw]
		if ls then dem = dem[qw]
	endif

	void_index = where(bild1 eq fillvalue1[0] or bild2 eq fillvalue2[0],complement=idx,ncomplement=n_gesamt)
	if keyword_set(land) then void_index = where(bild1 eq fillvalue1[0] or bild2 eq fillvalue2[0] or dem eq 0,complement=idx,ncomplement=n_gesamt)
	if keyword_set(sea)  then void_index = where(bild1 eq fillvalue1[0] or bild2 eq fillvalue2[0] or dem ne 0,complement=idx,ncomplement=n_gesamt)

	rmse = grmse(bild1[idx],bild2[idx],lat[idx])
	bias = gbias(bild1[idx],bild2[idx],lat[idx])
; 	stdd = stddev(bild1[idx]-bild2[idx]) 
	stdd = sqrt(rmse^2 - bias^2)
	corr = gcorrelate(bild1[idx],bild2[idx],lat[idx])

	str_pholder = strjoin(replicate(' ',max([strlen(f1str+satn1),strlen(f2str+satn2)])))
	print,'-----------'+dat+'--------------'
	if stregex(dat,'nobs',/fold,/bool) or stregex(dat,'nretr',/fold,/bool) then begin
		print,'Total           '+string(f1str+satn1,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(total(bild1[idx]),f='(f15.1)')
		print,'Total           '+string(f2str+satn2,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(total(bild2[idx]),f='(f15.1)')
	endif
	print,'Glob. Mean      '+string(f1str+satn1,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(gmean(bild1[idx],lat[idx]),f='(f11.4)')
	print,'Glob. Mean      '+string(f2str+satn2,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(gmean(bild2[idx],lat[idx]),f='(f11.4)')
	print,'Glob. BIAS      '+str_pholder+' :',string(bias,f='(f11.4)')
	print,'Glob. RMSE      '+str_pholder+' :',string(rmse,f='(f11.4)')
	print,'Glob. BC-RMSE   '+str_pholder+' :',string(stdd,f='(f11.4)')
	print,'Correlation(^2) '+str_pholder+' :',string(corr,f='(f11.4)') +' ('+strcompress(string(corr^2.,f='(f11.4)'),/rem)+')'
	print,'-----------------------------'

	start_save, save_as, thick = thick, size = [40.,24. ];,size = 'A4',/landscape;[32,16];

	if keyword_set(box_only) then begin
		boxplot,year,month,day,data=dat,sat=sat1,limit=limit,mini = mini, maxi = maxi		, $
			coverage = coverage,algo=algo1,reference=algo2,error=error,win_nr=win_nr	, $
			datum = datum1,filename1=file1,filename2=file2,level=level
		end_save, save_as
		return
	endif

	set_proj, globe = globe, limit = limit, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat, nobar = nobar	, $
		    Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson	, $
		    ortho=ortho,iso=iso,bar_horizontal=bar_horizontal,grid=grid,londel=londel,latdel=latdel,label=label,noborder=noborder,msg=msg	, $
		    no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify,no_label=no_label,  $
		    stereographic=stereographic,maxvalue = adv_keyword_set(maxi) ? maxi[0]:maxvalue, bar_format=bar_format,lambert=lambert,$
		    lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_box=no_box,position = map_position,no_grid=no_grid,horizon=horizon	

	if keyword_set(zonal_only) then begin
		; cci und cci2 limit schon drin nur nicht land, sea
		cci = bild1
		idx_neu1 = where(cci ne fillvalue1,chk_idx1)
		if keyword_set(sea)  then idx_neu1 = where(cci ne fillvalue1 and dem eq 0,chk_idx1)
		if keyword_set(land) then idx_neu1 = where(cci ne fillvalue1 and dem ne 0,chk_idx1)
		if chk_idx1 gt 0 then medi1 = zonal_average(cci[idx_neu1],lat[idx_neu1],fillvalue=fillvalue1,lat_zon=lat1d1,/mean,/nan,lat_res=lat_res)

		; neu
		cci2= bild2
		idx_neu2 = where(cci2 ne fillvalue2,chk_idx2)
		if keyword_set(sea) then idx_neu2 = where(cci2 ne fillvalue2 and dem eq 0,chk_idx2)
		if keyword_set(land)  then idx_neu2 = where(cci2 ne fillvalue2 and dem ne 0,chk_idx2)
		if chk_idx2 gt 0 then medi2 = zonal_average(cci2[idx_neu2],lat[idx_neu2],fillvalue=fillvalue2,lat_zon=lat1d2,/mean,/nan,lat_res=lat_res)

		if chk_idx1 gt 0 and chk_idx2 gt 0 then begin
			lat1d   = [[lat1d1],[lat1d2]]
			array   = [[medi1],[medi2]]
			name    = [f1str+satn1,f2str+satn2]
			colors  = [(is_new eq 1 ? 'red':fg_col),(is_new ne 1 ? 'red':fg_col)]
			lstyle  = [0,0]
		endif else if chk_idx1 gt 0 and chk_idx2 eq 0 then begin
			lat1d   = [[lat1d1]]
			array   = [[medi1]]
			name    = [f1str+satn1]
			colors  = fg_col
			lstyle  = 0
		endif else if chk_idx1 eq 0 and chk_idx2 gt 0 then begin
			lat1d   = [[lat1d2]]
			array   = [[medi2]]
			name    = [f2str+satn2]
			colors  = fg_col
			lstyle  = 0
		endif else begin
			stop
		endelse

		if is_new and level eq 'l3c' then begin
			annex = (sat1 eq 'aatme' and total(dat[0] eq ['ctt','ctp','cph','cfc','cc_total','cth'])?'_day':'')
			set_algolist, algo_list, sat = sat, data = dat, exclude = [algo1,algo2],/default
			struc = get_all_avail_data(year,month,day,data=dat+annex,sat=sat1,level=level,algo_list=algo_list,$
			/make_compare,verbose=verbose,coverage = coverage,land=land,sea=sea,limit=limit,antarctic = antarctic, arctic = arctic,/zonal_mean)
			if is_struct(struc) then begin
				name    = [name,struc.algo_names]
				lat1d   = [[lat1d],[struc.zonal_lats]]
				array   = [[array],[struc.zonal_mean]]
				lspos   = ['top','top',struc.LEGEND_SPOS]
				lystr   = [0,1,struc.LEGEND_YSTRETCH]
				colors  = [colors,struc.COLORS]
				lstyle  = [lstyle,struc.LINESTYLE]
				free, struc
			endif
 		endif

		plot,[0,0],[1,1],xr=[-90,90],xs=3,xticks=6,xtickname=['-90','-60','-30','0','30','60','90'], $
		xtitle='latitude [degrees]',ytitle='zonal mean of '+strupcase(dat+annex)+unit1,yr=[mini[0],maxi[0]],title=title, $
		charthick = (keyword_set(save_as) ? 1.5:1.2), charsize = (keyword_set(save_as) ? 1.7:1.2), $
		xcharsize = (keyword_set(save_as) ? 1.7:1.2), ycharsize= (keyword_set(save_as) ? 1.7:1.2), $
		xmargin=[10,3]+(keyword_set(save_as) ? [4,0]:0),ymargin=[5,2]+(keyword_set(save_as) ? [2,1]:0)

		for i = 0, n_elements(name)-1 do begin & $
			oplot,lat1d[*,i],array[*,i],thick=thick,color=cgcolor(colors[i]),linestyle= lstyle[i] & $
			if i gt 1 then legend,name[i],color=cgcolor(colors[i]),thick=thick,spos=lspos[i],ystretch=lystr[i],$
			charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? 2.5:1.5),linestyle= lstyle[i] & $
		endfor
		legend,name[0:1],color=cgcolor(colors[0:1]),thick=replicate(thick,2),spos='top',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? 2.5:1.5)

		end_save, save_as,verbose=verbose
		return
	endif

	if ~keyword_set(diff_only) and ~keyword_set(maps_only) then begin
		if keyword_set(hist_only) then begin
			; cci und cci2 limit schon drin nur nicht land, sea
			cci = bild1
			idx1 = where(cci  ne fillvalue1,chk_idx1)
			if keyword_set(sea) then idx1 = where(cci ne fillvalue1 and dem eq 0,chk_idx1)
			if keyword_set(land)  then idx1 = where(cci ne fillvalue1 and dem ne 0,chk_idx1)
			; neu
			cci2= bild2
			idx2 = where(cci2 ne fillvalue2,chk_idx2)
			if keyword_set(sea) then idx2 = where(cci2 ne fillvalue2 and dem eq 0,chk_idx2)
			if keyword_set(land)  then idx2 = where(cci2 ne fillvalue2 and dem ne 0,chk_idx2)
			maximum = adv_keyword_set(maxi) ? maxi[0] : max([cci[idx1],cci2[idx2]])
			minimum = adv_keyword_set(mini) ? mini[0] : min([cci[idx1],cci2[idx2]])

			chk1= where([chk_idx1,chk_idx2] ne 0,chknr1)
			if chknr1 ne 2 then begin
				ok = dialog_message('compare_l2: No valid data found! ')
				return ; die sollten da sein sonst machts kein sinn
			endif

			name   = [f1str+satn1,f2str+satn2]
			colors = [(is_new eq 1 ? 'red':fg_col),(is_new ne 1 ? 'red':fg_col)]
			lspos  = ['top','top']
			lystr  = [0,1]
			lstyle = [0,0]

			if is_new and dat ne 'nobs' then begin
				annex = (sat1 eq 'aatme' and total(dat[0] eq ['ctt','ctp','cph','cfc','cc_total','cth'])?'_day':'')
				set_algolist, algo_list, sat = sat, data = dat, exclude = [algo1,algo2],/default
				struc = get_all_avail_data(year,month,day,data=dat+annex,sat=sat1,level=level,algo_list=algo_list,$
					/make_compare,verbose=verbose,coverage=coverage,land=land,sea=sea,limit=limit,antarctic = antarctic, arctic = arctic,histograms=bin)

					if is_struct(struc) then begin
; 						maximum = max([maximum,struc.MAXIMUM])
; 						minimum = min([minimum,struc.MINIMUM])
						maximum = struc.MAXIMUM
						minimum = struc.MINIMUM
						min = (-1e6 > minimum)
						max = (maximum < 1e6)
						dum   = histogram(cci[idx1],bin=bin,min=min,max=max)
						hist1 = dum/total(dum)*100.
						dum   = histogram(cci2[idx2],bin=bin,min=min,max=max)
						hist2 = dum/total(dum)*100.
						array = [[hist1],[hist2]]
						name  = [name,struc.algo_names]
						array = [[array],[struc.HISTOGRAMS]]
						lspos   = [lspos,struc.LEGEND_SPOS]
						lystr   = [lystr,struc.LEGEND_YSTRETCH]
						lstyle  = [lstyle,struc.LINESTYLE]
						colors  = [colors,struc.COLORS]
						free, struc
					endif
			endif else begin
				min = (-1e6 > minimum) ; ansonsten gibts abbruch oder memory allocation fehler
				max = (maximum < 1e6)  ; ansonsten gibts abbruch oder memory allocation fehler
				dum   = histogram(cci[idx1],bin=bin,min=min,max=max)
				hist1 = dum/total(dum)*100.
				dum   = histogram(cci2[idx2],bin=bin,min=min,max=max)
				hist2 = dum/total(dum)*100.
				array = [[hist1],[hist2]]
			endelse
			hmax  = max(array,/nan)
			dumm1 = total(array,2,/nan)

			xx = findgen(n_elements(hist1)) * bin + rnd(min,bin)
			xx_idx = where(dumm1 gt 0.1,xx_cnt)
			if xx_cnt eq 0 then stop
			xr=[adv_keyword_set(mini) ? mini[0] : min[0],adv_keyword_set(maxi) ? maxi[0] : max(xx[xx_idx])]
			if n_elements(mini) eq 2 then yr = [0,mini[1]] else $
			if n_elements(maxi) eq 2 then yr = [0,maxi[1]] else $
			yr = [0,hmax*1.05]

			plot,[0,0],[1,1],xtitle=(keyword_set(htypes)?strupcase(htypes[0]+' '):'')+strupcase(dat+annex)+' '+unit1,title=datum,xr=xr,thick=thick,$
			yrange=yr,ytitle='% of occur.'
			for i = 0, n_elements(name)-1 do begin & $
				oplot,xx,array[*,i],thick=thick,color=cgcolor(colors[i]),linestyle= lstyle[i] & $
				if i gt 1 then legend,name[i],color=cgcolor(colors[i]),thick=thick,spos=lspos[i],ystretch=lystr[i],$
				charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? 2.5:1.5),linestyle= lstyle[i] & $
			endfor
			legend,name[0:1],color=cgcolor(colors[0:1]),thick=replicate(thick,2),spos='top',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? 2.5:1.5)

		endif else begin
			if dat eq 'cc_total' then dat = 'cfc'
			; hist_2d plots
			regr  = linfit(bild1[idx],bild2[idx],yfit=yfit,CHISQR=CHISQR,/double)
			min_a = keyword_set(mini) ? mini[0] : (n_gesamt gt 0 ? min([bild1[idx],bild2[idx]]) : 0)
			max_a = keyword_set(maxi) ? maxi[0] : (n_gesamt gt 0 ? max([bild1[idx],bild2[idx]]) : 0)
			min_a = -1e6 > min_a ; ansonsten gibts abbruch oder memory allocation fehler
			max_a = max_a < 1e6  ; ansonsten gibts abbruch oder memory allocation fehler

			cc = 5
			if ~keyword_set(hist_only) then position = [0.02,0.0,0.4,0.87]

			dum = hist_2d(bild1[idx],bild2[idx],bin1=bin,bin2=bin,max1=max_a,max2=max_a,min1=min_a,min2=min_a)
			if stregex(dat,'cc_mask',/bool,/fold) then begin
				if n_gesamt gt 0 then dum = dum/total(dum)*100.
				bar_title= 'nr of occurrence [%]'
				bar_nlev = 3
				log=1
				bin_dum = 0
			endif else begin
				bar_title= 'nr of occurrence'
				log=1
				bin_dum=bin
			endelse

			if n_elements(dum) lt 4 then dum=congrid(dum,2,2)
			if total(dum) eq 0 then dum[0]=1
			view2d,dum,no_data_val=0,xtitle=f1str+get_product_name(dat,algo=algo1,level=level,/upper,h_types=htypes)+' '+unit1,$
			ytitle=f2str+get_product_name(dat,algo=algo2,level=level,/upper,h_types=htypes)+' '+unit2,bar_title= bar_title, $
			xticks = cc, xtickv = vector(0,(size(dum,/dim))[0]-1,cc+1),yticks = cc, ytickv = vector(0,(size(dum,/dim))[1]-1,cc+1), $
			xtickname=strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f3.1)':'(i)')),/rem), $
			ytickname=strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f3.1)':'(i)')),/rem), $
			title = 'Binsize = '+string(bin,f='(f6.3)')+unit1,log=log,position=position, bar_format='(i)', $
; 			charthick = 1., xcharsize = 1., ycharsize = 1.,bar_nlev=bar_nlev
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize

			if ~stregex(dat,'cc_mask',/bool,/fold) then begin
; 				oplot,!x.crange,[regr[0]/bin,regr[1]*!x.crange[1]+regr[0]/bin],linestyle=2
				oplot,!x.crange,[regr[1]*!x.crange+regr[0]/bin],linestyle=2
				oplot,!x.crange,!y.crange
				xyouts,.22,.87,'m  :'+string(regr[1],f='(f6.2)'),/normal
				xyouts,.22,.84,'Y0 :'+string(regr[0],f='(f6.2)'),/normal
			endif

			max_a = max([bild1[idx]-bild2[idx]])
bin= ( bin/100.) > 0.01 
			dd = where(abs(float(bild1[idx]-bild2[idx])) gt bin_dum,n_diffgt100)
			prozent = n_gesamt gt 0 ? string(float(n_diffgt100)/float(n_gesamt) *100.,f='(f5.2)')+'%' : ' No valid Data found'
			if n_gesamt gt 0 then dd = histogram(float(bild1[idx]-bild2[idx]),bin=bin)
			if ~keyword_set(hist_only) then position = [0.55,0.59,0.95,0.9]

			if n_gesamt gt 0 or total(dd) eq 0 then begin
				xx = findgen(n_elements(dd))*bin+min(bild1[idx]-bild2[idx])
				xrange = minmax(xx)
				if keyword_set(mini) then begin
					minv = strsplit(mini,',',/ext)
					if n_elements(minv) gt 1 then xrange[0] = fix(mini[1])
				endif
				if keyword_set(maxi) then begin
					maxv = strsplit(maxi,',',/ext)
					if n_elements(maxv) gt 1 then xrange[1] = fix(maxi[1])
				endif
				plot,xx,(dd/float(n_gesamt)*100.>0.001),/ylog,ytitle="% of occur.",$
				title=(keyword_set(htypes)?strupcase(htypes[0]+' '):'')+strupcase(dat)+' Difference '+f1str+'- '+f2str,position=position,/noerase,$
				charthick = 1., xcharsize = 1., ycharsize = 1.,thick=thick,xrange=xrange
				oplot,[0,0],10^!y.crange,linestyle=1
			endif else begin
				plot,[0,0],[1,1],/ylog,ytitle="% of occur.",title=(keyword_set(htypes)?strupcase(htypes[0]+' '):'')+ $
				strupcase(dat)+' Difference '+f1str+'- '+f2str,position=position,/noerase,charthick = 1., xcharsize = 1., ycharsize = 1.,thick=thick
			endelse
			xyouts,0.57,.87,'BIAS :'+string(bias,f='(f6.2)')+(strlowcase(dat) eq 'ctp'?' hPa':''),/normal
			xyouts,0.57,.84,'RMSE :'+string(rmse,f='(f6.2)')+(strlowcase(dat) eq 'ctp'?' hPa':''),/normal
			xyouts,0.57,.81,'STDD :'+string(stdd,f='(f6.2)')+(strlowcase(dat) eq 'ctp'?' hPa':''),/normal
			xyouts,0.57,.78,'gt +-'+strcompress(string(bin_dum,f='(f5.2)'),/rem)+' :'+prozent,/normal

		endelse
	endif

	if keyword_set(map_position) and keyword_set(nobar) then begin
		if nobar eq 99 and ~keyword_set(diff_only) and ~keyword_set(hist_only) then begin
			map_position1 = [0.,0.5,1.,1.]
			map_position  = [0.,0.,1.,0.5]
		endif
	endif

	if ~keyword_set(hist_only) then begin
		if keyword_set(maps_only) then begin
			figure_title = keyword_set(notitle) ? '' : f1str+satn1+' '+datum1
			void_index  = where(bild1 eq fillvalue1[0],complement=idix)
			if keyword_set(land) then void_index = where(bild1 eq fillvalue1[0] or dem eq 0,complement=idx)
			if keyword_set(sea)  then void_index = where(bild1 eq fillvalue1[0] or dem ne 0,complement=idx)
			if adv_keyword_set(maxi) then if float(maxi[0]) lt (max(floor(bild1[where(bild1 ne fillvalue1[0])]*1000.))/1000.) then g_eq = 1
			if adv_keyword_set(mini) then begin
				mindum = min(bild1[where(bild1 ne fillvalue1[0],mincount)])
				if mincount gt 0 then if mindum lt mini[0] then l_eq = 1
			endif

			m = obj_new("map_image",bild1,lat,lon,void_index=void_index, $
				box_axes=box_axes,n_lev=n_lev1, max=adv_keyword_set(maxi) ? maxi[0] : max(bild1[idx]),$
				min=adv_keyword_set(mini) ? mini[0]: min(bild1[idx]), panoply = panoply, $
				countries=countries,usa=countries,magnify = magnify, figure_title = figure_title, $
				;charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
				charthick = !m_charthick, charsize  = !m_charsize,  $
				title= keyword_set(cbar_title) ? cbar_title : $
				(strupcase(dat) eq 'PHASE' ? 'CPH':get_product_name(dat,algo=algo1,level=level,/upper,h_types=htypes))+$
				' '+(total(strupcase(dat) eq ['COT','PHASE']) ? '' : unit1), logarithmic=logarithmic,horizon=horizon,$
				format=bar_format,  limit = limit,lambert=lambert, bar_tickname=bar_tickname1,discrete =discrete1, $
				ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel, g_eq=g_eq, l_eq=l_eq, $
				noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar, flip_colours = flip_colours,$
				p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine,position = map_position1, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
				mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,$
				stereographic=stereographic,label=label, $
				bwr=bwr,rainbow=rainbow,extended_rainbow=extended_rainbow,elevation=elevation,greyscale=greyscale,$
				brewer=brewer,ctable=ctable,debug=verbose)
			obj_destroy,m
			void_index = where(bild2 eq fillvalue2[0],complement=idx)
			if keyword_set(land) then void_index = where(bild2 eq fillvalue2[0] or dem eq 0,complement=idx)
			if keyword_set(sea)  then void_index = where(bild2 eq fillvalue2[0] or dem ne 0,complement=idx)
			title= 	keyword_set(cbar_title) ? cbar_title :(strupcase(dat) eq 'PHASE' ? 'CPH':$
				get_product_name(dat,algo=algo2,level=level,/upper,h_types=htypes))+$
			' '+(total(strupcase(dat) eq ['COT','PHASE']) ? '' : unit1)
			if keyword_set(globe) then xyouts,0.13,0.9,figure_title,/norm,charsize=2
			figure_title = keyword_set(notitle) ? '' : f2str+satn2+' '+datum2
		endif else begin
			bild2 = float(bild1)-float(bild2)
			title= 	keyword_set(cbar_title) ? 'Difference '+cbar_title :(keyword_set(htypes)?strupcase(htypes[0]+' '):'')+$
				(strupcase(dat) eq 'PHASE' ? 'CPH':strupcase(dat))+' Difference '+(total(strupcase(dat) eq ['COT','PHASE']) ? '' : unit1)
			figure_title = keyword_set(notitle) ? '' : f1str+datum1+' '+satn1+' - '+f2str+datum2+' '+satn2
			if ~keyword_set(diff_only) then begin
; 				maxi = min(abs(rnd(minmax(bild2[idx]),bin,/down))) < (0.1 * max(bild1[idx]))
				maxi = rnd(min(abs(rnd(minmax(bild2[idx]),bin,/down))) < (0.1 * max(bild1[idx])),bin,/down)
				mini = maxi * (-1.)
			endif
		endelse
		if adv_keyword_set(maxi) then if float(maxi[0]) lt (max(floor(bild2[where(bild2 ne fillvalue2[0])]*1000.))/1000.) then g_eq = 1
		if adv_keyword_set(mini) then if min(bild2[where(bild2 ne fillvalue2[0])]) lt float(mini[0]) then l_eq = 1

		rotate_globe = 	keyword_set(globe) and ~keyword_set(save_as) and ~keyword_set(zoom) and !p.multi[0] le 0 and keyword_set(wtext) and $
				~keyword_set(antarctic) and ~keyword_set(arctic) and keyword_set(diff_only)

		m = obj_new("map_image",bild2,lat,lon,void_index=void_index, $
			box_axes=box_axes,n_lev=n_lev2, max=adv_keyword_set(maxi) ? maxi[0] : max(bild2[idx]),min=adv_keyword_set(mini) ? mini[0]: min(bild2[idx]), $
			countries=countries,usa=countries,magnify = magnify,figure_title=figure_title,title=title, $
; 			charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
			charthick = !m_charthick, charsize  = !m_charsize ,  $
			format=bar_format,  limit = limit, flip_colours = flip_colours, bar_tickname=bar_tickname2,discrete =discrete2 , $
			ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel,g_eq=g_eq, l_eq=l_eq	, $
			noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar,lambert=lambert,horizon=horizon, $
			p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine, panoply = panoply, $
			lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
			mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal, logarithmic=logarithmic,$
			robinson=robinson,stereographic=stereographic,label=label,position = map_position, $
			bwr=bwr,rainbow=rainbow,extended_rainbow=extended_rainbow,elevation=elevation,greyscale=greyscale,$
			brewer=brewer,ctable=ctable,debug=verbose)
		if keyword_set(zoom) then begin
			get_new_corners = 1
			m -> zoom,get_new_corners = get_new_corners,/print_new, ztext = ztext,magnify=magnify
			if win_nr ne -1 then win, win_nr
		endif

	if (2 eq 1) then begin
		;include LWP_Allsky areas of JFM for PVIR
		;SAF: 
		brd = [-20, 0,-10,10]
		plots,[brd[1],brd[1]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); left
		plots,[brd[3],brd[3]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); right
		plots,[brd[1],brd[3]],[brd[2],brd[2]],thick=3,color=cgcolor('black'); top
		plots,[brd[1],brd[3]],[brd[0],brd[0]],thick=3,color=cgcolor('black'); bottom
		;SAM: 
		brd = [-26,-86,-16,-76]
		plots,[brd[1],brd[1]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); left
		plots,[brd[3],brd[3]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); right
		plots,[brd[1],brd[3]],[brd[2],brd[2]],thick=3,color=cgcolor('black'); top
		plots,[brd[1],brd[3]],[brd[0],brd[0]],thick=3,color=cgcolor('black'); bottom
		;NAM:
		brd = [ 20,-130,30,-120]
		plots,[brd[1],brd[1]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); left
		plots,[brd[3],brd[3]],[brd[0],brd[2]],thick=3,color=cgcolor('black'); right
		plots,[brd[1],brd[3]],[brd[2],brd[2]],thick=3,color=cgcolor('black'); top
		plots,[brd[1],brd[3]],[brd[0],brd[0]],thick=3,color=cgcolor('black'); bottom
	endif
		if keyword_set(globe) and (keyword_set(diff_only) or keyword_set(maps_only)) then $
		xyouts,(keyword_set(maps_only) ? 0.63:0.25 ),(keyword_set(maps_only) ? 0.9:0.95 ),figure_title,/norm,charsize=2

		if ~rotate_globe then obj_destroy, m

		if keyword_set(zoom) then begin
			compare_l2, file1, file2, data1=data1, data2=data2, mini=mini, maxi=maxi, bin=bin	, $
					save_as=save_as, win_nr=win_nr, check_quality=check_quality, g_eq=g_eq	, $
					l_eq=l_eq, verbose=verbose, land=land,sea=sea, sat2=sat2		, $
					limit=get_new_corners, show_values=show_values, out = out, sat1=sat1	, $
					algo2=algo2, algo1=algo1, year = year	, htypes = htypes		, $
					month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
					p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
					mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
					robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
					diff_only=diff_only,hist_only=hist_only,level=level,maps_only=maps_only	, $
					ctable = ctable, other = other,zonal_only = zonal_only			, $
					box_only = box_only, coverage = coverage, nobar = nobar, ztext = ztext	, $
					msg=msg, logarithmic=logarithmic,timeseries=timeseries,dim3=dim3	, $
					addtext=addtext,rot=rot,magnify=magnify, stereographic = stereographic	, $
					countries=countries
		endif

		if rotate_globe then begin

			!mouse.button = 1
			bitset = total(dat[0] eq ['QCFLAG']) ? 1 : 0
			value = '                                   '
			bild_dum = size(bild2,/type) eq 1 ? fix(bild2) : bild2
			si = size(bild_dum,/dim)
			while (!mouse.button ne 4) do begin
				cursor, x, y, /change,/device
				cursor, lo, la, /change,/data

				if (total(finite([lo,la])) eq 2) then begin
					qw = where(between(lon,lo-1.,lo+1.) and between(lat,la-1.,la+1.),count)
						if count gt 0 then begin
							idx=qw[where((abs(lon[qw]-lo)+abs(lat[qw] -la)) eq min(abs(lon[qw]-lo)+abs(lat[qw] -la))) ]
							dum_string = '['+strjoin(string([lo,la],f='(f6.1)'),',')+'] '
							werte      = bitset ? '['+strjoin(strcompress( where( (byte(bild_dum[idx[0]]) and 2^(indgen(9))) ne 0),/rem ),',')+']' : $
							strcompress(bild_dum[idx[0]],/rem)
							widget_control,wtext,set_value='[lon,lat] '+dum_string+' '+werte
						endif else widget_control,wtext,set_value=value
				endif
				if !mouse.button eq 1 then begin
					inf = (total(finite([lo,la])) eq 0.)
					x_r = x/float(!d.x_vsize)
					y_r = y/float(!d.y_vsize)
					do_it = 1
					if between(x_r,.00,.25) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon -30) mod 360)	else $
					if between(x_r,.75,1.0) and between(y_r,.25,.75) and inf then p0lon = (round(p0lon +30) mod 360 )	else $
					if between(x_r,.25,.75) and between(y_r,.75,1.0) and inf then p0lat = ( 90 < round(p0lat +30) > (-90) )	else $
					if between(x_r,.25,.75) and between(y_r,.00,.25) and inf then p0lat = ( 90 < round(p0lat -30) > (-90) )	else do_it = 0

					limit = p0lat ge 0 ? 	[0.,p0lon-90.,90.-p0lat,p0lon+180.,0.,p0lon+90.,p0lat-90.,p0lon] : $
								[0.,p0lon-90.,p0lat+90.,p0lon,0.,p0lon+90.,-90.-p0lat,p0lon+180.]
					if do_it then m -> project,limit=limit,p0lon=p0lon,p0lat=p0lat, void_index=void_index
					xyouts,(keyword_set(maps_only) ? 0.63:0.25 ),(keyword_set(maps_only) ? 0.9:0.95 ),figure_title,/norm,charsize=2
				endif
			endwhile
			obj_destroy, m
			widget_control,wtext,set_value='             Pixel Values'
		endif
	endif

	end_save, save_as

	if keyword_set(verbose) then begin
		print,mem_cur/1024.^3
		caldat, systime(/utc, /julian), mo, da, ye, ho, mi, se
		dat_str	= string(da, mo, ye, ho, mi, format = '(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2," [UTC] / ")')
		print, dat_str + 'Compare_L2 -> '+string(systime(1)-starttime,f='("Duration        : ", f7.3, " Sec")')
		print, dat_str + 'Compare_L2 -> '+string(float(memory(/highwater)-mem_cur)/1024.^3,f='("Memory required : ", f7.3, " GiB")')
 	endif

	if keyword_set(show_values) then show_pixel_value,bild,lon,lat,data=data,unit=unit1

end
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
pro compare_l2_save_serie,file1,file2,data1=data1,data2=data2,mini=mini,maxi=maxi,save_as=save_as,win_nr=win_nr	, $
		land=land,sea=sea,limit=limit,zoom=zoom,lon=lon,lat=lat,bild=bild,unit=unit,sat1=sat1,sat2=sat2	, $
		algo2=algo2,algo1=algo1,verbose=verbose,year=year,month=month,day=day,orbit=orbit,datum1=datum1	, $
		datum2=datum2,globe=globe,p0lon=p0lon,p0lat=p0lat,antarctic=antarctic,arctic=arctic,level=level	, $
		mollweide=mollweide,hammer=hammer,goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,ctable=ctable	, $
		other=other,robinson=robinson,nobar=nobar, stereographic = stereographic, dim3=dim3		, $
		all_parameter = all_parameter, logarithmic=logarithmic,coverage=coverage,countries=countries,notitle=notitle,$
		no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box,version=version

	jetzt      = strjoin((unix2ymdhms(systime(1),/arr))[0:2],'_')
	mtime1     = strjoin(unix2ymdhms((file_info(file1)).mtime,/arr))
	mtime2     = strjoin(unix2ymdhms((file_info(file2)).mtime,/arr))
	satn1      = sat_name(algo1,sat1,version=version)
	satn2      = sat_name(algo2,sat2,version=version)
	datum      = keyword_set(datum1) ? datum1 : (keyword_set(datum2) ? datum2 : 'YYYYMM')
	lev        = keyword_set(level) ? datum+'_'+strupcase(level)+'_' : datum+'_'
	dir        = '/cmsaf/cmsaf-cld1/sstapelb/bilder/feedbackloop/loop2/'
	dumdat     = strlowcase(data1[0])
	if dumdat eq 'cc_total' then dumdat = 'cfc'

	; falls noch mit alten histogrammen gearbeitet wird (Fame-C,AATSR) dann mit 
	; ncatted -O -h -a product_version,global,d,, -a product_version,global,c,c,"1.0" file.nc
	; product_version auf 1.0 setzen , dann wird cci_old angewendet und es sollte funktionieren!
	; Achtung erst das alte ändern dann das neue!
	dumdat = keyword_set(all_parameter) ? ['cph','cwp','lwp','iwp','cth','cot','ref','cfc','ctt','ctp','hist2d_cot_ctp'] : dumdat 
	if sat1 eq 'aatme' and sat2 eq 'aatme' then dumdat = [dumdat,'cth2']

	for ii = 0,n_elements(dumdat)-1 do begin
		dat = dumdat[ii]

		print, string(float(memory(/current))/1024.^2,f='("Current Memory : ", f7.2, " MB")')

		case dat of
			'cwp'	: begin & mini =   0. & maxi =   1000. & end
			'lwp'	: begin & mini =   0. & maxi =   1000. & end
			'iwp'	: begin & mini =   0. & maxi =   1000. & end
			'cth'	: begin & mini =   0. & maxi =     10. & end
			'cth2'	: begin & mini =   0. & maxi =     10. & end
			'cot'	: begin & mini =   0. & maxi =     50. & end
			'ref'	: begin & mini =   0. & maxi =     60. & end
			'cfc'	: begin & mini =   0. & maxi =      1. & end
			'cph'	: begin & mini =   0. & maxi =      1. & end
			'ctt'	: begin & mini = 200. & maxi =    300. & end
			'ctp'	: begin & mini = 200. & maxi =    900. & end
			else	:
		endcase

		if is_jch(dat) then begin
			sat3  = sat1 eq 'aatme' or sat1 eq 'aatsr' ? 'noaa17' : sat1
			algo3 = stregex(sat3,'noaa',/fold,/bool) ? 'clara':'coll5'  
			satn3 = sat_name(algo3,sat3,version=version)
			file3 = get_filename(strmid(datum,0,4),strmid(datum,4,2),strmid(datum,6,2),algo=algo3,data=dat,level=level,sat=sat3)
			f1str  = 'File1: '
			f2str  = 'File2: '
			if satn1 eq satn2 and satn1 ne '' then begin
				; wann wurde das letzte mal in datei geschrieben?
				if mtime1 gt mtime2 then begin
					f1str  = 'File1 (new): '
					f2str  = 'File2 (old): '
				endif else if mtime2 gt mtime1 then begin
					f1str  = 'File1 (old): '
					f2str  = 'File2 (new): '
				endif
			endif
			; 6) hist2d max 
			!p.multi=0
			; file1
			save_file1 = dir+satn1+'_'+lev+strupcase(dat)+'_max_type_fig1.pdf'
			plot_l2,year[0],month[0],day[0],file=file1,data=dat,mini=0,maxi=8,sat=sat1		, $
			algo=algo1,hist_cloud_type='max',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon						, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			ctable = ctable, other = oth, verbose = verbose,level=level, prefix=f1str		, $
			save_as = save_file1,countries=countries
			; file2
			save_file2 = dir+satn2+'_'+lev+strupcase(dat)+'_max_type_fig2.pdf'
			plot_l2,year[0],month[0],day[0],file=file2,data=dat,mini=0,maxi=8,sat=sat2		, $
			algo=algo2,hist_cloud_type='max',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon						, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			ctable = ctable, other = oth, verbose = verbose,level=level, prefix=f2str			, $
			save_as = save_file2,countries=countries
			; file3
			save_file3 = dir+satn3+'_'+lev+strupcase(dat)+'_max_type_fig3.pdf'
			plot_l2,year[0],month[0],day[0],file=file3,data=dat,mini=0,maxi=8,sat=sat3		, $
			algo=algo3,hist_cloud_type='max',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon						, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			ctable = ctable, other = oth, verbose = verbose,level=level				, $
			save_as = save_file3,countries=countries
			; 6) hist2d - hist2d
			; file1
			save_file4 = dir+satn1+'_'+lev+strupcase(dat)+'_hist2d_fig1.pdf'
			plot_l2,year[0],month[0],day[0],file=file1,data=dat,mini=0,maxi=8,sat=sat1		, $
			algo=algo1,hist_cloud_type='hist2d',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon, other = oth				, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			verbose = verbose,level=level,prefix=f1str,save_as=save_file4,countries=countries
			; file2
			save_file5 = dir+satn2+'_'+lev+strupcase(dat)+'_hist2d_fig2.pdf'
			plot_l2,year[0],month[0],day[0],file=file2,data=dat,mini=0,maxi=8,sat=sat2		, $
			algo=algo2,hist_cloud_type='hist2d',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon, other = oth				, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			verbose = verbose,level=level,prefix=f2str,save_as=save_file5,countries=countries
			; file3
			save_file6 = dir+satn3+'_'+lev+strupcase(dat)+'_hist2d_fig3.pdf'
			plot_l2,year[0],month[0],day[0],file=file3,data=dat,mini=0,maxi=8,sat=sat3		, $
			algo=algo3,hist_cloud_type='hist2d',win_nr=win_nr,sea = sea,land=land			, $
			limit=limit,zoom=zoom,globe=globe,p0lon=p0lon, other = oth,countries=countries		, $
			p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer		, $
			goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
			verbose = verbose,level=level,prefix='File3 (ref): ',save_as=save_file6
			; 7) hist2d - 1d
			save_file7 = dir+satn1+'_'+lev+strupcase(dat)+'_1d_fig1.pdf'
			plot_1d_from_jch_4all,file1,file2,year=year[0],month=month[0],sat=sat1			, $
			prefix=[f1str,f2str],land=land,sea=sea,limit=limit,save_as=save_file7			, $
			algo2=algo2,algo1=algo1,mini=0,maxi=50
			; 7) hist2d - 1d ice
			save_file8 = dir+satn1+'_'+lev+strupcase(dat)+'_ICE_1d_fig1.pdf'
			plot_1d_from_jch_4all,file1,file2,year=year[0],month=month[0],sat=sat1			, $
			prefix=[f1str,f2str],land=land,sea=sea,limit=limit,save_as=save_file8,/ice		, $
			algo2=algo2,algo1=algo1,mini=0,maxi=50
			; 7) hist2d - 1d liq
			save_file9 = dir+satn1+'_'+lev+strupcase(dat)+'_LIQUID_1d_fig1.pdf'
			plot_1d_from_jch_4all,file1,file2,year=year[0],month=month[0],sat=sat1			, $
			prefix=[f1str,f2str],land=land,sea=sea,limit=limit,save_as=save_file9,/liquid		, $
			algo2=algo2,algo1=algo1,mini=0,maxi=50
		endif else begin
			; 1) maps only
			save_file1 = dir+satn1+'_'+lev+strupcase(dat)+'_fig4.pdf'
			compare_l2, file1, file2, data1=dat, data2=data2,mini=mini,maxi=maxi, bin=bin	, $
				win_nr=win_nr, check_quality=check_quality, g_eq=g_eq, ctable = ctable	, $
				l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom	, $
				limit=limit, show_values=show_values, sat1=sat1, sat2=sat2, algo2=algo2	, $
				algo1=algo1, year = year, dim3=dim3,countries=countries	, $
				month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
				p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
				robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
				level=level, other = other, /maps_only, save_as = save_file1

			; 2) Zonal
			!p.multi=0
			save_file2 = dir+satn1+'_'+lev+strupcase(dat)+'_fig3.pdf'
			compare_l2, file1, file2, data1=dat, data2=data2,mini=mini,maxi=maxi, bin=bin	, $
				win_nr=win_nr, check_quality=check_quality, g_eq=g_eq, ctable = ctable	, $
				l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom	, $
				limit=limit, show_values=show_values, sat1=sat1, sat2=sat2, algo2=algo2	, $
				algo1=algo1, year = year, dim3=dim3,countries=countries	, $
				month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
				p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
				robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
				level=level, other = other, /zonal_only, save_as = save_file2
			
			; 3) Boxplots
			save_file3 = dir+satn1+'_'+lev+strupcase(dat)+'_fig1.pdf'
			compare_l2, file1, file2, data1=dat, data2=data2,mini=mini,maxi=maxi, bin=bin	, $
				win_nr=win_nr, check_quality=check_quality, g_eq=g_eq, ctable = ctable	, $
				l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom	, $
				limit=limit, show_values=show_values, sat1=sat1, sat2=sat2, algo2=algo2	, $
				algo1=algo1, year = year,countries=countries	, $
				month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
				p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
				robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
				level=level, other = other, /box_only, save_as = save_file3

			; 4) Histos
			save_file4 = dir+satn1+'_'+lev+strupcase(dat)+'_fig2.pdf'
			compare_l2, file1, file2, data1=dat, data2=data2,mini=mini,maxi=maxi, bin=bin	, $
				win_nr=win_nr, check_quality=check_quality, g_eq=g_eq, ctable = ctable	, $
				l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom	, $
				limit=limit, show_values=show_values, sat1=sat1, sat2=sat2, algo2=algo2	, $
				algo1=algo1, year = year, dim3=dim3,countries=countries	, $
				month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
				p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
				robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
				level=level, other = other, /hist_only, save_as = save_file4

			; 5) Overview zum Schluss MIni und MAxi wird hier verändert
			save_file5 = dir+satn1+'_'+lev+strupcase(dat)+'_fig5.pdf'
			compare_l2, file1, file2, data1=dat, data2=data2,mini=mini,maxi=maxi, bin=bin	, $
				win_nr=win_nr, check_quality=check_quality, g_eq=g_eq, ctable = ctable	, $
				l_eq=l_eq, verbose=verbose, land=land,sea=sea, zoom=zoom	, $
				limit=limit, show_values=show_values, sat1=sat1, sat2=sat2, algo2=algo2	, $
				algo1=algo1, year = year, dim3=dim3,countries=countries	, $
				month = month, day = day, orbit = orbit, datum1 = datum1, datum2=datum2	, $
				p0lon = p0lon, p0lat = p0lat, antarctic = antarctic, arctic = arctic	, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal 	, $
				robinson=robinson, hammer = hammer, goode = goode, globe = globe	, $
				level=level,other = 'bwr', save_as = save_file5
		endelse
	endfor
end
;------------------------------------------------------------------------------------------
pro polyfill_ts_error,ts_data,ts_unce,error=error,color=color, fill = fill, bars = bars
	idx     = where(finite(ts_data),nc_cnt)
	idx_unc = where(finite(ts_unce),nc_unc_cnt)
	col     = keyword_set(color) ? color : cgColor("Sky Blue")
; 	colf    = keyword_set(fill) and keyword_set(bars) ? cgColor("Sky Blue") : col
	colf    = keyword_set(fill) and keyword_set(bars) ? cgColor("GRN3") : col

	if ~keyword_set(fill) and ~keyword_set(bars) then bars = 1

	if keyword_set(error) and nc_unc_cnt ne 0 then begin
		if keyword_set(fill) then begin
			split = where((idx[1:*]-idx[0:*]) ne 1,count)
			if count ge 1 then begin
				polyfill,[idx[0:split[0]],reverse(idx[0:split[0]])],[reform(ts_data[idx[0:split[0]]]+ts_unce[idx[0:split[0]]]), $
				reverse(reform(ts_data[idx[0:split[0]]]-ts_unce[idx[0:split[0]]]))],col=colf
				if count gt 1 then begin
					for i=1,count-1 do begin
						polyfill,[idx[(split[i-1]+1):split[i]],reverse(idx[(split[i-1]+1):split[i]])],$
						[reform(ts_data[idx[(split[i-1]+1):split[i]]]+ts_unce[idx[(split[i-1]+1):split[i]]]), $
						reverse(reform(ts_data[idx[(split[i-1]+1):split[i]]]-ts_unce[idx[(split[i-1]+1):split[i]]]))],col=colf
					endfor
				endif
				polyfill,[idx[split[count-1]+1:*],reverse(idx[split[count-1]+1:*])],$
				[reform(ts_data[idx[split[count-1]+1:*]]+ts_unce[idx[split[count-1]+1:*]]),$
				reverse(reform(ts_data[idx[split[count-1]+1:*]]-ts_unce[idx[split[count-1]+1:*]]))],col=colf
			endif else begin
				polyfill,[idx,reverse(idx)],[reform(ts_data[idx]+ts_unce[idx]),reverse(reform(ts_data[idx]-ts_unce[idx]))],col=colf
			endelse
		endif
		if keyword_set(bars) then begin
			ERRPLOT, ts_data - ts_unce, ts_data + ts_unce, color=col
		endif
	endif
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro gac_ts_plots,struc,ts_data,dat,algon1,yrange,lines,anz,xtickname,qu,ref,anomalies=anomalies	, $
		 log=log,save_as=save_as,error=error,show_trend = show_trend			, $
		 no_compare=no_compare,opl=opl,coverage=coverage						, $
		 longname=longname,hct=hct,white_bg=white_bg,standard=standard,datum=datum	, $
		 trend=trend,symsize=symsize,notitle=notitle,tr_corr=tr_corr, stddev = stddev	, $
		 uncertainty = uncertainty,satnames = satnames, sum=sum, dtn=dtn		, $
		 satn_background = satn_background,smoothTS = smoothTS,bias=bias,rmse=rmse,bc_rmse=bc_rmse,corr=corr

	;specials
	pinatubo  = 0
	mst_paper = 0
	pvir      = 0

	sav     = keyword_set(save_as)
	wbg     = keyword_set(white_bg)

	if mst_paper then begin
		pvir   = 0
		case algon1 of
			'Cloud_cci AVHRR-AM'    : color = 'Dodger Blue'
			'Cloud_cci AVHRR-PM'    : color = 'Navy'
			'Cloud_cci MODIS-Terra' : color = 'Dark Green'
			'Cloud_cci MODIS-Aqua'  : color = 'Lime Green'
			'Cloud_cci MERIS+AATSR' : color = 'Dark Violet'
			'Cloud_cci ATSR2-AATSR' : color = 'Dark Orange'
			else	:
		endcase
	endif
	
	if keyword_set(bias) or keyword_set(rmse) or keyword_set(corr) or keyword_set(bc_rmse) then no_compare = 1
	
	syms    = adv_keyword_set(symsize) ? symsize : 1.5
	psym    = syms eq 0 ? 0 : -8 ; 8 :=circles, changes here will use cgsymcat() 
	datum   = keyword_set(datum) ? datum : ''
	; style of uncertainty tube
	fill = 1
	bars = 1
	; ---
	d       = struc
	tsi     = d.TS_INDICES
	unit    = d.unit
	dec1 = ''
	dec2 = ''
	dec3 = ''

	sn_cov = short_cov_name(coverage)
	if keyword_set(sn_cov) and keyword_set(no_compare) and ~keyword_set(smoothTS) then dtn += ' ('+ sn_cov+')'

	if sav then begin
		charthick = !p_charthick ;1.5
		xcharsize = !p_xcharsize ;1.7 
		ycharsize = !p_ycharsize ;1.7
		lcharsize = !l_charsize  ;2.5
		xmargin   = [14,6]
		ymargin   = [ 7,3]
	endif else if wbg then begin
		charthick = !p_charthick ;3.0
		xcharsize = !p_xcharsize ;2.5
		ycharsize = !p_ycharsize ;2.5
		lcharsize = !l_charsize  ;3.0
		xmargin   = [20,6]
		ymargin   =  [8,3]
	endif else begin
		charthick = !p_charthick ;1.2
		xcharsize = !p_xcharsize ;1.2 
		ycharsize = !p_ycharsize ;1.2
		lcharsize = !l_charsize  ;1.5
		xmargin   =[10,3]
		ymargin   = [5,2]
	endelse

	if sav then begin
		!p.multi = 0
		save_dum = file_dirname(save_as)+'/'+file_basename(save_as,'.eps')
		save_as1 = save_dum + '_time_series.eps'
	endif

	cc = 5
	title = longname + 	(keyword_set(anomalies) ? ' (Anomalies)' : '')+ $
						(keyword_set(uncertainty) ? ' Unc.' : '') + $
						(keyword_set(stddev) ? ' Stdd.' : '')
	if mst_paper then title = dat

	if ~keyword_set(no_compare) then begin
		start_save, save_as1, thick = thick, size = [42,22]
			if wbg then thick=4
; 			idx = where(finite(ts_data[tsi.gm1,*]) and finite(ts_data[tsi.gm2,*]),idx_cnt)
			nc1 = tsi.gm1 & nc_unc1 = tsi.unc1
			nc2 = tsi.gm2 & nc_unc2 = tsi.unc2
			if keyword_set(uncertainty) then begin
				nc1 = tsi.unc1
				nc2 = tsi.unc2
				error = 0
			endif
			if keyword_set(stddev) then begin
				nc1 = tsi.gm1_std 
				nc2 = tsi.gm2_std 
				error = 0
			endif
			idx = where(finite(ts_data[nc1,*]),idx_cnt)
			if idx_cnt eq 0 then begin
				idx = where(finite(ts_data[nc2,*]),idx_cnt)
				if idx_cnt eq 0 then return
			endif
			if opl eq 0 then begin
				xtitle='Time [years]'
				anz=anz-0.5
				ymargin= [6,2]+(sav or wbg ? [2,1]:0)
				; for PVIR
				if pvir then ymargin += [18,0]
				plot,[0,0],[1,1],xr=[anz[0],anz[1]],/xs,xticks=n_elements(xtickname)-1,xtickname=xtickname,yr=yrange,ys=(qu eq 0 ? 1:9),$
				xticklen=0.01,ytitle=title+' '+unit,xminor=xminor, ylog = log,xtitle=xtitle, $
				xmargin=[12,10]+(sav or wbg ? [(wbg ? 10:4),(qu eq 0 ? 0:6)]:0),ymargin=ymargin,$
				charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize, title = keyword_set(notitle) ? '' : datum
				pf_ycr = keyword_set(log) ? 10.^(!y.crange) : (!y.crange)
				; polyfills
				if keyword_set(satn_background) then begin
					eq_sn = ree(satnames)
					for sn = 0,n_elements(eq_sn)-1 do begin
						dumname = satnames[eq_sn[sn]]
						if dumname eq '' then continue
						dumidx = where(satnames eq dumname,dumidxcnt)
						if dumidxcnt gt 1 then begin
							print,'Glob. Mean1    '+dumname+'  : '+string(mean(ts_data[nc1,dumidx]),f='(f14.4)')
							print,'Glob. Mean2    '+dumname+'  : '+string(mean(ts_data[nc2,dumidx]),f='(f14.4)')
							col = (sn mod 2) eq 0 ? cgcolor('lavender') : !p.background
							polyfill,[min(dumidx),max(dumidx),max(dumidx),min(dumidx)],$
							[pf_ycr[0],pf_ycr[0],pf_ycr[1],pf_ycr[1]],col=col
						endif
					endfor
				endif
				polyfill_ts_error,ts_data[nc1,*],ts_data[nc_unc1,*],error=error,color=cgColor("Gray"),fill=fill,bars=bars
				polyfill_ts_error,ts_data[nc2,*],ts_data[nc_unc2,*],error=error,color=cgcolor('Red4'),fill=fill,bars=bars
				if keyword_set(pinatubo) then polyfill,[160.,162.,162.,160.],[pf_ycr[0],pf_ycr,pf_ycr[1]],col=cgcolor('tomato')
				div = ((anz[1]-anz[0]) le 12 ? 3. : 12.)
				for i = 0, (anz[1]-anz[0])/div do oplot,anz[0]+[i*div,i*div],pf_ycr,linestyle=2
				for i = 0, n_elements(lines)-1 do oplot,!x.crange,[lines[i],lines[i]],linestyle=1
				if keyword_set(satn_background) then begin
					eq_sn = ree(satnames)
					for sn = 0,n_elements(eq_sn)-1 do begin
						dumname = satnames[eq_sn[sn]]
						if dumname eq '' then continue
						dumidx = where(satnames eq dumname,dumidxcnt)
						if dumidxcnt gt 1 then begin
							dum_xx = (max(dumidx)-min(dumidx))/2 + min(dumidx)
							if between(dum_xx,anz[0],anz[1]) then begin
								xyouts, dum_xx,(keyword_set(log) ? $
								10^(((!y.crange)[1]-(!y.crange)[0])*0.02 + (!y.crange)[0]) : $
								((pf_ycr[1]-pf_ycr[0])*0.02) + pf_ycr[0]), $
								dumname, charthick = charthick, charsize = xcharsize,align=0.5
							endif
						endif
					endfor
					;polyfill plots over max yrange and yticks, set axes again
					axis,xaxis=1,xs=1,xr=[anz[0],anz[1]], charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize,$
					xtickformat="(A1)",xticks=(anz[1]-anz[0])/12,xticklen=0.00001
					if qu eq 0 then axis,yaxis=1,yr=yrange,ys=1,ylog=log, charthick = charthick, xcharsize = xcharsize, $
					ycharsize = ycharsize, ytickformat="(A1)"
					axis,yaxis=0,yr=yrange,ys=1,ylog=log, charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize
				endif
				if qu ne 0 then axis,yaxis=1,ystyle=1,yrange=[0,abs([yrange[1]-yrange[0]])],col = cgColor("Slate Gray"),ytitle='BC-RMSD'+' '+unit, $
				charthick = charthick, xcharsize = xcharsize, ycharsize= ycharsize
				if keyword_set(coverage) then begin
					legend,'Coverage: '+strupcase(coverage),color=-1,spos='top',charsize=lcharsize, charthick=charthick, numsym=1
				endif
				if keyword_set(smoothTS) then begin
					oplot,ts_data[nc1,*],thick=2,col=cgColor(!compare_col1)
					sm_data = smooth(reform(ts_data[nc1,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc1,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgColor(!compare_col1)
					oplot,ts_data[nc2,*],thick=2,col=cgColor(!compare_col2) 
					sm_data = smooth(reform(ts_data[nc2,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc2,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgColor(!compare_col2) 
					if keyword_set(coverage) then begin
						legend,'Coverage: '+strupcase(coverage),color=cgColor(!compare_col2) ,spos='top',charsize=lcharsize,charthick=charthick, numsym=1
					endif
				endif else begin
					oplot,ts_data[nc1,*],psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgColor(!compare_col1)
					oplot,ts_data[nc2,*],psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgColor(!compare_col2) 
				endelse
				legend,algon1+dtn[0],psym=cgsymcat(psym),thick=thick,color=[cgColor(!compare_col1)] ,spos='tl',$
				charsize=lcharsize,charthick=charthick,ystretch=1.5
				legend,ref+dtn[0],psym=cgsymcat(psym),thick=thick,color=cgColor(!compare_col2) ,spos='tr',charsize=lcharsize,charthick=charthick,ystretch=1.5
			endif

			if qu ne 0 then begin
				if keyword_set(smoothTS) then begin
					if yrange[0] gt yrange[1] then begin
						oplot,yrange[0] - ts_data[tsi.bcr,*],col=cgColor("Slate Gray"), thick=2 
						sm_data = smooth(reform(yrange[0] - ts_data[tsi.bcr,*]),8,/nan,/edge_truncate)
						sm_idx  = where(~finite(yrange[0] - ts_data[tsi.bcr,*]),sm_cnt)
					endif else begin
						oplot,ts_data[tsi.bcr,*]+yrange[0],col=cgColor("Slate Gray"), thick=2
						sm_data = smooth(reform(ts_data[tsi.bcr,*]+yrange[0]),8,/nan,/edge_truncate)
						sm_idx  = where(~finite(ts_data[tsi.bcr,*]+yrange[0]),sm_cnt)
					endelse
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgColor("Slate Gray")
				endif else begin
					if yrange[0] gt yrange[1] then $
					oplot,yrange[0] - ts_data[tsi.bcr,*],psym=cgsymcat(psym),col=cgColor("Slate Gray"), $
					thick=thick,symsize=syms else $
					oplot,ts_data[tsi.bcr,*]+yrange[0],psym=cgsymcat(psym),col=cgColor("Slate Gray"), $
					thick=thick,symsize=syms
				endelse
			endif
			if keyword_set(show_trend) then begin
				define_oplots, opl, cols, spos, linestyle, psymm, ystretch,/timeseries
				res1=linfit(idx,ts_data[nc1,idx],yfit=yfit1)
				res2=linfit(idx,ts_data[nc2,idx],yfit=yfit2)
				res3=linfit(idx,ts_data[tsi.bcr,idx],yfit=yfit3)
				dec1 = ' '+string(((yfit1[idx_cnt-1]-yfit1[0])/float(idx_cnt)*120.),f='(f10.5)')+unit+' / decade'
				dec2 = ' '+string(((yfit2[idx_cnt-1]-yfit2[0])/float(idx_cnt)*120.),f='(f10.5)')+unit+' / decade'
				dec3 = ' '+string(((yfit3[idx_cnt-1]-yfit3[0])/float(idx_cnt)*120.),f='(f10.5)')+unit+' / decade'
				oplot,idx,yfit1,col=opl eq 0 ? cgColor(!compare_col1) : cgcolor(cols)
				oplot,idx,yfit2,col=cgColor(!compare_col2) 
				if qu ne 0 then begin
				if yrange[0] gt yrange[1] then $
					oplot,idx,yrange[0] - yfit3,col=cgColor("Slate Gray") else $
					oplot,idx,yfit3+yrange[0],col=cgColor("Slate Gray")
				endif
				str_pholder = strjoin(replicate(' ',max([strlen(algon1),strlen(ref)])))
				print,'Trend (ALG1)  '+str_pholder+' : '+dec1
				print,'Trend (ALG2)  '+str_pholder+' : '+dec2
				if ~keyword_set(error) then print,'RMSD          '+str_pholder+' : '+dec3
			endif
			if opl ne 0 then begin
				define_oplots, opl, cols, spos, linestyle, psymm, ystretch,/timeseries
				polyfill_ts_error,ts_data[nc1,*],ts_data[nc_unc1,*],error=error,color=cgColor("Gray"),fill=fill,bars=bars
				polyfill_ts_error,ts_data[nc2,*],ts_data[nc_unc2,*],error=error,color=cgcolor('blue'),fill=fill,bars=bars
				if keyword_set(smoothTS) then begin
					oplot,ts_data[nc1,*],thick=2,col=cgcolor(cols)
					sm_data = smooth(reform(ts_data[nc1,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc1,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgcolor(cols)
					oplot,ts_data[nc2,*],thick=2
					sm_data = smooth(reform(ts_data[nc2,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc2,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms
				endif else begin
					oplot,ts_data[nc1,*],psym=cgsymcat(psym),thick=thick,symsize=syms;,col=cgcolor(cols)
					oplot,ts_data[nc2,*],psym=cgsymcat(psym),thick=thick,symsize=syms,col=cgcolor(cols)
				endelse
; 				legend,algon1+dtn[0],psym=cgsymcat(psym),thick=thick,color=[cgColor(cols)],spos='tl',charsize=lcharsize,charthick=!p_charthick, $
; 				ystretch=((opl+1)*1.1)+0.5,linestyle = linestyle
				legend,ref+dtn[0],psym=cgsymcat(psym),thick=thick,color=[cgColor(cols)],spos='tr',charsize=lcharsize,charthick=charthick,$
				ystretch=((opl+1)*1.1)+0.5,linestyle = linestyle
			endif
		end_save,save_as1
	endif

	if keyword_set(no_compare) then begin
		apx    = keyword_set(uncertainty) ? ' UNC' : ''
		nc     = keyword_set(uncertainty) ? tsi.unc1 : tsi.gm1
		spos   = 'top'
		if keyword_set(sum) then begin
			nc = tsi.sum1
			if max(ts_data[nc,*],/nan) gt 1000000l then begin
				ts_data[nc,*] /= 1000000. ; in millions
				unit = textoidl('* 10^6')
			endif
		endif
		nc_unc = tsi.unc1 ;tsi.gm1_std

		if keyword_set(bias)    then begin & nc = tsi.bias    & error = 0 & apx = ' Bias'        & combine_name = 1 & end
		if keyword_set(rmse)    then begin & nc = tsi.rmse    & error = 0 & apx = ' RMSE'        & combine_name = 1 & end
		if keyword_set(bc_rmse) then begin & nc = tsi.bcr     & error = 0 & apx = ' BC-Rmse'     & combine_name = 1 & end
		if keyword_set(corr)    then begin & nc = tsi.corr    & error = 0 & apx = ' Correlation' & combine_name = 1 & end

		if keyword_set(combine_name) then begin
			algon1 = algon1 +', '+ ref
			spos='tl'
		endif
		idx = where(finite(ts_data[nc,*]),idx_cnt)
		if idx_cnt eq 0 then return
		start_save, save_as1, thick = thick, size = [42,22]
			if wbg then thick=4
			if opl eq 0 then begin
				anz=anz-0.5
				xtitle='Time [years]'
; 				if keyword_set(notitle) then begin
; 					xtickname = [' ',' ']
; 					ymargin   = [2,2]+(sav or wbg ? [2,1]:0)
; 					xtitle    = ''
; 				endif else $
				ymargin= [6,2]+(sav or wbg ? [2,1]:0)
				; for PVIR
				if pvir then ymargin += [18,0]
				; for MST Paper
				if mst_paper then ymargin += [40,0]
				plot,[0,0],[1,1],xr=[anz[0],anz[1]],/xs,xticks=n_elements(xtickname)-1,xtickname=xtickname,yr=yrange,ys=1,xticklen=0.01,$
				ytitle= title+' '+strcompress(unit,/rem),xminor=xminor,ylog=log,xtitle=xtitle, $
				xmargin=[12,10]+(sav or wbg ? [(wbg ? 10:4),0]:0),ymargin=ymargin,$
				charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize,$
				title = keyword_set(notitle) ? '' : datum
				pf_ycr = keyword_set(log) ? 10.^(!y.crange) : (!y.crange)
				; polyfills
				if keyword_set(satn_background) then begin
					eq_sn = ree(satnames)
					for sn = 0,n_elements(eq_sn)-1 do begin
						dumname = satnames[eq_sn[sn]]
						if dumname eq '' then continue
						dumidx = where(satnames eq dumname,dumidxcnt)
						if dumidxcnt gt 1 then begin
							print,'Glob. Mean    '+dumname+'  : '+string(mean(ts_data[nc,dumidx]),f='(f14.4)')
							col = (sn mod 2) eq 0 ? cgcolor('lavender') : !p.background
							polyfill,[min(dumidx),max(dumidx),max(dumidx),min(dumidx)],$
							[pf_ycr[0],pf_ycr[0],pf_ycr[1],pf_ycr[1]],col=col
						endif
					endfor
				endif
				polyfill_ts_error,ts_data[nc,*],ts_data[nc_unc,*],error=error,color=cgColor("Gray"),fill=fill,bars=bars
				if keyword_set(pinatubo) then polyfill,[160.,162.,162.,160.],[pf_ycr[0],pf_ycr,pf_ycr[1]],col=cgcolor('tomato')
				;---
				div = ((anz[1]-anz[0]) le 12 ? 3. : 12.)
				for i = 0, (anz[1]-anz[0])/div do oplot,anz[0]+[i*div,i*div],pf_ycr,linestyle=2
				for i = 0, n_elements(lines)-1 do oplot,!x.crange,[lines[i],lines[i]],linestyle=1

				if keyword_set(satn_background) then begin
					eq_sn = ree(satnames)
					if (mst_paper eq 0) or (mst_paper eq 1 and dat eq 'IWP') then begin
						for sn = 0,n_elements(eq_sn)-1 do begin
							dumname = satnames[eq_sn[sn]]
							if dumname eq '' then continue
							dumidx = where(satnames eq dumname,dumidxcnt)
							if dumidxcnt gt 1 then begin
								dum_xx = (max(dumidx)-min(dumidx))/2 + min(dumidx)
								if between(dum_xx,anz[0],anz[1]) then begin
								xyouts, dum_xx,(keyword_set(log) ? $
									10^(((!y.crange)[1]-(!y.crange)[0])*0.02 + (!y.crange)[0]) : $
									((pf_ycr[1]-pf_ycr[0])*0.02) + pf_ycr[0]), $
									dumname, charthick = charthick, charsize = xcharsize,align=0.5
								endif
							endif
						endfor
					endif
					;polyfill plots over max yrange and yticks, set axes again 
					axis,xaxis=1,xs=1,xr=[anz[0],anz[1]], charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize,$
					xtickformat="(A1)",xticks=(anz[1]-anz[0])/12,xticklen=0.00001
					axis,yaxis=1,yr=yrange,ys=1,ylog=log, charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize,$
					ytickformat="(A1)"
					axis,yaxis=0,yr=yrange,ys=1,ylog=log, charthick = charthick, xcharsize = xcharsize, ycharsize = ycharsize
; 					if keyword_set(coverage) then begin
; 						legend,'Coverage: '+strupcase(coverage),color=-1,spos='top',charsize=lcharsize,charthick=!p_charthick, numsym=1
; 					endif
				endif

				; set all values out of plotting range to NAN to avoid outside the box dots
				dumidx = where(~between(ts_data[nc,*],pf_ycr[0],pf_ycr[1]) and finite(ts_data[nc,*]),dumidxcnt)
				if dumidxcnt gt 0 then begin
					print, 'Found '+strcompress(dumidxcnt,/rem)+' Value(s) out of plotting range -> Set to NAN.'
					if dumidxcnt lt 10 then begin
						print,'Index ',dumidx
						print,'Value ',reform(ts_data[nc,dumidx])
					endif
					ts_data[nc,dumidx] = !values.f_nan
				endif
				if keyword_set(smoothTS) then begin
					oplot,ts_data[nc,*],thick=2
					sm_data = smooth(reform(ts_data[nc,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,symsize=syms
					if keyword_set(coverage) and ~mst_paper then begin
						legend,'Coverage: '+strupcase(coverage),color=-1,spos='top',charsize=lcharsize,charthick=charthick, numsym=1
					endif
				endif else oplot,ts_data[nc,*],psym=cgsymcat(psym),thick=thick,symsize=syms
; 				if ~keyword_set(show_trend) and ~keyword_set(smoothTS) then $
				if ~keyword_set(smoothTS) then $
				legend,algon1+dtn[0]+hct+apx,psym=cgsymcat(psym),thick=thick,color=-1,spos=spos,charsize=lcharsize,charthick=charthick
			endif else begin
				pf_ycr = keyword_set(log) ? 10.^(!y.crange) : (!y.crange)
				dumidx = where(~between(ts_data[nc,*],pf_ycr[0],pf_ycr[1]) and finite(ts_data[nc,*]),dumidxcnt)
				if dumidxcnt gt 0 then begin
					print, 'Found '+strcompress(dumidxcnt,/rem)+' Value(s) out of plotting range -> Set to NAN.'
					if dumidxcnt lt 10 then begin
						print,'Index ',dumidx
						print,'Value ',ts_data[nc,dumidx]
					endif
					ts_data[nc,dumidx] = !values.f_nan
				endif
				define_oplots, opl, cols, spos, linestyle, psymm, ystretch,/timeseries, color = color
				polyfill_ts_error,ts_data[nc,*],ts_data[nc_unc,*],error=error,color=cgColor("Gray"),fill=fill,bars=bars
				if keyword_set(smoothTS) then begin
					oplot,ts_data[nc,*],col=cgcolor(cols),linestyle=linestyle,thick=2
					sm_data = smooth(reform(ts_data[nc,*]),8,/nan,/edge_truncate)
					sm_idx = where(~finite(ts_data[nc,*]),sm_cnt)
					if sm_cnt gt 0 then sm_data[sm_idx] = !values.f_nan
					oplot,sm_data,psym=cgsymcat(psym),thick=thick,col=cgcolor(cols),linestyle=linestyle,symsize=syms
				endif else oplot,ts_data[nc,*],psym=cgsymcat(psym),thick=thick,col=cgcolor(cols),linestyle=linestyle,symsize=syms
				ystretch = mst_paper ? (0.8 * ystretch) : ystretch*(opl le 2 ? 1.3 : 1.1)
				legend,algon1+dtn+hct+apx,thick=thick,color=cgcolor(cols),spos=spos,ystretch=ystretch,$
				charsize=lcharsize,charthick=charthick, linestyle = linestyle,psym=cgsymcat(psym)
			endelse
			if keyword_set(show_trend) then begin
				define_oplots, opl, cols, spos, linestyle, psymm, ystretch,/timeseries, color = color
				res1=linfit(idx,ts_data[nc,idx],yfit=yfit1)
				dec1 = ' '+string(((yfit1[idx_cnt-1]-yfit1[0])/float(idx_cnt)*120.),f='(f10.5)')+unit
				if opl eq 0 then oplot,idx,yfit1 else oplot,idx,yfit1,col=cgcolor(cols)
				str_pholder = strjoin(replicate(' ',strlen(ref)))
				print,'Trend / decade'+str_pholder+' : '+dec1
			endif
		end_save, save_as1
	endif

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro plot_cci_gac_time_series, 	diff = diff,algo=algo, sat = sat, reference = reference, save_as=save_as,win_nr=win_nr	, $
				coverage=coverage, single_var = single_var, mini=mini,maxi=maxi,limit=limit,land=land,sea=sea		, $
				lon=lon,lat=lat,unit=unit,bild=bild,show_values = show_values,zoom=zoom,error=error,verbose=verbose	, $
				other=other,ctable=ctable, globe = globe, antarctic = antarctic, arctic = arctic, p0lon = p0lon		, $
				p0lat = p0lat, Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, ztext = ztext	, $
				sinusoidal = sinusoidal,robinson=robinson,zonal_only=zonal_only,nobar=nobar, stereographic = stereographic,$
				msg=msg, logarithmic=logarithmic,white_bg=white_bg,oplots=oplots,rot=rot,magnify=magnify,countries=countries,$
				symsize=symsize,notitle=notitle,no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box,$
				version1=version1,version2=version2,ts_extras=ts_extras

	sat = keyword_set(sat) ? strlowcase(sat) : 'noaa18'
	ref = keyword_set(reference) ? strlowcase(reference) : 'gac'
	alg = keyword_set(algo) ? algo2ref(algo) : 'cci'
	win_nr = keyword_set(win_nr) ? win_nr : 1
	if stregex(sat,'noaa',/bool,/fold) then sat = strlowcase(strjoin(strsplit(sat,/ext,'-')))
	if keyword_set(land) then coverage = 'land'
	if keyword_set(sea) then coverage = 'sea'
	datum = !DEFAULT_TS_PERIOD ; start dummy
	no_trends_etc = 0
	!p.multi=0

	if adv_keyword_set(mini) and adv_keyword_set(maxi) then yrange = [mini,maxi] 

	sav     = keyword_set(save_as)
	zoo     = keyword_set(zonal_only)
	wbg     = keyword_set(white_bg)

	algon1 = sat_name(alg,sat,version=version1)
	algon2 = sat_name(ref,sat,version=version2)

	set_colors, rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, $
			panoply = panoply

	single = strlowcase(single_var)
	if single eq 'cc_total' then single = 'cfc'

	varn = strcompress(strsplit(single,',',/ext),/rem)

	if n_elements(varn) ge 2 then begin

		if n_elements(varn) ne 2 then begin
			ok = dialog_message('plot_cci_gac_time_series: Same file! , same variable? Too many Variables? '+single)
			return
		endif
		single = strjoin(varn,'-')
		d = get_available_time_series( 	alg, varn[0], sat, coverage = coverage, period = datum, uncertainty = uncertainty, $
						sav_file = sfile, unit = unit, trend = trend, tr_corr = tr_corr, ts_extras=ts_extras, $
						stddev = stddev, anomalies = anomalies, found = found, no_trend_found = no_trend_found)
		if not found then begin
			if ~no_trend_found then ok = dialog_message('plot_cci_gac_time_series: Sav_file not found: '+sfile)
			return
		endif
		longname = full_varname(varn[0],/universal)
		if trend then begin
			bild1 = d.TREND.MEAN
			longname = longname + ' - trend p.D.'
		endif else if anomalies then begin
			bild1 = d.TREND.ANOM_MEAN
			longname = longname + ' - Anomalies'
		endif else if uncertainty then begin
			bild1 = d.UNC 
			longname = longname + ' - Uncertainty'
		endif else if stddev then begin
			bild1 = d.STD 
			longname = longname + ' - Standard Deviation'
		endif else begin
			bild1  = d.MEAN
		endelse

		d1 = get_available_time_series( alg, varn[1], sat, coverage = coverage, period = datum, uncertainty = uncertainty, $
						sav_file = sfile, unit = unit2, trend = trend, tr_corr = tr_corr,ts_extras=ts_extras, $
						stddev = stddev, anomalies = anomalies, found = found, no_trend_found = no_trend_found)
		if not found then begin
			if ~no_trend_found then ok = dialog_message('plot_cci_gac_time_series: Sav_file not found: '+sfile)
			return
		endif
		longname2 = full_varname(varn[1],/universal)
		if trend then begin
			bild2 = d1.TREND.MEAN
			longname2 = longname2 + ' - trend p.D.'
		endif else if anomalies then begin
			bild2 = d1.TREND.ANOM_MEAN
			longname2 = longname2 + ' - Anomalies'
		endif else if uncertainty then begin
			bild2 = d1.UNC 
			longname2 = longname2 + ' - Uncertainty'
		endif else if stddev then begin
			bild2 = d1.STD 
			longname2 = longname2 + ' - Standard Deviation'
		endif else begin
			bild2 = d1.MEAN
		endelse
		dtn    = [appendix(varn[0],trend_corrected=tr_corr),appendix(varn[1],trend_corrected=tr_corr)]
	endif else begin
		d = get_available_time_series( 	alg, single, sat, coverage = coverage, reference = ref, period = datum, uncertainty = uncertainty, $
						sav_file = sfile, longname = longname, unit = unit, trend = trend, tr_corr = tr_corr,ts_extras=ts_extras, $
						stddev = stddev, anomalies = anomalies, found = found, no_trend_found = no_trend_found)
		if not found then begin
			if ~no_trend_found then ok = dialog_message('plot_cci_gac_time_series: Sav_file not found: '+sfile)
			return
		endif

		if trend then begin
			bild1 = d.TREND.MEAN
			bild2 = d.TREND.MEAN2
			longname = longname + ' - trend p.D.'
		endif else if anomalies then begin
			bild1 = d.TREND.ANOM_MEAN
			bild2 = d.TREND.ANOM_MEAN2
			longname = longname + ' - Anomalies'
		endif else if uncertainty then begin
			bild1 = d.UNC 
			bild2 = d.UNC2
			longname = longname + ' - Uncertainty'
		endif else if stddev then begin
			bild1 = d.STD 
			bild2 = d.STD2 
			longname = longname + ' - Standard Deviation'
		endif else begin
			bild1 = d.MEAN
			bild2 = d.MEAN2
			no_trends_etc = 1
		endelse
		longname2 = longname
		unit2     = unit
		dtn       = [appendix(single,trend_corrected=tr_corr),appendix(single,trend_corrected=tr_corr)]
		varn      = [single[0],single[0]]
	endelse
	datum = d.actual_date

	if keyword_set(save_as) then begin
		if strcompress(save_as,/rem) eq '1' then begin
			save_as = !SAVE_DIR +'timeseries'+'/'+algon1+'_vs_'+algon2+'_time_series_difference_'+single_var+$
				'_L3C_'+strcompress(datum,/rem)+ (keyword_set(coverage) ? '_'+strlowcase(coverage):'')+'.eps'
			save_as1 = !SAVE_DIR +'timeseries'+'/'+algon1+'_time_series_2d_mean_'+single_var+'_'+strcompress(datum,/rem)+ $
				(keyword_set(coverage) ? '_'+strlowcase(coverage):'')+'.eps'
			save_as2 = !SAVE_DIR +'timeseries'+'/'+algon2+'_time_series_2d_mean_'+single_var+'_'+strcompress(datum,/rem)+ $
				(keyword_set(coverage) ? '_'+strlowcase(coverage):'')+'.eps'
			save_as3 = !SAVE_DIR +'timeseries'+'/'+algon1+'_vs_'+algon2+'_time_series_2dhist_'+single_var+$
				'_L3C_'+strcompress(datum,/rem)+ (keyword_set(coverage) ? '_'+strlowcase(coverage):'')+'.eps'
			save_as4 = !SAVE_DIR +'timeseries'+'/'+algon1+'_vs_'+algon2+'_time_series_zonal_mean_'+single_var+$
				'_L3C_'+strcompress(datum,/rem)+ (keyword_set(coverage) ? '_'+strlowcase(coverage):'')+'.eps'
		endif else begin
			save_as1 = file_name_info(save_as,/path,/name) + '_2d_mean_'+algon1+'_'+file_name_info(save_as,/ext)
			save_as2 = file_name_info(save_as,/path,/name) + '_2d_mean_'+algon2+'_'+file_name_info(save_as,/ext)
			save_as3 = file_name_info(save_as,/path,/name) + '_2dhist_'+algon1+'_vs_'+algon2+'_'+file_name_info(save_as,/ext)
			save_as4 = file_name_info(save_as,/path,/name) + '_zonal_mean_'+algon1+'_vs_'+algon2+'_'+file_name_info(save_as,/ext)
		endelse
	endif else if win_nr ne -1 then win, win_nr, size=700,ysize=1200,title='CCI CLARA time series'

	make_geo,lon,lat,grid = get_grid_res(bild1)

	if ~zoo then begin
		set_proj, globe = globe, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat,no_label=no_label				, $
			Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, $
			limit = limit, ortho=ortho,iso=iso,bar_horizontal=bar_horizontal,grid=grid,londel=londel,latdel=latdel,label=label,noborder=noborder,$
			stereographic=stereographic, no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify,$
			nobar=nobar,msg=msg, maxvalue = adv_keyword_set(maxi) ? maxi[0]:maxvalue, bar_format=bar_format,lambert=lambert, $
			lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_box=no_box,position = position,no_grid=no_grid,horizon=horizon	

		if keyword_set(diff) then begin
			start_save, save_as, thick = thick, size = [32,20]
				dumdata = bild1 - bild2 & minv = -20. & maxv = 20.
				ititle = keyword_set(notitle) ? '' : datum+' '+strupcase(single)
				btitle = 'Diff '+algon1+' '+strupcase(varn[0])+' - '+algon2+' '+strupcase(varn[1])+unit
				m = obj_new("map_image",dumdata,lat,lon,void_index=where(bild1 eq -999.),n_lev=4	, $
					max=(adv_keyword_set(maxi) ? maxi : maxv),min=(adv_keyword_set(mini) ? mini: minv), $
					countries=countries,usa=countries,magnify = magnify, figure_title = ititle, title= btitle, g_eq=g_eq, l_eq=l_eq	, $
					charthick = !m_charthick, charsize  = !m_charsize , $
					bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow, box_axes=box_axes,$
					brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow,flip_colours = flip_colours,$
					ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel, logarithmic=logarithmic	, $
					noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar,lambert=lambert,horizon=horizon, $
					p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine, panoply = panoply, $
					lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
					mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson, $
					stereographic=stereographic,label=label,position = position, $
					limit = limit,format=bar_format,debug=verbose)
				obj_destroy,m
			end_save,save_as
			bild = dumdata
			if keyword_set(show_values) then show_pixel_value, dumdata, lon,lat, data=single, unit=unit

			if keyword_set(zoom) then begin
				get_new_corners = 1
				m -> zoom,get_new_corners = get_new_corners,/print_new, ztext = ztext,magnify=magnify
				if win_nr ne -1 then win, win_nr
				plot_cci_gac_time_series, diff = diff, sat = sat, reference = reference, save_as=save_as,win_nr=win_nr,$
				coverage=coverage, single_var = single_var, mini=mini,maxi=maxi,limit=get_new_corners,land=land,sea=sea, $
				lon=lon,lat=lat,unit=unit,bild=bild,show_values = show_values
			endif
			return
		endif
		start_save, save_as1, thick = thick, size = [32,20]
			!p.multi= keyword_set(save_as1) ? 0 : [0,2,2]
			dumdata = bild1 & minv = 0. & maxv = 100.
			ititle = keyword_set(notitle) ? '' : datum+(strmatch(varn[0],varn[1]) ? ' '+longname+dtn[0]+unit:'')
			btitle = algon1+(strmatch(varn[0],varn[1]) ? '': ' '+longname+dtn[0]+unit)
			m = obj_new("map_image",dumdata,lat,lon,void_index=where(dumdata eq -999.),n_lev=4	, $
				max=(adv_keyword_set(maxi) ? maxi : maxv),min=(adv_keyword_set(mini) ? mini: minv)	, $
				countries=countries,usa=countries,magnify = magnify, figure_title = ititle, title= btitle, $
				charthick = !m_charthick, charsize  = !m_charsize , $
				bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow	, box_axes=box_axes,$
				brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow,flip_colours = flip_colours,$
				ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel, logarithmic=logarithmic	, $
				noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar,lambert=lambert,horizon=horizon, $
				p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine, panoply = panoply, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
				mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,stereographic=stereographic, $
				label=label, limit = limit,debug=verbose,format=bar_format,position = position)
			obj_destroy,m
		end_save, save_as1
		start_save, save_as2, thick = thick, size = [32,20]
			dumdata = bild2 & minv = 0. & maxv = 100.
			ititle = keyword_set(notitle) ? '' : datum+(strmatch(varn[0],varn[1]) ? ' '+longname2+dtn[1]+unit : '')
			btitle = algon2+(strmatch(varn[0],varn[1]) ? '': ' '+longname2+dtn[1]+unit2)
			m = obj_new("map_image",dumdata,lat,lon,void_index=where(dumdata eq -999.),n_lev=4	, $
				max=(adv_keyword_set(maxi) ? maxi : maxv),min=(adv_keyword_set(mini) ? mini: minv)	, $
				countries=countries,usa=countries,magnify = magnify, figure_title = ititle, title= btitle		, $
				charthick = !m_charthick, charsize  = !m_charsize ,$
				bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow	, box_axes=box_axes,$
				brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow,flip_colours = flip_colours,$
				ortho = ortho,bar_horizontal = bar_horizontal, grid=grid,londel=londel,latdel=latdel, logarithmic=logarithmic	, $
				noborder=noborder, no_draw_border=no_draw_border, no_color_bar=no_color_bar,lambert=lambert,horizon=horizon, $
				p0lon= p0lon, p0lat = p0lat, iso = iso , goodeshomolosine = goodeshomolosine, panoply = panoply, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
				mollweide=mollweide,hammer=hammer,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,stereographic=stereographic, $
				label=label, limit = limit,debug=verbose,format=bar_format,position = position)
			obj_destroy,m
		end_save,save_as2

		if keyword_set(zoom) then begin
			get_new_corners = 1
			m -> zoom,get_new_corners = get_new_corners,/print_new, ztext = ztext,magnify=magnify
			if win_nr ne -1 then win, win_nr
			plot_cci_gac_time_series, diff = diff, sat = sat, reference = reference, save_as=save_as,win_nr=win_nr,$
			coverage=coverage, single_var = single_var, mini=mini,maxi=maxi,limit=get_new_corners,land=land,sea=sea, $
			lon=lon,lat=lat,unit=unit,bild=bild,show_values = show_values
		endif

		cc = 5
		; hist2d
		start_save, save_as3, thick = thick, size = [32,20]
			; 2d hist
			if no_trends_etc then begin
				aa     = d.HIST_2D.data
				regr   = d.HIST_2D.linfit.regr
				min_a  = d.HIST_2D.minvalue[0]
				max_a  = d.HIST_2D.maxvalue[0]
				bin    = d.HIST_2D.bin
			endif else if ~strmatch(varn[0],varn[1]) then begin
				bin    = d.HISTOGRAM.bin
				min_a  = min([d.HISTOGRAM.minvalue[0],d1.HISTOGRAM.minvalue[0]])
				max_a  = max([d.HISTOGRAM.maxvalue[0],d1.HISTOGRAM.maxvalue[0]])
				aa     = hist_2d(float(bild1),float(bild2),min1=min_a,max1=max_a,bin1=bin,min2=min_a,max2=max_a,bin2=bin)
				h2_idx = where((bild1 ne -999.) and (bild2 ne -999.),h2_cnt )
				regr   = h2_cnt gt 0 ? linfit(float(bild1[h2_idx]),float(bild2[h2_idx])) : [-999.,0.]
			endif else begin
				bin    = (d.HISTOGRAM.bin / 10.) > 0.01
				maxi   = max(rnd(minmax(abs([bild1,bild2]),/nan,no_data_value=-999),bin))
				min_a  = (-1.*maxi)
				max_a  = maxi
				aa     = hist_2d(float(bild1),float(bild2),min1=min_a,max1=max_a,bin1=bin,min2=min_a,max2=max_a,bin2=bin)
				h2_idx = where((bild1 ne -999.) and (bild2 ne -999.),h2_cnt )
				regr   = h2_cnt gt 0 ? linfit(float(bild1[h2_idx]),float(bild2[h2_idx])) : [-999.,0.]
			endelse

			bar_format='(i)' & bar_title= 'nr of occurrence'
			if (1 eq 1) then begin
				; normalize to maximum of occurrence
				aa = float(aa)/max(float(aa)) *100. 
				bar_format='(f7.3)' 
				bar_title= 'norm. to max. occurrence'
			endif

			view2d,aa,xtitle=algon1+(strmatch(varn[0],varn[1]) ? '':' '+longname+dtn[0]+unit) , $
			ytitle=algon2+(strmatch(varn[0],varn[1]) ? '':' '+longname2+dtn[1]+unit2) ,$
			title = (keyword_set(notitle) ? '':(strmatch(varn[0],varn[1]) ? longname+dtn[0]+unit:'')+' (Binsize='+string(bin,f='(f6.3)')+')'), $
			bar_format=bar_format,no_data_val=0,/log,position=pos1, xmargin=[8,3],ymargin=[3,2]+(sav ? [0,2]:0),$
			bar_title= bar_title, xticks = cc, xtickv = vector(0,(size(aa,/dim))[0]-1,cc+1),yticks = cc, $
			ytickv = vector(0,(size(aa,/dim))[1]-1,cc+1), $
			xtickname=strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f5.2)':'(i)')),/rem), $
			ytickname=strcompress(string(vector(min_a,max_a,cc+1),f=(max_a lt 10 ? '(f5.2)':'(i)')),/rem), $
			xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize
			oplot,!x.crange,[regr[1]*!x.crange+regr[0]/bin],linestyle=2, thick=thick
			oplot,!x.crange,!y.crange, thick=thick
		end_save, save_as3
	endif

	if sav then begin
		charthick = !p_charthick ;1.5
		xcharsize = !p_xcharsize ;1.7 
		ycharsize = !p_ycharsize ;1.7
		lcharsize = !l_charsize  ;2.5
		xmargin   = [14,6]
		ymargin   = [ 7,3]
	endif else if wbg and zoo then begin
		charthick = !p_charthick ;3.0
		xcharsize = !p_xcharsize ;2.5
		ycharsize = !p_ycharsize ;2.5
		lcharsize = !l_charsize  ;3.0
		xmargin   = [20,6]
		ymargin   =  [8,3] ;+ (keyword_set(notitle) ? 0 : [0,1])
	endif else begin
		charthick = 1.2
		xcharsize = 1.2 
		ycharsize = 1.2
		lcharsize = 1.5
		xmargin   =[10,3]
		ymargin   = [5,2]
	endelse

	; zonal mean
	start_save, save_as4, thick = thick, size = [32,20]
		if wbg then thick=4
		date = zoo ? datum : ''
		;------zonal-means----------------------
		fillvalue = -999.
		lat_res = 1. > get_grid_res(bild1)
		medi_c = zonal_average(bild1,lat,fillvalue=fillvalue,lat_zon=lat1d_c,/nan,/mean,lat_res=lat_res)
		medi_g = zonal_average(bild2,lat,fillvalue=fillvalue,lat_zon=lat1d_g,/nan,/mean,lat_res=lat_res)
		yr  = keyword_set(yrange) ? yrange : [(unit eq ' [K]' ? 200:0.1),max([medi_c,medi_g],/nan)]
		; globale bias, rmse und bc-rmse gewichtet nach latitude
		if no_trends_etc then begin
			bias = d.OVERALL_STATS.LATITUDE_WEIGHTED.BIAS
			rmse = d.OVERALL_STATS.LATITUDE_WEIGHTED.RMSE
			stdd = d.OVERALL_STATS.LATITUDE_WEIGHTED.BC_RMSE
		endif else begin
			md_idx = where((bild1 ne -999.) and (bild2 ne -999.),md_cnt )
			if md_cnt gt 0 then begin
				bias = gbias(bild1[md_idx],bild2[md_idx],lat[md_idx])
				rmse = grmse(bild1[md_idx],bild2[md_idx],lat[md_idx])
				stdd = bc_rmse(bias,rmse)
			endif else begin
				bias = -999.
				rmse = -999.
				stdd = -999.
			endelse
		endelse
		plot,[0,0],[1,1],xr=[-90,90],xs=3,xticks=6,xtickname=['-90','-60','-30','0','30','60','90'], $
		xtitle='latitude [degrees]',ytitle=(trend ? 'linear trend ':'')+longname+' '+unit,yr=yr,position=pos2,$
		noerase=~zoo,ylog=logarithmic, title= keyword_set(notitle) ? '':'bias: '+string(bias,f='(f7.2)')+' ; rmse: '+$
		string(rmse,f='(f6.2)')+' ; bc-rmse: '+string(stdd,f='(f6.2)')+unit,$
		charthick=charthick,xcharsize=xcharsize,ycharsize=ycharsize,xmargin=xmargin,ymargin=ymargin
		oplot,lat1d_c,medi_c,thick=thick,col=cgColor(!compare_col1)
		oplot,lat1d_g,medi_g,thick=thick,col=cgColor(!compare_col2)
		if ~keyword_set(show_values) then begin
			legend,[date+' '+algon2+dtn[1],date+' '+algon1+dtn[0]],thick=replicate(thick,2),spos='top',$
			charsize=lcharsize,color=[cgColor(!compare_col2) , cgColor(!compare_col1)] ,charthick=charthick
		endif else begin
			legend,date+' '+algon2+dtn[1],thick=thick,color=cgColor(!compare_col2) ,spos='bl',charsize=lcharsize,charthick=charthick
			legend,date+' '+algon1+dtn[0],thick=thick,color=cgColor(!compare_col1) ,spos='br',charsize=lcharsize,charthick=charthick
		endelse
		;--------------------------------------------
	end_save,save_as4

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro vergleiche_ctp_cot_histogram_cci_mit_clara, ccifile, varname = varname, mini = mini, maxi = maxi, limit=limit, zoom=zoom,algo1 = algo1, sat=sat,$
			win_nr = win_nr,save_as= save_as,land = land, sea = sea,hist_cloud_type = hist_cloud_type, ztext = ztext, $
			hist_phase=hist_phase, reference = reference,timeseries=timeseries,ctable=ctable,other=other, $
			difference = difference, show_values = show_values, out = out, p0lon = p0lon, p0lat = p0lat, $
			antarctic = antarctic, arctic = arctic, mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,msg=msg,$
			robinson=robinson, hammer = hammer, goode = goode, globe = globe, verbose = verbose,nobar=nobar, $
			stereographic = stereographic, logarithmic=logarithmic,datum=datum,magnify=magnify,countries=countries,notitle=notitle,$
			no_continents=no_continents,no_grid=no_grid,no_label=no_label,no_box=no_box,version1=version1,version2=version2,shape_file=shape_file

	datum = keyword_set(datum) ? datum : stregex(file_basename(ccifile),'[0-9]+',/ext)
	year  = strmid(datum,0,4)
	month = strmid(datum,4,2)
	ts    = keyword_set(timeseries)
	data  = keyword_set(varname) ? strlowcase(varname) : 'hist2d_cot_ctp'
	; the limits needs later to be freed otherwise map_image will have an error
	if keyword_set(antarctic) then limit = [-90.0,-180,-60.,180]
	if keyword_set(arctic) then limit = [ 60.,-180, 90.0,180]
	limit_test = keyword_set(limit)

	ls    = ( keyword_set(land) or keyword_set(sea) ) ? 1:0
	hct   = keyword_set(hist_cloud_type) ? hist_cloud_type : 'cu'
	algo2 = keyword_set(reference) ? algo2ref(reference) : ''
	apx   = stregex(data,'ice',/fold,/bool) ? 'Ice ' : (stregex(data,'liq',/fold,/bool) ? 'Liquid ' : '')

	if (hct eq 'hist2d' or hct eq 'hist_2d' or hct eq 'max') and is_jch(varname,/rat) then begin
		ok = dialog_message('vergleiche_ctp_cot_histogram_cci_mit_clara: '+hct+' not possible with hist2d_ratio!')
		return
	endif

	if keyword_set(shape_file) then begin
		if ~file_test(shape_file) then free, shape_file
	endif

	algon1 = sat_name(algo1,sat,year=(ts ? 0:year),month=(ts ? 0:month),level='l3c',version=version1)
	algon2 = sat_name(algo2,sat,year=(ts ? 0:year),month=(ts ? 0:month),level='l3c',version=version2)

	set_colors,rainbow,bwr,extended_rainbow,greyscale,elevation,flip_colours,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply

	save_dir = !SAVE_DIR +'ctp_cot_histogram/'
	win_nr    = adv_keyword_set(win_nr) ? win_nr : 1
	sav = keyword_set(save_as)

	if ts then begin
		datum = !DEFAULT_TS_PERIOD
		struc = get_histo_time_series(algo1, data, sat, period = datum, longname = longname, unit = unit, sav_file = sav_file, found = found, compare=algo2)
		if not found then begin
			ok = dialog_message('vergleiche_ctp_cot_histogram_cci_mit_clara: No time_series "sav" file found for '+algon1)
			return
		endif
		cci = struc.bild
		adc = struc.actual_date
		struc = get_histo_time_series(algo2, data, sat, period = datum, longname = longname, unit = unit, sav_file = sav_file, found = found, compare=algo2)
		if not found then begin
			ok = dialog_message('vergleiche_ctp_cot_histogram_cci_mit_clara: No time_series "sav" file found for '+algon2)
			return
		endif
		gac = struc.bild
		adg = struc.actual_date
		datum = strmatch(adg,adc) ? adc : 'Alg1-'+adc+' Alg2-'+adg
		fillvalue1 = -999.
		fillvalue2 = -999.
		free,struc
	endif else begin
		adc=''
		cci = get_data(file=ccifile,year,month,sat=sat,algo=algo1,data=data,found = found,no_data_val=fillvalue1,dim3=dim3, verbose = verbose,/print_filename)
		if not found then begin
			if ~file_test(ccifile) then return
			ok = dialog_message('vergleiche_ctp_cot_histogram_cci_mit_clara: Data '+data+' not found in '+algo1+' file!')
			return
		endif
		adg=''
		gac = get_data(file=gacfile,year,month,sat=sat,algo=algo2,data=data,found = found,no_data_val=fillvalue2,dim3=dim3, verbose = verbose,print_filename=2)
		if not found then begin
			if ~file_test(gacfile) then return
			ok = dialog_message('vergleiche_ctp_cot_histogram_cci_mit_clara: Data '+data+' not found in Reference ('+algo2+') file!')
			return
		endif
	endelse
	lname = ''
	if strmatch(algon1,algon2) then begin
		if ~ts then begin
			mtime1 = (file_info(ccifile)).mtime
			mtime2 = (file_info(gacfile)).mtime
			if mtime1 gt mtime2 then begin
				algon1 = 'NEW_'+algon1
				algon2 = 'OLD_'+algon2
			endif else if mtime2 gt mtime1 then begin
				algon1 = 'OLD_'+algon1
				algon2 = 'NEW_'+algon2
			endif else begin
				algon1 = 'File1__'+algon1
				algon2 = 'File2__'+algon2
			endelse
		endif else begin
			algon1 = 'File1__'+algon1
			algon2 = 'File2__'+algon2
		endelse
	endif

	; get grid res of both and create final gridsize
	cci_grid = get_grid_res(cci[*,*,0,0,0,0],found=found)
	if not found then begin print,'Stop! this should not happen, except an unknown regular grid resolution is used ' & stop & end
	gac_grid = get_grid_res(gac[*,*,0,0,0,0],found=found)
	if not found then begin print,'Stop! this should not happen, except an unknown regular grid resolution is used ' & stop & end
	out_grid = max([cci_grid,gac_grid],grid_index)

	;this is a poor mans solution
	;make sure both algos see the same and mark areas where one of both is not seeing anything
	;e.g compare to claas
	si_cci = size(cci,/dim)
	ndv_cci_array = total(total((cci eq fillvalue1),3),3) eq product(si_cci[2:3]) ; no observations on cci grid
	si_gac = size(gac,/dim)
	ndv_gac_array = total(total((gac eq fillvalue2),3),3) eq product(si_gac[2:3])
	;make it available for each other
	ndv_cci_array_gac = congrid(ndv_cci_array,si_gac[0],si_gac[1]) ; no observations on cci grid brought to gac grid
	ndv_gac_array_cci = congrid(ndv_gac_array,si_cci[0],si_cci[1]) ; no observations on gac grid brought to cci grid
	fillv_index_cci = where(ndv_cci_array eq 1 or ndv_gac_array_cci eq 1,idxcnt)
	fillv_index_gac = where(ndv_gac_array eq 1 or ndv_cci_array_gac eq 1,idxcnt)
	fillv_index_out = grid_index eq 0 ? fillv_index_cci : fillv_index_gac
	;-------

	make_geo,file=ccifile,lon_cci,lat_cci,grid=cci_grid ; cci
	make_geo,file=gacfile,lon_gac,lat_gac,grid=gac_grid ; ref
	if ls then begin
		dem_cci = get_dem(grid_res=cci_grid)
		dem_gac = get_dem(grid_res=gac_grid)
	endif

	if hct eq '1d' or hct eq '1d_cot' or hct eq '1d_ctp' then begin

		if keyword_set(limit)     then dumlimit = limit
		if keyword_set(save_as)   then begin
			!p.multi=0
			save_dum = save_dir+strcompress(data+'_'+datum+'_'+$
			(keyword_set(hct) ? '_'+strupcase(hct) : '')+(keyword_set(dumlimit) ? '_limit_'+strjoin(strcompress(string(dumlimit,f='(i)'),/rem),'_'):'')+$
			(keyword_set(land) ? '_land':'')+ (keyword_set(sea) ? '_sea':''),/rem)
		endif else begin
			if win_nr ne -1 then win, win_nr,title=data
			if hct eq '1d' then !p.multi=[0,1,2]
		endelse
		yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,50]

		if is_jch(varname,/rat) then begin
			liq = get_1d_hist_from_jch(cci[*,*,*,*,0],algo1,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_cci,found=found1,shape_file=shape_file,lon=lon_cci,lat=lat_cci,dem=dem_cci)
			ice = get_1d_hist_from_jch(cci[*,*,*,*,1],algo1,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_cci,found=found1,shape_file=shape_file,lon=lon_cci,lat=lat_cci,dem=dem_cci)
			cci_histos = {	cot:(reform(float(liq.cot)/float((liq.cot>0)+(ice.cot>0)>1)) *100.),$
					ctp:(reform(float(liq.ctp)/float((liq.ctp>0)+(ice.ctp>0)>1)) *100.)}
			liq = get_1d_hist_from_jch(gac[*,*,*,*,0],algo2,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_gac,found=found2,shape_file=shape_file,lon=lon_gac,lat=lat_gac,dem=dem_gac)
			ice = get_1d_hist_from_jch(gac[*,*,*,*,1],algo2,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_gac,found=found2,shape_file=shape_file,lon=lon_gac,lat=lat_gac,dem=dem_gac)
			gac_histos = {	cot:(reform(float(liq.cot)/float((liq.cot>0)+(ice.cot>0)>1)) *100.),$
					ctp:(reform(float(liq.ctp)/float((liq.ctp>0)+(ice.ctp>0)>1)) *100.)}
			ytitle = 'Liquid Fraction [%]'
		endif else begin
			cci_histos = get_1d_hist_from_jch(cci,algo1,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_cci,found=found1,shape_file=shape_file,lon=lon_cci,lat=lat_cci,dem=dem_cci)
			gac_histos = get_1d_hist_from_jch(gac,algo2,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic, $
						land=land, sea=sea,fillv_index=fillv_index_gac,found=found2,shape_file=shape_file,lon=lon_gac,lat=lat_gac,dem=dem_gac)
			cci_histos = {	cot:cci_histos.cot/total(cci_histos.cot)*100.,$
					ctp:cci_histos.ctp/total(cci_histos.ctp)*100.}
			gac_histos = {	cot:gac_histos.cot/total(gac_histos.cot)*100.,$
					ctp:gac_histos.ctp/total(gac_histos.ctp)*100.}
			ytitle = 'Relative Occurrence [%]'
		endelse
		if found1 and found2 then begin
			thick = 2
			; CTP
			if hct eq '1d' or hct eq '1d_ctp' then begin
				if keyword_set(save_as) then start_save, save_dum+'_'+algon1+'_-_'+algon2+'_CTP.eps', thick = thick, size = [32,16]
					plot,[0,0],[1,1],yr=yrange,xr=[0,6],xticks=7,xtickname=xtickname.ctp, $
					xtitle=apx+'Cloud Top Pressure [hPa]',ytitle=ytitle,xminor=2,$
					charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
					xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
					ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
					xmargin   = [12,4],ymargin = [6,2]
					oplot,cci_histos.ctp,thick=thick+1,col=cgcolor(!compare_col1) ,psym=-8
					oplot,gac_histos.ctp,thick=thick+1,col=cgcolor(!compare_col2) ,psym=-8
					legend,[algon1,algon2],psym=[8,8],numsym=1,color=[cgcolor(!compare_col1) ,cgcolor(!compare_col2)],$
					thick=replicate(thick,2),clrbox=clrbox,spos='tr', charsize=(keyword_set(save_as) ? !l_charsize : 1.5)
				if keyword_set(save_as) then end_save, save_dum+'_'+algon1+'_-_'+algon2+'_CTP.eps'
			endif
			; COT

			if hct eq '1d' or hct eq '1d_cot' then begin
				if keyword_set(save_as) then start_save, save_dum+'_'+algon1+'_-_'+algon2+'_COT.eps', thick = thick, size = [32,16]
					plot,[0,0],[1,1],yr=yrange,xr=[0,5],xticks=6,xtickname=xtickname.cot, $
					xtitle=apx+'Cloud optical Thickness',ytitle=ytitle,xminor=2, $
					charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
					xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
					ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
					xmargin   = [12,4],ymargin = [6,2]
					oplot,cci_histos.cot,thick=thick+1,col=cgcolor(!compare_col1) ,psym=-8
					oplot,gac_histos.cot,thick=thick+1,col=cgcolor(!compare_col2) ,psym=-8
					legend,[algon1,algon2],psym=[8,8],numsym=1,color=[cgcolor(!compare_col1) ,cgcolor(!compare_col2)],$
					thick=replicate(thick,2),clrbox=clrbox,spos='tr', charsize=(keyword_set(save_as) ? !l_charsize : 1.5)
				if keyword_set(save_as) then end_save, save_dum+'_'+algon1+'_-_'+algon2+'_COT.eps'
			endif
		endif
		return
	endif

	make_geo, lon_c, lat_c, grid_res = out_grid
	if ls then dem_c = get_dem(grid_res=out_grid)

	if keyword_set(shape_file) then begin
		print,'Using shape_file, ',shape_file
		shp = get_coverage(lon_c,lat_c,shape_file=shape_file)
	endif else shp = byte(lon_c * 0) +1b

	; CCI
	make_geo,file=ccifile,lon_cci,lat_cci,grid=cci_grid
	if hct eq 'max' then begin
		; -------maxtype
		cci = get_hct_maxtype( cci, algo1, grid_res = out_grid,lon=lon_cci,lat=lat_cci,fillvalue=fillvalue1,htypes=htypes)
		; -------
	endif else if hct eq 'hist2d' or hct eq 'hist_2d' then begin
		; cci
		cci = get_2d_rel_hist_from_jch( cci, algo1, dem = dem_cci, land = land, sea = sea, limit = limit, antarctic = antarctic, arctic = arctic, $
						lon = lon_cci, lat = lat_cci, fillvalue = fillvalue1, fillv_index = fillv_index_cci, found = found,shape_file=shape_file)
		if not found then return
	endif else begin
		if is_jch(varname,/rat) then begin
			liq = get_hct_data(hct,cci[*,*,*,*,0],algo1,sdum=sdum,found=found,grid=out_grid)
			ice = get_hct_data(hct,cci[*,*,*,*,1],algo1,sdum=sdum,found=found,grid=out_grid)
			cci = reform(float(liq)/float((liq>0)+(ice>0)>1)) *100.
			bar_title = 'Liquid Fraction '+(keyword_set(hct) ? 'of '+strupcase(hct)+' Clouds [%]':'')
		endif else begin
			cci = get_hct_data(hct,cci,algo1,sdum=sdum,found=found,grid=out_grid,/relative)
			ratio_cci = get_hct_ratio(cci,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon_c,lat=lat_c, $
						dem=dem_c,land=land,sea=sea,fillv_index=fillv_index_out,shape_file=shape_file,/relative)
		endelse
	endelse

	lon_g=lon_c
	lat_g=lat_c
	if ls then dem_g = dem_c

	if hct eq 'max' then begin
		; -------maxtype
		gac = get_hct_maxtype( gac, algo2, grid_res = out_grid,lon=lon_gac,lat=lat_gac,fillvalue=fillvalue2, htypes=htypes)
		; -------
	endif else if hct eq 'hist2d' or hct eq 'hist_2d' then begin
		gac = get_2d_rel_hist_from_jch( gac, algo2, dem = dem_gac, land = land, sea = sea, limit = limit, antarctic = antarctic, arctic = arctic, $
						lon = lon_gac, lat = lat_gac, fillvalue = fillvalue2, fillv_index = fillv_index_gac, found = found, shape_file=shape_file)
		if not found then return
		if keyword_set(save_as) then begin
			!p.multi=0
			save_dum = save_dir+strcompress(data+'_'+datum+'_'+$
			(keyword_set(hct) ? '_'+strupcase(hct) : '')+(keyword_set(limit) ? '_limit_'+strjoin(strcompress(string(limit,f='(i)'),/rem),'_'):'')+$
			(keyword_set(land) ? '_land':'')+ (keyword_set(sea) ? '_sea':''),/rem)
		endif else begin
			if win_nr ne -1 then win, win_nr,title=data
		endelse
		plot_2d_rel_hist, cci, algon1, bild2=gac, name2=algon2, col_tab=col_tab, brewer=brewer, $
				mini=mini, maxi=maxi, save_as=save_dum, difference = difference, appendix = apx,nobar=nobar
		return
	endif else begin
		if is_jch(varname,/rat) then begin
			liq = get_hct_data(hct,gac[*,*,*,*,0],algo2,sdum=sdum,found=found,grid=out_grid)
			ice = get_hct_data(hct,gac[*,*,*,*,1],algo2,sdum=sdum,found=found,grid=out_grid)
			gac = reform(float(liq)/float((liq>0)+(ice>0)>1)) *100.
			bar_title = 'Liquid Fraction '+(keyword_set(hct) ? 'of '+strupcase(hct)+' Clouds [%]':'')
		endif else begin
			gac = get_hct_data(hct,gac,algo2,/relative,found=found,sdum=sdum,grid=out_grid)
			ratio_gac = get_hct_ratio(gac,sdum,limit=limit,antarctic=antarctic,arctic=arctic,lon=lon_g,lat=lat_g, $
						dem=dem_g,land=land,sea=sea,fillv_index=fillv_index_out,shape_file=shape_file,/relative)
		endelse
	endelse

	good_idx_g = where(lon_g ne -999 and lat_g ne -999 and finite(gac) and gac ne fillvalue2 and shp eq 1, gidx_cnt_g, ncomplement = bidx_cnt_g)
	good_idx_c = where(lon_c ne -999 and lat_c ne -999 and finite(cci) and cci ne fillvalue1 and shp eq 1, gidx_cnt_c, ncomplement = bidx_cnt_c)

	if gidx_cnt_c eq 0 or gidx_cnt_g eq 0 then begin
		print,'no valid Lon Lat points!'
		return
	endif

	if bidx_cnt_g gt 0 then begin
		lon_g = lon_g[good_idx_g]
		lat_g = lat_g[good_idx_g]
		gac = gac[good_idx_g]
		if ls then dem_g = dem_g[good_idx_g]
	endif
	if bidx_cnt_c gt 0 then begin
		lon_c = lon_c[good_idx_c]
		lat_c = lat_c[good_idx_c]
		cci = cci[good_idx_c]
		if ls then dem_c = dem_c[good_idx_c]
	endif

	max_cci = max(cci)
	max_gac = max(gac)
	if hct ne 'max' and ~is_jch(varname,/rat) then begin
		str_pholder = strjoin(replicate(' ',max([strlen(algon1),strlen(algon2)])))
		print,strupcase(hct)+'/all Ratio '+string(algon1,f='(A'+strcompress(strlen(str_pholder),/rem)+')')+' : '+ratio_cci
		print,strupcase(hct)+'/all Ratio '+string(algon2,f='(A'+strcompress(strlen(str_pholder),/rem)+')')+' : '+ratio_gac
	endif
	minvalue = 0
	maxvalue = 1
	if ls then begin
		if keyword_set(land) then begin
			void_index1 = where(cci eq (hct eq 'max'?-1:0) or dem_c eq 0,vd1_cnt)
			void_index2 = where(gac eq (hct eq 'max'?-1:0) or dem_g eq 0,vd2_cnt)
			if keyword_set(limit) then begin
				idx_h2d = where(cci ne 0 and gac ne 0 and between(lon_g,limit[1],limit[3]) and between(lat_g,limit[0],limit[2]) $
				        and dem_c ne 0 and dem_g ne 0,h2d_cnt)
			endif else idx_h2d = where(cci ne 0 and gac ne 0 and dem_c ne 0 and dem_g ne 0,h2d_cnt)
		endif
		if keyword_set(sea)  then begin
			void_index1 = where(cci eq (hct eq 'max'?-1:0) or dem_c ne 0,vd1_cnt)
			void_index2 = where(gac eq (hct eq 'max'?-1:0) or dem_g ne 0,vd2_cnt)
			if keyword_set(limit) then begin
				idx_h2d = where(cci ne 0 and gac ne 0 and between(lon_g,limit[1],limit[3]) and between(lat_g,limit[0],limit[2]) $
					        and dem_c eq 0 and dem_g eq 0,h2d_cnt)
			endif else idx_h2d = where(cci ne 0 and gac ne 0 and dem_c eq 0 and dem_g eq 0,h2d_cnt)
		endif
	endif else begin
		void_index1 = where(cci eq (hct eq 'max'?-1:0),vd1_cnt)
		void_index2 = where(gac eq (hct eq 'max'?-1:0),vd2_cnt)
		if keyword_set(limit) then begin
			idx_h2d = where(cci ne 0 and gac ne 0 and between(lon_g,limit[1],limit[3]) and between(lat_g,limit[0],limit[2]),h2d_cnt)
		endif else idx_h2d = where(cci ne 0 and gac ne 0,h2d_cnt)
	endelse

	if keyword_set(save_as) then begin
		!p.multi=0
		save_dum = save_dir+strcompress(data+'_'+datum+'_'+$
		(keyword_set(hct) ? '_'+strupcase(hct) : '')+(keyword_set(limit) ? '_ausschnitt':'')+(keyword_set(land) ? '_land':'')+ $
		(keyword_set(sea) ? '_sea':''),/rem)
	endif else begin
		if win_nr ne -1 then win, win_nr,title=data
		if ~keyword_set(difference) then !p.multi=[0,2,2]
 		if hct eq 'max' then !p.multi=[0,1,2]
	endelse

	; free limit for plotting only, no harm, all the void_indices are set properly,already
	if keyword_Set(antarctic) or keyword_Set(arctic) then free, limit

	set_proj, globe = globe, limit = limit, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat,no_label=no_label		, $
		  Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson			, $
		  ortho=ortho,iso=iso,bar_horizontal=bar_horizontal,grid=grid,londel=londel,latdel=latdel,label=label,noborder=noborder,stereographic=stereographic	, $
		  no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify,nobar=nobar,msg=msg	, $
		  maxvalue = adv_keyword_set(maxi) ? maxi[0]:maxvalue, bar_format=bar_format,lambert=lambert, $
  		  lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_box=no_box,position = position	,no_grid=no_grid,horizon=horizon

  	if keyword_set(shape_file) and limit_test eq 0 then limit = set_limits(lon_c,lat_c, p0lon=p0lon, p0lat=p0lat, /four_elements,bounds=2,verbose=verbose)

	if hct eq 'max' then begin
		if keyword_set(save_as) then save_as = save_dum+'_'+algon1+'_max_type.eps'
		start_save, save_as, thick = thick,size=[32,20]
			m = obj_new("map_image",cci,lat_c,lon_c,void_index=void_index1,box_axes=box_axes,n_lev=9, $
				magnify=magnify,  min=0,max=9,ctable = 13,discrete=findgen(10),$
				figure_title = keyword_set(notitle) ? '' : adc+' '+algon1+' most frequent '+apx+'Cloud Type'	, $
; 				charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
				charthick = !m_charthick, charsize  = !m_charsize,$
				title= 'Cloud type',$
				bar_tickname= [htypes], logarithmic=logarithmic,label=label,bar_horizontal=bar_horizontal, $
				format=bar_format,no_continents=no_continents,no_grid=no_grid,horizon=horizon,  $
				p0lon = p0lon, p0lat = p0lat,lambert=lambert	, no_color_bar=no_color_bar,$
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, hammer = hammer, goode = goode  , $
				globe = globe, limit = limit,countries=countries,usa=countries,debug=verbose)
			obj_destroy,m
		end_save, save_as
		if keyword_set(save_as) then save_as = save_dum+'_'+algon2+'_max_type.eps'
		start_save, save_as, thick = thick,size=[32,20]
			m = obj_new("map_image",gac,lat_g,lon_g,void_index=void_index2,box_axes=box_axes,n_lev=9, $
				magnify=magnify, min=0,max=9,ctable = 13,discrete=findgen(10),$
				figure_title = keyword_set(notitle) ? '' : adg+' '+algon2+' most frequent '+apx+'Cloud Type'	, $
; 				charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
				charthick = !m_charthick, charsize  = !m_charsize,$
				title= 'Cloud type',$
				bar_tickname= [htypes], logarithmic=logarithmic,label=label,bar_horizontal=bar_horizontal, $
				format=bar_format,no_continents=no_continents,no_grid=no_grid,horizon=horizon,  $
				p0lon = p0lon, p0lat = p0lat,lambert=lambert	,no_color_bar=no_color_bar, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, hammer = hammer, goode = goode  , $
				globe = globe, limit = limit,countries=countries,usa=countries,debug=verbose)
			if keyword_set(zoom) and not keyword_set(save_as) then begin
				m -> zoom,get_new_corners = zoom,/print_new, ztext = ztext,magnify=magnify
			endif
			obj_destroy, m
		end_save, save_as
	endif else begin
		if ~keyword_set(difference) then begin
			if keyword_set(save_as) then save_as = save_dum+'_'+algon1+'_Ratio_'+strcompress(ratio_cci,/rem)+'.eps'
			start_save, save_as, thick = thick
				m = obj_new("map_image",cci,lat_c,lon_c,void_index=void_index1,box_axes=box_axes,n_lev=5, $
					max=adv_keyword_set(maxi) ? maxi:maxvalue, min=adv_keyword_set(mini) ? mini:minvalue, magnify=magnify, $
					figure_title = keyword_set(notitle) ? '' : adc+' '+algon1+' '+lname, $
; 					charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
					charthick = !m_charthick, charsize  = !m_charsize,$
					title= keyword_set(bar_title) ? bar_title : 'Rel. Occ. '+(keyword_set(hct) ? 'of '+strupcase(hct)+' '+apx+'Clouds [%]' : ''),$
					limit = limit,countries=countries,usa=countries,no_continents=no_continents,no_grid=no_grid, $
					format=bar_format,  logarithmic=logarithmic,no_color_bar=no_color_bar,horizon=horizon, $
					bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow	, flip_colours = flip_colours,$
					p0lon = p0lon, p0lat = p0lat,lambert=lambert,label=label,bar_horizontal=bar_horizontal, panoply = panoply	, $
					lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,position = position, $
					mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, hammer = hammer, goode = goode  , $
					globe = globe,brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow,debug=verbose)
				obj_destroy,m
			end_save, save_as

			if keyword_set(save_as) then save_as = save_dum+'_'+algon2+'_Ratio_'+strcompress(ratio_gac,/rem)+'.eps'
			start_save, save_as, thick = thick
				m = obj_new("map_image",gac,lat_g,lon_g,void_index=void_index2,box_axes=box_axes,n_lev=5, $
					max=adv_keyword_set(maxi) ? maxi:maxvalue, min=adv_keyword_set(mini) ? mini:minvalue, magnify=magnify, $
					figure_title = keyword_set(notitle) ? '' : adg+' '+algon2+' '+lname,  $
; 					charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
					charthick = !m_charthick, charsize  = !m_charsize,$
					title= keyword_set(bar_title) ? bar_title :'Rel. Occ. '+(keyword_set(hct) ? 'of '+strupcase(hct)+' '+apx+'Clouds [%]' : ''),$
					format=bar_format,  logarithmic=logarithmic, no_color_bar=no_color_bar, $
					bwr = bwr, elevation = elevation, extended_rainbow = extended_rainbow	, flip_colours = flip_colours,$
					brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow ,label=label,bar_horizontal=bar_horizontal, $
					p0lon = p0lon, p0lat = p0lat,lambert=lambert, panoply = panoply,position = position,horizon=horizon, $
					lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
					mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, hammer = hammer, goode = goode  , $
					globe = globe, limit = limit,countries=countries,usa=countries,debug=verbose)
				obj_destroy,m
			end_save, save_as

			; 2d histogram
			if keyword_set(save_as) then save_as = save_dum+'_'+algon1+'_'+algon2+'_2dHist.eps'
			start_save, save_as, thick = thick;,size=[20,16]
				dum = h2d_cnt gt 0 ? hist_2d(cci[idx_h2d],gac[idx_h2d],bin1=1,bin2=1,min1=0,max1=100,min2=0,max2=100) : intarr(101,101)
				if h2d_cnt eq 0 then dum[0]=1
				view2d,dum,no_data_val=0,/log,xticks=5,xtickname=['0','20','40','60','80','100'],$
				yticks=5,ytickname=['0','20','40','60','80','100'],bar_format='(i)',bar_title='# of occur.',$
				xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize, $
				xtitle=algon1,ytitle=algon2,color= ~keyword_set(no_color_bar)
				regr = linfit(cci[idx_h2d],gac[idx_h2d], YFIT=yfit)
				oplot,!x.crange,[regr[1]*!x.crange+regr[0]],linestyle=2
				oplot,!x.crange,!y.crange
			end_save, save_as
		endif
		;difference
		if keyword_set(save_as) then save_as = save_dum+'_'+algon1+'-'+algon2+'_Diff.eps'
		start_save, save_as, thick = thick
			if vd1_cnt gt 0 and vd2_cnt gt 0 then void_index=[void_index1,void_index2]
			if vd1_cnt gt 0 and vd2_cnt eq 0 then void_index=[void_index1]
			if vd1_cnt eq 0 and vd2_cnt gt 0 then void_index=[void_index2]
			if vd1_cnt eq 0 and vd2_cnt eq 0 then void_index=-1
			m = obj_new("map_image",(cci-gac),lat_g,lon_g,void_index=void_index,box_axes=box_axes,n_lev=5, $
				max=(keyword_set(difference) ? maxi : 50), min=(keyword_set(difference) ? mini : -50), magnify=magnify, $
				figure_title = keyword_set(notitle) ? '' : 'Diff. '+algon1+' - '+algon2+' '+lname, $
; 				charthick = keyword_set(save_as) ? 2. : 1.5, charsize = (keyword_set(save_as) ? 3. : 1.5), $
				charthick = !m_charthick, charsize  = !m_charsize , $
				title= keyword_set(bar_title) ? bar_title:'Rel. Occ. '+(keyword_set(hct) ? 'of '+strupcase(hct)+' '+apx+'Clouds [%]' : ''),$
				format='(f6.2)',  logarithmic=logarithmic, no_color_bar=no_color_bar,  $
				bwr = (keyword_set(difference) ? bwr:1 ), elevation = elevation, extended_rainbow = extended_rainbow, flip_colours = flip_colours,$
				brewer = brewer, greyscale = greyscale,ctable=ctable,rainbow = rainbow ,label=label,bar_horizontal=bar_horizontal, $
				p0lon = p0lon, p0lat = p0lat,lambert=lambert, panoply = panoply	,position = position,horizon=horizon, $
				lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,no_continents=no_continents,no_grid=no_grid, $
				mollweide = mollweide, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, hammer = hammer, goode = goode  , $
				globe = globe, limit = limit,countries=countries,usa=countries,debug=verbose)
			if keyword_set(zoom) and not keyword_set(save_as) then begin
				m -> zoom,get_new_corners = zoom,/print_new, ztext = ztext,magnify=magnify
			endif
			obj_destroy,m
		end_save, save_as
	endelse

	if keyword_set(zoom) then begin
		vergleiche_ctp_cot_histogram_cci_mit_clara, ccifile,  mini = mini, maxi = maxi, limit=zoom,algo1 = algo1, sat=sat,$
			win_nr = win_nr,save_as= save_as,land = land, sea = sea,hist_cloud_type = hist_cloud_type, hist_phase=hist_phase, $
			reference = reference,timeseries=timeseries,ctable=ctable,other=other, difference = difference,varname=varname
	endif

	if keyword_set(show_values) and keyword_set(difference) and ~total(hct eq ['hist2d','hist_2d','max']) then $
	out = {bild:(cci-gac),lon:lon_g,lat:lat_g,unit:'[%]',fillvalue:fillvalue1,longname:lname}

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro plot_isccp_uebersicht, short=short, save_as = save_as, win_nr = win_nr

	win_nr = keyword_set(win_nr) ? win_nr : 14
	if keyword_set(save_as) then save_as = !save_dir + 'isccp_uebersicht.eps' else if win_nr ne -1 then win,win_nr,xs=460,ys=550,title='ISCCP 2D Histograms' 
	start_save, save_as, size = [10,12] , thick = thick
	plot,[0,0],[1,1],xr=[0,6],yr=[0,7],/xs,/ys,xtickname = ['0.3','1.3','3.6','9.4','23','60','100'],$ 
	xticks=6,ytickname=['1100','800','680','560','440','310','180','10'],yticks=7,$
	xtitle='Cloud Optical Thickness',ytitle='Cloud Top Pressure'
	oplot,!x.crange,[2,2]
	oplot,!x.crange,[4,4]
	oplot,[2,2],!y.crange
	oplot,[4,4],!y.crange
	if keyword_set(short) then begin
		xyouts,0.75,0.9,'Cu'
		xyouts,2.75,0.9,'Sc'
		xyouts,4.75,0.9,'St'
		xyouts,0.75,2.9,'Ac'
		xyouts,2.75,2.9,'As'
		xyouts,4.75,2.9,'Ns'
		xyouts,0.75,5.4,'Ci'
		xyouts,2.75,5.4,'Cs'
		xyouts,4.75,5.4,'Cb'
	endif else begin
		xyouts,0.5,0.9,'Cumulus (Cu)'
		xyouts,2.05,0.9,'Stratocumulus (Sc)'
		xyouts,4.5,0.9,'Stratus (St)'
		xyouts,0.2 ,2.9,'Altocumulus (Ac)'
		xyouts,2.3,2.9,'Altostratus (As)'
		xyouts,4.05 ,2.9,'Nimbostratus (Ns)'
		xyouts,0.5 ,5.4,'Cirrus (Ci)'
		xyouts,2.2,5.4,'Cirrostratus (Cs)'
		xyouts,4.6 ,5.5,'Deep'
		xyouts,4.2 ,5.3,'Convection (Cb)'
	endelse
	end_save, save_as
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro make_2d_overview,year=year,month=month,satellite,reference=reference, coverage = coverage, algo = algo, out = out,addtext=addtext, $
		time_series = time_series,sat2=sat2, save_as = save_as,mini=mini,maxi=maxi,verbose=verbose,file1=file1,file2=file2,$
		datum1=datum1,datum2=datum2,notitle=notitle,version1=version1,version2=version2

	sat1 = keyword_set(satellite) ? strlowcase(satellite) : ''
	ref  = keyword_set(reference) ? strlowcase(reference) : ''
	cov  = keyword_set(coverage)  ? strlowcase(coverage)  : ''
	ts   = keyword_set(time_series)
	if ~keyword_set(ref) then begin
		ok=dialog_message('Choose reference data!')
		return
	endif
	make_geo, lon,lat, grid=1
; 	dem  = get_dem(lon,lat,grid=1)
	dem  = get_coverage(lon,lat,coverage = coverage)
	dem = [[dem,dem,dem],[dem,dem,dem],[dem,dem,dem]]

	if keyword_set(file2) then begin
		if keyword_set(addtext) then begin
			at = strsplit(addtext,',',/ext)
			if n_elements(at) eq 2 then begin
				f1str  = 'F1 ('+at[0]+'): '
				f2str  = 'F2 ('+at[1]+'): '
			endif else begin
				f1str  = 'F1 ('+addtext[0]+'): '
				f2str  = 'F2 ('+addtext[0]+'): '
			endelse
			no_time_string = 1
		endif else begin
			f1str  = 'File1: '
			f2str  = 'File2: '
		endelse
		annex  = ''
		is_new = 0.
		if strlowcase(sat2) eq sat1 and sat1 ne '' then begin
			; wann wurde das letzte mal in datei geschrieben?
			mtime1 = (file_info(file1)).mtime
			mtime2 = (file_info(file2)).mtime
			if mtime1 gt mtime2 then begin
				if keyword_set(verbose) then print,'File1 with modification time: '+unix2ymdhms(mtime1)+' is newer than File2: '+unix2ymdhms(mtime2)
				if ~keyword_set(no_time_string) then f1str  = 'F1 (new): '
				if ~keyword_set(no_time_string) then f2str  = 'F2 (old): '
				is_new = 1.
			endif else if mtime2 gt mtime1 then begin
				if keyword_set(verbose) then print,'File2 with modification time: '+unix2ymdhms(mtime2)+' is newer than File1: '+unix2ymdhms(mtime1)
				if ~keyword_set(no_time_string) then f1str  = 'F1 (old): '
				if ~keyword_set(no_time_string) then f2str  = 'F2 (new): '
				is_new = 2.
			endif else if keyword_set(verbose) then print,'modification time of File1: '+unix2ymdhms(mtime1)+' and File2: '+unix2ymdhms(mtime2)+' are equal!'
		endif
	endif else begin
		f1str  = ''
		f2str  = ''
	endelse

	case strmid(ref,0,3) of
		'gac'	: sat2 = (total(sat1 eq ['msg','aqua','terra','modises','allsat','avhrrs']) ? 'allsat' : sat1)
		'myd'	: sat2 = 'aqua'
		'mod'	: sat2 = 'terra'
		'cla'	: sat2 = ''
		'cci'	: sat2 = (total(sat1 eq ['msg','allsat']) ? 'allsat' : sat1)
		'pmx'	: sat2 = noaa_primes(year,month,ampm=sat_ampm(sat1,/ampm),/patmos)
		'gwx'	: sat2 = 'noaa18'
		'cal'	: sat2 = 'calipso'
		'isp'	: sat2 = 'noaa18' ; ??
		'era'	: sat2 = ''
		else	: sat2 = sat1
	endcase

	algo1   = algo2ref(algo,sat=sat1)
	algon1  = sat_name(algo,sat1,year=(ts ? 0:year),month=(ts ? 0:month),level='l3c',version=version1)
	algo2   = ref
	algon2  = sat_name(ref,sat2,year=(ts ? 0:year),month=(ts ? 0:month),level='l3c',version=version2)
	dummy   = fltarr(360,180) -999.

	if ts then begin
		dum = restore_var(!SAVS_DIR + 'time_series/overview_matrix/two_dim_overview_matrix_200701-200912_'+algo1+'_'+sat1+'.sav',found=found)
		if found then gesamt_cci = dum.mean else begin
			ok = dialog_message('make_2d_overview: Time Series Sav not found for '+algo1+' '+sat1)
			return
		endelse
		dum = restore_var(!SAVS_DIR + 'time_series/overview_matrix/two_dim_overview_matrix_200701-200912_'+algo2+'_'+sat2+'.sav')
		if found then gesamt_ref = dum.mean else begin
			ok = dialog_message('make_2d_overview: Time Series Sav not found for '+algo2+' '+sat2)
			return
		endelse
	endif else begin
		; read in cci parameter
		ctt     = get_data(year,month,data='ctt',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then ctt = dummy
		ctp     = get_data(year,month,data='ctp',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then ctp = dummy
		cfc     = get_data(year,month,data='cfc',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then cfc = dummy
		cot     = get_data(year,month,data='cot',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then cot = dummy
		cph     = get_data(year,month,data='cph',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then cph = dummy
		ref     = get_data(year,month,data='cer',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then ref = dummy
		lwp     = get_data(year,month,data='lwp',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then lwp = dummy
		iwp     = get_data(year,month,data='iwp',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then iwp = dummy
		cwp     = get_data(year,month,data='cwp',algo=algo1,sat=sat1,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found = found)
		if ~found then cwp = dummy
		ctt_ref = get_data(year,month,data='ctt',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then ctt_ref = dummy
		ctp_ref = get_data(year,month,data='ctp',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then ctp_ref = dummy
		cfc_ref = get_data(year,month,data='cfc',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then cfc_ref = dummy
		cot_ref = get_data(year,month,data='cot',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then cot_ref = dummy
		cph_ref = get_data(year,month,data='cph',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then cph_ref = dummy
		ref_ref = get_data(year,month,data='cer',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then ref_ref = dummy
		lwp_ref = get_data(year,month,data='lwp',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then lwp_ref = dummy
		iwp_ref = get_data(year,month,data='iwp',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then iwp_ref = dummy
		cwp_ref = get_data(year,month,data='cwp',algo=algo2,sat=sat2,level='l3c',/glob,no_data_value=no_data_value,/mean,/make_comp,/sil,found=found)
		if ~found then cwp_ref = dummy

		min_ctt = (minmax([ctt,ctt_ref],no=-999))[0]
		min_cot = (minmax([cot,cot_ref],no=-999))[0]
		min_lwp = (minmax([lwp,lwp_ref],no=-999))[0]
		min_iwp = (minmax([iwp,iwp_ref],no=-999))[0]
		min_ref = (minmax([ref,ref_ref],no=-999))[0]
		min_ctp = (minmax([ctp,ctp_ref],no=-999))[0]
		min_cwp = (minmax([cwp,cwp_ref],no=-999))[0]
		min_cph = (minmax([cph,cph_ref],no=-999))[0]
		min_cfc = (minmax([cfc,cfc_ref],no=-999))[0]

		gesamt_cci = [  [temporary(lwp)    - min_lwp,temporary(iwp)    - min_iwp,temporary(cwp)    - min_cwp],$
				[temporary(cot)    - min_cot,temporary(ref)    - min_ref,temporary(cph)    - min_cph],$
				[temporary(ctt)    - min_ctt,temporary(ctp)    - min_ctp,temporary(cfc)    - min_cfc] ]
		gesamt_ref = [  [temporary(lwp_ref)- min_lwp,temporary(iwp_ref)- min_iwp,temporary(cwp_ref)- min_cwp],$
				[temporary(cot_ref)- min_cot,temporary(ref_ref)- min_ref,temporary(cph_ref)- min_cph],$
				[temporary(ctt_ref)- min_ctt,temporary(ctp_ref)- min_ctp,temporary(cfc_ref)- min_cfc] ]

	endelse
	; was ist wenn einer von beiden null ist (auch raus!!)
	gesamt_ref[where(gesamt_ref le 0)] = -999
	gesamt_cci[where(gesamt_cci le 0)] = -999
	if keyword_set(save_as) then save_as = !SAVE_DIR + '2d_overview/2d_overview_'+(keyword_set(datum1)?datum1+'_':'')+algon1+'-'+(keyword_set(datum2)?datum2+'_':'')+algon2+'.eps'
	start_save,save_as,size=size
; 		out = gesamt_cci/gesamt_ref
; 		out = (gesamt_cci-gesamt_ref)/(gesamt_cci+gesamt_ref) 	; werte maximal 1 : Ref = 0 und CCI != 0, minimal -1 : CCI = 0 und Ref !=0, 0 ist perfekt.
		out = 100*((gesamt_cci-gesamt_ref)/(gesamt_ref))		; relative differenz in %
		view2d, out,min=adv_keyword_set(mini) ? float(mini[0]):-100.,max=adv_keyword_set(maxi)? float(maxi[0]) : 100.,$
		col_tab=4,no_data_idx=where(gesamt_ref le 0. or gesamt_cci le 0. or dem ne 1),xmargin=[8,9],ymargin=[3,3],$;xmargin=[8,8]$
		xticks=6,xtickname=[' ','LWP',' ','IWP', ' ','CWP',' '],yticks=6,ytickname=[' ','LWP',' ','COT', ' ','CTT',' '],$
		xcharsize = !v_xcharsize, ycharsize = !v_ycharsize, charthick = !v_charthick, charsize = !v_charsize, $
		bar_title='Rel. Difference [%] '+f1str+algon1+' - '+f2str+algon2,bar_nlev=6;,charsize=1.5
		axis,xaxis=1,xticks=6,xtickname=[' ','CTT',' ','CTP', ' ','CFC',' '],charsize=1.5
		axis,yaxis=1,yticks=6,ytickname=[' ','CWP',' ','CPH', ' ','CFC',' '],charsize=1.5
	end_save,save_as

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro moviemaker, year, month, day, sat = sat, data = data, mini = mini, maxi = maxi, $
		land = land, sea = sea,	hist_cloud_type = hist_cloud_type, algo = algo, $
		level = level, timeseries = timeseries

	tmp_dir = '/cmsaf/cmsaf-cld1/sstapelb/bilder/neu/movies/tmp/'
	datum   = (keyword_set(year) ? string(year,f='(i4.4)') : '200701-200912') + $
		  (keyword_set(month) ? string(month,f='(i2.2)') : '')+ $
		  (keyword_set(day) ? string(day,f='(i2.2)') : '')

	p0lon   = [vector(0,360,25),intarr(26)]
	p0lat   = [intarr(25),vector(0,90,7),vector(75,-90,12),vector(-75,0,6),0]
	dat     = keyword_set(data) ? strupcase(data) : 'CTP'

	for ii = 0, n_elements(p0lon) -1 do begin
		plot_l2, year, 	month, day ,sat = sat, data = data, mini = mini, maxi = maxi, $
				land = land, sea = sea,/globe, algo = algo, $
				hist_cloud_type = hist_cloud_type, level = level, $
				timeseries = timeseries, p0lon=p0lon[ii],p0lat=p0lat[ii], $
				save_as = tmp_dir+datum+'_'+dat+'_'+string(ii,f='(i2.2)')+'.png'
	endfor

	; create animated gif
	spawn, "convert -delay 45 -loop 0 "+tmp_dir+datum+"_"+dat+"_??.png "+tmp_dir+"../"+datum+"_"+dat+"_animated.gif"
	file_delete, file_search(tmp_dir+datum+'_'+dat+'*.png'), /allow_nonexistent
	print, "Animated Gif created!"
	print, "Play with: "
	print, "mplayer -loop 0 -fixed-vo "+tmp_dir+"../"+datum+"_"+dat+"_animated.gif"

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro plot_hovmoeller, data, algo, satellite, save_as = save_as, mini = mini, maxi = maxi, win_nr = win_nr,notitle=notitle,$
		     ctable = ctable, other = other, reference = reference, out = out, land = land, sea = sea,$
		     oplots = oplots, found = found, nobar = nobar, limit = limit, antarctic=antarctic, arctic=arctic, $
		     coverage=coverage,version=version,version2=version2,ts_extras = ts_extras

; 	vali_set_path
	opl   = keyword_set(oplots)
	datum = !DEFAULT_TS_PERIOD
	dat   = strlowcase(data[0])
	if keyword_set(antarctic) then limit = [-90.,-180.,-60.,180.] 
	if keyword_set(arctic)    then limit = [ 60.,-180., 90.,180.] 

	algo  = keyword_set(algo) ? strlowcase(algo) : ''
	sat   = keyword_set(satellite) ? strlowcase(strjoin(strsplit(satellite,'-',/ext))) : ''
	algon = sat_name(algo,opl eq 0 ? sat:'',version=version)
	ref   = keyword_set(reference) ? algo2ref(reference) : '' ; must be of type 'gac','myd','mod',etc
	if keyword_set(ref) then ref_name = sat_name(ref,opl eq 0 ? sat:'',version=version2)
	no_data_val = -999.

	struc_idx = 0
	if keyword_set(land) then struc_idx = 1
	if keyword_set(sea)  then struc_idx = 2
	if keyword_set(land) and keyword_set(sea) then begin
		struc_idx = 0
		land = 0
		sea  = 0
	endif
	set_colors,other=other,ctable=ctable,brewer=brewer,col_tab=col_tab, panoply = panoply

	varn  = strsplit(dat,',',/ext)
	
	same_var = n_elements(varn) eq 1
	if same_var then varn = [varn,varn]
	
	vollername = full_varname(varn[0],/universal)

	d = get_available_time_series( algo, varn[0], sat, period = datum, /hovmoeller, found = found, anomalies = anomalies,ts_extras = ts_extras)

	if ~float(found) then begin
		ok = dialog_message('plot_hovmoeller: File not found for '+algon+' '+varn[0])
		if is_defined(out) then out = {bild:out.bild,sm:out.sm}
		return
	endif

	if anomalies eq 1 and keyword_set(reference) then begin
		print,'Plot_hovmoeller: Plotting of Anomalies while comparing at the moment not possible.'
		anomalies = 0
	endif

	matrix = transpose(d.(struc_idx))
	longname = d.longname

	mima = minmax(matrix,no_data_value=-999)
	if keyword_set(mini) then mima[0] = mini[0]
	if keyword_set(maxi) then mima[1] = maxi[0]
	mima = float(mima)
	dist = mima[1]/20.
	print,'Dist  : ', string(dist)
	nlev = 16 > fix(((mima[1]-mima[0])/dist)+1) < 30
	nbar = 8 > nlev < 11

	if is_h1d(varn[0]) then begin
		set_coverage, dum_cov
		bin_borders = (d.bin_borders) 
		form = stregex(varn[0],'cla_vis',/fold,/bool) ?  '(f20.2)' : '(f20.1)'
		idx = where(bin_borders ge 10000.,idxcnt)
		bin_borders = strcompress(string(bin_borders,f=form),/rem)
		if idxcnt gt 0 then bin_borders[idx] = strcompress(string(bin_borders[idx],f='(G20.1)'),/rem)
		dum_cov_idx = where(coverage[0] eq dum_cov,dum_cov_cnt)
		dum_cov_idx = dum_cov_cnt gt 0 ? (dum_cov_idx mod 9) : 0
		matrix = reform(matrix[dum_cov_idx,*,*])
		mat_sm = matrix * 0
	endif else mat_sm = transpose(d.SEASONAL_MEAN.(struc_idx))
	datum  = d.period
	unit   = d.unit
	ori_period = fix(strsplit(d.period,'-',/ext))

	if keyword_set(ref) then begin
		d = get_available_time_series( ref, varn[1], sat, period = datum, /hovmoeller, anomalies = anomalies, found = found)
		if ~float(found) then begin
			ok = dialog_message('plot_hovmoeller: Reference File not found for '+ref+' '+sat+' '+varn[1])
			return
		endif
		mat_ref = transpose(d.(struc_idx))
		if is_h1d(varn[1]) then mat_ref = reform(mat_ref[dum_cov_idx,*,*])
		idx     = where(matrix eq no_data_val[0] or mat_ref eq no_data_val[0],icnt)
		matrix  = matrix - mat_ref
		; contour blacks out all values lower or greater than min / max values
		; set values lower/greater to min/max to avoid this
		matrix    = mima[0] > matrix < mima[1]
		same_algo = strmatch(algon,ref_name)
		if same_var eq 0 then longname = longname+ ' - '+d.longname
	endif else idx = where(matrix eq no_data_val[0],icnt)
	if icnt gt 0 then matrix[idx]=no_data_val

	if total(matrix ne no_data_val[0]) eq 0 then print,'   !!! NO valid data found !!!'
	;-----------
	dum        = fix(strsplit(datum,'-',/ext))
	if keyword_set(mini) and keyword_set(maxi) then begin
		mini = float(strsplit(mini[0],',',/ext))
		maxi = float(strsplit(maxi[0],',',/ext))
		if n_elements(maxi) gt 1 then dum[1] = (1970 > (fix(maxi[1])-1) < 2050 )
		if n_elements(mini) gt 1 then dum[0] = (2050 < (fix(mini[1])-0) > 1970 )
		if dum[1] lt dum[0] then dum[1] = ori_period[1]
		if dum[0] gt dum[1] then dum[0] = ori_period[0]
	endif

	jumps=0
	nochmal:
	num_of_yy = ((dum[1]+1)-dum[0])
	if num_of_yy lt 10 then begin
		divi_yy = 1
	endif else if num_of_yy/3 eq num_of_yy/3. then begin
		divi_yy = 3
	endif else if num_of_yy/2 eq num_of_yy/2. then begin
		divi_yy = 2
		if num_of_yy gt 20 then begin
			if (num_of_yy/4 eq num_of_yy/4.) then divi_yy = 4 $
			else begin
				dum[1]++
				jumps++
				if jumps lt 5 then goto, nochmal
			endelse
		endif
	endif else begin
		dum[1]++
		jumps++
		if jumps lt 5 then goto, nochmal
		divi_yy = 2
	endelse

	xtickname = strcompress(vector(dum[0],dum[1]+1,(2>(num_of_yy/divi_yy+1))),/rem)
	if dum[0] lt ori_period[0] then begin
		cnts = (ori_period[0] - dum[0]) *12
		mat_dummy = fltarr(cnts,(size(matrix,/dim))[1]) + no_data_val
		matrix  = [mat_dummy,matrix]
		mat_sm  = [mat_dummy,mat_sm]
		ori_period[0] = dum[0]
	endif
	anf = ((dum[0]-ori_period[0])*12)
	eef = ((dum[1]-ori_period[0]+1)*12);-1)
	if eef gt (size(matrix,/dim))[0] then begin
		cnts = eef-(size(matrix,/dim))[0]+1
		mat_dummy = fltarr(cnts,(size(matrix,/dim))[1]) + no_data_val
		matrix  = [matrix,mat_dummy]
		mat_sm  = [mat_sm,mat_dummy]
	endif

	matrix = matrix[anf:eef<((size(matrix,/dim))[0]-1),*]
	mat_sm = mat_sm[anf:eef<((size(mat_sm,/dim))[0]-1),*]

	;-----------
	if keyword_set(save_as) then begin
		save_as = !SAVE_DIR +'hovmoeller/'+(keyword_set(ref) ? 'Diff_'+algon+'_-_'+ref_name:algon)+'_'+dat+'_hovmoeller_L3C_'$
				    +strjoin(minmax(xtickname),'-')+(keyword_set(land)?'_land':'')+(keyword_set(sea)?'_sea':'')$
				    +(anomalies?'_anomalies':'')+'.eps'
		!p.multi=0
	endif else if win_nr ne -1 then win, win_nr, size=700,ysize=1200
	;-----------

	if opl gt 0 and keyword_set(out) then begin
		; make sure both array have same size
		osm = out.sm
		out = out.bild
		if total(size(out,/dim)) eq total(size(matrix,/dim)) and $
		product(size(out,/dim)) eq product(size(matrix,/dim)) then begin
			dum = [[[matrix]],[[out]]]
			idx = where(dum eq no_data_val[0],idx_cnt)
			if idx_cnt gt 0 then dum[idx] = !values.f_nan
			matrix = mean(dum,dim=3,/nan)
			idx = where(~finite(matrix),idx_cnt)
			if idx_cnt gt 0 then matrix[idx] = no_data_val[0]
			dum = [[[mat_sm]],[[osm]]]
			idx = where(dum eq no_data_val[0],idx_cnt)
			if idx_cnt gt 0 then dum[idx] = !values.f_nan
			mat_sm = mean(dum,dim=3,/nan)
			idx = where(~finite(mat_sm),idx_cnt)
			if idx_cnt gt 0 then mat_sm[idx] = no_data_val[0]
		endif
	endif

	out = {bild:matrix,sm:mat_sm,unit:unit}
	if keyword_set(anomalies) then begin
		idx = where(matrix eq no_data_val[0] or mat_sm eq no_data_val[0],icnt)
		matrix = matrix-mat_sm
		if icnt gt 0 then matrix[idx]=no_data_val[0]
	endif
	si  = size(matrix,/dim)

	print,'Nlevel: ', string(nlev)
	print,'Minmax: ', string(minmax(matrix,no=no_data_val,/nan))

	ytickname = is_h1d(varn[0]) ? bin_borders : ['-90','-60','-30','0','30','60','90']
	title     = keyword_set(ref) ? 'Difference '+(same_algo ? algon : algon+' - '+ref_name)+' '+longname : algon+' '+longname

	if keyword_set(land) then title=title+' Land'
	if keyword_set(sea)  then title=title+' Sea'

	if si[0] le 13 then begin
		xtickname = [' ','Jan',' ','Feb',' ','Mar',' ','Apr',' ','May',' ','Jun',' ','Jul',' ','Aug',' ','Sep',' ','Oct',' ','Nov',' ','Dec',' ']
		title     = strcompress(dum[0],/rem)+'  '+title
	endif

	if keyword_set(notitle) then free, title

	form_len = strlen(strcompress(floor(float(mima[1])),/rem))
	plus = form_len eq 1 ? 1 : 0
	bar_format = '(f'+strcompress(form_len+3+plus,/rem)+'.'+string(1+plus,f='(i1)')+')'

	if keyword_set(limit) and ~is_h1d(varn[0]) then begin
		dum_lat   = vector(-89.5,89.5,180.)
		rnd_limit = rnd(limit,30,/out)
		if rnd_limit[0] eq rnd_limit[2] then begin
			ok = dialog_message('Min and Max Latitude are equal! Set proper Limits!')
			return
		endif
		matrix    = matrix[*,where(between(dum_lat,rnd_limit[0],rnd_limit[2]))]
		si = size(matrix,/dim)
		ytickname = ytickname[where(between(ytickname,rnd_limit[0],rnd_limit[2]))]
		if n_elements(ytickname) le 3 then begin
			ytickname = strcompress(string(vector(min(ytickname),max(ytickname),(n_elements(ytickname) eq 3 ? 5 : 4)),f='(i3)'),/rem)
		endif
		if rnd_limit[0] ne limit[0] or rnd_limit[2] ne limit[2] then begin
			if rnd_limit[0] lt limit[0] then matrix[*,0:abs(limit[0]-rnd_limit[0])-1.] = no_data_val
			if rnd_limit[2] gt limit[2] then matrix[*,(si[1]-(rnd_limit[2]-limit[2])):*] = no_data_val
		endif
	endif

	start_save, save_as

	make_cool_contour,matrix,findgen(si[0]),findgen(si[1]),nlev,lines = keyword_set(nobar), charsize=2,xmargin=[10,4],ymargin=[5,2], $
		title=title,bar_title=longname + unit,no_data_val=no_data_val[0],/cc4cl_hovmoeller,nbar=nbar,/contin_bar, $
		ytickname=ytickname,yticks=n_elements(ytickname)-1,ytitle=is_h1d(varn[0]) ? 'Bin Borders':'Latitude',c_charsize = 2., $
		format = bar_format,xtickname=xtickname,xticks=n_elements(xtickname)-1,min=mima[0],max=mima[1],color=(keyword_set(nobar) ? 0.:2),$
		col_table=col_tab,brewer =brewer,xticklen=0.00001;,xminor=2,col_tab=105,color=(i/7 eq i/7. ? 1:0 )
		if si[0] le 13 then axis,xaxis=0,xtickname=replicate(' ',si[0]),xticks=si[0]-1,xminor=2
	end_save, save_as
end
;-------------------------------------------------------------------------------------------------------------------------
pro plot_histogram,year,month,day,file,varname,mini=mini,maxi=maxi,limit=limit,sea=sea,land=land, save_as=save_as,$
		win_nr = win_nr,timeseries=timeseries, algo=algo, sat = sat, datum=datum, level=level,$
		reference=reference,change_side = change_side,verbose=verbose,coverage=coverage,oplots=oplots	,$
		addtext = addtext, found=found, white_bg = white_bg,logarithmic=logarithmic,notitle=notitle

	opl   = keyword_set(oplots)    ? fix(oplots)           : 0
	ref   = keyword_set(reference) ? strlowcase(reference) : '' 
	alg   = keyword_set(algo)      ? strlowcase(algo)      : 'esacci' 
	lev   = keyword_set(level)     ? strlowcase(level)     : 'l3c' 
	hct   = keyword_set(addtext)   ? ' - '+strupcase(addtext[0]) : ''
	date1 = keyword_set(datum)     ? datum+' ' : ''
	sav   = keyword_set(save_as)
	wbg   = keyword_set(white_bg)
	ts    = keyword_set(timeseries)

	varn  = strsplit(varname,',',/ext)
	if n_elements(varn) eq 1 then varn = [varn,varn]
	vollername = full_varname(varn[0],/universal)

	algon1= sat_name(algo,sat,year=(ts ? 0:year),month=(ts ? 0:month),level=lev)
	if keyword_set(ref) then algon2 = sat_name(reference,sat,year=(ts ? 0:year),month=(ts ? 0:month),level=lev)

	if ts then begin
		if keyword_set(limit) and ~keyword_set(coverage) then begin
			ok = dialog_message('Limit not possible choose pre defined areas!')
			return
		endif
		datum = !DEFAULT_TS_PERIOD ; use this as first guess

		if ~strmatch(varn[0],varn[1]) then begin
			d = get_available_time_series( 	alg, varn[0], sat, coverage = coverage, period = datum, $
							sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, $
							anomalies = anomalies, no_trend_found = no_trend_found)
			if not found then begin
				if ~no_trend_found then ok = dialog_message("plot_histogram: Sav File not found! "+sav_file)
				return
			endif
			hh   = d.HISTOGRAM.data
			bin  = d.HISTOGRAM.bin
			minv = d.HISTOGRAM.minvalue
			maxv = d.HISTOGRAM.maxvalue
			d = get_available_time_series( 	reference, varn[1], sat, coverage = coverage, period = datum, $
							sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, $
							anomalies = anomalies, no_trend_found = no_trend_found)
			if not found then begin
				if ~no_trend_found then ok = dialog_message("plot_histogram: Sav File not found! "+sav_file)
				return
			endif
			hh2  = d.HISTOGRAM.data
			minv = min([minv,d.HISTOGRAM.minvalue])
			maxv = max([maxv,d.HISTOGRAM.maxvalue])
			xx   = findgen(n_elements(hh)) * bin + minv[0]
		endif else begin
			d = get_available_time_series( 	alg, varname, sat, coverage = coverage, reference = reference, period = datum, $
							sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, $
							anomalies = anomalies, no_trend_found = no_trend_found)
			if not found then begin
				if ~no_trend_found then ok = dialog_message("plot_histogram: Sav File not found! "+sav_file)
				return
			endif
			hh   = d.HISTOGRAM.data
			bin  = d.HISTOGRAM.bin
			minv = d.HISTOGRAM.minvalue
			maxv = d.HISTOGRAM.maxvalue
			xx   = findgen(n_elements(hh)) * bin + minv[0]
			if keyword_set(reference) then hh2 = d.HISTOGRAM2.data
		endelse
		unit  = d.unit
		date1 = d.actual_date
		date2 = d.actual_date
	endif else begin
		if lev eq 'l3u' and ( ref eq 'gac' and (alg eq 'esacci' or alg eq 'patmos')) then join_nodes = 1
		bild = 	get_data(year,month,day,file=file,data=varn[0],sat=sat,found=found,algo=alg,verbose=verbose,/print_filename,$
			longname=longname,unit=unit,no_data_value=fillvalue1, level=lev,join_nodes=join_nodes,dim3=dim3,/make_compareable)
		free,join_nodes
		if ~found then begin
			ok= dialog_message('plot_histogramm: '+(file_test(file) ? 'Data '+varn[0]+' not found!': 'File not found!'))
			return
		endif
		if keyword_set(reference) then begin
			if lev eq 'l3u' and ( alg eq 'clara' and (ref eq 'cci' or ref eq 'pmx')) then join_nodes = 1
			bild2 = get_data(strmid(date1,0,4),strmid(date1,4,2),strmid(date1,6,2),algo=ref,data=varn[1],found=found,print_filename=2,$
				sat=sat,verbose=verbose,level=lev,join_nodes=join_nodes,/make_compareable,dim3=dim3,no_data_value=fillvalue2)
			free,join_nodes
			if ~found then begin
				ok= dialog_message('plot_histogramm: Data '+varn[1]+' not found in Reference '+reference+' File! ')
				return
			endif
		endif
		if keyword_set(limit) or keyword_set(land) or keyword_set(sea) then begin
			make_geo,grid_res = get_grid_res(bild),lon,lat
			area  = get_coverage( lon, lat, coverage = coverage, limit = limit, found = found)
			bild  = (area * bild + (area eq 0) * fillvalue1)
			if keyword_set(bild2) then begin
				make_geo,grid_res = get_grid_res(bild2),lon,lat
				area2 = get_coverage( lon, lat, coverage = coverage, limit = limit, found = found)
				bild2 = (area2 * bild2 + (area2 eq 0) * fillvalue2)
			endif
		endif
		date2 = date1
	endelse

	win_nr   = adv_keyword_set(win_nr) ? win_nr : 1
	if keyword_set(save_as) then begin
		save_as = !save_dir +(ts ? 'TS_'+strcompress(date1,/rem) : file_basename(file[0],is_hdf(file[0]) ? '.hdf':'.nc'))+'_histogram_'+varname+ $
			(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')+ $
			(keyword_set(limit) ? '_limit_'+strjoin(strcompress(limit,/rem),'_'):'')+$
			(keyword_set(oplots) ? '_oplots':'')+'.eps'
	endif else if win_nr ne -1 then win, win_nr,title=dat

	start_save,save_as,thick=thick,snapshot=(float(opl)?'png':'')
		if ~ts then begin
			minv = keyword_set(mini) ? strsplit(mini[0],',',/ext) : min(bild[where(bild ne fillvalue1)])
			maxv = keyword_set(maxi) ? strsplit(maxi[0],',',/ext) : max(bild[where(bild ne fillvalue1)])
			bin  = 1.
			bin  = (maxv[0] le 0.5 ? 0.001 : (maxv[0] le 1 ? 0.01 : (maxv[0] lt 10 ? 0.1 : 1)))
			hh   = histogram(bild,bin=bin,min=minv[0],max=maxv[0])
			if total(hh) eq 0 then begin
				ok=dialog_message('Total histogram is zero. Reduce MIN/Max Value. Note, the unit of some Products might be altered to work with oplot.')
				return
			endif
			xx   = findgen(n_elements(hh)) * bin + minv[0]
			if is_defined(bild2) then begin
				hh2    = histogram(bild2,bin=bin,min=minv[0],max=maxv[0])
				if total(hh2) eq 0 then begin
					ok=dialog_message('Total histogram is zero. Reduce MIN/Max Value. Note, the unit of some Products might be altered to work with oplot.')
					return
				endif
			endif
		endif else begin
			minv = keyword_set(mini) ? strsplit(mini[0],',',/ext) : minv
			maxv = keyword_set(maxi) ? strsplit(maxi[0],',',/ext) : maxv
		endelse

		if is_defined(hh2) then begin
			yrange = [(n_elements(minv) gt 1 ? minv[1]:0),(n_elements(maxv) gt 1 ? maxv[1]: 1.1*max([hh/total(hh),hh2/total(hh2)]) *100.)]
		endif else begin
			yrange = [(n_elements(minv) gt 1 ? minv[1]:0),(n_elements(maxv) gt 1 ? maxv[1]: 1.1*max(hh/total(hh)) *100.)]
		endelse

		if sav then begin
			charthick = !p_charthick ;1.5
			xcharsize = !p_xcharsize ;1.7 
			ycharsize = !p_ycharsize ;1.7
			lcharsize = !l_charsize  ;2.5
			xmargin   = [14,6]
			ymargin   = [ 7,3]
		endif else if wbg then begin
			charthick = !p_charthick ;3.0
			xcharsize = !p_xcharsize ;2.5
			ycharsize = !p_ycharsize ;2.5
			lcharsize = !l_charsize  ;3.0
			xmargin   = [20,6]
			ymargin   =  [8,3]
			thick     = 4
			symsize   = 1.5
		endif else begin
			charthick = !p_charthick ;1.2
			xcharsize = !p_xcharsize ;1.2 
			ycharsize = !p_ycharsize ;1.2
			lcharsize = !l_charsize  ;1.5
			xmargin   =[10,3]
			ymargin   = [5,2]
		endelse

		dtn = [appendix(varn[0]),appendix(varn[1])]
		sn_cov = short_cov_name(coverage)
		if keyword_set(sn_cov) then dtn += ' ('+ sn_cov+')'
		if opl eq 0 then begin
			plot,xx,hh/total(hh)*100.,xtitle=vollername+' '+unit,title=keyword_set(notitle) ? '' : (ts ? date1:''),$
			xr=[minv[0],maxv[0]],thick=thick,yrange=yrange,ytitle='% of occur.',xlog=logarithmic,$
			charthick = charthick, xcharsize = xcharsize, ycharsize= ycharsize,xmargin=xmargin,ymargin=ymargin
; 			if ts then date=''
			if is_defined(hh2) then begin
				oplot,xx,hh/total(hh)*100.,color = cgcolor(!compare_col1) ,thick=thick
; phere
; 				oplot,xx,hh2/total(hh2)*100.,color = cgcolor('Red'),thick=thick
; 				legend,[date1+algon1+dtn[0]+hct,date2+algon2+dtn[1]+hct],thick=[thick,thick],$
; 				color=[-1,cgcolor('Red')],spos=(keyword_set(change_side) ? 'bot':'top'),charsize=lcharsize,charthick = charthick
				oplot,xx,hh2/total(hh2)*100.,color = cgcolor(!compare_col2) ,thick=thick
				legend,[date1+algon1+dtn[0]+hct,date2+algon2+dtn[1]+hct],thick=[thick,thick],$
				color=[cgcolor(!compare_col1) ,cgcolor(!compare_col2) ],spos=(keyword_set(change_side) ? 'bot':'top'),charsize=lcharsize,charthick = charthick
			endif else begin
				legend,date2+algon1+dtn[0]+hct,thick=thick,spos=(keyword_set(change_side) ? 'bot':'top'),$
; 				charsize=lcharsize,charthick = charthick,color =-1
				charsize=lcharsize,charthick = charthick,color =cgcolor(!compare_col1)
			endelse
		endif else begin
; 			if ts then date1=''
			define_oplots, opl, cols, spos, linestyle, psym, ystretch, error=error
			oplot,xx,hh/total(hh)*100.,thick=thick,col=cgcolor(cols),linestyle=linestyle,psym=psym,symsize=symsize
			legend,date1+algon1+dtn[0]+hct,thick=thick,color=cgcolor(cols), $
			spos=spos,ystretch=ystretch+0.5,charsize=lcharsize,charthick = charthick,linestyle = linestyle,psym=psym
		endelse
	end_save,save_as

end
;-------------------------------------------------------------------------------------------------------------------------
pro plot_zonal_average,year ,month ,day, file,varname,algo=algo,limit=limit,sea=sea,land=land, save_as=save_as,mini=mini,maxi=maxi, $
			win_nr = win_nr,timeseries=timeseries,satellite=satellite,oplots=oplots,found=found,level=level, coverage = coverage		,$
			addtext = addtext,datum=datum,error=error, white_bg = white_bg,old_ts=old_ts,simulator=simulator,dirname=dirname,$
			notitle=notitle,nobar=nobar,logarithmic=logarithmic

	opl   = keyword_set(oplots) ? fix(oplots) : 0
	hct   = keyword_set(addtext) ? ' - '+strupcase(addtext[0]) : ''
	date  = keyword_set(datum) ? datum+' ' : ''
	sav   = keyword_set(save_as) 
	wbg   = keyword_set(white_bg)
	sim   = keyword_set(simulator)
	ts    = keyword_set(timeseries)
	satn  = sat_name(algo,satellite, year=(ts ? 0:year), month=(ts ? 0:month),level=level)

; 	datum = string(year[0],f='(i4.4)')+string(month[0],f='(i2.2)')

	if ts then begin
		date = !DEFAULT_TS_PERIOD ; use this as first guess
		d = get_available_time_series( 	algo, varname, satellite, coverage = coverage, reference = reference, period = date, sav_file = sav_file,$
						found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, anomalies = anomalies, $
						no_trend_found = no_trend_found, pvir=pvir)
		if not found then begin
			if ~no_trend_found then ok = dialog_message("plot_zonal_average: Sav File not found! "+sav_file)
			return
		endif
		unit = d.unit
		bild = d.mean
		make_geo,grid_res=get_grid_res(bild),lon,lat
		fillvalue = -999.
		date=d.actual_date
		file = 'time_series'
	endif else begin
		bild = get_data(year,month,day,file=file,data=varname,no_data_value=fillvalue,minvalue=minvalue,algo=algo,sat=satellite,dirname=dirname,/print_filename, $
			maxvalue=maxvalue,longname=longname,unit=unit,verbose=verbose,found=found,/make_compareable,level=level,dim3=dim3,error=error)
		make_geo,file=file,lon,lat,grid=get_grid_res(bild),found=found_geo
		if ~found_geo then begin
			ok=dialog_message('plot_zonal_average: No Latitude information found!')
			return
		endif
	endelse

	if ~found then begin
		ok= dialog_message('plot_zonal_average: '+(file_test(file) ? 'Data '+varname+' not found!': 'File not found!'))
		return
	endif
	win_nr   = adv_keyword_set(win_nr) ? win_nr : 1
	if keyword_set(save_as) then begin
		save_as = !save_dir +file_basename(file[0],is_hdf(file[0]) ? '.hdf':'.nc')+'_zonal_median_'+varname+ $
			(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')+ $
			(keyword_set(limit) ? '_limit_'+strjoin(strcompress(limit,/rem),'_'):'')+$
			(keyword_set(oplots) ? '_oplots':'')+'.eps'
		save_as=strjoin(save_as,'_')
	endif else if win_nr ne -1 then win, win_nr,title=dat

	if keyword_set(limit) then begin
		idx = where(between(lon,limit[1],limit[3]) and between(lat,limit[0],limit[2]),complement=nd_idx,ncomp=nd_cnt)
		if nd_cnt gt 0 then lat[nd_idx] = fillvalue
	endif

	if keyword_set(land) or keyword_set(sea) then begin
; 		dem = get_dem(lon,lat,grid=get_grid_res(bild))
		dem = get_coverage(lon, lat, /land)
		if keyword_set(sea) then lat[where(dem ne 0)] = fillvalue
		if keyword_set(land) then lat[where(dem eq 0)] = fillvalue
	endif

; 	if strlowcase(algo) eq 'calipso' then lat_res =2.
	lat_res = 1. > get_grid_res(bild)
	medi = zonal_average(bild[where(lat ne fillvalue)],lat[where(lat ne fillvalue)],fillvalue=fillvalue,lat_zon=lat1d,/mean,/nan,lat_res = lat_res);,/median)
	idx  = where(finite(medi),chk_idx)
	yr = [(adv_keyword_set(mini)? mini : (varname eq 'ctt' ? 200:0)),(adv_keyword_set(maxi)? maxi : max(medi[idx])*1.05)] 

	if sav or sim then begin
		charthick = !p_charthick ;1.5
		xcharsize = !p_xcharsize ;1.7 
		ycharsize = !p_ycharsize ;1.7
		lcharsize = sim ? 2.0 : !l_charsize  ;sim ? 2.0 : 2.5
		xmargin   = [14,6]
		ymargin   = [ 7,3]
	endif else if wbg then begin
		charthick = !p_charthick ;3.0
		xcharsize = !p_xcharsize ;2.5
		ycharsize = !p_ycharsize ;2.5
		lcharsize = !l_charsize  ;3.0
		xmargin   = [20,6]
		ymargin   =  [8,3]
	endif else begin
		charthick = !p_charthick ;1.2
		xcharsize = !p_xcharsize ;1.2 
		ycharsize = !p_ycharsize ;1.2
		lcharsize = !l_charsize  ;1.5
		xmargin   =[10,3]
		ymargin   = [5,2]
	endelse

	start_save,save_as,thick=thick,snapshot=(float(opl)?'png':'')
		title = keyword_set(notitle) ? '' : (ts ? date : '')
		dtn = appendix(varname)
		sn_cov = short_cov_name(coverage)
		if keyword_set(sn_cov) then dtn += ' ('+ sn_cov+')'
		if wbg or sim then thick = 5
		if opl eq 0 then begin
			plot,[0,0],[1,1],xr=[-90,90],xs=3,/ys,xticks=6,xtickname=['-90','-60','-30','0','30','60','90'], $
			xtitle='latitude [degrees]',ytitle=full_varname(varname) + unit,yr=yr,title=title,ylog=logarithmic, $
			charthick = charthick, xcharsize = xcharsize, ycharsize= ycharsize,xmargin=xmargin,ymargin=ymargin
			if ts then date = ''
			if chk_idx gt 0 then oplot,lat1d,medi,thick=thick
			if ~keyword_set(nobar) then legend,date+satn+dtn+hct,thick=thick,spos='top',charsize=lcharsize,color =-1,charthick=charthick
		endif else begin
			define_oplots, opl, cols, spos, linestyle, psym, ystretch, error=error,timeseries=nobar
			if yr[0] lt yr[1] then begin
				if chk_idx gt 0 then oplot,lat1d,medi,thick=thick,col=cgcolor(cols),linestyle=linestyle,min_value=yr[0],max_value=yr[1]
			endif else begin
				if chk_idx gt 0 then oplot,lat1d,medi,thick=thick,col=cgcolor(cols),linestyle=linestyle,min_value=yr[1],max_value=yr[0]
			endelse
			if ts then date = ''
			legend,date+satn+dtn+hct,thick=thick,color=cgcolor(cols), spos=spos,ystretch=ystretch+0.5,charsize=lcharsize,$
			linestyle=linestyle,charthick=charthick
		endelse
	end_save,save_as

end
;-------------------------------------------------------------------------------------------------------------------------
pro plot_simple_timeseries, varname, satellite, algo, cov, reference = reference, mini=mini, maxi=maxi,$
			    sav_file = sav_file, verbose=verbose, oplots=oplots, found=found,$
			    addtext = addtext,error=error,save_as=save_as, win_nr=win_nr,white_bg=white_bg, $
			    logarithmic=logarithmic,show_trend=show_trend, rot = rot,$
			    symsize=symsize,notitle=notitle,nobar=nobar,smoothTS = smoothTS, ts_extras = ts_extras

	hct        = keyword_set(addtext)    ? ' - '+strupcase(addtext[0]) : ''
	win_nr     = adv_keyword_set(win_nr) ? win_nr : 1
	opl        = keyword_set(oplots)     ? fix(oplots) : 0
	dat        = strsplit(strlowcase(varname),',',/ext)
	sat        = strlowcase(strjoin(strsplit(satellite,'-',/ex)))
	cmp_2vars  = n_elements(dat) eq 2

	if n_elements(dat) ge 2 then begin
		if n_elements(dat) gt 2 then begin
			ok = dialog_message('Found more than 2 variable names. Stop')
			return
		endif
		d = get_available_time_series( 	algo, dat[0], sat, coverage = cov, period = !DEFAULT_TS_PERIOD, pvir = pvir, $
						sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, $
						anomalies = anomalies, sum=sum,no_trend_found = no_trend_found, uncertainty = uncertainty,ts_extras = ts_extras)
		if not found then begin
			if ~no_trend_found then ok = dialog_message("plot_simple_timeseries: Sav File not found! "+sav_file)
			return
		endif

		ts_data  = d.stats ; stats = global mean (latitude weighted) ;ts_data = d.STATS_NON_WEIGHTED
; 		ts_data  = d.STATS_NON_WEIGHTED
		if anomalies and is_tag(d,'TREND') then begin
			ts_data = d.trend.stats_anom
		endif else if anomalies and ~is_tag(d,'TREND') then begin
			ts_data = (d.stats-d.stats_sm)
		endif
		if tr_corr and is_tag(d,'TREND') then begin
			;replace gm1 with trend_corrected gm
			ts_data = d.TREND.stats
			dat[0]  = strreplace(dat[0],'_trend_corr','')
		endif else if tr_corr and ~is_tag(d,'TREND') then begin
			print,'No Trend corrected Time series available! For CCI, Clara and Patmos only NOAA-AM,NOAA-PM satellites have trends!'
		endif
		dtn = appendix(dat[0],trend_corrected=tr_corr)
		d1  = get_available_time_series( keyword_set(reference) ? reference :algo, dat[1], sat, coverage = cov, period = !DEFAULT_TS_PERIOD, $
						sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, pvir = pvir,$
						anomalies = anomalies, sum=sum1,no_trend_found = no_trend_found, uncertainty = uncertainty,ts_extras = ts_extras)
		if not found then begin
			if ~no_trend_found then ok = dialog_message("plot_simple_timeseries: Sav File not found! "+sav_file)
			return
		endif
		ts_data1  = d1.stats ; stats = global mean (latitude weighted)
; 		ts_data1  = d1.STATS_NON_WEIGHTED
		if anomalies and is_tag(d1,'TREND') then begin
			ts_data1 = d1.trend.stats_anom
		endif else if anomalies and ~is_tag(d1,'TREND') then begin
			ts_data1 = (d1.stats-d1.stats_sm)
		endif
		if tr_corr and is_tag(d1,'TREND') then begin
			;replace gm1 with trend_corrected gm
			ts_data1 = d1.TREND.stats
			dat[1]   = strreplace(dat[1],'_trend_corr','')
		endif else if tr_corr and ~is_tag(d1,'TREND') then begin
			print,'No Trend corrected Time series available! For CCI, Clara and Patmos only NOAA-AM,NOAA-PM satellites have trends!'
		endif
		dtn     = [dtn,appendix(dat[1],trend_corrected=tr_corr)]
		if stregex(dat[1],'nretr',/fold,/bool) and stregex(dat[0],'nobs',/fold,/bool) and keyword_set(sum1) and keyword_set(sum) then begin
			dat = ''
			d.longname = 'Rel. Diff NObs. - NRetr. [%]'
			ts_data = (ts_data - ts_data1)/ts_data1 *100.
		endif else begin
			dat     = strjoin(dat,' - ')
			ts_data = ts_data - ts_data1
		endelse
		dtn     = strjoin(dtn,' -')
		free_reference = 1
	endif else begin
		d = get_available_time_series( 	algo, dat, sat, coverage = cov, reference = reference, period = !DEFAULT_TS_PERIOD, $
						sav_file = sav_file, found = found, stddev = stddev, trend = trend, tr_corr = tr_corr, pvir = pvir,$
						anomalies = anomalies, sum=sum,no_trend_found = no_trend_found, ts_extras = ts_extras, $
						uncertainty = uncertainty, bias = bias, rmse = rmse, corr = corr, bc_rmse = bc_rmse)
		if not found then begin
			if ~no_trend_found then ok = dialog_message("plot_simple_timeseries: Sav File not found! "+sav_file)
			return
		endif
		ts_data  = d.stats ; stats = global mean (latitude weighted)
		if anomalies and is_tag(d,'TREND') then begin
			ts_data = d.trend.stats_anom
		endif else if anomalies and ~is_tag(d,'TREND') then begin
			ts_data = (d.stats-d.stats_sm)
		endif
		if tr_corr and is_tag(d,'TREND') then begin
			;replace gm1 with trend_corrected gm
			ts_data = d.TREND.stats
			dat     = strreplace(dat,'_trend_corr','')
		endif else if tr_corr and ~is_tag(d,'TREND') then begin
			print,'No Trend corrected Time series available! For CCI, Clara and Patmos only NOAA-AM,NOAA-PM satellites have trends!'
		endif
		dtn = appendix(dat,trend_corrected=tr_corr)
	endelse

	zeitraum = d.period
	ori_period = fix(strsplit(d.period,'-',/ext))
	longname = d.longname
	if stregex(dat,'nretr',/fold,/bool) then begin
		longname = '# of valid Retrievals'
	endif
	if stregex(dat,'nobs',/fold,/bool) then begin
		longname = '# of Observations'
	endif

	if stregex(d.coverage,'midlat_trop',/fold,/bool) then begin
		coverage = string(177b)+'60'+string(176b)+' Lat'
		if stregex(d.coverage,'sea',/fold,/bool)  then coverage += ' (Sea)'
		if stregex(d.coverage,'land',/fold,/bool) then coverage += ' (Land)'
	endif else coverage = d.coverage

	only_sat = (stregex(dat[0],'refl',/fold,/bool) or stregex(dat[0],'rad',/fold,/bool) ) and algo2ref(algo) eq 'gac2'
	algon    = sat_name(algo,sat,only_sat=only_sat)
	ref      = sat_name((keyword_set(reference) ? reference:algo),sat,only_sat=only_sat)
	datum    = strcompress(d.actual_date,/rem)
	satn_background = (sat eq 'noaaam' or sat eq 'noaapm') and keyword_set(white_bg)

	if keyword_set(free_reference) then begin
		free, reference
		if ~keyword_set(only_sat) then begin
			dtn   = ''
			if strmatch(algon,ref) then begin
				algon = algon +' '+strupcase(dat)
			endif else begin
				algon = strjoin([' '+algon,ref] + ' '+strupcase(strsplit(dat,' - ',/ext)),' -!C ')
			endelse
		endif
	endif

	; set all reflectance and brightness TS dots to NAN if Noaa15 and end of 2000 (very low coverage, orbit counts below 100)
	if only_sat and sat eq 'noaa15' or sat eq 'noaaam' then begin
		dumidx = indgen(5)+(2000-ori_period[0])*12 + 7
		bla = where(dumidx ge 0,blacnt)
		if blacnt gt 0 then begin
			dumidx= dumidx[bla]
			print,'Remove NOAA-15 data from end of year 2000, because of very low observation.'
			ts_data[*,dumidx] = !values.f_nan
		endif
	endif

	count=0
	satnames = strarr(n_elements(ts_data[0,*]))
	for y=ori_period[0],ori_period[1] do begin & $
		for m=1,12 do begin & $
			short=0 & $
			satnames[count] = noaa_primes(y,m,ampm=sat_ampm(sat,/ampm),/no_zero,short=short,hirs=(algo2ref(algo) eq 'hec')) & $
			count ++ & $
		endfor & $
	endfor

	if keyword_set(save_as) then begin
		save_as = strcompress(save_as,/rem) eq '1' ? !save_dir +'/timeseries/'+file_basename(sav_file,'.sav')+'.eps' : save_as
	endif else if win_nr ne -1 then win, win_nr,title=dat[0]

	yrange     = minmax(ts_data[0,*],/nan)
	dum        = fix(strsplit(zeitraum,'-',/ext))

	if adv_keyword_set(mini) and adv_keyword_set(maxi) then begin
		mini = float(strsplit(mini[0],',',/ext))
		maxi = float(strsplit(maxi[0],',',/ext))
		yrange =   [mini[0],maxi[0]]
		if n_elements(maxi) gt 1 then dum[1] = (1970 > (fix(maxi[1])-1) < 2050 )
		if n_elements(mini) gt 1 then dum[0] = (2050 < (fix(mini[1])-0) > 1970 )
		if dum[1] lt dum[0] then dum[1] = ori_period[1]
		if dum[0] gt dum[1] then dum[0] = ori_period[0]
	endif
	lines = pgrid(yrange,log=logarithmic)

	;Quotient für BCRMSE Achse
	qu = keyword_set(rot) ? (rot>0) : 0 ; now 0 means dont show 

	jumps=0
	nochmal:
	num_of_yy = ((dum[1]+1)-dum[0])
	if num_of_yy lt 10 then begin
		divi_yy = 1
	endif else if num_of_yy/3 eq num_of_yy/3. then begin
		divi_yy = 3
	endif else if num_of_yy/2 eq num_of_yy/2. then begin
		divi_yy = 2
		if num_of_yy gt 20 then begin
			if (num_of_yy/4 eq num_of_yy/4.) then divi_yy = 4 $
			else begin
				dum[1]++
				jumps++
				if jumps lt 5 then goto, nochmal
			endelse
		endif
	endif else begin
		dum[1]++
		jumps++
		if jumps lt 5 then goto, nochmal
		divi_yy = 2
	endelse

	xtickname  = strcompress(vector(dum[0],dum[1]+1,(2>(num_of_yy/divi_yy+1))),/rem)
	
	if dum[0] lt ori_period[0] then begin
		cnts = ((fix(strsplit(d.period,'-',/ext)))[0] - dum[0]) *12
		ts_dummy = fltarr((size(ts_data,/dim))[0],cnts) + !values.f_nan
		ts_data  = [[ts_dummy],[ts_data]]
		ori_period[0] = dum[0]
	endif
	anz = ( dum[1]-dum[0] + 1 ) *12.

	if anz eq 12. then begin
; 		xtickname = [xtickname[0],'Jan',' ','Feb',' ','Mar',' ','Apr',' ','May',' ','Jun',' ','Jul',' ',$
; 				'Aug',' ','Sep',' ','Oct',' ','Nov',' ','Dec',xtickname[1]]
		xtickname = [xtickname[0],' ','Q1',' ',' ',' ','Q2',' ',' ',' ','Q3',' ',' ',' ','Q4',' ',xtickname[1]]
	endif

	if anz lt n_elements(ts_data[0,*]) then begin
		anf = ((dum[0]-ori_period[0])*12)
		eef = ((dum[1]-ori_period[0]+1)*12)
		if dum[0] gt ori_period[0] then ts_data[*,0:(anf-1)] = !values.f_nan
		if ori_period[1] gt dum[1] then ts_data[*,eef:*] = !values.f_nan
		anz = [anf,eef]
	endif else anz = [0,anz]

	satnames[where(~finite(ts_data[0,*]))] = ''

	str_pholder = strjoin(replicate(' ',max([strlen(algon),strlen(ref)])))
	print,'-------'+dat+'--------'

	if trend then begin
		make_geo,lon_d,lat_d,grid=([360.,180.]/size(d.trend.mean,/dim))[0]
		idx = keyword_set(reference) ? where((d.trend.mean ne -999.) and (d.trend.mean2 ne -999.),md_cnt) : where((d.trend.mean ne -999.),md_cnt)
		print,'Glob. Mean      '+string(algon,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(gmean(d.trend.mean[idx],lat_d[idx]),f='(f11.4)')
		if keyword_set(reference) then begin
			bias = gbias(d.trend.mean[idx],d.trend.mean2[idx],lat_d[idx])
			rmse = grmse(d.trend.mean[idx],d.trend.mean2[idx],lat_d[idx])
			stdd = bc_rmse(bias,rmse)
			corr = gcorrelate(d.trend.mean[idx],d.trend.mean2[idx],lat_d[idx])
			print,'Glob. Mean      '+string(ref,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' :',string(gmean(d.trend.mean2[idx],lat_d[idx]),f='(f11.4)')
			print,'Glob. BIAS      '+str_pholder+' :',string(bias,f='(f11.4)')
			print,'Glob. RMSE      '+str_pholder+' :',string(rmse,f='(f11.4)')
			print,'Glob. BC-RMSE   '+str_pholder+' :',string(stdd,f='(f11.4)')
			print,'Correlation     '+str_pholder+' :',string(corr,f='(f11.4)') +' ('+strcompress(string(corr,f='(f11.4)'),/rem)+')'
		endif
	endif else begin
		print,'Glob. Mean    '+string(algon,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' : ',$
		string(d.OVERALL_STATS.LATITUDE_WEIGHTED.AVGERAGE,f='(f11.4)')
		if cmp_2vars then begin
			print,'Glob. Mean2   '+string(algon,f='(A'+strcompress(strlen(str_pholder),/rem)+')') +' : ',$
			string(d1.OVERALL_STATS.LATITUDE_WEIGHTED.AVGERAGE,f='(f11.4)')
		endif
		if keyword_set(reference) then begin
			print,'Glob. Mean    '+string(ref,f='(A'+strcompress(strlen(str_pholder),/rem)+')')+' : ',$
			string(d.OVERALL_STATS.LATITUDE_WEIGHTED.AVGERAGE2,f='(f11.4)')
			print,'Glob. BIAS    '+str_pholder        +' : ',string(d.OVERALL_STATS.LATITUDE_WEIGHTED.BIAS,f='(f11.4)')
			print,'Glob. RMSE    '+str_pholder        +' : ',string(d.OVERALL_STATS.LATITUDE_WEIGHTED.RMSE,f='(f11.4)')
			print,'Glob. BC-RMSE '+str_pholder        +' : ',string(d.OVERALL_STATS.LATITUDE_WEIGHTED.BC_RMSE,f='(f11.4)')
			print,'Correlation   '+str_pholder        +' : ',string(d.OVERALL_STATS.LATITUDE_WEIGHTED.CORRELATION,f='(f11.4)')
		endif
	endelse

	gac_ts_plots,d,ts_data,strupcase(dat),algon,yrange,lines,anz,xtickname,qu,ref	, $
		 log=logarithmic,save_as=save_as,error=error,show_trend=show_trend		, $
		 no_compare=~keyword_set(reference),opl=opl									, $
		 longname=longname,coverage=coverage,hct=hct,white_bg=white_bg,datum=datum	, $
		 anomalies=anomalies,trend=trend,symsize=symsize,tr_corr=tr_corr,notitle=notitle			, $
		 stddev = stddev, uncertainty = uncertainty,bias=bias,rmse=rmse,bc_rmse=bc_rmse,corr=corr,$
		 satnames = satnames,sum=sum, dtn=dtn, satn_background = satn_background,smoothTS = smoothTS

end
;-------------------------------------------------------------------------------------------------------------------------
pro boxplot, year, month, day, data=data, satellite = satellite, timeseries = timeseries, limit = limit, coverage = coverage	, $
		error = error, mini = mini, maxi = maxi, save_as = save_as, win_nr = win_nr, datum = datum, verbose=verbose	, $
		level = level, algo = algo, reference=reference, filename1 = filename1, filename2=filename2,notitle=notitle

	; Die Box gibt an, in welchem Bereich 50 % der Daten liegen, und die Box inklusive Whisker gibt an, in welchem Bereich der Großteil der Daten liegt. 
	; An der Lage des Medians innerhalb dieser Box kann man erkennen, ob eine Verteilung symmetrisch oder schief ist.
	if n_params() lt 2 and ~keyword_set(timeseries) then begin 
		print,'Syntax : boxplot, year, month, data = data, satellite = satellite, timeseries=timeseries'
		return
	endif

	annex = ''
	apx1  = ''
	apx2  = ''
	ts    = keyword_set(timeseries)
	dat   = keyword_set(data) 	? strsplit(strlowcase(data),',',/ext) 	: ''
	if n_elements(dat) ne 2 then dat = [dat,dat]
	sat   = keyword_set(satellite) 	? strlowcase(satellite)	: ''
	cov   = keyword_set(coverage) 	? strlowcase(coverage) 	: ''

	if keyword_set(save_as) then begin
		save_as = !SAVE_DIR +'boxplots'+'/'+datum+'_'+dat[0]+'_'+sat+'_'+dat[1]+'_'+cov+'_box_plots'+'.eps'
	endif else if win_nr ne -1 then win, win_nr, size=700,ysize=1200

	if ts then begin
		datum = !DEFAULT_TS_PERIOD
		if ~strmatch(dat[0],dat[1]) then begin
			d = get_available_time_series( 	algo, dat[0], sat, coverage = cov, period = datum, found = found, $
							stddev = stddev, trend = trend, tr_corr = tr_corr, anomalies = anomalies, $
							no_trend_found = no_trend_found)
			if found then begin
				cci  = d.mean
				cci  = percentile(cci[where(cci ne -999.)],[.5,.75,.25,.975,.025])
				unit = d.unit
				longname = d.longname
				datum = d.actual_date
				name  = sat_name(algo,sat)+' '+strupcase(dat[0])
				free,d
			endif else begin
				if ~no_trend_found then ok = dialog_message('No Time Series found!')
				return
			endelse
			d = get_available_time_series( 	reference, dat[1], sat, coverage = cov, period = datum, found = found, $
							stddev = stddev, trend = trend, tr_corr = tr_corr, anomalies = anomalies, $
							no_trend_found = no_trend_found)
			if found then begin
				ref   = d.mean
				ref   = percentile(ref[where(ref ne -999.)],[.5,.75,.25,.975,.025])
				name  = [name,sat_name(reference,sat)+' '+strupcase(dat[1])]
				free,d
			endif else begin
				if ~no_trend_found then ok = dialog_message('No Time Series found!')
				return
			endelse
			array = [[temporary(cci)],[temporary(ref)]]
		endif else begin
			d = get_available_time_series( 	algo, dat[0], sat, reference = reference, coverage = cov, period = datum, found = found, $
							stddev = stddev, trend = trend, tr_corr = tr_corr, anomalies = anomalies, $
							no_trend_found = no_trend_found)
			if found then begin
				cci = d.percentiles
				ref = d.percentiles2
				unit = d.unit
				longname = d.longname
				datum = d.actual_date
				name  = [sat_name(algo,sat),sat_name(reference,sat)]
				array = [[temporary(cci)],[temporary(ref)]]
				free,d
			endif else begin
				if ~no_trend_found then ok = dialog_message('No Time Series found!')
				return
			endelse
		endelse
	endif else begin
		datum = string(month,f='(i2.2)')+'/'+string(year,f='(i4.4)')
		cci  = 	get_data(year,month,day,file = filename1,data=dat[0],algo=algo,sat=sat,glob=1,/mean,verbose=verbose,finfo=cci_info,/print_filename, $
			no_data=ndv_cci,unit=unit,longname=longname,found=found,level=level,/sil,/make_compareable,/join_nodes,dim3=dim3)
		if not found then begin
			ok=dialog_message('Data not found! '+algo+' '+dat[0])
			return
		endif

		ref  = 	get_data(year,month,day,file = filename2,data=dat[1],algo=reference,sat=sat,glob=1,/mean,verbose=verbose,dim3=dim3, $
			no_data=ndv_ref,found=found,level=level,/sil,/make_compareable,/join_nodes,finfo=ref_info,print_filename=2)
		if not found then begin
			ok=dialog_message('Data not found! '+reference+' '+dat[1])
			return
		endif

		make_geo,lon,lat,grid=1
		dem = get_coverage( lon, lat, coverage = cov,limit = limit, found = found)
		if ~found then begin & ok = dialog_message('box_plot: coverage '+cov+' not defined!') & return & end

		ref  =  percentile(ref[where(ref ne ndv_ref and dem eq 1)],[.5,.75,.25,.975,.025])
		cci  =  percentile(cci[where(cci ne ndv_cci and dem eq 1)],[.5,.75,.25,.975,.025])

		if is_the_same(algo,reference,sat=sat) then begin
			if ref_info.mtime gt cci_info.mtime then begin & apx1 = ' (old)' & apx2 = ' (new)' & new = 1 & end
			if cci_info.mtime gt ref_info.mtime then begin & apx1 = ' (new)' & apx2 = ' (old)' & new = 0 & end
			if ~strmatch(dat[0],dat[1]) then begin
				apx1 += ' '+strupcase(dat[0])
				apx2 += ' '+strupcase(dat[1])
			endif
		endif else new = -1

		name  = [sat_name(algo,sat,year=year,month=month,level=level)+apx1,sat_name(reference,sat,year=year,month=month,level=level)+apx2]
		array = [[temporary(cci)],[temporary(ref)]]

		; see what else we have
		if level eq 'l3u' then median = 1 else mean = 1
		annex = (sat eq 'aatme' and total(dat[0] eq ['ctt','ctp','cph','cfc','cc_total','cth'])?'_day':'')
		set_algolist, algo_list, sat = sat, data = dat[0], exclude = [algo,reference],/default
		struc = get_all_avail_data(year,month,day,data=dat[0]+annex,sat=sat,level=level,algo_list=algo_list,coverage=coverage, $
			glob=1,/make_compare,mean=mean,median=median,verbose=verbose,percentile=[.5,.75,.25,.975,.025],limit=limit)
		if is_struct(struc) then begin
			name  = [name,struc.algo_names]
			array = [[array],[struc.PERCENTILE]]
			free, struc
		endif
	endelse

	medi     = array[0,*]
	oben     = array[1,*]
	unten    = array[2,*]
; 	iqr      = oben - unten
	wh_oben  = array[3,*]
	wh_unten = array[4,*]

	free, array

	case n_elements(medi) of 
		8	: begin & xox = -0.35 & cc  = [-0.2,0.0,0.2,0.4,0.6,0.8,1.0,1.2] & end
		7	: begin & xox = -0.25 & cc  = [-0.1,0.1,0.3,0.5,0.7,0.9,1.1] & end
		6	: begin & xox = -0.15 & cc  = [0.0,0.2,0.4,0.6,0.8,1.0] & end
		5	: begin & xox = -0.05 & cc  = [0.1,0.3,0.5,0.7,0.9] & end
		4	: begin & xox =  0.05 & cc  = [0.2,0.4,0.6,0.8] & end
		3	: begin & xox = -0.20 & cc  = [0.0,0.5,1.] & end
		2	: begin & xox =  0.05 & cc  = [0.25,0.75] & end
		1	: begin & xox =  0.05 & cc  = [0.5] & end
		else	: begin & ok = dialog_message('boxplot: TBD') & stop & end
	endcase

	start_save,save_as,thick=thick

	plot,[0,0],[1,1],yr=[min(wh_unten[where(wh_unten ne -999.)]),max(wh_oben[where(wh_oben ne -999.)])],xticks = n_elements(name)+1, $
	xtickname=[' ',name,' '], ytitle=full_varname(dat[0]+annex,/universal)+' '+unit, xminor=1, xticklen=0.007, charsize=2, $
	title=strupcase(cov)+'  '+longname+'  '+datum

	for i = 0, n_elements(medi)-1 do begin
		if medi[i] eq -999 then continue
		if keyword_set(apx1) and strmatch(dat[0],dat[1]) then begin
			if (i eq new) then color = cgcolor('red') ; red only for the new cci
		endif
		; box
		; horizontal
		oplot,[cc[i]-.025,cc[i]+.025],[unten[i],unten[i]],color = color,thick=thick
		oplot,[cc[i]-.025,cc[i]+.025],[oben[i],oben[i]],color = color,thick=thick
		oplot,[cc[i]-.025,cc[i]+.025],[medi[i],medi[i]],thick=6,col=cgcolor('Goldenrod')
		; median von cci als reference linie
		; vertical
		oplot,[cc[i]-.025,cc[i]-.025],[unten[i],oben[i]],color = color,thick=thick
		oplot,[cc[i]+.025,cc[i]+.025],[unten[i],oben[i]],color = color,thick=thick
		; whiskers
		; horizontal
		oplot,[cc[i]-.005,cc[i]+.005],[wh_unten[i],wh_unten[i]],color = color,thick=thick
		oplot,[cc[i]-.005,cc[i]+.005],[wh_oben[i],wh_oben[i]],color = color,thick=thick
		; vertical
		oplot,[cc[i],cc[i]],[oben[i],wh_oben[i]],color = color,thick=thick
		oplot,[cc[i],cc[i]],[wh_unten[i],unten[i]],color = color,thick=thick
		free, color
	endfor

	oplot,!x.crange,[medi[0],medi[0]],linestyle = 2
	xyouts, xox,1.003 *medi[0],string(medi[0],f='(f7.2)'),charsize=2

	end_save,save_as
; 	!p.multi=0
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro plot_1d_from_jch_4all,year=year,month=month,file,file2,sat1=sat1, prefix=prefix,land=land,sea=sea,limit=limit,$
			save_as=save_as,verbose=verbose,win_nr=win_nr,antarctic=antarctic,arctic=arctic,liquid=liquid,ice=ice, $
			algo1=algo1,algo2=algo2,mini=mini,maxi=maxi,hist_cloud_type=hist_cloud_type,notitle=notitle,sat2=sat2

	pref   = keyword_set(prefix[0]) ? prefix[0]  : ''
	pref2  = keyword_set(prefix[1]) ? prefix[1] : ''
	hct    = keyword_set(hist_cloud_type) ? strlowcase(hist_cloud_type[0]) : '1d'
	is_new = stregex(pref,'new',/fold,/bool) ? 1 : (stregex(pref2,'new',/fold,/bool) ? 2 : 0)
	ls     = keyword_set(land) or keyword_set(sea)
	win_nr = adv_keyword_set(win_nr) ? win_nr : 1
; 	dat    = 'cot_ctp_hist2d'+(keyword_set(liquid) ? '_liq' : '')+(keyword_set(ice) ? '_ice' : '')
	dat    = 'hist2d_cot_ctp'+(keyword_set(liquid) ? '_liq' : '')+(keyword_set(ice) ? '_ice' : '')
	apx    = keyword_set(liquid) ? 'Liquid ': (keyword_set(ice) ? 'Ice ' : '')
	level  = 'l3c' ; this needs to be changed once l3s has histos too

	algon1 = sat_name(algo1,sat1,year=year,month=month,level=level)
	algon2 = sat_name(algo2,sat2,year=year,month=month,level=level)

	if keyword_set(save_as) then begin
		if save_as eq '1' then begin 
			save_dum = !SAVE_DIR +'diffs/Diff_'+file_basename(file,is_hdf(file) ? '.hdf':'.nc')+'_-_'+$
			file_basename(file2,is_hdf(file2) ? '.hdf':'.nc')+$
			(keyword_set(land) ? '_land':'')+(keyword_set(sea) ? '_sea':'')+$
			(keyword_set(limit) ? '_limit_'+strjoin(strcompress(string(limit,f='(f6.1)'),/rem),'_') : '')+$
			'_cot_ctp_hist2d_'+(keyword_set(liquid) ? 'liq_' : '_')+(keyword_set(ice) ? 'ice_' : '_')+$
			'1D_HIST.eps'
		endif else save_dum = save_as
		save_as1 = file_name_info(save_dum,/path,/name)+'_CTP.'+file_name_info(save_dum,/ext)
		save_as2 = file_name_info(save_dum,/path,/name)+'_COT.'+file_name_info(save_dum,/ext)
		!p.multi=0
	endif else begin
		if win_nr ne -1 then win, win_nr, xs=1200, ys=800,title=dat
		if hct eq '1d' then !p.multi=[0,1,2]
	endelse

	; get the input_data
	if keyword_set(file) then begin
		cci1       = get_data(year,month,day,file = file[0],algo=algo1,data=dat,sat=sat1,level=level,verbose=verbose,found=found,dim3=dim3,/print_filename)
		if found then begin
			dum = get_1d_hist_from_jch(cci1,algo1,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic,land=land,sea=sea,found=found)
			if found then begin
				cot_array  = dum.cot
				ctp_array  = dum.ctp
				name_arr   = pref+algon1
				cci1_color = is_new eq 1 ? !compare_col2 : !compare_col1
				col_arr    = cci1_color
				lspos      = 'top'
				lystr      = 0
				lstyle     = 0
			endif
		endif
	endif else begin
		ok=dialog_message('plot_1d_from_jch_4all: Input File1 not found!' )
		return
	endelse
	if keyword_set(file2) then begin
		cci2       = get_data(year,month,day,file = file2,algo=algo2,data=dat,sat=sat2,level=level,verbose=verbose,found=found,dim3=dim3,print_filename=2)
		if found then begin
			dum = get_1d_hist_from_jch(cci2,algo2,bin_name=xtickname,limit=limit,antarctic=antarctic,arctic=arctic,land=land,sea=sea,found=found)
			if found then begin
				cot_array  = is_defined(cot_array) ? [[cot_array],[dum.cot]] : dum.cot
				ctp_array  = is_defined(ctp_array) ? [[ctp_array],[dum.ctp]] : dum.ctp
				name_arr   = is_defined(name_arr)  ? [name_arr,pref2+algon2] : pref2+algon2
				cci2_color = is_new ne 1 ? !compare_col2 : !compare_col1
				col_arr    = is_defined(col_arr)   ? [col_arr,cci2_color] : cci2_color
				lspos      = is_defined(lspos)     ? [lspos,'top'] : 'top'
				lystr      = is_defined(lystr)     ? [lystr,1] : 1
				lstyle     = is_defined(lstyle)    ? [lstyle,0] : 0
			endif
		endif
	endif else begin
		ok=dialog_message('plot_1d_from_jch_4all: Input File2 not found!' )
		return
	endelse

	; get all the rest of the data
	dumsat = (sat1 eq '' ? sat2 : sat1)
	annex = (dumsat eq 'aatme' and total(dat[0] eq ['ctt','ctp','cph','cfc','cc_total','cth'])?'_day':'')
	set_algolist, algo_list, sat = dumsat, data = dat, exclude = [algo1,algo2],/default
	struc = get_all_avail_data(year,month,day,data=dat+annex,sat=dumsat,level=level,algo_list=algo_list,verbose=verbose)
	if is_struct(struc) then begin
		for i = 0,n_elements(struc.ref_names)-1 do begin
			tag = struc.ref_names[i]
			num = (tag_name2num(struc,tag))
			if tag eq 'modi' then tag = 'mod'
			dum = struc.(num).data
			dum = get_1d_hist_from_jch(dum,tag,limit=limit,antarctic=antarctic,arctic=arctic,land=land,sea=sea,found=found)
			if found then begin
				cot_array = is_defined(cot_array) ? [[cot_array],[dum.cot]] 		: dum.cot
				ctp_array = is_defined(ctp_array) ? [[ctp_array],[dum.ctp]] 		: dum.ctp
				name_arr  = is_defined(name_arr)  ? [name_arr,struc.(num).ALGONAME] 	: struc.(num).ALGONAME
				col_arr   = is_defined(col_arr)   ? [col_arr,struc.colors[i]] 		: struc.colors[i]
				lspos     = is_defined(lspos)     ? [lspos,struc.LEGEND_SPOS[i]] 	: struc.LEGEND_SPOS[i]
				lystr     = is_defined(lystr)     ? [lystr,struc.LEGEND_YSTRETCH[i]] 	: struc.LEGEND_YSTRETCH[i]
				lstyle    = is_defined(lstyle)    ? [lstyle,struc.LINESTYLE[i]] 	: struc.LINESTYLE[i]
			endif
		endfor
		free,struc
	endif

	yrange = adv_keyword_set(mini) and adv_keyword_set(maxi) ? [mini,maxi] : [0,50]

	if hct eq '1d' or hct eq '1d_ctp' then begin
		; ctp
		start_save,save_as1,thick=thick,size=[32,16]
			plot,[0,0],[1,1],yr=yrange,xr=[0,6],xticks=7,xtickname=keyword_set(found) ? xtickname.ctp:'', $
			xtitle=apx+'Cloud Top Pressure [hPa]',ytitle='Relative Occurrence [%]',xminor=2, $
			charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
			xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
			ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
			xmargin   = [12,4],ymargin = [6,2]
			legend,name_arr[0],color=cgcolor(col_arr[0]),thick=thick+1,spos='tl',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize :1.5)
			legend,name_arr[1],color=cgcolor(col_arr[1]),thick=thick+1,spos='tr',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize :1.5)
			for i = 0, n_elements(name_arr)-1 do begin & $
				oplot,ctp_array[*,i]/total(ctp_array[*,i])*100.,thick=thick+1,color=cgcolor(col_arr[i]),psym=-8,linestyle=lstyle[i] & $
				if i gt 1 then legend,name_arr[i],color=cgcolor(col_arr[i]),thick=thick+1,spos=lspos[i],ystretch=lystr[i]+1,$
				charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize : 1.5),linestyle=lstyle[i] & $
			endfor
		end_save, save_as1
	endif

	if hct eq '1d' or hct eq '1d_cot' then begin
		; cot
		start_save,save_as2,thick=thick,size = [32,16]
			plot,[0,0],[1,1],xr=[0,5],yr=yrange,xticks=6,xtickname=keyword_set(found) ? xtickname.cot:'', $
			xtitle=apx+'Cloud optical Thickness',ytitle='Relative Occurrence [%]',xminor=2, $
			charthick = (keyword_set(save_as) ? !p_charthick  : 1.7), $
			xcharsize = (keyword_set(save_as) ? !p_xcharsize  : 1.4), $
			ycharsize = (keyword_set(save_as) ? !p_ycharsize  : 1.4), $
			xmargin   = [12,4],ymargin = [6,2]
			legend,name_arr[0],color=cgcolor(col_arr[0]),thick=thick+1,spos='tl',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize :1.5)
			legend,name_arr[1],color=cgcolor(col_arr[1]),thick=thick+1,spos='tr',charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize :1.5)
			for i = 0, n_elements(name_arr)-1 do begin & $
				oplot,cot_array[*,i]/total(cot_array[*,i])*100.,thick=thick+1,color=cgcolor(col_arr[i]),psym=-8,linestyle=lstyle[i] & $
				if i gt 1 then legend,name_arr[i],color=cgcolor(col_arr[i]),thick=thick+1,spos=lspos[i],ystretch=lystr[i]+1,$
				charsize=(keyword_set(save_as) and ~keyword_set(zoom) ? !l_charsize :1.5),linestyle=lstyle[i] & $
			endfor
		end_save,save_as2
	endif
end
; ----------------------------------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------------------------------------------------------
