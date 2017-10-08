function save_name

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

	
end
