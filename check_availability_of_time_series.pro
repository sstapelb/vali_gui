pro check_availability_of_time_series

	;compare
	cli  = 'cci'
	covd  = ['midlat_trop','full','land', 'sea','antarctica','midlat_south','tropic','midlat_north','arctic']
	ref  = ['gac2','pmx','gac','myd2','mod2']
	data = ['cfc','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	sat  = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa18']
	count = 0
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(sat)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					if covd[j] eq 'full' then cov = '' else cov = covd[j]+'_' & $
					if total(data[l] eq ['cfc_low','cfc_mid','cfc_high','cer']) and ref[k] eq 'gac' then do_it = 0 & $
					if total(data[l] eq ['cfc_day','cfc_night','cth','cwp','cph','cer']) and ref[k] eq 'pmx' then do_it = 0 & $
					if ref[k] eq 'pmx' and sat[i] eq 'noaa17' then do_it = 0 & $ ; kein n17 in patmos 
					if total(data[l] eq ['cot','lwp','iwp']) and ref[k] eq 'pmx' and total(sat[i] eq ['noaa12','noaa15']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'myd' then do_it = 0 & $
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'mod' then do_it = 0 & $
					if strmid(ref[k],0,3) eq 'myd' and total(sat[i] eq ['noaa7','noaa9','noaa11','noaa12','noaa14']) then do_it = 0 & $
					if strmid(ref[k],0,3) eq 'mod' and total(sat[i] eq ['noaa7','noaa9','noaa11','noaa12']) then do_it = 0 & $
					if do_it then begin & $
						dum_file = 'compare_'+data[l]+'_'+cli+'_vs_'+ref[k]+'_time_series_'+sat[i]+'_'+cov+'1978-2016.sav' & $
						ff = file_search(!SAVS_DIR + 'time_series/compare/' + dum_file,count = found) & $
						if found ne 1 then begin & $
							print,'Missing: '+dum_file & $
							count++ & $
						endif & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	print,count, ' compare files are Missing!'

	;plot
	covd  = ['midlat_trop','full','land', 'sea','antarctica','midlat_south','tropic','midlat_north','arctic']
	ref  = ['gac2','pmx','gac','myd2','mod2','cci']
	data = ['cfc','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	satd = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa18']
	count = 0
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(satd)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					if covd[j] eq 'full' then cov = '' else cov = covd[j]+'_' & $
					if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
					if total(data[l] eq ['cfc_low','cfc_mid','cfc_high','cer']) and ref[k] eq 'gac' then do_it = 0 & $
					if total(data[l] eq ['cfc_day','cfc_night','cth','cwp','cph','cer']) and ref[k] eq 'pmx' then do_it = 0 & $
					if ref[k] eq 'pmx' and sat eq 'noaa17' then do_it = 0 & $ ; kein n17 in patmos 
					if total(data[l] eq ['cot','lwp','iwp']) and ref[k] eq 'pmx' and total(sat eq ['noaa12','noaa15']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'myd' then do_it = 0 & $
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'mod' then do_it = 0 & $
					if do_it then begin & $
						dum_file = 'plot_'+data[l]+'_'+ref[k]+'_time_series_'+sat+'_'+cov+'1978-2016.sav' & $
						ff = file_search(!SAVS_DIR + 'time_series/plot/' + dum_file,count = found) & $
						if found ne 1 then begin & $
							print,'Missing: '+dum_file & $
							count++ & $
						endif & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	print,count, ' plot files are Missing!'


	;hovmoeller
	covd  = ['midlat_trop','full','land', 'sea','antarctica','midlat_south','tropic','midlat_north','arctic']
	ref  = ['gac2','pmx','gac','myd2','mod2','cci']
	data = ['cfc','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	satd = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa18']
	count = 0
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(satd)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					if covd[j] eq 'full' then cov = '' else cov = covd[j]+'_' & $
					if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
					if total(data[l] eq ['cfc_low','cfc_mid','cfc_high','cer']) and ref[k] eq 'gac' then do_it = 0 & $
					if total(data[l] eq ['cfc_day','cfc_night','cth','cwp','cph','cer']) and ref[k] eq 'pmx' then do_it = 0 & $
					if ref[k] eq 'pmx' and sat eq 'noaa17' then do_it = 0 & $ ; kein n17 in patmos 
					if total(data[l] eq ['cot','lwp','iwp']) and ref[k] eq 'pmx' and total(sat eq ['noaa12','noaa15']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'myd' then do_it = 0 & $
					if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high']) and strmid(ref[k],0,3) eq 'mod' then do_it = 0 & $
					if do_it then begin & $
; 						cfc_hovmoeller_1978-2016_gac_noaa18.sav
						dum_file = data[l]+'_hovmoeller_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
						ff = file_search(!SAVS_DIR + 'time_series/hovmoeller/' + dum_file,count = found) & $
						if found ne 1 then begin & $
							print,'Missing: '+dum_file & $
							count++ & $
						endif & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	print,count, ' hovmoeller files are Missing!'


	;1d histos
	ref   = ['gac2','myd2','mod2','cci']
	data  = 'hist1d_'+['cwp','cot','ctp','ctt','cer','cwp_liq','cot_liq','ctp_liq','ctt_liq','cer_liq','cwp_ice','cot_ice','ctp_ice','ctt_ice','cer_ice']
	satd  = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa18']
	count = 0
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(satd)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					if covd[j] eq 'full' then cov = '' else cov = covd[j]+'_' & $
					if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
					if total(data[l] eq ['hist1d_ctp_ice','hist1d_ctp_liq']) and (ref[k] eq 'myd2' or ref[k] eq 'mod2') then do_it = 0 & $
					if do_it then begin & $
						dum_file = data[l]+'_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
						ff = file_search(!SAVS_DIR + 'time_series/hist1d/' + dum_file,count = found) & $
						if found ne 1 then begin & $
							print,'Missing: '+dum_file & $
							count++ & $
						endif & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	print,count, ' 1D histogram files are Missing!'

	;2d histos
	ref   = ['gac2','myd2','mod2','cci','pmx','gac','myd','mod']
	data  = 'hist2d_cot_ctp'
	satd  = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa18']
	count = 0
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(satd)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					if covd[j] eq 'full' then cov = '' else cov = covd[j]+'_' & $
					if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
					if ref[k] eq 'pmx' and total(sat eq ['noaa12','noaa15','noaa17']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
					if do_it then begin & $
						dum_file = data[l]+'_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
						ff = file_search(!SAVS_DIR + 'time_series/hist2d/' + dum_file,count = found) & $
						if found ne 1 then begin & $
							print,'Missing: '+dum_file & $
							count++ & $
						endif & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	print,count, ' 2D histogram files are Missing!'


	print,'Number of Orbits'
	
	ff = file_search('/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C','*.nc',count=nff)
	for ii = 0, nff -1 do begin & $
		num = get_ncdf_data_by_name(ff[ii],'number_of_processed_orbits',/global)  & $
		if stregex(ff[ii],'modis',/fold,/bool) then begin  & $
			print, string(num)+' '+ff[ii]  & $
		endif else begin  & $
			if num lt 200 then print,string(num)+' '+ff[ii]  & $
		endelse  & $
	endfor

end
