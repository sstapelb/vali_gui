pro check_availability_of_time_series,print_missing_files=print_missing_files

	print_it = keyword_set(print_missing_files)
	satellites = ['noaa7','noaa9','noaa11','noaa12','noaa14','noaa14','noaa15','noaa16','noaa18','noaaam','noaapm']

	coverage = ['midlat_trop','full','southern_hemisphere','northern_hemisphere','antarctica','midlat_south','tropic','midlat_north','arctic']
	;compare
	cli  = 'cci'
	covd = [coverage,coverage+'_land',coverage+'_sea']
	ref  = ['gac2','pmx','gac','myd2','mod2']
	data = ['cfc','cfc_day','cfc_night','cfc_low','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	sat  = satellites
	count = 0l
	count_ok = 0l
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(sat)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					cov = covd[j]+'_' & $
					if covd[j] eq 'full' then cov = '' & $
					if covd[j] eq 'full_land' then cov = 'land_' & $
					if covd[j] eq 'full_sea' then cov = 'sea_' & $
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
							if print_it then print,'Missing: '+dum_file & $
							count++ & $
						endif else count_ok++ & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	count_compare = count
	count_ok_compare = count_ok

	;plot
	covd = [coverage,coverage+'_land',coverage+'_sea']
	ref  = ['gac2','pmx','gac','myd2','mod2','cci']
	data = ['cfc','cfc_day','cfc_night','cfc_twl','cfc_low','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	satd = satellites

	count = 0l
	count_ok = 0l
	for j=0,n_elements(covd) -1 do begin & $
		for i= 0,n_elements(satd)-1 do begin & $
			for k=0,n_elements(ref)-1 do begin & $
				for l=0,n_elements(data)-1 do begin & $
					do_it = 1 & $
					cov = covd[j]+'_' & $
					if covd[j] eq 'full' then cov = '' & $
					if covd[j] eq 'full_land' then cov = 'land_' & $
					if covd[j] eq 'full_sea' then cov = 'sea_' & $
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
							if print_it then print,'Missing: '+dum_file & $
							count++ & $
						endif else count_ok++ & $
					endif & $
				endfor & $
			endfor & $
		endfor & $
	endfor

	count_plot = count
	count_ok_plot = count_ok

	;hovmoeller
	ref  = ['gac2','pmx','gac','myd2','mod2','cci']
	data = ['cfc','cfc_day','cfc_night','cfc_low','cfc_twl','cfc_mid','cfc_high','ctp','ctt','cot','cer','cth','lwp','iwp','cwp','cph']
	satd = satellites
	count = 0l
	count_ok = 0l
	for i= 0,n_elements(satd)-1 do begin & $
		for k=0,n_elements(ref)-1 do begin & $
			for l=0,n_elements(data)-1 do begin & $
				do_it = 1 & $
				if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
				if total(data[l] eq ['cfc_low','cfc_mid','cfc_high','cer','cfc_twl']) and ref[k] eq 'gac' then do_it = 0 & $
				if total(data[l] eq ['cfc_day','cfc_night','cth','cwp','cph','cer','cfc_twl']) and ref[k] eq 'pmx' then do_it = 0 & $
				if ref[k] eq 'pmx' and sat eq 'noaa17' then do_it = 0 & $ ; kein n17 in patmos 
				if total(data[l] eq ['cot','lwp','iwp']) and ref[k] eq 'pmx' and total(sat eq ['noaa12','noaa15']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
				if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high','cfc_twl']) and strmid(ref[k],0,3) eq 'myd' then do_it = 0 & $
				if total(data[l] eq ['cth','cfc_low','cfc_mid','cfc_high','cfc_twl']) and strmid(ref[k],0,3) eq 'mod' then do_it = 0 & $
				if do_it then begin & $
					dum_file = data[l]+'_hovmoeller_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
					ff = file_search(!SAVS_DIR + 'time_series/hovmoeller/' + dum_file,count = found) & $
					if found ne 1 then begin & $
						if print_it then print,'Missing: '+dum_file & $
						count++ & $
					endif else count_ok++ & $
				endif & $
			endfor & $
		endfor & $
	endfor

	count_hovm = count
	count_ok_hovm = count_ok

	;1d histos
	ref   = ['gac2','myd2','mod2','cci']
	data  = 'hist1d_'+['cwp','cot','ctp','ctt','cer','cwp_liq','cot_liq','ctp_liq','ctt_liq','cer_liq','cwp_ice','cot_ice','ctp_ice','ctt_ice','cer_ice']
	satd  = satellites
	count = 0l
	count_ok = 0l
	for i= 0,n_elements(satd)-1 do begin & $
		for k=0,n_elements(ref)-1 do begin & $
			for l=0,n_elements(data)-1 do begin & $
				do_it = 1 & $
				if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
				if total(data[l] eq ['hist1d_ctp_ice','hist1d_ctp_liq']) and (ref[k] eq 'myd2' or ref[k] eq 'mod2') then do_it = 0 & $
				if do_it then begin & $
					dum_file = data[l]+'_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
					ff = file_search(!SAVS_DIR + 'time_series/hist1d/' + dum_file,count = found) & $
					if found ne 1 then begin & $
						if print_it then print,'Missing: '+dum_file & $
						count++ & $
					endif else count_ok++ & $
				endif & $
			endfor & $
		endfor & $
	endfor

	count_h1d = count
	count_ok_h1d = count_ok

	;2d histos
	ref   = ['gac2','myd2','mod2','cci','pmx','gac','myd','mod']
	data  = ['hist2d_cot_ctp','hist2d_cot_ctp_liq','hist2d_cot_ctp_ice']
	satd  = satellites
	count = 0l
	count_ok = 0l
	for i= 0,n_elements(satd)-1 do begin & $
		for k=0,n_elements(ref)-1 do begin & $
			for l=0,n_elements(data)-1 do begin & $
				do_it = 1 & $
				if total(ref[k] eq ['myd','mod','myd2','mod2']) then sat = '' else sat = satd[i] & $
				if ref[k] eq 'pmx' and total(sat eq ['noaa12','noaa15','noaa17']) then do_it = 0 & $ ; nur PM sats haben microphysic in patmos
				if do_it then begin & $
					dum_file = data[l]+'_1978-2016_'+ref[k]+'_'+sat+'.sav' & $
					ff = file_search(!SAVS_DIR + 'time_series/hist2d/' + dum_file,count = found) & $
					if found ne 1 then begin & $
						if print_it then print,'Missing: '+dum_file & $
						count++ & $
					endif else count_ok++ & $
				endif & $
			endfor & $
		endfor & $
	endfor

	count_h2d    = count
	count_ok_h2d = count_ok

	print,'# Compare TS      : Found '+string(count_ok_compare,f='(i7)')+' Files, '+string(count_compare,f='(i7)')+' files are Missing!'
	print,'# Plot TS         : Found '+string(count_ok_plot,f='(i7)')   +' Files, '+string(count_plot,f='(i7)')+' files are Missing!'
	print,'# Hovmoeller TS   : Found '+string(count_ok_hovm,f='(i7)')   +' Files, '+string(count_hovm,f='(i7)')+' files are Missing!'
	print,'# 1D Histogram TS : Found '+string(count_ok_h1d,f='(i7)')    +' Files, '+string(count_h1d,f='(i7)')+' files are Missing!'
	print,'# 2D Histogram TS : Found '+string(count_ok_h2d,f='(i7)')    +' Files, '+string(count_h2d,f='(i7)')+' files are Missing!'

	print,'Number of L3C files with lt 100 Orbits: '

	ff = file_search('/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C','*.nc',count=nff)
	for ii = 0, nff -1 do begin & $
		num = get_ncdf_data_by_name(ff[ii],'number_of_processed_orbits',/global)  & $
		if num lt 100 then print,string(num)+' '+ff[ii]  & $
	endfor

end
