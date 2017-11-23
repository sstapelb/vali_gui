;-------------------------------------------------------------------------------------------------------------------------
function get_product_name, data, algo=algo, upper_case = upper_case, lower_case = lower_case, level = level, path=path ,h_types=h_types, node = node

	if n_params() ne 1 then return,'-1'
	if ~keyword_set(algo) then return, (keyword_set(upper_case) ? strupcase(data) : strlowcase(data))
	alg = strlowcase(algo[0])
	lev = keyword_set(level) ? strlowcase(level) : ''
	if keyword_set(level) then begin
		if (strmid(alg,0,6) eq 'patmos' or strmid(alg,0,3) eq 'pmx') and (lev eq 'l3c' or lev eq 'l3s') then alg = 'gewex'
	endif

	is_gewex = total(alg eq ['gewex','gwx','gac2-gewex','g2_gwx','isccp'])
	if is_gewex then alg = 'gewex'

	dat = strlowcase(data[0])

	is_stdd = (reverse(strsplit(dat,'_',/ext)))[0] eq 'std'
	if is_stdd then dat = strreplace(dat,'_std','')

	if total(alg eq ['cal','calipso']) then begin
	       case dat of
			'cfc'		: dat = 'cfc_allclouds'
; 			'cfc'		: dat = 'cfc_allclouds_max'
			'cfc_day'	: dat = 'cfc_allclouds_day'
			'cfc_night'	: dat = 'cfc_allclouds_night'
			'cfc_high'	: dat = 'cfc_allclouds_high'
			'cfc_mid'	: dat = 'cfc_allclouds_mid'
			'cfc_low'	: dat = 'cfc_allclouds_low'
			'ctp'		: dat = 'ctp_mean_all'
			'cph'		: dat = 'liq_cloud_fraction_all'
			else		:
		endcase
	endif

	if total(alg eq ['cla','claas']) then begin
		if keyword_set(path) then begin
			datd = dat
			if strmid(dat,0,3) eq 'cph' 	then datd = 'cph'
			if dat eq 'cfc_day'			then datd = 'cfc'
			if dat eq 'cfc_night' 		then datd = 'cfc'
			if dat eq 'cfc_twl' 		then datd = 'cfc'
			if dat eq 'cfc_middle' 		then datd = 'cfc'
			if dat eq 'cfc_high' 		then datd = 'cfc'
			if dat eq 'cfc_low' 		then datd = 'cfc'
			if dat eq 'cfc_mid' 		then datd = 'cfc'
			if dat eq 'cfc_cloudsgt03'	then datd = 'cfc'
			if dat eq 'cfc_cloudsgt02'	then datd = 'cfc'
			if dat eq 'cfc_cloudsgt01'	then datd = 'cfc'
			if dat eq 'cfc_allclouds'	then datd = 'cfc'
			if dat eq 'cfc_allclouds_day'	then datd = 'cfc'
			if dat eq 'cfc_allclouds_night'	then datd = 'cfc'
			if dat eq 'lwp_allsky'  	then datd = 'cwp'
			if dat eq 'iwp_allsky'  	then datd = 'cwp'
			if dat eq 'lwp'  		then datd = 'cwp'
			if dat eq 'iwp'  		then datd = 'cwp'
			if dat eq 'cwp_allsky'  	then datd = 'cfc'
			if dat eq 'ref' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cer' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cot' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cot_ice' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cot_liq' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'ref_ice' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'ref_liq' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cer_ice' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if dat eq 'cer_liq' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if total(strmid(dat,0,10) eq ['hist1d_ctp','hist1d_ctt','nobs','nobs_asc','nobs_desc']) then datd = 'cto'
			if dat eq 'nobs_day' and total(lev eq ['l3c','l3s']) then datd = 'cwp'
			if strmid(dat,0,6) eq 'hist2d' then datd = 'jch'
			if total(strmid(dat,0,10) eq ['hist1d_cwp','hist1d_cot','hist1d_ref','hist1d_cer']) then datd = 'cwp'
 			if total(strmid(dat,0,3) eq ['ctp','ctt','cth']) then datd = 'cto'
 			if total(dat eq ['satzen','satza','pixel_area','alt','altitude','lsm','land_sea','land_sea_mask']) then datd = 'aux'
			return, keyword_set(upper_case) ? strupcase(datd) : strlowcase(datd)
		endif else begin
			case dat of
				'cer'        		: dat = 'ref'
				'cer_ice'    		: dat = 'ref_ice'
				'cer_liq'    		: dat = 'ref_liq'
				'hist1d_cer' 		: dat = 'hist1d_ref'
				'cfc_cloudsgt03'	: dat = 'cfc'
				'cfc_cloudsgt02'	: dat = 'cfc'
				'cfc_cloudsgt01'	: dat = 'cfc'
				'cfc_allclouds'		: dat = 'cfc'
				'cfc_allclouds_day'	: dat = 'cfc_day'
				'cfc_allclouds_night': dat = 'cfc_night'
				'satza'				: dat = 'satzen'
				'altitude'			: dat = 'alt'
				'land_sea'			: dat = 'lsm'
				'land_sea_mask'		: dat = 'lsm'
				else				:
			endcase
		endelse
	endif
	if total(alg eq ['gac2','clara2','hec','hector']) then begin
		if keyword_set(path) then begin
			datd = dat
			; Martins Calipso Jahresmittel
			if dat eq 'cfc_cloudsgt03'	then datd = 'cfc'
			if dat eq 'cfc_cloudsgt02'	then datd = 'cfc'
			if dat eq 'cfc_cloudsgt01'	then datd = 'cfc'
			if dat eq 'cfc_allclouds'	then datd = 'cfc'
			if dat eq 'cfc_allclouds_day'	then datd = 'cfc'
			if dat eq 'cfc_allclouds_night'	then datd = 'cfc'
			; ------------
			if dat eq 'refl_vis006_asc'	then datd = 'cac'
			if dat eq 'refl_vis008_asc'	then datd = 'cac'
			if dat eq 'refl_nir016_asc'	then datd = 'cac'
			if dat eq 'bt_nir037_asc'	then datd = 'cac'
			if dat eq 'bt_tir108_asc'	then datd = 'cac'
			if dat eq 'bt_tir120_asc'	then datd = 'cac'
			if dat eq 'refl1_asc'		then datd = 'cac'
			if dat eq 'refl2_asc'		then datd = 'cac'
			if dat eq 'refl3a_asc'		then datd = 'cac'
			if dat eq 'refl3b_asc'		then datd = 'cac'
			if dat eq 'rad3b_asc'		then datd = 'cac'
			if dat eq 'rad4_asc'		then datd = 'cac'
			if dat eq 'rad5_asc'		then datd = 'cac'
			if dat eq 'refl_vis006_desc'	then datd = 'cac'
			if dat eq 'refl_vis008_desc'	then datd = 'cac'
			if dat eq 'refl_nir016_desc'	then datd = 'cac'
			if dat eq 'bt_nir037_desc'	then datd = 'cac'
			if dat eq 'bt_tir108_desc'	then datd = 'cac'
			if dat eq 'bt_tir120_desc'	then datd = 'cac'
			if dat eq 'refl1_desc'		then datd = 'cac'
			if dat eq 'refl2_desc'		then datd = 'cac'
			if dat eq 'refl3a_desc'		then datd = 'cac'
			if dat eq 'refl3b_desc'		then datd = 'cac'
			if dat eq 'rad3b_desc'		then datd = 'cac'
			if dat eq 'rad4_desc'		then datd = 'cac'
			if dat eq 'rad5_desc'		then datd = 'cac'
			; ------------
			if dat eq 'cc_total' 		then datd = 'cfc'
			if dat eq 'cc_total_day' 	then datd = 'cfc'
			if dat eq 'cc_total_night' 	then datd = 'cfc'
			if dat eq 'cc_total_twl' 	then datd = 'cfc'
			if dat eq 'cfc_day' 		then datd = 'cfc'
			if dat eq 'cfc_night' 		then datd = 'cfc'
			if dat eq 'cfc_twl' 		then datd = 'cfc'
			if dat eq 'cfc_middle' 		then datd = 'cfc'
			if dat eq 'cfc_high' 		then datd = 'cfc'
			if dat eq 'cfc_low' 		then datd = 'cfc'
			if dat eq 'cfc_mid' 		then datd = 'cfc'
			if dat eq 'cfc' and lev eq 'l3u' then datd = 'cma'
			if dat eq 'cc_mask_asc'   	then datd = 'cma'
			if dat eq 'cc_mask_desc'	then datd = 'cma'
			if dat eq 'cmask_asc'   	then datd = 'cma'
			if dat eq 'cmask_desc'  	then datd = 'cma'
			if dat eq 'ref_asc'   		then datd = 'cwp'
			if dat eq 'ref_desc'  		then datd = 'cwp'
			if dat eq 'cer_asc'   		then datd = 'cwp'
			if dat eq 'cer_desc'  		then datd = 'cwp'
			if dat eq 'cot_asc'   		then datd = 'cwp'
			if dat eq 'cot_desc'  		then datd = 'cwp'
			if dat eq 'cwp_asc'   		then datd = 'cwp'
			if dat eq 'cwp_desc'  		then datd = 'cwp'
			if dat eq 'cdnc_asc'   		then datd = 'cwp'
			if dat eq 'cdnc_desc'  		then datd = 'cwp'
			if strmid(dat,0,3) eq 'lwp' 	then datd = 'lwp'
			if strmid(dat,0,3) eq 'iwp' 	then datd = 'iwp'
 			if dat eq 'cwp' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cwp_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
 			if dat eq 'cwp_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cwp_allsky' and total(lev eq ['l3c','l3s']) then datd = 'cfc'
			if total(strmid(dat,0,6) eq ['solzen','sunzen','satzen','relazi','sungli','glint_','scanli','time_a','time_d']) then datd = 'caa'
			if total(strmid(dat,0,3) eq ['ctp','ctt','cth']) then datd = 'cto'
			if total(strmid(dat,0,10) eq ['hist1d_ctp','hist1d_ctt','nobs_asc','nobs_desc']) then datd = 'cto'
; 			if dat eq 'nobs' and total(lev eq ['l3c','l3s']) then datd = 'cfc'
			if dat eq 'nobs' and total(lev eq ['l3c','l3s']) then datd = 'cto'
			if dat eq 'nobs_day' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if strmid(dat,0,6) eq 'hist2d' then datd = 'jch'
			if total(strmid(dat,0,10) eq ['hist1d_cwp','hist1d_cot','hist1d_ref','hist1d_cer']) then datd = 'cwp'
			if dat eq 'cot' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_ice' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_liq' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cot_ice_log' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_liq_log' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'ref_ice' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref_liq' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cer_ice' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer_liq' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cot_ice_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_liq_error' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'ref_ice_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref_liq_error' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cer_ice_error' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer_liq_error' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cot_ice_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cot_liq_unc' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'ref_ice_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'ref_liq_unc' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if dat eq 'cer_ice_unc' and total(lev eq ['l3c','l3s']) then datd = 'iwp'
			if dat eq 'cer_liq_unc' and total(lev eq ['l3c','l3s']) then datd = 'lwp'
			if strmid(dat,0,8) eq 'cld_type' or strmid(dat,0,10) eq 'cloud_type' or strmid(dat,0,3) eq 'cty' then datd = 'cph
			if strmid(dat,0,3) eq 'cph' then datd = 'cph'
			return, keyword_set(upper_case) ? strupcase(datd) : strlowcase(datd)
		endif else begin
			case dat of
				; Martins Calipso Jahresmittel
				'cfc_cloudsgt03'	: dat = 'cfc'
				'cfc_cloudsgt02'	: dat = 'cfc'
				'cfc_cloudsgt01'	: dat = 'cfc'
				'cfc_allclouds'		: dat = 'cfc'
				'cfc_allclouds_day'	: dat = 'cfc_day'
				'cfc_allclouds_night'	: dat = 'cfc_night'
				;------------------------------------------
				'cfc_mid' 		: dat = 'cfc_middle'
				'cc_total_mid' 		: dat = 'cfc_middle'
				'cc_total' 		: dat = 'cfc'
				'cc_total_day' 		: dat = 'cfc_day'
				'cc_total_night' 	: dat = 'cfc_night'
				'cc_total_twl' 		: dat = 'cfc_twl'
				'jch'			: dat = 'hist2d_cot_ctp'
				'cot_ctp_hist2d'	: dat = 'hist2d_cot_ctp'
				'cot_ctp_hist2d_liq'	: dat = 'hist2d_cot_ctp_liq'
				'cot_ctp_hist2d_ice'	: dat = 'hist2d_cot_ctp_ice'
				'h_cod_cp'		: dat = 'hist2d_cot_ctp'
				'jch_liq'		: dat = 'hist2d_cot_ctp_liq'
				'solzen_asc'		: dat = 'sunzen_asc'
				'solzen_desc'		: dat = 'sunzen_desc'
				'npoints_l2b_asc'	: dat = 'nobs_asc'
				'npoints_l2b_desc'	: dat = 'nobs_desc'
				'jch_ice'		: dat = 'hist2d_cot_ctp_ice'
				'cloud_type_asc'	: dat = 'cph_extended_asc'
				'cloud_type_desc'	: dat = 'cph_extended_desc'
				'cty_asc'		: dat = 'cph_extended_asc'
				'cty_desc'		: dat = 'cph_extended_desc'
				'cld_type_asc'		: dat = 'cph_extended_asc'
				'cld_type_desc'		: dat = 'cph_extended_desc'
				'cld_type'		: dat = 'cph_extended'
				'cmask_asc'   		: dat = 'cc_mask_asc'
				'cmask_desc'  		: dat = 'cc_mask_desc'
				'time_asc'		: dat = 'scanline_time_asc'
				'time_desc'		: dat = 'scanline_time_desc'
				'cer_ice'		: dat = 'ref_ice'
				'cer_liq'		: dat = 'ref_liq'
				'cer_asc'   	: dat = 'ref_asc'
				'cer_desc'  	: dat = 'ref_desc'
				'cer'			: dat = 'ref'
				'lwp_unc'		: dat = 'lwp_error'
				'iwp_unc'		: dat = 'iwp_error'
				'cwp_unc'		: dat = 'cwp_error'
				'cer_ice_error'		: dat = 'ref_ice_error'
				'cer_liq_error'		: dat = 'ref_liq_error'
				'cer_error'		: dat = 'ref_error'
				'cer_ice_unc'		: dat = 'ref_ice_error'
				'cer_liq_unc'		: dat = 'ref_liq_error'
				'cer_unc'		: dat = 'ref_error'
				'ref_ice_unc'		: dat = 'ref_ice_error'
				'ref_liq_unc'		: dat = 'ref_liq_error'
				'ref_unc'		: dat = 'ref_error'
				'cot_ice_unc'		: dat = 'cot_ice_error'
				'cot_liq_unc'		: dat = 'cot_liq_error'
				'cot_unc'		: dat = 'cot_error'
				'hist1d_cer'		: dat = 'hist1d_ref'
				'hist1d_cer_liq'	: dat = 'hist1d_ref_liq'
				'hist1d_cer_ice'	: dat = 'hist1d_ref_ice'
				'refl1_asc'		: dat = 'refl_vis006_asc'
				'refl2_asc'		: dat = 'refl_vis008_asc'
				'refl3a_asc'		: dat = 'refl_nir016_asc'
				'rad3b_asc'		: dat = 'bt_nir037_asc'
				'rad4_asc'		: dat = 'bt_tir108_asc'
				'rad5_asc'		: dat = 'bt_tir120_asc'
				'refl1_desc'		: dat = 'refl_vis006_desc'
				'refl2_desc'		: dat = 'refl_vis008_desc'
				'refl3a_desc'		: dat = 'refl_nir016_desc'
				'rad3b_desc'		: dat = 'bt_nir037_desc'
				'rad4_desc'		: dat = 'bt_tir108_desc'
				'rad5_desc'		: dat = 'bt_tir120_desc'
				'nobs_day' 		: dat = 'nobs'
				else			:
			endcase
		endelse
	endif
	if total(alg eq ['gac','clara']) then begin
		if keyword_set(path) then begin
			if dat eq 'lwp_allsky'  then dat = 'lwp'
			if dat eq 'iwp_allsky'  then dat = 'iwp'
			if dat eq 'cwp_allsky'  then dat = 'cfc'
 			if dat eq 'cwp'         then dat = 'iwp'
			if dat eq 'cfc_day'     then dat = 'cfc'
			if dat eq 'cfc_night'   then dat = 'cfc'
			if strmid(dat,0,6) eq 'hist2d' then dat = 'jch'
			if total(strmid(dat,0,3) eq ['ctp','ctt','cth']) then dat = 'cto'
			return, keyword_set(upper_case) ? strupcase(dat) : strlowcase(dat)
		endif else begin
			case dat of
				'cfc_cloudsgt03'	: dat = 'cfc'
				'cfc_cloudsgt02'	: dat = 'cfc'
				'cfc_cloudsgt01'	: dat = 'cfc'
				'cfc_allclouds'		: dat = 'cfc'
				'cfc_allclouds_day'	: dat = 'cfc_day'
				'cfc_allclouds_night'	: dat = 'cfc_night'
				'cc_total_day'		: dat = 'cfc_day'
				'cc_total_night'	: dat = 'cfc_night'
				'cot_ctp_hist2d'	: dat = 'jch'
				'h_cod_cp'		: dat = 'jch'
				'h_codw_cp'		: dat = 'jch_liq'
				'h_codi_cp'		: dat = 'jch_ice'
				'hist2d_cot_ctp'	: dat = 'jch'
				'cot_ctp_hist2d_liq'	: dat = 'jch_liq'
				'hist2d_cot_ctp_liq'	: dat = 'jch_liq'
				'cot_ctp_hist2d_ice'	: dat = 'jch_ice'
				'hist2d_cot_ctp_ice'	: dat = 'jch_ice'
				'cot_ctp_hist2d_ratio'	: dat = 'jch_ratio'
				'hist2d_cot_ctp_ratio'	: dat = 'jch_ratio'
				'cot_ctp_hist2d_mixed'	: dat = 'jch_mixed'
				'hist2d_cot_ctp_mixed'	: dat = 'jch_mixed'
				'hist2d_cot_ctp2'	: dat = 'jch'
				'hist2d_cot_ctp2_liq'	: dat = 'jch_liq'
				'hist2d_cot_ctp2_ice'	: dat = 'jch_ice'
				'hist2d_cot_ctp2_ratio'	: dat = 'jch_ratio'
				'hist2d_cot_ctp2_mixed'	: dat = 'jch_mixed'
				'cc_total' 		: dat = 'cfc'
				'a_ca'			: dat = 'cfc'
				'cwp_ice'		: dat = 'iwp'
				'cwp_liq'		: dat = 'lwp'
				'cph_day'		: dat = 'cph'
				else			:
			endcase
			dat = (total(dat eq ['ctp','ctt','cth']) ? dat+'_arith_mean' : dat)
			dat = (total(dat eq ['ctp_std','ctt_std','cth_std']) ? strmid(dat[0],0,3)+'_arith_stdv' : dat)
		endelse
	endif
	if total(alg eq ['coll6','coll5','mod','myd','mod2','myd2']) then begin
		case dat of
			'ctp_corrected'		: dat = 'ctp'
			'cth_corrected'		: dat = 'cth'
			'ctt_corrected'		: dat = 'ctt'
			'cfc_cloudsgt03'	: dat = 'cfc'
			'cfc_cloudsgt02'	: dat = 'cfc'
			'cfc_cloudsgt01'	: dat = 'cfc'
			'cfc_allclouds'		: dat = 'cfc'
			'cfc_allclouds_day'	: dat = 'cfc_day'
			'cfc_allclouds_night'	: dat = 'cfc_night'
			'cc_total'		: dat = 'cfc'
			'cc_total_day'		: dat = 'cfc_day'
			'cc_total_night'	: dat = 'cfc_night'
			'cwp_ice'		: dat = 'iwp'
			'cwp_liq'		: dat = 'lwp'
			'hist2d_cot_ctp'	: dat = 'cot_ctp_hist2d'
			'hist2d_cot_ctp_liq'	: dat = 'cot_ctp_hist2d_liq'
			'hist2d_cot_ctp_ice'	: dat = 'cot_ctp_hist2d_ice'
			'h_cod_cp'		: dat = 'cot_ctp_hist2d'
			'jch'			: dat = 'cot_ctp_hist2d'
			'jch_liq'		: dat = 'cot_ctp_hist2d_liq'
			'jch_ice'		: dat = 'cot_ctp_hist2d_ice'
; 			'cer_liq'		: dat = 'cer_37_liq'
; 			'cer_ice'		: dat = 'cer_37_ice'
; 			'ref_liq'		: dat = 'cer_37_liq'
; 			'ref_ice'		: dat = 'cer_37_ice'
			'ref_liq'		: dat = 'cer_liq'
			'ref_ice'		: dat = 'cer_ice'
			else	:
		endcase
	endif

	if alg eq 'esacci_old' or alg eq 'cci_old' then begin ; version 1.4
		case dat of
			'cer'			: dat = 'ref'
			'cer_liq'		: dat = 'ref_liq'
			'cer_ice'		: dat = 'ref_ice'
			'cfc'			: dat = 'cc_total'
			'a_cod'			: dat = 'cot_log'
			'cwp_ice'		: dat = 'iwp'
			'cwp_liq'		: dat = 'lwp'
			'hist2d_cot_ctp'	: dat = 'cot_ctp_hist2d'
			'hist2d_cot_ctp_liq'	: dat = 'cot_ctp_hist2d_liq'
			'hist2d_cot_ctp_ice'	: dat = 'cot_ctp_hist2d_ice'
			'h_cod_cp'		: dat = 'cot_ctp_hist2d'
			'jch'			: dat = 'cot_ctp_hist2d'
			'jch_liq'		: dat = 'cot_ctp_hist2d_liq'
			'jch_ice'		: dat = 'cot_ctp_hist2d_ice'
			else			:
		endcase
	endif
	if alg eq 'esacci' or alg eq 'cci' then begin
		case dat of
			; Martins Calipso Jahresmittel
			'cfc_cloudsgt03'	: dat = 'cfc'
			'cfc_cloudsgt02'	: dat = 'cfc'
			'cfc_cloudsgt01'	: dat = 'cfc'
			'cfc_allclouds'		: dat = 'cfc'
			'cfc_allclouds_day'	: dat = 'cfc_day'
			'cfc_allclouds_night'	: dat = 'cfc_night'
			'ctp_mean_all'		: dat = 'ctp'
			'ctp_mean_ice'		: dat = 'ctp'
			'ctp_mean_liq'		: dat = 'ctp'
			'ctp_mean_th_ice'	: dat = 'ctp'
			'ctp_mean_sc_liq'	: dat = 'ctp'
			;------------------------------------------
			'a_cod'			: dat = 'cot_log'
			'h_cp'			: dat = 'hist1d_ctp'
			'h_ct'			: dat = 'hist1d_ctt'
			'h_cod'			: dat = 'hist1d_cot'
			'h_codw'		: dat = 'hist1d_cot_liq'
			'h_codi'		: dat = 'hist1d_cot_ice'
			'h_ctw'			: dat = 'hist1d_ctt_liq'
			'h_cti'			: dat = 'hist1d_ctt_ice'
			'h_clwp'		: dat = 'hist1d_cwp_liq'
			'h_ciwp'		: dat = 'hist1d_cwp_ice'
			'h_crew'		: dat = 'hist1d_cer_liq'
			'h_crei'		: dat = 'hist1d_cer_ice'
			'cwp_ice'		: dat = 'iwp'
			'cwp_liq'		: dat = 'lwp'
			'cot_ctp_hist2d': dat = 'hist2d_cot_ctp'
			'cot_ctp_hist2d_liq'	: dat = 'hist2d_cot_ctp_liq'
			'cot_ctp_hist2d_ice'	: dat = 'hist2d_cot_ctp_ice'
			'h_cod_cp'		: dat = 'hist2d_cot_ctp'
			'jch'			: dat = 'hist2d_cot_ctp'
			'jch_liq'		: dat = 'hist2d_cot_ctp_liq'
			'jch_ice'		: dat = 'hist2d_cot_ctp_ice'
			'cph_daynight'	: dat = 'cph'
			'sunzen_asc'	: dat = 'solzen_asc'
			'sunzen_desc'	: dat = 'solzen_desc'
			'sunza_asc'		: dat = 'solzen_asc'
			'sunza_desc'	: dat = 'solzen_desc'
			'satza_asc'		: dat = 'satzen_asc'
			'satza_desc'	: dat = 'satzen_desc'
			'refl1_asc'			: dat = 'refl_vis006_asc'
			'refl2_asc'			: dat = 'refl_vis008_asc'
			'refl3a_asc'		: dat = 'refl_vis016_asc'
			'refl_nir016_asc'	: dat = 'refl_vis016_asc'
			'rad3b_asc'			: dat = 'bt_nir037_asc'
			'rad4_asc'			: dat = 'bt_tir108_asc'
			'rad5_asc'			: dat = 'bt_tir120_asc'
			'refl1_desc'		: dat = 'refl_vis006_desc'
			'refl2_desc'		: dat = 'refl_vis008_desc'
			'refl3a_desc'		: dat = 'refl_vis016_desc'
			'refl_nir016_desc'	: dat = 'refl_vis016_desc'
			'rad3b_desc'		: dat = 'bt_nir037_desc'
			'rad4_desc'			: dat = 'bt_tir108_desc'
			'rad5_desc'			: dat = 'bt_tir120_desc'
			'cph'				: begin & if lev eq 'l2' then dat = 'phase' & end
			'solzen'			: begin & if lev eq 'l2' then dat = 'solar_zenith_view_no1' & end
			'sunzen'			: begin & if lev eq 'l2' then dat = 'solar_zenith_view_no1' & end
			'sunza' 			: begin & if lev eq 'l2' then dat = 'solar_zenith_view_no1' & end
			'satzen'			: begin & if lev eq 'l2' then dat = 'satellite_zenith_view_no1' & end
			'satza'				: begin & if lev eq 'l2' then dat = 'satellite_zenith_view_no1' & end
			'relazi'			: begin & if lev eq 'l2' then dat = 'rel_azimuth_view_no1' & end
			else			:
		endcase
	endif
	if alg eq 'esacciv3' or alg eq 'cciv3' then begin
		if keyword_set(path) then begin ; the new L3u files are splitted!!!
			dumdat = 'cld_products'
			;cloud mask
			if stregex(dat,'cph_',/bool)    then dumdat = 'cld_masktype'
			if stregex(dat,'cty_',/bool)    then dumdat = 'cld_masktype'
			if stregex(dat,'cccot_',/bool)  then dumdat = 'cld_masktype'
			if stregex(dat,'cmask_',/bool)  then dumdat = 'cld_masktype'
			if stregex(dat,'cee_',/bool)    then dumdat = 'rad_products'
			if stregex(dat,'cla_',/bool)    then dumdat = 'rad_products'
			if stregex(dat,'boa_',/bool)    then dumdat = 'rad_products'
			if stregex(dat,'toa_',/bool)    then dumdat = 'rad_products'
			if stregex(dat,'illum_',/bool)  then dumdat = 'cld_angles'
			if stregex(dat,'satzen_',/bool) then dumdat = 'cld_angles'
			if stregex(dat,'satza_',/bool)  then dumdat = 'cld_angles'
			if stregex(dat,'solzen_',/bool) then dumdat = 'cld_angles'
			if stregex(dat,'sunzen_',/bool) then dumdat = 'cld_angles'
			if stregex(dat,'sunza_',/bool)  then dumdat = 'cld_angles'
			if stregex(dat,'relazi_',/bool) then dumdat = 'cld_angles'
			if stregex(dat,'refl_vis006_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl_vis008_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl_vis016_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl_nir016_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'bt_nir037_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'bt_tir108_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'bt_tir120_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl1_',/bool)  then dumdat = 'sat_measurements'
			if stregex(dat,'refl2_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl3a_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'refl3b_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'rad3b_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'rad4_',/bool) then dumdat = 'sat_measurements'
			if stregex(dat,'rad5_',/bool) then dumdat = 'sat_measurements'
			return, keyword_set(upper_case) ? strupcase(dumdat) : strlowcase(dumdat)
		endif else begin
			case dat of
				; Martins Calipso Jahresmittel
				'cfc_cloudsgt03'	: dat = 'cfc'
				'cfc_cloudsgt02'	: dat = 'cfc'
				'cfc_cloudsgt01'	: dat = 'cfc'
				'cfc_allclouds'		: dat = 'cfc'
				'cfc_allclouds_day'	: dat = 'cfc_day'
				'cfc_allclouds_night'	: dat = 'cfc_night'
				'ctp_mean_all'		: dat = 'ctp'
				'ctp_mean_ice'		: dat = 'ctp'
				'ctp_mean_liq'		: dat = 'ctp'
				'ctp_mean_th_ice'	: dat = 'ctp'
				'ctp_mean_sc_liq'	: dat = 'ctp'
				;------------------------------------------
				'refl1_asc'			: dat = 'refl_vis006_asc'
				'refl2_asc'			: dat = 'refl_vis008_asc'
				'refl3a_asc'		: dat = 'refl_vis016_asc'
				'refl_nir016_asc'	: dat = 'refl_vis016_asc'
				'rad3b_asc'			: dat = 'bt_nir037_asc'
				'rad4_asc'			: dat = 'bt_tir108_asc'
				'rad5_asc'			: dat = 'bt_tir120_asc'
				'refl1_desc'		: dat = 'refl_vis006_desc'
				'refl2_desc'		: dat = 'refl_vis008_desc'
				'refl3a_desc'		: dat = 'refl_vis016_desc'
				'refl_nir016_desc'	: dat = 'refl_vis016_desc'
				'rad3b_desc'		: dat = 'bt_nir037_desc'
				'rad4_desc'			: dat = 'bt_tir108_desc'
				'rad5_desc'			: dat = 'bt_tir120_desc'
				'sunzen_asc'		: dat = 'solzen_asc'
				'sunzen_desc'		: dat = 'solzen_desc'
				'sunza_asc'			: dat = 'solzen_asc'
				'sunza_desc'		: dat = 'solzen_desc'
				'satza_asc'			: dat = 'satzen_asc'
				'satza_desc'		: dat = 'satzen_desc'
				;------------------------------------------
				'a_cod'			: dat = 'cot_log'
				'h_cp'			: dat = 'hist1d_ctp'
				'h_ct'			: dat = 'hist1d_ctt'
				'h_cod'			: dat = 'hist1d_cot'
				'h_codw'		: dat = 'hist1d_cot_liq'
				'h_codi'		: dat = 'hist1d_cot_ice'
				'h_ctw'			: dat = 'hist1d_ctt_liq'
				'h_cti'			: dat = 'hist1d_ctt_ice'
				'h_clwp'		: dat = 'hist1d_cwp_liq'
				'h_ciwp'		: dat = 'hist1d_cwp_ice'
				'h_crew'		: dat = 'hist1d_cer_liq'
				'h_crei'		: dat = 'hist1d_cer_ice'
				'cwp_ice'		: dat = 'iwp'
				'cwp_liq'		: dat = 'lwp'
				'cot_ctp_hist2d': dat = 'hist2d_cot_ctp'
				'cot_ctp_hist2d_liq'	: dat = 'hist2d_cot_ctp_liq'
				'cot_ctp_hist2d_ice'	: dat = 'hist2d_cot_ctp_ice'
				'h_cod_cp'		: dat = 'hist2d_cot_ctp'
				'jch'			: dat = 'hist2d_cot_ctp'
				'jch_liq'		: dat = 'hist2d_cot_ctp_liq'
				'jch_ice'		: dat = 'hist2d_cot_ctp_ice'
				'cph_daynight'	: dat = 'cph'
				else			:
			endcase
		endelse
	endif
	; gilt auch für patmos l3c (=gewex)
	if alg eq 'gewex' or alg eq 'gwx' then begin
		if keyword_set(path) then begin
			if total(dat eq ['cer','ref']) then dat = 'a_crew' 
			if total(dat eq ['cwp']) then dat = 'a_clwp'
			if total(dat eq ['iwp_allsky']) then dat = 'a_ciwp'
			if total(dat eq ['lwp_allsky']) then dat = 'a_clwp'
			if total(dat eq ['cwp_allsky']) then dat = 'a_clwp'
			if total(dat eq ['hist1d_cot_ratio']) then dat = 'h_codw'
			if total(dat eq ['hist1d_ctt_ratio']) then dat = 'h_ctw'
			if total(dat eq ['hist2d_cot_ctp_ratio']) then dat = 'h_codw_cp'
			if total(dat eq ['hist1d_cwp_ratio','hist1d_cwp']) then dat = 'h_clwp'
			if total(dat eq ['hist1d_cer_ratio','hist1d_cer']) then dat = 'h_crew'
		endif
		case dat of
			'cot_ctp_hist2d': dat = 'h_cod_cp'
			'hist2d_cot_ctp': dat = 'h_cod_cp'
			'hist2d_cot_ctp_liq': dat = 'h_codw_cp'
			'hist2d_cot_ctp_ice': dat = 'h_codi_cp'
			'jch'		: dat = 'h_cod_cp'
			'hist1d_ctp'	: dat = 'h_cp'
			'hist1d_cot'	: dat = 'h_cod'
			'hist1d_cot_liq': dat = 'h_codw'
			'hist1d_cot_ice': dat = 'h_codi'
			'hist1d_ctt'	: dat = 'h_ct'
			'hist1d_ctt_liq': dat = 'h_ctw'
			'hist1d_ctt_ice': dat = 'h_cti'
			'hist1d_cer_liq': dat = 'h_crew'
			'hist1d_cer_ice': dat = 'h_crei'
			'hist1d_cwp_liq': dat = 'h_clwp'
			'hist1d_cwp_ice': dat = 'h_ciwp'
			'cfc'		: dat = 'a_ca'
			'cfc_cloudsgt03': dat = 'a_ca'
			'cfc_cloudsgt02': dat = 'a_ca'
			'cfc_cloudsgt01': dat = 'a_ca'
			'cfc_allclouds'	: dat = 'a_ca'
			'cc_total_liq'	: dat = 'a_caw'
			'cc_total_ice'	: dat = 'a_cai'
			'cfc_ice'	: dat = 'a_cai'
			'cfc_liq'	: dat = 'a_caw'
			'cfc_high'	: dat = 'a_cah'
			'cfc_mid'	: dat = 'a_cam'
			'cfc_middle'	: dat = 'a_cam'
			'cfc_low'	: dat = 'a_cal'
			'cloud_fraction': dat = 'a_ca'
			'cfc_day'	: dat = 'a_cad'
			'cot'		: dat = 'a_cod'
			'cot_liq'	: dat = 'a_codw'
			'cot_ice'	: dat = 'a_codi'
			'cot_liq_log': dat = 'a_codw'
			'cot_ice_log': dat = 'a_codi'
			'cot_log'	: dat = 'a_cod'
			'lwp'		: dat = 'a_clwp'
			'cwp_liq'	: dat = 'a_clwp'
			'iwp'		: dat = 'a_ciwp'
			'cwp_ice'	: dat = 'a_ciwp'
			'ref_liq'	: dat = 'a_crew'
			'ref_ice'	: dat = 'a_crei'
			'cer_liq'	: dat = 'a_crew'
			'cer_ice'	: dat = 'a_crei'
			'ctp'		: dat = 'a_cp'
			'ctt'		: dat = 'a_ct'
			'cph'		: dat = 'a_cawr'
			'cph_day'	: dat = 'a_cawdr'
			'cth'		: dat = 'a_cz'
			'cee'		: dat = 'a_cem'
			else		: 
		endcase
	endif
	if alg eq 'patmos' or alg eq 'pmx' or alg eq 'patmos_old' or alg eq 'pmx_old' then begin
		if lev eq 'l2' then begin
			case dat of 
				'phase'		: dat = 'cty'      
				'cmask'		: dat = 'cc_total'
				'cloudmask'	: dat = 'cc_total'
				'cloudmask_pre'	: dat = 'cc_total'
				'cc_mask'	: dat = 'cc_total'
				'cfc'		: dat = 'cc_total'
				else		:
			endcase
		endif else begin
			node  = strlowcase((reverse(strsplit(dat,'_',/ext)))[0])
			if total(node eq ['asc','des','desc']) then dat = strreplace(dat,'_'+node,'')
			case dat of 
				'satzen'	: dat = 'sensor_zenith_angle'
				'satza'		: dat = 'sensor_zenith_angle'
				'sunzen'	: dat = 'solar_zenith_angle'
				'sunza'		: dat = 'solar_zenith_angle'
				'solzen'	: dat = 'solar_zenith_angle'
				'relazi'	: dat = 'relative_azimuth_angle'
				'cfc'		: dat = 'cloud_fraction'
				'cc_total'	: dat = 'cloud_fraction'
				'cwp'		: dat = 'cloud_water_path'
				'cot'		: dat = 'cld_opd_dcomp'
				'ref'		: dat = 'cld_reff_dcomp'
				'cer'		: dat = 'cld_reff_dcomp'
				'ctp'		: dat = 'cld_press_acha'
				'ctt'		: dat = 'cld_temp_acha'
				'cth'		: dat = 'cld_height_acha'
				'cc_mask'	: dat = 'cloud_mask'
				'cmask'		: dat = 'cloud_mask'
				'cph'		: dat = 'cloud_phase'
				'phase'		: dat = 'cloud_phase'
				'cty'		: dat = 'cloud_type'
				'cph_extended'	: dat = 'cloud_type'
				else		: 
			endcase
		endelse
	endif
	if alg eq 'isccp_old' or alg eq 'isp_old' then begin
		case dat of 
			'data_set_6'	: dat = '6'
			'data_set_8'	: dat = '8'
			'data_set_10'	: dat = '10'
			'data_set_12'	: dat = '12'
			'data_set_14'	: dat = '14'
			'data_set_16'	: dat = '16'
			'data_set_18'	: dat = '18'
			'data_set_20'	: dat = '20'
			'data_set_22'	: dat = '22'
			'cfc'		: dat = '8' 
			'cc_total'	: dat = '8' 
			'ctp'		: dat = '20'
			'ctt'		: dat = '23' 
			'cot'		: dat = '26'
			'cwp'		: dat = '29'
			'cfc_low'	: dat = '32' 
			'cfc_mid'	: dat = '35' 
			'cfc_high'	: dat = '38' 
			'stemp'		: dat = '116'
			'spress'	: dat = '120'
			else		:
		endcase
	endif

	if is_stdd then dat = dat+'_std'
	if keyword_set(h_types) then dat = h_types+' '+dat
	return, keyword_set(upper_case) ? strupcase(dat) : strlowcase(dat)
end
;-------------------------------------------------------------------------------------------------------------------------
function is_jch, name, liquid = liquid, ice = ice, combined = combined, ratio=ratio, mixed = mixed

	if (keyword_set(liquid) + keyword_set(ice)) eq 2 then begin
		liquid = 0
		ice = 0
		combined = 1
	endif
	; liquid + ice
	if keyword_set(combined) then return, (get_product_name(name[0],alg='clara') eq 'jch')
	; only liquid
	if keyword_set(liquid)   then return, (get_product_name(name[0],alg='clara') eq 'jch_liq')
	; only ice
	if keyword_set(ice)      then return, (get_product_name(name[0],alg='clara') eq 'jch_ice')
	; ratio liq/(liq+ice)
	if keyword_set(ratio)    then return, (get_product_name(name[0],alg='clara') eq 'jch_ratio')
	; mixed / unknown so far Calipso only
	if keyword_set(mixed)    then return, (get_product_name(name[0],alg='clara') eq 'jch_mixed')
	; all jchs
	return, (strmid(get_product_name(name[0],alg='clara'),0,3) eq 'jch')
end
;-------------------------------------------------------------------------------------------------------------------------
function is_h1d, name, liquid = liquid, ice = ice, ratio = ratio, combined = combined, mixed = mixed

	dat = get_product_name(name[0],alg='cci')
	if (keyword_set(liquid) + keyword_set(ice)) eq 2 then begin
		liquid = 0
		ice = 0
		combined = 1
		mixed = 0
	endif

	if stregex(dat,'_bin_',/fold,/bool) then return,0
	hist1d     = strmid(dat,0,6) eq 'hist1d'
	hist1d_liq = hist1d and stregex(dat,'_liq',/bool)
	hist1d_ice = hist1d and stregex(dat,'_ice',/bool)
	hist1d_rat = hist1d and stregex(dat,'_ratio',/bool)
	hist1d_mix = hist1d and stregex(dat,'_mixed',/bool)
	hist1d_com = hist1d and ~hist1d_liq and ~hist1d_ice and ~hist1d_rat and ~hist1d_mix
	; only liquid
	if keyword_set(liquid)   then return, hist1d_liq
	; only ice
	if keyword_set(ice)      then return, hist1d_ice
	; only ratio
	if keyword_set(ratio)    then return, hist1d_rat
	; only mixed
	if keyword_set(mixed)    then return, hist1d_mix
	; liquid + ice
	if keyword_set(combined) then return, hist1d_com

	return,hist1d

end
;-------------------------------------------------------------------------------------------------------------------------
function full_varname, varname, unit = unit, universal = universal

	dat  = strlowcase(varname)
	unit = '' ; set standard unit
	if is_h1d(dat) then begin
		pref = '# of occurrence of ' 
		dat  = strreplace(dat[0],'hist1d_','',/fold)
	endif else pref = ''

	case strmid(dat,0,3) of
		'cot'	: vollername = 'Cloud Optical Thickness' 
		'cth'	: begin & vollername = 'Cloud Top Height' & unit = ' [km]' & end
		'cwp'	: begin & vollername = 'Cloud Water Path' & unit = textoidl(' [g/m^2]') & end
		'iwp'	: begin & vollername = 'Cloud Ice Water Path' & unit = textoidl(' [g/m^2]') & end
		'lwp'	: begin & vollername = 'Cloud Liquid Water Path' & unit = textoidl(' [g/m^2]') & end
		'ctp'	: begin & vollername = 'Cloud Top Pressure' & unit = ' [hPa]' & end
		'ctt'	: begin & vollername = 'Cloud Top Temperature' & unit = ' [K]' & end
		'cfc'	: vollername = 'Cloud Fraction'
		'cdn'	: begin & vollername = 'Cloud Droplet Number Concentration' & unit = textoidl(' [cm^{-3}]') & end
		'cph'	: begin
					vollername = 'Liquid Cloud Fraction'
					if stregex(dat,'cph_asc',/fold,/bool) or stregex(dat,'cph_desc',/fold,/bool) then vollername = 'Cloud Phase'
				  end
		'cer'	: begin & vollername = 'Cloud Effective Radius' & unit = textoidl(' [\mum]') & end
		'cma'	: vollername = 'Cloud Mask'
		'cty'	: vollername = 'Cloud Type'
		'sal'	: begin & vollername = 'Surface Albedo' & unit = ' [%]' & end
		'cee'	: vollername = 'Cloud Effective Emissivity'
		'cla'	: begin
					vollername = 'Cloud Albedo'
					if ~keyword_set(universal) then begin
						if stregex(dat,'vis006',/fold,/bool) then vollername = vollername+textoidl(' at 0.6 \mum')
						if stregex(dat,'vis008',/fold,/bool) then vollername = vollername+textoidl(' at 0.8 \mum')
					endif
				  end
		'nob'	: vollername = 'Number of Observations'
		'rad'	: begin 
					vollername = 'Brightness Temperature' & unit = ' [K]' 
					if ~keyword_set(universal) then begin
						if stregex(dat,'rad3b',/fold,/bool) then vollername = vollername+textoidl(' at 3.7 \mum')
						if stregex(dat,'rad4',/fold,/bool)  then vollername = vollername+textoidl(' at 10.8 \mum')
						if stregex(dat,'rad5',/fold,/bool)  then vollername = vollername+textoidl(' at 12.0 \mum')
					endif
				  end
		'ref'	: begin 
					if stregex(dat,'refl',/fold,/bool) then begin
						vollername = 'Reflectance' & unit = ' [%]'
						if ~keyword_set(universal) then begin
							if stregex(dat,'refl1',/fold,/bool)  then vollername = vollername+textoidl(' at 0.6 \mum')
							if stregex(dat,'refl2',/fold,/bool)  then vollername = vollername+textoidl(' at 0.8 \mum')
							if stregex(dat,'refl3a',/fold,/bool) then vollername = vollername+textoidl(' at 1.6 \mum')
						endif
					endif else begin
						vollername = 'Cloud Effective Radius' & unit = textoidl(' [\mum]')
					endelse
				  end
		else : vollername = varname
	endcase

	if stregex(dat,'ctp2',/bool,/fold) then vollername = 'Cloud Top Pressure (MERIS only)'
	if stregex(dat,'cph_extended',/bool,/fold) then vollername = 'Cloud Type'
	if stregex(dat,'cloud_type',/bool,/fold) then vollername = 'Cloud Type'
	if stregex(dat,'prop_unc',/bool,/fold) then vollername = vollername + ' - propagated Uncertainty' else $
	if stregex(dat,'corr_unc',/bool,/fold) then vollername = vollername + ' - correlated Uncertainty' else $
	if stregex(dat,'_unc',/fold,/bool) and ~stregex(vollername,'_unc',/fold,/bool) then  vollername = vollername + ' - Uncertainty'
	if stregex(dat,'_asc',/fold,/bool)  and ~stregex(vollername,'_asc',/fold,/bool)  then  vollername = vollername + ' - Ascending'
	if stregex(dat,'_desc',/fold,/bool) and ~stregex(vollername,'_desc',/fold,/bool) then  vollername = vollername + ' - Descending'
	if stregex(dat,'_std',/fold,/bool) and ~stregex(vollername,'_std',/fold,/bool) then  vollername = vollername + ' - Standard Deviation'
	if stregex(dat,'_log',/fold,/bool)  and ~stregex(vollername,'_log',/fold,/bool)  then  vollername = vollername + ' (log. Averaged)'
	if stregex(dat,'_day',/fold,/bool)  and ~stregex(vollername,'_day',/fold,/bool)  then  vollername = vollername + ' - Day'
	if stregex(dat,'_twl',/fold,/bool)  and ~stregex(vollername,'_twl',/fold,/bool)  then  vollername = vollername + ' - Twilight'
	if stregex(dat,'_night',/fold,/bool)  and ~stregex(vollername,'_night',/fold,/bool)  then  vollername = vollername + ' - Night'
	if stregex(dat,'toa_lwup',/bool,/fold) then vollername = 'TOA longwave upward radiation'
	if stregex(dat,'toa_lwup_clr',/bool,/fold) then vollername = 'TOA longwave upward radiation - clear sky'
	if stregex(dat,'toa_swup',/bool,/fold) then vollername = 'TOA shortwave upward radiation'
	if stregex(dat,'toa_swup_clr',/bool,/fold) then vollername = 'TOA shortwave upward radiation - clear sky'

	return,pref+vollername

end
;------------------------------------------------------------------------------------------
function appendix, varname,trend_corrected=trend_corrected
	dtn = ''

	if stregex(varname,'refl',/fold,/bool) then dtn += '-Ch'+strmid(varname,4)
	if stregex(varname,'rad',/fold,/bool) then dtn += '-Ch'+strmid(varname,3)

	if stregex(varname,'_16',/fold,/bool) then dtn += textoidl('-1.6\mum')
	if stregex(varname,'_37',/fold,/bool) then dtn += textoidl('-3.7\mum')
	if stregex(varname,'_day',/fold,/bool)   then dtn += '-DAY'
	if stregex(varname,'_night',/fold,/bool) then dtn += '-NIGHT'
	if stregex(varname,'_twl',/fold,/bool)   then dtn += '-TWL'
	if stregex(varname,'_low',/fold,/bool)   then dtn += '-LOW'
	if stregex(varname,'_mid',/fold,/bool)   then dtn += '-MID'
	if stregex(varname,'_high',/fold,/bool)  then dtn += '-HIGH'
	if stregex(varname,'_liq',/fold,/bool)   then dtn += '-LIQ'
	if stregex(varname,'_ice',/fold,/bool)   then dtn += '-ICE'

	if stregex(varname,'_allsky',/fold,/bool) then dtn += '-Allsky'
	if stregex(varname,'_corrected',/fold,/bool) then dtn += '-Corrected'

	;CAlipso only
	if stregex(varname,'_sc_liq',/fold,/bool)   then dtn = ' (SC) LIQ'
	if stregex(varname,'_th_ice',/fold,/bool)   then dtn = ' (TH) ICE'
	if stregex(varname,'_cloudsgt01',/fold,/bool)   then dtn = ' COD > 0.1'
	if stregex(varname,'_cloudsgt02',/fold,/bool)   then dtn = ' COD > 0.2'
	if stregex(varname,'_cloudsgt03',/fold,/bool)   then dtn = ' COD > 0.3'
	if stregex(varname,'_allclouds_max',/fold,/bool)   then dtn = ' ALL+MAX'
	;FAME-C only
	if stregex(varname,'ctt2',/fold,/bool) then dtn = ' CTT2'
	if stregex(varname,'cth2',/fold,/bool) then dtn = ' CTH2'
	if stregex(varname,'ctp2',/fold,/bool) then dtn = ' CTP2'
	
	if keyword_set(trend_corrected) then dtn += '-(TC)'
	if strpos(dtn,'-') eq 0 then dtn = ' '+strmid(dtn,1) 
	return,dtn
end
;-------------------------------------------------------------------------------------------------------------------------
pro symball,filled=filled,thick=thick
	aa = findgen(17) * (!PI*2/16.) & usersym, cos(aa), sin(aa),fill=filled,thick=thick ; kullerchen
end
;------------------------------------------------------------------------------------------
; Taken from fileinfo.pro Liam.Gumley
;- Get netCDF status
; print,is_ncdf('/cmsaf/cmsaf-cld1/sstapelb/gac/data/2008/06/metop02/metop02_20080601_0246_99999_satproj_00000_12423_physiography.h5')
; !!! Achtung bei idl 8.4.1 wird idl beendet wenn ein hdf4 file mit ncdf_open() geöffnet wird !!!
function is_ncdf, filename
	error_status = 0
	if file_test(filename[0],/read) then begin
		catch, error_status
		if (error_status ne 0) then begin
			catch, /cancel
			return, 0
		endif
		cdfid = ncdf_open(filename[0])
		ncdf_close, cdfid
	endif

	; hier muss neuerdings auch noch der extension test hin, leider. hdf5 und ncdf4 sind absolut gleich ab idl8.3
; ; 	if stregex((reverse(strsplit(file_basename(filename[0]),'.',/ext)))[0],'nc',/bool,/fold) then return,1
	return, 1
end
;----------------------------------------------------------------------------------------
; Taken from fileinfo.pro Liam.Gumley
;- Get HDF status
function is_hdf, filename, version
	hdf = 0
	version = 0
	error_status = 0
	if file_test(filename[0],/read) then begin
		hdfid = hdf_open(filename[0], /read)
		if (hdfid ne -1) then begin
			hdf = 1
			version = 4
			hdf_close, hdfid
		endif else begin
			catch, error_status
			if (error_status ne 0) then begin
				catch, /cancel
				return, hdf
			endif
			hdfid = h5f_open(filename[0])
			hdf = 1
			version = 5
			h5f_close, hdfid
			if is_ncdf(filename[0]) then begin
				hdf = 0
				version = 0
			endif
		endelse
	endif
	return, hdf
end
;---------------------------------------------------------------------------------------
function is_hdf5, filename
	hdf = 0
	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, 0
	endif
	if file_test(filename[0],/read) then begin
		hdf = H5F_IS_HDF5(filename[0])
		if hdf then begin
			if is_ncdf(filename[0]) then hdf = 0
		endif
	endif
	return, hdf
end
;------------------------------------------------------------------------------------------
function is_h5data, fileid, data
	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, 0
	endif
	dID = h5d_open(fileID, data )
	h5d_close, dID
	return, 1
end
;----------------------------------------------------------------------------------------
function h5t_get_member_value_error, Datatype_id, member
; h5t_get_member_value 
; use this instead of the original !
	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, '<Read Error>'
	endif
	result = H5T_GET_MEMBER_VALUE(Datatype_id, Member)
	return,result
end
;------------------------------------------------------------------------------------------
; returns 1 if str is a number also a string number
function is_number, str

	n_str = n_elements(str)
	if n_str eq 0 then return,0
	nums = bytarr(n_str)

	for ii = 0, n_str -1 do begin
		if size(str[ii],/type) eq 0 then continue
		if str[ii] eq '' then continue
		error_status = 0
		catch, error_status
		if (error_status ne 0) then begin
			catch, /cancel
			bad_io:
			continue
		endif
		ON_IOError, bad_io
		a = str[ii] * 5
		free,a
		nums[ii] = 1b
	endfor
	return, n_str eq 1 ? nums[0] : nums
end
;------------------------------------------------------------------------------------------
function is_struct, file
	return, (size(file,/type) eq 8)
end
;------------------------------------------------------------------------------------------
function is_string, file
	return, (size(file,/type) eq 7)
end
;------------------------------------------------------------------------------------------
function is_defined, keyword
	return, (size(keyword,/type) < 1)
end
;-------------------------------------------------------------------------------------------------------
function adv_keyword_set, keyword
	return, (size(keyword,/type) < 1)
end
;------------------------------------------------------------------------------------------
; check if 2 arrays match rebin criteria
; shrinking array1 with dimensions1 being integral multiples 
; or factors of the original dimension (rebin criteria)
; bring array res1 to res2
function rebinable,res1,res2

	reg   = res1 ne 0 and res2 ne 0		; reguläres gitter
	loe   = res1 le res2 				; auflösung kleiner(höher)-gleich als zielgitter
	ganz  = fix(float(res2)/float(res1)) eq float(float(res2)/float(res1)) ; ganzzahliges vielfaches

	result = reg and loe and ganz

	return,result

end
;------------------------------------------------------------------------------------------
function neighbour_pixel,array,neighbors,no_data_value=no_data_value,fill_index=fill_index, $
		normalize=normalize,median=median,mean=mean,stddev=stddev,total=total,uniformity = uniformity

; 	array must be 2D
	si=size(array,/dim)
	if n_elements(si) ne 2 then begin
		print,'neighbour_pixel: Input array must be 2D'
		return,-1
	endif

	if n_params() eq 1 then neighbors = 3
	nb  = fix(neighbors)/2 < 10
	arr = array
	ndv = adv_keyword_set(no_data_value) ? no_data_value[0] : -999.
	idx = where(arr eq ndv,idx_cnt)
	if idx_cnt gt 0  then arr[idx] = !values.f_nan

	if keyword_set(fill_index) then begin
		;this is not necessarily faster but should at least save some memory for big arrays
		if size(fill_index,/n_dim) eq 1 and n_elements(fill_index) eq 2 then begin
			print,'!! Warning !! neighbour_pixel will treat given fill_index as two 1D-Indizes not as One 2D-Index!'
			print,'              If meant otherwise fill_index must be of size [1,2]'
		endif
		idx2d  = size(fill_index,/n_dim) eq 1 ? transpose(array_indices(arr,fill_index)) : fill_index
		si2d   = size(idx2d,/dim)
		;idx2d must be of dimsize [*,2]
		if n_elements(si2d) ne 2 then begin
			print,'neighbour_pixel: Check given Index (must be of dimsize fill_index[*,2])'
			return,-1
		endif
		if si2d[1] ne 2 then begin
			if total(si2d eq 2) then begin
				idx2d = transpose(idx2d)
				if size(idx2d,/dim) ne 2 then idx2d = reform(idx2d,1,n_elements(idx2d))
			endif else begin
				print,'neighbour_pixel: Check given Index (must be of dimsize fill_index[*,2])'
				return,-1
			endelse
		endif
		si2d   = size(idx2d,/dim)
		result = fltarr((1+2*nb)^2.,n_elements(idx2d[*,0])) +!values.f_nan
		for i=0ul,n_elements(idx2d[*,0])-1 do begin
; 			dummy = arr[(0>(idx2d[i,0]-nb)):((idx2d[i,0]+nb)<(si[0]-1)),(0>(idx2d[i,1]-nb)):((idx2d[i,1]+nb)<(si[1]-1))]
			; we want all neighbor pixel for Longitude (includes crossing the dateline), but not for Latitude!!
			xx = ( si[0] + ( (idx2d[i,0]) + vector((-1)*nb,nb,(2*nb+1)) ) ) mod si[0]
			dummy = arr[ [xx], (0>(idx2d[i,1]-nb)):((idx2d[i,1]+nb)<(si[1]-1)) ]
			result[0:n_elements(dummy)-1,i] = reform(dummy,n_elements(dummy))
		endfor
		dim=1
	endif else begin
		for i = (-1)*nb,nb do begin
			for j = (-1)*nb,nb do begin
				dum = shift(arr,i,j)
				result = is_defined(result) ? [[[result]],[[dum]]] : dum
			endfor
		endfor
		dim=3
	endelse

	if keyword_set(uniformity) then begin
		maxi = max(result,dim=dim,/nan)
		mini = min(result,dim=dim,/nan)
		mean = mean(result,dim=dim,/nan)
		result = ( temporary(maxi) - temporary(mini) ) / temporary(mean)
	endif
	if keyword_set(median) then result = median(result,dim=dim)
	if keyword_set(mean)   then result = mean(result,dim=dim,/nan)
	if keyword_set(total)  then result = total(result,dim,/nan)
	if keyword_set(stddev) then result = stddev(result,dim=dim,/nan)

	idx =where(~finite(result),idx_cnt)
	if keyword_set(normalize) then result = result/max(result)
	if idx_cnt gt 0  then result[idx] = ndv

	return,result

end
;------------------------------------------------------------------------------------------
;returns histogram with irregular bin sizes
function ihistogram,data,bin_borders

; 	bins     = [1.,90.,180.,245.,310.,375.,440.,500.,560.,620.,680.,740.,800.,875.,950.,1100.]
	n_bins   = n_elements(bin_borders)
	bins     = bin_borders
	histo    = lonarr(n_bins-1)
	dum_hist = histogram(ctp,min=0,max=max(bins))
	for i = 0,n_bins-2 do histo[i] = total(dum_hist[bins[i]:bins[i+1]-1])

	return,histo

end
;------------------------------------------------------------------------------------------
; maps a filled shapefile (polygons only) onto a regular grid, also orbits via regular grid
function shape2grid, shape_file,longitude=longitude,latitude=latitude,grid=grid,plot_it=plot_it,found=found,germany=germany

	found = 1
	if keyword_set(germany) then shape_file = !SHAPE_DIR + 'Germany/DEU_adm1.shp'
	grd = adv_keyword_set(grid) ? float(grid) : 0.05

	if grd le 0 or ~file_test(shape_file,/read) then begin
		if grd le 0. then begin
			;check if lon/lat are available
			if keyword_set(longitude) and keyword_set(latitude) then begin
				lon_in = longitude
				lat_in = latitude
				si = size(lon_in,/dim)
				if n_elements(si) eq 2 then begin
					;grd will also be 0 if the grid is regular but not global
					;e.g., the europe or africa l3u files of CCI
					grid_res = get_grid_res(claas = claas, lon_in, cci_l3u_eu = cci_l3u_eu, cci_l3u_af = cci_l3u_af)
					if cci_l3u_eu or cci_l3u_af then begin
						lo_mima = minmax(lon_in,no_data_value=-999.,/nan)
						la_mima = minmax(lat_in,no_data_value=-999.,/nan)
						grd   = rnd(float(lo_mima[1] - lo_mima[0]) / float(si[0]-1),0.01)
						slon  = (lo_mima[0] - grd/2.)
						slat  = (la_mima[0] - grd/2.)
						x_dim = si[0]
						y_dim = si[1]
					endif else if keyword_set(claas) then begin
						grd   =  180./float(si[1])
						slon  =  -90.
						slat  =  -90.
						x_dim =  180./grd
						y_dim =  180./grd
					endif else begin
						;here we prob. have a L2 file or MSG Full disk
						print,'shape2grid: Reading shape_file onto a non-regular grid! Will now set up a fake regular 0.05° Grid and do bilinear interpolation!'
						grd   = 1/20.
						slon  = -180.
						slat  =  -90.
						x_dim =  360./grd
						y_dim =  180./grd
						;check if lon_in is from-180-+180°
						ilondum = WHERE(lon_in GT 180.,icnt)
						if icnt gt 0 then begin
							print,'shape2grid: Bring longitude to [-180,180]'
							lon_in[ilondum]= 360.-lon_in[ilondum]
						endif
						index_lon = fix((lon_in - slon)*(1./grd))
						index_lat = fix((lat_in - slat)*(1./grd))
					endelse
				endif else begin
					found = 0
					return,-1
				endelse
			endif else begin
				found = 0
				return,-1
			endelse
		endif else begin
			print,'shape2grid: Shape_file does not exist or is not readable!', shape_file
			found = 0
			return,-1
		endelse
	endif else begin
		slon  = -180.
		slat  =  -90.
		x_dim = 360./grd
		y_dim = 180./grd
	endelse

	;use adm0 file for this, if available
	sfile = strreplace(shape_file,'_adm1.shp','_adm0.shp')

	myshape = OBJ_NEW('IDLffShape', file_test(sfile) ? sfile : shape_file)
	myshape -> GetProperty, N_ENTITIES=num_ent

	filled = BYTARR(x_dim,y_dim)
	!quiet=1 ; suppress "No points in polygon" warnings by polyfillv

	FOR x=0l,num_ent-1 DO BEGIN
		entity  = myshape->GetEntity(x)
		if 	entity.shape_type EQ 5 OR $    		; Polygon.
			entity.shape_type EQ 15 OR $   		; PolygonZ (ignoring Z)
			entity.shape_type EQ 25 then begin 	; PolygonM (ignoring M)

			IF Ptr_Valid(entity.parts) THEN BEGIN
				cuts = [*entity.parts, entity.n_vertices]
				FOR j=0l, entity.n_parts-1 DO BEGIN
					lon = reform((*entity.vertices)[0, cuts[j]:cuts[j+1]-1])
					lat = reform((*entity.vertices)[1, cuts[j]:cuts[j+1]-1])
					;convert lon/lat to x/y
					xxx = fix((lon - slon)*(1./grd))
					yyy = fix((lat - slat)*(1./grd))
					subscripts = POLYFILLV( reform(xxx), reform(yyy), long(x_dim), long(y_dim))
					if subscripts[0] ne -1 then filled[subscripts] += 1
				endfor
			endif
		endif else print,'shape2grid: Entity Type: ',entity.shape_type
	endfor
	!quiet=0
	OBJ_DESTROY, myshape

	if is_defined(index_lon) then filled = bilinear(filled, index_lon, index_lat)

	if keyword_set(plot_it) then begin
		make_geo,lon_g,lat_g,grid = grd
		map_image,filled,lat_g,lon_g,/rainbow,min=0,max=1,/countries,limit=limit
	endif

	return, (filled < 1b)

end
;------------------------------------------------------------------------------------------

pro get_era_info, file, version, threshold , algo = algo, set_as_sysvar = set_as_sysvar

		filen = file_basename(file)
		pathn = file_dirname(file)

		pos = STREGEX(pathn, '_DWDscops',/fold,length=len)
		version = strmid(pathn,pos-4,4)

		pos = STREGEX(filen, 'cot-thv-' ,/fold,length=len)
		threshold = strmid(filen,pos+len,4)

		if keyword_set(set_as_sysvar) then begin
			if algo2ref(algo) eq 'era' then begin
				DEFSYSV, '!era_i1_threshold', threshold
				DEFSYSV, '!era_i1_version'  , version
			endif
			if algo2ref(algo) eq 'era2' then begin
				DEFSYSV, '!era_i2_threshold', threshold
				DEFSYSV, '!era_i2_version'  , version
			endif
		endif

end
;------------------------------------------------------------------------------------------
function low_pass_filtering,array,no_data_value=no_data_value,fill_index=fill_index

	ndv = keyword_set(no_data_value) ? no_data_value[0] : -999.

	kernel = gaussian_function([1,1], width=5, maximum=max(array))

	image = convol(array,kernel,invalid=ndv,missing=ndv,/edge_truncate,/normalize)

	if keyword_set(fill_index) then begin
		dum = array
		dum[fill_index] = image[fill_index]
		image = temporary(dum)
	endif
	return,image

end
;------------------------------------------------------------------------------------------
function PlanckInv, input_platform, T 

    Planck_C1 = 1.19104E-5 ; 2hc^2 in mW m-2 sr-1 (cm-1)-4
    Planck_C2 = 1.43877 ; hc/k  in K (cm-1)-1

    ; remove leading zero as well
    platform = strlowcase(strjoin(strsplit(input_platform,'-',/ext)))

    ; select approproate row of coefficient values
	case platform of
		"noaa5"		: index = 0
		"noaa6"		: index = 1
		"noaa7"		: index = 2
		"noaa8"		: index = 3
		"noaa9"		: index = 4
		"noaa10"	: index = 5
		"noaa11"	: index = 6
		"noaa12"	: index = 7
		"noaa14"	: index = 8
		"noaa15"	: index = 9
		"noaa16"	: index = 10
		"noaa17"	: index = 11
		"noaa18"	: index = 12
		"noaa19"	: index = 13
		"metopb"	: index = 14
		"metopa"	: index = 15
		"npp"		: index = 16
		"terra"		: index = 17
		"aqua"		: index = 18
		"env"		: index = 19
		"msg2"		: index = 20
		"default"	: index = 21
		else		: begin
					print, "Error: Platform name does not match local string in function PlanckInv"
					print, "Input platform name = ", input_platform 
					stop
				  end
	endcase

    ;   v: wave number (cm-1)                                                           
    ;   a: alpha parameter                                                                
    ;   b: beta parameter   
    ;   solcon: solar constant

    ; Conversion from SADChan Planck coefficients to the ones here:
    ; v = (B1 / Planck_C1)^(1/3) = B2 / Planck_C2
    ; a = T2
    ; b = T1
    coefficients = reform( [ $
    ;        v          a         b     solcon
         2655.741,  0.997915,  1.64511, 4.957, $ ;noaa05, tirosn
         2671.543,  0.997563,  1.76241, 5.010, $ ;noaa06
         2684.523,  0.997083,  1.94314, 5.061, $ ;noaa07
         2651.378,  0.997580,  1.77211, 4.939, $ ;noaa08
         2690.045,  0.997111,  1.87782, 5.081, $ ;noaa09
         2672.616,  0.997374,  1.79397, 5.017, $ ;noaa10
         2680.050,  0.996657,  1.73316, 5.077, $ ;noaa11
         2651.771,  0.996999,  1.89956, 4.948, $ ;noaa12
         2654.250,  0.996176,  1.87812, 4.973, $ ;noaa14
         2695.974,  0.998015,  1.62126, 5.088, $ ;noaa15
         2681.254,  0.998271,  1.67456, 5.033, $ ;noaa16
         2669.141,  0.997335,  1.69576, 5.008, $ ;noaa17
         2660.647,  0.997145,  1.71735, 4.981, $ ;noaa18
         2670.242,  0.997411,  1.68202, 5.010, $ ;noaa19
         2664.338,  0.997016,  1.76585, 4.996, $ ;metop01,metopb
         2687.039,  0.996570,  2.05823, 5.077, $ ;metop02,metopa
         2707.560,  0.999085,  0.58063, 5.123, $ ;npp (viirs)
         2641.775,  0.999341,  0.47705, 4.804, $ ;terra
         2647.409,  0.999336,  0.48184, 4.822, $ ;aqua
         2675.166,  0.996344,  1.72695, 5.030, $ ;env (aatsr),ers2??
         2568.832,  0.995400,  3.43800, 4.660, $ ;msg1, msg2
         2670.000,  0.998000,  1.75000, 5.000  $ ;default
         ], 4, 22 )

;   print,index
; 	print,coefficients[ * , index ]

    PlanckInv = Planck_C1 * coefficients[ 0 , index ]^3 / $
         ( exp( Planck_C2 * coefficients[ 0 , index] / $
         ( coefficients[ 1 , index ] * T $
         + coefficients[ 2 , index ] ) ) - 1. )
    solar_const = coefficients[ 3 , index ]

    return, {PlanckInv:PlanckInv,solar_const:solar_const}

end 
;------------------------------------------------------------------------------------------
function bt37_to_ref37, doy, bt37, bt11, solzen, platform, no_data_value = no_data_value, emis_ch3b=emis_ch3b, true_reflectance = true_reflectance

	fillv = keyword_set(no_data_value) ? no_data_value[0] : -999.
	idx = where( (bt37 eq fillv) or (bt11 eq fillv) ,idxcnt) 

	; calculate ch3b emissivity and reflectance
	PlanckInv_out  = PlanckInv( platform, bt37 )
	rad_ch3b       = PlanckInv_out.PlanckInv
	solcon_ch3b    = PlanckInv_out.solar_const
	PlanckInv_out  = PlanckInv( platform, bt11 )
	rad_ch3b_emis  = PlanckInv_out.PlanckInv
	mu0 = cos ( solzen * !dtor ) 
	esd = 1.0 - 0.0167 * cos( 2.0 * !pi * ( doy - 3 ) / 365.0 )
	c_sun = 1. / esd^2
	emis_ch3b = rad_ch3b / rad_ch3b_emis
	ref_ch3b = ( rad_ch3b - rad_ch3b_emis ) / ( solcon_ch3b * c_sun * mu0 - rad_ch3b_emis )
	if ~keyword_set(true_reflectance) then ref_ch3b *= mu0

	if idxcnt gt 0 then begin
		emis_ch3b[idx] = fillv
		ref_ch3b[idx]  = fillv
	endif

	return, ref_ch3b

end
;------------------------------------------------------------------------------------------
function false_color_max, filter, ref06, ref08, ref37, bt37, bt11, bt12, solzen, longname=longname,$
			no_hist_equal=no_hist_equal,no_byte_scale=no_byte_scale

	print_name = ( not arg_present(longname) )

	description =[	'00) ir108 [DAY/NIGHT - warm->white, cold->black]', $
					'01) ir108-ir039 [NIGHT ONLY - fog->white, cirrus -> black]', $
					'02) ir120-ir108/ir108-ir039/ir108 [NIGHT ONLY - fog,contr.->green, ground->pink, ice->red, thin c.->dark]', $
					'03) vi006 [DAY ONLY - high reflectance->white, low reflectance->black]', $
					'04) rgb [DAY ONLY - true clolor image]', $
					'05) vi006, vi006, BTnir37 , Ice clouds yellow', $
					'06) vi006/vi008,ref37/vi008,bt11-bt12', $
					'07) vi006,vi008,bt11, High-clouds -> yellow, low clouds -> pale yellow shade; vegetated land -> purple, water -> blue.',$
					'08) vi008,vi006,vi006',$
					'09) vi008,bt11,bt12', $
					'10) ref37,vi008,vi006', $
					'11) ir120-ir108/ir108/ir108 [NIGHT ONLY - water->cyan, ground->pink, ice clouds->red, thin c.->dark]',$
					'12) vi008,bt11,bt11-bt12']

	longname = description[fix(filter)]
	if print_name then print,longname

	mu0 = cos ( solzen * !dtor ) 

	case fix(filter) of
		0:begin
			c1       = bt11
			data_idx = where((c1 gt 0), data_anz)
			data     = transpose([[[c1]],[[c1]],[[c1]]], [2, 0, 1])
		end
		1:begin
			c1 = bt11
			c2 = bt37
			data_idx = where((c1 gt 0) and (c2 gt 0), data_anz)
			c3 = -2 > c1 - c2 < 15
			data = transpose([[[c3]],[[c3]],[[c3]]], [2, 0, 1])
		end
		2:begin
			c1 = bt12
			c2 = bt11
			c3 = bt37
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0), data_anz)
			data = transpose([[[c1 - c2]],[[(c2 - c3)]],[[c2]]], [2, 0, 1])
		end
		3:begin
			c1 = ref06/mu0
			data_idx = where((c1 ge 0) and (solzen ge 0), data_anz)
			data = transpose([[[c1]],[[c1]],[[c1]]], [2, 0, 1])
		end
		4:begin
			data_idx = where(bt11 ge 0)
			data = calc_rgb(ref06, ref08, bt37, bt11, solzen,0)
			data = byte(data*1.3 < 255.)
; 			return,transpose(data,[1,2,0])
		end
		5:begin
			c1 = ref06
			c2 = ref06
			c3 = ref37
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0), data_anz)
			data     = transpose([[[c1]],[[c2]],[[c3]]], [2, 0, 1])
		end
		6:begin
			c1 = ref06
			c2 = ref08
			c3 = ref37
			c4 = bt11
			c5 = bt12
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0) and (c4 gt 0) and (c5 gt 0), data_anz)
			data     = transpose([[[c1]],[[c1/c2]],[[c3/c2]]], [2, 0, 1])
		end
		7:begin
			c1 = ref06/mu0
			c2 = ref08/mu0
; 			c3 = bt11
			c3 = bt11/max(bt11) *100.
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0) , data_anz)
			data     = transpose([[[c2]],[[c1]],[[c3]]], [2, 0, 1])
; 			data     = transpose([[[c1]],[[c2]],[[c3]]], [2, 0, 1])
		end
		8:begin
			c1 = ref08
			c2 = ref06
 			c3 = ref06
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0) , data_anz)
 			data     = transpose([[[c1]],[[c2]],[[c3]]], [2, 0, 1])
		end
		9:begin
			c1 = ref08
			c2 = bt11
 			c3 = bt12
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0) , data_anz)
			data     = transpose([[[c1]],[[c2]],[[c3]]], [2, 0, 1])
		end
		10:begin
			c1 = ref37;*mu0
			c2 = ref08;*mu0
			c3 = ref06;*mu0
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0), data_anz)
			data     = transpose([[[c1]],[[c2]],[[c3]]], [2, 0, 1])
		end
		11:begin
			c1 = bt12
			c2 = bt11
			data_idx = where((c1 gt 0) and (c2 gt 0), data_anz)
			data = transpose([[[c1 - c2]],[[c2]],[[c2]]], [2, 0, 1])
		end
		12:begin
			c1 = ref08
			c2 = bt11
			c3 = bt12
			c4 = bt37
			data_idx = where((c1 gt 0) and (c2 gt 0) and (c3 gt 0) and (c4 gt 0), data_anz)
			data = transpose([[[c1]],[[c2-c4]],[[c2 - c3]]], [2, 0, 1])
		end
	endcase

	;min und max werte fuer jeden kanal berechnen
	dum = reform(data[0, *, *])
	min_r = (data_idx[0] ne -1) ? min(dum[data_idx]) : 0
	max_r = (data_idx[0] ne -1) ? max(dum[data_idx]) : 0
	dum = reform(data[1, *, *])
	min_g = (data_idx[0] ne -1) ? min(dum[data_idx]) : 0
	max_g = (data_idx[0] ne -1) ? max(dum[data_idx]) : 0
	dum = reform(data[2, *, *])
	min_b = (data_idx[0] ne -1) ? min(dum[data_idx]) : 0
	max_b = (data_idx[0] ne -1) ? max(dum[data_idx]) : 0

	;hist_equal
	if ~keyword_set(no_hist_equal) then begin
		dum = reform(data[0, *, *])
		if min_r ne max_r then begin
			dum[data_idx] = hist_equal(dum[data_idx])
			data[0, *, *] = dum
		endif
		dum = reform(data[1, *, *])
		if min_g ne max_g then begin
			dum[data_idx] = hist_equal(dum[data_idx])
			data[1, *, *] = dum
		endif
		dum = reform(data[2, *, *])
		if min_b ne max_b then begin
			dum[data_idx] = hist_equal(dum[data_idx])
			data[2, *, *] = dum
		endif
	endif
	if ~keyword_set(no_byte_scale) then begin
		;bytescale
		if min_r ne max_r then begin
			dum_alt = reform(data[0, *, *])
			dum_neu = dum_alt and 0
			dum_neu[data_idx] = bytscl(dum_alt[data_idx])
			data[0, *, *] = dum_neu
		endif
		if min_g ne max_g then begin
			dum_alt = reform(data[1, *, *])
			dum_neu = dum_alt and 0
			dum_neu[data_idx] = bytscl(dum_alt[data_idx])
			data[1, *, *] = dum_neu
		endif
		if min_b ne max_b then begin
			dum_alt = reform(data[2, *, *])
			dum_neu = dum_alt and 0
			dum_neu[data_idx] = bytscl(dum_alt[data_idx])
			data[2, *, *] = dum_neu
		endif
	endif

	return,transpose(data,[1,2,0])
end
;------------------------------------------------------------------------------------------
pro set_coverage, cov
	coverage = ['midlat_trop','full','southern_hemisphere','northern_hemisphere','antarctica','midlat_south','tropic','midlat_north','arctic']
	cov      = [coverage,coverage+'_land',coverage+'_sea']
end
;------------------------------------------------------------------------------------------
function get_coverage, 	lon, lat, dem = dem, limit = limit, land = land, sea = sea, coverage = coverage	, $
						antarctic = antarctic, arctic = arctic, shape_file = shape_file, opposite = opposite, $
						l3u_index = l3u_index, fillv_index = fillv_index, found = found, grid_res = grid_res, $
						index = index, count = count, complement = complement, ncomplement = ncomplement	, $
						msg = msg, l3ue=l3ue ; where indizes and counts

	found = 1
	count = 0
	index =-1
	lim = keyword_set(limit) ? limit   : [-90.0,-180., 90.0,180.]
	if keyword_set(antarctic) then lim = [-90.0,-180.,-60.0,180.]
	if keyword_set(arctic)    then lim = [ 60.0,-180., 90.0,180.]

	if keyword_set(shape_file) then begin
		if ~file_test(shape_file) then free, shape_file
	endif

	fvi_cnt = n_elements(fillv_index)
	if fvi_cnt eq 1 then begin
		if fillv_index[0] eq -1 then fvi_cnt = 0
	endif

	l3ui_cnt = n_elements(l3u_index)
	if l3ui_cnt eq 1 then begin
		if l3u_index[0] eq -1 then l3ui_cnt = 0
	endif

	if keyword_set(coverage) then begin
		cov = strlowcase(coverage)
		if cov eq 'sea' then begin
			sea  = 1
			land = 0
			mnts = 0
			cov  = 'full'
		endif else if cov eq 'land' then begin
			sea  = 0
			land = 1
			mnts = 0
			cov  = 'full'
		endif else if cov eq 'mountains' then begin
			sea  = 0
			land = 1
			mnts = 1
			cov  = 'full'
		endif else if stregex(cov,'_land',/bool) then begin
			sea  = 0
			land = 1
			mnts = 0
			cov  = strreplace(cov,'_land','')
		endif else if stregex(cov,'_sea',/bool) then begin
			sea  = 1
			land = 0
			mnts = 0
			cov = strreplace(cov,'_sea','')
		endif else if stregex(cov,'_mountains',/bool) then begin
			sea  = 0
			land = 1
			mnts = 1
			cov  = strreplace(cov,'_mountains','')
		endif
		case cov of
			''						: lim = [-90.0,-180.0, 90.0, 180.0]
			'full'					: lim = [-90.0,-180.0, 90.0, 180.0]
			'global'				: lim = [-90.0,-180.0, 90.0, 180.0]
			'antarctica'			: lim = [-90.0,-180.0,-60.0, 180.0]
			'antarctica_kgk'		: lim = [-90.0,-180.0,-75.0, 180.0]
			'midlat_south'			: lim = [-60.0,-180.0,-30.0, 180.0]
			'midlat_south_kgk'		: lim = [-75.0,-180.0,-45.0, 180.0]
			'tropic'				: lim = [-30.0,-180.0, 30.0, 180.0]
			'tropic_kgk'			: lim = [-10.0,-180.0, 10.0, 180.0]
			'subtropic'				: lim = [-45.0,-180.0,-10.0, 180.0, 10.0,-180.0, 45.0, 180.0]
			'midlat_north'			: lim = [ 30.0,-180.0, 60.0, 180.0]
			'midlat_north_kgk'		: lim = [ 45.0,-180.0, 75.0, 180.0]
			'midlats'				: lim = [-60.0,-180.0,-30.0, 180.0, 30.0,-180.0, 60.0, 180.0]
			'midlats_kgk'			: lim = [-75.0,-180.0,-45.0, 180.0, 45.0,-180.0, 75.0, 180.0]
			'polar'					: lim = [-90.0,-180.0,-60.0, 180.0, 60.0,-180.0, 90.0, 180.0]
			'polar_kgk'				: lim = [-90.0,-180.0,-75.0, 180.0, 75.0,-180.0, 90.0, 180.0]
			'europe'				: lim = [ 40.0,   0.0, 51.0,  20.0]
			'australia'				: lim = [-10.0, 110.0,-40.0, 155.0]
			'arctic'				: lim = [ 60.0,-180.0, 90.0, 180.0]
			'arctic_kgk'			: lim = [ 75.0,-180.0, 90.0, 180.0]
			'midlat_trop'			: lim = [-60.0,-180.0, 60.0, 180.0]
			'pm60'					: lim = [-60.0,-180.0, 60.0, 180.0]
			'northern_hemisphere'	: lim = [  0.0,-180.0, 90.0, 180.0]
			'southern_hemisphere'	: lim = [-90.0,-180.0,  0.0, 180.0]
			else					: begin & print,'coverage not defined!' & return,-1 & found = 0 & end
		endcase
	endif

	if ~keyword_set(lon) or ~keyword_set(lat) then begin
		if keyword_set(grid_res) then begin
			make_geo,lon,lat,grid=grid_res
		endif else if keyword_set(l3u_index) then begin
			make_geo,lon,lat,grid=0.05
		endif else begin
			ok = dialog_message('get_coverage: Need Lon / Lat info:')
			found = 0.
			return,-1
		endelse
	endif

	if keyword_set(shape_file) then begin
		result = shape2grid(shape_file,grid=get_grid_res(lon),lon=lon,lat=lat)
	endif else begin
		if n_elements(lim) eq 4 then begin
			result = between(lat,lim[0],lim[2]) and between(lon,lim[1],lim[3])
		endif else if n_elements(lim) eq 8 then begin
			result = ( between(lat,lim[0],lim[2]) and between(lon,lim[1],lim[3]) ) or $
					 ( between(lat,lim[4],lim[6]) and between(lon,lim[5],lim[7]) )
		endif else begin
			ok = dialog_message('Limit vector has unknown length.')
			found=0.
			return,-1
		endelse
	endelse

	if keyword_set(land) then begin
		if ~keyword_set(mnts) then begin
			ddem = keyword_set(dem) ? dem : shape2grid(!SHAPE_DIR + 'Land_Mask/ne_10m_land.shp',grid=get_grid_res(lon),lon=lon,lat=lat)
		endif else begin
			ddem = keyword_set(dem) ? dem : get_dem(lon,lat,grid_res=get_grid_res(lon))
		endelse
		result = keyword_set(mnts) ? (result eq 1) and (ddem gt 1000.) : (result eq 1) and (ddem ne 0)
	endif else if keyword_set(sea) then begin
		if ~keyword_set(mnts) then begin
			ddem = keyword_set(dem) ? dem : shape2grid(!SHAPE_DIR + 'Land_Mask/ne_10m_land.shp',grid=get_grid_res(lon),lon=lon,lat=lat)
		endif else begin
			ddem = keyword_set(dem) ? dem : get_dem(lon,lat,grid_res=get_grid_res(lon))
		endelse
		result = (result eq 1) and (ddem eq 0)
	endif

	if fvi_cnt gt 0 then result[fillv_index] = 0

	if keyword_set(opposite) then result = (result eq 0)

	if l3ui_cnt gt 0 then result = result[l3u_index]

	index = where(result eq 1,count,complement=complement,ncomplement=ncomplement)

	return, result

end
;------------------------------------------------------------------------------------------
function short_cov_name, coverage

	short_name = ''
	if keyword_set(coverage) then begin
		case strlowcase(coverage) of
			''				: short_name = ''
			'full'				: short_name = ''
			'antarctica'			: short_name = 'ANT'
			'midlat_south'			: short_name = 'MLS'
			'tropic'			: short_name = 'TRO'
			'midlat_north'			: short_name = 'MLN'
			'arctic'			: short_name = 'ARC'
			'midlat_trop'			: short_name = textoidl('\pm60\circ')
			'northern_hemisphere'		: short_name = 'NH'
			'southern_hemisphere'		: short_name = 'SH'
			'land'				: short_name = 'LAND'
			'full_land'			: short_name = 'LAND'
			'antarctica_land'		: short_name = 'ANT-L'
			'midlat_south_land'		: short_name = 'MLS-L'
			'tropic_land'			: short_name = 'TRO-L'
			'midlat_north_land'		: short_name = 'MLN-L'
			'arctic_land'			: short_name = 'ARC-L'
			'midlat_trop_land'		: short_name = textoidl('\pm60\circ-L')
			'northern_hemisphere_land'	: short_name = 'NH-L'
			'southern_hemisphere_land'	: short_name = 'SH-L'
			'sea'				: short_name = 'SEA'
			'full_sea'			: short_name = 'SEA'
			'antarctica_sea'		: short_name = 'ANT-S'
			'midlat_south_sea'		: short_name = 'MLS-S'
			'tropic_sea'			: short_name = 'TRO-S'
			'midlat_north_sea'		: short_name = 'MLN-S'
			'arctic_sea'			: short_name = 'ARC-S'
			'midlat_trop_sea'		: short_name = textoidl('\pm60\circ-S')
			'northern_hemisphere_sea'	: short_name = 'NH-S'
			'southern_hemisphere_sea'	: short_name = 'SH-S'
			else				: short_name = Coverage
		endcase
	endif

	return, short_name

end

;------------------------------------------------------------------------------------------
function is_the_same,algo,reference,satellite=satellite

	alg = strlowcase(algo[0])
	ref = strlowcase(reference[0])
	sat = keyword_set(satellite) ? strlowcase(satellite[0]) : ''

	if alg eq ref then return, 1b

	case ref of 
		'cci'	: result = alg eq 'esacci'
		'cciv3'	: result = alg eq 'esacciv3'
		'cci_old'	: result = alg eq 'esacci_old'
		'gac'	: result = alg eq 'clara' 
		'myd'	: result = alg eq 'coll5' and sat eq 'aqua' 
		'mod'	: result = alg eq 'coll5' and sat eq 'terra' 
		'pmx_old'	: result = alg eq 'patmos_old' 
		'pmx'	: result = alg eq 'patmos' 
		'cla'	: result = alg eq 'claas' 
		'gwx'	: result = alg eq 'gewex' 
		'g2_gwx': result = alg eq 'gac2-gewex' 
		'cal'	: result = alg eq 'calipso'
		'era'	: result = alg eq 'era-i' 
		'era2'	: result = alg eq 'era-i2' 
		'myd2'	: result = alg eq 'coll6' and sat eq 'aqua' 
		'mod2'	: result = alg eq 'coll6' and sat eq 'terra' 
		'gac2'	: result = alg eq 'clara2' 
		'isp'	: result = alg eq 'isccp'
		'isp_old'	: result = alg eq 'isccp_old'
		; check also other way around
		'esacci_old': result = alg eq 'cci_old'
		'esacci': result = alg eq 'cci'
		'esacciv3': result = alg eq 'cciv3'
		'clara'	: result = alg eq 'gac' 
		'coll5'	: result = alg eq 'myd' or alg eq 'mod' 
		'patmos': result = alg eq 'pmx' 
		'patmos_old': result = alg eq 'pmx_old' 
		'claas'	: result = alg eq 'cla' 
		'gewex'	: result = alg eq 'gwx' 
		'gac2-gewex': result = alg eq 'g2_gwx' 
		'calipso': result = alg eq 'cal'
		'era-i'	: result = alg eq 'era' 
		'era-i2': result = alg eq 'era2' 
		'coll6'	: result = alg eq 'myd2' or alg eq 'mod2' 
		'clara2': result = alg eq 'gac2' 
		'isccp'	: result = alg eq 'isp'
		'isccp_old'	: result = alg eq 'isp_old'
		else	: result = 0
	endcase

	return, result

end
;------------------------------------------------------------------------------------------
function ref2algo, reference ,lower_case = lower_case, upper_case = upper_case, sat = sat

	ref = keyword_set(reference) ? strlowcase(reference[0]) : 'unknown' 

	case ref of 
		'cci_old'	: alg = 'esacci_old'
		'esacci_old': alg = 'esacci_old'
		'cci'		: alg = 'esacci'
		'esacci'	: alg = 'esacci'
		'cciv3'		: alg = 'esacciv3'
		'esacciv3'	: alg = 'esacciv3'
		'gac'		: alg = 'clara'
		'clara'		: alg = 'clara'
		'gac2'		: alg = 'clara2'
		'clara2'	: alg = 'clara2'
		'hec'		: alg = 'hector'
		'hector'	: alg = 'hector'
		'mod'		: begin & alg = 'coll5' & sat = 'terra' & end
		'coll5'		: alg = 'coll5'
		'myd'		: begin & alg = 'coll5' & sat = 'aqua' & end
		'mod2'		: begin & alg = 'coll6' & sat = 'terra' & end
		'coll6'		: alg = 'coll6'
		'myd2'		: begin & alg = 'coll6' & sat = 'aqua' & end
		'cal'		: begin & alg = 'calipso' & sat = 'calipso' & end
		'calipso'	: begin & alg = 'calipso' & sat = 'calipso' & end
		'gwx'		: alg = 'gewex'
		'gewex'		: alg = 'gewex'
		'g2_gwx'	: alg = 'gac2-gewex'
		'gac2-gewex': alg = 'gac2-gewex'
		'pmx'		: alg = 'patmos'
		'patmos'	: alg = 'patmos'
		'pmx_old'	: alg = 'patmos_old'
		'patmos_old': alg = 'patmos_old'
		'cla'		: begin & alg = 'claas' & sat = 'msg' & end
		'claas'		: begin & alg = 'claas' & sat = 'msg' & end
		'isp'		: alg = 'isccp'
		'isccp'		: alg = 'isccp'
		'isp_old'		: alg = 'isccp_old'
		'isccp_old'		: alg = 'isccp_old'
		'era'		: begin & alg = 'era-i' & sat = '' & end
		'era-i'		: begin & alg = 'era-i' & sat = '' & end
		'era2'		: begin & alg = 'era-i2' & sat = '' & end
		'era-i2'	: begin & alg = 'era-i2' & sat = '' & end
		else		: alg = 'unknown'
	endcase

	if keyword_set(sat) then begin
		sat = (keyword_set(upper_case) ? strupcase(sat) : sat)
	endif

	return , (keyword_set(upper_case) ? strupcase(alg) : strlowcase(alg))

end
;-------------------------------------------------------------------------------------------------------
function algo2ref, algo ,sat=sat,lower_case = lower_case, upper_case = upper_case

	alg = keyword_set(algo) ? strlowcase(algo[0]) : 'unknown' 

	if strlowcase(alg) eq 'coll5' and keyword_set(sat) then begin 
		if strlowcase(sat) eq 'aqua' then alg = 'myd'
		if strlowcase(sat) eq 'terra' then alg = 'mod'
	endif
	if strlowcase(alg) eq 'coll6' and keyword_set(sat) then begin 
		if strlowcase(sat) eq 'aqua' then alg = 'myd2'
		if strlowcase(sat) eq 'terra' then alg = 'mod2'
	endif
	case alg of 
		'cci_old'	: ref = 'cci_old'
		'esacci_old': ref = 'cci_old'
		'esacci-pt'	: ref = 'cci_old'
		'cci'		: ref = 'cci'
		'cciv3'		: ref = 'cciv3'
		'esacciv3'	: ref = 'cciv3'
		'esacci'	: ref = 'cci'
		'cloud_cci'	: ref = 'cci'
		'gac'		: ref = 'gac'
		'clara'		: ref = 'gac'
		'clara-a1'	: ref = 'gac'
		'gac2'		: ref = 'gac2'
		'clara2'	: ref = 'gac2'
		'clara-a2'	: ref = 'gac2'
		'hector'	: ref = 'hec'
		'hec'		: ref = 'hec'
		'mod'		: ref = 'mod'
		'coll5-terra': ref = 'mod'
		'myd'		: ref = 'myd'
		'coll5-aqua': ref = 'myd'
		'mod2'		: ref = 'mod2'
		'coll6-terra': ref = 'mod2'
		'myd2'		: ref = 'myd2'
		'coll6-aqua': ref = 'myd2'
		'cal'		: ref = 'cal'
		'calipso'	: ref = 'cal'
		'gwx'		: ref = 'gwx'
		'gewex'		: ref = 'gwx'
		'g2_gwx'	: ref = 'g2_gwx'
		'gac2-gewex': ref = 'g2_gwx'
		'cci-gewex'	: ref = 'gwx'
		'pmx'		: ref = 'pmx'
		'patmos'	: ref = 'pmx'
		'patmos-x'	: ref = 'pmx'
		'patmos_old'	: ref = 'pmx_old'
		'patmos-x_old'	: ref = 'pmx_old'
		'pmx_old'	: ref = 'pmx_old'
		'cla'		: ref = 'cla'
		'claas'		: ref = 'cla'
		'isp'		: ref = 'isp'
		'isccp'		: ref = 'isp'
		'isp_old'	: ref = 'isp_old'
		'isccp_old'	: ref = 'isp_old'
		'era'		: ref = 'era'
		'era-i'		: ref = 'era'
		'era-interim'	: ref = 'era'
		'era2'		: ref = 'era2'
		'era-i2'	: ref = 'era2'
		'era-interim2'	: ref = 'era2'
		else		: ref = 'unknown'
	endcase

	return , (keyword_set(upper_case) ? strupcase(ref) : strlowcase(ref))

end
;-------------------------------------------------------------------------------------------------------
function lat_sat_node,lat
	; old better use sat_node with longitude below
; 	returns 0 for descending and 1 for ascending
	if n_params() ne 1 then begin
		print,'% SAT_NODE: Variable is undefined: latitude'
		return,-1
	endif
	si=size(lat,/dim)
   	if n_elements(si) eq 1 then return, ([(lat[1:*]-lat[0:*]) ge 0.,( (lat[1:*]-lat[0:*]) ge 0. )[0]])
	return, ( (fltarr(si[0])+1) # ([[(lat[si[0]/2,1:*]-lat[si[0]/2,0:*]) ge 0.],[( (lat[si[0]/2,1:*]-lat[si[0]/2,0:*]) ge 0. )[*,0]]]) )
end
;-------------------------------------------------------------------------------------------------------------------------
function sat_node,lon,diff=diff,vector=vector

	if n_params() ne 1 then begin
		print,'% SAT_NODE: Variable is undefined: longitude'
		return,-1
	endif
	si = size(lon,/dim)
	lasc = fltarr(si[1]) -999.
	diff = fltarr(si[1]) -999.
	real_fill_value = -999.

	for jdim = 0, si[1] -1 do begin
		min_satzen_line = si[0]/2
		bla = min([min_satzen_line,si[0]-min_satzen_line])
		for i=1,bla do begin
			lon1 = lon[0        > (min_satzen_line-i),jdim]
			lon2 = lon[(si[0]-1)< (min_satzen_line+i),jdim]
			if (lon1 ge (real_fill_value+1.0)  and lon2 ge (real_fill_value+1.0) )  then break
		endfor

		if (lon1 ge (real_fill_value+1.0)  and lon2 ge (real_fill_value+1.0) )  then begin
			if lon1 lt -170 and lon2 gt 170 then begin
				lon1 = 360 + lon1
			endif else if lon1 gt 170 and lon2 lt -170 then begin
				lon2 = 360 + lon2
			endif
			diff[jdim] = lon1-lon2
			if lon1 lt lon2 then begin
				lasc[jdim] = 0 ;descending
			endif else if lon1 gt lon2 then begin
				lasc[jdim] = 1 ; ascending
			endif else if lon1 eq lon2 then begin
				lasc[jdim] = lasc[(jdim-1)>0]
			endif
		endif else lasc[jdim] = -999.
	endfor

	if ~keyword_set(vector) then lasc = ((fltarr(si[0])+1) # lasc)

	return,lasc

end
;-------------------------------------------------------------------------------------------------------------------------
function gmean,data,latitude,no_data_value=no_data_value,nan=nan,verbose=verbose

; 	returns global mean weighted with latitude
	if n_params() ne 2 then begin
		print,'% GMEAN: At least one input Variable is undefined: data , latitude'
		return,-1
	endif
	
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	ndv = keyword_set(no_data_value) ? no_data_value : -999.
	dat = data
	lat = latitude
	idx = where(finite(dat) and dat ne ndv[0] and finite(lat) and lat ne ndv[0],cnt)

	if cnt eq 0 then begin
		if keyword_set(verbose) then print,'No valid data found!'
		return, ( keyword_set(nan) ? !values.f_nan : -999. )
	endif

	lat=lat[idx]
	dat=dat[idx]
	weight = cosd(float(lat))
	return, total(float(dat) * weight) / total(weight)
end
;-------------------------------------------------------------------------------------------------------------------------
function gstddev,data,latitude
; 	returns global standard deviation weighted with latitude
	if n_params() ne 2 then begin
		print,'% GSTDDEV: At least one input Variable is undefined: data, latitude'
		return,-1
	endif
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	weight = cosd(float(latitude))
	nnn    = total(weight)
	xx     = (float(data))
	return, sqrt( ( total((xx)^2. * weight) - (total(xx * weight)^2. / nnn) ) / nnn )

end
;-------------------------------------------------------------------------------------------------------------------------
function gcorrelate,data1,data2,latitude
; 	returns global bias weighted with latitude
	if n_params() ne 3 then begin
		print,'% GCORRELATE: At least one input Variable is undefined: data1, data2, latitude'
		return,-1
	endif
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	weight   = cosd(double(latitude))
	nnn      = total(weight)
	sum_prod = total((double(data1) * double(data2)) * weight) 
	sum2_1   = total(double(data1)^2. * weight)
	sum2_2   = total(double(data2)^2. * weight)
	avg1     = total(double(data1) * weight) / nnn
	avg2     = total(double(data2) * weight) / nnn
	var1     = ( (sum2_1 - nnn * avg1^2.) > 0.) 
	var2     = ( (sum2_2 - nnn * avg2^2.) > 0.) 
	covar    = sum_prod - nnn * avg1 * avg2				; Verschiebungssatz Covarianz
	corr     = covar / sqrt( var1 * var2)				; Correlation

	return, float(corr)
end
;-------------------------------------------------------------------------------------------------------------------------
function gbias,data1,data2,latitude
; 	returns global bias weighted with latitude
	if n_params() ne 3 then begin
		print,'% GBIAS: At least one input Variable is undefined: data1, data2, latitude'
		return,-1
	endif
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	weight = cosd(latitude)
	return, total( (float(data1)-float(data2) ) * weight ) / total(weight)
end
;-------------------------------------------------------------------------------------------------------------------------
function grmse,data1,data2,latitude
; 	returns global rmse weighted with latitude
	if n_params() ne 3 then begin
		print,'% GRMSE: At least one input Variable is undefined: data1, data2, latitude'
		return,-1
	endif
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif
	weight = cosd(float(latitude))
	return, sqrt(total( (float(data1) - float(data2) )^2. * weight )  / total(weight) )
end
;-------------------------------------------------------------------------------------------------------------------------
function bc_rmse,bias,rmse
; 	returns bias corrected rmse
	if n_params() ne 2 then begin
		print,'% BC_RMSE: At least one input Variable is undefined: bias, rmse'
		return,-1
	endif
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif
	return, sqrt(float(rmse)^2. - float(bias)^2. )
end
;-------------------------------------------------------------------------------------------------------------------------
; PBIAS = 100 * [ sum( sim - obs ) / sum( obs ) ] 
function pbias, data1,data2,latitude

	if ~between(n_params(),2,3) then begin
		print,'% PBIAS: At least one input Variable is undefined: data1, data2 [ optional:, latitude]'
		return,-1
	endif

	if n_params() eq 3 then begin
		weight   = cosd(float(latitude))
		sum_diff = total((data1 - data2) * weight )
		sum_obs  = total((data2) * weight)
	endif else begin
		sum_diff = total(data1-data2) 
		sum_obs  = total(data2)
	endelse

	return, 100. * sum_diff / sum_obs
end
;-------------------------------------------------------------------------------------------------------------------------
function ganzahlige_vielfache, num, inkl_null = inkl_null
	dumlon = lindgen(num)+1
	dumflt = findgen(num)+1.
	result = dumlon[where(num/dumflt eq num/dumlon)]
	return,(keyword_set(inkl_null) ? [0l,result] : result)
end
;-------------------------------------------------------------------------------------------------------------------------
pro color_table_names, color_tbl_name, colors1_tbl = colors1_tbl, brewer_tbl=brewer_tbl,map_image_tbl=map_image_tbl

	vali_set_path

	file = Filepath('colors1.tbl', SUBDIRECTORY=['resource', 'colors'])		
	OPENR, lun, file, /GET_LUN
	ntables = 0B
	READU, lun, ntables
	get_names = BytArr(32, ntables)
	Point_LUN, lun, ntables * 768L + 1
	READU, lun, get_names
	FREE_LUN, LUN
	get_names = StrTrim(get_names, 2)
	colors1_tbl = get_names
	for i = 0, n_elements(get_names)-1 do colors1_tbl[i] = string((i),f='(i2)')+': CT: '+get_names[i]

	file = !BREWER_CT_FILE
	OPENR, lun, file, /GET_LUN
	ntables = 0B
	READU, lun, ntables
	get_names = BytArr(32, ntables)
	Point_LUN, lun, ntables * 768L + 1
	READU, lun, get_names
	FREE_LUN, LUN
	get_names = StrTrim(get_names, 2)
	brewer_tbl = get_names

; 	for i = 0, n_elements(get_names)-1 do brewer_tbl[i] = string((i),f='(i2)')+': Brewer: '+(strsplit(get_names[i],/ext,'('))[0]
	for i = 0, n_elements(get_names)-1 do brewer_tbl[i] = string((i),f='(i2)')+': Brewer: '+$
						strreplace(get_names[i],['Sequential','Diverging','Qualitative'],['Seq.','Div.','Qua.'])

	map_image_tbl = [ 		  $
		'Default (Rainbow)'	, $
		'Extended Rainbow'	, $
		'Cloud Mask'		, $
		'Elevation'			, $
		'Blue to Red'		, $
; 		'GIST Earth'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'GMT Globe'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'GMT Relief'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'GMT Split'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'NYT Drought'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'UKM Hadcrut'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
; 		'Temp Anomaly'		, $ ; taken from panoply , only works buggy at the moment ; uncomment to use it
		'Greyscale'		]

		color_tbl_name = [map_image_tbl,colors1_tbl,brewer_tbl]

end
;-------------------------------------------------------------------------------------------------------------------------
pro define_oplots, opl, cols, spos, linestyle, psym, ystretch, error=error, timeseries=timeseries,compare=compare,color=color
	if keyword_set(error) then spos = ("tl"+strcompress(indgen(12)+1,/rem))[(opl-1) mod 12] else $
	cb_tables = "CG"+strcompress([9,5,2,4,6,1,8,7,3,10,11,12],/rem)
	spos      = (['tl1','tr1','tl2','tr2','tl3','tr3','tl4','tr4','tl5','tr5','tl6','tr6'])[(opl-1) mod 12]	
	even      = opl/2 eq opl/2.
	gt12      = ((opl-1) / 12)
	ystretch  = fix(strmid(spos,2,1))+(gt12 * 6)
	spos      = strmid(spos,0,2)
	if keyword_set(timeseries) then begin
		; solid until no color left then dashed
		linestyle = gt12 * 2
		col_idx   = ( (opl-1) ) mod 12
	endif else if keyword_set(compare) then begin
		linestyle = opl
		col_idx   = ( (opl-1) ) mod 12
	endif else begin
		; even dashed, odd solid
		linestyle = even*2
		col_idx   = ( (opl-1)/2 ) mod 12
	endelse
	cols      = keyword_set(color) ? color : cb_tables[col_idx]
	psym      = keyword_set(timeseries) ? -8 : gt12 ? -8 : 0
end
;-------------------------------------------------------------------------------------------------------------------------
function dperc, data, value, found = found
	; some default percentiles with help of the fast median function
	found = 1
	case value[0] of
		 0./16.	: return,min(data)
		 1./16.	: return,median(data[where(data lt median(data[where(data lt median(data[where(data lt median(data))]))]))])
		 2./16.	: return,median(data[where(data lt median(data[where(data lt median(data))]))])
		 3./16.	: return,median(data[where(data lt median(data[where(data lt median(data[where(data gt median(data))]))]))])
		 4./16.	: return,median(data[where(data lt median(data))])
		 5./16.	: return,median(data[where(data lt median(data[where(data gt median(data[where(data lt median(data))]))]))])
		 6./16.	: return,median(data[where(data lt median(data[where(data gt median(data))]))])
		 7./16.	: return,median(data[where(data lt median(data[where(data gt median(data[where(data gt median(data))]))]))])
		 8./16.	: return,median(data)
		 9./16.	: return,median(data[where(data gt median(data[where(data lt median(data[where(data lt median(data))]))]))])
		10./16.	: return,median(data[where(data gt median(data[where(data lt median(data))]))])
		11./16.	: return,median(data[where(data gt median(data[where(data lt median(data[where(data gt median(data))]))]))])
		12./16.	: return,median(data[where(data gt median(data))])
		13./16.	: return,median(data[where(data gt median(data[where(data gt median(data[where(data lt median(data))]))]))])
		14./16.	: return,median(data[where(data gt median(data[where(data gt median(data))]))])
		15./16.	: return,median(data[where(data gt median(data[where(data gt median(data[where(data gt median(data))]))]))])
		16./16.	: return,max(data)
		else	: begin & found = 0 & return,-1 & end
	endcase
end
;-------------------------------------------------------------------------------------------------------------------------
; I didn't become a scientist for financial gain.
; Whatever little money you have will be just fine.
function percentile,in_data,value, niter = niter, no_data_value=no_data_value

	data = in_data

	if keyword_set(no_data_value) then begin
		idx = where(data ne no_data_value,idxcnt)
		if idxcnt gt 0 then data = data[idx]
	endif
	anzv = float(n_elements(value))
	anzd = float(n_elements(data))
	nitt = adv_keyword_set(niter) ? ( float(niter) < 4. ) : (anzd le 10. ? 0. : ( anzd le 100. ? 1. : 4  ) )
	if anzv eq 0. then return, median(data) ; default is median
	if total(between(value,0.,1.)) ne anzv then begin
		print,'each value must be a number between 0 and 1'
		stop
		return,-1
	endif
	; do normal percentile calulation 
	; for huge data arrays use keyword niter .. faster
	if anzv gt 1 or nitt eq 0. then begin
		sidx = sort(data)
		return,data[sidx[round(value*anzd) < (anzd-1) ]]
 	endif
	; try not to sort all of the array (takes long for huge arrays), 
	; try to use median function instead of reducing array size
	; at first check the obvious ones
	dum  = dperc(data,value[0],found=found)
	if found then return, dum
	cc   = ( 1./ (2.^(nitt)) )
	temp = data[where(data ge dperc(data,rnd(value[0],/down,cc)) and data lt dperc(data,rnd(value[0],/up,cc)),count)]
	sidx = sort(temp)
	fact = (value[0] - rnd(value[0],/down,cc))/cc
	return,temp[sidx[round(fact * count) < (count -1)]]
end
;-------------------------------------------------------------------------------------------------------------------------
function get_perc_from_hist, histo, value, mini, maxi, bin, data = data

	bin_center_value = (vector(mini,maxi,n_elements(histo))+bin/2.) < maxi
	count  = total(histo)
	sums   = fltarr(n_elements(histo))
	for i = 0,n_elements(histo) -1 do begin
		sums[i] = total(histo[0:i])
		if histo[i] eq 0 then continue
		data = is_defined(data) ? [data,replicate(bin_center_value[i],histo[i])] : replicate(bin_center_value[i],histo[i])
	endfor
	result = fltarr(n_elements(value))

	for i = 0,n_elements(value) -1 do begin
		aim   = count * value[i]
		idx   = where(abs(sums-aim) eq min(abs(sums-aim)),idx_cnt)
		result[i] = idx_cnt gt 0 ? bin_center_value[idx[0]] : -1
	endfor

	return, result
end
;-------------------------------------------------------------------------------------------------------------------------
function pgrid,drange,interval,log=log
	range = keyword_set(log) ? alog10((drange>0.00001)) :drange
	if not keyword_set(interval) then begin
		interval = 1000
		qw = [(float(range[1])-float(range[0]))]
		if abs(qw) lt .25 then interval = 0.02
		if abs(qw) ge .25 and abs(qw) lt 0.5 then interval = 0.05
		if abs(qw) ge .5 and abs(qw) lt 1 then interval = 0.1
		if abs(qw) ge 1 and abs(qw) lt 2 then interval = .2
		if abs(qw) ge 2 and abs(qw) lt 6 then interval = 1
		if abs(qw) ge 6 and abs(qw) lt 11 then interval = 2
		if abs(qw) ge 11 and abs(qw) lt 50 then interval = 5
		if abs(qw) ge 50 and abs(qw) lt 100 then interval = 10
		if abs(qw) ge 100 and abs(qw) lt 300 then interval = 20
		if abs(qw) ge 300 and abs(qw) lt 1000 then interval = 50
		if abs(qw) ge 1000 and abs(qw) lt 3000 then interval = 100
		if abs(qw) ge 3000 and abs(qw) lt 10000 then interval = 500
	endif

	d = rnd(range,interval)
	d = d[sort(d)]
	
	vec = vector(d[0],d[1],abs((d[1]-d[0])/(float(interval)) + 1))

	return,keyword_set(log) ? 10.^vec : vec
end
;-------------------------------------------------------------------------------------------------------------------------
function strreverse,in,found=found

	found=1
	if size(in,/type) ne 7 then begin
		print,'We want only strings!'
		found=0
		return,-1
	endif

	len = strlen(in)

	return, (len le 1 ? in : strjoin(reverse(strmid(in,findgen(len),fltarr(len)+1) )))

end
;-------------------------------------------------------------------------------------------------------------------------
; removes leading and/or trailing "trims" from the input String Numbers.
; trim can be any single character
function adv_strtrim,in,flag,trim=trim

; 	flags 0: remove trailing trims
; 	flags 1: remove leading  trims
; 	flags 2: remove leading and trailing trims

	if size(in,/type) ne 7 then begin
		print,'We want only strings!'
		return,-1
	endif

	flag = keyword_set(flag) ? flag : 0

	out = in

	char = keyword_set(trim) ? trim[0] : '0' ; zero is default
	if size(char,/type) ne 7 then begin
		print,'character to remove needs to be a string or character.'
		return,-1
	endif

	for i = 0l,n_elements(in) -1 do begin
		dum = in[i]
		if is_number(char) and ~is_number(dum) then continue
		len = strlen(dum)
		case flag of
			0	: begin
					if len gt 1 then begin
						if strmid(dum,len-1) eq char then begin
							rev = strreverse(dum)
							pos = stregex(rev,char+'+',length=len)
							cut = strmid(rev,len)
							if (strmid(cut,0,1) eq '.') and is_number(char) then cut = strmid(cut,1)
							out[i]= strreverse(cut)
						endif else if strmid(dum,len-1) eq '.' and is_number(char) then begin
							out[i]= strmid(dum,0,len-1)
						endif
					endif
				  end
			1	: begin
					if len gt 1 then begin 
						if strmid(dum,0,1) eq char then begin
							rev = dum
							pos = stregex(rev,char+'+',length=len)
							cut = strmid(rev,len)
							out[i]=cut
						endif
					endif
				  end
			2	: begin
					cut    = adv_strtrim(in[i],0,trim=char)
					out[i] = adv_strtrim(cut  ,1,trim=char)
				  end
			else:
		endcase
	endfor

	return,out
end
;-------------------------------------------------------------------------------------------------------------------------
function zonal_average, bild, latitude, fillvalue = fillvalue, lat_res = lat_res, mean = mean, median = median,lat_zon=lat_zon,nan=nan,found=found

	found=1

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	; zonal means
	fillvalue = keyword_set(fillvalue) ? fillvalue : -999.
	lat     = latitude
	aaa     = histogram(lat,bin=lat_res,reverse_indices = Ri,min=-89.99,max=89.99)
	lat_zon = fltarr(n_elements(aaa)) + (keyword_set(nan) ? !values.f_nan : fillvalue[0])
	mean_c  = fltarr(n_elements(aaa)) + (keyword_set(nan) ? !values.f_nan : fillvalue[0])
	for i   = 0,n_elements(aaa)-1 do begin & $
		if Ri[i] eq Ri[i+1] then continue & $
		dum_bild   = bild[Ri[Ri[i] : Ri[i+1]-1]] & $
		dum_idx    = where(dum_bild ne fillvalue and finite(dum_bild), chk_idx) & $
		if chk_idx gt 0 then mean_c[i] = keyword_set(mean) ? mean(dum_bild[dum_idx]) : median(dum_bild[dum_idx]) & $
		if chk_idx gt 0 then lat_zon[i] = mean(lat[Ri[Ri[i] : Ri[i+1]-1]]) & $
	endfor

	return, mean_c
end
;--------------------------------------------------------------------------------------------------------------------------
function cci_name, sat, algoname_only = algoname_only
	; e.g. convert noaa18 -> CC4CL-NOAA-18
 	names = ''
	for i = 0, n_elements(sat) -1 do begin
		dumsat = stregex(sat[i],'noaa',/bool,/fold) ? strjoin(strmid(strjoin(strsplit(sat[i],'-',/ext)),[0,4],[4,2]),'-') : sat[i]
		if total(strlowcase(dumsat) eq ['aatme','aatsrmeris','merisaatsr']) then dum = 'Fame-C-FBL3' else $
		dum = keyword_set(algoname_only) ? 'CC4CL-FBL3': 'CC4CL-FBL3-'+strupcase(dumsat)
		names = i eq 0 ? dum : [names,dum]
	endfor
	return, names
end
;--------------------------------------------------------------------------------------------------------------------------
function modis_sats,year,month,found=found

	found = 1
	aqua_start  = my_julday(2002,08,01)
	terra_start = my_julday(2000,02,01)
	if between(my_julday(year,month,01),terra_start,aqua_start,/not_include_upper ) then begin
		return, 'TERRA'
	endif else if my_julday(year,month,01) ge aqua_start then begin
		return, 'TERRA,AQUA'
	endif
	found = 0
	return,'No MODIS Sats Available'
end
;--------------------------------------------------------------------------------------------------------------------------
function atsr_prime,year,month,instrument=instrument,found=found

	found = 1
	atsr2_start = my_julday(1995,08,01)
	if my_julday(year,month,01) lt atsr2_start then begin
		found = 0
		return,'No A(A)TSR Sats Available'
	endif
	aatsr_start = my_julday(2002,07,24) ; at least RAL starts here with first L3U
	if my_julday(year,month,01) lt aatsr_start then begin
		return, keyword_set(instrument) ? 'ATSR2' : 'ERS2'
	endif else begin
		return, keyword_set(instrument) ? 'AATSR' : 'ENVISAT'
	endelse
end
;--------------------------------------------------------------------------------------------------------------------------
function noaa_primes,year,month, ampm=ampm		, $ ; ampm := 0  am,1 pm, 2 ampm
				 which=which					, $ ; which file from database
				 hirs=hirs						, $ ; Search for HIRS primes intead of AVHRR
				 no_zero=no_zero				, $ ; No leading zero in Noaa number, e.g NOAA-7 not NOAA-07
				 no_minus=no_minus				, $ ; Not NOAA-17 but NOAA17
				 include_all = include_all		, $ ; include also satellites not proccessed in cloud_cci 
				 short_names = short_names		, $
				 found = found

	found = 1
	ampm  = adv_keyword_set(ampm) ? ampm : 2 ; default ist am+pm
	hir   = keyword_set(hirs)
	mm    = fix(month)
	inc   = keyword_set(include_all)
	sht   = keyword_set(short_names)

	case fix(year) of	;   HIRS (Hector)								AVHRR (CCI,CLARA,PATMOS)
		1979: sats = hir ? [mm le 6 ? 'nn':'06','05']				: ['nn',inc ? '05':'nn']
		1980: sats = hir ? ['06','05']								: [(mm le  6 or ~inc ? 'nn':'06'),(mm le  1 and inc ? '05':'nn')]
		1981: sats = hir ? ['06','07']								: [inc ? '06':'nn',(mm le  7 ? 'nn':'07')]
		1982: sats = hir ? ['06','07']								: [(mm le  7 and inc ? '06':'nn'),'07']
		1983: sats = hir ? [mm le 4 ? '06':'08','07']				: [(mm le  4 or ~inc ? 'nn':'08'),'07']
		1984: sats = hir ? [mm le 6 ? '08':'nn','07']				: [inc ? '08':'nn','07']
		1985: sats = hir ? ['nn','09']								: [(mm le 10 and inc ? '08':'nn'),(mm le  1 ? '07':'09')]
		1986: sats = hir ? ['nn','09']								: ['nn','09']
		1987: sats = hir ? ['10','09']								: [inc ? '10':'nn','09']
		1988: sats = hir ? ['10',(mm le 10 ? '09':'11')]			: [inc ? '10':'nn',(mm le 10 ? '09':'11')]
		1989: sats = hir ? ['10','11']								: [inc ? '10':'nn','11']
		1990: sats = hir ? ['10','11']								: [inc ? '10':'nn','11']
		1991: sats = hir ? [mm le 9 ? '10':'12','11']				: [(mm le  9 ? (inc ? '10':'nn'):'12'),'11']
		1992: sats = hir ? ['12','11']								: ['12','11']
		1993: sats = hir ? ['12','11']								: ['12','11']
		1994: sats = hir ? ['12','11']								: ['12',(mm le  9 ? '11':'nn')]
		1995: sats = hir ? ['12','14']								: ['12',(mm le  1 ? 'nn':'14')]
		1996: sats = hir ? ['12','14']								: ['12','14']
		1997: sats = hir ? ['12','14']								: ['12','14']
		1998: sats = hir ? [(mm le  9 ? '12':'15'),'14']			: ['12','14']
		1999: sats = hir ? ['15','14']								: ['15','14']
		2000: sats = hir ? ['15','14']								: ['15','14']
		2001: sats = hir ? ['15',mm le 3 ? '14':'16']				: ['15',(mm le  3 ? '14':'16')]
		2002: sats = hir ? ['15','16']								: [(mm le 10 ? '15':'17'),'16']
		2003: sats = hir ? ['17','16']								: ['17','16']
		2004: sats = hir ? ['17','16']								: ['17','16']
		2005: sats = hir ? ['17','16']								: ['17',(mm le  8 ? '16':'18')]
		2006: sats = hir ? ['17','16']								: ['17','18']
		2007: sats = hir ? [mm le 6 ? '17':'MA',mm le 5 ? '16':'nn']: [(mm le  6 ? '17':'MA'),'18']
		2008: sats = hir ? ['MA','nn']								: ['MA','18']
		2009: sats = hir ? ['MA',mm le 3 ? 'nn':'18']				: ['MA',(mm le  5 ? '18':'19')]
		2010: sats = hir ? ['MA','19']								: ['MA','19']
		2011: sats = hir ? ['MA','19']								: ['MA','19']
		2012: sats = hir ? ['MA','19']								: ['MA','19']
		2013: sats = hir ? ['MA','19']								: [(mm le  4 or ~inc ? 'MA':'MB'),'19'] ; cci style so far no metopb, 
		2014: sats = hir ? ['MA','19']								: [inc ? 'MB':'MA','19'];['MB','19']	; but metopa till end.
		2015: sats = hir ? ['MA','19']								: [inc ? 'MB':'MA','19'];['MB','19']	; change it if MB is avail.
		2016: sats = hir ? ['MA','19']								: [inc ? 'MB':'MA','19'];['MB','19']
		else: sats = ['nn','nn']
	endcase

	if ampm eq 0 then begin ; am
		which = 'AM'
		sats  = sats[0]
	endif else if ampm eq 1 then begin ; pm
		which = 'PM'
		sats  = sats[1]
	endif else if ampm eq 2 then begin
		which = 'AMPM'
		idx  = where(sats ne 'nn',idxcnt)
		if idxcnt gt 0 then sats = sats[idx] else begin
			found = 0
			return,'No NOAA Sats Available'
		endelse
		for i = 0, idxcnt -1 do begin
			if strmid(sats[i],0,1) eq 'M' then begin
				sats[i] = (sht ? 'M' : 'METOP')+strmid(sats[i],1,1)
			endif else begin
				if keyword_set(no_zero) then begin
					sats[i] = (sht ? 'N': 'NOAA-')+strcompress(fix(sats[i]),/rem)
				endif else begin
					sats[i] = (sht ? 'N': 'NOAA-')+sats[i]
				endelse
			endelse
		endfor
		return, strjoin(sats,',')
	endif else begin
		found = 0
		which = 'UNKNOWN'
		return,-1
	endelse

	if sats[0] eq 'nn' then begin
		found = 0
		return, 'No NOAA '+which+' Sat Available'
	endif

	if sht then return, (strmid(sats[0],0,1) eq 'M') ? sats[0] : 'N'+(keyword_set(no_zero) ? strcompress(fix(sats),/rem) : sats[0])
	
	if sats[0] eq 'MA' and ~sht then return, 'METOPA'
	if sats[0] eq 'MB' and ~sht then return, 'METOPB'

	return, 'NOAA'+(keyword_set(no_minus)?'':'-')+(keyword_set(no_zero) ? strcompress(fix(sats),/rem) : sats[0])

end
;--------------------------------------------------------------------------------------------------------------------------
function vali_satname, satellite

	sat  = strlowcase(strjoin(strsplit(satellite,'-',/ext)))

	case sat of
		'noaa05'	: result = 'noaa5'
		'noaa06'	: result = 'noaa6'
		'noaa07'	: result = 'noaa7'
		'noaa08'	: result = 'noaa8'
		'noaa09'	: result = 'noaa9'
		'metop01'	: result = 'metopb'
		'metop02'	: result = 'metopa'
		'metop1'	: result = 'metopb'
		'metop2'	: result = 'metopa'
		'ers02'		: result = 'ers2'
		else		: result = sat
	endcase

	return, result
end
;--------------------------------------------------------------------------------------------------------------------------
function sat_ampm, satellite, ampm = ampm, avhrr_only = avhrr_only, main_sensor = main_sensor

	sat  = strlowcase(strjoin(strsplit(satellite,'-',/ext)))
	keyword_set_ampm = keyword_set(ampm)
	ampm = 2
	
	case sat of
		'noaaam'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaapm'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'tirosn'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa5'		: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa05'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa6'		: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa06'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa7'		: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa07'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa8'		: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa08'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa9'		: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa09'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa10'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa11'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa12'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa14'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa15'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa16'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa17'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'noaa18'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'noaa19'	: begin & result = 'pm'      & main_sensor = 'AVHRR' & end
		'metopa'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'metopb'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'metop01'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'metop02'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'metop1'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'metop2'	: begin & result = 'am'      & main_sensor = 'AVHRR' & end
		'npp'		: begin & result = 'pm'      & main_sensor = 'VIIRS' & end
		'allsat'	: begin & result = 'ampm'    & main_sensor = 'AVHRR' & end
		'avhrrs'	: begin & result = 'ampm'    & main_sensor = 'AVHRR' & end
		'modises'	: begin & result = 'ampm'    & main_sensor = 'MODIS' & end
		'aqua'		: begin & result = 'pm'      & main_sensor = 'MODIS' & end
		'terra'		: begin & result = 'am'      & main_sensor = 'MODIS' & end
		'envisat'	: begin & result = 'am'      & main_sensor = 'AATSR' & end
		'ers2'		: begin & result = 'am'      & main_sensor = 'ATSR2' & end
		'aatsr'		: begin & result = 'am'      & main_sensor = 'AATSR' & end
		'aatme'		: begin & result = 'am'      & main_sensor = 'AATRS+MERIS' & end
		else		: begin & result = 'unknown' & main_sensor = 'UNKNOWN' & ampm = -1 & end
	endcase

	if strmatch(result,'am') then ampm = 0
	if strmatch(result,'pm') then ampm = 1

	if keyword_set(avhrr_only) and main_sensor ne 'AVHRR' then begin
		ampm = -1
		result = 'unknown'
		main_sensor = 'UNKNOWN'
	endif

	return, keyword_set_ampm ? ampm : result

end
;--------------------------------------------------------------------------------------------------------------------------
function sat_name,algoname,sat,only_sat=only_sat,year=year,month=month,version=version,level=level

	; e.g. convert noaa18 -> Cloud_cci-NOAA-18
	satn  = keyword_set(sat)       ? strlowcase(sat)   : ''
	lev   = keyword_set(level)     ? strlowcase(level) : ''
; 	algo  = keyword_set(algoname)  ? algo2ref(algoname,sat=satn) : ''
	algo  = algo2ref(algoname,sat=satn)

	if total(satn eq ['aatme','aatsrmeris','merisaatsr','meris-aatsr']) then satn = 'MERIS+AATSR'
	if total(satn eq ['atsr','atsr2','ers','ers2']) then satn = 'ATSR2'
	if total(satn eq ['envisat','aatsr','env']) then satn = 'AATSR'
	if total(satn eq ['atsrs']) then satn = 'ATSR2-AATSR'

	case algo of
		'cci'	: begin & algon = 'Cloud_cci'  & version = keyword_set(version) ? strlowcase(version) : 'v2.0' & end
		'cci_old': begin & algon = 'Cloud_cci' & version = keyword_set(version) ? strlowcase(version) : 'v1.0' & end
		'cciv3' : begin & algon = 'Cloud_cci'  & version = keyword_set(version) ? strlowcase(version) : 'v3.0' & end
		'isp'	: return,'ISCCP'
		'isp_old': return,'ISCCP_OLD'
		'gac'	: algon = 'CLARA-A1'
		'gac2'	: algon = 'CLARA-A2'
		'l1gac' : algon = 'L1-GAC'
		'hec'	: algon = 'HECTOR'
		'myd'	: return,'COLL5-Aqua'
		'mod'	: return,'COLL5-Terra'
		'myd2'	: return,'COLL6-Aqua'
		'mod2'	: return,'COLL6-Terra'
		'gwx'	: algon = 'Cloud_cci-GEWEX'
		'g2_gwx': algon = 'CLARA-A2-GEWEX'
		'cal'	: return,'CALIPSO-Caliop'
		'era'	: return,'ERA-INTERIM '+ !ERA_I1_VERSION +' (Thr. '+ !ERA_I1_THRESHOLD +')'
		'era2'	: return,'ERA-INTERIM '+ !ERA_I1_VERSION +' (Thr. '+ !ERA_I2_THRESHOLD +')'
		'cla'	: return,'CLAAS'
		'pmx'	: begin
				algon = 'PATMOS-X'
				if lev eq 'l3c' then $
				satn  = keyword_set(year) and keyword_set(month) ? noaa_primes(year,month,ampm=sat_ampm(satn,/ampm),which=which) : satn
			  end
		'pmx_old': begin
				algon = 'PATMOS-X (old)'
				if lev eq 'l3c' then $
				satn  = keyword_set(year) and keyword_set(month) ? noaa_primes(year,month,ampm=sat_ampm(satn,/ampm),which=which,/patmos) : ''
			  end
		else	: algon = strupcase(algo)
	endcase

	if stregex(satn,'noaa',/bool,/fold) then begin
		dum = strmid(strjoin(strsplit(satn,'-',/ext)),[0,4],[4,90])
		if n_elements(dum) gt 1 then begin
			if (strlen(dum[1]) eq 1) and is_number(dum[1]) then dum[1] = string(dum[1],f='(i2.2)')
		endif
		dumsat = strupcase(strjoin(dum,'-'))
	endif else dumsat = strupcase(satn)

	;new datset names AVHRR AM,PM
	if total(dumsat eq ['NOAA-AM','NOAA-PM']) then begin
		if keyword_set(year) and keyword_set(month) and algo ne 'pmx' then begin
			dumsat = noaa_primes(year,month,ampm=sat_ampm(dumsat,/ampm),which=which)
		endif else begin
			if dumsat eq 'NOAA-AM' then dumsat = 'AVHRR-AM'
			if dumsat eq 'NOAA-PM' then dumsat = 'AVHRR-PM'
		endelse
	endif

	;MODIS
	if total(dumsat eq ['AQUA','TERRA']) and strmid(algon,0,9) eq 'Cloud_cci' then begin
		if dumsat eq 'AQUA'  then dumsat = 'MODIS-Aqua' else $
		if dumsat eq 'TERRA' then dumsat = 'MODIS-Terra'
	endif

	if keyword_set(only_sat) then return, dumsat
	return,	algon+ (keyword_set(version) and strmid(algon,0,9) eq 'Cloud_cci' ? ' '+version : '')+ $
			(keyword_set(dumsat) and keyword_set(algon) ? ' ':'') + dumsat
end
;--------------------------------------------------------------------------------------------------------------------------
function tag_name2num,struc,tag
	all_names = tag_names(struc)
	result = lonarr(n_elements(tag))
	for i = 0l, n_elements(tag) -1 do begin
		result[i] = where(strlowcase(all_names) eq strlowcase(tag[i]))
	endfor
	
	return, n_elements(result) eq 1 ? result[0] : result
end
;--------------------------------------------------------------------------------------------------------------------------
pro show_pixel_value, bild, lon_in,lat, data = data, unit = unit, wtext = wtext

	if ~keyword_set(bild) then return
	dat = keyword_set(data) ? strupcase(data[0]) : ''
	uni = keyword_set(unit) ? strupcase(unit) : ''

	if keyword_set(lon_in) and keyword_set(lat) then begin 
		lon = lon_in
		bla = where(lon_in gt 180,blacnt)
		if blacnt gt 0 then begin
			lon[bla] = lon_in[bla] - 360
		endif
		; lon,lat,bild must be of same dimension 
		xygrid = ( total(size(bild,/dim) eq size(lon,/dim)) eq size(bild,/n_dim) and total(size(bild,/dim) eq size(lat,/dim)) eq size(bild,/n_dim)  ) ? 0 : 1 
	endif else begin
		xygrid = 1
		if size(bild,/n_dim) ne 2 then return
	endelse

; 	bits_set = print, 'Bits set: ['+strjoin(strcompress( where( (byte(98) and 2^(indgen(9))) ne 0),/rem ),',')+']'
	bitset = total(strmid(dat,0,6) eq ['QCFLAG']) ? 1 : 0
; bitset=0
	value = '                                   '
	bild_dum = size(bild,/type) eq 1 ? fix(bild) : bild
	si = size(bild_dum,/dim)
	if not adv_keyword_set(wtext) then begin
		cont_base = WIDGET_BASE(TITLE=dat+'-Values ', SCR_XSIZE=250)
		wtext = WIDGET_TEXT(cont_base,value = value, SCR_XSIZE=250)
		WIDGET_CONTROL, cont_base, /REALIZE
	endif
	!mouse.button = 1
	while (!mouse.button ne 4) do begin
		cursor, x, y, /change,data = ~keyword_set(xygrid)
		if (total(finite([x,y])) eq 2) then begin
			lo = x & la = y
			if keyword_set(xygrid) then begin
				if between(lo,0,si[0]) and between(la,0,si[1]) then begin
					dum_string = '['+strjoin(string([lo,la],f='(i5)'),',')+'] '
					werte      = bitset ? '['+strjoin(strcompress( where( (fix(bild_dum[lo,la]>0) and 2^(indgen(11))) ne 0),/rem ),',')+']' : $
							strcompress(bild_dum[lo,la],/rem)
					widget_control,wtext,set_value='[x,y] '+dum_string+' '+werte
				endif else widget_control,wtext,set_value=value
			endif else begin
				qw = where(between(lon,lo-1.,lo+1.) and between(lat,la-1.,la+1.),count)
				if count gt 0 then begin
					idx=qw[where((abs(lon[qw]-lo)+abs(lat[qw] -la)) eq min(abs(lon[qw]-lo)+abs(lat[qw] -la))) ]
					dum_string = '['+strjoin(string([lo,la],f='(f6.1)'),',')+'] '
					werte      = bitset ? '['+strjoin(strcompress( where( (fix(bild_dum[idx[0]]>0) and 2^(indgen(11))) ne 0),/rem ),',')+']' : $
					strcompress(bild_dum[idx[0]],/rem)
					widget_control,wtext,set_value=(bitset ? '' : '[lon,lat] ')+dum_string+' '+werte
				endif else widget_control,wtext,set_value=value
			endelse
		endif else widget_control,wtext,set_value=value
	endwhile
	widget_control,wtext,set_value='             Pixel Values'
	if not adv_keyword_set(wtext) then WIDGET_CONTROL, cont_base, /destroy
end
;------------------------------------------------------------------------------------------
function set_limits, longitude, latitude, four_elements = four_elements, bounds = bounds, p0lon=p0lon, p0lat=p0lat,verbose=verbose

	bds = keyword_set(bounds) ? float(bounds) : 0.

	; USA:[15,-200,75,-60] Shape File

	;seperate between neg. and positive longitudes
	idx_total = where(finite(longitude),anz_total)
	if anz_total eq 0 then begin
		p0lon = 0.
		limit = four_elements eq 1 ? [-90,-180,90,180] : [0,-180,90,0,0,180,-90,0]
		return, limit
	endif
	idx_neg   = where(longitude lt 0,anz_neg)
	idx_pos   = where(longitude ge 0,anz_pos)
	rat_neg   = float(anz_neg)/float(anz_total)
	rat_pos   = float(anz_pos)/float(anz_total)

	anz = 1
	if (rat_neg gt rat_pos and rat_neg ne 1. and rat_pos lt 0.01) then begin & anz = 2 & neg = 1 & end
	if (rat_pos gt rat_neg and rat_pos ne 1. and rat_neg lt 0.01) then begin & anz = 2 & neg = 0 & end

	dum_limit = fltarr(4,anz)

	for i=0,anz-1 do begin
		if anz eq 1 then idx = idx_total
		if anz eq 2 and i eq 0 then idx = idx_neg
		if anz eq 2 and i eq 1 then idx = idx_pos

		d1=min(longitude[idx], i1,/nan)
		d2=max(longitude[idx], i2,/nan)
		d3=min(latitude [idx], i3,/nan)
		d4=max(latitude [idx], i4,/nan)

		dum_limit[*,i] = [    (d3 - bds) > (-90.), (d1 - bds), (d4 + bds) < ( 90.), (d2 + bds) ]
	endfor

	if anz eq 2 then begin
		limit    = reform(dum_limit[*,1])
		limit[0] = min(dum_limit[0,*])
		limit[2] = max(dum_limit[2,*])
		if neg eq 1 then begin
			limit[1] = dum_limit[1,1] - 360.
			limit[3] = dum_limit[3,0]
		endif else begin
			limit[1] = dum_limit[1,1]
			limit[3] = dum_limit[3,0] + 360.
		endelse
	endif else begin
		limit    = reform(dum_limit[*,0])
	endelse

	if total(abs(limit[[1,3]])) gt 360. then limit[[1,3]] = [-180.,180.]
	if total(abs(limit[[0,2]])) gt 180. then limit[[0,2]] = [- 90., 90.]

	p0lon = total(limit[[1,3]])/2.
; 	p0lat = total(limit[[0,2]])/2.

	if ~keyword_set(four_elements) then limit = [	$
			total(limit[[0,2]])/2., limit[1], $	; left 
			limit[2], total(limit[[1,3]])/2., $	; top
			total(limit[[0,2]])/2., limit[3], $	; right
			limit[0], total(limit[[1,3]])/2.]	; bottom

	if keyword_set(verbose) then begin
		print,'set_limits -> limit: ', limit
		print,'set_limits -> p0lon: ', p0lon
		if n_elements(p0lat) gt 0 then print,'set_limits -> p0lat: ', p0lat
	endif

	return, limit

end
;------------------------------------------------------------------------------------------
; sets predefined projections to use with map_image__define
pro set_proj, globe = globe, limit = limit, antarctic = antarctic, arctic = arctic, p0lon = p0lon, p0lat = p0lat,nobar=nobar, 	$ ; input 
		Goode = Goode, mollweide = mollweide, hammer = hammer, aitoff = aitoff, sinusoidal = sinusoidal,robinson=robinson, 		$ ; input 
		lambert = lambert, enhanced_robinson = enhanced_robinson, no_label=no_label,no_box =no_box,no_grid=no_grid,				$ ; input 
		ortho=ortho,iso=iso,bar_horizontal=bar_horizontal,grid=grid,londel=londel,latdel=latdel,label=label,noborder=noborder		, 	$ ; output
		no_color_bar=no_color_bar,box_axes=box_axes,no_draw_border=no_draw_border,magnify=magnify		, 						$
		lonlab=lonlab,latlab=latlab,latalign=latalign,lonalign=lonalign,lonnames=lonnames,latnames=latnames,lons=lons,lats=lats,$
		stereographic=stereographic,msg=msg,maxvalue = maxvalue, bar_format=bar_format,position = position,horizon=horizon		  ; output

	box_axes = 1
	ksl = keyword_set(limit)

	if ((keyword_set(globe) or keyword_set(antarctic) or keyword_set(arctic)   or $
		keyword_set(goode) or keyword_set(hammer)    or keyword_set(aitoff)   or $
		keyword_set(mollweide) or keyword_set(sinusoidal) or keyword_set(robinson) or $
		keyword_set(stereographic) or keyword_set(msg) or keyword_set(lambert) or keyword_set(enhanced_robinson))) then begin
		; set map_image defaults
		ortho   = 1
		iso     = 1
		bar_horizontal = 1
		horizon = 1
; 		grid    = 1
		label   = 1
		noborder= 1
		no_color_bar = 1
		box_axes = 0
		no_draw_border = 1
		if ~adv_keyword_set(magnify) then magnify = 2

		; create limit vector
		if keyword_set(antarctic) then p0lat = -90
		if keyword_set(arctic)    then p0lat =  90
		p0lat = ( -90) > ( keyword_set(p0lat) ? p0lat[0] : 0 ) <  90
		p0lon = (-360) > ( keyword_set(p0lon) ? p0lon[0] : 0 ) < 360
		if keyword_set(antarctic) or keyword_set(arctic) or keyword_set(globe) then begin
			limit = p0lat ge 0 ? 	[0.,p0lon-90.,90.-p0lat,p0lon+180.,0.,p0lon+90.,p0lat-90.,p0lon] : $
									[0.,p0lon-90.,p0lat+90.,p0lon,0.,p0lon+90.,-90.-p0lat,p0lon+180.]

			if keyword_set(antarctic) then begin
				mollweide = 0 & goode = 0 & hammer = 0 & aitoff = 0 & robinson = 0 & sinusoidal = 0
				limit = limit + [-50, 0,-50, 0,-50, 0,-50, 0]
				no_color_bar = 0
				if ~adv_keyword_set(magnify) then magnify = 1
				bar_horizontal=0
				label=1
			endif
			if keyword_set(arctic) then begin
				mollweide = 0 & goode = 0 & hammer = 0 & aitoff = 0 & robinson = 0 & sinusoidal = 0
				limit = limit + [ 50, 0, 50, 0, 50, 0, 50, 0]
				no_color_bar = 0
				if ~adv_keyword_set(magnify) then magnify = 1
				bar_horizontal=0
				label=1
			endif
			lons = vector(-180,180,13)
			lonnames=strtrim(lons, 2)
			lonnames=string(lons,format='(I4)')
			lonnames(where(lons eq -180)) = ' ' ;avoid -180 and +180 printed on same spot
		endif

		; other projections
		other_limit = ksl ? limit : [-90,((p0lon mod 360) -180),90,((p0lon mod 360) +180)]
		if keyword_set(goode) then begin & goodeshomolosine = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & box_axes = 1 & end
		if keyword_set(hammer) then begin & hammer = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & end
		if keyword_set(robinson) then begin & robinson = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & iso =1 & end
		if keyword_set(sinusoidal) then begin & sinusoidal = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & end
		if keyword_set(aitoff) then begin & aitoff = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & end
		if keyword_set(mollweide) then begin & mollweide = 1 & no_color_bar = 0 & ortho = 0  & limit = other_limit & end
		if keyword_set(stereographic) then begin & stereographic = 1 & no_color_bar = 0 & ortho = 0 & bar_horizontal = 0 & limit = [-90,((p0lon mod 360) -90),90,((p0lon mod 360) +90)] & end
		if keyword_set(lambert) then begin & print,'lambert' & ortho = 0 & stereographic = 1 & end
		if keyword_set(msg) then begin
			p0lat = ( -90) > ( keyword_set(p0lat) ? p0lat[0] : 0 ) <  90
			p0lon = (-360) > ( keyword_set(p0lon) ? p0lon[0] : 0 ) < 360
			limit = p0lat ge 0 ? 	[0.,p0lon-90.,90.-p0lat,p0lon+180.,0.,p0lon+90.,p0lat-90.,p0lon] : $
									[0.,p0lon-90.,p0lat+90.,p0lon,0.,p0lon+90.,-90.-p0lat,p0lon+180.]
			no_color_bar = 0
			bar_horizontal = 0
			no_draw_border=0
		endif
		if keyword_set(enhanced_robinson) then begin
			londel  = 60
			latdel  = 30
			lonlab =  -90
			latlab = -180
			latalign=1.2
			lonalign=0.5
			lats = vector(-90,90,7)
			latnames=string(lats,format='(I3)')
			latnames(where(lats eq 0)) = 'Eq.'
			latnames(where(lats eq -90)) = ' '
			latnames(where(lats eq  90)) = ' '
			lons = vector(-180,180,7)
			lonnames=strtrim(lons, 2)
			lonnames=string(lons,format='(I4)')
			lonnames(where(lons eq -180)) = ' '
			lonnames(where(lons eq  180)) = ' '
			robinson = 1 & no_color_bar = 0 & ortho = 0 & limit = other_limit & iso =1
		endif
	endif else if keyword_set(p0lon) and ~keyword_set(p0lat) and ~ksl then begin
		limit = [-90,((p0lon mod 360) -180),90,((p0lon mod 360) +180)]
	endif else if keyword_set(p0lat) and ksl then begin
		if total(limit eq [-90,-180,90,180]) eq 4. then free ,limit ; let map_image do the calculation
	endif else if keyword_set(p0lon) and keyword_set(p0lat) and ~ksl then begin
; stop
	endif

	if adv_keyword_set(maxvalue) then begin
		if float(maxvalue[0]) lt 0.1 then begin
			bar_format = '(G6.1)'
		endif else begin
			form_len = strlen(strcompress(floor(float(maxvalue[0])),/rem))
			plus = form_len eq 1 ? 1 : 0
			bar_format = '(f'+strcompress(form_len+3+plus,/rem)+'.'+string(1+plus,f='(i1)')+')'
		endelse
	endif

	if adv_keyword_set(nobar) then begin
		if nobar eq 1  then no_color_bar = 1 ; no color bar
		if nobar eq 2  then begin & no_color_bar = 0 & bar_horizontal = 1 & end ; force horizontal colorbar
		if nobar eq 3  then begin & no_color_bar = 0 & bar_horizontal = 0 & end ; force vertical colorbar
		if nobar eq 99 then begin & position = [0.,0.,1.,1.] & no_color_bar = 1 & no_box = 1 & end
	endif

	if keyword_set(no_box)   then begin
		label = 1
		box_axes = 0
		noborder=1
 		no_draw_border=1
	endif
	if keyword_set(no_label) or keyword_set(no_grid) then label = 0
	if keyword_set(no_grid) then horizon = 0
	
	print,'magnify: ',keyword_set(magnify) ? magnify :' not set'

end
;------------------------------------------------------------------------------------------
; stapel
; very fast down gridding using rebin. 
; does only work when shrinking array with dimensions being integral multiples 
; or factors of the original dimension (rebin criteria)
; 
; All nan's will always be treated as fillvalue
; 
function grid_down_globe, array_in, grid_res, no_data_value = no_data_value, found = found, nan_fillv = nan_fillv, $
						  sample = sample, stddev = stddev, variance = variance, total = total

	fillvalue = keyword_set(no_data_value) ? double(no_data_value[0]) : -999d0
	std = keyword_set(stddev)
	var = keyword_set(variance)
	sum = keyword_set(total)

	if ~finite(fillvalue) then begin
		fillvalue = -999d0
		nan_fillv = 1
	endif
	found = 1
	gres  = float(grid_res)
	array = double(array_in) 	; if input array is an (long-) integer, rebin wont work properly
								; because the output(average) is of same datatype, so we have to do this!!
								; for most accuracy I decided to go double precision. But this step is quite slow
								; takes almost half the time that rebin needs!! 

	odims = size(array,/dim)
	odims[0:1] = [360.,180.]/gres ; first 2 dims must be the ones to change

	fvidx = where(~finite(array),fvcnt)
	if fvcnt gt 0 then array[fvidx] = fillvalue

	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0
		return, -1
	endif

	if keyword_set(sample) then begin
		result = rebin(array,odims,/sample)
		if keyword_set(nan_fillv) then begin
			fvidx = where(result eq fillvalue,fvcnt)
			if fvcnt gt 0 then result[fvidx] = !values.f_nan
		endif
	endif else begin
		; This is how it works with rebin and fillvalues
		; AVG = ( rebin(array) * N - ( N_fv * fill_value ) / (N - N_fv)
		avg_all = rebin(array,odims)
		; fillvalues included?
		dum = array eq fillvalue
		if total(dum) eq 0. and ~(std or var or sum) then return, avg_all

		N       = double(product(size(array,/dim)/([360.,180.]/gres))) ; number of elements of new grid
		if (std or var) then avg_all2 = rebin(array^2.,odims) ; mean of squares

		anz_fv  = round(rebin(double(temporary(dum)),odims) * N) ; number of fillvalues
		tot_fv  = anz_fv * fillvalue

		divisor = double( N - anz_fv)
		fvidx   = where(anz_fv eq N,fvcnt)
		if fvcnt gt 0 then divisor[fvidx] = 1.
		if (std or var) then begin
			avg_all = ( avg_all * N - temporary(tot_fv) ) / divisor
			sum2    = ( temporary(avg_all2) * N - temporary(anz_fv) * (fillvalue)^2. )
			result  = ( (temporary(sum2) - (divisor) * temporary(avg_all)^2) > 0.) / ((temporary(divisor-1)) > 1.) 	; variance
			if std then result = sqrt(result > 0.)																	; stddev
		endif else begin
			result  = ( temporary(avg_all) * N - temporary(tot_fv) ) 												; sum
			if ~sum then result = result / temporary(divisor)														; average
		endelse
		if fvcnt gt 0 then result[fvidx] = keyword_set(nan_fillv) ? !values.f_nan : fillvalue
	endelse

	return, float(result)

end
;------------------------------------------------------------------------------------------
; stapel with main part taken from lonlat2reg.pro [P. Albert]
function sat2global, dlon, lat, in_data , no_data_value = no_data_value, grid_res = grid_res, $
					verbose = verbose, found = found, nan_fillv = nan_fillv, sample = sample, $
					skill_ref_data = skill_ref_data ; add reference data for 2D skill analysis

	found = 1
	do_view=0
	kss   = keyword_set(sample)
	lon   = dlon
	grd_res       = keyword_set(grid_res)          ? float(grid_res)      : 0.25 ; GME 30 km 1/4 grad auflösung
	no_data_val   = adv_keyword_set(no_data_value) ? float(no_data_value) : -999.
	if ~finite(no_data_val) then begin
		no_data_val = -999.
		nan_fillv   = 1
	endif
	data = in_data
	if keyword_set(skill_ref_data) then begin
		data2 = skill_ref_data
		if n_elements(data) ne n_elements(data2) then begin
			print,'Skill Reference data needs to be of same dimension as input data'
			free,data2
		endif
	endif

	idx = where(lon gt 180,idxcnt)
	if idxcnt gt 0 then begin
		print,'sat2global: Warning! Max Longitude is gt 180. Will go on, assuming that lon input is from 0-360°W.'
		lon[idx] -= 360
	endif

	fvidx = where(~finite(data),fvcnt)
	if fvcnt gt 0 then data[fvidx] = no_data_val

	arr_dim       = [360,180] / grd_res
	y_model       = long(( 90. + lat) / grd_res)
	x_model       = long((180. + lon) / grd_res)

	glon          = (indgen(arr_dim[0]) #  (intarr(arr_dim[1])+1)) * grd_res - (180.-(grd_res/2.))
	glat          = (indgen(arr_dim[1]) ## (intarr(arr_dim[0])+1)) * grd_res - ( 90.-(grd_res/2.))

	idx   = where(x_model ge 0 and x_model lt arr_dim[0] and y_model ge 0 and y_model lt arr_dim[1] and data ne no_data_val, idxcnt)
	if idxcnt eq 0 then begin
		if total(data eq no_data_val) eq n_elements(data) then begin
			; set all output to fillvalue 
			dum   = fltarr(arr_dim) + ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
			sum   = fltarr(arr_dim)
			return, { sum : dum, mean: dum, median:dum, random_sample : dum, null_sample : dum, stddev: sum, variance: sum, count: sum, lon: glon, lat: glat }
		endif
		found = 0
		return,-1
	endif

	if ~kss then begin
		si = size(data,/dim)
		do_view = size(data,/n_dim) eq 2
		minv_idx = round(si[0]/2.-1)

		lodata  = lon[idx]
		ladata  = lat[idx]
		lon_sum = fltarr(arr_dim)
		lat_sum = fltarr(arr_dim)

		view  = fltarr(arr_dim) + ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
		sum   = fltarr(arr_dim)
		sum2  = fltarr(arr_dim)
		sam   = fltarr(arr_dim)
		med   = fltarr(arr_dim)
	endif
	nnn   = lonarr(arr_dim)
	nul   = fltarr(arr_dim)
	xdata = data[idx]

	if is_defined(data2) then begin
		xdata2    = data2[idx]
		tss_data  = fltarr(arr_dim)+ ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
		pec_data  = fltarr(arr_dim)+ ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
		pod_data  = fltarr(arr_dim)+ ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
		bias_data = fltarr(arr_dim)+ ( keyword_set(nan_fillv) ? !values.f_nan : no_data_val)
	endif
	min_x = min(x_model[idx], max = max_x)
	xxx   = max_x - min_x + 1
	s     = x_model[idx] - min_x + y_model[idx] * xxx

	h = histogram(s, reverse_indices = ri)

	for i = 0ul, n_elements(h) - 1 do begin
		if ri[i] ne ri[i+1] then begin
			ix = s[ri[ri[i]]] mod xxx + min_x
			iy = s[ri[ri[i]]] / xxx
			nnn[ix, iy]  = ri[i+1] - ri[i]
			if ~kss then begin
				if do_view then view[ix, iy] = nnn[ix, iy] gt 1 ? $
				(xdata[ri[ri[i]: ri[i+1]-1]])[(where(abs(minv_idx-(ri[ri[i]: ri[i+1]-1] mod si[0])) eq $
				min(abs(minv_idx-(ri[ri[i]: ri[i+1]-1] mod si[0])))))[0]] : xdata[ri[ri[i]]]

				rdm_idx         = round(randomu(seed,1) * (nnn[ix, iy] -1))
				sam[ix, iy]     = nnn[ix, iy] gt 1 ? (xdata[ri[ri[i]: ri[i+1]-1]])[rdm_idx] : xdata[ri[ri[i]]]
				med[ix, iy]     = nnn[ix, iy] gt 1 ? median(xdata[ri[ri[i]: ri[i+1]-1]])    : xdata[ri[ri[i]]]
				sum[ix, iy]     = nnn[ix, iy] gt 1 ? total( xdata[ri[ri[i]: ri[i+1]-1]])    : xdata[ri[ri[i]]]
				sum2[ix, iy]    = nnn[ix, iy] gt 1 ? total((xdata[ri[ri[i]: ri[i+1]-1]])^2) : xdata[ri[ri[i]]]^2
				lon_sum[ix, iy] = nnn[ix, iy] gt 1 ? total( lodata[ri[ri[i]: ri[i+1]-1]])   : lodata[ri[ri[i]]]
				lat_sum[ix, iy] = nnn[ix, iy] gt 1 ? total( ladata[ri[ri[i]: ri[i+1]-1]])   : ladata[ri[ri[i]]]
			endif
			nul[ix, iy]     = nnn[ix, iy] gt 1 ? (xdata[ri[ri[i]: ri[i+1]-1]])[0]       	: xdata[ri[ri[i]]]
			if is_defined(data2) and nnn[ix, iy] gt 1 then begin
				skills_n_scores,xdata2[ri[ri[i]: ri[i+1]-1]],xdata[ri[ri[i]: ri[i+1]-1]],tss=tss,pec=pec,pod=pod
				tss_data[ix, iy] = tss
				pec_data[ix, iy] = pec
				pod_data[ix, iy] = pod
				bias_data[ix, iy] = bias(xdata[ri[ri[i]: ri[i+1]-1]],xdata2[ri[ri[i]: ri[i+1]-1]])
			endif
		endif
	endfor

	if ~kss then begin
		avg  = sum / (nnn > 1l)
		alon = lon_sum / (nnn > 1l)
		alat = lat_sum / (nnn > 1l)
		var  = ( (sum2 - nnn * avg^2) > 0.) / ((nnn-1l) > 1l)
		sdv  = sqrt(var)
	endif

	no_data_idx = where(nnn eq 0l,ndc)
	if ndc gt 0 then begin
		if is_defined(avg)   then avg[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(alon)  then alon[no_data_idx] 	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(alat)  then alat[no_data_idx] 	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(sdv)   then sdv[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(nul)   then nul[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(sam)   then sam[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(med)   then med[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(var)   then var[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(sum)   then sum[no_data_idx]  	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(data2) then tss_data[no_data_idx]	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(data2) then pec_data[no_data_idx]	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(data2) then pod_data[no_data_idx]	= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if is_defined(data2) then bias_data[no_data_idx]= keyword_set(nan_fillv) ? !values.f_nan : no_data_val
		if do_view then view[no_data_idx] = keyword_set(nan_fillv) ? !values.f_nan : no_data_val
	endif

	if is_defined(data2) then begin
		skills = {tss:tss_data,pec:pec_data,pod:pod_data,bias:bias_data}
	endif else skills =-1

	if kss then begin
		return, { null_sample : nul, lon: glon, lat: glat }
	endif else begin
		return, { sum : sum, mean: avg,mean_lat: alat,mean_lon: alon, median:med, random_sample : sam, null_sample : nul, $
		stddev: sdv, variance: var, count: nnn, lon: glon, lat: glat, nearest_nadir:view ,s:s,min_x:min_x,xxx:xxx,skills:skills}
	endelse
end
;------------------------------------------------------------------------------------------
function read_modis_obj_val, stringname, group, value = value,found=found

	val = keyword_set(value) ? value : 'VALUE'

	sub_string = strmid(stringname,strpos(stringname,group),strpos(stringname,group,/reverse_search)-strpos(stringname,group))
 	sub_string = strreplace(sub_string,'=',' = ')
	dum        = strsplit(sub_string,/ext)
; 	idx        = where(dum eq val,cnt)
	idx = where(stregex(dum,val,/fold,/bool),cnt)
	neu        = -1
	count      = 0
	for i = 0,cnt -1 do begin
		if dum[idx[i]+1] eq '=' then begin
			neu = count eq 0 ? strreplace((dum[idx[i]+2])[0],[string(10B),'"'],['','']) : $
					   [neu,strreplace((dum[idx[i]+2])[0],[string(10B),'"'],['',''])]
			count ++
		endif
	endfor
	if count eq 0 then found = 0 else found = 1
	return,(count eq 0 ? -1 : neu) 
end
;------------------------------------------------------------------------------------------
pro make_geo, file = file, lon, lat, grid_res = grid_res, verbose = verbose, dimension = dimension, found = found, $
		nise = nise,nsidc=nsidc,pick_file=pick_file, osisaf=osisaf, algo=algo, offsets = offsets, claas=claas, $
		msg = msg, cms_msg = cms_msg ; MSG: MSG with DIMS: [3712,3712]; CMS_MSG: MSG with DIMS: [3636,3636]

	ndim  = keyword_set(dimension) ? (n_elements(dimension) < 2) : 2

	if keyword_set(file) then begin
		filen = file[0]
		if stregex(file_basename(filen),'ISCCP.D2',/bool) then grid_res = 2.5
	endif
	fillv = -999.

	if keyword_set(msg) or keyword_set(cms_msg) then begin
		xrange = [0,keyword_set(cms_msg) ? 3635 : 3711] & yrange=[0,keyword_set(cms_msg) ? 3635 : 3711]
		msg_x = (indgen((xrange[1]-xrange[0])+1)+xrange[0]) #  (intarr((yrange[1]-yrange[0])+1)+1)
		msg_y = (indgen((yrange[1]-yrange[0])+1)+yrange[0]) ## (intarr((xrange[1]-xrange[0])+1)+1)
		dum = msg_to_geo(temporary(msg_x),temporary(msg_y),scale_params=scale_params,sub_sat_lon=sub_sat_lon)
		lon = dum.lon
		lat = dum.lat
		found = 1
		free, dum
		return
	endif else if keyword_set(claas) then begin
		lon_dum = vector(-89.9750,89.9750,3600.)
		lat_dum = lon_dum
		lon = lon_dum #  ( fltarr(n_elements(lat_dum))+1)
		lat = lat_dum ## ( fltarr(n_elements(lon_dum))+1) 
		free, lon_dum
		free, lat_dum
		found = 1
		return
	endif 

	if keyword_set(grid_res) then begin
		if keyword_set(offsets) then begin
			lon_dum = findgen((offsets.ELON-offsets.SLON)/grid_res) * grid_res - (((-1) * offsets.SLON) - grid_res/2.)
			lat_dum = findgen((offsets.ELAT-offsets.SLAT)/grid_res) * grid_res - (((-1) * offsets.SLAT) - grid_res/2.)
		endif else begin
			lon_dum = findgen(360./grid_res) * grid_res - (180.- grid_res/2.)
			lat_dum = findgen(180./grid_res) * grid_res - ( 90.- grid_res/2.)
		endelse
		found = 1
	endif else begin
		if keyword_set(nise) then begin
			lon_nhk = restore_var(!SAVS_DIR + 'nise/NISE_lon_nhk.sav')
			lat_nhk = restore_var(!SAVS_DIR + 'nise/NISE_lat_nhk.sav')
			lon_shk = restore_var(!SAVS_DIR + 'nise/NISE_lon_shk.sav')
			lat_shk = restore_var(!SAVS_DIR + 'nise/NISE_lat_shk.sav')
			lon_dum = [[[lon_nhk]],[[lon_shk]]]
			lat_dum = [[[lat_nhk]],[[lat_shk]]]
			found =1
		endif else if keyword_set(nsidc) then begin
			if strcompress(nsidc,/rem) eq 'south' then begin
				;print,'south'
				lat_dum=lonarr(316, 332, /NOZERO)
				openr,lun,'/cmsaf/nfshome/sstapelb/idl/pss25lats_v3.dat',/get_lun             
				READU, lun, lat_dum
				free_lun,lun
				lat_dum = rotate(lat_dum/100000.,7)
				lon_dum=lonarr(316, 332, /NOZERO)
				openr,lun,'/cmsaf/nfshome/sstapelb/idl/pss25lons_v3.dat',/get_lun             
				READU, lun, lon_dum
				free_lun,lun
				lon_dum = rotate(lon_dum/100000.,7)
			endif else begin
				;print,'north'
				lat_dum=lonarr(304, 448, /NOZERO)
				openr,lun,'/cmsaf/nfshome/sstapelb/idl/psn25lats_v3.dat',/get_lun             
				READU, lun, lat_dum
				free_lun,lun
				lat_dum = rotate(lat_dum/100000.,7)
				lon_dum=lonarr(304, 448, /NOZERO)
				openr,lun,'/cmsaf/nfshome/sstapelb/idl/psn25lons_v3.dat',/get_lun             
				READU, lun, lon_dum
				free_lun,lun
				lon_dum = rotate(lon_dum/100000.,7)
			endelse
			found =1
		endif else if keyword_set(osisaf) then begin
			if strcompress(osisaf,/rem) eq 'south' then begin
				read_data,'/cmsaf/nfshome/sstapelb/idl/osisaf_latlon_sh.hdf5','/Data/data[00]',lat_dum,fillv,found=found,verbose = verbose
				read_data,'/cmsaf/nfshome/sstapelb/idl/osisaf_latlon_sh.hdf5','/Data/data[01]',lon_dum,fillv,found=found,verbose = verbose
			endif else begin
				read_data,'/cmsaf/nfshome/sstapelb/idl/osisaf_latlon_nh.hdf5','/Data/data[00]',lat_dum,fillv,found=found,verbose = verbose
				read_data,'/cmsaf/nfshome/sstapelb/idl/osisaf_latlon_nh.hdf5','/Data/data[01]',lon_dum,fillv,found=found,verbose = verbose
			endelse
		endif else if keyword_set(file) then begin
			read_data, filen[0],'longitude',lon_dum,fillv,verbose = verbose, found = found_lon, attribute = att_lon
			if not found_lon then read_data, filen[0],'lon',lon_dum,fillv,verbose = verbose, found = found_lon, attribute = att_lon
			if not found_lon and is_hdf5(filen[0]) then read_data, filen[0],'/where/lon/data',lon_dum,fillv,verbose = verbose, found = found_lon, attribute = att_lon
			read_data, filen[0],'latitude',lat_dum,fillv,verbose = verbose, found = found_lat, attribute = att_lat
			if not found_lat then read_data, filen[0],'lat',lat_dum,fillv,verbose = verbose, found = found_lat, attribute = att_lat
			if not found_lat and is_hdf5(filen[0]) then read_data, filen[0],'/where/lat/data',lat_dum,fillv,verbose = verbose, found = found_lat, attribute = att_lat
			found = found_lon and found_lat
		endif else found = 0
	endelse

	if ~found then begin
		if keyword_set(pick_file) then begin
			ok = dialog_message('No LON/LAT info found. Press ok if you wanna choose a file.',/cancel)
			if ok eq 'OK' then begin
				filen = dialog_pickfile(path=file_dirname(filen),file=file_basename(filen))
				read_data, filen[0],'longitude',lon_dum,fillv,verbose = verbose, found = found, attribute = att_lon
				if not found then read_data, filen[0],'lon',lon_dum,fillv,verbose = verbose, found = found, attribute = att_lon
				if not found and is_hdf5(filen[0]) then read_data, filen[0],'/where/lon/data',lon_dum,fillv,verbose = verbose, found = found, attribute = att_lon
				read_data, filen[0],'latitude',lat_dum,fillv,verbose = verbose, found = found, attribute = att_lat
				if not found then read_data, filen[0],'lat',lat_dum,fillv,verbose = verbose, found = found, attribute = att_lat
				if not found and is_hdf5(filen[0]) then read_data, filen[0],'/where/lat/data',lat_dum,fillv,verbose = verbose, found = found, attribute = att_lat
			endif
		endif
		if ~found then begin
			if keyword_set(verbose) then print, 'make_geo : -> No LON/LAT found!'
			lon = fillv
			lat = fillv
			return
		endif
	endif

	if size(lon_dum,/n_dim) eq 1 and size(lat_dum,/n_dim) eq 1 and ndim eq 2 then begin
		lon = lon_dum #  ( fltarr(n_elements(lat_dum))+1)
		lat = lat_dum ## ( fltarr(n_elements(lon_dum))+1) 
		free, lon_dum
		free, lat_dum
	endif else begin
		lon = temporary(lon_dum)
		lat = temporary(lat_dum)
	endelse

	if keyword_set(dimension) and ndim eq 2 then begin
		if (size(lon,/dim))[0] lt dimension[0] or (size(lon,/dim))[1] lt dimension[1] then begin
			; shift to > +180 if granule or orbit passes dateline before congrid to real size
			mima = minmax(lon,no=-999.)
			if (mima[0] lt -179 and mima[1] gt 179) then begin
				lon_neu = lon
				aa=where(lon lt 0, count)
				if count gt 0 then lon_neu[aa] =lon_neu[aa]+360	;shift
				lon_neu = lon_neu - 180     			;translate
				lon_neu = congrid(lon_neu,dimension[0],dimension[1],/interp)
				; and back again
				lon_neu = lon_neu + 180     			;translate
				aa=where(lon_neu ge 180, count)
				if count gt 0 then lon_neu[aa]=lon_neu[aa]-360 	;shift
				lon = lon_neu
				free, lon_neu
			endif else lon = congrid(lon,dimension[0],dimension[1],/interp)
			lat = congrid(lat,dimension[0],dimension[1],/interp)
		endif
	endif

; 	; reset lon to [-180,180]
; 	idx = where(lon gt 180.,idxcnt)
; 	if idxcnt gt 0 then begin
; 		lon[idx] = lon[idx] - 360.
; 	endif
	
end
;---------------------------------------------------------------------------------------------
pro read_ncdf, 	nc_file, data, verbose = verbose, found = found	, algoname = algoname, set_fillvalue = set_fillvalue , $		;input 
				count = count, offset=offset, stride = stride, $
				bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, raw=raw, $
				attributes = attributes, var_dim_names = var_dim_names	;output

	if ~file_test(nc_file[0],/read) then begin
		if keyword_set(verbose) then begin
			print,'File not found or not readable! ',nc_file[0]
		endif
		found = 0.
		return
	endif

	ff = nc_file[0]
	
	bild_raw = 	get_ncdf_data_by_name(ff, data, count = count, offset=offset, stride = stride, $
				found = found, verbose = verbose, var_dim_names = var_dim_names, data_type=data_type)
	if not found then begin
		if keyword_set(verbose) then print, data+' not found!'
		return
	end

	if arg_present(attributes) then begin
		attributes = get_ncdf_data_by_name(ff, data, /all_attributes)
	endif

	bild_raw  = reform(bild_raw)
	raw = bild_raw
	raw_type  = size(bild_raw,/type)
	sfv = keyword_set(set_fillvalue)
	if raw_type eq 1 and sfv then begin
		; turn byte into long integer for fillvalue
		if ~between(set_fillvalue[0],0,255) then begin
			raw_type = 3
			bild_raw = long(bild_raw)
		endif
 	endif

	patmos  = 0
	title   = get_ncdf_data_by_name(ff,'title',/global_attr,verbose=verbose,found=found_title)
	if found_title then begin
		if stregex(title,'patmos',/fold,/bool) then patmos = 1
	endif else begin
		summary = get_ncdf_data_by_name(ff,'summary',/global_attr,verbose=verbose,found=found_summary)
		if found_summary then begin
			if stregex(summary,'patmos',/fold,/bool) then patmos = 1
		endif
	endelse

	scaling_method = get_ncdf_data_by_name(ff,data,attr='scaled',verbose=verbose,found=found_scme)
	scaling_method = found_scme ? scaling_method : -1

	scale  = get_ncdf_data_by_name(ff,data,attr='scale_factor',verbose=verbose,found=found_scl) 
	if not found_scl then scale  = get_ncdf_data_by_name(ff,data,attr='scale',verbose=verbose,found=found_scl) 
	if not found_scl then scale  = get_ncdf_data_by_name(ff,data,attr='gain',verbose=verbose,found=found_scl) 
	scale  = found_scl ? scale : make_array(1,val=1,type=raw_type)

	add_offset = get_ncdf_data_by_name(ff,data,attr='add_offset',verbose=verbose,found=found_ofs)
	if not found_ofs then add_offset = get_ncdf_data_by_name(ff,data,attr='offset',verbose=verbose,found=found_ofs)
	if not found_ofs then add_offset = get_ncdf_data_by_name(ff,data,attr='intercept',verbose=verbose,found=found_ofs)
	add_offset = found_ofs ? add_offset : make_array(1,val=0,type=raw_type)

	; find fillvalue if not defined set attribute fillvalue to -999. 
	dum_fv  = get_ncdf_data_by_name(ff,data,attr='_fillvalue',verbose=verbose,found=found_fvattr)
	if not found_fvattr then dum_fv  = get_ncdf_data_by_name(ff,data,attr='no_data',verbose=verbose,found=found_fvattr)
	if not found_fvattr then dum_fv  = get_ncdf_data_by_name(ff,data,attr='missing_data',verbose=verbose,found=found_fvattr)
	raw_fill_value = found_fvattr ? dum_fv : 'not_defined'

	if found_fvattr then begin
		fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : raw_fill_value[0]),type=size(scale,/type))
	endif else begin
		fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : size(scale,/type) eq 1 ? 255 : -999),type=size(scale,/type))
	endelse

	if patmos then begin
		if strcompress(raw_fill_value[0],/rem) eq 'not_defined' then begin
			raw_fill_value = [0]
			if (scaling_method[0] eq 1) then begin
				if (data_type[0] eq 'BYTE')  then raw_fill_value = [-128]
				if (data_type[0] eq 'INT') then raw_fill_value = [-32768]
			endif
		endif else begin
			if (data_type[0] eq 'BYTE') then begin
				fill_value_temp = long(raw_fill_value[0])
				if (fill_value_temp ge 128) then fill_value_temp -= 256L
				raw_fill_value = [fill_value_temp]
			endif
		endelse
		if (data_type[0] eq 'BYTE') then begin
			bild_raw = long(bild_raw)
			index = where(bild_raw ge 128L,nindex)
			if (nindex gt 0) then bild_raw[index] -= 256L
		endif
		;--- unscale data if necessary
		if (scaling_method[0] eq 1) then begin
			index = where(bild_raw eq raw_fill_value[0],n_miss)
			bild = bild_raw * scale[0] + add_offset[0]
			if (n_miss gt 0) then bild[index] = fillvalue[0]
		endif
		if (scaling_method[0] eq 0) then begin
			dum_fv = get_ncdf_data_by_name(ff,data,attr='missing_value',verbose=verbose,found=found_miss)
			missing_value = found_miss ? dum_fv[0] : raw_fill_value[0]
			bild  = bild_raw
			if missing_value[0] ne -888.0 then begin
				index = where(bild_raw eq missing_value[0],n_miss)
				if (n_miss gt 0) then bild[index] = fillvalue[0]
			endif
		endif
	endif else begin
		if found_fvattr then begin
			idx_miss  = where(bild_raw EQ raw_fill_value[0], n_miss)
			bild      = (found_scl or found_ofs) ? (bild_raw * scale[0] + add_offset[0]) : bild_raw
			IF n_miss GT 0 then bild[idx_miss] = fillvalue[0]
		endif else begin
			bild      = (found_scl or found_ofs) ? (bild_raw * scale[0] + add_offset[0]) : bild_raw
			n_miss    = 0
		endelse
	endelse

	minvalue = min((n_miss gt 0 ? bild[where(bild ne fillvalue[0])>0] : bild),max=maxvalue)

	longname     = string(get_ncdf_data_by_name(ff,data,attr='long_name',verbose=verbose,found=found_attr))
	if not found_attr then longname = 'long_name unknown'
	unit         = string(get_ncdf_data_by_name(ff,data,attr='units',verbose=verbose,found=found_attr))
	unit         = total(strcompress(unit,/rem) eq ['1','[1]','[]','-1','none','',"''",' ']) ? '' : ' ['+unit+']'
	flag_meanings = string(get_ncdf_data_by_name(ff,data,attr='flag_meanings',verbose=verbose,found=found_attr))
	flag_meanings = found_attr ? strreplace(strsplit(string(flag_meanings),/ext),['_','-','supercooled','over'],['!C','','super!Ccooled','over!C'],/fold) : '' 
	
	if strlowcase(unit) eq ' [percent]' then unit = ' ['+string(37b)+']'

	if minvalue ge maxvalue then maxvalue = minvalue + 1.
	;-----------------------------------------------------------------------------------------------------------------------

	source = get_ncdf_data_by_name(nc_file,'source',/global,found = found_source)

	if found_source then begin
		si   = size(bild,/dim)
		ndim = size(bild,/n_dim)
		if strlowcase(source[0]) eq 'era-interim' then begin
			;only if second dimension has one more line than expected in a regular grid , e.q 361 instead of 360
			if ndim eq 1 then begin
				if total(strlowcase(data) eq ['latitude','lat']) then begin
					half_grid_res = 1./((si[0]-1)/180.)/2.
					if ( (si[0]-1)/180. eq fix((si[0]-1))/180 ) then begin
						bild = vector(half_grid_res - 90., 90. - half_grid_res,si[0]-1)
					endif
				endif
				if total(strlowcase(data) eq ['longitude','lon']) then begin
					half_grid_res = 1./((si[0])/360.)/2.
					bild = vector(half_grid_res-180.,180.-half_grid_res,si[0])
				endif
			endif else if si[1] eq ((si[0])/2.+1.) and ndim le 5 then begin
				if ndim eq 2 then begin
					bild = rotate(bild,7)
					bild = congrid(bild,si[0],si[1]-1)
					bild = shift(bild,(si[0]/2.-1),0)
				endif
				if ndim eq 3 then begin
					bild_rot = bild[*,0:si[1]-2,*] * 0
					for i = 0,si[2] -1 do begin
						bild_rot[*,*,i] = congrid(shift(rotate(bild[*,*,i],7),(si[0]/2.-1),0),si[0],si[1]-1)
					endfor
					bild = temporary(bild_rot)
				endif
				if ndim eq 4 then begin
					bild_rot = bild[*,0:si[1]-2,*,*] * 0
					for i = 0,si[2] -1 do begin
						for j = 0,si[3] -1 do begin
							bild_rot[*,*,i,j] = congrid(shift(rotate(bild[*,*,i,j],7),(si[0]/2.-1),0),si[0],si[1]-1)
						endfor
					endfor
					bild = temporary(bild_rot)
				endif
				if ndim eq 5 then begin
					bild_rot = bild[*,0:si[1]-2,*,*,*] * 0
					for i = 0,si[2] -1 do begin
						for j = 0,si[3] -1 do begin
							for k = 0,si[4] -1 do begin
								bild_rot[*,*,i,j,k] = congrid(shift(rotate(bild[*,*,i,j,k],7),(si[0]/2.-1),0),si[0],si[1]-1)
							endfor
						endfor
					endfor
					bild = temporary(bild_rot)
				endif
			endif else if ndim gt 5 then begin
				print,'Found ERA-Interim data with more than 5 dimensions, dont know if it needs to be rotated! Please check!'
			endif
		endif
	endif
	;-----------------------------------------------------------------------------------------------------------------------
	if keyword_set(verbose) then begin
		print,'NCDF File        : ',nc_file
		print,'Data             :  ',data
		print,'Unit             : ' ,unit
		help, bild, raw
		print,'MIN Max Bild Raw : ', minmax(raw)
		print,'Fill Value Raw   : ', raw_fill_value
		print,'MIN Max Bild     : ', minmax(bild)
		print,'Scale            : ', scale
		print,'Offset           : ', add_offset
		print,'Fill Value       : ', fillvalue
		print,'Min value        : ', minvalue
		print,'Max value        : ', maxvalue
		print,'Longname         :  '+longname
		print,'Flag Meanings    :  '+flag_meanings
		print,'--------------------------------------------------'
	endif

	free, bild_raw

end
;---------------------------------------------------------------------------------------------
; reads scientific data (SD) from hdf (hdf-4) files
pro read_hdf4, 	hdf_file, data, verbose = verbose,find_tagnames=find_tagnames,	algoname = algoname, set_fillvalue = set_fillvalue, $	;input
		bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, found = found , attribute = attribute,raw=raw	;output

	if ~file_test(hdf_file[0]) then begin
		found = 0
		bild = -1.
		if keyword_set(verbose) then print,'hdf_file not found!'
		return
	endif

	if keyword_set(find_tagnames) then begin
		dcount = 0ul
		FileID = HDF_SD_START(hdf_file[0], /READ)
		HDF_SD_FILEINFO, FileID, datasets , attributes
		for j = 0, datasets -1 do begin
			thisSDS = HDF_SD_SELECT(FileID, j)
			HDF_SD_GETINFO, thisSDS, NAME=thisSDSName
			if strcompress(find_tagnames,/rem) eq '1' then print, thisSDSName else begin
				if stregex(thisSDSName,find_tagnames,/fold,/bool) then begin
					print,thisSDSName
					dcount++
				endif
			endelse
		endfor
		print,'Total Datasets found := ', string(strcompress(find_tagnames,/rem) eq '1' ? datasets:dcount,f='(i5)')
		HDF_SD_ENDACCESS, thisSDS
		HDF_SD_END, FileID
		return
	endif

	found     = 1
	hdf_var   = 'not yet found'
	fillvalue = -999d0
	if ~keyword_set(data) then begin
		found = 0
		bild = -1.
		if keyword_set(verbose) then print,"You need to Specify a Dataset Name! Syntax: read_hdf4, hdf_file, 'DatasetName' "
		return
	endif
	attribute = create_struct('Var',data[0])

	sd_id = HDF_SD_START(hdf_file[0],/read)
	HDF_SD_FILEINFO, sd_id , num_sds , num_attr

	; modis coll5/6 file ?
	c5_l3 = 0
	c6_l3 = 0
	c5_l2 = 0
	c6_l2 = 0
	co_l1 = 0
	FOR j=0,num_attr-1 DO BEGIN
		HDF_SD_ATTRINFO, sd_id, j, DATA=value, NAME=name

		IF stregex(name,'COREMETADATA.0',/fold,/bool) then begin
			; only coll5 or coll6 , not Albedo or BRDF Aux files
			collection = ''
			level = ''
			dum = read_modis_obj_val(value,'LOCALVERSIONID',found=found_name)
			if found_name and stregex(dum,'006',/bool,/fold) then collection = 'coll6'
			if found_name and stregex(dum,'051',/bool,/fold) then collection = 'coll5'
			dum1 = read_modis_obj_val(value,'RANGEBEGINNINGDATE',found=found_date)
			dum2 = read_modis_obj_val(value,'RANGEBEGINNINGTIME',found=found_time)
			if found_date and found_time then begin
				dum1 = strsplit(dum1,'-',/ext)
				dum2 = strsplit(dum2,':',/ext)
				usta = ymdhms2unix(dum1[0],dum1[1],dum1[2],dum2[0],dum2[1])
			endif
			dum1 = read_modis_obj_val(value,'RANGEENDINGDATE',found=found_date)
			dum2 = read_modis_obj_val(value,'RANGEENDINGTIME',found=found_time)
			if found_date and found_time then begin
				dum1 = strsplit(dum1,'-',/ext)
				dum2 = strsplit(dum2,':',/ext)
				uend = ymdhms2unix(dum1[0],dum1[1],dum1[2],dum2[0],dum2[1])
			endif
			if keyword_set(usta) and keyword_set(uend) then begin
				if (uend-usta) eq 300l    then level = 'l2'  	; one l2 granule is 5 minute
				if (uend-usta) gt 100000l then begin		; more than a day means 1 month
					level = 'l3c'
					day   = ''
					orbit = ''
				endif
			endif
			if collection eq 'coll6' and level eq 'l3c' then c6_l3 = 1
			if collection eq 'coll6' and level eq 'l2'  then c6_l2 = 1
			if collection eq 'coll5' and level eq 'l3c' then c5_l3 = 1
			if collection eq 'coll5' and level eq 'l2'  then c5_l2 = 1
		endif
	ENDFOR

	isccp  = stregex(file_basename(hdf_file),'ISCCP.D2',/bool,/fold)
	modis  = c5_l3 or c6_l3 or c5_l2 or c6_l2

	; isccp
	if isccp and is_number(data) then begin
		isccp_read_d2,bild,hdf_file,data,9,found=found
		if not found then return
		nd_idx = where(bild eq -1000.,nd_cnt)
		if nd_cnt gt 0 then bild[nd_idx] = fillvalue
		bild = shift(bild,72,0)
		mima = minmax(bild,no=fillvalue)
		minvalue = mima[0]
		maxvalue = mima[1]
		longname = strjoin((einlesen('/cmsaf/nfshome/sstapelb/idl/isccp/d2_variables.txt',start=19+fix(data),ende=19+fix(data)+1,/string))[1:*],' ')
		unit     = ''
		if keyword_set(verbose) then begin
			print,'Calculated with isccp_read_d2 routine!'
			print,'HDF4 File        : ',hdf_file
			print,'Data             :  ',data
			print,'Unit             : ' ,unit
			help, bild
			print,'Fill Value Raw   : ', -1000.
			print,'MIN Max Bild     : ', minmax(bild)
			print,'Scale            :     ???'
			print,'Offset           :     ???'
			print,'Fill Value       : ', fillvalue
			print,'Min value        : ', minvalue
			print,'Max value        : ', maxvalue
			print,'Longname         :  '+longname
			print,'--------------------------------------------------'
		endif
		HDF_SD_END,sd_id
		return
	endif

	if (c5_l3 or c6_l3) then begin

		; generell coll6 hat kein QA mehr und COMBINED VON REF UND WATER PATH GIBTS NICHT MEHR!!!
		;Change all "Cloud_Fraction_(Phase)" parameters to "Cloud_Retrieval_Fraction_(Phase)”
		pha = c5_l3 ? '' : 'Retrieval_'
		jch = c5_l3 ? 'Cloud_Optical_Thickness_ISCCP_Joint_Histogram_vs_Pressure'  : 'Cloud_Optical_Thickness_ISCCP_JHisto_vs_Pressure'
		jch_liq = c5_l3 ? 'Cloud_Optical_Thickness_Liquid_Joint_Histogram_vs_Pressure' : 'Cloud_Optical_Thickness_Liquid_JHisto_vs_Pressure'
		jch_ice = c5_l3 ? 'Cloud_Optical_Thickness_Ice_Joint_Histogram_vs_Pressure' : 'Cloud_Optical_Thickness_ICE_JHisto_vs_Pressure'
		h1d_ctt = c5_l3 ? 'Cloud_Top_Temperature_Histogram_Counts' : 'Cloud_Phase_Optical_Properties_JHisto_vs_Temperature'
		case strUpCase(data) OF
; 			;coll5/6 1d_histogramme ctp und ctt nicht! getrennt für ice+liquid
			'HIST1D_CTT'		: hdf_var = h1d_ctt
			'HIST1D_CTP'		: hdf_var = 'Cloud_Top_Pressure_Histogram_Counts'
			'HIST1D_COT_LIQ'	: hdf_var = 'Cloud_Optical_Thickness_Liquid_Histogram_Counts'
			'HIST1D_COT_ICE'	: hdf_var = 'Cloud_Optical_Thickness_Ice_Histogram_Counts'
			'HIST1D_CER_LIQ'	: hdf_var = 'Cloud_Effective_Radius_Liquid_Histogram_Counts'
			'HIST1D_CER_ICE'	: hdf_var = 'Cloud_Effective_Radius_Ice_Histogram_Counts'
			'HIST1D_CWP_LIQ'	: hdf_var = 'Cloud_Water_Path_Liquid_Histogram_Counts'
			'HIST1D_CWP_ICE'	: hdf_var = 'Cloud_Water_Path_Ice_Histogram_Counts'
			;coll5 only
			'HIST2D_CTT_CPH_DAY'	: hdf_var = 'Cloud_Phase_Infrared_Day_Joint_Histogram_vs_Temperature'
			'HIST2D_CTT_CPH_NIGHT'	: hdf_var = 'Cloud_Phase_Infrared_Night_Joint_Histogram_vs_Temperature'
			;coll6 only
			'HIST2D_CTP_CPH_DAY'	: hdf_var = 'Cloud_Phase_Infrared_Day_JHisto_vs_Pressure'
			'HIST2D_CTP_CPH_NIGHT'	: hdf_var = 'Cloud_Phase_Infrared_Night_JHisto_vs_Pressure'

			'CTP'			: hdf_var = 'Cloud_Top_Pressure_Mean_Mean'      
			'CTP_DAY'		: hdf_var = 'Cloud_Top_Pressure_Day_Mean_Mean'  
			'CTP_NIGHT'		: hdf_var = 'Cloud_Top_Pressure_Night_Mean_Mean'
			'CTP_COUNTS'		: hdf_var = 'Cloud_Top_Pressure_Pixel_Counts'   
			'CTP_HIST'		: hdf_var = 'Cloud_Top_Pressure_Histogram_Counts'
			'CTP_STD' 		: hdf_var = 'Cloud_Top_Pressure_Std_Deviation_Mean'
			'CTP_DAY_HIST'		: hdf_var = 'Cloud_Top_Pressure_Day_Histogram_Counts'
			'CTP_DAY_COUNTS'	: hdf_var = 'Cloud_Top_Pressure_Day_Pixel_Counts'
			'CTP_NIGHT_HIST'	: hdf_var = 'Cloud_Top_Pressure_Night_Histogram_Counts'
			'CTP_NIGHT_COUNTS'	: hdf_var = 'Cloud_Top_Pressure_Night_Pixel_Counts'

			'COT_COM'		: hdf_var = 'Cloud_Optical_Thickness_Combined_Mean_Mean'    
			'COT_LOG'		: hdf_var = 'Cloud_Optical_Thickness_Combined_Log_Mean_Mean'
			'COT_STD'		: hdf_var = 'Cloud_Optical_Thickness_Combined_Std_Deviation_Mean'     
			'COT_LIQ'		: hdf_var = 'Cloud_Optical_Thickness_Liquid_Mean_Mean'
			'COT_LOG_LIQ'		: hdf_var = 'Cloud_Optical_Thickness_Liquid_Log_Mean_Mean'
			'COT_ICE'		: hdf_var = 'Cloud_Optical_Thickness_Ice_Mean_Mean'
			'COT_LOG_ICE'		: hdf_var = 'Cloud_Optical_Thickness_Ice_Log_Mean_Mean'
			
			'COT_16_LIQ'		: hdf_var = 'Cloud_Optical_Thickness_16_Liquid_Mean_Mean'
			'COT_16_LIQ_STD'	: hdf_var = 'Cloud_Optical_Thickness_16_Liquid_Std_Deviation_Mean'
			'COT_16_LIQ_UNC'	: hdf_var = 'Cloud_Optical_Thickness_16_Liquid_Mean_Uncertainty'
			'COT_16_ICE'		: hdf_var = 'Cloud_Optical_Thickness_16_Ice_Mean_Mean'
			'COT_16_ICE_STD'	: hdf_var = 'Cloud_Optical_Thickness_16_Ice_Std_Deviation_Mean'
			'COT_16_ICE_UNC'	: hdf_var = 'Cloud_Optical_Thickness_16_Ice_Mean_Uncertainty'
			'COT_37_LIQ'		: hdf_var = 'Cloud_Optical_Thickness_37_Liquid_Mean_Mean'
			'COT_37_LIQ_STD'	: hdf_var = 'Cloud_Optical_Thickness_37_Liquid_Std_Deviation_Mean'
			'COT_37_LIQ_UNC'	: hdf_var = 'Cloud_Optical_Thickness_37_Liquid_Mean_Uncertainty'
			'COT_37_ICE'		: hdf_var = 'Cloud_Optical_Thickness_37_Ice_Mean_Mean'
			'COT_37_ICE_STD'	: hdf_var = 'Cloud_Optical_Thickness_37_Ice_Std_Deviation_Mean'
			'COT_37_ICE_UNC'	: hdf_var = 'Cloud_Optical_Thickness_37_Ice_Mean_Uncertainty'

			'COT_CTP_HIST2D_LIQ'	: hdf_var = jch_liq ; andere bins keine ISCCP standard bins -> nicht vergleichbar
			'COT_CTP_HIST2D_ICE'	: hdf_var = jch_ice ; andere bins keine ISCCP standard bins -> nicht vergleichbar
			'COT_CTP_HIST2D'	: hdf_var = jch

			;nur c5_l3
			'CER'			: hdf_var = 'Cloud_Effective_Radius_Combined_Mean_Mean'
			'CER_STD'		: hdf_var = 'Cloud_Effective_Radius_Combined_Std_Deviation_Mean'
			; only COLL5
			'CWP'			: hdf_var = 'Cloud_Water_Path_Combined_Mean_Mean'
			'CWP_STD'		: hdf_var = 'Cloud_Water_Path_Combined_Std_Deviation_Mean'

			'CER_LIQ'		: hdf_var = 'Cloud_Effective_Radius_Liquid_Mean_Mean'
			'CER_LIQ_STD'		: hdf_var = 'Cloud_Effective_Radius_Liquid_Std_Deviation_Mean'
			'CER_LIQ_UNC'		: hdf_var = 'Cloud_Effective_Radius_Liquid_Mean_Uncertainty'
			'CER_ICE_STD'		: hdf_var = 'Cloud_Effective_Radius_Ice_Std_Deviation_Mean''
			'CER_ICE'		: hdf_var = 'Cloud_Effective_Radius_Ice_Mean_Mean'
			'CER_ICE_UNC'		: hdf_var = 'Cloud_Effective_Radius_Ice_Mean_Uncertainty'
			'CER_16_LIQ'		: hdf_var = 'Cloud_Effective_Radius_16_Liquid_Mean_Mean'
			'CER_16_LIQ_STD'	: hdf_var = 'Cloud_Effective_Radius_16_Liquid_Std_Deviation_Mean'
			'CER_16_LIQ_UNC'	: hdf_var = 'Cloud_Effective_Radius_16_Liquid_Mean_Uncertainty'
			'CER_16_ICE'		: hdf_var = 'Cloud_Effective_Radius_16_Ice_Mean_Mean'
			'CER_16_ICE_STD'	: hdf_var = 'Cloud_Effective_Radius_16_Ice_Std_Deviation_Mean'
			'CER_16_ICE_UNC'	: hdf_var = 'Cloud_Effective_Radius_16_Ice_Mean_Uncertainty'
			'CER_37_LIQ'		: hdf_var = 'Cloud_Effective_Radius_37_Liquid_Mean_Mean'
			'CER_37_LIQ_STD'	: hdf_var = 'Cloud_Effective_Radius_37_Liquid_Std_Deviation_Mean'
			'CER_37_LIQ_UNC'	: hdf_var = 'Cloud_Effective_Radius_37_Liquid_Mean_Uncertainty'
			'CER_37_ICE'		: hdf_var = 'Cloud_Effective_Radius_37_Ice_Mean_Mean'
			'CER_37_ICE_STD'	: hdf_var = 'Cloud_Effective_Radius_37_Ice_Std_Deviation_Mean'
			'CER_37_ICE_UNC'	: hdf_var = 'Cloud_Effective_Radius_37_Ice_Mean_Uncertainty'

			'CFC'			: hdf_var = 'Cloud_Fraction_Mean_Mean'    
			'CFC_STD'		: hdf_var = 'Cloud_Fraction_Std_Deviation_Mean'     
			'CFC_COUNTS'		: hdf_var = 'Cloud_Fraction_Pixel_Counts' 
			'CFC_DAY'		: hdf_var = 'Cloud_Fraction_Day_Mean_Mean'
			'CFC_DAY_MICRO'		: hdf_var = 'Cloud_Retrieval_Fraction_Combined_FMean' ; diese muss für die optischen produkte benutzt werden
			'CFC_NIGHT'		: hdf_var = 'Cloud_Fraction_Night_Mean_Mean'
			'CFC_HIGH'		: hdf_var = 'High_Cloud_Fraction_Infrared_FMean'
			'CFC_CIRRUS'		: hdf_var = 'Cirrus_Fraction_Infrared_FMean'

			'SUNGLINT'		: hdf_var = 'Sunglint_Fraction_Day_FMean'

			; coll6 cph: "Cloud_Retrieval_Fraction_Liquid_FMean(FSTD,Pixel_Counts)"
			'CPH'			: hdf_var = 'Cloud_Phase_Infrared_Histogram_Counts'
			'CTT'			: hdf_var = 'Cloud_Top_Temperature_Mean_Mean'
			'CTT_DAY'		: hdf_var = 'Cloud_Top_Temperature_Day_Mean_Mean'
			'CTT_NIGHT'		: hdf_var = 'Cloud_Top_Temperature_Night_Mean_Mean'
			'CTT_STD'		: hdf_var = 'Cloud_Top_Temperature_Std_Deviation_Mean'
			'CTT_COUNTS'		: hdf_var = 'Cloud_Top_Temperature_Pixel_Counts'

			'LWP'			: hdf_var = 'Cloud_Water_Path_Liquid_Mean_Mean'
			'LWP_UNC'		: hdf_var = 'Cloud_Water_Path_Liquid_Mean_Uncertainty'
			'LWP_STD'		: hdf_var = 'Cloud_Water_Path_Liquid_Std_Deviation_Mean'
			; nur coll6
			'LWP_16'		: hdf_var = 'Cloud_Water_Path_16_Liquid_Mean_Mean'
			'LWP_16_UNC'		: hdf_var = 'Cloud_Water_Path_16_Liquid_Mean_Uncertainty'
			'LWP_16_STD'		: hdf_var = 'Cloud_Water_Path_16_Liquid_Std_Deviation_Mean'
			'LWP_37'		: hdf_var = 'Cloud_Water_Path_37_Liquid_Mean_Mean'
			'LWP_37_UNC'		: hdf_var = 'Cloud_Water_Path_37_Liquid_Mean_Uncertainty'
			'LWP_37_STD'		: hdf_var = 'Cloud_Water_Path_37_Liquid_Std_Deviation_Mean'
	
			'IWP'			: hdf_var = 'Cloud_Water_Path_Ice_Mean_Mean'
			'IWP_UNC'		: hdf_var = 'Cloud_Water_Path_Ice_Mean_Uncertainty'
			'IWP_STD'		: hdf_var = 'Cloud_Water_Path_Ice_Std_Deviation_Mean'
			; nur coll6
			'IWP_16'		: hdf_var = 'Cloud_Water_Path_16_Ice_Mean_Mean'
			'IWP_16_UNC'		: hdf_var = 'Cloud_Water_Path_16_Ice_Mean_Uncertainty'
			'IWP_16_STD'		: hdf_var = 'Cloud_Water_Path_16_Ice_Std_Deviation_Mean'
			'IWP_37'		: hdf_var = 'Cloud_Water_Path_37_Ice_Mean_Mean'
			'IWP_37_UNC'		: hdf_var = 'Cloud_Water_Path_37_Ice_Mean_Uncertainty'
			'IWP_37_STD'		: hdf_var = 'Cloud_Water_Path_37_Ice_Std_Deviation_Mean'

			'ZENITH_ANGLE'		: hdf_var = 'Sensor_Zenith_Mean_Mean'
			'AZIMUTH_ANGLE'		: hdf_var = 'Sensor_Azimuth_Mean_Mean'
			'LON'			: hdf_var = 'XDim'
			'LAT'			: hdf_var = 'YDim'
			else 			:
		endcase
	endif

	IF hdf_var eq 'not yet found' then hdf_var = data

	patmos = 0
	if (~modis) then begin
		for i = 0, num_attr -1 do begin
			HDF_SD_ATTRINFO,sd_id,i,data=datatt,name=name
			if strmatch(name,'processor',/fold) then proc = datatt
			if strmatch(name,'title',/fold) then title = datatt
			if strmatch(name,'summary',/fold) then summary = datatt
			if strmatch(name,'latitude_spacing',/fold)  then la_res = datatt
			if strmatch(name,'longitude_spacing',/fold) then lo_res = datatt
		endfor
		patmos = 0
		if keyword_set(proc) then begin
			if stregex(proc,'clavr',/fold,/bool) then patmos = 1
			if stregex(proc,'patmo',/fold,/bool) then patmos = 1
		endif
		if keyword_set(title) and patmos eq 0 then begin
			if stregex(title,'clavr',/fold,/bool) then patmos = 1
			if stregex(title,'patmo',/fold,/bool) then patmos = 1
		endif
		if keyword_set(summary) and patmos eq 0 then begin
			if stregex(summary,'clavr',/fold,/bool) then patmos = 1
			if stregex(summary,'patmo',/fold,/bool) then patmos = 1
		endif
	endif

	var_index = is_number(hdf_var[0]) ? hdf_var[0] < (num_sds-1) : HDF_SD_NAMETOINDEX(sd_id,hdf_var[0])

	IF var_index[0] eq -1 then begin
		; search again but case insensitive
		for j = 0, num_sds -1 do begin
			thisSDS = HDF_SD_SELECT(sd_id, j)
			HDF_SD_GETINFO, thisSDS, NAME=thisSDSName
			if strmatch(thisSDSName,hdf_var[0],/fold) then begin
				var_index = HDF_SD_NAMETOINDEX(sd_id,thisSDSName)
				HDF_SD_ENDACCESS, thisSDS
				hdf_var=thisSDSName
				break
			endif
		endfor
		if var_index[0] eq -1 then begin
			found = 0
			HDF_SD_ENDACCESS, thisSDS
			HDF_SD_END, sd_id
			bild = -1.
			if keyword_set(verbose) then print,'SDSName '+data+'('+hdf_var+') not found in '+hdf_file[0]
			return
		endif
	endif

	varid = HDF_SD_SELECT(sd_id , var_index)

	HDF_SD_GETDATA,varid,bild_raw
	raw = bild_raw
	raw_type  = size(bild_raw,/type)
	sfv = keyword_set(set_fillvalue)
	if raw_type eq 1 and sfv then begin
		; turn byte into long integer for fillvalue
		if ~between(set_fillvalue[0],0,255) then begin
			raw_type = 3
			bild_raw = long(bild_raw)
		endif
 	endif

	; attribute scale, offset, raw_fill_value, minvalue, maxvalue, unit, longname
	scale          = (make_array(1,val=1,type=raw_type))[0]
	offset         = (make_array(1,val=0,type=raw_type))[0]
	raw_fill_value = 'not_defined'
	minvalue       = (make_array(1,val=0,type=raw_type))[0]
	unit           = ''
	longname       = 'long_name unknown'
	scaling_method = -1
	missing_value  = 'not_defined'
	flag_meanings  = ''
	hdf_sd_getinfo,varid, natts=natts
	for i =0,natts-1 do begin
		HDF_SD_ATTRINFO,varid,i,data=datatt,name=name

		attribute = create_struct(attribute,idl_validname(name,/convert_all),datatt)
		; patmosx, clavrx
		if strmatch(name,'scaled',/fold)         then scaling_method = datatt
		if strmatch(name,'scaled_min',/fold)     then scale_min      = datatt
		if strmatch(name,'scaled_max',/fold)     then scale_max      = datatt
		if strmatch(name,'scaled_missing',/fold) then scale_miss     = datatt
		if strmatch(name,'range_missing',/fold)  then missing_value  = datatt
		if strmatch(name,'missing_value',/fold)  then missing_value  = datatt
		if strmatch(name,'actual_missing',/fold) then missing_value  = datatt
		if strmatch(name,'*range_min*',/fold)    then minvalue       = datatt
		if strmatch(name,'*range_max*',/fold)    then maxvalue       = datatt
		; coll5
		if strmatch(name,'valid_range',/fold)    then valid_range    = datatt
		; general
		if strmatch(name,'scale',/fold)          then scale    = datatt; else $
		if strmatch(name,'scale_factor',/fold)   then scale    = datatt; else $
		if strmatch(name,'gain',/fold)           then scale    = datatt; else $
		if strmatch(name,'*scales*',/fold)       then scale    = datatt
		if strmatch(name,'offset',/fold)         then offset   = datatt; else $
		if strmatch(name,'intercept',/fold)      then offset   = datatt; else $
		if strmatch(name,'*offset*',/fold)       then offset   = datatt 
		if strmatch(name,'*fillvalue*',/fold)    then raw_fill_value = datatt
		if strmatch(name,'*minvalue*',/fold)     then minvalue = datatt
		if strmatch(name,'*maxvalue*',/fold)     then maxvalue = datatt
		if strmatch(name,'flag_meanings',/fold)  then $
		flag_meanings = strreplace(strsplit(string(datatt),/ext),['_','-','supercooled','over'],['!C','','super!Ccooled','over!C'],/fold)
		if strmatch(name,'unit*',/fold) then begin
			unit = datatt
			unit = total(strcompress(unit,/rem) eq ['1','[1]''[]','-1','none','',"''",' ']) ? '' : ' ['+unit+']'
		endif
		if strmatch(name,'long_name',/fold)      then longname = datatt
		if strmatch(name,'longname',/fold)       then longname = datatt
	endfor

; 	fillvalue = (make_array(1,val=-999,type=size(scale,/type)))[0]
	if strcompress(raw_fill_value[0],/rem) eq 'not_defined' then begin
		fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : size(scale,/type) eq 1 ? 255:-999),type=size(scale,/type))
	endif else begin
		fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : raw_fill_value[0]),type=size(scale,/type))
	endelse

	; coll5
	if size(valid_range,/type) ne 0 and ~patmos then begin
		if n_elements(valid_range) ge 2 then begin
			minvalue = (modis) ? ( (valid_range[0] - offset[0]) * scale[0] ) : ( valid_range[0] * scale[0] + offset[0] )
			maxvalue = (modis) ? ( (valid_range[1] - offset[0]) * scale[0] ) : ( valid_range[1] * scale[0] + offset[0] )
		endif
	endif

	if patmos then begin
		hdf_sd_getinfo,varid,hdf_type=data_type
		if strcompress(raw_fill_value[0],/rem) eq 'not_defined' then begin
			raw_fill_value = [0L]
			if (scaling_method[0] eq 1) then begin
				if (data_type[0] eq 'DFNT_INT8')  then raw_fill_value = [-128]
				if (data_type[0] eq 'DFNT_INT16') then raw_fill_value = [-32768]
			endif
		endif else begin
			if (data_type[0] eq 'DFNT_INT8') then begin
				fill_value_temp = long(raw_fill_value[0])
				if (fill_value_temp ge 128) then fill_value_temp -= 256L
				raw_fill_value = [fill_value_temp]
			endif
		endelse
		if (data_type[0] eq 'DFNT_INT8') then begin
			bild_raw = long(bild_raw)
			index = where(bild_raw ge 128L,nindex)
			if (nindex gt 0) then bild_raw[index] -= 256L
		endif
		;--- unscale data if necessary
		if (scaling_method[0] eq 1) then begin
			index = where(bild_raw eq raw_fill_value[0],nbad)
			bild = bild_raw * scale[0] + offset[0]
			if (nbad gt 0) then bild[index] = fillvalue
		endif
		if (scaling_method[0] eq 0) then begin
			if strcompress(missing_value[0],/rem) eq 'not_defined' then missing_value[0] = raw_fill_value[0]
			bild  = bild_raw
			if missing_value[0] ne -888.0 then begin
				index = where(bild_raw eq missing_value[0],nbad)
				if (nbad gt 0) then bild[index] = fillvalue
			endif
		endif
	endif else begin
		bild = (modis) ? ( (bild_raw - offset[0]) * scale[0] ) : ( bild_raw * scale[0] + offset[0] )
		if strcompress(raw_fill_value,/rem) ne 'not_defined' then begin
			idx_miss = where(bild_raw EQ raw_fill_value[0], n_miss)
			IF n_miss GT 0 then bild[temporary(idx_miss)] = fillvalue
		endif
	endelse

	HDF_SD_ENDACCESS,varid
	HDF_SD_END,sd_id

	; cph muss noch berechnet werden
	if modis and strUpCase(data) eq 'CPH' then begin
		liq = reform(bild[*,*,1])
		ice = reform(bild[*,*,2])
		oth = reform(bild[*,*,3:*])
		if size(oth,/n_dim) eq 3 then oth = total(oth,3)
		liq = (liq>0) + (0.5 * (oth>0))
		ice = (ice>0) + (0.5 * (oth>0))
		bild = liq / ((liq + ice)>1)
;  		bild = (bild[*,*,1]/(total(reform(bild[*,*,1:2]),3)>1))
		idx = where(liq eq 0 and ice eq 0,idxcnt)
		if idxcnt gt 0 then bild[idx] = fillvalue
		Longname = 'Liquid Cloud Fraction from Cloud Phase Infrared Histogram Counts'
	endif

	; coll5/6 muss rotiert werden, auch YDim muss rotiert werden allerdings mit reverse
	if (c5_l3 or c6_l3) then begin
		;muss auch level2 rotiert werden??
		ndim = float(size(bild,/n_dim))
		if ndim eq 1 and strlowcase(hdf_var[0]) eq 'ydim' then bild = reverse(bild)
		if ndim eq 2 then bild = rotate(bild,7)
		if ndim eq 3 then begin
			bild_rot = bild * 0
			si = size(bild,/dim)
			for i = 0,si[2] -1 do begin
				bild_rot[*,*,i] = rotate(bild[*,*,i],7)
			endfor
			bild = temporary(bild_rot)
		endif
		if ndim eq 4 then begin
			bild_rot = bild * 0
			si = size(bild,/dim)
			for i = 0,si[2] -1 do begin
				for j = 0,si[3] -1 do begin
					bild_rot[*,*,i,j] = rotate(bild[*,*,i,j],7)
				endfor
			endfor
			bild = temporary(bild_rot)
		endif
		if ndim gt 4 then begin
			print,'Found MODIS data with more than 4 dimensions, dont know if it needs to be rotated! Please check!'
		endif
	endif

	if patmos and adv_keyword_set(la_res) and adv_keyword_set(lo_res) then begin
		si = float(size(bild,/dim))
		if total(si) lt total(rnd(si,180/lo_res)) then begin
			if size(bild,/n_dim) eq 2 then begin
				; bring auf volle Erde.
				img = make_array(360./lo_res,180./la_res,type = size(bild,/type),value=fillvalue)
			endif else begin
				img = make_array(rnd(si,180/lo_res),type = size(bild,/type),value=fillvalue)
			endelse
			img[0,0] = bild
			bild = temporary(img)
		endif
	endif

	if n_elements(maxvalue) eq 0 then maxvalue = max(bild[where(bild ne fillvalue[0])],min=minvalue)
	if minvalue gt maxvalue then maxvalue = minvalue + 1.

	if keyword_set(verbose) then begin
		print,'HDF4 File        : ',hdf_file
		print,'Data             :  ',data
		print,'Unit             : ' ,unit
		help, bild, raw
		print,'MIN Max Bild Raw : ', minmax(raw)
		print,'Fill Value Raw   : ', raw_fill_value
		print,'MIN Max Bild     : ', minmax(bild)
		print,'Scale            : ', scale
		print,'Offset           : ', offset
		print,'Fill Value       : ', fillvalue
		print,'Min value        : ', minvalue
		print,'Max value        : ', maxvalue
		print,'Longname         :  '+longname
		print,'Flag Meanings    :  '+flag_meanings
		print,'--------------------------------------------------'
	endif

	free, bild_raw

END
;-----------------------------------------------------------------------------------------------------
pro read_hdf, 	hdf_file, data, verbose = verbose ,find_tagnames=find_tagnames, algoname = algoname, set_fillvalue = set_fillvalue, $	;input
		bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, found = found , raw=raw, attribute = attribute	;output

	check = is_hdf(hdf_file, version) or H5F_IS_HDF5(hdf_file)

	if check then begin
		version = H5F_IS_HDF5(hdf_file) ? 5 : 4
		if version eq 4 then begin
			if keyword_set(verbose) then print, 'Found hdf4 file'
			read_hdf4, hdf_file, data, verbose=verbose,find_tagnames=find_tagnames,raw=raw,algoname=algoname,set_fillvalue=set_fillvalue,$
			           bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, found = found , attribute = attribute
		endif
		if version eq 5 then begin
			if keyword_set(verbose) then print, 'Found hdf5 file'
			free,att
;			fillvalue = -999.
			sfv = keyword_set(set_fillvalue)
			unit = ''
			longname = 'long_name unknown'
			raw_fill_value = 'not_defined'
			flag_meanings = ''
			found = 1
			if keyword_set(find_tagnames) then bild_raw = read_hdf5(hdf_file[0], att=att) else $
			bild_raw = read_hdf5(hdf_file[0], data, att=att)
			raw = bild_raw
			raw_type = size(bild_raw,/type)

			if size(bild_raw,/n_ele) eq 1 then begin
				bild = bild_raw
				found = raw_type eq 8 ? 1:0 ; return structure as variable
				return
			endif

			if raw_type eq 1 and sfv then begin
				; turn byte into long integer for fillvalue
				if ~between(set_fillvalue[0],0,255) then begin
					raw_type = 3
					bild_raw = long(bild_raw)
				endif
			endif
			scale    = make_array(1,val=1,type=raw_type)
			offset   = make_array(1,val=0,type=raw_type)
			minvalue = make_array(1,val=0,type=raw_type)

			maxvalue = max(bild_raw)

			if ~is_struct(att) then begin
				; is dataset member of a group?  
				dum  = strsplit(data,'/',/ext)
				ndum = n_elements(dum)
				if ndum gt 1 then begin
					;Yes:  whats the name of the group
					group_name  = strjoin(dum[0:ndum-2],'/')
					; make a list of names of the group 
					name_list = read_hdf5(hdf_file[0], att=att,/list,group_name)
					if is_string(name_list) then begin
						fileid = h5f_open(hdf_file[0])
						; attach all attributes found in each subgroup or dataset
						for i = 0, n_elements(name_list) -1 do begin
							dum = read_hdf5(hdf_file[0], att=att1,/list,name_list[i])
							if is_struct(att1) then att = is_defined(att) ? create_struct(att,att1) : att1
						endfor
					endif
				endif
			endif

			if is_struct(att) then begin
				pos = where(stregex(tag_names(att),'scale',/fold,/bool) eq 1, pos_cnt)
				pos = where(stregex(tag_names(att),'scaling_factor',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'gain',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then scale = att.(pos[0])

				pos = where(stregex(tag_names(att),'offset',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'intercept',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then offset = att.(pos[0])

				pos = where(stregex(tag_names(att),'fillvalue',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'missingdata',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'no_data',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'nodata' ,/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then begin
					raw_fill_value = att.(pos[0])
					fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : raw_fill_value[0]),type=size(scale,/type))
					idx_miss = where(bild_raw EQ raw_fill_value, n_miss)
					bild = bild_raw * scale[0] + offset[0]
					IF n_miss GT 0 then bild[idx_miss] = fillvalue
				endif else begin
					raw_fill_value = 'not_defined'
					fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : -999),type=size(scale,/type))
					bild = bild_raw * scale[0] + offset[0]
					n_miss = 0
				endelse
				pos = where(stregex(tag_names(att),'unit',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then unit = att.(pos[0])
				unit = total(strcompress(unit,/rem) eq ['[]','-1','none','',"''",' ']) ? '' : ' ['+unit+']'

				pos = where(stregex(tag_names(att),'long_name',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'longname',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt eq 0 then pos = where(stregex(tag_names(att),'description',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then longname = att.(pos[0])

				minvalue = min((n_miss gt 0 ? bild[where(bild_raw ne raw_fill_value[0])] : bild),max=maxvalue)
				pos = where(stregex(tag_names(att),'minvalue',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then minvalue = att.(pos[0]) * scale + offset

				pos = where(stregex(tag_names(att),'maxvalue',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then maxvalue = att.(pos[0]) * scale + offset

				pos = where(stregex(tag_names(att),'flag_meanings',/fold,/bool) eq 1, pos_cnt)
				if pos_cnt ne 0 then flag_meanings = $
				strreplace(strsplit(string(att.(pos[0])),/ext),['_','-','supercooled','over'],['!C','','super!Ccooled','over!C'],/fold)
			endif else begin
				bild = bild_raw
				fillvalue = make_array(1,val=(sfv ? set_fillvalue[0] : -999),type=size(bild,/type))
			endelse

			if keyword_set(verbose) then begin
				print,'HDF5 File        : ',hdf_file
				print,'Data             :  ',data
				print,'Unit             : ' ,unit
				help, bild, raw
				print,'MIN Max Bild Raw : ', minmax(raw)
				print,'Fill Value Raw   : ', raw_fill_value
				print,'MIN Max Bild     : ', minmax(bild)
				print,'Scale            : ', scale
				print,'Offset           : ', offset
				print,'Fill Value       : ', fillvalue
				print,'Min value        : ', minvalue
				print,'Max value        : ', maxvalue
				print,'Longname         :  '+longname
				print,'Flag Meanings    :  '+flag_meanings
				print,'--------------------------------------------------'
			endif
		free, bild_raw
		endif
	endif else print, 'No hdf file found : '+hdf_file[0]

end
;-----------------------------------------------------------------------------------------------------
function read_cmsaf_seviri, file, cloud_data, fillvalue = fillvalue, longname = longname, structure = structure, found = found, $
				  minvalue = minvalue, maxvalue = maxvalue, unit = unit, verbose = verbose

	found = 0
	if n_params() ne 2 then begin
		print,'Syntax: RESULT = read_cmsaf_seviri( hdf5_file, cloud_data_name, fillvalue = fillvalue, structure = structure)'
		return, -1
	endif

	if ~is_hdf5(file[0]) then begin
		print,file_test(file) ? 'File is not a hdf5 file! ' : 'File not found! ',file
		return,-1
	endif

	; Groß und Kleinschreibung unterscheidet sich von file zu file!! und h5d_open ist case sensitiv!!
	; cloud_data = 
	;	CPP: cot,reff,cwp,cph,cph_ir,dcot,dreff,dcwp
	;	CTX: CTTH_EFFECT,CTTH_HEIGHT,CTTH_PRESS,CTTH_QUALITY,CTTH_TEMPER
	;	CTY: CT,CT_PHASE,CT_QUALITY
	;	CFC: CMa,CMa_DUST,CMa_QUALITY,CMa_TEST,CMa_VOLCANIC
	ftype   = strlowcase(strmid(file_basename(file[0]),0,3))
	cl_data = ftype eq 'cpp' ? strlowcase(cloud_data) : strupcase(cloud_data)
	if strmid(strlowcase(cl_data),0,3) eq 'cma' then $
	cl_data = strupcase(strmid(cl_data,0,2))+strlowcase(strmid(cl_data,2,1))+strupcase(strmid(cl_data,3))

	; aliase
	case strlowcase(cl_data) of
		'ctp'	: cl_data = 'CTTH_PRESS'
		'ctt'	: cl_data = 'CTTH_TEMPER'
		'cth'	: cl_data = 'CTTH_HEIGHT'
		'cm'	: cl_data = 'CMa'
		'clm'	: cl_data = 'CMa'
		else	: 
	endcase

	if keyword_set(structure) then begin
		res = h5_parse(file,/read_data)
		found = 1
		return, res
	endif

	fileID      = h5f_open( file[0] )
	if ~is_h5data(fileid, cl_data) then begin
		print,'Name "'+cl_data+'" not found in hdf file!'
		return,-1 
	endif
	dataID      = h5d_open(fileID, cl_data )
	data        = rotate( h5d_read(dataID) ,7 )
	gain        = 1.
	offset      = 0.
	no_data_val = (make_array(1,type=size(data,/type),value=-1))[0]
	longname    = cl_data
	n_att       = h5a_get_num_attrs(dataid)
	unit        = ''
	if n_att gt 0 then begin
		for j = 0, n_att - 1 do begin
			att_id   = h5a_open_idx(dataid, j)
			name     = h5a_get_name(att_id)
			if stregex(name,'unit',/fold,/bool)        then unit        = h5a_read(att_id)
			if stregex(name,'gain',/fold,/bool)        then gain        = h5a_read(att_id)
			if stregex(name,'scal',/fold,/bool)        then gain        = h5a_read(att_id)
			if stregex(name,'offset',/fold,/bool)      then offset      = h5a_read(att_id)
			if stregex(name,'intercept',/fold,/bool)   then offset      = h5a_read(att_id)
			if stregex(name,'fill_val',/fold,/bool)    then no_data_val = h5a_read(att_id)
			if stregex(name,'fillval',/fold,/bool)     then no_data_val = h5a_read(att_id)
			if stregex(name,'no_data_val',/fold,/bool) then no_data_val = h5a_read(att_id)
			if stregex(name,'description',/fold,/bool) then longname    = h5a_read(att_id)
			if stregex(name,'longname',/fold,/bool)    then longname    = h5a_read(att_id)
			if stregex(name,'product',/fold,/bool)     then longname    = h5a_read(att_id)
			h5a_close, att_id
		endfor
	endif
	H5D_CLOSE,dataID
	H5F_CLOSE,fileID

	data        = data * gain + offset
	no_data_val = no_data_val * gain + offset
	; bring auf volle Seviri Scheibe.
	img = make_array(3712,3712,type = size(data,/type),value=no_data_val)
	si  = size(data,/dim)
	img[(3712-si[0])/2.,(3712-si[1])/2.] = data
	no_data_idx = where(img eq no_data_val[0],no_data_count)
	fillvalue = make_array(1,type = size(img,/type),value = keyword_set(fillvalue) ? fillvalue : -999)
	if no_data_count < 1 then img[no_data_idx] = fillvalue
	found    = 1
	mima     = minmax(img,no_data_value=fillvalue)
	minvalue = mima[0]
	maxvalue = mima[1]
	if keyword_set(verbose) then begin
		print,'HDF5 SEVIRI File : ',file
		print,'Data             :  ',cl_data
		print,'Unit             : ' ,unit
		help, img, data
		print,'MIN Max Bild Raw : ', minmax(data)
		print,'Fill Value Raw   : ', no_data_val
		print,'MIN Max Bild     : ', minmax(img)
		print,'Scale            : ', gain
		print,'Offset           : ', offset
		print,'Fill Value       : ', fillvalue
		print,'Min value        : ', minvalue
		print,'Max value        : ', maxvalue
		print,'Longname         :  '+longname
		print,'--------------------------------------------------'
	endif
	free, data
	return, img

end
;-----------------------------------------------------------------------------------------------------
pro read_data, 	filename, data, verbose = verbose, found = found, $;input 
				algoname = algoname, silent = silent, set_fillvalue = set_fillvalue, $;input 
				count = count, offset = offset, stride = stride, $;input 
				bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, $	;output
				raw=raw,attribute = attribute, var_dim_names = var_dim_names 	;output

	if keyword_set(verbose) then z=systime(1)

	file = filename[0]

	if is_compressed(file,/gzip) then begin
		ff = adv_tempname('dummy')
		spawn,'zcat '+ file +' >'+ ff
		if file_test(ff,/zero) then begin
			file_delete, ff, /allow
			found = 0
			return
		endif
		rm_comressed_file = 1
		file = ff
	endif else rm_comressed_file = 0

	found = 0

	if is_hdf(file)  then read_hdf ,file, data, verbose = verbose, found = found, raw=raw, algoname = algoname, set_fillvalue = set_fillvalue, $
					bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, attribute = attribute else   $
; 	if is_hdf(file) or H5F_IS_HDF5(file) then read_hdf ,file, data, verbose = verbose, found = found, raw=raw, algoname = algoname, set_fillvalue = set_fillvalue, $
; 					bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, attribute = attribute else   $
	if is_ncdf(file) then read_ncdf,file, data, verbose = verbose, found = found, raw=raw, algoname = algoname, set_fillvalue = set_fillvalue, $
					bild, fillvalue, minvalue, maxvalue, longname, unit, flag_meanings, attribute = attribute, $
					var_dim_names = var_dim_names, count = count, offset = offset, stride = stride else   $
	if ~keyword_set(silent) then $
	ok = dialog_message('The file '+file+' is neither a netcdf nor a hdf file. Note netcdf4 files are not supported in IDL versions below 8!')

	if keyword_set(verbose) then print,'validation_tool_box::read_data: ',systime(1)-z
	if keyword_set(maxvalue) then begin
		if stregex(strcompress(maxvalue),'e',/fold,/bool) then maxvalue = maxvalue *1d 
	endif
	if keyword_set(minvalue) then begin
		if stregex(strcompress(minvalue),'e',/fold,/bool) then minvalue = minvalue *1d 
	endif
	
	if rm_comressed_file then file_delete, file, /allow

end
;-----------------------------------------------------------------------------------------------------
function get_filename, year, month, day, data=data, satellite=satellite, instrument = instrument, $
					 algo = algo, found = found, level = level, node = node , $
					 silent = silent, orbit = orbit, dirname = dirname	, $
					 filename = filename, version = version, $
					 no_recursive_search = no_recursive_search,$
					 gewex_style = gewex_style ; gewex_style = [0130PM,0130AM,0730PM,0730AM,0130AMPM,0730AMPM,AMPM]

	filen = '-1'
	addon = ''
	sil   = keyword_set(silent)
	din   = keyword_set(dirname)
	if n_params() lt 1 then begin
		if ~sil then print, "Syntax error : result = get_filename(year,month,day,/orbit,/node,/data,/satellite,/instrument,/algo,/level,/silent,/found)"
		found = 0.
		return, filen
	endif
	yyyy  = string(year,f='(i4.4)')
	mm    = keyword_set(month) ? strcompress(month,/rem) eq '??' ? month : string(month,f='(i2.2)') : ''
	dd    = keyword_set(day) ? strcompress(day,/rem) : ''
	lev   = keyword_set(level) ? strlowcase(level) : 'l3c'
	if ( lev eq 'l2' or lev eq 'l3u' and not keyword_set(day) ) then begin
		if ~sil then print, 'l2 or l3u data requires a day input!'
		found = 0.
		return, filen
	endif
	if lev eq 'l3c' then dd = ''
	if keyword_set(dd) then dd = string(dd,f='(i2.2)')
	orb   = adv_keyword_set(orbit) ? strcompress(orbit,/rem) : '' 
	sat   = keyword_set(satellite)  ? strupcase(strjoin(strsplit(satellite,/ext,'-')))  : 'Sat Unknown'
	if stregex(sat,'NOAA',/bool) then sat = strjoin(strmid(sat,[0,4],[4,2]),'-')
	alg   = keyword_set(algo) ? ref2algo(algo,sat=sat,/upper)  : ''
	dat   = keyword_set(data) ? strlowcase(data) : ''
	if strmid(alg,0,6) eq 'ESACCI' and total(sat eq ['AVHRRS','MODISES','ALLSAT']) then lev = 'l3s'
	if alg eq 'CALIPSO' then sat = 'CALIPSO'

	if alg eq 'SELECT' then begin
		DEFSYSV,'!SELECTED_FILE', exists = select_file_exists
		filename = dialog_pickfile(	path = (select_file_exists ? file_dirname ( !SELECTED_FILE ) : !STD_DIR ) , $
									file = (select_file_exists ? file_basename( !SELECTED_FILE ) : '' ) , $
									filter = '*.nc;*.ncd;*.ncdf;*.hdf,*.h5')
		if file_test(filename) then begin
			DEFSYSV,'!SELECTED_FILE' , filename
			obj = obj_new('ncdf_data')
			ok  = obj -> get_file_infos(infile=filename[0])
			obj_destroy, obj
			satellite	= strlowcase(ok.SATNAME)
			year 		= strlowcase(ok.YEAR)
			month 		= strlowcase(ok.MONTH)
			day 		= strlowcase(ok.DAY)
			algo 		= strlowcase(ok.ALGONAME)
			level 		= strlowcase(ok.LEVEL)
			version 	= strlowcase(ok.VERSION)
			datum 		= strlowcase(ok.DATUM)
			orbit 		= strlowcase(ok.ORBIT)
			reference	= strlowcase(ok.REFERENCE)
			if algo eq 'ERA-I' then get_era_info, filename[0],algo = algo, /set_as_sysvar
		endif
		return,filename
	endif

	if not keyword_set(instrument) then begin
		inst = 'no_Instrument_defined'
		if stregex(sat,'CALIPSO',/bool) then inst = 'CALIPSO'
		if stregex(sat,'NOAA',/bool)    then inst = 'AVHRR'
		if stregex(sat,'AVHRRS',/bool)  then inst = 'AVHRR'
		if stregex(sat,'ALLSAT',/bool) and strmid(alg,0,5) eq 'CLARA'  then inst = 'AVHRR'
		if stregex(sat,'ALLSAT',/bool) and strmid(alg,0,6) eq 'ESACCI' then inst = 'ALL'
		if stregex(sat,'METOP',/bool)   then inst = 'AVHRR'
		if stregex(sat,'MSG',/bool)     then inst = 'SEVIRI'
		if stregex(sat,'METEO',/bool)   then inst = 'SEVIRI'
		if stregex(alg,'CLAAS',/bool)   then inst = 'SEVIRI'
		if stregex(sat,'AQUA',/bool)    then inst = 'MODIS'
		if stregex(sat,'TERRA',/bool)   then inst = 'MODIS'
		if stregex(sat,'MODISES',/bool) then inst = 'MODIS'
		if stregex(sat,'AATSR',/bool)   then inst = 'AATSR'
		if stregex(sat,'ATSRS',/bool)   then begin
			inst = atsr_prime(yyyy,mm,/inst) ; either ATSR2 or AATSR use prime 
			sat  = atsr_prime(yyyy,mm)	 ; either ers2 or envisat use prime
		endif
		if stregex(sat,'ATSR2',/bool)   then inst = 'ATSR2'
		if stregex(sat,'ENVISAT',/bool) then inst = 'AATSR'
		if stregex(sat,'ERS',/bool)     then inst = 'ATSR2'
		if stregex(sat,'AATME',/bool)   then inst = 'MERISAATSR'
		if stregex(alg,'ERA',/bool)     then inst = 'MODEL'
		if stregex(alg,'ISCCP',/bool)   then inst = 'AVHRR'
		if stregex(alg,'ISCCP_OLD',/bool)   then inst = 'AVHRR'
	endif else inst  = strupcase(instrument)

	if alg eq 'HECTOR' and inst eq 'AVHRR' then inst = 'HIRS'

	case inst of
		'CALIPSO': begin
				if lev eq 'l2' then begin
					km1 = 1 ; 1: take 1kmClay or 0: take 5kmClay
					if keyword_set(node) then begin
						dn = node eq 'asc' ? 'D' : 'N' ; asc:daylight,desc:night
					endif else dn = '*'
					orbdum = strlen(orb) eq 4 ? strjoin(strmid(orb,[0,2],[2,2]),'-') : '*'
					zwischdir = km1 ? '1km' : '5km'
; 					dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld1/thanschm/VALIDATION/DATASETS/CALIOP/'+zwischdir+yyyy+'/'+mm+'/'+dd+'/'
					dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld8/EXTERNAL_DATA/CALIPSO/LEVEL2/'+zwischdir+'/'+yyyy+'/'+mm+'/'+dd+'/'
					filen = dir+'CAL_LID_L2_0?kmCLay-*-V3-01.'+yyyy+'-'+mm+'-'+dd+'T'+orbdum+'-*Z'+dn+'.hdf'
				endif else if alg eq 'CALIPSO' and (lev eq 'l3c' or lev eq 'l3s') then begin
; 					dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld6/mstengel/data/Calipso/CLIM/'+yyyy+'/'
; 					filen = dir+'CALIPSO_CTPCFC_climatology_deg2.0_'+yyyy+mm+'.nc'
; 					dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld7/thanschm/DATASET/CALIPSO_L3/'+yyyy+'/'
					dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld8/EXTERNAL_DATA/CALIPSO/LEVEL3/'+yyyy+'/'
					filen = dir+'CPRmm'+yyyy+mm+'01000000120CALIP01GL.nc'
				endif
			   end
		'SEVIRI': begin
				if alg eq 'CLAAS-1' then begin
					if dat eq '' then begin
						if ~sil then print,'Claas1 needs a productname to find filename!'
						found =0
						return,''
					endif
					if lev eq 'l2' then begin
						dum = strupcase(dat)
						orbdum = strlen(orb) eq 4 ? orb : '' 
						if strmid(dat,0,3) eq 'cma' then dum = 'CFC'
						if total(dat eq ['clwp','cwp','cot','cph']) then dum = 'CPP'
						dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld7/cmsaf_cld5/SEVIRI/operational/inst/'+dum+'/'+yyyy+'/'+mm+'/'
						filen = dir+dum+'in'+yyyy+mm+dd+orbdum+'*MD.hdf'
					endif else if dat eq 'zenith_angle' then begin
						dir   = '/cmsaf/cmsaf-cld7/cmsaf_cld5/SEVIRI/repr1/aux/'
						filen = dir+'meteosat'+(julday(mm,1,yyyy) ge julday(04,11,2007,13,00) ? '9':'8')+'zenanlge.nc'
					endif else begin
						dat   = strmid(get_product_name(dat,algo=alg,/lower),0,3)
						dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld7/cmsaf_cld5/SEVIRI/repr1/level3_ncdf/'+(total(dat eq ['ctp','ctt','cth']) ? 'cto' : dat)+'/'+yyyy+'/'
						apx   = dat eq 'jch' ? 'mh' : (dd ? 'dm' : 'mm')
						filen = dir+(total(dat eq ['ctp','ctt','cth']) ? 'CTO' : strupcase(dat))+apx+yyyy+mm+dd+'*MA.nc'
					endelse
				endif
				if alg eq 'CLAAS' then begin
					if dat eq '' then begin
						if ~sil then print,'Claas needs a productname to find filename!'
						found =0
						return,''
					endif
					if lev eq 'l2' then begin
						dum = strupcase(dat)
						orbdum = strlen(orb) eq 4 ? orb : '' 
						if strmid(dat,0,3) eq 'cma' then dum = 'CMA'
						if total(dat eq ['clwp','cwp','cot','cph']) then dum = 'CPP'
						dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld6/SEVIRI/repr2/level2/'+strlowcase(dum)+'/'+yyyy+'/'+mm+'/'+dd+'/'
						filen = dir+dum+'in'+yyyy+mm+dd+orbdum+'*MD.nc'
					endif else begin
						apx   = (dd ? 'dm' : 'mm')
						if is_h1d(dat) or is_jch(dat) then apx = (dd ? 'dh' : 'mh')
						pathdat = get_product_name(dat,algo=alg,level=lev,/path)
						if pathdat eq 'aux' then begin
							dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld6/SEVIRI/repr2/aux/'
							;how do I decide between(0.05 or 0.25 degree?)
							;decision read 005 and resample if necassary
							filen = dir+'claas2_level3_aux_data_005deg.nc'
						endif else begin
							dat     = get_product_name(dat,algo=alg,/lower)
							if dat eq 'cwp' then dat = 'iwp' else $
							if dat eq 'iwp' then dat = 'iwp' else $
							if dat eq 'lwp' then dat = 'lwp' else $
							if dat eq 'cwp_allsky' then dat = 'cfc' else $
							if dat eq 'lwp_allsky' then dat = 'lwp' else $
							if dat eq 'iwp_allsky' then dat = 'iwp' else $
							if dat eq 'ref' then dat = 'iwp'  else $
							if dat eq 'cot' then dat = 'iwp'  else $
							if dat eq 'ref_ice' then dat = 'iwp'  else $
							if dat eq 'ref_liq' then dat = 'lwp'  else $
							if dat eq 'cot_ice' then dat = 'iwp'  else $
							if dat eq 'cot_liq' then dat = 'lwp'  else dat = pathdat
							dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld6/SEVIRI/repr2/level3/'+pathdat+'/'+yyyy+'/'+mm+'/'+dd+'/'
							filen = dir+strupcase(dat)+apx+yyyy+mm+dd+'*MA.nc'
						endelse
					endelse
				endif
				if alg eq 'PATMOS' then begin
					if lev eq 'l2' then begin
						orbdum= strlen(orb) eq 4 ? orb : '*' 
						doy   = string(doy(yyyy,mm,dd eq '' ? 1:dd),f='(i3.3)')
						dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld1/mstengel/Patmos-X/SEVIRI/2006/'+doy+'/'
						filen = dir + 'met*'+yyyy+'_'+doy+'_'+orbdum+'.level2.hdf'
					endif
				endif
			  end
		'AVHRR' : begin
				case alg of
					'ESACCIV3': begin
							if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
								sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime)
							endif
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v3.0/'+strupcase(lev)+$
; 							(lev eq 'l3u' ? 'x':'')+ $ ;temporär!!!!!!
							'/'+yyyy+'/'+mm+'/'
							if dat eq '' and lev eq 'l3u' then begin
								if ~sil then print,'Since Version 3, ESA CCI L3U data need a productname to find filename!'
								found =0
								return,''
							endif
							if lev eq 'l2' then begin
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' :dir+strlowcase(sat)+'/*/'
								orbdum = strlen(orb) eq 4 ? orb : '' 
							endif else orbdum = ''
							vers  = keyword_set(version) ? strlowcase(version[0]) : 'v3.0'
							if strmid(dat,0,3) eq 'rgb' or strmid(dat,0,3) eq 'fci' then begin
								pathdat = lev eq 'l3u' ? get_product_name('refl_vis006_asc',algo=alg,level=lev,/path,/upper) : ''
							endif else begin
								pathdat = lev eq 'l3u' ? get_product_name(dat,algo=alg,level=lev,/path,/upper) : ''
							endelse
							filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-'+strupcase(lev)+'_*'+pathdat+'-AVHRR*'+(lev eq 'l3s' ? '':sat)+'-f'+vers+'.nc'
						end
					'ESACCI': begin
							if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
								sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime)
; 								if found_prime then satellite = sat
							endif
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+strupcase(lev)+'/'+yyyy+'/'+mm+'/'
							if lev eq 'l2' then begin
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' :dir+strlowcase(sat)+'/*/'
								orbdum = strlen(orb) eq 4 ? orb : '' 
							endif else orbdum = ''
							if strmid(dat,0,3) eq 'rgb' then begin
								dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld8/esa_cloud_cci/pics/v2.0/jpg/'+yyyy+'/'
								ampm  = sat_ampm(sat)
								filen = dir + 'Cloudcci_v2.0_'+strupcase(ampm)+'_*_'+dat+'_'+yyyy+mm+dd+'*.jpg'
							endif else if strmid(dat,0,3) eq 'fci' then begin
								filen = 'Cloud_cci v2.0 has no Measurements available. FCI not possible! Try Clara-A2 or Cloud_cci v3.0!'
							endif else begin
								vers  = keyword_set(version) ? strlowcase(version[0]) : 'v2.0'
								filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-'+strupcase(lev)+'_*-AVHRR*'+(lev eq 'l3s' ? '':sat)+'-f'+vers+'.nc'
							endelse
						  end
					'ESACCI_v14': begin
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld6/esa_cci_cloud_data/data/'+lev+'/'+yyyy+'/'+mm+'/'+dd+'/'
							filen = dir+yyyy+mm+dd+'*ESACCI-'+strupcase(lev)+'_*-AVHRR*'+(lev eq 'l3s' ? '':sat)+'-fv1.4.nc'
						  end
					'ESACCI_OLD': begin
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/data/'+lev+'/'+yyyy+'/'+mm+'/'+dd+'/'
							if lev eq 'l2' then begin 
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' :dir+strlowcase(sat)+'/*/'
								orbdum = strlen(orb) eq 4 ? orb : '' 
							endif else orbdum = ''
							filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-'+strupcase(lev)+'_*-AVHRR*'+(lev eq 'l3s' ? '':sat)+'-fv1.0.nc'
						  end
					'CLARA' : begin
							if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
								sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime)
; 								if found_prime then satellite = sat
							endif
							if lev eq 'l2' then begin
								orbdum = strlen(orb) eq 4 ? orb : '' 
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' : '/cmsaf/cmsaf-cld4/esa_cloud_cci/data/round_robin/sensor_data/AVHRR/L2_CMS2_CH37/'+$
									yyyy+'_'+mm+'_'+dd+'_converted/'
								filen = dir+'RR_AVHRR_L2_cp_'+strupcase(sat)+'_'+yyyy+mm+dd+'_'+orbdum+'*_v0_*_CMS2.nc'
; 								goto, ende
							endif else begin
								if dat eq '' then begin
									if ~sil then print,'CLARA needs a productname to find filename!'
									found =0
									return,''
								endif
								pathdat = get_product_name(dat,algo=alg,level=lev,/path)
								dat   = strmid(get_product_name(dat,algo=alg,/lower),0,3)
								apx   = dat eq 'jch' ? 'mh' : (dd ? 'dm' : 'mm')
								if din then begin
									last_subdir = (reverse(strsplit(dirname,'/',/ext)))[0]
									if is_number(last_subdir) and strlen(last_subdir) eq 4 then begin
										dirname = strmid(dirname,0,strpos(dirname,last_subdir))+yyyy
									endif
								endif
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/cmsaf_cld5/AVHRR_GAC/gacrepr1/level3_ncdf/CLARA-A1_cloudproperties/'+ $
									pathdat+'/'+ $
									strlowcase(strjoin(strsplit((sat eq 'METOPA' ? 'metop02' : (sat eq 'AVHRRS' ? 'allsat' : sat)),/ext,'-')))+$
									'/'+yyyy+'/'
								filen = dir+strupcase(pathdat)+apx+yyyy+mm+dd+'*GL.nc'
							endelse
						  end
					'CLARA2': begin
							if dat eq '' then begin
								if ~sil then print,'CLARA2 needs a productname to find filename!'
								found =0
								return,''
							endif
							if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
								sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime)
; 								if found_prime then satellite = sat
							endif
							dumdat = dat
							dat    = get_product_name(dat,algo=alg,/upper,level=lev,/path)
							case lev of 
								'l3c'	:	apx = 'mm'
								'l3s'	:	apx = 'mm'
								'l3u'	:	apx = 'in'
								'l3dh'	:	apx = 'dh'
								'l3dm'	:	apx = 'dm'
								'l3pm'	:	apx = 'pm'
								else 	: 	apx=(dd ? 'in' : 'mm')
							endcase

							if ( (dat eq 'JCH' or is_h1d(dumdat)) and apx eq 'mm' ) then apx = 'mh'
; 							if  dat eq 'JCH' and apx eq 'dm' then apx = 'dh'
							c2_ende = keyword_set(filename) ? strmid(file_basename(filename),strlen(file_basename(filename))-5,2) : 'GL'

							case strmid(sat,0,4) of
								'NOAA'	: satn = 'AVN'+string(strmid(sat,5,2),f='(i2.2)')
								'METO'	: satn = 'AVME'+strmid(sat,5,1)
								'AVHR'	: satn = 'AVPOS'
								'ALLS'	: satn = 'AVPOS'
								else	: satn = 'NN'
							endcase
							dumlevel = apx eq 'in' ? 'LEVEL2B' : 'LEVEL3'
							if din then begin
								last_subdir = (reverse(strsplit(dirname,'/',/ext)))[0]
								if is_number(last_subdir) and strlen(last_subdir) eq 4 then begin
									dirname = strmid(dirname,0,strpos(dirname,last_subdir))+yyyy
								endif
							endif
							if strmid(dat,0,3) eq 'RGB' or strmid(dat,0,3) eq 'FCI' then begin
								dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld7/AVHRR_GAC_2/'+dumlevel+'/CAC/'+satn+'/'+yyyy+'/'
								filen = dir + 'CAC'+apx+yyyy+mm+dd+'*'+ satn+'*'+c2_ende+'.nc'
							endif else begin
								dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld7/AVHRR_GAC_2/'+dumlevel+'/'+dat+'/'+satn+'/'+yyyy+'/'
								filen = dir + dat+apx+yyyy+mm+dd+'*'+ satn+'*'+c2_ende+'.nc'
							endelse
						end
					'PATMOS_OLD': begin
							if lev eq 'l3c' or lev eq 'l3s' then begin
								satpat = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),which=which,/patmos,/no_zero,found=found)
								if found then begin
									if strmatch(sat,satpat) or total(sat eq ['NOAA-AM','NOAA-PM']) then begin
										dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/Patmos-X/gewex/'+yyyy+'/'
										dat   = strmid(get_product_name(dat,algo='gewex',/upper),2)
										if dat eq '' then begin
											if ~sil then print,'PATMOS L3C data need a productname to find filename!'
											found =0
											return,''
										endif
										if dat eq 'COD_CP' then dat = 'HIST2D_COD_CP'
										filen = dir+dat+'_PATMOSX_NOAA_'+which+'_'+yyyy+'.nc' ; nur 0130PM
									endif else begin
										addon = ' - Choose right Satellite!'
									endelse
								endif
							endif else if lev eq 'l3u' then begin
								if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
									sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,/patmos,found=found_prime)
								endif
								node  = keyword_set(node) ? strmid(node,0,3) : strlowcase(strmid((reverse(strsplit(dat,'_',/ext)))[0],0,3))
								node  = node eq 'asc' or node eq 'des' ? node : '*'
								doy   = string(doy(yyyy,mm,dd eq '' ? 1:(dd eq '??' ? dom(yyyy,mm):dd)),f='(i3.3)')
								satd  = sat eq 'METOPA' ? 'metop-02' : strlowcase(sat)
								if stregex(satd,'noaa',/bool) then begin
									dum = strsplit(satd,/ext,'-')
									satd = dum[0]+'-'+(is_number(dum[1]) ? string(dum[1],f='(i2.2)') : dum[1])
								endif
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/mstengel/Patmos-X/V6ncdc/'+yyyy+'/level2b_daily/'
								filen = dir+'patmosx_'+satd+'_'+strmid(node,0,3)+'_'+yyyy+'_'+doy+'.level2b.hdf'
							endif else if lev eq 'l2' then begin
								sat   = strjoin(strsplit(sat,/ext,'-'))
								satd  = sat eq 'METOPA' ? 'METOP-02' : strupcase(sat)
								orbdum = strlen(orb) eq 4 ? orb : '*' 
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld4/esa_cloud_cci/data/round_robin/sensor_data/AVHRR/L2_WISC_CH37/'+$
									yyyy+'_'+mm+'_'+dd+'_converted/'
								filen = dir+'RR_AVHRR_L2_cp_'+satd+'_'+yyyy+mm+dd+'_'+orbdum+'_v0_*_WISC.nc'
							endif
						 end
					'PATMOS': begin
							if lev eq 'l3c' or lev eq 'l3s' then begin
								satpat = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),which=which,/no_zero,found=found)
								if found then begin
									if strmatch(sat,satpat) or total(sat eq ['NOAA-AM','NOAA-PM']) then begin
; 										dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/thanschm/DATASET/PATMOS/'+yyyy+'/'
										dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld8/EXTERNAL_DATA/PATMOS_X/LEVEL3/'+yyyy+'/'
										dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
										if dat eq '' then begin
											if ~sil then print,'PATMOS L3C data need a productname to find filename!'
											found =0
											return,''
										endif
										if dat eq 'COD_CP' then dat = 'CODW_CP'
										filen = dir+dat+'_PATMOS4CLARA2_NOAA_'+which+'_'+yyyy+'.nc'
									endif else begin
										addon = ' - Choose right Satellite!'
									endelse
								endif
							endif else if lev eq 'l3u' then begin
								if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
									sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime)
								endif
								node  = keyword_set(node) ? strmid(node,0,3) : strlowcase(strmid((reverse(strsplit(dat,'_',/ext)))[0],0,3))
								node  = node eq 'asc' or node eq 'des' ? node : sat_ampm(sat) eq 'pm' ? 'asc' : 'desc'

								doy   = string(doy(yyyy,mm,dd eq '' ? 1:(dd eq '??' ? dom(yyyy,mm):dd)),f='(i3.3)')
								satd  = sat eq 'METOPA' ? 'metop-02' : (sat eq 'METOPB' ? 'metop-01' : strlowcase(sat))
								if stregex(satd,'noaa',/bool) then begin
									dum = strsplit(satd,/ext,'-')
									satd = n_elements(dum) gt 1 ? (dum[0]+'-'+(is_number(dum[1]) ? string(dum[1],f='(i2.2)') : dum[1])) : dum[0]
								endif
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld6/cmcld/data/PATMOSX/level2b/'+yyyy+'/'
								filen = dir+'patmosx_'+satd+'_'+strmid(node,0,3)+'_'+yyyy+'_'+doy+'.level2b.{hdf,nc}'
							endif
						 end
					'GEWEX': begin
						if ~total(lev eq ['l3c','l3s']) then goto, ende
							satgwx = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),which=which,/no_zero,found=found)
							if found then begin
								if strmatch(sat,satgwx) or total(sat eq ['NOAA-AM','NOAA-PM','AVHRRS']) then begin
									dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/new/AVHRR/'+yyyy+'/'
; 									dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/old/AVHRR/'+yyyy+'/'
									dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
									if keyword_set(gewex_style) then style = strupcase(gewex_style[0]) else begin
										case which of
											'AMPM'		: style = 'AMPM'
											'AM'		: style = '0730AMPM'
											'PM'		: style = '0130AMPM'
											else		: style = 'AMPM'
										endcase
									endelse
									filen = dir+dat+'_AVHRR-ESACCI_NOAA_'+style+'_'+yyyy+'.nc'
								endif else begin
									addon = ' - Choose right Satellite!'
								endelse
							endif
						 end
					'GAC2-GEWEX': begin
							if ~total(lev eq ['l3c','l3s']) then goto, ende
							satgwx = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),which=which,/no_zero,found=found)
							if found then begin
								if strmatch(sat,satgwx) or total(sat eq ['NOAA-AM','NOAA-PM','AVHRRS']) then begin
									dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/AVHRR_GAC_2/LEVEL3_GEWEX/'+yyyy+'/'
									dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
									if keyword_set(gewex_style) then style = strupcase(gewex_style[0]) else begin
										case which of 
											'AMPM'	: style = 'AMPM'
											'AM'	: style = '0730AMPM'
											'PM'	: style = '0130AMPM'
											else	: style = 'AMPM'
										endcase
									endelse
									filen = dir+dat+'_CLARA_A2_NOAA_'+style+'_'+yyyy+'.nc'
								endif else begin
									addon = ' - Choose right Satellite!'
								endelse
							endif
						end
					'ISCCP_OLD': begin
							if ~total(lev eq ['l3c','l3s']) then goto, ende
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld4/ISCCP_mjerg/complete_clouds/'
							filen = dir+'ISCCP.*'+yyyy+'.'+mm+'*.hdf'
							end
					'ISCCP': begin
							if ~total(lev eq ['l3c','l3s']) then goto, ende
							satgwx = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),which=which,/no_zero,found=found)
							if found then begin
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld8/EXTERNAL_DATA/ISCCP/GEWEX/'+yyyy+'/'
								dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
								if keyword_set(gewex_style) then style = strupcase(gewex_style[0]) else begin
									case which of 
										'AMPM'	: style = 'AMPM'
										'AM'	: style = '0900AMPM'
										'PM'	: style = '0300AMPM'
										else	: style = 'AMPM'
									endcase
								endelse
								filen = dir+dat+'_ISCCP_D1_'+style+'_'+yyyy+'.nc'
							endif
							end
					'L1GAC': begin
							if lev ne 'l1' then goto, ende
							orbdum= strlen(orb) eq 4 ? orb : '*' 
							satd  = sat eq 'METOPA' ? 'metop02' : strlowcase(strjoin(strsplit(sat,/ext,'-')))
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/sstapelb/gac/data/'+yyyy+'/'+mm+'/'+satd+'/'
							filen = dir + satd+'_'+yyyy+mm+dd+'_'+orbdum+'_99999_satproj_*'+strlowcase(dat)+'.h5'
						end
					else	:
				endcase
			  end
		'MODIS' : begin
				case alg of
					'ESACCIv3': begin
							print,'MODIS will not be processed! with v3. If this has changed! Edit code here!'
							filen=''
					end
					'ESACCI': begin
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+strmid(strupcase(lev),0,3)+'/'+yyyy+'/'+mm+'/'
							vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
							filen = dir+yyyy+mm+dd+'*ESACCI-'+strmid(strupcase(lev),0,3)+'_*-MODIS*'+$
								(lev eq 'l3s' ? '':sat)+(lev eq 'l3ue' ? '_Europe':'')+'-f'+vers+'.nc'
						  end
					'ESACCI_v14': begin
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld6/esa_cci_cloud_data/data/'+lev+'/'+yyyy+'/'+mm+'/'+dd+'/'
							if lev eq 'l2' then begin 
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' :dir+strlowcase(sat)+'/*/'
								orbdum = strlen(orb) eq 4 ? orb : '' 
							endif else orbdum = ''
							vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
							filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-'+strupcase(lev)+'_*-MODIS*'+(lev eq 'l3s' ? '':sat)+'-f'+vers+'.nc'
						  end
					'ESACCI_OLD': begin
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/data/'+lev+'/'+yyyy+'/'+mm+'/'+dd+'/'
							if lev eq 'l2' then begin
								sat = strjoin(strsplit(sat,/ext,'-'))
								dir = din ? dirname+'/' :dir+strlowcase(sat)+'/'
								orbdum = strlen(orb) eq 4 ? orb : '' 
							endif else orbdum = ''
							filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-'+strupcase(lev)+'_*-MODIS*'+(lev eq 'l3s' ? '':sat)+'-fv1.0.nc'
						  end
					'GEWEX': begin
							if ~total(lev eq ['l3c','l3s']) then goto, ende
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/new/MODIS/'+yyyy+'/'
;							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/old/MODIS/'+yyyy+'/'
							which  = strupcase(sat_ampm(sat))
							if keyword_set(gewex_style) then style = strupcase(gewex_style[0]) else begin
								case which of 
									'AMPM'	: style = 'AMPM'
									'AM'	: style = '1030AMPM'
									'PM'	: style = '0130AMPM'
									else	: style = 'AMPM'
								endcase
							endelse
							dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
							filen = dir+dat+'_MODIS-ESACCI_*_'+style+'_'+yyyy+'.nc'
						 end
					'COLL5' : begin
							doy   = string(doy(yyyy,mm,dd eq '' ? 1:dd),f='(i3.3)')
							teaq  = sat eq 'TERRA' ? 'MOD' : 'MYD'
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld6/cmcld/data/MODIS/'
; 							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/MODIS/'
							if lev eq 'l2' then begin
								orbdum= strlen(orb) eq 4 ? orb : '*'
								dir   = din ? dirname+'/' : dir+teaq+'06_L2_051/'+yyyy+'/'+mm+'/'+dd+'/'
								filen = dir+teaq+'06_L2.A'+yyyy+doy+'.'+orbdum+'.051.*.hdf'
							endif else if total(lev eq ['l3s','l3c']) then begin
								dir   = din ? dirname+'/' : dir+teaq+'08_M3_051/'+yyyy+'/'
								filen = dir+teaq+'08_M3.A'+yyyy+doy+'.051.*.hdf'
							endif
						  end
					'COLL6' : begin
							doy   = string(doy(yyyy,mm,dd eq '' ? 1:dd),f='(i3.3)')
							teaq  = sat eq 'TERRA' ? 'MOD' : 'MYD'
; 							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/MODIS/'
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld6/cmcld/data/MODIS/'
							if lev eq 'l2' then begin
								orbdum= strlen(orb) eq 4 ? orb : '*'
								dir   = din ? dirname+'/' : dir+teaq+'06_L2_006/'+yyyy+'/'+mm+'/'+dd+'/'
								filen = dir+teaq+'06_L2.A'+yyyy+doy+'.'+orbdum+'.006.*.hdf'
							endif else if total(lev eq ['l3s','l3c']) then begin
; 								dir   = din ? dirname+'/' : '/cmsaf/cmsaf-scr1/QualiMon/output_new/cloud/val_raw/CTP/'
; 								filen = dir+yyyy+mm+strlowcase(teaq)+'.hdf'
								dir   = din ? dirname+'/' : dir+teaq+'08_M3_006/'+yyyy+'/'
								filen = dir+teaq+'08_M3.A'+yyyy+doy+'.006.*.hdf'
							endif
						  end
					else	:
				endcase
			  end
		'AATSR'	: begin
				if alg eq 'APOLLO' then begin
					orbdum = strlen(orb) eq 4 ? orb : ''
					dir = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/dlr_apollo/'
					filen = dir+'ATS_'+yyyy+mm+dd+'_'+orbdum+'*N*.nc' ; N (Nadir) or F (Forwardview)
				endif
				if alg eq 'SWANSEA' then begin
					orbdum = strlen(orb) eq 4 ? orb : ''
					dir = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/dlr_apollo/'
					if dat eq 'cm' then filen = dir+'swansea_cm/ATS_TOA_1PRUPA'+yyyy+mm+dd+'*cldMask.nc' $
					else filen = dir+yyyy+mm+dd+orbdum+'*ESACCI-L2P_AEROSOL-ALL-AATSR_ENVISAT-SU*.nc'
				endif
				if alg eq 'ESACCIv3' then begin
					print,'To do if available!'
					filen = ''
				endif
				if alg eq 'ESACCI' then begin
					if lev eq 'l2' then begin
						dir = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/ral_l2_aatsr/'+yyyy+'/'+mm+'/'+dd+'/'
						filen = dir+'*PP.primary.nc'
					endif else begin
						dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+strmid(strupcase(lev),0,3)+'/'+yyyy+'/'+mm+'/'
						vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
						filen = dir+yyyy+mm+dd+'*ESACCI*'+strupcase(lev)+'_CLOUD-CLD_PRODUCTS-AATSR*-f'+vers+'.nc'
					endelse
				endif
				if alg eq 'GEWEX' then begin
					if ~total(lev eq ['l3c','l3s']) then goto, ende
					dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/old/ATSR/'+yyyy+'/'
					dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
					filen = dir+dat+'_AATSR-ESACCI_ENVISAT_1030AMPM_'+yyyy+'.nc'
				endif
			  end
		'ATSR2'	: begin
				if alg eq 'ESACCIv3' then begin
					print,'To do if available!'
					filen = ''
				endif
				if alg eq 'ESACCI' then begin
					if lev eq 'l3c' then begin
						dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+strmid(strupcase(lev),0,3)+'/'+yyyy+'/'+mm+'/'
						vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
						filen = dir+yyyy+mm+dd+'*ESACCI*'+strupcase(lev)+'_CLOUD-CLD_PRODUCTS-ATSR2_ERS2-f'+vers+'.nc'
					endif
				endif
				if alg eq 'GEWEX' then begin
					if ~total(lev eq ['l3c','l3s']) then goto, ende
					dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/old/ATSR/'+yyyy+'/'
					dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
					filen = dir+dat+'_ATSR2-ESACCI_ERS2_1030AMPM_'+yyyy+'.nc'
				endif
			  end
		'MERISAATSR': begin
				case alg of 
					'GEWEX': begin
							if ~total(lev eq ['l3c','l3s']) then goto, ende
							dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/gewex/old/FAMEC/'+yyyy+'/'
							dat   = strmid(get_product_name(dat,algo='gewex',/upper,/path),2)
							filen = dir+dat+'_MERIS+AATSR-ESACCI_ENVISAT_1030AM_'+yyyy+'.nc'
						end
					'ESACCI': begin
							if lev eq 'l2' then begin
								; noch kein offizieller DateiName
								orbdum   = strlen(orb) eq 4 ? orb : ''
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/l2_meris_aatsr/'+yyyy+'/'+mm+'/'+dd+'/'
								filen = dir+'*'+yyyy+mm+dd+orbdum+'*.nc'
							endif else begin
								dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+strmid(strupcase(lev),0,3)+'/'+yyyy+'/'+mm+'/'
								vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
								zwisch = alg eq 'ESACCI_OLD' ? '' : '-'
								filen = dir+yyyy+mm+dd+'-ESACCI-'+strupcase(lev)+'_CLOUD-CLD_PRODUCTS-MERIS'+zwisch+'AATSR_ENVISAT-f'+vers+'.nc'
							endelse
						end
					else:
				endcase
			  end
		'HIRS'	: begin
				case alg of
					'HECTOR': begin
							if total(sat eq ['NOAA-AM','NOAA-PM']) then begin
								sat   = noaa_primes(yyyy,mm,ampm=sat_ampm(sat,/ampm),/no_zero,found=found_prime,/hirs)
							endif
							dumdat = dat
							dat    = get_product_name(dat,algo=alg,/upper,level=lev,/path)
							if dat eq '' then begin
								if ~sil then print,'HECTOR needs a productname to find filename!'
								found =0
								return,''
							endif
							case lev of 
								'l3c'	:	apx = 'mm'
								'l3s'	:	apx = 'mm'
								'l3u'	:	apx = 'in'
								'l3dh'	:	apx = 'dh'
								'l3dm'	:	apx = 'dm'
								'l3pm'	:	apx = 'pm'
								else 	: 	apx = (dd ? 'in' : 'mm')
							endcase
							dumlevel = apx eq 'in' ? 'LEVEL2B' : 'LEVEL3'

							if ( (dat eq 'JCH' or is_h1d(dumdat)) and apx eq 'mm' ) then apx = 'mh'
; 							if  dat eq 'JCH' and apx eq 'dm' then apx = 'dh'
							c2_ende = keyword_set(filename) ? strmid(file_basename(filename),strlen(file_basename(filename))-5,2) : 'GL'

							case strmid(sat,0,4) of
								'NOAA'	: satn = 'HIN'+string(strmid(sat,5,2),f='(i2.2)')
								'METO'	: satn = 'HIM0'+(strmid(sat,5,1) eq 'A' ? '2':'1')
								'AVHR'	: satn = 'HIPOS'
								'ALLS'	: satn = 'HIPOS'
								else	: satn = 'NN'
							endcase
; 							dumlevel = 'V1_2'
							if din then begin
								last_subdir = (reverse(strsplit(dirname,'/',/ext)))[0]
								if is_number(last_subdir) and strlen(last_subdir) eq 4 then begin
									dirname = strmid(dirname,0,strpos(dirname,last_subdir))+yyyy
								endif
							endif
							dir   = din ? dirname+'/' : '/cmsaf/cmsaf-cld8/HECTOR/BETA_paper/'+dumlevel+'/'+satn+'/'+yyyy+'/'
							filen = dir + dat+apx+yyyy+mm+dd+'*'+ satn+'*'+c2_ende+'.nc'
						end
					else:
				endcase
			  end
		'ALL'	: begin
				if alg eq 'ESACCIv3' then begin
					print,'To do if available!'
					filen = ''
				endif
				if alg eq 'ESACCI' then begin
					dir   = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3S/'+yyyy+'/'+mm+'/'
					vers  = keyword_set(version) ? strlowcase(version[0]) : 'v*'
					filen = dir+yyyy+mm+dd+'*ESACCI-L3S_CLOUD-CLD_PRODUCTS-MERGED-f'+vers+'.nc'
				endif
			  end
		'MODEL'	: begin
				if alg eq 'ERA-I' then begin
					if lev eq 'l2' then goto, ende
					if lev eq 'l3u' then goto, ende
					thr = !ERA_I1_THRESHOLD 
					ver = !ERA_I1_VERSION
					dir = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/cschlund/output/simulator/'+ver+'_DWDscops_MaxRand_SeparPhase_OriCWC/timeseries/'
					apx = keyword_set(filename) ? strmid(filename,12,2) : 'MM'
					if ~total(apx eq ['MM','MH']) then begin
						apx = stregex(dat,'hist',/fold,/bool) ? 'MH' : 'MM'
					endif
					filen = dir + 'ERA_Interim_'+apx+yyyy+mm+'_cot-thv-'+thr+'.nc'
				endif
				if alg eq 'ERA-I2' then begin
					if lev eq 'l2' then goto, ende
					if lev eq 'l3u' then goto, ende
					thr = !ERA_I2_THRESHOLD
					ver = !ERA_I2_VERSION
					dir = din ? dirname+'/' :'/cmsaf/cmsaf-cld7/cschlund/output/simulator/'+ver+'_DWDscops_MaxRand_SeparPhase_OriCWC/timeseries/'
					apx = keyword_set(filename) ? strmid(filename,12,2) : 'MM'
					if ~total(apx eq ['MM','MH']) then begin
						apx = stregex(dat,'hist',/fold,/bool) ? 'MH' : 'MM'
					endif
					filen = dir + 'ERA_Interim_'+apx+yyyy+mm+'_cot-thv-'+thr+'.nc'
				endif
			  end
		else	:
	endcase

	ende :

	filename = file_search(filen,count = found)
	if found eq 0 then begin
		; try if file is gzipped 
		filename = file_search(filen+'.gz',count = found)
	endif

	if found gt 1 and is_number(orb) then begin
; 		if between(fix(orb),0,found-1) then filename = file_search(filename[fix(orb)],count = found)
		if ~between(fix(orb),0,found-1) then print,'Index is higher than number of files. Choose last one!'
		filename = file_search(filename[(fix(orb) < (found-1))],count = found)
	endif
	found = float(found)
	if ~found and keyword_set(filen) and keyword_set(dir) and ~keyword_set(no_recursive_search) then begin
		; try again recursively
		filename = file_search(dir,file_basename(filen),count = found)
	endif
	if ~found and ~sil then begin
		dir = keyword_set(dir) ? dir : 'unknown'
		ok  = dialog_message('get_filename: (Database: '+dir+'): No '+alg+' level '+strcompress(lev)+$
			' data files found! '+(dat ne '' ? 'Varname: '+dat+', ':'')+'Satellite: '+sat+' '+inst+', Date: '+yyyy+'/'+mm+'/'+dd+addon)
		if strmid(alg,0,5) eq 'CLARA' then begin
			print,'For CLARA files make sure you have the right combination of Product-Name and Product-Level: '
		endif
		print,'composed Filename was: "'+filen+'"'
	endif

	return, filename
end
;------------------------------------------------------------------------------------------
function get_histo_time_series, algo, data, satellite, period = period, this_period_only = this_period_only, pvir = pvir, $
				longname = longname, unit = unit, sav_file = sav_file, found = found, compare = compare

	sat = strlowcase(satellite)
	alg = algo2ref(algo,sat=sat)
	per = keyword_set(period)   ? strlowcase(period)   : '????-????'
	dat = (strlowcase(data))[0]
	dat = stregex(dat,'hist1d_ref',/fold,/bool) ? strlowcase(strreplace(dat,'_ref','_cer',/fold)) : strlowcase(dat)
	pvir    = stregex((reverse(strsplit(dat,'_',/ext)))[0],'pvir',/fold,/bool)
	if pvir then dat = strreplace(dat,'_pvir','',/fold)
	if pvir then per = '2003-2011'

	if algo2ref(alg,sat=sat) eq 'gac2' and sat eq 'avhrrs'  then sat = 'allsat'
	if algo2ref(alg,sat=sat) eq 'cci'  and sat eq 'envisat' then sat = 'aatsr' ; is this a good idea? I don't know.

	satdum = (total(strmid(alg,0,3) eq ['myd','mod','cla','era','era2']) ? '':sat)
	found = 0.
	phase = ''
	try_again = keyword_set(period) and ~keyword_set(this_period_only)
	if stregex(dat,'_liq',/fold,/bool) then phase ='liquid water'
	if stregex(dat,'_ice',/fold,/bool) then phase ='ice water'
	if stregex(dat,'_ratio',/fold,/bool) then begin & dat = strreplace(dat,'_ratio','_liq',/fold) & phase ='ratio' & end
	if keyword_set(compare) then begin
		vs = ''
		if algo2ref(alg) eq 'cci' then vs = '_vs_cci'
		if algo2ref(alg) eq 'cciv3' then vs = '_vs_cciv3'
	endif else vs = ''

	if is_h1d(dat) then begin
		sfile = !SAVS_DIR + 'time_series/hist1d/'+dat+'_'+per+'_'+alg+vs+'_'+satdum+'.sav'
		sav_file = file_search( sfile ,count = found)
		if found eq 0 and try_again then begin
			if vs ne '' then print,'No compare sav_file found. Try again without compare!'
			sfile = !SAVS_DIR + 'time_series/hist1d/'+dat+'_????-????_'+alg+'_'+satdum+'.sav'
			sav_file = file_search( sfile ,count = found)
		endif
		dum = strsplit(dat,'_',/ext)
		case dum[1] of
			'cot'	: longname = 'Cloud optical thickness'
			'cer'	: longname = 'Cloud effective radius'
			'ctp'	: longname = 'Cloud Top Pressure'
			'ctt'	: longname = 'Cloud Top Temperature'
			'cwp'	: longname = 'Cloud Water Path'
			'cla'	: longname = 'Cloud Albedo '+strupcase(dum[2])
			else	: longname = 'I dont know this variable'
		endcase
		longname = '1D Histogram of '+phase+' '+longname
		unit = ''
	endif else if is_jch(dat) then begin
		sfile = !SAVS_DIR + 'time_series/hist2d/'+get_product_name(dat,algo='gac2')+'_'+per+'_'+alg+vs+'_'+satdum+'.sav'
		sav_file = file_search( sfile ,count = found)
		if found eq 0 and try_again then begin
			if vs ne '' then print,'No compare sav_file found. Try again without compare!'
			sfile = !SAVS_DIR + 'time_series/hist2d/'+get_product_name(dat,algo='gac2')+'_????-????_'+alg+'_'+satdum+'.sav'
			sav_file = file_search( sfile ,count = found)
		endif
		longname = 'Joint cloud property Histogram of ice and water clouds'
		if is_jch(dat,/liq) then longname = 'Joint cloud property Histogram of water clouds'
		if is_jch(dat,/ice) then longname = 'Joint cloud property Histogram of ice clouds'
		if is_jch(dat,/ratio) then longname = 'Joint cloud property Histogram fraction of liquid clouds'
		unit = ''
	endif

	if found then begin
		datum = (stregex(file_basename(sav_file),'[0-9]+-[0-9]+',/ext))[0]
		struc = restore_var(sav_file,found=found)
		if found then begin
			if phase eq 'ratio' then begin
				struci = restore_var(strreplace(sav_file,'_liq','_ice'),found=found)
				if found then begin
					bildl = struc.bild
					bildi = struci.bild
					sil = size(bildl,/dim)
					sii = size(bildi,/dim)
					if (n_elements(sil) eq n_elements(sii)) and (total(sii) eq total(sil)) then begin
						bild  = lonarr([sii,2])
						if is_jch(dat) and n_elements(sii) eq 4 then begin
							bild[*,*,*,*,0] = temporary(bildl)
							bild[*,*,*,*,1] = temporary(bildi)
						endif else if is_h1d(dat) and n_elements(sii) eq 3 then begin
							bild[*,*,*,0] = temporary(bildl)
							bild[*,*,*,1] = temporary(bildi)
						endif else begin
							ok=dialog_message('get_histo_time_series: Kenn ich nich! Stop')
							stop
						endelse
						struc = {bild:bild,actual_date:struc.actual_date,period:datum}
						print,'Sav File: ',sav_file
						return, struc
					endif else begin
						print,'Liquid and Ice water dimensions do not agree! No Ratio possible!'
						print,'Liq Dim: ',sil
						print,'Ice Dim: ',sii
						found = 0.
					endelse
				endif
			endif
			if found then begin
				struc = create_struct(struc,{period:datum})
				print,'Sav File: ',sav_file
				return,struc
			endif
		endif
	endif

	return,-1
end
;------------------------------------------------------------------------------------------
; reads the plot, compare and hovmoeller time series sav-files.
function get_available_time_series, algo, data, satellite, coverage = coverage, reference = reference	, $
				period = period,longname = longname, unit = unit, sav_file = sav_file, found = found	, $
				hovmoeller = hovmoeller, trend = trend, tr_corr = tr_corr, anomalies = anomalies		, $
				stddev = stddev, uncertainty = uncertainty, sum = sum, bias = bias			, $
				rmse = rmse, corr = corr, no_trend_found = no_trend_found, silent = silent, pvir = pvir	, $
				bc_rmse = bc_rmse, season = season, ts_extras = ts_extras

	cov = keyword_set(coverage) ? strlowcase(coverage) : ''
	sat = strlowcase(satellite)
	dat = (strlowcase(data))[0]
	per = keyword_set(period)   ? strlowcase(period)   : '????-????'
	if cov eq 'full' then cov = ''
	no_trend_found = 0
	
	TSE = keyword_set(ts_extras) ? (strsplit(strlowcase(ts_extras),/ext))[0] : ''

	trend 		= TSE eq 'trend'
	anomalies 	= TSE eq 'anomalies'	
	season 		= TSE eq 'season'
	sum 		= TSE eq 'sum' and 	(total(algo2ref(algo,sat=sat) eq ['cci','cciv3'])) and $
									(stregex(dat,'nobs',/fold,/bool) or stregex(dat,'nretr',/fold,/bool))
	tr_corr 	= TSE eq 'tc'
	bias 		= TSE eq 'bias' and keyword_set(reference)
	rmse 		= TSE eq 'rmse' and keyword_set(reference)
	bc_rmse 	= TSE eq 'bc_rmse' and keyword_set(reference)
	corr 		= TSE eq 'correlation' and keyword_set(reference)

	if keyword_set(reference) and strmid(TSE,0,3) eq 'tc-' then begin
		tr_corr	= 1
		bias 	= strmid(TSE,3) eq 'bias'
		rmse 	= strmid(TSE,3) eq 'rmse'
		bc_rmse = strmid(TSE,3) eq 'bc_rmse'
		corr 	= strmid(TSE,3) eq 'correlation'
	endif

	pvir        = stregex((reverse(strsplit(dat,'_',/ext)))[0],'pvir',/fold,/bool) ; Pvir is always last added to varname
	if pvir    		then dat = strreplace(dat,'_pvir','',/fold)
	uncertainty = stregex((reverse(strsplit(dat,'_',/ext)))[0],'unc',/fold,/bool)
	if uncertainty 	then dat = strreplace(dat,'_unc','',/fold)
	stddev      = stregex((reverse(strsplit(dat,'_',/ext)))[0],'std',/fold,/bool)
	if stddev    	then dat = strreplace(dat,'_std','',/fold)

	if algo2ref(algo,sat=sat) eq 'gac2' and sat eq 'avhrrs' then sat = 'allsat'
	if algo2ref(algo,sat=sat) eq 'cci' and sat eq 'envisat' then sat = 'aatsr' ; is this a good idea? I don't know.

	vali_set_path

	if pvir then per = '2003-2011'

	if keyword_set(reference) then begin
		pref  = '/compare/compare_'
		ref   = algo2ref(reference,sat=sat)
		cli   = algo2ref(algo,sat=sat)
		dumalgo = cli+'_vs_'+ref
		;time series indices
		; stats : [cci-gmean,cci-stdd,cci-unc-gmean,cci-unc-stdd,gac-gmean,gac-stdd,gac-unc-gmean,gac-unc-stdd,gbias,grmse,bc_rmse,correlate]
		tsi = {gm1:0,gm1_std:1,unc1:2,unc1_std:3,gm2:4,gm2_std:5,unc2:6,unc2_std:7,bias:8,rmse:9,bcr:10,corr:11}
		if total(strmid(cli,0,3) eq ['myd','mod','cal','cla']) then sat = ''
	endif else begin
		cli   = algo2ref(algo,sat=sat)
		pref  = '/plot/plot_'
		dumalgo = cli
		tsi = sum ? {gm1:0,gm1_std:1,unc1:2,unc1_std:3,sum1:4} : {gm1:0,gm1_std:1,unc1:2,unc1_std:3}
		if total(strmid(cli,0,3) eq ['myd','mod','cal','cla','era','era2','isp_old']) then sat = ''
		if cli eq 'isp' and total(sat eq ['avhrrs','allsat']) then sat = ''
	endelse

	if total(dat eq ['cloud_fraction','cc_total','a_ca','8']) then dat =  'cfc'
	if total(dat eq ['a_cod']) then  dat =  'cot'
	if total(dat eq ['a_cp','20']) then   dat =  'ctp'
	if total(dat eq ['a_ciwp']) then dat =  'iwp'
	if total(dat eq ['a_clwp']) then dat =  'lwp'
	if total(dat eq ['a_cawr']) then dat =  'cph'
	if total(dat eq ['a_crew']) then dat =  'cer_liq'
	if total(dat eq ['a_crei']) then dat =  'cer_ice'
	if total(dat eq ['a_codw']) then dat =  'cot_liq'
	if total(dat eq ['a_codi']) then dat =  'cot_ice'
	if total(dat eq ['a_cah'])  then dat =  'cfc_high'
	if total(dat eq ['a_cam'])  then dat =  'cfc_mid'
	if total(dat eq ['a_cal'])  then dat =  'cfc_low'

	if stregex(dat,'ref',/fold,/bool) and ~stregex(dat,'refl',/fold,/bool) then dat = strreplace(dat,'ref','cer')

	if algo eq 'select' and ~keyword_set(hovmoeller) then begin
		DEFSYSV,'!SELECTED_SAV_FILE', exists = select_file_exists
		sfile = dialog_pickfile(	path = (select_file_exists ? file_dirname ( !SELECTED_SAV_FILE ) : !SAVS_DIR +'time_series/' ) , $
									file = (select_file_exists ? file_basename( !SELECTED_SAV_FILE ) : '' ) , $
									filter = '*.sav')
		if file_test(sfile) then begin
			filen = file_basename(sfile,'.sav')
			if strmid(filen,0,8) eq 'compare_' then begin
				print,'This is only allowed on Single Time Series not Compare Time series!'
				found=0
			endif else begin
				DEFSYSV,'!SELECTED_SAV_FILE' , sfile
				data     = strlowcase((restore_var(sfile)).VARNAME)
				dat      = data
				coverage = strlowcase((restore_var(sfile)).COVERAGE)
				filen = file_basename(sfile,'.sav')
				per   = strmid(filen,8,9,/reverse)
				rest  = 	strreplace(filen,[per,dat+'_',coverage+'_','plot_','time_series_'],['','','','',''])
				dum   = strsplit(rest,'_',/ext)
				algo  = dum[0]
				if n_elements(dum) gt 1 then sat = dum[1] else sat = ''
				satellite = sat
				cli  = algo2ref(algo,sat=sat)
				found = 1
			endelse
		endif else found=0
	endif else begin
		if keyword_set(hovmoeller) then begin
			sav_file = !SAVS_DIR + 'time_series/hovmoeller/'+dat+'_hovmoeller_'+per+'_'+cli+'_'+sat+'.sav'
			sfile    = file_search( sav_file ,count = found)
			if found eq 0 and keyword_set(period) then begin
				sav_file = !SAVS_DIR + 'time_series/hovmoeller/'+dat+'_hovmoeller_????-????_'+cli+'_'+sat+'.sav'
				sfile    = file_search( sav_file ,count = found)
			endif
		endif else begin
			sav_file = !SAVS_DIR + 'time_series/'+pref+dat+'_'+dumalgo+'_time_series_'+sat+(cov eq '' ? '':'_')+cov+'_'+per+'.sav'
			sfile    = file_search( sav_file ,count = found)
			if found eq 0 and keyword_set(period) then begin
				if ~pvir then begin 
					sav_file = !SAVS_DIR + 'time_series/'+pref+dat+'_'+dumalgo+'_time_series_'+sat+(cov eq '' ? '':'_')+cov+'_????-????.sav'
					sfile    = file_search( sav_file ,count = found)
				endif
			endif
		endelse
	endelse

	if found eq 0 then return,-1

	if found gt 1 then begin
		; welches file nehmen wir, wenn wir mehr als ein file haben
		zeitraum = intarr(found)
		; nimm größten zeitraum
		for i = 0,found -1 do begin
			dum = strsplit(stregex(file_basename(sfile[i]),'[0-9]+-[0-9]+',/ext),'-',/ext)
			zeitraum[i] = fix(dum[1])-fix(dum[0])
		endfor
		idx = (where(zeitraum eq max(zeitraum)))[0]
	endif else idx = 0
	sav_file = sfile[idx]

	datum    = stregex(file_basename(sav_file),'[0-9]+-[0-9]+',/ext)
	longname = full_varname(strreplace(dat,'hist1d_','',/fold),unit=unit)

	struc = restore_var(sav_file,found=found)

	if found then begin
		if keyword_set(hovmoeller) then begin
			; make seasonal mean for anomalies
			if ~is_h1d(dat) then begin
				anz_mm  = n_elements(struc.all[0,*])
				all_sm  = fltarr(180,anz_mm)
				land_sm = fltarr(180,anz_mm)
				sea_sm  = fltarr(180,anz_mm)
				all     = struc.all
				all[where(all eq -999)]=!values.f_nan
				land    = struc.land
				land[where(land eq -999)]=!values.f_nan
				sea     = struc.sea
				sea[where(sea eq -999)]=!values.f_nan
				for i = 0,anz_mm -1 do begin
					all_sm[*,i]  = mean(all [*,(i mod 12):*:12],dim=2,/nan)
					land_sm[*,i] = mean(land[*,(i mod 12):*:12],dim=2,/nan)
					sea_sm[*,i]  = mean(sea [*,(i mod 12):*:12],dim=2,/nan)
				endfor
				idx = where(~finite(all_sm),idx_cnt)
				if idx_cnt gt 0 then all_sm[idx] = -999.
				idx = where(~finite(land_sm),idx_cnt)
				if idx_cnt gt 0 then land_sm[idx] = -999.
				idx = where(~finite(sea_sm),idx_cnt)
				if idx_cnt gt 0 then sea_sm[idx] = -999.
				struc = create_struct(struc,'seasonal_mean',{all:all_sm,land:land_sm,sea:sea_sm})
			endif
			if is_h1d(dat) then begin
				longname = 'rel. occur. of '+longname
				unit = ' [%]'
			endif
			struc = create_struct(struc,{period:datum,longname:longname,unit:unit})
		endif else begin
			if tr_corr    and ~is_tag(struc,'TREND') then no_trend_found = 1
			if trend      and ~is_tag(struc,'TREND') then no_trend_found = 1
			if anomalies  and ~is_tag(struc,'TREND') then no_trend_found = 1
			if season     and ~is_tag(struc,'TREND') then no_trend_found = 1
			if is_tag(struc,'TREND') then begin
				if ~is_struct(struc.trend) and (tr_corr or trend or anomalies or season) then no_trend_found = 1
			endif
			if no_trend_found then begin
				ok = dialog_message('get_available_time_series: "Trend" not found or is not a structure!')
				trend 		= 0
				anomalies 	= 0
				season 		= 0
				tr_corr 	= 0
				found = 0
				return,-1
			endif
			struc.coverage = cov
			if (cli eq 'cci' or cli eq 'cciv3') and dat eq 'cfc' then struc.stats[tsi.unc1,*] /= 100.
			if keyword_set(reference) then begin
				if (ref eq 'cci' or cli eq 'cciv3') and dat eq 'cfc' then struc.stats[tsi.unc2,*] /= 100.
			endif
			anz_mm      = n_elements(struc.stats[0,*])
			stats_sm    = struc.stats * 0. + !values.f_nan
			stats_sm_nw = struc.stats_non_weighted * 0. + !values.f_nan
			for i = 0, anz_mm -1 do stats_sm[*,i]    = mean(struc.stats[*,(i mod 12):*:12],dim=2,/nan)
			for i = 0, anz_mm -1 do stats_sm_nw[*,i] = mean(struc.stats_non_weighted[*,(i mod 12):*:12],dim=2,/nan)
			struc = create_struct({stats_sm:stats_sm,stats_sm_non_weighted:stats_sm_nw},struc)
			if ~is_tag(struc,'actual_date') then begin
				dumyear     = minmax(where(finite(struc.stats[0,*])))/12 + fix((strsplit(datum,/ext,'-'))[0])
				dummonth    = minmax(where(finite(struc.stats[0,*]))) mod 12
				actual_date = strjoin(string(dumyear,f=('(i4.4)'))+string(dummonth+1,f=('(i2.2)')),'-')+' '
				struc = create_struct(struc,{period:datum,actual_date:actual_date})
			endif else struc = create_struct(struc,{period:datum})
	
			struc = create_struct(struc,'ts_indices',tsi)
			if is_tag(struc,'unit') then unit = struc.unit
		endelse
		if ~keyword_set(silent) then print,'Sav File: ',sav_file
		return,struc
	endif else begin
		return,-1
	endelse
end
;------------------------------------------------------------------------------------------
pro check_era,date

	print, date+ (file_test('/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/AUXDATA/ERA_Interim/'+date,/dir) ? ': Yes' : ': No')

end
;------------------------------------------------------------------------------------------
function map_ecmwf_to_orbit, orbit_date, orb_lon, orb_lat, parameter, found = found, index = index, grid_res=grid_res, verbose = verbose, $
	 only_hr = only_hr, no_hr = no_hr,cci_l3u_eu=cci_l3u_eu

	found = 1.
	error_status = 0
	ecmwf_path = '/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/AUXDATA/ERA_Interim/'

	if n_params() ne 4 then begin
		print,'Syntax: map_ecmwf_to_orbit, orbit_date, orb_lon, orb_lat, parameter ,/keywords'
		found = 0
		return,-1
	endif

	para = strlowcase(parameter)
	grid = keyword_set(grid_res) ? grid_res : get_grid_res(orb_lon,cci_l3u_eu=cci_l3u_eu,cci_l3u_af = cci_l3u_af,claas=claas,found=found)

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0.
		return, -1
	endif

	; find proper ecmwf file
	orb_usecs = is_string(orbit_date) ? ymdhms2unix(strmid(orbit_date,0,12)) :long64(orbit_date)
	orb_udays = (orb_usecs / 86400d0)
	orb_date  = unix2ymdhms(orb_usecs)

	t1 = floor(orb_udays / (6d0/24d0))       * (6d0/24d0)
	t2 = floor(orb_udays / (6d0/24d0) + 1d0) * (6d0/24d0)
	w2 = (orb_udays[0] - t1[0])/(t2[0] -t1[0])
	w1 = (1d0 - w2[0])

	ecm_date1 = unix2ymdhms(long64(t1[0] * 86400d0))
	ecm_date2 = unix2ymdhms(long64(t2[0] * 86400d0))

	day = strmid(strreplace(ecm_date1,['/',':',' '],['','','']),0,8)
	hh  = strmid(strreplace(ecm_date1,['/',':',' '],['','','']),8,2)
	ecmwf_file1 = ~keyword_set(no_hr) ? ecmwf_path+day+'/ERA_Interim_an_'+day+'_'+hh+'+00_HR.nc' : ''
	if ~file_test(ecmwf_file1) and ~keyword_set(only_hr) then ecmwf_file1 = ecmwf_path+day+'/ERA_Interim_an_'+day+'_'+hh+'+00.nc'
	if ~file_test(ecmwf_file1) then begin
		print,'No ECMWF file found! For Orbit date '+orb_date
		found = 0.
		return,-1
	endif

	day = strmid(strreplace(ecm_date2,['/',':',' '],['','','']),0,8)
	hh  = strmid(strreplace(ecm_date2,['/',':',' '],['','','']),8,2)
	ecmwf_file2 = ~keyword_set(no_hr) ? ecmwf_path+day+'/ERA_Interim_an_'+day+'_'+hh+'+00_HR.nc' :''
	if ~file_test(ecmwf_file2) and ~keyword_set(only_hr) then ecmwf_file2 = ecmwf_path+day+'/ERA_Interim_an_'+day+'_'+hh+'+00.nc'
	if ~file_test(ecmwf_file2) then begin
		print,'No ECMWF file found! For Orbit date '+orb_date
		found = 0.
		return,-1
	endif

	;check if lon/lat dims are equal
	lon_dim1 = strcompress(get_ncdf_data_by_name(ecmwf_file1,'lon',/dim,found=found_londim1),/rem)
	lat_dim1 = strcompress(get_ncdf_data_by_name(ecmwf_file1,'lat',/dim,found=found_latdim1),/rem)
	lon_dim2 = strcompress(get_ncdf_data_by_name(ecmwf_file2,'lon',/dim,found=found_londim2),/rem)
	lat_dim2 = strcompress(get_ncdf_data_by_name(ecmwf_file2,'lat',/dim,found=found_latdim2),/rem)
	if ~found_londim1 or ~found_latdim1 or ~found_londim2 or ~found_latdim2 then begin
		print,'No Dimension Information found in ECMWF file! Orbit date '+orb_date
		found = 0.
		return,-1
	endif
	if (lon_dim1 ne lon_dim2 or lat_dim1 ne lat_dim2) then begin
		print,'Dimensions do not agree! Orbit date '+orb_date
		found = 0.
		return,-1
	endif

	if keyword_set(grid) or keyword_set(cci_l3u_eu) or keyword_set(cci_l3u_af) then begin
		grid_string = keyword_set(grid) ? string(grid,f='(f4.2)') : (keyword_set(cci_l3u_eu) ? 'CCI_L3U_EU' : 'CCI_L3U_AF')
		sav_file = !SAVS_DIR + '/ecmwf/ECMWF_dimension_'+lon_dim1+'_'+lat_dim1+'_collocation_index_to_global_grid_'+grid_string+'.sav'
		index = restore_var(sav_file,found=found_idx)
		if ~found_idx then begin
			save_sav  = 1
			free,index
		endif
	endif

	if ~keyword_set(index) then begin
		; make geolocation
		if keyword_set(verbose) then print,'map_ecmwf_to_orbit -> Geolocation.'
		make_geo,lon,lat_ec,file=ecmwf_file1
		; lon ist in 0-360 grad
		bla = where(lon gt 180)
		lon_ec=lon
		lon_ec[bla] = lon[bla] - 360
		if keyword_set(verbose) then print,'map_ecmwf_to_orbit -> Triangulate/Griddata.'
		; find nearest neighbor and make it fast!
		triangulate, lon_ec, lat_ec, c
		if n_elements(orb_lon) eq 1 and n_elements(orb_lat) eq 1 then begin
			; griddata needs at least two points
			o_lon  = [orb_lon,orb_lon]
			o_lat  = [orb_lat,orb_lat]
			n_eq_1 = 1
		endif else begin
			o_lon  = orb_lon
			o_lat  = orb_lat
			n_eq_1 = 0
		endelse
		index = GRIDDATA(lon_ec,lat_ec, LINDGEN(n_elements(lon_ec)), XOUT=o_lon, YOUT=o_lat, /NEAREST_N,TRIANGLES = c)
		if n_eq_1 then index = index[0]

		free,lon_ec
		free,lat_ec
		free,lon
	endif

	if keyword_set(verbose) then print,'map_ecmwf_to_orbit -> Read and interpolate ECMWF data. ', orb_date
	if keyword_set(verbose) then print,'map_ecmwf_to_orbit -> ',ecmwf_file1
	if keyword_set(verbose) then print,'map_ecmwf_to_orbit -> ',ecmwf_file2

	for i = 0,n_elements(para)-1 do begin
		; read data from file
		if para[i] eq 'stemp' then begin
			read_ncdf,ecmwf_file1,'t',data
			data1 = reform(data[*,*,59])
			read_ncdf,ecmwf_file2,'t',data
			data2 = reform(data[*,*,59])
			free,data
		endif else begin
			read_ncdf,ecmwf_file1,para[i],data1
			read_ncdf,ecmwf_file2,para[i],data2
		endelse
		dum = float(w1[0] * data1 + w2[0] * data2)
		sub = {data1:data1,data2:data2,time1:ecm_date1,time2:ecm_date2,index:long(index),weight1:w1,weight2:w2}
		sub = {time:orb_date,data:reform((dum)[index],size(orb_lon,/dim)>1),ori_data:sub}
		num   = para[i]
		struc = is_defined(struc) ? create_struct(struc,num,sub) : create_struct(num,sub)
		free, dum
		free, data1
		free, data2
	endfor

	if keyword_set(save_sav) then save_var, index, sav_file

	return, struc

end
;------------------------------------------------------------------------------------------
function map_nise_to_orbit, orb_date, orb_lon, orb_lat, found = found, grid = grid, index = index

	found = 1.
	error_status = 0
	save_sav = 0

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0.
		return, -1
	endif

	; find proper NISE file 
	yy = strmid(orb_date,0,4)
	mm = strmid(orb_date,4,2)
	dd = strmid(orb_date,6,2)

	nise_file = file_search('/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/AUXDATA/NISE_Snow_Ice/NISE???/'+yy+'/'+mm+'/'+dd+'/NISE_SSMI*_'+yy+mm+dd+'.HDFEOS',count=anz_files)
	if anz_files gt 1 then nise_file = nise_file[1]

	if ~file_test(nise_file) then begin
		print,'No NISE file found! For Orbit date '+orb_date
		return,-1
	endif

	if keyword_set(grid) then begin
		index = restore_var(!SAVS_DIR + '/nise/NISE_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav',found=found_idx)
		if ~found_idx then begin
			save_sav  = 1
			free,index
		endif
	endif
	if ~keyword_set(index) then begin
		; make geolocation
		make_geo,lon,lat,/nise

		; das ist wichtig sonst gibts später streifen über den polen bei +-135° lon und +-45° lon
		lon_n = reform(lon[*,*,0])
		lon_s = reform(lon[*,*,1])
		lat_n = reform(lat[*,*,0])
		lat_s = reform(lat[*,*,1])
		lat_n[where(lat_n lt 0)] = -999.
		lat_s[where(lat_s gt 0)] = -999.
		lon_n[where(lat_n lt 0)] = -999.
		lon_s[where(lat_s gt 0)] = -999.
		;---------------------------------

		lon_ni = [[temporary(lon_n)],[temporary(lon_s)]]
		lat_ni = [[temporary(lat_n)],[temporary(lat_s)]]

		free, lon
		free, lat

		; find nearest neighbor and make it fast!
		triangulate, lon_ni, lat_ni, c
		index = GRIDDATA(lon_ni,lat_ni, LINDGEN(n_elements(lon_ni)), XOUT=orb_lon, YOUT=orb_lat, /NEAREST_N,TRIANGLES = c)
		free, lon_ni
		free, lat_ni
	endif

	; read data from file
	read_hdf4,nise_file,'0',north,algo='nise'
	read_hdf4,nise_file,'2',south,algo='nise'
	data = [[temporary(north)],[temporary(south)]]

	if save_sav then save_var, index, !SAVS_DIR + '/nise/NISE_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav'

	return, reform((temporary(data))[index],size(orb_lon,/dim))

end
;------------------------------------------------------------------------------------------
function map_osisaf_to_orbit, orb_date, orb_lon, orb_lat, found = found, grid = grid, index = index

	found = 1.
	error_status = 0
	save_sav = 0

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0.
		return, -1
	endif

	; find proper NISE file 
	yy = strmid(orb_date,0,4)
	mm = strmid(orb_date,4,2)
	dd = strmid(orb_date,6,2)

	osi_file = file_search('/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/data/osisaf/ice_conc_??_polstere-100_reproc_'+yy+mm+dd+'1200.nc',count=anz_files)

	if anz_files ne 2 then begin
		print,'No OSI file found! For Orbit date '+orb_date
		return,-1
	endif

	osi_file_n = osi_file[where(stregex(file_basename(osi_file),'nh') ge 0)]
	osi_file_s = osi_file[where(stregex(file_basename(osi_file),'sh') ge 0)]

	if ~file_test(osi_file_n) or ~file_test(osi_file_s) then begin
		print,'Need 2 OSI files for this NH and SH!'
		return,-1
	endif

	if keyword_set(grid) then begin
		index = restore_var(!SAVS_DIR + '/osisaf/OSISAF_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav',found=found_idx)
		if ~found_idx then begin
			save_sav  = 1
			free,index
		endif
		if ~keyword_set(orb_lon) or ~keyword_set(orb_lat) then make_geo,orb_lon,orb_lat,grid=grid
	endif
	if ~keyword_set(index) then begin
		; make geolocation
		make_geo,lon_n,lat_n,file=osi_file_n[0]
		make_geo,lon_s,lat_s,file=osi_file_s[0]

		lon_ni = [reform(lon_n,n_elements(lon_n)),reform(lon_s,n_elements(lon_s))]
		lat_ni = [reform(lat_n,n_elements(lat_n)),reform(lat_s,n_elements(lat_s))]

		free, lon
		free, lat

		; find nearest neighbor and make it fast!
		triangulate, lon_ni, lat_ni, c
		index = GRIDDATA(lon_ni,lat_ni, LINDGEN(n_elements(lon_ni)), XOUT=orb_lon, YOUT=orb_lat, /NEAREST_N,TRIANGLES = c)
		free, lon_ni
		free, lat_ni
	endif

	; read data from file
	read_data,osi_file_n[0],'ice_conc',north
	read_data,osi_file_s[0],'ice_conc',south

	data = [reform(north,n_elements(north)),reform(south,n_elements(south))]

	if save_sav then save_var, index, !SAVS_DIR + '/osisaf/OSISAF_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav'

	data = reform((data)[index],size(orb_lon,/dim))
	idx = where(data le 0,idxcnt)
	if idxcnt gt 0 then data[idx] = -999.

	return, data

end
;------------------------------------------------------------------------------------------
function map_nsidc_to_orbit, orb_date, orb_lon, orb_lat, found = found, grid = grid, index = index

	found = 1.
	error_status = 0
	save_sav = 0

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0.
		return, -1
	endif

	; find proper NISE file 
	yy = strmid(orb_date,0,4)
	mm = strmid(orb_date,4,2)
	dd = strmid(orb_date,6,2)

	nise_file= file_search('/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/temp/ICE_mask/nt_'+yy+mm+dd+'_f17_v01_?.bin',count=anz_files)

	if anz_files gt 1 then nise_file = nise_file[1]

	if ~file_test(nise_file) then begin
		print,'No NSIDC file found! For Orbit date '+orb_date
		return,-1
	endif

	if keyword_set(grid) then begin
		index = restore_var(!SAVS_DIR + '/nsidc/NSIDC_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav',found=found_idx)
		if ~found_idx then begin
			save_sav  = 1
			free,index
		endif
		if ~keyword_set(orb_lon) or ~keyword_set(orb_lat) then make_geo,orb_lon,orb_lat,grid=grid
	endif
	if ~keyword_set(index) then begin
		; make geolocation
		make_geo,lon_n,lat_n,nsidc='north'
		make_geo,lon_s,lat_s,nsidc='south'

		lon_ni = [reform(lon_n,n_elements(lon_n)),reform(lon_s,n_elements(lon_s))]
		lat_ni = [reform(lat_n,n_elements(lat_n)),reform(lat_s,n_elements(lat_s))]

		free, lon_n
		free, lat_n
		free, lon_s
		free, lat_s

		; find nearest neighbor and make it fast!
		triangulate, lon_ni, lat_ni, c
		index = GRIDDATA(lon_ni,lat_ni, LINDGEN(n_elements(lon_ni)), XOUT=orb_lon, YOUT=orb_lat, /NEAREST_N,TRIANGLES = c)
		free, lon_ni
		free, lat_ni
	endif

	; read data from file
	north = READ_NSIDC_BIN('/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/temp/ICE_mask/nt_'+yy+mm+dd+'_f17_v01_n.bin')
	south = READ_NSIDC_BIN('/cmsaf/cmsaf-cld1/esa_cci_cloud_data/data/temp/ICE_mask/nt_'+yy+mm+dd+'_f17_v01_s.bin')

	data = [reform(north,n_elements(north)),reform(south,n_elements(south))]

	if save_sav then save_var, index, !SAVS_DIR + '/nsidc/NSIDC_collocation_index_global_grid_'+string(grid,f='(f4.2)')+'.sav'

	data = reform((data)[index],size(orb_lon,/dim))
	idx = where(data le 0,idxcnt)
	if idxcnt gt 0 then data[idx] = -999.

	return, data

end
;------------------------------------------------------------------------------------------
function map_usgs_to_orbit, orb_date, orb_lon, orb_lat, parameter, high_res = high_res, found = found, verbose = verbose

	found = 1.
	error_status = 0
	file = keyword_set(high_res) ? 'Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc' : 'Aux_file_CM_SAF_AVHRR_GAC_0.25deg.nc'
	para = keyword_set(parameter) ? strlowcase(parameter) : 'lus' ; ['lsm','dem','lus']

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		help, /last_message
		found = 0.
		return, -1
	endif

	; find proper ecmwf file
	orb_us    = ymdhms2unix(strmid(orb_date,0,12))
	usgs_file = '/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/usgs_type_dem/'+file

	if ~file_test(usgs_file) then begin
		print,'No USGS file found! For Orbit date '+orb_date
		found = 0.
		return,-1
	endif

	; make geolocation
	if keyword_set(verbose) then print,'map_usgs_to_orbit -> Geolocation.'
	make_geo,lon_ec,lat_ec,grid = (keyword_set(high_res) ? 0.05 : 0.25)

	; find nearest neighbor and make it fast!
	if keyword_set(verbose) then print,'map_usgs_to_orbit -> Triangulate/Griddata.'
	triangulate, lon_ec, lat_ec, c
	idx = GRIDDATA(lon_ec,lat_ec, LINDGEN(n_elements(lon_ec)), XOUT=orb_lon, YOUT=orb_lat, /NEAREST_N,TRIANGLES = c)

	if keyword_set(verbose) then print,'map_usgs_to_orbit -> Read USGS data.'
	for i = 0,n_elements(para)-1 do begin
		; read data from file
		read_ncdf,usgs_file,para[i],data
		data  = rotate(data,7)
                sub   = {data : reform((temporary(data))[idx],size(orb_lon,/dim))}
                num   = para[i]
                struc = is_defined(struc) ? create_struct(struc,num,sub) : create_struct(num,sub)
	endfor

	free,lon_ec
	free,lat_ec

	return, struc

end
;------------------------------------------------------------------------------------------
function get_new_lus,lus,nise

	; lus = land_use aus usgs 24 Klassen
	new_nise = between(nise,50,105) 

	; new land use für ANN 18
	new_lus = lus *0 -999.
	new_lus[where(lus eq 1 or between(lus,7,10) or lus eq 17 or between(lus,20,22))] = 1 	; mittlere Albedo 30-35%
	new_lus[where(between(lus,2,6) or lus eq 19 or lus eq 23)]  = 2 			; mittlere Albedo 23%
	new_lus[where(between(lus,11,15) or lus eq 18)]  = 3					; mittlere Albedo 18 %
	new_lus[where(lus eq 16)]  = 4								; Wasser
	new_lus[where(lus eq 24)]  = 5								; Ice
	; überschreiben von Ice durch Nise
	new_lus[where(new_nise eq 1)] = 5

	return,new_lus

end
;------------------------------------------------------------------------------------------
function int_stemp,cen_time,ecmwf_time,skt

	;cen_time entweder zentrum von orbit oder start von orbit oder vom aktuellen pixel
	time1 = (floor(cen_time/(6d0/24d0))     * 6d0/24d0)[0]
	time2 = (floor(cen_time/(6d0/24d0)+1d0) * 6d0/24d0)[0]

	w2 = ((cen_time - time1)/(time2-time1))[0]
	w1 = (1d0 - w2)[0]

	idx1 = where(long64(ecmwf_time*100) eq long64(time1*100),idx1_cnt)
	idx2 = where(long64(ecmwf_time*100) eq long64(time2*100),idx2_cnt)

	return, ( ((idx1_cnt+idx2_cnt) eq 2) ? (w1 * skt[idx1] + w2 * skt[idx2]) : -1)
end
;------------------------------------------------------------------------------------------
function get_l3u_ecmwf_wind, date, time, grid_res = grid_res, found = found, verbose = verbose,l3u_europe = l3u_europe

	gres  = keyword_set(grid_res) ? float(grid_res) : 0.1
	date1 = strmid(strjoin(unix2ymdhms(ymdhms2unix(date)+86400l,/arr)),0,8) ; next day

	para = ['u10m','v10m']
	if keyword_set(l3u_europe) then begin
		offsets = {grid:0.02,slon:(-15),elon:45,slat:35,elat:75}
		gres    = offsets.grid
	endif
	
	make_geo,lon,lat,grid = gres,offsets=offsets
	; ECMWF Skin temperature
	struc = map_ecmwf_to_orbit(date+'0000' , lon, lat, para, grid = gres, found = found, verbose = verbose)
		if ~found then return,-1
		um_00  = struc.(0).data
		vm_00  = struc.(1).data
	struc = map_ecmwf_to_orbit(date+'0600' , lon, lat, para, grid = gres, found = found, verbose = verbose)
		if ~found then return,-1
		um_06  = struc.(0).data
		vm_06  = struc.(1).data
	struc = map_ecmwf_to_orbit(date+'1200' , lon, lat, para, grid = gres, found = found, verbose = verbose)
		if ~found then return,-1
		um_12  = struc.(0).data
		vm_12  = struc.(1).data
	struc = map_ecmwf_to_orbit(date+'1800' , lon, lat, para, grid = gres, found = found, verbose = verbose)
		if ~found then return,-1
		um_18  = struc.(0).data
		vm_18  = struc.(1).data
	struc = map_ecmwf_to_orbit(date1+'0000', lon, lat, para, grid = gres, found = found, verbose = verbose)
		if ~found then return,-1
		um_24  = struc.(0).data
		vm_24  = struc.(1).data
	free, struc
	free, lon

	ndx  = where(time lt 0,nd_cnt)
	tdum = 	(time > 0) / 24.
	t1   = 	floor(tdum/(6d0/24d0))
	t2   = 	floor(tdum/(6d0/24d0)+1d0)
	w2   = 	(temporary(tdum) - t1 * (6d0/24d0)) / ((t2-t1) * (6d0/24d0))
	w1   = 	(1d0 - w2)
	V1   = 	(t1 eq 0) * um_00 + (t1 eq 1)  * um_06  + (t1 eq 2) * um_12    + (t1 eq 3) * um_18  + (t1 eq 4) * um_24
	V2   = 	(t2 eq 0) * temporary(um_00)   + (t2 eq 1) * temporary(um_06)  + (t2 eq 2) * temporary(um_12)   + $
		(t2 eq 3) * temporary(um_18)   + (t2 eq 4) * temporary(um_24)
	um   = 	w1 * temporary(V1) + w2 * temporary(V2)
	V1   = 	(t1 eq 0) * vm_00 + (t1 eq 1)  * vm_06  + (t1 eq 2) * vm_12    + (t1 eq 3) * vm_18  + (t1 eq 4) * vm_24
	V2   = 	(t2 eq 0) * temporary(vm_00)   + (t2 eq 1) * temporary(vm_06)  + (t2 eq 2) * temporary(vm_12)   + $
		(t2 eq 3) * temporary(vm_18)   + (t2 eq 4) * temporary(vm_24)
	vm   = 	w1 * temporary(V1) + w2 * temporary(V2)
	if nd_cnt gt 0 then um[ndx] = -999.
	if nd_cnt gt 0 then vm[ndx] = -999.

	return,{u10m:um,v10m:vm}

end
;------------------------------------------------------------------------------------------
function get_l3u_ecmwf_data, date, time, ls, grid_res = grid_res, found = found, verbose = verbose, $
							only_hr = only_hr, no_hr = no_hr, wind = wind, offsets = offsets

	gres  = keyword_set(grid_res) ? float(grid_res) : 0.1
	date1 = strmid(strjoin(unix2ymdhms(ymdhms2unix(date)+86400l,/arr)),0,8) ; next day

	para = ['skt','ci','sd']
	if keyword_set(wind) then begin
		para = [para,'u10m','v10m']
		only_hr = 1
	endif

	if keyword_set(offsets) then gres = offsets.grid
	make_geo,lon,lat,grid = gres, offsets = offsets

	; ECMWF Skin temperature
	struc = map_ecmwf_to_orbit(date+'0000' , lon, lat, para, grid = gres, found = found, only_hr = only_hr, no_hr = no_hr, verbose = verbose)
		if ~found then return,-1
		skt_00 = struc.(0).data
		ci_00  = struc.(1).data
		sd_00  = struc.(2).data
		if n_tags(struc) ge 4 then um_00  = struc.(3).data
		if n_tags(struc) ge 5 then vm_00  = struc.(4).data
	struc = map_ecmwf_to_orbit(date+'0600' , lon, lat, para, grid = gres, found = found, only_hr = only_hr, no_hr = no_hr, verbose = verbose)
		if ~found then return,-1
		skt_06 = struc.(0).data
		ci_06  = struc.(1).data
		sd_06  = struc.(2).data
		if n_tags(struc) ge 4 then um_06  = struc.(3).data
		if n_tags(struc) ge 5 then vm_06  = struc.(4).data
	struc = map_ecmwf_to_orbit(date+'1200' , lon, lat, para, grid = gres, found = found, only_hr = only_hr, no_hr = no_hr, verbose = verbose)
		if ~found then return,-1
		skt_12 = struc.(0).data
		ci_12  = struc.(1).data
		sd_12  = struc.(2).data
		if n_tags(struc) ge 4 then um_12  = struc.(3).data
		if n_tags(struc) ge 5 then vm_12  = struc.(4).data
	struc = map_ecmwf_to_orbit(date+'1800' , lon, lat, para, grid = gres, found = found, only_hr = only_hr, no_hr = no_hr, verbose = verbose)
		if ~found then return,-1
		skt_18 = struc.(0).data
		ci_18  = struc.(1).data
		sd_18  = struc.(2).data
		if n_tags(struc) ge 4 then um_18  = struc.(3).data
		if n_tags(struc) ge 5 then vm_18  = struc.(4).data
	struc = map_ecmwf_to_orbit(date1+'0000', lon, lat, para, grid = gres, found = found, only_hr = only_hr, no_hr = no_hr, verbose = verbose)
		if ~found then return,-1
		skt_24 = struc.(0).data
		ci_24  = struc.(1).data
		sd_24  = struc.(2).data
		if n_tags(struc) ge 4 then um_24  = struc.(3).data
		if n_tags(struc) ge 5 then vm_24  = struc.(4).data
	free, struc
	free, lon

	ndx  = where(time lt 0,nd_cnt)
	tdum = 	(time > 0) / 24.
	t1   = 	floor(tdum/(6d0/24d0))
	t2   = 	floor(tdum/(6d0/24d0)+1d0)
	w2   = 	(temporary(tdum) - t1 * (6d0/24d0)) / ((t2-t1) * (6d0/24d0))
	w1   = 	(1d0 - w2)
	V1   = 	(t1 eq 0) * skt_00 + (t1 eq 1) * skt_06 + (t1 eq 2) * skt_12   + (t1 eq 3) * skt_18 + (t1 eq 4) * skt_24
	V2   = 	(t2 eq 0) * temporary(skt_00)  + (t2 eq 1) * temporary(skt_06) + (t2 eq 2) * temporary(skt_12)  + $
		(t2 eq 3) * temporary(skt_18)  + (t2 eq 4) * temporary(skt_24)
	skt  = 	w1 * temporary(V1) + w2 * temporary(V2)
	V1   = 	(t1 eq 0) * ci_00 + (t1 eq 1)  * ci_06  + (t1 eq 2) * ci_12    + (t1 eq 3) * ci_18  + (t1 eq 4) * ci_24
	V2   = 	(t2 eq 0) * temporary(ci_00)   + (t2 eq 1) * temporary(ci_06)  + (t2 eq 2) * temporary(ci_12)   + $
		(t2 eq 3) * temporary(ci_18)   + (t2 eq 4) * temporary(ci_24)
	ci   = 	w1 * temporary(V1) + w2 * temporary(V2)
	V1   = 	(t1 eq 0) * sd_00 + (t1 eq 1)  * sd_06  + (t1 eq 2) * sd_12    + (t1 eq 3) * sd_18  + (t1 eq 4) * sd_24
	V2   = 	(t2 eq 0) * temporary(sd_00)   + (t2 eq 1) * temporary(sd_06)  + (t2 eq 2) * temporary(sd_12)   + $
		(t2 eq 3) * temporary(sd_18)   + (t2 eq 4) * temporary(sd_24)
	sd   = 	w1 * temporary(V1) + w2 * temporary(V2)
	nise = 	( (temporary(ci) gt 0.15) and (ls eq 0) ) or ( (sd gt 0.01) and (ls eq 1) ) or $
		( (temporary(lat) lt -60) and (temporary(sd) gt 0.01) )
	if nd_cnt gt 0 then skt[ndx]  = -999.
	if nd_cnt gt 0 then nise[ndx] = -999.
	if keyword_set(wind) then begin
		V1   = 	(t1 eq 0) * um_00 + (t1 eq 1)  * um_06  + (t1 eq 2) * um_12    + (t1 eq 3) * um_18  + (t1 eq 4) * um_24
		V2   = 	(t2 eq 0) * temporary(um_00)   + (t2 eq 1) * temporary(um_06)  + (t2 eq 2) * temporary(um_12)   + $
			(t2 eq 3) * temporary(um_18)   + (t2 eq 4) * temporary(um_24)
		um   = 	w1 * temporary(V1) + w2 * temporary(V2)
		V1   = 	(t1 eq 0) * vm_00 + (t1 eq 1)  * vm_06  + (t1 eq 2) * vm_12    + (t1 eq 3) * vm_18  + (t1 eq 4) * vm_24
		V2   = 	(t2 eq 0) * temporary(vm_00)   + (t2 eq 1) * temporary(vm_06)  + (t2 eq 2) * temporary(vm_12)   + $
			(t2 eq 3) * temporary(vm_18)   + (t2 eq 4) * temporary(vm_24)
		vm   = 	w1 * temporary(V1) + w2 * temporary(V2)
		if nd_cnt gt 0 then um[ndx] = -999.
		if nd_cnt gt 0 then vm[ndx] = -999.
		return,{stemp:skt,nise:nise,u10m:um,v10m:vm}
	endif else begin
		return,{stemp:skt,nise:nise}
	endelse

end
;------------------------------------------------------------------------------------------
;this is an extended version of david fannings cgsnapshot.pro 
;           it is now including 'EPS' and 'PDF' support
FUNCTION cgSnapshot_extended, xstart, ystart, ncols, nrows, $
   BMP=bmp, $
   Cancel=cancel, $
   Colors=colors, $
   Cube=cube, $
   Dither=dither, $
   Filename=filename, $
   GIF=gif, $
   JPEG=jpeg, $
   NoDialog=nodialog, $
   Order=order, $
   Overwrite_Prompt=overwrite_prompt, $
   PICT=pict, $
   PNG=png, $
   POSITION=position, $
   TIFF=tiff, $
   True=true, $
   Type=type, $
   Quality=quality, $
   WID=wid, $
   EPS=eps, $
   PDF=pdf, $
   _Ref_Extra=extra
   

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, /Cancel
       ok = Error_Message()
       IF N_Elements(thisWindow) EQ 0 THEN RETURN, -1
       IF thisWindow GE 0 THEN WSet, thisWindow
       
       ; Need to set color decomposition back?
       IF (N_Elements(theDecomposedState) NE 0) && (theDepth GT 0) THEN BEGIN
           Device, Decomposed=theDecomposedState
       ENDIF
       RETURN, -1
    ENDIF
    
    cancel = 0
    
    ; Check for availability of GIF files.
    thisVersion = Float(!Version.Release)
    IF (thisVersion LT 5.3) OR (thisVersion GE 6.1) THEN haveGif = 1 ELSE haveGIF = 0
    
    ; Go to correct window.
    IF N_Elements(wid) EQ 0 THEN wid =!D.Window
    thisWindow = !D.Window
    IF (!D.Flags AND 256) NE 0 THEN WSet, wid
    
    ; Did the user specify a normalized position in the window?
    IF N_Elements(position) NE 0 THEN BEGIN
       xstart = position[0] * !D.X_VSize
       ystart = position[1] * !D.Y_VSize
       ncols = (position[2]*!D.X_VSize) - xstart
       nrows = (position[3]*!D.Y_VSize) - ystart
    ENDIF

    ; Check keywords and parameters. Define values if necessary.
    IF N_Elements(xstart) EQ 0 THEN xstart = 0
    IF N_Elements(ystart) EQ 0 THEN ystart = 0
    IF N_Elements(ncols) EQ 0 THEN ncols = !D.X_VSize - xstart
    IF N_Elements(nrows) EQ 0 THEN nrows = !D.Y_VSize - ystart
    IF N_Elements(order) EQ 0 THEN order = !Order
    IF N_Elements(true) EQ 0 THEN true = 1
    dialog = 1 - Keyword_Set(nodialog)
  
    ; Is the FILENAME keyword being used? If so, get the type of the
    ; file from the filename extension.
    IF N_Elements(filename) NE 0 THEN BEGIN
       root_name = cgRootName(filename, DIRECTORY=theDir, EXTENSION=ext)
       IF ext NE "" THEN BEGIN
           type = StrUpCase(ext)
           typeFromExtension = 1
       ENDIF ELSE typeFromExtension = 0
    ENDIF ELSE typeFromExtension = 0
  
    ; Do you want to write an image file instead of capturing an image?
    IF N_Elements(type) NE 0 THEN BEGIN
       CASE StrUpCase(type) OF
          'PS' : ps =1
          'PDF': pdf = 1
          'EPS': eps = 1
          'BMP': bmp = 1
          'GIF': gif = 1
          'JPEG': jpeg = 1
          'JPG': jpeg = 1
          'PICT': pict = 1
          'PNG': png = 1
          'TIFF': tiff = 1
          'TIF': tif = 1
          ELSE: Message, 'Cannot write a file of type: ' + StrUpCase(type) + '.'
       ENDCASE
    ENDIF
    writeImage = 0
    fileType = ""
    extention = ""
    IF Keyword_Set(ps)THEN BEGIN
       writeImage = 1
       fileType = 'PS'
       extension = 'ps'
    ENDIF
    IF Keyword_Set(pdf)THEN BEGIN
       writeImage = 1
       fileType = 'PDF'
       extension = 'pdf'
    ENDIF
    IF Keyword_Set(eps)THEN BEGIN
       writeImage = 1
       fileType = 'EPS'
       extension = 'eps'
    ENDIF
    IF Keyword_Set(bmp)THEN BEGIN
       writeImage = 1
       fileType = 'BMP'
       extension = 'bmp'
    ENDIF
    IF Keyword_Set(gif) THEN BEGIN
       IF havegif THEN BEGIN
          writeImage = 1
          fileType = 'GIF'
          extension = 'gif'
        ENDIF ELSE BEGIN
           ok = Dialog_Message('GIF files not supported in this IDL version. Replacing with JPEG.')
           writeImage = 1
          fileType = 'JPEG'
          extension = 'jpg'
       ENDELSE
    ENDIF
    IF Keyword_Set(jpeg) THEN BEGIN
       writeImage = 1
       fileType = 'JPEG'
       extension = 'jpg'
    ENDIF
    IF Keyword_Set(PICT) THEN BEGIN
       writeImage = 1
       fileType = 'PICT'
       extension = 'pict'
    ENDIF
    IF Keyword_Set(png) THEN BEGIN
       writeImage = 1
       fileType = 'PNG'
       extension = 'png'
    ENDIF
    IF Keyword_Set(tiff) THEN BEGIN
       writeImage = 1
       fileType = 'TIFF'
       extension = 'tif'
    ENDIF
    
    IF N_Elements(colors) EQ 0 THEN colors = 256
    IF N_Elements(quality) EQ 0 THEN quality = 75
    dither = Keyword_Set(dither)

    ; On 24-bit displays, make sure color decomposition is ON.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
       Device, Get_Decomposed=theDecomposedState, Get_Visual_Depth=theDepth
       IF theDepth GT 8 THEN BEGIN
          Device, Decomposed=1
          IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
       ENDIF ELSE truecolor = 0
       IF wid LT 0 THEN $
          Message, 'No currently open windows. Returning.', /NoName
    ENDIF ELSE BEGIN
       truecolor = 0
       theDepth = 8
    ENDELSE

    ; Fix for 24-bit Z-buffer.
    IF (Float(!Version.Release) GE 6.4) AND (!D.NAME EQ 'Z') THEN BEGIN
       Device, Get_Decomposed=theDecomposedState, Get_Pixel_Depth=theDepth
       IF theDepth EQ 24 THEN truecolor = true ELSE truecolor = 0
    ENDIF
    
   ; Get the screen dump. 2D image on 8-bit displays. 3D image on 24-bit displays.
    image = TVRD(xstart, ystart, ncols, nrows, True=truecolor, Order=order)
    
    ; Need to set color decomposition back?
    IF theDepth GT 8 THEN Device, Decomposed=theDecomposedState
    
    ; If we need to write an image, do it here.
    IF writeImage THEN BEGIN
    
       ; Get the name of the output file.
       IF N_Elements(filename) EQ 0 THEN BEGIN
          filename = 'idl.' + StrLowCase(extension)
       ENDIF ELSE BEGIN
          IF typeFromExtension EQ 0 THEN filename = filename + "." + StrLowCase(extension)
       ENDELSE
       IF dialog THEN filename = Dialog_Pickfile(/Write, File=filename, OVERWRITE_PROMPT=Keyword_Set(overwrite_prompt))
    
       IF filename EQ "" THEN BEGIN
          cancel = 1
          RETURN, image
       ENDIF
    
       ; Write the file.
       CASE fileType OF
			'PS': BEGIN
				si = GET_SCREEN_SIZE(RESOLUTION=res) ; resolution in cm/pixel
				eps_size = float([!d.x_vsize * res[0],!d.y_vsize * res[1]])
				IF truecolor THEN BEGIN
					start_eps, filename, xyscale = eps_size, /silent,/ps
					tv, image, /true
					end_eps
				ENDIF ELSE BEGIN
					;find out later
					RETURN, -1
				ENDELSE
             END
			'EPS': BEGIN
				si = GET_SCREEN_SIZE(RESOLUTION=res) ; resolution in cm/pixel
				eps_size = float([!d.x_vsize * res[0],!d.y_vsize * res[1]])
				IF truecolor THEN BEGIN
					start_eps, filename, xyscale = eps_size, /silent
					tv, image, /true
					end_eps
				ENDIF ELSE BEGIN
					;find out later
					RETURN, -1
				ENDELSE
             END
			'PDF': BEGIN
				si = GET_SCREEN_SIZE(RESOLUTION=res) ; resolution in cm/pixel
				eps_size = float([!d.x_vsize * res[0],!d.y_vsize * res[1]])
				IF truecolor THEN BEGIN
					eps_filename = file_dirname(filename)+'/'+file_basename(filename,'pdf')+'eps'
					start_eps, eps_filename, xyscale = eps_size, /silent
					tv, image, /true
					end_eps
					eps2pdf, eps_filename , filename, ok = ok
				ENDIF ELSE BEGIN
					;find out later
					RETURN, -1
				ENDELSE
             END
          'BMP': BEGIN
             IF truecolor THEN BEGIN
                ; BMP files assume blue, green, red planes.
                temp = image[0,*,*]
                image[0,*,*] = image[2,*,*]
                image[2,*,*] = temp
                Write_BMP, filename, image, _Extra=extra
             ENDIF ELSE BEGIN
                TVLCT, r, g, b, /Get
                Write_BMP, filename, image, r, g, b, _Extra=extra
             ENDELSE
             END
    
          'GIF': BEGIN
             IF truecolor THEN BEGIN
                CASE Keyword_Set(cube) OF
                   0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
                   1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
                ENDCASE
             ENDIF ELSE BEGIN
                TVLCT, r, g, b, /Get
                image2D = image
             ENDELSE
             Write_GIF, filename, image2D, r, g, b, _Extra=extra
             END
    
          'JPEG': BEGIN
             IF truecolor THEN BEGIN
                image3D = image
             ENDIF ELSE BEGIN
                s = Size(image, /Dimensions)
                image3D = BytArr(3, s[0], s[1])
                TVLCT, r, g, b, /Get
                image3D[0,*,*] = r[image]
                image3D[1,*,*] = g[image]
                image3D[2,*,*] = b[image]
             ENDELSE
             Write_JPEG, filename, image3D, True=1, Quality=quality, _Extra=extra
             END
    
          'PICT': BEGIN
             IF truecolor THEN BEGIN
                CASE Keyword_Set(cube) OF
                   0: image2D = Color_Quan(image, 1, r, g, b, Colors=colors, Dither=dither)
                   1: image2D = Color_Quan(image, 1, r, g, b, Cube=2 > cube < 6)
                ENDCASE
             ENDIF ELSE BEGIN
                TVLCT, r, g, b, /Get
                image2D = image
             ENDELSE
             Write_PICT, filename, image2D, r, g, b
             END
    
          'PNG': BEGIN
             IF truecolor THEN BEGIN
                Write_PNG, filename, image, _Extra=extra
             ENDIF ELSE BEGIN
                TVLCT, r, g, b, /Get
                image2D = image
                Write_PNG, filename, image2D, r, g, b, _Extra=extra
             ENDELSE
             END
    
          'TIFF': BEGIN
             IF truecolor THEN BEGIN
                image3D = Reverse(image,3)
             ENDIF ELSE BEGIN
                s = Size(image, /Dimensions)
                image3D = BytArr(3, s[0], s[1])
                TVLCT, r, g, b, /Get
                image3D[0,*,*] = r[image]
                image3D[1,*,*] = g[image]
                image3D[2,*,*] = b[image]
                image3D = Reverse(Temporary(image3D), 3)
             ENDELSE
             Write_TIFF, filename, image3D, 1, _Extra=extra
             END
       ENDCASE
       RETURN, -1
    ENDIF
    
    ; Return the screen dump image.
    RETURN, image
 
END 
;------------------------------------------------------------------------------------------
pro start_save, save_as, thick = thick, size = size, landscape = landscape, snapshot = snapshot
	if keyword_set(save_as) then begin
		save_as = strcompress(strreplace(save_as,[']','[',' ',':',';',',','(',')'],['','','_','','','','',''],/noregex),/rem)
		if ~file_test( !SAVE_DIR ,/write) then begin
			ok = dialog_message('The Standard Save Directory '+ !SAVE_DIR +' does not exists or is not writeable! First edit !SAVE_DIR in vali_set_path!')
			free, save_as
			return
		endif
		if save_as eq '1' then save_as = dialog_pickfile(/over, /write, path= !SAVE_DIR , $
		title='Save Image as ...', filter=['*.png','*.eps','*.pdf'], file = 'dummy',/DEFAULT_EXTENSION)
		if save_as eq '' then begin
			print,'No Image path/name selected! Will not save anything.'
			return
		endif
		if ~file_test(file_dirname(save_as)) then begin
			ok = dialog_message('The Directory '+file_dirname(save_as)+' is unknown! Press ok to create it or cancel to Stop. ! Make sure its not a typo!',/cancel)
			if ok eq 'OK' then file_mkdir,file_dirname(save_as) else return
		endif
		if keyword_set(snapshot) then begin
			save_as = file_dirname(save_as)+'/'+file_basename(save_as,'eps')+snapshot
			image = cgSnapshot(Filename=save_as,/nodialog)
			thick=2
			!p.charsize = 2.
		endif else begin
			if keyword_set(size) then begin
				case strupcase((strcompress(size,/rem))[0]) of
					'A0'  :	si = [84.1,118.9]
					'A1'  :	si = [59.4, 84.1]
					'A2'  :	si = [42.0, 59.4]
					'A3'  :	si = [29.7, 42.0]
					'A4'  :	si = [21.0, 29.7]
					'A5'  :	si = [14.8, 21.0]
					'A6'  :	si = [10.5, 14.8]
					'A7'  :	si = [ 7.4, 10.5]
					'A8'  :	si = [ 5.2,  7.4]
					'A9'  :	si = [ 3.7,  5.2]
					'A10' :	si = [ 2.6,  3.7]
					else  : si = size
				endcase
			endif else si = [32,20]

			if keyword_set(landscape) then si = reverse(si)
			thick = 4.
			!p.charsize = 2.
			path    = file_dirname(save_as)+'/'
			ext_pos = strpos(save_as,'.',/reverse_search)+1
			ext     = ext_pos eq 0 ? '' : strmid(save_as,ext_pos)
			name    = file_basename(save_as,'.'+ext)
			case strlowcase(ext) of
				'pdf'	: pathname = path+name+'.dummy'
				'png'	: pathname = path+name+'.dummy'
				'eps'	: pathname = path+name
				else  	: begin & print,'Extension '+ext+' not supported! Choose between [eps, png, pdf]' & return & end
			endcase
			start_plot, pathname,'eps', eps_size = si
		endelse
	endif else begin
		thick = 2.
		!p.charsize=1.5
	endelse
end
;------------------------------------------------------------------------------------------
pro end_save, save_as, verbose=verbose
	if keyword_set(save_as) then begin
		save_as = strcompress(strreplace(save_as,[']','[',' ',':',';',',','(',')'],['','','_','','','','',''],/noregex),/rem)
		path    = file_dirname(save_as)+'/'
		ext_pos = strpos(save_as,'.',/reverse_search)+1
		ext     = ext_pos eq 0 ? '' : strmid(save_as,ext_pos)
		name    = file_basename(save_as,'.'+ext)
		if ~file_test(path+name+'.'+(ext eq 'eps' ? ext : 'dummy.eps')) then begin
			print, 'End_Save:: Dummy File not found! Please Check!'
			return
		endif
		end_plot
		case strlowcase(ext) of
			'pdf'	: eps2pdf,path+name+'.dummy.eps',path+name+'.'+ext,/remove, ok = ok,verbose=verbose
			'png'	: begin 
					if file_test(path+name+'.png') then ok = 1 else $
					eps2png,path+name+'.dummy.eps',path+name+'.'+ext,/remove, ok = ok
				  end
			'eps'   : ok = 1
			else	: return
		endcase
		print, 'Image saved as: '+path+name+'.'+(ok ? ext : 'dummy.eps')
	endif
end
;------------------------------------------------------------------------------------------
pro save_as_kml, image, save_file, minvalue, maxvalue, fillvalue, brewer = brewer, ctable=ctable, limit = limit, transparent=transparent

    latlonbox = keyword_set(limit)  ? limit[[2,0,3,1]] : [90.,-90.,179.9999,-180.] ; 179.999 because of marble, seems to be buggy
    ctindex   = keyword_set(ctable) ? fix(ctable) : 33

    cgImage2KML, image, Min_Value=minvalue, max_value = maxvalue, missing_value = fillvalue, $
          CTIndex=abs(ctindex),reverse=(ctindex lt 0), brewer=brewer,latlonbox=latlonbox,$
          Filename=save_file, transparent=transparent

end
;------------------------------------------------------------------------------------------
function get_data, year, month, day, orbit=orbit,data=data,satellite=satellite	, $
			instrument=instrument,global_grid=global_grid,algo=algo	, $
			no_data_value=no_data_value,found=found,level=level	, $
			filename=filename,verbose=verbose,minvalue=minvalue	, $
			maxvalue=maxvalue,longname=longname,unit=unit,sum=sum	, $
			mean=mean,median=median,random_sample=random_sample	, $
			silent=silent,raw=raw,make_compareable=make_compareable , $
			join_nodes=join_nodes,finfo=finfo,nan_fillv=nan_fillv	, $
			dirname = dirname,error=error,node=node,dim3=dim3	, $
			print_filename=print_filename,keep_data_name=keep_data_name, $
			var_dim_names=var_dim_names,flag_meanings=flag_meanings	,$
			no_recursive_search=no_recursive_search, $
			set_fillvalue = set_fillvalue,threshold = threshold, $
			offset = offset, count=count, stride=stride

	if keyword_set(verbose) then x=systime(1)

	if not keyword_set(data) then begin
		if ~keyword_set(silent) then print, 'You need to specify the parameter [data=] that you want to extract from the file!'
		found = 0.
		return,-1
	endif

	vali_set_path

	sil = keyword_set(silent)
	sat = keyword_set(satellite) 	? strlowcase(satellite) 	: ''
	lev = keyword_set(level) 		? strlowcase(level) 		: 'l3c'
	alg = keyword_set(algo)   		? ref2algo(algo,sat=sat)	: ''

	if alg eq 'myd'  then begin & alg = 'coll5' & sat = 'aqua' & end
	if alg eq 'mod'  then begin & alg = 'coll5' & sat = 'terra' & end
	if alg eq 'myd2' then begin & alg = 'coll6' & sat = 'aqua' & end
	if alg eq 'mod2' then begin & alg = 'coll6' & sat = 'terra' & end
	if alg eq 'claas' then sat = 'msg'
	if alg eq 'clara' and ( sat eq 'aatme' or sat eq 'aatsr') then sat = 'noaa17'

	is_gewex = (total(lev eq ['l3c','l3s']) and total(alg eq ['patmos','patmos_old','gewex','gac2-gewex','isccp']))

	dat = keyword_set(keep_data_name) ? strlowcase(data) : get_product_name(data,algo=alg,level=lev)

	if ( total(dat eq ['blue_marble','marble']) ) and not keyword_set(filename) then filename='dum'

	need_filename = ~total(dat eq ['blue_marble','marble','usgs_dem','usgs_lus','usgs_ls','usgs_lsm', $
					'refl1','refl2','refl3a','refl3b','rad3b','rad4','rad5','rad4,rad5'])

	if not keyword_set(filename) and need_filename then begin
		if n_params() eq 0 then begin
			print, "Syntax: result = get_data( year, month, day, /data, /algo, /satellite, /instrument, /global_grid, /no_data_value,/found,/level,/filename)"
			found = 0.
			return, -1
		endif
		select = strlowcase(alg) eq 'select'
		filename = get_filename(year,month,day,data=data,satellite=sat,level=lev,algo=alg,found=found,instrument=instrument,$
					orbit=orbit,silent=silent,dirname=dirname,node=node,no_recursive_search=no_recursive_search )
		if select then begin
			satellite = sat
			level     = lev
			algo      = alg
		endif
		if found gt 1 then print,'Found more than one file!'
	endif

	if need_filename then begin
		found = file_test(filename[0])
		if ~found and need_filename then return,-1

		if keyword_set(print_filename) and file_test(filename[0]) then $
		print,'get_data: Read File'+strcompress(print_filename,/rem)+': ', strcompress(filename[0],/rem)

		if arg_present(finfo) then finfo = file_info(filename[0])
	endif

	; create additional products
	if alg eq 'l1modis' then begin
		outdata = read_modis_l1b(filename[0], sat, dat, found = found, index = dim3, $
			no_data_value=no_data_value, minvalue=minvalue, maxvalue=maxvalue, longname=longname, unit=unit)
	endif else if (alg eq 'calipso' and (dat eq 'cfc' or dat eq 'cfc_std') ) then begin
		tmp = restore_var(filename[0],found=found)
		if found then begin
			outdata = dat eq 'cfc_std' ? tmp.stddev : tmp.mean
			no_data_value = -999.
			unit = ''
			minvalue=0
			maxvalue=1
			longname = 'Cloud Fraction' + dat eq 'cfc_std' ? ' standard deviation' : ''
			free,tmp
		endif else return,-1
	endif else if ( strmid(dat,0,3) eq 'rgb' and lev eq 'l3u' and alg eq 'esacci' ) then begin
		found = is_image(filename[0])
		if arg_present(finfo) then finfo = file_info(filename[0])
		if found then begin
			outdata = read_image(filename[0])
			if outdata[0] eq -1 then begin
				found = 0
				return,-1
			endif
			outdata = transpose(outdata,[1,2,0])
			no_data_value = -999.
			longname = 'True color image'+(keyword_set(strmid(dat,3)) ? ' + '+strupcase(strmid(dat,3)) : '')
			minvalue = 0
			maxvalue = 1
			unit=''
		endif else return,-1
	endif else if ( strmid(dat,0,3) eq 'rgb' and total(lev eq ['l3u','l3ue']) and total(alg eq ['clara2','esacciv3','esacci'])) then begin
		if alg eq 'esacci' and lev ne 'l3ue' then begin
			if ~sil then print, ' Cloud_cci v2.0 has only measurements in "L3Ue" (MODIS-Europe) data. Try Cloud_cci v3.0 instead or change level to "L3ue"'
			found=0
			return,-1
		endif
		node = strmid(dat,3)
		if node eq '' then node = sat_ampm(sat) eq 'pm' ? '_asc' : '_desc'
		read_ncdf,filename[0],get_product_name('REFL_VIS006'+node,algo=alg,level=lev),rad1 ,found=found,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('REFL_VIS008'+node,algo=alg,level=lev),rad2 ,found=f2,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('REFL_NIR016'+node,algo=alg,level=lev),rad3a,found=f3,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('BT_NIR037'  +node,algo=alg,level=lev),rad3b,found=f4,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('BT_TIR108'  +node,algo=alg,level=lev),rad4 ,found=f5,set_fillvalue=-999.

		dumdat   = get_product_name('SUNZEN'+node,algo=alg,level=lev)
		ang_file = get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,instrument=instrument,$
					silent=silent,dirname=dirname,found=f6)
		if f6 then read_ncdf,ang_file[0],dumdat,sunza,found=f6,set_fillvalue=-999.
		if total([found,f2,f3,f4,f5,f6]) eq 6. then begin
			refl_nir = (total(rad3a gt 0) gt 0)
			fac      = stregex(alg,'esacci',/bool) ? 100. : 1. ; reflectance in percent oder normalized?
			if sat eq 'aqua' or sat eq 'terra' then begin
				;MODIS vis008 channel gets saturated at high reflectances 
				idx = where(rad1 gt (50./fac) and rad2 le 0.,idxcnt )
				if idxcnt gt 0 then rad2[idx] = rad1[idx] * 1.02
			endif
			if ~sil then print, ' Calculating RGB image for '+sat_name(alg,sat)+' ('+string(get_grid_res(rad4),f='(f4.2)')+' degree). Be patient ...'
			outdata = calc_rgb(rad1*fac,rad2*fac,(refl_nir ? rad3a*fac:rad3b),rad4,sunza,0,/enhance,refl_nir037=refl_nir)
		endif else begin
			darr = strjoin((['VIS006','VIS008','NIR16','NIR37','BT108','SZA'])[where([found,f2,f3,f4,f5,f6] eq 0)],' and ')
			if ~sil then print, 'Missing measurements neeeded for RGB calculations: ',darr
			found=0
			return,-1
		endelse
		no_data_value = -999.
		longname = 'True color image'
		minvalue = 0
		maxvalue = 1
		unit=''
	endif else if ( strmid(dat,0,3) eq 'fci' and total(lev eq ['l3u','l3ue']) and total(alg eq ['clara2','esacciv3','esacci'])) then begin
		if alg eq 'esacci' and lev ne 'l3ue' then begin
			if ~sil then print, ' Cloud_cci v2.0 has only measurements in "L3Ue" (MODIS-Europe) data. Try Cloud_cci v3.0 instead or change level to "L3ue"'
			found=0
			return,-1
		endif
		dum  = strsplit(dat,'_',/ext)
		node = n_elements(dum) gt 1 ? '_'+dum[n_elements(dum)-1] : ''
		if node eq '' then node = sat_ampm(sat) eq 'pm' ? '_asc' : '_desc'
		read_ncdf,filename[0],get_product_name('REFL_VIS006'+node,algo=alg,level=lev),rad1 ,found=found,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('REFL_VIS008'+node,algo=alg,level=lev),rad2 ,found=f2,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('REFL_NIR016'+node,algo=alg,level=lev),rad3a,found=f3,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('BT_NIR037'  +node,algo=alg,level=lev),rad3b,found=f4,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('BT_TIR108'  +node,algo=alg,level=lev),rad4 ,found=f5,set_fillvalue=-999.
		read_ncdf,filename[0],get_product_name('BT_TIR120'  +node,algo=alg,level=lev),rad5 ,found=f6,set_fillvalue=-999.
		dumdat   =  get_product_name('SUNZEN'+node,algo=alg,level=lev)
		ang_file =  get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,instrument=instrument,$
					silent=silent,dirname=dirname,found=f7)
		if f7 then read_ncdf,ang_file[0],dumdat,sunza,found=f7,set_fillvalue=-999.

		if total([found,f2,f3,f4,f5,f6,f7]) eq 7. then begin
			filter = is_number(strmid(dum[0],3)) ? fix(strmid(dum[0],3)) : 0; default is 0
			;calculate reflectances for the 3.7 channel and multiply with 100
			satdum  = noaa_primes(year,month,ampm=sat_ampm(sat,/ampm),/no_zero,/no_minus)
			ref3b	= bt37_to_ref37( doy(year,month,day), rad3b, rad4, sunza, satdum, no_data_val=-999.)
			ref3b	= ( ref3b *100. )
			idx     = where(rad3b eq -999. or rad4 eq -999. or sunza eq -999.,idxcnt)
			if idxcnt gt 0 then ref3b[idx]=-999.
			;--------------------------------
			fac     = (alg eq 'esacciv3') ? 100. : 1. ; reflectance in percent oder normalized?
			if sat eq 'aqua' or sat eq 'terra' then begin
				;MODIS vis008 channel gets saturated at high reflectances 
				idx = where(rad1 gt (50./fac) and rad2 le 0.,idxcnt )
				if idxcnt gt 0 then rad2[idx] = rad1[idx] * 1.02
			endif
			if ~sil then print, ' Calculating False Color Image for '+sat_name(alg,sat)+' ('+string(get_grid_res(rad4),f='(f4.2)')+' degree). Be patient ...'
			outdata = false_color_max(filter, rad1*fac, rad2*fac, ref3b, rad3b, rad4, rad5, sunza, longname=longname)
		endif else begin
			darr = strjoin((['VIS006','VIS008','NIR16','NIR37','BT108','BT120','SZA'])[where([found,f2,f3,f4,f5,f6,f7] eq 0)],' and ')
			if ~sil then print, 'Missing measurements neeeded for FCI calculations: ',darr
			found=0
			return,-1
		endelse
		no_data_value = -999.
		minvalue = 0
		maxvalue = 1
		unit=''
	endif else if ( strmid(dat,0,3) eq 'fci' and total(lev eq ['l3u','l3ue']) and alg eq 'patmos') then begin
		dum  = strsplit(dat,'_',/ext)
		read_data,filename[0],'refl_0_65um_nom',rad1 ,found=found,set_fillvalue=-999.
		read_data,filename[0],'refl_0_86um_nom',rad2 ,found=f2,set_fillvalue=-999.
		read_data,filename[0],'refl_1_60um_nom',rad3a,found=f3,set_fillvalue=-999.
		read_data,filename[0],'temp_3_75um_nom',rad3b,found=f4,set_fillvalue=-999.
		read_data,filename[0],'temp_11_0um_nom',rad4 ,found=f5,set_fillvalue=-999.
		read_data,filename[0],'temp_12_0um_nom',rad5 ,found=f6,set_fillvalue=-999.
		read_data,filename[0],'solar_zenith_angle',sunza,found=f7,set_fillvalue=-999.
		if total([found,f2,f3,f4,f5,f6,f7]) eq 7. then begin
			filter = is_number(strmid(dum[0],3)) ? fix(strmid(dum[0],3)) : 0; default is 0
			;calculate reflectances for the 3.7 channel and multiply with 100
			satdum  = noaa_primes(year,month,ampm=sat_ampm(sat,/ampm),/no_zero,/no_minus)
			ref3b	= bt37_to_ref37( doy(year,month,day), rad3b, rad4, sunza, satdum, no_data_val=-999.)
			ref3b	= ( ref3b *100. )
			idx     = where(rad3b eq -999. or rad4 eq -999. or sunza eq -999.,idxcnt)
			if idxcnt gt 0 then ref3b[idx]=-999.
			;--------------------------------
			
			if ~sil then print, ' Calculating False Color Image for PATMOS-X ('+string(get_grid_res(rad4),f='(f4.2)')+' degree). Be patient ...'
			outdata = false_color_max(filter, rad1, rad2, ref3b, rad3b, rad4, rad5, sunza, longname=longname)
		endif else begin
			darr = strjoin((['VIS006','VIS008','NIR16','NIR37','BT108','BT120','SZA'])[where([found,f2,f3,f4,f5,f6,f7] eq 0)],' and ')
			if ~sil then print, 'Missing measurements neeeded for FCI calculations: ',darr
			found=0
			return,-1
		endelse
		no_data_value = -999.
		minvalue = 0
		maxvalue = 1
		unit=''
	endif else if ( strmid(dat,0,3) eq 'rgb' and total(lev eq ['l3u','l3ue']) and alg eq 'patmos') then begin
		read_data,filename[0],'refl_0_65um_nom',rad1 ,found=found,set_fillvalue=-999.
		read_data,filename[0],'refl_0_86um_nom',rad2 ,found=f2,set_fillvalue=-999.
		read_data,filename[0],'refl_1_60um_nom',rad3a,found=f3,set_fillvalue=-999.
		read_data,filename[0],'temp_3_75um_nom',rad3b,found=f4,set_fillvalue=-999.
		read_data,filename[0],'temp_11_0um_nom',rad4 ,found=f5,set_fillvalue=-999.
		read_data,filename[0],'solar_zenith_angle',sunza,found=f6,set_fillvalue=-999.
		if total([found,f2,f3,f4,f5,f6]) eq 6. then begin
			refl_nir = (total(rad3a gt 0) gt 0)
			if ~sil then print, ' Calculating RGB image for PATMOS-X (0.10 degree). Be patient ...'
			outdata = calc_rgb(rad1,rad2,(refl_nir ? rad3a:rad3b),rad4,sunza,0,/enhance,refl_nir037=refl_nir,/true)
		endif else begin
			darr = strjoin((['VIS006','VIS008','NIR16','NIR37','BT108','SZA'])[where([found,f2,f3,f4,f5,f6] eq 0)],' and ')
			if ~sil then print, 'Missing measurements neeeded for RGB calculations: ',darr
			found=0
			return,-1
			if ~sil then print, ' At least one of the measurements neeeded for RGB is missing!'
			found=0
			return,-1
		endelse
		no_data_value = -999.
		longname = 'True color image'
		minvalue = 0
		maxvalue = 1
		unit=''
	endif else if (dat eq 'cloud_phase' and alg eq 'patmos' and lev eq 'l3u') then begin
		; die neuen patmos l2b haben kein cloud_phase mehr -> berechne alte cloud_phase definition aus cloud_type (auch für die alten l2b's)
		read_data, filename[0], 'cloud_type', cty, no_data_value, minvalue, maxvalue, longname, unit, flag_meanings, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		outdata = cty * 0 + no_data_value[0]
		idx = where(between(cty,0,1),idxcnt)
		if idxcnt gt 0 then outdata[idx]	=0 ; 0=clear,1=probably clear
		idx = where(between(cty,2,3),idxcnt)
		if idxcnt gt 0 then outdata[idx]	=1 ; 2=fog,3=water
		idx = where(cty eq 4,idxcnt)
		if idxcnt gt 0 then outdata[idx]        =2 ; 4=supercooled water
		idx = where(cty eq 5,idxcnt)
		if idxcnt gt 0 then outdata[idx]        =3 ; 5=mixed
		idx = where(between(cty,6,9),idxcnt)
		if idxcnt gt 0 then outdata[idx]	=4 ; 6=opaque_ice,7=cirrus,8=overlapping,9=overshooting
		idx = where(cty eq 10,idxcnt)
		if idxcnt gt 0 then outdata[idx]	=5 ; 10=unknown
		flag_meanings = ['clear','water','supercooled water','mixed','ice','unknown']
		longname = 'integer classification of the cloud phase including clear and aerosol type,0=clear,1=water,2=supercooled water,3=mixed,4=ice,5=unknown'
		minvalue = 0
		maxvalue = 5
		unit = ''
	endif else if ( strmid(dat,0,4) eq 'cdnc' and total(alg eq ['clara2','esacci','esacciv3','patmos']) and total(lev eq ['l3u','l3ue','l2']) ) then begin
		if lev eq 'l2' then begin
			print, 'get_data:: Creating CDNC on level 2 files is not fully testet yet. We assume now all needed data is in one file!'
			;We dont have l2 anymore we judst assume that all data is in one file
			if lev eq 'l2' then dirname = file_dirname(filename[0]) else free, dirname
			node = ''
		endif else begin
			if ~keyword_set(node) then begin
				node   = strmid(lev,0,3) eq 'l3u' ? '_'+(reverse(strsplit(dat,'_',/ext)))[0] : ''
			endif else node = '_'+node
			if alg eq 'patmos' then node=''
		endelse
		set_fillvalue = -999.

		; 1) cot
		dumdat = get_product_name('cot'+node,algo=alg,level=lev)
		cot_file = get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,found=found,instrument=instrument,/silent,dirname=dirname)
		if found then begin
			if ~sil then print,'cot_file: ',dumdat,'    : ',cot_file
			read_data, cot_file[0], dumdat, cot, no_data_valuec, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
					found = found,/silent, count = count, offset = offset, stride = stride
		endif
		; 2) cer
		dumdat = get_product_name('cer'+node,algo=alg,level=lev)
		cer_file = get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,found=f1,instrument=instrument,/silent,dirname=dirname)
		if f1 then begin
			if ~sil then print,'cer_file: ',dumdat,'    : ',cer_file
			read_data, cer_file, dumdat, cer, no_data_valuer, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
					found = f1,/silent, count = count, offset = offset, stride = stride
			if f1 then begin
				;e.g. clara cer in m, needs to be in µm
				ui = strlowcase(strreplace(strcompress(unit,/rem),['\[','\]'],['','']))
				if ui eq 'm' then cer /= 1.e-06
			endif
		endif
		; 3) ctp
		dumdat = get_product_name('ctp'+node,algo=alg,level=lev)
		ctp_file = get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,found=f2,instrument=instrument,/silent,dirname=dirname)
		if f2 then begin
			if ~sil then print,'ctp_file: ',dumdat,': ',ctp_file
			read_data, ctp_file, dumdat, ctp, no_data_valuep, minvalue, maxvalue, longname, unit, found = f2, $
					set_fillvalue = set_fillvalue, /silent, count = count, offset = offset, stride = stride
			if f2 then begin
				;e.g. clara cer in m, needs to be in µm
				ui = strlowcase(strreplace(strcompress(unit,/rem),['\[','\]'],['','']))
				if ui eq 'hpa' then ctp *= 100. else if ui ne 'pa' then begin
					print,'CTP is not in Pascal not in hPa! Check unit.'
					stop
				endif
			endif
		endif
		; 4) ctt
		dumdat = get_product_name('ctt'+node,algo=alg,level=lev)
		ctt_file = get_filename(year,month,day,data=dumdat,satellite=sat,level=lev,algo=alg,found=f3,instrument=instrument,/silent,dirname=dirname)
		if f3 then begin
			if ~sil then print,'ctt_file: ',dumdat,': ',ctt_file
			read_data, ctt_file, dumdat, ctt, no_data_valuet, found = f3, set_fillvalue = set_fillvalue,$
					/silent, count = count, offset = offset, stride = stride
		endif
		;illum 1, über wasser, nur liquid clouds
		; 5) cph
		dumdat = get_product_name('cph'+node,algo=alg,level=lev)
		cph_file = get_filename(year,month,day,data=dumdat, satellite=sat,level=lev,algo=alg,found=f4,instrument=instrument,/silent,dirname=dirname)
		if f4 then begin
			if ~sil then print,'cph_file: ',dumdat,': ',ctt_file
			;do this for Patmos L3U
			cph = get_data(	year,month,day,file=cph_file,data=dumdat,satellite=sat,level=lev,$
							algo=alg,dirname=dirname,/silent,set_fillvalue=set_fillvalue,found=f4,/make_compare)
		endif
		; 6) SZA
		dumdat = get_product_name('solzen'+node,algo=alg,level=lev)
		sza_file = get_filename(year,month,day,data=dumdat, satellite=sat,level=lev,algo=alg,found=f5,instrument=instrument,/silent,dirname=dirname)
		if f5 then begin
			if ~sil then print,'sza_file: ',dumdat,': ',sza_file
			read_data, sza_file, dumdat, sza, found = f5, set_fillvalue = set_fillvalue,$
					/silent, count = count, offset = offset, stride = stride
		endif
		; 7) Land / Sea
		if ~sil then print,'ls_file: ','lsflag',': ',filename
		read_data, 	filename, 'lsflag', ls, found = f6, set_fillvalue = set_fillvalue,$
					/silent, count = count, offset = offset, stride = stride
		if f6 eq 0 then begin
			;try lon/lat instead
			make_geo,lon,lat,file=filename,found=f6
			if f6 then ls = get_coverage(lon,lat,/land,found=f6)
		endif
		if not found then begin
			ok = dialog_message('get_data:: CDNC: could not find ls flag!')
			return,-1
		endif
		if total([found,f1,f2,f3,f4,f5,f6]) eq 7. then begin
			; cdnc = tr2nh_export(cot,cer,ctt,ctp) ; R.Bennartz et.al
			data_idx = where(cot gt 0. and cer gt 4. and ctp gt 10000. and between(ctt,268,300) and cph eq 1 and between(sza,0.,75.) and ls eq 0,cnt_il)
			outdata  = cot * 0. -999.
			if cnt_il gt 0 then outdata[data_idx] = tr2nh_export(cot[data_idx], cer[data_idx], ctt = ctt[data_idx], p = ctp[data_idx])
			longname = 'Cloud Droplet Number concentration'
			unit = textoidl(' [cm^{-3}]')
			no_data_value = -999.
			minvalue = 0.
			maxvalue = 300.
		endif else begin
			darr = strjoin((['COT','CER','CTP','CTT','CPH','SZA','LSF'])[where([found,f1,f2,f3,f4,f5,f6] eq 0)],' and ')
			if ~sil then print, 'Missing parameter neeeded for CDNC calculations: ',darr
			found=0
			return,-1
		endelse
	endif else if ( ((total(alg eq ['clara2','clara','claas']) or is_gewex) and (dat eq 'cwp')) or (alg eq 'clara2' and dat eq 'cwp_error') $
			and (lev eq 'l3c' or lev eq 'l3s')) then begin
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: cwp = lwp * cph + iwp * (1-cph)'
		err = stregex(dat,'_error',/bool) ? '_error' : ''
		; 1) iwp
		ice_file = get_filename(year,month,day,data='iwp', satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		dumdat = get_product_name('iwp'+err,algo=alg,level=lev)
		if ~sil then print,'ice_file: ',dumdat,'    : ',ice_file
		read_data, ice_file[0], dumdat, ice, no_data_valuei, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 2) lwp
		liq_file = get_filename(year,month,day,data='lwp', satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		dumdat = get_product_name('lwp'+err,algo=alg,level=lev)
		if ~sil then print,'liq_file: ',dumdat,'    : ',liq_file
		read_data, liq_file, dumdat, liq, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 3) cph
		cph_file = get_filename(year,month,day,data=(is_gewex ? 'cph_day':'cph'), satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		dumdat = get_product_name('cph_day',algo=alg,level=lev)
		if ~sil then print,'cph_file: ',dumdat,': ',cph_file
		read_data, cph_file, dumdat, cph, no_data_valuec, verbose = verbose, found = found, set_fillvalue = set_fillvalue,$
		silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; cwp = lwp * cph + iwp * (1-cph)
		no_idx_ice = where((ice eq no_data_valuei[0] and liq eq no_data_value[0]) or cph eq no_data_valuec[0],cnt_il)
		cph = cph/100.
		outdata = ( (temporary(ice) > 0.) * (1- (cph)) ) + ( (temporary(liq) > 0.) * (temporary(cph)) )
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		longname = 'monthly mean cloud water path'
		if ~sil then print,''
	endif else if (total(alg eq ['esacci','esacciv3','coll6']) and total(dat eq ['cwp','cwp_16','cwp_37']) and (lev eq 'l3c' or lev eq 'l3s')) then begin
		ch = ''
		if alg eq 'coll6' and stregex(dat,'_16',/fold,/bool) then ch = '_16'
		if alg eq 'coll6' and stregex(dat,'_37',/fold,/bool) then ch = '_37'
		; 1) iwp
		dumdat = get_product_name('iwp'+ch,algo=alg,level=lev)
		read_data, filename[0], dumdat, ice, no_data_valuei, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 2) lwp
		dumdat = get_product_name('lwp'+ch,algo=alg,level=lev)
		read_data, filename[0], dumdat, liq, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 3) cph
		if alg eq 'coll6' then begin
			cph = 	get_data(year,month,day,file=filename[0],data='cph'+ch+'_day', satellite=sat, level=lev, verbose = verbose,$
				algo=alg,dirname=dirname,silent=silent,no_data_value=no_data_valuec,found=found)
		endif else begin
			dumdat = get_product_name('cph_day',algo=alg,level=lev)
			read_data, filename[0], dumdat, cph, no_data_valuec, verbose = verbose, set_fillvalue = set_fillvalue,$
			found = found, silent=silent, count = count, offset = offset, stride = stride
		endelse
		if not found then return,-1
		; cwp = lwp * cph + iwp * (1-cph)
		no_idx_ice = where((ice eq no_data_valuei[0] and liq eq no_data_value[0]) or cph eq no_data_valuec[0],cnt_il)
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: cwp = lwp * cph + iwp * (1-cph)'
		outdata = ( (temporary(ice) > 0.) * (1- (cph)) ) + ( (temporary(liq) > 0.) * (temporary(cph)) )
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		longname = 'monthly mean cloud water path'+ch
		if ~sil then print,''
	endif else if (dat eq 'cwp_allsky' and alg ne 'era-i' and alg ne 'era-i2' and (lev eq 'l3c' or lev eq 'l3s') ) then begin
		cwp = 	get_data(year,month,day,file=filename[0],data='cwp', satellite=sat, level=lev, verbose = verbose,$
				algo=alg,dirname=dirname,silent=silent,no_data_value=no_data_value,found=found, longname=longname,unit=unit)
		if not found then return,-1
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: cwp_allsky=cwp*cfc_day'
		; 2) cloud fraction day
		if alg eq 'esacci' or alg eq 'esacciv3' then begin
			; this is the actual microphysical daytime cloud fraction of cc4cl based on same datset as lwp,iwp,cph_day
			read_data, filename[0], 'nobs_clear_day'  , nclear, no_data_valuei, minvalue, maxvalue, longnamei, set_fillvalue = set_fillvalue,$
			uniti, verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
			read_data, filename[0], 'nretr_cloudy_day', ncloudy, no_data_valuei, minvalue, maxvalue, longnamei, set_fillvalue = set_fillvalue,$
			uniti, verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
			cfc = ncloudy/float((nclear+ncloudy)>1)
			idx = where(((nclear>0)+(ncloudy>0)) eq 0,idxcnt)
			if idxcnt gt 0 then cfc[idx] = -999.
		endif else begin
			dumdat = get_product_name('cfc_day',algo=alg,level=lev)
			if alg eq 'coll6' then dumdat = 'CFC_DAY_MICRO'
			cfc_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
			read_data, cfc_file[0], dumdat, cfc, no_data_valuei, minvalue, maxvalue, longnamei, uniti, set_fillvalue = set_fillvalue,$
			verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
			if is_gewex and keyword_set(month) then cfc = reform(cfc[*,*,fix(month)-1])
		endelse
		if not found then return,-1
		if ~sil and (total(alg eq ['clara2','clara','claas']) or is_gewex) then print,'cfc_file: ',dumdat,': ',cfc_file
		; cwp_allsky=cwp*cfc_day
		no_idx_ice = where(cfc eq no_data_valuei[0],cnt_il)
		if total(alg eq ['clara2','clara','claas','isccp']) then cfc = cfc/100.
		outdata = ( (temporary(cwp) > 0.) * (temporary(cfc)) )
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		longname = 'All Sky '+longname
		if ~sil then print,''
	endif else if ( (total(alg eq ['coll5','coll6']) or is_gewex) and $
			total(dat eq ['iwp_allsky','lwp_allsky','iwp_16_allsky','lwp_16_allsky','iwp_37_allsky','lwp_37_allsky']) $ 
			and (lev eq 'l3c' or lev eq 'l3s') ) then begin
		ch = ''
		if alg eq 'coll6' and stregex(dat,'_16',/fold,/bool) then ch = '_16'
		if alg eq 'coll6' and stregex(dat,'_37',/fold,/bool) then ch = '_37'
		; 1) iwp oder lwp
		dumdat = get_product_name(strmid(dat,0,3),algo=alg,level=lev)
		read_data, filename[0], dumdat+ch, cwp, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 2) cloud fraction
		; for coll6 use 'CFC_DAY_MICRO' !!!
		dumdat = get_product_name('cfc_day',algo=alg,level=lev)
		; for coll6 use 'CFC_DAY_MICRO' !!!
		if alg eq 'coll6' then dumdat = 'CFC_DAY_MICRO'
		cfc_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		read_data, cfc_file[0], dumdat, cfc, no_data_valuei, minvalue, maxvalue, longnamei, uniti, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 3) cph
		if is_gewex then begin
			cph_file = get_filename(year,month,day,data='cph_day', satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
			if not found then return,-1
			dumdat = get_product_name('cph_day',algo=alg,level=lev)
			if ~sil then print,'cph_file: ',dumdat,': ',cph_file
			read_data, cph_file[0], dumdat, cph, no_data_valuec, verbose = verbose, set_fillvalue = set_fillvalue,$
			found = found, silent=silent, count = count, offset = offset, stride = stride
		endif else begin
			cph = get_data(year,month,day,file=filename[0],data='cph'+ch+'_day', satellite=sat, level=lev, verbose = verbose,$
				algo=alg,dirname=dirname,silent=silent,no_data_value=no_data_valuec,found=found)
		endelse
		if not found then return,-1
		; lwp_allsky=lwp*cfc_day*cph_day
		; iwp_allsky=iwp*cfc_day*(1.-cph_day)
		no_idx_ice = where(cph eq no_data_valuec[0] and cfc eq no_data_valuei[0] and cwp eq no_data_value[0],cnt_il)
		if is_gewex then cph = cph/100.
		if stregex(dat,'iwp',/fold,/bool) then begin
			if ~sil then print,'Calculating '+dat+' for '+alg+' with: iwp_allsky=iwp*cfc_day*(1.-cph_day)'
			outdata = ( (temporary(cwp) > 0.) * (temporary(cfc)>0.) * (1- (temporary(cph)>0.)) )
		endif
		if stregex(dat,'lwp',/fold,/bool) then begin
			if ~sil then print,'Calculating '+dat+' for '+alg+' with: lwp_allsky=lwp*cfc_day*cph_day'
			outdata = ( (temporary(cwp) > 0.) * (temporary(cfc)>0.) * (   (temporary(cph)>0.)) )
		endif
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		if ~sil then print,''
	endif else if total(dat eq ['rad3b','rad4','rad5','refl1','refl2','refl3a','refl3b','rad4,rad5']) and alg eq 'clara2' and lev eq 'l3c' then begin
		ref    = algo2ref(alg)
		if sat_ampm(sat,/avhrr) eq 'am' then sat = 'noaaam'
		if sat_ampm(sat,/avhrr) eq 'pm' then sat = 'noaapm'
		filename = !SAVS_DIR + ref+'_rad_mm/'+string(year,f='(i4.4)')+string(month,f='(i2.2)')+'_'+strlowcase(sat)+'_'+ref+'_radiances_L3c.sav'
		d = restore_var(filename,found=found)
		if found then begin
			if arg_present(finfo) then finfo = file_info(filename[0])
			if keyword_set(print_filename) then print,'get_data: Read File'+strcompress(print_filename,/rem)+': ', strcompress(filename[0],/rem)
			strucname = stregex(dat,'refl',/bool,/fold) ? strupcase('rad'+strmid(dat,4)) : strupcase(dat)
			strucidx  = where(tag_names(d) eq strucname,idxcnt)
			if idxcnt gt 0 then begin
				outdata  = d.(strucidx)
				no_data_value = -999.
				unit     = stregex(dat,'refl',/bool,/fold) ? ' [%]' : ' [K]'
				maxvalue = stregex(dat,'refl',/bool,/fold) ? 120 : 350
				minvalue = stregex(dat,'refl',/bool,/fold) ?   0 : 150
				longname = stregex(dat,'refl',/bool,/fold) ? 'reflectance in AVHRR channel no '+strmid(dat,4) : $
						'brightness temperature in AVHRR channel no '+strmid(dat,3)
			endif else if dat eq 'rad4,rad5' then begin
				outdata			= d.RAD4 - d.RAD5
				no_data_value	= -999.
				unit			= ' [K]'
				maxvalue		= 10.
				minvalue		= -10.
				longname 		= 'brightness temperature difference in AVHRR channel no 4 minus 5'
				ndidx = where(d.RAD4 eq no_data_value or d.RAD5 eq no_data_value,ndcnt)
				if ndcnt gt 0 then outdata[ndidx] = no_data_value
			endif else if dat eq 'refl3b' then begin
				print,'please include refl3b'
				stop
				sunza			= '???'
				outdata			= bt37_to_ref37( doy(yy,mm,dd), d.rad3b, d.rad4, sunza, sat, no_data_val=-999.)
				no_data_value	= -999.
				unit			= ' [%]'
				maxvalue		= 120.
				minvalue		=   0.
				longname 		= 'reflectance in AVHRR channel no 3b'
			endif else found = 0
		endif
	endif else if ( total(dat eq ['blue_marble','marble']) )   then begin
		marble_file = !SAVS_DIR + '/blue_marble/blue_marble_0.10.sav'
		if arg_present(finfo) then finfo = file_info(marble_file[0])
		outdata = restore_var(marble_file,found=found)
		if found then outdata = fix(outdata)
		if keyword_set(print_filename) then print,'get_data: Read File'+strcompress(print_filename,/rem)+': ', strcompress(marble_file[0],/rem)
		no_data_value = -999.
		longname = 'Blue Marble 0.1 degree global grid'
		maxvalue = 1
		minvalue = 0
		unit = ''
	endif else if ( total(strmid(dat,0,5) eq ['usgs_']) ) then begin
		if dat eq 'usgs_ls' then begin
			dumdat = 'lus'
			ls = 1
		endif else begin 
			dumdat = strmid(dat,5)
			ls = 0
		endelse
		if keyword_set(error) and dumdat eq 'lus' then begin
			usgs_file   	= !SAVS_DIR + 'usgs/USGS_0.5km_MODIS_based_Land_Cover_Type_2_0.05_degree_regular.sav'
			dum 			= restore_var(usgs_file[0],found=found)
			outdata 		= dum.sampled ; dum.averaged
			minvalue 		= dum.minvalue
			maxvalue 		= dum.maxvalue
			no_data_value 	= dum.no_data_value
			longname 		= dum.longname
			flag_meanings 	= dum.flag_meanings
			unit 			= dum.unit
			if ls then outdata = outdata ne 0
		endif else begin
			usgs_file = '/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/usgs_type_dem/Aux_file_CM_SAF_AVHRR_GAC_ori_0.05deg.nc'
			if keyword_set(print_filename) then print,'get_data: Read File'+strcompress(print_filename,/rem)+': ', strcompress(usgs_file[0],/rem)
			read_data, usgs_file, dumdat, outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
			unit, verbose = verbose,found = found, count = count, offset = offset, stride = stride
			if dumdat eq 'lus' then flag_meanings = ['URBAN','DRYLAND','IRRIGATED','MIXED_DRYLAND_IRRIGATED','CROPLAND_GRASSLAND','CROPLAND_WOODLAND',$
								'GRASSLAND','SHRUBLAND','SHRUBLAND_GRASSLAND','SAVANNA','FOREST_DDS_BROADLEAF'		,$
								'FOREST_DDS_NEEDLELEAF','FOREST_EVG_BROADLEAF','FOREST_EVG_NEEDLELEAF'	,$
								'FOREST_MIXED','WATER','WETLAND_HERBACEOUS','WETLAND_WOODED','BARREN','TUNDRA_HERBACEOUS'	,$
								'TUNDRA_WOODED','TUNDRA_MIXED','TUNDRA_BARE','ICE']
			if ls then outdata = outdata ne 16 else begin
				outdata  -= 1
				minvalue -= 1
				maxvalue -= 1
			endelse
		endelse
		outdata = rotate(outdata,7)
		if ls then begin
			minvalue = 0
			maxvalue = 1
			longname = 'Land Sea Mask based on USGS Land USE'
			flag_meanings = ['Sea','Land']
		endif
		if arg_present(finfo) then finfo = file_info(usgs_file[0])
	endif else if ( (total(alg eq ['coll5','coll6']) and total(dat eq ['cph_day','cph_16_day','cph_37_day']) ) ) then begin
		ch = ''
		if stregex(dat,'_16_',/fold,/bool) and alg eq 'coll6' then ch = '16_'
		if stregex(dat,'_37_',/fold,/bool) and alg eq 'coll6' then ch = '37_'
		dumdat = alg eq 'coll6' ? 'CLOUD_RETRIEVAL_FRACTION_'+ch+'ICE_PIXEL_COUNTS' : 'Cloud_Fraction_Ice_Pixel_Counts'
		read_data, filename[0],dumdat,ice_cnt, verbose = verbose,found = found, set_fillvalue = set_fillvalue,$
		silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		dumdat = alg eq 'coll6' ? 'CLOUD_RETRIEVAL_FRACTION_'+ch+'LIQUID_PIXEL_COUNTS' : 'Cloud_Fraction_Liquid_Pixel_Counts'
		read_data, filename[0],dumdat,liq_cnt, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose,found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: cph_day= '+ch+'Liquid_Pixel_Counts/('+ch+'Ice_Pixel_Counts+'+ch+'Liquid_Pixel_Counts)'
		outdata = LIQ_CNT/(ICE_CNT+LIQ_CNT)
		idx = where(LIQ_CNT le 0 and ICE_CNT le 0,idxcnt)
		if idxcnt gt 0 then outdata[idx] = no_data_value[0]
		longname = ch+'Liquid Cloud Fraction'
		maxvalue = 1
		minvalue = 0
		unit = ''
		if ~sil then print,''
	endif else if (alg eq 'coll6' and total(dat eq ['cer','cer_16','cer_37','cot','cot_16','cot_37']) ) then begin
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: ice*(1.-cph_day)+liq*cph_day'
		ch = ''
		if stregex(dat,'_16',/fold,/bool) and alg eq 'coll6' then ch = '16_'
		if stregex(dat,'_37',/fold,/bool) and alg eq 'coll6' then ch = '37_'
		; 1) ice
		read_data, filename[0], dat+'_ice', ice, no_data_valuei, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 2) liq
		read_data, filename[0], dat+'_liq', liq, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 3) phase
		cph = 	get_data(year,month,day,file=filename[0],data='cph_'+ch+'day', satellite=sat, level=lev, verbose = verbose,$
				algo=alg,dirname=dirname,silent=silent,no_data_value=no_data_valuec,found=found)
		if not found then return,-1

		; cwp = lwp * cph + iwp* (1-cph)
		no_idx_ice = where((ice eq no_data_valuei[0] and liq eq no_data_value[0]) or cph eq no_data_valuec[0],cnt_il)
		outdata = ( (temporary(ice) > 0.) * (1. - (cph)) ) + ( (temporary(liq) > 0.) * (temporary(cph)) )
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		if stregex(dat,'cot',/fold,/bool) then longname = ch+'monthly mean cloud optical thickness'
		if stregex(dat,'cer',/fold,/bool) then longname = ch+'monthly mean cloud effective radius'
	endif else if ( ( ( total(alg eq ['claas','clara2']) and total(dat eq ['cot','ref','cot_error','ref_error']) )  $
			  or ( is_gewex and total(dat eq ['cer','ref']) ) ) and (lev eq 'l3c' or lev eq 'l3s') ) then begin
		if ~sil then print,'Calculating '+dat+' for '+alg+' with: ice*(1.-cph_day)+liq*cph_day'
		err = stregex(dat,'_error',/bool) ? '_error' : ''
		; 1) iwp
		dumdat = get_product_name(strmid(dat,0,3)+'_ice'+err,algo=alg,level=lev)
		ice_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		if ~sil then print,'ice_file: ',dumdat,': ',ice_file
		read_data, ice_file[0], dumdat, ice, no_data_valuei, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 2) lwp
		dumdat = get_product_name(strmid(dat,0,3)+'_liq'+err,algo=alg,level=lev)
		liq_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		if ~sil then print,'liq_file: ',dumdat,': ',liq_file
		read_data, liq_file[0], dumdat, liq, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		verbose = verbose, found = found, silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; 3) cph
		cph_file = get_filename(year,month,day,data=(is_gewex ? 'cph_day':'cph'), satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		dumdat = get_product_name('cph_day',algo=alg,level=lev)
		if ~sil then print,'cph_file: ',dumdat,': ',cph_file
		read_data, cph_file[0], dumdat, cph, no_data_valuec, verbose = verbose, found = found, set_fillvalue = set_fillvalue,$
		silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; cwp = lwp * cph + iwp* (1-cph)
		no_idx_ice = where((ice eq no_data_valuei[0] and liq eq no_data_value[0]) or cph eq no_data_valuec[0],cnt_il)
		if (total(alg eq ['claas','clara2']) or is_gewex) then cph = cph/100.
		outdata = ( (temporary(ice) > 0.) * (1. - (cph)) ) + ( (temporary(liq) > 0.) * (temporary(cph)) )
		if cnt_il gt 0 then outdata[no_idx_ice] = no_data_value[0]
		if dat eq 'cot' then longname = 'monthly mean cloud optical thickness'
		if dat eq 'ref' then longname = 'monthly mean cloud effective radius'
		if dat eq 'cer' then longname = 'monthly mean cloud effective radius'
		if ~sil then print,''
	endif else if is_hdf5(filename) and (sat eq 'msg' or alg eq 'claas') then begin
		outdata = read_cmsaf_seviri(filename[0], dat, fillvalue = no_data_value, longname = longname, found = found, $
			  minvalue = minvalue, maxvalue = maxvalue, unit = unit, verbose = verbose)
	endif else if strmid(dat,0,8) eq 'tempdiff' and strmid(alg,0,6) eq 'esacci' and (lev eq 'l2' or lev eq 'l3u' or lev eq 'l3ue') then begin
		; level2 only
		node   = strmid(lev,0,3) eq 'l3u' ? '_'+(reverse(strsplit(dat,'_',/ext)))[0] : ''
		read_data, filename[0],'stemp'+node, stemp, no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then begin
			ok=dialog_message('get_data: Stemp not found! Make sure file is an ESA CCI Cloud l2 file.')
			return,-1
		endif
		read_data, filename[0] , 'ctt'+node, ctt, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		idx  = where(stemp eq no_data_value1[0] or ctt eq no_data_value[0],idx_cnt)
		outdata = temporary(stemp)-temporary(ctt)
		if idx_cnt gt 0 then outdata[idx]=no_data_value[0]
		minvalue = -10.
		maxvalue =  50.
		longname = 'Difference of Surface Temperature and Cloud Top Temperature'
	endif else if (strmid(dat,0,8) eq 'sunglint' or strmid(dat,0,11) eq 'glint_angle') and (lev ne 'l3c' and lev ne 'l3s') then begin
		node   = strmid(lev,0,3) eq 'l3u' ? (reverse(strsplit(dat,'_',/ext)))[0] : ''
		if (strmid(lev,0,3) eq 'l3u') and (node ne 'asc') and (node ne 'desc') then begin
			if ~keyword_set(silent) then ok=dialog_message('get_data: Satellite Node not defined or wrong! Append "_asc" or "_desc" to data name.')
			return,-1
		endif
		dumdat = strmid(lev,0,3) eq 'l3u' ? get_product_name('solzen_'+node,algo=alg) : 'solar_zenith_view_no1'
		read_data, filename[0],dumdat, solza, no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then begin
			if ~keyword_set(silent) then ok=dialog_message('get_data: Solar Zenith angle not found! Make sure file is an ESACCI l2/l3u (clara2[l2b]) file.')
			return,-1
		endif
		dumdat = strmid(lev,0,3) eq 'l3u' ? get_product_name('satzen_'+node,algo=alg) : 'satellite_zenith_view_no1'
		read_data, filename[0],dumdat, satza, no_data_value2, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then begin
			if ~keyword_set(silent) then ok=dialog_message('get_data: Sensor Zenith angle not found! Make sure file is an ESACCI l2/l3u (clara2[l2b]) file.')
			return,-1
		endif
		dumdat = strmid(lev,0,3) eq 'l3u' ? get_product_name('relazi_'+node,algo=alg) : 'rel_azimuth_view_no1'
		read_data, filename[0],dumdat, relazi, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then begin
			if ~keyword_set(silent) then ok=dialog_message('get_data: Relative Azimuth Angle not found! Make sure file is an ESACCI l2/l3u (clara2[l2b]) file.')
			return,-1
		endif
		;PATMOS sunglint calculation:
		idx = where(solza eq  no_data_value1[0] or satza eq no_data_value2[0] or relazi eq no_data_value[0],idx_cnt)
		if alg eq 'clara2' then relazi = 180 - relazi
		glint_angle = acosd( (-1.0 > ( cosd(solza) * cosd(satza) + sind(temporary(solza)) * sind(temporary(satza)) * cosd(temporary(relazi)) < 1.0 )))
		;sun glint = glint angle lt. 40 degrees
		outdata = strmid(dat,0,8) eq 'sunglint' ? float(temporary(glint_angle) lt 40) : temporary(glint_angle)
		if idx_cnt gt 0 then outdata[idx] = no_data_value[0]
		longname = strmid(dat,0,8) eq 'sunglint' ? 'Sunglint Flag (glint_angle < 40 degrees)' : 'glint angle'
		minvalue = 0.
		maxvalue = strmid(dat,0,8) eq 'sunglint' ? 1.: 180.
		unit     = strmid(dat,0,8) eq 'sunglint' ? '': ' [degrees]'
		if strmid(dat,0,8) eq 'sunglint' then flag_meanings =  ['No','Yes']
	endif else if (total(alg eq ['clara']) and (is_jch(dat,/combined) or is_jch(dat,/ratio)) ) then begin
		read_data, filename[0] , 'jch_liq', liq, no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		read_data, filename[0] , 'jch_ice', ice, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		if is_jch(dat,/ratio) then begin
			si = size(ice,/dim)
			outdata = fltarr([si,2])
			outdata[*,*,*,*,0] = liq
			outdata[*,*,*,*,1] = ice
		endif else begin
			idx  = where(ice eq no_data_value[0] and liq eq no_data_value1[0],idx_cnt)
			outdata = (temporary(ice)>0)+(temporary(liq)>0)
			if idx_cnt gt 0 then outdata[idx]=no_data_value[0]
		endelse
		longname = 'Joint cloud property Histogram of ice and water clouds'
	endif else if (total(alg eq ['patmos']) and (is_jch(dat,/combined) or is_jch(dat,/ratio)) ) then begin
		if ~sil then print,'Combine '+dat+' for '+alg
		liq_file = get_filename(year,month,day,data='h_codw_cp', satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		if ~sil then print,'liq_file: h_codw_cp: ',liq_file
		read_data, liq_file[0] , 'h_codw_cp', liq, no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		ice_file = get_filename(year,month,day,data='h_codi_cp', satellite=sat, level=lev,algo=alg,found=found,instrument=instrument,silent=silent,dirname=dirname)
		if not found then return,-1
		if ~sil then print,'ice_file: h_codi_cp: ',ice_file
		read_data, ice_file[0] , 'h_codi_cp', ice, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		if is_jch(dat,/ratio) then begin
			si = size(ice,/dim)
			outdata = fltarr([si,2])
			outdata[*,*,*,*,*,0] = liq
			outdata[*,*,*,*,*,1] = ice
		endif else begin
			idx  = where(ice eq no_data_value[0] and liq eq no_data_value1[0],idx_cnt)
			outdata = (temporary(ice)>0)+(temporary(liq)>0)
			if idx_cnt gt 0 then outdata[idx]=no_data_value[0]
		endelse
		longname = 'Joint cloud property Histogram of ice and water clouds'
	endif else if (total(alg eq ['coll5','coll6']) and  is_jch(dat,/ratio)) then begin
		if ~sil then print,'Combine '+dat+' for '+alg
		read_data, filename[0] , 'cot_ctp_hist2d_liq', liq, no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
				unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		read_data, filename[0] , 'cot_ctp_hist2d_ice', ice, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue ,$
				unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if found and found1 then begin
			si  = size(ice,/dim)
			si1 = size(liq,/dim)
			;bin dimension needs to be equal
			if total(si) eq total(si1) then begin
				si = size(ice,/dim)
				outdata = fltarr([si,2])
				outdata[*,*,*,*,*,0] = liq
				outdata[*,*,*,*,*,1] = ice
			endif else begin
				if ~keyword_set(silent) then print,'Dimensions of liquid and ice do not agree.'
				if ~keyword_set(silent) then print,si
				if ~keyword_set(silent) then print,si1
				found   =  0
				outdata = -1
			endelse
		endif else begin
			outdata = -1
			found=0
		endelse
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) and (is_jch(dat,/combined) or is_jch(dat,/ratio))) then begin
		read_data, filename[0], 'hist2d_cot_ctp', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose, var_dim_names=var_dim_names , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		; total ist "total" langsam ca. 0.95 sek im vergleich zu 0.15 sek!!!
;   		outdata  = total(outdata>0,5)
		if alg eq 'calipso' then begin
			if is_jch(dat,/combined) then begin
				idx  = where(outdata[*,*,*,*,0] eq no_data_value[0] and $
				outdata[*,*,*,*,1] eq no_data_value[0] and outdata[*,*,*,*,2] eq no_data_value[0],idx_cnt)
				outdata = reform((outdata[*,*,*,*,0]>0)+(outdata[*,*,*,*,1]>0)+(outdata[*,*,*,*,2]>0))
				if idx_cnt gt 0 then outdata[idx] = no_data_value[0]
			endif else begin
				outdata = outdata[*,*,*,*,0:1]
			endelse
		endif else begin
			if is_jch(dat,/combined) then begin
				idx  = where(outdata[*,*,*,*,0] eq no_data_value[0] and outdata[*,*,*,*,1] eq no_data_value[0],idx_cnt)
				outdata = reform((outdata[*,*,*,*,0]>0)+(outdata[*,*,*,*,1]>0))
				if idx_cnt gt 0 then outdata[idx] = no_data_value[0]
			endif
		endelse
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) and is_jch(dat,/liquid)) then begin
		read_data, filename[0] , 'hist2d_cot_ctp', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,*,*,0])
		longname = longname+' liquid only'
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) and is_jch(dat,/ice)) then begin
		read_data, filename[0] , 'hist2d_cot_ctp', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,*,*,1])
		longname = longname+' ice only'
	endif else if (total(alg eq ['calipso']) and is_jch(dat,/mixed)) then begin
		read_data, filename[0] , 'hist2d_cot_ctp', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,*,*,2])
		longname = longname+' mixed / unknown'
	endif else if ((alg eq 'esacci_old') and is_jch(dat,/combined)) then begin
		read_data, filename[0] , 'cot_ctp_hist2d', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata = reform(outdata[*,*,*,*,0])
	endif else if ((alg eq 'esacci_old') and is_jch(dat,/liquid)) then begin
		read_data, filename[0] , 'cot_ctp_hist2d', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata = reform(outdata[*,*,*,*,1])
		longname = longname+' liquid only'
	endif else if ((alg eq 'esacci_old') and is_jch(dat,/ice)) then begin
		read_data, filename[0] , 'cot_ctp_hist2d', outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata = (reform(outdata[*,*,*,*,0] - outdata[*,*,*,*,1]))
		longname = longname+' ice only'
	endif else if is_gewex and is_h1d(dat,/combined) then begin
		if (stregex(dat,'cwp',/fold,/bool) or stregex(dat,'cer',/fold,/bool)) then begin
			dumdat   = get_product_name(dat+'_liq',algo='gwx')
			liq_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,$
						found=foundl,instrument=instrument,silent=silent,dirname=dirname)
			if foundl then read_data, liq_file[0] , dumdat, liq, no_data_value, minvalue, maxvalue, longname1, $
						  set_fillvalue = set_fillvalue,unit, found = found, verbose = verbose , silent=silent, $
						  count = count, offset = offset, stride = stride
			dumdat   = get_product_name(dat+'_ice',algo='gwx')
			ice_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,$
						found=foundi,instrument=instrument,silent=silent,dirname=dirname)
			if foundi then read_data, ice_file[0] , dumdat, ice, no_data_valuei, minvalue, maxvalue, longname, $
						  set_fillvalue = set_fillvalue, unit, found = found1, verbose = verbose , silent=silent,$
						  count = count, offset = offset, stride = stride
			if found and found1 then begin
				si  = size(ice,/dim)
				si1 = size(liq,/dim)
				;bin dimension needs to be equal
				if total(si) eq total(si1) then begin
					idx = where(liq eq no_data_value[0] and ice eq no_data_valuei[0],idxcnt)
					outdata = (liq>0) + (ice>0)
					if idxcnt gt 0 then outdata[idx] = no_data_value[0]
				endif else begin
					if ~keyword_set(silent) then print,'Dimensions of liquid and ice do not agree.'
					if ~keyword_set(silent) then print,si
					if ~keyword_set(silent) then print,si1
					found   =  0
					outdata = -1
				endelse
			endif else begin
				found   =  0
				outdata = -1
			endelse
		endif else begin
			read_data, filename[0], dat, outdata, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
			flag_meanings, verbose = verbose, raw=raw,found = found, algo =alg, silent=silent,var_dim_names=var_dim_names,$
			count = count, offset = offset, stride = stride
		endelse
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','coll5','coll6','era-i','era-i2','calipso','hector']) and is_h1d(dat,/combined)) then begin
		if total(alg eq ['coll5','coll6']) and (stregex(dat,'cot',/fold,/bool) or stregex(dat,'ref',/fold,/bool) or $
			stregex(dat,'cwp',/fold,/bool) or stregex(dat,'cer',/fold,/bool) ) then begin
			read_data, filename[0] , dat+'_liq', ice, no_data_valuei, minvalue, maxvalue, longname1, set_fillvalue = set_fillvalue,$
			unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			read_data, filename[0] , dat+'_ice', liq, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
			unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			if found1 and found then begin
				outdata = (ice>0) + (liq>0) 
				idx  = where(ice eq no_data_valuei[0] and liq eq no_data_value[0],idxcnt)
				if idxcnt gt 0 then outdata[idx] = no_data_value[0]
			endif else begin 
				found = 0 
				outdata=-1
			endelse
			longname = 'Ice + ' + longname1
		endif else begin
			if alg eq 'coll6' then begin
				; is actually not the same!!! daytime only
				read_data, filename[0],dat, outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
				unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
				if not found then return,-1
				if stregex(dat,'ctt',/fold,/bool) then begin
					liq  = reform(outdata[*,*,0,*])
					ice  = reform(outdata[*,*,1,*])
					idx  = where(liq eq no_data_value[0] and ice eq no_data_value[0],idxcnt)
					outdata = (liq > 0) + (ice > 0)
					if idxcnt gt 0 then outdata[idx] = no_data_value[0]
					longname = 'Cloud Top Temperature (Day) liquid + ice'
				endif
			endif else begin
				read_data, filename[0] , dat, outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
				unit,var_dim_names=var_dim_names, $
				found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
				if not found then return,-1
				if ~total(alg eq ['coll5']) then begin
					if alg eq 'calipso' then begin
						idx = where(outdata[*,*,*,0] eq no_data_value[0] and outdata[*,*,*,1] eq no_data_value[0] and $
						outdata[*,*,*,2] eq no_data_value[0],idxcnt)
						outdata = reform((outdata[*,*,*,0]>0)+(outdata[*,*,*,1]>0)+(outdata[*,*,*,2]>0))
						if idxcnt gt 0 then outdata[idx] = no_data_value[0]
					endif else begin
						idx = where(outdata[*,*,*,0] eq no_data_value[0] and outdata[*,*,*,1] eq no_data_value[0],idxcnt)
						outdata = reform((outdata[*,*,*,0]>0)+(outdata[*,*,*,1]>0))
						if idxcnt gt 0 then outdata[idx] = no_data_value[0]
					endelse
				endif
			endelse
		endelse
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) and is_h1d(dat,/liquid) ) then begin
		read_data, filename[0] , strreplace(dat,'_liq',''), outdata, no_data_value, minvalue, maxvalue, set_fillvalue = set_fillvalue,$
		longname, unit,var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent,$
		count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata = reform(outdata[*,*,*,0])
		longname = longname+' liquid only'
	endif else if (total(alg eq ['coll5']) and is_h1d(dat,/liquid) and stregex(dat,'ctt',/fold,/bool)) then begin
		; only for coll5 not for coll6 anymore
		read_data, filename[0] , 'HIST2D_CTT_CPH_DAY'  , day, no_data_valued, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		read_data, filename[0] , 'HIST2D_CTT_CPH_NIGHT', nig, no_data_value , minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if found and found1 then begin
			nig = reform(nig[*,*,0,*]) ; [360, 180, 4, 17], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain) 
			day = reform(day[*,*,0,*]) ; [360, 180, 4, 17], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain) 
			outdata = (day>0) + (nig>0)
			idx = where(day eq no_data_valued[0] and nig eq no_data_value[0],idxcnt)
			if idxcnt gt 0 then outdata[idx] = no_data_value[0]
			longname = longname+' liquid only'
		endif else begin
			found = 0
			outdata = -1
		endelse
	endif else if (total(alg eq ['coll6']) and is_h1d(dat,/liquid) and stregex(dat,'ctt',/fold,/bool)) then begin
		; ist actually not the same!!! daytime only
		read_data, filename[0],strreplace(dat,'_liq',''), outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,0,*])
		longname = 'Cloud Top Temperature (Day) liquid only'
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) and is_h1d(dat,/ice)) then begin
		read_data, filename[0] , strreplace(dat,'_ice',''), outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, $
		found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,*,1])
		longname = longname+' ice only'
	endif else if (total(alg eq ['calipso']) and is_h1d(dat,/mixed)) then begin
		read_data, filename[0] , strreplace(dat,'_mixed',''), outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit,var_dim_names=var_dim_names, $
		found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,*,2])
		longname = longname+' mixed / unknown'
	endif else if (total(alg eq ['coll5']) and is_h1d(dat,/ice) and stregex(dat,'ctt',/fold,/bool)) then begin
		; only for coll5 not for coll6 anymore
		read_data, filename[0] , 'HIST2D_CTT_CPH_DAY'  , day, no_data_valued, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		read_data, filename[0] , 'HIST2D_CTT_CPH_NIGHT', nig, no_data_value , minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
		if found and found1 then begin
			nig = reform(nig[*,*,1,*]) ; [360, 180, 4, 17], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain)
			day = reform(day[*,*,1,*]) ; [360, 180, 4, 17], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain)
			outdata = (day>0) + (nig>0)
			idx = where(day eq no_data_valued[0] and nig eq no_data_value[0],idxcnt)
			if idxcnt gt 0 then outdata[idx] = no_data_value[0]
			longname = longname+' ice only'
		endif else begin
			found = 0
			outdata = -1
		endelse
	endif else if (total(alg eq ['coll6']) and is_h1d(dat,/ice) and stregex(dat,'ctt',/fold,/bool)) then begin
		; ist actually not the same!!! daytime only
		read_data, filename[0],strreplace(dat,'_ice',''), outdata, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
		unit, found = found, verbose = verbose , count = count, offset = offset, stride = stride
		if not found then return,-1
		outdata  = reform(outdata[*,*,1,*])
		longname = 'Cloud Top Temperature (Day) ice only'
	endif else if (total(alg eq ['clara2','claas','esacci','esacciv3','coll5','coll6','era-i','era-i2','calipso','patmos']) and is_h1d(dat,/ratio)) then begin
		if total(alg eq ['clara2','claas','esacci','esacciv3','era-i','era-i2','calipso']) then begin
			read_data, filename[0] , strreplace(dat,'_ratio',''), outdata, no_data_value, set_fillvalue = set_fillvalue,$
			minvalue, maxvalue, longname, unit, var_dim_names=var_dim_names, found = found, verbose = verbose , silent=silent,$
			count = count, offset = offset, stride = stride
			if alg eq 'calipso' then outdata = outdata[*,*,*,0:1]
			if not found then return,-1
		endif else if total(alg eq ['coll5','coll6','patmos']) and (stregex(dat,'cot',/fold,/bool) or stregex(dat,'ref',/fold,/bool) or $
				stregex(dat,'cwp',/fold,/bool) or stregex(dat,'cer',/fold,/bool) ) then begin
			dumdat   = get_product_name(strreplace(dat,'_ratio','')+'_liq',algo=(is_gewex ? 'gwx' : alg))
			liq_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,$
				found=foundl,instrument=instrument,silent=silent,dirname=dirname)
			if foundl then read_data, liq_file[0] , dumdat, liq, no_data_value, minvalue, maxvalue, longname1, $
						  set_fillvalue = set_fillvalue,unit, found = found, verbose = verbose , silent=silent,$
						  count = count, offset = offset, stride = stride
			dumdat   = get_product_name(strreplace(dat,'_ratio','')+'_ice',algo=(is_gewex ? 'gwx' : alg))
			ice_file = get_filename(year,month,day,data=dumdat, satellite=sat, level=lev,algo=alg,$
				found=foundi,instrument=instrument,silent=silent,dirname=dirname)
			if foundi then read_data, ice_file[0] , dumdat, ice, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
						  unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			if found and found1 and size(ice,/n_dim) eq 3 then begin
				si  = size(ice,/dim)
				si1 = size(liq,/dim)
				;bin dimension needs to be equal
				if total(si) eq total(si1) then begin
					outdata = fltarr([si,2])
					outdata[*,*,*,0] = liq
					outdata[*,*,*,1] = ice
				endif else begin
					if ~keyword_set(silent) then print,'Dimensions of liquid and ice do not agree.'
					if ~keyword_set(silent) then print,si
					if ~keyword_set(silent) then print,si1
					found   =  0
					outdata = -1
				endelse
			endif else if found and found1 and is_gewex then begin
				si = size(ice,/dim)
				outdata = fltarr([si[0:2],2,si[3]])
				outdata[*,*,*,0,*] = liq
				outdata[*,*,*,1,*] = ice
			endif else begin
				found   =  0
				outdata = -1
			endelse
		endif else if total(alg eq ['coll5']) and (stregex(dat,'ctt',/fold,/bool)) then begin
			; only for coll5 not for coll6 anymore
			read_data, filename[0] , 'HIST2D_CTT_CPH_DAY'  , day, no_data_valued, minvalue, maxvalue, set_fillvalue = set_fillvalue,$
			longname, unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			read_data, filename[0] , 'HIST2D_CTT_CPH_NIGHT', nig, no_data_value , minvalue, maxvalue, set_fillvalue = set_fillvalue,$
			longname, unit, found = found1, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			if found and found1 then begin
				nig = (transpose(nig,[0,1,3,2]))[*,*,*,0:1] ; [360, 180, 4, 17] -> [360, 180, 17, 2], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain) 
				day = (transpose(day,[0,1,3,2]))[*,*,*,0:1] ; [360, 180, 4, 17] -> [360, 180, 17, 2], cph bin=4 (0:water,1:ice,2:mixed,3:uncertain)
				outdata = (day>0) + (nig>0)
				idx = where(day eq no_data_valued[0] and nig eq no_data_value[0],idxcnt)
				if idxcnt gt 0 then outdata[idx] = no_data_value[0]
			endif else begin
				found = 0
				outdata = -1
			endelse
		endif else if total(alg eq ['coll6']) and (stregex(dat,'ctt',/fold,/bool)) then begin
			; ist actually not the same!!! daytime only
			read_data, filename[0],strreplace(dat,'_ratio',''), outdata, no_data_value, minvalue, maxvalue, set_fillvalue = set_fillvalue,$
			longname, unit, found = found, verbose = verbose , silent=silent, count = count, offset = offset, stride = stride
			if not found then return,-1
			outdata  = (transpose(outdata,[0,1,3,2]))[*,*,*,0:1]
			longname = 'Cloud Top Temperature (Day)'
		endif else if total(alg eq ['patmos']) and (stregex(dat,'ctt',/fold,/bool)) then begin
			liq_file = get_filename(year,month,day,data='h_ctw', satellite=sat, level=lev,algo=alg,$
				found=foundl,instrument=instrument,silent=silent,dirname=dirname)
			if foundl then read_data, liq_file[0] , 'h_ctw', liq, no_data_value, minvalue, maxvalue, longname1, $
						  set_fillvalue = set_fillvalue,unit, found = found, verbose = verbose , silent=silent, $
						  count = count, offset = offset, stride = stride
			ice_file = get_filename(year,month,day,data='h_cti', satellite=sat, level=lev,algo=alg,$
				found=foundi,instrument=instrument,silent=silent,dirname=dirname)
			if foundi then read_data, ice_file[0] , 'h_cti', ice, no_data_value, minvalue, maxvalue, longname, $
						  set_fillvalue = set_fillvalue,unit, found = found1, verbose = verbose , silent=silent, $
						  count = count, offset = offset, stride = stride
			if found and found1 then begin
				si = size(ice,/dim)
				outdata = fltarr([si[0:2],2,si[3]])
				outdata[*,*,*,0,*] = liq
				outdata[*,*,*,1,*] = ice
			endif else begin
				found = 0
				outdata = -1
			endelse
		endif else begin
			found = 0
			outdata = -1
		endelse
		if found then longname = longname+' liquid fraction'
	endif else if ((alg eq 'esacci' or alg eq 'esacciv3' or strmid(alg,0,6) eq 'patmos') and keyword_set(join_nodes) and lev eq 'l3u') then begin
		files  = filename
		dumdat = (alg eq 'esacci' or alg eq 'esacciv3') and stregex(dat,'cc_total',/bool,/fold) ? 'cc_mask' : dat
		dumdat = strreplace(dumdat,'_asc','')
		dumdat = strreplace(dumdat,'_desc','')
		if strmid(alg,0,6) eq 'patmos' then begin
			if n_elements(filename) ne 2 then begin
				files = stregex(files[0],'asc',/bool,/fold) ? [files[0],strreplace(filename[0],'asc','des')] : [files[0],strreplace(filename[0],'des','asc')]
			endif
			read_data, files[0], dumdat, bild_asc , no_data_value1, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
			unit, verbose = verbose, raw=raw,found = found1, algo =alg, silent=silent, count = count, offset = offset, stride = stride
			read_data, files[1], dumdat, bild_desc, no_data_value, minvalue, maxvalue, longname, set_fillvalue = set_fillvalue,$
			unit, verbose = verbose, raw=raw,found = found2, algo =alg, silent=silent, count = count, offset = offset, stride = stride
		endif else begin
			files = [filename[0],filename[0]] 
			read_data, files[0], dumdat+'_asc' , bild_asc , no_data_value1, minvalue, maxvalue, set_fillvalue = set_fillvalue,$
			longname, unit, verbose = verbose, raw=raw,found = found1, algo =alg, silent=silent, count = count, offset = offset, stride = stride
			read_data, files[1], dumdat+'_desc', bild_desc, no_data_value, minvalue, maxvalue, set_fillvalue = set_fillvalue,$
			longname, unit, verbose = verbose, raw=raw,found = found2, algo =alg, silent=silent, count = count, offset = offset, stride = stride
		endelse
		if found1 and found2 then begin
			if keyword_set(verbose) then print,'Ascending and Descending are now joined together.'
			found = 1
			idx = where(bild_asc eq no_data_value1[0] and bild_desc eq no_data_value[0],idxcnt)
			dum = where(bild_asc eq no_data_value1[0],cnt)
			if cnt gt 0 then bild_asc[dum] = !values.f_nan
			dum = where(bild_desc eq no_data_value[0],cnt)
			if cnt gt 0 then bild_desc[dum] = !values.f_nan
			dummy = [[[temporary(bild_asc)]],[[temporary(bild_desc)]]]
			outdata = mean(dummy,dimension=3,/nan)
			if idxcnt gt 0 then outdata[idx] = no_data_value[0]
		endif else found = 0
	endif else if total(dat eq ['refl3b_asc','refl3b_desc']) and (lev eq 'l3u') then begin
		nd = stregex(dat,'asc',/fold,/bool) ? '_asc' : '_desc'
		read_data, filename[0], 'bt_nir037'+nd, rad3b, no_data_value3b, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		flag_meanings, verbose = verbose, raw=raw,found = found, algo =alg, silent=silent,var_dim_names=var_dim_names,$
		count = count, offset = offset, stride = stride
if found then begin
		read_data, filename[0], 'bt_tir108'+nd, rad4, no_data_value4, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		flag_meanings, verbose = verbose, raw=raw,found = found, algo =alg, silent=silent,var_dim_names=var_dim_names,$
		count = count, offset = offset, stride = stride
endif
if found then begin
		ang_file = strreplace(filename,'CAC','CAA')
		read_data, ang_file[0], 'sunzen'+nd, sunza, no_data_values, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		flag_meanings, verbose = verbose, raw=raw,found = found, algo =alg, silent=silent,var_dim_names=var_dim_names,$
		count = count, offset = offset, stride = stride
		noaasat = noaa_primes(year,month,ampm=sat_ampm(sat,/ampm),which=which,/no_zero,found=found)
; 		idx = where(rad3b eq no_data_value3b[0] or rad4 eq no_data_value4[0] or sunza eq no_data_values[0],idxcnt)
		idx = where(rad3b eq no_data_value3b[0] or rad4 eq no_data_value4[0] or ~between(sunza,0.,80.),idxcnt)
		no_data_value = -999.
		if idxcnt gt 0 then begin
			rad3b[idx] = no_data_value[0]
			rad4 [idx] = no_data_value[0]
			sunza[idx] = no_data_value[0]
		endif
		outdata = bt37_to_ref37( doy(year,month,day), rad3b, rad4, sunza, noaasat,no_data_val=no_data_value)
		idx = where(outdata eq -999.,idxcnt)
		outdata *= 100.
		if idxcnt gt 0 then outdata[idx] = no_data_value[0]
endif
		minvalue = 0
		maxvalue =100
		longname = textoidl('Reflectance at 3.7 \mum , ')+(stregex(dat,'asc',/fold,/bool) ? 'ascending' : 'descending' )
	endif else begin
		if size(filename,/type) eq 0 then begin
			found = 0
			return,-1
		endif
		read_data, filename[0], dat, outdata, no_data_value, minvalue, maxvalue, longname, unit, set_fillvalue = set_fillvalue,$
		flag_meanings, verbose = verbose, raw=raw,found = found, algo =alg, silent=silent,var_dim_names=var_dim_names,$
		count = count, offset = offset, stride = stride
		; specials for testing etc, remove if not needed anymore
		if keyword_set(error) then begin
			if strmid(alg,0,6) eq 'esacci' and strmid(dat,0,5) eq 'cccot' and lev eq 'l3u' then begin
				make_geo,lon,lat,grid=0.1
				idx            = where(outdata ne no_data_value[0],complement=nd_idx)
				print,'GMean Original CCCOT   Full:', gmean(outdata[idx],lat[idx])
				node = stregex(dat,'desc',/bool,/fold) ? '_desc' : '_asc'
				glint = get_data(year,month,day,file=filename[0], data='glint_angle'+node,no_data_value=ndv_glint,algo=alg,level=lev,found=found_glint,/silent)
				if found_glint then begin
					dum = (( 1-cosd(glint)/cosd(50) ) < 0 )*(.3)
					print,'Reduce cccot by min/max: ',minmax(dum)
					exp1           = 0. > (outdata + dum) < 1.
					exp1[nd_idx]   = no_data_value[0]
					outdata        = exp1
				endif
			endif
		endif
		;---------------------------------
	endelse

	if not found then return,-1

	; make sure no_data_value is not a 1-element vector ; causes strange behaviours
	if is_defined(no_data_value) then no_data_value = no_data_value[0]
	if is_defined(minvalue) then minvalue = minvalue[0]
	if is_defined(maxvalue) then maxvalue = maxvalue[0]

	if alg eq 'claas' and (is_jch(dat) or is_h1d(dat)) then begin

		;regrid 0.05 histograms to 0.25 (same as JCH)
		; noone needs this high resolution
 		x=systime(1)
; 		if is_h1d(dat) then begin
; 			si = size(outdata,/dim)
; 			si[0:1]=[720,720]
; 			stop
; 			tic & outdata1 = (rebin(float(outdata),si) * 25.) & toc
; 			print,'get_data::CLAAS Histos: Regridding Histo1d from 0.05 to 0.25 took [seconds]:', systime(1)-x
; 			x=systime(1)
; 		endif
		; CLAAS histos dont have fillvalues and have rectangle areas
		; we read claas aux data satza at 0 degree SSP to get the mask we need
		; the files are preprocessed sav files to make it faster
		; fillvalues are defined where sza[*,*,1] gt 79.5
		if is_jch(dat) then sav_file = !SAVS_DIR + 'CLAAS_AUX/claas2_level3_field_of_view_aux_data_025deg.sav'
		if is_h1d(dat) then sav_file = !SAVS_DIR + 'CLAAS_AUX/claas2_level3_field_of_view_aux_data_005deg.sav'
		fov = restore_var(sav_file,found=ffov)
		if ffov then begin
			si = size(outdata,/dim)
			outdata = (outdata * rebin(fov,si)) + ( rebin(fov,si) eq 0) * (no_data_value)
			print,'get_data::CLAAS Histos: Including Fillvalues based on SatZA >80° took [seconds]:', systime(1)-x
			x=systime(1)
		endif
		help,outdata
		;bring claas to regular grid
		si  = size(outdata)
		if si[0] eq 3 then begin
			dum_cla = make_array(si[1]*2,si[2],si[3],type=size(outdata,/type),value=no_data_value) 
			dum_cla[(si[1]/2-1):(si[1]/2-1)+si[1]-1,*,*] = outdata
		endif else if si[0] eq 4 then begin
			dum_cla = make_array(si[1]*2,si[2],si[3],si[4],type=size(outdata,/type),value=no_data_value) 
			dum_cla[(si[1]/2-1):(si[1]/2-1)+si[1]-1,*,*,*] = outdata
		endif else if si[0] eq 5 then begin
			dum_cla = make_array(si[1]*2,si[2],si[3],si[4],si[5],type=size(outdata,/type),value=no_data_value)
			dum_cla[(si[1]/2-1):(si[1]/2-1)+si[1]-1,*,*,*,*] = outdata
		endif
		outdata = dum_cla
		help,outdata
		print,'get_data::CLAAS Histos: Conversion to global grid took [seconds]:', systime(1)-x
	endif

	if is_gewex and keyword_set(month) then begin
		case size(outdata,/n_dim) of
			3	: outdata = reform(outdata[*,*,fix(month)-1])
			4	: outdata = reform(outdata[*,*,*,fix(month)-1])
			5	: outdata = reform(outdata[*,*,*,*,fix(month)-1])
			6	: outdata = reform(outdata[*,*,*,*,fix(month)-1,*]) ; patmos cod_cp_ratio last bin = phase
			else	:
		endcase
	endif

	if keyword_set(make_compareable) then begin 
		datd = strlowcase(data)
		is_stdd = (reverse(strsplit(datd,'_',/ext)))[0] eq 'std'
		is_unce = (reverse(strsplit(datd,'_',/ext)))[0] eq 'unc'
		is_erro = (alg eq 'clara2' and (reverse(strsplit(datd,'_',/ext)))[0] eq 'error')
		if is_stdd then datd = strreplace(datd,'_std','')
		if is_unce then datd = strreplace(datd,'_unc','')
		if is_erro then datd = strreplace(datd,'_error','')
		if strmid(alg,0,6) eq 'patmos' and lev eq 'l3u' then begin
			if get_product_name(datd,algo='patmos') eq 'cloud_mask' then begin
				outdata = float(outdata)
				ndidx   = where(outdata eq no_data_value,ndidxcnt)
				; make_compareable =1 ; clear=clear+pob-clear,cloudy=cloudy+prob-cloudy
				; make_compareable =2 ; clear=clear+pob-clear+prob-cloudy,cloudy=cloudy
				; make_compareable =3 ; clear=clear, cloudy=pob-clear+prob-cloudy+cloudy
				if make_compareable eq 3 then make_compareable = 0
				outdata = float(outdata gt float(make_compareable))
				if ndidxcnt gt 0 then outdata[ndidx] = no_data_value
				minvalue = 0.
				maxvalue = 1.
				flag_meanings = flag_meanings[[0,3]]
			endif
			if get_product_name(datd,algo='patmos') eq 'cloud_phase' then begin
				outdata = float(outdata)
				ndidx   = where(outdata eq no_data_value or outdata eq 5.,ndidxcnt) ; fillvalue,unknown -> fillvalue
				idx     = where(outdata eq 2.,idxcnt)
				if idxcnt gt 0 then outdata[idx] = 1. ; supercooled -> water
				idx     = where(between(outdata,3.,4.),idxcnt)
				if idxcnt gt 0 then outdata[idx] = 2. ; mixed,ice -> ice
				if ndidxcnt gt 0 then outdata[ndidx] = no_data_value
				minvalue = 0.
				maxvalue = 2.
				flag_meanings = flag_meanings[[0,1,4]]
			endif
		endif else if total(datd eq ['cloud_fraction','a_ca']) or strmid(datd,0,8) eq 'cc_total' $
				or strmid(datd,0,3) eq 'cfc' or dat eq '8' then begin
			if total(alg eq ['clara2','clara','claas','isccp_old','hector']) then begin
				outdata = float(outdata)
				idx = where(outdata ne no_data_value,idxcnt)
				if idxcnt gt 0 then outdata[idx] /= 100.
				minvalue = 0.
				maxvalue = 1.
				unit = ' '
			endif
		endif else if total(datd eq ['cph','cph_day','a_cawr','a_cawdr']) and total(lev eq ['l3c','l3s']) then begin
			if total(alg eq ['clara2','clara','claas','isccp_old','hector']) or is_gewex then begin
				outdata = float(outdata)
				idx = where(outdata ne no_data_value,idxcnt)
				if idxcnt gt 0 then outdata[idx] /= 100.
				minvalue = 0.
				maxvalue = 1.
				unit = ' '
			endif
		endif else if total(datd eq ['cwp_asc','cwp_desc','cwp_ice','cwp_liq','cwp','29','lwp','a_clwp','iwp','a_ciwp','iwp_allsky','lwp_allsky','cwp_allsky']) then begin
			if total(alg eq ['clara2','clara','claas','hector']) then begin
				outdata = float(outdata)
				idx = where(outdata ne no_data_value,idxcnt)
				if idxcnt gt 0 then outdata[idx] *= 1000.
				if keyword_set(minvalue) then minvalue *= 1000.
				if keyword_set(maxvalue) then maxvalue *= 1000.
				unit = textoidl(' [ g/m^2]')
			endif
		endif else if total(datd eq ['cth_asc','cth_desc','cth','cth_arith_mean','cth_corrected']) then begin
			if total(alg eq ['clara2','clara','claas','hector']) then begin
				outdata = float(outdata)
				idx = where(outdata ne no_data_value,idxcnt)
				if idxcnt gt 0 then outdata[idx] /= 1000.
				if keyword_set(minvalue) then minvalue /= 1000.
				if keyword_set(maxvalue) then maxvalue /= 1000.
				unit = textoidl(' [ km]')
			endif
		endif else if total(datd eq ['ref','ref_liq','ref_ice','cer','cer_liq','cer_ice']) then begin
			if total(alg eq ['clara2','claas','hector']) then begin
				outdata = float(outdata)
				idx = where(outdata ne no_data_value,idxcnt)
				if idxcnt gt 0 then outdata[idx] /= 1.e-06
				if keyword_set(minvalue) then minvalue /= 1.e-06
				if keyword_set(maxvalue) then maxvalue /= 1.e-06
				unit = textoidl(' [ \mum]')
			endif
		endif else if total(datd eq ['scanline_time_asc','scanline_time_desc','time_asc','time_desc']) then begin
			if total(alg eq ['esacci','esacciv3']) then begin
				idx = where(outdata ne no_data_value,idxcnt)
				minzeit = get_ncdf_data_by_name(filename[0],datd,attr='add_offset',found = found_offset)
				if idxcnt gt 0 and not found_offset then minzeit = rnd(min(outdata[idx]),/down)
				if idxcnt gt 0 then outdata[idx] = (outdata[idx] - minzeit)*24d
				if keyword_set(minvalue) then minvalue =  0d
				if keyword_set(maxvalue) then maxvalue = 24d
				unit = textoidl(' [ hours]')
			endif
		endif

		if is_stdd then datd = datd + '_std'
		if size(outdata,/type) ne 5 then begin ; dont do this on double precision data type
			iidx = where(outdata eq no_data_value,iidxcnt)
			no_data_value = -999.
			if iidxcnt gt 0 then begin
				outdata = float(outdata)
				outdata[iidx] = -999.
			endif
		endif
	endif else begin
; 		if strmid(dat,0,12) eq 'cph_extended' and alg eq 'clara2' then begin
; 			if min(outdata[where(outdata ne no_data_value)]) eq 1 then begin
; 				idx = where(outdata eq no_data_value,idxcnt)
; 				outdata -= 1
; 				if idxcnt gt 0 then outdata[idx] = no_data_value
; 			endif
; 		endif
	endelse

	if keyword_set(global_grid) then begin
		si    = size(outdata,/dim)
		if n_elements(si) eq 2 then begin
			gg    = ([360.,180.])/float(si)
			reg   = gg[0] eq gg[1] ; reguläres gitter
			if reg and rebinable(gg[0],global_grid) then begin
				outdata = grid_down_globe(outdata, global_grid, no_data_value = no_data_value, $
				found = found, nan_fillv = nan_fillv,total = keyword_set(sum) , $
				sample = (keyword_set(random_sample) or keyword_set(median)))
			endif else begin
				make_geo,file = filename,lon_dum,lat_dum
				outdata = sat2global(lon_dum,lat_dum,outdata,no_data_value=no_data_value,grid_res=global_grid, nan_fillv = nan_fillv )
				if is_struct(outdata) then begin
					if keyword_set(mean) then if is_tag(outdata,'mean') then outdata = outdata.mean else found = 0
					if keyword_set(median) then if is_tag(outdata,'median') then outdata = outdata.median else found = 0
					if keyword_set(sum) then if is_tag(outdata,'sum') then outdata = outdata.sum else found = 0
					if keyword_set(random_sample) then if is_tag(outdata,'random_sample') then outdata = outdata.random_sample else found = 0
				endif else found = 0
			endelse
		endif else print,'Array needs exactly 2 dimensions! Skip regridding!'
	endif else if adv_keyword_set(no_data_value) and keyword_set(nan_fillv) then begin
		idx = where(outdata eq no_data_value,idxcnt)
		if idxcnt gt 0 then begin
			outdata[idx] = !values.f_nan
			no_data_value = !values.f_nan
		endif
	endif
	if keyword_set(verbose) then print,'Validation_tool_box::get_data : ',systime(1)-x

	;make fillvalue of same datatype as outdata
	no_data_value = (make_array(1,type=size(outdata,/type),value=no_data_value))[0]
	return, outdata
end
;------------------------------------------------------------------------------------------
function temp_mean, array, log = log, random_sample=random_sample, no_data_value = no_data_value
	; das hier stimmt
	ndv = adv_keyword_set(no_data_value) ? no_data_value : -999.

	res_count = total(array gt ndv, 3 ,/NAN)
	if keyword_set(random_sample) then begin
		sample_arr=round(randomu(seed,size(res_count,/dim)))*res_count
		; keine ahnung wie am besten/schnellsten machen
	endif
	idx_miss  = where(res_count EQ 0,cnt_miss)
	result    = total(array*(array gt ndv),3,/NAN) /(res_count>1)
	if keyword_set(log) then result = exp(result-10.)
	if cnt_miss gt 0 then result[idx_miss] = ndv
	return, result
end
;------------------------------------------------------------------------------------------
function make_l3c_from_l3u, year, month, data = data, algo = algo, satellite = satellite, $
			    grid_res = grid_res, l3u_dir = l3u_dir, save_as = save_as	, $
			    arithmetical = arithmetical, in_cloud_mean = in_cloud_mean	, $
			    found = found, verbose = verbose, print_file = print_file	, $
			    nadir_only = nadir_only,set_fillvalue = set_fillvalue

	found   = 1
	yyyy  	= keyword_set(year)  	? string(year,f='(i4.4)')  	: '2007'
	mm 	= keyword_set(month) 	? string(month,f='(i2.2)') 	: '01'
	dom 	= dom(yyyy,mm,/num)
	dat	= keyword_set(data)	? strreplace(data,['_asc','_desc'],['',''])	: 'CTP'
	res   	= keyword_set(grid_res)	? float(grid_res)		: 0.5 ; in degree

	res_l3u = (algo2ref(algo) eq 'pmx' ? 0.1 : 0.05)

	day   = stregex(dat,'_day',/fold,/bool)
	twl   = stregex(dat,'_twl',/fold,/bool)
	night = stregex(dat,'_night',/fold,/bool)
	
	dat = strreplace(dat,['_day','_twl','_night'],['','',''])

	if res/res_l3u ne fix(res/res_l3u) and ~keyword_set(arithmetical) then begin
		print,'L3C output Grid-Size ('+string(res,f='(f5.2)')+') needs to be an integral multiple of the L3U Grid-Size ('+string(res_l3u,f='(f5.2)')+').'
		print,'Change L3C output Grid-Size with keyword /grid_res or try keyword /arithmetical.'
		found=0
		return,-1
	endif

	if get_product_name(dat,algo='gac') eq 'cfc' then dat = 'cmask'

	full_array = fltarr(360./res,180./res,2*dom) + !values.f_nan
	if keyword_set(arithmetical) then begin
		make_geo,lon,lat,grid = res_l3u
		full_count = fltarr(360./res,180./res)
	endif

	o_myt_orb = obj_new('my_timer',dom)
	count = 0
	for i = 0, dom-1 do begin
		dum = get_data(yyyy,mm,(i+1),data = dat+'_asc', no_data_value = no_data_value, longname = longname, unit = unit,/silent	, $
		verbose = verbose, level='l3u', algo = algo, sat=satellite,error=error, found = found,/make_compare, dirname=l3u_dir,print_file=print_file)
		if strlowcase(dat) eq 'cph' then dum = ( no_data_value[0] * (dum eq no_data_value[0] or dum eq 0) ) + (dum * (dum eq 1))
		if found then begin
			; cci prototype had parameter no matter if clear or cloudy , some versions of cci phase2 too
			; this can be always done but of course takes its time ,set keyword in_cloud_mean if necassary
			if strmid(algo2ref(algo),0,3) eq 'cci' and keyword_set(in_cloud_mean) and dat ne 'cmask' then begin	
				cma = get_data(yyyy,mm,(i+1),data = 'cmask_asc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					dum = ( no_data_value[0] * (cma eq ndv_cma[0] or cma eq 0) ) + (dum * (cma eq 1))
					free, cma
				endif
			endif
			if day or twl or night then begin
				cma = get_data(yyyy,mm,(i+1),data = 'illum_asc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					if day   then dum = ( no_data_value[0] * (cma ne 1) ) + (dum * (cma eq 1))
					if twl   then dum = ( no_data_value[0] * (cma ne 2) ) + (dum * (cma eq 2))
					if night then dum = ( no_data_value[0] * (cma ne 3) ) + (dum * (cma eq 3))
					free, cma
				endif
			endif
			if keyword_set(nadir_only) then begin	
				cma = get_data(yyyy,mm,(i+1),data = 'satzen_asc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					dum = ( no_data_value[0] * (cma eq ndv_cma[0] or cma gt 0.5) ) + (dum * (between(cma,0,0.5)))
					free, cma
				endif
			endif
			if keyword_set(arithmetical) then begin
				dum = sat2global(lon,lat,dum,no_data_value=no_data_value,grid=res,/nan_fillv)
				full_array[*,*,(i*2)] = dum.sum
				full_count += dum.count
			endif else begin
				full_array[*,*,(i*2)] = grid_down_globe(dum,res,no_data_value=no_data_value[0],/nan_fillv)
			endelse
			count++
		endif
		dum = get_data(yyyy,mm,(i+1),data = dat+'_desc', no_data_value = no_data_value, longname = longname, unit = unit,/silent	, $
		verbose = verbose, level='l3u', algo = algo, sat=satellite,error=error, found = found,/make_compare, dirname=l3u_dir,print_file=print_file)
		if strlowcase(dat) eq 'cph' then dum = ( no_data_value[0] * (dum eq no_data_value[0] or dum eq 0) ) + (dum * (dum eq 1))
		if found then begin
			if strmid(algo2ref(algo),0,3) eq 'cci' and keyword_set(in_cloud_mean) and dat ne 'cmask' then begin
				cma = get_data(yyyy,mm,(i+1),data = 'cmask_desc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					dum = ( no_data_value[0] * (cma eq ndv_cma[0] or cma eq 0) ) + (dum * (cma eq 1))
					free, cma
				endif
			endif
			if day or twl or night then begin
				cma = get_data(yyyy,mm,(i+1),data = 'illum_desc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					if day   then dum = ( no_data_value[0] * (cma ne 1) ) + (dum * (cma eq 1))
					if twl   then dum = ( no_data_value[0] * (cma ne 2) ) + (dum * (cma eq 2))
					if night then dum = ( no_data_value[0] * (cma ne 3) ) + (dum * (cma eq 3))
					free, cma
				endif
			endif
			if keyword_set(nadir_only) then begin	
				cma = get_data(yyyy,mm,(i+1),data = 'satzen_desc', no_data_value = ndv_cma,/silent,print_file=print_file	, $
				level='l3u', algo = algo, sat=satellite,error=error,found = found,/make_compare, dirname=l3u_dir)
				if found then begin
					dum = ( no_data_value[0] * (cma eq ndv_cma[0] or cma gt 0.5) ) + (dum * (between(cma,0,0.5)))
					free, cma
				endif
			endif
			if keyword_set(arithmetical) then begin
				dum = sat2global(lon,lat,dum,no_data_value=no_data_value[0],grid=res,/nan_fillv)
				full_array[*,*,(i*2+1)] = dum.sum
				full_count += dum.count
			endif else full_array[*,*,(i*2+1)] = grid_down_globe(dum,res,no_data_value=no_data_value[0],/nan_fillv)
			count++
		endif
		o_myt_orb -> wie_lang_noch
	endfor

	if count eq 0 then begin
		print,'Found Nothing. Check satnames, launches, variable names, etc.'
		found = 0
		return,-1
	endif
	
	if keyword_set(arithmetical) then begin
		fsum = total(full_array,3,/nan)
		mean = fsum/float(full_count>1)
	endif else mean = mean(full_array,dim=3,/nan)

	no_data_value = keyword_set(set_fillvalue) ? set_fillvalue[0] : no_data_value[0]

	idx = where(~finite(mean),idxcnt)
	if idxcnt gt 0 then mean[idx] = no_data_value[0]

	if keyword_set(save_as) then begin
		if day   then dat += '_day'
		if twl   then dat += '_twl'
		if night then dat += '_night'
		name = yyyy+mm+'_'+ref2algo(algo)+'_'+dat+'_'+sat+'_l3c_from_l2b.sav'
		save_var,{image:mean,longname:'monthly mean of '+longname,unit:unit,fillvalue:no_data_value[0]},!SAVS_DIR + name
		print,"Saved -> Open with d = restore_var('"+!SAVS_DIR +name+"')"
	endif

	obj_destroy, o_myt_orb

	return, mean
end
;------------------------------------------------------------------------------------------
function make_l3u_from_l2, year, month, day, data = data, algo = algo, satellite = satellite	, $
			    grid_res = grid_res, l2_dir = l2_dir, l2_files = l2_files		, $
			    ascending=ascending, descending=descending, save_as = save_as

	; nicht wirklich l3u -> orbit auf global_grid -> dann alle mitteln 
  
	yyyy  	= keyword_set(year)  	? string(year,f='(i4.4)')  	: '2007'
	mm 	= keyword_set(month) 	? string(month,f='(i2.2)') 	: '01'
	dd 	= keyword_set(day) 	? string(day,f='(i2.2)') 	: '01'
	dat	= keyword_set(data)	? strupcase(data)		: 'CTP'
	satn  	= keyword_set(satellite)? satellite			: 'AQUA'
	alg	= keyword_set(algo)	? strupcase(algo)		: 'ESACCI'
	res	= keyword_set(grid_res)	? grid_res 			: 0.1 ; in degree
	asc	= keyword_set(ascending)
	desc	= keyword_set(descending)
	if (asc+desc) eq 2 then begin asc = 0 & desc = 0 & end

	l2_fil = keyword_set(l2_files) ? l2_files : get_filename(yyyy,mm,dd,sat=satn,algo=alg,level='l2',dirname=l2_dir)
	l2_cnt = total(file_test(l2_fil))
	if l2_cnt eq 0 then return,-1
	day_array = fltarr(360/res,180/res,l2_cnt)-1.

	print,'create '+dat+' l3u from l2:'
	o_myt_orb = obj_new('my_timer',l2_cnt)
	for num = 0, l2_cnt -1 do begin
		dum = get_data(file=l2_fil[num],data=dat,no_data_value=no_data_value,algo=alg,level='l2',found=found)
		if found then begin
			make_geo,lon,lat,file=l2_fil[num]
			if asc or desc then begin
				if asc  then dum[where(sat_node(lon) eq 1)] = no_data_value
				if desc then dum[where(sat_node(lon) eq 0)] = no_data_value
			endif
			dum = sat2global(lon,lat,dum,grid=res,no_data_value=no_data_value, found = found)
			day_array[*,*,num] = found ? dum.random_sample : (fltarr(360/res,180/res) + no_data_value)
		endif
		o_myt_orb -> wie_lang_noch
	endfor

	gwx   = temp_mean(day_array,no_data_value=no_data_value)

	obj_destroy, o_myt_orb

; 	create_sav
	if keyword_set(save_as) then begin
		dir  = '/cmsaf/cmsaf-cld1/sstapelb/savs/l3u_from_l2/'
		name = yyyy+mm+dd+'_'+sat_name(algo,satn)+'_L3U_'+dat+'_from_L2'+(asc ? '_descending':'')+(desc ? '_descending':'')+'.sav'
		save_var,gwx,dir+name
		print,"Saved -> Open with d = restore_var('"+dir+name+"')"
	endif

	return, gwx
end
;------------------------------------------------------------------------------------------
pro set_algolist, algo_list, sat = sat, data = data, default = default, exclude = exclude

	if keyword_set(default) and keyword_set(sat) and keyword_set(data) then begin
		dat = strlowcase(data)
		co5 = (sat eq 'terra' or sat eq 'aqua' or sat eq 'aatme' or sat eq 'aatsr')
		ampm  = sat_ampm(sat)
		; define algo_list
		if ampm eq 'am' then begin
			algo_list = co5 ? ['mod2','cci','cciv3'] : ['pmx','gac2','cci','cciv3','mod2','hec','era','cla']
		endif else begin
			algo_list = co5 ? ['myd2','cci','cciv3'] : ['pmx','gac2','cci','cciv3','myd2','hec','era','cla','cal']
		endelse
	endif else algo_list = ['cci','cciv3','cci_old','pmx','myd','myd2','mod','mod2','gac','gac2','gwx','cla','isp','era','cal','hec']

	; now remove algo and reference from algo_list
	if keyword_set(exclude) then begin
		for i = 0,n_elements(exclude)-1 do begin
			idx = where(algo_list ne algo2ref(exclude[i]),idx_cnt)
			if idx_cnt gt 0 then algo_list = algo_list[idx]
		endfor
	endif
end
;------------------------------------------------------------------------------------------
function get_all_avail_data, year, month, day, orbit = orbit, data = data, level = level	, $
			     timeseries = timeseries, satellite=satellite, algo_list = algo_list, $
			     global_grid=global_grid, make_compareable = make_compareable	, $
			     mean=mean, median=median, verbose=verbose,land=land,sea=sea	, $
			     limit=limit,zonal_mean=zonal_mean,percentile=percentile		, $
			     histograms=histograms,found=found,coverage=coverage		, $
			     antarctic = antarctic, arctic = arctic

	if keyword_set(algo_list) then begin
		alle = algo_list 
	endif else begin
		set_algolist,alle
	endelse

	count = 1
	if ~keyword_set(mean) and ~keyword_set(median) and keyword_set(global_grid) then mean = 1

	if keyword_set(timeseries) then begin
		print,'später'
		stop
	endif else begin
		for i = 0, n_elements(alle)-1 do begin
			dum = get_data( year, month, day, orbit=orbit,data=data,satellite=satellite	, $
					algo=alle[i],level=level,found=found,no_data_value=ndv		, $
					minv=minv,maxv=maxv,longname=lname,unit=unit,/silent,mean=mean	, $
					make_compareable=make_compareable,global_grid=global_grid	, $
					median=median,verbose=verbose)
			if found then begin
				make_geo,lon,lat,grid=get_grid_res(dum[*,*,0,0,0,0]),found=found, algo=alle[i]
				area = 	get_coverage( lon, lat, coverage = coverage,limit = limit, found = found, $
					antarctic=antarctic,arctic=arctic,land=land,sea=sea,index=ndidx,count=ndidx_cnt,/opposite)
				if ndidx_cnt gt 0 then dum[ndidx] = ndv
				algoname = sat_name(alle[i],strmid(alle[i],0,3) eq 'gac' and (strlowcase(satellite) eq 'aatme' or $
					   strlowcase(satellite) eq 'aatsr')?'noaa17':satellite,year=year,month=month,level=level)
				define_oplots, count, cols, spos, linestyle, psym, ystretch
				sub   = {algoname : algoname,  data : dum , no_data_value : ndv, minvalue:minv, $
					 maxvalue:maxv,longname:lname,unit:unit}
				num   = alle[i] eq 'mod' ? 'modi' : alle[i] ; mod wird als tagname nicht akzeptiert! 
				struc = ~is_defined(struc) ? create_struct(num,sub) : create_struct(struc,num,sub)
				;add arrays of name, colors, zonal_means , percentiles, histograms, values min,max
				ref_name = is_defined(ref_name)  ? [ref_name,num] : num
				names  = is_defined(names)  ? [names,algoname]    : algoname
				colors = is_defined(colors) ? [colors,cols]       : cols 
				lspos  = is_defined(lspos)  ? [lspos,spos]        : spos 
				lystr  = is_defined(lystr)  ? [lystr,ystretch]    : ystretch
				lstyle = is_defined(lstyle) ? [lstyle,linestyle]  : linestyle
				if keyword_set(zonal_mean) or keyword_set(percentile) then begin
					if keyword_set(zonal_mean) then begin
						medi = zonal_average(dum,lat,fillvalue=ndv,mean=mean,median=median,/nan,lat_zon=lat1d)
						zon_medis = is_defined(zon_medis)  ? [[zon_medis],[medi]] : medi 
						zon_lats  = is_defined(zon_lats)   ? [[zon_lats],[lat1d]] : lat1d 
					endif
					if keyword_set(percentile) then begin
						perc = percentile(dum,percentile,no_data_value=ndv)
						percents = is_defined(percents)  ? [[percents],[perc]] : perc 
					endif
				endif
				mima    = minmax(dum,no=ndv)
				minimum = is_defined(minimum) ? max([minimum,mima[0]]) : mima[0]
				maximum = is_defined(maximum) ? max([maximum,mima[1]]) : mima[1]
				count++
			endif
		endfor
	endelse

	if is_defined(struc) then begin
		dum = {ref_names:ref_name,algo_names:names,colors:colors,linestyle:lstyle,legend_spos:lspos,legend_ystretch:lystr,minimum:minimum,maximum:maximum}
		if keyword_set(histograms) then begin
			min   = (-1e6 > minimum) ; ansonsten gibts abbruch oder memory allocation fehler
			max   = (maximum < 1e6)  ; ansonsten gibts abbruch oder memory allocation fehler
			for i = 0,n_elements(names) -1 do begin
				hi = histogram(struc.(i).DATA,bin=histograms[0],min=min,max=max)
				hi = hi/total(hi)*100.
				histos = is_defined(histos) ? [[histos],[hi]] : hi
			endfor
			dum = create_struct(dum,{histograms:histos})
		endif
		if keyword_set(zonal_mean) then dum = create_struct(dum,{zonal_mean:zon_medis})
		if keyword_set(zonal_mean) then dum = create_struct(dum,{zonal_lats:zon_lats})
		if keyword_set(percentile) then dum = create_struct(dum,{percentile:percents})
		return, create_struct(dum,struc)
	endif

	return,-1

end
;------------------------------------------------------------------------------------------
pro read_all_avail_struc, struc, tagname, lat = lat, lon = lon, limit = limit, land = land , sea = sea, $
				index = index, index_count = index_count

	num = tag_name2num(struc,tagname)
	ls  = keyword_set(land) or keyword_set(sea)

	arr = struc.(num).data
	if get_grid_res(lat) ne get_grid_res(arr) then make_geo,lon,lat,grid=get_grid_res(arr)
	if ls then begin
; 		if get_grid_res(dem) ne get_grid_res(arr) then dem = get_dem(grid=get_grid_res(arr))
		if get_grid_res(dem) ne get_grid_res(arr) then dem = get_coverage(lon, lat, /land)
	endif
	if keyword_set(limit) then begin
		qw  = where(between(lon,limit[1],limit[3]) and between(lat,limit[0],limit[2]),qw_cnt)
		if qw_cnt eq 0 then return
		arr = arr[qw]
		lat = lat[qw]
		if ls then dem = dem[qw]
	endif
	index = where(arr ne struc.(num).no_data_value,index_count)
	if keyword_set(sea) then index = where(arr ne struc.(num).no_data_value and dem eq 0,index_count)
	if keyword_set(land)  then index = where(arr ne struc.(num).no_data_value and dem ne 0,index_count)
	if index_count gt 0 then begin
		medi3 = zonal_average(arr[index],lat[index],fillvalue=struc.(num).no_data_value,lat_zon=lat1d3,/mean)
		idx_medi3 = where(medi3 ne struc.(num).no_data_value,index_count)
	endif
	if index_count gt 0 then maximum = max([maximum,medi3])
	name_arr = is_defined(name_arr) ? [name_arr,struc.(num).algoname] : struc.(num).algoname
	col_arr  = is_defined(col_arr)  ? [col_arr,cgcolor('Green')] : cgcolor('Green')

end
;------------------------------------------------------------------------------------------
pro bring_to_same_grid, bild1,bild2,fillvalue1,fillvalue2,file1=file1,file2=file2		,$	; input
			lon,lat,grid_res_out,verbose = verbose,algo1=algo1,algo2=algo2,level=level	; output

	verb = keyword_set(verbose)
	alg1 = keyword_set(algo1) ? strlowcase(algo1[0]) : 'Algorithm1'
	alg2 = keyword_set(algo2) ? strlowcase(algo2[0]) : 'Algorithm2'
	si1  = size(bild1,/dim)
	si2  = size(bild2,/dim)
	lev  = keyword_set(level) ? strlowcase(level) : ''

	if si1[0] ne si2[0] or si1[1] ne si2[1] then begin
		dum1 = [360,180]/float(si1)
		dum2 = [360,180]/float(si2)
		reg1 = dum1[0] eq dum1[1] ; reguläres grid ??
		reg2 = dum2[0] eq dum2[1] ; reguläres grid ??
		if reg1 and reg2 then begin
			if dum1[0] gt dum2[0] then begin
				if verb then print,'Grid down '+alg2+' array to regular grid size of '+strcompress(dum1[0],/rem)+' degree.'
				if rebinable(dum2[0],dum1[0]) then begin
					bild2 = grid_down_globe(bild2,dum1[0],no_data_value=fillvalue2, sample=(strmid(lev,0,3) eq 'l3u'))
					make_geo,lon,lat,grid=dum1[0], verbose = verbose
				endif else begin
					make_geo,lon,lat,grid=dum2[0], verbose = verbose
					dum   = sat2global(lon,lat,bild2,grid_res=dum1[0],no_data_value=fillvalue2,found=found)
					if found then begin
						bild2 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
						lon   = dum.lon
						lat   = dum.lat
					endif
					free,dum
				endelse
				grid_res_out = dum1[0]
			endif else begin
				if verb then print,'Grid down '+alg1+' array to regular grid size of '+strcompress(dum2[0],/rem)+' degree.'
				if rebinable(dum1[0],dum2[0]) then begin
					bild1 = grid_down_globe(bild1,dum2[0],no_data_value=fillvalue1, sample=(strmid(lev,0,3) eq 'l3u'))
					make_geo,lon,lat,grid=dum2[0], verbose = verbose
				endif else begin
					make_geo,lon,lat,grid=dum1[0], verbose = verbose
					dum   = sat2global(lon,lat,bild1,grid_res=dum2[0],no_data_value=fillvalue1,found=found)
					if found then begin
						bild1 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
						lon   = dum.lon
						lat   = dum.lat
					endif
					free,dum
				endelse
				grid_res_out =dum2[0]
			endelse
		endif else if reg1 and ~reg2 then begin
			if verb then print,'Grid down unregular grid array of '+alg2+' to regular grid size of '+strcompress(dum1[0],/rem)+' degree.'
			make_geo,file=file2,lon,lat, verbose = verbose
			dum = sat2global(lon,lat,bild2,grid_res = dum1[0],no_data_value=fillvalue2,found=found)
			if found then begin
				bild2 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
				lon   = dum.lon
				lat   = dum.lat
			endif
			free,dum
			grid_res_out = dum1[0]
		endif else if reg2 and ~reg1 then begin
			if verb then print,'Grid down unregular grid array of '+alg1+' to regular grid size of '+strcompress(dum2[0],/rem)+' degree.'
			make_geo,file=file1,lon,lat, verbose = verbose
			dum = sat2global(lon,lat,bild1,grid_res = dum2[0],no_data_value=fillvalue1,found=found)
			if found then begin
				bild1 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
				lon   = dum.lon
				lat   = dum.lat
			endif
			free,dum
			grid_res_out = dum2[0]
		endif else begin
			; bring both to 0.5
			if verb then print,'Grid down unregular grid arrays of '+alg1+' and '+alg2+' to regular grid size 0.5 degree.'
			make_geo,file=file2,lon,lat, verbose = verbose
			dum = sat2global(lon,lat,bild2,grid_res = 0.5,no_data_value=fillvalue2,found=found)
			if found then bild2 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
			make_geo,file=file1,lon,lat, verbose = verbose
			dum = sat2global(lon,lat,bild1,grid_res = 0.5,no_data_value=fillvalue1,found=found)
			if found then begin
				bild1 = strmid(lev,0,3) eq 'l3u' ? dum.null_sample : dum.mean
				lon   = dum.lon
				lat   = dum.lat
			endif
			free, dum
			grid_res_out = 0.5
		endelse
	endif else begin
		grid_res_out   = get_grid_res(bild1)
		make_geo, file = strreplace(file1,'secondary','primary'), lon,lat, verbose = verbose,grid=grid_res_out
	endelse

end
;---------------------------------------------------------------------------------------------
pro bring_to_same_unit,	data,bild1,bild2,fillvalue1,fillvalue2,algo1,algo2,unit1,unit2,level=level,verbose = verbose , $
			minv1=minv1,minv2=minv2,maxv1=maxv1,maxv2=maxv2,flag_meanings1=flag_meanings1,flag_meanings2=flag_meanings2

	alg1 = ref2algo(algo1)
	alg2 = ref2algo(algo2)
	verb = keyword_set(verbose)
	dat  = strlowcase(data[0])
	lev  = keyword_set(level) ? strlowcase(level) : 'l3c'

	is_gewex1 = (total(lev eq ['l3c','l3s']) and total(alg1 eq ['patmos','patmos_old','gewex','gac2-gewex','isccp']))
	is_gewex2 = (total(lev eq ['l3c','l3s']) and total(alg2 eq ['patmos','patmos_old','gewex','gac2-gewex','isccp']))

; 	alle = ['esacci','patmos','coll5','coll6','clara','clara2','gewex','claas','isccp','era-i']
	if (strmid(alg1,0,6) eq 'patmos' or strmid(alg2,0,6) eq 'patmos') and lev eq 'l3u' and get_product_name(dat,algo='patmos') eq 'cloud_mask' then begin
		if verb then print,'Reset Patmos l3u cloud mask (clear+prob. clear)->clear, (cloudy+prob. cloudy) -> cloudy!'
		if ~is_the_same(alg1,alg2) then begin
			if strmid(alg1,0,6) eq 'patmos' then begin
				ndidx = where(bild1 eq fillvalue1,ndidxcnt)
				bild1 = float(bild1 gt 1)
				if ndidxcnt gt 0 then bild1[ndidx] = fillvalue1
				if keyword_set(flag_meanings1) then flag_meanings1 = flag_meanings1[[0,3]]
			endif 
			if strmid(alg2,0,6) eq 'patmos' then begin
				ndidx = where(bild2 eq fillvalue2,ndidxcnt)
				bild2 = float(bild2 gt 1)
				if ndidxcnt gt 0 then bild2[ndidx] = fillvalue2
				if keyword_set(flag_meanings2) then flag_meanings2 = flag_meanings2[[0,3]]
			endif
		endif
	endif else if get_product_name(dat,algo='patmos') eq 'cloud_phase' and lev eq 'l3u' then begin
			if total(alg1 eq ['patmos','patmos_old']) then begin
				bild1 = float(bild1)
				ndidx   = where(bild1 eq fillvalue1 or bild1 eq 5.,ndidxcnt) ; fillvalue,unknown -> fillvalue
				idx     = where(bild1 eq 2.,idxcnt)
				if idxcnt gt 0 then bild1[idx] = 1. ; supercooled -> water
				idx     = where(between(bild1,3.,4.),idxcnt)
				if idxcnt gt 0 then bild1[idx] = 2. ; mixed,ice -> ice
				if ndidxcnt gt 0 then bild1[ndidx] = fillvalue1
				minv1 = 0.
				maxv1 = 2.
				unit1 = ' '
				if keyword_set(flag_meanings1) then flag_meanings1 = flag_meanings1[[0,1,4]]
			endif
			if total(alg2 eq ['patmos','patmos_old']) then begin
				bild2 = float(bild2)
				ndidx   = where(bild2 eq fillvalue2 or bild2 eq 5.,ndidxcnt) ; fillvalue,unknown -> fillvalue
				idx     = where(bild2 eq 2.,idxcnt)
				if idxcnt gt 0 then bild2[idx] = 1. ; supercooled -> water
				idx     = where(between(bild2,3.,4.),idxcnt)
				if idxcnt gt 0 then bild2[idx] = 2. ; mixed,ice -> ice
				if ndidxcnt gt 0 then bild2[ndidx] = fillvalue2
				minv2 = 0.
				maxv2 = 2.
				unit2 = ' '
				if keyword_set(flag_meanings2) then flag_meanings2 = flag_meanings2[[0,1,4]]
			endif
	endif else if get_product_name(dat,algo='patmos') eq 'cloud_type' and lev eq 'l3u' then begin
			if total(alg1 eq ['clara2','esacci','esacciv3','patmos']) then begin
				; compare pavolonis cloud types ; 
				; keep only fog, liquid(water),supercooled,opaque,cirrus,overlap and re-order
				if keyword_set(flag_meanings1) then begin
					;combine synonym meanings with a "+", e.g. CCI calls it liquid while clara and patmos call it water, but the meaning is the same
					;we also patmos classify Patmos "overshooting" into "opaque_ice"
					flgms          = strlowcase(strreplace(flag_meanings1,['!C',' '],['','']))
					search_flags   = ['fog','liquid+water','supercooled+supercooledwater','opaque+opaqueice+overshooting','cirrus','overlap+overlapping']
					flag_meanings1 = ['fog','liquid','super!Ccooled','opaque!Cice','cirrus','overlap'] ; overwrite actual flag meanings
					new_bild       = make_array(size(bild1,/dim),type=size(bild1,/type),value=fillvalue1)
					minmaxv        = minmax(bild1,no_data_value=fillvalue1)
					for i = 0, n_elements(search_flags) -1 do begin
						dum = strsplit(search_flags[i],'+',/ext)
						for j = 0,n_elements(dum) -1 do begin
							flagn_idx = where(strmatch(flgms,dum[j]),flagn_idxcnt) +minmaxv[0]; e.g., clara starts with 1 not 0
							if flagn_idxcnt gt 0 then begin
								index = where(bild1 eq flagn_idx[0],indexcnt)
								if indexcnt gt 0 then new_bild[index] = i
							endif
						endfor
					endfor
					bild1 = new_bild
					minv1 = 0
					maxv1 = n_elements(search_flags) -1 
					unit1 = ' '
					longname = 'Pavolonis Cloud Types'
				endif else begin			
					; leave out fillvalue(-999),clear(0) and N/A(1)
					; remove 0:clear, 1:(N/A) and 5(mixed) from outdata
					bild1 -= (alg1 eq 'clara2' ? 1:2)
					mixed  = where(bild1 eq 3,mix_cnt)
					ice_idx = where(bild1 gt 3,ice_cnt)
					if ice_cnt gt 0 then bild1[ice_idx] -= 1
					idx = where(~between(bild1,0,10),vi_count)
					if vi_count gt 0 then bild1[idx] = fillvalue1
					if mix_cnt gt 0 then bild1[mixed] = fillvalue1
					minv1 = 0
					maxv1 = 5
					unit1 = ' '
					longname = 'Pavolonis Cloud Types'
					flag_meanings1 = ['fog','liquid','super!Ccooled','opaque!Cice','cirrus','overlap'] ; this should be right
				endelse
			endif
			if total(alg2 eq ['clara2','esacci','esacciv3','patmos']) then begin
				; compare pavolonis cloud types ; 
				; keep only fog, liquid(water),supercooled,opaque,cirrus,overlap and re-order
				if keyword_set(flag_meanings2) then begin				
					;combine synonym meanings with a "+", e.g. CCI calls it liquid while clara and patmos call it water, but the meaning is the same
					;we also patmos classify Patmos "overshooting" into "opaque_ice"
					flgms          = strlowcase(strreplace(flag_meanings2,['!C',' '],['','']))
					search_flags   = ['fog','liquid+water','supercooled+supercooledwater','opaque+opaqueice+overshooting','cirrus','overlap+overlapping']
					flag_meanings2 = ['fog','liquid','super!Ccooled','opaque!Cice','cirrus','overlap'] ; overwrite actual flag meanings
					new_bild       = make_array(size(bild2,/dim),type=size(bild2,/type),value=fillvalue2)
					minmaxv        = minmax(bild2,no_data_value=fillvalue2)
					for i = 0, n_elements(search_flags) -1 do begin
						dum = strsplit(search_flags[i],'+',/ext)
						for j = 0,n_elements(dum) -1 do begin
							flagn_idx = where(strmatch(flgms,dum[j]),flagn_idxcnt) +minmaxv[0]; e.g., clara starts with 1 not 0
							if flagn_idxcnt gt 0 then begin
								index = where(bild2 eq flagn_idx[0],indexcnt)
								if indexcnt gt 0 then new_bild[index] = i
							endif
						endfor
					endfor
					bild2 = new_bild
					minv2 = 0
					maxv2 = n_elements(search_flags) -1 
					unit2 = ' '
					longname = 'Pavolonis Cloud Types'
				endif else begin			
					bild2 -= (alg2 eq 'clara2' ? 1:2)
					mixed  = where(bild2 eq 3,mix_cnt)
					ice_idx = where(bild2 gt 3,ice_cnt)
					if ice_cnt gt 0 then bild2[ice_idx] -= 1
					idx = where(~between(bild2,0,10),vi_count)
					if vi_count gt 0 then bild2[idx] = fillvalue2
					if mix_cnt gt 0 then bild2[mixed] = fillvalue2
					minv2 = 0
					maxv2 = 5 
					unit2 = ' '
					longname = 'Pavolonis Cloud Types'
					flag_meanings2 = ['fog','liquid','super!Ccooled','opaque!Cice','cirrus','overlap'] ; this should be right
				endelse
			endif
	endif else if total(dat eq ['cph','cph_day']) then begin
		if total(alg1 eq ['clara2','clara','claas','isccp_old','hector']) or is_gewex1 then begin
			bild1 = float(bild1)
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then bild1[idx] /= 100.
			if keyword_set(minv1) then minv1 /= 100.
			if keyword_set(maxv1) then maxv1 /= 100.
			unit1 = ' '
			if verb then print,'Divide now '+dat+' of '+alg1+' by 100.'
		endif
		if total(alg2 eq ['clara2','clara','claas','isccp_old','hector']) or is_gewex1 then begin
			bild2 = float(bild2)
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then bild2[idx] /= 100.
			if keyword_set(minv2) then minv2 /= 100.
			if keyword_set(maxv2) then maxv2 /= 100.
			unit2 = ' '
			if verb then print,'Divide now '+dat+' of '+alg2+' by 100.'
		endif
	endif else if total(dat eq ['cloud_fraction','a_ca']) or strmid(dat,0,8) eq 'cc_total' $
				or strmid(dat,0,3) eq 'cfc'  then begin
		if total(alg1 eq ['clara2','clara','claas','isccp_old','hector']) then begin
			bild1 = float(bild1)
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then bild1[idx] /= 100.
			if keyword_set(minv1) then minv1 /= 100.
			if keyword_set(maxv1) then maxv1 /= 100.
			unit1 = ' '
			if verb then print,'Divide now '+dat+' of '+alg1+' by 100.'
		endif
		if total(alg2 eq ['clara2','clara','claas','isccp_old','hector']) then begin
			bild2 = float(bild2)
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then bild2[idx] /= 100.
			if keyword_set(minv2) then minv2 /= 100.
			if keyword_set(maxv2) then maxv2 /= 100.
			unit2 = ' '
			if verb then print,'Divide now '+dat+' of '+alg2+' by 100.'
		endif
	endif else if total(dat eq ['cwp_asc','cwp_desc','cwp_ice','cwp_liq','cwp','29','lwp','a_clwp','iwp','a_ciwp','iwp_allsky','lwp_allsky','cwp_allsky']) then begin
		if total(alg1 eq ['clara2','clara','claas','hector']) then begin
			bild1 = float(bild1)
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then bild1[idx] *= 1000.
			if keyword_set(minv1) then minv1 *= 1000.
			if keyword_set(maxv1) then maxv1 *= 1000.
			unit1 = textoidl(' [ g/m^2]')
			if verb then print,'Multiply now '+dat+' of '+alg1+' with 1000.'
		endif
		if total(alg2 eq ['clara2','clara','claas','hector']) then begin
			bild2 = float(bild2)
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then bild2[idx] *= 1000.
			if keyword_set(minv2) then minv2 *= 1000.
			if keyword_set(maxv2) then maxv2 *= 1000.
			unit2 = textoidl(' [ g/m^2]')
			if verb then print,'Multiply now '+dat+' of '+alg2+' with 1000.'
		endif
	endif else if total(dat eq ['cth','cth_day','cth_arith_mean','cth_corrected']) then begin
		if total(alg1 eq ['clara2','clara','claas','hector']) then begin
			bild1 = float(bild1)
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then bild1[idx] /= 1000.
			if keyword_set(minv1) then minv1 /= 1000.
			if keyword_set(maxv1) then maxv1 /= 1000.
			unit1 = textoidl(' [ km]')
			if verb then print,'Divide now '+dat+' of '+alg1+' by 1000.'
		endif
		if total(alg2 eq ['clara2','clara','claas','hector']) then begin
			bild2 = float(bild2)
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then bild2[idx] /= 1000.
			if keyword_set(minv2) then minv2 /= 1000.
			if keyword_set(maxv2) then minv2 /= 1000.
			unit2 = textoidl(' [ km]')
			if verb then print,'Divide now '+dat+' of '+alg2+' by 1000.'
		endif
	endif else if total(strmid(dat,0,3) eq ['ref','cer']) then begin
		if total(alg1 eq ['clara2','claas','hector']) then begin
			bild1 = float(bild1)
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then bild1[idx] /= 1.e-06
			if keyword_set(minv1) then minv1 /= 1.e-06
			if keyword_set(maxv1) then maxv1 /= 1.e-06
			unit1 = textoidl(' [ \mum]')
			if verb then print,'Divide now '+dat+' of '+alg1+' by 1.e-08'
		endif
		if total(alg2 eq ['clara2','claas','hector']) then begin
			bild2 = float(bild2)
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then bild2[idx] /= 1.e-06
			if keyword_set(minv2) then minv2 /= 1.e-06
			if keyword_set(maxv2) then minv2 /= 1.e-06
			unit2 = textoidl(' [ \mum]')
			if verb then print,'Divide now '+dat+' of '+alg2+' by 1.e-08'
		endif
	endif else if total(dat eq ['scanline_time_asc','scanline_time_desc','time_asc','time_desc']) then begin
		if total(alg1 eq ['esacci','esacciv3']) then begin
			idx = where(bild1 ne fillvalue1,idxcnt)
			if idxcnt gt 0 then minzeit = rnd(min(bild1[idx]),0.5)
			if idxcnt gt 0 then bild1[idx] = (bild1[idx] - minzeit)*24d
			if keyword_set(minv1) then minv1 =  0d
			if keyword_set(maxv1) then maxv1 = 24d
			unit1 = ' [h]'
			if verb then print,'Convert now '+dat+' of '+alg1+' to fractional hours'
		endif
		if total(alg2 eq ['esacci','esacciv3']) then begin
			idx = where(bild2 ne fillvalue2,idxcnt)
			if idxcnt gt 0 then minzeit = rnd(min(bild2[idx]),0.5)
			if idxcnt gt 0 then bild2[idx] = (bild2[idx] - minzeit)*24d
			if keyword_set(minv2) then minv2 =  0d
			if keyword_set(maxv2) then maxv2 = 24d
			unit2 = ' [h]'
			if verb then print,'Convert now '+dat+' of '+alg2+' to fractional hours'
		endif
	endif
	if verb then print,'--------------------------------------------------'

end
;---------------------------------------------------------------------------------------------
pro bring_to_same_grid_and_unit,data,bild1,bild2,fillvalue1,fillvalue2,file1=file1,file2=file2		, $
				algo1,algo2,unit1,unit2,level=level,minv1=minv1,minv2=minv2,maxv1=maxv1,maxv2=maxv2		, $
				lon,lat,grid_res_out,verbose = verbose,flag_meanings1=flag_meanings1, $
				flag_meanings2=flag_meanings2,no_unit=no_unit

	alg1 = strlowcase(algo1[0])
	alg2 = strlowcase(algo2[0])
	verb = keyword_set(verbose)

	dat  = strlowcase(data[0])
	lev  = keyword_set(level) ? strlowcase(level) : 'l3c'

	bring_to_same_grid, bild1,bild2,fillvalue1,fillvalue2,file1=file1,file2=file2		, $
			lon,lat,grid_res_out,verbose = verbose,algo1=algo1,algo2=algo2,level=level

	if ~keyword_set(no_unit) then bring_to_same_unit,data,bild1,bild2,fillvalue1,fillvalue2,algo1,algo2,unit1,unit2	, $
			level=level,verbose=verbose,minv1=minv1,minv2=minv2,maxv1=maxv1,maxv2=maxv2,flag_meanings1=flag_meanings1, $
					flag_meanings2=flag_meanings2

end
;-------------------------------------------------------------------------------------------------------------------------
pro find_local_files, startyear = startyear, endyear = endyear, sat = sat, algo = algo, $
		      level = level, data = data, instrument = instrument, node = node, $
		      verbose = verbose, help = help

	if keyword_set(help) then begin
		print, 'Syntax: find_local_files, /startyear, /endyear, /sat, /algo, /level, /data, /instrument, /node, /verbose'
		return
	end

	startyear = keyword_set(startyear) ? startyear : 1982
	endyear   = keyword_set(endyear)   ? endyear   : 2014
	anzyears  = (endyear-startyear)+1
	
	years  = string(indgen(anzyears)+startyear,f='(i4.4)')
	months = string(indgen(12)+1,f='(i2.2)')
	n_ye   = n_elements(years)
	n_mo   = n_elements(months)
	level  = keyword_set(level) ? level : 'l3c'
	sil    = ~keyword_set(verbose)
	
	cnt_all = 0l
	cnt1    = 0l
	for yy = 0, n_ye-1 do begin
		for mm = 0, n_mo-1 do begin
			cnt_mo = 0l
			cnt  = 0l
			days = level eq 'l3c' ? 1:dom(years[yy],months[mm])
			for dd = 0, n_elements(days)-1 do begin
				ff = get_filename(years[yy],months[mm],days[dd],sat=sat,algo=algo,level=level,data=data,instrument=instrument,node=node,found=found,sil=sil)
				if found gt 1 then begin & print,ff & stop & end
				cnt = cnt + found
				cnt1 = cnt1 +found
				cnt_mo++
				cnt_all++
			endfor
			print, years[yy]+'/'+months[mm]+string(cnt,f='(",   Num : ", i6)')+string(100.*float(cnt)/float(cnt_mo),f='(",",f8.1," [%]")')
		endfor
	endfor
	print, string(cnt1,f='("Summary,   Num : ", i6)')+string(100.*float(cnt1)/float(cnt_all),f='(",",f8.1," [%]")')

end
;------------------------------------------------------------------------------------------
function get_hct_data, hist_cloud_type, array, algoname, relative = relative, sdum = sdum, grid = grid, found = found

	found = 1.
	si=size(array,/dim)

	if n_elements(si) ne 4 then begin
		ok = dialog_message('Array must have 4 dimensions (x,y,cotbin,ctpbin)')
		found = 0.
		return, -1.
	endif
	arr  = (array > 0)

	if keyword_set(relative) or arg_present(sdum) then begin
		;back to loop, total is substantial slower 
		;sdum = total(total(reform(arr[*,*,*,*,0]>0),4),3)
		sdum = fltarr(si[0:1])
		for i = 0,si[2] -1 do begin
			for j=0,si[3] -1 do begin
				sdum += (reform(arr[*,*,i,j]))
			endfor
		endfor
		found2 = 1.
	endif

	algo = ref2algo(algoname)
	; combine algos that use same definitions of histograms
	is_gewex = total(algo eq ['gewex','gac2-gewex','patmos','patmos_old','isccp'])
	if is_gewex then algo = 'gewex'
	is_cmsaf = total(algo eq ['clara2','esacci','esacciv3','era-i','era-i2','claas','calipso'])
	if is_cmsaf then algo = 'cmsaf'

	; cci
	case algo of
		'esacci_old': begin
				;plev = [1100.,800.,680.,560.,440.,310.,180.,10.]
				;tau  = [0.3,1.3,3.6,9.4,23.,60.,100.]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0,1,0,1]
					'sc'	: idxse = [2,3,0,1]
					'st'	: idxse = [4,5,0,1]
					'low'	: idxse = [0,5,0,1]
					; mid level
					'ac'	: idxse = [0,1,2,3]
					'as'	: idxse = [2,3,2,3]
					'ns'	: idxse = [4,5,2,3]
					'mid'	: idxse = [0,5,2,3]
					; high
					'ci'	: idxse = [0,1,4,6]
					'cs'	: idxse = [2,3,4,6]
					'cb'	: idxse = [4,5,4,6]
					'high'	: idxse = [0,5,4,6]
					else	: idxse = [0,1,0,1]; 'cu'
				endcase
			 end
		'clara'	: begin
				;plev = [10,90,180,145,310,375,440,500,560,620,680,740,800,950,1100]
				;tau  = [0.,0.3,0.6,1.3,2.2,3.6,5.8,9.4,15.0,23.0,41.0,60.0,80.0,100.]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0, 4,10,13]
					'sc'	: idxse = [5, 8,10,13]
					'st'	: idxse = [9,12,10,13]
					'low'	: idxse = [0,12,10,13]
					; mid level
					'ac'	: idxse = [0, 4, 6, 9]
					'as'	: idxse = [5, 8, 6, 9]
					'ns'	: idxse = [9,12, 6, 9]
					'mid'	: idxse = [0,12, 6, 9]
					; high
					'ci'	: idxse = [0, 4, 0, 5]
					'cs'	: idxse = [5, 8, 0, 5]
					'cb'	: idxse = [9,12, 0, 5]
					'high'	: idxse = [0,12, 0, 5]
					else	: idxse = [0, 4,10,13] ; 'cu'
				endcase
			  end
		'cmsaf': begin
				;plev = [1,90,180,245,310,375,440,500,560,620,680,740,800,875,950,1100]
				;tau  = [0.,0.3,0.6,1.3,2.2,3.6,5.8,9.4,15.0,23.0,41.0,60.0,80.0,100.]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0, 4,10,14]
					'sc'	: idxse = [5, 8,10,14]
					'st'	: idxse = [9,12,10,14]
					'low'	: idxse = [0,12,10,14]
					; mid level
					'ac'	: idxse = [0, 4, 6, 9]
					'as'	: idxse = [5, 8, 6, 9]
					'ns'	: idxse = [9,12, 6, 9]
					'mid'	: idxse = [0,12, 6, 9]
					; high
					'ci'	: idxse = [0, 4, 0, 5]
					'cs'	: idxse = [5, 8, 0, 5]
					'cb'	: idxse = [9,12, 0, 5]
					'high'	: idxse = [0,12, 0, 5]
					else	: idxse = [0, 4,10,14] ; 'cu'
				endcase
			  end
		'coll5'	: begin
				;plev = [0.,180.,310.,440.,560.,680.,800.,1100.]
				;tau  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0,2,5,6]
					'sc'	: idxse = [3,4,5,6]
					'st'	: idxse = [5,6,5,6]
					'low'	: idxse = [0,6,5,6]
					; mid level
					'ac'	: idxse = [0,2,3,4]
					'as'	: idxse = [3,4,3,4]
					'ns'	: idxse = [5,6,3,4]
					'mid'	: idxse = [0,6,3,4]
					; high
					'ci'	: idxse = [0,2,0,2]
					'cs'	: idxse = [3,4,0,2]
					'cb'	: idxse = [5,6,0,2]
					'high'	: idxse = [0,6,0,2]
					else	: idxse = [0,2,5,6] ; 'cu'
				endcase
			  end
		'coll6'	: begin
				;plev = [0.,180.,310.,440.,560.,680.,800.,1100.]
				;tau  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.,150]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0,2,5,6]
					'sc'	: idxse = [3,4,5,6]
					'st'	: idxse = [5,7,5,6]
					'low'	: idxse = [0,7,5,6]
					; mid level
					'ac'	: idxse = [0,2,3,4]
					'as'	: idxse = [3,4,3,4]
					'ns'	: idxse = [5,7,3,4]
					'mid'	: idxse = [0,7,3,4]
					; high
					'ci'	: idxse = [0,2,0,2]
					'cs'	: idxse = [3,4,0,2]
					'cb'	: idxse = [5,7,0,2]
					'high'	: idxse = [0,7,0,2]
					else	: idxse = [0,2,5,6] ; 'cu'
				endcase
			  end
		'gewex'	: begin
				;plev = [0.,180.,310.,440.,560.,680.,800.,1100.]
				;tau  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.]
				case strlowcase(hist_cloud_type) of
					; low
					'cu'	: idxse = [0,2,5,6]
					'sc'	: idxse = [3,4,5,6]
					'st'	: idxse = [5,6,5,6]
					'low'	: idxse = [0,6,5,6]
					; mid level
					'ac'	: idxse = [0,2,3,4]
					'as'	: idxse = [3,4,3,4]
					'ns'	: idxse = [5,6,3,4]
					'mid'	: idxse = [0,6,3,4]
					; high
					'ci'	: idxse = [0,2,0,2]
					'cs'	: idxse = [3,4,0,2]
					'cb'	: idxse = [5,6,0,2]
					'high'	: idxse = [0,6,0,2]
					else	: idxse = [0,2,5,6] ; 'cu'
				endcase
			  end
		else	: begin
				print,'Algorithm name unknown. '+algo
				found = 0.
				return,-1.
			  end
		endcase

; 		arr = total(total(arr[*,*,idxse[0]:idxse[1],idxse[2]:idxse[3]],3),3)
		si = size(arr,/dim)
		dum = fltarr(si[0:1])
		for i = idxse[0], idxse[1] do begin
			for j= idxse[2],idxse[3] do dum += arr[*,*,i,j]
		endfor
		arr = temporary(dum)

		if keyword_set(grid) then begin
			if rebinable(get_grid_res(arr),grid) then begin
				arr = grid_down_globe(arr,grid,/total,found=found)
			endif else begin
				make_geo,lond,latd,grid = get_grid_res(arr)
				dum = sat2global(lond,latd,arr,grid=grid,found=found)
				if found then arr = (dum).sum
			endelse
			if keyword_set(found2) then begin
				if rebinable(get_grid_res(sdum),grid) then begin
					sdum = grid_down_globe(sdum,grid,/total,found=found2)
				endif else begin
					dum = sat2global(lond,latd,sdum,grid=grid,found=found2)
					if found2 then sdum = (dum).sum
				endelse
			endif
		endif

		if keyword_set(relative) then arr = (found and found2) ? arr/(sdum > 1) *100. : -1

		; Ab sofort werden auch 4dim colls in read_hdf4 rotiert
; 		return, strmid(algo,0,4) eq 'coll' ? rotate(arr,7) : arr
		return, arr
end
;-------------------------------------------------------------------------------------------------------------------------
function get_hct_ratio, array, sdum, limit=limit, antarctic = antarctic, arctic=arctic,lon=lon,lat=lat,dem=dem, land=land,sea=sea, $
			void_index = void_index, shape_file = shape_file, fillv_index = fillv_index, relative = relative, texstyle = texstyle

	bild = array

	if keyword_set(relative) then bild = bild/100. * sdum

	area = get_coverage( lon, lat, dem = dem, limit = limit, land = land, sea = sea, shape_file = shape_file, $
						antarctic = antarctic, arctic = arctic, coverage = coverage, fillv_index = fillv_index)

	void_index = where((sdum*area) eq 0,complement=nvoid,ncomp=nvoid_cnt)

	; global mean ratio
	if keyword_set(lat) then begin
		rat   = nvoid_cnt gt 0 ? string((total(bild[nvoid])/total(sdum[nvoid]))*100.,f='(f6.2)')+'%' : 'No data points!' 
		ratio = nvoid_cnt gt 0 ? string((gmean(bild[nvoid]/float(sdum[nvoid]),lat[nvoid]))*100.,f='(f6.2)')+$
		(keyword_set(texstyle) ? '\%':'%') : 'No data points!' 
		if nvoid_cnt gt 0 then ratio = keyword_set(texstyle) ? ratio : ratio + ' ('+rat+')'
	endif else begin
		ratio = nvoid_cnt gt 0 ? string((total(bild[nvoid])/total(sdum[nvoid]))*100.,f='(f6.2)')+$
		(keyword_set(texstyle) ? '\%':'%') : 'No data points!' 
	endelse

	return, ratio

end
;-------------------------------------------------------------------------------------------------------------------------
function get_hct_maxtype, array, algo, grid_res = grid_res, lon=lon, lat=lat, fillvalue=fillvalue, htypes=htypes, found=found

	found=1
	if size(array,/n_dim) ne 4 then begin
		ok = dialog_message('Array must have 4 dimensions (x,y,cotbin,ctpbin)')
		found = 0.
		return, -1.
	endif
	arr    = (array > 0)
	si_arr = size(arr,/dim)
	o_grid = get_grid_res(arr[*,*,0,0])
	htypes = ['cu','sc','st','ac','as','ns','ci','cs','cb']
	if keyword_set(grid_res) then begin
		gres = grid_res
		si   = [360.,180.]/float(gres)
	endif else begin
		gres = o_grid
		si = (size(arr,/dim))[0:1]
	endelse

	dum_maxtype  = lonarr(si[0],si[1],n_elements(htypes))
	max_count    = intarr(si[0],si[1])

	if rebinable(o_grid,gres) then begin
		;first regrid then hct faster
		arr_regrid = o_grid gt gres ? arr : (rebin(float(arr),si[0],si[1],si_arr[2],si_arr[3]) * (gres/o_grid)^2); sum up always works because of (arr >0)
		for i =0,n_elements(htypes)-1 do begin
			dum = get_hct_data(htypes[i],arr_regrid,algo,found=found)
			if found then begin
				dum_maxtype[*,*,i] = dum
				max_count += dum
			endif
		endfor
	endif else begin
		if ~keyword_set(lon) and ~keyword_set(lat) then begin
			if o_grid ne 0 then make_geo,lon,lat,grid=o_grid else begin
				found=0
				return,-1
			endelse
		endif
		for i =0,n_elements(htypes)-1 do begin
			dum = get_hct_data(htypes[i],arr,algo,found=found)
			if found then begin
				if keyword_set(gres) then begin
					dumsat = sat2global(lon,lat,dum,grid=gres,found=found,no_data_value=fillvalue)
					if found then begin
						dum_maxtype[*,*,i] = dumsat.sum
						max_count += dumsat.sum
					endif
				endif
			endif else begin
				found=0
				return,-1
			endelse
		endfor
	endelse

	dum = max(dum_maxtype,dim=3,max_index)
	max_type = fix(max_index / product(si))
	idx = where(max_count eq 0,idxcnt)
	if idxcnt gt 0 then max_type[idx] = -1

	return, max_type
end
;-------------------------------------------------------------------------------------------------------------------------
function get_1d_hist_from_jch, bild, algo, data=data, bin_name = bin_name, found = found	, $
				limit = limit, antarctic=antarctic, arctic=arctic	, $
				lon=lon, lat=lat, dem=dem, land=land, sea=sea, fillv_index=fillv_index,shape_file=shape_file

	found = 1.
	if n_params() ne 2 then begin
		print,'Syntax: result = get_1d_hist_from_jch( bild, algo, data, found = found)'
		found = 0.
		return,-1.
	endif

	if ~keyword_set(data) then data = 'both'
	ls = keyword_set(land) or keyword_set(sea)

	si = size(bild,/dim)
	if n_elements(si) ne 4 then begin
		print, 'array must have 4 dimensions (x,y,cotbin,ctpbin)'
		found = 0.
		return, -1.
	endif
	alg = ref2algo(algo)
	is_gewex = total(alg eq ['gewex','gac2-gewex','patmos','patmos_old','isccp'])

	if ~keyword_set(lon) or ~keyword_set(lat) then begin
		make_geo,lon,lat,grid=get_grid_res(bild[*,*,0,0,0]),found=found
		if not found then begin
			print,'Could not find Lon/lat info!'
			return,-1
		endif
	endif

	area = 	get_coverage( 	lon, lat, dem = dem, limit = limit, land = land, sea = sea, coverage = coverage,shape_file=shape_file, $
			antarctic = antarctic, arctic = arctic, /opposite, index = lidx, count = lidx_cnt, fillv_index=fillv_index)

	if lidx_cnt gt 0 then begin & $
		for i = 0,si[2] -1 do begin & $
			for j = 0,si[3] -1 do begin & $
				ddb = reform(bild[*,*,i,j]) & $
				ddb[lidx] = -999 & $
				bild[*,*,i,j] = ddb & $
			endfor & $
		endfor & $
	endif

	if strupcase(data) eq 'CTP' or strupcase(data) eq 'BOTH' then begin
		; CTP
		bin_name=['10','180','310','440','560','680','800','1100']
		; total is much slower than for loop!!
; 		cci_hist_ctp = total(total(total(bild[*,*,*,*,0]>0,1,/double),1,/double),1,/double) ; >0 wichtig da z.B. coll5 fillvalue hat, bei den anderen steht null
		cci_hist_ctp = fltarr(si[3])
		for i = 0,si[3]-1 do cci_hist_ctp[i] = total((bild[*,*,*,i,0]>0),/double)
		; Ziel:	plev_isccp = ['10','180','310','440','560','680','800','1100']
		;	plev_coll5_coll6_gewex_patmos = [0.,180.,310.,440.,560.,680.,800.,1100.]
		;	plev_clara = [10,90,180,145,310,375,440,500,560,620,680,740,800,950,1100]
		if alg eq 'clara' then begin
			dum = fltarr(7)
			for i = 0,6 do dum[i] = total(cci_hist_ctp[2*i:(2*i)+1])
			cci_hist_ctp=dum
		endif
		;	plev_claas_clara2 = [1,90,180,245,310,375,440,500,560,620,680,740,800,875,950,1100]
		if total(alg eq ['esacci','esacciv3','clara2','claas','era-i','era-i2','calipso']) then begin
			dum = fltarr(7)
			for i = 0,5 do dum[i] = total(cci_hist_ctp[2*i:(2*i)+1])
			dum[6] = total(cci_hist_ctp[12:*])
			cci_hist_ctp=dum
		endif
		;	plev_cci = [1100.,800.,680.,560.,440.,310.,180.,10.]
		if alg eq 'esacci_old' then cci_hist_ctp = reverse(cci_hist_ctp)
	endif
	if strupcase(data) eq 'COT'  or strupcase(data) eq 'BOTH' then begin
		; COT
		bin_name= strupcase(data) eq 'BOTH' ? {ctp:bin_name,cot:['0.3','1.3','3.6','9.4','23','60','100']} : ['0.3','1.3','3.6','9.4','23','60','100']
		; total is much slower than for loop!!
; 		cci_hist_cot = total(total(total(bild[*,*,*,*,0]>0,1,/double),1,/double),2,/double) ; >0 wichtig da z.B. coll5 fillvalue hat, bei den anderen steht null
                cci_hist_cot = dblarr(si[2])
                for i = 0,si[2]-1 do cci_hist_cot[i] = total((bild[*,*,i,*,0]>0),/double)
		;Ziel:	tau_isccp  = ['0.3','1.3','3.6','9.4','23','60','100']
		;	tau_cci  = [0.3,1.3,3.6,9.4,23.,60.,100.]
		;	tau_coll5_gewex_patmos  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.]
		if total(alg eq ['coll5']) or is_gewex then begin
			cci_hist_cot[1]+=cci_hist_cot[0]
			cci_hist_cot=cci_hist_cot[1:*]
		endif
		;	tau_clara_clara2_claas  = [0.,0.3,0.6,1.3,2.2,3.6,5.8,9.4,15.0,23.0,41.0,60.0,80.0,100.]
		if total(alg eq ['esacci','esacciv3','clara','clara2','claas','era-i','era-i2','calipso']) then begin
			dum = fltarr(6)
			dum[0] = total(cci_hist_cot[0:2])
			for i = 1,5 do dum[i] = total(cci_hist_cot[((2*i)+1):(2*(i+1))])
			cci_hist_cot=dum
		endif
		;	tau_coll6               = [0.,0.3,1.3,3.6,9.4,23.,60.,100.,150]
		if alg eq 'coll6' then begin
			cci_hist_cot[1]+=cci_hist_cot[0]
			cci_hist_cot[6]+=cci_hist_cot[7]
			cci_hist_cot=cci_hist_cot[1:6]
		endif
	endif

	case strupcase(data) of
		'COT'	: return, cci_hist_cot
		'CTP'	: return, cci_hist_ctp
		'BOTH'	: return, {cot:cci_hist_cot,ctp:cci_hist_ctp}
		else 	: return,-1
	endcase

end
;-------------------------------------------------------------------------------------------------------------------------
function get_2d_rel_hist_from_jch, array, algoname, dem = dem, land = land, sea = sea, limit = limit	, $
				   longitude = longitude, latitude = latitude, fillvalue = fillvalue, found = found		, $
				   antarctic = antarctic, arctic = arctic, fillv_index = fillv_index,shape_file=shape_file

	found = 1.
	if n_params() ne 2 then begin
		print,'Syntax: result = get_2d_rel_hist_from_jch( bild, algo, /dem,/land,/sea,/limit,/lon,/lat,/fillvalue,/found)'
		found = 0.
		return,-1.
	endif
	
	fillv = keyword_set(fillvalue) ? fillvalue : -999

	alg  = ref2algo(algoname)
	is_gewex = total(alg eq ['gewex','gac2-gewex','patmos','patmos_old','isccp'])

	dum  = array
	si   = size(dum,/dim)
	if n_elements(si) ne 4 then begin
		print, 'Get_2d_rel_hist_from_jch: array must have 4 dimensions (lon,lat,cotbin,ctpbin)'
		found = 0.
		return, -1.
	endif

	if ~keyword_set(latitude) or ~keyword_set(longitude) then begin
		make_geo ,lon,lat,grid=get_grid_res(array[*,*,0,0])
	endif else begin
		lon = longitude
		lat = latitude
	endelse

	area = 	get_coverage( 	lon, lat, dem = dem, limit = limit, land = land, sea = sea, coverage = coverage, fillv_index = fillv_index, $
				antarctic = antarctic, arctic = arctic, /opposite, index = lidx, count = lidx_cnt,shape_file=shape_file)

	bild = fltarr(si[2:3])
	for i = 0,si[2] -1 do begin & $
		for j = 0,si[3] -1 do begin & $
			; Ab sofort werden auch 4dim colls in read_hdf4 rotiert
; 			bla_bild = strmid(alg,0,4) eq 'coll' ? rotate(reform(dum[*,*,i,j]),7) : reform(dum[*,*,i,j])
			bla_bild = reform(dum[*,*,i,j]) & $
			if lidx_cnt gt 0 then bla_bild[lidx] = fillv & $
			didx = where(bla_bild ne fillv,didx_cnt) & $
			if didx_cnt gt 0 then bild[i,j] = total(bla_bild[didx]) & $
		endfor & $
	endfor

	si = size(bild,/dim)

	if total(alg eq ['esacci','esacciv3','clara','clara2','claas','era-i','era-i2','calipso']) then begin
		; plev_claas_clara2 = reverse([1,90,180,245,310,375,440,500,560,620,680,740,800,875,950,1100])
		; plev_clara = reverse([10,90,180,145,310,375,440,500,560,620,680,740,800,950,1100])
		; tau  = [0.,0.3,0.6,1.3,2.2,3.6,5.8,9.4,15.0,23.0,41.0,60.0,80.0,100.]
		bild = rotate(bild,7)
		qwe  = fltarr(6,si[1])
		qwe[0,*] = bild[0,*]+bild[1,*]+bild[2,*]
		for i = 1,5 do qwe[i,*] = bild[(i*2)+1,*] + bild[(i+1)*2,*]
		bild = qwe
		qwe  = fltarr(6,7)
		if total(alg eq ['esacci','esacciv3','claas','clara2','era-i','era-i2','calipso']) then begin
			qwe[*,0] = total(bild[*,0:2],2)
			for i = 1,6 do qwe[*,i] = bild[*,(2*i)+1] + bild[*,(2*i)+2]
		endif else for i = 0,6 do qwe[*,i] = bild[*,2*i] + bild[*,(2*i)+1]
		bild = temporary(qwe)
	endif else if total(alg eq ['coll5']) or is_gewex then begin
		; plev = reverse([0.,180.,310.,440.,560.,680.,800.,1100.])
		; tau  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.]
		bild = rotate(bild,7)
		qwe  = fltarr(6,7)
		qwe[0,*] = bild[0,*]+bild[1,*]
		qwe[1:*,*] = bild[2:*,*]       
		bild = temporary(qwe)
	endif else if alg eq 'coll6' then begin
		; plev = reverse([0.,180.,310.,440.,560.,680.,800.,1100.])
		; tau  = [0.,0.3,1.3,3.6,9.4,23.,60.,100.,150]
		bild = rotate(bild,7)
		qwe  = fltarr(6,7)
		qwe[0,*] = bild[0,*]+bild[1,*]
		qwe[1:4,*] = bild[2:5,*]
		qwe[5,*] = bild[6,*]+bild[7,*]
		bild = temporary(qwe)
	endif

	return, bild/(total(bild) > 1)*100.

end	
;---------------------------------------------------------------------------------------------------------------------------------------------
function get_1d_rel_hist_from_1d_hist, array, dataname, algoname=algoname, limit=limit, land=land,sea=sea,arctic=arctic,antarctic=antarctic, $
					xtickname=xtickname, bin_val=bin_val, ytitle = ytitle, hist_name=hist_name, found=found, $
					var_dim_names=var_dim_names, file=file, fillv_index = fillv_index,shape_file=shape_file

	found = 1.
	si    = size(array,/dim)
	data  = get_product_name(dataname,algo='cci')
	algo  = ref2algo(algoname,/lower)
	if n_elements(si) ne 3 then begin
		if ~(n_elements(si) eq 4 and stregex(data,'ratio',/bool,/fold)) then begin
			print,'Wrong Array Size!'
			found = 0.
			return,-1l
		endif
	endif
; 
	make_geo,lon,lat,grid=get_grid_res(array[*,*,0,0]),file = file

	area = 	get_coverage( 	lon, lat, dem = dem, limit = limit, land = land, sea = sea, coverage = coverage, $
				antarctic = antarctic, arctic = arctic, fillv_index = fillv_index,shape_file = shape_file, index = lidx, count = lidx_cnt)

	dum    = array * 0
	si     = [si,1]
	for j=0,si[3]-1 do begin
		for i=0,si[2]-1 do dum[*,*,i,j] = reform(array[*,*,i,j]) * area
	endfor
	bild = total(total(dum>0,1),1)

	; works for esacci and clara2
	if stregex(data,'ctp',/fold,/bool) then begin
		bin_border = '1,90,180,245,310,375,440,500,560,620,680,740,800,875,950,1100'
		hist_name  = 'Cloud Top Pressure [hPa]'
		; hier gibts zu wenig übereinstimmung mit CC4CL-> getrennt plotten
		if algo eq 'coll5' then bin_border = '1,100,200,300,400,500,600,700,800,900,1000,1100'
		if algo eq 'coll6' then bin_border = '0,80,200,320,440,560,680,800,920,1040,1100'
		if algo eq 'isccp' then bin_border = strjoin(strcompress(100 + (indgen(11) * 100),/rem),',')
	endif
	if stregex(data,'ctt',/fold,/bool) then begin
		bin_border = '200,210,220,230,235,240,245,250,255,260,265,270,280,290,300,310,350'
		hist_name  = 'Cloud Top Temperature [K]'
		if total(algo eq ['coll5','coll6']) then begin
			dum         = (bild *0)[0:15,*]
			dum[0,*]    = bild[0,*]+bild[1,*]
			dum[1,*]    = bild[2]
			dum[2,*]    = bild[3,*]+bild[4,*]
			dum[3:10,*] = bild[5:12,*]
			dum[11,*]   = bild[13,*]+bild[14,*]
			dum[12,*]   = bild[15,*]+bild[16,*]
			bild        = temporary(dum)
		endif
	endif

	if stregex(data,'cot',/fold,/bool) then begin
		bin_border = '0,0.3,0.6,1.3,2.2,3.6,5.8,9.4,15,23,41,60,80,100,1000'
		hist_name  = 'Cloud Optical Thickness'
		if total(algo eq ['coll5','coll6']) then ok = dialog_message('get_1d_rel_hist_from_1d_hist: COT. Nothing done so far for COLL?')
	end
	if stregex(data,'cwp',/fold,/bool) then begin
		bin_border = '0,5,10,20,35,50,75,100,150,200,300,500,1000,2000,100000'
		hist_name  = 'Cloud Water Path'
		if total(algo eq ['coll5','coll6']) then ok = dialog_message('get_1d_rel_hist_from_1d_hist: CWP. Nothing done so far for COLL?')
	end
	if stregex(data,'ref',/fold,/bool) or stregex(data,'cer',/fold,/bool) then begin
		bin_border = total(algo eq ['claas','clara2']) ? '3,6,9,12,15,20,25,30,40,60,80' : '0,3,6,9,12,15,20,25,30,40,60,80'
		hist_name  = 'Cloud Effective Radius'
		if total(algo eq ['coll5','coll6']) then ok = dialog_message('get_1d_rel_hist_from_1d_hist: REF. Nothing done so far for '+algo)
	end
	if stregex(data,'cla_vis006',/fold,/bool) then begin
		bin_border = '0.00,0.10,0.20,0.30,0.40,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.90,1.00'
; 		hist_name  = 'Cloud Albedo Channel1'
		hist_name  = textoidl('Cloud Albedo at 0.6\mum')
	end
	if stregex(data,'cla_vis008',/fold,/bool) then begin
		bin_border = '0.00,0.10,0.20,0.30,0.40,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.90,1.00'
; 		hist_name  = 'Cloud Albedo Channel2'
		hist_name  = textoidl('Cloud Albedo at 0.8\mum')
	end

	if keyword_set(file) and keyword_set(var_dim_names) then begin
		dum_bin_val = get_ncdf_data_by_name(file,var_dim_names[2],found=found)
		brd_name    = strreplace(var_dim_names[2],'_centre','_border')
		; is equal if dim_name is not '_centre'
		if strmatch(brd_name,var_dim_names[2]) then begin
			brd_name = var_dim_names[2]+'_bounds' ; e.g. GEWEX files
		endif
		dum_border  = get_ncdf_data_by_name(file,brd_name,found=found_border)
; 		if found_border and stregex(var_dim_names[2],'_centre',/fold,/bool) then begin
		if found_border then begin
			if size(dum_border,/n_dim) eq 2 then begin
				dum_border=[reform(dum_border[0,0]),reform(dum_border[1,*])]
			endif
			xtickname = algo eq 'claas' and stregex(data,'ref',/fold,/bool) ? dum_border/1.e-06  : dum_border
			bin_val   = algo eq 'claas' and stregex(data,'ref',/fold,/bool) ? dum_bin_val/1.e-06 : dum_bin_val
		endif else begin
			xtickname = float(strsplit(textoidl(bin_border),',',/ext))
			bin_val   = (xtickname[1:*]+xtickname[0:*])/2.
		endelse
	endif else begin
		xtickname = float(strsplit(textoidl(bin_border),',',/ext))
		bin_val   = (xtickname[1:*]+xtickname[0:*])/2.
	endelse

	if (stregex(data,'ref',/fold,/bool) or stregex(data,'cer',/fold,/bool)) and total(algo eq ['claas','clara2']) then begin
		if xtickname[0] then begin
			xtickname = [0.,xtickname]
			bin_val   = [1.5,bin_val]
			bild      = [0.,bild]
		endif
	endif

	print,string(data,f='(A16)')+' '+string(algo,f='(A8)')+': ', strjoin(string(ulong(bild))+' ('+$
	strcompress(string(float(bild)/total(bild)*100.,f='(f6.2)'),/rem),'%) , ')+'%)'

	if stregex(data,'ctp',/fold,/bool) then begin
		pos_h = where(xtickname eq 440.,h_cnt)
		pos_m = where(xtickname eq 680.,m_cnt)
		; if algo eq 'isccp' then begin
		; 		pos_h = where(xtickname eq 500.,h_cnt)
		; 		pos_m = where(xtickname eq 700.,m_cnt)
		; endif
		if h_cnt eq 1 and m_cnt eq 1 then begin
			sum_h = string(total(bild[0:pos_h-1])/total(bild) * 100.,f='(f6.2)')+'%'
			sum_m = string(total(bild[pos_h:pos_m-1])/total(bild) * 100.,f='(f6.2)')+'%'
			sum_l = string(total(bild[pos_m:*])/total(bild) * 100.,f='(f6.2)')+'% '
			print,string(data,f='(A16)')+' '+string(algo,f='(A8)')+': Low/Mid/high: '+sum_l+sum_m+sum_h
		endif
	endif

	if stregex(data,'ratio',/fold,/bool) then begin
		idx = where(bild[*,0] eq 0. and bild[*,1] eq 0.,idx_cnt)
		bild   = bild[*,0]/(total(bild>0.,2)>1.) *100.
		if idx_cnt gt 0 then bild[idx] = -999.
		ytitle ='Liquid Cloud Fraction [%]'
	endif else begin
		idx = where(bild eq -999.,idx_cnt)
		bild   = bild/(total(bild)>1.)*100.
		if idx_cnt gt 0 then bild[idx] = -999.
		ytitle ='Relative Occurrence [%]'
	endelse

	idx = where(xtickname ge 1000.,idxcnt)
	if idxcnt gt 0 then begin
		pot =fix(alog10(xtickname))
		xtickname[idx] = xtickname[idx]/(10.^pot[idx])
	end

	form = '(f20.2)'
	xtickname = strcompress(string(xtickname,f=form),/rem)
	xtickname = adv_strtrim(xtickname,0,trim='0')

	if idxcnt gt 0 then begin
		for i = 0,idxcnt -1 do begin
			if fix(xtickname[idx[i]]) ne float(xtickname[idx[i]]) then begin
				xtickname[idx[i]] = strcompress(string(float(xtickname[idx[i]])*10.^pot[idx[i]],f=form),/rem)
				xtickname[idx[i]] = adv_strtrim(xtickname[idx[i]],2,trim='0')
			endif else begin
				if xtickname[idx[i]] eq '1' then begin
					xtickname[idx[i]] = textoidl('10^'+strcompress(pot[idx[i]],/rem))
				endif else begin
					xtickname[idx[i]] += textoidl('*10^'+strcompress(pot[idx[i]],/rem))
				endelse
			endelse
		endfor
	endif

	return, bild

end
;---------------------------------------------------------------------------------------------------------------------------------------------
pro hitrates, obs, ref, illum, ls, titel, out = out, dont_print = dont_print,lun=lun

	ls_string=['All cases : ','day&land  : ','night&land: ','day&sea   : ','night&sea : ']
	if not keyword_set(dont_print) then begin
		if keyword_set(lun) then begin
; 			printf,lun,titel
; 			printf,lun, '               hrcl  :    hrcf  :     hr   :    pec   :     hss  :    tss   : number  '
		endif else begin
			print,titel
			print, '               hrcl  :    hrcf  :     hr   :    pec   :     hss  :    tss   : number  '
		endelse
	endif
	hrcl_tot = 0.
	hrcf_tot = 0.
	hr_tot   = 0.
	pec_tot  = 0.
	hss_tot  = 0.
	tss_tot  = 0.
	for i=0,4 do begin & $
		if(i eq 0) then wo_cm=where(obs ne -999 and ref ne -999,count)					& $; all data, dummy where
		if(i eq 1) then wo_cm=where(obs ne -999 and ref ne -999 and illum eq 1 and ls eq 1,count)	& $; day land
		if(i eq 2) then wo_cm=where(obs ne -999 and ref ne -999 and illum ne 1 and ls eq 1,count)	& $; night land
		if(i eq 3) then wo_cm=where(obs ne -999 and ref ne -999 and illum eq 1 and ls eq 0,count)	& $; day sea
		if(i eq 4) then wo_cm=where(obs ne -999 and ref ne -999 and illum ne 1 and ls eq 0,count)	& $; night sea
		if count gt 0 then begin
			cs_11=float(n_elements(where(obs[wo_cm] eq 1 and ref[wo_cm] eq 1))) & $
			cs_10=float(n_elements(where(obs[wo_cm] eq 1 and ref[wo_cm] eq 0))) & $
			cs_01=float(n_elements(where(obs[wo_cm] eq 0 and ref[wo_cm] eq 1))) & $
			cs_00=float(n_elements(where(obs[wo_cm] eq 0 and ref[wo_cm] eq 0))) & $
		endif else stop
		skills_n_scores,skills=[cs_00,cs_01,cs_10,cs_11],hss=hss,pec=pec,tss=tss
		hrcl = cs_11/(cs_11+cs_01)
		hrcf = cs_00/(cs_00+cs_10)
		hr   = (cs_00+cs_11)/float(count)
		if not keyword_set(dont_print) then begin
			if keyword_set(lun) then begin
				printf,lun,strjoin([string([hrcl,hrcf,hr,pec,hss,tss],f='(f8.4)'),''],' : ' )+strtrim(count,1)
			endif else begin
				print, ls_string[i]+strjoin([string([hrcl,hrcf,hr,pec,hss,tss],f='(f8.4)'),''],' : ' )+strtrim(count,1)
			endelse
		endif
		hrcl_tot += hrcl
		hrcf_tot += hrcf
		hr_tot   += hr
		pec_tot  += pec
		hss_tot  += hss
		tss_tot  += tss
	endfor
	if not keyword_set(dont_print) then begin
		if keyword_set(lun) then begin
			printf,lun,strjoin([string([hrcl_tot,hrcf_tot,hr_tot,pec_tot,hss_tot,tss_tot]/5.,f='(f8.4)'),''],' : ' ) $
					+string(total(   [hrcl_tot,hrcf_tot,hr_tot,pec_tot/100.,hss_tot,tss_tot]/5.),f='(f8.4)')
; 			printf,lun,'----------------------------------------------------------------------------------------'
		endif else begin
			print,'mean      : '+strjoin([string([hrcl_tot,hrcf_tot,hr_tot,pec_tot,hss_tot,tss_tot]/5.,f='(f8.4)'),''],' : ' ) $
					+string(total(   [hrcl_tot,hrcf_tot,hr_tot,pec_tot/100.,hss_tot,tss_tot]/5.),f='(f8.4)')
			print,'----------------------------------------------------------------------------------------'
		endelse
	endif
	out = [hrcl_tot,hrcf_tot,hr_tot,pec_tot,hss_tot,tss_tot]/5.

end
;------------------------------------------------------------------------------------------
pro test_ncdf_browser, file_type

	ft = keyword_set(file_type) ? strlowcase(file_type) : 'ncdf4'

	case ft of 
		; hdf4 eos point or grid file - coll5 l3c braucht ca. 2 min zum einlesen
		'hdf4'		: file = get_filename(2008,06,algo='coll5',sat='terra')
		; hdf5 file - nur datsets  ; claas l2
		'hdf5_data'	: file = get_filename(2008,06,06,orbit='1245',algo='claas',level='l2',data='cfc')
		; hdf5 files mit groups, datasets und datatypes ; l1_gac
		'hdf5_group'	: file = '/cmsaf/cmsaf-cld1/sstapelb/gac/data/2008/06/noaa18/noaa18_20080601_0048_99999_satproj_00000_13110_avhrr.h5'
		; ncdf3 file ; cci_old l3c
		'ncdf3'		: file = get_filename(2008,06,algo='cci_old',sat='noaa18')
		; ncdf4 file; cci_new l3c
		'ncdf4'		: file = get_filename(2008,06,algo='cci',sat='noaa18')
		else:
	endcase
	
	x=systime(1)
	mem_cur   = memory(/current)    
	starttime = systime(1)
	
	ncdf_browser, file

	caldat, systime(/utc, /julian), mo, da, ye, ho, mi, se
	dat_str	= string(da, mo, ye, ho, mi, format = '(i2.2,".",i2.2,".",i4.4," ",i2.2,":",i2.2,"[UTC] / ")')
	print, dat_str + 'TEST_NCDF_BROWSER -> '+string(systime(1)-starttime,f='("Duration        : ", f7.3, " Sec")')
	print, dat_str + 'TEST_NCDF_BROWSER -> '+string(float(memory(/highwater)-mem_cur)/1024.^2,f='("Memory required : ", f7.3, " MB")')

end
;------------------------------------------------------------------------------------------
pro find_corrupt_cci_files,l3u=l3u

	lev = keyword_set(l3u) ? 'L3U' : 'L3C'

	files = file_search('/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/'+lev+'/','*.nc',count=fcount)

	check = strarr(fcount)
	o_mytimer = obj_new('my_timer',fcount)
	for i = 0,fcount -1 do begin
		if ~is_ncdf(files[i]) then check[i] = files[i]
		o_mytimer -> wie_lang_noch
	endfor
	obj_destroy, o_mytimer

	stop

end
;------------------------------------------------------------------------------------------
pro test_first_bits, file_type

	ft = keyword_set(file_type) ? strlowcase(file_type) : 'ncdf4'

	case ft of 
		'eos'		: file = '/cmsaf/cmsaf-hcp1/osus/MODIS_BRDF/ladsftp.nascom.nasa.gov/allData/5/MCD43C1/2003/017/MCD43C1.A2003017.005.2007276124354.hdf'
		; hdf4 eos point or grid file - braucht ca. 2 min zum einlesen; coll5 l3c
		'hdf4'		: file = get_filename(2008,06,algo='coll5',sat='terra',found=found)
		; hdf5 file - nur datsets  ; claas l2
		'hdf5_data'	: file = get_filename(2009,06,06,orbit='1245',algo='claas-1',level='l2',data='cmask',sat='msg',found=found)
		; hdf5 files mit groups, datasets und datatypes ; l1_gac
		'hdf5_group': file = '/cmsaf/cmsaf-cld7/cmsaf_cld5/esa_cci_cloud_data/data/l1/avhrr/noaa18_20080620_2256_99999_satproj_07638_12407_avhrr.h5'
		; ncdf3 file ; cci_old l3c
		'ncdf3'		: file = get_filename(2008,06,algo='cci_old',sat='noaa18',found=found)
		; ncdf4 file; cci_new l3c
		'ncdf4'		: file = get_filename(2008,06,algo='cci',sat='noaa18',found=found)
		else:	file = ''
	endcase

	found = file_test(file)

	; Achtung wenn hdf4 oder hdf_eos mit is_ncdf getestet wird gibts segmentation fault ab IDL version 8.4!!!
	if found then begin
		print,file[0]
		if HDF_ISHDF(file) then begin
			print,'HDF_ISHDF()  : Is HDF4 file' 
		endif else if H5F_IS_HDF5(file) then begin
			print,'H5F_IS_HDF5(): Is HDF5 file'
			if is_ncdf(file) then $
			print,'IS_NCDF()    : Is CDF4 file'
		endif else if is_hdf(file,version) then begin
			if version eq 4 then $
			print,'IS_HDF()     : Is HDF4 file' else if version eq 5 then $
			print,'IS_HDF()     : Is HDF5 file'
		endif else if is_ncdf(file) then begin
			print,'IS_NCDF()    : Is CDF3 file'
		endif

		print,'Magic Numbers:'
		dum=bytarr(4)
		openr,lun,file,/get_lun
		readu,lun,dum
		free_lun,lun
		print,dum , ': ',string(dum)
	endif else print,'File with file_type '+ft+' not found!'

end
;------------------------------------------------------------------------------------------
pro read_panoply_ct,name,r,g,b,h

	if n_params() ne 5 then begin
		print, [ 'Syntax: read_panoply_ct,name,r,g,b', $
		'Name: ' , $
		'GIST_earth,',    $                                                                                                                                                      
		'GMT_globe,' ,     $                                                                                                                                                     
		'GMT_relief,',      $                                                                                                                                                    
		'GMT_split,' ,       $                                                                                                                                                   
		'NYT_drought,',       $                                                                                                                                                  
		'UKM_hadcrut_10,',     $                                                                                                                                                 
		'SVS_tempanomaly']
		return
	endif

	dum = einlesen('/cmsaf/nfshome/sstapelb/panoply_ct/'+name+'.cpt',comment=['#','B','F','N'])

	h = reform(dum[4,*])
	r = reform(dum[5,*])
	g = reform(dum[6,*])
	b = reform(dum[7,*])
	
end
;------------------------------------------------------------------------------------------
; set color tables including brewer for map_image, view2d (col_tab) and make_cool_contour (col_tab)
pro set_colors,	rainbow, bwr, extended_rainbow, greyscale, elevation, flip_colours , $
		other = other, ctable = ctable, brewer = brewer, col_tab = col_tab , panoply = panoply

	rainbow 		= 0
	bwr				= 0
	extended_rainbow= 0
	greyscale		= 0
	elevation		= 0
	flip_colours	= 0
	check_other     = 0

	if keyword_set(ctable) then col_tab = (ctable lt 0) ? (fix(ctable) - 100) : (fix(ctable) + 100)
	if keyword_set(other) then begin
		check_other = 1
		; default ist rainbow
; 		print,other
		case other of 
			'bwr'					: begin & free,ctable & bwr = 1 & col_tab = 4 & end
			'elevation'				: begin & free,ctable & elevation = 1 & end
			'rainbow'				: begin & free,ctable & rainbow = 1 & col_tab = 1 & end
			'extended_rainbow'		: begin & free,ctable & extended_rainbow = 1 & col_tab = 12 & end
			'cloud_mask'			: begin & ctable = 13 & flip_colours = 1 & brewer =  1 & col_tab = 130 & end
			'greyscale'				: begin & free,ctable & greyscale = 1 & col_tab = 5 & end
			'gistearth'				: begin & free,ctable & panoply = 'GIST_earth' & col_tab = 0 & end
			'gmtglobe'				: begin & free,ctable & panoply = 'GMT_globe' & col_tab = 0 & end
			'gmtrelief'				: begin & free,ctable & panoply = 'GMT_relief' & col_tab = 0 & end
			'gmtsplit'				: begin & free,ctable & panoply = 'GMT_split' & col_tab = 0 & end
			'nytdrought'			: begin & free,ctable & panoply = 'NYT_drought' & col_tab = 0 & end
			'ukmhadcrut'			: begin & free,ctable & panoply = 'UKM_hadcrut_10' & col_tab = 0 & end
			'tempanomaly'			: begin & free,ctable & panoply = 'SVS_tempanomaly' & col_tab = 0 & end
			'flip_bwr'				: begin & free,ctable & bwr = 1 & flip_colours = 1 & col_tab = -4 & end
			'flip_elevation'		: begin & free,ctable & elevation = 1 & flip_colours = 1 & end
			'flip_rainbow'			: begin & free,ctable & rainbow = 1 & flip_colours = 1 & col_tab = -1 & end
			'flip_extended_rainbow'	: begin & free,ctable & extended_rainbow = 1 & flip_colours = 1 & col_tab = -2 & end
			'flip_cloud_mask'		: begin & ctable = 13 & brewer =  1 & col_tab = -130 & end
			'flip_greyscale'		: begin & free,ctable & greyscale =1 & flip_colours = 1 & col_tab = -5 & end
			'flip_gistearth'		: begin & free,ctable & panoply = 'GIST_earth' & flip_colours = 1 & col_tab = 0 & end
			'flip_gmtglobe'			: begin & free,ctable & panoply = 'GMT_globe' & flip_colours = 1 & col_tab = 0 & end
			'flip_gmtrelief'		: begin & free,ctable & panoply = 'GMT_relief' & flip_colours = 1 & col_tab = 0 & end
			'flip_gmtsplit'			: begin & free,ctable & panoply = 'GMT_split' & flip_colours = 1 & col_tab = 0 & end
			'flip_nytdrought'		: begin & free,ctable & panoply = 'NYT_drought' & flip_colours = 1 & col_tab = 0 & end
			'flip_ukmhadcrut'		: begin & free,ctable & panoply = 'UKM_hadcrut_10' & flip_colours = 1 & col_tab = 0 & end
			'flip_tempanomaly'		: begin & free,ctable & panoply = 'SVS_tempanomaly' & flip_colours = 1 & col_tab = 0 & end
			'brewer'				: begin & brewer =  1 & col_tab = ctable & end
			else					: check_other=0
		endcase
	endif

	if ~check_other and ~is_number(ctable) then begin & rainbow=1 & free, ctable & col_tab = 1 & end
end
;------------------------------------------------------------------------------------------
pro mach_zeitreihe,start_year,n_months,data=data

	obj = obj_new('debug')

	start_year = keyword_Set(start_year) ? start_year       : 2007
	n_months   = keyword_Set(n_months)   ? n_months         : 36
	dat        = keyword_Set(data)       ? strlowcase(data) : 'cot'

	yy = indgen(n_months)/12+start_year
	mm = (indgen(n_months) mod 12) +1
	make_geo,lon,lat,grid=0.5
	gm = fltarr(n_months)-999.
	hist=0

	case dat of
		'cwp'	: begin & mini =   0. & maxi = 10000. & bin =  50 & end
		'lwp'	: begin & mini =   0. & maxi = 10000. & bin =  50 & end
		'iwp'	: begin & mini =   0. & maxi = 10000. & bin =  50 & end
		'cth'	: begin & mini =   0. & maxi =    20. & bin =  .1 & end
		'cot'	: begin & mini =   0. & maxi =   260. & bin =   1 & end
		'ref'	: begin & mini =   0. & maxi =   300. & bin =   1 & end
		'cfc'	: begin & mini =   0. & maxi =     1. & bin = .01 & end
		'cph'	: begin & mini =   0. & maxi =     1. & bin = .01 & end
		'ctt'	: begin & mini = 150. & maxi =   350. & bin =   1 & end
		'ctp'	: begin & mini =  10. & maxi =  1100. & bin =  10 & end
		else	:
	endcase

	for i = 0ul, n_months-1 do begin
		dum = get_data(yy[i],mm[i],data=dat,/nan )
		idx = where(finite(dum),idxcnt)
		if idxcnt gt 0 then gm[i] = gmean(dum[idx],lat[idx]) 
		array = is_defined(array) ? [ [[array]],[[dum]] ] : dum
		hist += histogram(dum[idx],min=mini,max=maxi,bin=bin)
	endfor

	avg = mean(array,dimension=3,/nan)
	idx = where(~finite(avg),idxcnt)
	if idxcnt gt 0 then avg[idx] = -999.

	obj_destroy,obj

	win,1,/full
	!p.multi=[0,2,2]
	plot,gm,/ynoz
	view2d,avg
	x = findgen(n_elements(hist)) * bin +mini
	plot,x,100.*hist/total(hist),/xs

end
;-----------------------------------------------------------------------------------------------------------------
function joint_rgb_product, rgb, product, no_data_value = no_data_value, col_table = col_table, $
			mini = mini, maxi= maxi, brewer = brewer, add_2nd = add_2nd
	
	result =-1

	ndv = keyword_set(no_data_value) ? no_data_value[0] : -999.
	idx = where(product ne ndv,idxcnt)
	if idxcnt gt 0 then begin

		prd = bw_to_color(product, mini = mini, maxi= maxi, col_table = col_table, brewer = brewer)

		r=bytscl(hist_equal(rgb[*,*,0]))
		g=bytscl(hist_equal(rgb[*,*,1]))
		b=bytscl(hist_equal(rgb[*,*,2]))

		r[idx] = (prd[*,*,0])[idx]
		g[idx] = (prd[*,*,1])[idx]
		b[idx] = (prd[*,*,2])[idx]
		if keyword_set(add_2nd) then begin
			;add_2nd must be a structure incl. mini,maxi,col_table,brewer
			if is_struct(add_2nd) then begin
				idx = where(add_2nd.image ne ndv,idxcnt)
				if idxcnt gt 0 then begin
					prd = bw_to_color(add_2nd.image, mini = add_2nd.mini, maxi= add_2nd.maxi, $
										col_table = add_2nd.col_table, brewer = add_2nd.brewer)
					r[idx] = (prd[*,*,0])[idx]
					g[idx] = (prd[*,*,1])[idx]
					b[idx] = (prd[*,*,2])[idx]
				endif
			endif
		endif

		result = [[[r]],[[g]],[[b]]]

	endif

	return,result

end
;-----------------------------------------------------------------------------------------------------------------
;copied from map_image
;-----------------------------------------------------------------------------------------------------------------
pro map_image_colors, $
             rainbow = rainbow, $
             extended_rainbow = extended_rainbow, $
             greyscale = greyscale, $
             elevation = elevation, $
             bwr = bwr, $
             ctable = ctable, $
             discrete = discrete, $
             red = red, green = green, blue = blue, $
             revert_background = revert_background, $
             flip_colours = flip_colours, $
             brewer = brewer, $
             ctp = ctp, $
             cth = cth, $
             void_color = void_color,ncolors=ncolors,bottom=bottom,mini=mini,maxi=maxi

	if keyword_set(brewer) then begin
		ctable = keyword_set(ctable) ? ctable : 4
		if ctable lt 0 then begin & ctable = abs(ctable) & flip_colours = 1 & end
		file = !BREWER_CT_FILE
	endif

	if keyword_set(rainbow) then begin
		r = interpol([  0,   0,   0,   0,    0,  200, 255, 255, 255, 180, 100], indgen(11), findgen(254) / 253 * 10)
		g = interpol([  0,   0, 125, 255, 255,  255, 255, 125,   0,   0,    0], indgen(11), findgen(254) / 253 * 10)
		b = interpol([125, 255, 255, 255,    0,    0,  0,   0,   0,   0,    0], indgen(11), findgen(254) / 253 * 10)
		r = [0, r, 255] & g = [0, g, 255] & b = [0, b, 255]
		color_save = !p.color & tvlct, r, g, b & !p.color = color_save
	endif

	if keyword_set(extended_rainbow) then begin
		r = interpol([152, 217, 105,   0,   0,   7, 109, 210, 250, 250, 250, 221, 156, 100, 80], indgen(15), findgen(254) / 253 * 14)
		g = interpol([  0,   0,   0, 125, 250, 245, 224, 250, 216, 159, 101,  46,   0,   0, 34], indgen(15), findgen(254) / 253 * 14)
		b = interpol([ 91, 230, 244, 250, 250, 140,  31,   0,   0,   0,   0,  46,   0,   0, 40], indgen(15), findgen(254) / 253 * 14)
		r = [0, r, 255] & g = [0, g, 255] & b = [0, b, 255]
		color_save = !p.color & tvlct, r, g, b & !p.color = color_save
	endif

	if keyword_set(elevation) then begin
		r = bytarr(256) + 255b
		g = bytarr(256) + 255b
		b = bytarr(256) + 255b
		r[2] = 0 & g[2] = 0 & b[2] = 150
		r[3:20] = 0 & g[3:20] = bindgen(18) * (190.-120.) / 17. + 120 & b[3:20] = 0
		r[21:40] = bindgen(20) * (220.) / 19. & g[21:40] = 190 & b[21:40] = 0
		r[41:60] = 220 - bindgen(20) * (30) / 19. & g[41:60] = 190 - bindgen(20) * (80) / 19. & b[41:60] = 0
		r[61:80] = bindgen(20) * 65 / 19. + 190
		g[61:80] = bindgen(20) * 145 / 19. + 110
		b[61:80] = bindgen(20) * 255 / 19.
		r[81:*]  = 255b & g[81:*] = 255b & b[81:*] = 255b
		color_save = !p.color & tvlct, r, g, b & !p.color = color_save
	endif

	if keyword_set(bwr) then begin
		saturation = fltarr(ncolors)
		value = intarr(ncolors) + 1
		hue = intarr(ncolors)
		white_index = round((ncolors * (-1. *  mini ) / (maxi - mini))>0)
		hue       [0 : white_index] = 240
		saturation[0 : white_index] = (1. - findgen(white_index+1) / white_index) * ncolors / 253.
		saturation[white_index + 1 : ncolors - 1] = findgen(ncolors-white_index-1) / (ncolors-white_index-2) * ncolors / 253.
		color_convert, hue, saturation, value, r, g, b, /hsv_rgb
		rr = intarr(254) & gg = intarr(254) & bb = intarr(254)
		rr[bottom-1] = r
		gg[bottom-1] = g
		bb[bottom-1] = b
		r = [0, rr, 255] & g = [0, gg, 255] & b = [0, bb, 255]
		color_save = !p.color & tvlct, r, g, b & !p.color = color_save
	endif

	if keyword_set(ctable) then begin
		if ctable lt 0 then begin & ctable = abs(ctable) & flip_colours = 1 & end
		loadct, ctable, file = file, /silent
	endif

	if keyword_set(greyscale) then begin
		loadct, 0, /silent
	endif

	if keyword_set(discrete) then begin
		mini  = min(discrete, max = maxi)
		n_lev = n_elements(discrete) - 1
		nnn   = n_elements(discrete) - 1

		; Setting number of colors such that colorbar shows the steps at the correct position
		ncolors = floor(float(ncolors) / nnn) * nnn + 1
		ddd = bytscl([mini, discrete, maxi], $
			min = mini, $
			max = maxi, $
			top = ncolors - 1, /nan) + bottom
		ddd = ddd[1:nnn+1]
		idx = intarr(256) - 1
		for i = 0, nnn-1 do if ddd[i] lt ddd[i+1] then idx[ddd[i]:ddd[i+1]] = i
		;Pick the colours from within the chosen intervals
		tvlct, red, green, blue, /get
		index = round(findgen(nnn) / (nnn - 1.) * (ncolors - 1) + bottom)
		red   = red  [index]
		green = green[index]
		blue  = blue [index]

		r = red[idx>0] & g = green[idx>0] & b = blue[idx>0]
		bad = where(idx eq -1)
		if bad[0] ne -1 then begin
			r[bad] = void_color
			g[bad] = void_color
			b[bad] = void_color
		endif
		color_save = !p.color & tvlct, r, g, b & !p.color = color_save
	endif

	if keyword_set(flip_colours) then begin
		tvlct, r, g, b, /get
		color_save = !p.color & tvlct, reverse(r), reverse(g), reverse(b) & !p.color = color_save
	endif

	;Set white, black and void-color:
	tvlct, r, g, b, /get
	r[0] = 0 & g[0] = 0 & b[0] = 0
	r[255] = 255 & g[255] = 255 & b[255] = 255
	r[1] = void_color & g[1] = void_color & b[1] = void_color
	color_save = !p.color & tvlct, r, g, b & !p.color = color_save

	if keyword_set(revert_background) then begin
		if strupcase(!d.name) eq 'PS' then begin
			print, "[map_image::colors]:Don't revert colours when using postscript"
		endif else begin
			background = !p.background
			color = !p.color
			!p.background = color
			!p.color = background
		endelse
	endif

end
;-----------------------------------------------------------------------------------------------------
;-------------------Create-Time-Series----------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
pro create_cci_vs_gac_or_aqua_time_series,data,climatology,reference,satellite,coverage,period=period

	vali_set_path ; nur zur sicherheit das auch alle pfade gesetzt sind
	vali_set_defaults

	per    = keyword_set(period) ? period : !default_ts_period
	yspl   = fix(strsplit(per,'-',/ext))
	years  = string(indgen(yspl[1]-yspl[0]+1)+yspl[0],f='(i4.4)')
	months = string(indgen(12)+1,f='(i2.2)')

	cov    = strlowcase(coverage)
	sat    = strlowcase(satellite)
	apxc   = '' ; appendix to climatology name, e.g. version
	apxr   = '' ; appendix to reference name, e.g. version
	ref    = algo2ref(reference)
	cli    = algo2ref(climatology)
	dat    = strlowcase(data)

	if cli eq 'mod'  then gridc = 1.0
	if cli eq 'myd'  then gridc = 1.0
	if cli eq 'mod2' then gridc = 1.0
	if cli eq 'myd2' then gridc = 1.0
	if cli eq 'pmx'  then gridc = 1.0
	if cli eq 'gac'  then gridc = 0.25
	if cli eq 'gac2' then gridc = 0.25
	if cli eq 'cci'  then gridc = 0.5
	if cli eq 'cciv3' then gridc = 0.5
	if cli eq 'era'  then gridc = 0.5
	if cli eq 'era2' then gridc = 0.5
	if cli eq 'cla'  then gridc = 0.25
	if cli eq 'isp'  then gridc = 1.0
	if cli eq 'isp_old'  then gridc = 2.5
	if cli eq 'cal'  then gridc = 2.0
	if cli eq 'hec'  then gridc = 0.5
	if ~keyword_set(gridc) then begin
		print,'Gridc not defined! Unknown Climatology?'
		return
	endif

	if ref eq 'mod'  then gridr = 1.0
	if ref eq 'myd'  then gridr = 1.0
	if ref eq 'mod2' then gridr = 1.0
	if ref eq 'myd2' then gridr = 1.0
	if ref eq 'pmx'  then gridr = 1.0
	if ref eq 'gac'  then gridr = 0.25
	if ref eq 'gac2' then gridr = 0.25
	if ref eq 'era'  then gridr = 0.5
	if ref eq 'era2' then gridr = 0.5
	if ref eq 'cci'  then gridr = 0.5
	if ref eq 'cciv3' then gridr = 0.5
	if ref eq 'cla'  then gridr = 0.25
	if ref eq 'isp'  then gridr = 1.0
	if ref eq 'isp_old'  then gridr = 2.5
	if ref eq 'cal'  then gridr = 2.0
	if ref eq 'hec'  then gridr = 0.5
	if ~keyword_set(gridc) then begin
		print,'Gridr not defined! Unknown Reference?'
		return
	endif

	grid = max([gridc,gridr])
	help,grid	
	make_geo,lon,lat,grid=grid
	dem = get_coverage(lon, lat, /land)

	vollername = full_varname(dat)

	case strmid(dat,0,3) of
		'cot'	: histv = [0.1,0.,100.]
		'cth'	: histv = [0.1,0.,20.]  
		'cwp'	: histv = [1.,0.,1000.]  
		'iwp'	: histv = [1.,0.,1000.]  
		'lwp'	: histv = [1.,0.,1000.]  
		'ctp'	: histv = [1.,100.,1000.]
		'ctt'	: histv = [1.,180.,330.] 
		'cfc'	: histv = [0.01,0.,1.]   
		'cph'	: histv = [0.01,0.,1.]   
		'cer'	: histv = [0.1,0.,80.]   
		'sal'	: histv = [0.1,0.,100.]  
		else : begin & print, 'tbd' & stop & end
	endcase

	satcci = sat
	satgac = ( ( total(satcci eq ['aatme','aatsr']) and total(ref eq ['pmx','gac','gac2']) ) ? 'noaaam' : sat )
	; fame-c only daytime, set ref dat to _day (!!but only if dat is not already a microphysical variable!!), and compare if available
	dt1 = (satgac eq 'aatme' and ~stregex(dat,'_day',/bool,/fold) and total(dat eq ['cfc','ctp','ctt','cth','cph']) ? '_day':'')
	dt2 = (satcci eq 'aatme' and ~stregex(dat,'_day',/bool,/fold) and total(dat eq ['cfc','ctp','ctt','cth','cph']) ? '_day':'')
	lev = total(sat eq ['avhrrs','modises','allsat']) ? 'l3s' : 'l3c'

	trend_sat = 1
	if total(cli eq ['cci','cciv3','gac','gac2','pmx']) and $
	  ~total(satcci eq ['noaaam','noaapm','aatme','aatsr','terra','aqua','avhrrs','modises','allsat']) then trend_sat = 0
	if keyword_set(period) then trend_sat=0 ; do only on full time series; '1978-2016'
	if apxc ne '' or apxr ne '' then begin
		trend_sat = 0
	endif

	algon1 = sat_name(cli,satcci)
	algon2 = sat_name(ref,satgac)

	dat1 = dat+dt1
	dat2 = dat+dt2
	if total(cli eq ['cci','cciv3','gac2']) and total(satcci eq ['noaaam','aatme']) and (ref eq 'mod2') then begin
		if dat2 eq 'cwp_allsky' then dat2 = 'cwp_16_allsky'
		if dat2 eq 'iwp_allsky' then dat2 = 'iwp_16_allsky'
		if dat2 eq 'lwp_allsky' then dat2 = 'lwp_16_allsky'
		if dat2 eq 'cwp' then dat2 = 'cwp_16'
		if dat2 eq 'iwp' then dat2 = 'iwp_16'
		if dat2 eq 'lwp' then dat2 = 'lwp_16'
		if dat2 eq 'cot' then dat2 = 'cot_16'
		if dat2 eq 'cer' then dat2 = 'cer_16'
		if dat2 eq 'cot_liq' then dat2 = 'cot_16_liq'
		if dat2 eq 'cer_liq' then dat2 = 'cer_16_liq'
		if dat2 eq 'cot_ice' then dat2 = 'cot_16_ice'
		if dat2 eq 'cer_ice' then dat2 = 'cer_16_ice'
	endif
	if total(cli eq ['cci','cciv3','gac2']) and (satcci eq 'noaapm') and (ref eq 'myd2') then begin
		if dat2 eq 'cwp_allsky' then dat2 = 'cwp_37_allsky'
		if dat2 eq 'iwp_allsky' then dat2 = 'iwp_37_allsky'
		if dat2 eq 'lwp_allsky' then dat2 = 'lwp_37_allsky'
		if dat2 eq 'cwp' then dat2 = 'cwp_37'
		if dat2 eq 'iwp' then dat2 = 'iwp_37'
		if dat2 eq 'lwp' then dat2 = 'lwp_37'
		if dat2 eq 'cot' then dat2 = 'cot_37'
		if dat2 eq 'cer' then dat2 = 'cer_37'
		if dat2 eq 'cot_liq' then dat2 = 'cot_37_liq'
		if dat2 eq 'cer_liq' then dat2 = 'cer_37_liq'
		if dat2 eq 'cot_ice' then dat2 = 'cot_37_ice'
		if dat2 eq 'cer_ice' then dat2 = 'cer_37_ice'
	endif
	if (cli eq 'cci' or cli eq 'cciv3') and total(satcci eq ['terra','aqua','aatsr','atsrs']) and (ref eq 'myd2' or ref eq 'mod2') then begin
		if dat2 eq 'cwp_allsky' then dat2 = 'cwp_37_allsky'
		if dat2 eq 'iwp_allsky' then dat2 = 'iwp_37_allsky'
		if dat2 eq 'lwp_allsky' then dat2 = 'lwp_37_allsky'
		if dat2 eq 'cwp' then dat2 = 'cwp_37'
		if dat2 eq 'iwp' then dat2 = 'iwp_37'
		if dat2 eq 'lwp' then dat2 = 'lwp_37'
		if dat2 eq 'cot' then dat2 = 'cot_37'
		if dat2 eq 'cer' then dat2 = 'cer_37'
		if dat2 eq 'cot_liq' then dat2 = 'cot_37_liq'
		if dat2 eq 'cer_liq' then dat2 = 'cer_37_liq'
		if dat2 eq 'cot_ice' then dat2 = 'cot_37_ice'
		if dat2 eq 'cer_ice' then dat2 = 'cer_37_ice'
	endif

	nyears  = n_elements(years)
	nmonths = n_elements(months)
	dim_cov = n_elements(cov)
	dim2d   = size(lon,/dim)
	dims    = [dim2d,dim_cov]
	dim0    = 12 ;[cci-gmean,cci-stdd,cci-unc-gmean,cci-unc-stdd,gac-gmean,gac-stdd,gac-unc-gmean,gac-unc-stdd,gbias,grmse,bc_rmse,correlate]

	stats           = fltarr(dim0,nyears*nmonths,dim_cov) + !values.f_nan
	gstats          = fltarr(dim0,nyears*nmonths,dim_cov) + !values.f_nan
	cci_mean_2d     = fltarr(dims)
	cci_mean2_2d    = fltarr(dims)
	cci_unce_2d     = fltarr(dims)
	cci_trend_2d    = fltarr([dim2d,nyears*nmonths]) +!values.f_nan
	gac_mean_2d     = fltarr(dims)
	gac_mean2_2d    = fltarr(dims)
	gac_unce_2d     = fltarr(dims)
	gac_trend_2d    = fltarr([dim2d,nyears*nmonths]) +!values.f_nan
	anz_2d          = fltarr(dims)
	cci_anz_unce_2d = fltarr(dims)
	gac_anz_unce_2d = fltarr(dims)
	mini            = histv[1]
	maxi            = histv[2]
	bin             = histv[0]
	nbins           = ( (maxi - mini) / bin ) +1
	histo1          = lonarr(nbins,dim_cov)
	histo2          = lonarr(nbins,dim_cov)
	histo2D         = lonarr(nbins,nbins,dim_cov)
	sum_diff        = dblarr(dim_cov)
	sum_diff2       = dblarr(dim_cov)
	sum_cci         = dblarr(dim_cov)
	sum2_cci        = dblarr(dim_cov)
	sum_gac         = dblarr(dim_cov)
	sum2_gac        = dblarr(dim_cov)
	nnn             = ulonarr(dim_cov)
	sum_prod        = dblarr(dim_cov)
	maxv1           = dblarr(dim_cov) -999.
	maxv2           = dblarr(dim_cov) -999.
	minv1           = dblarr(dim_cov) +99999.
	minv2           = dblarr(dim_cov) +99999.
	gsum_cci        = dblarr(dim_cov)
	gsum_gac        = dblarr(dim_cov)
	gsum2_cci       = dblarr(dim_cov)
	gsum2_gac       = dblarr(dim_cov)
	gsum_diff       = dblarr(dim_cov)
	gsum_diff2      = dblarr(dim_cov)
	gsum_prod       = dblarr(dim_cov)
	weight          = fltarr(dim_cov)

; 	if cli eq 'cci' then cci_dirname='/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C' else free,cci_dirname
; 	if ref eq 'cci' then gac_dirname='/cmsaf/cmsaf-cld7/esa_cloud_cci/data/v2.0/L3C' else free,gac_dirname
	first = 1
	counti=0ul
	for yy1=0,nyears-1,1 do begin
	    for mm1=0,nmonths-1,1 do begin

		yyyy=years[yy1]
		mmmm=months[mm1]
		cci_dum_file = get_filename(yyyy,mmmm,data=dat1,algo=cli,sat=satcci,level=lev,found=found_cci,/silent,dirname=cci_dirname,/no_recursive)

		if (cli eq 'cci' or cli eq 'cciv3') and found_cci then begin
			num = get_ncdf_data_by_name(cci_dum_file,'number_of_processed_orbits',/global)
			if num lt 100 then begin
				print,'File has only '+string(num)+' Orbits, and will be skipped! ',cci_dum_file
				found_cci = 0
			endif
		endif
		gac_dum_file = get_filename(yyyy,mmmm,data=dat2,algo=ref,sat=satgac,level=lev,found=found_gac,/silent,dirname=gac_dirname,/no_recursive)
		if (ref eq 'cci' or ref eq 'cciv3') and found_gac then begin
			num = get_ncdf_data_by_name(gac_dum_file,'number_of_processed_orbits',/global)
			if num lt 100 then begin
				print,'File has only '+string(num)+' Orbits, and will be skipped! ',gac_dum_file
				found_gac = 0
			endif
		endif
		if found_cci and found_gac then begin
			cci_tmp = get_data(yyyy,mmmm,file=cci_dum_file,data=dat1,algo=cli,sat=satcci,level=lev,found=found_cci,glob=grid,$
					/mean,/make_compare,no_data_val=fv_cci,/silent,unit=unit,dirname=cci_dirname)
			cci_unc = get_data(yyyy,mmmm,data=dat1+'_unc',algo=cli,sat=satcci,level=lev,found=found_cci_unc,glob=grid,/mean,$
				   /make_compare,no_data_val=fv_cci_unc,/silent,dirname=cci_dirname)
			gac_tmp = get_data(yyyy,mmmm,file=gac_dum_file,data=dat2,algo=ref,sat=satgac,level=lev,found=found_gac,glob=grid,$
					/mean,/make_compare,no_data_val=fv_gac,/silent,dirname=gac_dirname)
			gac_unc = get_data(yyyy,mmmm,data=dat2+'_unc',algo=ref,sat=satgac,level=lev,found=found_gac_unc,glob=grid,/mean,$
				   /make_compare,no_data_val=fv_gac_unc,/silent,dirname=gac_dirname)
			if found_cci and found_gac then begin
				if (first) and (trend_sat) then begin 
					; save some memory, e.g. cal has grid = 0.05 , arr with [3600,3600,468] = 22.595 GiB RAM
					tr_start  = counti/12*12
					cci_trend_2d  = fltarr([dim2d,nyears*nmonths-tr_start]) +!values.f_nan
					gac_trend_2d  = fltarr([dim2d,nyears*nmonths-tr_start]) +!values.f_nan
					first     = 0
				endif
				print, 	strupcase(dat)+': '+yyyy+'/'+mmmm+' '+sat+' - '+ $
					cli+' ('+get_product_name(dat1,algo=cli)+') vs '+$
					ref+' ('+get_product_name(dat2,algo=ref)+')'
				print,'"'+cci_dum_file+'" , "'+gac_dum_file+'"'
				for ii = 0, dim_cov -1 do begin
					area         = get_coverage( lon, lat, dem = dem, coverage = cov[ii],found = found)
					anz_tmp      = ( (gac_tmp ne fv_gac[0]) and (cci_tmp ne fv_cci[0]) and (area eq 1) )
					dum_cci_tmp  = anz_tmp * cci_tmp
					dum_gac_tmp  = anz_tmp * gac_tmp
					good_idx     = where(anz_tmp,good_count)
					if good_count gt 0 then begin
						stats [0,counti,ii] = mean  (cci_tmp[good_idx])
						stats [1,counti,ii] = stddev(cci_tmp[good_idx])
						gstats[0,counti,ii] = gmean  (cci_tmp[good_idx],lat[good_idx])
						gstats[1,counti,ii] = gstddev(cci_tmp[good_idx],lat[good_idx])
						if found_cci_unc then begin
							dum_anz_unc_cci = ( anz_tmp and (cci_unc ne fv_cci_unc[0]) )
							dum_cci_unc     = dum_anz_unc_cci * cci_unc
							idx2            = where(dum_anz_unc_cci,idx2_cnt)
							if idx2_cnt gt 0 then stats[2,counti,ii]       = mean   (cci_unc[idx2])
							if idx2_cnt gt 0 then stats[3,counti,ii]       = stddev (cci_unc[idx2])
							if idx2_cnt gt 0 then gstats[2,counti,ii]      = gmean  (cci_unc[idx2],lat[idx2])
							if idx2_cnt gt 0 then gstats[3,counti,ii]      = gstddev(cci_unc[idx2],lat[idx2])
							if idx2_cnt gt 0 then cci_unce_2d[*,*,ii]     += dum_cci_unc
							if idx2_cnt gt 0 then cci_anz_unce_2d[*,*,ii] += dum_anz_unc_cci
						endif
						stats[4,counti,ii]  = mean  (gac_tmp[good_idx])
						stats[5,counti,ii]  = stddev(gac_tmp[good_idx])
						gstats[4,counti,ii] = gmean  (gac_tmp[good_idx],lat[good_idx])
						gstats[5,counti,ii] = gstddev(gac_tmp[good_idx],lat[good_idx])
						if found_gac_unc then begin
							dum_anz_unc_gac = ( anz_tmp and (gac_unc ne fv_gac_unc[0]) )
							dum_gac_unc = dum_anz_unc_gac * gac_unc
							idx2        = where(dum_anz_unc_gac,idx2_cnt)
							if idx2_cnt gt 0 then stats[6,counti,ii]       = mean   (gac_unc[idx2])
							if idx2_cnt gt 0 then stats[7,counti,ii]       = stddev (gac_unc[idx2])
							if idx2_cnt gt 0 then gstats[6,counti,ii]      = gmean  (gac_unc[idx2],lat[idx2])
							if idx2_cnt gt 0 then gstats[7,counti,ii]      = gstddev(gac_unc[idx2],lat[idx2])
							if idx2_cnt gt 0 then gac_unce_2d[*,*,ii]     += dum_gac_unc
							if idx2_cnt gt 0 then gac_anz_unce_2d[*,*,ii] += dum_anz_unc_gac
						endif
						stats[8,counti,ii]   = bias(cci_tmp[good_idx],gac_tmp[good_idx])
						stats[9,counti,ii]   = rmse(cci_tmp[good_idx],gac_tmp[good_idx])
						stats[10,counti,ii]  = bc_rmse(stats[8,counti,ii],stats[9,counti,ii])
						stats[11,counti,ii]  = correlate(cci_tmp[good_idx],gac_tmp[good_idx])
						gstats[8,counti,ii]  = gbias(cci_tmp[good_idx],gac_tmp[good_idx],lat[good_idx])
						gstats[9,counti,ii]  = grmse(cci_tmp[good_idx],gac_tmp[good_idx],lat[good_idx])
						gstats[10,counti,ii] = bc_rmse(gstats[8,counti,ii],gstats[9,counti,ii])
						gstats[11,counti,ii] = gcorrelate(cci_tmp[good_idx],gac_tmp[good_idx],lat[good_idx])
						histo1[*,ii]        += histogram(float(cci_tmp[good_idx]),min=mini,max=maxi,nbins=nbins)
						histo2[*,ii]        += histogram(float(gac_tmp[good_idx]),min=mini,max=maxi,nbins=nbins)
						histo2D[*,*,ii]     += hist_2d(float(cci_tmp[good_idx]),float(gac_tmp[good_idx]),min1=mini,max1=maxi,bin1=bin,min2=mini,max2=maxi,bin2=bin)
						cci_mean_2d[*,*,ii] += dum_cci_tmp
						gac_mean_2d[*,*,ii] += dum_gac_tmp
						cci_mean2_2d[*,*,ii]+= (dum_cci_tmp)^2.
						gac_mean2_2d[*,*,ii]+= (dum_gac_tmp)^2.
						anz_2d[*,*,ii]      += anz_tmp
						sum_diff[ii]        += total((cci_tmp[good_idx]-gac_tmp[good_idx]))
						sum_diff2[ii]       += total((cci_tmp[good_idx]-gac_tmp[good_idx])^2.)
						sum_cci[ii]         += total((cci_tmp[good_idx]))
						sum2_cci[ii]        += total((cci_tmp[good_idx])^2.)
						sum_gac[ii]         += total((gac_tmp[good_idx]))
						sum2_gac[ii]        += total((gac_tmp[good_idx])^2.)
						sum_prod[ii]        += total((cci_tmp[good_idx] * gac_tmp[good_idx]))
						maxv1[ii]            = max([maxv1[ii],max(cci_tmp[good_idx])])
						maxv2[ii]            = max([maxv2[ii],max(gac_tmp[good_idx])])
						minv1[ii]            = min([minv1[ii],min(cci_tmp[good_idx])])
						minv2[ii]            = min([minv2[ii],min(gac_tmp[good_idx])])
						;latitude weighted
						weight[ii]          += total(cosd(lat[good_idx]))
						gsum_cci[ii]        += total((cci_tmp[good_idx])    * cosd(lat[good_idx]))
						gsum2_cci[ii]       += total((cci_tmp[good_idx])^2. * cosd(lat[good_idx]) )
						gsum_gac[ii]        += total((gac_tmp[good_idx])    * cosd(lat[good_idx]))
						gsum2_gac[ii]       += total((gac_tmp[good_idx])^2. * cosd(lat[good_idx]) )
						gsum_diff[ii]       += total((cci_tmp[good_idx] - gac_tmp[good_idx])    * cosd(lat[good_idx]) )
						gsum_diff2[ii]      += total((cci_tmp[good_idx] - gac_tmp[good_idx])^2. * cosd(lat[good_idx]) )
						gsum_prod[ii]       += total((cci_tmp[good_idx] * gac_tmp[good_idx])    * cosd(lat[good_idx]) )
						nnn[ii]             += good_count
						; das muss zuletzt kommen 
						if (first eq 0) and (cov[ii] eq 'full') then begin
							dum_idx = where(anz_tmp eq 0,dum_idx_cnt)
							if dum_idx_cnt gt 0 then dum_cci_tmp[dum_idx] = !values.f_nan
							if dum_idx_cnt gt 0 then dum_gac_tmp[dum_idx] = !values.f_nan
							cci_trend_2d[*,*,counti-tr_start] = dum_cci_tmp
							gac_trend_2d[*,*,counti-tr_start] = dum_gac_tmp
						endif
					endif
					free, anz_tmp
					free, dum_cci_tmp
					free, dum_gac_tmp
					free, dum_anz_unc_cci
					free, dum_cci_unc
					free, dum_anz_unc_gac
					free, area
				endfor
				free,cci_tmp
				free,cci_unc
				free,gac_tmp
				free,gac_unc
			endif
		endif
		free,cci_dum_file
		free,gac_dum_file
		counti++
	    endfor
	endfor

	;linear_trends
	if (first eq 0) then begin
		idx  = where(finite(stats[0,*,0]),idxcnt)
		if idxcnt eq 0 then return
		mima = minmax(idx)/12 * 12
		mima[1] = mima[1]+11
		if total(cli eq ['cci','cciv3','gac','gac2','pmx']) then begin
			ect = restore_var(!SAVS_DIR + 'time_series/plot_monthly_ECTs_cci_primes_'+satcci+'.sav',found=found)
			if found then ect = ect[mima[0]:mima[1]] else free,ect
		endif
		cci_tr_dum = give_trend(cci_trend_2d[*,*,0:mima[1]-tr_start],mini,maxi,years[mima[0]/12],years[mima[1]/12],ect)
		cci_lfit   = cci_tr_dum.slope *10.
		dummean    = mean(cci_trend_2d,dim=3,/nan)
		idx = where(~finite(dummean),idxcnt)
		if idxcnt gt 0 then cci_lfit[idx] = -999.
		dumm    = cci_tr_dum.anom
		idx = where(dumm eq -999.,idxcnt)
		if idxcnt gt 0 then dumm[idx] = !values.f_nan
		cci_anom_mean = mean(dumm,dim=3,/nan)
		idx = where(~finite(cci_anom_mean),idxcnt)
		if idxcnt gt 0 then cci_anom_mean[idx] = -999.
		free, dumm
		free, cci_trend_2d
		free,ect
		if total(ref eq ['cci','cciv3','gac','gac2','pmx']) then begin
			ect = restore_var(!SAVS_DIR + 'time_series/plot_monthly_ECTs_cci_primes_'+satgac+'.sav',found=found)
			if found then ect = ect[mima[0]:mima[1]] else free,ect
		endif
		gac_tr_dum = give_trend(gac_trend_2d[*,*,0:mima[1]-tr_start],mini,maxi,years[mima[0]/12],years[mima[1]/12],ect)
		gac_lfit   = gac_tr_dum.slope * 10.
		dummean    = mean(gac_trend_2d,dim=3,/nan)
		idx = where(~finite(dummean),idxcnt)
		if idxcnt gt 0 then gac_lfit[idx] = -999.
		dumm       = gac_tr_dum.anom
		idx        = where(dumm eq -999.,idxcnt)
		if idxcnt gt 0 then dumm[idx] = !values.f_nan
		gac_anom_mean = mean(dumm,dim=3,/nan)
		idx = where(~finite(gac_anom_mean),idxcnt)
		if idxcnt gt 0 then gac_anom_mean[idx] = -999.
		free, dumm
		free, gac_trend_2d
		free,ect
	endif

	for ii = 0, dim_cov -1 do begin
		if nnn[ii] le 100 then continue

		hist1        = {data:reform(histo1[*,ii])   ,minvalue:mini[0],maxvalue:maxi[0],bin:bin[0]}
		hist2        = {data:reform(histo2[*,ii])   ,minvalue:mini[0],maxvalue:maxi[0],bin:bin[0]}
		hist2d       = {data:reform(histo2D[*,*,ii]),minvalue:mini[0],maxvalue:maxi[0],bin:bin[0]}
		percentiles  = get_perc_from_hist(reform(histo1[*,ii]),[.5,.75,.25,.975,.025],mini,maxi,bin,data=cci_all)
		percentiles2 = get_perc_from_hist(reform(histo2[*,ii]),[.5,.75,.25,.975,.025],mini,maxi,bin,data=gac_all)

		if n_elements(cci_all) ne n_elements(gac_all) then begin
			print,'All: Dimensions do not agree! , set larger to lower Dims!'
			help,cci_all,gac_all
			mi = min([n_elements(cci_all),n_elements(gac_all)])
			cci_all = cci_all[0:(mi-1)]
			gac_all = gac_all[0:(mi-1)]
		endif
		regr         = linfit(cci_all,gac_all,chisqr=chisqr,prob=prob,sigma=sigma,covar=covar)
		hist2d       = create_struct(hist2d,'linfit',{regr:regr,chisqr:chisqr,prob:prob,sigma:sigma,covar:covar})
		free,cci_all
		free,gac_all

		; arithmetic statistics over all good pixel and monthly means
		anz          = float(nnn[ii])
		;cci
		all_avg_cci  = sum_cci[ii] / (anz > 1.)							; average
		dum_var_cci  = ( (sum2_cci[ii] - anz * all_avg_cci^2.) > 0.)
		all_var_cci  = dum_var_cci / ((anz-1.) > 1.) 					; variance
		all_sdv_cci  = sqrt(all_var_cci)								; stddev
		; gac
		all_avg_gac  = sum_gac[ii] / (anz > 1.)							; average
		dum_var_gac  = ( (sum2_gac[ii] - anz * all_avg_gac^2) > 0.) 
		all_var_gac  = dum_var_gac / ((anz-1.) > 1.) 					; variance
		all_sdv_gac  = sqrt(all_var_gac)								; stddev
		; both
		all_bias     = sum_diff[ii] / anz								; BIAS
		all_rmse     = sqrt(sum_diff2[ii] / anz)						; RMSE
		all_bcrmse   = sqrt((all_rmse)^2. - (all_bias)^2. )				; Bias corrected RMSE
		all_cov      = sum_prod[ii] - anz * all_avg_cci * all_avg_gac	; Verschiebungssatz Covarianz
		all_corr     = all_cov / sqrt( dum_var_cci * dum_var_gac)		; Correlation

		; latitude_weighted
		; cci
		anz = weight[ii]
		gall_avg_cci = gsum_cci[ii] / (anz > 1.)						; average
		gdum_var_cci = ( (gsum2_cci[ii] - anz * gall_avg_cci^2.) > 0.) 
		gall_var_cci = gdum_var_cci / ((anz-1.) > 1.) 					; variance
		gall_sdv_cci = sqrt(gall_var_cci)								; stddev
		; gac
		gall_avg_gac = gsum_gac[ii] / anz 								; average
		gdum_var_gac = ( (gsum2_gac[ii] - anz * gall_avg_gac^2.) > 0.)
		gall_var_gac = gdum_var_gac / ((anz-1.) > 1.) 					; variance
		gall_sdv_gac = sqrt(gall_var_gac)								; stddev
		; both
		gall_bias    = gsum_diff[ii] / anz								; bias
		gall_rmse    = sqrt(gsum_diff2[ii] / anz) 						; rmse
		gall_bcrmse  = sqrt((gall_rmse)^2. - (gall_bias)^2. ) 			; bias corrected rmse or stddev of difference
		gall_cov     = gsum_prod[ii] - anz * gall_avg_cci * gall_avg_gac; Verschiebungssatz Covarianz
		gall_corr    = gall_cov / sqrt( gdum_var_cci * gdum_var_gac)	; Correlation

		gall = {nnn:weight[ii],$
			sum :gsum_cci[ii],sum_sq :gsum2_cci[ii],$
			sum2:gsum_gac[ii],sum2_sq:gsum2_gac[ii],$
			sum_diff:gsum_diff[ii],sum_diff_sq:gsum_diff2[ii],sum_prod:gsum_prod[ii],$
			avgerage :gall_avg_cci,variance :gall_var_cci,stddev :gall_sdv_cci,$
			avgerage2:gall_avg_gac,variance2:gall_var_gac,stddev2:gall_sdv_gac,$
			bias:gall_bias,rmse:gall_rmse,bc_rmse:gall_bcrmse,covariance:gall_cov,correlation:gall_corr}

		all  = {nnn:nnn[ii],$
			sum :sum_cci[ii],sum_sq :sum2_cci[ii],$
			sum2:sum_gac[ii],sum2_sq:sum2_gac[ii],$
			sum_diff:sum_diff[ii],sum_diff_sq:sum_diff2[ii],sum_prod:sum_prod[ii], $
			minvalue :minv1[ii],maxvalue :maxv1[ii],avgerage :all_avg_cci,variance :all_var_cci,stddev :all_sdv_cci,$
			minvalue2:minv2[ii],maxvalue2:maxv2[ii],avgerage2:all_avg_gac,variance2:all_var_gac,stddev2:all_sdv_gac,$
			bias:all_bias,rmse:all_rmse,bc_rmse:all_bcrmse,covariance:all_cov,correlation:all_corr,latitude_weighted:gall}

		; 2D average
		qq = where(reform(anz_2d[*,*,ii] eq 0),c_qq)
		mean_cci = reform(cci_mean_2d[*,*,ii]/float(anz_2d[*,*,ii] > 1))
		mean_gac = reform(gac_mean_2d[*,*,ii]/float(anz_2d[*,*,ii] > 1))
		cci_std  = sqrt( ( (cci_mean2_2d[*,*,ii] - anz_2d[*,*,ii] * mean_cci^2) > 0.) / ((anz_2d[*,*,ii]-1.) > 1.) )
		gac_std  = sqrt( ( (gac_mean2_2d[*,*,ii] - anz_2d[*,*,ii] * mean_gac^2) > 0.) / ((anz_2d[*,*,ii]-1.) > 1.) )
		if c_qq gt 0 then begin
			mean_cci[qq] = -999.
			mean_gac[qq] = -999.
			cci_std[qq]  = -999.
			gac_std[qq]  = -999.
		endif
		qq = where(reform(cci_anz_unce_2d[*,*,ii]) eq 0,c_qq)
		unce_cci  = reform(cci_unce_2d[*,*,ii]/float(cci_anz_unce_2d[*,*,ii] > 1))
		if c_qq gt 0 then unce_cci[qq] = -999.
		qq = where(reform(gac_anz_unce_2d[*,*,ii]) eq 0,c_qq)
		unce_gac  = reform(gac_unce_2d[*,*,ii]/float(gac_anz_unce_2d[*,*,ii] > 1))
		if c_qq gt 0 then unce_gac[qq] = -999.
		
 		; trend
		if (first eq 0) then begin 
			area          = get_coverage( lon, lat, dem = dem, coverage = cov[ii],found = found)
			trend_cci     = cci_lfit * (area eq 1)  + (area eq 0) * (-999.) 
			trend_gac     = gac_lfit * (area eq 1)  + (area eq 0) * (-999.) 
			cci_an_mean2d = cci_anom_mean * (area eq 1)  + (area eq 0) * (-999.) 
			gac_an_mean2d = gac_anom_mean * (area eq 1)  + (area eq 0) * (-999.) 

			tr_dum_stats  = reform( stats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			tr_dum_gstats = reform(gstats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			anom_stats    = reform( stats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			anom_gstats   = reform(gstats[*,mima[0]:mima[1],ii])*0 +!values.f_nan

			for tr = 0,n_elements(tr_dum_stats[0,*])-1 do begin
				cci_dummy = (reform(cci_tr_dum.ANOM[*,*,tr]) + mean_cci) * (area eq 1)  + (area eq 0) * (-999.)
				gac_dummy = (reform(gac_tr_dum.ANOM[*,*,tr]) + mean_gac) * (area eq 1)  + (area eq 0) * (-999.)
				idx = 	where(area eq 1 and mean_cci ne -999. and mean_gac ne -999. and $
					reform(cci_tr_dum.ANOM[*,*,tr]) ne -999. and reform(gac_tr_dum.ANOM[*,*,tr]) ne -999.,idxcnt)
				if idxcnt gt 0 then begin
					tr_dum_stats [ 0,tr] = mean      (cci_dummy[idx])
					tr_dum_stats [ 1,tr] = stddev    (cci_dummy[idx])
					tr_dum_stats [ 4,tr] = mean      (gac_dummy[idx])
					tr_dum_stats [ 5,tr] = stddev    (gac_dummy[idx])
					tr_dum_stats [ 8,tr] = bias      (cci_dummy[idx],gac_dummy[idx])
					tr_dum_stats [ 9,tr] = rmse      (cci_dummy[idx],gac_dummy[idx])
					tr_dum_stats [10,tr] = bc_rmse   (tr_dum_stats[ 8,tr],tr_dum_stats[ 9,tr])
					tr_dum_stats [11,tr] = correlate (cci_dummy[idx],gac_dummy[idx])
					tr_dum_gstats[ 0,tr] = gmean     (cci_dummy[idx],lat[idx])
					tr_dum_gstats[ 1,tr] = gstddev   (cci_dummy[idx],lat[idx])
					tr_dum_gstats[ 4,tr] = gmean     (gac_dummy[idx],lat[idx])
					tr_dum_gstats[ 5,tr] = gstddev   (gac_dummy[idx],lat[idx])
					tr_dum_gstats[ 8,tr] = gbias     (cci_dummy[idx],gac_dummy[idx],lat[idx])
					tr_dum_gstats[ 9,tr] = grmse     (cci_dummy[idx],gac_dummy[idx],lat[idx])
					tr_dum_gstats[10,tr] = bc_rmse   (tr_dum_gstats[ 8,tr],tr_dum_gstats[ 9,tr])
					tr_dum_gstats[11,tr] = gcorrelate(cci_dummy[idx],gac_dummy[idx],lat[idx])
				endif
				cci_dummy = (reform(cci_tr_dum.ANOM[*,*,tr])) * (area eq 1)  + (area eq 0) * (-999.)
				gac_dummy = (reform(gac_tr_dum.ANOM[*,*,tr])) * (area eq 1)  + (area eq 0) * (-999.)
				idx = where(cci_dummy ne -999. and gac_dummy ne -999.,idxcnt)
				if idxcnt gt 0 then begin 
					anom_stats [ 0,tr] = mean      (cci_dummy[idx])
					anom_stats [ 1,tr] = stddev    (cci_dummy[idx])
					anom_stats [ 4,tr] = mean      (gac_dummy[idx])
					anom_stats [ 5,tr] = stddev    (gac_dummy[idx])
					anom_stats [ 8,tr] = bias      (cci_dummy[idx],gac_dummy[idx])
					anom_stats [ 9,tr] = rmse      (cci_dummy[idx],gac_dummy[idx])
					anom_stats [10,tr] = bc_rmse   (anom_stats[ 8,tr],anom_stats[ 9,tr])
					anom_stats [11,tr] = correlate (cci_dummy[idx],gac_dummy[idx])
					anom_gstats[ 0,tr] = gmean     (cci_dummy[idx],lat[idx])
					anom_gstats[ 1,tr] = gstddev   (cci_dummy[idx],lat[idx])
					anom_gstats[ 4,tr] = gmean     (gac_dummy[idx],lat[idx])
					anom_gstats[ 5,tr] = gstddev   (gac_dummy[idx],lat[idx])
					anom_gstats[ 8,tr] = gbias     (cci_dummy[idx],gac_dummy[idx],lat[idx])
					anom_gstats[ 9,tr] = grmse     (cci_dummy[idx],gac_dummy[idx],lat[idx])
					anom_gstats[10,tr] = bc_rmse   (anom_gstats[ 8,tr],anom_gstats[ 9,tr])
					anom_gstats[11,tr] = gcorrelate(cci_dummy[idx],gac_dummy[idx],lat[idx])
				endif
			endfor
			dum_stats                          = reform(stats[*,*,ii])
			dum_stats[*,mima[0]:mima[1]]       = tr_dum_stats
			dum_gstats                         = reform(gstats[*,*,ii])
			dum_gstats[*,mima[0]:mima[1]]      = tr_dum_gstats
			dum_anom_stats                     = reform(stats[*,*,ii])
			dum_anom_stats[*,mima[0]:mima[1]]  = anom_stats
			dum_anom_gstats                    = reform(gstats[*,*,ii])
			dum_anom_gstats[*,mima[0]:mima[1]] = anom_gstats

			trends = {mean:trend_cci,mean2:trend_gac,anom_mean:cci_an_mean2d,anom_mean2:gac_an_mean2d,stats_non_weighted:dum_stats,stats:dum_gstats,$
				  stats_anom_non_weighted:dum_anom_stats,stats_anom:dum_anom_gstats}
		endif else trends = -1

		str_cov = cov[ii]+'_'
		if cov[ii] eq 'full'      then str_cov = '' 
		if cov[ii] eq 'full_land' then str_cov = 'land_'
		if cov[ii] eq 'full_sea'  then str_cov = 'sea_'

		out_struc = {algoname:algon1,algoname2:algon2,varname:dat,longname:vollername,unit:unit,coverage:coverage[ii],percentiles:percentiles,$
			percentiles2:percentiles2,stats:reform(gstats[*,*,ii]),stats_non_weighted:reform(stats[*,*,ii]),mean:temporary(mean_cci),$
			unc:temporary(unce_cci),std:temporary(cci_std),mean2:temporary(mean_gac),unc2:temporary(unce_gac),std2:temporary(gac_std),trend:trends}

		out_struc = create_struct(out_struc,'histogram',hist1)
		out_struc = create_struct(out_struc,'histogram2',hist2)
		out_struc = create_struct(out_struc,'hist_2d',hist2D)
		out_struc = create_struct(out_struc,'Overall_Stats',all)
		sav_file  = !SAVS_DIR + 'time_series/compare/compare_'+dat+'_'+cli+apxc+'_vs_'+ref+apxr+'_time_series_'+sat+'_'+str_cov+strjoin([years[0],(reverse(years))[0]],'-')+'.sav'
		print,'Create: '+sav_file
		save_var,out_struc,sav_file,chmod = '664'
	endfor

	;cleanup
	free,cci_mean_2d 
	free,cci_unce_2d 
	free,gac_mean_2d 
	free,gac_unce_2d 
	free,anz_2d
	free,cci_anz_unce_2d 
	free,gac_anz_unce_2d 

end
; ----------------------------------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------------------------------------------------------
pro create_time_series,data,algon,coverage,period=period

	mem_cur   = memory(/current)
	starttime = systime(1)

	vali_set_path ; nur zur sicherheit das auch alle pfade gesetzt sind
	vali_set_defaults

	per    = keyword_set(period) ? period : !default_ts_period
	yspl   = fix(strsplit(per,'-',/ext))
	years  = string(indgen(yspl[1]-yspl[0]+1)+yspl[0],f='(i4.4)')
	months = string(indgen(12)+1,f='(i2.2)')

	cov    = strlowcase(coverage)
	apx = ''

	dat    = strlowcase(data)
	dum    = strsplit(strlowcase(algon[0]),'-',/ext)
	cli    = algo2ref(dum[0])
	sat    = n_elements(dum) eq 2 ? dum[1] : ''
	algon1 = sat_name(cli,sat)

	if cli eq 'mod'  then grid = 1.0
	if cli eq 'myd'  then grid = 1.0
	if cli eq 'mod2' then grid = 1.0
	if cli eq 'myd2' then grid = 1.0
	if cli eq 'pmx'  then grid = 1.0
	if cli eq 'gac'  then grid = 0.25
	if cli eq 'gac2' then grid = 0.25
	if cli eq 'cci'  then grid = 0.5
	if cli eq 'cciv3' then grid = 0.5
	if cli eq 'era'  then grid = 0.5
	if cli eq 'era2' then grid = 0.5
	if cli eq 'cal'  then grid = 2.0
	if cli eq 'cla'  then grid = 0.05
	if cli eq 'isp'  then grid = 1.0
	if cli eq 'isp_old'  then grid = 2.5
	if cli eq 'hec'  then grid = 0.5
	if ~keyword_set(grid) then begin
		print,'Grid not defined! Unknown Climatology?'
		return
	endif
	make_geo,lon,lat,grid=grid,algo=cli
; 	dem = get_dem(lon,lat,grid=grid)
	dem = get_coverage(lon, lat, /land)

	case strmid(dat,0,3) of
		'cot'	: begin & histv = [0.1,0.,100.]    & vollername = 'Cloud Optical Thickness' & end
		'cth'	: begin & histv = [0.1,0.,20.]     & vollername = 'Cloud Top Height' & end
		'cwp'	: begin & histv = [1.,0.,1000.]    & vollername = 'Cloud Water Path' & end
		'iwp'	: begin & histv = [1.,0.,1000.]    & vollername = 'Cloud Ice Water Path' & end
		'lwp'	: begin & histv = [1.,0.,1000.]    & vollername = 'Cloud Liquid Water Path' & end
		'ctp'	: begin & histv = [1.,100.,1000.]  & vollername = 'Cloud Top Pressure' & end
		'ctt'	: begin & histv = [1.,180.,330.]   & vollername = 'Cloud Top Temperature' & end
		'cfc'	: begin & histv = [0.01,0.,1.]     & vollername = 'Cloud Fraction' & end
		'cph'	: begin & histv = [0.01,0.,1.]     & vollername = 'Liquid Cloud Fraction' & end
		'cer'	: begin & histv = [0.1,0.,80.]     & vollername = 'Cloud Effective Radius' & end
		'sal'	: begin & histv = [0.1,0.,100.]    & vollername = 'Surface Albedo' & end
		'ref'	: begin & histv = [0.1,0.,100.]    & vollername = 'Reflectance' & end
		'rad'	: begin & histv = [1.,180.,350.]   & vollername = 'Bright. Temp.' & end
		'nob'	: begin & histv = [100.,0.,20000.] & vollername = 'Number of Observations' & end
		'nre'	: begin & histv = [100.,0.,20000.] & vollername = 'Number of valid cloudy Retrievals' & end
		'cla'	: begin & histv = [.1,0.,1.]       & vollername = 'Cloud Albedo' & end
		else : begin & print, 'tbd' & stop & end
	endcase

	lev = total(sat eq ['avhrrs','modises','allsat']) ? 'l3s' : 'l3c'
	trend_sat = 1
	if total(cli eq ['cci','cciv3','gac','gac2','pmx']) and ~total(sat eq ['noaaam','noaapm','aatme','atsrs','terra','aqua','avhrrs','modises','allsat']) then trend_sat = 0
	if total(strmid(dat,0,4) eq ['nobs','nret']) then begin
		trend_sat = 0
		sum_up    = 1
	endif else sum_up = 0
	if keyword_set(period) then trend_sat=0 ; do only on full time series; '1978-2016'

	if cli eq 'isp' and total(sat eq ['avhrrs','allsat']) then sat = ''
	if apx ne '' then begin
		trend_sat = 0
	endif
	nyears  = n_elements(years)
	nmonths = n_elements(months)
	dim_cov = n_elements(cov)
	dim2d   = size(lon,/dim)
	dims    = [dim2d,dim_cov]
	dim0    = 4 + sum_up

	stats       = fltarr(dim0,nyears*nmonths,dim_cov) +!values.f_nan
	gstats      = fltarr(dim0,nyears*nmonths,dim_cov) +!values.f_nan
	mean_2d     = fltarr(dims)
	mean2_2d    = fltarr(dims)
	unce_2d     = fltarr(dims)
	anz_2d      = fltarr(dims)
	anz_unce_2d = fltarr(dims)
	mini        = histv[1]
	maxi        = histv[2]
	bin         = histv[0]
	nbins       = ( (maxi - mini) / bin ) +1
	histo       = lonarr(nbins,dim_cov)
	sum         = fltarr(dim_cov)
	sum2        = fltarr(dim_cov)
	gsum        = fltarr(dim_cov)
	gsum2       = fltarr(dim_cov)
	weight      = fltarr(dim_cov)
	maxv        = fltarr(dim_cov) -999.
	minv        = fltarr(dim_cov) +99999.
	nnn         = ulonarr(dim_cov)

	first  = 1
	counti = 0ul
	for yy1=0,nyears-1,1 do begin
	    for mm1=0,nmonths-1,1 do begin

		yyyy=years[yy1]
		mmmm=months[mm1]

		dsat = noaa_primes(yyyy,mmmm,ampm=sat_ampm(sat,/ampm))

		tmp = get_data(	yyyy,mmmm,file=dum_file,data=dat,algo=cli,sat=sat,level=lev,found=found,no_data_val=fv,unit=unit,/silent,$
				/make_compare,dirname=dirname,/no_recursive)
		if found then begin
			if ( mean((tmp eq 0) or (tmp eq fv)) eq 1. ) then begin
				print,algon1+': '+get_product_name(dat,algo=cli)+' '+yyyy+'/'+mmmm+' ### All Zeros or Fillvalues! Skipped! ###'
				found = 0
			ENDIF
		endif
		if (cli eq 'cci' or cli eq 'cciv3') and found then begin
			num = get_ncdf_data_by_name(dum_file,'number_of_processed_orbits',/global)
			if num lt 100 then begin
				print,'File has only '+string(num)+' Orbits, and will be skipped! ',dum_file
				found = 0
			endif
		endif
		if (found) then begin
			if (first) and (trend_sat) then begin
				; save some memory, e.g. cal has grid = 0.05 , arr with [3600,3600,468] = 22.595 GiB RAM
				tr_start  = counti/12*12
				trend_2d  = fltarr([dim2d,nyears*nmonths-tr_start]) +!values.f_nan
				first     = 0
			endif
			print, algon1+': '+get_product_name(dat,algo=cli,lev='l3c')+' '+yyyy+'/'+mmmm+' (File: '+temporary(dum_file)+')'
			unc = get_data(	yyyy,mmmm,data=dat+'_unc',algo=cli,sat=sat,level=lev,found=found_unc,no_data_val=fv_unc,/silent,$
					/make_compare,dirname=dirname,/no_recursive)
			for ii = 0, dim_cov -1 do begin
				area     = get_coverage( lon, lat, dem = dem, coverage = cov[ii],found = found)
				anz_tmp  = ( (tmp ne fv[0]) and (area eq 1) )
				dum_tmp  = anz_tmp * tmp
				good_idx = where(anz_tmp,good_count)
				if good_count gt 0 then begin
					stats[0,counti,ii]  = mean   (tmp[good_idx])
					stats[1,counti,ii]  = stddev (tmp[good_idx])
					gstats[0,counti,ii] = gmean  (tmp[good_idx],lat[good_idx])
					gstats[1,counti,ii] = gstddev(tmp[good_idx],lat[good_idx])
					if found_unc then begin
						dum_anz_unc = ( anz_tmp and (unc ne fv_unc[0]) )
						dum_unc     = dum_anz_unc * unc
						idx2        = where(dum_anz_unc,idx2_cnt)
						if idx2_cnt gt 0 then stats[2,counti,ii]   = mean   (unc[idx2])
						if idx2_cnt gt 0 then stats[3,counti,ii]   = stddev (unc[idx2])
						if idx2_cnt gt 0 then gstats[2,counti,ii]  = gmean  (unc[idx2],lat[idx2])
						if idx2_cnt gt 0 then gstats[3,counti,ii]  = gstddev(unc[idx2],lat[idx2])
						if idx2_cnt gt 0 then unce_2d[*,*,ii]     += dum_unc
						if idx2_cnt gt 0 then anz_unce_2d[*,*,ii] += dum_anz_unc
					endif
					if sum_up then stats[4,counti,ii]  = total(tmp[good_idx])
					if sum_up then gstats[4,counti,ii] = total(tmp[good_idx] * cosd(float(lat[good_idx])))
					histo[*,ii]     += histogram(float(tmp[good_idx]),min=mini,max=maxi,nbins=nbins)
					mean_2d[*,*,ii] += dum_tmp
					mean2_2d[*,*,ii]+= dum_tmp^2
					anz_2d[*,*,ii]  += anz_tmp
					sum[ii]         += total((tmp[good_idx]))
					sum2[ii]        += total((tmp[good_idx])^2.)
					maxv[ii]         = max([maxv[ii],max(tmp[good_idx])])
					minv[ii]         = min([minv[ii],min(tmp[good_idx])])
					nnn[ii]         += good_count
					;gmeans
					weight[ii]      += total(cosd(lat[good_idx]))
					gsum[ii]        += total((tmp[good_idx])    * cosd(lat[good_idx]))
					gsum2[ii]       += total((tmp[good_idx])^2. * cosd(lat[good_idx]))
					; das muss zuletzt kommen 
					if (first eq 0) and (cov[ii] eq 'full') then begin
						dum_idx = where(anz_tmp eq 0,dum_idx_cnt)
						if dum_idx_cnt gt 0 then dum_tmp[dum_idx] = !values.f_nan
						trend_2d[*,*,counti-tr_start] = dum_tmp
					endif
				endif
				free, anz_tmp
				free, dum_tmp
				free, dum_anz_unc
				free, dum_unc
				free, area
			endfor
			free,tmp
			free,unc
		endif
		free,dum_file
		counti++
	    endfor
	endfor

	;trends
	if (first eq 0) then begin
		mima = minmax(where(finite(stats[0,*,0])))/12 * 12
		mima[1] = mima[1]+11
		if total(cli eq ['cci','cciv3','pmx','gac','gac2','hec']) then begin
			ect = restore_var(!SAVS_DIR + 'time_series/plot_monthly_ECTs_cci_primes_'+sat+'.sav',found=found)
			if found then ect = ect[mima[0]:mima[1]] else free,ect
		endif
		tr_dum  = give_trend(trend_2d[*,*,0:mima[1]-tr_start],mini,maxi,years[mima[0]/12],years[mima[1]/12],ect)
		lfit    = tr_dum.slope * 10. ; make it per decade
		dummean = mean(trend_2d,dim=3,/nan)
		idx = where(~finite(dummean),idxcnt)
		if idxcnt gt 0 then lfit[idx] = -999.
		dumm    = tr_dum.anom
		idx = where(dumm eq -999.,idxcnt)
		if idxcnt gt 0 then dumm[idx] = !values.f_nan
		anom_mean = mean(dumm,dim=3,/nan)
		idx = where(~finite(anom_mean),idxcnt)
		if idxcnt gt 0 then anom_mean[idx] = -999.
		season_mean = fltarr([dim2d,12]) + !values.f_nan
		for i = 0,11 do season_mean[*,*,i] = mean(trend_2d[*,*,i:*:12],dim=3,/nan)
		free, trend_2d
		free, dumm
	endif

	for ii = 0, dim_cov -1 do begin

		if nnn[ii] le 100 then continue

		hist = {data:reform(histo[*,ii]),minvalue:mini[0],maxvalue:maxi[0],bin:bin[0]}
		;artihm statistics over all good pixel and monthly means
		anz        = float(nnn[ii])
		all_avg    = sum[ii] / (anz > 1.)					; average
		all_var    = ( (sum2[ii] - anz * all_avg^2) > 0.) / ((anz-1.) > 1.) 	; varianz
		all_sdv    = sqrt(all_var)						; stddev
		;latitude weighted
		anz        = weight[ii]
		gall_avg   = gsum[ii] / anz						; average
		gall_var = ( (gsum2[ii] - anz * gall_avg^2.) > 0.) / ((anz-1.) > 1.) 	; varianz
		gall_sdv = sqrt(gall_var)						; stddev

		gall = {nnn:weight[ii],sum:gsum[ii],sum_sq:gsum2[ii],avgerage:gall_avg,variance:gall_var,stddev:gall_sdv}
		all  = {nnn:nnn[ii],sum:sum[ii],sum_sq:sum2[ii],minvalue:minv[ii],maxvalue:maxv[ii],$
			avgerage:all_avg,variance:all_var,stddev:all_sdv,latitude_weighted:gall}

		;average
		qq      = where(reform(anz_2d[*,*,ii]) eq 0,c_qq)
		cci     = reform(mean_2d[*,*,ii]/float(anz_2d[*,*,ii] > 1))
		cci_std = sqrt( ( (mean2_2d[*,*,ii] - anz_2d[*,*,ii] * cci^2) > 0.) / ((anz_2d[*,*,ii]-1.) > 1.) )
		if c_qq gt 0 then cci[qq] = -999.
		if c_qq gt 0 then cci_std[qq] = -999.
		qq = where(reform(anz_unce_2d[*,*,ii]) eq 0,c_qq)
		unce  = reform(unce_2d[*,*,ii]/float(anz_unce_2d[*,*,ii] > 1))
		if c_qq gt 0 then unce[qq] = -999.
		;stddev

		;trend
		if (first eq 0) then begin 
			area  = get_coverage( lon, lat, dem = dem, coverage = cov[ii],found = found)
			trend = lfit * (area eq 1)  + (area eq 0) * (-999.) 
			an_mean2d = anom_mean * (area eq 1)  + (area eq 0) * (-999.) 
			seasonal_mean2d = season_mean *0. -999.
			for i = 0,11 do seasonal_mean2d[*,*,i] = reform(season_mean[*,*,i]) * (area eq 1)  + (area eq 0) * (-999.)

			tr_dum_stats  = reform( stats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			tr_dum_gstats = reform(gstats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			anom_stats    = reform( stats[*,mima[0]:mima[1],ii])*0 +!values.f_nan
			anom_gstats   = reform(gstats[*,mima[0]:mima[1],ii])*0 +!values.f_nan

			for tr = 0,n_elements(tr_dum_stats[0,*])-1 do begin
				dummy = (reform(tr_dum.ANOM[*,*,tr]) + cci) * area
				idx   = where(area eq 1 and cci ne -999. and reform(tr_dum.ANOM[*,*,tr]) ne -999.,idxcnt)
				if idxcnt gt 0 then begin
					tr_dum_stats[0,tr]  = mean  (dummy[idx])
					tr_dum_stats[1,tr]  = stddev(dummy[idx])
					tr_dum_gstats[0,tr] = gmean  (dummy[idx],lat[idx])
					tr_dum_gstats[1,tr] = gstddev(dummy[idx],lat[idx])
				endif
				dummy = reform(tr_dum.ANOM[*,*,tr]) * (area eq 1)  + (area eq 0) * (-999.)
				idx = where(dummy ne -999.,idxcnt)
				if idxcnt gt 0 then begin
					anom_stats [0,tr]  = mean   (dummy[idx])
					anom_stats [1,tr]  = stddev (dummy[idx])
					anom_gstats[0,tr]  = gmean  (dummy[idx],lat[idx])
					anom_gstats[1,tr]  = gstddev(dummy[idx],lat[idx])
				endif
			endfor
			dum_stats                          = reform(stats[*,*,ii])
			dum_stats[*,mima[0]:mima[1]]       = tr_dum_stats
			dum_gstats                         = reform(gstats[*,*,ii])
			dum_gstats[*,mima[0]:mima[1]]      = tr_dum_gstats
			dum_anom_stats                     = reform(stats[*,*,ii])
			dum_anom_stats[*,mima[0]:mima[1]]  = anom_stats
			dum_anom_gstats                    = reform(gstats[*,*,ii])
			dum_anom_gstats[*,mima[0]:mima[1]] = anom_gstats

			trends = {	mean:trend,anom_mean:an_mean2d,stats_non_weighted:dum_stats,stats:dum_gstats,$
						stats_anom_non_weighted:dum_anom_stats,stats_anom:dum_anom_gstats, $
						seasonal_mean2d:seasonal_mean2d}
		endif else trends = -1

		str_cov = cov[ii]+'_'
		if cov[ii] eq 'full'      then str_cov = ''
		if cov[ii] eq 'full_land' then str_cov = 'land_'
		if cov[ii] eq 'full_sea'  then str_cov = 'sea_'
		out_struc = {algoname:algon1,varname:dat,longname:vollername,unit:unit,coverage:coverage[ii],stats:reform(gstats[*,*,ii]),$
				stats_non_weighted:reform(stats[*,*,ii]),mean:temporary(cci),unc:temporary(unce),std:temporary(cci_std),trend:trends}
		out_struc = create_struct(out_struc,'histogram',hist)
		out_struc = create_struct(out_struc,'Overall_Stats',all)
		sav_file  = !SAVS_DIR + 'time_series/plot/plot_'+dat+'_'+cli+apx+'_time_series_'+sat+'_'+$
		str_cov+strjoin([years[0],(reverse(years))[0]],'-')+'.sav'
		print,'Create: '+sav_file
		save_var, out_struc, sav_file,chmod = '664'
	endfor

	;cleanup
	free,mean_2d
	free,unce_2d
	free,anz_2d
	free,anz_unce_2d

end
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

pro validation_tool_box

end

; 		if self.which_file eq '1330' then file_copy, nc_file, strjoin(strsplit(nc_file,'_[0-9]{4}PM_',/ext,/regex),'_PM_'),/allow_same,/overwrite
; 		if self.which_file eq '0730' then file_copy, nc_file, strjoin(strsplit(nc_file,'_[0-9]{4}AM_',/ext,/regex),'_AM_'),/allow_same,/overwrite
