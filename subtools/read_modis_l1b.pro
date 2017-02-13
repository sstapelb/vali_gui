@/cmsaf/nfshome/sstapelb/idl/vali_gui/subtools/modis_bright.pro

function read_modis_l1b, filename, satellite, channel,radiance=radiance, found = found, index = index, $
			no_data_value=no_data_value, minvalue=minvalue, maxvalue=maxvalue, longname=longname, unit=unit

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		if (size(varid,/type) ne 0) then HDF_SD_ENDACCESS,varid
		if (size(sd_id,/type) ne 0) then HDF_SD_END,sd_id
		help,/last_message
		found = 0.
		return, -1
	endif

	cha = strlowcase(channel)

	if cha eq 'ev_250_aggr1km_refsb' and adv_keyword_set(index) then begin
		if index eq 0 then cha = 'ch1'
		if index eq 1 then cha = 'ch2'
	endif
	if cha eq 'ev_1km_emissive' and adv_keyword_set(index) then begin
		if index eq 0 then cha = 'ch3b'
		if index eq 1 then cha = 'ch4'
		if index eq 2 then cha = 'ch5'
	endif

	sat = strlowcase(satellite) ; aqua or terra
	found  = 1
	case cha of
		'ev_250_aggr1km_refsb'	: begin & refl = ~keyword_set(radiance) & index = -1 & para_name = 'EV_250_Aggr1km_RefSB' & bt_ch = -1 & end
		'ev_500_aggr1km_refsb'	: begin & refl = ~keyword_set(radiance) & index =  0 & para_name = 'EV_500_Aggr1km_RefSB' & bt_ch = -1 & end
		'ev_1km_emissive'	: begin & refl = 0 & index = -1 & para_name = 'EV_1KM_Emissive' & bt_ch = ([20,31,32,-1])[index] & end
		'ch1'			: begin & refl = ~keyword_set(radiance) & index = 0 & para_name = 'EV_250_Aggr1km_RefSB' & bt_ch = -1 & end	; channel  1: 620-670 nm
		'ch2'			: begin & refl = ~keyword_set(radiance) & index = 1 & para_name = 'EV_250_Aggr1km_RefSB' & bt_ch = -1 & end	; channel  2: 841-876 nm
		'ch3a'			: begin & refl = ~keyword_set(radiance) & index = 0 & para_name = 'EV_500_Aggr1km_RefSB' & bt_ch = -1 & end	; channel  6: 1628-1652 nm
		'ch3b'			: begin & refl = 0 & index = 0 & para_name = 'EV_1KM_Emissive' & bt_ch = 20 & end				; channel 20: 3.66-3.84 µm
		'ch4'			: begin & refl = 0 & index = 1 & para_name = 'EV_1KM_Emissive' & bt_ch = 31 & end				; channel 31: 10.78-11.28 µm
		'ch5'			: begin & refl = 0 & index = 2 & para_name = 'EV_1KM_Emissive' & bt_ch = 32 & end				; channel 32: 11.77-12.27 µm
		'solarzenith'		: begin & refl = 2 & index = 0 & para_name = 'SolarZenith' & bt_ch = -1 & end
		'sensorzenith'		: begin & refl = 2 & index = 0 & para_name = 'SensorZenith' & bt_ch = -1 & end
		'solarazimuth'		: begin & refl = 2 & index = 0 & para_name = 'SolarAzimuth' & bt_ch = -1 & end
		'sensorazimuth'		: begin & refl = 2 & index = 0 & para_name = 'SensorAzimuth' & bt_ch = -1 & end
		'sunza'			: begin & refl = 2 & index = 0 & para_name = 'SolarZenith' & bt_ch = -1 & end
		'satza'			: begin & refl = 2 & index = 0 & para_name = 'SensorZenith' & bt_ch = -1 & end
		'sunazi'		: begin & refl = 2 & index = 0 & para_name = 'SolarAzimuth' & bt_ch = -1 & end
		'satazi'		: begin & refl = 2 & index = 0 & para_name = 'SensorAzimuth' & bt_ch = -1 & end
		'lon'			: begin & refl = 3 & index = 0 & para_name = 'Longitude' & bt_ch = -1 & end
		'lat'			: begin & refl = 3 & index = 0 & para_name = 'Latitude' & bt_ch = -1 & end
		'longitude'		: begin & refl = 3 & index = 0 & para_name = 'Longitude' & bt_ch = -1 & end
		'latitude'		: begin & refl = 3 & index = 0 & para_name = 'Latitude' & bt_ch = -1 & end
		else			: begin & refl = 3 & index = 0 & para_name = channel   & bt_ch = -1 & end
	endcase

	sd_id = HDF_SD_START(filename[0],/read)
		var_index = HDF_SD_NAMETOINDEX(sd_id,para_name[0])
		if var_index eq -1 then begin
			HDF_SD_END,sd_id
			found = 0.
			return, -1
		endif
		varid = HDF_SD_SELECT(sd_id , var_index)
			HDF_SD_GETDATA,varid,dum
			hdf_sd_getinfo,varid, natts=natts
			att = create_struct('Var',para_name[0])
			for i =0,natts-1 do begin & $
				HDF_SD_ATTRINFO,varid,i,data=datatt,name=name  & $
				att = create_struct(att,idl_validname(name,/convert_all),datatt)  & $
			endfor
			longname = is_tag(att,'LONG_NAME')   ? att.LONG_NAME : ''
			unit     = is_tag(att,'UNITS')       ? (att.UNITS eq 'none' ? '' : att.UNITS) : ''
			minvalue = is_tag(att,'VALID_RANGE') ? att.VALID_RANGE[0] : 0
			maxvalue = is_tag(att,'VALID_RANGE') ? att.VALID_RANGE[1] : max(dum)
		HDF_SD_ENDACCESS,varid
	HDF_SD_END,sd_id

	if ~found then return,-1
; 	free,dummy
	; return unscaled arrays

	if index eq -1 then begin
		if para_name eq 'EV_250_Aggr1km_RefSB' and ~keyword_set(radiance) then begin & minvalue = 0. & maxvalue=100. & end
		if para_name eq 'EV_1KM_Emissive' then begin & minvalue = 180. & maxvalue=340. & end
		no_data_value = is_tag(att,'_FILLVALUE') ? att._FILLVALUE[0] : -999.
		return,dum
	endif
	if refl eq 1 then begin 		; reflectance
		rad  = ( reform(dum[*,*,index]) - att.REFLECTANCE_OFFSETS[index] ) * att.REFLECTANCE_SCALES[index]
		minvalue =  ( minvalue - att.REFLECTANCE_OFFSETS[index] ) * att.REFLECTANCE_SCALES[index]
		maxvalue =  ( maxvalue - att.REFLECTANCE_OFFSETS[index] ) * att.REFLECTANCE_SCALES[index]
	endif else if refl eq 0 then begin 	; radiance
		rad  = ( reform(dum[*,*,index]) - att.RADIANCE_OFFSETS[index] ) * att.RADIANCE_SCALES[index]
		minvalue =  ( minvalue - att.RADIANCE_OFFSETS[index] ) * att.RADIANCE_SCALES[index]
		maxvalue =  ( maxvalue - att.RADIANCE_OFFSETS[index] ) * att.RADIANCE_SCALES[index]
	endif else if refl eq 2 then begin 	; angles
		rad  =  reform(dum[*,*,index]) * att.SCALE_FACTOR[index]
		minvalue =  ( minvalue * att.SCALE_FACTOR[index] )
		maxvalue =  ( maxvalue * att.SCALE_FACTOR[index] )
	endif else if refl eq 3 then begin 	; lon / lat
		rad  =  reform(dum[*,*,index])
	endif else begin
		print,'Something went wrong!'
		found = 0
		return,-1
	endelse

	if bt_ch ne -1 then begin
		rad      = modis_bright(sat,rad,bt_ch,1)
		minvalue = modis_bright(sat,(minvalue>0.0001),bt_ch,1)
		maxvalue = modis_bright(sat,(maxvalue>0.0001),bt_ch,1)
	endif
	if is_tag(att,'VALID_RANGE') then begin
		nd_idx = where(reform(dum[*,*,index]) eq att._FILLVALUE[0] or ~between(dum[*,*,index],att.VALID_RANGE[0],att.VALID_RANGE[1]), nd_cnt)
	endif else nd_idx = where(reform(dum[*,*,index]) eq att._FILLVALUE[0], nd_cnt)

	no_data_value = -999.
	if nd_cnt gt 0 then rad[nd_idx] = -999.

	return, rad
end