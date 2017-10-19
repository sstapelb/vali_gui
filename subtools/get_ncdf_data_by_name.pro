function get_ncdf_data_by_name, filename, name, attribute = attribute, $
						dimensions = dimensions, $
						all_attributes = all_attributes, $
						global_attribute = global_attribute, $
						found = found, verbose = verbose, $
						var_dim_names = var_dim_names, $
						data_type = data_type,$
						count = count, offset = offset, stride = stride

	verb = keyword_set(verbose)

	if not file_test(filename[0],/read) then begin
		if file_test(filename[0]) then ok=dialog_message('I have No read Permissions for Ncdf file! filename[0]'+filename[0]) else $
		if verb then print, 'Ncdf file not found!'
		found = 0.
		return,-1
	endif

	id    = ncdf_open(filename[0])
	res   = ncdf_inquire(id)
	tag   = ''
	found = 1.

	if keyword_set(dimensions) then begin
		for j=0l,res.ndims -1 do begin
			ncdf_diminq, id, j, dimension_name, dimension_size
			if strupcase(dimension_name) eq strupcase(name) then begin
				value = dimension_size
				continue
			endif
		endfor
		ncdf_close, id
		if not float(size(value,/type)) then begin
			if verb then print, 'The Dimension '+strupcase(name)+' is unknown in '+filename[0]
			value = -1
			found =  0.
		endif 
		return, value
	endif

	if keyword_set(global_attribute) then begin
		for j=0l,res.ngatts-1 do begin
			globname = ncdf_attname(id, j, /GLOBAL)
			if strupcase(globname) eq strupcase(name) then begin
				ncdf_attget, id, globname, value, /global
				attinfo = ncdf_attinq(id, globname, /global)
				att_type = strupcase(attinfo.dataType)
				IF size(value, /tname) eq 'BYTE' and att_type eq 'CHAR' then value = string(value)
				continue
			endif
		endfor
		ncdf_close, id
		if not float(size(value,/type)) then begin
			if verb then print, 'The Global Attribute '+strupcase(name)+' is unknown in '+filename[0]
			value = -1
			found =  0.
		endif 
		return, value
	endif

	for i=0l,res.nvars-1 do begin
		dum = ncdf_varinq(id,i)
		if strupcase(dum.name) eq strupcase(name) then begin
			tag  = dum.name
			dimi = dum.dim
			data_type = dum.DATATYPE
			break
		endif
	end
	if tag then begin
		if keyword_set(all_attributes) then begin
			ncdf_varget,id,tag,dum,count = count, offset=offset, stride = stride
			erg = ncdf_varinq(id,tag)
			for j = 0l, erg.natts -1 do begin
				attname = ncdf_attname(id,tag,j)
				ncdf_attget,id,tag,attname,attval
				if j ge 1 then begin
					attstr=create_struct(attstr,attname,attval)
				endif else begin
					attstr=create_struct(attname,attval)
				endelse
			endfor
			if not float(size(attstr,/type)) then attstr=""
			dum={data:dum,attributes:attstr}
		endif else if keyword_set(attribute) then begin
			erg = ncdf_varinq(id,tag)
			if erg.natts gt 0 then begin
				for j = 0l, erg.natts -1 do begin
					attname = ncdf_attname(id,tag,j)
					if strupcase(attname) eq strupcase(attribute) then begin
						ncdf_attget,id,tag,attname,attval
						break
					endif
				endfor
			endif
			if not float(size(attval,/type)) then begin
				if verb then print, 'The Attribute '+strupcase(attribute)+' is unknown for '+tag+' in '+filename[0]
				dum   = -1
				found =  0.
			endif else dum = attval
		endif else begin
			ncdf_varget,id,tag,dum,count = count, offset=offset, stride = stride
			var_dim_names = strarr(n_elements(dimi))
			for j=0l,n_elements(dimi) -1 do begin
				ncdf_diminq, id, dimi[j], dimension_name, dimension_size
				var_dim_names[j] = dimension_name
			endfor
		endelse
	endif else begin
		if verb then print, 'Data '+name+' is undefined in '+filename[0]
		dum   = -1
		found =  0.
	endelse

	ncdf_close,id

	return, dum

end