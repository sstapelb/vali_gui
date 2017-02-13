;-------------------------------------------------------------------------------------------------------------------------
function get_grid_res, data, found = found, cci_l3u_eu = cci_l3u_eu, cci_l3u_af = cci_l3u_af, claas = claas
	cci_l3u_eu=0.
	cci_l3u_af=0.
	claas=0.
	if n_elements(data) gt 0 then begin
		found=1.
		dumdata = data
		if size(dumdata,/n_dim) eq 3 then begin
			si  = size(dumdata,/dim)
			dum = where(si eq 3,scnt,complement=sidx)
			if scnt eq 1 then dumdata = indgen(si[sidx])
		endif
		if size(dumdata,/n_dim) eq 2 then begin
			dum = [360.,180.]/float(size(dumdata,/dim))
			if dum[0] eq dum[1] then begin ; regular grid?
				if total(dum[0] eq [0.01,0.02,0.05,0.10,0.25,0.50,0.75,1.00,2.00,2.50,5.00]) then return,dum[0] ; CCI vali standards
			endif else if long(total(double(dum))*1000000d) eq 210000l then begin ; not nice but nessacary ;)
				;CCI Europe L3U MODIS files
				cci_l3u_eu = 1.
			endif else if long(total(double(dum))*1000000d) eq 162000l then begin ; not nice but nessacary ;)
				;CCI Africa L3U MODIS files
				cci_l3u_af = 1.
			endif else if dum[0] eq 0.1 and dum[1] eq 0.05 then begin
				claas = 1.
			endif
		endif
	endif
	found=0.
end
;-------------------------------------------------------------------------------------------------------------------------
