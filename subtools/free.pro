; (c) by David N. Bresch, 960116
PRO free, var, var1, var2, var3, var4, var5, var6, var7, var8, var9

	if n_params() gt 0 then begin
		var=fltarr(2)
		dummy=0.*total(temporary(var))
		if n_params() gt 1 then begin
			var1=fltarr(2)
			dummy=0.*total(temporary(var1))
		endif
		if n_params() gt 2 then begin
			var2=fltarr(2)
			dummy=0.*total(temporary(var2))
		endif
		if n_params() gt 3 then begin
			var3=fltarr(2)
			dummy=0.*total(temporary(var3))
		endif
		if n_params() gt 4 then begin
			var4=fltarr(2)
			dummy=0.*total(temporary(var4))
		endif
		if n_params() gt 5 then begin
			var5=fltarr(2)
			dummy=0.*total(temporary(var5))
		endif
		if n_params() gt 6 then begin
			var6=fltarr(2)
			dummy=0.*total(temporary(var6))
		endif
		if n_params() gt 7 then begin
			var7=fltarr(2)
			dummy=0.*total(temporary(var7))
		endif
		if n_params() gt 8 then begin
			var8=fltarr(2)
			dummy=0.*total(temporary(var8))
		endif
		if n_params() gt 9 then begin
			var9=fltarr(2)
			dummy=0.*total(temporary(var9))
		endif
	endif
END
