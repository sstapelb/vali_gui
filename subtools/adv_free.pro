PRO adv_free, var, var1, var2, var3, var4, var5, var6, var7, var8, var9
	if n_params() gt 0 then begin
		dummy=temporary(var)
		if n_params() gt 1 then dummy=temporary(var1)
		if n_params() gt 2 then dummy=temporary(var2)
		if n_params() gt 3 then dummy=temporary(var3)
		if n_params() gt 4 then dummy=temporary(var4)
		if n_params() gt 5 then dummy=temporary(var5)
		if n_params() gt 6 then dummy=temporary(var6)
		if n_params() gt 7 then dummy=temporary(var7)
		if n_params() gt 8 then dummy=temporary(var8)
		if n_params() gt 9 then dummy=temporary(var9)
	endif
END
