FUNCTION bias,x,y,no_data_value=no_data_value

	ndv = keyword_set(no_data_value) ? no_data_value[0] : -999.

	idx = where(x ne ndv and y ne ndv,idxcnt)

	result = idxcnt eq 0. ? !values.f_nan : total(float(x[idx])-float(y[idx])) / float(idxcnt)
	
	return, result
END
