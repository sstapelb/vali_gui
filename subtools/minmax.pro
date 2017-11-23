function minmax, werte, pos, nan = nan, absolute = absolute, no_data_value = no_data_value

	if n_elements(werte) eq 0 then begin
		print,'% MINMAX: Variable is undefined: WERTE'
		return,-1
	endif

	dummy = n_elements(no_data_value) gt 0 ? werte[where(werte ne no_data_value)] : werte

	mi = min(dummy, p1, nan = nan, absolute = absolute)
	ma = max(dummy, p2, nan = nan, absolute = absolute)

	if n_params() eq 2 then pos = [p1, p2]
	return, [mi, ma]
end