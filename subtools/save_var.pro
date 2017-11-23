;calculate the decimal number from an octal (e.g., chmod)
;Should have the same result as "n or 'n'o, but can be used with variables
function oct2dec, octal
	len    = strlen(strcompress(octal,/rem))
	digits = reverse(fix( strmid(octal,findgen(len),fltarr(len)+1)))
	deci   = long(total(digits * (8^findgen(len))))
	return, deci
end
;---------------------------------------------------------------------------------------------------
;routine:		save_var
;description:	saves a variable to an idl sav file
;author:		max reuter
;---------------------------------------------------------------------------------------------------
pro save_var, data, file, compress = compress, chmod = chmod
	if ((size(file, /type) ne 7) || (n_elements(file) ne 1)) && (size(data, /type) eq 7) && (n_elements(data) eq 1) then begin
		var_swap, data, file
		save, data, file = file, compress = compress
		var_swap, data, file
	endif else begin
		save, data, file = file, compress = compress
	endelse
	if keyword_set(chmod) then file_chmod,file,oct2dec(chmod)
end
;---------------------------------------------------------------------------------------------------

