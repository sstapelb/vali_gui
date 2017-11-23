function h5a_get_num_attrs_error, file_id
; same as h5a_get_num_attrs but with error handling
; use this instead of the original !
	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, -1
	endif
	result = h5a_get_num_attrs(file_id)
	return,result
end
