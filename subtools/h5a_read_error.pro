function h5a_read_error, att_id
; h5a_read_error is a workaround because h5a_read does have problems with 0 Byte characters (ncdf4)
; use this instead of the original !
	error_status = 0
	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, '<Read Error>'
	endif
	result = h5a_read(att_id)
	return,result
end
