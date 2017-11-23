;---------------------------------------------------------------------------------------------------
;routine:		restore_var
;description:	restores a sav file to a variable saved with save_var
;author:		max reuter
;---------------------------------------------------------------------------------------------------
function restore_var, file, found = found
	found = 1
	catch, error
	if error ne 0l then begin
		found = 0
		return, -1
	endif
	restore, file
	case 1b of
		(n_elements(data) gt 0l)			:	return, data
		(n_elements(place_holder_var_name_314159) gt 0l):	return, place_holder_var_name_314159
		else						:	begin & found = 0 & return, -1 & end
	endcase
end
;---------------------------------------------------------------------------------------------------
