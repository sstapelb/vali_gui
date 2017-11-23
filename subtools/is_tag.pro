function is_struct, file
	if size(file,/type) eq 8 then return,1b
	return,0b
end
;------------------------------------------------------------------------------------------
function is_tag, structure, tag

	error_status = 0

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, 0b
	endif

	result = byte(total(strmatch(tag_names(structure),tag,/fold)) < 1.)
	
	return, result
end

