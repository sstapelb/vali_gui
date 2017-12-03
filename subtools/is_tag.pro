;------------------------------------------------------------------------------------------
function is_tag, structure, tag, index, set = set

	error_status = 0

	catch, error_status
	if (error_status ne 0) then begin
		catch, /cancel
		return, 0
	endif

	result = fix(max(strmatch(tag_names(structure),tag,/fold),index))
	if result eq 0 then index = -1
	
	if keyword_set(set) and result eq 1 then begin
		result = keyword_set(structure.(index))
	endif 

	return, result
end
;------------------------------------------------------------------------------------------
