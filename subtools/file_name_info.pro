;---------------------------------------------------------------------------------------------
;ermittelt aus einem pfad+filename+extension string den pfad oder die extension, den filename
;oder den filename.extension, je nach parameter mode ("path", "name", "name.ext" oder "extension")
;default = "name"
;
;Author: Max Reuter, 2000
;---------------------------------------------------------------------------------------------
function file_name_info, file_name, help = help, path = path, name = name, extension = extension
	error_count = 0
;	catch, error
	error = 0
	if (error_count gt 0) then return, 0
	if (error ne 0) then begin
		error_count = 1
		print_help, 'file_name_info'
		return, 0
	endif
	if keyword_set(help) then begin
		print_help, 'file_name_info'
		return, 0
	endif
	last_slash	= transpose( ( ( strpos( [file_name], "/",/reverse_search ) > strpos( [file_name], "\",/reverse_search ) ) > (-1)) + 1)
	all_path	= strmid(file_name, 0, last_slash)
	all_name_ext	= strmid(file_name, last_slash)
	last_dot	= transpose(strpos([all_name_ext], ".",/reverse_search))
	no_dot_idx	= where(last_dot lt 0, no_dot_anz)
	if no_dot_anz gt 0 then last_dot[no_dot_idx] = (strlen(all_name_ext))[no_dot_idx]
	all_name	= strmid(all_name_ext, 0, last_dot)
	all_ext		= strmid(all_name_ext, last_dot + 1)
	all_dot		= strarr(n_elements(all_ext)) + '.'
	if no_dot_anz gt 0 then all_dot[no_dot_idx] = ''

	if not (keyword_set(path) or keyword_set(name) or keyword_set(extension)) then begin
		return, all_name
	endif else begin
		return,		(keyword_set(path) ? all_path : '')														+ $
					(keyword_set(name) ? all_name : '')														+ $
					((keyword_set(extension) and (keyword_set(path) or keyword_set(name))) ? all_dot : '')	+ $
					(keyword_set(extension) ? all_ext : '')
	endelse
end





function file_name_info_alt, file_name, mode = mode, help = help, path = path, name = name, extension = extension
	error_count = 0
	catch, error
	if (error_count gt 0) then return, 0
	if (error ne 0) then begin
		error_count = 1
		print_help, 'file_name_info'
		return, 0
	endif
	if keyword_set(help) then begin
		print_help, 'file_name_info'
		return, 0
	endif
	if keyword_set(name) then mode = 'name'
	if keyword_set(path) then mode = 'path'
	if keyword_set(extension) then mode = 'extension'
	if (not keyword_set(mode)) then mode = "name"
	last_slash	= max([strpos(file_name,"/",/reverse_search), strpos(file_name,"\",/reverse_search)])
	last_dot	= strpos(file_name,".",/reverse_search)
	laenge		= strlen(file_name)
	no_slash	= 0
	no_dot		= 0
	if (laenge lt 1) then return, ""
	if (last_slash lt 0) then no_slash = 1
	if ((last_dot lt 0) or (last_dot le last_slash + 1)) then no_dot = 1
	case mode of
	"name"		:	begin
						if no_slash then von = 0 else von = last_slash + 1
						if no_dot then len = laenge - von else len = last_dot - von
						return, strmid(file_name, von, len)
					end
	"path"		: begin
						if no_slash then return, ""
						von = 0
						len = last_slash + 1
						return, strmid(file_name, von, len)
					end
	"extension"	: begin
						if no_dot then return, ""
						von = last_dot + 1
						len = laenge - last_dot
						return, strmid(file_name, von, len)
					end
	"name.ext"	: begin
						if no_slash then von = 0 else von = last_slash + 1
						if no_dot then return, ""
						len = laenge - last_slash
						return, strmid(file_name, von, len)
					end
	else		: return, ""
	endcase
end

