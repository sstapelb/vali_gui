pro end_eps, silent = silent
	if strlowcase(!d.name) ne 'ps' then begin
		if not keyword_set(silent) then print, 'PS not open -> do nothing'
		return
	endif
	device, /close_file
	os = !version
	os = strlowcase(os.os_family)
	case os of
	'windows'  :set_plot, 'win'
	'unix'     :set_plot, 'X'
	endcase
	!P.FONT=-1
	loadct, 0, /silent
end
