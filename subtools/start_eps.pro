;
;start_eps
;
pro start_eps, out_file, xyscale = xyscale, font_size = font_size, _extra = _extra, ps = ps, no_preview = no_preview, silent = silent

	if strlowcase(!d.name) eq 'ps' then begin & print, 'PS already open -> do nothing' & return & endif

	init = {xscale		:	13.0		, $
		yscale		:	9.0		, $
		font_size	:	8		, $
		file_name	:	'idl.eps'	}
	a4_x = 21.0
	a4_y = 29.7

	if ((n_params() ne 0) and (n_params() ne 1)) then begin
		print_help, 'start_eps'
		return
	endif

	xs = init.xscale & ys = init.yscale & fn = init.file_name & fs = init.font_size

	if advanced_keyword_set(xyscale) then begin
		if (n_elements(xyscale) ne 2) then begin
			print_help, 'start_eps'
			return
		endif
		xs = xyscale[0] & ys = xyscale[1]
	endif

	if advanced_keyword_set(font_size) then begin
		fs = font_size
	endif

	if (n_params() eq 1) then begin
		fn = out_file
	endif

	deviceKeyword={	xsize:xs, xoff:(a4_x-xs)/2., $
					ysize:ys, yoff:(a4_y-ys)/2., $
					filename:fn, $
					inches:0, $
					color:1, $
					bits_per_pixel:8, $
					encapsulated:keyword_set(ps)?0:1, $
					landscape:keyword_set(landscape)?1:0, $
					preview:(keyword_set(no_preview) or keyword_set(ps) ? 0:2)}

	!p.font = 0
	set_plot, 'PS', /copy
	Device, /Helvetica, /ISOLATIN1, _Extra=deviceKeyword, font_size = fs

	if keyword_set(silent) then return
	init.xscale = xs & init.yscale = ys & init.file_name = fn & init.font_size = fs
	print, 'Font / Size: Arial (Helvetica), ', init.font_size
	print, 'x [cm]: ', init.xscale
	print, 'y [cm]: ', init.yscale
end
