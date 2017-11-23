;
;start_png
;
pro start_png, out_file				, $
		xyscale		= xyscale		, $
		fontsize	= fontsize		, $
		background 	= background	, $
		foreground 	= foreground	, $
		png_8bit	= png_8bit		, $
		png_z		= png_z			, $
		_extra		= _extra
	common start_png_common, fn, wi, old_background, old_foreground, old_charsize, p8, pz
	;mal sehen ob schon in png geschrieben wird
	if (n_elements(fi) gt 0) then begin
		if fn ne '' then begin & print, 'PNG already open -> do nothing' & return & endif
	endif
	fn				= (n_params() eq 1)		? out_file		: 'idl.png'
	xy				= keyword_set(xyscale)	? xyscale		: [1000, 1000]
	fs				= keyword_set(fontsize)	? fontsize		: 8
	p8				= keyword_set(png_8bit)
	pz				= keyword_set(png_z)
	old_background	= !p.background
	old_foreground	= !p.color
	old_charsize	= !p.charsize
	if pz then begin
		set_plot, 'Z'
		device, set_colors = 256, set_resolution = xy, z_buffering = 0
	endif else begin
		window, retain = 2, /pixmap, xsize = xy[0], ysize = xy[1], /free, _extra = _extra
		wi				= !d.window
	endelse
	background		= advanced_keyword_set(background)	? background	: t_c_i(255, 255, 255)
	foreground		= advanced_keyword_set(foreground)	? foreground	: t_c_i(0, 0, 0)
	background		= (n_elements(background) eq 3) ? t_c_i(background[0], background[1], background[2]) : background
	foreground		= (n_elements(foreground) eq 3) ? t_c_i(foreground[0], foreground[1], foreground[2]) : foreground
	!p.background	= background
	!p.color		= foreground
	!p.charsize		= fs
;	print, fs
end
