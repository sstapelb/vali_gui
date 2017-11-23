pro end_png, silent = silent
	common start_png_common, fn, wi, old_background, old_foreground, old_charsize, p8, pz
	if not (n_elements(pz) gt 0) then begin
		if not keyword_set(silent) then print, 'first do start_png -> done nothing'
		return
	endif
	if pz then begin
		if strlowcase(!d.name) ne 'z' then begin
			if not keyword_set(silent) then print, 'Z-buffer not open -> do nothing'
			return
		endif
		;lesen aus dem z-buffer
		buffer	= TVRD()
		;lesen der color_table
		tvlct, ct_r, ct_g, ct_b, /get
		;schliessen des z-buffers
		device, /close
		os = !version
		os = strlowcase(os.os_family)
		case os of
		'windows'  :set_plot, 'win'
		'unix'     :set_plot, 'X'
		endcase
		!P.FONT=-1
		loadct, 0, /silent
		;schreiben der gelesenen daten
		write_png, fn, buffer, ct_r, ct_g, ct_b
	endif else begin
		if wi ne !d.window then begin
			if not keyword_set(silent) then print, 'posible PNG error: window_index has changed'
			return
		endif
		buffer	= TVRD(True=1)
		wdelete, wi
		wi		= 0
		if p8 then begin
			colquan			= color_quan(buffer, 1, r, g, b, colors=256)
			write_png, fn, colquan, r, g, b
		endif else begin
			write_png, fn, buffer
		endelse
	endelse
	if (n_elements(old_background) gt 0)	then !p.background	= old_background
	if (n_elements(old_foreground) gt 0)	then !p.color		= old_foreground
	if (n_elements(old_charsize) gt 0)		then !p.charsize	= old_charsize
	fn		= ''
end
