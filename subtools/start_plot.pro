pro start_plot, file, type, preset = preset, color_8bit = color_8bit, _extra = _extra
	;parameter ueberpruefen
	type		= (n_params() lt 2)	? 'win'			: type
	file		= (n_params() lt 1)	? ('idl_plot.' + type)	: (file + '.' + type)

	;voreinstellungen festlegen
	preset		= keyword_set(preset)	? preset 	: 'default'
	_extra		= keyword_set(_extra)	? _extra	: {dum1:0}
	win_nr		= total(strlowcase(tag_names(_extra)) eq 'win_nr')	? _extra.win_nr		: 30
	win_pos		= total(strlowcase(tag_names(_extra)) eq 'win_pos')	? _extra.win_pos	: [0, 0]
	win_size	= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [400, 300]
	eps_font	= total(strlowcase(tag_names(_extra)) eq 'eps_font')	? _extra.eps_font	: (is_win() ? 'Arial' : 'Helvetica')
	eps_fontsize	= total(strlowcase(tag_names(_extra)) eq 'eps_fontsize')? _extra.eps_fontsize	: 8
	eps_size	= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [13, 9]
	ps_font		= total(strlowcase(tag_names(_extra)) eq 'ps_font')	? _extra.ps_font	: eps_font
	ps_fontsize	= total(strlowcase(tag_names(_extra)) eq 'ps_fontsize')	? _extra.ps_fontsize	: eps_fontsize
	ps_size		= total(strlowcase(tag_names(_extra)) eq 'ps_size')	? _extra.ps_size	: eps_size
	png_fontsize	= total(strlowcase(tag_names(_extra)) eq 'png_fontsize')? _extra.png_fontsize	: 1
	png_size	= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [720, 576]

	case strlowcase(preset) of
	'dr_full'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00600, 00800]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [15.00, 20.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [02500, 04000]
	end
	'dr_normal'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00400, 00240]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [15.00, 09.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [03000, 01800]
	end
	'dr_normal_legend'		: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00400, 00080]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [15.00, 03.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [03000, 00600]
	end
	'dr_flat'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00400, 00160]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [15.00, 06.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [03000, 01200]
	end
	'dr_extra_flat'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00400, 00100]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [15.00, 03.50]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [03000, 00800]
	end
	'dr_narrow'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00300, 00372]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [07.25, 09.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [02000, 02483]
	end
	'dr_small'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00362, 00300]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [07.25, 06.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [00967, 00800]
	end
	'dr_small_flat'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00362, 00200]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [07.25, 03.50]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [00967, 00500]
	end
	'dr_small_disk'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00362, 00362]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [06.00, 06.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [00800, 00800]
	end
	'dr_small_legend'		: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00362, 00150]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [07.25, 03.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [02000, 00827]
	end
	'dr_tiny'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00230, 00230]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [04.60, 04.60]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [01150, 01150]
	end
	'dr_tiny_legend'		: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00230, 00250]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [04.60, 03.00]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [01150, 00750]
	end
	'dr_ch_combo_ex'		: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00480, 00320]
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [04.80, 03.20]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [01024, 00768]
	end
	'vali_paper_01'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [01024, 00768]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [01024, 00768]
			png_fontsize		= total(strlowcase(tag_names(_extra)) eq 'png_fontsize')	? _extra.png_fontsize	: 2
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [13.33, 10.00]
			eps_fontsize		= total(strlowcase(tag_names(_extra)) eq 'eps_fontsize')	? _extra.eps_fontsize	: 7
	end
	'vali_paper_02'			: begin
			win_size		= total(strlowcase(tag_names(_extra)) eq 'win_size')	? _extra.win_size	: [00768, 00768]
			png_size		= total(strlowcase(tag_names(_extra)) eq 'png_size')	? _extra.png_size	: [00768, 00768]
			png_fontsize		= total(strlowcase(tag_names(_extra)) eq 'png_fontsize')	? _extra.png_fontsize	: 2
			eps_size		= total(strlowcase(tag_names(_extra)) eq 'eps_size')	? _extra.eps_size	: [10.00, 10.00]
			eps_fontsize		= total(strlowcase(tag_names(_extra)) eq 'eps_fontsize')	? _extra.eps_fontsize	: 7
	end
	else				: begin
		endelse
	endcase

	;eigentliches program
	case type of
	'eps' :	begin
			;try to close maybe open postscript device
			end_eps, /silent
			start_eps, file, xyscale = eps_size, font_size = eps_fontsize, /silent
		end
	'ps' :	begin
			;try to close maybe open postscript device
			end_eps, /silent
			start_eps, file, xyscale = ps_size, font_size = ps_fontsize, /ps, /silent
		end
	'png' :	begin
			;try to close maybe open png device
			end_png, /silent
			png_foreground	= total(strlowcase(tag_names(_extra)) eq 'png_foreground')	? _extra.png_foreground	: [000, 000, 000]
			png_background	= total(strlowcase(tag_names(_extra)) eq 'png_background')	? _extra.png_background	: [255, 255, 255]
			png_8bit		= total(strlowcase(tag_names(_extra)) eq 'png_8bit')		? _extra.png_8bit		: 0
			png_z			= total(strlowcase(tag_names(_extra)) eq 'png_z')			? _extra.png_z			: 0
			start_png, file, xyscale = png_size, fontsize = png_fontsize, background = png_background, foreground = png_foreground, png_8bit = png_8bit, png_z = png_z
		end
	else :	begin
			;try to get current window position and close the window
			device, window_state = open_windows
			if open_windows[win_nr] eq 1 then begin
				wset, win_nr
			endif
			win_foreground	= total(strlowcase(tag_names(_extra)) eq 'win_foreground')	? _extra.win_foreground	: [000, 000, 000]
			win_background	= total(strlowcase(tag_names(_extra)) eq 'win_background')	? _extra.win_background	: [255, 255, 255]
			!p.color	= (n_elements(win_foreground) eq 3) ? t_c_i(win_foreground[0], win_foreground[1], win_foreground[2]) : win_foreground
			!p.background	= (n_elements(win_background) eq 3) ? t_c_i(win_background[0], win_background[1], win_background[2]) : win_background
			;open new window with correct size and old position
			awin, win_nr, xsize = win_size[0], ysize = win_size[1], xpos = win_pos[0], ypos = win_pos[1], title = file_name_info(file, /name)
		end
	endcase
end
