;--------------------------------------------------------------------------------
;bw_to_color converts bw images to color images using common or user color tables
;
;Syntax:
;	result = bw_to_color(bw, [mini = mini], [maxi = maxi], [col_table = col_table], [dont_scale = dont_scale], [col_center = col_center])
;
;Result:
;	color values in r,g,b
;
;Arguments:
;	bw:			grey values of any num type
;
;Keywords:
;	mini:		guess what
;	maxi:		guess what
;	col_table:	table number, 1- > 99 are user defined (currently 1 - 10)
;								100 -> 140 idl common color tables
;	dont_scale:	use dont_scale if bw is already scaled between 0 and 255
;	col_center:	center data value of color table, default: 0
;
;Example:		tv, bw_to_color(dist(200,200), col_table = -103), true = 3
;
;Remarks:		negative color_table numbers reverse
;
;History:		Created: 24.09.2004, 01:43[UTC]
;			05.02.2014 stapel included brewer ct file
;
;------------------Author: Max Reuter, Rene Preusker, 2004-----------------------
function adv_keyword_set, keyword
	return, (size(keyword,/type) < 1)
end
function bw_to_color, bw, mini = mini, maxi= maxi, col_table = col_table, dont_scale = dont_scale, col_center = col_center, brewer = brewer,use_map_image_ct = use_map_image_ct
	n_col	= 4096l		;wer hier noch streifen sieht:hoeher setzen!!!
	mi = adv_keyword_set(mini) ? mini : min(bw)
	ma = adv_keyword_set(maxi) ? maxi : max(bw)

	if not keyword_set(col_table) then col_table = 0
	if n_elements(col_table) gt 1 then begin
		ct	= col_table
	endif else begin
		if ( (abs(col_table[0]) lt 100) or (abs(col_table[0]) gt 140) ) and ( ~keyword_set(brewer) and ~keyword_set(use_map_image_ct) ) then begin
			case abs(col_table) of
			;in diese tabelle können nun beliebig farbtabellen eingefügt werden!!!!!!!!!!!!!!!!!!!!!!!!!!
			;die angabe einer negativen nummer ergibt die invertierung der gewaehlten tabelle
			;					r		g		b		bw
			01:		ct = [	[	000,	000,	255,	000	], $		;standard von blau nach rot
							[	000,	255,	255,	063	], $
							[	000,	255,	000,	127	], $
							[	255,	255,	000,	195	], $
							[	255,	000,	000,	255	]]
			02:		ct = [	[	000,	000,	125,	000	], $		;peter cool von blau nach rot
							[	000,	000,	255,	025	], $
							[	000,	125,	255,	051	], $
							[	000,	255,	255,	076	], $
							[	000,	255,	000,	102	], $
							[	200,	255,	000,	127	], $
							[	255,	255,	000,	153	], $
							[	255,	125,	000,	178	], $
							[	255,	000,	000,	204	], $
							[	180,	000,	000,	229	], $
							[	100,	000,	000,	255	]]
			03:		ct = [	[	230,	000,	255,	000	], $		;rene und icke von blau nach violet (weniger grün) gelb mitte noch cooler
							[	255,	000,	255,	015	], $
							[	255,	000,	000,	063	], $
							[	255,	127,	000,	100	], $
							[	255,	255,	000,	127	], $
							[	180,	255,	000,	148	], $
							[	000,	255,	000,	169	], $
							[	000,	255,	255,	191	], $
							[	000,	180,	255,	212	], $
							[	000,	000,	255,	233	], $
							[	000,	000,	127,	255	]]
			04:		ct = [	[	000,	000,	255,	000	], $		;von blau nach weiss nach rot (fuer eyk)
							[	255,	255,	255,	127	], $
							[	255,	000,	000,	255	]]
			05:		ct = [	[	000,	000,	000,	000	], $		;von schwarz nach weiss
							[	255,	255,	255,	255	]]
			06:		ct = [	[	127,	127,	127,	000	], $		;von 50%grau nach weiss
							[	255,	255,	255,	255	]]
			07:		ct = [	[	000,	000,	000,	000	], $		;von schwarz nach 50%grau
							[	127,	127,	127,	255	]]
			08:		ct = [	[	255,	000,	000,	000	], $		;von rot nach weiss
							[	255,	255,	255,	255	]]
			09:		ct = [	[	000,	255,	000,	000	], $		;von gruen nach weiss
							[	255,	255,	255,	255	]]
			10:		ct = [	[	000,	000,	255,	000	], $		;von blau nach weiss
							[	255,	255,	255,	255	]]
			11:		ct = [	[	255,	000,	000,	000	], $		;von rot nach magenta
							[	255,	255,	000,	051	], $
							[	000,	255,	000,	102	], $
							[	000,	255,	255,	153	], $
							[	000,	000,	255,	204	], $
							[	255,	000,	255,	255	]]
			12:		ct = [	[	152,	000,	091,	000	], $		; extended rainbow (P.Albert map_image)
							[	217,	000,	230,	018	], $
							[	105,	000,	244,	036	], $
							[	000,	125,	250,	054	], $
							[	000,	250,	250,	072	], $
							[	007,	245,	140,	091	], $
							[	109,	224,	031,	109	], $
							[	210,	250,	000,	127	], $
							[	250,	216,	000,	145	], $
							[	250,	159,	000,	163	], $
							[	250,	101,	000,	182	], $
							[	221,	046,	046,	200	], $
							[	156,	000,	000,	218	], $
							[	100,	000,	000,	236	], $
							[	080,	034,	040,	255	]]
			13:	begin	; elevation (P.Albert map_image)
					r = bytarr(256) + 255b
					g = bytarr(256) + 255b
					b = bytarr(256) + 255b
					a = bindgen(256)
					r[2] = 0 & g[2] = 0 & b[2] = 150
					r[3:20] = 0 & g[3:20] = bindgen(18) * (190.-120.) / 17. + 120 & b[3:20] = 0
					r[21:40] = bindgen(20) * (220.) / 19. & g[21:40] = 190 & b[21:40] = 0
					r[41:60] = 220 - bindgen(20) * (30) / 19. & g[41:60] = 190 - bindgen(20) * (80) / 19. & b[41:60] = 0
					r[61:80] = bindgen(20) * 65 / 19. + 190
					g[61:80] = bindgen(20) * 145 / 19. + 110
					b[61:80] = bindgen(20) * 255 / 19.
					r[81:*] = 255b & g[81:*] = 255b & b[81:*] = 255b
					ct = transpose([[r],[g],[b],[a]])
				end
			else:	ct = [	[	000,	000,	255,	000	], $		;standard von blau nach rot
							[	000,	255,	255,	063	], $
							[	000,	255,	000,	127	], $
							[	255,	255,	000,	195	], $
							[	255,	000,	000,	255	]]
			endcase
		endif else begin
			color_save = !p.color
			; stapel included brewer ct file
			if keyword_set(brewer) then file = !BREWER_CT_FILE 
			;suchen nach der idl col tab
			if ~keyword_set(use_map_image_ct) then begin
				tvlct, coltab_r_old, coltab_g_old, coltab_b_old, /get
				loadct, abs(col_table) - (keyword_set(brewer)? 0:100), file = file, /silent
			endif
			tvlct, dum_r, dum_g, dum_b, /get
			if keyword_set(use_map_image_ct) then begin
; 				n_col = 252
				; we need to replace idx 0,1 and 255 of r,g,b when used within map_image
; 				print,'bw2col'
				dum_r[0:1] = dum_r[2] & dum_g[0:1] = dum_g[2] & dum_b[0:1] = dum_b[2]
				dum_r[254:255] = dum_r[253] & dum_g[254:255] = dum_g[253] & dum_b[254:255] = dum_b[253]
			endif else tvlct, coltab_r_old, coltab_g_old, coltab_b_old
			ct = transpose(reform([[[dum_r]], [[dum_g]], [[dum_b]], [[indgen(256)]]]))
			!p.color = color_save
		endelse
	endelse

	int_r	= reform(ct[0, *])
	int_g	= reform(ct[1, *])
	int_b	= reform(ct[2, *])
	if mi eq ma then begin
		return, [[[(bw - bw) + int_r[0]]], [[(bw - bw) + int_g[0]]], [[(bw - bw) + int_b[0]]]]
	endif else begin
		int_x	= ((col_table[0] lt 0) and (n_elements(col_table eq 1))) ? reverse(reform(ct[3, *])) : reform(ct[3, *])
		col_v	= 255. * findgen(n_col) / (float(n_col) - 1.)
		int_r	= (round(interpol(int_r, int_x, col_v)))
		int_g	= (round(interpol(int_g, int_x, col_v)))
		int_b	= (round(interpol(int_b, int_x, col_v)))
		if keyword_set(dont_scale) then	dum_bw	= (float(n_col) - 1.) * (0 > bw < 255) / 255. else begin
			if adv_keyword_set(col_center) then begin
;				dum		= max(abs([mi, ma] - col_center))
				dum		= max(abs([mi, ma]))
				mic		= col_center - dum
				mac		= col_center + dum
				dum_bw	= fix((0l > (((float(n_col) - 1.) * ((mi > bw < ma)- mic)) / (mac - mic)) < (float(n_col) - 1.)))
			endif else begin
				dum_bw	= fix((0l > (((float(n_col) - 1.) * ((mi > bw < ma) - mi)) / (ma - mi)) < (float(n_col) - 1.)))
			endelse
		endelse
		dum_bw	= round(dum_bw)
		return,	[[[		int_r[dum_bw]	]], $
				 [[		int_g[dum_bw] 	]], $
				 [[		int_b[dum_bw]	]]]
	endelse
end
