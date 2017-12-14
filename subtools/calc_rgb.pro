function calc_rgb, refl_vis006, refl_vis008, bt_nir037, bt_tir108, sunzen, ir_flag, refl_nir037=refl_nir037,enhance=enhance,true=true

	ir	= bt_tir108 > 0.; (self -> get_data('ir108', /temperature))	> 0.
	if keyword_set(refl_nir037) then begin
		r = keyword_set(true) ? bt_nir037 : bt_nir037/cosd(sunzen) > 0
	endif else begin
		r = ((bt_nir037-ir)/cosd(sunzen)+20. +(0. > (ir-302.))/30.*40.) > 0.
	endelse
	g	= keyword_set(true) ? refl_vis008 : refl_vis008/cosd(sunzen) > 0.
	b	= keyword_set(true) ? refl_vis006 : refl_vis006/cosd(sunzen) > 0.

	r = 255 * (0. > (r / 150.0)*1.0 < 1.)
	g = 255 * (0. > (g / 138.0)*0.8 < 1.)
	b = 255 * (0. > (b / 122.0)*0.5 < 1.)

	c1 = 298. & c2 = 10.
	r_weight = atan((ir - c1) / c2 * !pi) / !pi + 0.5
	r = r_weight * r + ((1 - r_weight) * (g > b))

	;veraenderung im hls farbraum -> weg von rot hin zu gruen blau
	color_convert, r, g, b, h, l, s, /rgb_hls
	;s und l vertauschen macht geile bilder !!! s.u.

	;aus untersuchung eines cumulativen histogrammes kann man nun immer die selbe histogrammveraenderung vornehmen
	cum_hist_l = ([0.0000, 0.3000, 0.4865, 0.5590, 0.6452, 0.7153, 0.7727, 0.8154, 0.8476, 0.8726, 0.8976, 0.9202, 0.9526, 0.9803, 0.9876, 0.9913, 1.0000] + 2 * vector(0,1.,17)) / 3
	l =interpolate(cum_hist_l, l * 16)
	color_convert, ((h + 24.) mod 360), l, s^1.1, r2, g2, b2, /hls_rgb

	;tunen der farben:
	;das meer soll blau und nicht rot sein
	c1 = 0.8 & c2 = 1. / c1 & c3 = 1. - c2
	r2 = 0. > (((c3 + c2 * (r2 / 255.)^c1) * atan(r2 / 255. * 100.) / !pi * 2) * float((r ne 0.))) < 1.
	;das meer soll blau und nicht rot sein
	c1 = 1.0 & c2 = 1. / c1 & c3 = 1. - c2
	b2 = 0. > (((c3 + c2 * (b2 / 255.)^c1) * atan(b2 / 255. * 100.) / !pi * 2) * float((b ne 0.))) < 1.
	;an gruen wird nix ver�ndert
	c1 = 1.0 & c2 = 1. / c1 & c3 = 1. - c2
	g2 = 0. > (((c3 + c2 * (g2 / 255.)^c1) * atan(g2 / 255. * 100.) / !pi * 2) * float((g ne 0.))) < 1.

	;bild zusammenfuegen
	img = fix(0 > (255.*(transpose([[[r2]],[[g2]],[[b2]]], [2, 0, 1]))) < 255)

	;Alles mit ir fuellen was schwarz ist
	;Wo ist das RGB schwarz?
	dum = total(img-1, 1) / 3
 
	;MST, make the dry regions less red
	if ~keyword_set(refl_nir037) then img[0,*,*] = img[0,*,*] < (img[1,*,*]+10)

	if keyword_set(enhance) then begin
		no_data_idx = where(sunzen gt 84,nd_cnt)
		if nd_cnt gt 0 then begin
			r=reform(img[0,*,*])
			g=reform(img[1,*,*])
			b=reform(img[2,*,*])
			r[no_data_idx] = 0
			g[no_data_idx] = 0
			b[no_data_idx] = 0
			img = transpose([[[r]],[[g]],[[b]]],[2,0,1])
		endif
		img = byte(img*1.3 < 255.)
		;histogram equal. um es ein wenig aufzuhellen
		r=reform(bytscl(hist_equal(img[0,*,*])))
		g=reform(bytscl(hist_equal(img[1,*,*])))
		b=reform(bytscl(hist_equal(img[2,*,*])))
		img = transpose([[[r]],[[g]],[[b]]],[2,0,1])
	endif

	if(ir_flag eq 1) then begin
		;Für die Überlagerung mache ich ein Gewicht
		weight = smooth(dum / (max(dum)>1), 10);
		weight = weight / (max(weight)>1)
		weight = 1. - exp(-10. * weight)

		factum=weight*0.
		factum[where(sunzen gt 80.)]=1.
		factum[where(sunzen lt 65.)]=0.
		tmpwo=where(sunzen ge 65. and sunzen le 80.)
		factum[tmpwo]=(sunzen[tmpwo]-65.)/15.
		weight=1.-factum

		;Und dann werden die Bilder mit dem Gewicht Überlagert
		;ir daten skalieren
		ir_min		= 180. & ir_max		=270.
		ir_byte		= ir_min > float(ir) < ir_max
		ir_byte		= ir_max - ir_byte
		ir_byte		= 0. > ((ir_byte * 255.) / (ir_max - ir_min)) < 255.
		if keyword_set(no_ir) then ir_byte = ir_byte * 0b

		bwc = 180. > bt_tir108 < 270.
		bwc=bwc-180.
		bwc = (1.-(bwc)/max(bwc))*255.
		for k = 0l, 2l do img[k, *, *] = 0. > (weight * img[k, *, *] + (1-weight) * bwc) < 255.
		img = byte(img)
	endif

	img = transpose(img,[1,2,0])

	return, img

end
