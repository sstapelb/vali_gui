function adv_keyword_set, keyword
	return, (size(keyword,/type) < 1)
end

pro make_color_bar,mini					$; Minimum
		  ,maxi							$; Maximum
		  ,n_lev						$; Number of levels
		  ,tickv=tickv					$; User given tickvalues, overwrites mini,maxi,n_lev
		  ,continous=continous			$; continous color bar
		  ,position=position			$; position in normal coodinaten
		  ,orientation=orientation		$; horizonatal (0) or vertical(1)
		  ,bar_format=bar_format		$; format of the colorbar labels
		  ,charsize=charsize			$
		  ,charthick=charthick			$
		  ,bar_title=bar_title			$; of colorbar
		  ,bw=bw						$; bw    colorbar
		  ,blue=blue					$; blue  colorbar
		  ,red=red						$; red   colorbar
 		  ,green=green					$; green colorbar
 		  ,minv=minv					$; min of bw colorbar
  		  ,maxv=maxv					$; min of bw colorbar
  		  ,g_eq = g_eq, l_eq = l_eq 	$
		  ,invers=invers        		$
		  ,col_table = col_table		$
		  ,dont_scale = dont_scale		$
		  ,col_center = col_center 		$
		  ,logarithmic = logarithmic 	$
		  ,bar_tickname = bar_tickname 	$
		  ,brewer = brewer				$ ; take brewer color table instead of default
		  ,_extra = _extra

	maxi = adv_keyword_set(maxi) ? (keyword_set(logarithmic) ? (10.^maxi) : maxi) : (keyword_set(logarithmic) ? 01. : 000.)
	mini = adv_keyword_set(mini) ? (keyword_set(logarithmic) ? (10.^mini) : mini) : (keyword_set(logarithmic) ? 10. : 255.)
	if mini eq maxi then maxi = mini + 1
	if keyword_set(tickv) then begin
		mini=min(tickv)
		maxi=max(tickv)
		if mini eq maxi then maxi = mini + 1
		n_lev= 1 > (n_elements(tickv)-1)
		n_tickv = n_elements(tickv)
; 		tickv=bytscl(tickv)
	endif else begin
		if keyword_set(bar_tickname) then tickv = (vector(float(mini),float(maxi),n_lev)) else $
		tickv	= keyword_set(logarithmic) ? (10^(vector(alog10(mini),alog10(maxi),n_lev+1))) : (vector(float(mini),float(maxi),n_lev+1))
		n_tickv = n_elements(tickv)
	endelse

	tickname = [strcompress(string(tickv,format=bar_format), /remove_all)]
	if keyword_set(bar_tickname) then begin
		strdum = strarr(n_elements(bar_tickname)*2+1)+' '
		for i = 0,n_elements(bar_tickname)-1 do strdum[i*2+1] = bar_tickname[i]
		tickname = strdum
	endif
	if keyword_set(l_eq) then begin
		dum = min(tickv, min_idx)
		tickname[min_idx] = textoidl('\leq') + ' ' + tickname[min_idx]
	endif
	if keyword_set(g_eq) then begin
		dum = max(tickv, max_idx)
		tickname[max_idx] = textoidl('\geq') + ' ' + tickname[max_idx]
	endif

	if keyword_set(orientation) then begin
		if not keyword_set(position) then begin  		;idl 5.2 bug
			plot, [0,1], [mini,maxi], /nodata, /noerase, charsize = charsize,charthick=charthick			, $
				xstyle = 4, xticklen = 0.00, xgridstyle = 0 							, $
				ystyle = 4, yticklen = 0.00, ygridstyle = 0							, $ 
				ytickv = tickv, yticks = n_lev, ytickname = tickname, ytitle = bar_title, ylog = logarithmic
		endif else begin
			plot, [0,1], [mini,maxi], /nodata, /noerase, charsize = charsize, position = position			, $
				xstyle = 4, xticklen = 0.00, xgridstyle = 0,charthick=charthick 				, $
				ystyle = 4, yticklen = 0.00, ygridstyle = 0							, $ 
				ytickv = tickv, yticks = n_lev, ytickname = tickname, ytitle = bar_title, ylog = logarithmic
		endelse
	endif else begin
		if not keyword_set(position) then begin  		;idl 5.2 bug
			plot, [mini,maxi], [0,1], /nodata, /noerase, charsize = charsize					, $
				ystyle = 4, yticklen = 0.00, ygridstyle = 0 ,charthick=charthick				, $
				xstyle = 4, xticklen = 0.00, xgridstyle = 0							, $
				xtickv = tickv, xticks = n_lev, xtickname = tickname, xtitle = bar_title, xlog = logarithmic
		endif else begin
			plot, [mini,maxi], [0,1], /nodata, /noerase, charsize = charsize, position = position			, $
				ystyle = 4, yticklen = 0.00, ygridstyle = 0 ,charthick=charthick				, $
				xstyle = 4, xticklen = 0.00, xgridstyle = 0							, $ 
				xtickv = tickv, xticks = n_lev, xtickname = tickname, xtitle = bar_title, xlog = logarithmic
		endelse
	endelse

	x  = !d.x_size * !x.window
	y  = !d.y_size * !y.window

	if (!d.flags and 1) eq 1 then begin 	; device has scalable pixel size
		dx = (x[1] - x[0])>1
		dy = (y[1] - y[0])>1
		dum_bar=findgen(255)
	endif else begin              			; pixels are non-scalable
		dx = (ceil(x[1]) - floor(x[0]))>1
		dy = (ceil(y[1]) - floor(y[0]))>1
		dum_bar = keyword_set(orientation) ? findgen(dy) : findgen(dx)
	endelse

	dum_bar = dum_bar / float(n_elements(dum_bar) - 1.) * (maxi - mini) + mini

	if not keyword_set(continous) then begin
		if n_tickv gt 1 and keyword_set(bar_tickname) then begin
			dum_bar = congrid(tickv,n_elements(dum_bar),/center)
		endif else begin
			for i=0,n_lev-1 do begin
				dum_bar[where((dum_bar ge tickv[i]) and (dum_bar lt tickv[i+1]))] = tickv[i]
			endfor
		endelse
; 		tickv = ( (maxi - mini) + mini) / (n_elements(tickname)-1) * findgen(n_elements(tickname))
	endif
	if keyword_set(bar_tickname) then tickv = ( (maxi - mini) + mini) / (n_elements(tickname)-1) * findgen(n_elements(tickname))

	;dum_bar=mbytscl(dum_bar,bot=mini,top=maxi)
	dum_bar = byte(0 > ((dum_bar - mini) / (maxi - mini) * 255) < 255)

	if keyword_set(invers)      then dum_bar=reverse(dum_bar)
	if keyword_set(orientation) then dum_bar=transpose(dum_bar)

	case 1 of
		keyword_set(bw)    : dum_bar=[[[dum_bar]],[[dum_bar]],[[dum_bar]]]
		keyword_set(red)   : dum_bar=[[[dum_bar]],[[dum_bar*0]],[[dum_bar*0]]]
		keyword_set(green) : dum_bar=[[[dum_bar*0]],[[dum_bar]],[[dum_bar*0]]]
		keyword_set(blue)  : dum_bar=[[[dum_bar*0]],[[dum_bar*0]],[[dum_bar]]]
		else: begin
			if adv_keyword_set(col_center) then begin
				dum_c	= vector(float(mini), float(maxi), 1000.)
				dum_c	= transpose(reform([[[dum_c]],[[dum_c]]]))
				print, minmax(dum_c)
				dum_bar	= bw_to_color(dum_c, col_table = col_table, dont_scale = dont_scale, col_center = col_center,brewer = brewer)
			endif else begin
				dum_bar	= bw_to_color(float(dum_bar), col_table = col_table, dont_scale = dont_scale, brewer = brewer)
			endelse
		endelse
	endcase
	if (!d.flags and 1) eq 1 then begin 	; device has scalable pixel size
		bar = dum_bar
	endif else begin              			; pixels are non-scalable
		bar = lonarr(dx,dy,3)
		if keyword_set(orientation) then begin
			bar = congrid(dum_bar,dx,dy,3)
		endif else begin
			for i=0,2 do bar[*,*,i]=transpose(congrid(transpose(dum_bar[*,*,i]),dy,dx))
		endelse
	endelse
	tv,bar,x[0] + 1,y[0],true=3,xsize=dx,ysize=dy,/device

	if keyword_set(orientation) then begin
; 		axis, 	yaxis = 0, ystyle = 1, yrange = [mini, maxi], yticklen = 0.25	, $
; 			ygridstyle = 0, yticks = n_lev, ytickname = tickname		, $
; 			ytickv = tickv, ytitle = bar_title, ylog = logarithmic, charsize = charsize,charthick=charthick
; 		axis, 	yaxis = 1, ystyle = 1, yrange = [mini, maxi], yticklen = 0.25	, $
; 			ygridstyle = 0, yticks = n_lev, ytickv = tickv	 		, $
; 			ytickname = strarr(n_lev + 1) + ' ',	 ylog = logarithmic
		axis, yaxis = 0, ystyle = 1, yrange = [mini, maxi], yticklen = 0.25	, $
			ygridstyle = 0, yticks = n_elements(tickname) -1, ytickname = tickname		, $
			ytitle = bar_title, ylog = logarithmic, charsize = charsize, charthick=charthick
		axis, yaxis = 1, ystyle = 1, yrange = [mini, maxi], yticklen = 0.25	, $
			ygridstyle = 0, yticks = n_elements(tickname) -1, ytickv = tickv	 		, $
			ytickformat = "(A1)", ylog = logarithmic
		axis, xaxis = 0, xstyle = 1, xrange = [0, 1], xticklen = 0.0001	, $
			xgridstyle = 0, xtickformat = "(A1)", xticks = 1
		axis, xaxis = 1, xstyle = 1, xrange = [0, 1], xticklen = 0.0001	, $
			xgridstyle = 0, xtickformat = "(A1)", xticks = 1
	endif else begin
; 		axis, 	xaxis = 0, xstyle = 1, xrange = [mini, maxi], xticklen = 0.25	, $
; 			xgridstyle = 0, xticks = n_lev, xtickname = tickname		, $
; 			xtickv = tickv, xtitle = bar_title, xlog = logarithmic, charsize = charsize,charthick=charthick
; 		axis, 	xaxis = 1, xstyle = 1, xrange = [mini, maxi], xticklen = 0.25	, $
; 			xgridstyle = 0, xticks = n_lev, xtickv = tickv			, $
; 			xtickname = strarr(n_lev + 1) + ' ', xlog = logarithmic
		axis, 	xaxis = 0, xstyle = 1, xrange = [mini, maxi], xticklen = 0.25	, $
			xgridstyle = 0, xticks = n_elements(tickname) -1, xtickname = tickname		, $
			xtitle = bar_title, xlog = logarithmic, charsize = charsize,charthick=charthick
		axis, 	xaxis = 1, xstyle = 1, xrange = [mini, maxi], xticklen = 0.25	, $
			xgridstyle = 0, xticks = n_elements(tickname) -1, xtickv = tickv			, $
			xtickformat = "(A1)", xlog = logarithmic
		axis, 	yaxis = 0, ystyle = 1, yrange = [0, 1], yticklen = 0.0001		, $
			ygridstyle = 0, yticks = 1, ytickformat = "(A1)"
		axis, 	yaxis = 1, ystyle = 1, yrange = [0, 1], yticklen = 0.0001	, $
			ygridstyle = 0, yticks = 1, ytickformat = "(A1)"
	endelse

end
