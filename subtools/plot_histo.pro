PRO bbbox, X0, Y0, X1, Y1, color=color
	POLYFILL, [X0, X0, X1, X1], [Y0, Y1, Y1, Y0], COL = color
END
FUNCTION plot_histo, $
                       array, $
		       MIN  = MIN, $
		       MAX  = MAX, $
		       BIN  = BIN,   $
		       OPLOT = OPLOT, $
		       norm = norm, $
		       color=color,	$
		       cumulative=cumulative, $
		       barplot=barplot,	$
		       barsize=barsize,	$
		       xachse=xachse,	$
		       _EXTRA = _EXTRA

IF NOT KEYWORD_SET(BIN)  THEN BIN = 1.
IF NOT KEYWORD_SET(MIN) THEN MIN = MIN(array)
IF NOT KEYWORD_SET(MAX) THEN MAX = MAX(array)
if not keyword_set(color) then color=!p.color
if not keyword_set(barsize) then bs=1. else bs=barsize
h = histogram(array, MIN = MIN, MAX = MAX, BIN = BIN)

n = N_ELEMENTS(h)
x = FINDGEN(n) * BIN + MIN
       
YTITLE = '# of occur.'

if keyword_set(cumulative) then begin
	ytitle="cum. % of occur."
	ii=1l
	hh=h
	while ii lt n_elements(h) do begin
		hh(ii)=hh(ii-1l)+h(ii)
		ii=ii+1l
	endwhile	
	h=float(hh)/float(total(h))
endif


if keyword_set(norm) then begin
	case norm of 
	1: begin
		h=float(h)/float(max(h))*100.
		ytitle="% of occur."
	   end
	3: begin
		h=float(h)/float(max(h))
		ytitle="% of occur."
	   end
	2: begin
		h=float(h)/float(total(h)) * 100.
		ytitle="% of occur."
		end
	else:  
	endcase    	
endif	

IF KEYWORD_SET(OPLOT) then begin
	if not keyword_set(barplot) then begin
		oplot, x, h,color=color, _extra = _extra 
	endif else begin
		for i=0,n_elements(x)-1l do 	$
		bbbox,x(i)-bin/3.*bs,0,x(i)+bin/3.*bs,h(i),color=color
	endelse	
endif ELSE begin
	PLOT, x, h, $
      	 XTITLE = 'value', $
       	YTITLE = ytitle, $
       	TITLE = 'binsize = ' + strcompress(string(bin),/rem), $
       	_EXTRA = _EXTRA,/nodata
	if not keyword_set(barplot) then begin
		oplot, x, h,color=color, _extra = _extra 
	endif else begin
		for i=0,n_elements(x)-1l do 	$
		bbbox,x(i)-bin/3.*bs,0,x(i)+bin/3.*bs,h(i),color=color
	endelse		
endelse	
xachse=x
RETURN, h
END
