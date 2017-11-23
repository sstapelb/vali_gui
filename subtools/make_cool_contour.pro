;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Erzeugt contourplots mit color bar sowohl
; auf ps als auch auf den Schirm (nur true color!!!!!!)
; Aufruf:

;make_cool_contour,bild,bild_X,bild_Y,			$
;		,n_level				$
;		,yrange=yrange 				$
;		,xrange=xrange 				$
;		,xtitle='x axxxe'			$
;		,ytitle='y axxxxe'			$
;		,bar_title=x_title_of_color_bar		$
;		,mini=minimum_of_color_bar		$
;		,maxi=maximum_of_color_bar		$
;		,format=format_string_of_bar 		$  z.B.: '(f6.2)'
;		,/bw					$  schwarz/weiss
;		,/lines					$  overplot mit lines
;		,nbar=nbar
; Achtung: mini=0 bedeutet,das mini nicht gesetzt ist
;(IDL keyword konvention! also 0.00001)
;		,/col_table				$ von stapel eingefuehrt (color_image raus bw_to_color rein, gleich wie bei view2d)
;		;/brewer				$ stapel take brewer color table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro make_bar,levels,position,title=title,bw=bw,format=format,lines=lines $
	    ,nbar=nbar,contin_bar=contin_bar,col_table=col_table,brewer=brewer

	if keyword_set (nbar) then $
			levels=vector(min(levels),max(levels),float(nbar))

	border=vector(0.,1.,n_elements(levels)+1)
	mittel=(border+shift(border,1))/2.
	mittel=[0,mittel(1:*),1]
	plot,[0,1],[0,0],ystyle=4		$
 	   ,xticks=n_elements(levels+2)		$
  	   ,xtickv=mittel				$
   	   ,xtickname=['  ',string(levels,format=format),'  ']  $
	   ,position=position				$
	   ,xtitle=title,/noerase



        if keyword_set(bw) then begin
		if keyword_set(contin_bar) then dum_bar=vector(20.,255.,2500.)  $
    	  	else dum_bar=vector(20,255,n_elements(levels)) 		; ps!!!!
		dum_bar=[transpose(dum_bar),transpose(dum_bar),transpose(dum_bar)]

 		if keyword_set(contin_bar) then   dum_bar=reform(transpose(dum_bar),2500,1,3)$
    	  	else dum_bar=reform(transpose(dum_bar),n_elements(levels),1,3)
     	endif else begin
; 		if keyword_set(contin_bar) then dum_bar=color_image(indgen(2500))				$
; 		   else dum_bar=color_image(indgen(n_elements(levels)))
		if keyword_set(contin_bar) then dum_bar=bw_to_color(indgen(2500),col_table=col_table,brewer=brewer)	$
		else dum_bar=bw_to_color(indgen(n_elements(levels)),col_table=col_table,brewer=brewer)
	endelse

;	dum_bar(3,*,0)=255
;	dum_bar(3,*,1)=0
; 	dum_bar(3,*,2)=255
;
  	x  = !d.x_size * !x.window
  	y  = !d.y_size * !y.window
  	if (!d.flags and 1) eq 1 then begin ; device has scalable pixel size
   	 dx = x[1] - x[0]
    	 dy = y[1] - y[0]
         bar=dum_bar
 	endif else begin              ; pixels are non-scalable
    	 dx = ceil(x[1]) - floor(x[0])
    	 dy = ceil(y[1]) - floor(y[0])
    	 bar= lonarr(dx,dy,3)
    	 for i=0,2 do bar(*,*,i)=transpose(congrid(transpose(dum_bar(*,*,i)),dy,dx))
    	endelse

  	oplot,[0,1]

        tv,bar,x(0),y(0),true=3,xsize=dx,ysize=dy,/device

	tv,bar,x(0),y(0),true=3,xsize=dx,ysize=dy,/device

	x_schlange=[border(0),border(1),border(1),border(0),border(0)]
	y_schlange=[0,0,1,1,0]

	if n_elements(levels) ge 2 then begin
	for i=1,n_elements(levels)-1 do begin
	  x_schlange=[x_schlange,border(i),border(i+1),border(i+1),border(i),border(i)]
	  y_schlange=[y_schlange,0,0,1,1,0]
	endfor
	endif
	plots,x_schlange,y_schlange




end

pro make_cool_contour,bild,bild_x,bild_y,n_lev		$
		     ,mini=mini,maxi=maxi		$
		     ,_extra=_extra			$
		     ,bar_title=bar_title		$
		     ,color_bar = color_bar		$
		     ,bw=bw,format=format, lines=lines	$
		     ,nbar=nbar,contin_bar=contin_bar	$
		     ,col_table = col_table		$
		     ,brewer=brewer			$
		     ,no_data_value=no_data_value		$
		     ,cc4cl_hovmoeller=cc4cl_hovmoeller

	no_data_val = keyword_set(no_data_value) ? no_data_value : -99999.

; 	if total(strmatch(tag_names(_extra),'xmargin',/fold)) then xmargin = _extra.xmargin
; 	if total(strmatch(tag_names(_extra),'ymargin',/fold)) then ymargin = _extra.ymargin
; 	plot,[0,0],[1,1],/nodata, xstyle=4, ystyle=4,xmargin=xmargin,ymargin=ymargin	;set window used by contour
	plot,[0,0],[1,1],/nodata, xstyle=4, ystyle = 4,_extra=_extra		;set window used by contour
	s_img = size(bild)					;Return to caller if an error occurs
	six = float(s_img[1])					;Image sizes
	siy = float(s_img[2])
	bar = (keyword_set(color_bar))?color_bar:0
	case bar of
		0:		begin & xscale = 1.		& yscale = 1.	& end
		1:		begin & xscale = 0.75	& yscale = 1.	& end
		2:		begin & xscale = 1.		& yscale = 0.85	& end
		else:	begin & xscale = 1.		& yscale = 1.	& end
	endcase
	px = !x.window * !d.x_vsize						;Get size of window in device units
	py = !y.window * !d.y_vsize
	swx = (px[1]-px[0])	* xscale					;Size in x in device units
	swy = (py[1]-py[0])	* yscale					;Size in Y
	aspi = six / siy								;Image aspect ratio
	aspw = swx / swy								;Window aspect ratio
	f = aspi / aspw									;Ratio of aspect ratios
	pos = [	px[0] + swx/xscale * (1. - xscale), $
		   	py[0] + swy / yscale * (1. - yscale), $
		   	px[0] + swx/xscale, $
		   	py[0] + swy/yscale]

	if is_defined(mini) then minii=mini else minii=min(bild)
	if is_defined(maxi) then maxii=maxi else maxii=max(bild)

; 	print,	minii,min(bild)
; 	print,	maxii,max(bild)

	if maxii lt max(bild) then 			$
	print,'Warning max of data is higher than max of bar. Plot could be ambiguous!'
	if minii gt min(bild) and min(bild) ne no_data_val[0] then 			$
	print,'Warning min of data is lower than min of bar. Plot could be ambiguous!'


	fill_levels=vector(minii,maxii,n_lev)

	line_levels=vector(minii,maxii,n_lev)
; 	c_labels = keyword_set(c_labels) ? c_labels : fltarr(n_lev)+1 ; jedes level kriegt label

	if keyword_set(bw) then begin
	   fill_col=vector(10,255,n_lev-1)
	   fill_col=[transpose(fill_col),transpose(fill_col),transpose(fill_col)]
 	   fill_col=reform(transpose(fill_col),n_lev-1,1,3)
; 	endif else fill_col=color_image(fill_levels)
	endif else fill_col=bw_to_color(fill_levels,col_table=col_table,brewer=brewer)

;	fill_col(3,*,0)=(255)
;	fill_col(3,*,1)=(0)
;	fill_col(3,*,2)=(255)

	if (!d.flags and 1) eq 1 then begin ; device has scalable pixel size
	   	tvlct,r,g,b,/get
 	  	tvlct, [0,fill_col(*,0,0)],[0,fill_col(*,0,1)],[0,fill_col(*,0,2)]
  	 	fill_col=(indgen(n_lev-1)+1)
 	endif else begin
 		device, get_decomposed = decomp
		if decomp eq 0 then begin
			Print,'make_cool_contour: Device will now set up with decomposed = 1'
			device, decomp = 1
		endif
 		fill_col=true_color_index(fill_col(*,0,0),fill_col(*,0,1),fill_col(*,0,2))
	endelse

;	position=[0.20,0.38,0.96, 0.95 ]

	contour, bild ,bild_x,bild_y		$
	,/follow				$
	,/close					$
 	,/fill					$
	,levels=line_levels			$
	,c_colors=fill_col			$
	,xstyle=1				$
	,ystyle=1				$
	,_extra=_extra				$
	,position=pos, /dev, /noerase

	if keyword_set(lines) then begin

		contour, bild ,bild_x,bild_y		$
		,/follow				$
		,/close					$
		,levels=line_levels			$
		,_extra=_extra				$
		, c_labels = c_labels			$
		,/overplot
	endif

	if (!d.flags and 1) eq 1 then tvlct,r,g,b
	if keyword_set(cc4cl_hovmoeller) then begin
		si     = size(bild,/dim)
		dum    = strpos(strlowcase(tag_names(_extra)), 'yticks')
		yticks = max(dum, index) eq 0 ? float((_extra).(index)) : si[1]
		dum    = strpos(strlowcase(tag_names(_extra)), 'ytickname')
		ytname = max(dum, index) eq 0 ? float(n_elements((_extra).(index))) : si[1]
		cc = 30
		if si[1] lt 100 then cc = 15
		if si[1] lt 50  then cc = 10
		if si[1] lt 20  then cc = 1
		dumm = findgen(ytname) * yticks/ytname
		for j = 0, ytname-1 do begin
			if (j mod cc) eq 0 then oplot,!x.crange,[dumm[j],dumm[j]],linestyle=1
		endfor
		dumm = (findgen(si[0]) mod 12) eq 0
		for j = 0, si[0]-1 do begin
			if dumm[j] eq 1 then oplot,[j,j],!y.crange,linestyle=2
		endfor
; 		stop
	endif

;	position=[!x.window(0),0.15,!x.window(1),0.2]

;	make_bar,fill_levels,position,title=bar_title,bw=bw,format=format,nbar=nbar,contin_bar=contin_bar
	case bar of
	1:	begin
			make_bar,fill_levels, $
					[	(px[0]+swx/xscale * (1. - xscale - 0.20))/!d.x_vsize, $
						py[0]/!d.y_vsize, $
						(px[0]+swx/xscale * (1. - xscale - 0.15))/!d.x_vsize, $
						(py[0]+swy)/!d.y_vsize], $
				title=bar_title,bw=bw,format=format,nbar=nbar,contin_bar=contin_bar,col_table=col_table,brewer=brewer
		end
	2:	begin
			make_bar,fill_levels, $
					[	px[0]/!d.x_vsize, $
						py[0]/!d.y_vsize, $
						(px[0]+swx/xscale)/!d.x_vsize, $
						(py[0]+swy/yscale*(1 - yscale - 0.1))/!d.y_vsize], $
				title=bar_title,bw=bw,format=format,nbar=nbar,contin_bar=contin_bar,col_table=col_table,brewer=brewer
		end
	else:
	endcase

end
