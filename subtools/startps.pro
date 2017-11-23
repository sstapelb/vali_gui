pro startps, ps=ps, outname=outname, xscale=xscale, yscale=yscale, colors=colors, $
             verbose = verbose

common pskeys, psflag,ra,ga,ba

currentDevice=!D.Name

psflag=1
xs=2
ys=1

;---------------------------------------------------------------------
; Definiere Kreis als usersym (psym=8)
;

a=findgen(16)*(!PI*2/16.)
usersym, cos(a), sin(a), /fill


;-----------------------------------------------------------------------
dummy='idl'

if (keyword_set(outname)) then begin

	dummy=outname
endif

if (keyword_set(xscale)) then xs=xscale
if (keyword_set(yscale)) then ys=yscale

outpsname=strcompress(dummy+'.eps', /rem)

preview=1


if (keyword_set(ps)) then begin

	psflag=0
	outpsname=strcompress(dummy+'.ps', /rem)
	preview=0

endif
if keyword_set(verbose) then print, "[startps] Creating " + outpsname



deviceKeyword={xsize:8.0*xs,xoff:5/xs,ysize:7.5*ys,yoff:10/ys,filename:outpsname $
		, inches:0, color:1, bits_per_pixel:8, encapsulated:psflag $
		, landscape:0,preview:preview}
		
!P.FONT=0

tvlct,ra,ga,ba,/get

set_plot, 'PS', /copy


device, /helvetica, Font_Size=12,/isolatin ,_Extra=deviceKeyword


end



