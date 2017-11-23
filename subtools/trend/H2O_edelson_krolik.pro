;discrete autocorrelation function from Edelson & Krolik
;
;data1: 2 dim vector, with 1. column=time, 2. column=data
;lag=lag
;
; Identity to original code
PRO H2O_edelson_krolik,data1,lag,out
   
   lag=-1   ; not used in original software from S. Mieruch, IUP, Bremen
   t=REFORM(data1[*,0])
   x=REFORM(data1[*,1]-MEAN(data1[*,1]))
   
   L=(SIZE(x))[1]
;    v=(MOMENT(x))[1] & IF v eq 0. THEN stop
   v=(MOMENT(x))[1] 
   
   a=WHERE(t(0:L-2)-t(1:L-1l) eq -1l)
   
   DCF=-999. & sigma_sq=-999. & sigma=-999.
;    out=[-999.,-999.] & LDa=N_ELEMENTS(a)

   LDa=N_ELEMENTS(a)

;    IF LDa gt 1l THEN BEGIN
   IF LDa gt 1l and v ne 0 THEN BEGIN
   
      ; t(a) gives the first month
      ; autocorrelation with 1 month time lag
      UCDF=(x[a]*x[a+1l])/v
    
      DCF=TOTAL(UCDF)/LDa
      sigma_sq=(1./(LDa-1.)*TOTAL((UCDF-DCF)^2))
      sigma=sqrt(sigma_sq/LDa)	;fehler des mittelwertes
   
;       out=[DCF, sigma]
   
   ENDIF

   out=[DCF, sigma]

END
