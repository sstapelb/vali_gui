FUNCTION saturation_pressure,T,ice=ice
; Returns  saturation pressure of wv in air ; Rogers&Yau Eqns 2.16 and 2.17
; Input:
;          T : Temperature [K]
;
; Output:
;         saturation pressure : [PA]
;
   e_sat = 611.2*exp(17.67*(T-273.16)/(T-273.16+243.5))
   
   IF KEYWORD_SET(ice) THEN BEGIN
     ind = WHERE(T LT 273.16,cnt)
     IF cnt NE 0 THEN  e_Sat[ind]=e_sat[ind]*(T[ind]/273.)^2.66
   ENDIF
   IF N_ELEMENTS(e_Sat) EQ 1 THEN e_Sat=e_sat[0] 
   
  RETURN,e_sat
END
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION density_moist_air_si, T, P, E
	;density of moist air in kg/m^3
	;input parameters 
    ;     T : Temperature[K], 
    ;     P : TOTAL air pressure (dry+wv) [Pa], 
    ;     E : Water Vapor partial pressure [Pa]  
	
	rd = 287.05
	rw = 461.5
	
    ; catch error here if there's more water vapor than total pressure...
    IF TOTAL(e GT p) THEN STOP
    
    rhoair = (p-e)/rd/t + e/rw/t

    RETURN, rhoair
END
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION tr2nh_export,t,r,p=p,ctt=ctt,factor=factor,k=k,lwp=lwp,hgeo=hgeo,cdnc=cdnc,cw=cw
 ;
 ; INPUT: 
 ;
 ; t      : opt thickness
 ; r      : [micron] reff radius 
 ;
 ; KEYWORDS: 
 ;
 ; ctt    : in (default 273.16)   : Cloud top temp [k]  
 ; factor : in (default 0.8)      : Subadiabaticity factor (use 0.8 or so for Sc)
 ; k      : in (default 0.8)      : Martin et al. (1994) ratio between R_v^3 and r_Eff^3
 ; p      : in (default 850E2)    : cloud top pressure (optional) [Pa]      
 ; cdnc   : out                   : CDNC in [cm^-3]
 ; hgeo   : out                   : Adiabatic geometrical thickness [m]
 ; lwp    : out                   : Liquid water path [kg/m^2]
 ; cw     : out                   : Condensation rate [kg/m^4]
 ; 
 ;
 ; RETURNS 
 ;   cdnc [cm^-3]
 ;
 ; Ralf Bennartz, updated 10 January 2011
 ;

;MST KEYWORD_DEFAULT,ctt   ,273.16           ; cloud temperature 
;MST KEYWORD_DEFAULT,factor,0.8              ; subadiabiaticity factor.... 
;MST KEYWORD_DEFAULT,k     ,0.8              ; Martin et al. (1994) ration between R_v and r_Eff
;MST KEYWORD_DEFAULT,p     ,ctt*0.0 + 850E2  ; cloud top pressure

; ctt=273.16
 factor=0.8
 k=0.8
 ;p=ctt*0.0 + 850E2  ; cloud top pressure                                                                                                                                                                   
 
; calculate condensation rate from cloud top temperature (ctt [K])

 rho = density_moist_air_si(ctt,p,SATURATION_PRESSURE(ctt))
 cw  = factor * rho * EXP(0.339568*ctt -0.000556085*ctt^2.0  -64.7094)

 q    = 2.0    ; extinction efficiency in MIE limit
 rhow = 1000.  ; density of liquid water [kg/m^3]
 
 lwp  = 5./9. * t * r * 1E-6 * rhow ; [kg/m^2]
 hgeo = (2.0/cw*lwp)^0.5            ; [m]
 cdnc = 2.0^(-5./2.)/k*t^3.0*lwp^(-5.0/2.)*(3./5.*!PI*q)^(-3.0)*(3./4/!PI/rhow)^(-2.0)*cw^0.5

 cdnc = cdnc * 1E-6 ; cm^-3
  
 RETURN,cdnc

END
