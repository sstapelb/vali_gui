; NAME:
;         trend_det (program)
;
; PURPOSE:
;
;         Dertermine trends in unit/month
;
; CATEGORY:
;
;        basic software for trend determination
;
; CALLING SEQUENCE:
;        trend_det
;
; INPUTS:
;                   see first lines in code
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;                   matrix with trend and fit parameters
;
; COMMON BLOCKS:    none
;
; SIDE EFFECTS:     none
;
; RESTRICTIONS:     Currently designed for water vapour data
;
;
; PROCEDURE:        none
;
; EXAMPLE:          none
;
; COMMENTS:         ENSO Index available only for 1988(or 1996)-2005
;                 
;                   IDL matrix def: (col,row) ; for-loops should run over rows - faster
;                   IDL matrix multiplication: # - column x row; ## row x column
;
;   	    	    Anomalies are not saved but determined and can be plotted together
;   	    	    with trend - decomment lines in code.
;
;   	    	    For mathematical details see S. Mieruch, Dissertation, 2009
;   	    	    or S. Mieruch et al., 2008, ACP
;
;                   Ca. 1 min for HOAPS 1996-2005 (136000 valid spatial pixels)
;
; MODIFICATION HISTORY:
;
;   (c) Marc Schroeder, DWD/KU22 (CM-SAF) 03.11.2009 first implementation
;       marc.schroeder@dwd.de
;
;-
;-------------------------------------------------------------------------------------
;
; -----------------------------------------------------------
; Below is the original header from S. Mieruchs software
;
; Path to original software:
; /home/mschroed/werkv/unibremen/VS_MIERUCH_SCRIPTS_2009/TRENDS/
;
; usage: P=HOAPS_TRENDS_ENSO(data,name,[plotting,[Anom_k,[EL]]])
;
; data:       (M x N) Matrix containing rows of data, thus as the
;             time variable I use 1,2,3,4,...,N. 
;	     MS: M is space dimension.
;	     NaN have to be used for missing data.
;
; name:       String indicating the output file. Two files are written
;	     to disk. "name"_paras.dat contains the fit parameters and
;	     "name"_erros.dat contains the standard errors of the parameters.
;	     The parameters and errors are written corresponding to the 
;	     Z - model. This means, look at the model declaration Z.
;	     For example Z(:,1) is the constant corresponding to mus, 
;	     Z(:,2) is the time corresponding to omega and so on.
;	     MS: para/error dimension is space dim x number paras	      
;
; plotting:   Optional parameter plotting=1 plots the data 
;	     and the fit and writes the data and the fit to disk
;	     into the file "name"_fit.dat. First column is the time,
;	     second is the data, third is the complete fit, fourth
;	     is only the offset, the trend and the level shift, fifth
;	     is the deseasonlised data.
;	     plotting=0 is default and skips the above.
;
; Anom_k:     Optional, but if used, then plotting must also be used.
;	     Anom_k=1 saves deseasonalised and El Nino corrected data in "name"_anom.dat.
;	     MS: difference between data and fit (BUT: y-axis intersept and trend still included)
;
; EL:	     Optional, but if used plotting and Anom_k have also to be used.
;	     ENSO index, e.g. JMA
;
; MS COMMENT: SM: optionals are really optional (anom_k can be used without plotting 
;	     being activated). They must be named, i.e., 0 or 1.
;
; additional programs needed: H2O_trends_edelson_krolik.m
;
;
; HOAPS_TRENDS_ENSO.m is based on
;
; Mieruch, S., Noel, S., Bovensmann, H., and Burrows, J. P.: 
; Analysis of global water vapour trends from satellite 
; measurements in the visible spectral range, Atmos. Chem. 
; Phys., 8, 491-504, 2008
;
; and Weatherhead et. al. (1998): Factors affecting the detection of trends:
; Statistical considerations and applications to environmental data
;
; General information on multivariate least squares and the estimation
; of errors of fit parameters can be found e.g. in Fahrmeir et al. (2004): Statistik
;
;Copyright S.Mieruch (2009)
;sebastian.mieruch@iup.physik.uni-bremen.de
;
@./H2O_edelson_krolik.pro
@./replicate_array.pro
function give_trend, dat0a, valmin, valmax, yearmin, yearmax, ectdat

	sizi=size(dat0a)

	if(sizi[0] eq 1) then dat0=reform(dat0a,1,1,n_elements(dat0a[*]))
	if(sizi[0] eq 2) then dat0=reform(dat0a,1,n_elements(dat0a[*,0]),n_elements(dat0a[0,*]))
	if(sizi[0] eq 3) then dat0=dat0a

	dim=lonarr(3)
	dim[0]=n_elements(dat0[*,0,0])
	dim[1]=n_elements(dat0[0,*,0])    	    	; data in fdat is saved as vector, give 2d Dim here
	dim[2]=n_elements(dat0[0,0,*])

; 	enso_file='/cmsaf/cmsaf-cld6/mstengel/data/ENSO/enso2.txt';jmasst1868-today_filter-5'
	enso_file='/cmsaf/nfshome/sstapelb/idl/trend/censo.data'
	EL=1         	    	    	    	
	ENSO=matrix_einlesen(enso_file)
	minyidx=where(ENSO[0,*] eq yearmin,mincnt)
	maxyidx=where(ENSO[0,*] eq yearmax,maxcnt)
	if mincnt ne 1 or maxcnt ne 1 then begin	; TURN ENSO OFF if period is outside 1948-2016
		print,'ENSO de-trending is turned off because the enso_file is out of range ',enso_file
		print,'Try to download an updated file. Use link from the enso_file header.'
		EL=0
	endif

	ECT=keyword_set(ectdat)
	print,ect

	IF EL  eq 1 THEN fitparas=10+1 ELSE fitparas=10  ; +1 due to ENSO
	IF ECT eq 1 THEN fitparas=fitparas+1
	add=2    ; additional paras, +2 due to phi, sigma_e for paras, sigma_phi for errors

	;rearranging dat0[a,b,c] to dat[a*b,c]
	dat=reform(dat0,dim[0]*dim[1],dim[2])
	;there needs to be a rotate at the end of this program to bring it back to
	;original projection

	ENSO=ENSO[1:*,minyidx:maxyidx]
	ENSO=ENSO[*]

	sss=SIZE(dat) & Lines=sss[1] & Cols=sss[2]

	; 4 frequencies implemented (1,1/2,1/3,1/4 yearly)
	freq1=2.*!pi/12. & freq2=2.*!pi/(12./2.)
	freq3=2.*!pi/(12./3.)
	freq4=2.*!pi/(12./4.)

	; time
	t=FINDGEN(Cols)

	Z=FLTARR(Cols,fitparas)
	Z(*,0)=1
	Z(*,1)=t
	Z(*,2)=sin(freq1*t)
	Z(*,3)=cos(freq1*t)
	Z(*,4)=sin(freq2*t)
	Z(*,5)=cos(freq2*t)
	Z(*,6)=sin(freq3*t)
	Z(*,7)=cos(freq3*t)
	Z(*,8)=sin(freq4*t)
	Z(*,9)=cos(freq4*t)
	IF EL eq 1 THEN Z(*,10)=ENSO
	IF ECT eq 1 and EL ne 1 THEN Z(*,10)=ectdat
	IF ECT eq 1 and EL eq 1 THEN Z(*,11)=ectdat

	; get pixel based trend
	b_errs_all=FLTARR(2,Lines,fitparas+add)-999.	; +2 due to phi, sigma_e for paras, sigma_phi for errors
	anom_all=FLTARR(Lines,Cols)-999.
	diac=FINDGEN(fitparas)
	FOR i=0l,Lines-1l DO BEGIN
		IF (i mod 10000) eq 0 THEN PRINT,'position: ',i,lines

		indices=WHERE(REFORM(dat[i,*]) ge valmin and REFORM(dat[i,*]) le valmax)

		IF indices[0] ne -1l THEN BEGIN
			Y=REFORM(dat[i,indices]) & tim=t[indices]
			L=N_ELEMENTS(Y)
			IF L gt fitparas THEN BEGIN   ; this is needed due to the error estimate (P'X)
				X=Z[indices,*]

				;b=((X'*X)^-1)*X'*Y;
				; comment: matrix multiplication (faster if # and transpose is applied)
				; is row x column, in IDL ##, but
				; a) IDL's matrix_multiply does column x row, in IDL #
				; b) in IDL matrix is defined as (col, row)
				; that is why the code below is correct - two inconsistencies compensate
				; each other
				dum=INVERT(MATRIX_MULTIPLY(X,X,/ATRANSPOSE),/DOUBLE)
				b=MATRIX_MULTIPLY(dum,X,/BTRANSPOSE)#Y

				; determine fit by using ALL fitted parameters
				bm=replicate_array(b,L)
				bfit=TOTAL(TRANSPOSE(bm)*X,2)

				; determined seasonal and ENSO contributions, here no trend is
				; included/considered
				bm=replicate_array(b[2:*],L)
				sefit=TOTAL(TRANSPOSE(bm)*X[*,2:*],2)+b[0]

				; plot anomalies and overlay trend
				anom=y-sefit & trend=b[1]*tim       
				anom_all[i,indices]=anom

				; autocorrelated noise
				noise=Y-bfit
				; prepare to remove autocorrelation from noise
				H2O_edelson_krolik,[[tim],[noise]],-1,out 
				IF out[0] gt -990. THEN BEGIN   ; this rarely happens for HOAPS 1996-2006

					epsilon=FLTARR(L)
					; remove autocorrelation from noise
					epsilon[0]=SQRT(1.-out[0]^2.)*noise[0]
					epsilon[1:*]=noise[1:*]-out[0]*noise(0:L-2)
					var_epsilon=(MOMENT(epsilon))[1]

					P_prime=FLTARR(L,L) & dia=FINDGEN(L)
					P_prime[dia,dia]=1. & P_prime[0,0]=SQRT(1.-out[0]^2.)
					P_prime[dia+1l,dia]=-out[0]

					X_star=P_prime#X
					Y_star=P_prime#Y

					; determine coefficients, after deautocorrelation
					dum=INVERT(MATRIX_MULTIPLY(X_star,X_star,/ATRANSPOSE),/DOUBLE)
						b_star=MATRIX_MULTIPLY(dum,X_star,/BTRANSPOSE)#Y_star

					cov_B=var_epsilon*INVERT(MATRIX_MULTIPLY(X_star,X_star,/ATRANSPOSE),/DOUBLE)
					; here are the erros for the fit parameters b
					errs=SQRT(ABS((cov_B[diac,diac])))

					b_errs_all[1,i,*]=[errs,out[1],-999.]
					b_errs_all[0,i,*]=[b_star,out[0],var_epsilon]

				ENDIF
			ENDIF
		ENDIF
	ENDFOR

	coeff=REFORM(b_errs_all[0,*,*],dim[0],dim[1],fitparas+add)
	err=REFORM(b_errs_all[1,*,*],dim[0],dim[1],fitparas+add)

	slope=ROTATE(REFORM(coeff[*,*,1]),7)*12.
	err_sl=ROTATE(reform(err[*,*,1]),7)*12.

	anom_out=REFORM(anom_all[*,*],dim[0],dim[1],dim[2])

	; exclude slopes with slope<2*sigma and with too many missing months, i.e, # time
	; steps < fitparas. Then, autocorrelation can not be determined
	nr=WHERE(ABS(slope) le 2.*err_sl or slope lt -990. or err_sl lt 0.)
	slope_sig=slope & IF nr[0] ne -1l THEN slope_sig[nr]=-999.

	;introduced by MST to make it consistent with dat0->dat transformation above
	slope=rotate(slope,7)

	return,{slope:slope,anom:anom_out,enso:enso}


END
