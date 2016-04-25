
function BRITE_M, v,r
 
	;c ... Planck constant (Joule second)
	h = 6.62606876d-34

	;c ... Speed of light in vacuum (meters per second)
	c = 2.99792458d+08

	;c ... Boltzmann constant (Joules per Kelvin)
	k = 1.3806503d-23

	;c ... Derived constants
	c1 = 2.0d0 * h * c * c
	c2 = (h * c) / k

	; ... Set default return value
	brite_m = -1.0

	; ... Check input parameters and return if they are bad
; 	if (v le 0.0 or r le 0.0) then return,brite_m

	; ... Convert wavenumber to inverse meters
	vs = 1.0d+2 * double(v)

	; ... Compute brightness temperature
	brite_m = float(c2 * vs / alog(c1 * vs^3 / (1.0d-5 * double(r)) + 1.0d+0))

	return,brite_m

END 


function BRIGHT_M, W, R


	;c ... Planck constant (Joule second)
	h = 6.62606876d-34

	;c ... Speed of light in vacuum (meters per second)
	c = 2.99792458d+08

	;c ... Boltzmann constant (Joules per Kelvin)
	k = 1.3806503d-23

	;c ... Derived constants
	c1 = 2.0d+0 * h * c * c
	c2 = (h * c) / k

	; ... Set default return value
	bright_m = -1.0

	; ... Check input parameters and return if they are bad
; 	if (w le 0.0 or r le 0.0)  then return,brite_m

	; ... Convert wavelength to meters
	ws = 1.0d-6 * double(w)

	; ... Compute brightness temperature
	bright_m = float(c2 / (ws * alog(c1 / (1.0d+6 * double(r) * ws^5) + 1.0d+0)))

	return,bright_m

END
