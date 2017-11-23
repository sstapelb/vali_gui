;-------------------------------------------------------------
;
;	Replicate Array
;
;	Vervielfaeltigt Felder, Vectoren und Skalare
;
;	z.B: f=replicate_array(farr, 5)
;
;	Lothar Schüller, Februar 1999
;
;-------------------------------------------------------------
function replicate_array,arr,n

dum=size(arr)

;if dum(0) eq 0 then return, replicate(arr,n) 

dims=dum[1:dum[0]]

n_arr=n_elements(arr)
index=lindgen(long(n_arr)*long(n)) mod long(n_arr)

dims=[dims, n]

return, reform(arr[index], dims)

end
