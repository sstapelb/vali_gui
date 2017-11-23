; NAME:
;	remove_equal_elements.pro
; PURPOSE:
;	Returns indexes of an array with equal elements removed.
;
; CALLING SEQUENCE:
;	result = remove_equal_elements(array, sort = sort)
; 
; 	keywords : 
; 		sort  ...  array[result] is sorted lowest to highest value
;
; EXAMPLE:
; 	array = [-2,-3,-4,0,3,0,3,5,6,7,1,-2]
; 	index = remove_equal_elements(array)
; 	print, array[index] returns : [-3,-4, 0, 3, 5, 6, 7, 1,-2]
; 
;	index = remove_equal_elements(array,/sort)
;	print, array[index] returns : [-4,-3,-2, 0, 1, 3, 5, 6, 7]
; 
; MODIFICATION HISTORY:
;	Written by S. Stapelberg, 2010.
; 
function remove_equal_elements, array, sort = sort, count, complement = complement, ncomplement=ncomplement

	dum_idx = (sort(array))[uniq(array[sort(array)])]
	dum_arr = lonarr(n_elements(array))

	if ~keyword_set(sort) then dum_idx = dum_idx[sort(dum_idx)]

	count = n_elements(dum_idx)
	dum_arr[dum_idx] = 1
	complement = where(dum_arr eq 0, ncomplement) 

	return, dum_idx

end
