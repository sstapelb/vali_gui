;calculates days of month (dom)

function dom, yr, mm, anz, number_of_days = number_of_days

	if n_params() lt 2 then begin
		print,"Syntax : result = dom(year,month,count,number_of_days = number_of_days"
		return,-1l
	endif

	if total(mm eq [4,6,9,11]) then anz = 30 else anz = 31
	if (mm eq 2) then anz = 28
	if ( (mm eq 2) and (yr mod 4 eq 0) and (yr mod 100 ne 0) ) $
	or ( (mm eq 2) and (yr mod 400 eq 0) ) then anz = 29

	if keyword_set(number_of_days) then return,anz else $
	return, string(indgen(anz)+1,format='(i2.2)')

end