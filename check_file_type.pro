function check_file_type, file


	idl_version = float(!VERSION.release)
	
	hdf5  = [137b,72b,68b,70b,13b,10b,26b,10b]
	ncdf3 = [ 67b,68b,70b]

	error=0
	catch,error
	dum=bytarr(8)
	if error eq 0 then begin
		openr,lun,file,/get_lun
		readu,lun,dum
		free_lun,lun
		print,dum
		print,string(dum)
		ret = total(dum eq hdf5) eq 8b
	endif else begin
		ret=0
	endelse

	return,ret
end
