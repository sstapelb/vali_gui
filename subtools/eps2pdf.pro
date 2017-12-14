
pro eps2pdf, infile, outfile, ok = ok, verbose = verbose, remove_epsfile = remove_epsfile

	ok = 1
	if not file_test(infile,/read) then begin
		print, 'Infile not found or not readable! : '+infile
		ok = 0
		return
	endif
	if strupcase(file_name_info(infile,/ext)) ne 'EPS' then begin
		print, 'Is infile an EPS? : '+infile
		ok = 0
		return
	endif
	if keyword_set(outfile) then begin
		outpath = file_dirname(outfile)+'/'
		if not file_test(outpath,/dir,/write) then begin
			print, 'Output directory does not exist or is not writeable! : '+outpath
			ok = 0
			return
		endif
	endif

	; epstopdf is buggy benutze neues script falls vorhanden
	epstopdf = '/cmsaf/nfshome/sstapelb/skripte/epstopdf_new.pl'
	if file_test(epstopdf) then begin
		if keyword_set(verbose) then spawn, epstopdf+' '+infile+' -o '+(keyword_set(outfile) ? outfile : '') $
		else spawn, epstopdf+' '+infile+' -o '+(keyword_set(outfile) ? outfile : ''), result, error_result
	endif else if is_installed('epstopdf') then begin
		spawn_arr = ['epstopdf',infile,'-o',keyword_set(outfile) ? outfile : '']
		if keyword_set(verbose) then spawn, spawn_arr, /noshell else $
		spawn, spawn_arr, /noshell, result, error_result
	endif else begin
		print,'This procedure requires "epstopdf" to be installed on your unix system!'
		ok = 0
		return
	endelse

	if keyword_set(remove_epsfile) then begin
		if keyword_set(verbose) then print, 'Removing input EPS file' + infile
		file_delete, infile, /allow_nonexistent
	endif
	if not file_test((keyword_set(outfile) ? outfile : file_dirname(infile)+'/'+file_basename(infile,'eps')+'pdf'),/read) then ok = 0

end