
function adv_tempname, extension, basename = basename, path = path

	ext  = keyword_set(extension) ? extension : 'dum'
	path = keyword_set(path)      ? path      : '/tmp/'
	name = keyword_set(basename)  ? basename  : 'tempfile_from'

	idlpid   = strcompress(call_external(!Dlm_path+'/libidl.so','getpid',/cdecl),/rem)
	user     = getenv('USER')
	host     = getenv('HOST')

	ok = 0
	while not ok do begin
		zufall = string(randomu(seed)*1000., format='(i3.3)')
		filename = path+name+'_'+user+'_idlsession_'+idlpid+'_at_'+host+'__'+zufall+'.'+ext
		if not total(file_test(filename)) then return, filename
	endwhile

end