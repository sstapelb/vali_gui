;---------------------------------------------------------------------------------------------
;advanced_keyword_set(keyword) ermittelt ob keyword übergeben wurde so wie auch keyword_set()
;allerdings mit dem vorteil, dass auch die moeglichkeit besteht eine null als keyword zu
;uebergeben. ausserdem kann man testen ob das keyword mittels _extra übergeben wurde
;
;Author: Max Reuter, 2000
;
;---------------------------------------------------------------------------------------------
function advanced_keyword_set, keyword, _extra = _extra
	if keyword_set(_extra) then begin
		if size(keyword, /type) ne 7 then return, 0b
		if size(_extra, /type) ne 8 then return, 0b
		return, (strmatch(tag_names(_extra), keyword, /fold_case))[0]
	endif
	return, (size(keyword))[1] gt 0l
end
;---------------------------------------------------------------------------------------------
pro advanced_keyword_set_test_01, par1, key1 = key1, _extra = _extra
	print, advanced_keyword_set(par1),						advanced_keyword_set(par2)
	print, advanced_keyword_set(key1),						advanced_keyword_set(key2)
	print, advanced_keyword_set('ext1', _extra = _extra),	advanced_keyword_set('ext2', _extra = _extra)
end
;---------------------------------------------------------------------------------------------
pro advanced_keyword_set_test_02
	advanced_keyword_set_test_01, 5, key1 = 0, ext1 = 3
end
;---------------------------------------------------------------------------------------------
