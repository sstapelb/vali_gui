;---------------------------------------------------------------------------------------------------
;strreplace:
; searches a sub-string in a string ans replaces it by the replace_string
;
;Syntax:
;       dum=strreplace(<in_string>, <search string>, <replace string> [, count=count,
;							 fold_case=fold_case])
;Result:
;
;
;Arguments:
;		in_string		:	input string
;		search_string	:	sub-string to be replaced
;		replace_string	:	sub-string to be inserted
;
;Keywords:
;       count			:	total number of replaced search_strings
;       fold_case		:	if not set, the searching will be case sensitive
;
;Example:
;
;Modification History:
;14.01.2006		MR			added funcionality for handling was and womit arrays
;
;----------------------------Authors: Max Reuter, Susanne Pfeifer, 2006-----------------------------
function strreplace, in, was, womit,fold_case=fold_case, count = count, recursive = recursive, noregex = noregex
	if ~keyword_set(in) then return,''
	regex	= 1b
	if keyword_set(noregex) then regex = 0b
	if n_elements(was)	ne n_elements(womit) then stop
	if n_elements(in)	gt 1l	then return, [	strreplace(in[0],		was, womit, fold_case = fold_case, count = count, /recursive, noregex = noregex)	, $
												strreplace(in[1: *],	was, womit, fold_case = fold_case, count = count, /recursive, noregex = noregex)	]
	out 	= strjoin(strsplit(in, was[0], /extract, regex = regex, /preserve_null, count = dum_count, fold_case = fold_case), womit[0], /single)
	count	= ((dum_count - 1l) > 0) + (keyword_set(recursive) ? (keyword_set(count) ? count : 0l) : 0l)
	if n_elements(was)	gt 1l	then return, strreplace(out, reform(was[1: *]), reform(womit[1: *]), fold_case = fold_case, count = count, /recursive, noregex = noregex) $
								else return, out
end
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
;                                            EXAMPLES
;---------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------
pro strreplace_ex01
	a = 'hallo, dies ist ein text'
	print, a
	print, strreplace(a, 'dies', 'das')
	print, strreplace(a, ['dies', 'ein'], ['das', 'kein'], count = count)
	print, 'count =', count
	print, strreplace('', 'dies', 'das', count = count)
	print, 'count =', count
end
;---------------------------------------------------------------------------------------------------
