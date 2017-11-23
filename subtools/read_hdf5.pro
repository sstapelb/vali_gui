;stapel 2012-10-15
;changed name to read_hdf5
;$Id: read_hdf.pro,v 1.00 2004/06/16
; 
;+
; NAME:
;   READ_HDF
; 
; PURPOSE:
;   Read one dataset and attributes from a hdf-file
;
; INPUT:
;   filename:     HDF-filename
;   dataset_name: Name of the HDF dataset
;
; KEYWORDS
;   attributes:   If set to a named variables, it will return the attributes
;                 associated with the dataset
;   scale:        If set, the data will be scaled with the appropriate gain
;                 and offsets, if avaialbe in the data. Existing nodata values 
;                 will also be scaled
;   list:         If set, and if dataset_name is actually the name of a valid
;                 dataset group, a list of all datasets and subgroups of this group
;                 is returned. If no datset_name is set, all datasets in the parent
;                 group "/" are returned.
;                 If LIST is set to 1 (equivalent to /LIST) the complete names
;                 are returned, e.g.
;                 "/Group1/Subgroup2/Dataset1"
;                 "/Group1/Subgroup2/Dataset2"
;                 "/Group1/Subgroup2/Dataset3".
;                 If LIST is set to a value greater then 1, only the lowermost level
;                 of the all names is returned, e.g.
;                 "Dataset1"
;                 "Dataset2"
;                 "Dataset3".
;    verbose:     Let read_hdf be a bit more talkative about what it does and
;                 tried to do.
;                 Setting verbose to 1 will also include the latest
;                 error message (if any), setting verbose to 2 will
;                 include all error messages (if any)
;
;
; PROCEDURES:
;       needs h5o_is_group.pro
;             h5a_read_error.pro
;
; MODIFICATION HISTORY:
;       written by:  Peter Albert, 16.06.2004
;       Added option for choosing dataset from "/" group members (PA 20050804)
;       Added keyword LIST (PA 20050816)
;       Forgot to close file when LIST was set. Fixed. (PA 20050822)
;       Choosing a group instead of a dataset now leads to recursive calls
;          to read_hdf until a valid dataset is chosen. If the keyword
;          LIST is set, the list of all subgroups and datasets is returned.
;          This worked previously just with "/" as base group. (PA 20051004)
;       Included printing of error messages when VERBOSE is set (PA 20051005)
;
; -
; ====================================================
function List_Group_Names, file_id, num_of_variables = num_of_variables, list_of_names = list_of_names

	; make a list of names including all subgroups
	num_vars = keyword_set(list_of_names) ? n_elements(list_of_names) : num_of_variables

	check_again = 0
	FOR j=0,num_vars-1 DO BEGIN
		name    = keyword_set(list_of_names) ? list_of_names[j] : h5g_get_obj_name_by_idx(file_id, j)
		objInfo = h5g_get_objinfo(file_id, name)
		if objInfo.type eq 'GROUP' then begin
			n = h5g_get_nmembers(file_id, name)
			if n gt 0 then begin
				base_name = name
				for i = 1, n do begin
					name_dum = base_name+'/'+h5g_get_member_name(file_id, base_name, i-1)
					objInfo = h5g_get_objinfo(file_id, name_dum)
					if objInfo.type eq 'GROUP' then begin
						if h5g_get_nmembers(file_id, name_dum) gt 0 then begin
							name_dum = List_Group_Names(file_id, list_of_names = name_dum)
						endif
					endif 
					name = [name,name_dum]
				endfor
			endif else name = name
		endif
		name_list =  n_elements(name_list) eq 0 ? name : [name_list,name]
	endfor

	return, name_list
	
end ;---------------------------------------------------------------------------------------------


function read_hdf5, filename, dataset_name, $
                   list = list, $
                   attributes = attributes, $
                   scale = scale, $
                   verbose = verbose

; Catch errors from e.g. non-existing datasets

  catch, error
  if error ne 0 then begin
      if size(file_id, /type) ne 0 then begin
          h5f_close, file_id
          if keyword_set(verbose) then begin
              catch, /cancel
              help, /Last_Message, Output = qwertz_theErrorMessage
              if verbose eq 1 then $
                  for j = 0, 1 do $
                  print, "[read_hdf5: Error]" + qwertz_theErrorMessage[j] $
              else $
                  for j=0,n_elements(qwertz_theErrorMessage) - 1 do $
                  print, "[read_hdf5: Error]" + qwertz_theErrorMessage[j]
          endif
      endif
      return, -1
  endif

; --------------------------------------------------------------

; If no parameter is given and no keyword is set,
; display the list of datasets in the "root directory"
; of the HDF file. The dataset is thus "/".

  if n_params() eq 1 then dataset_name = "/"

  file_id = h5f_open(filename)

; Check whether the dataset_name given is already a dataset
; or rather a group of datasets.

  n_data = h5o_is_group(file_id, dataset_name) ? h5g_get_nmembers(file_id, dataset_name) : 0

; --------------------------------------------------------------

; If dataset_name is a group of datasets, show either a list of 
; datasets (or subgroups) contained within or return
; the list.
  if n_data gt 0 then begin

; Concatenate base group name and its contents.
; Make sure that there are not too many "/"s 
      prefix = ( strmid(dataset_name, 0, 1) eq "/" ? dataset_name : "/" + dataset_name ) + $
               ( strmid(dataset_name, strlen(dataset_name)-1, 1) eq "/" ? "" : "/" )
;       sub_dataset_name = strarr(n_data)
;       for i = 0, n_data - 1 do sub_dataset_name[i] = prefix + h5g_get_member_name(file_id, dataset_name, i)
; stapel (07/2104) make recursive name list of group incl subsub groups , etc--------
	dataset_id = h5g_open(file_id,dataset_name)
	sub_dataset_names = List_Group_Names( dataset_id, num_of_variables = n_data)
	sub_dataset_name = prefix + sub_dataset_names
	n_data = n_elements(sub_dataset_name)
	h5g_close,dataset_id
;------------------------------------------------------------------------------------
      h5f_close, file_id

; If keyword LIST is set, just return the list of subgroups and datasets

      if keyword_set(list) then return, list eq 1 $
          ? sub_dataset_name $
          : strmid(sub_dataset_name, strpos(sub_dataset_name[0], "/", /reverse_search) + 1) $
      else begin

; If keyword LIST is not set, show the list of subgroups and datasets,
; ask for selection and call read_hdf again with the chosen
; subgroup or dataset

          for i = 0, n_data - 1 do $
              print, i, sub_dataset_name[i], format = '(i3, ": ", a)'

          s = ""
          read, s, prompt = "Read dataset number: "

; Recursively call read_hdf with new selected dataset name

          out = read_hdf5( $
                            filename, $
                            sub_dataset_name[fix(s)], $
                            list = list, $
                            attributes = attributes, $
                            scale = scale, $
                            verbose = verbose $
                        )
          return, out
      endelse
  endif

; --------------------------------------------------------------


; Now that we have a valid datast name, we can go ahead
; and read it from the file

  if keyword_set(verbose) then print, "[INFO read_hdf5:] Reading " + dataset_name

	if h5o_is_group(file_id, dataset_name) then begin
		dataset_id = h5g_open(file_id, dataset_name)
		dataset = -1
	endif else begin
		;---stapel-(11/2014)--case_sensitive----
		num_vars = H5G_GET_NUM_OBJS(file_id)
		var_list = List_Group_Names( file_id, num_of_variables = num_vars)
		for nv = 0, n_elements(var_list) -1 do begin
; 			dum = h5g_get_obj_name_by_idx(file_id,nv)
			dum = var_list[nv]
			; compare by removing all '/' if any
			if strmatch(strjoin(strsplit(dum,'/',/ext)),strjoin(strsplit(dataset_name,'/',/ext)),/fold) then begin
				dataset_name = dum
				continue
			endif
		endfor
		;-----
		dataset_id = h5d_open(file_id, dataset_name)
		dataset = h5d_read(dataset_id)
	endelse

; Read the aattributes
  if (n_elements(attributes) ne 0) then tempvar = size(temporary(attributes))
  n_att = h5a_get_num_attrs(dataset_id)
  if n_att gt 0 then begin
      for j = 0, n_att - 1 do begin
          att_id = h5a_open_idx(dataset_id, j)
          name = h5a_get_name(att_id)
          ;stapel h5a_read_error is a workaround because h5a_read does have problems with 0 Byte characters
          att_data = h5a_read_error(att_id)
          if strpos(strlowcase(name), "no_data") ne -1 then att_data = float(att_data)
          attributes = j ne 0 ? $
              create_struct(attributes, name, att_data) : $
              create_struct(name, att_data)
          h5a_close, att_id
      endfor
  endif
  h5d_close, dataset_id

; --- < Identify nodata value > ---
; ---   Sometimes it's NODATA, sometimes NO_DATA ---

  if size(attributes, /type) eq 8 then begin
      tags = strlowcase(tag_names(attributes))
      index = where(strpos(tags, "nodata") ne -1)
      if index[0] eq -1 then index = where(strpos(tags, "no_data") ne -1)
      attributes = create_struct(attributes, "nodata_id", index[0])
  endif

; --- < /Identify nodata value > ---

; --- < Scale data > ---

  if keyword_set(scale) then begin
      gain = 1.0
      offset = 0.0
      if size(attributes, /type) eq 8 then begin
          tnames = strlowcase(tag_names(attributes))
          i = where(tnames eq "gain", c)
          if c eq 1 then gain = attributes.(i[0])
          i = where(tnames eq "scaling_factor", c)
          if c eq 1 then gain = attributes.(i[0])
          i = where(tnames eq "intercept", c)
          if c eq 1 then offset = attributes.(i[0])
          i = where(tnames eq "offset", c)
          if c eq 1 then offset = attributes.(i[0])
      endif
      if gain ne 1 or offset ne 0 then begin
          if keyword_set(verbose) then print, "[INFO read_hdf5:] Scaling " + dataset_name
          dataset = float(dataset) * gain + offset
          if attributes.nodata_id ne -1 then begin
              if keyword_set(verbose) then print, "[INFO read_hdf5:] Scaling " + dataset_name + "." + tags[attributes.nodata_id]
              attributes.(attributes.nodata_id) = attributes.(attributes.nodata_id) * gain + offset
          endif
      endif
  endif

; --- < /Scale data > ---

  h5f_close, file_id
  return, dataset
end
