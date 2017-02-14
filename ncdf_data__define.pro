@plot_l3.pro
;+
; NAME:
;       NCDF_DATA__DEFINE
;
; PURPOSE:
;
;       This program is designed to make it easier to browse and read the 
;       data and metadata in netCDF and HDF files. The user can browse files, 
;       and read the data and metadata into main-level IDL variables. New netCDF 
;       and HDF files can be opened at any time. The user interacts with the 
;       program via a browser window (GUI) or directly through the methods of
;       the object. The program implements an IDL object.
;       
;       Note that only HDF files with scientific datasets (SD) can be read currently.
;       There is no support for VDATA objects or other objects sometimes found in HDF
;       files. Also note that when variables are returned from HDF files, they are returned
;       in a calibrated form, if calibration information about the variable is present in the
;       file. Calibration information is presented as an extra variable attribute in the
;       browser.
;       
;          calibratedData = calData.cal * (uncalibratedData - calData.offset)
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;       File I/O
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject = Obj_New('NCDF_DATA', filename)
;
; ARGUMENTS:
;
;       filename: The name of a netCDF or HDF file to open and browse.
;
; KEYWORD PARAMETERS:
;       
;       BROWSE:   If this keyword is set, the Browse Window is invoked as soon
;                 as the object is initiated.
;
;       DESTROY_FROM_BROWSER:  As with all objects, this object is persistent until
;                  it is destroyed. However, with this keyword set, the object will
;                  be destroyed when the user closes the Browse Window.
;
;       EXTENSION: In general, netCDF and HDF files use *.nc, *.ncf, *.ncdf of *.hdf file extensions to
;                  identify themselves as netCDF or HDF files. Some users have their own file extensions.
;                  You can use this keyword to identify the file extension you wish to use. If
;                  set here, it will be used as the file filter in place of the normal file 
;                  extensions in DIALOG_PICKFILE.
;
;                      obj = ('NCDF_DATA', file, EXTENSION='*.bin')
;                
;       NO_READ_ON_PARSE: Normally, when a file is opened it is parsed for information.
;                  One piece of information is the minimum and maximum values of the variables.
;                  This requires actually reading the variables. This can slow things down 
;                  considerably is the variable is large. Setting this keyword will suppress 
;                  the reading of the variables during the parsing of the data file, with the
;                  result that no minimum or maximum values will be reported.
;
; NOTES:
;       
;       This program is designed to be flexible in how it is used, so it
;       can be used in both interactive and non-interactive (called directly)
;       ways. A less flexible way of interacting with the program is via the
;       NCDF_BROWSER program, which is a front-end to this object.
;       
;       The netCDF and HDF file formats are thought to be "standards". And to 
;       a large extent, they are. But files are not always created to standards,
;       and both netCDF and HDF files can be quirky. If you look carefully at the 
;       code you will see places where I work around quirks in the files I typically
;       use on a daily basis. If you find you can't read a particular file, let me know
;       about it. I may be able to improve the program in such as way that it can be read.
;       
;       This program is not meant to be the be-all and end-all of programs. Rather, it is
;       a tool I use, and improve upon whenever necessary, in my own work with netCDF and HDF
;       files. It will get better for all of us if you report problems to me directly.
;
; REQUIRES:
;
;     The following programs are required from the Coyote Library. And it is always a
;     good idea to make sure you have the latest version of the Coyote Library code,
;     as updates are irregular and frequent.
;
;              http://www.idlcoyote.com/programs/netcdf_data__define.pro
;              http://www.idlcoyote.com/programs/error_message.pro
;              http://www.idlcoyote.com/programs/centertlb.pro
;              http://www.idlcoyote.com/programs/undefine.pro
;              http://www.idlcoyote.com/programs/textbox.pro
;              http://www.idlcoyote.com/programs/fsc_base_filename.pro
;              http://www.idlcoyote.com/programs/textlineformat.pro
;
; METHODS:
;
;     The following methods can be used directly.
;
;     ncdfObject -> Browse                             ; Use GUI to browse file data and metadata.
;     ncdfObject -> OpenFile, filename                 ; Opens a new netCDF or HDF file.
;     globalAttr = ncdfObject -> ReadGlobalAttr()      ; Return a structure containing global attributes.
;     attribute = ncdfObject -> ReadAttribute(attrname); Return an attribute, identified by name.
;     dim = ncdfObject -> ReadDimension(dimName)        ; Return a dimension, identified by name.
;     variable = ncdfObject -> ReadVariable(varname)   ; Return a variable, identified by name.
;     varstruct = ncdfObject -> ReadVariableWithAttr(varname)   ; Return a variable, identified by 
;                                                               ; name, along with its attributes.
;     allData = ncdfObject -> ReadFile(filename)        ; Read all the data in the file, into structures.
;
; EXAMPLE:
;
;       IDL> filename = 'example.nc'
;       IDL> ncdfObj = Obj_New('NCDF_DATA', filename)
;       IDL> ncdfObj -> Browse
;       IDL> Obj_Destroy, ncdfObj
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 03 Feb 2008. Used ideas from many
;           people, including Chris Torrence, Ken Bowman, Liam Gumely, 
;           Andrew Slater, and Paul van Delst.
;       Added EXTENSION keyword, resizeable TLB, and ability to download
;           individual global attibutes. DWF. 04 Feb 2008.
;       Added ReadDimension and ReadVariableWithAttr methods. DWF. 05 Feb 2008.
;       Ill-formed attribute names giving me fits. Now doing checks with IDL_VALIDNAME
;            before creating structures. 06 February 2008. DWF.
;       Same problem. Wide use of IDL_VALIDNAME everywhere it seems wise. 06 Feb 2008. DWF.
;       Added functionality to read a variable with its attributes from the browser interface,
;            and fixed a problem with reading CHAR values. 2 March 2008. DWF.
;       Fixed a problem with changing variable name when reading variable plus attributes. 6 March 2008. DWF.
;       Fixed a problem with not setting GLOBAL keyword when inquiring about global attribute. 6 March 2008. DWF.
;       Made sure file was parsed before attempting to read variables and attributes to avoid errors. 7 March 2008. DWF.
;       Small bug with variable attributes fixed. 18 Dec 2008. DWF.
;       Added ability to read HDF files containing Scientific Datasets (SD). 21 February 2009. DWF.
;       Added error handling and protection for NCDF variables that have a dimension of length zero. 22 April 2009. DWF.
;       Added NO_READ_ON_PARSE keyword. 22 April 2009. DWF.
;       Now convert NCDF CHAR type variables to strings on output. 22 April 2009. DWF
;       Fixed a problem with the directory being correct when file name passed in. 11 May 2009. DWF.
;       Added COUNT, OFFSET, and STRIDE keywords to ReadVariable method. 25 June 2009. DWF.
;       When reading a netCDF variable by itself (without it's attributes), the program now looks for
;          a SCALE_FACTOR and ADD_OFFSET attribute, and if found will apply this to the variable before
;          it is returned to the user. 24 August 2009. DWF.
;       Added the methods GetAttrNames, GetVarNames, GetVarAttrNames, and ReadVarAttr to retrieve specfic
;          information from the data files. 16 November 2009. DWF.
;       Modified the ReadVariableWithAttr method to include the number of dimensions (in the NDIMS field,
;          and the dimensions (in the DIMS field) in the return structure. For HDF files, the DIMS field
;          is a vector of the dimensions of the variable. For netCDF files, the DIMS field is a vector
;          of dimension IDs for the dimensions of the variable. 27 Nov 2009. DWF.
;       Andy Meigs alerted me to a problem creating a structure when the ncdf variable name
;          is ill-formed according to IDL structure tag name rules. Fixed in the ReadFile method.
;          30 November 2009. DWF.
;       Added NO_NEW_FILE keyword to the BROWSE method. This keyword will suppress the OPEN FILE
;          button on the browse interface. 3 Feb 2010. DWF.
;       Made the default browser size a bit larger to accomodate longer variable names. 3 Feb 2010. DWF.
;       Add a check for HDF/netCDF file type in the INIT method to better accommodate reading data
;          from the file without first parsing the file. 16 March 2010. DWF.
;       Changed the ReadVariable for netCDF files to now check for missing data, using either the
;           depreciated missing_value attribute or the compliant _FillValue attribute. Missing data
;           is now identified via new output keywords MISSINGINDICES and FILLVALUE, and missing data
;           is not scaled or offset, if these operations are applied to the data prior to return. 
;           21 March 2010. DWF. Problem with these changes, fixed 23 March 2010. DWF.
;       Fixed a problem with memory leakage when the input file cannot be read. 1 May 2010. DWF.
;       Fixed a problem with memory leakage from created structures. 1 May 2010. DWF.
;       Have done some work on parsing HDF-EOS swath files, but currently unused in code. 15 May 2010. DWF.
;       Modified the ReadVariable method to check for 0 length dimensions when reading variables
;           from HDF files. 21 July 2010. DWF.
;       Modified the global attribute structure so that the "filename" field, which holds the
;           name of the netCDF of HDF file is now named "ncdf_filename" or "hdf_filename". This
;           will avoid conflicts with global attributes with "filename". 20 January 2011. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;

PRO NCDF_DATA::Browse, $
    NO_NEW_FILE=no_new_file, $
    SUCCESS=success, $
    TITLE=title, $
    XOFFSET=xoffset, $
    YOFFSET=yoffset, silent = silent

    ;
; NAME:
;       NCDF_DATA::Browse
;
; PURPOSE:
;
;       This method is invoked to create a Browser Window the user can
;       interact with to explored the data and metadata in a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> Browse
;
; ARGUMENTS:
;
;       None.
;
; KEYWORD PARAMETERS:
; 
;       NONEWFILE: If this keyword is set, the browser does not allow selecting
;                  a new netCDF file from the interface.
;                  
;       TITLE:     The text on the title bar. By default, 'File Browser'.
;       
;       SUCCESS:   An output keyword set to 1 if this method exits successfully.
;       
;       XOFFSET:   Normally, the Browser Window is centered, however is this
;                  keyword and the YOFFSET keywords are used, the Browser Window
;                  can be located with the upper-left corner at these locations in 
;                  device coordinates. The X offset of the Browser Window.
;
;       YOFFSET:    The Y offset of the Browser Window.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      Widget_Control, self.tlb, /DESTROY
      success = 0
      RETURN
   ENDIF
   
   ; Assume the best.
   success = 1
   
   ; Check input parameters.
   IF N_Elements(xoffset) EQ 0 THEN xoffset = -1
   IF N_Elements(yoffset) EQ 0 THEN yoffset = -1
   newFileOK = ~Keyword_Set(no_new_file)
   IF N_Elements(title) EQ 0 THEN title = 'File Browser'

   ; Only one browser with this TLB on the display at a time.
   IF Widget_Info(self.tlb, /VALID_ID) THEN BEGIN
      Widget_Control, self.tlb, /SHOW
      success = 0
      RETURN
   ENDIF

   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   IF self.hasBeenParsed EQ 0 THEN BEGIN
        success = 0
        RETURN
   ENDIF

   x=systime(1)   

   ; Get some bitmaps for the widget_tree.
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'volume.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      variableBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF

   bmfile = Filepath(SubDir=['resource','bitmaps'], 'axis.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      dimensionBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'ascii.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 80, count)
      IF count GT 0 THEN bm[i] = 95B
      attributeBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'sum.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      summaryBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF

   ; Set up the initial tree widget.
   self.tlb = Widget_Base(TITLE=title, COLUMN=1, UVALUE=self, /BASE_ALIGN_CENTER, $
      TLB_SIZE_EVENT=1)
   rowbase = Widget_Base(self.tlb, ROW=1, XPAD=0, YPAD=0)
   theTree = Widget_Tree(rowbase, SCR_XSIZE=350, SCR_YSIZE=400, UNAME='theTree')
   self.textDisplay = Widget_Text(rowbase, SCR_XSIZE=450, SCR_YSIZE=400, /SCROLL)

   ; Set up fundamental branch.
   aBranch = Widget_Tree(theTree, Value='File Overview', /FOLDER, /EXPANDED, UNAME='FOLDER')
   aNode = Widget_Tree(aBranch, Value='Directory', UValue=self.directory, UNAME='DIRECTORY', BITMAP=summaryBM)
   aNode = Widget_Tree(aBranch, Value='File Name', UValue=self.filename, UNAME='FILENAME', BITMAP=summaryBM)
   summaryNode = Widget_Tree(aBranch, Value='Summary', UNAME='SUMMARY', BITMAP=summaryBM)
   Widget_Control, summaryNode, Set_Tree_Select=1

   ; Set up global attribute branch.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Global Attributes', /FOLDER, UNAME='FOLDER')
      theAttributes = *self.theAttributes
      FOR j=0,N_Elements(theAttributes)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theAttributes[j].name, UValue=theAttributes[j].name, $
            UNAME='GLOBAL ATTRIBUTE', BITMAP=attributeBM)
      ENDFOR
   ENDIF

   ; Set up dimension branch.
   IF Ptr_Valid(self.theDimensions) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Dimensions', /FOLDER, UNAME='FOLDER')
      theDimensions = *self.theDimensions
      FOR j=0,N_Elements(theDimensions)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theDimensions[j].name, UValue=theDimensions[j].name, $
            UNAME='DIMENSION', BITMAP=dimensionBM)
      ENDFOR
   ENDIF

   ; Set up variable branch.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Variables', /FOLDER, UNAME='FOLDER')

      theVariables = *self.theVariables
      FOR j=0,N_Elements(theVariables)-1 DO BEGIN
	; stapel included for groups and subsub... groups-------------------------
	gbranch = 0
	IF Ptr_Valid(theVariables[j].GROUP_VARIABLES) THEN BEGIN
		gbranch++
		self -> recursive_group_vars, aBranch, theVariables[j], varBranch, variableBM, attributeBM
	endif
	;-------------------------------------------------------------------------
         IF Ptr_Valid(theVariables[j].var_attributes) THEN BEGIN
            if ~gbranch then varBranch = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
                                        UNAME='VARIABLE', /FOLDER, BITMAP=(theVariables[j].DATATYPE ne 'GROUP' ? variableBM:0))
            theAttributes = *theVariables[j].var_attributes
            FOR k=0,N_Elements(theAttributes)-1 DO BEGIN
               IF theAttributes[k].name NE "" THEN BEGIN
                  aNode = Widget_Tree(varBranch, Value=theAttributes[k].name, $
                     UValue=[theVariables[j].name,theAttributes[k].name], $
                     BITMAP=attributeBM, UNAME='VARIABLE ATTRIBUTE')
               ENDIF
            ENDFOR
         ENDIF ELSE BEGIN
            aNode = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
               UNAME='VARIABLE', BITMAP=variableBM)
         ENDELSE
;endelse ; stapel test
      ENDFOR

   ENDIF

   ; Application Buttons
   buttonBase = Widget_Base(self.tlb, /ROW, BASE_ALIGN_CENTER=1)
;--stapel
   button = Widget_Button(buttonBase, Value='Plot Variable', UVALUE='PLOT_VAR_FROM_GUI')
;---
   button = Widget_Button(buttonBase, Value='Read Variable', UVALUE='READ_VAR_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Variable with Attributes', UVALUE='READ_VARPLUS_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Global Attribute', UVALUE='READ_ATTRIBUTE_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Entire File', UVALUE='READ_FILE_FROM_GUI')
   IF newFileOK THEN button = Widget_Button(buttonBase, Value='Open New File', UVALUE='OPEN_NEW_FILE')
   button = Widget_Button(buttonBase, Value='Exit', UVALUE='QUIT_BROWSER')
   button = Widget_Button(buttonBase, Value='?', UVALUE='HELP',xoffset=xsize)

;  check_it = self -> get_file_infos(/verbose) & return

   ; Get the geometries of the tree widget and the button base. These
   ; will set the minimun and maximum values for resizing.
   self.geoWindow = Widget_Info(self.tlb, /GEOMETRY)
   self.geoTree = Widget_Info(theTree, /GEOMETRY)
   self.geoButton = Widget_Info(buttonBase, /GEOMETRY)
   self.geoDisplay = Widget_Info(self.textDisplay, /GEOMETRY)
   self.minxsize = self.geoDisplay.scr_xsize
   self.minysize = self.geoDisplay.scr_ysize

   ; Position the application and realize it.
   IF (xoffset LT 0 AND yoffset LT 0) THEN CenterTLB, self.tlb ELSE CenterTLB, self.tlb, xoffset, yoffset, /DEVICE, /NOCENTER
   Widget_Control, self.tlb, /REALIZE
   self.theTree = theTree
   XManager, 'ncdf_data', self.tlb, /NO_BLOCK, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', $
      CLEANUP='NCDF_DATA_WIDGET_CLEANUP'

   ; Send an event to start up in the summary browse look.
   event ={WIDGET_TREE_SEL}
   event.top = self.tlb
   event.id = summaryNode
   event.handler = self.tlb
   event.clicks = 1
   event.type = 0
   Widget_Control, summaryNode, Send_Event=event

   print,'Browse    : ',systime(1)-x

END ;---------------------------------------------------------------------------------------------
; stapel controls groups recursivly , incl. subsubsubsubsbu ... groups
pro NCDF_DATA::recursive_group_vars, aBranch, theVariables, varBranch, only_once = only_once, thevarname = thevarname, variableBM, attributeBM

	if ~keyword_set(only_once) then varBranch = Widget_Tree(aBranch, Value=theVariables.name, UValue=theVariables.name, $
		UNAME='VARIABLE', /FOLDER);, BITMAP=variableBM)
	thevarname   = keyword_set(thevarname) ? thevarname : theVariables.name 
	thesubgroups = *theVariables.GROUP_VARIABLES
	FOR k=0,N_Elements(thesubgroups)-1 DO BEGIN
		IF thesubgroups[k].name NE "" THEN BEGIN
			IF ptr_valid(thesubgroups[k].var_attributes) THEN BEGIN
				var2Branch = Widget_Tree(varBranch, Value=thesubgroups[k].name, UValue=[thevarname,thesubgroups[k].name], $
				UNAME='SUBGROUP VARIABLE', /FOLDER);, BITMAP=variableBM)
				theAttributes = *thesubgroups[k].var_attributes
				FOR m=0,N_Elements(theAttributes)-1 DO BEGIN
					IF theAttributes[m].name NE "" THEN BEGIN
						aNode = Widget_Tree(var2Branch, Value=theAttributes[m].name, $
						UValue=[thevarname,thesubgroups[k].name,theAttributes[m].name], $
						BITMAP=attributeBM, UNAME='SUBGROUP ATTRIBUTE')
					ENDIF
				ENDFOR
			ENDIF ELSE BEGIN
				IF ~Ptr_Valid(thesubgroups[k].GROUP_VARIABLES) THEN BEGIN
					aNode = Widget_Tree(varBranch, Value=thesubgroups[k].name, UValue=[thevarname,thesubgroups[k].name], $
					UNAME='SUBGROUP VARIABLE', BITMAP=variableBM)
				endif
			ENDELSE
			IF Ptr_Valid(thesubgroups[k].GROUP_VARIABLES) THEN BEGIN
				var2Branch = Widget_Tree(varBranch, Value=thesubgroups[k].name, UValue=[thevarname,thesubgroups[k].name], $
				UNAME='SUBGROUP VARIABLE', /FOLDER);, BITMAP=variableBM)
				self -> recursive_group_vars, aBranch, thesubgroups[k], var2Branch,/only_once, $
				thevarname = [thevarname,thesubgroups[k].name], variableBM, attributeBM
			endif
		ENDIF
	ENDFOR
end ;---------------------------------------------------------------------------------------------

PRO NCDF_DATA::CleanParsedStructures
   ; An internal method used to clean up pointers in names structures
   ; to prevent memory leakage from the object.
   
   ; Clean up all pointers in the global attribute structures.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      num = N_Elements(*self.theAttributes)
      FOR j=0,num-1 DO Ptr_Free, (*self.theAttributes)[j].value
   ENDIF

   ; Clean up all pointers in the variable structures.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      num = N_Elements(*self.theVariables)
      FOR j=0,num-1 DO BEGIN
         Ptr_Free, (*self.theVariables)[j].value
         Ptr_Free, (*self.theVariables)[j].datasize
         Ptr_Free, (*self.theVariables)[j].calibration
         IF Ptr_Valid((*self.theVariables)[j].var_attributes) THEN BEGIN
            attrs = *(*self.theVariables)[j].var_attributes
            attnum = N_Elements( attrs )
            FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
            Ptr_Free, (*self.theVariables)[j].var_attributes
         ENDIF
      ENDFOR
   ENDIF

   ; Clean up all pointers in the swath structures.
   IF Ptr_Valid(self.theSwaths) THEN BEGIN
      num = N_Elements(*self.theSwaths)
      FOR j=0,num-1 DO BEGIN
         Ptr_Free, (*self.theSwaths)[j].maps
         Ptr_Free, (*self.theSwaths)[j].idxmaps
         Ptr_Free, (*self.theSwaths)[j].dimensions
         IF Ptr_Valid((*self.theSwaths)[j].attributes) THEN BEGIN
            attrs = *(*self.theSwaths)[j].attributes
            attnum = N_Elements( attrs )
            FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
            Ptr_Free, (*self.theSwaths)[j].attributes
         ENDIF
         IF Ptr_Valid((*self.theSwaths)[j].geofields) THEN BEGIN
              num = N_Elements(*(*self.theSwaths)[j].geofields)
              FOR j=0,num-1 DO BEGIN
                 thisGeoField = (*(*(*self.theSwaths)[j].geofields))[j]
                 Ptr_Free, thisGeoField.value
                 Ptr_Free, thisGeoField.datasize
                 Ptr_Free, thisGeoField.calibration
                 IF Ptr_Valid(*thisGeoField.var_attributes) THEN BEGIN
                    attrs = *(*thisGeoField.var_attributes)
                    attnum = N_Elements( attrs )
                    FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
                    Ptr_Free, (*thisGeoField)[j].var_attributes
                 ENDIF
                 Ptr_Free,(*self.theSwaths)[j].geofields
              ENDFOR
         ENDIF
         IF Ptr_Valid((*self.theSwaths)[j].datafields) THEN BEGIN
              num = N_Elements(*(*self.theSwaths)[j].datafields)
              FOR j=0,num-1 DO BEGIN
                 thisGeoField = (*(*(*self.theSwaths)[j].datafields))[j]
                 Ptr_Free, thisGeoField.value
                 Ptr_Free, thisGeoField.datasize
                 Ptr_Free, thisGeoField.calibration
                 IF Ptr_Valid(*thisGeoField.var_attributes) THEN BEGIN
                    attrs = *(*thisGeoField.var_attributes)
                    attnum = N_Elements( attrs )
                    FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
                    Ptr_Free, (*thisGeoField)[j].var_attributes
                 ENDIF
                 Ptr_Free,(*self.theSwaths)[j].datafields
              ENDFOR
         ENDIF
      ENDFOR
   ENDIF

END ;---------------------------------------------------------------------------------------------

PRO NCDF_DATA::EventHandler, event
   ; An internal method used to process events and sent them to appropriate event handler methods.
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Event branching is based initially on event structure names.
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)

   CASE thisEvent OF
	'WIDGET_BASE': BEGIN
		; das hier ist wahrscheinlich die schlimmste aller Lösungen, aber immerhin eine Lösung!!
		; Irgendwie stimmt noch die Zuordnung der eventhandler nicht. stapel 09/2013
		error_status = 0
		catch, error_status
		if (error_status ne 0) then begin
			Widget_Control, self.textDisplay, $
			SCR_XSIZE = (event.x -(self.geoTree.scr_xsize + 8)) > self.minXSize , $
			SCR_YSIZE = (event.y -(self.geoButton.scr_ysize + 8)) > self.minYSize
			catch, /cancel
			goto , wbase_ende
		endif
		Widget_Control, self.draw, $
		SCR_XSIZE = (event.x -(self.georow.scr_xsize + 1)) > self.minXS , $
		SCR_YSIZE = (event.y -(self.geoToprow.scr_ysize + self.geoButtrow.scr_ysize + 22)) > self.minYS
		wbase_ende:
	END

	'WIDGET_TREE_SEL': self -> SelectionInTree, event
         
	'WIDGET_TREE_EXPAND': 
     
	'WIDGET_BUTTON': BEGIN
		Widget_Control, event.id, GET_UVALUE=buttonUValue
		CASE buttonUValue OF
			'READ_ATTRIBUTE_FROM_GUI': self -> ReadAttributeFromGUI, event
			'READ_ATTRIBUTE_AND_LEAVE': self -> ReadAttributeFromGUI_Events, event
			'READ_ATTRIBUTE_AND_STAY': self -> ReadAttributeFromGUI_Events, event
			'QUIT_READ_ATTRIBUTE_GUI': self -> ReadAttributeFromGUI_Events, event

			'READ_VAR_FROM_GUI'	: self -> ReadVariableFromGUI, event
			'READ_AND_LEAVE'	: self -> ReadVariableFromGUI_Events, event
			'READ_AND_STAY'		: self -> ReadVariableFromGUI_Events, event
			'QUIT_READ_VARIABLE_GUI': self -> ReadVariableFromGUI_Events, event

			; stapel plot eingefuehrt
			'PLOT_VAR_FROM_GUI'	: self -> PlotVariableFromGUI, event
			'PLOT_AND_STAY'		: self -> PlotVariableFromGUI_Events, event
			'SET_PLOT_DEFAULTS'	: self -> PlotVariableFromGUI_Events, event
			'JUST_COMPARE_CCI_WITH'	: self -> PlotVariableFromGUI_Events, event
			'PLOT_DIFF'		: self -> PlotVariableFromGUI_Events, event
			'SNAPSHOT'		: self -> PlotVariableFromGUI_Events, event
			'QUIT_PLOT_VARIABLE_GUI': self -> PlotVariableFromGUI_Events, event
			'HELP'			: self -> PlotVariableFromGUI_Events, event
			;------------------------

			'READ_VARPLUS_FROM_GUI'	: self -> ReadVarPlusFromGUI, event
			'READ_VARPLUS_AND_STAY'	: self -> ReadVarPlusFromGUI_Events, event
			'READ_VARPLUS_AND_LEAVE': self -> ReadVarPlusFromGUI_Events, event
			'QUIT_READ_VARPLUS_GUI'	: self -> ReadVarPlusFromGUI_Events, event
			
			'READ_FILE_FROM_GUI'	: self -> ReadFileFromGUI, event
			'OPEN_NEW_FILE'		: self -> OpenNewFile, event
			'QUIT_BROWSER'		: Widget_Control, event.top, /DESTROY
			'APPEND_FILENAME'	: 
			
			ELSE: Print, 'No case for ' + buttonUValue
		ENDCASE
        END
         
	'WIDGET_DROPLIST': BEGIN
		theName = Widget_Info(event.id, /UNAME)
		CASE theName OF
			'VARIABLES': self -> ReadVariableFromGUI_Events, event
			'VARIABLESPLUS': self -> ReadVarPlusFromGUI_Events, event
			'ATTRIBUTES': self -> ReadAttributeFromGUI_Events, event
		ENDCASE
         END

	'WIDGET_COMBOBOX': BEGIN
		theName = Widget_Info(event.id, /UNAME)
		CASE strmid(theName,0,5) OF
			'PLOTS': self -> PlotVariableFromGUI_Events, event
		ENDCASE
	END

	'WIDGET_TEXT_CH':

	'WIDGET_SLIDER':
	
	ELSE: BEGIN
		ok = Dialog_Message('Unrecognized event in EventHandler method. See Console for details.')
		HELP, event, /Structure
	END

   ENDCASE 
END ;---------------------------------------------------------------------------------------------

FUNCTION NCDF_DATA::FindDimensions, fileID, varID
    ; Get information about the variable.
    r = NCDF_VarInq(fileID, varID)
    dims = LonArr(N_Elements(r.dim))
    FOR j=0,N_Elements(dims)-1 DO BEGIN
        dimID = r.dim[j]
        NCDF_DIMINQ, fileID, dimID, dimension_name, dimension_size
        dims[j] = dimension_size
    ENDFOR
    
    ; If this is a character variable, then the first dimension is irrelevant
    ; if there is more than one variable.
    IF r.datatype EQ 'CHAR' THEN IF N_Elements(dims) GT 1 THEN dims = dims[1:*] 

    RETURN, dims

END ;---------------------------------------------------------------------------------------------

FUNCTION NCDF_DATA::Destroy_From_Browser
	;stapel (12/2014) only true if IDL 8.2 Bug see init!
; 	if self.current_dir ne "" then cd, self.current_dir
RETURN, self.destroy_from_browser
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetAttrNames, silent = silent

   ; Returns a list of the global attribute names.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   numAttrs = N_Elements(*self.theAttributes)
   attributeList = StrArr(numAttrs)
   
   FOR j=0,numAttrs-1 DO BEGIN
        thisAttribute = (*self.theAttributes)[j]
        attributeList[j] = thisAttribute.name
   ENDFOR
   
   RETURN, attributeList
   
END ;---------------------------------------------------------------------------------------------


PRO NCDF_DATA::ListAttrNames, silent = silent

   ; Prints out the global attribute names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   numAttrs = N_Elements(*self.theAttributes)
   
   Print, 'There are ', StrTrim(numAttrs,2), ' attributes in ' + self.filename
   Print, ""
   FOR j=0,numAttrs-1 DO BEGIN
        thisAttribute = (*self.theAttributes)[j]
        Print, '   ', StrTrim(j+1,2), ') ', thisAttribute.name
   ENDFOR
   Print, ""
   
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetVarNames, silent = silent

   ; This function returns a list of variable names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   numVars = N_Elements(*self.theVariables)
   varList = StrArr(numVars)
   
   FOR j=0,numVars-1 DO BEGIN
        thisVariable = (*self.theVariables)[j]
        varList[j] = thisVariable.name
   ENDFOR
   
   RETURN, varList
   
END ;---------------------------------------------------------------------------------------------


PRO NCDF_DATA::ListVarNames, silent = silent

   ; Prints out the variable names found in the file.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   numVars = N_Elements(*self.theVariables)
   
   Print, 'There are ', StrTrim(numVars,2), ' variables in ' + self.filename
   Print, ""
   FOR j=0,numVars-1 DO BEGIN
        thisVariable = (*self.theVariables)[j]
        Print, '   ', StrTrim(j+1,2), ') ', thisVariable.name
   ENDFOR
   Print, ""
   
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::GetVarAttrNames, theVariable, silent = silent

   ; This function returns a list of attribute names which it finds for a particular variable.
    
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF
    
   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   ; Get the variable list.
   theVarStructures = *self.theVariables
   
   ; Find this variable in the variable structures.
   index = Where(theVarStructures.name EQ theVariable, count)
   IF count EQ 0 THEN Message, 'Cannot find the variable ' + theVariable + ' in the file.'
   thisVariableStruct = theVarStructures[index]
   
   ; Get the pointer to the variable attribute structures.
   varAttrStructures = *thisVariableStruct.var_attributes
   
   ; Extract the attribute names.
   numAttrs = N_Elements(varAttrStructures)
   varAttrList = StrArr(numAttrs)
   FOR j=0,numAttrs-1 DO BEGIN
        varAttrList[j] = varAttrStructures[j].name
   ENDFOR
   
   RETURN, varAttrList
   
END ;---------------------------------------------------------------------------------------------
PRO NCDF_DATA::OpenNewFile, event

   ; Creates a dialog for the user to specify the name of a new netCDF or HDF file to open.
   ; Loads the file into the object.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Get the name of a new file. Look in the directory of the last file.
   filename = Dialog_Pickfile(FILTER=self.extension , PATH=self.directory, $
      /READ, TITLE='Select a File to Open')
   IF filename EQ "" THEN RETURN
   
   ; Open the new file.
   self -> OpenFile, filename
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::OpenFile, filename, silent = silent

;
; NAME:
;       NCDF_DATA::OpenFile
;
; PURPOSE:
;
;       This method is used to open a new netCDF or HDF file and add it to the object.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> OpenFile, filename
;
; ARGUMENTS:
;
;       filename:  The name of a netCDF or HDF file to open.
;
; KEYWORD PARAMETERS:
;       
;       None.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   IF N_Elements(filename) NE 0 THEN BEGIN
   
      ; Clean up from the old file that will here before.
      self -> CleanParsedStructures
      
      ; Set the filename and directory name.
      directory = File_Dirname(filename)
      IF directory EQ "" THEN CD, CURRENT=directory
      self.directory = directory
      self.filename = File_Basename(filename)
      
     ; stapel (12/2014)
     ; this is a workaround for an IDL 8.2 bug causing segmentation faults ------
     ; for ncdf4 files with path+filename length ge 273 characters
     ; (Note ncdf4 will be recognized as hdf5! and ncdf)
     if strlen(self.directory+'/'+self.filename) ge 273 and H5F_IS_HDF5(self.directory+'/'+self.filename) then begin
	print,'String length (self.directory+"/"+self.filename) too long ('+strcompress(strlen(self.directory+'/'+self.filename),/rem)+$
		' ge 273) (IDL 8.2 ncdf4 bug!)! Start Workaround with cd self.directory and set self.directory to ./'
	cd , self.directory,current = current
	if self.current_dir eq "" then self.current_dir = current
	self.directory = './'
     endif
      
      ; Parse the new file.
      self.hasbeenParsed = 0
      self -> ParseFile, silent = silent
      
      ; If a browser is currently displayed, kill it and display
      ; a browser in the same location with new file information.
      IF Widget_Info(self.tlb, /Valid_ID) THEN BEGIN
         thisState = self.destroy_from_browser
         IF thisState THEN self.destroy_from_browser = 0
         Widget_Control, self.tlb, TLB_GET_OFFSET=offsets
         Widget_Control, self.tlb, /DESTROY
         self -> Browse, XOFFSET=offsets[0], YOFFSET=offsets[1]
         self.destroy_from_browser = thisState
      ENDIF
      
   ENDIF ELSE Message, 'Must pass name of file to open.'  
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ParseFile, silent = silent

; This internal method parses the new netCDF or HDF file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN NCDF_Close, fileID
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF

   x=systime(1)   

   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID
 
   ; Special processing if this is an HDF file. Otherwise, we believe it is a netCDF file.
   self.isHDF  = HDF_ISHDF(Filepath(ROOT_DIR=self.directory, self.filename))
   ; replaced by stapel in order to make hdf 5 work too, s. Parse_HDF_File; and ncdf4 seems to appear as hdf5 and ncdf
   self.isHDF5 = H5F_IS_HDF5(self.directory+'/'+self.filename)
   if self.isHDF5 then begin
	if is_ncdf(self.directory+'/'+self.filename) then self.isHDF5 = 0
   endif

   IF self.isHDF THEN BEGIN
	; Is this an HDF-EOS type file? If so, we want to parse it differently
	; ergibt bus error bei Modis L1 files , wird im moment auch nicht gebraucht.
;  	isEOS_File = EOS_Query(Filepath(ROOT_DIR=self.directory, self.filename), info)
	isEOS_File =0
	IF isEOS_File THEN BEGIN
		IF info.num_grids NE 0 OR info.num_points NE 0 THEN BEGIN
			Message, 'This program does not current parse HDF-EOS grids or point data.', /INFORMATIONAL
		ENDIF
		; This code is not quite ready. Bypassing this for the moment.
		self -> Parse_HDF_File
	ENDIF ELSE self -> Parse_HDF_File
;         print,'Parse File: ',systime(1)-x
	RETURN
   ENDIF

   ; ncdf4 werden auch als hdf5 erkannt dann gibts probleme mit dem einlesen von null byte value character attributen!!!!
   IF self.isHDF5 THEN BEGIN
	; stapel
	self -> Parse_HDF5_File
        if ~keyword_set(silent) then print,'Parse File: ',systime(1)-x
	return
   endif

   if ~is_ncdf(Filepath(ROOT_DIR=self.directory, self.filename)) then begin
	ext = (reverse(strsplit(self.filename,'.',/ext)))[0]
	ok = dialog_message('NCDF_DATA::PARSEFILE: Filetype ('+ext+') not supported! Please use hdf4,hdf5,ncdf3 and ncdf4 (IDL version > 8.0) files!')
	self.hasBeenParsed = 0
	return
   endif
   ; Open the file and find out how many dimensions, global attributes, and variables are there.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   info = NCDF_Inquire(fileID)

   ; Is there an unlimited dimension in this file?
   IF info.recdim NE -1 THEN BEGIN
        NCDF_DIMINQ, fileID, info.recdim, unlimitedName, unlimitedSize
        unlimitedID = info.recdim
   ENDIF ELSE unlimitedName = ""
   
   ; First, get the global attributes.
   num_attr = info.ngatts
   IF num_attr GT 0 THEN BEGIN
      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
      FOR j=0,num_attr-1 DO BEGIN
          attribute_name = NCDF_AttName(fileID, j, /GLOBAL)
          NCDF_AttGet, fileID, attribute_name, theAttribute, /GLOBAL
          attinfo = NCDF_ATTINQ(fileID, attribute_name, /GLOBAL)
          att_type = StrUpCase(attinfo.dataType)
           ; Strings are stored as byte values in attributes, so convert them back.
          IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
            THEN theAttribute = String(theAttribute)
          theAttributes[j].attrType = 'GLOBAL'
          theAttributes[j].dataType = att_type
          theAttributes[j].length = N_Elements(theAttribute)
          theAttributes[j].name = attribute_name
          theAttributes[j].value = Ptr_New(theAttribute)

      ENDFOR
      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
   ENDIF
   
   ; Next, get the dimensions.
   num_dims = info.ndims
   IF num_dims GT 0 THEN BEGIN
      theDimensions = REPLICATE({NCDF_DATA_DIMENSION}, num_dims)
      FOR j=0,num_dims-1 DO BEGIN
          NCDF_DIMINQ, fileID, j, dimension_name, dimension_size
          IF dimension_size EQ 0 THEN BEGIN
            IF Ptr_Valid(self.zeroDimensionID) $
                THEN *self.zeroDimensionID = [*self.zeroDimensionID, j] $
                ELSE  self.zeroDimensionID = Ptr_New(j)
          ENDIF
          theDimensions[j].name = dimension_name
          theDimensions[j].value = String(dimension_size)
      ENDFOR
      self.theDimensions = Ptr_New(theDimensions, /No_Copy)
   ENDIF

   ; Next, get the variables.
   num_vars = info.nvars
   IF num_vars GT 0 THEN BEGIN
      theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)
      FOR j=0,num_vars-1 DO BEGIN

         ; Get information about the variable.
         varinfo = NCDF_VarInq(fileID, j)
         theVariables[j].datatype = varinfo.datatype
         theVariables[j].name = varinfo.name

          ; If this variable has attributes, get those, too.
          IF varinfo.natts GT 0 THEN BEGIN
               varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, varinfo.natts)
               FOR k=0,varinfo.natts-1 DO BEGIN
                   attribute_name = NCDF_AttName(fileID, j, k)
                   NCDF_AttGet, fileID, j, attribute_name, theAttribute
                   attinfo = NCDF_ATTINQ(fileID, j, attribute_name)
                   att_type = StrUpCase(attinfo.dataType)
          
                   ; Strings are stored as byte values in attributes, so convert them back.
                   IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
                     THEN theAttribute = String(theAttribute)
                   varAttributes[k].attrType = StrUpCase(varinfo.name)
                   varAttributes[k].dataType = att_type
                   varAttributes[k].length = N_Elements(theAttribute)
                   varAttributes[k].name = attribute_name
                   varAttributes[k].value = Ptr_New(theAttribute)
               ENDFOR
               theVariables[j].var_attributes = Ptr_New(varAttributes)
          ENDIF

          ; Get information about the variable, including the dimension IDs.
          r = NCDF_VarInq(fileID, j)
          
          ; Now, read the data so you can collect information about it. It is possible
          ; that one of the dimensions has a zero length. If true, reading the data will
          ; cause an error. We want to prevent that, so if we have a zero dimension we
          ; are going to make sure we don't read that variable.
          ; 

          IF Ptr_Valid(self.zeroDimensionID) EQ 0 THEN BEGIN
              IF self.no_read_on_parse THEN BEGIN
                  theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                  theVariables[j].minValue = 0
                  theVariables[j].maxValue = 0                
              ENDIF ELSE BEGIN
                  NCDF_VarGet, fileID, j, data
                  theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                  IF r.datatype NE 'CHAR' THEN BEGIN
                      minData = Min(data, MAX=maxData)
                      theVariables[j].minValue = minData
                      theVariables[j].maxValue = maxData
                  ENDIF
                  Undefine, data       
              ENDELSE
          ENDIF ELSE BEGIN
          
                ; Is there a match between the dimension IDs and any zero dimension ID we 
                ; have stored?
                match = 0
                FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
                    i = Where(r.dim EQ (*self.zeroDimensionID)[m], count)
                    IF count GT 0 THEN match = 1
                ENDFOR
                IF match GT 0 THEN BEGIN
                          theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                          theVariables[j].minValue = 0
                          theVariables[j].maxValue = 0                
                ENDIF ELSE BEGIN
                          IF self.no_read_on_parse THEN BEGIN
                              theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                              theVariables[j].minValue = 0
                              theVariables[j].maxValue = 0                
                          ENDIF ELSE BEGIN
                              NCDF_VarGet, fileID, j, data
                              theVariables[j].dataSize = Ptr_New(self -> FindDimensions(fileID, j))
                              IF r.datatype NE 'CHAR' THEN BEGIN
                                  minData = Min(data, MAX=maxData)
                                  theVariables[j].minValue = minData
                                  theVariables[j].maxValue = maxData
                              ENDIF
                              Undefine, data  
                          ENDELSE     
                ENDELSE
          ENDELSE
      ENDFOR
      self.theVariables = Ptr_New(theVariables, /No_Copy)
   ENDIF

   ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   NCDF_Close, fileID
   if ~keyword_set(silent) then print,'Parse File: ',systime(1)-x

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::Parse_HDF_File

; This internal method parses the new HDF file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN HDF_SD_End, fileID
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

   ; Open the file and find out how many dimensions, global attributes, and variables are there.
;    fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
   fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename),read=~self.no_read_on_parse)
   HDF_SD_Fileinfo, fileID, num_vars, num_attr
   
   ; First, get the global attributes.
   IF num_attr GT 0 THEN BEGIN
      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
      FOR j=0,num_attr-1 DO BEGIN
          HDF_SD_ATTRINFO, fileID, j, DATA=theAttribute, HDF_TYPE=hdf_type, NAME=attribute_name, TYPE=att_type

          theAttributes[j].attrType = 'GLOBAL'
          theAttributes[j].dataType = att_type
          theAttributes[j].length = N_Elements(theAttribute)
          theAttributes[j].name = attribute_name
          IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
          theAttributes[j].value = Ptr_New(theAttribute)
       ENDFOR
      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
   ENDIF

   ; Next, get the variables.
   IF num_vars GT 0 THEN BEGIN
      theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)
      FOR j=0,num_vars-1 DO BEGIN

         ; Get information about the variable.
         sdID = HDF_SD_Select(fileID, j)
         
         ; This routine throws all kinds of scary messages if CALDATA, for example, is
         ; not in the file. Turn this off for this call.
         !QUIET = 1
         HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
            RANGE=range, TYPE=datatype, CALDATA=calData
         !QUIET = 0

         theVariables[j].datatype = datatype
         theVariables[j].name = name
         theVariables[j].calibration = Ptr_New(calData)

          ; If this variable has attributes, get those, too.
          IF natts GT 0 THEN BEGIN
               varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
               FOR k=0,natts-1 DO BEGIN
                   HDF_SD_ATTRINFO, sdID, k, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                             
                   varAttributes[k].attrType = StrUpCase(name)
                   varAttributes[k].dataType = attribute_datatype
                   varAttributes[k].length = N_Elements(theAttribute)
                   varAttributes[k].name = attribute_name
                   IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                   varAttributes[k].value = Ptr_New(theAttribute)
               ENDFOR
               
               ; Add the calibration data as an attibute.
               IF calData.cal EQ 0 THEN BEGIN
                    varAttributes[natts].attrType = StrUpCase(name)
                    varAttributes[natts].dataType = 'STRING'
                    varAttributes[natts].length = 0
                    varAttributes[natts].name = '_calibration_data'
                    varAttributes[natts].value = Ptr_New('Not Present in File')               
               ENDIF ELSE BEGIN
                    varAttributes[natts].attrType = StrUpCase(name)
                    varAttributes[natts].dataType = 'STRUCT'
                    varAttributes[natts].length = N_Tags(calData, /Length)
                    varAttributes[natts].name = '_calibration_data'
                    varAttributes[natts].value = Ptr_New(calData)
               ENDELSE
               
               theVariables[j].var_attributes = Ptr_New(varAttributes)
          ENDIF

          ; Now, read the data so you can collect information about it.
          theVariables[j].dataSize = Ptr_New(dims)
          IF N_Elements(range) NE 0 THEN BEGIN
              theVariables[j].minValue = range[0]
              theVariables[j].maxValue = range[1]
          ENDIF ELSE BEGIN
              IF self.no_read_on_parse THEN BEGIN
                  theVariables[j].minValue = !VALUES.F_NAN
                  theVariables[j].maxValue = !VALUES.F_NAN             
              ENDIF ELSE BEGIN
                  HDF_SD_GetData, sdID, data
                  IF calData.cal NE 0 THEN BEGIN
                        data = calData.cal * (Temporary(data) - calData.offset)
                  ENDIF
                  minData = Min(data, MAX=maxData)
                  theVariables[j].minValue = minData
                  theVariables[j].maxValue = maxData
                  Undefine, data
              ENDELSE
          ENDELSE
          HDF_SD_EndAccess, sdID
      ENDFOR
      self.theVariables = Ptr_New(theVariables, /No_Copy)
   ENDIF

   ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   HDF_SD_End, fileID

END ;---------------------------------------------------------------------------------------------

function NCDF_DATA::parse_hdf5_subgroups, dataset_id, dataset_name, nsubgroups

	varsubgroups  = Replicate({NCDF_DATA_VARIABLE}, nsubgroups+1)
	for m = 0, nsubgroups -1 do begin
		nsubgroups1   = 0
		dataset_name1 = h5g_get_obj_name_by_idx(dataset_id,m)
		objInfo       = h5g_get_objinfo(dataset_id, dataset_name1)
		if objInfo.type eq 'GROUP' then begin
			dataset_id1 = h5g_open(dataset_id, dataset_name1)
			nsubgroups1 = H5G_GET_NUM_OBJS(dataset_id1)
			natts1      = h5a_get_num_attrs_error(dataset_id1)
			varsubgroups[m].name = dataset_name1
			varsubgroups[m].datatype = objInfo.type
			varsubgroups[m].dataSize = Ptr_New(1)
			varsubgroups[m].minValue = !VALUES.F_NAN
			varsubgroups[m].maxValue = !VALUES.F_NAN
		endif else if objInfo.type eq 'DATASET' then begin
			dataset_id1  = h5d_open(dataset_id, dataset_name1)
			parsed       = H5_PARSE(dataset_id, '/'+strjoin([dataset_name,dataset_name1],'/'), FILE=self.filename, PATH=self.directory, READ_DATA = ~self.no_read_on_parse)
			dims         = parsed._DIMENSIONS
			ndims        = parsed._NDIMENSIONS
			natts1       = h5a_get_num_attrs_error(dataset_id1)
			varsubgroups[m].dataSize = Ptr_New(dims)
			varsubgroups[m].datatype = parsed._DATATYPE
			varsubgroups[m].name = dataset_name1
			IF self.no_read_on_parse THEN BEGIN
				varsubgroups[m].minValue = !VALUES.F_NAN
				varsubgroups[m].maxValue = !VALUES.F_NAN             
			ENDIF ELSE BEGIN
				data = h5d_read(dataset_id1)
				minData = Min(data, MAX=maxData)
				varsubgroups[m].minValue = minData
				varsubgroups[m].maxValue = maxData
				Undefine, data
			ENDELSE
		endif else if objInfo.type eq 'TYPE' then begin
			dataset_id1 = h5t_open(dataset_id, dataset_name1)
			parsed      = H5_PARSE(dataset_id, '/'+strjoin([dataset_name,dataset_name1],'/'), FILE=self.filename, PATH=self.directory, READ_DATA = ~self.no_read_on_parse)
			natts1      = H5T_GET_NMEMBERS(Dataset_id1)
			theVariables[j].dataSize = Ptr_New(1)
			theVariables[j].datatype = parsed._DATATYPE
			theVariables[j].name = dataset_name
		endif else begin
			print,'Was tun mit '+objInfo.type+' types??'
			stop
		endelse
		IF natts1 GT 0 THEN BEGIN
			varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts1+1)
			FOR k=0,natts1-1 DO BEGIN
				; types
				if objInfo.type eq 'TYPE' then begin
					theAttribute       = h5t_get_member_value_error(Dataset_id1, k)
					attribute_name     = H5T_GET_MEMBER_NAME(Dataset_id1, k)
					attribute_datatype = H5T_GET_MEMBER_CLASS(Dataset_id1, k)
				endif else begin
					att_id             = h5a_open_idx(dataset_id1, k)
					att_datatype_id    = h5a_get_type(att_id)
					att_dataspace_id   = h5a_get_space(att_id)
					attribute_name     = h5a_get_name(att_id)
					; h5a_read_error is a workaround because h5a_read cannot deal with 0 Byte characters
					theAttribute       = h5a_read_error(att_id)
					attribute_datatype = h5t_get_class(att_datatype_id)
					h5t_close, att_datatype_id
					h5s_close, att_dataspace_id
					h5a_close, att_id
				endelse
				varAttributes[k].dataType = attribute_datatype
				varAttributes[k].length = N_Elements(theAttribute)
				varAttributes[k].name = attribute_name
				varAttributes[k].attrType = StrUpCase(dataset_name1)
				IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
				varAttributes[k].value = Ptr_New(theAttribute)
			ENDFOR
			varsubgroups[m].var_attributes = Ptr_New(varAttributes)
		ENDIF

		if nsubgroups1 gt 0 then begin
			varsubgroups1 = self -> parse_hdf5_subgroups(dataset_id1, [dataset_name,dataset_name1], nsubgroups1)
			varsubgroups[m].group_variables = Ptr_New(varsubgroups1)
		endif

		if objInfo.type eq 'GROUP'   then h5g_close, dataset_id1 
		if objInfo.type eq 'DATASET' then h5d_close, dataset_id1
		if objInfo.type eq 'TYPE'    then h5t_close, dataset_id1

	endfor

	return,varsubgroups
end ;---------------------------------------------------------------------------------------------

function NCDF_DATA::List_Group_Names, file_id, num_of_variables = num_of_variables, list_of_names = list_of_names

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
							name_dum = self -> List_Group_Names(file_id, list_of_names = name_dum)
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

PRO NCDF_DATA::Parse_HDF5_File

	; stapel: routine to read in hdf5 as well
	; This internal method parses the new HDF file initially, and creates 
	; the IDL structures necessary for browsing the object.

	MESSAGE,'  !!Parsing HDF5 files at the moment highly experimental!!', /INFORMATIONAL

	; Error handling
	CATCH, theError
	IF theError NE 0 THEN BEGIN
		CATCH, /CANCEL
		void = Error_Message()
		self.hasBeenParsed = 0
		IF N_Elements(fileID) NE 0 THEN HDF_SD_End, fileID
		RETURN
	ENDIF

	; Check to see if file is available.
	IF self.filename EQ "" THEN BEGIN
		ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
		RETURN
	ENDIF

	; Be sure everything is cleaned up and ready to go.
	self -> CleanParsedStructures
	Ptr_Free, self.theAttributes
	Ptr_Free, self.theDimensions
	Ptr_Free, self.theCalibration
	Ptr_Free, self.theSwaths
	Ptr_Free, self.theVariables
	Ptr_Free, self.zeroDimensionID

	file_id  = h5f_open(self.directory+'/'+self.filename)
	num_attr = h5a_get_num_attrs_error(file_id)

	; First, get the global attributes.
	IF num_attr GT 0 THEN BEGIN
		theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
		FOR j=0,num_attr-1 DO BEGIN
			att_id          = h5a_open_idx(file_id, j)
			att_datatype_id = h5a_get_type(att_id)
			attribute_name  = h5a_get_name(att_id)
			att_type        = h5t_get_class(att_datatype_id)
			;stapel h5a_read_error is a workaround because h5a_read does have problems with 0 Byte characters
			theAttribute    = h5a_read_error(att_id)
			theAttributes[j].attrType = 'GLOBAL'
			theAttributes[j].dataType = att_type
			theAttributes[j].length = N_Elements(theAttribute)
			theAttributes[j].name = attribute_name
			IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
			theAttributes[j].value = Ptr_New(theAttribute)
			h5t_close, att_datatype_id
			h5a_close, att_id
		ENDFOR
		self.theAttributes = Ptr_New(theAttributes, /No_Copy)
	ENDIF

	num_vars  = H5G_GET_NUM_OBJS(file_id)
	; Next, get the variables.
	IF num_vars GT 0 THEN BEGIN
 		theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)

		FOR j=0,num_vars-1 DO BEGIN
			nsubgroups   = 0
			dataset_name = h5g_get_obj_name_by_idx(file_id, j)
			objInfo      = h5g_get_objinfo(file_id, dataset_name)
			if objInfo.type eq 'GROUP' then begin
				dataset_id = h5g_open(file_id, dataset_name)
				nsubgroups = H5G_GET_NUM_OBJS(dataset_id)
				natts      = h5a_get_num_attrs_error(dataset_id)
				theVariables[j].name = dataset_name
				theVariables[j].datatype = objInfo.type
				theVariables[j].dataSize = Ptr_New(1)
				theVariables[j].minValue = !VALUES.F_NAN
				theVariables[j].maxValue = !VALUES.F_NAN             
			endif else if objInfo.type eq 'DATASET' then begin
				dataset_id   = h5d_open(file_id, dataset_name)
				parsed       = H5_PARSE(dataset_id, '/'+dataset_name, FILE=self.filename, PATH=self.directory, READ_DATA = ~self.no_read_on_parse)
				natts        = h5a_get_num_attrs_error(dataset_id)
				dims         = parsed._DIMENSIONS
				ndims        = parsed._NDIMENSIONS
				theVariables[j].dataSize = Ptr_New(dims)
				theVariables[j].datatype = parsed._DATATYPE
				theVariables[j].name = dataset_name
				IF self.no_read_on_parse THEN BEGIN
					theVariables[j].minValue = !VALUES.F_NAN
					theVariables[j].maxValue = !VALUES.F_NAN             
				ENDIF ELSE BEGIN
					data = h5d_read(dataset_id)
					minData = Min(data, MAX=maxData)
					theVariables[j].minValue = minData
					theVariables[j].maxValue = maxData
					Undefine, data
				ENDELSE
			endif else if objInfo.type eq 'TYPE' then begin
				dataset_id = h5t_open(file_id, dataset_name)
				parsed     = H5_PARSE(dataset_id, '/'+dataset_name, FILE=self.filename, PATH=self.directory, READ_DATA = ~self.no_read_on_parse)
				natts      = H5T_GET_NMEMBERS(Dataset_id)
				theVariables[j].dataSize = Ptr_New(1)
				theVariables[j].datatype = parsed._DATATYPE
				theVariables[j].name = dataset_name
			endif else begin
				print,'Was tun mit '+objInfo.type+' types??'
				stop
			endelse
			; search recursively for sub groups
			if nsubgroups gt 0 then begin
				varsubgroups  = self -> parse_hdf5_subgroups(dataset_id, dataset_name, nsubgroups)
 				theVariables[j].group_variables = Ptr_New(varsubgroups)
			endif

; 			natts        = h5a_get_num_attrs_error(dataset_id)
; 			if natts eq 0 and objInfo.type eq 'TYPE' then natts = H5T_GET_NMEMBERS(Dataset_id)

			IF natts GT 0 THEN BEGIN
				varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
				FOR k=0,natts-1 DO BEGIN
					; types
					if objInfo.type eq 'TYPE' then begin
						theAttribute       = h5t_get_member_value_error(Dataset_id, k)
						attribute_name     = H5T_GET_MEMBER_NAME(Dataset_id, k)
						attribute_datatype = H5T_GET_MEMBER_CLASS(Dataset_id, k)
					endif else begin
						att_id             = h5a_open_idx(dataset_id, k)
						att_datatype_id    = h5a_get_type(att_id)
						att_dataspace_id   = h5a_get_space(att_id)
						attribute_name     = h5a_get_name(att_id)
						; h5a_read_error is a workaround because h5a_read does have problems with 0 Byte characters
						theAttribute       = h5a_read_error(att_id)
						attribute_datatype = h5t_get_class(att_datatype_id)
						h5t_close, att_datatype_id
						h5s_close, att_dataspace_id
						h5a_close, att_id
					endelse
					varAttributes[k].dataType = attribute_datatype
					varAttributes[k].length = N_Elements(theAttribute)
					varAttributes[k].name = attribute_name
					varAttributes[k].attrType = StrUpCase(dataset_name)
					IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
					varAttributes[k].value = Ptr_New(theAttribute)
				ENDFOR
				theVariables[j].var_attributes = Ptr_New(varAttributes)
			ENDIF
			if objInfo.type eq 'GROUP'   then h5g_close, dataset_id
			if objInfo.type eq 'DATASET' then h5d_close, dataset_id
			if objInfo.type eq 'TYPE'    then h5t_close, dataset_id
		ENDFOR
		self.theVariables = Ptr_New(theVariables, /No_Copy)
	ENDIF

	; Successfully parsed file.
	self.hasBeenParsed = 1
	
	; Close the file
	h5f_close,file_ID
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::Parse_HDF_EOS_File

   ; This internal method parses the new HDF-EOS file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN BEGIN
        IF N_Elements(swathID) NE 0 THEN ok = EOS_SW_DETACH(swathID)
        ok = EOS_SW_CLOSE(fileID)
      ENDIF
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

   ; Open the file and find out how many swaths, grids, and points there are.
   filename = Filepath(ROOT_DIR=self.directory, self.filename)
   ok = EOS_Query(filename, info)
   ; Process the swaths first.
   IF info.num_swaths GT 0 THEN BEGIN
       theSwaths = Replicate({NCDF_DATA_SWATH}, info.num_swaths)
       FOR j=0, info.num_swaths-1 DO BEGIN
            swathNames = StrSplit(info.swath_names, ',', /EXTRACT)
            fileID = EOS_SW_OPEN(filename, /READ)
            swathID = EOS_SW_Attach(fileID, swathNames[j])
            Print, 'Swath Name: ', swathNames[j]
            theSwaths[j].name = swathNames[j]
            nattr = EOS_SW_INQATTRS(swathID, attrlist)
            IF nattr GT 0 THEN BEGIN
                theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, nattr)
                attrNames = StrSplit(attrlist, ',', /EXTRACT)
                FOR k=0,nattr-1 DO BEGIN
                    ok = EOS_SW_READATTR(swathID, attrNames[k], attrValue)
                    theAttributes[k].name = attrNames[k]
                    theAttributes[k].attrType = 'SWATH ATTRIBUTE'
                    theAttributes[k].datatype = Size(attrValue, /TNAME)
                    theAttributes[k].length = N_Elements(attrValue)
                    IF N_Elements(attrValue) EQ 1 THEN attrValue = attrValue[0]
                    theAttributes[k].value = Ptr_New(attrValue)
                    Help, attrValue
                ENDFOR
                theSwaths[j].attributes = Ptr_New(theAttributes)
                theSwaths[j].nattrs = nattr
            ENDIF
            ndims = EOS_SW_INQDIMS(swathID, dimslist, dimSize)
            IF ndims GT 0 THEN BEGIN
               dimNames = StrSplit(dimslist, ',', /EXTRACT)
               theDimensions = Replicate({NCDF_DATA_DIMENSION}, ndims)
                FOR k=0,ndims-1 DO BEGIN
                    theDimensions[k].name = dimNames[k]
                    theDimensions[k].value = dimSize[k]
                ENDFOR
                theSwaths[j].dimensions = Ptr_New(theDimensions)
                theSwaths[j].ndims = ndims
            ENDIF
            ngeofields = EOS_SW_INQGEOFIELDS(swathID, geofieldslist, rank, numbertype)
            IF ngeofields GT 0 THEN BEGIN
               geoFieldNames = StrSplit(geofieldslist, ',', /EXTRACT)
               theGeoFields = Replicate({NCDF_DATA_VARIABLE}, ngeofields)
                FOR k=0,ngeofields-1 DO BEGIN
    
                    ; Get information about the variable.
                    sdFileID = HDF_SD_START(filename)
                    sdIndex = HDF_SD_NameToIndex(sdFileID, geoFieldNames[k])
                    sdID = HDF_SD_Select(sdFileID, sdIndex)
                 
                    ; This routine throws all kinds of scary messages if CALDATA, for example, is
                    ; not in the file. Turn this off for this call.
                    !QUIET = 1
                    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
                        RANGE=range, TYPE=datatype, CALDATA=calData
                    !QUIET = 0
                 
                    theGeoFields[k].name = geoFieldNames[k]
                    theGeoFields[k].datatype = datatype
                    theGeoFields[k].calibration = Ptr_New(calData)
                    theGeoFields[k].datasize = Ptr_New(dims)
                    IF N_Elements(range) NE 0 THEN BEGIN
                        theGeoFields[k].minValue = range[0]
                        theGeoFields[k].maxValue = range[1]
                        Undefine, range ; Do this so it is not hanging around for the next variable.
                    ENDIF ELSE BEGIN
                        IF self.no_read_on_parse THEN BEGIN
                            theGeoFields[k].minValue = !VALUES.F_NAN
                            theGeoFields[k].maxValue = !VALUES.F_NAN             
                        ENDIF ELSE BEGIN
                            HDF_SD_GetData, sdID, data
                            IF calData.cal NE 0 THEN BEGIN
                                data = calData.cal * (Temporary(data) - calData.offset)
                            ENDIF
                            minData = Min(data, MAX=maxData)
                            theGeoFields[k].minValue = minData
                            theGeoFields[k].maxValue = maxData
                            Undefine, data
                         ENDELSE
                    ENDELSE
                  
                    ; If this variable has attributes, get those, too.
                    ; If this variable has attributes, get those, too.
                    IF natts GT 0 THEN BEGIN
                         varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
                         FOR m=0,natts-1 DO BEGIN
                             HDF_SD_ATTRINFO, sdID, m, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                                     
                             varAttributes[m].attrType = StrUpCase(name)
                             varAttributes[m].dataType = attribute_datatype
                             varAttributes[m].length = N_Elements(theAttribute)
                             varAttributes[m].name = attribute_name
                             IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                             varAttributes[m].value = Ptr_New(theAttribute)
                         ENDFOR
                       
                         ; Add the calibration data as an attibute.
                         IF calData.cal EQ 0 THEN BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRING'
                            varAttributes[natts].length = 0
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New('Not Present in File')               
                         ENDIF ELSE BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRUCT'
                            varAttributes[natts].length = N_Tags(calData, /Length)
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New(calData)
                         ENDELSE
                       
                         theGeoFields[k].var_attributes = Ptr_New(varAttributes)
                     ENDIF
                     theSwaths[j].ngeoFields = ngeoFields
                     HDF_SD_EndAccess, sdID
                ENDFOR
            ENDIF
            ndatafields = EOS_SW_INQDATAFIELDS(swathID, datafieldslist, rank, numbertype)
            IF ndatafields GT 0 THEN BEGIN
               dataFieldNames = StrSplit(datafieldslist, ',', /EXTRACT)
               theDataFields = Replicate({NCDF_DATA_VARIABLE}, ndatafields)
                FOR k=0,ndatafields-1 DO BEGIN
    
                    ; Get information about the variable.
                    sdFileID = HDF_SD_START(filename)
                    sdIndex = HDF_SD_NameToIndex(sdFileID, dataFieldNames[k])
                    sdID = HDF_SD_Select(sdFileID, sdIndex)
                 
                    ; This routine throws all kinds of scary messages if CALDATA, for example, is
                    ; not in the file. Turn this off for this call.
                    !QUIET = 1
                    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
                        RANGE=range, TYPE=datatype, CALDATA=calData
                    !QUIET = 0
                 Print, 'number of swath dataset attributes for variable ' + name + ': ', natts
                    theDataFields[k].name = dataFieldNames[k]
                    theDataFields[k].datatype = datatype
                    theDataFields[k].calibration = Ptr_New(calData)
                    theDataFields[k].datasize = Ptr_New(dims)
                    IF N_Elements(range) NE 0 THEN BEGIN
                        theDataFields[k].minValue = range[0]
                        theDataFields[k].maxValue = range[1]
                        Undefine, range ; Do this so it is not hanging around for the next variable.
                    ENDIF ELSE BEGIN
                        IF self.no_read_on_parse THEN BEGIN
                            theDataFields[k].minValue = !VALUES.F_NAN
                            theDataFields[k].maxValue = !VALUES.F_NAN             
                        ENDIF ELSE BEGIN
                            HDF_SD_GetData, sdID, data
                            IF calData.cal NE 0 THEN BEGIN
                                data = calData.cal * (Temporary(data) - calData.offset)
                            ENDIF
                            minData = Min(data, MAX=maxData)
                            theGeoFields[k].minValue = minData
                            theGeoFields[k].maxValue = maxData
                            Undefine, data
                         ENDELSE
                    ENDELSE
                  
                    ; If this variable has attributes, get those, too.
                    ; If this variable has attributes, get those, too.
                    IF natts GT 0 THEN BEGIN
                         varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
                         FOR m=0,natts-1 DO BEGIN
                             HDF_SD_ATTRINFO, sdID, m, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
                                     
                             varAttributes[m].attrType = StrUpCase(name)
                             varAttributes[m].dataType = attribute_datatype
                             varAttributes[m].length = N_Elements(theAttribute)
                             varAttributes[m].name = attribute_name
                             IF N_Elements(theAttribute) EQ 1 THEN theAttribute = theAttribute[0]
                             varAttributes[m].value = Ptr_New(theAttribute)
                         ENDFOR
                       
                         ; Add the calibration data as an attibute.
                         IF calData.cal EQ 0 THEN BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRING'
                            varAttributes[natts].length = 0
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New('Not Present in File')               
                         ENDIF ELSE BEGIN
                            varAttributes[natts].attrType = StrUpCase(name)
                            varAttributes[natts].dataType = 'STRUCT'
                            varAttributes[natts].length = N_Tags(calData, /Length)
                            varAttributes[natts].name = '_calibration_data'
                            varAttributes[natts].value = Ptr_New(calData)
                         ENDELSE
                       
                         theDataFields[k].var_attributes = Ptr_New(varAttributes)
                     ENDIF
                     theSwaths[j].ndataFields = ndataFields
                     HDF_SD_EndAccess, sdID
                ENDFOR
            ENDIF
            nmaps = EOS_SW_INQMAPS(swathID, mapslist, offset, increment)
            theSwaths[j].nmaps = nmaps
            IF nmaps GT 0 THEN BEGIN
               mapNames = StrSplit(mapslist, ',', /EXTRACT)
                FOR k=0,nmaps-1 DO BEGIN
                    Print, 'Map Name: ', mapNames[k], $
                        '   Offset: ', offset[k], '   Increment: ', increment[k]
                ENDFOR
                Print, ''
            ENDIF
            nidxmaps = EOS_SW_INQIDXMAPS(swathID, mapslist, sizes)
            theSwaths[j].nidxmaps = nidxmaps
            IF nidxmaps GT 0 THEN BEGIN
               mapNames = StrSplit(mapslist, ',', /EXTRACT)
                FOR k=0,nidxmaps-1 DO BEGIN
                    Print, 'Map Name: ', mapNames[k], '   Size: ', sizes[k]
                ENDFOR
                Print, ''
            ENDIF
            ok = EOS_SW_DETACH(swathID)
            ok = EOS_SW_CLOSE(fileID)
        ENDFOR 
        self.theSwaths = Ptr_New(theSwaths)
   ENDIF
   
   
   fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
   HDF_SD_Fileinfo, fileID, num_vars, num_attr
   
  ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   HDF_SD_End, fileID

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadAttribute, theAttribute, SUCCESS=success, silent=silent

;
; NAME:
;       NCDF_DATA::ReadAttribute
;
; PURPOSE:
;
;       This method is used to read and return a global attribute from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> value = nCDFObject -> ReadAttribute(theAttribute)
;
; RETURN VALUE:
;
;       value:      A variable containing the attribute.
;
; ARGUMENTS:
;
;       theAttribute: The name of the attribute you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent

   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1
   
   ; Check for the name of the attribute.
   IF N_Elements(theAttribute) EQ 0 THEN ok = Dialog_Message ('Must pass name of the attribute to read.')

   IF Ptr_Valid(self.theAttributes) EQ 0 THEN begin
	if not keyword_set(silent) then ok = Dialog_Message('No global attributes currently available for file.')
	return,-1
   endif
   index = Where(StrUpCase((*self.theAttributes).name) EQ StrUpCase(theAttribute), count)
   IF count GT 0 THEN BEGIN
      value = *(*self.theAttributes)[index].value
      success = 1
   ENDIF ELSE Begin
      if not keyword_set(silent) then begin
		Message, 'Cannot locate global attribute ' + theAttribute + ' in file.'
      endif else begin
        value = ''
        success = 0
      endelse
   ENDELSE

   RETURN, value

END ;---------------------------------------------------------------------------------------------
; stapel get_file_infos
function NCDF_DATA::get_file_infos, verbose=verbose, infile = infile

	if keyword_set(infile) then begin
		filen = file_basename(infile)
		pathn = file_dirname(infile)
	endif else begin
		filen = self.filename
		pathn = self.directory
		; reset
		self.datum     = stregex(strmid(filen,stregex(filen,'[0-9]{4}')),'[0-9]+',/ext)
; 		self.datum     = stregex(filen,'[0-9]+',/ext)
		self.algoname  = ''
		self.satname   = ''
		self.level     = ''
		self.year      = strmid(self.datum,0,4)
		if strlen(self.datum) eq 7 then begin
			; probably year and doy
			caldat,julday(1,strmid(self.datum,4,3),self.year),mo,da
			self.month     = string(mo,f='(i2.2)')
			self.day       = string(da,f='(i2.2)')
			self.orbit     = ''
		endif else begin
			self.month     = strmid(self.datum,4,2)
			self.day       = strmid(self.datum,6,2)
			self.orbit     = strmid(self.datum,8,4)
		endelse
		self.reference = ''
		self.version   = ''
		self.satnode   = ''
	endelse

	; reset
	datum     = stregex(strmid(filen,stregex(filen,'[0-9]{4}')),'[0-9]+',/ext)
;	datum     = stregex(filen,'[0-9]+',/ext)
	algoname  = stregex(filen,'calipso',/bool,/fold) ? 'CALIPSO' : (stregex(filen,'esacci',/bool,/fold) ? 'ESACCI' : '')
	satname   = stregex(filen,'calipso',/bool,/fold) ? 'calipso' : ''
	level     = ''
	year      = strmid(datum,0,4)
	if strlen(datum) eq 7 then begin
		; probably year and doy
		caldat,julday(1,strmid(datum,4,3),year),mo,da
		month     = string(mo,f='(i2.2)')
		day       = string(da,f='(i2.2)')
		orbit     = ''
	endif else begin
		month     = strmid(datum,4,2)
		day       = strmid(datum,6,2)
		orbit     = strmid(datum,8,4)
	endelse
	reference = ''
	version   = ''
	satnode   = ''

	verb = keyword_set(verbose)
	; hier mal ein versuch für die L1 avhrr dateien
	if H5F_IS_HDF5(pathn+'/'+filen) and stregex(filen,'99999_satproj',/bool) then begin
		dum = strsplit(filen,'_',/ext)
		satname  = dum[0] eq 'metop02' ? 'metopa' : dum[0]
		algoname = 'L1GAC'
		level    = 'l1'
		year     = strmid(dum[1],0,4)
		month    = strmid(dum[1],4,2)
		day      = strmid(dum[1],6,2)
		orbit    = dum[2]
		datum    = year+month+day+orbit
	endif

	theglobattr = self -> ReadGlobalAttr(Success=success,infile = infile)

	; first read from filename
	if stregex(pathn+'/'+filen,'m?d08',/fold,/bool) then begin
		algoname = 'COLL5'
		level    = 'l3c'
		satname  = strmid(filen,0,5) eq 'MOD08' ? 'terra' : 'aqua'
		datum    = stregex(filen,'[0-9]{7}',/ext)
		year     = strmid(datum,0,4)
		caldat,julday(01,strmid(datum,4,3),year),mo,da
		month    = string(mo,f='(i2.2)')
	endif

	if stregex(pathn+'/'+filen,'era?i',/fold,/bool) then begin
		algoname = 'ERA-I'
		level    = 'l3c'
	endif

	if stregex(filen,'isccp.d2',/fold,/bool) then begin
		algoname = 'ISCCP'
		level    = 'l3c'
		year     = stregex(filen,'[0-9]{4}',/ext)
		month    = stregex(strmid(filen,strpos(filen,year)+4),'[0-9]{2}',/ext)
		datum    = year+month
	endif

	if stregex(filen,'patmosx',/fold,/bool) then begin
		algoname = 'PATMOS'
		level    = stregex(pathn,'gewex',/fold,/bool) ? 'l3c' : ''
		year     = stregex(filen,'[0-9]{4}',/ext)
		month    = stregex(strmid(filen,strpos(filen,year)+4),'[0-9]{2}',/ext)
		datum    = year+month
	endif

	if stregex(pathn+'/'+filen,'apollo',/fold,/bool) then begin
		if where(tag_names(theglobattr) eq 'ORIGINATOR_LONG') ge 0 then begin
			tcd = (theglobattr).ORIGINATOR_LONG
			if tcd eq 'Swansea University' then begin
				algoname = 'SWANSEA'
				level    = 'l2'
			endif
		endif else begin
			algoname = 'APOLLO'
			level    = 'l2'
			dum           = strsplit(filen,/ext,'_')
			if dum[0] eq 'ATS' then satname = 'envisat'
			datum    = dum[1]+dum[2]
			year     = strmid(datum,0,4)
			month    = strmid(datum,4,2)
			day      = strmid(datum,6,2)
			orbit    = strmid(datum,8,4)
		endelse
	endif

	; MODIS COLL5 AND COLL6
	if where(tag_names(theglobattr) eq 'COREMETADATA_0') ge 0 then begin
		tcd = (theglobattr).COREMETADATA_0
		dum = read_modis_obj_val(tcd,'LOCALVERSIONID')
		if stregex(dum,'006',/bool,/fold) then algoname = 'COLL6'
		if stregex(dum,'051',/bool,/fold) then algoname = 'COLL5'
		dum1 = read_modis_obj_val(tcd,'RANGEBEGINNINGDATE',found=found_date)
		dum2 = read_modis_obj_val(tcd,'RANGEBEGINNINGTIME',found=found_time)
		if found_date and found_time then begin
			dum1 = strsplit(dum1,'-',/ext)
			dum2 = strsplit(dum2,':',/ext)
			year  = dum1[0]
			month = dum1[1]
			day   = dum1[2]
			orbit= dum2[0]+dum2[1]
			usta = ymdhms2unix(dum1[0],dum1[1],dum1[2],dum2[0],dum2[1])
		endif
		dum1 = read_modis_obj_val(tcd,'RANGEENDINGDATE',found=found_date)
		dum2 = read_modis_obj_val(tcd,'RANGEENDINGTIME',found=found_time)
		if found_date and found_time then begin
			dum1 = strsplit(dum1,'-',/ext)
			dum2 = strsplit(dum2,':',/ext)
			uend = ymdhms2unix(dum1[0],dum1[1],dum1[2],dum2[0],dum2[1])
		endif
		if keyword_set(usta) and keyword_set(uend) then begin
			if (uend-usta) eq 300l    then level = 'l2'  	; one l2 granule is 5 minute
			if (uend-usta) gt 100000l then begin		; more than a day means 1 month
				level = 'l3c'
				day   = ''
				orbit = ''
			endif
		endif
		dumn = read_modis_obj_val(tcd,'ASSOCIATEDPLATFORMSHORTNAME',found=found_name)
		if found_name then satname = strjoin(strlowcase(dumn),',')
		datum   = year+month+day+orbit
	endif

	; MODIS COLL5 AND COLL6
	if where(tag_names(theglobattr) eq 'STRUCTMETADATA_0') ge 0 then begin
		tcd = (theglobattr).STRUCTMETADATA_0
		dum = read_modis_obj_val(tcd,'SWATH_1',value='SwathName',found=found_name)
		if found_name then begin
			case strlowcase(dum) of 
				'mod06'			: level = 'l2'
				'myd06'			: level = 'l2'
				'modis_swath_type_l1b'	: begin & level = 'l1' & algoname = 'L1MODIS' & end
				'modis_swath_type_geo'	: begin & level = 'l1' & algoname = 'L1MODIS' & end
				else:		stop
			endcase
		endif
	endif

	if where(tag_names(theglobattr) eq 'TITLE') ge 0 then begin
		tcd = (theglobattr).title
		if verb then print,'TITLE: ',tcd
		if stregex(tcd,'CALIPSO',/fold,/bool)  then algoname = 'CALIPSO'
		if stregex(tcd,'CLARA-A1',/fold,/bool) then algoname = 'CLARA'
		if stregex(tcd,'CLARA-A2',/fold,/bool) then algoname = 'CLARA2'
		if stregex(tcd,'CLAAS',/fold,/bool)    then algoname = 'CLAAS'
		; Modis science team
		if stregex(tcd,'MODIS',/fold,/bool) then begin
			if stregex(tcd,'LEVEL 2',/fold,/bool) then level = 'l2'
		endif
		;cc4cl, fame-c,aatsr
		if stregex(tcd,'ESA Cloud CCI Retrieval Products',/fold,/bool) then begin
			algoname = 'ESACCI'
			if stregex(tcd,'L2',/fold,/bool)  then level = 'l2'
			if stregex(tcd,'L3',/fold,/bool)  then level = 'l3c'
			if stregex(tcd,'L3U',/fold,/bool) then level = 'l3u'
			if stregex(filen,'L2B_SUM',/bool,/fold) then level = 'l2b_sum'
		endif
		; Gewex
		if stregex(tcd,'GEWEX',/bool,/fold) then begin
			if where(tag_names(theglobattr) eq 'CLIMATOLOGY') ge 0 then begin
				tcd = (theglobattr).climatology
				if stregex(tcd,'CLARA-A2',/fold,/bool) then algoname = 'GAC2-GEWEX'
				if stregex(tcd,'CLARA_A2',/fold,/bool) then algoname = 'GAC2-GEWEX'
				if stregex(tcd,'PATMOSX',/bool,/fold)  then algoname = 'PATMOS'
				if stregex(tcd,'ESACCI',/bool,/fold)  then algoname = 'GEWEX'
				if stregex(tcd,'CLOUD_CCI',/bool,/fold)  then algoname = 'GEWEX'
			endif else algoname = 'GEWEX'
			year     = (reverse(strsplit((file_basename(filen,is_compressed(file) ? 'nc.gz':'.nc')),'_',/ext)))[0]
			month    = '01'
			level    = 'l3c'
		endif
	endif
	if where(tag_names(theglobattr) eq 'INSTRUMENT') ge 0 then begin
		tcd = (theglobattr).instrument
		if verb then print,'Instrument: ',tcd
		if stregex(tcd,'HIRS',/bool)  then algoname = 'HECTOR'
	endif
	if where(tag_names(theglobattr) eq 'DATA_NODE') ge 0 then begin
		tcd = (theglobattr).DATA_NODE
		if verb then print,'DATA_NODE: ',tcd
		satnode = tcd
	endif
	if where(tag_names(theglobattr) eq 'TIME_COVERAGE_RESOLUTION') ge 0 then begin
		tcd = (theglobattr).TIME_COVERAGE_RESOLUTION
		if verb then print,'TIME_COVERAGE_RESOLUTION: ',tcd
		if tcd eq 'P1M' then level = 'l3c'
		if tcd eq 'P1D' then level = 'l3u'
		if tcd eq 'P5D' then level = 'l3pm'
		if algoname eq 'CLARA2' then begin
			if stregex(filen,'dm',/bool) then level = 'l3dm'
			if stregex(filen,'dh',/bool) then level = 'l3dh'
		endif
		if algoname eq 'ESACCI' then begin
			if stregex(filen,'L2B_SUM',/bool,/fold) then level = 'l2b_sum'
		endif
	endif
	if where(tag_names(theglobattr) eq 'TIME_COVERAGE_START') ge 0 then begin
; 		tcd = strjoin(strsplit((theglobattr).TIME_COVERAGE_START,'-',/ext))
		tcd = strjoin(strreplace((theglobattr).TIME_COVERAGE_START,['\.','-',':','T','Z'],['','','','','']))
		if verb then print,'TIME_COVERAGE_START: ',tcd
		year   = strmid(tcd,0,4)
		month  = strmid(tcd,4,2)
		if level ne 'l3c' then day = strmid(tcd,6,2)
		if level eq '' then orbit  = strmid(tcd,8,4)
		datum = year+month+day+orbit
	endif

	; clara, claas
	if where(tag_names(theglobattr) eq 'PROCESSED_SATELLITES') ge 0 then begin
		tcd = (theglobattr).PROCESSED_SATELLITES
		if verb then print,'PROCESSED_SATELLITES: ',tcd
		satname  = strlowcase((strsplit(tcd,'(',/ext))[0])
	endif
	; Claas l2
	if where(tag_names(theglobattr) eq 'PACKAGE') ge 0 then begin
		tcd = (theglobattr).PACKAGE
		if verb then print,'PACKAGE: ',tcd
		if stregex(tcd,'safnwc/msg',/bool,/fold) then begin
			algoname = 'CLAAS'
			satname  = 'msg'
			level    = 'l2'
			if where(tag_names(theglobattr) eq 'IMAGE_ACQUISITION_TIME') ge 0 then begin
				datum    = (theglobattr).IMAGE_ACQUISITION_TIME
				year     = strmid(datum,0,4)
				month    = strmid(datum,4,2)
				day      = strmid(datum,6,2)
				orbit    = strmid(datum,8,4)
			endif
		endif
	endif

	; esacci
	if where(tag_names(theglobattr) eq 'PRODUCT_VERSION') ge 0 then begin
		tcd = (theglobattr).product_version
		if verb then print,'PRODUCT_VERSION: ',tcd
		version = 'V'+strjoin(strlowcase(strjoin(strsplit((strsplit(tcd,'_',/ext))[0],'-',/ext))),',')
	endif
	if where(tag_names(theglobattr) eq 'PLATFORM') ge 0 then begin
		tcd = (theglobattr).platform
		if verb then print,'PLATFORM: ',tcd
		satname  = strjoin(strlowcase(strjoin(strsplit((strsplit(tcd,'_',/ext))[0],'-',/ext))),',')
		if algoname eq 'CLARA2' or algoname eq 'HECTOR' then begin
			dumsatn = strcompress((strsplit(satname,'>',/ext))[0],/rem)
			case dumsatn of
				'noaapoes'	: begin & dumsatn = (algoname eq 'HECTOR') ?'hirss':'avhrrs' &  level = 'l3s' & end
				'metop'		: dumsatn = (strsplit(satname,'>',/ext))[1]
				else		:
			endcase
			satname = strlowcase(dumsatn)
		endif
		if algoname eq 'GAC2-GEWEX' then begin
			case strupcase(strmid(satname,0,3)) of
				'AVN'	: dumsatn = 'noaa'+(strsplit(strmid(satname,3),',',/ext))[0]
				'AVM'	: dumsatn = 'metop'+(strsplit(strmid(satname,4),',',/ext))[0]
				'AVP'	: dumsatn = 'allsat'
				else	: dumsatn = satname
			endcase
			satname = strlowcase(dumsatn)
		endif
		if algoname eq 'ESACCI' and level eq '' then begin
			if stregex(filen,'L2',/fold,/bool) then level = 'l2'
			if stregex(filen,'L2B_SUM',/bool,/fold) then level = 'l2b_sum'
			if stregex(filen,'L3U',/bool,/fold) then level = 'l3u'
			if stregex(filen,'L3C',/bool,/fold) then level = 'l3c'
			if stregex(filen,'L3S',/bool,/fold) then level = 'l3s'
		endif
	endif

	if where(tag_names(theglobattr) eq 'SENSOR') ge 0 then begin
		tcd = (theglobattr).sensor
		if verb then print,'SENSOR: ',tcd
		if tcd eq 'MERISAATSR' then satname = 'aatme'
		if tcd eq 'MERIS-AATSR' then satname = 'aatme'
; 		if tcd eq 'AATSR' then satname = 'aatsr'
; 		if tcd eq 'ATSR2' then satname = 'atsr2'
		if tcd eq 'AATSR' then satname = 'envisat'
		if tcd eq 'ATSR2' then satname = 'ers2'
		if tcd eq 'AVHRR_MERGED' then begin & satname = 'avhrrs' &  level = 'l3s' & end
		if tcd eq 'MODIS_MERGED' then begin & satname = 'modises' &  level = 'l3s' & end
		if tcd eq 'MERGED' then begin & satname = 'allsat' &  level = 'l3s' & end
	endif
	; esacci l2
	if where(tag_names(theglobattr) eq 'PRODUCT_LEVEL') ge 0 then begin
		tcd = (theglobattr).PRODUCT_LEVEL
		if verb then print,'PRODUCT_LEVEL: ',tcd
		level = strlowcase(tcd)
	endif
; 	if where(tag_names(theglobattr) eq 'CLIMATOLOGY') ge 0 then begin
; 		tcd = (theglobattr).CLIMATOLOGY
; 		if verb then print,'CLIMATOLOGY: ',tcd
; 		if stregex(tcd,'patmosx',/bool,/fold) then begin
; 			algoname = 'PATMOS'
; 			level    = 'l3c'
; 		endif
; 	endif
	; patmos l3c has misspelled global attribute!!!
	if where(tag_names(theglobattr) eq 'CLIMATOLOY') ge 0 then begin
		tcd = (theglobattr).CLIMATOLOY
		if verb then print,'CLIMATOLOY: ',tcd
		if stregex(tcd,'patmosx',/bool,/fold) then begin
			algoname = 'PATMOS'
			level    = 'l3c'
		endif
	endif
	if where(tag_names(theglobattr) eq 'PROCESSOR') ge 0 then begin
		tcd = (theglobattr).PROCESSOR
		if verb then print,'PROCESSOR: ',tcd
		if stregex(tcd,'patmos',/bool,/fold) then begin
			algoname = 'PATMOS'
			if stregex(filen,'.level2b.',/fold,/bool) then level='l3u'
			if stregex(filen,'.level2.',/fold,/bool) then level='l2'
		endif
	endif
	if where(tag_names(theglobattr) eq 'SOURCE') ge 0 then begin
		tcd = (theglobattr).SOURCE
		if verb then print,'SOURCE: ',tcd
		if stregex(tcd,'patmos',/bool,/fold) then begin
			algoname = 'PATMOS'
			if stregex(filen,'.level2b.',/fold,/bool) then level='l3u'
			if stregex(filen,'.level2.',/fold,/bool) then level='l2'
		endif
		if stregex(tcd,'ERA-Interim',/bool,/fold) then begin
			algoname = 'ERA-I'
			level    = 'l3c'
		endif
		if stregex(tcd,'COSMO-DE',/bool,/fold) then begin
			algoname = 'COSMO'
			level    = 'l2'
			satname  = 'DE'
		endif
		if stregex(tcd,'COSMO-EU',/bool,/fold) then begin
			algoname = 'COSMO'
			level    = 'l2'
			satname  = 'EU'
		endif
	endif
	if where(tag_names(theglobattr) eq 'SENSOR_NAME') ge 0 then begin
		tcd = (theglobattr).SENSOR_NAME
		if verb then print,'SENSOR_NAME: ',tcd
		dum = strlowcase((strsplit(tcd,'(',/ext))[0])
		satname = strjoin(strcompress(strjoin(strsplit((strsplit(dum,':',/ext))[0],'-',/ext)),/rem),',')
		if stregex(satname[0],['meteosat'],/bool) then satname  = 'msg'
	endif
	if where(tag_names(theglobattr) eq 'YEAR') ge 0 then begin
		tcd = (theglobattr).YEAR
		if verb then print,'YEAR: ',tcd
		year = string((strsplit(tcd,'(',/ext))[0],f='(i4.4)')
	endif
	if where(tag_names(theglobattr) eq 'START_YEAR') ge 0 then begin
		tcd = (theglobattr).START_YEAR
		if verb then print,'START_YEAR: ',tcd
		year = string((strsplit(tcd,'(',/ext))[0],f='(i4.4)')
	endif
	if where(tag_names(theglobattr) eq 'DAY') ge 0 then begin
		tcd = (theglobattr).day
		if verb then print,'DAY: ',tcd
		doy = (strsplit(tcd,'(',/ext))[0]
		caldat,julday(1,doy,year),mm,dd,yy
		month=string(mm,f='(i2.2)')
		day=string(dd,f='(i2.2)')
	endif
	if where(tag_names(theglobattr) eq 'START_DAY') ge 0 then begin
		tcd = (theglobattr).start_day
		if verb then print,'START_DAY: ',tcd
		doy = (strsplit(tcd,'(',/ext))[0]
		caldat,julday(1,doy,year),mm,dd,yy
		month=string(mm,f='(i2.2)')
		day=string(dd,f='(i2.2)')
	endif
	if where(tag_names(theglobattr) eq 'START_TIME') ge 0 then begin
		tcd = (theglobattr).start_time
		if verb then print,'START_TIME: ',tcd
		dumorb     = float((strsplit(tcd,'(',/ext))[0])
		orbit = string(fix(dumorb),f='(i2.2)')+string(60*(dumorb-fix(dumorb)),f='(i2.2)')
	endif

	if level eq 'l3c' or level eq 'l3s' then begin & day = '' & orbit = '' & end
	if total(level eq ['l3u','l2b_sum','l3pm','l3dm','l3dh']) then orbit = ''

	if level eq 'l3u' and algoname eq 'ESACCI' and total(satname eq ['terra','aqua']) then begin
		; Esacci high resolution l3u data for europe and modis only  
		if (theglobattr).geospatial_lat_min eq  35.0100 and (theglobattr).geospatial_lat_max eq 74.9900 and $
		   (theglobattr).geospatial_lon_min eq -14.9900 and (theglobattr).geospatial_lon_max eq 44.9900 then level = 'l3ue'
	endif
	
	if stregex(satname[0],'meteosat',/bool,/fold) then satname = 'msg'

	; workaround for l2 aatsr files , hopefully not necassary in future
	if satname[0] eq 'envisat' and algoname eq 'ESACCI' and level eq 'l2' then begin
		datum = stregex(filen,'[0-9]{12}',/ext)
		year  = strmid(datum,0,4)
		month = strmid(datum,4,2)
		day   = strmid(datum,6,2)
		orbit = strmid(datum,8,4)
	endif

	; für den Übergang 
	if strupcase(algoname) eq 'ESACCI' and version eq 'fv1.0' then algoname = 'ESACCI_OLD'
	; 
	case strlowcase(algoname) of 
		'coll5'	: reference = (satname[0] eq 'terra' ? 'mod' : 'myd')
		'coll6'	: reference = (satname[0] eq 'terra' ? 'mod2' : 'myd2')
		'era-i'	: reference = 'era'
		'clara'	: reference = 'gac'
		'clara2': reference = 'gac2'
		'gewex'	: reference = 'gwx'
		'gac2-gewex': reference = 'g2_gwx'
		'esacci': reference = 'cci'
		'patmos': reference = 'pmx'
		'claas'	: reference = 'cla'
		'l1gac'	: reference = 'l1gac'
		'hector': reference = 'hec'
		'esacci_old' : reference = 'cci_old'
		else :
	endcase

	if level eq 'l3c' then datum = strmid(datum,0,6)

	if ~keyword_set(infile) then begin
		self.satname   =  satname[0]
		self.algoname  =  algoname
		self.level     =  level
		self.year      =  year
		self.month     =  month
		self.day       =  day
		self.datum     =  datum
		self.orbit     =  orbit
		self.reference =  reference
		self.version   =  version
		self.satnode   =  satnode
	endif

	out = {satname:satname[0],algoname:algoname,level:level,year:year,month:month,day:day,orbit:orbit,datum:datum,reference:reference,version:version}

	if verb then begin
		print,'Set Satname to   : '+satname[0]
		print,'Set Algoname to  : '+algoname
		print,'Set Level to     : '+level
		print,'Set Year to      : '+year
		print,'Set Month to     : '+month
		print,'Set Day to       : '+day
		print,'Set Date to      : '+datum
		print,'Set Orbit to     : '+orbit
		print,'Set SatNode to   : '+satnode
		print,'Set Reference to : '+reference
		print,'Set Version      : '+version
	endif

	return, out
end

PRO NCDF_DATA::ReadAttributeFromGUI, event

   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Attribute to Read: ')

;    theList = ['All', (*self.theAttributes).name]
	theList = ['All', (Ptr_Valid(self.theAttributes) ? (*self.theAttributes).name : 'none')]
   self.attributeID = Widget_Droplist(row, Value=theList, UNAME='ATTRIBUTES', UVALUE=[theList], SCR_XSIZE=250)
   label = Widget_Label(row, Value='Attribute Name: ')
   self.attrnameID = Widget_Text(row, Value='All_Attributes', /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Attribute Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Attribute and Leave', UVALUE='READ_ATTRIBUTE_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Attribute and Stay', UVALUE='READ_ATTRIBUTE_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_ATTRIBUTE_GUI')

   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=attrName
   index = Where(theList EQ attrName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
      Widget_Control, self.attrnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_attribute_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadAttributeFromGUI_Events, event

   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_ATTRIBUTE_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               
               attrName = (addName) ? FSC_Base_FileName(self.filename) + '_' + attrName[0] : attrName[0]
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN begin
                        Widget_Control, event.top, /DESTROY
			RETURN
		   endif
                   IF success EQ 0 THEN RETURN
               ENDELSE
               
               ; Create the variable at the main IDL level. 
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               ; Go to the next attribute on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
               Widget_Control, self.attrNameID, Set_Value=theList[index]
               END
               
            'READ_ATTRIBUTE_AND_LEAVE': BEGIN
               
               ; Get the attribute name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               attrName = (addName) ? FSC_Base_FileName(self.filename) + '_' + attrName[0] : attrName[0]
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN begin
                        Widget_Control, event.top, /DESTROY
			RETURN
		   endif
               ENDELSE
               
               ; Create the attribute at the main IDL level.
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_ATTRIBUTE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.attrNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadDimension, dimensionName, SUCCESS=success

;
; NAME:
;       NCDF_DATA::ReadDimension
;
; PURPOSE:
;
;       This method is used to read and return a dimension of a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> dimension = nCDFObject -> ReadDimension(dimensionName)
;
; RETURN VALUE:
;
;       dimension: The value of the dimension.
;
; ARGUMENTS:
;
;       dimensionName:   The name of the dimension to read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   theFile = Filepath(ROOT_DIR=self.directory, self.filename)

   ; Open the file.
   fileID = NCDF_Open(theFile)
   info = NCDF_Inquire(fileID)
   
   ; Add the dimensions.
   IF info.ndims GT 0 THEN BEGIN
      dimsStruct = Create_Struct('ndims', info.ndims)
      FOR j=0,info.ndims-1 DO BEGIN
         NCDF_DIMINQ, fileID, j, name, value
         name = IDL_ValidName(name, /CONVERT_ALL)
         dimsStruct = Create_Struct(dimsStruct, name, value)
       ENDFOR
    ENDIF
   
   ; Can you find a field in the structure with the dimension name?
   fields = Tag_Names(dimsStruct)
   i = Where(fields EQ StrUpCase(dimensionName), count)
   IF count EQ 0 THEN Message, 'Cannot find a dimension with name: ' + dimensionName + ' in file.'
   value = dimsStruct.(i)
  
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, value

END ;---------------------------------------------------------------------------------------------




FUNCTION NCDF_DATA::ReadFile, theFile, SUCCESS=success, silent = silent

;
; NAME:
;       NCDF_DATA::ReadFile
;
; PURPOSE:
;
;       This method is used to read and return the contents of a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadFile(theFile)
;
; RETURN VALUE:
;
;       data:      A structure variable containing the filename, a structure of global attributes,
;                  a structure of dimensions, and one struture for each variable in the file.
;
; ARGUMENTS:
;
;       theFile:   The optional name of a netCDF or HDF file to read. If not supplied, the
;                  name of the file currently stored in the object will be read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   IF N_Elements(theFile) EQ 0 THEN theFile = Filepath(ROOT_DIR=self.directory, self.filename)
   IF File_Test(theFile, /READ) EQ 0 THEN Message, 'Specified file does not exist or is not readable.'

   ; Branch appropriately.
   IF self.isHDF THEN BEGIN
   
       ; Open the file.
       fileID = HDF_SD_Start(theFile)
       
       ; Create the initial structure.
       struct = Create_Struct('_filename', self.filename)
   
       ; Add the global attributes.
       g_attributes = self -> ReadGlobalAttr(Success=success,silent=silent)
       IF success THEN struct = Create_Struct(struct, '_global_attr', Temporary(g_attributes))

       HDF_SD_Fileinfo, fileID, nvars, nattrs
       FOR j=0,nvars-1 DO BEGIN
       
           ; Select the variable and read it.
           varID = HDF_SD_Select(fileID, j)
           HDF_SD_GetData, varID, data
           HDF_SD_GetInfo, varID, NAME=varName, NATTS=natts

           data = Reform(Temporary(data))
           varStruct = Create_Struct('data', Temporary(data))
           varStruct = Create_Struct(varStruct,'dataset_name', varName)

           ; If this variable has attributes, get those, too.
           IF natts GT 0 THEN BEGIN
                varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts)
                FOR k=0,natts-1 DO BEGIN
                    HDF_SD_ATTRINFO, varID, k, DATA=value, NAME=attrName
                    attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                    IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
                    varStruct = Create_Struct(varStruct, attrName, value)         
                ENDFOR
                struct = Create_Struct(struct, varName, Temporary(varStruct))
                
           ENDIF
       ENDFOR
       HDF_SD_EndAccess, varID
       HDF_SD_END, fileID
       success = 1
   endif else if self.isHDF5 then begin    
	; stapel read in hdf5 file
	struct = h5_parse(theFile,/read)
	success = 1
   ENDIF ELSE BEGIN
   
       ; Open the file.
       fileID = NCDF_Open(theFile)
       info = NCDF_Inquire(fileID)

       ; Create the initial structure.
       struct = Create_Struct('_filename', self.filename)

       ; Add the global attributes.
       g_attributes = self -> ReadGlobalAttr(Success=success,silent=silent)
       IF success THEN struct = Create_Struct(struct, '_global_attr', Temporary(g_attributes))
    
       ; Add the dimensions.
       IF info.ndims GT 0 THEN BEGIN
          dimsStruct = Create_Struct('_ndims', info.ndims)
          FOR j=0,info.ndims-1 DO BEGIN
             NCDF_DIMINQ, fileID, j, name, value
             name = IDL_ValidName(name, /CONVERT_ALL)
             dimsStruct = Create_Struct(dimsStruct, name, value)
           ENDFOR
           struct = Create_Struct(struct, '_dimensions', dimsStruct)
       ENDIF
       
       ; Add the variables.
       IF info.nvars GT 0 THEN BEGIN
          FOR j=0,info.nvars-1 DO BEGIN
             varInfo = NCDF_VarInq(fileID, j)
             NCDF_VarGet, fileID, j, data
             varName = varInfo.name
             IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
             varStruct = Create_Struct('data', Temporary(data))
             varStruct = Create_Struct(varStruct,'dataset_name', varName)
             
             ; Add the variable attributes to the structure.
             FOR k=0,varInfo.natts-1 DO BEGIN
                attrName = NCDF_AttName(fileID, j, k)
                NCDF_AttGet, fileID, j, attrName, theAttribute
                attinfo = NCDF_ATTINQ(fileID, j, attrName)
                att_type = StrUpCase(attinfo.dataType)
              
                ; Strings are stored as byte values in attributes, so convert them back.
                IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
                   THEN theAttribute = String(theAttribute)
                
                 attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                 varStruct = Create_Struct(varStruct, attrName, theAttribute)
             ENDFOR
          struct = Create_Struct(struct, IDL_ValidName(varInfo.name, /CONVERT_ALL), $
              Temporary(varStruct))
          ENDFOR
       ENDIF
       
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
   
   ENDELSE
   
   RETURN, struct

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadFileFromGUI, event

   ; This internal method obtains the name of the variable from the user
   ; and creates a variable of that name at the main IDL level.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; What would you like to name the variable.
   varName = TextBox(Title='Name of IDL Variable...', Label='Name of IDL Variable:', $
      Value='data', Cancel=cancelled)
   IF cancelled THEN RETURN
   varName = IDL_ValidName(varName, /CONVERT_ALL)
   IF varName EQ "" THEN Message, 'Variable names cannot be NULL.'
   
   ; Read the NCDF data file.
   Widget_Control, /HOURGLASS
   data = self -> ReadFile(Success=success)
   IF success EQ 0 THEN RETURN
   
   ; Create a main-level variable.
   (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = data
    Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadGlobalAttr, SUCCESS=success, infile=infile, silent = silent

;
; NAME:
;       NCDF_DATA::ReadGlobalAttr
;
; PURPOSE:
;
;       This method is used to read and return the global attributes of a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadGlobalAttr()
;
; RETURN VALUE:
;
;       struct:      A structure variable containing global attributes of the file.
;                    The attribute names are the fields of the structure.
;
; ARGUMENTS:
;
;       None. The global attributes of the file loaded into the object will be read and returned.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF

   if keyword_set(infile) then begin
	filen = file_basename(infile)
	pathn = file_dirname(infile)
   endif else begin
		filen = self.filename
		pathn = self.directory
		; Make sure the file has been parsed.
		IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
		; Check again.
		success = 0
		IF self.hasBeenParsed EQ 0 THEN RETURN, -1
   endelse

   ; Determine if this is a netCDF or HDF file.
   isHDF  = HDF_ISHDF(Filepath(ROOT_DIR=pathn, filen))
   isHDF5 = H5F_IS_HDF5(pathn+'/'+filen)
   if isHDF5 then begin
	if is_ncdf(pathn+'/'+filen) then isHDF5 = 0
   endif

   IF isHDF THEN BEGIN

        ; Open the file and find out how many dimensions, global attributes, and variables are there.
;         fileID = HDF_SD_START(Filepath(ROOT_DIR=self.directory, self.filename), /READ)
;         HDF_SD_Fileinfo, fileID, num_vars, num_attr
        fileID = HDF_SD_START(Filepath(ROOT_DIR=pathn, filen), /READ)
        HDF_SD_Fileinfo, fileID, num_vars, num_attr

       ; Create a structure to hold the global attribute values.
;        attrStruct = Create_Struct('hdf_filename', self.filename)
       attrStruct = Create_Struct('hdf_filename', filen)
       FOR j=0,num_attr-1 DO BEGIN
           HDF_SD_ATTRINFO, fileID, j, DATA=value, NAME=name
           
          ; Names cannot have oddball characters in them. They have to
          ; conform to IDL's rules for creating variable names.
          name = IDL_ValidName(name, /CONVERT_ALL)
          IF Where(Tag_Names(attrStruct) EQ StrUpCase(name)) NE -1 THEN CONTINUE
          attrStruct = Create_Struct(attrStruct, name, value)
       ENDFOR
       
       ; Close the file, set status flag, return the data.
       HDF_SD_End, fileID
       success = 1

       RETURN, attrStruct

   endif else if isHDF5 then begin
	; stapel read from hdf5 file
	fileid     = h5f_open(pathn+'/'+filen)
	num_attr   = h5a_get_num_attrs_error(fileid)
	attrStruct = Create_Struct('hdf5_filename', filen)
	FOR j=0,num_attr-1 DO BEGIN
		att_id = h5a_open_idx(fileid, j)
		name   = h5a_get_name(att_id)
		; Names cannot have oddball characters in them. They have to
		; conform to IDL's rules for creating variable names.
		name = IDL_ValidName(name, /CONVERT_ALL)
		IF Where(Tag_Names(attrStruct) EQ StrUpCase(name)) NE -1 THEN CONTINUE
		; stapel h5a_read_error is a workaround because h5a_read does have problems with 0 Byte characters
		attrStruct = Create_Struct(attrStruct, name, h5a_read_error(att_id))
		h5a_close, att_id
	ENDFOR
	; Close the file, set status flag, return the data.
	h5f_close,fileID
	success = 1
	RETURN, attrStruct
   ENDIF ELSE BEGIN

       ; Open the file.
;        fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
;        info = NCDF_Inquire(fileID)
       fileID = NCDF_Open(Filepath(ROOT_DIR=pathn, filen))
       info = NCDF_Inquire(fileID)
    
       ; Create a structure to hold the global attribute values.
;        attrStruct = Create_Struct('ncdf_filename', self.filename)
       attrStruct = Create_Struct('ncdf_filename', filen)
       FOR j=0,info.ngatts-1 DO BEGIN
          name = NCDF_AttName(fileID, j, /GLOBAL)
          NCDF_AttGet, fileID, name, value, /GLOBAL
          attinfo = NCDF_ATTINQ(fileID, name, /GLOBAL)
          att_type = StrUpCase(attinfo.dataType)
          IF Size(value, /TNAME) EQ 'BYTE' AND att_type EQ 'CHAR' THEN value = String(value)
          
          ; Names cannot have oddball characters in them. They have to
          ; conform to IDL's rules for creating variable names.
          name = IDL_ValidName(name, /CONVERT_ALL)
          attrStruct = Create_Struct(attrStruct, name, value)
       ENDFOR
    
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
       RETURN, attrStruct

   ENDELSE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVariable, theVariable, $
    SUCCESS=success, $
    COUNT=count, $
    FILLVALUE=fillvalue, $
    MISSINGINDICES=missingIndices, $
    OFFSET=offset, $
    START=start, $
    STRIDE=stride
    
;
; NAME:
;       NCDF_DATA::ReadVariable
;
; PURPOSE:
;
;       This method is used to read and return a variable from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       data:      The nCDF variable.
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; INPUT KEYWORD PARAMETERS:
; 
;       COUNT:      An optional vector containing the counts to be used in reading theVariable.
;                   Count is a 1-based vector with an element for each dimension. The default 
;                   matches the size of the variable so that all data is written out. 
;                   
;       OFFSET:     An optional vector containing the starting position for the read. The default 
;                   start position is [0, 0, ...].
;                   
;       START:      Equivalent to the OFFSET vector, except for HDF files.
;                   
;       STRIDE:     An optional vector containing the strides, or sampling intervals, between 
;                   accessed values of the netCDF variable. The default stride vector is that 
;                   for a contiguous read, [1, 1, ...]. Note that for HDF files, the default
;                   STRIDE vector is [0, 0, ...].
;       
; OUTPUT KEYWORD PARAMETERS:
; 
;       FILLVALUE:  The value that is being used for the "missing" value in this variable.
;                                                                              
;       MISSINGINDICES: A vector containing the missing indices in the returned data. Missing
;                   data is identified by either the depreciated "missing_value" attribute
;                   or the approved "_FillValue" attribute.  
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      IF self.isHDF THEN HDF_SD_End, fileID ELSE if self.ishdf5 then h5f_close,fileID else NCDF_CLOSE, fileID
      success = 0
      RETURN, -1
   ENDIF

   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'
   
   ; Assume no success.
   success = 0

   ; Read the variable, based on what kind of file this is.
   IF self.isHDF THEN BEGIN
       
       ; Open the file.
       fileID = HDF_SD_Start(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the index of the variable.
       index = HDF_SD_NameToIndex(fileID, theVariable)
       IF index EQ -1 THEN Message, 'Variable (' + theVariable + ') not found.'
       
       ; Select the variable and read it.
       varID = HDF_SD_Select(fileID, index)
       
       ; Make sure this variable has a valid dimension.
       HDF_SD_GetInfo, varID, DIMS=dims
       IF dims[0] EQ 0 THEN BEGIN
            void = Dialog_Message('Requested data variable has a dimension of 0 and cannot be read.')
            RETURN, -1
       ENDIF
       
       ; Read the data.
       HDF_SD_GetData, varID, data, COUNT=count, START=start, STRIDE=stride
       
       ; This routine throws all kinds of scary messages if CALDATA is
       ; not in the file. Turn this off for this call.
       !QUIET = 1
       HDF_SD_GetInfo, varID, CALDATA=calData
       !QUIET = 0
       
       HDF_SD_EndAccess, varID
       
       ; Reverse the indices in HDF files.
       data = Reform(Temporary(data))
       IF calData.cal NE 0 THEN data = calData.cal * (Temporary(data) - calData.offset)

       ; Close the file
       HDF_SD_End, fileID
       success = 1
       RETURN, data
   endif else if self.ishdf5 then begin
	; stapel read hdf5 variable
	; Open the file.
	fileid  = h5f_open(self.directory+'/'+self.filename)
	; Select the variable and read it.
	if ~is_h5data(fileid, theVariable) then begin
		ok = dialog_message( 'Variable (' + theVariable + ') not found or not of type data.')
		return, -1
	endif
	varid = h5d_open(fileid, theVariable)
	data  = h5d_read(varid)
	h5d_close, varID
	h5f_close, fileid
	success = 1
	return, data
   ENDIF ELSE BEGIN

       ; Open the file.
       fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       varID = NCDF_VarID(fileID, theVariable)
       
       ; Get information about the variable.
       r = NCDF_VarInq(fileID, varID)
           
       ; Do we have to worry about zero dimensions?
       IF Ptr_Valid(self.zeroDimensionID) THEN BEGIN
       
           ; Is there a match between the dimension IDs and any zero dimension ID we 
           ; have stored?
           match = 0
           FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
               i = Where(r.dim EQ (*self.zeroDimensionID)[m], count)
               IF count GT 0 THEN match = 1
           ENDFOR
           IF match GT 0 THEN BEGIN
               ok = Dialog_Message('This variable has a dimension of length zero and cannot be read.')
               NCDF_CLOSE, fileID
               success = 0
               RETURN, -1             
           ENDIF 
       ENDIF

       ; Read the data.
       NCDF_VarGet, fileID, varID, data, COUNT=count, OFFSET=offset, STRIDE=stride
       
       ; Get the variable attribute names
       IF r.natts GT 0 THEN varAttNames = StrArr(r.natts)
       FOR k=0,r.natts-1 DO BEGIN
           varAttNames[k] = NCDF_AttName(fileID, varID, k)
       ENDFOR
       
       ; Does this variable contain "missing" values. If so, identify and return
       ; the missing data indices so they can be identified after scaling.
       index = Where(StrUpCase(varAttNames) EQ 'MISSING_VALUE', count)
       IF count GT 0 THEN BEGIN
           varAttName = (varAttNames[index])[0]
           NCDF_AttGet, fileID, varID, varAttName, missingValue
           missingIndices = Where(data EQ missingValue, missingCount)
       ENDIF
       index = Where(StrUpCase(varAttNames) EQ '_FILLVALUE', count)
       IF count GT 0 THEN BEGIN
           varAttName = (varAttNames[index])[0]
           NCDF_AttGet, fileID, varID, varAttName, missingValue
           missingIndices = Where(data EQ missingValue, missingCount)
       ENDIF
    
       ; Is there a scale_factor attribute? If so, get and scale the data.
       IF N_Elements(varAttNames) NE 0 THEN BEGIN
           index = Where(StrUpCase(varAttNames) EQ 'SCALE_FACTOR', count)
           IF count GT 0 THEN BEGIN
               varAttName = (varAttNames[index])[0]
               NCDF_AttGet, fileID, varID, varAttName, scale_factor
               IF scale_factor NE 1.0 THEN data = Temporary(data) * scale_factor
           ENDIF
           
           ; Is there an add_offset attribute? If so, get and add to the data.
           index = Where(StrUpCase(varAttNames) EQ 'ADD_OFFSET', count)
           IF count GT 0 THEN BEGIN
               varAttName = (varAttNames[index])[0]
               NCDF_AttGet, fileID, varID, varAttName, add_offset
               data = Temporary(data) + add_offset
           ENDIF
       ENDIF
       
       ; If there was missing data, restore it.
       IF (N_Elements(missingIndices) NE 0) THEN BEGIN
            IF missingCount GT 0 THEN data[missingIndices] = missingValue
       ENDIF
       
       ; Is this a CHAR data type? If so, convert it to a string.
       IF StrUpCase(r.datatype) EQ 'CHAR' THEN data = String(data)
              
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
       RETURN, data
       
   ENDELSE
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI, event

   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
   theList = self -> make_VARList(/data_only)
;    theList = [(*self.theVariables).name]
   self.variablelistID = Widget_Droplist(row, Value=[theList], UVALUE=[theList], SCR_XSIZE=250, UNAME='VARIABLES')
   label = Widget_Label(row, Value='Variable Name: ')
;    self.varnameID = Widget_Text(row, Value=IDL_ValidName((*self.theVariables)[0].name, /CONVERT_ALL), $
;       /Editable, SCR_XSIZE=250)
    self.varnameID = Widget_Text(row, Value='Choose A Variable from List above!', /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)

   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARIABLE_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
;    Widget_Control, theSelection, Get_Value=varName
;    index = Where(theList EQ varName, count)
	Widget_Control, theSelection, Get_UValue=UName
	varname = strjoin(UName,'/')
	index   = Where(theList EQ varName, count)
	IF count GT 0 THEN BEGIN
		Widget_Control, self.variablelistID, SET_DROPLIST_SELECT=index[0]
		Widget_Control, self.varnameID, Set_Value=theList[index[0]]
	ENDIF 
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------

function NCDF_DATA::make_VARList, data_only = data_only

	if self.isHDF5 then begin
		file_id  = h5f_open(self.directory+'/'+self.filename)
		num_vars = H5G_GET_NUM_OBJS(file_id)
		theList  = self -> List_Group_Names( file_id, num_of_variables = num_vars)
		; exclude all except type "dataset" from list
		if keyword_set(data_only) then begin
			typeList = strarr(n_elements(thelist))
			for i=0,n_elements(thelist)-1 do typeList[i] = (h5g_get_objinfo(file_id, thelist[i])).TYPE
			idx = where(typelist eq 'DATASET',tlcnt)
			if tlcnt gt 0 then theList = theList[idx]
		endif
		h5f_close, file_id
	endif else theList = [(*self.theVariables).name]

	if ( self.algoname eq 'ESACCI' and total(self.level eq ['l2','l3u','l3ue']) ) or ( self.algoname eq 'CLARA2' and total(self.level eq ['l3u']) ) then begin
		if total(self.level eq ['l3u','l3ue']) then theList=[theList,'sunglint_asc','sunglint_desc','glint_angle_asc','glint_angle_desc'] else $
		theList=[theList,'sunglint','glint_angle']
	endif
	hists = where((  stregex(thelist,'hist1d_',/bool,/fold) or stregex(thelist,'hist2d_',/bool,/fold) ) and $
			~stregex(thelist,'_bin_',/bool,/fold),nhists,complement=not_hist)

	if nhists gt 0 then begin
		thenewList = thelist[not_hist]
		for i = 0,nhists -1 do begin
			thenewList = [thenewList,thelist[hists[i]]+['','_liq','_ice','_ratio']]
		endfor
		thelist = thenewList
	endif

	theList=[theList,'blue_marble','usgs_lus','usgs_dem']

	return, theList
end

; stapel plotting eingefuehrt
PRO NCDF_DATA::PlotVariableFromGUI, event

	; This internal method sets up a dialog for obtaining information from the user
	; about which variables to read, etc.

	; Error handling
	CATCH, theError
	IF theError NE 0 THEN BEGIN
		CATCH, /CANCEL
		void = Error_Message()
		RETURN
	ENDIF

	ok = self -> get_file_infos()
	screen_size = GET_SCREEN_SIZE(RESOLUTION=resolution)
	xsize = screen_size[0]-480 < 1100
	ysize = screen_size[1]-200 < 880

	; Get some position information.
	Widget_Control, event.top, TLB_GET_OFFSET=offsets
	; We want a non-modal pop-up dialog widget.
	tlb2 = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0], YOFFSET=offsets[1],col=2, BASE_ALIGN_CENTER=1, /FLOATING, $
			  UVALUE=self,title='File: '+self.filename,TLB_SIZE_EVENT=1)
 	  if ysize lt 880 then leftrow = Widget_Base(tlb2, ROW=26,FRAME=1,/scroll,x_scroll_size=280,y_scroll_size=800) else $
	  leftrow = Widget_Base(tlb2, ROW=26,FRAME=1)
	    label = Widget_Label(leftrow, Value='Variable to Plot: ', SCR_XSIZE=270,ALIGN_CENTER=1)
	    theList = self -> make_VARList(/data_only)
	    ; neu use combobox instead of droplist
	    self.variablelistID = Widget_combobox(leftrow, Value=[theList],UVALUE=[theList],Scr_XSize=270, UNAME='PLOTS_VARLIST',/EDITABLE,font='8x13',Scr_YSize=30)
	    bla = Widget_Base(leftrow, row=1,Frame=0, Scr_XSize=270,/nonexclusive)
	      self.enablemima = Widget_Button(bla, Value='Min Value:  Max Value:     P.Multi   Win', UVALUE='SET_PLOT_DEFAULTS',Scr_YSize=21)
	    bla = Widget_Base(leftrow, Column=4,Frame=0, Scr_XSize=270)
	      self.minimumID = Widget_Text(bla, Value=strcompress((*self.theVariables)[0].minvalue), /Editable, SCR_XSIZE=85)
	      self.maximumID = Widget_Text(bla, Value=strcompress((*self.theVariables)[0].maxvalue), /Editable, SCR_XSIZE=85)
              pmult_list     = ['1x1','2x2','3x3','1x2','1x3','2x1','2x3','3x1','3x2','4x1','4x2','4x3','5x1','5x2','5x3']
	      self.pmultID   = Widget_combobox(bla,VALUE=[pmult_list],UVALUE=[pmult_list],Scr_XSize=58,Scr_YSize=28,UNAME='PLOTS_PMULTILIST')
	      self.winnrID   = Widget_Text(bla, Value='1', /Editable, SCR_XSIZE=30)
	    bla = Widget_Base(leftrow, row=1,Frame=0, Scr_XSize=270,/nonexclusive)
	      self.enablelim = Widget_Button(bla, Value='Limit=[lat0,lon0,lat1,lon1] Projections ', UVALUE='SET_PLOT_DEFAULTS',Scr_YSize=21)
	    bla = Widget_Base(leftrow, Column=3,Frame=0, Scr_XSize=270)
	      self.limitID = Widget_Text(bla, Value='No', /Editable, SCR_XSIZE=180)
	      proj_list = ['Default','Globe (Ortho)','Mollweide','Aitoff','Hammer','Goode','Sinusoidal','Robinson','Stereo','Satellite']; EASE-grid  = equal angle 'Lambert' ??
	      self.projlist = Widget_combobox(bla, Value=[proj_list],UVALUE=[proj_list],Scr_XSize=85,Scr_YSize=28,UNAME='PLOTS_PROJLIST')
	    label = Widget_Label(leftrow, Value='  TS-SymSiz Bin/Z  Rot/BC Magn.  P0lon  P0lat ', SCR_XSIZE=270)
	    bla = Widget_Base(leftrow, Column=6,Frame=0, Scr_XSize=270)
	      syms_list      = [string([0.,0.5,1+((indgen(21))/10.) ],f='(f3.1)')]
	      self.symsizeID = Widget_combobox(bla,VALUE=[syms_list],UVALUE=[syms_list],Scr_XSize=54,Scr_YSize=28,UNAME='PLOTS_SYMSIZELIST')
	      self.zkompID   = Widget_combobox(bla,VALUE=strcompress(indgen(12),/rem),UVALUE=strcompress(indgen(12),/rem),Scr_XSize=46,Scr_YSize=28,UNAME='PLOTS_ZLIST')
;               rotlist        = string(indgen(8),f='(i1)')
;               rotlist        = string(indgen(20)/2.,f='(f3.1)')
              rotlist        = string(indgen(8),f='(i1)')
	      self.rotateID  = Widget_combobox(bla, Value=[rotlist],UVALUE=[rotlist],Scr_XSize=40,Scr_YSize=28,UNAME='PLOTS_ROTATELIST') 
	      maglist        = string(indgen(11)-1,f='(f3.0)')
	      self.magniID   = Widget_combobox(bla,UVALUE=maglist,VALUE=['Auto',strcompress(fix(maglist[1:*]),/rem)],Scr_XSize=40,Scr_YSize=28,UNAME='PLOTS_MAGNIFYLIST')
	      self.p0lonID   = Widget_Text(bla,Value='0',SCR_XSIZE=40,/Editable)
	      self.p0latID   = Widget_Text(bla,Value='0',SCR_XSIZE=35,/Editable)
	    label = Widget_Label(leftrow,Value='ISCCP-CT  Color Tables                    ', SCR_XSIZE=270)
	    bla = Widget_Base(leftrow, Column=2,Frame=0, Scr_XSize=270)
	      histlist = strupcase(['--','h_1d','-cot','-ctp','h_2d','low','cu','sc','st','mid','ac','as','ns','high','ci','cs','cb','max','ovw'])
	      self.histct= Widget_combobox(bla, Value=histlist,UVALUE=histlist,Scr_XSize=60,Scr_YSize=28,UNAME='PLOTS_HISTCTLIST')
	      color_table_names,color_tbl_name
	      self.ctlistID= Widget_combobox(bla, Value=[color_tbl_name],UVALUE=[color_tbl_name],Scr_XSize=205,Scr_YSize=28,UNAME='PLOTS_CTLIST')
	    label = Widget_Label(leftrow, Value='YYYY      MM      DD      HHMN    Level  ', SCR_XSIZE=270)
	    bla = Widget_Base(leftrow, Column=5,Frame=0, Scr_XSize=270)
	      yy_list        = reverse(string(indgen(38)+1978,f='(i4.4)'))
	      self.yearID    = Widget_combobox(bla,VALUE=['--',yy_list],UVALUE=['--',yy_list],Scr_XSize=58,Scr_YSize=28,UNAME='PLOTS_YEARLIST')
	      mm_list        = string(indgen(12)+1,f='(i2.2)')
	      self.monthID   = Widget_combobox(bla,VALUE=['--',mm_list],UVALUE=['--',mm_list],Scr_XSize=46,Scr_YSize=28,UNAME='PLOTS_MONTHLIST')
	      dd_list        = keyword_set(self.year) and keyword_set(self.month) ? dom(self.year,self.month) : string(indgen(31)+1,f='(i2.2)')
	      self.dayID     = Widget_combobox(bla,VALUE=['--',dd_list],UVALUE=['--',dd_list],Scr_XSize=46,Scr_YSize=28,UNAME='PLOTS_DAYLIST')
	      self.orbID     = Widget_Text(bla,Value='1',SCR_XSIZE=45,/Editable,UNAME='ORBITS_TEXT')
	      level_list     = ['L3C','L3S','L3U','L3UE','L3DM','L3PM','L3DH','L2B_SUM','L2','L1']
	      self.levelID   = Widget_combobox(bla,VALUE=['??',level_list],UVALUE=['??',level_list],Scr_XSize=60,Scr_YSize=28,UNAME='PLOTS_DAYTYPELIST')
	    bla = Widget_Base(leftrow, col=3, /Exclusive,Frame=1, XSize=270)
	      self.allpixID  = Widget_Button(bla, Value='All Surfaces  ', UVALUE='SET_PLOT_DEFAULTS')
	      self.landpixID = Widget_Button(bla, Value='Land Only ', UVALUE='SET_PLOT_DEFAULTS')
	      self.seapixID  = Widget_Button(bla, Value='Sea Only    ', UVALUE='SET_PLOT_DEFAULTS')
	    bla = Widget_Base(leftrow, row=2, /Exclusive,Frame=1, XSize=270)
	      self.globalID  = Widget_Button(bla, Value='Global', UVALUE='SET_PLOT_DEFAULTS')
	      self.arcticID  = Widget_Button(bla, Value='Arctic', UVALUE='SET_PLOT_DEFAULTS')
	      self.midlatnID = Widget_Button(bla, Value='MidLN', UVALUE='SET_PLOT_DEFAULTS')
	      self.northhmID = Widget_Button(bla, Value='NH', UVALUE='SET_PLOT_DEFAULTS')
	      self.southhmID = Widget_Button(bla, Value='SH', UVALUE='SET_PLOT_DEFAULTS')
	      self.tropicID  = Widget_Button(bla, Value='Tropic', UVALUE='SET_PLOT_DEFAULTS')
	      self.antarcID  = Widget_Button(bla, Value='AntArc', UVALUE='SET_PLOT_DEFAULTS')
	      self.midlatsID = Widget_Button(bla, Value='MidLS', UVALUE='SET_PLOT_DEFAULTS')
	      self.pm70ID    = Widget_Button(bla, Value=''+string(177b)+'60'+string(176b)+' Lat     ', UVALUE='SET_PLOT_DEFAULTS')
	    self.sat_base = Widget_Base(leftrow, Column=6, Scr_XSize=270, /Exclusive,Frame=1,sensitive=1)
; 	      self.noaa5     = Widget_Button(self.sat_base, Value='N05', UVALUE='SET_PLOT_DEFAULTS')
; 	      self.tirosn    = Widget_Button(self.sat_base, Value='T-N', UVALUE='SET_PLOT_DEFAULTS')
;  	      self.noaa6     = Widget_Button(self.sat_base, Value='N06', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa7     = Widget_Button(self.sat_base, Value='N07', UVALUE='SET_PLOT_DEFAULTS')
;  	      self.noaa8     = Widget_Button(self.sat_base, Value='N08', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa9     = Widget_Button(self.sat_base, Value='N09', UVALUE='SET_PLOT_DEFAULTS')
  	      self.noaa10    = Widget_Button(self.sat_base, Value='N10', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa11    = Widget_Button(self.sat_base, Value='N11', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa12    = Widget_Button(self.sat_base, Value='N12', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa14    = Widget_Button(self.sat_base, Value='N14', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa15    = Widget_Button(self.sat_base, Value='N15', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa16    = Widget_Button(self.sat_base, Value='N16', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa17    = Widget_Button(self.sat_base, Value='N17', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa18    = Widget_Button(self.sat_base, Value='N18', UVALUE='SET_PLOT_DEFAULTS')
	      self.noaa19    = Widget_Button(self.sat_base, Value='N19', UVALUE='SET_PLOT_DEFAULTS')
	      self.aatme     = Widget_Button(self.sat_base, Value='A/M', UVALUE='SET_PLOT_DEFAULTS')
	      self.metopa    = Widget_Button(self.sat_base, Value='MA' , UVALUE='SET_PLOT_DEFAULTS')
	      self.metopb    = Widget_Button(self.sat_base, Value='MB' , UVALUE='SET_PLOT_DEFAULTS')
	      self.noaaAM    = Widget_Button(self.sat_base, Value='AM' , UVALUE='SET_PLOT_DEFAULTS')
	      self.noaaPM    = Widget_Button(self.sat_base, Value='PM' , UVALUE='SET_PLOT_DEFAULTS')
; 	      self.msg       = Widget_Button(self.sat_base, Value='MSG', UVALUE='SET_PLOT_DEFAULTS')
	      self.ers2      = Widget_Button(self.sat_base, Value='ERS', UVALUE='SET_PLOT_DEFAULTS')
	      self.envisat   = Widget_Button(self.sat_base, Value='ENV', UVALUE='SET_PLOT_DEFAULTS')
	      self.aqua      = Widget_Button(self.sat_base, Value='MYD', UVALUE='SET_PLOT_DEFAULTS')
	      self.terra     = Widget_Button(self.sat_base, Value='MOD', UVALUE='SET_PLOT_DEFAULTS')
	      self.avhrrs    = Widget_Button(self.sat_base, Value='AVs', UVALUE='SET_PLOT_DEFAULTS')
	      self.modises   = Widget_Button(self.sat_base, Value='MOs', UVALUE='SET_PLOT_DEFAULTS')
	      self.aatsr     = Widget_Button(self.sat_base, Value='ATs', UVALUE='SET_PLOT_DEFAULTS')
	      self.allsat    = Widget_Button(self.sat_base, Value='ALL', UVALUE='SET_PLOT_DEFAULTS')
	    bla = Widget_Base(leftrow, ROW=1,Frame=0)
	      label = Widget_Label(bla, Value='Plot/Compare? - Choose Style: ')
	    bla = Widget_Base(leftrow, Column=2, Scr_XSize=270, /Exclusive,Frame=1)
	      self.pcsingle  = Widget_Button(bla, Value='Single Time Step', UVALUE='SET_PLOT_DEFAULTS')
	      self.pcmulti   = Widget_Button(bla, Value='Multi Time Steps', UVALUE='SET_PLOT_DEFAULTS')
	    bla = Widget_Base(leftrow, Column=2, Scr_XSize=270, /Exclusive,Frame=1)
	      self.pcvar     = Widget_Button(bla, Value='Map2D', UVALUE='SET_PLOT_DEFAULTS') 				; Map2D  = Variable (TS-MEAN)
	      self.pcts      = Widget_Button(bla, Value='Serie', UVALUE='SET_PLOT_DEFAULTS') 				; Serie  = Timeseries
	      self.pchist    = Widget_Button(bla, Value='Histo', UVALUE='SET_PLOT_DEFAULTS') 				; Histo  = Histogram
	      self.pczm      = Widget_Button(bla, Value='Zonal', UVALUE='SET_PLOT_DEFAULTS') 				; Zonal  = Zonal Mean
	      self.pchov     = Widget_Button(bla, Value='HovMoell', UVALUE='SET_PLOT_DEFAULTS')				; Hovm   = Hovmoeller
	      self.pcms      = Widget_Button(bla, Value='Pic-Serie (PVIR,PUG) (+Save)', UVALUE='SET_PLOT_DEFAULTS')	; TS-Multi-Sat
	      self.pcmat     = Widget_Button(bla, Value='Diff Matrix (Compare Only)' , UVALUE='SET_PLOT_DEFAULTS') 	; Matrix = Matrix (MATRIX-TS)
	      self.pcdts     = Widget_Button(bla, Value='Diff2D (Compare Only)' , UVALUE='SET_PLOT_DEFAULTS') 		; Diff   = TS-Diff
	      self.pcmts     = Widget_Button(bla, Value='Box Plots (Compare Only)' , UVALUE='SET_PLOT_DEFAULTS') 	; BoxPlot= TS-Mean 
	      self.pcmatts   = Widget_Button(bla, Value='Taylor-Diagr. (Compare Only)', UVALUE='SET_PLOT_DEFAULTS')	; Stats  = Matrix-TS
	    bla = Widget_Base(leftrow, Column=2, Scr_XSize=270,Frame=1)
; 	      self.archive  = Widget_Button(bla, Value='Archive', UVALUE='SET_PLOT_DEFAULTS')
	      bla1 = Widget_Base(bla, Column=1, Scr_XSize=52,/NonExclusive)
	         self.refself  = Widget_Button(bla1, Value='Load:', UVALUE='SET_PLOT_DEFAULTS')
	      bla2 = Widget_Base(bla, Column=1, Scr_XSize=208)
	         loaded_algo_list = [self.datum+' '+sat_name(self.algoname,self.satname)+' '+strupcase(self.level),'CLARA-A1','Coll5-AQUA','Coll5-TERRA',$
			  'CLARA-A2','Coll6-AQUA','Coll6-TERRA','CLAAS','ISCCP','ERA-INTERIM','ERA-INTERIM2','PATMOS-X',$
			  'CALIPSO','ESACCI-PT','ESACCI','CCI-GEWEX','GAC2-GEWEX','HECTOR','SELECT FILE'];'PATMOS_OLD'
	         self.lalgID   = Widget_combobox(bla2,VALUE=loaded_algo_list,UVALUE=loaded_algo_list,Scr_XSize=205,Scr_YSize=28,UNAME='PLOTS_LOAD_ALGO_LIST')
	    bla = Widget_Base(leftrow, ROW=1,Frame=0)
	      label = Widget_Label(bla, Value='Plot/Compare? - Choose Dataset: ')
	    bla = Widget_Base(leftrow, row=5, Scr_XSize=270, /Exclusive,Frame=1)
	      self.refgac    = Widget_Button(bla, Value='CLARA1'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refmyd    = Widget_Button(bla, Value='C5-AQUA'     , UVALUE='SET_PLOT_DEFAULTS')
	      self.refmod    = Widget_Button(bla, Value='C5-TERRA'    , UVALUE='SET_PLOT_DEFAULTS')
	      self.refisp    = Widget_Button(bla, Value='ISCCP'       , UVALUE='SET_PLOT_DEFAULTS')
	      self.refgac2   = Widget_Button(bla, Value='CLARA2'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refmyd2   = Widget_Button(bla, Value='C6-AQUA'     , UVALUE='SET_PLOT_DEFAULTS')
	      self.refmod2   = Widget_Button(bla, Value='C6-TERRA'    , UVALUE='SET_PLOT_DEFAULTS')
	      self.refcla    = Widget_Button(bla, Value='CLAAS'       , UVALUE='SET_PLOT_DEFAULTS')
	      self.refpmx2   = Widget_Button(bla, Value='PATMOS'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refhec    = Widget_Button(bla, Value='HECTOR'      , UVALUE='SET_PLOT_DEFAULTS')
; 	      self.refpmx    = Widget_Button(bla, Value='PATMOS_v1'   , UVALUE='SET_PLOT_DEFAULTS')
	      self.refg2gwx  = Widget_Button(bla, Value='GAC2-GWX'    , UVALUE='SET_PLOT_DEFAULTS')
	      self.refera    = Widget_Button(bla, Value='ERA-I'       , UVALUE='SET_PLOT_DEFAULTS')
	      self.refcci2   = Widget_Button(bla, Value='ESACCI'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refcci    = Widget_Button(bla, Value='CCI-v1'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refgwx    = Widget_Button(bla, Value='CCI-GWX'     , UVALUE='SET_PLOT_DEFAULTS')
	      self.refera2   = Widget_Button(bla, Value='ERA-(T1)'    , UVALUE='SET_PLOT_DEFAULTS')
	      self.refcal    = Widget_Button(bla, Value='CALIPSO'     , UVALUE='SET_PLOT_DEFAULTS')
	      self.refselect = Widget_Button(bla, Value='Select File' , UVALUE='SET_PLOT_DEFAULTS')
;  	      self.refl1gac  = Widget_Button(bla, Value='L1_GAC'      , UVALUE='SET_PLOT_DEFAULTS')
	      self.refnone   = Widget_Button(bla, Value='Loaded File' , UVALUE='SET_PLOT_DEFAULTS')
	  rightrow = Widget_Base(tlb2, ROW=3,/base_align_center)
        topright = Widget_Base(rightrow,column=5,/base_align_center)
	      bla = Widget_Base(topright,row=1,/NonExclusive,Frame=0)
	        self.saveID    = Widget_Button(bla, Value='Save Image', UVALUE='SET_PLOT_DEFAULTS')
	        self.oplotID   = Widget_Button(bla, Value='Oplot'     , UVALUE='SET_PLOT_DEFAULTS')
	        self.zoomID    = Widget_Button(bla, Value='Zoom'      , UVALUE='SET_PLOT_DEFAULTS')
	        self.errID     = Widget_Button(bla, Value='Error'     , UVALUE='SET_PLOT_DEFAULTS')
	        self.verbID    = Widget_Button(bla, Value='Verbose'   , UVALUE='SET_PLOT_DEFAULTS')
	        self.bordID    = Widget_Button(bla, Value='Borders'   , UVALUE='SET_PLOT_DEFAULTS')
	        self.invID     = Widget_Button(bla, Value='Flip Color', UVALUE='SET_PLOT_DEFAULTS')
	        self.noTitleID = Widget_Button(bla, Value='No Title'  , UVALUE='SET_PLOT_DEFAULTS')
	        self.logID     = Widget_Button(bla, Value='Log-Plot'  , UVALUE='SET_PLOT_DEFAULTS')
	      bla = Widget_Base(topright, row=1,/GRID_LAYOUT, FRAME=1, Scr_XSize=250)
	        self.showpvalID = Widget_Text(bla, Value='', SCR_XSIZE=245)
	      bla = Widget_Base(topright, row=1,/NonExclusive,Frame=0)
	        self.pixvalID  = Widget_Button(bla, Value='Show Pixel Values', UVALUE='SET_PLOT_DEFAULTS')
	        self.wbgrID    = Widget_Button(bla, Value='White-BG'  , UVALUE='SET_PLOT_DEFAULTS')
		  bla = Widget_Base(topright,row=1,Frame=0)
	        barlist = strupcase(['Color Bar','No Bar','Horiz. CB','Verti. CB','HCB Only','VCB Only'])
	        self.noBarID   = Widget_combobox(bla, Value=barlist,UVALUE=barlist,Scr_XSize=90,Scr_YSize=28,UNAME='PLOTS_BARLIST')
 	      bla = Widget_Base(topright, column=1, Scr_XSize=78,/align_right);           
	        self.selftxt   = Widget_Text(bla,Value='0',SCR_XSIZE=54,/Editable)
; 	        quot_list      = ['Axis-Qu.',string((indgen(20))/2.,f='(f3.1)')]
;                 self.axquotID  = Widget_combobox(bla,VALUE=[quot_list],UVALUE=[quot_list],Scr_XSize=20,Scr_YSize=28,UNAME='PLOTS_AXISQUOTLIST')
; 	        sym_list       = ['PSymbols',string((indgen(9)),f='(f3.1)')]
;                 self.symbolID  = Widget_combobox(bla,VALUE=[sym_list],UVALUE=[sym_list],Scr_XSize=20,Scr_YSize=28,UNAME='PLOTS_SYMBOLLIST')
      
	    self.draw = WIDGET_DRAW(rightrow, scr_XSIZE=xsize, scr_YSIZE=ysize,frame=1)

	    bottomright = Widget_Base(rightrow, col=5,/base_align_center)
	      button = Widget_Button(bottomright, Value='Plot Variable', UVALUE='PLOT_AND_STAY')
	      button = Widget_Button(bottomright, Value='Compare Variable', UVALUE='JUST_COMPARE_CCI_WITH')
	      button = Widget_Button(bottomright, Value='File Difference', UVALUE='PLOT_DIFF')
	      button = Widget_Button(bottomright, Value='Take a Snapshot', UVALUE='SNAPSHOT')
	      button = Widget_Button(bottomright, Value='Quit', UVALUE='QUIT_PLOT_VARIABLE_GUI')

; 	plot_l3

	; Get the geometries of the tree widget and the button base. These
	; will set the minimun and maximum values for resizing.
	self.georow     = Widget_Info(leftrow, /GEOMETRY)
	self.geoButtrow = Widget_Info(bottomright, /GEOMETRY)
	self.geoToprow  = Widget_Info(topright, /GEOMETRY)
	self.geodraw    = Widget_Info(self.draw, /GEOMETRY)
	self.minxs      = self.geodraw.scr_xsize
	self.minys      = self.geodraw.scr_ysize

	; If there is a tree selection, see if this corresponds to a variable in the list.
	; If so, set this variable in the combobox widget.
	theSelection = Widget_Info(self.theTree,/TREE_SELECT)
	Widget_Control, theSelection, Get_Value=varName
	index        = Where(theList EQ varName, count)
	if count eq 0 then begin
		Widget_Control, theSelection, Get_UValue=UName
		if ~is_defined(UName) then begin
			ok = dialog_message('Select a variable!')
			return
		endif
		varname = strjoin(UName,'/')
		index   = Where(theList EQ varName, count)
	endif
	if count eq 0 then begin
		ok = dialog_message('No Selection made?')
		return
	endif
;  	theData      = self -> ReadVariable((theList[index])[0], Success=success)
;	stapel (08/2014)  replaced with :
	thedata = get_data(self.year,self.month,self.day,file=self.directory+'/'+self.filename,data=(theList[index[0]])[0], $
			   algo=self.algoname,level=self.level,/keep_data_name, sat=self.satname,minvalue=minvalue,$
			   maxvalue=maxvalue,/make_compareable, var_dim_names=var_dim_names, found = success,/silent)

	if ~success then begin
		ok=dialog_message('Could not read data '+(theList[index[0]])[0] +' from file.')
		return
	endif

	; avoid converting byte into strings (ascii code!)
	if size(minvalue,/type) eq 1 then minvalue = fix(minvalue)
	if size(maxvalue,/type) eq 1 then maxvalue = fix(maxvalue)

	si = [size(thedata,/dim),1,1]
	if keyword_set(var_dim_names) and n_elements(si) ge 5 then begin
		binval = get_ncdf_data_by_name(self.directory+'/'+self.filename,var_dim_names[2])
		widget_control, self.zkompID, set_uvalue = strcompress(indgen(si[2]),/rem),set_value=strcompress(string(binval,f='(f20.2)'),/rem)
	endif else begin
		widget_control, self.zkompID, set_uvalue = strcompress(indgen(si[2]),/rem),set_value=strcompress(indgen(si[2]),/rem)
	endelse
	;symsize default
	self.PsymSize = 1.
	;magnify defaults
	self.magnify = -1
	;set defaults for comboboxes
	self.varname_plotID = varName
	; 3rd dimension
	self.dim3 = 0
	; color tables
	self.ctab = 'Default (Rainbow)'
	; projection
	self.proj = 'Default'
	; colorbar
	self.handlebar = barlist[0]
	; day,month,year
	yy_idx = where(self.year eq yy_list,yy_cnt)
; 	self.yy = ([yy_list,''])[yy_idx]; does not work with IDL versions below 8
	self.yy = yy_cnt gt 0 ? yy_list[yy_idx] : ''
	mm_idx = where(self.month eq mm_list,mm_cnt)
; 	self.mm = ([mm_list,''])[mm_idx]; does not work with IDL versions below 8
	self.mm = mm_cnt gt 0 ? mm_list[mm_idx] : ''
	dd_idx = where(self.day eq dd_list,dd_cnt)
; 	self.dd = ([dd_list,'--'])[dd_idx] ; does not work with IDL versions below 8
	self.dd = dd_cnt gt 0 ? dd_list[dd_idx] : '--'
	if yy_cnt gt 0 then yy_idx++
	if mm_cnt gt 0 then mm_idx++
	if dd_cnt gt 0 then dd_idx++
	; pmulti
	self.pmulti = '1x1'
	; hist cloud types
	self.hct=''
	; rotate
	self.rot=0
	; type of daily file
	lidx = where(strupcase(self.level) eq ['',level_list],lcnt)
	self.leveltype=(['??',level_list])[lidx]
	self.year_idx  = (yy_idx>0)
	self.month_idx = (mm_idx>0)
	self.day_idx   = (dd_idx>0)
	self.level_idx = (lidx>0)
	self.compare_algo1 = self.algoname
	IF count GT 0 THEN BEGIN
		Widget_Control, self.variablelistID, SET_COMBOBOX_SELECT=index[0]
		Widget_Control, self.ctlistID, SET_COMBOBOX_SELECT=0
		Widget_Control, self.projlist, SET_COMBOBOX_SELECT=0
		Widget_Control, self.minimumID , Set_Value=is_string(theData) ? '' : strcompress(minvalue,/rem)
		Widget_Control, self.maximumID , Set_Value=is_string(theData) ? '' : strcompress(maxvalue,/rem)
		Widget_Control, self.yearID    , SET_COMBOBOX_SELECT=yy_idx,sensitive=0
		Widget_Control, self.monthID   , SET_COMBOBOX_SELECT=mm_idx,sensitive=0
		Widget_Control, self.dayID     , SET_COMBOBOX_SELECT=dd_idx,sensitive=0
		Widget_Control, self.levelID   , SET_COMBOBOX_SELECT=lidx,sensitive=0
; 		Widget_Control, self.axquotID  , SET_COMBOBOX_SELECT=0
; 		Widget_Control, self.symbolID  , SET_COMBOBOX_SELECT=0
		Widget_Control, self.symsizeID , SET_COMBOBOX_SELECT=2,sensitive=0
		Widget_Control, self.orbID     , Set_Value=self.orbit
		Widget_Control, self.zkompID   , SET_COMBOBOX_SELECT=0
		Widget_Control, self.magniID   , SET_COMBOBOX_SELECT=0
		Widget_Control, self.rotateID  , SET_COMBOBOX_SELECT=0
		Widget_Control, self.pmultID   , SET_COMBOBOX_SELECT=0
		Widget_Control, self.winnrID   , Set_Value=''
		Widget_Control, self.histct    , SET_COMBOBOX_SELECT=0
 		Widget_Control, self.selftxt   , Set_Value=' Add Text'
		Widget_Control, self.limitID   , Set_Value=''
; 		Widget_Control, self.whatever  , Set_Value=''
		Widget_Control, self.p0lonID   , Set_Value=''
		Widget_Control, self.p0latID   , Set_Value=''
		WIDGET_CONTROL, self.showpvalID, Set_Value='             Pixel Values'
		Widget_Control, self.enablelim , Set_Button=0
		Widget_Control, self.enablemima, Set_Button=0
		Widget_Control, self.allpixID  , Set_Button=1
		Widget_Control, self.landpixID , Set_Button=0
		Widget_Control, self.seapixID  , Set_Button=0
		Widget_Control, self.globalID  , Set_Button=1
		Widget_Control, self.northhmID , Set_Button=0
		Widget_Control, self.southhmID , Set_Button=0
		Widget_Control, self.antarcID  , Set_Button=0
		Widget_Control, self.midlatsID , Set_Button=0
		Widget_Control, self.tropicID  , Set_Button=0
		Widget_Control, self.midlatnID , Set_Button=0
		Widget_Control, self.arcticID  , Set_Button=0
		Widget_Control, self.pm70ID    , Set_Button=0
		Widget_Control, self.pixvalID  , Set_Button=0
		Widget_Control, self.invID     , Set_Button=0
		Widget_Control, self.verbID    , Set_Button=0
		Widget_Control, self.bordID    , Set_Button=0
		Widget_Control, self.saveID    , Set_Button=0
		Widget_Control, self.zoomID    , Set_Button=0
		Widget_Control, self.oplotID   , Set_Button=0
		Widget_Control, self.errID     , Set_Button=0
; 		Widget_Control, self.noBarID   , Set_Button=0
		Widget_Control, self.noBarID   , SET_COMBOBOX_SELECT=0
		Widget_Control, self.noTitleID , Set_Button=0
;  		Widget_Control, self.noaa5     , Set_Button=(self.satname eq 'noaa5'  ? 1:0)
; 		Widget_Control, self.tirosn    , Set_Button=(self.satname eq 'tirosn' ? 1:0)
;  		Widget_Control, self.noaa6     , Set_Button=(self.satname eq 'noaa6'  ? 1:0)
		Widget_Control, self.noaa7     , Set_Button=(self.satname eq 'noaa7'  ? 1:0)
; 		Widget_Control, self.noaa8     , Set_Button=(self.satname eq 'noaa8'  ? 1:0)
		Widget_Control, self.noaa9     , Set_Button=(self.satname eq 'noaa9'  ? 1:0)
 		Widget_Control, self.noaa10    , Set_Button=(self.satname eq 'noaa10' ? 1:0)
		Widget_Control, self.noaa11    , Set_Button=(self.satname eq 'noaa11' ? 1:0)
		Widget_Control, self.noaa12    , Set_Button=(self.satname eq 'noaa12' ? 1:0)
		Widget_Control, self.noaa15    , Set_Button=(self.satname eq 'noaa15' ? 1:0)
		Widget_Control, self.noaa14    , Set_Button=(self.satname eq 'noaa14' ? 1:0)
		Widget_Control, self.noaa16    , Set_Button=(self.satname eq 'noaa16' ? 1:0)
		Widget_Control, self.noaa17    , Set_Button=(self.satname eq 'noaa17' ? 1:0)
		Widget_Control, self.noaa18    , Set_Button=(self.satname eq 'noaa18' ? 1:0)
		Widget_Control, self.noaa19    , Set_Button=(self.satname eq 'noaa19' ? 1:0)
		Widget_Control, self.metopa    , Set_Button=(self.satname eq 'metopa' ? 1:0)
 		Widget_Control, self.metopb    , Set_Button=(self.satname eq 'metopb' ? 1:0)
; 		Widget_Control, self.msg       , Set_Button=(self.satname eq 'msg'    ? 1:0)
		Widget_Control, self.aqua      , Set_Button=(self.satname eq 'aqua'   ? 1:0)
		Widget_Control, self.envisat   , Set_Button=(self.satname eq 'envisat'? 1:0)
		Widget_Control, self.ers2      , Set_Button=(self.satname eq 'ers2'   ? 1:0)
		Widget_Control, self.aatme     , Set_Button=(self.satname eq 'aatme'  ? 1:0)
		Widget_Control, self.aatsr     , Set_Button=(self.satname eq 'atsrs'  ? 1:0)
		Widget_Control, self.terra     , Set_Button=(self.satname eq 'terra'  ? 1:0)
		Widget_Control, self.avhrrs    , Set_Button=(self.satname eq 'avhrrs' ? 1:0)
		Widget_Control, self.modises   , Set_Button=(self.satname eq 'modises'? 1:0)
		Widget_Control, self.noaaAM    , Set_Button=(self.satname eq 'noaaam' ? 1:0)
		Widget_Control, self.noaaPM    , Set_Button=(self.satname eq 'noaapm' ? 1:0)
		Widget_Control, self.allsat    , Set_Button=(self.satname eq 'allsat' ? 1:0)
		Widget_Control, self.lalgID    , SET_COMBOBOX_SELECT=0,sensitive=0
		Widget_Control, self.refself   , Set_Button=1
		Widget_Control, self.refselect , Set_Button=0
		Widget_Control, self.refnone   , Set_Button=1
		Widget_Control, self.refgac    , Set_Button=0
		Widget_Control, self.refmyd    , Set_Button=0
		Widget_Control, self.refmod    , Set_Button=0
		Widget_Control, self.refgac2   , Set_Button=0
		Widget_Control, self.refmyd2   , Set_Button=0
		Widget_Control, self.refmod2   , Set_Button=0
  		Widget_Control, self.refcal    , Set_Button=0
   		Widget_Control, self.refgwx    , Set_Button=0
		Widget_Control, self.refisp    , Set_Button=0
		Widget_Control, self.refcci2   , Set_Button=0
		Widget_Control, self.refcci    , Set_Button=0
; 		Widget_Control, self.refpmx    , Set_Button=0
		Widget_Control, self.refg2gwx  , Set_Button=0
		Widget_Control, self.refpmx2   , Set_Button=0
; 		Widget_Control, self.refl1gac  , Set_Button=0
		Widget_Control, self.refcla    , Set_Button=0
		Widget_Control, self.refhec    , Set_Button=0
		Widget_Control, self.refera    , Set_Button=0
		Widget_Control, self.refera2   , Set_Button=0
; 		Widget_Control, self.loaded    , Set_Button=1
		Widget_Control, self.pcsingle  , Set_Button=1
		Widget_Control, self.pcmulti   , Set_Button=0
		Widget_Control, self.pcvar     , Set_Button=1
		Widget_Control, self.pcmat     , Set_Button=0
		Widget_Control, self.pcts      , Set_Button=0
		Widget_Control, self.pchist    , Set_Button=0
		Widget_Control, self.pczm      , Set_Button=0
		Widget_Control, self.pcdts     , Set_Button=0
		Widget_Control, self.pcmts     , Set_Button=0
		Widget_Control, self.pcmatts   , Set_Button=0
		Widget_Control, self.pcms      , Set_Button=0
		Widget_Control, self.pchov     , Set_Button=0
		;--
	ENDIF
	; Get it going...
	Widget_Control, tlb2, /REALIZE
	;stapel Set the draw widget as the current drawable area.
	WIDGET_CONTROL, self.draw, GET_VALUE=drawID
	self.drawID=drawID

	!p.multi = 0

	XMANAGER, 'PlotVariableFromGUI', tlb2, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------


FUNCTION NCDF_DATA::ReadVarAttr, theVariableName, theAttributeName, silent = silent

; This method reads and returns a particular variable attribute.
; Both the name of the variable and the name of the attribute are
; required parameters.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, ""
   ENDIF

   IF N_Params() NE 2 THEN Message, 'Both the variable name and the attribute name must be present.'

   ; The file has to be parsed to carry this out.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   ; Get the variable list.
   theVarStructures = *self.theVariables

   ; Find this variable in the variable structures.
   index = Where(StrUpCase(theVarStructures.name) EQ StrUpCase(theVariableName), count)
   IF count EQ 0 THEN Message, 'Cannot find the variable ' + theVariableName + ' in the file.'
   thisVariableStruct = theVarStructures[index]
   
   ; Get the pointer to the variable attribute structures.
   varAttrStructures = *thisVariableStruct.var_attributes
   
   ; Find the name of the attribute in the varAttrStructures list
   index = Where(StrUpCase(varAttrStructures.name) EQ StrUpCase(theAttributeName), count)
   IF count EQ 0 THEN Message, 'Cannot find the attribute ' + theAttributeName + ' in the file.'
   
   ; Extract the attribute names.
   theAttributeStruct = varAttrStructures[index]
   theAttributeValue = *theAttributeStruct.value
   
   RETURN, theAttributeValue
    
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + IDL_ValidName(varName[0], /CONVERT_ALL)   : IDL_ValidName(varName[0], /CONVERT_ALL)
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'

               ; Which variable do you want to read?
;                Widget_Control, self.variableListID, Get_UValue=theList
               Widget_Control, self.variableListID, Get_Value=theList
               index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
               theVariable = theList[index]
               Widget_Control, /HOURGLASS
;                theData = self -> ReadVariable(theVariable, Success=success)
               ; sometimes a bit slower but sets fillvalues , makes scaling etc,  
               thedata = get_data(file=self.directory+'/'+self.filename,algo=self.algoname,data=varName,found = success,/keep_data_name)
;                read_data,self.directory+'/'+self.filename,theVariable,thedata,found = success
               IF success EQ 0 THEN begin
			Print, 'A variable named "' + varName + '" not found or not of type dataset.'
			RETURN
	       endif

               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varName + '" has been created at the main IDL level.'

               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.variableListID, SET_DROPLIST_SELECT=index
;                Widget_Control, self.varnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
               Widget_Control, self.varnameID, Set_Value=theList[index]
               END

            'READ_AND_LEAVE': BEGIN

               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
                  Widget_Control, self.varNameID, Get_Value=varName
                  varNamen = (addName) ? FSC_Base_FileName(self.filename) + '_' + IDL_ValidName(varName[0], /CONVERT_ALL)   : IDL_ValidName(varName[0], /CONVERT_ALL)
               IF varNamen EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               ; Which variable do you want to read?
;                Widget_Control, self.variableListID, Get_UValue=theList
;                index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
;                theVariable = theList[index]
               Widget_Control, /HOURGLASS
;               theData = self -> ReadVariable(varName, Success=success)
               ; sometimes a bit slower but sets fillvalues , makes scaling etc,  
               thedata = get_data(file=self.directory+'/'+self.filename,algo=self.algoname,data=varName,found = success,/keep_data_name)
;                read_data,self.directory+'/'+self.filename,varName,thedata,found = success
               IF success EQ 0 THEN begin
			Widget_Control, event.top, /DESTROY
			Print, 'A variable named "' + varName + '" not found or not of type dataset.'
			RETURN
	       endif
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varNamen, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varNamen + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END

            'QUIT_READ_VARIABLE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
;          Widget_Control, event.id, Get_UVALUE=list
;          Widget_Control, self.varNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         Widget_Control, event.id, Get_VALUE=list
         Widget_Control, self.varNameID, Set_Value=list[event.index]
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't care what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------

; test
PRO NCDF_DATA::	get_info_from_plot_interface											, $
		varName,mini=mini,maxi=maxi,opl,hct,oth,ctab,show_values,verbose,all,sea,land,ant,mls,tro,mln,arc,pm7,glo,nhm,shm, $
		save_as,error,zoom,gac,modi,myd,gac2,modi2,myd2,syn,ccigwx,isp,cci,cci2,era,era2,g2gwx,pmx2,l1g,cla,hec,sel,pcms,win_nr,year,month,day, $
		orbit,pcsing,pcmult,pcvar,pcmat,pcts,pchist,pczm,pcdts,pcmts,pcmatts,pchov,pmulti,load,select,none,sat=sat		, $
		limit=limit,globe=globe,p0lat=p0lat,p0lon=p0lon,mollweide=mollweide,aitoff=aitoff,hammer=hammer,goode=goode	, $
		sinusoidal=sinusoidal,robinson=robinson,cov=cov,nobar=nobar,stereographic=stereographic,msg=msg,log=log		, $
		dim3=dim3,rot=rot,addtext=addtext,found=found,magnify=magnify,countries=countries,symsize=symsize,notitle=notitle

	found=1
; 	varName=self.varname_plotID
	varName = Widget_Info(self.variableListID, /combobox_gettext)
; self.varname_plotID=varName


	IF varName EQ "" THEN begin
		ok = dialog_message( 'Must have a non-null variable name to create a variable.')
		found=0
		return
	endif
	widget_control,self.minimumID,get_value=mini
	widget_control,self.p0lonID,get_value=p0lon
	widget_control,self.p0latID,get_value=p0lat
	widget_control,self.maximumID,get_value=maxi
; 	widget_control,self.pmultID,get_value=pmulti
	widget_control,self.winnrID,get_value=win_nr
	widget_control,self.limitID,get_value=dumlimit
	widget_control,self.histct,get_value=hct
; 	widget_control,self.other,get_value=oth
; 	oth = strlowcase(oth[0])

	widget_control,self.selftxt,get_value=addtext
 	addtext = addtext eq ' Add Text' ? '' : addtext
; 	addtext = '' ; this is a dummy remove this if you want to reinstate the addtext 

	hct = self.hct eq '--' ? '' : strlowcase(self.hct)
	if hct eq 'ovw' then hct = 'overview'
	if hct eq 'h_2d' then hct = 'hist2d'
	if hct eq 'h_1d' then hct = '1d'
	if hct eq '-cot' then hct = '1d_cot'
	if hct eq '-ctp' then hct = '1d_ctp'
	rot = self.rot

	pmulti=strjoin(['0',strsplit(self.pmulti,'x',/ext)],',')
	ctab = strcompress((strsplit(self.ctab,/ext,':'))[0],/rem)
	inv  = Widget_Info(self.invID, /BUTTON_SET)
	if ~is_number(ctab) then begin
		case ctab of 
			'Default(Rainbow)'	: oth = 'rainbow'
			'ExtendedRainbow'	: oth = 'extended_rainbow'
			'Elevation'		: oth = 'elevation'
			'BluetoRed'		: oth = 'bwr'
			'Greyscale'		: oth = 'greyscale'
			'GISTEarth'		: oth = 'gistearth'
			'GMTGlobe'		: oth = 'gmtglobe'
			'GMTRelief'		: oth = 'gmtrelief'
			'GMTSplit'		: oth = 'gmtsplit'
			'NYTDrought'		: oth = 'nytdrought'
			'UKMHadcrut'		: oth = 'ukmhadcrut'
			'TempAnomaly'		: oth = 'tempanomaly'
			else			: oth = 'rainbow'
		endcase
		if inv then oth = 'flip_'+oth
		ctab = ''
	endif else begin
		oth = ''
		if strcompress((strsplit(self.ctab,/ext,':'))[1],/rem) eq 'Brewer' then oth = 'brewer'
		if inv then ctab = strcompress(fix(ctab) *(-1),/rem)
	endelse

; 	widget_control,self.yearID,get_value=year
; 	widget_control,self.monthID,get_value=month
; 	widget_control,self.dayID,get_value=day
	year=self.yy
	month=self.mm
	day=self.dd
	if year  eq '--' then year  = ''
	if month eq '--' then month = ''
	if day   eq '--' then day   = ''
	dim3 = self.dim3
	magnify = self.magnify
	symsize = self.PsymSize
	dtyp=self.leveltype

	widget_control,self.orbID,get_value=orbit

	show_values = Widget_Info(self.pixvalID, /BUTTON_SET)
	verbose     = Widget_Info(self.verbID, /BUTTON_SET)
	countries   = Widget_Info(self.bordID, /BUTTON_SET)
	save_as     = Widget_Info(self.saveID, /BUTTON_SET)
	zoom        = Widget_Info(self.zoomID, /BUTTON_SET)
	error       = Widget_Info(self.errID, /BUTTON_SET)
	notitle     = Widget_Info(self.noTitleID, /BUTTON_SET)
	log         = Widget_Info(self.logID, /BUTTON_SET)

	case strlowcase(self.handlebar) of
		'color bar'	: nobar = 0
		'no bar'	: nobar = 1
		'horiz. cb'	: nobar = 2
		'verti. cb'	: nobar = 3
		'hcb only'	: nobar = 4
		'vcb only'	: nobar = 5
		else		: nobar = 0
	endcase

	if Widget_Info(self.oplotID, /BUTTON_SET) then begin
		self.oplotnr++
	endif else begin
		self.oplotnr = 0
	endelse
	opl 	= self.oplotnr

	all         = Widget_Info(self.allpixID, /BUTTON_SET)
	sea         = Widget_Info(self.seapixID, /BUTTON_SET)
	land        = Widget_Info(self.landpixID, /BUTTON_SET)
	ant         = Widget_Info(self.antarcID, /BUTTON_SET)
	mls         = Widget_Info(self.midlatsID, /BUTTON_SET)
	tro         = Widget_Info(self.tropicID, /BUTTON_SET)
	mln         = Widget_Info(self.midlatnID, /BUTTON_SET)
	arc         = Widget_Info(self.arcticID, /BUTTON_SET)
	pm7         = Widget_Info(self.pm70ID, /BUTTON_SET)
	glo         = Widget_Info(self.globalID, /BUTTON_SET)
	nhm         = Widget_Info(self.northhmID, /BUTTON_SET)
	shm         = Widget_Info(self.southhmID, /BUTTON_SET)

; 	load        = Widget_Info(self.loaded, /BUTTON_SET)
	gac         = Widget_Info(self.refgac, /BUTTON_SET)
	modi        = Widget_Info(self.refmod, /BUTTON_SET)
	myd         = Widget_Info(self.refmyd, /BUTTON_SET)
	gac2        = Widget_Info(self.refgac2, /BUTTON_SET)
	modi2       = Widget_Info(self.refmod2, /BUTTON_SET)
	myd2        = Widget_Info(self.refmyd2, /BUTTON_SET)
	syn         = Widget_Info(self.refcal, /BUTTON_SET)
 	ccigwx      = Widget_Info(self.refgwx, /BUTTON_SET)
	isp         = Widget_Info(self.refisp, /BUTTON_SET)
	cci2        = Widget_Info(self.refcci2, /BUTTON_SET)
	cci         = Widget_Info(self.refcci, /BUTTON_SET) ; prototype
	era         = Widget_Info(self.refera, /BUTTON_SET)
	era2        = Widget_Info(self.refera2, /BUTTON_SET)
	g2gwx		= Widget_Info(self.refg2gwx, /BUTTON_SET)
; 	pmx         = Widget_Info(self.refpmx, /BUTTON_SET)
	pmx2        = Widget_Info(self.refpmx2, /BUTTON_SET)
; 	l1g         = Widget_Info(self.refl1gac, /BUTTON_SET)
l1g = 0
	cla         = Widget_Info(self.refcla, /BUTTON_SET)
	hec         = Widget_Info(self.refhec, /BUTTON_SET)
	sel         = Widget_Info(self.refself, /BUTTON_SET)
	select      = Widget_Info(self.refselect, /BUTTON_SET)
	none        = widget_Info(self.refnone,/BUTTON_SET)

	;set background color
	if Widget_Info(self.wbgrID, /BUTTON_SET) then begin
		!P.Color = cgcolor('Black')
		!P.Background = cgcolor('White')
	endif else begin
		!P.Color = cgcolor('White')
		!P.Background = cgcolor('Black')
	endelse
	; Set the draw widget as the current drawable area.
	if strlowcase(win_nr[0]) eq '' then begin
		WSET, self.drawID
		win_nr = -1
	endif else win_nr = fix(win_nr[0])

	pcsing = Widget_Info(self.pcsingle,/BUTTON_SET)
	pcmult = Widget_Info(self.pcmulti,/BUTTON_SET)
	pcvar  = Widget_Info(self.pcvar,/BUTTON_SET)
	pcmat  = Widget_Info(self.pcmat,/BUTTON_SET)
	pcts   = Widget_Info(self.pcts,/BUTTON_SET)
	pchist = Widget_Info(self.pchist,/BUTTON_SET)
	pczm   = Widget_Info(self.pczm,/BUTTON_SET)
	pcdts  = Widget_Info(self.pcdts,/BUTTON_SET)
	pcmts  = Widget_Info(self.pcmts,/BUTTON_SET)
	pcmatts= Widget_Info(self.pcmatts,/BUTTON_SET)
	pcms   = Widget_Info(self.pcms,/BUTTON_SET)
	pchov  = Widget_Info(self.pchov,/BUTTON_SET)

	limit_enabled = ( Widget_Info(self.enablelim,/BUTTON_SET) )

	if limit_enabled then begin
		if dumlimit[0] eq '' then dumlimit = '[-90,-180,90,180]'
		dum = float(strsplit(strcompress(dumlimit,/rem),'],[()',/ext))
		if n_elements(dum) eq 4 then limit = dum
	endif

	; limit will be overwritten by defaults (if set!) 
	if ant then begin
		if pczm or pchist then limit = [-90.0,-180,-60.0,180] else begin
			globe = 1
			p0lat = -90
		endelse
	end
	if mls then limit = [-60.0,-180,-30.0,180]
	if tro then limit = [-30.0,-180, 30.0,180]
	if mln then limit = [ 30.0,-180, 60.0,180]
	if arc then begin
 		if pczm or pchist then limit = [ 60.0,-180, 90.0,180] else begin
			globe = 1 
			p0lat = 90
		endelse
	end
	if pm7 then limit = [-60.0,-180, 60.0,180]
	if nhm then limit = [  0.0,-180, 90.0,180]
	if shm then limit = [-90.0,-180,  0.0,180]

	if keyword_set(p0lon) and keyword_set(limit) then limit = limit+[0,p0lon,0,p0lon]
	
	case strlowcase(self.proj) of 
 		'stereo'	: stereographic = 1
		'mollweide'	: mollweide = 1
		'aitoff'	: aitoff    = 1
		'hammer'	: hammer    = 1
		'goode'		: goode     = 1
		'sinusoidal'	: sinusoidal= 1
		'robinson'	: robinson  = 1
		'globe (ortho)'	: globe     = 1
		'satellite'	: msg       = 1
; 		'msg'		: msg       = 1
		else		: 
	endcase

	; map_image crash
	if (keyword_set(robinson) or keyword_set(mollweide) or keyword_set(goode) or keyword_set(sinusoidal)) and ~ant and ~arc then begin
		if p0lat[0] ne '' then print,'Setting P0lat different to Zero, causes Map_image to crash! Use "Default","Globe","Aitoff","Hammer" or "Stereo" instead!'
		p0lat=['']
	endif
	setlist = Widget_Info([	self.noaa7,	$
; 				self.tirosn,	$
; 				self.noaa5,	$
; 				self.noaa6,	$
; 				self.noaa8,	$
				self.noaa9,	$
 				self.noaa10,	$
				self.noaa11,	$
				self.noaa12,	$
				self.noaa14,	$
				self.noaa15,	$
				self.noaa16,	$
				self.noaa17,	$
				self.noaa18,	$
				self.noaa19,	$
				self.metopa,	$
				self.metopb,	$
; 				self.msg,	$
				self.aqua,	$
				self.terra,	$
				self.avhrrs,	$
				self.modises,	$
				self.allsat,	$
				self.ers2,	$
				self.envisat,	$
				self.aatme,	$
				self.aatsr,	$
				self.noaaam,	$
				self.noaapm	$
				],/button_set)
; 	satlist = ['noaa'+strcompress([7,5,6,8,9,10,11,12,14,15,16,17,18,19],/rem),$
; 		   'metopa','metopb','msg','aqua','terra','avhrrs','modises','allsat','aatsr','aatme','noaaam','noaapm']

	satlist = ['noaa'+strcompress([7,9,10,11,12,14,15,16,17,18,19],/rem),'metopa','metopb','aqua','terra','avhrrs',$
		   'modises','allsat','ers2','envisat','aatme','atsrs','noaaam','noaapm']
	blabal = where(setlist eq 1,bla_cnt)
	sat = bla_cnt gt 0 ? (satlist[blabal])[0] : self.satname

	if total([ant,mls,tro,mln,arc,pm7,shm,nhm]) then begin
		cov = ['antarctica','midlat_south','tropic','midlat_north','arctic','midlat_trop','southern_hemisphere','northern_hemisphere']
		cov = cov[where([ant,mls,tro,mln,arc,pm7,shm,nhm] eq 1)]
		if land then cov = cov+'_land'
		if sea  then cov = cov+'_sea'
	endif else begin
		cov = ''
		if land then cov = 'land'
		if sea  then cov = 'sea'
	endelse

	if not keyword_set(maxi) then free,maxi
	if not keyword_set(mini) then free,mini

end

function ncdf_data::get_new_filename, sat, year, month, day, orbit, algo, varname, level = level, dirname = dirname, gewex_style=gewex_style, found = found

	scol = total(Widget_Info([self.avhrrs,self.modises,self.allsat],/button_set))
	if strlowcase(self.leveltype) eq 'l3s' and ~scol then begin
		ok = dialog_message('For Level L3S data, press either "AVHRRs", "MODISs" or "ALLSAT"')
		found=0
		return,-1
	endif
	self -> setlevel
	level = strlowcase(self.leveltype)

	;check for patmos l3u node
	if algo eq 'PATMOS' and level eq 'l3u' then begin
		last = strlowcase(strmid((reverse(strsplit(varname,'_',/ext)))[0],0,3))
		if last eq 'asc' then node = 'asc'  else $
		if last eq 'des' then node = 'desc' else $
		node = self.satnode
	endif

	; das alles hier muss noch getestet werden!!
	set_dummy = total(strlowcase(varname) eq ['blue_marble','usgs_lus','usgs_dem','usgs_ls','usgs_lsm',$
						  'refl1','refl2','refl3a','rad3b','rad4','rad5'])
	if keyword_set(set_dummy) then begin
		found=1
		return,self.directory+'/'+self.filename
	endif

; 	check   = (( Widget_Info(self.refself, /BUTTON_SET)))
	if keyword_set(check) then begin & dirname = self.directory & filename = self.filename & version = self.version & end
	file =  get_filename(year, month, day, data=varname, sat=sat, algo = algo, level=level, found = found, $
	orbit = orbit[0], silent= check, dirname = dirname, filename = filename, version = version, node=node, gewex_style=gewex_style)

	if found eq 0. and keyword_set(check) then begin
		if (sat ne self.satname or year ne self.year or month ne self.month or day ne self.day or orbit ne self.orbit or level ne self.level) then begin
			file =  get_filename(year, month, day, data=varname, sat=sat, algo = algo, level=level, found = found, $
			orbit = orbit[0], dirname = self.directory, filename = self.filename, version = self.version, node=node)
		endif else begin
			file = self.directory+'/'+self.filename
			found = 1.
		endelse
	endif

	; return only one file even if more have been found
	if n_elements(file) gt 1 then begin
		print,'Found more than 1 file! I will take the first one only!'
		file = file[0]
		found = 1.
	endif

	return, file
end

PRO NCDF_DATA::PlotVariableFromGUI_Events, event

  ; This internal method processes events from the user dialogs.

  ; Error handling
  CATCH, theError
  IF theError NE 0 THEN BEGIN
     CATCH, /CANCEL
     void = Error_Message()
     RETURN
  ENDIF
   
  thisEvent = Tag_Names(event, /STRUCTURE_NAME)

  CASE thisEvent OF

    'WIDGET_BUTTON': BEGIN
	Widget_Control, event.id, Get_UValue=buttonValue

	CASE buttonValue OF

		'HELP'	: begin
			xhelp,'/cmsaf/nfshome/sstapelb/idl/xless.pro',/unmanaged
		end
		'SET_PLOT_DEFAULTS'	: begin
			sel    = Widget_Info(self.refself,/BUTTON_SET)
			if sel then begin
				Widget_Control, self.lalgID,SET_COMBOBOX_SELECT=0,sensitive=0
				self.compare_algo1 = self.algoname
			endif else Widget_Control, self.lalgID,sensitive=1
			none   = widget_Info(self.refnone,/BUTTON_SET)
			pcmult = Widget_Info(self.pcmulti,/BUTTON_SET)
			hide   = ((sel+none) eq 2 and ~pcmult);or pcmult)
			Widget_Control, self.SymSizeID,sensitive=pcmult 
			if hide then begin
				Widget_Control, self.yearID  ,SET_COMBOBOX_SELECT=self.year_idx,sensitive=( hide ? 0:1 )
				Widget_Control, self.monthID ,SET_COMBOBOX_SELECT=self.month_idx,sensitive=( hide ? 0:1 )
				Widget_Control, self.dayID   ,SET_COMBOBOX_SELECT=self.day_idx,sensitive=( hide ? 0:1 )
				Widget_Control, self.orbID   ,Set_Value=self.orbit
				Widget_Control, self.levelID ,SET_COMBOBOX_SELECT=self.level_idx,sensitive=( hide ? 0:1 )
				self.yy = ( self.year  eq '' ? '--' : self.year )
				self.mm = ( self.month eq '' ? '--' : self.month )
				self.dd = ( self.day   eq '' ? '--' : self.day )

; 				Widget_Control, self.noaa5     , Set_Button=(self.satname eq 'noaa5'  ? 1:0)
;				Widget_Control, self.tirosn    , Set_Button=(self.satname eq 'tirosn' ? 1:0)
; 				Widget_Control, self.noaa6     , Set_Button=(self.satname eq 'noaa6'  ? 1:0)
				Widget_Control, self.noaa7     , Set_Button=(self.satname eq 'noaa7'  ? 1:0)
; 				Widget_Control, self.noaa8     , Set_Button=(self.satname eq 'noaa8'  ? 1:0)
				Widget_Control, self.noaa9     , Set_Button=(self.satname eq 'noaa9'  ? 1:0)
				Widget_Control, self.noaa10    , Set_Button=(self.satname eq 'noaa10' ? 1:0)
				Widget_Control, self.noaa11    , Set_Button=(self.satname eq 'noaa11' ? 1:0)
				Widget_Control, self.noaa12    , Set_Button=(self.satname eq 'noaa12' ? 1:0)
				Widget_Control, self.noaa15    , Set_Button=(self.satname eq 'noaa15' ? 1:0)
				Widget_Control, self.noaa14    , Set_Button=(self.satname eq 'noaa14' ? 1:0)
				Widget_Control, self.noaa16    , Set_Button=(self.satname eq 'noaa16' ? 1:0)
				Widget_Control, self.noaa17    , Set_Button=(self.satname eq 'noaa17' ? 1:0)
				Widget_Control, self.noaa18    , Set_Button=(self.satname eq 'noaa18' ? 1:0)
				Widget_Control, self.noaa19    , Set_Button=(self.satname eq 'noaa19' ? 1:0)
				Widget_Control, self.metopa    , Set_Button=(self.satname eq 'metopa' ? 1:0)
				Widget_Control, self.metopb    , Set_Button=(self.satname eq 'metopb' ? 1:0)
; 				Widget_Control, self.msg       , Set_Button=(self.satname eq 'msg'    ? 1:0)
				Widget_Control, self.ers2      , Set_Button=(self.satname eq 'ers2'   ? 1:0)
				Widget_Control, self.aqua      , Set_Button=(self.satname eq 'aqua'   ? 1:0)
				Widget_Control, self.envisat   , Set_Button=(self.satname eq 'envisat'? 1:0)
				Widget_Control, self.aatme     , Set_Button=(self.satname eq 'aatme'  ? 1:0)
				Widget_Control, self.aatsr     , Set_Button=(self.satname eq 'atsrs'  ? 1:0)
				Widget_Control, self.terra     , Set_Button=(self.satname eq 'terra'  ? 1:0)
				Widget_Control, self.avhrrs    , Set_Button=(self.satname eq 'avhrrs' ? 1:0)
				Widget_Control, self.modises   , Set_Button=(self.satname eq 'modises'? 1:0)
				Widget_Control, self.allsat    , Set_Button=(self.satname eq 'allsat' ? 1:0)
				Widget_Control, self.noaaAM    , Set_Button=(self.satname eq 'noaaam' ? 1:0)
				Widget_Control, self.noaaPM    , Set_Button=(self.satname eq 'noaapm' ? 1:0)
			endif else begin
				Widget_Control, self.yearID  ,sensitive=( hide ? 0:1 )
				Widget_Control, self.monthID ,sensitive=( hide ? 0:1 )
				Widget_Control, self.dayID   ,sensitive=( hide ? 0:1 )
				Widget_Control, self.levelID ,sensitive=( hide ? 0:1 )
				self -> setlevel
			endelse
		end
		'SNAPSHOT'	: begin
			save_as = !SAVE_DIR + 'snapshots/'
			if ~file_test( !SAVE_DIR ,/write) then begin
				ok = dialog_message('The Standard Save Directory '+ !SAVE_DIR +' does not exists or is not writeable! First edit !SAVE_DIR in vali_set_path!')
				return
			endif
			if ~file_test(save_as) then file_mkdir, save_as
			filename = Dialog_Pickfile(/write, File=filename,/overwrite_prompt,path=save_as, $
				   filter='*.png,*.bmp,*.gif,*.jpg,*.pict,*.tiff')
			image    = cgSnapshot(Filename=filename,/nodialog)
                        print,'Image saved as: ',filename
		end
		'JUST_COMPARE_CCI_WITH'	: begin

			Widget_Control, /HOURGLASS

			self -> get_info_from_plot_interface,varName,mini=mini,maxi=maxi,opl,hct,oth,ctab,show_values,verbose,all,sea,land,ant,magnify=magnify, $
				mls,tro,mln,arc,pm7,glo,nhm,shm,save_as,error,zoom,gac,modi,myd,gac2,modi2,myd2,syn,ccigwx,isp,cci,cci2,era,era2,g2gwx,pmx2,l1g,cla,hec,sel,pcms,win_nr,year, $
				month,day,orbit,pcsing,pcmult,pcvar,pcmat,pcts,pchist,pczm,pcdts,pcmts,pcmatts,pchov,pmulti,load,select,none,sat=sat,limit=limit, $
				globe=globe,p0lat=p0lat,p0lon=p0lon,mollweide=mollweide,aitoff=aitoff,hammer=hammer,goode=goode,cov=cov	,found=found, $
				sinusoidal=sinusoidal,robinson=robinson,nobar=nobar,stereographic=stereographic,msg=msg,log=log,dim3=dim3,rot=rot,addtext=addtext,$
				countries=countries,symsize=symsize,notitle=notitle

			if ~found then return

			if none then begin
				ok = dialog_message('Choose Reference Dataset!')
				return
			endif
			if (pcsing and pchov) then begin
				ok = dialog_message('Use "Multi Time Steps" for Hovmoeller plots!')
				return
			endif

			if select then ref = 'select'

			if pcmat and (total(self.leveltype eq ['L2','L3U','L1','L3DH','L2B_SUM'])) then begin 
				ok = dialog_message('This works with Monthly Means only!')
				return
			endif

			algo = self.compare_algo1

			names    = strsplit(varname,',',/ext)
			if n_elements(names) gt 1 then begin
				varname  = names[0]
				varname2 = names[1]
			endif else varname2 = varname

			if modi   then ref = 'mod'
			if modi2  then ref = 'mod2'
			if myd    then ref = 'myd'
			if myd2   then ref = 'myd2'
			if era    then ref = 'era'
			if era2   then ref = 'era2'
			if gac    then ref = 'gac'
			if gac2   then ref = 'gac2'
; 			if pmx    then ref = 'pmx_old'
			if pmx2   then ref = 'pmx'
			if l1g    then ref = 'l1gac'
			if cla    then ref = 'cla'
			if cci    then ref = 'cci_old'
			if cci2   then ref = 'cci'
			if ccigwx then ref = 'gwx'
			if g2gwx  then ref = 'g2_gwx'
			if syn    then ref = 'cal'
			if isp    then ref = 'isp'
			if hec    then ref = 'hec'

			;set System Variables
			plot_l3, save_as = save_as, white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),reference = ref

			if pcmult then begin
				file  = ''
				found = 1
				datum = ''
			endif else if algo eq 'SELECT FILE' then begin
				self.file2 = dialog_pickfile(path=(self.file2 eq '' ? self.directory:file_dirname(self.file2)),$
							; following needs to be done for filenames that include white spaces
							file =	strmid(strjoin('\ '+strsplit(self.file2 eq '' ? self.filename:file_basename(self.file2),/ext)),2),$
							filter=self.extension)
				file = 	self.file2
				if file eq '' then return
				if ~is_hdf(file) then begin
					if ~is_ncdf(file) then begin
						ok = dialog_message('The file '+file+' is neither a netcdf nor a hdf file. '+$
								'Note netcdf4 files are not supported in IDL versions below 8!')
						return
					endif
				endif
				ok     = self -> get_file_infos(infile=file)
				algo   = ok.algoname eq '' ? self.algoname : ok.algoname
				satn   = ok.satname  eq '' ? '' : ok.satname
				datum  = ok.datum eq '' ? strjoin([year,month,day,orbit]) : ok.datum
			endif else begin
				if sel and (sat ne self.satname or year ne self.year or month ne self.month or day ne self.day or orbit ne self.orbit) then $
				print,'Load button is checked! Search for file in loaded directory!'
				file = self -> get_new_filename( sat, year, month, day, orbit, algo, varname2, level = level, found = found,$
								 dirname = (sel ? self.directory:0))
				datum=strjoin([year,month,day,orbit])
			endelse
			if ~found then return

			if not keyword_set(maxi) then free,maxi
			if not keyword_set(mini) then free,mini

			if pmulti ne self.pmulti_default then begin
				!p.multi = fix(strsplit(strcompress(pmulti,/rem),'],[()',/ext))
				self.pmulti_default = pmulti
			endif

			varname = strjoin(names,',')

			if is_jch(varname) then begin
				vergleiche_ctp_cot_histogram_cci_mit_clara,file,varname = varname, mini = mini, maxi = maxi, limit=limit, zoom=zoom, sat=sat,$
				win_nr = win_nr, save_as= save_as,land = land, sea = sea,hist_cloud_type = hct[0], reference = ref,timeseries=pcmult	,$
				globe=globe,p0lon=p0lon,p0lat=p0lat,antarctic=ant,arctic=arc,mollweide=mollweide,hammer=hammer,goode=goode,log=log		,$
				aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,ctable=ctab,other=oth,difference=pcdts,show_values=show_values	,$
				out=out, verbose = verbose,nobar=nobar,algo1=algo, stereographic = stereographic, ztext = ztext, msg = msg,datum=datum,magnify=magnify, $
				countries=countries,notitle=notitle
				if show_values and pcdts and ~total(strlowcase(hct[0]) eq ['hist2d','hist_2d','max']) then $
				show_pixel_value, out.bild, out.lon,out.lat, data = varname, unit = out.unit, wtext = self.showpvalID
				if ~(pcdts and ~total(strlowcase(hct[0]) eq ['max','1d'])) then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
				if save_as then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
				if zoom and ~arc and ~ant then Widget_Control, self.limitID, Set_Value=strcompress(ztext[0],/rem)
				return
			endif

			histo1d = is_h1d(varname[0])

			if histo1d and pcmult and ~pchov then begin
				if verbose then print,'Map2d + 2D-Histogram + Zonal Mean'
				if ~(histo1d and strlowcase(hct[0]) eq '1d') then !p.multi=[0,2,2]
				compare_cci_with_clara, year, month, day, data = varname, ccifile = file, reference=ref, sat=sat, orbit = orbit, $
				mini = mini, maxi = maxi , zoom=zoom, limit=limit, win_nr = win_nr, save_dir = save_as,land=land, ztext = ztext,$
				sea=sea,cov=cov, show_values = show_values, verbose = verbose, other = oth, ctable=ctab, level = level,log=log, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer, msg = msg,$
				goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,algo1=algo,nobar=nobar, stereographic = stereographic, $
				hist_cloud_type=hct[0],error=error,timeseries=pcmult,dim3=dim3,magnify=magnify,oplots=opl,countries=countries,$
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),notitle=notitle
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
				return
			endif

			; erstmal nicht belegt
			; print,pcsing,pcmult,pcvar,pcmat,pcts,pchist,pczm,pcdts,pcmts,pcmatts,pchov
			if (pcms) or (pcts and pcsing) then begin
				ok = dialog_message(pcmatts ? 'This combi is currently not set!':'This combi is currently not set! Try "Multi Time Steps" instead!')
				return
			endif
			; compare this var
			if pcvar and pcsing then begin
				if verbose then print,'Map2d + 2D-Histogram + Zonal Mean'
				if ~(histo1d and strlowcase(hct[0]) eq '1d') then !p.multi=[0,2,2]
				compare_cci_with_clara, year, month, day, data = varname, ccifile = file, reference=ref, sat=sat, orbit = orbit, $
				mini = mini, maxi = maxi , zoom=zoom, limit=limit, win_nr = win_nr, save_dir = save_as,land=land, ztext = ztext,$
				sea=sea,cov=cov, show_values = show_values, verbose = verbose, other = oth, ctable=ctab, level = level,log=log, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer, msg = msg,$
				goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,algo1=algo,nobar=nobar, stereographic = stereographic, $
				hist_cloud_type=hct[0],error=error,timeseries=pcmult,dim3=dim3,magnify=magnify,oplots=opl,countries=countries,$
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),notitle=notitle
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif
			if pcdts and pcsing then begin
				if verbose then print,'Diff2D Single Time Step'
				if (histo1d and strlowcase(hct[0]) eq '1d') then begin
					ok = dialog_message('Diff2D not possible for this combi!')
					return
				endif
				compare_cci_with_clara, year, month, day, data = varname, ccifile = file, reference=ref, sat=sat, level = level,$
				mini = mini, maxi = maxi , zoom=zoom, limit=limit, win_nr = win_nr, save_dir = save_as,land=land, orbit = orbit,$
				sea=sea,cov=cov, show_values = show_values, verbose = verbose, other = oth, ctable=ctab, /difference, ztext = ztext, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer, msg = msg,out=out,$
				goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,algo1=algo,nobar=nobar, stereographic = stereographic,$
				error=error,log=log,dim3=dim3,magnify=magnify,wtext = self.showpvalID,countries=countries,notitle=notitle
				if show_values then show_pixel_value, out.bild, out.lon, out.lat, data = varname, unit = out.unit, wtext = self.showpvalID
			endif
			if pcmat then begin
				if verbose then print,'Matrix'
				make_2d_overview,year=year,month=month,sat,reference=ref, cov = cov,sat2=sat2, out = out, $
				save_as = save_as,mini=mini,maxi=maxi, verbose = verbose, time_series = pcmult,algo = algo,file1 = file
				if show_values then show_pixel_value, out, data = 'Diff', unit='%', wtext = self.showpvalID
			endif
			if pcmult and pcts then begin
				if verbose then print,'Time Series'
				plot_simple_timeseries, varname, sat, algo, cov, reference = ref,mini=mini, maxi=maxi,verbose=verbose,  $
				oplots=opl,win_nr=win_nr,logarithmic=log,white_bg=Widget_Info(self.wbgrID, /BUTTON_SET),$
				show_values = show_values, rot=rot,error=error,save_as=save_as,symsize=symsize,notitle=notitle,$
				nobar=nobar, addtext = addtext
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif
			if pczm and pcsing then begin
				if verbose then print,'Zonal Average Single Time Step'
				compare_cci_with_clara, year, month, day, data = varname, ccifile = file, reference=ref, sat=sat, level = level,error=error,$
				mini = mini, maxi = maxi , zoom=zoom, limit=limit, win_nr = win_nr, save_dir = save_as,land=land, orbit = orbit,$
				sea=sea,cov=cov, show_values = show_values, verbose = verbose, other = oth, ctable=ctab,/zonal_only, ztext = ztext, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer, msg = msg,log=log,$
				goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,algo1=algo,nobar=nobar, stereographic = stereographic, $
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),dim3=dim3,magnify=magnify,countries=countries,notitle=notitle
			endif
			if pczm and pcmult then begin
				if verbose then print,'Zonal Average Multi Time Step'
				plot_cci_gac_time_series, algo = algo, sat = sat, save_as = save_as,win_nr=win_nr,cov=cov,reference=ref,/zonal_only,$
				single_var=varname,mini=mini,maxi=maxi,limit=limit,bild=bild,lon=lon,lat=lat,unit=unit,zoom=zoom,$
				error=error, other = oth, ctable=ctab, globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, $
				arctic=arc,mollweide=mollweide,hammer=hammer,goode=goode,aitoff=aitoff,sinusoidal=sinusoidal, msg = msg,	$
				robinson=robinson,show_values = show_values,nobar=nobar, stereographic = stereographic, ztext = ztext,log=log, $
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),magnify=magnify,countries=countries,symsize=symsize,notitle=notitle
			endif
			if pcdts and pcmult then begin
				if verbose then print,'2D Difference Plot Multi Time Step'
				plot_cci_gac_time_series, algo = algo, sat = sat, save_as = save_as,win_nr=win_nr,cov=cov,reference=ref,/diff,$
				single_var=varname,mini=mini,maxi=maxi,limit=limit,bild=bild,lon=lon,lat=lat,unit=unit,zoom=zoom,$
				error=error, other = oth, ctable=ctab, globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant,log=log, $
				arctic=arc,mollweide=mollweide,hammer=hammer,goode=goode,aitoff=aitoff,sinusoidal=sinusoidal, msg = msg,$
				robinson=robinson, verbose = verbose,nobar=nobar, stereographic = stereographic, ztext = ztext,magnify=magnify, $
				countries=countries,symsize=symsize,notitle=notitle
				if show_values then show_pixel_value, bild, lon,lat, data = varname, unit = unit, wtext = self.showpvalID
			endif
			if pcvar and pcmult then begin
				if verbose then print,'Map2d Multi Time Step'
				plot_cci_gac_time_series, algo = algo, sat = sat, save_as = save_as,win_nr=win_nr,cov=cov,reference=ref, verbose = verbose,$
				single_var=varname,mini=mini,maxi=maxi,limit=limit,bild=bild,lon=lon,lat=lat,unit=unit,zoom=zoom,error=error, other = oth,$
				ctable=ctab,globe=globe,p0lon=p0lon,p0lat=p0lat,antarctic=ant,arctic=arc,mollweide=mollweide,hammer=hammer,goode=goode,magnify=magnify,$
				aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,nobar=nobar, stereographic = stereographic, ztext = ztext, msg = msg,log=log,$
				show_values = show_values,countries=countries,symsize=symsize,notitle=notitle
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif
			if pcmatts then begin
				if verbose then print,'Taylor Diagram'
				plot_taylor_diagram, year, month, day, file1=file, sat=sat, save_as = save_as, win_nr=win_nr,reference=ref, verbose =verbose,$
				varname=varname,mini=mini,maxi=maxi,limit=limit,unit=unit, other =oth,antarctic=ant,arctic=arc,algo = algo,level=level,$
				time_series=pcmult,notitle=notitle
			endif
			if pcms and pcmult then begin
 				if verbose then print,'Currently Unset'
			endif
			if pchov then begin
				if ptr_valid(self.out_hovmoeller) then begin
					if opl eq 0 then ptr_free,self.out_hovmoeller else out = *self.out_hovmoeller
				endif
				if verbose then print,'Hovmoeller Time Series'
				plot_hovmoeller, varname, algo[0], sat[0], save_as = save_as,mini=mini,maxi=maxi, win_nr=win_nr,nobar=nobar,antarctic=ant,arctic=arc, $
				ctable=ctab, other = oth,reference = ref, out = out, land = land, sea = sea, oplots = opl,found = found, limit = limit,coverage=cov, $
				notitle=notitle
				if show_values then begin
					if ~keyword_set(nobar) then begin
						print,'Set "No Bar" to get Pixel Values'
					endif else show_pixel_value, out.bild, data = varname, unit=out.unit, wtext = self.showpvalID
				endif
; 				self.out_hovmoeller = ptr_new(out.bild)
				self.out_hovmoeller = ptr_new(out)
				if ~found then opl = 0 > (self.oplotnr -=1 )
			endif
			if pchist then begin
				if verbose then print,'Histograms'
				plot_histogram,year,month,day,file,varname[0],mini=mini,maxi=maxi,limit=limit,sea=sea,land=land,level=level, $
				algo=algo[0],save_as=save_as,win_nr=win_nr,timeseries=pcmult, sat = sat[0], cov=cov,addtext = addtext[0], $
				datum=datum, ref = ref, change_side = show_values,verbose=verbose, $
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),log=log,notitle=notitle
			endif
			if pcmts then begin
				if verbose then print,'BoxPlots'
				boxplot, year, month, day, data=varname, satellite = sat, timeseries=pcmult,level=level, limit = limit, $
				filename1 = file, coverage = cov, error = error, mini = mini, maxi = maxi, save_as = save_as, win_nr = win_nr, $
				datum=datum,algo=algo[0],verbose=verbose,reference = ref,notitle=notitle
				if pcmult then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif

			if zoom and ~arc and ~ant then Widget_Control, self.limitID, Set_Value=strcompress(ztext[0],/rem)

			; reset values
; 			Widget_Control, self.pixvalID, Set_Button=0
 			Widget_Control, self.zoomID  , Set_Button=0
; 			Widget_Control, self.saveID  , Set_Button=0
			if save_as then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
		end

		'PLOT_DIFF'	: BEGIN

			Widget_Control, /HOURGLASS

			self -> get_info_from_plot_interface,varName,mini=mini,maxi=maxi,opl,hct,oth,ctab,show_values,verbose,all,sea,land,ant,mls,magnify=magnify, $
				tro,mln,arc,pm7,glo,nhm,shm,save_as,error,zoom,gac,modi,myd,gac2,modi2,myd2,syn,ccigwx,isp,cci,cci2,era,era2,g2gwx,pmx2,l1g,cla,hec,sel,pcms,win_nr,year, $
				month,day,orbit,pcsing,pcmult,pcvar,pcmat,pcts,pchist,pczm,pcdts,pcmts,pcmatts,pchov,pmulti,load,select,none,sat=sat,limit=limit	, $
				globe=globe,p0lat=p0lat,p0lon=p0lon,mollweide=mollweide,aitoff=aitoff,hammer=hammer,goode=goode,cov=cov,found=found		, $
				sinusoidal=sinusoidal,robinson=robinson,nobar=nobar,stereographic=stereographic,msg=msg,log=log,dim3=dim3,rot=rot,addtext=addtext,$
				countries=countries,symsize=symsize,notitle=notitle

			if ~found then return

			;set System Variables
			plot_l3, save_as = save_as, white_bg = Widget_Info(self.wbgrID, /BUTTON_SET)

			if (pcms and ~save_as) or pchov then begin
				ok = dialog_message('This combi is currently not set! Try "Compare" or "Multi Time Steps" instead!')
				return
			endif

			if pcmat and (total(self.leveltype eq ['L2','L3U','L1','L3DH','L2B_SUM'])) then begin 
				ok = dialog_message('This works with Monthly Means only!')
				return
			endif
			algo     = self.algoname
			level    = self.level
			datum1   = self.datum
			names    = strsplit(varname,',',/ext)
			if n_elements(names) gt 1 then begin
				varname  = names[0]
				varname2 = names[1]
			endif else varname2 = varname

			if sel then file1  = self.directory+'/'+self.filename else begin
				datum1 = strjoin([year,month,day,orbit])
				file1  = self -> get_new_filename( sat, year, month, day, orbit, algo, varname, level = level, found = found,dirname = self.directory)
				if ~found then return
			endelse

			if pmulti ne self.pmulti_default then begin
				!p.multi = fix(strsplit(strcompress(pmulti,/rem),'],[()',/ext))
				self.pmulti_default = pmulti
			endif
			if select or none then begin
				if none and n_elements(names) gt 1 then begin
					algo2      = algo
					satn       = self.satname
					datum2     = datum1
					self.file2 = file1
				endif else begin
					self.file2 = dialog_pickfile(path=(self.file2 eq '' ? file_dirname(file1):file_dirname(self.file2)),$
								; following needs to be done for filenames that include white spaces
								file = 	strmid(strjoin('\ '+strsplit(self.file2 eq '' ? file_basename(file1):file_basename(self.file2),/ext)),2),$
								filter=self.extension)
					if self.file2 eq '' then return
					if ~is_hdf(self.file2) then begin
						if ~is_ncdf(self.file2) then begin
							ok = dialog_message('The file '+self.file2+' is neither a netcdf nor a hdf file. '+$
									'Note netcdf4 files are not supported in IDL versions below 8!')
							return
						endif
					endif
					ok     = self -> get_file_infos(infile=self.file2)
					algo2  = ok.algoname
					satn   = ok.satname
					datum2 = ok.datum eq '' ? strjoin([year,month,day,orbit]) : ok.datum
				endelse
			endif else begin
				if modi   then begin & algo2 = 'coll5'      & satn = 'terra' & end
				if modi2  then begin & algo2 = 'coll6'      & satn = 'terra' & end
				if myd    then begin & algo2 = 'coll5'      & satn = 'aqua' & end
				if myd2   then begin & algo2 = 'coll6'      & satn = 'aqua' & end
				if era    then begin & algo2 = 'era-i'      & satn = '' & end
				if era2   then begin & algo2 = 'era-i2'     & satn = '' & end
				if gac    then begin & algo2 = 'clara'      & satn = sat & end
				if gac2   then begin & algo2 = 'clara2'     & satn = sat & end
				if ccigwx then begin & algo2 = 'gewex'      & satn = sat & end
				if syn    then begin & algo2 = 'calipso'    & satn = 'calipso' & end
				if isp    then begin & algo2 = 'isccp'      & satn = sat & end
				if cci    then begin & algo2 = 'esacci_old' & satn = sat & end
				if cci2   then begin & algo2 = 'esacci'     & satn = sat & end
				if g2gwx  then begin & algo2 = 'gac2-gewex' & satn = sat & end
; 				if pmx    then begin & algo2 = 'patmos_old' & satn = sat & end
				if pmx2   then begin & algo2 = 'patmos'     & satn = sat & end
				if l1g    then begin & algo2 = 'l1gac'      & satn = sat & end
				if cla    then begin & algo2 = 'claas'      & satn = 'msg' & end
				if hec    then begin & algo2 = 'hector'     & satn = sat & end
				if pcmult then begin
					self.file2 = ''
					found = 1
					datum2 = ''
				endif else begin
					self.file2 = (get_filename(year, month, day, data=varname2, sat=satn, algo=algo2, level=level, found = found, orbit=orbit))[0]
					if not found then begin
						self.file2 = self.directory+'/'+self.filename
						sat  = self.satname
						return
					endif
					datum2 = strjoin([year,month,day,orbit])
				endelse
			endelse

			if sel then sat  = self.satname
			if self-> filematch(file1,self.file2) and strmatch(varname,varname2) then begin
				ok = dialog_message('File1 and File2 are the same!',/cancel,/default_cancel)
				if ok eq 'Cancel' then return
			endif
			; plotte serie und speichere in pdf für feedback loop (Varnames intern gesetzt!)
			if pcms and save_as then begin
				ok = dialog_message("This combination will create PDF's for the Feedbackloop!",/cancel)
				if ok eq 'Cancel' then return
				!p.multi = 0
				compare_l2_save_serie,file1,self.file2,data1=varname,data2=varname,mini=mini,maxi=maxi	, $
				save_as=save_as, win_nr=win_nr,land=land,sea=sea,limit=limit,zoom=zoom,lon=lon,lat=lat	, $
				bild=bild,unit=unit,sat1 = sat, sat2 = satn,algo2=algo2,algo1=algo, verbose = verbose	, $
				year = year, month = month, day = day, orbit = orbit, datum1 = datum1, datum2 = datum2	, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide	, $
				hammer=hammer,goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,log=log	, $
				other=oth,ctable=ctab, level=level,nobar=nobar, stereographic = stereographic		, $
				/all_parameter,dim3=dim3,coverage=cov,rot=rot,magnify=magnify,countries=countries	, $
				notitle=notitle
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif else if is_jch(varname) and total(strlowcase(hct[0]) eq ['1d','1d_cot','1d_ctp','max']) then begin
				satn1  = sat_name(algo,sat)
				satn2  = sat_name(algo2,satn)
				if keyword_set(addtext) then begin
					at = strsplit(addtext,',',/ext)
					if n_elements(at) eq 2 then begin
						f1str  = 'F1 ('+at[0]+'): '
						f2str  = 'F2 ('+at[1]+'): '
					endif else begin
						f1str  = 'F1 ('+addtext[0]+'): '
						f2str  = 'F2 ('+addtext[0]+'): '
					endelse
					no_time_string = 1
				endif else begin
					f1str  = 'File1: '
					f2str  = 'File2: '
					if ~strmatch(varname,varname2) then begin
						f1str  = strreplace(f1str,':','')+strupcase(dat1)+': '
						f2str  = strreplace(f2str,':','')+strupcase(dat2)+': '
					endif
				endelse
				if strlowcase(hct[0]) eq 'max' then !p.multi = [0,2,1]
				if satn1 eq satn2 and satn1 ne '' then begin
					; wann wurde das letzte mal in datei geschrieben?
					mtime1 = (file_info(file1)).mtime
					mtime2 = (file_info(self.file2)).mtime
					if mtime1 gt mtime2 then begin
						if ~keyword_set(no_time_string) then f1str  = strreplace(f1str,':','')+' (new): '
						if ~keyword_set(no_time_string) then f2str  = strreplace(f2str,':','')+' (old): '
					endif else if mtime2 gt mtime1 then begin
						if ~keyword_set(no_time_string) then f1str  = strreplace(f1str,':','')+' (old): '
						if ~keyword_set(no_time_string) then f2str  = strreplace(f2str,':','')+' (new): '
					endif
				endif
				if strmid(strlowcase(hct[0]),0,2) eq '1d' then begin
					plot_1d_from_jch_4all,file1,self.file2,year=year,month=month,sat1=sat,prefix=[f1str,f2str], $
					land=land,sea=sea,limit=limit,save_as=save_as,win_nr=win_nr,antarctic=ant,arctic=arc	 , $
					liquid = stregex(varname,'ice',/fold,/bool),ice = stregex(varname,'liq',/fold,/bool)	 , $
					algo1=algo,algo2=algo2,mini=mini,maxi=maxi,hist_cloud_type=hct[0],notitle=notitle,sat2=satn
				endif else begin
					; file1
					plot_l2,year[0],month[0],day[0],file=file1,data=varname[0],mini=mini,maxi=maxi,sat=sat	, $
					algo=algo[0],hist_cloud_type=hct[0],win_nr=win_nr,sea = sea,land=land,save_as=save_as	, $
					limit=limit,zoom=zoom,globe=globe,p0lon=p0lon,stereographic=stereographic,msg_proj=msg	, $
					p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer,log=log	, $
					goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0]	, $
					ctable = ctab, other = oth, verbose = verbose,level=level, prefix=f1str,nobar=nobar	, $
					cov=cov,magnify=magnify,countries=countries,notitle=notitle
					; file2
					plot_l2,year[0],month[0],day[0],file=self.file2,data=varname2,mini=mini,maxi=maxi,sat=satn, $
					algo=algo2[0],hist_cloud_type=hct[0],win_nr=win_nr,sea = sea,land=land,save_as=save_as	, $
					limit=limit,zoom=zoom,lon=lon,lat=lat,bild=bild,unit=unit,globe=globe,p0lon=p0lon	, $
					p0lat=p0lat,antarctic=ant,arctic=arc,mollweide=mollweide,hammer=hammer,msg_proj=msg	, $
					goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0],log=log, $
					ctable = ctab, other = oth, verbose = verbose,level=level, prefix=f2str,nobar=nobar	, $
					stereographic = stereographic,cov=cov,magnify=magnify,countries=countries,notitle=notitle
					if show_values and ~pchist and ~pczm and ~pcmts then show_pixel_value, bild, lon,lat	, $
					data = varname2, unit = unit, wtext = self.showpvalID
				endelse
				!p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endif else if pcmatts then begin
				if verbose then print,'Taylor Diagram'
				plot_taylor_diagram,year,month,day,file1=file1,file2=self.file2,sat=sat,save_as=save_as		, $
				win_nr=win_nr,reference=algo2,verbose=verbose,varname=varname,mini=mini,maxi=maxi,limit=limit	, $
				unit=unit,other=oth,antarctic=ant,arctic=arc,algo = algo,level=level,time_series=pcmult,notitle=notitle
			endif else if pcmat then begin
				if verbose then print,'Matrix'
				make_2d_overview,year=year,month=month,sat,reference=algo2, cov = cov,sat2=satn, out = out, $
				save_as = save_as,mini=mini,maxi=maxi, verbose = verbose, time_series = pcmult,algo = algo,$
				file1 = file1,file2=self.file2,addtext=addtext[0], datum1 = datum1, datum2 = datum2,notitle=notitle
				if show_values then show_pixel_value, out, data = 'Diff', unit='%', wtext = self.showpvalID
			endif else begin
				compare_l2,file1,self.file2,data1=varname,data2=varname2,mini=mini,maxi=maxi,level=level, $
				save_as=save_as, win_nr=win_nr,land=land,sea=sea,limit=limit,zoom=zoom,out=out		, $
				sat1 = sat, sat2 = satn,algo2=algo2,algo1=algo, htypes = hct[0],verbose = verbose	, $
				year = year, month = month, day = day, orbit = orbit, datum1 = datum1, datum2 = datum2	, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide	, $
				hammer=hammer,goode=goode,aitoff=aitoff,sinusoidal=sinusoidal,robinson=robinson	,log=log, $
				diff_only=pcdts,hist_only=pchist,maps_only=pcvar,other=oth,ctable=ctab,zonal_only=pczm	, $
				box_only = pcmts,nobar=nobar, stereographic = stereographic, ztext = ztext, msg = msg	, $
				timeseries=pcmult,dim3=dim3,coverage=cov,addtext=addtext[0],rot=rot,magnify=magnify	, $
				wtext = self.showpvalID,countries=countries,notitle=notitle
				if show_values and ~pchist and ~pczm and ~pcmts and ~pcvar then show_pixel_value, out.bild, out.lon,out.lat, $
				data = varname, unit = out.unit, wtext = self.showpvalID
				if zoom and ~arc and ~ant then Widget_Control, self.limitID, Set_Value=strcompress(ztext[0],/rem)
			endelse
			; reset values
; 			Widget_Control, self.pixvalID, Set_Button=0
			Widget_Control, self.zoomID  , Set_Button=0
; 			Widget_Control, self.saveID  , Set_Button=0
			if ~pchist and ~pczm and ~pcdts and ~pcmts then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
		END

		'PLOT_AND_STAY'	: BEGIN

			Widget_Control, /HOURGLASS

			self -> get_info_from_plot_interface,varName,mini=mini,maxi=maxi,opl,hct,oth,ctab,show_values,verbose,all,sea,land,ant,mls,magnify=magnify, $
				tro,mln,arc,pm7,glo,nhm,shm,save_as,error,zoom,gac,modi,myd,gac2,modi2,myd2,syn,ccigwx,isp,cci,cci2,era,era2,g2gwx,pmx2,l1g,cla,hec,sel,pcms,$
				win_nr,year,month,day,orbit,pcsing,pcmult,pcvar,pcmat,pcts,pchist,pczm,pcdts,pcmts,pcmatts,pchov,pmulti,load,select,none,sat=sat,$
				limit=limit,globe=globe,p0lat=p0lat,p0lon=p0lon,mollweide=mollweide,aitoff=aitoff,hammer=hammer,goode=goode,cov=cov,found=found	, $
				sinusoidal=sinusoidal,robinson=robinson,nobar=nobar, stereographic = stereographic,msg=msg,log=log,dim3=dim3,rot=rot,addtext=addtext,$
				countries=countries,symsize=symsize,notitle=notitle

			if ~found then return

			if (pcsing and pchov) then begin
				ok = dialog_message('Use "Multi Time Steps" for Hovmoeller plots!')
				return
			endif
			; erstmal nicht belegt
			if pcmts or pcmat or pcmatts or pcdts or (pcms and ~save_as) or (pcts and pcsing) then begin
				ok = dialog_message('This combi is currently not set! Try "Compare" oder "Multi Time Steps" instead!')
				return
			endif
			if none   then begin & algo = self.algoname & ref = self.reference & end
			if modi   then begin & algo = 'coll5'       & sat = 'terra'   & ref = 'mod'  & end
			if modi2  then begin & algo = 'coll6'       & sat = 'terra'   & ref = 'mod2' & end
			if myd    then begin & algo = 'coll5'       & sat = 'aqua'    & ref = 'myd'  & end
			if myd2   then begin & algo = 'coll6'       & sat = 'aqua'    & ref = 'myd2' & end
			if era    then begin & algo = 'era-i'       & ref = 'era'     & end
			if era2   then begin & algo = 'era-i2'      & ref = 'era2'    & end
			if gac    then begin & algo = 'clara'       & ref = 'gac'     & end
			if gac2   then begin & algo = 'clara2'      & ref = 'gac2'    & end
			if g2gwx  then begin & algo = 'gac2-gewex'  & ref = 'g2_gwx'  & end
			if ccigwx then begin & algo = 'gewex'       & ref = 'gwx'     & end
			if syn    then begin & algo = 'calipso'     & ref = 'cal'     & end
			if isp    then begin & algo = 'isccp'       & ref = 'isp'     & end
			if cci    then begin & algo = 'esacci_old'  & ref = 'cci_old' & end
			if cci2   then begin & algo = 'esacci'      & ref = 'cci'     & end
; 			if pmx    then begin & algo = 'patmos_old'  & ref = 'pmx_old' & end
			if pmx2   then begin & algo = 'patmos'      & ref = 'pmx'     & end
			if l1g    then begin & algo = 'l1gac'       & ref = 'l1gac'   & end
			if cla    then begin & algo = 'claas'       & sat = 'msg'     & ref = 'cla'  & end
			if hec    then begin & algo = 'hector'      & ref = 'hec'     & end

			;set System Variables
			plot_l3, save_as = save_as, white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),reference = ref

			if select and ~pcmult then begin
				self.file2 = dialog_pickfile(path=(self.file2 eq '' ? self.directory:file_dirname(self.file2)),$
							; following needs to be done for filenames that include white spaces
							file=strmid(strjoin('\ '+strsplit(self.file2 eq '' ? self.filename:file_basename(self.file2),/ext)),2),$
							filter=self.extension)
				if self.file2 eq '' then return
				ok     = self -> get_file_infos(infile=self.file2)
				sat    = ok.satname ne '' ? ok.satname : sat
				algo   = ok.algoname
				ref    = ok.reference
				level  = ok.level
				year   = ok.year
				month  = ok.month
				day    = ok.day
				orbit  = ok.orbit
				datum  = ok.datum eq '' ? strjoin([year,month,day,orbit]) : ok.datum
				file   = self.file2
				if algo eq 'ERA-I' then get_era_info, self.file2, /set_as_sysvar
				found  = 1
			endif else begin
				if pcmult then begin
					file  = ''
					found = 1
					datum = ''
					level = self.level
					if select then algo = 'select'
				endif else if sel and none then begin
					file    = self.directory+'/'+self.filename
					year    = self.year
					month   = self.month
					day     = self.day
					orbit   = self.orbit
					level   = self.level
					datum   = self.datum
					version = self.version
				endif else begin
					if none and algo eq 'GEWEX' and algo eq self.algoname then begin
						gewex_style = (strsplit(self.filename,'_',/ext))[3]
						help,gewex_style
					endif
					file  = self -> get_new_filename( sat, year, month, day, orbit, algo, varname, found = found, level=level, $
									 dirname = (none ? self.directory:0),gewex_style=gewex_style)
					datum = strjoin([year,month,day,orbit])
 				endelse
			endelse

			if ~found then begin
				opl = 0 > (self.oplotnr -=1 )
				return
			endif ;else if ~pcmult then print,'ncdf_data_def: File1: ',file[0]

			if pmulti ne self.pmulti_default then begin
				!p.multi = fix(strsplit(strcompress(pmulti,/rem),'],[()',/ext))
				self.pmulti_default = pmulti
			endif

			hist2d = is_jch(varname)

			hist1d = is_h1d(varname)
			if hist1d and ~(pcvar or pchov) then begin
				ok=dialog_message('For hist1d choose plot style "Map2D" or "Hovmoell"')
				return
			endif

			if pcms and save_as then begin
				ok = dialog_message("This combination will create PUG/PVIR Pictures! Make sure that Date, Projection and Limit is properly set. ",/cancel)
				if ok eq 'Cancel' then return
				plot_l2_save_serie,year[0],month[0],day[0],sat=sat[0],algo=algo[0],file=file, $
				sea = sea,land=land,save_as=save_as,limit=limit,timeseries=pcmult, $
				p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer,goode=goode,aitoff=aitoff,$
				sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0], ctable = ctab, other = oth, verbose = verbose,level=level,nobar=nobar,$
				cov=cov, ztext = ztext, stereographic = stereographic,msg_proj=msg,error=error,log=log,$
				dim3=dim3,rot=rot,datum=datum, prefix=addtext[0],addtext = addtext[0],$
				magnify=magnify,countries=countries,notitle=notitle
			endif else if pchov then begin
				; hovmoeller
				if hist2d then begin 
					   ok=dialog_message('Hovmoeller Diagram Not available for '+varname)
					   return
				endif
				if ptr_valid(self.out_hovmoeller) then begin
					if opl eq 0 then ptr_free,self.out_hovmoeller else out = *self.out_hovmoeller
				endif
				plot_hovmoeller, varname, algo, sat, save_as = save_as,mini=mini,maxi=maxi, win_nr=win_nr,ctable=ctab,coverage=cov,$
				oplots = opl, other = oth,land=land,sea=sea, out = out,found = found,nobar=nobar, limit = limit,antarctic=ant,arctic=arc
				if show_values and is_defined(out) then begin
					if nobar ne 1 then begin
						print,'Set "No Bar" to get Pixel Values'
					endif else show_pixel_value, out.bild, data = varname, unit=out.unit, wtext = self.showpvalID
				endif
; 				if is_defined(out) then self.out_hovmoeller = ptr_new(out.bild)
				if is_defined(out) then self.out_hovmoeller = ptr_new(out)
				if ~found then opl = 0 > (self.oplotnr -=1 )
			endif else if pcts and ~hist2d then begin
				; time series
				plot_simple_timeseries, varname, sat, algo, cov, mini=mini, maxi=maxi, win_nr=win_nr,symsize=symsize,$
				verbose=verbose,oplots = opl,found=found, addtext = addtext[0],error=error,save_as = save_as,$
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),show_values=show_values,$
				notitle=notitle,nobar=nobar,logarithmic=log
				if ~found then opl = 0 > (self.oplotnr -=1 )
			endif else if pchist then begin
				; histogram
				if hist2d then begin
					   ok=dialog_message('Not available for '+varname)
					   return
				endif
				plot_histogram,year[0],month[0],day[0],file,varname[0],mini=mini,maxi=maxi,limit=limit,sea=sea,land=land,$
				algo=algo[0],save_as=save_as,win_nr=win_nr,timeseries=pcmult, sat = sat[0], found = found,addtext = addtext[0], $
				datum=datum, change_side = show_values,verbose=verbose,cov=cov,oplots = opl,$
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),notitle=notitle,logarithmic=log
				if ~found then opl = 0 > (self.oplotnr -=1 )
			endif else if pczm then begin
				; zonal median
				if hist2d then begin 
					   ok=dialog_message('Not available for '+varname)
					   return
				endif
				plot_zonal_average,year[0],month[0],day[0],file,varname,limit=limit,sea=sea,land=land,save_as=save_as,win_nr=win_nr,$
				algo=algo,timeseries=pcmult,sat=sat,oplots = opl,found = found,mini=mini,maxi=maxi,level=level,addtext = addtext[0],$
				datum=datum,error=error, white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),coverage=cov,notitle=notitle,nobar=nobar,$
				logarithmic=log
				if ~found then opl = 0 > (self.oplotnr -=1 )
			endif else if pcms and pcmult then begin
				; Unset change to something new!
			endif else begin
				if obj_valid(self.map_objout) then obj_out = self.map_objout
				if opl eq 0 then begin
					if obj_valid(obj_out) then obj_destroy,obj_out
					if obj_valid(self.map_objout) then obj_destroy,self.map_objout
				endif
				if hist2d and strlowcase(hct[0]) eq '1d' then !p.multi=[0,2,1] 
				plot_l2,year[0],month[0],day[0],file=file,data=varname[0],mini=mini,maxi=maxi,sat=sat[0],algo=algo[0],hist_cloud_type=hct[0], $
				win_nr=win_nr,sea = sea,land=land,save_as=save_as,limit=limit,zoom=zoom,lon=lon,lat=lat,bild=bild,unit=unit,timeseries=pcmult, $
				globe=globe,p0lon=p0lon,p0lat=p0lat, antarctic = ant, arctic = arc, mollweide=mollweide,hammer=hammer,goode=goode,aitoff=aitoff,$
				sinusoidal=sinusoidal,robinson=robinson,orbit=orbit[0], ctable = ctab, other = oth, verbose = verbose,level=level,nobar=nobar,$
				cov=cov, wtext = self.showpvalID, ztext = ztext, stereographic = stereographic,msg_proj=msg,oplots = opl,error=error,log=log,$
				white_bg = Widget_Info(self.wbgrID, /BUTTON_SET),dim3=dim3,rot=rot,datum=datum, prefix=addtext[0],obj_out=obj_out,addtext = addtext[0],$
				magnify=magnify,countries=countries,notitle=notitle

				if obj_valid(obj_out) then self.map_objout = obj_out else begin
					; in map_image wird intern decompose auf 0 gesetzt für nicht rgb bilder, im cleanup dann wieder auf vorherigen wert,
					; cleanup wird hier nicht aufgerufen also decompose=1 sonst gibts verrückte farben
					device, decompose = self.decompose
				endelse

				if zoom and ~arc and ~ant then Widget_Control, self.limitID, Set_Value=strcompress(ztext[0],/rem)
				if hist2d and strlowcase(hct[0]) eq '1d' then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
			endelse

			if show_values and is_defined(bild) then begin
				if is_defined(lon) and is_defined(lat) then begin
					if ~(hist2d and strlowcase(strmid(hct[0],0,2)) eq '1d') then show_pixel_value, bild, lon, lat, data = varname, unit = unit, wtext = self.showpvalID
				endif
			endif
			; reset values
; 			Widget_Control, self.pixvalID, Set_Button=0
 			Widget_Control, self.zoomID  , Set_Button=0
; 			if reset_selfbutton then Widget_Control, self.refself , Set_Button=1
; 			Widget_Control, self.saveID  , Set_Button=0	
			if save_as then !p.multi = fix(strsplit(strcompress(self.pmulti_default,/rem),'],[()',/ext))
		END

		'QUIT_PLOT_VARIABLE_GUI': Widget_Control, event.top, /DESTROY

	ENDCASE

    END

    'WIDGET_COMBOBOX': BEGIN
		; The name of the variable to write has to be changed when the droplist value changes.
		Widget_Control, event.id, Get_UVALUE=list
		theName = Widget_Info(event.id, /UNAME)
		if theName eq 'PLOTS_PROJLIST' then begin
			self.proj = list[event.index]
		endif else if theName eq 'PLOTS_SYMSIZELIST' then begin
			self.PsymSize=list[event.index]
		endif else if theName eq 'PLOTS_BARLIST' then begin
			self.handlebar=list[event.index]
		endif else if theName eq 'PLOTS_MAGNIFYLIST' then begin
			self.magnify=list[event.index]
		endif else if theName eq 'PLOTS_CTLIST' then begin
			self.ctab=list[event.index]
		endif else if theName eq 'PLOTS_YEARLIST' then begin
			self.yy=list[event.index]
			ndays = ['--', (self.yy ne '--' and self.mm ne '--' ? dom(self.yy,self.mm) : string(indgen(31)+1,f='(i2.2)'))]
			widget_control,self.dayID, set_uvalue = ndays, set_value=ndays,Scr_XSize=47,Scr_YSize=28
			widget_Control,self.dayID, SET_COMBOBOX_SELECT=where(self.dd eq ndays)
		endif else if theName eq 'PLOTS_MONTHLIST' then begin
			self.mm=list[event.index]
			ndays = ['--', (self.yy ne '--' and self.mm ne '--' ? dom(self.yy,self.mm) : string(indgen(31)+1,f='(i2.2)'))]
			widget_control,self.dayID, set_uvalue = ndays, set_value=ndays,Scr_XSize=47,Scr_YSize=28
			Widget_Control,self.dayID, SET_COMBOBOX_SELECT=where(self.dd eq ndays)
		endif else if theName eq 'PLOTS_DAYLIST' then begin
			self.dd=list[event.index]
			self -> SetLevel
		endif else if theName eq 'PLOTS_PMULTILIST' then begin
			self.pmulti=list[event.index]
		endif else if theName eq 'PLOTS_ZLIST' then begin
			self.dim3=list[event.index]
		endif else if theName eq 'PLOTS_HISTCTLIST' then begin
			self.hct=list[event.index]
		endif else if theName eq 'PLOTS_ROTATELIST' then begin
			self.rot=list[event.index]
		endif else if theName eq 'PLOTS_DAYTYPELIST' then begin
			self.leveltype=list[event.index]
			if total(self.leveltype eq ['L3C','L3S']) then begin
				Widget_Control, self.dayID     , SET_COMBOBOX_SELECT=0
				self.dd='--'
			endif
		endif else if theName eq 'PLOTS_LOAD_ALGO_LIST' then begin
			self.compare_algo1 = event.index eq 0 ? self.algoname : algo2ref(event.str)
			if self.compare_algo1 eq 'unknown' then self.compare_algo1 = event.str
		endif else if theName eq 'PLOTS_VARLIST' then begin
	; 		Widget_Control, self.varname_plotID, Set_Value=list[event.index]
	; 		self.varname_plotID=event.str
			self.varname_plotID=strreplace(event.str,['_liq','_ice','_ratio'],['','',''])
			keep_mima = ( Widget_Info(self.enablemima,/BUTTON_SET) )
			if ~keep_mima then begin
	; 			read_data,self.directory+'/'+self.filename, self.varname_plotID, theData, found = success, algo = self.algoname, fillvalue, minvalue, maxvalue
				thedata = get_data(self.year,self.month,self.day,file=self.directory+'/'+self.filename,data=self.varname_plotID, $
						algo=self.algoname,level=self.level,/keep_data_name,sat=self.satname,minvalue=minvalue,$
						maxvalue=maxvalue,/make_compareable, var_dim_names=var_dim_names, found = success,/silent)

				; avoid converting byte into strings (ascii code!)
				if size(minvalue,/type) eq 1 then minvalue = fix(minvalue)
				if size(maxvalue,/type) eq 1 then maxvalue = fix(maxvalue)
				if success then begin;and event.index ge 0 then begin
					if stregex(event.str,'_ratio',/bool,/fold) then begin
						minvalue = 0
						maxvalue = 100
					endif
					if is_string(thedata) then begin
						Widget_Control, self.minimumID, Set_Value=''
						Widget_Control, self.maximumID, Set_Value=''
					endif else begin
						Widget_Control, self.minimumID, Set_Value=strcompress(minvalue,/rem)
						Widget_Control, self.maximumID, Set_Value=strcompress(maxvalue,/rem)
						si = [size(thedata,/dim),1,1]
						if keyword_set(var_dim_names) and n_elements(si) ge 5 then begin
							binval = get_ncdf_data_by_name(self.directory+'/'+self.filename,var_dim_names[2])
							widget_control, self.zkompID, set_uvalue = strcompress(indgen(si[2]),/rem),set_value=strcompress(string(binval,f='(f20.2)'),/rem)
						endif else begin
							widget_control, self.zkompID, set_uvalue = strcompress(indgen(si[2]),/rem),set_value=strcompress(indgen(si[2]),/rem)
						endelse
					endelse
	; 			endif else if success then begin
	; 				si = [size(thedata,/dim),1,1]
	; 				widget_control, self.zkompID, set_uvalue = strcompress(indgen(si[2]),/rem),set_value=strcompress(indgen(si[2]),/rem)
				endif
			endif
		endif else begin
			print,'Unknown List '+theName
			stop
		endelse
    END

    'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't care what it is.

 ENDCASE
END ;---------------------------------------------------------------------------------------------

PRO NCDF_DATA::SetLevel

	lcnt = 0
	widget_control, self.orbID,get_value=orbit
	widget_control, self.levelID, Get_UVALUE=level_list
	scol = total(Widget_Info([self.avhrrs,self.modises,self.allsat],/button_set))
	; monthly
	if self.dd eq '--' then begin
		lidx = scol ? where('L3S' eq [level_list],lcnt) : where('L3C' eq [level_list],lcnt)
		self.leveltype = scol ? 'L3S' : 'L3C'
		widget_control, self.orbID,set_value=''
	endif else if self.dd ne '--' and orbit eq '' then begin
		if total(self.leveltype eq ['L3S','L3C','L2','L1']) then begin
			lidx = where('L3U' eq [level_list],lcnt)
			self.leveltype = 'L3U'
		endif else lidx = where(self.leveltype eq [level_list],lcnt)
	endif else if self.dd ne '--' and orbit ne '' then begin
		l1orl2 = strupcase(self.level) eq 'L1' ? 'L1' : 'L2'
		lidx = where(l1orl2 eq [level_list],lcnt)
		self.leveltype = l1orl2
	endif

	if lcnt gt 0 then Widget_Control, self.levelID, SET_COMBOBOX_SELECT=lidx

END ;---------------------------------------------------------------------------------------------
FUNCTION NCDF_DATA::filematch,file1,file2

	return, strmatch(strreplace(file1,['//','///'],['/','/']),strreplace(file2,['//','///'],['/','/']))

END ;---------------------------------------------------------------------------------------------

FUNCTION NCDF_DATA::ReadVariableWithAttr, theVariable, SUCCESS=success, silent = silent
;
; NAME:
;       NCDF_DATA::ReadVariableWithAttr
;
; PURPOSE:
;
;       This method is used to read and return a variable and its attributes from a netCDF or HDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       struct:      A structure containing the variable (in the field "data") and its
;                    attributes in other fields. Plus, the field NDIMS holds the number
;                    of dimensions of the variable, and the field DIMS is a vector of
;                    the dimensions of the variable (for HDF files) or the dimension
;                    IDs (for netCDF file).
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile, silent = silent
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1

   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'
   
   ; Branch on type of file to read.
   IF self.isHDF THEN BEGIN
   
       ; Open the file.
       fileID = HDF_SD_Start(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       index = HDF_SD_NameToIndex(fileID, theVariable)
       IF index EQ -1 THEN Message, 'Variable (' + theVariable + ') not found.'
       
       ; Select the variable and read it.
       varID = HDF_SD_Select(fileID, index)
       HDF_SD_GetData, varID, data
       
       ; This routine throws all kinds of scary messages if CALDATA is
       ; not in the file. Turn this off for this call.
       !QUIET = 1
       HDF_SD_GetInfo, varID, CALDATA=calData, DIMS=dims, NDIMS=ndims
       !QUIET = 0
         
       ; Reverse the indices in HDF files and calibrate, if neccesary.
       data = Reform(Temporary(data))
       IF calData.cal NE 0 THEN data = calData.cal * (Temporary(data) - calData.offset)
       varStruct = Create_Struct('data', Temporary(data))
       
       ; If this variable has attributes, get those, too.
       HDF_SD_GetInfo, varID, NATTS=natts
       IF natts GT 0 THEN BEGIN
            varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, natts+1)
            FOR k=0,natts-1 DO BEGIN
                HDF_SD_ATTRINFO, varID, k, DATA=value, NAME=attrName
                attrName = IDL_ValidName(attrName, /CONVERT_ALL)
                IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
                varStruct = Create_Struct(varStruct, attrName, value)         
            ENDFOR
            IF calData.cal EQ 0 $
                THEN varStruct = Create_Struct(varStruct, '_calibration_data', 'Not Present in File') $
                ELSE varStruct = Create_Struct(varStruct, '_calibration_data', calData)
       ENDIF
       
       ; Add dimension informatio to the structure.
       varStruct = Create_Struct(varStruct, 'ndims', ndims, 'dims', dims)

       HDF_SD_EndAccess, varID
       HDF_SD_END, fileID
       success = 1

   endif else if self.isHDF5 then begin    
	; Open the file.
	fileID  = h5f_open(self.directory+'/'+self.filename)
	objInfo = h5g_get_objinfo(fileid, theVariable)
	case objInfo.type of
		'GROUP'		: dataset_id = h5g_open(fileid, theVariable )
		'DATASET'	: dataset_id = h5d_open(fileid, theVariable )
		'TYPE'		: dataset_id = h5t_open(fileid, theVariable)
		else 	: begin
				print,'Was tun mit '+objInfo.type+' types??'
				Message, 'Variable (' + theVariable + ') not found.'
				success = 0
				return, -1
			  end
	endcase

	; Add dimension informatio to the structure.
	varstruct = H5_PARSE(dataset_id, '/'+theVariable, FILE=self.filename, PATH=self.directory,/read)
	case objInfo.type of
		'GROUP'		: h5g_close, dataset_id
		'DATASET'	: h5d_close, dataset_id
		'TYPE'		: h5t_close, dataset_id
		else :
	endcase
	success = 1
   ENDIF ELSE BEGIN

       ; Open the file.
       fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
       
       ; Get the variable ID.
       varID = NCDF_VarID(fileID, theVariable)

       ; Get information about the variable.
       varInfo = NCDF_VarInq(fileID, varID)

       ; Do we have to worry about zero dimensions?
       IF Ptr_Valid(self.zeroDimensionID) THEN BEGIN
       
           ; Is there a match between the dimension IDs and any zero dimension ID we 
           ; have stored?
           match = 0
           FOR m = 0, N_Elements(*self.zeroDimensionID)-1 DO BEGIN
               i = Where(varInfo.dim EQ (*self.zeroDimensionID)[m], count)
               IF count GT 0 THEN match = 1
           ENDFOR
           IF match GT 0 THEN BEGIN
               ok = Dialog_Message('This variable has a dimension of length zero and cannot be read.')
               NCDF_CLOSE, fileID
               success = 0
               RETURN, -1             
           ENDIF 
       ENDIF
       
       ; Read the variable.
       NCDF_VarGet, fileID, varID, data
       IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
       IF StrUpCase(varInfo.datatype) EQ 'CHAR' THEN data = String(Temporary(data))
       varStruct = Create_Struct('data', Temporary(data))
             
       ; Add the variable attributes to the structure.
       FOR k=0,varInfo.natts-1 DO BEGIN
           attrName = NCDF_AttName(fileID, varID, k)
           NCDF_AttGet, fileID, varID, attrName, value
           IF Size(value, /TNAME) EQ 'BYTE' THEN value = String(value)
           attrName = IDL_ValidName(attrName, /CONVERT_ALL)
           IF Where(Tag_Names(varStruct) EQ StrUpCase(attrName)) NE -1 THEN CONTINUE
           varStruct = Create_Struct(varStruct, attrName, value)
       ENDFOR
       
       ; Add a dimensions field to the structure.
       varStruct = Create_Struct(varStruct, 'ndims', varInfo.Ndims, 'dims', varInfo.dim)
       
       ; Close the file, set status flag, return the data.
       NCDF_CLOSE, fileID
       success = 1
   
   ENDELSE
   
   RETURN, varstruct

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI, event

   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
;    theList = [(*self.theVariables).name]
   theList = self -> make_VARList()
   self.varpluslistID = Widget_Droplist(row, Value=[theList],UVALUE=[theList], SCR_XSIZE=250, UNAME='VARIABLESPLUS')
   label = Widget_Label(row, Value='Variable Name: ')
;    thisVarname = IDL_ValidName((*self.theVariables)[0].name + '_struct', /CONVERT_ALL)
   thisVarname = (*self.theVariables)[0].name + '_struct'

   self.varplusnameID = Widget_Text(row, Value=thisVarname, /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_VARPLUS_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_VARPLUS_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARPLUS_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
;    Widget_Control, theSelection, Get_Value=varName
;    index = Where(theList EQ varName, count)
	; Beobachten ob das immer funktioniert
	
	Widget_Control, theSelection, Get_UValue=UName
	if size(UName,/type) gt 0 then begin
		varname = strjoin(UName,'/')
		index   = Where(theList EQ varName, count)
		IF count GT 0 THEN BEGIN
			Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
			;       Widget_Control, self.varplusnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
			Widget_Control, self.varplusnameID, Set_Value=theList[index]
		ENDIF

		; Get it going...
		Widget_Control, tlb, /REALIZE
		XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
	endif
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_VARPLUS_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
                  Widget_Control, self.varPlusNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               VarName = IDL_ValidName(varName, /CONVERT_ALL)
;                IF thisVarName NE varName THEN BEGIN
;                   Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(thisVarName, /CONVERT_ALL)       
;                   varName = thisVarName
;                ENDIF
               IF varName EQ "" THEN dialog_Message, 'Must have a non-null variable name to create a variable.'

               ; Which variable do you want to read?
;                Widget_Control, self.varpluslistID, Get_UValue=theList
               Widget_Control, self.varpluslistID, Get_Value=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
;                Widget_Control, self.varplusnameID, Set_Value=IDL_ValidName(theList[index] + '_struct', /CONVERT_ALL)
               Widget_Control, self.varplusnameID, Set_Value=theList[index] + '_struct'
               END
               
            'READ_VARPLUS_AND_LEAVE': BEGIN
               
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varplusNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               VarName = IDL_ValidName(varName, /CONVERT_ALL)
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
;                Widget_Control, self.varpluslistID, Get_UValue=theList
               Widget_Control, self.varpluslistID, Get_Value=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_VARPLUS_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA::SelectionInTree, event
   ; This internal method processes events from the tree widget.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Create variable for better formatting.
   tab = '   '
   
   ; What to do depends on the node name of the tree selection.
   nodeName = Widget_Info(event.id, /UNAME)
   CASE nodeName OF
   
      'FOLDER': Widget_Control, self.textDisplay, Set_Value=""
               
      'GLOBAL ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_Value=name
         gattr = *self.theAttributes
         i = Where(gattr.name EQ name, count)
         IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisAttr = (*self.theAttributes)[i]
             IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   text[si+1:si+1+(lines-1)] = tab + str
                   si = si + 2 + lines
             ENDIF ELSE BEGIN
                   var = *thisAttr.value
                   IF N_Elements(var) EQ 0 THEN var = var[0]
                   Help, var, OUTPUT=helptext, /STRUCTURE
                   text = [text, StrArr(N_Elements(helptext) + 2)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   aString = StrMid(helptext, 3)
                   parts = StrSplit(aString, '=', /Extract)
                   lastPart = StrCompress(parts[1], /REMOVE_ALL)
                   IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                        IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                            lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                        ENDIF ELSE BEGIN
                            lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                        ENDELSE
                   ENDIF
                   aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                   text[si+1] = aString
                   si = si + N_Elements(helptext) + 2
             ENDELSE
             Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
                  
     'DIMENSION': BEGIN
          si = 0
          Widget_Control, event.id, Get_Value=name
          dims = *self.theDimensions
          i = Where(dims.name EQ name, count)
          IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisDim = (*self.theDimensions)[i]
             text[si] = StrUpCase(thisDim.name) + ':'
             text[si+1] = tab + StrTrim(thisDim.value,2)
          ENDIF
          Widget_Control, self.textDisplay, Set_Value=text
          END
                   
     'VARIABLE': BEGIN
           Widget_Control, event.id, Get_Value=name
           vars = *self.theVariables 
           IF N_Elements(vars) EQ 0 THEN vars = vars[0]
           i = Where(vars.name EQ name, count)
           FOR k=0,count-1 DO BEGIN
               thisVar = vars[i[k]]
               text = StrArr(6)
               text[0] = tab + 'NAME: ' + name
               text[1] = tab + 'DATATYPE: ' + thisVar.datatype
               n = StrTrim(N_Elements(*thisVar.datasize),2)
               f = ' (' + n + '(I0, :, ", "))'
               d = String(*thisVar.datasize, FORMAT=f)
               text[2] = tab + 'N_DIMENSIONS:  ' + StrTrim(N_Elements(*thisVar.datasize),2)
               text[3] = tab + 'DIMENSIONS:  [' + d + ']'
               IF (self.no_read_on_parse EQ 0) THEN BEGIN
                    IF  thisVar.datatype NE 'CHAR' THEN BEGIN 
                        text[4] = tab + 'MIN VALUE:  ' + StrTrim(thisVar.minValue,2)
                        text[5] = tab + 'MAX VALUE:  ' + StrTrim(thisVar.maxValue,2)
                    ENDIF
               ENDIF
                      
               Widget_Control, self.textDisplay, Set_Value=text
           ENDFOR    
           END
                  
      'FILENAME': BEGIN
             text = StrArr(3)
             text[0] = 'FILENAME:'
             text[1] = tab + self.filename
             Widget_Control, self.textDisplay, Set_Value=text
             END
                  
      'DIRECTORY': BEGIN
                  text = StrArr(3)
                  text[0] = 'DIRECTORY:'
                  text[1] = tab + self.directory
                  Widget_Control, self.textDisplay, Set_Value=text
                  END
                  
       'SUMMARY': BEGIN
         text = StrArr(3)
         text[0] = 'DIRECTORY:'
         text[1] = tab + self.directory
         si = 3
         
         text = [text, strArr(3)]
         text[3] = 'FILENAME:'
         text[4] = tab + self.filename
         si = si + 3
         
         IF Ptr_Valid(self.theAttributes) THEN BEGIN
            attr = *self.theAttributes
            FOR j=0,N_Elements(attr)-1 DO BEGIN
               thisAttr = attr[j]
               IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
               ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext, /STRUCTURE
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                aString = StrMid(helptext, 3)
                parts = StrSplit(aString, '=', /Extract)
                lastPart = StrCompress(parts[1], /REMOVE_ALL)
                IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                    IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                        lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                    ENDIF ELSE BEGIN
                        lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                    ENDELSE
                ENDIF
                aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                text[si+1] = aString
                si = si + N_Elements(helptext) + 2
            ENDELSE
            ENDFOR
         ENDIF
         
         Widget_Control, self.textDisplay, Set_Value=text
         END
         
      'VARIABLE ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_UValue=value
         varname = value[0]
         name = value[1]
         vars = *self.theVariables 
         i = Where(vars.name EQ varname, count)
         thisVar = vars[i]
         var_attributes = *thisVar.var_attributes
         i = Where(var_attributes.name EQ name, count)
         IF count GT 0 THEN BEGIN
            text = StrArr(3)
            thisAttr = var_attributes[i]
            IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
             ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext, /STRUCTURE
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                aString = StrMid(helptext, 3)
                IF Size(var, /TNAME) NE 'STRUCT' THEN BEGIN
                    parts = StrSplit(aString, '=', /Extract)
                    lastPart = StrCompress(parts[1], /REMOVE_ALL)
                    IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
                        IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
                            lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
                        ENDIF ELSE BEGIN
                            lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
                        ENDELSE
                    ENDIF
                    aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
                    text[si+1] = aString
                    si = si + N_Elements(helptext) + 2
                ENDIF ELSE BEGIN
                  text[si + 1] = '      ' + 'Structure Variable:'
                  FOR kk=2,N_Elements(aString) DO BEGIN
                    text[si + kk] = '      ' + aString[kk-1]
                  ENDFOR
                ENDELSE
            ENDELSE
            Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
	; stapel groups included
	'SUBGROUP ATTRIBUTE': BEGIN
		si = 0
		vars    = *self.theVariables 
		Widget_Control, event.id, Get_UValue=uname
		for un = 0 , n_elements(uname) -3 do begin
			i = Where(vars.name EQ uname[un], count)
			vars = *(vars[i].GROUP_VARIABLES)
		endfor
		i = Where(vars.name EQ uname[un], count)
		thisVar = vars[i]
		var_attributes = *thisVar.var_attributes
		i = Where(var_attributes.name EQ uname[un+1], count)
		IF count GT 0 THEN BEGIN
			text = StrArr(3)
			thisAttr = var_attributes[i]
			IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
				str = TextLineFormat(*thisAttr.value, LENGTH=80)
				lines = N_Elements(str)
				text = [text, StrArr(2 + lines)]
				text[si] = StrUpCase(thisAttr.name) + ':'
				text[si+1:si+1+(lines-1)] = tab + str
				si = si + 2 + lines
			ENDIF ELSE BEGIN
				var = *thisAttr.value
				Help, var, OUTPUT=helptext, /STRUCTURE
				text = [text, StrArr(N_Elements(helptext) + 2)]
				text[si] = StrUpCase(thisAttr.name) + ':'
				aString = StrMid(helptext, 3)
				IF Size(var, /TNAME) NE 'STRUCT' THEN BEGIN
					parts = StrSplit(aString, '=', /Extract)
					lastPart = StrCompress(parts[1], /REMOVE_ALL)
					IF StrUpCase(lastPart) EQ 'ARRAY[2]' THEN BEGIN
						IF Size(var, /TNAME) EQ 'BYTE' THEN BEGIN
							lastpart = '[' + StrTrim(Fix(var[0]),2) + ', ' + StrTrim(Fix(var[1]),2) + ']'                    
						ENDIF ELSE BEGIN
							lastpart = '[' + StrTrim(var[0],2) + ', ' + StrTrim(var[1],2) + ']'
						ENDELSE
					ENDIF
					aString = '   ' + StrCompress(parts[0],/REMOVE_ALL) + ' = ' + lastPart
					text[si+1] = aString
					si = si + N_Elements(helptext) + 2
				ENDIF ELSE BEGIN
					text[si + 1] = '      ' + 'Structure Variable:'
					FOR kk=2,N_Elements(aString) DO BEGIN
						text[si + kk] = '      ' + aString[kk-1]
					ENDFOR
				ENDELSE
			ENDELSE
			Widget_Control, self.textDisplay, Set_Value=text
		ENDIF ELSE Message, 'Cannot find global attribute ' + name
	END

	'SUBGROUP VARIABLE': BEGIN
		Widget_Control, event.id, Get_Value=name
		Widget_Control, event.id, Get_UValue=uname
		vars = *self.theVariables 
		for un = 0 , n_elements(uname) -2 do begin
			IF N_Elements(vars) EQ 0 THEN vars = vars[0]
			i = Where(vars.name EQ uname[un], count)
			vars = *(vars[i].GROUP_VARIABLES) 
		endfor
		IF N_Elements(vars) EQ 0 THEN vars = vars[0]
		i = Where(vars.name EQ uname[un], count)
		FOR k=0,count-1 DO BEGIN
			thisVar = vars[i[k]]
			text = StrArr(6)
			text[0] = tab + 'NAME: ' + name
			text[1] = tab + 'DATATYPE: ' + thisVar.datatype
			n = StrTrim(N_Elements(*thisVar.datasize),2)
			f = ' (' + n + '(I0, :, ", "))'
			d = String(*thisVar.datasize, FORMAT=f)
			text[2] = tab + 'N_DIMENSIONS:  ' + StrTrim(N_Elements(*thisVar.datasize),2)
			text[3] = tab + 'DIMENSIONS:  [' + d + ']'
			IF (self.no_read_on_parse EQ 0) THEN BEGIN
				IF  thisVar.datatype NE 'CHAR' THEN BEGIN 
					text[4] = tab + 'MIN VALUE:  ' + StrTrim(thisVar.minValue,2)
					text[5] = tab + 'MAX VALUE:  ' + StrTrim(thisVar.maxValue,2)
				ENDIF
			ENDIF
			Widget_Control, self.textDisplay, Set_Value=text
		ENDFOR    
	END
         
         ; stapel; wenn auf bereich ohne tree selection geklickt wird wird fehlermeldung ausgespuckt
         ; das soll hier abgefangen werden, weil es nervt und unnötig ist. 
         ELSE: 	Begin
		if nodename eq 'theTree' then begin
		; just don't need to tell me!
		endif else Message, 'Unexpected event from ' + nodename + '. Please investigate.'
		END
   ENDCASE

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::CLEANUP
   ; This is the main cleanup routine for the object. Delete all created pointers.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theCalibration
   Ptr_Free, self.theSwaths
   Ptr_Free, self.theVariables
   Ptr_Free, self.zeroDimensionID

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::INIT, filename, $
   BROWSE=browse, $
   DESTROY_FROM_BROWSER=destroy_from_browser, $
   EXTENSION=extension, $
   NO_READ_ON_PARSE=no_read_on_parse, $
   NO_NEW_FILE=no_new_file, $
   debug=debug

   ; Error handling. Return 0 if can't finish.
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF

   !EXCEPT= (keyword_set(debug) ? 2 : 0)

   device, get_decompose = decomp
   self.decompose = decomp

   ; Check parameters.
   IF N_Elements(filename) NE 0 THEN BEGIN
      IF File_Test(filename, /READ) EQ 0 THEN begin
	print, 'Specified file does not exist or is not readable. '+filename
; 	Obj_Destroy, self
	RETURN, 0
      endif

      basename = File_BaseName(filename)
      directory = File_DirName(filename, /MARK_DIRECTORY)
      IF directory EQ '.\' OR directory EQ './' THEN CD, Current=directory
      self.filename = basename
      self.directory = directory
   ENDIF

   IF N_Elements(extension) EQ 0 THEN extension = '*.nc;*.ncd;*.ncdf;*.hdf,*.h5'
   ; stapel (12/2014)
   ; this is a workaround for an IDL 8.2 bug causing segmentation faults ------
   ; for ncdf4 files with path+filename length ge 273 characters
   ; (Note ncdf4 will be recognized as hdf5! and ncdf)
   if strlen(self.directory+'/'+self.filename) ge 273 and H5F_IS_HDF5(self.directory+'/'+self.filename) then begin
	print,'String length (self.directory+"/"+self.filename) too long ('+strcompress(strlen(self.directory+'/'+self.filename),/rem)+$
		' ge 273) (IDL 8.2 ncdf4 bug!)! Start Workaround with cd self.directory and set self.directory to ./'
	cd , self.directory,current = current
	self.current_dir = current
	self.directory = './'
   endif
   ;---------------------------------------------------------------------------
   ; Set other object properties
   self.destroy_from_browser = Keyword_Set(destroy_from_browser)
   self.extension = extension
   self.no_read_on_parse = Keyword_Set(no_read_on_parse)

   ; Browse now?
   success = 1
   IF Keyword_Set(browse) THEN self -> Browse, SUCCESS=success
   IF success EQ 0 THEN BEGIN
       Obj_Destroy, self
       RETURN, 0
   ENDIF

   ; Determine if this is a netCDF or HDF file.
   self.isHDF  = HDF_ISHDF(Filepath(ROOT_DIR=self.directory, self.filename))
   self.isHDF5 = H5F_IS_HDF5(self.directory+'/'+self.filename)
   if self.isHDF5 then begin
	if is_ncdf(self.directory+'/'+self.filename) then self.isHDF5 = 0
   endif

   ; set default color table for plots
   self.ctab = 'Default (Rainbow)'
   ; set default projection
   self.proj = 'Default'
   
   RETURN, 1

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_ATTRIBUTE__DEFINE

   struct = { NCDF_DATA_ATTRIBUTE, $
              attrtype: "", $
              datatype: "", $
              length: 0L, $
              name: "", $
              value: Ptr_New() }
              
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_DIMENSION__DEFINE

   struct = { NCDF_DATA_DIMENSION, $
              name: "", $
              value: "" }       ; Length or UNLIMITED.

END ;---------------------------------------------------------------------------------------------

; stapel including groups
PRO NCDF_DATA_VARIABLE__DEFINE

   struct = { NCDF_DATA_VARIABLE, $
              datasize: Ptr_New(), $
              datatype: "", $
              minValue: 0.0D, $
              maxValue: 0.0D, $
              name: "", $
              group_variables: Ptr_New(), $
              var_attributes: Ptr_New(), $
              calibration: Ptr_New(), $
              value: Ptr_New() }

END;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_SWATH__DEFINE

   struct = { NCDF_DATA_SWATH, $
              name: "", $
              nattrs: 0L, $
              ndims: 0L, $
              ngeofields: 0L, $
              ndatafields: 0L, $
              nmaps: 0L, $
              nidxmaps: 0L, $
              attributes: Ptr_New(), $
              dimensions: Ptr_New(), $
              geofields: Ptr_New(), $
              datafields: Ptr_New(), $
              maps: Ptr_New(), $
              idxmaps: Ptr_New() }

END;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_CLEANUP, tlb

   Widget_Control, tlb, GET_UVALUE=self
   IF self -> Destroy_From_Browser() THEN Obj_Destroy, self
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_EVENTS, event

   Widget_Control, event.TOP, GET_UVALUE=self
   self -> EventHandler, event
   
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA__DEFINE, class

   class = { NCDF_DATA,                $  ; The object class NCDF_DATA.
             appendNameID: 0L,         $  ; The button for appending filenames to variables.
             attributeID: 0L,          $  ; The widget containing the attribute list.
             attrNameID: 0L,           $  ; The widget containing the main-level attribute name.
             filename: "",             $  ; The filename of the netCDF or HDF file.
             file2:"",                 $  ; Filenamedummy for plot_diff
             satname:"",               $  ; 
             algoname:"",              $  ; 
             reference:"",             $  ; 
             destroy_from_browser: 0B, $  ; A flag to indicate the object is destroyed if brower is destroyed.
             directory: "",            $  ; The directory the file is located in.
             extension: '',            $  ; The file extension for FILTER keyword in DIALOG_PICKFILE.
             geoDisplay: {WIDGET_GEOMETRY}, $ ; Widget geometries for calculating resizeable windows.
             geoWindow: {WIDGET_GEOMETRY}, $
             geoButton: {WIDGET_GEOMETRY}, $
             geoTree: {WIDGET_GEOMETRY}, $
             hasBeenParsed: 0B,        $  ; A flag to indicate if the file has been parsed.
             no_read_on_parse: 0B,     $  ; A flag to indicate that the variables should not be read while parsing.
             isHDF: 0B,                $  ; A flag to indicate this is an HDF4 file instead of a netCDF file.
             isHDF5: 0B,                $  ; A flag to indicate this is an HDF5 file instead of a netCDF file.
             minXSize: 0L,             $  ; Minimum X size of the Browser window.
             minYSize: 0L,             $  ; Minimum Y size of the Browser window.             
             textDisplay: 0L,          $  ; The widget where text information is displayed
             theAttributes: Ptr_New(), $  ; An array of global attribute structures.
             theDimensions: Ptr_New(), $  ; An array of dimension structures.
             theCalibration: Ptr_New(),$  ; An array of calibration structures for the HDF SD variable.
             zeroDimensionID: Ptr_New(), $ ; A pointer to the dimension IDs whose current size is 0.
             theSwaths: Ptr_New(),     $  ; A pointer to an array of swath structures.
             theTree: 0L,              $  ; The tree widget ID.
             theVariables: Ptr_New(),  $  ; An array of variable structures.
             tlb: 0L,                  $  ; The TLB of the browser window.
;              tlb2: 0L,                 $      
             varpluslistID: 0L,        $  ; The list of variable plus attributes to be read.
             varplusnameID: 0L,        $  ; The widget containing the main-level variable name.
             variablelistID: 0L,       $  ; The list of variables available to be read.
             ctlistID: 0L,             $  ; The List of colornames
             varnameID: 0L,            $  ; The widget containing the main-level variable name.
;              varname_plotID: 0L,            $ ; The widget containing the plot-level variable name.
             varname_plotID: "",       $ ; The widget containing the plot-level variable name.
             minimumID: 0L,            $  ; The widget containing the main-level minimum.
             maximumID: 0L,            $  ; The widget containing the main-level maximum.
             winnrID:0l,               $  ; The widget containing the main-level plot window number.
             pmultID:0l,               $  
             current_dir:'',           $
             level:'',                 $
             satnode:'',               $
             datum:'',                 $
             year:'',                  $
             month:'',                 $
             day:'',                   $
             orbit:'',                 $
             yearID:'',                $
             monthID:'',               $
             dayID:'',                 $
             levelID:'',               $
             symsizeID:'',             $
             PsymSize:0.,              $
             symbolID:'',              $
             axquotID:'',              $
             lalgID:'',                $
             handlebar:'',             $   
             horizontal:'',            $    
             vertical:'',              $  
             year_idx:0L,              $
             month_idx:0L,             $
             day_idx:0L,               $
             level_idx:0L,             $
             leveltype:'',             $
             daytype:'',               $
             orbID:'',                 $
             zkompID:'',               $
             dim3:0L,                  $
             magniID:'',               $
             magnify:0,                $
             rotateID:'',              $
             rot:0.,                   $
             histct:'',                $
             other:'',                 $
             ctab:'',                  $
             hct:'',                   $
             wbgrID:0L,                $
             version:'',               $
             selftxt:'',               $
             pixvalID:0L,              $
             bordID:0L,                $
             verbID:0L,                $
             noTitleID:0L,             $
             noBarID:0L,               $
             invID:0L,                 $
             enablelim:0l,             $
             enablemima:0l,            $
             globalID:0L,              $
             northhmID:0L,             $
             southhmID:0L,             $
             allpixID:0L,              $
             seapixID:0L,              $
             landpixID:0L,             $
             antarcID:0L,              $
             midlatsID:0L,             $
             tropicID:0L,              $
             midlatnID:0L,             $
             arcticID:0L,              $
             pm70ID:0L,                $
             zoomID:0L,                $
             limitID:0l,               $
             p0lonID:0l,               $
             p0latID:0l,               $
             projlist:'',              $
             proj:'',                  $
             drawID:0L,                $
             showpvalID:"",            $
             yy:'',                    $
             mm:'',                    $
             dd:'',                    $
             whatever:'',              $
             compare_algo1:'',         $
             logid:0L,                 $
             sat_base:0L,              $
             tirosn:0L,                $
             noaa5:0L,                 $
             noaa6:0L,                 $
             noaa7:0L,                 $
             noaa8:0L,                 $
             noaa9:0L,                 $
             noaa10:0L,                $
             noaa11:0L,                $
             noaa12:0L,                $
             noaa14:0L,                $
             noaa15:0L,                $
             noaa16:0L,                $
             noaa17:0L,                $
             noaa18:0L,                $
             noaa19:0L,                $
             metopa:0L,                $
             metopb:0L,                $
             msg:0L,                   $
             aqua:0L,                  $
             envisat:0L,               $
             ers2:0L,                  $
             aatsr:0L,                 $
             aatme:0L,                 $
             noaaAM:0L,                $
             noaaPM:0L,                $
             terra:0L,                 $
             avhrrs:0L,                $
             modises:0L,               $
             allsat:0L,                $
             refgac:0L,                $
             refmod:0L,                $
             refgac2:0L,               $
             refmod2:0L,               $
             refcal:0L,                $
             refgwx:0L,                $
             refisp:0L,                $
             refcci:0L,                $
             refcci2:0L,               $
             loaded:0l,                $
             refera:0l,                $
             refera2:0l,               $
             refg2gwx:0L,              $
             refpmx:0L,                $
             refpmx2:0L,               $
             refl1gac:0L,              $
             refhec:0L,                $
             refcla:0L,                $
             refself:0L,               $
             refselect:0L,             $
             refnone:0L,               $
             refmyd:0L,                $
             refmyd2:0L,               $
             pcsingle:0L,              $
             pcmulti:0L,               $
             pcvar:0L,                 $
             pcmat:0L,                 $
             pcts:0L,                  $
             pchist:0L,                $
             out_hovmoeller:Ptr_New(), $
             pczm:0L,                  $
             pcdts:0L,                 $
             pcmts:0L,                 $
             pcmatts:0L,               $
             pcms:0L,                  $
             pchov:0L,                 $
;              geoPlotWin:{WIDGET_GEOMETRY}, $
             georow:{WIDGET_GEOMETRY}, $
             geoButtrow:{WIDGET_GEOMETRY},$
             geoToprow:{WIDGET_GEOMETRY},$
             geodraw:{WIDGET_GEOMETRY},$
             minxs:0l,                 $
             minys:0L,                 $
             decompose:1L,             $
             pmulti:'',                $
	     pmulti_default:'',        $
             oplotID:0l,               $
	     oplotnr:0l,               $
;              map_objout:obj_new(''),   $
             map_objout:obj_new('IDL_Container'),   $
	     draw:0L,                  $
             errID:0L,                 $
             saveID:0L                 $
           }

END ;---------------------------------------------------------------------------------------------
