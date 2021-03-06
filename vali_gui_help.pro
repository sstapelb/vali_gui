pro vali_gui_help
;+
; This is a help File for the Vali_Gui Tool!
;
; First of all: Wow someone is really still using IDL. Congratulations!
;
; If you have any questions about this tool write me an email : stefanstapelberg@posteo.de
;
; The vali_gui program is nested in the ncdf_data__define object of D. Fanning, which provides
; a nice browsing tool. The browser has been extended to browse hdf5 files as well was already  
; set up to browse hdf4 eos files. The main purpose and idea to this program is to provide an 
; easy and fast to use way to visualize the data and compare it to similar data sets. The main
; tools to plot are "map_image__define" by P. Albert and "view2d" by M. Reuter. I only applied 
; minor changes to make it work the way i wanted to. Some tools and even some plot reoutines
; (e.g. plot_taylor_diagram.pro) are taken from D. Fannings coyote libary. And there some more 
; small other tools used written by a "non-me" person! Thanks for this.
;
; There should be no limitations if you just want to plot the data of the file you just read in.
;
; There are some limitations for the comparison part, though:
; 
;	- It is mainly made to compare cloud variables 
; 	- It will only work at the "CMSAF" data storage environment
; 	- There will always be some maintenance necassary (believe me!)
;
; The comparison "Compare Variable" mainly follows only a few rules:
;	- Only the same or at least similar variables can be compared!
;	- We need to know where the dataset is located ("get_filename.pro")
;
; Main structure of tools:
; 	- All the plot routines can be found in plot_l3.pro
; 	- The main tools can be found in "validation_tool_box.pro"
; 	- Some other tools are loaded via vali_pre_compile file.
;	  
; Note ! All the tools can be used without the interface as well.
; 
; start using vali_gui
; 	- check out your directory and find vali_set_path.pro.template
; 		- make your changings and copy it to vali_set_path.pro
; 		- vali_set_path.pro is not under version control so all
; 		  your changes will be kept.
; 	- .r call (compile all) compiles all routines of vali_gui
; 	- plot_l3 (running this sets all sys_vars set in vali_set_path.pro) 
; 
; start GUI with : ncdf_browser or ncdf_browser, filename 
; 	- Click on the "?" on lower right to open this help file. 
;     Keep it open if yout want to read more.
; 	- Scroll through variable list and pick one variable to display 
; 	- Choose the button "Plot Variable" to enter the actual vali_gui 
; 	- Choose the button "Plot Variable" in the newly opended window to make a simple first plot. 
; 
; ...TBD
; ...
; ...
;
; --------------------------------------------------------------------------------------------------
; --TOC---------------------------------------------------------------------------------------------
; - Add new Algorithms to GUI and Plot Routines
; - Satname explanation (internal and GUI)
; --------------------------------------------------------------------------------------------------
; --------------------------------------------------------------------------------------------------
; 
; - Add new Algorithms to GUI and Plot Routines
;
;	give it a proper algoname and a shorter reference name 
; 
;	add to GUI : edit ncdf_data__define.pro
; 		- 1) edit NCDF_DATA::get_file_infos 
; 			- reads global attributes to get algonames, satnames, etc
; 			- If file is unknown, plot will still work but compare might not
; 		- 2) edit NCDF_DATA::PlotVariableFromGUI
; 			- here all the buttons and dropboxes,etc are defined
; 			- define here your identifier 
; 		- 3) edit NCDF_DATA::	get_info_from_plot_interface
; 
; 	Introduce it for the plot routines: edit validation_tool_box.pro
; 	edit:
; 		- 1) get_filename
; 		- 2) get_data
; 		- 3) get_product_name
; 		- 4) ref2algo
; 		- 5) algo2ref
; 		- 6) sat_name
; 		if new satellite is introduced then as well
; 		sat_ampm,noaa_primes,atsr_prime,modis_sats, etc
; 
; --------------------------------------------------------------------------------------------------
; 
; - Satname explanation
;  
; 	Names as follows : internal-name(GUI-name), e.g. 'noaa7(N07)' or 'aqua(MYD)'
; 
; 	Some satnames used internally are the 1) actual satellite names, some uses only the 2) Instruments to define the datasets, 
; 	then we have the 3) CCI L3S names
; 
; 	1) actual names of Satellites 
; 	'noaa7(N07)' ,'noaa9(N09)' ,'noaa11(N11)' ,'noaa14(N14)','noaa16(N16)','noaa18(N18)','noaa19(N19)'  ; AVHRR afternoon Satellites
; 	'noaa12(N12)','noaa15(N15)','noaa17(N18)' ,'metopa(MA)','metopb(MB)'                                ; AVHRR morning Satellites
; 	'aqua(MYD)'  ,'terra(MOD)'                                                                          ; MODIS satellites
;	'envisat(ENV)','ers-2(ERS)'                                                                         ; A(A)TSR satellites
; 
; 	2) Satellites/datasets defined by Instruments
; 	'noaaam(AM)'		: only prime AVHRR morning satellites   (see noaa_primes() in validation_tool_box.pro)
; 	'noaapm(PM)'		: only prime AVHRR afternoon satellites (see noaa_primes() in validation_tool_box.pro)
; 	'aatme(A/M)'		: defines FAME-C datasets; AATSR+MERIS synergy on Satellite Envisat
; 	'aatsr(ENV)'		: defines CCI AATSR dataset on Satellite Envisat
; 	'atsr2(ERS)'		: defines CCI ATSR2 dataset on Satellite ERS-2 (predecessor of AATSR on ENVISAT)
; 	'atsrs(ATs)'		: combines ATSR2 and AATSR into 1 dataset
; 
;	3) CCI L3S Satellites (S : super-collated monthly means only, combines Morning and Afternoon Sats)
; 	'avhrrs(AVs)'		: all avail. AVHRR Sats (NOAA-AM, NOAA-PM)
; 	'modises(MOs)'		: all avail. MODIS Sats (TERRA, AQUA)
; 	'allsat(ALL)'		: all avail. Sats  		(TERRA, AQUA, AATSR, NOAA-AM, NOAA-PM)
;- 
end
