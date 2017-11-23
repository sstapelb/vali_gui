;$Id: get_dem.pro,v 1.1 2002/06/10
;
;+
; NAME:
;       GET_DEM
;
; PURPOSE:
;   This function reads terrain heights from a digital elevation model and
;   interpolates the results to the desired Longitude / Latitude values.
;   The original resolution of the DEM data is 1 / 120 [Degree / Pixel].
;   The resolution with which the data is read into memory is chosen
;   in such a way not to consume too much main memory.
;   However, a higher resolution for reading the original data can be
;   set with the RESOLUTION keyword.
;
; CALLING SEQUENCE:
; changed to lon,lat (stapel)
;   dem = get_dem(lon, lat)
;
; INPUTS:
;   LAT, LON: Geographic latitude and longitude of grid points for
;             which the terrain height is to be calculated. LAT and LON
;             can be either scalars, vectors or arrays. The way the original
;             data is interpolated to the grid points can be defined with
;             the INTERPOLATE keyword. Default is to use BILINEAR
; KEYWORD PARAMETERS:
;   RESOLUTION: Can be used to set the resolution in Degree / Pixel with
;             with which the original DEM data is read into memory.
;             Default is to use 1000 Pixel for the maximum of Longitude or
;             Latitude difference
;   EFF_RES: This keyword returns the effective resolution in Degree / Pixel
;             which was used to read the DEM data
;   INTERPOLATE: If set, the built-in "interpolate"  routine is used for
;             interpolation of the original dem-data to the specified
;             lat/lon values. Default is using the "bilinear" function.
;             If two vectors are given for LAT and LON, the bilinear
;             functions returns an array with the dimensions
;             [n_elements(lon), n_elements(lat)].
;   DEBUG: If set, the program gives some information on the size of the
;             used arrays and stops before returning the result
;
; OUTPUT: Terrain height at the specified lat/lon points in Meter.
;             Ocean areas have been assigned a value of 0.
;             Lowland coastal areas have an elevation of at least 1 meter, thus the
;             data can be used aas a land-sea mask
;
;
; CAVEATS:
;   When called for the first time, it can take a while until IDL can handle
;   the large ncdf-file.
;
; Acknowledgements:
;   The digital elevation map is taken from the GTOPO30 maps available at
;   http://edcdaac.usgs.gov/gtopo30/gtopo30.html
;
; DATA:
;    The routine needs the file /cmsaf/cmsaf-cld1/sstapelb/elevation/dem.nc
;
; EXAMPLE:
;;   Global land-sea-mask:
;    lon = ((intarr(500) + 1) ## indgen(500)) / 500. * 359. - 180
;    lat = 90. - (indgen(500) ## (intarr(500) + 1)) / 500. * 179.
;    dem = get_dem(lat,lon, eff_res = eff_res)
;    print, 'Reading DEM data with ', eff_res, ' �/Pixel'
;    fub_image, dem, lon, lat, void_index = where(dem eq 0), /thick
;
;;   Or specify the resolution:
;    dem = get_dem(lat,lon, eff_res = eff_res, resolution = 1.)
;    print, 'Reading DEM data with ', eff_res, ' �/Pixel'
;    fub_image, dem, lon, lat, void_index = where(dem eq 0), /thick
;
; MODIFICATION HISTORY:
;   created 10.06.2002 P. ALbert
;   12.06.2002: Added default resolution and EFF_RES keyword (PA)
;   11.07.2002: Wenn called with scalar lon and lat values, the
;               calculated resolution was 0. Default is 1e-3 now. (PA)
;   19.02.2013: Added keyword grid_res, reads sav files of regular (stapel)
;               grids with resolution 1.,0.5,0.25 and 0.1 degrees, 
;               faster when using first time (no nc file reading necessary)
;-
;-------------------------------------------------------------------------------------------------------------------------
; shifts and / or rotates dem from sav files based on lon/lat info
function autoshift, array, longitude, latitude, grid_res = grid_res

	dum_array = array

	if keyword_set(longitude) and keyword_set(latitude) then begin
		x_shift = (-180. + grid_res/2. - longitude[0,0])
		y_shift = (- 90. + grid_res/2. - latitude [0,0])
		if x_shift ne 0 or y_shift ne 0 then print,'Autoshift DEM file by [x,y]: ['+string(x_shift)+','+string(y_shift)+']'
		dum_array = shift(array,x_shift,y_shift)
	endif

	return, dum_array

end
;-------------------------------------------------------------------------------------------------------------------------
function get_dem, longitude, latitude, $
	resolution = resolution, $
	eff_res    = eff_res, $
	interpolate = interpolate, $
	debug = debug, grid_res = grid_res, $
	found = found

	grid_resn = keyword_set(grid_res) ? grid_res : get_grid_res(longitude,cci_l3u_eu=cci_l3u_eu,cci_l3u_af=cci_l3u_af,claas=claas,found=found)

	if keyword_set(grid_resn) then begin
		dem = restore_var('/cmsaf/cmsaf-cld1/sstapelb/savs/dem/dem_regular_grid_'+$
		string(grid_resn,f='(f4.2)')+(keyword_set(interpolate)?'_interpolated':'')+'.sav',found=found)
		if found then begin
			dem = autoshift(dem, longitude, latitude, grid_res = grid_resn)
			return,dem
		endif else begin
			lon = vector((-180.+grid_resn/2.),(180.-grid_resn/2.),360./grid_resn) #  ( fltarr(180./grid_resn)+1)
			lat = vector((- 90.+grid_resn/2.),( 90.-grid_resn/2.),180./grid_resn) ## ( fltarr(360./grid_resn)+1)
		endelse
	endif else if keyword_set(cci_l3u_eu) then begin
		dem = restore_var('/cmsaf/cmsaf-cld1/sstapelb/savs/dem/dem_regular_grid_0.02_CCI_MODIS_europe_L3U.sav',found=found)
		if found then return,dem else begin
			if n_elements(longitude) eq 0 or n_elements(latitude) eq 0 then begin
				found = 0.
				return,-1
			endif else begin
				lon = longitude
				lat = latitude
			endelse
		endelse
	endif else if keyword_set(cci_l3u_af) then begin
		dem = restore_var('/cmsaf/cmsaf-cld1/sstapelb/savs/dem/dem_regular_grid_0.01_CCI_MODIS_west_africa_L3U.sav',found=found)
		if found then return,dem else begin
			if n_elements(longitude) eq 0 or n_elements(latitude) eq 0 then begin
				found = 0.
				return,-1
			endif else begin
				lon = longitude
				lat = latitude
			endelse
		endelse
	endif else if keyword_set(claas) then begin
		dem = restore_var('/cmsaf/cmsaf-cld1/sstapelb/savs/dem/dem_regular_grid_0.05_CLAAS.sav',found=found)
		if found then return,dem else begin
			if n_elements(longitude) eq 0 or n_elements(latitude) eq 0 then begin
				found = 0.
				return,-1
			endif else begin
				lon = longitude
				lat = latitude
			endelse
		endelse
	endif else begin
		found = 1.
		if n_elements(longitude) eq 0 or n_elements(latitude) eq 0 then begin
			found = 0.
			return,-1
		endif else begin
			lon = longitude
			lat = latitude
		endelse
	endelse
; Begrenzt die maximale Gr��e des einzulesenden Feldes
max_num = 1e9

; Korrektur 270� --> -90�
ilondum	= WHERE(lon GT 180.)
londum	= lon
if ilondum[0] ne -1 then londum[ilondum]=360.-londum[ilondum]

if not keyword_set(resolution) then begin
	resolution = ([(max(lon) - min(lon)) > (max(lat) - min(lat))] / 1000.)[0]
	if resolution eq 0 then resolution = 1e-3
endif
; Nur zur Sicherheit, falls jemand resolution = 1 (als integer) �bergibt
resolution = float(resolution)

dem_file = '/cmsaf/cmsaf-cld1/sstapelb/elevation/dem.nc'

; Das dem-Feld ist 43200 x 21600 Pixel gro�, mit einer Aufl�sung von
; 30 Bogensekunden, d.h. Ein Grad entspricht 120 Pixel.
dem_lon = findgen(43200l) / 120. - 180
dem_lat = 90. - findgen(21600l) / 120.

; Der Offset ist der index des n�chstkleineren L�nge-gridwert /
; des n�chstgr��eren Breite-gridwert
; stapel changed to round (problem mit 180° lon werten (nummerisches rauschen -180 + 180 nicht immer gleich Null!!))
; lon_offset = floor((min(lon[where(lon gt -999.)]) + 180) * 120.)
; lat_offset = floor((90 - (max(lat)))  * 120.)
lon_offset = round((min(lon[where(lon gt -999.)]) + 180) * 120.)
lat_offset = round((90 - (max(lat)))  * 120.)

; Und das sind die Werte f�r L�nge und Breite an diesem Punkt
min_lon = lon_offset / 120. - 180.
max_lat = 90 - lat_offset / 120.

; Die gew�nschte Aufl�sung beim Einlesen der Original-Daten entscheidet dar�ber,
; ob und wieviele Pixel beim Einlesen ausgelassen werden. Wenn die gew�nschte
; Aufl�sung kein ganzzahliges Vielfaches der Originalaufl�sung ist, wird die
; n�chstgr��ere Schrittweite gew�hlt
stride	= ceil(resolution * 120.) * [1,1]
eff_res = stride[0] / 120.

; Es m�ssen soviele Counts gelesen werden, da� der n�chstgr��ere
; L�ngen-gridpunkt / der n�chstkleinere Breiten-gridpunkt hinter
; den gr��ten / kleinsten L�nge- / Breitewerten liegt
lon_count = ceil((max(lon[where(lon gt -999.)]) - min_lon) / eff_res) + 1
lat_count = ceil((max_lat - min(lat[where(lat gt -999.)])) / eff_res) + 1
if (float(lon_count) * float(lat_count)) ge max_num then begin
	print, 'Warning: You are trying to read a very large array'
	help, lon_count, lat_count
	print, 'If this is o.k., then modify "max_num" in get_dem.pro'
	stop
endif

; hier wird getestet, ob ich nicht �bers Ziel hinausschie�e
; wieder auskommentiert (stapel 02/2013) weil um 1 übers Ziel hinaus bei einem avhrr orbit ; und wieder einkommentiert stapel hier stimmt was nicht!!
lon_count = (lon_count < (floor(n_elements(dem_lon)/float(stride[0]))-lon_offset))>1
lat_count = (lat_count < (floor(n_elements(dem_lat)/float(stride[1]))-lat_offset))>1

offset	= [lon_offset, lat_offset]
count	= [lon_count, lat_count]

; Einlesen der Daten aus dem ncdf-file
error=0
catch,error
if error eq 0 then begin
	id = hdf_sd_start(dem_file, /read)
	sds_id = hdf_sd_select(id, 2)
	hdf_sd_getdata, sds_id, dem, start = offset, count = count, stride = stride
	hdf_sd_end, id
endif else begin
	dem = fltarr(lon_count, lat_count)-9999
	help, /last_message, output = output
	ok = dialog_message(output)
endelse
catch,/cancel
; Floating-point damit das Interpolieren richtig klappt
dem = float(dem)

if total(count) eq 2 then return, dem else begin


	ss = size(dem)

	index_lon = (float(lon) - min_lon) / eff_res
	index_lat = (float(max_lat) - lat) / eff_res

	if keyword_set(debug) then begin
		id = hdf_sd_start(dem_file, /read)
		sds_id = hdf_sd_select(id, 0) ; Lon
		hdf_sd_getdata, sds_id, l1, start = offset[0], count = count[0], stride = stride[0]
		sds_id = hdf_sd_select(id, 1) ; Lat
		hdf_sd_getdata, sds_id, l2, start = offset[1], count = count[1], stride = stride[1]
		hdf_sd_end, id
		max_lon = 		(lon_offset + (lon_count - 1) * stride[0]) / 120. - 180
		min_lat = 90. -	(lat_offset + (lat_count - 1) * stride[1]) / 120.
		print, 'Min_lon: ', min_lon, min(l1)
		print, 'Max_lon: ', max_lon, max(l1)
		print, 'Min_lat: ', min_lat, min(l2)
		print, 'Max_lat: ', max_lat, max(l2)
		print, 'Offset: ', offset
		print, 'Count: ', count
		print, 'Stride: ', stride
		print, 'Effective res.: ', eff_res, ' Grad / Pixel'
		help, dem
;		print, 'Index_lon: ', (index_lon)
;		print, 'Index_lat: ',(index_lat)
		stop
	endif


	case 1 of
	(lon_count eq 1): return, interpolate(dem, index_lat)
	(lat_count eq 1): return, interpolate(dem, index_lon)
	else: begin
		if keyword_set(interpolate) then begin
				return, interpolate(dem, index_lon, index_lat)
		endif else 	return, bilinear(dem, index_lon, index_lat)
	end
	endcase
endelse

end
