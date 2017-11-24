; @./vali_gui/subtools/df_colorbar.pro
;$Id: map_image__define.pro,v 1.9 2005/04/26
;
;
;+
; NAME:
;     map_image
;
; PURPOSE:
;    Colour coded display of georeferences data
;
; BASIS:
;    This program is a mixture of Liam Gumley's IMAGEMAP, Rene Preusker's
;    FUB_IMAGE and Chris O'Dell's WIS_IMAGE. They all use MAP_SET,
;    CONVERT_COORD and and TV. What's new is that this one is object oriented,
;    allowing the easy overlay of different images and the zooming into the images.
;    Which doe not necessarily requires object oriented programs, but anyway.
;    Uses COLORBAR (by David Fanning) to draw the colorbar.
;    N.b: The (new) default behaviour of the program is to automaticaylly scale the
;    individual pixels such that the full plot window is covered. Use
;    MAGNIFY = 0 for just plotting the individual pixels. See keyword
;    description below.
;
; CALLING SEQUENCE:
;
;     THERE ARE A LOT OF EXAMPLES AT THE END OF THE PROGRAM!
;    CALL MAP_IMAGE_DEMO
;
;     m = obj_new("map_image", image, lat, lon)
;    obj_destroy, m
;
;     OR, after compilation of map_image__define.pro
;
;    map_image, image, lat, lon
;
; INPUTS:
;     IMAGE: 1- or 2-dimensional data field. Or (x, y, 3) or (n, 3)
;        data field for true colour images.
; LAT:
;     Array or vector of latitudes
; LON:
;    Array or vector of longitudes
;
; OPTIONAL KEYWORDS:
;    Generally, all keywords related to MAP_SET, MAP_GRID, etc. cann be passed
;    through via the _EXTRA keyword and are not all repeated here.
;    Most important may be the projection keywords like STEREO, SINUSOIDAL
;    and keywords like LONDEL, LATDEL, LIMIT or BOX_AXES.
;
;    Keywords specific to MAP_IMAGE_DEFINE are:
;
; NO_COPY:
;     Saves memory when set, but the original
;    arrays are lost for the calling routine.
;
; NO_PROJECT:
;    Surpresses image drawing directly when object is created.
;    This is useful when you e.g. want to modify the colour table
;    prior to projecting the data.
;
; NO_DRAW:
;    Only surpresses drawing of the projected data. Useful when plotting
;    several datasets subsequently in the same projection.
;
; NO_ERASE:
;    If set, the plot region is not erased. Necessary when plotting
;    several datasets subsequently in the same window. See examples below.
;
; NO_SCALE:
;       If set, the original data is not bytescaled.
;
; MINI, MAXI: If set, and NO_SCALE is not set, the original data is
;     bytescaled between the given MINI and MAXI values (in
;     order to use a 256 elements colour table). The same
;     values are used for the range of the colour bar.
;     The default behaviour (i.e. when neither NO_SCALE nor MINI nor MAXI)
;     are set, is to scale between the minimum and maximum values of
;     the input data (trying to get reasonable intervals and numbers)
;
; VOID_INDEX: If set to an index vector with respect to the input data,
;     longitude and latitude values, the appropriate pixels are masked out.
;
; VOID_COLOUR: The pixels masked out using VOID_INDEX will be set to gray. The
;     default RGB values for the used gray shade is [150,150,150] but can be
;     overwritten using VOID_COLOUR. VOID_COLOUR = 0 gives black pixels,
;     VOID_COLOUR = 255 gives white pixels.
;
; AUTOSCALE: If only a subset of the data is displayed using e.g. the LIMIT
;     keyword, it might be that scaling the data between the (global) minimum
;     and maximum values is not appropriate. Setting AUTOSCALE will scale the
;     data between the local minimum and maximum of the displayed subregion.
;     Overrides MINI and MAXI.
;
; P0LON, P0LAT, ROT:
;     Must be set as keywords in order to get passed through to map_set.
;
; BOTTOM and NCOLOR:
;     Are handled like in Dave Fannings COLORBAR.PRO
;
; WINDOW:
;    Set plot window
;
; REVERT_BACKGROUND:
;     If set, the back- and foreground colors are switched,
;    typically useful for creatring black on white plots
;    instead of the standard black background.
;
; CTABLE:
;    Set color table
;
; RAINBOW:
;     Use a predefined, colour enhanced rainbow colour table
;
; GREYSCALE:
;     Use greyscale colour table.
;
; ELEVATION
;    Use a 'elevation' type of color table, ranging from blue over green and
;    brown to white
;    Attention: Currently, only 79 colours are defined, so a call should be
;    combined with ", ncolors=79"
;
; BWR
;    Use a 'blue white red' colour table with negative values in blue, 0 in white
;       and positve values in red
;
; DISCRETE:
;    Use discrete colours.
;
; RED, GREEN, BLUE:
;     Specify red, green and blue values for discrete  colours.
;     N.B.: The number of elements of the RED, GREEN and BLUE
;     vectors must equal the number of elements of the DISCRETE
;    vector minus 1 (Assume DISCRETE set to [0, 10, 20], then 2 colours
;    are needed for the range 0 to 10 and 10 to 20).
;
; FLIP_COLOURS
;    If set, the colour bar is reverted, eg. is ranging from red to blue instead
;    of blue to red
;
; MAGNIFY
;    If plotting low resolution data, the individual pixels would appear on
;    the plot window as seperated pixels. The default behaviour is to use
;    DILATE for scaling the individual pixels such that a continuous image is
;    drawn. This behaviour can be surpressed by providing MAGNIFY = 0.
;    Enlarging the pixels can further be cotrolled by setting MAGNIFY to
;    either a scalar value greater than one or to a two-elements vector with
;    values greater than one. If a scalar is provided, the pixels are
;    increased by the same number of device pixels in in x- and y-direction.
;    Providing a vector leads to different scaling in x- and y-direction.
;    In any case, the individual pixels are increased by as many device pixels
;    as given by the appropriate scalar or vector elements' value(s).
;
; HORIZONTAL:
;    If set, a horizontal colour bar is plotted. Default is vertical.
;
; HISTOGRAM:
;    If set, a colour coded histogram of the displayed data is shown in an
;    extra window. The window number is actually the value given to HISTOGRAM,
;    e.g. setting HISTOGRAM = 4 will display the histogram in window 4.
;    Requires PLOT_HISTO and accepts further keywords to PLOT_HISTO via
;    _extra (e.g. BIN, PSYM, ...)
;
; DEBUG:
;    Print some debug information.
;
;----------------------------------------------------------------------------------------;
;
; OBJECT METHODS:
;
; MAP_IMAGE::COLORS
;     Set color table
;     KEYWORDS:
;         RAINBOW, GREYSCALE, CTABLE, DISCRETE
;
; MAP_IMAGE::WORLDVIEW
;    Copied from the GEOV.PRO, provided by Andrey Savtchenko of the MODIS
;    Data Support Team, this method shows the location of the data on a globe.
;
; MAP_IMAGE::project
;    Performs the actual projection of the data on the desired grid.
;     KEYWORDS:
;         NO_DRAW, NO_ERASE, AUTOSCALE
;
; MAP_IMAGE::display
;    Displays the projected data.
;    KEYWORDS:
;         HISTOGRAM
;
; MAP_IMAGE::rdpix
;    Executes RDPIX on the projected dataset
;
; MAP_IMAGE::colorbar
;    This method calls David Fannings COLORBAR.pro. Different to COLORBAR, the
;    default colour bar is vertical. You can set a keyword HORIZONTAL on order to get
;    a horizontal one.
;
; MAP_IMAGE::draw_border
;    Just draw a line around the draw area as this one sometimes gets lost ...
;
; MAP_IMAGE::zoom
;    Call this method in order to interactively zoom into an image.
;
; MAP_IMAGE::init
;    Creates a new object.
;    KEYWORDS:
;        NO_COPY, NO_PROJECT, DEBUG
;
; MAP_IMAGE::cleanup
;    Dereference pointers etc.
;
; MAP_IMAGE::message
;    Internal routine for debug information
;
; MAP_IMAGE::remove_tag
;    Internal routine for the handling of structures
;
; MAP_IMAGE::set_range
;    Internal routine which does a first guess for nicely looking values for the colour bar.
;
; MAP_IMAGE::set_limit
;    Internal routine which sets a first-guess limit for the map projection.
;
; MAP_IMAGE::decomposed
;    Sets DEVICE, decomposed = 0 for 8 bit images and decomposed = 1 for true colour images
;
; MAP_IMAGE::limit428
;    map_image expects the LIMIT keyword to MAP_SET to be an 8-element vector. This routines
;    converts a given 4-element vector into an 8-element vector.
;
; MAP_IMAGE::set_one_var
;    Internal method for structure handling. All keywords passed to map_image via _EXTRA
;    are stored in an internal structure.
;
; MAP_IMAGE::set_var
;    Internal method for structure handling. All keywords passed to map_image via _EXTRA
;    are stored in an internal structure.
;
; MAP_IMAGE::get_var
;    Return values of internal structure elements.
;
; MAP_IMAGE::debug
;    Can be used for debug purposes in order to stop within a method (and get access to
;    object variables).

;----------------------------------------------------------------------------------------;
; EXTERNAL SUBROUTINES
;    win.pro
;    colorbar.pro
;    plot_histo.pro (if keyword HISTOGRAM is set at map_image__display)
;
;----------------------------------------------------------------------------------------;
;
; CAVEATS:
;    MAP_IMAGE works on 8 bit colour mode, i.e. you should set
;    DEVICE, PSEUDE=8 after you started IDL!
;
;----------------------------------------------------------------------------------------
; -
;
; Modification history:
; Created by Peter Albert, 23.9.2004
;
; 14.10.2004    PA    map_image::colors now checking for keyword "discrete" after all other
;                                       keywords -> discrete works together with "ctable"
; 01.12.2004    PA    map_image::colors now sets self.var.draw.maxi if max(image) or the value
;                                       provided via "MAXI= " is lower than max(discrete)
;                                       It also checks ranges when the colour index (idx) is
;                                       set.
; 13.12.2004    PA    map_image::init and ::cleanup: now restoring original values
;                                       of !p.color and !p.background
;
;                                       The background keyword to dilate does not what I think, so
;                                       magnifying the image plotting black on white (revert_background)
;                                       did not work. I am now setting all pixels to 0 at the start (::project)
;                                       and do not use the background keyword with dilate (::display). The
;                                       remaining black pixels are set to !p.background afterwards.
; 28.12.2004    PA    map_image::project: Consistent use of self.var.draw.xoffset instead of corners[0] should
;                                       now avoid the fact that sometimes the map_set frame was erased by the image.
;                                       Additionally, ceil and floor is now used when calculating size and offset
;                                       from normalized device coordinates.
;
; 29.12.2004    PA    colorbar / ex12: The new example ex12 shows how data with logarithmic scaling
;                                       can be displayed. Attention: The subroutine colorbar.pro
;                                       was modified!
;
; 05.01.2005    PA    colorbar / ex12: The updated example 12 illustrates how logarithmic scaling can be achieved
;                                       without modifications of colorbar.pro.
;                     map_image::project: There was a confusing offset of 2 at the numbers of used colors when
;                                       bytescaling the data. Removed.
;
; 05.01.2005    PA/JS map_image::colors: Added colour table "maxcolors"
;
; 10.01.2005    PA    map_image::colors: When keywords discrete and red are set, only set r[0] = 0 (and g, b)
;
; 11.01.2005    PA    map_image::project / map_image::colors: Going back to saving 2 colours for background and void-color and 1 for
;                                       the foreground colour. Using a maximum of 253 colours for the image.
;                                       Avoid error messages when displaying byte data with a negative user-defined minimum value
;                     map_image:draw_border: Added routine
;
; 12.01.2005    PA    map_image::project: Corected small bug with respect to size and offset of plot region
;                     map_image__define: Changed type of self.var.draw.color / background from byte to long
;                     map_image::project: No bytscl when displaying true colour images
;                     map_image::display: No setting of background colour when displaying true colour images
;
; 13.01.2005    PA    Corrected some bugs with regard to true colour images, magnify and postscript output
;
; 24.01.2005    PA    map_image::colors: Renamed keyword "maxcolors" to "extended_rainbow" in order to avoid
;                                       conflicts with self.var.draw.maxi (When keyword "MAX) was set
;                                       it was regognised for self.var.draw.maxi and for "maxcolors"
;
; 27.01.2005    PA    map_image::colors: With discrete colours, the colours were sometimes defined such that
;                                       colorbar.pro sometimes showed the steps at a slightly wrong position.
;                                       Workaround with setting the number of colours.
;                                       No revert_background for postscript
;
; 28.01.2005    PA    map_image::set_one_var: Call set_range for new image only when mini and maxi are not yet set
;                     map_image::remove_tag: The new structure was built from *self.var.internal._extra instead from struct_in
;
; 01.02.2005    PA    map_image::init, map_image::colorbar: When called with keyword "bar_horizontal=0", a
;                                       horizontal colourbar was drawn anyway. Fixed
;                     map_init::project: Added keyword no_scale (controlled by self.var.draw.no_scale)
;
; 11.02.2005    PA    map_image::colors: Added keyword flip_colours
;
; 24.02.2005    PA    map_init::colours: Added keyword "elevation"
;
; 01.03.2005    PA    Added map_image::rdpix
;
; 03.03.2005    PA    map_image::set_limit: Improved calculation of default limits
;                                       for map_set
;                     map_image::colors: Is it a bug or is it a feature? When called for the first time
;                                       TVLCT modifies !p.color. It will now be set back t the original
;                                       value.
; 17.03.2005    PA    map_init::colours: Added keyword "bwr"
; 15.04.2005    PA    map_init::project: Check for NaNs from convert_coord before proceeding
; 26.04.2005    PA    map_init::project: When drawing true colour images using /REVERT, areas where all three
;                                        channels are equal to zero were drawn in white. Now, the minimum value
;                                        for all three channels is set to one.
; 19.07.2005    PA    map_init::project/display: Added _extra = _extra to calls to win. Allows setting XSIZE and
;                                        YSIZE in calls to map_image
; 12.08.2005    PA    map_image::colors: When keyword BWR was set together with NCOLORS and / or BOTTOM,
;                                        white was not at the 0 value
;                     map_image::project: When the keyword REVERT_BACKGROUND was abbreviated to REV, it was interpreted
;                                        as REVERSE by the first call to map_set, and the image was flipped around the
;                                        x-axis
; 18.08.2005    PA    map_image::display: deleted /horiz from call to map_grid
; 02.09.2005    PA    map_image::worldview: Crashed when lon / lat data was
;                                        1D. Not fixed but no execution in
;                                        this case.
; 25.10.2005    PA    map_image::get_var: Added function
; 27.10.2005    PA/MR map_image::project: Added automated scaling for
;                                displayed sub-area
; 28.10.2005    PA               modified approach such that the keyword
;                                AUTOSCALE is used instead of testing whether
;                                MINI and MAXI are explictely set.
; 31.10.2005    PA    map_image::project: AUTOSCALE now also works correctly
;                                when void_index is set. And void_index also
;                                works with true colour images.
; 31.10.2005    MR    map_image::display: Added auto_magnify functionality
;                                (not yet tested so much)
; 01.11.2005    PA    map_image::display: Made auto-magnify the default
;                                behaviour (map_image::init) and deleted the
;                                internal variables magnify_x and
;                                magnify_y. Instead, MAGNIFY can be set to
;                                either a scalar or 2-elements vector. Can
;                                also be set to 0 for surpressing auto-magnify.
; 01.11.2005    MR    map_image::Added !p.multi functionality (not yet tested for EPS)
; 02.11.2005    PA    map_image::project: Added some lines of code for erasing
;                                the plot window if and only if !p.multi and /NOERASE
;                                are not set.
;                     map_image::init: Added xmargin and ymargin to the plot
;                                command --> !x.window and !y.window in
;                                map_image::position really cover the whole
;                                window.
; 03.11.2005    PA    map_image::colorbar: colorbar was called with
;                                ncolors = self.var.draw.colors -1, which I
;                                think was a workaround for a colour bug which
;                                later was solved elsewhere .... Using
;                                e.g. tek_color and ncol = 5 with map_image
;                                showed that one colour was missing in the
;                                colour bar. Fixed now.
; 04.11.2005    PA    map_image::project and ::display: First,
;                                self.var.draw.magnify now is defined to be
;                                half the device pixel distance between image
;                                pixels, as dilate will fill the gap "from
;                                both sides". The structuring element for
;                                dilate was modified accordingly in order to
;                                always have an odd number in both
;                                dimensions. Anyway, using one large
;                                structuring element showed to override pixels
;                                if the structuring element was chosen too
;                                large, so two for loops for dilating in x-
;                                and y-direction with structuring elements
;                                being [1,1,1] and transpose([1,1,1]) are used
;                                subsequently. Which by the way is faster for
;                                large magnify values.
; 25.11.2004   PA    map_image::display: Addes keyword HISTOGRAM
; 25.11.2004   MR    map_image::display: Bug fix to keyword HISTOGRAM
; 02.01.2005   MR    map_image::display: Addes keyword CONTOUR
;
;----------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------
;=============================================================================
; 	baro (from fub_image.pro , R. Preusker)
;=============================================================================
function map_image::baro, p

	z=18388.*alog10(1013./(p > 1e-6))

	return, z
end
; ====================================================================================================
; Set predefined colors
; ====================================================================================================

pro map_image::colors, $
             rainbow = rainbow, $
             extended_rainbow = extended_rainbow, $
             greyscale = greyscale, $
             elevation = elevation, $
             bwr = bwr, $
             ctable = ctable, $
             discrete = discrete, $
             red = red, green = green, blue = blue, $
             revert_background = revert_background, $
             flip_colours = flip_colours, $
             brewer = brewer, $
             ctp = ctp, $
             cth = cth, $
             panoply = panoply

 
  if keyword_set(brewer) then begin
      ctable = keyword_set(ctable) ? ctable : 4
      if ctable lt 0 then begin & ctable = abs(ctable) & flip_colours = 1 & end
      self -> message, "[map_image::colors]: Brewer Color table " + string(ctable, format = '(i2)')
      file = !BREWER_CT_FILE
    ;       CTLoad, ctable, /BREWER, RGB_TABLE=ct
;       r = [0, reform(ct[*,0]), 255] & g = [0, reform(ct[*,1]), 255] & b = [0, reform(ct[*,2]), 255]
;       color_save = !p.color & tvlct, r, g, b & !p.color = color_save
;       free, ctable
  endif

  if keyword_set(rainbow) then begin
      self -> message, "[map_image::colors]: Rainbow color table"
      r = interpol([  0,   0,   0,   0,    0,  200, 255, 255, 255, 180, 100], indgen(11), findgen(254) / 253 * 10)
      g = interpol([  0,   0, 125, 255, 255,  255, 255, 125,   0,   0,    0], indgen(11), findgen(254) / 253 * 10)
      b = interpol([125, 255, 255, 255,    0,    0,  0,   0,   0,   0,    0], indgen(11), findgen(254) / 253 * 10)
      r = [0, r, 255] & g = [0, g, 255] & b = [0, b, 255]
      color_save = !p.color & tvlct, r, g, b & !p.color = color_save
  endif

  if keyword_set(extended_rainbow) then begin
      self -> message, "[map_image::colors]: Extended Rainbow color table"
      r = interpol([152, 217, 105,   0,   0,   7, 109, 210, 250, 250, 250, 221, 156, 100, 80], indgen(15), findgen(254) / 253 * 14)
      g = interpol([  0,   0,   0, 125, 250, 245, 224, 250, 216, 159, 101,  46,   0,   0, 34], indgen(15), findgen(254) / 253 * 14)
      b = interpol([ 91, 230, 244, 250, 250, 140,  31,   0,   0,   0,   0,  46,   0,   0, 40], indgen(15), findgen(254) / 253 * 14)
      r = [0, r, 255] & g = [0, g, 255] & b = [0, b, 255]
      color_save = !p.color & tvlct, r, g, b & !p.color = color_save
  endif

  if keyword_set(panoply) then begin
; 	name =	'GIST_earth'
; 	name =	'GMT_globe'
; 	name =	'GMT_relief'
; 	name =	'GMT_split'
; 	name =	'NYT_drought'
; 	name =	'UKM_hadcrut_10'
; 	name =	'SVS_tempanomaly'

 	read_panoply_ct,panoply,red,green,blue,h
; 	read_panoply_ct,panoply,r,g,b,h
; 	white_index = 	self.var.draw.mini
; 			self.var.draw.maxi
	;discrete
	bh  = bytscl(h)
	idx = intarr(256) - 1
	for i = 0, n_elements(bh)-2 do if bh[i] lt bh[i+1] then idx[bh[i]:bh[i+1]] = i
	r = red[idx>0] & g = green[idx>0] & b = blue[idx>0] 
; stop

; 	r = interpol(r,indgen(n_elements(r)),findgen(254) / 253 * n_elements(r) -1)
; 	g = interpol(g,indgen(n_elements(g)),findgen(254) / 253 * n_elements(g) -1)
; 	b = interpol(b,indgen(n_elements(b)),findgen(254) / 253 * n_elements(b) -1)

; 	r = interpolate(reform(red),indgen(256))
; 	g = interpolate(reform(green),indgen(256))
; 	b = interpolate(reform(blue),indgen(256))

	color_save = !p.color & tvlct, r, g, b & !p.color = color_save
 endif
  
  if keyword_set(elevation) then begin
      self -> message, "[map_image::colors]: 'Elevation' style: blue, green, yellow, brown, white"

      r = bytarr(256) + 255b
      g = bytarr(256) + 255b
      b = bytarr(256) + 255b
      r[2] = 0 & g[2] = 0 & b[2] = 150
      r[3:20] = 0 & g[3:20] = bindgen(18) * (190.-120.) / 17. + 120 & b[3:20] = 0
      r[21:40] = bindgen(20) * (220.) / 19. & g[21:40] = 190 & b[21:40] = 0
      r[41:60] = 220 - bindgen(20) * (30) / 19. & g[41:60] = 190 - bindgen(20) * (80) / 19. & b[41:60] = 0
      r[61:80] = bindgen(20) * 65 / 19. + 190
      g[61:80] = bindgen(20) * 145 / 19. + 110
      b[61:80] = bindgen(20) * 255 / 19.

      r[81:*] = 255b & g[81:*] = 255b & b[81:*] = 255b
      color_save = !p.color & tvlct, r, g, b & !p.color = color_save

  endif

  if keyword_set(bwr) then begin
      self -> message, "[map_image::colors]: Blue to red"
      ncols = 253; self.var.draw.ncolors+1
      saturation = fltarr(ncols)
      value = intarr(ncols) + 1
      hue = intarr(ncols)

;       white_index = round((ncols * (-1. *  self.var.draw.mini ) / (self.var.draw.maxi - self.var.draw.mini))>0)
      white_index = floor((ncols * (-1. *  self.var.draw.mini ) / (self.var.draw.maxi - self.var.draw.mini))>0)
      hue       [0 : white_index] = 240
      saturation[0 : white_index] = (1. - findgen(white_index+1) / white_index) * ncols / 253.
      saturation[white_index + 1 : ncols - 1] = findgen(ncols-white_index-1) / (ncols-white_index-2) * ncols / 253.

      color_convert, hue, saturation, value, r, g, b, /hsv_rgb

      rr = intarr(254) & gg = intarr(254) & bb = intarr(254)
      rr[self.var.draw.bottom-1] = r
      gg[self.var.draw.bottom-1] = g
      bb[self.var.draw.bottom-1] = b
      r = [0, rr, 255] & g = [0, gg, 255] & b = [0, bb, 255]
      color_save = !p.color & tvlct, r, g, b & !p.color = color_save
  endif

  if keyword_set(ctable) then begin
      self -> message, "[map_image::colors]: Color table " + string(ctable, format = '(i2)')
;       loadct, ctable, /silent
        if ctable lt 0 then begin & ctable = abs(ctable) & flip_colours = 1 & end
;         loadct, ctable, file = file, _strict_extra=e, /silent
        loadct, ctable, file = file, /silent
  endif
  if keyword_set(greyscale) then begin
      self -> message, "[map_image::colors]: Greyscale color table"
      loadct, 0, /silent
  endif

  if keyword_set(discrete) then begin
      self -> message, "[map_image::colors]: Discrete colors"
      self.var.draw.mini = min(discrete, max = maxi)
      self -> message, "[map_image::colors]: Setting minValue to " + string(self.var.draw.mini)
      self.var.draw.maxi = maxi
      self -> message, "[map_image::colors]: Setting maxValue to " + string(self.var.draw.maxi)
      self.var.draw.n_lev = n_elements(discrete) - 1
      self -> message, "[map_image::colors]: Setting n_lev to " + string(self.var.draw.n_lev)

      nnn = n_elements(discrete) - 1
     ; Setting number of colors such that colorbar shows the steps at the correct position
      self.var.draw.ncolors = floor(float(self.var.draw.ncolors) / nnn) * nnn + 1

      ddd = bytscl([min(*self.var.data.image, max=maxi), discrete, maxi], $
                   min = self.var.draw.mini, $
                   max = self.var.draw.maxi, $
                   top = self.var.draw.ncolors - 1, /nan) + self.var.draw.bottom

      ddd = ddd[1:nnn+1]
      idx = intarr(256) - 1
      for i = 0, nnn-1 do if ddd[i] lt ddd[i+1] then idx[ddd[i]:ddd[i+1]] = i
      if keyword_set(red) and keyword_set(blue) and keyword_set(green) then begin
          if n_elements(red) ne nnn or n_elements(green) ne nnn or n_elements(blue) ne nnn then begin
              print, nnn, format = '("[map_image::colors] ERROR: red, green and blue must have " , i3, " elements")'
              help, red, green, blue
              return
          endif
      endif else begin
          ;Pick the colours from within the chosen intervals
          tvlct, red, green, blue, /get
          index = round(findgen(nnn) / (nnn - 1.) * (self.var.draw.ncolors - 1) + self.var.draw.bottom)
          red   = red  [index]
          green = green[index]
          blue  = blue [index]
      endelse
      r = red[idx>0] & g = green[idx>0] & b = blue[idx>0]
      bad = where(idx eq -1)
      if bad[0] ne -1 then begin
          r[bad] = self.var.draw.void_color
          g[bad] = self.var.draw.void_color
          b[bad] = self.var.draw.void_color
      endif
      color_save = !p.color & tvlct, r, g, b & !p.color = color_save

  endif

  if keyword_set(flip_colours) then begin
      tvlct, r, g, b, /get
      color_save = !p.color & tvlct, reverse(r), reverse(g), reverse(b) & !p.color = color_save
  endif

  ;Set white, black and void-color:
  tvlct, r, g, b, /get
  r[0] = 0 & g[0] = 0 & b[0] = 0
  r[255] = 255 & g[255] = 255 & b[255] = 255
  r[1] = self.var.draw.void_color & g[1] = self.var.draw.void_color & b[1] = self.var.draw.void_color
  color_save = !p.color & tvlct, r, g, b & !p.color = color_save

 if keyword_set(revert_background) then begin
      if strupcase(!d.name) eq 'PS' then begin
          self -> message, "[map_image::colors]:Don't revert colours when using postscript"
      endif else begin
          self -> message, "[map_image::colors]: Reverting background and plot colour:"
          background = !p.background
          color = !p.color
          self -> message, "[map_image::colors]: !p.background: " + $
              strcompress(string(background), /rem) + " --> " + $
              strcompress(string(color), /rem)
          self -> message, "[map_image::colors]: !p.color: " + $
              strcompress(string(color), /rem) + " --> " + $
              strcompress(string(background), /rem)

          !p.background = color
          !p.color = background
      endelse
  endif

end

; ====================================================================================================
; Debug messages
; ====================================================================================================

pro map_image::message, msg_string, time = time

  if keyword_set(time) then begin
      if self.var.internal.debug then print, string(systime(1)-self.var.internal.time, format = '(f5.1, " sec. : ")') + msg_string
  endif else begin
      if self.var.internal.debug then print, msg_string
  endelse
end

; ====================================================================================================
; Location on the globe
; ====================================================================================================

pro map_image::worldview

;    This IDL subroutine was copied from geov.pro, develped at the
;    Modis Data Support Team
;    by Andrey Savtchenko,(asavtche@daac.gsfc.nasa.gov).

  tvlct, r, g, b, /get
  loadct,0, /silent
  dims=size(*self.var.data.latitude)
  if dims[0] eq 2 then begin
      lon0=(*self.var.data.longitude)[dims[1]/2,dims[2]/2]
      lat0=(*self.var.data.latitude)[dims[1]/2,dims[2]/2] ;latmin((*self.var.data.latitude))+(max((*self.var.data.latitude))-min((*self.var.data.latitude)))/2.

      mpl=[(*self.var.data.latitude)[0,dims[2]-1],(*self.var.data.longitude)[0,dims[2]-1],$
           (*self.var.data.latitude)[0,0],(*self.var.data.longitude)[0,0],$
           (*self.var.data.latitude)[dims[1]-1,0],(*self.var.data.longitude)[dims[1]-1,0],$
           (*self.var.data.latitude)[dims[1]-1,dims[2]-1],(*self.var.data.longitude)[dims[1]-1,dims[2]-1]]

  window,22,xsize=400,ysize=400,retain=2,title="granule geolocation"
  map_set,lat0,lon0,/ortho,/isotropic,/noborder,xmargin=0,$
      ymargin=0,/horizon,/grid,color=100, londel = 15, latdel=15

  map_continents,fill_continents=1,color=40
  map_continents,/coasts,color=100

  oplot,(*self.var.data.longitude)[*,0],(*self.var.data.latitude)[*,0],psym=3,color=255
  oplot,(*self.var.data.longitude)[*,dims[2]-1],(*self.var.data.latitude)[*,dims[2]-1],psym=3,color=255
  oplot,(*self.var.data.longitude)[0,*],(*self.var.data.latitude)[0,*],psym=3,color=255
  oplot,(*self.var.data.longitude)[dims[1]-1,*],(*self.var.data.latitude)[dims[1]-1,*],psym=3,color=255
  tvlct, r, g, b
endif else begin
    print, "[map_image::worldview]: Latitude and Longitude data has to be 2D for this subroutine."
endelse
  return
end

; ====================================================================================================
; Remove tag from structure
; ====================================================================================================

function map_image::remove_tag, struct_in, tag

  if size(struct_in, /type) eq 8 then begin
      st_names = strlowcase(tag_names(struct_in))
      for i = 0, n_tags(struct_in) -1 do begin
          match = -1
          for j = 0, n_elements(tag) - 1 do  if strpos(st_names[i], strlowcase(tag[j])) ne -1 then match = 0
          if match eq -1 then $
              output = size(output, /type) eq 8 $
;                ? create_struct(output, st_names[i], (*self.var.internal._extra).(i)) $
;                : create_struct(        st_names[i], (*self.var.internal._extra).(i))
          ? create_struct(output, st_names[i], struct_in.(i)) $
              : create_struct(        st_names[i], struct_in.(i))
      endfor
  endif else output = 0
  return, output
end


; ====================================================================================================
; Project image
; ====================================================================================================

pro map_image::project, $
             no_draw = no_draw, $
             no_erase = no_erase, $
             autoscale = autoscale, $
             _extra = _extra

	; print,'project: set_var'
	self -> set_var, _extra = _extra
	self -> colors, _extra = _extra
	win,  self.var.internal.window, _extra = _extra

	; Define map projection if keyword no_erase is not set
	if not keyword_set(no_erase) or (size(*self.var.draw.draw_image))[0] eq 0 then begin

		; First, see whether p0lon, p0lat or rot have user-defined values
		if size(*self.var.internal._extra, /type) eq 8 then begin
			p0lon = max(strpos(strlowcase(tag_names(*self.var.internal._extra)), 'p0lon')) eq -1 $
				? fix((*self.var.internal._extra).limit[3]) $
				: (*self.var.internal._extra).p0lon
			p0lat = max(strpos(strlowcase(tag_names(*self.var.internal._extra)), 'p0lat')) eq -1 $
				? fix((*self.var.internal._extra).limit[0]) $
				: (*self.var.internal._extra).p0lat
			rot = max(strpos(strlowcase(tag_names(*self.var.internal._extra)), 'rot')) eq -1 $
				? 0 $
				: (*self.var.internal._extra).rot
		endif else begin
			p0lon = 0 & p0lat = 0 & rot = 0
		endelse

		self -> message, string(p0lon, p0lat, rot, format = '("[map_image::project]: p0lon, p0lat, rot: ", 3f6.1)')

		; Now we have to decide whether to erase the screen or not.
		; There are some options: !p.multi might be set, or the user
		; has provided /NOERASE. Which is not to be confused with the (admittedly
		; stupidly chosen /NO_ERASE...). Setting /NOERASE simply means not to erase
		; the screen, this is an IDL built-in graphics keyword. NO_ERASE is a
		; map_image__define keyword and means not to erase the projection, i.e. add
		; e.g. multiple satellite overpasses into one projection. So, here we have to
		; check against NOERASE, which will, if set, come as part of _EXTRA.
		; If !p.multi is set, we will set the ADVANCED keyword to map_set.

		_extra_tags = size(_extra, /type) eq 8 $
			? strlowcase(tag_names(_extra)) $
			: ""
		noerase_set = max(strpos(_extra_tags, "noer") eq 0)

		; in map_set the function map_point_valid is called and produces Floating underflows when convert_coord with the digit 0 is called
		; so far there is nothing to do about it and it should be harmless (stapel)

		map_set, $
			p0lat[0], $
			p0lon[0], $
			rot, $
			position = self.var.draw.draw_image_position, $
			advance = (!p.multi[0] gt 0) or noerase_set, $
			e_grid={label:0,no_grid:1}, $
			_extra = self -> remove_tag(*self.var.internal._extra, ['title', 'lond', 'latd', 'rev','usa','pos'])
	endif

	; Corners of draw region:
	; x, y, lower right, x, y, upper left in device coordinates
	; don't use *self.var.proj.position as things can be confused using !p.multi
	corners = ([!d.x_size * !x.window, !d.y_size * !y.window])[[0, 2, 1, 3]] * self.var.draw.scalef
	self.var.draw.xoffset = ceil(corners[0])
	self.var.draw.yoffset = ceil(corners[1])

	; Size of the draw region
	xsize = floor((!d.x_size * (!x.window[1] - !x.window[0])) * self.var.draw.scalef)
	ysize = floor((!d.y_size * (!y.window[1] - !y.window[0])) * self.var.draw.scalef)

	if xsize eq 0 or ysize eq 0 then begin
		ok =dialog_message(	'[map_image::project]: Array Dimensions must be greater than Zero: '+$
							'[xsize,ysize] ['+strcompress(xsize,/rem)+','+strcompress(ysize,/rem)+']'+$
							' If Projection is stereographic reduce lon limits to +/- 90 around p0lon!')
		return
	endif

	; Create new image or add no data to existing image
	if not keyword_set(no_erase) or (size(*self.var.draw.draw_image))[0] eq 0 then begin
		self -> message, string(xsize, ysize, format = '("[map_image::project]: Create new image (", i5, ", ", i5, ")")')
		ptr_free, self.var.draw.draw_image
		self.var.draw.draw_image = ptr_new( $
												( $
													self.var.draw.true_color $
													? bytarr(xsize, ysize, 3) $
													: bytarr(xsize, ysize) $
												) $
											)
	endif else self -> message, string(xsize, ysize, format = '("[map_image::project]: Add to existing image (", i5, ", ", i5, ")")')

	self -> message, "[map_image::project]: Convert_coord ..."
	index = convert_coord(*self.var.data.longitude, *self.var.data.latitude, /data, /to_device) * self.var.draw.scalef
	self -> message, "[map_image::project]: Convert_coord done"

	; Only choose pixel within the displayed map region
	local_pix = where( $
						finite(index[0, *]) eq 1 and $
						finite(index[1, *]) eq 1 and $
						index[0, *] ge self.var.draw.xoffset and $
						index[1, *] ge self.var.draw.yoffset and $
						index[0, *] lt xsize + self.var.draw.xoffset and $
						index[1, *] lt ysize + self.var.draw.yoffset, $
						n $
					)

	if n gt 0 then begin

		x = round(index[0, local_pix] - self.var.draw.xoffset)
		y = round(index[1, local_pix] - self.var.draw.yoffset)

		; Calculate maximum difference between pixels in device coordinates (MR 20051031)
		if max(self.var.draw.magnify) lt 0 then begin
			x_i = where(histogram(x) ne 0l)
			x_i = max(x_i - shift(x_i, 1))
			y_i = where(histogram(y) ne 0l)
			y_i = max(y_i - shift(y_i, 1))
			; If difference is greater than 1 than auto-magnify pixels. I just added 1
			; pixel to be sure. Without that, there sometimes still was a small line
			; between the magnified pixels.
			if x_i gt 1 or y_i gt 1 then $
				self.var.draw.magnify = [ $
											floor(x_i/2.), $
											floor(y_i/2.) $
										]
		endif
		;---- stapel edited-------------
		ptr_free, self.var.draw.draw_image_orig
		self.var.draw.draw_image_orig = $
			ptr_new( $
						fltarr( $
								(size(*self.var.draw.draw_image) $
								)[1:(self.var.draw.true_color ? 3:2)] $
							) $
					)
		;           ptr_new( $
		;                      fltarr( $
		;                                (size(*self.var.draw.draw_image) $
		;                                )[1:2] $
		;                            ) $
		;                  )

		; Save original pixel values at reprojected positions for e.g. rdpix
		if self.var.draw.true_color then begin
			dum1      = (*self.var.draw.draw_image_orig)[*,*,0]
			dum1[x,y] = ((*self.var.data.image)[*,*,0])[local_pix]
			dum2      = (*self.var.draw.draw_image_orig)[*,*,1]
			dum2[x,y] = ((*self.var.data.image)[*,*,1])[local_pix]
			dum3      = (*self.var.draw.draw_image_orig)[*,*,2]
			dum3[x,y] = ((*self.var.data.image)[*,*,2])[local_pix]
			(*self.var.draw.draw_image_orig) = [[[dum1]],[[dum2]],[[dum3]]]
		endif else (*self.var.draw.draw_image_orig)[x, y] = (*self.var.data.image)[local_pix]
		;---- stapel edited-------------

		; If AUTOSCALE is set, scale the data values within the range of the pixels
		; actually to be displayed, therefore we need the reporjected void_indices,
		; too

		if keyword_set(autoscale) then begin
			print,'Achtung map_image:: autoscaling'
			if (*self.var.draw.void_index)[0] ne -1 then begin
				void_index = convert_coord( $
												(*self.var.data.longitude)[*self.var.draw.void_index], $
												(*self.var.data.latitude)[*self.var.draw.void_index], $
												/data, $
												/to_device $
											) * self.var.draw.scalef
				xx = round(void_index[0, *] - self.var.draw.xoffset)
				yy = round(void_index[1, *] - self.var.draw.yoffset)
				(*self.var.draw.draw_image_orig)[xx, yy] = 0
			endif
			idx = where(*self.var.draw.draw_image_orig ne 0, n)
			if n gt 0 then begin
				self.var.draw.mini = min((*self.var.draw.draw_image_orig)[idx], max = maxi)
				self.var.draw.maxi = maxi
				_extra = self -> remove_tag(_extra, ["min", "max"])
				self -> message,  $
					"[map_image::project]: Autoscaling between  " + $
					string(self.var.draw.mini) +  " and " + $
					string(self.var.draw.maxi)
			endif
		endif

		self -> message,  "[map_image::project]: Project image start"

		; Remap data using the above transformed indices, things are different
		; for true colours images of the type [x, y, 3] or [x, 3] and for
		; "normal" 1D or 2D images using a colour table, respectively

		; True colour images: define RGB values individualy from the input data
		if self.var.draw.true_color then begin
			z = x*0
			sss = size(*self.var.data.image)
			case sss[0] of
				3: begin
					idx_x = local_pix mod sss[1]
					idx_y = local_pix / sss[1]
					for i = 0, 2 do (*self.var.draw.draw_image)[x, y, z * 0 + i] = (*self.var.data.image)[idx_x, idx_y, idx_x * 0 + i] > 1
				end
				2: for i = 0, 2 do (*self.var.draw.draw_image)[x, y, z * 0 + i] = (*self.var.data.image)[local_pix, i] > 1
				else: begin
					print, "[map_image::project]: I am confused about the size of the image... giving up"
					help, *self.var.data.image
				endelse
			endcase
		endif else begin
			; When displaying discrete byte data it is sometimes useful to set self.var.draw.mini to -0.5 in order to
			; have a nice colour bar. In these cases bytscl creates an error message when applied to the byte data.
			if self.var.draw.no_scale ne 0 then $
				dum_data = *self.var.data.image $
			else if size(*self.var.data.image, /type) eq 1 and self.var.draw.mini lt 0 then $
				dum_data=bytscl(fix(*self.var.data.image), $
								min = self.var.draw.mini, $
								max = self.var.draw.maxi, $
								top = self.var.draw.ncolors - 1, /nan) + self.var.draw.bottom $
			; Bytescale input data such in accordance to the chosen colour bar
			else $
			;---- stapel edited-------------
			;               dum_data=bytscl(*self.var.data.image, $
			;                               min = self.var.draw.mini, $
			;                               max = self.var.draw.maxi, $
			;                               top = self.var.draw.ncolors - 1, /nan) + self.var.draw.bottom
			; Assign the bytecaled values to the draw image
			if size((*self.var.draw.draw_image),/n_dim) eq 3  then begin
				fillv_it =0
				if size((*self.var.data.image),/n_dim) le 2 then begin
					dum_data = bw_to_color(*self.var.data.image,mini = self.var.draw.mini,maxi = self.var.draw.maxi,/use_map_image_ct)
					if n_elements((*self.var.data.image)) gt n_elements((*self.var.draw.draw_image)[*,*,0]) then begin
						if (*self.var.draw.void_index)[0] ne -1 then begin
							dum_data = fix(dum_data)
							dum1 = dum_data[*,*,0]
							dum1[(*self.var.draw.void_index)] = -999
							dum2 = dum_data[*,*,1]
							dum2[(*self.var.draw.void_index)] = -999
							dum3 = dum_data[*,*,2]
							dum3[(*self.var.draw.void_index)] = -999
							dum_data = [[[dum1]],[[dum2]],[[dum3]]]
							(*self.var.draw.void_index) = -1
							fillv_it = 1
						endif
					endif
				endif else dum_data = *self.var.data.image
				if fillv_it then (*self.var.draw.draw_image) = fix(*self.var.draw.draw_image)
				dum1      = (*self.var.draw.draw_image)[*,*,0]
				dum1[x,y] = ((dum_data)[*,*,0])[local_pix]
				dum2      = (*self.var.draw.draw_image)[*,*,1]
				dum2[x,y] = ((dum_data)[*,*,1])[local_pix]
				dum3      = (*self.var.draw.draw_image)[*,*,2]
				dum3[x,y] = ((dum_data)[*,*,2])[local_pix]
				if fillv_it then begin
					idx = where(dum1 eq -999,idx_cnt)
					if idx_cnt gt 0 then (dum1)[idx] = self.var.draw.void_color
					idx = where(dum2 eq -999,idx_cnt)
					if idx_cnt gt 0 then (dum2)[idx] = self.var.draw.void_color
					idx = where(dum3 eq -999,idx_cnt)
					if idx_cnt gt 0 then (dum3)[idx] = self.var.draw.void_color
				endif
				(*self.var.draw.draw_image) = byte([[[dum1]],[[dum2]],[[dum3]]])
				self.var.draw.true_color=1
			endif else begin
				dum_data=bytscl(*self.var.data.image, $
										min = self.var.draw.mini, $
										max = self.var.draw.maxi, $
										top = self.var.draw.ncolors - 1, /nan) + self.var.draw.bottom
				; stapel edited, new fillvalue approach 
				; convert_coord for array with higher reslution congridded too much
				; help,*self.var.draw.draw_image,*self.var.data.image

				if n_elements((*self.var.data.image)) gt n_elements((*self.var.draw.draw_image)) then begin
					if (*self.var.draw.void_index)[0] ne -1 then begin
						dum_data[(*self.var.draw.void_index)] = (self.var.draw.ncolors+self.var.draw.bottom)<254 
																; discrete kann ncol eins hochstzen dann ist bei weissem
																; hintergrund die summe gleich 255 = byte(!p.color) d.h. 
																; auch hintergrund wird grau, daher < 254, muss auch in
																; display (ca.Zeile 1108) gemacht werden
						(*self.var.draw.void_index) = -1
					endif
				endif
				(*self.var.draw.draw_image)[x, y] = (dum_data)[local_pix]
			endelse
			;---- stapel edited-------------

			; Save an index for building the histogram
			;       print,'Project: set_one_var, hist_index'
			self -> set_one_var, 'hist_index', where(*self.var.draw.draw_image ne 0)
		endelse

		; If void_index is set, mask the appropriate pixels in gray. N.b. void_index
		; is given with respect to the original data, thus we need convert_coord again

		if (*self.var.draw.void_index)[0] ne -1 then begin

			; If AUTOSCALE is set, we already calculated those indices
			if not keyword_set(autoscale) then begin
				void_index = convert_coord( $
												(*self.var.data.longitude)[*self.var.draw.void_index], $
												(*self.var.data.latitude)[*self.var.draw.void_index], $
												/data, $
												/to_device $
											) * self.var.draw.scalef
				xx = round(void_index[0, *] - self.var.draw.xoffset)
				yy = round(void_index[1, *] - self.var.draw.yoffset)
			endif

			; True colour: Set all three values, RGB, to self.var.draw.void_color in order
			; to get the appropriate gray value
			; All other: Set the appropriate pixels to 1, as in map_image::colors the
			; gray value defined by self.var.draw.void_color is set to the index 1 in the
			; colour table.
			if self.var.draw.true_color then $
				for i = 0, 2 do (*self.var.draw.draw_image)[xx, yy, xx*0+i] = self.var.draw.void_color $
			else $
				(*self.var.draw.draw_image)[xx, yy] = 1
		endif
		self -> message,  "[map_image::project]: Project image done"

		if not keyword_set(no_draw) then self -> display, _extra = _extra
	endif
end
; ====================================================================================================
; Draw projected image
; ====================================================================================================
pro map_image::display, $
             histogram = histogram, $
             contour = contour, $
             _extra = _extra
             
	; stapel hier wird true_color wieder auf null gesetzt weil das image gecheckt wird und nicht das draw_image
	; print,'map_image::display : Achtung hier wurde set_var auskommentiert. Beobachten!!'
	; self -> set_var, _extra = _extra

	; Magnify image
	if max(self.var.draw.magnify) gt 0 then begin
		self -> message, "[map_image::project]: Magnify image: " + string(self.var.draw.magnify, format = '(2i4)')
		structuring_element = $
			replicate( $
						1, $
						2 * self.var.draw.magnify[0] + 1, $
						2 * self.var.draw.magnify[1] + 1 $
					)

		if strupcase(!d.name) eq 'PS' then begin
			idx = where(*self.var.draw.draw_image eq !p.background)
			if idx[0] ne -1 then (*self.var.draw.draw_image)[idx] = 0
		endif
		; Now we have two options: When using one large structuring element in one
		; step, and the structuring element is chosen such as to be much larger than
		; the average distance between pixels, then pixes will get overdrawn. So we
		; better go pixel by pixel, first in x and then in y-direction. However, we
		; need evil for loops here, so it might become slow.
		; However, my first tests showed that using one large structuring element in
		; one step is in fact *slower* than using the two foor loops.
		; Anyway, I will let the second option commented out below, so you might want to test both.
		for i = 0, self.var.draw.magnify[0] < 100 do $
			*self.var.draw.draw_image = $
			dilate( $
						*self.var.draw.draw_image, $
						[1,1,1], $
						/gray, $
						/constrained $
					)
		for i = 0, self.var.draw.magnify[1] < 100 do $
			*self.var.draw.draw_image = $
			dilate( $
						*self.var.draw.draw_image, $
						transpose([1,1,1]), $
						/gray, $
						/constrained $
					)
		; Now here is the option with one large structuring element:
		; Dilate refuses to work if the structuring element exceeds the image borders,
		; so we'll add a temporary boder around the image. For the sake of memory
		; we'll limit the extra border to 100 pixels. Auto_magnify could principally
		; give any number.
		;
		;
		;       sss = size(*self.var.draw.draw_image, /dim)
		;       tmp_img = bytarr( $
		;                           sss[0] + 2 * (self.var.draw.magnify[0] < 100), $
		;                           sss[1] + 2 * (self.var.draw.magnify[1] < 100) $
		;                       )

		;       tmp_img[self.var.draw.magnify[0]<100, self.var.draw.magnify[1]<100] = temporary(*self.var.draw.draw_image)
		;        tmp_img = $
		;            dilate( $
		;                      tmp_img, $
		;                      structuring_element, $
		;                      /gray, $
		;                      /constrained $
		;                  )
		;
		;       *self.var.draw.draw_image = $
		;           tmp_img[ $
		;                      self.var.draw.magnify[0]<100 : self.var.draw.magnify[0]<100 + sss[0] - 1, $
		;                      self.var.draw.magnify[1]<100 : self.var.draw.magnify[1]<100 + sss[1] - 1 $
		;                  ]
	endif

	; The image was originally set to 0, as I am not able to let the BACKGROUND
	; keyword of dilate do what I expect ... so here we set all black pixels to
	; !p.background
	if self.var.draw.true_color and !p.color eq 0 then begin
		idx = where(total(*self.var.draw.draw_image, 3) eq 0)
		if idx[0] ne -1 then begin
			nnn = (size(*self.var.draw.draw_image))[5] / 3
			(*self.var.draw.draw_image)[[idx, idx + nnn, idx + nnn * 2]] = 255
		endif
	endif else begin
		;---- stapel edited-------------
		if ~self.var.draw.true_color then begin
			idx = where(*self.var.draw.draw_image eq 0,idx_cnt)
			if idx_cnt gt 0 then (*self.var.draw.draw_image)[idx] = !p.background
			; stapel edited , see above (in project) fillvalue problem with higher resoluted images
			idx = where(*self.var.draw.draw_image eq ((self.var.draw.ncolors+self.var.draw.bottom)<254),idx_cnt)
			if idx_cnt gt 0 then (*self.var.draw.draw_image)[idx] = 1
		endif
		;---- stapel edited-------------
	endelse

	win,  self.var.internal.window, _extra = _extra
	ss = size(*self.var.draw.draw_image) / self.var.draw.scalef
	tv, *self.var.draw.draw_image, $
		self.var.draw.xoffset / self.var.draw.scalef, $
		self.var.draw.yoffset / self.var.draw.scalef, $
		xsize = ss[1], ysize = ss[2], $
		true = self.var.draw.true_color * 3

	; stapel 09/2014 new keyword no_continents
	if not self.var.draw.no_continents then map_continents, /hires, /continents, _extra = *self.var.internal._extra ;, col = !p.background
	map_grid, _extra = *self.var.internal._extra

	if keyword_set(contour) then begin
		if ((*self.var.draw.void_index)[0] ne -1l) or (size(reform(*self.var.data.image), /n_dimensions) ne 2) then begin
			dum = byte(*self.var.data.image * 0) + 1b
			dum[*self.var.draw.void_index] = 0b
			data_idx = where(dum, data_cou)
			x = (*self.var.data.longitude)[data_idx]
			y = (*self.var.data.latitude)[data_idx]
			z = (*self.var.data.image)[data_idx]
			if data_cou gt 0l then begin
				if size(contour, /type) eq 8 then begin
					contour, z, x, y, /overplot, _strict_extra = contour, /irregular
				endif else begin
					contour, z, x, y, /overplot
				endelse
			endif
		endif else begin
			if size(contour, /type) eq 8 then begin
				contour, *self.var.data.image, *self.var.data.longitude, *self.var.data.latitude, /overplot, _strict_extra = contour
			endif else begin
				contour, *self.var.data.image, *self.var.data.longitude, *self.var.data.latitude, /overplot
			endelse
		endelse
	endif

	if not self.var.draw.no_color_bar   then self -> colorbar
	; stapel 06/2013 new keyword no_draw_border
	if not self.var.draw.no_draw_border then self -> draw_border
	if keyword_set(histogram) and ((*self.var.draw.hist_index)[0] ge 0) then begin
		tags = strlowcase(tag_names(*self.var.internal._extra))
		idx = max(strpos(tags, "tit"), index)
		title = idx ne -1 ? (*self.var.internal._extra).(index) : "Value"
		win, histogram
		a = plot_histo( $
							(*self.var.draw.draw_image_orig)[(*self.var.draw.hist_index)], $
							min = self.var.draw.mini, $
							max = self.var.draw.maxi, $
							xrange = [self.var.draw.mini,self.var.draw.maxi], $
							xstyle = 1, $
							ncolors = self.var.draw.ncolors, $
							bottom = self.var.draw.bottom, $
							/wallpaper, $
							norm = 2, $
							xtitle = title, $
							title = "", $
							_extra = self -> remove_tag( $
														*self.var.internal._extra, $
														['pos', 'iso', 'tit'] $
													) $
						)
	endif

	; Image title stapel 03/2013
	dum = strpos(strlowcase(tag_names(_extra)), 'figure_title')
	if max(dum, index) eq 0 and not self.var.draw.no_draw_border then begin
		axis,xaxis=1,xtitle=(_extra).(index),xticklen=0.0001,xticks=1,xtickname=[' ',' '],_extra = _extra
	endif
; 	if keyword_set(figure_title) and not self.var.draw.no_draw_border then begin
; 		axis,xaxis=1,xtitle=figure_title,xticklen=0.0001,xticks=1,xtickname=[' ',' '],_extra = _extra
; 	endif

end
; ====================================================================================================
; Use RDPIX
; ====================================================================================================
pro map_image::rdpix

win,  self.var.internal.window
rdpix, *self.var.draw.draw_image_orig, $
    self.var.draw.xoffset / self.var.draw.scalef, $
    self.var.draw.yoffset / self.var.draw.scalef
end

; ====================================================================================================
; Draw color bar
; ====================================================================================================

pro map_image::colorbar, _extra = _extra

	self -> set_var, _extra = _extra

; self -> remove_tag:
; Create temporary _extra structure with all elements of *self.var.internal._extra but position.
; Position must not be passed to colorbar, as it controls the position of the mapped image
; The position of the colorbar is controlled by self.var.draw.cb_position.

; Now this is difficult :-)
; The default behaviour of 'colorbar' is to draw a horizontal colorbar. I want the default to
; be vertical, therefore I am checking whether a keyword like 'horizontal' is provided in
; _extra, otherwise, the keyword 'vertical' is passed to colorbar.
  dum = strpos(strlowcase(tag_names(*self.var.internal._extra)), 'bar_horizontal')
  if max(dum, index) eq 0 then vertical = (*self.var.internal._extra).(index) eq 0 else vertical = 1

  df_colorbar, $
      minrange = self.var.draw.mini, maxrange = self.var.draw.maxi, $
      divisions = self.var.draw.n_lev, $
      vertical = vertical, $
      position = self.var.draw.cb_position, $
      bot=self.var.draw.bottom, ncolors=self.var.draw.ncolors-1, $
      logarithmic = self.var.draw.log, $
      _extra = self -> remove_tag(*self.var.internal._extra, ['pos', 'iso'])

end

; ====================================================================================================
; Draw border
; ====================================================================================================

pro map_image::draw_border
  plots, !x.window[[0, 1, 1, 0, 0]], !y.window[[0, 0, 1, 1, 0]], /normal
end
; ====================================================================================================
; Select region of interest or zoom in and out by a choosen factor (in degree or percent)
; ====================================================================================================

pro map_image::zoom, _extra = _extra, get_new_corners = get_new_corners, print_new_corners=print_new_corners,ztext=ztext
  if strupcase(!d.name) eq 'PS' then begin
      print, "[map_image::zoom] ERROR: method zoom not defined for postscript mode!"
      return
  endif

  self -> message, "[map_image::zoom]: Zoom into image"
  ; Set automatic resizing of pixels as default[if not told otherwise (sstapelb 09/2017)]
  if total(stregex(tag_names(_extra),'magnify',/fold,/bool)) eq 0 then self -> set_one_var, "magnify", -1
  self -> set_var, _extra = _extra

  first_corner = 0
  second_corner = 0
  selection_done = 0

  device, get_graphics = old, set_graphics = 6 ;Set xor
  print, "Press left button to choose first corner."
  while not selection_done do begin
      cursor, xx, yy, wait = 2, /dev
      wait, .2
      butn = !err
      if butn eq 1 then begin
          if first_corner eq 0  then begin
              if second_corner eq 1 then begin
                  plots, [x0, x1, x1, x0, x0], [y0, y0, y1, y1, y0], /device
                  plots, x0, y0, /device, psym = 4
              endif
              second_corner = 0
              x0 = xx & y0 = yy
              first_corner = 1
              plots, x0, y0, /device, psym = 4
              print, "Press second button to choose second corner."
          endif else begin
              x1 = xx & y1 = yy
              plots, [x0, x1, x1, x0, x0], [y0, y0, y1, y1, y0], /device
              second_corner = 1 & first_corner = 0
              print, "Press right button to quit or left button for new selection."
          endelse
      endif

      if butn eq 4 and second_corner eq 1 then begin
          if x0 eq x1 or y0 eq y1 $
              then print, "Please choose rectangular area." $
          else selection_done = 1
      endif
  endwhile

  device,set_graphics = old, cursor_standard=30

  if x0 gt x1 then begin
      xx = x0 & x0 = x1 & x1 = xx
  endif
  if y0 gt y1 then begin
      yy = y0 & y0 = y1 & y1 = yy
  endif

  new_corners = ((convert_coord([x0, (x0+x1)/2, x1, (x0+x1)/2], [(y0+y1)/2, y1, (y0+y1)/2, y0], /device, /to_data))[0:1, *])[*]

  if not keyword_set(get_new_corners) then begin
	(*self.var.internal._extra).limit  = new_corners[[1, 0, 3, 2, 5, 4, 7, 6]]
	self -> project, _extra = _extra
  endif else get_new_corners = new_corners[[7,0,3,4]]

  if keyword_set(print_new_corners) then begin
     if arg_present(ztext) then ztext = '['+strjoin(string(new_corners[[7,0,3,4]],f='(f7.1)'),',')+']' else $
     print, 'Zoom :: limit = ','['+strjoin(string(new_corners[[7,0,3,4]],f='(f7.1)'),',')+']'
  endif

end

; ====================================================================================================
; Calculate range for color bar
; ====================================================================================================

pro map_image::set_range
  self.var.draw.mini = min(*self.var.data.image, max=maxi,/nan)
  self.var.draw.maxi = maxi

  if self.var.draw.mini eq self.var.draw.maxi then begin
      self.var.draw.mini = self.var.draw.mini - 1
      self.var.draw.maxi = self.var.draw.maxi + 1
      self.var.draw.n_lev = 2
  endif else begin
      ;self.var.draw.maxi = percentiles((*self.var.data.image), value = [.95])
      binsize = 10.^float(floor(alog10((self.var.draw.maxi-self.var.draw.mini))))
      count=0
      start_here:
      count=count+1
      minout = floor(self.var.draw.mini / binsize) * binsize
      maxout = ceil(self.var.draw.maxi / binsize) * binsize
      nlev = round((maxout-minout) / binsize)

      if count gt 10 then stop
      if nlev lt 3 then begin
          binsize = binsize / 2.
          goto, start_here
      endif
      if nlev gt 8 then begin
          binsize = binsize * 2.
          goto, start_here
      endif
      self.var.draw.n_lev = nlev
      self.var.draw.mini = minout
      self.var.draw.maxi = maxout
      self -> message, string(minout, maxout, nlev, format = '("[map_image::set_range]: Mini, maxi, n_lev: ", 3f7.2)')
  endelse
end

; ====================================================================================================
; Set object variables
; ====================================================================================================

;----------------------------------------------------------------------------------------

function map_image::set_limit
  d1=min(*self.var.data.longitude, i1)
  d2=max(*self.var.data.longitude, i2)
  d3=min(*self.var.data.latitude,  i3)
  d4=max(*self.var.data.latitude,  i4)

  i1 = where(*self.var.data.longitude eq d1)
  if n_elements(i1) gt 1 then i1 = i1[n_elements(i1)/2]
  i2 = where(*self.var.data.longitude eq d2)
  if n_elements(i2) gt 1 then i2 = i2[n_elements(i2)/2]
  i3 = where(*self.var.data.latitude eq d3)
  if n_elements(i3) gt 1 then i3 = i3[n_elements(i3)/2]
  i4 = where(*self.var.data.latitude eq d4)
  if n_elements(i4) gt 1 then i4 = i4[n_elements(i4)/2]

  limit = [$
              (*self.var.data.latitude)[i1], d1,  $
              d4, (*self.var.data.longitude)[i4], $
              (*self.var.data.latitude)[i2], d2,  $
              d3, (*self.var.data.longitude)[i3]  $
          ]

  return, limit
end

;----------------------------------------------------------------------------------------

function map_image::decomposed, sss

  case (sss)[0] of
      3:begin
          if (sss)[3] ne 3 then begin
              print, "[map_image::decomposed]: For true color images, the image must be of dimension [xsize, ysize, 3]"
              stop
          endif
          self.var.draw.true_color=1
      end
      2: self.var.draw.true_color = (sss)[2] eq 3
      1: self.var.draw.true_color = 0
      else: begin
          print, "[map_image::decomposed]:I don't know what to do with an image of these dimensions: "
          return, 1
      end
  endcase

  if strupcase(!d.name) ne 'PS' then begin
      device, get_decomposed = decomp
      self.var.draw.decomposed = decomp
      self -> message, self.var.draw.true_color eq 1 $
          ? "[map_image::decomposed]: For true color images, I will set device, decomposed = 1" $
          : "[map_image::decomposed]: For 8-bit images, I will set device, decomposed = 0"
      device, decomp = self.var.draw.true_color
  endif

  if self.var.draw.true_color then self.var.draw.no_color_bar = 1
  
  return, 0
end

;----------------------------------------------------------------------------------------

function map_image::limit428, limit
; Transform 4-element limit into 8-element vector
;
; If no map projection is defined yet, just assume (equidistant)
; cylindrical:
  x0 = limit[1] & y0 = limit[0] & x1 = limit[3] & y1 = limit[2]
  if !map.projection eq 0 then begin
      new_limit = [ $
                      (y0 + y1) / 2., x0, $
                      y1, (x0 + x1) / 2., $
                      (y0 + y1) / 2., x1, $
                      y0, (x0 + x1) / 2.  $
                  ]
  endif else begin
                                ; Normal coordinates of lower left and upper right corner:
      dum = convert_coord([x0, x1], [y0, y1], /data, /to_normal)
                                ; Data coordinates of 4 points on each side:
      x0 = dum[0, 0] & x2 = dum[0, 1] & x1 = (x2 + x0) / 2.
      y0 = dum[1, 0] & y2 = dum[1, 1] & y1 = (y2 + y0) / 2.
      dum = convert_coord([x0, x1, x2, x1], [y1, y2, y1, y0], /normal, /to_data)
                                ; Return correct limit vector
      new_limit = dum[[1, 0, 4, 3, 7, 6, 10, 9]]
  endelse
  self -> message, "[map_image::limit428]: " + $
      "Making limit a 8-element vector:"
  self -> message, "[map_image::limit428]: " + $
      "old: " + string(limit, format = '("[", 4(i4, ","), "]")')
  self -> message, "[map_image::limit428]: " + $
      "new: " + string(new_limit, format = '("[", 8(i4, ","), "]")')

  return, new_limit
end

;----------------------------------------------------------------------------------------

pro map_image::set_one_var, tag, value, $
             no_copy = no_copy, $
             error = error
; Look for the first matching tag name in self.var.(x)
; Some variables like limit can change their shape, so they are
; stored as pointers.
; If no tag in self.var.(x) matches, the tag and value are stored in
; self.var.internal._extra
; help,value
  error = 0
  for i = 0, n_tags(self.var) -1 do begin
      st_names = strlowcase(tag_names(self.var.(i)))
      return_now = 0
      for j = 0, n_tags(self.var.(i)) - 1 do begin
          if strpos(st_names[j], strlowcase(tag)) eq 0 then begin
              if st_names[j] eq 'image' then if self -> decomposed(size(value)) ne 0 then begin
                  help, value
                  error = 1
                  return
              endif

              if size(self.var.(i).(j), /type) eq 10 $
                  then if ptr_valid(self.var.(i).(j)) $
                  then *self.var.(i).(j) = value $
              else self.var.(i).(j) = ptr_new(value, no_copy = no_copy) $

; We need the [*] here in order to set both values for self.draw.magnify to
; the same value if just a scalar is provided
              else self.var.(i).(j)[*] = value
              if $
                  strpos(st_names[j] ,'ima') eq 0 and $
                  self.var.draw.mini eq 0 and $
                  self.var.draw.maxi eq 0 then begin 
; 			print,self.var.draw.mini
; 			print,self.var.draw.maxi
; ; 			SHELP, self.var
; 			print,st_names
; ; 			shelp,self.var.draw
; 			print,'set_range'
 			self -> set_range
	      endif
; Save two colours "at the bottom" for background and void color and one "at the top" for the foreground colour
              if st_names[j] eq 'bottom' then self.var.draw.bottom = self.var.draw.bottom > 2
              if st_names[j] eq 'ncolors' or st_names[j] eq 'bottom' then self.var.draw.ncolors = self.var.draw.ncolors < (255-self.var.draw.bottom)
          return
          endif
      endfor
  endfor
  if size(*self.var.internal._extra, /type) eq 8 then begin
      st_names = strlowcase(tag_names(*self.var.internal._extra))
      for j = 0, n_tags(*self.var.internal._extra) - 1 do begin
          if strpos(st_names[j], strlowcase(tag)) eq 0 then begin
              if strpos(strlowcase(tag), 'lim') eq 0 then begin
                  if n_elements(value) eq 4 then value = self -> limit428(value)
                  if value[0] eq -1 then value = self -> set_limit()
              endif
              (*self.var.internal._extra).(j) = value
              return
          endif
      endfor
      *self.var.internal._extra = create_struct(*self.var.internal._extra, tag, value)
  endif else begin
      *self.var.internal._extra = create_struct(tag, value)
  endelse

;  			shelp,self.var.draw
end

;----------------------------------------------------------------------------------------

function map_image::get_var, tag
; Look for the first matching tag name in self.var.(x)
; Some variables like limit can change their shape, so they are
; stored as pointers.

  for i = 0, n_tags(self.var) -1 do begin
      st_names = strlowcase(tag_names(self.var.(i)))
      for j = 0, n_tags(self.var.(i)) - 1 do begin
          if strpos(st_names[j], strlowcase(tag)) eq 0 then begin
              if size(self.var.(i).(j), /type) eq 10 then begin
                  if ptr_valid(self.var.(i).(j)) then $
                      return,  *self.var.(i).(j) $
                  else $
                      return, -1
              endif else begin
                  return, self.var.(i).(j)
              endelse
          endif
      endfor
  endfor
end

;----------------------------------------------------------------------------------------

function map_image::calc_position, position

  corners = ([!x.window, !y.window])[[0, 2, 1, 3]]	;Get size of window in device units
  if max(corners) eq 0 then corners = [0,0,1,1]
  x0 = corners[0] + position[0] * (corners[2] - corners[0])
  x1 = corners[0] + position[2] * (corners[2] - corners[0])
  y0 = corners[1] + position[1] * (corners[3] - corners[1])
  y1 = corners[1] + position[3] * (corners[3] - corners[1])
  return, [x0, y0, x1, y1]
end

;----------------------------------------------------------------------------------------

pro map_image::set_var, $
             error = error, $
             _extra = _extra

  if size(_extra, /type) eq 8 then begin
      names = tag_names(_extra)
;        print,'Set_Var: set_one_var ',names
      for i = 0, n_tags(_extra) - 1 do self -> set_one_var, names[i], _extra.(i), error = error
  endif
end

; ====================================================================================================
; Internal subroutines
; ====================================================================================================
pro map_image::debug
  stop
end


;----------------------------------------------------------------------------------------

function map_image::init, $
                  img, lat, lon, $
                  no_copy = no_copy, $
                  no_project = no_project, $
                  no_color_bar = no_color_bar, $
                  no_draw_border = no_draw_border, $
                  no_continents = no_continents, $
                  debug = debug, $
                  logarithmic = logarithmic, $
                  _extra = _extra

         
   if size(img, /type) * size(lat, /type) * size(lon, /type) eq 0 then begin
      print, "Image or lon or lat are not defined:"
      help, img, lat, lon
      return, 0
  endif
  image = img

  self.var.internal.debug = keyword_set(debug)
  self.var.internal.time = systime(1)
  self.var.draw.log = keyword_set(logarithmic)

 if size(_extra,/type) eq 8 then begin
   if keyword_set(logarithmic) then begin
	image = alog10(image)
	if is_tag(_extra,'max') then _extra.max = alog10(_extra.max>0.0001)
	if is_tag(_extra,'min') then _extra.min = alog10(_extra.min>0.00001)
; 	if is_tagname(_extra,'min') then _extra.min = alog10(_extra.min)
   endif
   if is_tag(_extra,'N_LEV') then self.var.draw.n_lev = _extra.n_lev
endif

; !EXCEPT= (keyword_set(debug) ? 2 : 0)

  if n_params() eq 3 then begin
;       print,'Init: set_one_var, image'
      self -> set_one_var, 'image', image, no_copy = no_copy, error = error
      if error eq 1 then return, 0
;       print,'Init: set_one_var, lon'
      self -> set_one_var, 'longitude', lon, no_copy = no_copy
;       print,'Init: set_one_var, lat'
      self -> set_one_var, 'latitude', lat, no_copy = no_copy
  endif else no_project = 1

; First, fill all pointer variables with a default value just in case
  for i = 0, n_tags(self.var) -1 do $
      for j = 0, n_tags(self.var.(i)) -1 do $
      if size(self.var.(i).(j), /type) eq 10 then $
      if not ptr_valid(self.var.(i).(j)) then begin

      self.var.(i).(j) = ptr_new(i*10+j)
  endif

;   print,'Init: set_one_var, defaults for void_index,void_color,bottom,ncolors,magnify,rold,gold,bold,color,background'
  self -> set_one_var, 'void_index', -1
  self -> set_one_var, 'void_color', 150
; Reserve colour 0 for background and colour 1 for void-color
  self -> set_one_var, 'bottom', 2
  self -> set_one_var, 'ncolors', 252;253 -minus 1 wegen fillvalues ; stapel 2016
  self.var.internal.window = !d.window ge 0 ? !d.window : 1
  self.var.draw.scalef = strupcase(!d.name) eq 'PS' ? 0.03 : 1.

; Default bahaviour is to automatically scale the pixel size
  self -> set_one_var, "magnify", [-1, -1]

; Save latest color settinngs:
  tvlct, rold, gold, bold, /get
  self -> set_one_var, 'rold', rold
  self -> set_one_var, 'gold', gold
  self -> set_one_var, 'bold', bold
  self -> set_one_var, 'color', !p.color
  self -> set_one_var, 'background', !p.background

  win,  self.var.internal.window, _extra = _extra
  ss = size(*self.var.draw.draw_image) / self.var.draw.scalef

  
  dum = strpos(strlowcase(tag_names(_extra)), 'xmargin')
  xmargin = max(dum, index) eq 0 ? (_extra).(index) : [0,0]
  dum = strpos(strlowcase(tag_names(_extra)), 'ymargin')
  ymargin = max(dum, index) eq 0 ? (_extra).(index) : [0,0]
  ;this is the image position
  dum = strpos(strlowcase(tag_names(_extra)), 'position')
  if max(dum, index) eq 0 then begin
		img_pos = (_extra).(index)
		_extra = remove_tag(_extra, ['position'])
  endif
  ;this is the bar position
  dum = strpos(strlowcase(tag_names(_extra)), 'bar_position')
  if max(dum, index) eq 0 then begin
		bar_pos = (_extra).(index)
		_extra = remove_tag(_extra, ['bar_position'])
  endif

  plot,[0,0],[1,1], $
      /nodata, $
      /noerase, $
      xstyle=4, $
      ystyle = 4, $
      xmargin = xmargin, $
      ymargin = ymargin

  if keyword_set(no_color_bar) then self.var.draw.no_color_bar = 1
  if keyword_set(no_draw_border) then self.var.draw.no_draw_border = 1
  if keyword_set(no_continents) then self.var.draw.no_continents = 1

  if self.var.draw.no_color_bar then begin
      self.var.draw.draw_image_position = self -> calc_position(keyword_set(img_pos) ? img_pos : [0.1,0.1,0.93,0.9])
  endif else begin
      vertical = 1
      if size(_extra, /type) eq 8 then begin
          dum = strpos(strlowcase(tag_names(_extra)), 'bar_horizontal')
          if max(dum, index) eq 0 then vertical = (_extra).(index) eq 0 else vertical = 1
      endif
      if vertical then begin
          ; Vertical colorbar
          self.var.draw.draw_image_position = self -> calc_position(keyword_set(img_pos) ? img_pos : [0.24, 0.10, 0.93, 0.9])
          self.var.draw.cb_position         = self -> calc_position(keyword_set(bar_pos) ? bar_pos : [0.13, 0.09, 0.16, 0.91])
      endif else begin
          ; Horizontal colorbar
          self.var.draw.draw_image_position = self -> calc_position(keyword_set(img_pos) ? img_pos : [0.07, 0.3, 0.93, 0.9])
          self.var.draw.cb_position         = self -> calc_position(keyword_set(bar_pos) ? bar_pos : [0.1, 0.08, 0.9, 0.14])
      endelse
  endelse

  self -> set_one_var, 'limit', self -> set_limit()
  if not keyword_set(no_project) then $
      self -> project, _extra = _extra $
  else $
      self -> set_var, _extra = _extra

  return, 1
end

;----------------------------------------------------------------------------------------

pro map_image::cleanup, image, lat, lon

  if n_params() ge 1 then image = *self.var.data.image
  if n_params() ge 2 then lat = *self.var.data.latitude
  if n_params() ge 3 then lon = *self.var.data.longitude

; Find all pointers and dereference them
  for i = 0, n_tags(self.var) -1 do $
      for j = 0, n_tags(self.var.(i)) -1 do $
      if size(self.var.(i).(j), /type) eq 10 then ptr_free, self.var.(i).(j)


  if strupcase(!d.name) ne 'PS' then begin
      self -> message, string(self.var.draw.decomposed, format = '("[map_image::cleanup]: Setting device, decomposed = ", i1)')
      device, decomposed = self.var.draw.decomposed
                                ; Restore original color settings:
      tvlct, self.var.draw.rold, self.var.draw.gold, self.var.draw.bold
  endif
  self -> message, "[map_image::colors]: !p.background: " + $
      strcompress(string(!p.background), /rem) + " --> " + $
      strcompress(string(self.var.draw.background), /rem)
  self -> message, "[map_image::colors]: !p.color: " + $
      strcompress(string(!p.color), /rem) + " --> " + $
      strcompress(string(self.var.draw.color), /rem)
;   !p.color = self.var.draw.color
;   !p.background = self.var.draw.background
end

;----------------------------------------------------------------------------------------

pro map_image__define

  struct = {map_image, var: {var, $
                             data:        {data, $
                                           image: ptr_new(), latitude: ptr_new(), longitude: ptr_new() $
                                          }, $
                             draw:        {draw, $
                                           figure_title: "", legend: "", comment: "", $
                                           mini: 0., maxi: 0., n_lev: 0,  $
                                           void_index:ptr_new(), hist_index:ptr_new(), void_color: 0, $
                                           no_scale: 0, corners: [0., 0., 0., 0.], $
                                           no_color_bar: 0, cb_position: [0., 0., 0., 0.], $
                                           magnify: [0, 0], draw_image_position: [0., 0., 0., 0.],  $
                                           draw_image: ptr_new(), xoffset: 0, yoffset: 0, scalef: 1., $
                                           draw_image_orig: ptr_new(), no_draw_border: 0,no_continents: 0,$
                                           decomposed: 0, true_color: 0, $
                                           bottom: 0b, ncolors: 0, log:0,$
                                           rold:bytarr(256), gold:bytarr(256), bold:bytarr(256), $
                                           color:0l, background:0l, contour:ptr_new() $
                                          }, $
                             internal:    {internal, debug: 0, window: 0, _extra: ptr_new(), time: 0d} $
                            }}
end

;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------

pro map_image, image, lat, lon, _extra = _extra
  m = obj_new("map_image", image, lat, lon, _extra = _extra)
  obj_destroy, m
end

;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------

pro testdata, image, lon, lat, imgsize = imgsize

  imgsize = keyword_set(imgsize) ? imgsize : 500
  image = dist(imgsize, imgsize)
  lon = fltarr(imgsize, imgsize)
  lat = fltarr(imgsize, imgsize)
  for i = 0, imgsize-1 do lon[*, i] = findgen(imgsize) / (imgsize-1) * 40. -10.
  for i = 0, imgsize-1 do lat[i, *] = findgen(imgsize) / (imgsize-1) * 40. + 30.
end

;----------------------------------------------------------------------------------------

pro check_ptr

  q = ptr_valid(count = c)
  if c gt 0 then begin
      print, c, format = '("[example1]: Still ", i3, " valid heap variable(s)!")'
      q=ptr_valid(/cast)
      help, q, output = output
      for i = 0, n_elements(q) -1 do print, i, ": ", output
      ptr_free, ptr_valid(/cast)
  endif
end

;----------------------------------------------------------------------------------------
; EXAMPLES
;----------------------------------------------------------------------------------------
; 
; pro ex1, _extra = _extra
; 
; ; Plain greyscale image
; win,1
;   testdata, image, lon, lat
;   map_image, image, lat, lon, title = "Legend title / unit", _extra = _extra
; end
; 
; pro ex2, _extra = _extra
; 
; ; Low resolution data (looks bad!)
; ; Now, plotting individual pixels must explicetely be set using MAGNIFY = 0!
; 
;   testdata, image, lon, lat
;   map_image, image, lat, lon, title = "Legend title / unit", $
;       limit=[48, -5, 52, 0], londel=1,latdel=1, $
;       magnify = 0, $
;       _extra = _extra
; end
; 
; pro ex3, _extra = _extra
; 
; ; Automatically magnify image
; 
;   testdata, image, lon, lat
;   map_image, image, lat, lon, title = "Legend title / unit", $
;       limit=[48, -5, 52, 0], londel=1,latdel=1, $
;       /rainbow, mini = 220, maxi=280, /box, _extra = _extra
; end
; 
; pro ex4, _extra = _extra
; 
; ; Different projection
; 
;   testdata, image, lon, lat
;   map_image, image, lat, lon, title = "Legend title / unit", $
;       /ortho, p0lon = 0, p0lat = 50, /iso, /rainbow, $
;       limit = [20, -70, 70, 180, 20, 90, -20, 10], _extra = _extra
; end
; 
; pro ex5, _extra = _extra
; 
; ; Discrete colours and different colour table
; 
;   testdata, image, lon, lat, imgsize = 10
;   map_image, image, lat, lon, title = "Legend title / unit", $
;       mini=2, maxi=8, n_lev=6, discrete=[2, 3, 4, 5, 6, 7, 8], $
;       ctable = 15, bot = 20, ncol = 121, /box, _extra = _extra
; end
; 
; pro ex6, _extra = _extra
; 
; ; Discrete colors with explicitely defined colours
; 
;   !quiet=1
;   testdata, image, lon, lat, _extra = _extra, imgs=250
;   map_image, lon, lat, lon, title = "Longitude / degrees", $
;       red = [255, 0, 0, 255], green = [0, 255, 0, 255], blue = [0, 0, 255, 100], $
;       discrete=[0, 5, 15, 20, 25], $
;       /box, $
;       void_index = where(lon lt 0 or lon gt 25), $
;       min = 0, max=25, n_lev = 5, _extra = _extra
; end
; 
; pro ex7, _extra = _extra
; 
; ; Discrete colors, check for exact discrimination
; 
;   testdata, image, lon, lat
;   map_image, lon, lat, lon, title="Longitude / degrees", $
;       mini = 4, $
;       maxi = 6, $
;       n_lev = 4, $
;       limit = [51, 4, 53, 6], $
;       londel = .5, latdel = .5, $
;       magnif = 3, $
;       format = '(f3.1)', /box, $
;       red = [122, 0, 255, 70], green = [0, 255, 70, 122], blue = [255, 70, 122, 0], $
;       discrete=[4, 4.5, 5, 5.5, 6.], _extra = _extra
; 
; end
; 
; pro ex8, _extra = _extra
; 
; ; Zoom into image
; 
;   testdata, image, lon, lat
;   win, 1
;   m = obj_new( $
;                  "map_image", $
;                  image, $
;                  lat, $
;                  lon, $
;                  /no_copy, $
;                  title = "Legend title / unit", $
;                  /rainbow, $
;                  _extra = _extra)
;   m -> zoom, win = 2
;   obj_destroy, m
;   check_ptr
;   !quiet=0
; end
; 
; 
; pro ex9, _extra = _extra
; 
; ; Multiple "overpasses"
;   win, 1
;   !quiet=1
;   testdata, image, lon, lat
;   m = obj_new($
;                  "map_image", image, lat, lon, $
;                  title = "Legend title / unit", $
;                  limit = [0, -65, 80, 10, 0, 35, -30, 10], $
;                  /no_draw, $
;                  /rainbow, $
;                  _extra = _extra)
;   m -> project, image = image, lon = lon, lat = lat - 50, /no_erase, /no_draw
;   m -> project, image = image, lon = lon - 50, lat = lat - 50, /no_erase, /no_draw
;   m -> project, image = image, lon = lon - 50, lat = lat, /no_erase, /no_draw
;   m -> display
;   obj_destroy, m
;   check_ptr
;   !quiet=0
; end
; 
; pro ex10, _extra = _extra
; 
; ; Multiple images with one colour bar
; ; "Old" version prior to support of !p.multi
;   !quiet=1
;   testdata, image, lon, lat
;   map_image, image, lat, lon, pos = [.05, .22, 0.45, .50], londel = 10, latdel = 10, /no_col, /rainbow, _extra = _extra
;   map_image, image, lat, lon, /noerase, pos =  [.05, .22, 0.45, .50] + [0.5, 0, 0.5, 0], londel = 20, latdel = 20, /sinu, /no_col, /rainbow, _extra = _extra
;   map_image, image, lat, lon, /noerase, pos =  [.05, .22, 0.45, .50] + [0, 0.45, 0, 0.45], /ortho, p0lon = 0, p0lat = 50, /iso, /rainbow, $
;       limit = [20, -70, 70, 180, 20, 90, -20, 10], /no_col, _extra = _extra
;   m = obj_new("map_image", image, lat, lon, /noerase, pos =  [.05, .22, 0.45, .50] + [0.5, 0.45, 0.5, 0.45], londel = 2, latdel = 2, limit = [55, -10, 60, -5, 55, 0, 50, -5], /no_col, magnif = 2, /rainbow, _extra = _extra)
;   m -> colorbar, cb_position = [.04, .07, .96, .1], /bar_horizontal, title = 'Legend title / unit'
;   obj_destroy, m
;   check_ptr
;   !quiet=0
; end
; 
; pro ex10a, _extra = _extra
; ; Multiple images with one colour bar
; ; (albeid smaller)
; ; New version using !p.multi
;   !quiet=1
;   testdata, image, lon, lat
;   !p.multi = [0,2,2]
;   map_image, image, lat, lon, londel = 10, latdel = 10, /rainbow, /no_col, _extra = _extra
;   map_image, image, lat, lon, londel = 20, latdel = 20, /sinu, /rainbow, /no_col, _extra = _extra
;   map_image, image, lat, lon, /ortho, p0lon = 0, p0lat = 50, /iso, /rainbow, $
;       limit = [20, -70, 70, 180, 20, 90, -20, 10], $
;       /bar_horizontal, /no_col, $
;       _extra = _extra
;   map_image, image, lat, lon, londel = 2, latdel = 2, limit = [55, -10, 60, -5, 55, 0, 50, -5], /rainbow, $
;       title = 'Legend title / unit', $
;       /horiz, $
;       _extra = _extra
;   !quiet=0
; end
; 
; pro ex11, _extra = _extra
; 
; ; True color image
; 
;   testdata, image, lon, lat
; 
;   data = [[[bytscl(lon)]], [[bytscl(lat)]], [[bytscl(image)*0b]]]
;   win, 1 & map_image, data, lat, lon, _extra = _extra
;   xyouts, .5, .95, /normal, align = .5, string(size(data, /dim), format = '(3i8)')
; 
;   data = reform([[bytscl(lon[*])], [bytscl(lat[*])], [bytscl(image[*])*0b]])
;   map_image, data, lat, lon, _extra = _extra
;   xyouts, .5, .95, /normal, align = .5, string(size(data, /dim), format = '(2i8)')
; end
; 
; pro ex12, _extra = _extra
; 
; ; Logarithmic colour scale
; 
;   testdata, image, lon, lat
;   image = image / max(image)
;   image = exp(image)
;   image = (image-min(image)) / max(image) * 20 + .1
; 
;   map_image, win = 1, image, lat, lon, /rainbow, min = 0, max = 20, n_lev = 4
; 
;   map_image, win = 2, alog10(image), lat, lon, /rainbow, $
;       mini = alog10(0.1), maxi = alog10(100), $
;       /ylog, range = [0.1, 100], $
;       format = '(f5.1)', div = 3, minor = 10
; end
; 
; 
; ;----------------------------------------------------------------------------------------
; 
; pro map_image_demo, _extra = _extra
;   s = ""
;   print, "[Example 1]: Just a plain greyscale image." & ex1, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 2]: Now, zoom into the image with the LIMIT keyword." & ex2, _extra = _extra
;   print, "[Example 2]: This looks comparably bad, so let's try MAGNIFY" & read, s, prompt = "     <ENTER> "
;   print, "[Example 3]: Much better. We also chose a rainbow colour bar"
;   print, "[Example 3]: and new min and max values. Plus the box around with the"
;   print, "[Example 3]: lat / lon values." & ex3, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 4]: Now with a different projection:" & ex4, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 5]: A different colour table and discrete values:" & ex5, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 6]: Again discrete colours, but this time with explicitely defined colours:" & ex6, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 7]: And again, now check for exact discrimination of colours / values:" & ex7, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 8]: Interactively zoom into an image:" & ex8, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 9]: Plot multiple datasets in one projection:" & ex9, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 10]: Plot multiple datasets in different projections, but with a common, horizontal colour bar:" & ex10, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 11]: And finally, a true colour image:" & ex11, _extra = _extra & read, s, prompt = "     <ENTER> "
;   print, "[Example 12]: Use logarithmic scaling:" & ex12, _extra = _extra
; end
; 
