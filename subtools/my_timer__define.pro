
pro my_timer::wie_lang_noch, add_string = add_string
	self.i++
	jetzt=systime(1)
	togo = (self.nnn - self.i)*((jetzt-self.zeit0)/self.i)

	hours   = floor(togo/3600.)>0.
        minutes = floor((togo - hours*3600.)/60.)>0.
        seconds = floor((togo - hours*3600.) - minutes * 60.)>0.
  
	add = keyword_set(add_string) ? ' '+add_string : ''

	timestring = string(hours,format="(i4.4)")+'h:'$
                   + string(minutes,format="(i2.2)")+'m:'+string(seconds,format="(i2.2)")+'s'
	run = strcompress(self.i,/rem)+'('+strcompress(self.nnn,/rem)+')'
        print,'run: '+run+add+' Estimated time to run: ',timestring,string(13b),format="($, 3A)"
end

function my_timer::init, nnn
	self.nnn   = nnn
	self.zeit0 = systime(1)
	return, 1
end

pro my_timer::CLEANUP
	ende = systime(1)
	zeit = ende - self.zeit0
	hh   = string(fix(zeit/3600.),format='(i4.4)')
	mn   = string((zeit mod 3600.)/60.,format='(i2.2)')
	se   = string(round((zeit mod 3600.) mod 60.),format='(i2.2)')
	zeitstr=hh+'h:'+mn+'m:'+se+'s'
	print,systime(0)+ ' duration: '+zeitstr
end

pro my_timer__define
	struct = { my_timer,      $
		   nnn	: 0ul, $
        	   i	: 0ul, $
        	   zeit0: 0.d  $
        	 }
end
