
pro endps

common pskeys, psflag,ra,ga,ba


device, /close_file


set_plot, 'X'

;case psflag of
	;0: spawn, 'gv idl.ps &'
	;1: spawn, 'gv idl.eps &'
;endcase

!P.FONT=-1

end




		
