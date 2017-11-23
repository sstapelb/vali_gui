pro end_plot, color_8bit = color_8bit
	dum = strlowcase(!d.name)
	case dum of
	'win'	: begin
		!p.color		= t_c_i(255, 255, 255)
		!p.background	= t_c_i(000, 000, 000)
		end_png, /silent
	end
	'x'		: begin
		!p.color		= t_c_i(255, 255, 255)
		!p.background	= t_c_i(000, 000, 000)
		end_png, /silent
	end
	'z'		: begin
		end_png, /silent
	end
	'ps'	: begin
		end_eps, /silent
	end
	else	: begin
	endelse
	endcase
end