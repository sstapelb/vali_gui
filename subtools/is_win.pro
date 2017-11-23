function is_win
	return, (strlowcase(!version.os_family) eq "windows")
end