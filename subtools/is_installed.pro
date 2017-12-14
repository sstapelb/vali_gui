function is_installed, prog_name

	return,file_test(file_which(getenv('PATH'),prog_name))

end
