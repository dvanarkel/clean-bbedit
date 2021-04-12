compiler: .FORCE
	$(MAKE) -$(MAKEFLAGS) -C compiler/main/Unix
	$(MAKE) -$(MAKEFLAGS) -C compiler/backendC/CleanCompilerSources -f Makefile.linux64
	mkdir -p compiler/backend/Clean\ System\ Files
	ln -fs ../../backendC/CleanCompilerSources/backend.a compiler/backend/Clean\ System\ Files/backend_library

compiler32: .FORCE
	CFLAGS=-m32 $(MAKE) -$(MAKEFLAGS) -C compiler/main/Unix
	$(MAKE) -$(MAKEFLAGS) -C compiler/backendC/CleanCompilerSources
	mkdir -p compiler/backend/Clean\ System\ Files
	ln -fs ../../backendC/CleanCompilerSources/backend.a compiler/backend/Clean\ System\ Files/backend_library

.FORCE:

.PHONY: .FORCE
