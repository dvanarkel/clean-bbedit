eastwoodTests: .FORCE
	cp eastwoodTests.prj.default eastwoodTests.prj
	cpm project eastwoodTests.prj build

runTest: eastwoodTests .FORCE
	cleantest -r ./$< --junit junit.xml

.FORCE:

.PHONY: .FORCE
