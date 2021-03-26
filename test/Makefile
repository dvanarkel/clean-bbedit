eastwoodTests: .FORCE
	cpm project eastwoodTests.prj.default eastwoodTests.prj build
	cpm project eastwoodTests.prj build

runTest: eastwoodTests .FORCE
	cleantest -r ./$< --junit junit.xml

.FORCE:

.PHONY: .FORCE
