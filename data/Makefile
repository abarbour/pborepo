all: bsmmeta.rda bsmCalibrations.rda clean

bsmmeta.rda: gen/bsm_coords r.bsmmeta
	rr r.bsmmeta s

bsmCalibrations.rda: r.calib
	rr r.calib s

clean:
	rm -f .Rdata *.Rout
