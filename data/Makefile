ALLRDA=bsmmeta2.rda bsmmeta.rda bsmCalibrations.rda

all: refresh

refresh: clean rda

rda: $(ALLRDA)

bsmmeta2.rda: r.new_bsmmeta
	Rscript r.new_bsmmeta

bsmmeta.rda: gen/bsm_coords r.bsmmeta
	Rscript r.bsmmeta

bsmCalibrations.rda: r.calib
	Rscript r.calib

clean:
	rm -f $(ALLRDA)
	rm -f .Rdata *.Rout .Rhistory
