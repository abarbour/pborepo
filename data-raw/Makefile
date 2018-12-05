METAURL=https://www.unavco.org/data/strain-seismic/bsm-data/lib/docs/bsm_metadata.txt
META=$(addsuffix .txt, $(basename $(notdir $(METAURL))))

SPOTURL=https://www.unavco.org/data/strain-seismic/bsm-data/lib/docs/spotl.tides.txt
SPOT=$(addsuffix .txt, $(basename $(notdir $(SPOTURL))))

all: $(SPOT) $(META)

$(META):
	rm -f /tmp/tmppbometa
	wget -O /tmp/tmppbometa $(METAURL)
	sed -e 's/->//g' /tmp/tmppbometa | expand > $(META)

$(SPOT):
	rm -f /tmp/tmpspotl
	wget -O /tmp/tmpspotl $(SPOTURL)
	sed -e 's/#BSM/BNUM/g' /tmp/tmpspotl | expand > $(SPOT)

clean:
	rm -f $(META)
	rm -f $(SPOT)

refresh: clean all
