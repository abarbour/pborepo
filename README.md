README (luigi.ucsd.edu)
Creation date:	Tue Mar 27 22:31:04 UTC 2012

pboRepo metadata
================

PBO borehole strainmeters
-------------------------

* bsm_azimuths		azimuths of the BSM channels
    * if Ch0==0 => assumed (and likely not correct)**
* bsm_coords		coordinates of bsm stations, from the latest listing
* bsm_gaps		gap-distances and dates, for linearization
* bsm_igppGeod		geodetic distances from my office to the PBO BSM sites
* bsm_regions		region classifications

PBO longbase laser strainmeters
-------------------------------

* lsm_coords		coordinates and gains for LSM sites, including inactive
			YMS, and non-PBO sites PFO[123] and DHL1

PBO GPS
-------

* cgps_coords		coordinates of continuous gps sites, from latest listing

PBO borehole network
--------------------

* pbo_stations_raw	raw listing of PBO stations, from NCEDC/IRIS-DMC

### generated from the raw listing:

* pbo_stations_instrumentcount	counts of the various instrument types in the network
* pbo_stations_accel	stations having strong motion accelerometers
* pbo_stations_bsm	stations having borehole strainmeters
* pbo_stations_lsm	stations having laser strainmeters
* pbo_stations_pp		stations having pore pressure sensors
* pbo_stations_seis	stations having seismometers (geophones)
