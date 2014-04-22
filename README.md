This project is intended to be a storage repository for metadata and other bits of
data for [PBO](http://pbo.unavco.org/), but it also
serves as a self-consistent R-package, meaning
one can do the following
~~~~~{.R}
install.packages("devtools",dependencies=TRUE)
library(devtools)
install_github("pborepo", username = "abarbour")
~~~~~
and from then on
~~~~~{.R}
library(pborepo)
?pborepo
~~~~~
will should work.

This project is developing slowly: metadata rarely changes, but the R-functions
likely will.
So,
you should re-install often, although once the package is of suitable
completeness, I may upload it to [CRAN](http://cran.r-project.org/).

Feel free to contact me (<a href="https://github.com/abarbour" class="user-mention">@abarbour</a>) 
should you have questions, or wish to see other data included; or, use github as it was
intended and commit some changes of your own! :)

----
####Metadata
----

PBO borehole strainmeters
-------------------------

~~~~~{.R}
data(bsmmeta)
~~~~~

`bsm_azimuths`
* azimuths of the BSM channels **if Ch0==0 => assumed (and likely not correct)**

`bsm_coords`
* coordinates of bsm stations, from the latest listing

`bsm_gaps`
* gap-distances and dates, for linearization

`bsm_igppGeod`
* geodetic distances from my office to the PBO BSM sites

`bsm_regions`
* region classifications

* calibration coefficients
~~~~~{.R}
data(bsmCalibrations)
?calibration_matrix
~~~~~

PBO longbase laser strainmeters
-------------------------------

`lsm_coords`
* coordinates and gains for LSM sites, including inactive
YMS, and non-PBO sites PFO[123] and DHL1

PBO GPS
-------

`cgps_coords`
* coordinates of continuous gps sites, from latest listing

PBO borehole network
--------------------

`pbo_stations_raw`
* raw listing of PBO stations, from NCEDC/IRIS-DMC

### generated from the raw listing:

`pbo_stations_instrumentcount`
* counts of the various instrument types in the network

`pbo_stations_accel`
* stations having strong motion accelerometers

`pbo_stations_bsm`
* stations having borehole strainmeters

`pbo_stations_lsm`
* stations having laser strainmeters

`pbo_stations_pp`
* stations having pore pressure sensors

`pbo_stations_seis`
* stations having seismometers (geophones)

---

This project was started in March 2012

