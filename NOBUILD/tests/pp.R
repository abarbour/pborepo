library(pborepo)

redo <- FALSE
if (!exists('xpp') | redo) xpp<-pore.pressure("B084",2010,94)
if (!exists('xpt') | redo) xpt<-pore.temperature("B084",2010,94)

layout(matrix(1:2))
plot(xpp, type='l')
plot(xpt, type='l')
layout(matrix(1))

# test windowing
st <- as.POSIXct("2010-04-04 00:00:00", tz='UTC')
en <- as.POSIXct("2010-04-04 23:59:59", tz='UTC')
if (!exists('dat')) dat <- unavco_dataload("B084",2010,94, dattype="pp")
plot(consistent(window(dat, start.=end(dat)-7200)), type='l')

# window in on the El Mayor quake:
st <- as.POSIXct("2010-04-04", tz='UTC') + 79200
redo <- FALSE
if (!exists('xppw') | redo) xppw <- pore.pressure("B084", 2010, 94, start.=st)
if (!exists('xptw') | redo) xptw <- pore.temperature("B084", 2010, 94, start.=st)

layout(matrix(1:4,ncol=2))
plot(xpp, type='l')
plot(xpt, type='l')
plot(xppw, type='l')
plot(xptw, type='l')
layout(matrix(1))
