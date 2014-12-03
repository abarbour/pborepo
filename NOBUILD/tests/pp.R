library(pborepo)
library(zoo)

# test windowing
redo <- FALSE
st <- as.POSIXct("2010-04-04 00:00:00", tz='UTC')
en <- as.POSIXct("2010-04-04 23:59:59", tz='UTC')
if (!exists('tdat') | redo) tdat <- unavco_dataload("B084",2010,94, dattype="pp")
plot(consistent(window(tdat, start.=end(tdat)-7200)), type='l')

redo <- FALSE
if (!exists('xpp') | redo) xpp<-pore.pressure("B084",2010,94)
if (!exists('xpt') | redo) xpt<-pore.temperature("B084",2010,94)

layout(matrix(1:2))
plot(xpp, type='l')
plot(xpt, type='l')
layout(matrix(1))

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

# test sampling
redo <- TRUE
if (!exists('test.dat') | redo) test.dat <- unavco_dataload("B003", 2006, 004, dattype="pp")
if (!exists('test.300') | redo) test.300 <- pore.pressure('B003', 2006, 004, as.POSIXct("2006-01-04 08:37:09", tz='UTC'), as.POSIXct("2006-01-04 10:59:30", tz='UTC'))

uncon.300 <- head(na.omit(test.300))