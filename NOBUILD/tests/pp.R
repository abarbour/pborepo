library(pborepo)

redo <- FALSE
if (!exists('xpp') | redo) xpp<-pore.pressure("B084",2010,94)
if (!exists('xpt') | redo) xpt<-pore.temperature("B084",2010,94)

layout(matrix(1:2))
plot(xpp, type='l')
plot(xpt, type='l')
layout(matrix(1))