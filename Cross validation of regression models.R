library(forecast)
myData <- housing_starts

# Convert the Date column to proper Date objects
myData$Date <- as.Date(paste0("01-", myData$Date), format = "%d-%b")

newData <- ts(myData$Date, start = c(2010,1), end = c(2020,4),
frequency=4)

TData <- window(newData, end = c(2018, 4))
VData <- window(newData, start = c(2019, 1))

Reg1 <- tslm(TData ~ trend + season)
Reg2 <- tslm(TData ~ trend + I(trend^2) + season)

nV <- length(VData)
fReg1 <- forecast(Reg1, h=nV)
fReg2 <- forecast(Reg2, h=nV)
accuracy(fReg1,VData)
accuracy(fReg2,VData)

RegFin <- tslm(newData ~ trend + I(trend^2) + season)
forecast(RegFin, h=4)