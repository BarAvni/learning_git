#This script uses the function that was written in the Writting Function assignment for the 
#Learning Git assignment. 
# 1. Load data:

Tdata = read.table("metdata.txt",header = TRUE, sep = "")
day = Tdata[,c("day")]
Tair = Tdata[,c("tavg")]
year = Tdata[,c("year")]
data_tmdh = read.table("TMDH.txt",header = TRUE, sep = "")
TMDH = data_tmdh[,c("TMDH")]
month_tmdh = data_tmdh[,c("month")]

Ep = Thornthwaite(day, year, Tair, TMDH)

x = month_tmdh[4:15] 
y1 = Ep[4:15]
y2 = Ep[28:39]
y3 = Ep[88:99]

plot(x, y1, type = "p", col = "red", xlab = "month", ylab = "Ep (cm)")
par(new = TRUE)
plot(x, y2, type = "p", col = "green")
par(new = TRUE)
plot(x, y3, type = "p", col = "blue")
# Create a title with a red, bold/italic font
title(main="Monthly potential ET for 2001, 2003, 2008", col.main="black", font.main=4)
legend("topleft", legend=c("2001", "2003", "2008"),
       col=c("red", "green", "blue"), pch = 1, cex=0.5)
#This is a change
