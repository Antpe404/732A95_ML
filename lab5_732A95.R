#Lab 5 732A95 Kernels
stations<-read.csv2("data/stations.csv", sep=",", header=T)
temps<-read.csv2("data/temps50k.csv", sep=",", header=T)

#Nedan Josés kod
set.seed(1234567890)
library(geosphere)
#stations <- read.csv("stations.csv")
#temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- # These three values are up to the students
  h_date <- #dessa tre är alltså smoothing factors.
  h_time <-
  a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length=length(times))
# Students’ code here
plot(temp, type="o")