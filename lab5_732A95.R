#Lab 5 732A95 Kernels
stations<-read.csv("data/stations.csv", sep=",", header=T)
temps<-read.csv("data/temps50k.csv", sep=",", header=T)

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
pred_date <- as.Date("2016-12-21") # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
pred_time<-("22:00:00")
temp <- vector(length=length(times))
# Students’ code here

#ngt sånthär, men x-xnew måste ju vara avstånde på ngt sätt. 
#h ska kunna varieras om jag förstår det rätt.
gau_kernel<-function(x, xnew, h){
  svar<-exp(-((x-xnew)**2)/h) #h motsvarar 2sd^2
  return(svar)
  }

#############Börjar med att ta fram alla distances, vektorn med namnet distances.
?distHaversine()
latlong<-data.frame(cbind(latitude=st$latitude, longitude=st$longitude))
which (grepl(pattern = "Norrköping", x = st$station_name)) #16818
st[16818,] #Nkpg! station_number=86340

nkpg<-st[st$station_number==86340,]
nkpg_latlong<-c(nkpg$latitude[1], nkpg$longitude[1])

distances<-distHaversine(p1 = latlong , p2 = nkpg_latlong)
which (grepl(pattern = "Kiruna", x = st$station_name)) #slaka@16398 kiruna 47631
which (grepl(pattern = "Finspång", x = st$station_name)) #slaka@16398 kiruna 47631 Fsp 16482

distances[16398] #avståndet till slaka.
distances[47631] #avståndet till kiruna.
distances[16482] #avståndet till Fsp.
distances[16818] #avstånd till nkpg, rimligt.


###########Datum-avstånden
#nöjer mig mig antal dagar här i nuläget, vet inte om man ev ska ha den konti i timmar också, dvs typ 
#rbinda st$time and st$date och sen räkna i units="hours" ist.
dates<-as.Date(st$date)
difftime(time1 = dates[1], time2=dates[2]) #Ger antalet dagar mellan första två datesen.
difftime(time1 = pred_date, time2=dates[1]) 

passage_of_time=difftime(time1 = pred_date, time2=dates, units="days") 
#detta är antalet dagar fr alla mätningar till mitt datum


###############timmar-avstånden.

hours<-substr(st$time, start=1, stop=8)

hour_func<-function(time_to_predict=pred_time){
hours_differential<-c(length(hours))
for (i in 1:length(hours)){
  klockslag<-as.difftime(c(hours[i], time_to_predict), format="%H:%M:%S", units="hours")
  if (abs(klockslag[1]-klockslag[2])>12){
    hours_differential[i]<-24-abs(klockslag[1]-klockslag[2])
  }
  else{
    hours_differential[i]<-abs(klockslag[1]-klockslag[2])
}
}
return(hours_differential)  
}

hours_diff<-hour_func()
