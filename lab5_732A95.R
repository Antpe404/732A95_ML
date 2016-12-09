#Lab 5 732A95 Kernels
stations<-read.csv("data/stations.csv", sep=",", header=T)
temps<-read.csv("data/temps50k.csv", sep=",", header=T)

#Nedan Josés kod
set.seed(1234567890)
library(geosphere)
#stations <- read.csv("stations.csv")
#temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 10000 # These three values are up to the students
  h_date <- 5#dessa tre är alltså smoothing factors.
  h_time <- 3
  a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
pred_date <- as.Date("2013-12-21") # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", ..., "24:00:00")
pred_time<-("22:00:00")
temp <- vector(length=length(times))
# Students’ code here

#ngt sånthär, men x-xnew måste ju vara avstånde på ngt sätt. 
#h ska kunna varieras om jag förstår det rätt.
#gau_kernel<-function(x, xnew, h){
#  svar<-exp(-((x-xnew)**2)/h) #h motsvarar 2sd^2
#  return(svar)
#  }
#dist_ker<-gau_kernel(x=distances, xnew=0,h=h_distance)
#plot(x=1:50000, sds)

#time_ker<-gau_kernel(x=passage_of_time, xnew=)

#Tror nedan är rätt eftersom jag bara har diff
gauss_kernel<-function(diff, h){
  svar<-exp(-((diff**2)/(h**2))) #h motsvarar 2sd^2
  return(svar)
}

dist_ker<-gauss_kernel(diff=dist_func(city_coord = nkpg_longlat), h=h_distance)
plot(x=1:50000, dist_ker)

time_ker<-gauss_kernel(diff=time_func(prediction_date=pred_date), h=h_date)
plot(x=1:50000, time_ker)

hour_ker<-gauss_kernel(diff=hour_func(), h=h_time)
plot(x=1:50000, hour_ker)

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

#----------------------------------------------
longlat<-data.frame(cbind(longitude=st$longitude, latitude=st$latitude))
nkpg<-st[st$station_number==86340,]
nkpg_longlat<-c(nkpg$longitude[1], nkpg$latitude[1])

dist_func<-function(data=data.frame(cbind(longitude=st$longitude, latitude=st$latitude)), city_coord){
  distances<-distHaversine(p1 = data , p2 = city_coord)
  return(distances)
}

dist_diff<-dist_func(city_coord = nkpg_longlat)
  
###########Datum-avstånden
#nöjer mig mig antal dagar här i nuläget, vet inte om man ev ska ha den konti i timmar också, dvs typ 
#rbinda st$time and st$date och sen räkna i units="hours" ist.
dates<-as.Date(st$date)
difftime(time1 = dates[1], time2=dates[2]) #Ger antalet dagar mellan första två datesen.
difftime(time1 = pred_date, time2=dates[1]) 
#----------------------------------------------------------------------
dates<-as.Date(st$date)

time_func<-function(prediction_date, dates=as.Date(st$date)){
passage_of_time=as.numeric(difftime(time1 = prediction_date, time2=dates, units="days")) 
#detta är antalet dagar fr alla mätningar till mitt datum
return(passage_of_time)
}

time_diff<-time_func(prediction_date=pred_date)

###############timmar-avstånden.

hours<-substr((st$time), start=1, stop=8)
#test<-substr((st$time), start=4, stop=8)
#unique(test)

hour_func<-function(time_to_predict=pred_time, hours=substr((st$time), start=1, stop=8)){
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


#De ovanstående callsen nu, dvs hours_diff, time_diff och dist_diff är så att säga mina x-xnew i gau_kernel.

##################################Övergripande funktion####################################

dist_ker<-gauss_kernel(diff=dist_func(city_coord = nkpg_latlong), h=10000000)
plot(x=1:50000, dist_ker)

time_ker<-gauss_kernel(diff=time_func(prediction_date=pred_date), h=h_date)
plot(x=1:50000, time_ker)

hour_ker<-gauss_kernel(diff=hour_func(), h=h_time)
plot(x=1:50000, hour_ker)

sum((time_ker[1:6]*st$air_temperature[1:6])+
  (hour_ker[1:6]*st$air_temperature[1:6])+
  (dist_ker[1:6]*st$air_temperature[1:6]))/
  sum(time_ker[1:6]+hour_ker[1:6]+dist_ker[1:6])

#-------------------------------------------------------------------------------------
totfunk<-function(data=st, h_time, h_date, h_distance, city_coord, pred_time, pred_date){
  
  hours<-substr((data$time), start=1, stop=8)
  dates<-as.Date(data$date)
  #head(dates)<as.Date("2003-01-01")
  #paste(head(dates), head(hours))<paste(pred_date, pred_time)
  #paste(head(dates), head(hours))<paste(as.Date("2000-01-01"), pred_time)
  logic_date_vector<-paste(dates, hours)<paste(pred_date, pred_time)
  data<-data[logic_date_vector,]
  
  dist_ker<-gauss_kernel(diff=dist_func(city_coord=city_coord),h_distance)
  time_ker<-gauss_kernel(diff=time_func(prediction_date=pred_date), h=h_date)
  hour_ker<-gauss_kernel(diff=hour_func(), h=h_time)
  
  predict<-(sum((dist_ker*st$air_temperature)+(time_ker*st$air_temperature)+(hour_ker*st$air_temperature)))/
    sum(hour_ker+ time_ker+ dist_ker)
  return(predict)
}

totfunk(h_time=3, h_date=4, h_distance=10000, city_coord=nkpg_longlat, pred_time=pred_time,
        pred_date = pred_date)

totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time="02:00:00",
        pred_date = as.Date("2016-12-24"))
