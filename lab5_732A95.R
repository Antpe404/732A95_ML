#Lab 5 732A95 Kernels
stations<-read.csv("data/stations.csv", sep=",", header=T)
temps<-read.csv("data/temps50k.csv", sep=",", header=T)
library(geosphere)

set.seed(1234567890)
st <- merge(stations,temps,by="station_number")

h_distance <- 100000 # These three values are up to the students
  h_date <- 7#dessa tre är alltså smoothing factor för passage_of_tome
  h_time <- 2

a <- 58.4274 # The point to predict (up to the students)
b <- 14.826

pred_date <- as.Date("2010-12-24") # The date to predict (up to the students)
times <- c("02:00:00","04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00", "24:00:00")
pred_time<-("12:00:00")
temp <- vector(length=length(times))
# Students’ code here

######Kernel function

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
#?distHaversine()
#latlong<-data.frame(cbind(latitude=st$latitude, longitude=st$longitude))
#which (grepl(pattern = "Norrköping", x = st$station_name)) #16818
#st[16818,] #Nkpg! station_number=86340

#nkpg<-st[st$station_number==86340,]
#nkpg_latlong<-c(nkpg$latitude[1], nkpg$longitude[1])

#distances<-distHaversine(p1 = latlong , p2 = nkpg_latlong)
#which (grepl(pattern = "Kiruna", x = st$station_name)) #slaka@16398 kiruna 47631
#which (grepl(pattern = "Finspång", x = st$station_name)) #slaka@16398 kiruna 47631 Fsp 16482

#distances[16398] #avståndet till slaka.
#distances[47631] #avståndet till kiruna.
#distances[16482] #avståndet till Fsp.
#distances[16818] #avstånd till nkpg, rimligt.

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
#dates<-as.Date(st$date)
#difftime(time1 = dates[1], time2=dates[2]) #Ger antalet dagar mellan första två datesen.
#difftime(time1 = pred_date, time2=dates[1]) 
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

#> exp(-((176**2)/(7**2)))
#[1] 2.850902e-275
#> max(time_ker)
#[1] 2.850902e-275
#sum(time_ker)
#res typ inget när man kör 2016-12-24

#-------------------------------------------------------------------------------------

totfunk<-function(data=st, h_time, h_date, h_distance, city_coord, pred_time, pred_date){
  #data<-st
  #city_coord<-nkpg_longlat
  hours<-substr((data$time), start=1, stop=8) #Defined to be able to cut the data
  dates<-as.Date(data$date) #Same here
  #head(dates)<as.Date("2003-01-01")
  #paste(head(dates), head(hours))<paste(pred_date, pred_time)
  #paste(head(dates), head(hours))<paste(as.Date("2000-01-01"), pred_time)
  logic_date_vector<-paste(dates, hours)<paste(pred_date, pred_time)
  data<-data[logic_date_vector,]
  
  longlat<-data.frame(cbind(longitude=data$longitude, latitude=data$latitude))
  
  dates<-as.Date(data$date) #Redefining them here after the data subset
  hours<-substr((data$time), start=1, stop=8)
  
  dist_ker<-gauss_kernel(diff=dist_func(data=longlat, city_coord=city_coord),h=h_distance)
  time_ker<-gauss_kernel(diff=time_func(prediction_date=pred_date, dates=dates), h=h_date)
  hour_ker<-gauss_kernel(diff=hour_func(time_to_predict=pred_time, hours=hours), h=h_time)
  
  predict<-(sum((dist_ker*data$air_temperature)+(time_ker*data$air_temperature)+(hour_ker*data$air_temperature)))/
    sum(hour_ker + time_ker + dist_ker)
  
  #predict<-sum((prod((dist_ker*data$air_temperature),(time_ker*data$air_temperature),(hour_ker*data$air_temperature))))/
    #prod(sum(hour_ker),sum(time_ker),sum(dist_ker))
  return(predict)
}

totfunk(h_time=3, h_date=4, h_distance=100000, city_coord=nkpg_longlat, pred_time=pred_time,
        pred_date = pred_date)

totfunk(h_time=3, h_date=4, h_distance=100000, city_coord=nkpg_longlat, pred_time="00:00:00",
        pred_date = pred_date)

totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time="02:00:00",
        pred_date = as.Date("2016-12-24"))

totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time="12:00:00",
        pred_date = as.Date("2016-12-24"))

for (i in 1:length(times)){
  temp[i]<-totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time=times[i],
                pred_date = as.Date("2016-12-24"))
}

hourly_predictions<-data.frame(cbind(times, temp=round(temp, 5)))

for (i in 1:length(times)){
  temp[i]<-totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time=times[i],
                   pred_date = as.Date("2016-07-24"))
}

hourly_predictions2<-data.frame(cbind(times, temp=round(temp, 5)))

#Detta beror på att time_ker blir så jävla liten, så den påverkar inte alls. Det enda som räknas in är alltså
#distance och tid på dygnet, vilket ju är samma för vinter och sommar. Detta eftersom jag predikterar så långt 
#fram i nuläget, alltså julafton i år. Alla time_ker blir assmå. Jag skulle kunna göra den kerneln cyklisk också,
#på samma sätt som hour. Dvs antal veckor ifrån prediktering tex. Det skulle göra att man tappade den linjära
#trenden i passage_of_time, men göra den till en "tid på året"-kernel istället. Detta är dock ett mindre problem
#om prediktionen görs på ett datum med data ikring.

for (i in 1:length(times)){
  temp[i]<-totfunk(h_time=2, h_date=7, h_distance=100000, city_coord=c(a,b), pred_time=times[i],
                   pred_date = as.Date("2016-07-24"))
}

hourly_predictions_multi<-data.frame(cbind(times, temp=round(temp, 5)))
