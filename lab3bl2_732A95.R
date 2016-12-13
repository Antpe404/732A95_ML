#Lab 3bl2

#Assignment 1
data<-read.csv2("data/data.csv", header=T, sep=";")
#Wide data.
data$Conference<-as.factor(data$Conference)
train<-data[1:45, ]
test<-data[46:64, ]

#1.1

library(pamr)
which_col<-which(colnames(train)=="Conference")
x<-t(train[,-which_col]) #Alla utom Conf
y<-train[, which_col] #endast Conf
head(as.character( 1:nrow(x) ))
mydata<-list(x=x,y=y,geneid=as.character(1:nrow(x)), genenames=rownames(x))
model<-pamr.train(mydata,threshold=seq(0,4, 0.1))



data0=read.csv2("voice.csv")
data=data0
data=as.data.frame(scale(data))
data$Quality=as.factor(data0$Quality)
library(pamr)
rownames(data)=1:nrow(data)
x=t(data[,-311])
y=data[[311]]
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))
pamr.plotcen(model, mydata, threshold=1)
pamr.plotcen(model, mydata, threshold=2.5)
a=pamr.listgenes(model,mydata,threshold=2.5)
cat( paste( colnames(data)[as.numeric(a[,1])], collapse='\n' ) )
cvmodel=pamr.cv(model,mydata)
print(cvmodel)
pamr.plotcv(cvmodel)