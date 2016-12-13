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
#head(as.character( 1:nrow(x) ))
mydata<-list(x=x,y=y,geneid=as.character(1:nrow(x)), genenames=rownames(x))
model<-pamr.train(mydata,threshold=seq(0,4, 0.1))
pamr.plotcen(model, mydata, threshold=1)
pamr.plotcen(model, mydata, threshold=2.5)


a_model<-pamr.listgenes(model,mydata,threshold=2.5)
a_model[,1] #Variablerna
my_variables_index<-as.numeric(a_model[,1])
my_variables<-colnames(data)[my_variables_index]
cat(paste(my_variables, collapse="\n")) #Here they are listed in propar manner.
length(my_variables)

cv_model<-pamr.cv(model,mydata)
print(cv_model)
pamr.plotcv(cv_model)

#Tror jag har allt jag behöver ovan, utan att påstå att jag fattat vad som händer.
#koden stulen fr:
#https://liuonline.sharepoint.com/sites/732A95/732A95-2016HT/CourseDocuments/lecture3b_bl2.pdfhttps://liuonline.sharepoint.com/sites/732A95/732A95-2016HT/CourseDocuments/lecture3b_bl2.pdf
#slide 11
