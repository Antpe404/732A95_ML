#Lab 3bl2

#Assignment 1
data<-read.csv2("data/data.csv", header=T, sep=";")
#Wide data.
set.seed(12345)
data<-data[sample(1:nrow(data)), ] #Shufflar raderna
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

#-------------------------------KLADD
pamr.plotcen(model, mydata, threshold=1)
pamr.plotcen(model, mydata, threshold=2.5)
a_model<-pamr.listgenes(model,mydata,threshold=2.5)
a_model[,1] #Variablerna
my_variables_index<-as.numeric(a_model[,1])
my_variables<-colnames(data)[my_variables_index]
cat(paste(my_variables, collapse="\n")) #Here they are listed in propar manner.
length(my_variables)
#--------------------------------
cv_model<-pamr.cv(model,mydata)
cv_model$error#.13 lägst, fr 0.7-1.8 och 2.6
#which((cv_model$error)==min(cv_model$error))
#which((cv_model$loglik)==max(cv_model$loglik))#30
print(cv_model) #Här kan man se samma, 0.7-1.8 eller 2.6
pamr.plotcv(cv_model)

chosen_model<-pamr.train(mydata,threshold=2.6) #Väljer 2.6 för simplicity.
pamr.plotcen(chosen_model, mydata, threshold=2.6)

cv_vars<-pamr.listgenes(chosen_model,mydata,threshold=2.6)
cv_vars[,1] #Variablerna
my_variables_index2<-as.numeric(cv_vars[,1])
my_variables2<-colnames(data)[my_variables_index2]
cat(paste(my_variables2, collapse="\n")) #Here they are listed in propar manner.
length(my_variables2)

x_test<-t(test[,-which_col]) #Alla utom Conf
y_test<-test[, which_col] #endast Conf
#head(as.character( 1:nrow(x) ))
my_testdata<-list(x=x_test,y=y_test,geneid=as.character(1:nrow(x)), genenames=rownames(x))

conf_mat_NSC<-table(preds=pamr.predict(chosen_model,my_testdata$x,threshold = 2.6),truth=my_testdata$y)
conf_mat_NSC
#Tror jag har allt jag behöver ovan, utan att påstå att jag fattat vad som händer.
number_of_features_NSC<-length(my_variables2)
test_error_rate_NSC<-(conf_mat_NSC[1,2]+conf_mat_NSC[2,1])/sum(conf_mat_NSC)
test_error_rate_NSC
#koden stulen fr:
#https://liuonline.sharepoint.com/sites/732A95/732A95-2016HT/CourseDocuments/lecture3b_bl2.pdfhttps://liuonline.sharepoint.com/sites/732A95/732A95-2016HT/CourseDocuments/lecture3b_bl2.pdf
#slide 11


#1.2
#1.2a
#	Elastic net, binomial response and alpha=0.5, penalty selected by cross-validation

library(glmnet)  

train_x<-as.matrix(train[,-which_col])
train_y<-as.factor(train[,which_col])
test_x<-as.matrix(test[, -which_col])
test_y<-as.factor(test[,which_col])

#glmnet(x=train_x, y=train_y, alpha=.5, family="binomial")
#cv.glmnet verkar vara crossvalidated samma.
set.seed(12345)
elastic<-cv.glmnet(x=train_x, y=train_y, alpha=.5, family="binomial")
plot(elastic)

elastic$lambda
elastic$

elastic_coe<-coefficients(elastic)
elastic_coeff<-elastic_coe[,1]
elastic_coef<-elastic_coeff[elastic_coeff!=0] #Tar ut de som inte är 0.

elastic_coefficients<-as.data.frame(cbind(coef=as.numeric(elastic_coef), variable=names(elastic_coef)))
#12 variabler med coef skild fr 0.
fitted<-predict(elastic, newx = test_x, s = elastic$lambda.1se, type="class") #default s.

fitted
length(fitted)
fitted<-as.numeric(fitted)

number_of_features_elastic<-nrow(elastic_coefficients)-1
conf_mat_elastic<-table(preds=fitted, truth=test_y)
test_error_rate_elastic<-(conf_mat_elastic[1,2]+conf_mat_elastic[2,1])/sum(conf_mat_elastic)

fitted
test_y

#1.2b
library(kernlab)
set.seed(12345)
my_svm<-ksvm(x=train_x, y=train_y, kernel="vanilladot", scale=FALSE, type="C-svc")
my_svm #training error 0.02222, 44 suppoert vectors
my_svm@coef #the corresponding coefficients times the training labels.
length(my_svm@coef[[1]])
my_svm@error
my_svm@nSV #The number of support vectors. Think this is the optimal size

fitted_svm<-predict(my_svm, test_x)
conf_mat_svm<-table(preds=fitted_svm, truth=test_y)
conf_mat_svm

test_error_rate_svm<-(conf_mat_svm[1,2]+conf_mat_svm[2,1])/sum(conf_mat_svm)

number_of_features_svm<-my_svm@nSV

#sum(predict(my_svm, train_x)==train_y) 44/45 rätt i träning->0.02222

#########################Comparison, last thing in ass 1.2###############################
comparison_table<-data.frame(cbind(rbind(number_of_features_NSC, test_error_rate_NSC), rbind(number_of_features_elastic, test_error_rate_elastic),
rbind(number_of_features_svm, test_error_rate_svm)))

colnames(comparison_table)<-c("NSC", "Elastic", "SVM")
rownames(comparison_table)<-c("# of features", "test_error_rate")
comparison_table[1,]<-as.integer(comparison_table[1,])
comparison_table
#---------------------------------------------------------------------------------------#