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
#Prolly choose SVM. Lowest test error. That's fucking why?
#---------------------------------------------------------------------------------------#

#1.3
################Kladd################

alpha<-0.05
pvalues<-data.frame(matrix(NA, nrow = ncol(data)-1, ncol = 2))
for (i in 1:(ncol(data)-1)){
  pvalues[i,2]<- t.test(data[,i]~data[, which_col], alternative="two.sided")$p.value
  pvalues[i,1]<-paste(colnames(data)[i])
}
colnames(pvalues)<-c("variable", "pvalue")
pvalues<-pvalues[order(pvalues$pvalue), ]

plot(x=1:nrow(pvalues), y=pvalues$pvalue, 
xlab="features ordered by p-value", ylab="p-value", las=1)


pvalues$Loss<-NA
for (j in 1:nrow(pvalues)){
  pvalues[j,3]<-(alpha*j)/nrow(pvalues)
}

pvalues$selected<-"Not rejected"
pvalues[pvalues$pvalue<pvalues$Loss,4]<-"Rejected" #Sätter de med lägre p än Loss till 1.
pvalues$selected<-as.factor(pvalues$selected)

plot(x=1:100, y=pvalues[1:100,2], col=pvalues$selected, xlab="features ordered by p-value",
     ylab="p-value", las=1, main="Rejected hypotheses by Benjamini Hochberg method")
legend('topleft', legend = levels(pvalues$selected), col = 1:2, cex = 0.8, pch = 1)

plot(x=1:nrow(pvalues), y=pvalues[,2], col=pvalues$selected, xlab="features ordered by p-value",
     ylab="p-value", las=1, main="Rejected hypotheses by Benjamini Hochberg method")

#sum(pvalues$pvalue<pvalues$Loss)
#I reject the first 39 hypotheses, i.e. i keep those variables to use in my model.
#Since they are ordered, those are the first 39 rows in pvalues-dataframe

selected_features_BH<-pvalues[pvalues$pvalue<pvalues$Loss, 1]


#--------------------------------KLADD----------------------------------------
res<-t.test(X10th~Conference,data=data,
            alternative="two.sided")
res<-t.test(X000euro~Conference,data=data,
            alternative="two.sided")

res<-t.test(members~Conference,data=data,
            alternative="two.sided")

res<-t.test(papers~Conference,data=data, alternative="two.sided") #papers significant, rimligt.

#which(colnames(data)=="papers") 3036
res$p.value

res<-t.test(data[,1]~data[, 4703], alternative="two.sided")
res<-t.test(data[,3036]~data[, 4703], alternative="two.sided")

res$p.value

test2<-oneway.test(X10th~Conference, data=data,paired=FALSE)

#test<-lm(as.numeric(Conference)~X000euro-1, data=data)
#summary(test)

pvalues<-vector(length=ncol(data)-1)
for (i in 1:(ncol(data)-1)){
  pvalues[i]<- t.test(data[,i]~data[, which_col], alternative="two.sided")$p.value
}
#low p-values indicating that I reject H0: has no effect on target.

head(pvalues)
pvalues<-as.data.frame(pvalues, ncol=1)
rownames(pvalues)<-colnames(data)[1:(ncol(data)-1)]

pvalues[,1]<-sort(pvalues[,1])
#---------------------------------------------------------

###########################Assignment 2###############################################
spam<-read.csv2("data/spambase_lab3bl2.csv", sep=";", header=T)
spam$Spam[spam$Spam==0]<--1 #Convertar för att underlätta SVM:s.

set.seed(1234567890)
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)] #Shufflar och ta bort 
h <- 1
b<-0
beta <- c(0, -.05)
  M <- c(500, 20)
  M1<-500
  M2<-20
  N <- 500 # number of training points

gaussian_k <- function(x, h) { # Gaussian kernel
ans<-exp(-(x**2)/(h**2))
return(ans)
}

SVM <- function(sv,i) { # SVM on point i with support vectors sv
  # Your code here
  # Note that the labels in spambase.csv are 0/1 and SVMs need -1/+1. Then, use 2*label-1 # to convert from 0/1 to -1/+1
  # Do not include the labels when computing the Euclidean distance between the point i 
  # and each of the support vectors. This is the distance to use in the kernel function
  # You can use dist() to compute the Euclidean distance
}

errors <- 1
errorrate <- vector(length = N)
errorrate[1] <- 1
sv <- c(1)
step4<-vector(length=N-1)

#m_rad<-spam[sv, -49] #x_m
#m_response<-spam[sv, 49] #t_m
x_m<-spam[sv, -49] #x_m
t_m<-spam[sv, 49] #t_m
for(i in 2:N) {
  x_i<-spam[i, -49]
  t_i<-spam[i, 49]
  distance<-dist(rbind(x_i, x_m), method="euclidean")
  step4[i-1]<-sum(t_m*gaussian_k(x=distance, h=h)+b)
  #ans<-sum(step4)
  #anss<-sum(t_m*gaussian_k(x=distance, h=h)+b)
}
sum(step4)
plot(errorrate[seq(from=1, to=N, by=10)], ylim=c(0.2,0.4), type="o")
length(sv)
errorrate[N]

#Test nedan.
#step3
x_i<-spam[3, -49] #detta är ex på x_i
t_i<-spam[3, 49] #detta är ex på t_i

m_rad<-spam[sv, -49] #x_m
m_response<-spam[sv, 49] #t_m
#step4
x_i
m_rad
test<-rbind(x_i, m_rad)
test_dist<-as.matrix(dist(test, method="euclidean"))
sum(m_response*gaussian_k(x=test_dist[-1,1], h=h)+b) #step4 slide, y(x_i)
