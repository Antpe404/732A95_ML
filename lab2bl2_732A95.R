#######Lab 2 block 2

#Assignment 2.
bodyfat<-read.csv2("data/bodyfatregression.csv", sep=";", header=T)
library(tree)

#2.1
set.seed(1234567890)
#sampling<-sample(1:nrow(bodyfat))
bodyfat_samplad<-bodyfat[sample(1:nrow(bodyfat)),]
bodyfat_tr<-bodyfat_samplad[1:73,] #Traningsset
bodyfat_te<-bodyfat_samplad[74:110,] #testset


#The lower bound

felen<-integer(0)
set.seed(1234567890)
for ( i in 1:100){
  saf<-sample(1:nrow(bodyfat_tr), replace=T)
  bodyfat_bag<-bodyfat_tr[saf,]
  fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_bag, split="deviance")
  fitsen<-predict(fat_tree, newdata=bodyfat_te)
  felen[i]<-sum(fitsen-bodyfat_te$Bodyfat_percent)
}
lowerbound<-mean((felen/length(felen))**2)
#This is the lower bound! 

#The upper bound ska 
#"At least, the bagged error is never larger than the average individual errors:"
#mao vara average felet.

#The upper bound ska istallet vara mean av felen:

felen_upper<-integer(0)
set.seed(1234567890)
for ( i in 1:100){
  saf<-sample(1:nrow(bodyfat_tr), replace=T)
  bodyfat_bag<-bodyfat_tr[saf,]
  fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_bag, split="deviance")
  fitsen<-predict(fat_tree, newdata=bodyfat_te)
  felen_upper[i]<-mean((fitsen-bodyfat_te$Bodyfat_percent)**2)
}
#Plockar nu ut mean av alla de individuella felen nedan.
upperbound<-mean((felen_upper/length(felen_upper)))
#upperbound<-mean(felen_upper)

#2.2----------------------------------------------------------------------------------
#blir alltsa 3 modeller for varje bagging, om jag tanker ratt. Forst en modell pa fold (1,2), sen (1,3), sen (2,3)
#borjar med att sampla 
#set.seed(1234567890)

folds<-3
set.seed(1234567890)
bodyfat_samplad<-bodyfat[sample(1:nrow(bodyfat)),]
folds_data<-suppressWarnings(split(bodyfat_samplad, 1:folds)) 

baggingar<-100
set.seed(1234567890)
folds_data<-suppressWarnings(split(bodyfat, 1:folds)) 

alla_fel<-matrix(0, nrow=folds, ncol=baggingar)

for (i in 1:folds){
  training<-folds_data[-i]
  del1_train<-data.frame(training[1])
  colnames(del1_train)<-colnames(bodyfat)
  del2_train<-data.frame(training[2])
  colnames(del2_train)<-colnames(bodyfat)
  training<-rbind(del1_train, del2_train)
  testing<-data.frame(folds_data[i])
  colnames(testing)<-colnames(bodyfat)
  
  for (j in 1:baggingar){
   

#set.seed(1234567890)
  urval<-sample(1:nrow(training), replace=T)
  bodyfat_bag<-training[urval,]
  fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_bag, split="deviance")
  fitsen<-predict(fat_tree, newdata=testing)
  alla_fel[i, j]<-mean((fitsen-testing$Bodyfat_percent)**2)
}
}

upperbound_2<-mean(alla_fel)

#The upper bound ar mean av alla individuella fel. Rimligt att detta bound ar ngt hogre, eftersom vi kor 3fold CV?

#2.3




#######Assignment 4
spam<-read.csv2("data/spambaselab2b2.csv", sep=";", header=T)
spam$Spam<-as.factor(spam$Spam)
set.seed(1234567890)
spam_samplad<-spam[sample(1:nrow(spam)), ]
spam_tr<-spam_samplad[1:round((2/3)*nrow(spam)), ]
spam_te<-spam_samplad[-(1:round((2/3)*nrow(spam))), ]

#install.packages("mboost")
library(mboost)
#install.packages("randomForest")
library(randomForest)
library(ggplot2)

#Förklaringar till inställningarna:
#AdaExp() uses the exponential loss, which essentially 
#leads to the AdaBoost algorithm of Freund and Schapire (1996).

#boost_control(mstop = 100, nu = 0.1,
#              risk = c("inbag", "oobag", "none"), stopintern = FALSE,
#              center = TRUE, trace = FALSE)
#Arguments

#mstop	
#an integer giving the number of initial boosting iterations.

#--------
#Examples


### a simple two-dimensional example: cars data
#cars.gb <- blackboost(dist ~ speed, data = cars,
 #                     control = boost_control(mstop = 50))

modellen<-blackboost(Spam~., data=spam_tr, family=AdaExp(), control=boost_control(mstop=10))

predict(modellen, newdata=spam_te, type="class")
tejbell_test<-table(pred=predict(modellen, newdata= spam_te, type="class"), truth=spam_te$Spam)

error_rate_test<-1-sum(diag(tejbell_test))/sum(tejbell_test)

tejbell_train<-table(predict(modellen, newdata= spam_tr, type="class"), spam_tr$Spam)
error_rate_train<-1-sum(diag(tejbell_train))/sum(tejbell_train)
#Sahar kan man ocksa rakna.
1 - (sum(predict(modellen, spam_te, type="class") == spam_te$Spam) / nrow(spam_te))

#----------------------------------Adaboost classification tree-------------------------------------------
sekvens<-seq(10,100, 10)
training_errors<-integer()
test_errors<-integer()
index<-1
for (i in sekvens){
  modellen_ct<-blackboost(Spam~., data=spam_tr, family=AdaExp(),  control=boost_control(mstop=i))
  
  tejbell_train<-table(pred=predict(modellen_ct, newdata= spam_tr, type="class"), truth=spam_tr$Spam)
  training_errors[index]<-1-sum(diag(tejbell_train))/sum(tejbell_train)
  
  tejbell_test<-table(pred=predict(modellen_ct, newdata= spam_te, type="class"), truth=spam_te$Spam)
  test_errors[index]<-1-sum(diag(tejbell_test))/sum(tejbell_test)
  index<-index+1
}

plotredo_ct<-data.frame(cbind(sekvens,training_errors, test_errors))

ggplot(data=plotredo_ct)+geom_point(aes(x=sekvens, y=training_errors, col="error train"))+
  geom_line(aes(x=sekvens, y=training_errors, col="error train"))+
  geom_point(aes(x=sekvens, y=test_errors, col="error test"))+
  geom_line(aes(x=sekvens, y=test_errors, col="error test"))+xlab("Number of trees")+
  ylab("Error rate")+ggtitle("Evaluation of Adaboost, classication tree")



#----------------------------------Adaboost classification tree-------------------------------------------
#Kladd
randomForest()
set.seed(1234567890)
modellen_rf<-randomForest(Spam ~ ., data=spam_tr, ntree=100, norm.votes=T)

modellen_rf
modellen_rf$err.rate
modellen_rf$predicted
modellen_rf$terms
predict(modellen_rf)

all(predict(modellen_rf)==modellen_rf$predicted)
modellen_rf$confusion

tr_tab<-table(predict(modellen_rf, newdata= spam_tr, type="class"), spam_tr$Spam)
err_tr<-1-sum(diag(tr_tab))/sum(tr_tab)
test_tab<-table(predict(modellen_rf, newdata= spam_te, type="class"), spam_te$Spam)
err_test<-1-sum(diag(test_tab))/sum(test_tab)

predict(modellen_rf, newdata=spam_tr, type="class")
predict(modellen_rf, newdata=spam_tr, type="class")==predict(modellen_rf)


#----------------------------------Adaboost classification tree-------------------------------------------
sekvens<-seq(10,100, 10)
training_errors_rf<-integer()
test_errors_rf<-integer()
index<-1
for (i in sekvens){
  modellen_rf<-randomForest(Spam ~ ., data=spam_tr, ntree=i, norm.votes=FALSE)
  
  tr_tab<-table(predict(modellen_rf, newdata= spam_tr, type="class"), spam_tr$Spam)
  training_errors_rf[index]<-1-sum(diag(tr_tab))/sum(tr_tab)
  
  test_tab<-table(predict(modellen_rf, newdata= spam_te, type="class"), spam_te$Spam)
  test_errors_rf[index]<-1-sum(diag(test_tab))/sum(test_tab)
  index<-index+1
}

training_errors_rf
test_errors_rf

plotredo_rf<-data.frame(cbind(sekvens,training_errors_rf, test_errors_rf))

ggplot(data=plotredo_rf)+geom_point(aes(x=sekvens, y=training_errors_rf, col="error train"))+
  geom_line(aes(x=sekvens, y=training_errors_rf, col="error train"))+
  geom_point(aes(x=sekvens, y=test_errors_rf, col="error test"))+
  geom_line(aes(x=sekvens, y=test_errors_rf, col="error test"))+xlab("Number of trees")+
  ylab("Error rate")+ggtitle("Evaluation of Adaboost, random tree")









#----
#fold1<-data.frame(folds_data$`1`)
#fold2<-data.frame(folds_data$`2`)
#fold3<-data.frame(folds_data$`3`)
#set.seed(1234567890)

#komber<-combn(1:folds, folds-1)
#komber
#komber[,1]

#assign(paste("perf.a", "1", sep=""),5)

#for(i in 1:folds){
#assign(paste("set", "i", sep=""),rbind(fold[HaR vill jag ha i komber[,1]],fold2))
#}

#
#for(i in sekvens){
 # assign(paste("error_rate_", i, sep=""), 
#         i+10
         #Har vill jag ju ha in sjalva funktionen istallet, alltsa det nedan
         #modellen<-blackboost(Spam~., data=spam_tr, family=AdaExp(),  control=boost_control(mstop=i))
         #tejbell_test<-table(predict(modellen, newdata= spam_te, type="class"), spam_te$Spam)
         
         #1-sum(diag(tejbell_test))/sum(tejbell_test)
         
 # )
#}
#assign(paste("error_rate", i, sep=""),5)