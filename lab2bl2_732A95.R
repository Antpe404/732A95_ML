#######Lab 2 block 2

#Assignment 2.
bodyfat<-read.csv2("data/bodyfatregression.csv", sep=";", header=T)
library(tree)

#2.1
set.seed(1234567890)
#sampling<-sample(1:nrow(bodyfat))
bodyfat_samplad<-bodyfat[sample(1:nrow(bodyfat)),]
bodyfat_tr<-bodyfat_samplad[1:73,] #Träningsset
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

#The upper bound ska istället vara mean av felen:

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

#2.2----------------------------------------------------------------------------------
#blir alltså 3 modeller för varje bagging, om jag tänker rätt. Först en modell på fold (1,2), sen (1,3), sen (2,3)
#börjar med att sampla 
#set.seed(1234567890)
#bodyfat2<-bodyfat[sample(1:nrow(bodyfat), replace=T),] 
#replace är true eftersom vi ska bagga. Datamaterialet ovan bör alltså innehålla dubletter.

set.seed(1234567890)
bodyfat_samplad<-bodyfat[sample(1:nrow(bodyfat)),]

folds<-3
baggingar<-100
folds_data<-suppressWarnings(split(bodyfat_samplad, 1:folds))
#fold1<-data.frame(folds_data$`1`)
#fold2<-data.frame(folds_data$`2`)
#fold3<-data.frame(folds_data$`3`)
alla_fel<-matrix(0, nrow=folds, ncol=baggingar)

for (i in 1:folds){
  training<-folds_data[-i]
  del1_train<-data.frame(training[1])
  colnames(del1_train)<-colnames(bodyfat_samplad)
  del2_train<-data.frame(training[2])
  colnames(del2_train)<-colnames(bodyfat_samplad)
  training<-rbind(del1_train, del2_train)
  testing<-data.frame(folds_data[i])
  colnames(testing)<-colnames(bodyfat_samplad)
  
  for (j in 1:baggingar){
   
#felen<-matrix(0, nrow=folds, ncol=baggingar)
set.seed(1234567890)
  urval<-sample(1:nrow(training), replace=T)
  bodyfat_bag<-training[urval,]
  fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_bag, split="deviance")
  fitsen<-predict(fat_tree, newdata=testing)
  alla_fel[i, j]<-mean((fitsen-testing$Bodyfat_percent)**2)
}
}

upperbound_2<-mean(alla_fel)
#The upper bound är mean av alla individuella fel. Rimligt att detta bound är ngt högre, eftersom vi kör 3fold CV?
#----
set.seed(1234567890)
bodyfat_samplad<-bodyfat[sample(1:nrow(bodyfat)),]
bodyfat_tr<-bodyfat_samplad[1:73,] #Träningsset
bodyfat_te<-bodyfat_samplad[74:110,] #testset

bf_tr_sam<-bodyfat_tr[sample(1:nrow(bodyfat_tr), replace=T), ]


folds_data<-suppressWarnings(split(bodyfat_samplad, 1:3))
fold1<-data.frame(folds_data$`1`)
fold2<-data.frame(folds_data$`2`)
fold3<-data.frame(folds_data$`3`)

set1<-rbind(fold1, fold2)
test1<-fold3

set2<-rbind(fold1, fold3)
test2<-fold2

set3<-rbind(fold2, fold3)
test3<-fold1

tree1<-tree(formula=Bodyfat_percent~., data=set1, split="deviance")
fits1<-predict(tree1, newdata=test1)
tree2<-tree(formula=Bodyfat_percent~., data=set2, split="deviance")
fits2<-predict(tree2, newdata=test2)
tree3<-tree(formula=Bodyfat_percent~., data=set3, split="deviance")
fits3<-predict(tree3, newdata=test3)


folds<-3
komber<-combn(1:folds, folds-1)
komber
komber[,1]

assign(paste("perf.a", "1", sep=""),5)

#for(i in 1:folds){
#assign(paste("set", "i", sep=""),rbind(fold[HÄR vill jag ha i komber[,1]],fold2))
#}