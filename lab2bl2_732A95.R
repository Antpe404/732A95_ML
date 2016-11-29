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

#--------------------------------------------------------------------------
fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat, split="deviance")
mean(residuals(fat_tree)**2)


set.seed(1234567890)
saf<-sample(1:nrow(bodyfat_tr), replace=T)
bodyfat_tr<-bodyfat_tr[saf,]
fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_tr, split="deviance")
fitsen<-predict(fat_tree, newdata=bodyfat_te)
fitsen-bodyfat_te$Bodyfat_percent
sum(fitsen-bodyfat_te$Bodyfat_percent)
#----------------------------------------------------------------------
felen<-integer(0)
set.seed(1234567890)
for ( i in 1:100){
  saf<-sample(1:nrow(bodyfat_tr), replace=T)
  bodyfat_bag<-bodyfat_tr[saf,]
  fat_tree<-tree(formula=Bodyfat_percent~., data=bodyfat_bag, split="deviance")
  fitsen<-predict(fat_tree, newdata=bodyfat_te)
  felen[i]<-sum(fitsen-bodyfat_te$Bodyfat_percent)
}
mean((felen/length(felen))**2)

mean((felen/100)**2)


