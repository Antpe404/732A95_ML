#Lab 4 ML 732A95
#Assignment 1
#1.1
#setwd("C:/Users/Anton/Documents/732A95 Machine learning/data")
state<-read.csv2("data/State.csv", sep=";", header=T)

state<-state[order(state$MET, decreasing=T), ] #Reordrar efter MET
plot(state$MET, state$EX, xlab="MET", ylab="EX")
#What model can be appropriate?
#-Appropriate for what? But prolly like a spline maybe? Maybe linear piecewise with one knot.

#1.2
library(tree)

regtree<-tree(formula=EX~MET, data=state, minsize=8, split="deviance")

plot(regtree)
set.seed(12345)
cv_regtree<-cv.tree(regtree)

plot(cv_regtree)
#Verkar som att tree leaves minimerar va?

tree_3leaves<-prune.tree(regtree, best=3)

plot(tree_3leaves)
text(tree_3leaves, pretty=0)

#Inte säker på vad han vill att jag ska plotta här

plot(state$MET, state$EX, main="Original data") #Originaldata
#hist(predict(tree_3leaves)) #Histo predicted
plot(state$MET,predict(tree_3leaves), main="The predicted values") #scatter predricted
#the quality of the fit obviously doesn't look super, but at least I can se the pattern with high
#values to the left, lower in the moddle and medium to the right. It's a underfit model however, its' 
#confident bands wouldn't look good.

library(ggplot2)
ggplot(data=data.frame(resid=residuals(tree_3leaves)))+geom_histogram(aes(x=resid), bins=10)
hist(residuals(tree_3leaves)) #Hist resid
#They dont really look like NF, but only 48 obs. Have a few really big positive residuals, but most are negative.
#If I compare the scatterplots above, i can see where they are coming from. The one at met ~22, and
#two obs at Met ~50-52 are at about EX0350, but gets classified at ~260. Obvisouly not great.

#1.3
library(boot)
regtree<-tree(formula=EX~MET, data=state, minsize=8, split="deviance")

#boot(data=state, statistic=state, sim="ordinary", R=1000)

#Olegs modifierade

#Ickeprunat------------------------------------------------------------------
#func_tree<-function(data, ind){
#  data1<-data[ind,]# extract bootstrap sample
#  regtrees<-tree(formula=EX~MET, data=data1, minsize=8, split="deviance") #fit tree model
#  #predict values for all Area values from the original data
#  priceP<-predict(regtrees,newdata=state)
#  return(priceP)
#}
#res=boot(data2, f, R=1000) #make bootstrap
#set.seed(12345)
#bootstrap_res<-boot(data=state, statistic=func_tree, sim="ordinary", R=1000)
#bootstrap_res

#Prunat till 3 löv
#func_tree<-function(data, ind){
#  data1<-data[ind,]# extract bootstrap sample
#  regtrees<-tree(formula=EX~MET, data=data1, minsize=8, split="deviance") #fit tree model
# 
#  trees_3<-prune.tree(regtrees, best=3) #prunes tree tol 3 leaves
#  
#  priceP<-predict(trees_3,newdata=state) #predict values from the original data
#  return(priceP)
#}
#set.seed(12345)
#bootstrap_res<-boot(data=state, statistic=func_tree, sim="ordinary", R=1000)
#bootstrap_res

#CI<-envelope(bootstrap_res, level=.95)

#plot(state$MET,state$EX) #originaldata
#points(state$MET,predict(tree_3leaves), col="red", type="l") #Trädet
#CI$point[1,] Detta är upper och CI$point[2,] lower
#points(state$MET, CI$point[1,], col="green", type="l")
#points(state$MET, CI$point[2,], col="green", type="l")

#Prunat till 3 löv---------------------------------
func_CI_tree<-function(data, ind){
  data1<-data[ind,]# extract bootstrap sample
  trees_3<-prune.tree(tree(formula=EX~MET, data=data1, minsize=8, split="deviance"), best=3) 
  #fit pruned tree model. I make the tree and prune it in the same code line.
  priceP<-predict(trees_3,newdata=state) #predict values from the original data
  return(priceP)
}
set.seed(12345)
bootstrap_CI<-boot(data=state, statistic=func_CI_tree, sim="ordinary", R=1000)
bootstrap_CI

CI<-envelope(bootstrap_CI, level=.95)

plot(state$MET,state$EX, xlab="MET", ylab="EX",
     main="Confidence band for regression tree \n by non parametric bootstrapping") #originaldata
points(state$MET,predict(tree_3leaves), col="red", type="l") #Trädet
#CI$point[1,] Detta är upper och CI$point[2,] lower
points(state$MET, CI$point[1,], col="green", type="l")
points(state$MET, CI$point[2,], col="green", type="l")

#The band is quite bumpy. That is probably beacuse some big outliers doesn't appear in all models
#created with bootstrap. For example, most of the trees seem to fit observations with MET [30,50]
# in the same way, that's why it has a more narrow band there, while it's much broader in the 
#beginning of the series.

#The result from assignment 1.2, which is the red line in th eplot above, doesn't seem very reliable.
#The confidant band is broad, and the confident band is supposed to show where the true value of EX 
#appeards according to MET. It's not supposed to cover 95 % of the observations.

###############################1.4---------------------------------------------------


##################Det ovan är enligt slide 32, men fattar inte riktigt vad rng är där.
#Provar nedanför enligt slide 29 ist.
#Update: Tror nu att vi ska använda först enligt s29 för confident band, sen ny run enligt s 32 för
#pred band

mle<-tree_3leaves

#Börjar med min random generator
random_gen<-function(data, mle){
  data1<-data.frame(EX=state$EX, MET=state$MET)
  n=nrow(data1)
  data1$EX<-rnorm(n=n, mean=predict(tree_3leaves, newdata=data1), sd=sd(residuals(tree_3leaves)))
  return(data1)
}

#Men motsvarande f1 enligt slide 29
funk_CI_tree<-function(state){
  regtrees<-tree(formula=EX~MET, data=state, minsize=8, split="deviance") #fit tree model
  trees_3<-prune.tree(regtrees, best=3)
  preds<-predict(trees_3, newdata=state)
  return(preds)
}

set.seed(12345)
bootstrap_CI<-boot(data=state, statistic=funk_CI_tree,
                   sim="parametric", R=1000, mle=tree_3leaves, ran.gen = random_gen) 

interval_CI<-envelope(bootstrap_CI, level=.95)

plot(state$MET,state$EX) #originaldata
points(state$MET,predict(tree_3leaves), col="red", type="l") #Trädet
#CI$point[1,] Detta är upper och CI$point[2,] lower
#points(state$MET, interval$overall[1,], col="green", type="l") #Pred upper
#points(state$MET, interval$overall[2,], col="green", type="l") #Pred lower
points(state$MET, interval_CI$point[1,], col="green", type="l") #CI upper
points(state$MET, interval_CI$point[2,], col="green", type="l") #CI lower

#Fortsätter nu med att göra PRED INTERVAL enligt slide 32.

funk_PI_tree<-function(state){
  regtrees<-tree(formula=EX~MET, data=state, minsize=8, split="deviance") #fit tree model
  
  trees_3<-prune.tree(regtrees, best=3) #prunes tree tol 3 leaves
  preds<-predict(trees_3,newdata=state) 
  
  n<-nrow(state)
  generated_predictions<-rnorm(n=n,mean=preds, sd=sd(residuals(trees_3))) #slumpar nf med mean av värdena
  #och samma varians som residualerna har. Alltså genererar slumpmässiga observation utifrån mina
  #predictioner.
  return(generated_predictions)
}
set.seed(12345)
bootstrap_PI<-boot(data=state, statistic=funk_PI_tree, sim="parametric", R=1000, mle=tree_3leaves,
                ran.gen=random_gen) 

interval_PI<-envelope(bootstrap_PI, level=.95)

plot(state$MET,state$EX, ylim=c(min(interval_PI$point[2,]), 450)) #originaldata
points(state$MET,predict(tree_3leaves), col="red", type="l") #Trädet, alltså fitten
points(state$MET, interval_CI$point[1,], col="green", type="l") #CI upper
points(state$MET, interval_CI$point[2,], col="green", type="l") #CI lower
points(state$MET, interval_PI$point[1,], col="blue", type="l") #PI upper
points(state$MET, interval_PI$point[2,], col="blue", type="l") #PI lower


#Assignment 2-----------------------------------------------------------
nir<-read.csv2("data/NIRspectra.csv", sep=";", header=T)
PC<-prcomp(nir[, 1:126], scale=T)
lambda<-(PC$sdev^2)
sprintf("%2.4f",lambda/sum(lambda)*100)



#############Ass 2.1, egen PCA
nir2<-as.data.frame(scale(nir[, 1:126])) #Tar bort y-variablen här
Y<-as.data.frame(scale(nir[, 127])) #Lägger Y här.
covar<-cov(nir2)#Cov av X-variablerna
eigen_stuff<-eigen(covar)
eigenvalues<-(eigen_stuff$values)
eigen_vectors<-eigen_stuff$vectors #första kol innebär förta egenvektorn
PC_proportion<-eigenvalues/sum(eigenvalues) #Detta är hur mkt de olika PC förklarar.
sprintf("%2.4f", PC_proportion) #Här skriver jag ut dem. Första förklarar 95.38 %.

i<-1
PCAs<-integer(0)
while(sum(PCAs)<.99){
  PCAs[i]<-PC_proportion[i]
  i<-i+1
}
number_of_components<-length(PCAs)

#PLOTTEN
barplot(PC_proportion[1:10], ylim=c(0,1), ylab="Share of the variance explained", 
 xlab="First 10 PC components", las=1)
grid()
title("Variation explained by the first 10 components")

#plot(eigen_vectors[,1], eigen_vectors[,2])

#Z = X U slide 14
X<-as.matrix(nir2)
U<-eigen_vectors[,1:number_of_components]
Z<-as.data.frame(X %*% U)
colnames(Z)<-c("PC1", "PC2")

#Nedan är scoresen för PC1.
plot(x=Z$PC1, y=Z$PC2, xlab=colnames(Z)[1], ylab=colnames(Z)[2],  main="The scores for PC1 and PC2, PCA" )
#Yes, it seems to a few unusual fuels, escpecially on the PC1.

#Note: My PC1 seems to have -1* deras (KS/rasmus) value, can be seen here and 2.2 below.
##############2.2
plot(U[,1], main="Traceplot PC1, it's eigenvector", ylab="value")
grid()
plot(U[,2], main="Traceplot PC2, it's eigenvector", ylab="value")
grid()
#############2.3
#install.packages("fastICA")
library(fastICA)
fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
        method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) 
set.seed(12345)
ica_model<-fastICA(nir2, n.comp=number_of_components,alg.typ="parallel",
                   fun="logcosh", alpha=1, row.norm=F,verbose=T )

#2.3a
ica_model$K
ica_model$W

W_prim<-ica_model$K%*%ica_model$W #126x2/2x2->126x2
W_prim

#Plotta w_prim
plot(W_prim[,1], main="Traceplot ICA1", ylab="value")
grid()
plot(W_prim[,2], main="Traceplot ICA2", ylab="value")
grid()

par(mfrow=c(2,2))
plot(U[,1], main="Traceplot PC1, it's eigenvector", ylab="value")
grid()
plot(U[,2], main="Traceplot PC2, it's eigenvector", ylab="value")
grid()
plot(W_prim[,1], main="Traceplot ICA1, it's eigenvector", ylab="value")
grid()
plot(W_prim[,2], main="Traceplot ICA2, it's eigenvector", ylab="value")
grid()
#They look very much the same.
#I think that W_prim is the eigenvectors of the ICA-model, corresponding to the eigenvectors
#in the U matrix in PCA.

#2.3b
Z_ica<-as.data.frame(X %*% W_prim) #395x126 126x2
colnames(Z_ica)<-c("IC1", "IC2")

#Nedan är scoresen för ICA
par(mfrow=c(1,1))
plot(x=Z_ica$IC1, y=Z_ica$IC2, xlab=colnames(Z_ica)[1], ylab=colnames(Z_ica)[2], main="The scores for the two latent features, ICA")
#Looks the same but mirrored in my case bc of the negative PC1 in my PCA.

##################2.4 PCR
#install.packages("pls")
library(pls)

#Detta nedan kan vara helt fel.
#pcr_data<-cbind(Y, Z)
#my_pcr<-lm(V1~PC1+PC2, data=pcr_data)
#summary(my_pcr)
#my_pcr$coefficients
#my_pcr$fitted.values

#Detta är PCR enligt slides.
set.seed(12345)
#pcr_model2<-pcr(Viscosity ~ ., data=nir, scale=TRUE )
pcr_model<-pcr(Viscosity ~ ., data=nir, scale=TRUE, validation="CV")
pcr_model
MSEP(pcr_model)#Detta är vad validationplotten plottar

validationplot(pcr_model,val.type="MSEP", xaxt="n") #xaxt tar bort x-axeln
axis(1, at = seq(0, 120, by = 10), las=1)
abline(v=28, lty=3) 
#Detta lyckas jag ej få fram på ett logiskt sätt, men kan se i MSEP(pcr_model) att 28 components är lägst.
#Inte säker på vad detta är, men ngnstans mellan 20/40?
#28 om man är exakt, kan man se om man tittar i MSEP(pcr_model)
plot(MSEP(pcr_model))

min(as.numeric(unlist((MSEP(pcr_model)))[1:254]))


#-------------------------------------skit nedan, först fr annat försök att lösa 2.4-------------------
#----------------------
#Försök att ta fram värdena i MSEP(pcr_model). Går dåligt. Listor.
#Vill ha fram min.
CV_adjCV<-MSEP(pcr_model)
min(CV_adjCV)
CV_adjCV[1:length(CV_adjCV)]
min(CV_adjCV[1:length(CV_adjCV)])
CV_adjCV<-unlist(CV_adjCV)
min(CV_adjCV)
CV_adjCV<-as.vector(CV_adjCV)
CV_adjCV
unlist(MSEP(pcr_model))
as.matrix(MSEP(pcr_model))
min(MSEP(pcr_model))

summary(pcr_model)
summary(pcr_model)
summary(pcr_model)[1]
summary(pcr_model)[2]


pcr_model$terms
pcr_model$Yloadings
pcr_model$coefficients
pcr_model$loadings
pcr_model$scores

#Detta är att ta fram dem själv
egen_cv<-crossval(pcr_model)
egen_cv
summary(egen_cv)


DDZ<-scale(y) 
DDZall<-scale(x)#standardiserar x, dvs alla variabler.

DDcov<-cov(DDZ)#kovarianserna av de standardiserade pendelvariablerna
sum(diag(DDcov))
Egen<-eigen(DDcov)$values
EV<-eigen(DDcov)$vector[,1]#Tar ut f?rsta kol ur matrisen med egenvektorer.

