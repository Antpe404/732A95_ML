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
cv_regtree<-cv.tree(regtree)

plot(cv_regtree)
#Verkar som att tree leaves minimerar va?

tree_3leaves<-prune.tree(regtree, best=3)

plot(tree_3leaves)
text(tree_3leaves, pretty=0)

#Inte säker på vad han vill att jag ska plotta här

plot(state$MET, state$EX) #Originaldata
hist(predict(tree_3leaves)) #Histo predicted
plot(state$MET,predict(tree_3leaves)) #scatter predricted
#the qouality of the fit obviously doesn't look super, but at least I can se the pattern with high
#values to the left, lower in the moddle and medium to the right.

hist(residuals(tree_3leaves)) #Hist resid
#They dont look like NF. Have a few really big positive residuals, but most are negative.
#If I compare the scatterplots above, i can see where they are coming from. The one at met ~22, and
#two obs at Met ~50-52 are at about EX0350, but gets classified at ~260. Obvisouly not great.

#Assignment 2
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
plot(W_prim[,1], main="Traceplot ICA1, it's eigenvector", ylab="value")
grid()
plot(W_prim[,2], main="Traceplot ICA2, it's eigenvector", ylab="value")
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
#---------------------


validationplot(pcr_model,val.type="MSEP", xaxt="n") #xaxt tar bort x-axeln
axis(1, at = seq(0, 120, by = 10), las=1)
#abline(h=min()) Detta lyckas jag ej få fram.
#Inte säker på vad detta är, men ngnstans mellan 20/40?
#28 om man är exakt, kan man se om man tittar i MSEP(pcr_model)
plot(MSEP(pcr_model))


#-------------------------------------
DDZ<-scale(y) 
DDZall<-scale(x)#standardiserar x, dvs alla variabler.

DDcov<-cov(DDZ)#kovarianserna av de standardiserade pendelvariablerna
sum(diag(DDcov))
Egen<-eigen(DDcov)$values
EV<-eigen(DDcov)$vector[,1]#Tar ut f?rsta kol ur matrisen med egenvektorer.

