

################################NEDAN ÄR RESULTATEN##################################
#Use NIRspectra.xls and Viscosity as a target variable in order to implement a linear regression model 
#in which features are first M components in ICA analysis.

library(ggplot2)
library(fastICA)

nir<-read.csv2("data/NIRspectra.csv", sep=";", header=T)

part_one<-function(data=nir, y=Viscosity, M=3){
  #data=nir
  #y="Viscosity"
  #M=3
  
  new_x<-data.frame(fastICA(data, n.comp=M)$S) #Extraherar M ICA components.
  newdata<-cbind(Viscosity=data$Viscosity, new_x)
  formula<-as.formula(paste("Viscosity~", paste(colnames(newdata)[-1], collapse="+")))
  
  des.mat<-model.matrix(formula, newdata)
  dep.var <- all.vars(formula)[1] 
  dep.var <- as.matrix(nir[dep.var])
  beta.hat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
  y.hat <- des.mat %*% beta.hat # Calculating the y-hat  , y_hat = X %*% beta_hat 
  res.err <- dep.var - y.hat  #Calculating the residuals e= y- y_hat 
  degree.free <- nrow(des.mat) - ncol(des.mat)
  res.var2 <-( t(res.err) %*% res.err ) / degree.free #Calculating the residual variance (e' %*% e) / df  
  var.hat.bhat <-diag( as.vector(res.var2) *  solve( t(des.mat) %*% des.mat )  )
  t.beta <- beta.hat / sqrt( var.hat.bhat )
  my.pvalues<- (1 - pt( abs( t.beta ) ,df = degree.free) ) * 2 
  
  l<-list(coefficients = t(beta.hat) , degree.free = degree.free, res.var2 = res.var2, var.hat.bhat = var.hat.bhat,
           t.beta = t.beta, my.pvalues = my.pvalues, formula = formula, dataset=deparse(substitute(data)),
           data=cbind(des.mat,dep.var),y.hat=y.hat,res.err=res.err)
  return(l$t.beta)
}

part_one()

#Provide a plot showing a mean square prediction error of this model (cross-validation error) versus
#number of ICA features selected. Compare with a corresponding ´plot in lab 4 and make conclusions


CV_ica <- function(data=nir, folds, M) {
  Y<-data$Viscosity
  Y<-as.matrix(Y)
  CV<-integer(0)
  resmat<-matrix(ncol=M, nrow=folds) 
  
  for (j in 1:M){ 

    X<-data.frame(fastICA(data[, 1:(ncol(data)-1)], n.comp=(j))$S) #Extraherar M ICA components.
    X<-as.matrix(X)
    
    n<-dim(X)[1]
    X<-cbind(rep(1, n), X)#Intercept-ettorna for X-matrisen
    p<-dim(X)[2]
    
    for (i in 1:folds){
      slumpat<-as.vector(unlist(suppressWarnings(split(1:n, f=1:folds)[i])))
      
      test<-X[slumpat, 1:(j+1)] #+1 eftersom jag har interceptcol
      train<-X[-slumpat, 1:(j+1)]
      Y_test<-Y[slumpat, ]
      Y_train<-Y[-slumpat, ]
      
      betahat<-solve(t(train)%*%train) %*% t(train) %*% Y_train #Skattar modellen pa traningsdatan
      yhat <- test %*% betahat  #Testar p? den nya datan, dvs skattar nya datan.
      resid_err <- Y_test - yhat #Och tar fram felen.
      
      CV[i]<-sum(resid_err**2)
    }
    resmat[,j]<-CV
  }
  resmat<-as.data.frame(resmat)
  CV_score<-apply(resmat, 2, mean)
  data<-as.data.frame(cbind(CV_score, M=1:M))
  #data<-as.data.frame(cbind(log_cv=log(CV_score), M=1:M))
  
  cv_plotten<-ggplot(data=data)+geom_point(aes(x=M, y=CV_score), size=2)+#y=log_cv
    theme_bw()+
    #ylim(c(0,.001))+
    xlab("Number of components")+
    ggtitle("CV-error versus number off ICA-components")+
    geom_vline(xintercept=data$M[data$CV_score==min(data$CV_score)], col="red")
  
  optimal_subset<-data[data$CV_score==min(data$CV_score),]
  
  plot(cv_plotten)
  return(data)
  #return(optimal_subset)
}

CV_ica(data=nir, folds=3, M=20)

#nedan kladd##################################################################3333
nir<-read.csv2("data/NIRspectra.csv", sep=";", header=T)

library(fastICA)

test<-fastICA(nir, n.comp=1)
test$X
test$K
test$W
test$A
x_dat<-data.frame(test$S)
dat<-test$S
newdata<-cbind(Visc=nir$Viscosity, x_dat)

as<-lm(formula =  Visc~, data=newdata)
summary(as)

betahat<-solve(t(dat)%*%dat) %*% t(dat) %*% nir$Viscosity
yhat<- dat %*% betahat
residualer<-nir$Viscosity-yhat

betahat<-solve(t(train)%*%train) %*% t(train) %*% Y_train #Skattar modellen p? tr?ningsdatan
yhat <- test %*% betahat  #Testar p? den nya datan, dvs skattar nya datan.
resid_err <- Y_test - yhat #Och tar fram felen.

des.mat <- model.matrix(formula , data) #Extracts the model matrix
dep.var <- all.vars(formula)[1]         #Extracts the name of the y-variable 
dep.var <- as.matrix(data[dep.var])     #Extracts the data of the y-variable 
# and overwrites it with the data-colum 

lm(nir$Viscosity~nir$X1000+nir$X998+nir$X996)

des.mat<-model.matrix(Viscosity~X1000+X998+X996, nir)
des.mat
dep.var <- all.vars(Viscosity~X1000+X998+X996)[1] 
dep.var <- as.matrix(nir[dep.var])
beta.hat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
y.hat <- des.mat %*% beta.hat # Calculating the y-hat  , y_hat = X %*% beta_hat 
res.err <- dep.var - y.hat  #Calculating the residuals e= y- y_hat 

