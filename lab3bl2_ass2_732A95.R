spam<-read.csv2("data/spambase_lab3bl2.csv", sep=";", header=T)
spam$Spam[spam$Spam==0]<--1 #Convertar för att underlätta SVM:s.

set.seed(1234567890)
ind <- sample(1:nrow(spam))
spam <- spam[ind,c(1:48,58)] #Shufflar och ta bort 
h <- 1
H<-1
b<-0
#beta <- c(0, -.05)
beta<-(-.05)
beta<-0
#beta2<--.05
# M <- c(500, 20)
M<-20
#M2<-20
N <- 500 # number of training points

gaussian_k <- function(x, h) { # Gaussian kernel
  ans<-exp(-(x**2)/(h**2))
  return(ans)
}

step8_func<-function(sv){
  #last_part<-vector(length=length(sv))
  res<-vector(length=length(sv))
  index<-1
  for(m in sv){
    #m<-234
    rad_m<-spam[m, -49]
    t_m<-spam[m, 49]
    dis<-as.matrix(dist(rbind(rad_m,rad_m))) #sista delen av step 8
    last_part<-t_m*gaussian_k(x=dis[-1,1], h=H) #sista delen av step8, med dis mellan samma vector
    #Nedan ist för step4[m]
    x_i<-spam[i, -49] #step3
    t_i<-spam[i, 49] #step3
    x_m<-spam[m, -49] #x_m
    t_m<-spam[m, 49]
    distance<-as.matrix(dist(rbind(x_i, x_m), method="euclidean"))
    yxm<-sum(t_m*gaussian_k(x=distance[-1,1], h=H)+b)
    #
    #yxm<-step4[m]#Motsvarar y(x_m) in step8, calculated in last loop
    res[index]<-t_m*(yxm-last_part)
    index<-index+1
  }
  #return(res)
  return(which.max(res)) #tar ut den minst viktiga, i.e. max.
}


SVM<-function(sv=c(1), H=1, b=0, N=500, M, beta){
  errors <- 1
  errorrate <- vector(length = N)
  errorrate[1] <- 1
  step4<-vector(length=N-1)
  
  for(i in 2:N) {
    #sv<-1:2
    #i<-1
    x_i<-spam[i, -49] #step3
    t_i<-spam[i, 49] #step3
    x_m<-spam[sv, -49] #x_m
    t_m<-spam[sv, 49]
    distance<-as.matrix(dist(rbind(x_i, x_m), method="euclidean"))
    step4[i-1]<-sum(t_m*gaussian_k(x=distance[-1,1], h=H)+b)
    
    yx_i<-sum(t_m*gaussian_k(x=distance[-1,1], h=H)+b) #step4
    if (t_i*yx_i<=beta){ #step5
      sv<- c(sv, i) #step6
      errors<-errors+1
      #ignore step7
      if (length(sv)>M){ #step 8
        sv <- sv[-step8_func(sv=sv)]
      }
    }
    errorrate[i] <- errors / i
  }
  plot(errorrate, main=paste("Errorate over iterations, M=", M, ", beta=", beta,"."))
}

SVM(M=500, beta=0)
SVM(M=500, beta=-.05)
SVM(M=20, beta=0)
SVM(M=20, beta=-0.05)
#m_rad<-spam[sv, -49] #x_m
#m_response<-spam[sv, 49] #t_m
#x_m<-spam[1, -49] #x_m
#t_m<-spam[1, 49] #t_m

#x_m<-spam[sv, -49] #x_m
#t_m<-spam[sv, 49] #t_m

errors <- 1
errorrate <- vector(length = N)
errorrate[1] <- 1

sv <- c(1)
step4<-vector(length=N-1)

for(i in 2:N) {
  #sv<-1:2
  #i<-1
  x_i<-spam[i, -49] #step3
  t_i<-spam[i, 49] #step3
  x_m<-spam[sv, -49] #x_m
  t_m<-spam[sv, 49]
  distance<-as.matrix(dist(rbind(x_i, x_m), method="euclidean"))
  step4[i-1]<-sum(t_m*gaussian_k(x=distance[-1,1], h=h)+b)
  
  yx_i<-sum(t_m*gaussian_k(x=distance[-1,1], h=H)+b) #step4
  if (t_i*yx_i<=beta){ #step5
    sv<- c(sv, i) #step6
    errors<-errors+1
    #ignore step7
    if (length(sv)>M){ #step 8
      sv <- sv[-step8_func(sv=sv)]
    }
  }
  
  errorrate[i] <- errors / i
}

plot(errorrate, main=paste("Errorrate over iterations, M=", M, ", beta=", beta,"."))
