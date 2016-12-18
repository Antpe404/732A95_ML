library(neuralnet)
#set.seed(1234567890)
points<-50
units<-10
set.seed(1234567890)
Var <- runif(n=points, min=0, max=10)
trva <- data.frame(Var, Sin=sin(Var))

tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
felen<-vector(, length=10)

# Random initializaiton of the weights in the interval [-1, 1]
#set.seed(1234567890)
winit <- runif(n = 3*units+1 , min=-1, max=1) #10 för input, 10 hidden, 10 output, 1 för bias.
#set.seed(1234567890)

 for(i in 1:10) {
    nn <- neuralnet(formula = Sin~Var, data=tr, threshold = i/1000, hidden=10, startweights = winit )# Your code here(inside parenthesis)
      # Your code here
    preds<-compute(nn,va$Var) #va$Var är allt utom target.
    felen[i]<-(sum((preds$net.result-va$Sin)**2))/nrow(va)   
    
    if (i > 1 && felen [i] > felen[i-1]){
      cat("No more progress, iteration stops")
      break
    
    }
 }

  
  #Report the chosen value for the threshold
  thresholds<-seq(1/1000, 1/100, 0.001)
  chosen_value<-thresholds[length(felen[felen>0])-1]
  chosen_value
  
  #Will now report the final NN learnt
  final_nn <- neuralnet(formula = Sin~Var, data=trva, threshold = chosen_value, hidden=10, startweights = winit )
  
  plot(final_nn)
  
  dev.print(pdf, 'nn_plot2.pdf')
  
  plot(prediction(final_nn)$rep1, pch=18)
  points(trva, col="red", pch=1)
  legend('bottomleft',legend=c('NN','Observed'),pch=c(18,1),col=c('black','red'))
  title("Predicted versus observed")
  
  

  