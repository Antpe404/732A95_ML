library(neuralnet)
set.seed(1234567890)
Var <- runif(n=50, min=0, max=10)
trva <- data.frame(Var, Sin=sin(Var))

tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initializaiton of the weights in the interval [-1, 1]
winit <-  # Your code here
  for(i in 1:10) {
    nn <- neuralnet()# Your code here(inside parenthesis)
      # Your code here
  }
plot(nn <- neuralnet(# Your code here))
  # Plot of the predictions (black dots) and the data (red dots)
  plot(prediction(nn)$rep1)
  points(trva, col = "red")