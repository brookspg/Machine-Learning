perceptron <- function(data, labels) {
  #only works on linearly seperable data, doesn't have a stopping mechanism
  
  #Make sure data and labels are proper format
  data <- as.data.frame(data)
  labels <- as.numeric(as.character(labels))
  
  #Initialize W and B to number of features and 0 respectively
  w <- rep(0, ncol(data))
  b <- 0
  
  #initialize complete flag and iteration counter
  iterations = 1
  complete = F
  corrections = 0
  
  #while loop iterating over data
  while(complete == F) {
    #each iteration starts from the beginning of the data
    for(i in 1:nrow(data)) {
      #generate testing prediction
      test <- crossprod(w, as.vector(unlist(data[i,]))) + b
      #check prediction matches
      if (labels[i]*test <= 0) {
        #if not correct our coefficients and bias
        w = w + as.vector(unlist(data[i,])) * labels[i]
        b = b + labels[i]
        #up the iteration count and start form the top of the data again
        iterations = iterations + 1
        break
      }
      #if we reach the bottom and haven't had issues we're done 
      else if (i == nrow(data)){
        complete = T
      }
    }
  }
  
  #print iteration count
  print(paste('iterations: ', as.character(iterations), sep = ''))
  
  #prints the regularized coefficients
  print(c(b, w)/b)
  
  #return vector of coefficients and bias
  return(c(b, w))
}

predict_perceptron <- function(new_data, w) {
  #intakes the new data that must be the same format and order as the original
  #but cannot have a labels column
  
  #format and initialize prediction vector, add bias column to data
  preds <- c()
  new_data <- cbind(rep(1, nrow(new_data)))
  
  #iterate over making a prediction for each value in the new dataset 
  for (i in 1:nrow(new_data)) {
    pred <- crossprod(unlist(new_data[i, ]), w)
    preds <- c(preds, pred)
  }
  
  #Coerce all values to -1 or 1
  preds <- ifelse(preds < 0, -1, 1)
  
  return(preds)
}