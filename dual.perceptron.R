dual_perceptron <- function(data, labels, maxiter=500, Kernal = 'Linear') {
  #Make sure data and labels are proper format
  data <- as.data.frame(cbind(rep(1, nrow(data)), data))
  labels <- as.numeric(as.character(labels))
  
  #initialize coefficients and bias
  m <- rep(0, nrow(data))

  #initialize iterations and complete flag
  iterations <- 0
  complete <- F

  #pretty self explanatory if statement
  if(Kernal == 'Linear') {
    #just so you know the right process started (capitalization matters with Gaussian)
    print('Linear Begun')
    
    while (complete == F) {  
      #tracks the number of changes in this iteration over data
      num_changes <- 0
      for (i in 1:nrow(data)) {
        #initialize test value
        test <- 0
        #sum up the inner products of all vectors 
        for (j in 1:nrow(data)) {
          test <- test + m[j] * crossprod(unlist(data[i, ]), unlist(data[j, ]))
        }
        #test if our prediction worked
        if (test * labels[i] <= 0) {
          #fix if it failed and update the num_changes to say we did something
          m[i] <- m[i] + labels[i]
          num_changes <- num_changes + 1
        }
      }
      #if we make it over the dataset without a change we are done 
      if (num_changes == 0) {
        complete <- T
      }
    }
    
    #convert the m vector into a w vector of coefficients to be returned
    w <- 0
    for(i in 1:nrow(data)) {
      w<- w + m[i] * c(1, unlist(data[i,]))
    }
    
    return(w)
    
  } else if (Kernal == 'Gaussian') {
    #again, make sure we got the proccess right
    print("Gaussian Begun")
    while (complete == F) {  
      num_changes <- 0
      for (i in 1:nrow(data)) {
        test <- 0
        for (j in 1:nrow(data)) {
          #the only change between this and above, the inner product was replaced with a 
          #Gaussian kernal as shown in class with lambda = 1
          test <- test + m[j] * exp(-1 * sum((unlist(data[i, ]) - unlist(data[j, ]))**2))
        }
        if (test * labels[i] <= 0) {
          m[i] <- m[i] + labels[i]
          num_changes <- num_changes + 1
        }
      }
      if (num_changes == 0) {
        complete <- T
      }
      #we don't convert into a coefficient vector with the Gaussian 
    }
    return(m)
  }
  
}

predict_dual <- function(new_data, m, Kernal = 'Linear', prior_data = NULL) {
  if(Kernal == 'Linear') {
    #m here should just be the w produced by the dual perceptron function above\
    #prior data should be left as NULL
    preds <- c()
    new_data <- cbind(rep(1, nrow(new_data)), new_data)
    
    for (i in 1:nrow(new_data)) {
      pred <- crossprod(unlist(new_data[i, ]), w)
      preds <- c(preds, pred)
    }
    
    return(preds)
  } else if(Kernal == "Gaussian") {
    #m here will be a vector of the length of the originial dataset
    #prior must be the original dataset
    preds <- c()
    new_data <- cbind(rep(1, nrow(new_data)), new_data)
    
    for(i in 1:nrow(new_data)) {
      pred<- 0
      for(j in 1:nrow(prior_data)) {
        pred <- pred + m[j] * exp(-1 * sum((unlist(new_data[i, ]) - c(1, unlist(prior_data[j, ])))**2))
      }
      preds <- c(preds, pred)
    }
     #coerce values to -1 or 1
    preds <- ifelse(preds < 0, -1, 1)
    
    return(preds)
  }
}