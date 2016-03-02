sigmoid <- function(x) {return(1/(1+exp(-x)))}

mlp <- function(data, labels, nodes, nu = 0.2, maxepochs = 500) {
  w <- matrix(rnorm(1, sd = 0.02), nrow = ncol(data)+1, ncol = nodes) 
  A <- rnorm(nodes, sd= 0.02)
  epochs <- 0
  
  while (epochs < maxepochs) {
    for (i in 1:nrow(data)) {
      xi <- c(1, unlist(data[i,]))
      hidden_output<- t(w) %*% xi
      total_output <- as.numeric(t(hidden_output) %*% A)
      delta <- sigmoid(total_output) * (1 - sigmoid(total_output)) * (labels[i] - sigmoid(total_output))
      for (j in 1:nodes) {
        deltaj <- sigmoid(hidden_output[j]) * (1 - sigmoid(hidden_output[j])) * (A[j]) * delta
        w[,j] <- w[,j] + (nu * deltaj * xi)
      }
    }
    epochs <- epochs + 1
  }
  
  return(list(A, w))
}

predict_mlp <- function(test, w) {
  preds <- c()
  
  A <- as.vector(unlist(w[1]))
  w <- as.data.frame(w[2])
  
  for (i in 1:nrow(test)) {
    xi <- c(1, unlist(test[i,]))
    hidden_output<- t(w) %*% xi
    total_output <- as.numeric(t(hidden_output) %*% A)
    preds <- c(preds, total_output)
  }
  
  return(preds)
}