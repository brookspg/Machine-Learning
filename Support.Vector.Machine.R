svm <- function(data, labels, C, tol, maxiter) {
  #Initialize Alpha's, b, and iteration count
  alpha <- rep(0, nrow(data))
  b <- 0
  iterations <- 0
  
  while (iterations < maxiter) {
    #count of alphas we've changed on this iteration
    num_changed_alphas <- 0
    for (i in 1:nrow(data)) {
      #begin by generating Ei for this iteration
      E_matrix <- t(matrix(unlist(data[i,]), nrow = ncol(data), ncol = nrow(data)))
      E_matrix <- E_matrix * as.matrix(data)
      E_matrix <- rowSums(E_matrix)
      E_matrix <- E_matrix * alpha * labels
      E_matrix <- sum(E_matrix)
      E <- E_matrix + b - labels[i]
      if (((labels[i] * E) < (-1 * tol) & (alpha[i] < C)) | (labels[i] * E > tol & alpha[i] > 0)) {
        j <- sample(1:nrow(data), 1)
        while (j == i) {
          j <- sample(1:nrow(data), 1)
        }
        Ej_matrix <- t(matrix(unlist(data[j,]), nrow = ncol(data), ncol = nrow(data)))
        Ej_matrix <- Ej_matrix * as.matrix(data)
        Ej_matrix <- rowSums(Ej_matrix)
        Ej_matrix <- Ej_matrix * alpha * labels
        Ej_matrix <- sum(Ej_matrix)
        Ej <- Ej_matrix + b - labels[j]
        alphai_old <- alpha[i]
        alphaj_old <- alpha[j]
        if (labels[i] == labels[j]) {
          L <- max(0, alpha[i] + alpha[j] - C)
          H <- min(C, alpha[i] + alpha[j])
        } else{
          H <- min(C, C + alpha[j] - alpha[i])
          L <- max(0, alpha[j] - alpha[i])
        }
        if (H == L) {
          print('broke at h=l')
          break
        }
        nu <- 2*crossprod(unlist(data[i,]), unlist(data[j,])) - crossprod(unlist(data[i,]), unlist(data[i,])) - crossprod(unlist(data[j,]), unlist(data[j,])) 
        if (nu >= 0) {
          print('broke at nu')
          break
        }
        alpha[j] <- alpha[j] - (labels[j] * (E - Ej))/nu
        if (alpha[j] > H) {
          alpha[j] <- H
        } else if(alpha[j] < L) {
          alpha[j] <- L
        } else {
          alpha[j] <- alpha[j]
        }
        if (abs(alpha[j] - alphaj_old) < .0001) {
          print('broke at abs')
          break
        }
        alpha[i] <- alpha[i] + labels[i] * labels[j] * (alphaj_old - alpha[j])
        b1 <- b - E - labels[i]*(alpha[i] - alphai_old) * crossprod(unlist(data[i,]), unlist(data[i,])) - labels[j]*(alpha[j] - alphaj_old) * crossprod(unlist(data[j,]), unlist(data[i,]))
        b2 <- b - Ej - labels[j]*(alpha[j] - alphaj_old) * crossprod(unlist(data[j,]), unlist(data[j,])) - labels[i]*(alpha[i] - alphai_old) * crossprod(unlist(data[j,]), unlist(data[i,]))
        if ((alpha[i] > 0) & (alpha[i] < C)) {
          b <- b1
          print('reached b1')
        } else if ((alpha[j] > 0) & (alpha[j] < C)) {
          b <- b2
          print('reached b2')
        } else {
          b <- (b1 + b2)/2
          print('reached b')
        }
        print('made it')
        num_changed_alphas <- num_changed_alphas + 1
      }
    }
    if (num_changed_alphas == 0) {
      iterations <- iterations + 1
      print(iterations)
    } else{
      iterations <- 0
    }
  }
  
  final_data <- as.data.frame(cbind(labels, data, alpha))
  final_data <- final_data[final_data[,ncol(final_data)] > 0,]
  final <- list(b, final_data)
  
  return(final)
}

svm_predict<- function(test, svm) {
  predictions <- c()
  
  b <- as.numeric(svm[1])
  vectors <- as.data.frame(svm[2])[, 1:(ncol(as.data.frame(svm[2])) - 1)]
  alphas <- as.numeric(unlist(as.data.frame(svm[2])[, ncol(as.data.frame(svm[2]))]))
  
  for (i in 1:nrow(test)) {
    pred_matrix <- t(matrix(unlist(test[i,]), nrow = ncol(vectors)-1, ncol = nrow(vectors)))
    pred_matrix <- pred_matrix * as.matrix(vectors[,2:ncol(vectors)])
    pred_matrix <- rowSums(pred_matrix)
    pred_matrix <- pred_matrix * alphas * vectors[,1]
    pred_matrix <- sum(pred_matrix)
    pred <- pred_matrix + b
    if(pred >= 0) {
      predictions <- c(predictions, 1)
    }
    else{
      predictions <- c(predictions, -1)
    }
  }
  
  return(predictions)
}