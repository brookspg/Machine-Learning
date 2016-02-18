kneighbors <- function(training, label, test, k) {
  #Function uses Euclidean Distance as a measure of distance
  #may add more measures later. Currently also needs better tie breaking
  
  #make label vector
  label<- as.vector(label)
  
  #initialize empty prediction vector
  predictions <- vector()
  
  for(i in 1:length(test[,1])) {
    #subtract test values from all training values
    dist <- as.matrix(sweep(training, 2, as.vector(unlist(as.list(test[i,]))), FUN = '-'))
    #Square all differences
    dist <- dist * dist
    #sum all rows
    dist <- rowSums(dist)
    #take square root of that sum
    dist <- as.vector(dist)^(0.5)
    #convert to data frame and assign id's before sorting to find labels later 
    dist <- data.frame(id = c(1:length(dist)), distance = as.vector(dist))
    #order our data by distance
    dist <- dist[order(dist$distance),]
    #find labels of neighbors
    neighbors <- label[dist$id[1:k]]
    #get frequecies of each factor
    freq <- as.data.frame(table(neighbors)/length(neighbors))
    #apply label
    predictions <- c(predictions, as.character((freq$neighbors[freq$Freq == max(freq$Freq)])[1]))
  }
  
  return(predictions)
}