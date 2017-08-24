
highest_variance_ordering <- function(no, donnees, x) {
  mean_of_x=mean(donnees$x)
  donnees$sq_dist_from_mean <- (donnees$x - mean_of_x)^2
  donnees <- donnees[order(donnees$sq_dist_from_mean),]
  donnees$group_allocation <- rep(c(1:20),each=20)
  
  donnees <<- donnees 
}