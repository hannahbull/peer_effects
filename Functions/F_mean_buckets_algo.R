mean_buckets_algo <- function(no, donnees, x) {
 # setwd("/Users/u4673164/Downloads/")
  
  if (no>1) { 
    repeats_vector <- sample(rep(floor(400/no),no)+c(rep(1,400 %% no),rep(0,no-400 %% no)))
    #sum(repeats_vector)
    donnees <- donnees[order(donnees$x),]
    
    halves_vector <- c()
    for (i in 1:no) {
      temp_vector <- rep(i,repeats_vector[i])
      halves_vector <- c(halves_vector, temp_vector)
    }
    
    donnees$halves <- halves_vector
    donnees$random_ordering <-  rnorm(400,0,1)
    
    for (i in 1:no) {
     
      donnees_temp <- donnees[donnees$halves==i,]
      
      donnees_temp <- donnees_temp[order(donnees_temp$random_ordering),]
      donnees_temp$first20 <- c(rep(1,(floor(nrow(donnees_temp)/20)*20)), rep(0,(nrow(donnees_temp) %% 20 )))
      
      donnees_temp$group_allocation[donnees_temp$first20==1] <- rep(c((1+floor(((400/no)/20))*(i-1)):(i*floor((400/no)/20))),each=20)
      
      if (i==1) {
        donnees_new <- donnees_temp
      }
      
      if (i!=1) {
        donnees_new <- rbind(donnees_new, donnees_temp)
      }
    }
    
    donnees <- donnees_new
    number_groups_left=nrow(donnees[donnees$first20==0,])
    
    donnees <- donnees[order(donnees$x),]
    
    if (number_groups_left!=0) {
      donnees$group_allocation[donnees$first20==0 ] <- rep(c((21-number_groups_left/20):20), each=20)
    }
    
    donnees <<- donnees 
  }
  
  if (no==1) {
    source("Functions/F_random_algo.R")
    random_algo(no, donnees, x)
  }
  
}

# mean_buckets_algo(no, donnees, x)

