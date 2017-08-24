### computes KL divergence between random allocation and allocation
random_matrix_for_KL <- function(donnees, x, m_or_v) {
  donnees_old <- donnees
  
  donnees <- select(donnees,x)
  
  source("Functions/F_random_algo.R")
  random_algo(no, donnees, x)
  
  donnees <- donnees
  
  #Compute leave out mean and leave out variance 
  source("Functions/F_group_mean_var.R")
  group_mean_var(donnees, x)
  
  donnees <- donnees
  
  donnees$x <- donnees$x-mean(donnees$x)
  donnees$left_out_mean <- donnees$left_out_mean-mean(donnees$left_out_mean)
  donnees$left_out_var <- donnees$left_out_var-mean(donnees$left_out_var)
  
  if (1==1) {
    x_random <- as.matrix(cbind(donnees$x, donnees$left_out_mean))
  }
  
  if (m_or_v==2) {
    x_random <- as.matrix(cbind(donnees$x, donnees$left_out_mean, donnees$left_out_var))
  }
   
  donnees <- donnees_old
  
  x_random <<- x_random

}
#install.packages("FNN")

