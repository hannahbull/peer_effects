list.of.packages <- c("plyr", "dplyr", "plm", "xtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("plyr")
library("dplyr")
library("plm")
library("xtable")

setwd("/home/hannah/Documents/Memoire")

temp_results <- data.frame(type=character(), m_or_v=character(), no_buckets=numeric(), s=numeric(), 
                          order_min_var=numeric(), order_tr_xtx_inv=numeric(), 
                                         order_tr_xtx=numeric(), order_det_xtx=numeric(), 
                          order_kl=numeric(), order_var_y_incorrect_2=numeric(), order_var_y_correct=numeric(), order_var_y_incorrect=numeric(), 
                          sum_LOV=numeric(), stringsAsFactors=FALSE)

xtx_mean_random <- 0*diag(2)
xtx_var_random <- 0*diag(3)

#no is the number of buckets, irrelevant for random allocation
for (no in 1:20) {

rm(list = setdiff(ls(),c("no","temp_results", "xtx_mean_random", "xtx_var_random")))
  
### choose group allocation rule c("random", "streaming", "var_buckets", "mean_var_buckets" )
  type="var_buckets"
### choose mean or variance 
  m_or_v="v"
### choose S, number of random draws
  S=100
### choose distribution  c("uniform", "normal", "left_tail" )
  dist="uniform"
    
set.seed(2)

n <- c(1:400)

# distribution
  if (dist=="normal"){
    x <- rnorm(400,0,1)
  }
if (dist=="uniform"){
  x <- runif(400,0,1)
}
if (dist=="left_tail"){
  x <- (1-rbeta(400,2,5,0))
}

estimates<- data.frame()
  
# S is the number of simulations
 for (s in 1:S) {
  donnees <- data.frame(x,n)
  donnees$group_allocation <- NA 

  #group allocation algorithm 
  # # 1) random group allocation 
  if (type=="random") {
  source("Functions/F_random_algo.R")
  random_algo(no, donnees, x)
  }
  
  # # 2) buckets algorithm, streaming into levels
  if (type=="streaming") {
  source("Functions/F_mean_buckets_algo.R")
  mean_buckets_algo(no, donnees, x)
  }
   
  # # 3) buckets algorithm, variance
  if (type=="var_buckets") {
  source("Functions/F_var_buckets_algo.R")
  var_buckets_algo(no, donnees, x)
  }
  
  # # 4) buckets algorithm, mix of mean and variance
  if (type=="mean_var_buckets") {
  source("Functions/F_mean_var_buckets_algo.R")
  mean_var_buckets_algo(no, donnees, x)
  }
  
  donnees <- select(donnees, x, group_allocation)

  #Compute leave out mean and leave out variance 
  source("Functions/F_group_mean_var.R")
  group_mean_var(donnees, x)
  
  #demeaning 
  donnees$x <- donnees$x-mean(donnees$x)
  donnees$left_out_mean <- donnees$left_out_mean-mean(donnees$left_out_mean)
  left_out_var_before_demean <- donnees$left_out_var
  donnees$left_out_var <- donnees$left_out_var-mean(donnees$left_out_var)
  
  #peer effects with only mean
  donnees$y_mean <- donnees$x+0.2*donnees$left_out_mean + rnorm(length(donnees$x),0,1)
  #peer effects with mean and variance
  donnees$y_mean_var <- donnees$x+0.2*donnees$left_out_mean +0.2*donnees$left_out_var + rnorm(length(donnees$x),0,1)
  
  donnees_summary <- donnees %>%
    group_by(group_allocation) %>%
    mutate_all(funs(mean))
  
  # compute different measures 
  
  x_mean=as.matrix(cbind(donnees$x, donnees$left_out_mean))
  x_var=as.matrix(cbind(donnees$x, donnees$left_out_mean, donnees$left_out_var))
  
  xtx_mean= t(x_mean)%*%(x_mean)
  xtx_var=  t(x_var)%*%(x_var)
  xtx_inverse_mean=solve(xtx_mean)
  xtx_inverse_var=solve(xtx_var)
  
  # 1) important variable 
  order_min_var_mean = xtx_inverse_mean[2,2]
  order_min_var_var = xtx_inverse_var[3,3]
  
  # 2a) trace
  order_tr_xtx_inv_mean = sum(diag(xtx_inverse_mean))
  order_tr_xtx_inv_var = sum(diag(xtx_inverse_var))
  
  # 2b) trace
  order_tr_xtx_mean = sum(diag(xtx_mean))
  order_tr_xtx_var = sum(diag(xtx_var))
  
  # 3) det 
  order_det_xtx_mean = det(xtx_mean)
  order_det_xtx_var = det(xtx_var)
  
  # 4) KL divergence
  ### keep no=1 matrix 
  if (no==1 & m_or_v=="m") {
    order_kl=0
    xtx_mean_random <<- xtx_mean+xtx_mean_random
  }
  
  if (no==1 & m_or_v=="v") {
    order_kl=0
    xtx_var_random <<- xtx_var+xtx_var_random
  }
  
  if (no!=1) {
  
    if (m_or_v=="m") {
      order_kl= 0.5*(sum(diag(solve(xtx_mean_random/S)%*%xtx_mean))+sum(diag((xtx_mean_random/S)%*%solve(xtx_mean)))-4)
    }
    if (m_or_v=="v") {
      order_kl= 0.5*(sum(diag(solve(xtx_var_random/S)%*%xtx_var))+sum(diag((xtx_var_random/S)%*%solve(xtx_var)))-6)
      }
  }
  
  # 5a) b'x'xb correct
  correct_b_mean = as.matrix(c(1, 0.2))
  correct_b_var = as.matrix(c(1, 0.2, 0.2))
  order_var_y_mean_correct = t(correct_b_mean)%*%xtx_mean%*%correct_b_mean
  order_var_y_var_correct = t(correct_b_var)%*%xtx_var%*%correct_b_var
  
  # 5b) b'x'xb incorrect
  incorrect_b_mean = as.matrix(c(0.5,0.5))
  incorrect_b_var = as.matrix(c(0.5, 0.5, 0.5))
  order_var_y_mean_incorrect = t(incorrect_b_mean)%*%xtx_mean%*%incorrect_b_mean
  order_var_y_var_incorrect = t(incorrect_b_var)%*%xtx_var%*%incorrect_b_var
  
  # 5b) b'x'xb incorrect
  incorrect_b_mean_2 = as.matrix(c(1,-0.2))
  incorrect_b_var_2 = as.matrix(c(-1, 0.2, -0.2))
  order_var_y_mean_incorrect_2 = t(incorrect_b_mean_2)%*%xtx_mean%*%incorrect_b_mean_2
  order_var_y_var_incorrect_2 = t(incorrect_b_var_2)%*%xtx_var%*%incorrect_b_var_2
  
  
  ### sum of leave out variance
  sum_LOV = sum(left_out_var_before_demean)
    
    if (m_or_v=="m") {
      temp_results[nrow(temp_results)+1,1:2] <- c(type, m_or_v)
      temp_results[nrow(temp_results),3:ncol(temp_results)] <- c(no, s, order_min_var_mean, order_tr_xtx_inv_mean, 
                          order_tr_xtx_mean, order_det_xtx_mean, order_kl, order_var_y_mean_incorrect_2, order_var_y_mean_correct, order_var_y_mean_incorrect, sum_LOV )
    }
    
    if (m_or_v=="v") { 
      temp_results[nrow(temp_results)+1,1:2] <- c(type, m_or_v)
      temp_results[nrow(temp_results),3:ncol(temp_results)] <- c( no, s, order_min_var_var, order_tr_xtx_inv_var, 
                            order_tr_xtx_var, order_det_xtx_var, order_kl, order_var_y_var_incorrect_2, order_var_y_var_correct, order_var_y_var_incorrect, sum_LOV )
    }

  }  


}

results_summary <- temp_results %>%
  group_by(type, m_or_v, no_buckets) %>%
  mutate_all(funs(mean))
results_summary$s <- S 
results_summary <- results_summary[!duplicated(results_summary),]


rank_each_variable <- data.frame(results_summary)
rank_each_variable$order_min_var <- rank(rank_each_variable$order_min_var)
rank_each_variable$order_tr_xtx_inv <- rank(rank_each_variable$order_tr_xtx_inv)
rank_each_variable$order_tr_xtx <- rank(-rank_each_variable$order_tr_xtx)
rank_each_variable$order_det_xtx <- rank(-rank_each_variable$order_det_xtx)
rank_each_variable$order_var_y_incorrect_2 <- rank(-rank_each_variable$order_var_y_incorrect_2)
rank_each_variable$order_kl <- rank(-rank_each_variable$order_kl)
rank_each_variable$order_var_y_correct <- rank(-rank_each_variable$order_var_y_correct)
rank_each_variable$order_var_y_incorrect <- rank(-rank_each_variable$order_var_y_incorrect)
rank_each_variable$sum_LOV <- rank(-rank_each_variable$sum_LOV)

if (m_or_v=="m") {
  rank_each_variable <- select(rank_each_variable, no_buckets, order_min_var, order_tr_xtx_inv, order_tr_xtx, order_det_xtx, 
                             order_kl, order_var_y_correct, order_var_y_incorrect, order_var_y_incorrect_2 )
}

if (m_or_v=="v") {
  rank_each_variable <- select(rank_each_variable, no_buckets, order_min_var, order_tr_xtx_inv, order_tr_xtx, order_det_xtx, 
                               order_kl, order_var_y_correct, order_var_y_incorrect, order_var_y_incorrect_2, sum_LOV )
}

print(xtable(rank_each_variable, digits = 0, row.names=F), include.rownames = F)

write.csv(temp_results, paste0("results_", dist, "_", type , "_", m_or_v, "_", S, "_", H, "_", K , ".csv"))
