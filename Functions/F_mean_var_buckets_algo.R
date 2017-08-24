mean_var_buckets_algo <- function(no, donnees, x) {
 
   if (no==1) {
    source("Functions/F_random_algo.R")
    random_algo(no, donnees, x)
  }
  
  else {
    
  assigning_groups_by_buckets <- function(b, donnees) {
  
  repeats_vector <- sample(rep(floor(nrow(donnees)/b),b)+c(rep(1,nrow(donnees) %% b),rep(0,b-nrow(donnees) %% b)))
  #sum(repeats_vector)
  donnees <- donnees[order(donnees$x),]
  
  buckets_vector <- c()
  for (i in 1:b) {
    temp_vector <- rep(i,repeats_vector[i])
    buckets_vector <- c(buckets_vector, temp_vector)
  }
  
  donnees$buckets <- buckets_vector
  donnees$random_ordering <-  rnorm(nrow(donnees),0,1)
  
  #b=1
  decimals <- sample(c(1:(2^b-1)), min(20,(2^b-1)), replace=F)
  m <- sapply(decimals,function(x){as.integer(intToBits(x))})
  m <- m[1:b,]
  m <- rbind(m,colSums (m, na.rm = FALSE, dims = 1))
  
  donnees <- donnees[order(donnees$x),]
  donnees$rank <- c(1:nrow(donnees))
  # donnees$buckets <- rep(c(1:b),each=400/b)
  
  #sample 20 values from columns where sum is 1
  for (j in 1:(min(20,(2^b-1)))) {
    draws=sample(c(20,20),1,replace=F)
    if (m[(b+1),j]==1) {
      for (i in 1:b) {
        if (m[i,j]==1) {
          m[i,j] <- draws[1]
        }
      }
    }
  }
  #sample 20 values from columns where sum is k>1
  for (k in 2:b) {
    for (j in 1:(min(20,(2^b-1)))) {
      sample_to_draw_from=rep(floor(20/k),k)+c(rep(1,20 %% k),rep(0,k-20 %% k))
      draws=sample(sample_to_draw_from,k,replace=F)
      counter_of_draws=0
      if (m[(b+1),j]==k) {
        for (i in 1:b) {
          if (m[i,j]==1) {
            counter_of_draws=counter_of_draws+1
            m[i,j] <- draws[counter_of_draws]
          }
        }
      }
    }
  }
  
  #choose items from buckets
  #ordering_sample=sample(1:(2^b-1),min(20,2^b-1),replace=F)
  counter=max(donnees$group_allocation, 0, na.rm = TRUE)
  for (j in 1:(min(20,2^b-1))) {
    data_temp_group <- data.frame(x=as.numeric(), n=as.integer(), rank=as.integer(), buckets=as.integer(), group_allocation=as.integer())
    for (i in 1:b) {
      data_temp <- donnees[donnees$buckets==i & is.na(donnees$group_allocation ),]
      if (nrow(data_temp)>=m[i,j]) {
        data_temp <- sample_n(data_temp, m[i,j])
        data_temp_group<-rbind(data_temp_group,data_temp)
      }
      else {
        data_temp_group <- data.frame(x=as.numeric(), n=as.integer(), rank=as.integer(), buckets=as.integer(), group_allocation=as.integer())
        break
      }
    }
    if (nrow(data_temp_group)>0) {
      counter=counter+1
    }
    donnees$group_allocation[donnees$rank %in% data_temp_group$rank] <- counter
  }
  donnees <<- donnees
  counter <<- counter
  }
  
  assigning_groups_by_buckets(no, donnees)
  donnees<-donnees
  counter_new=counter
  
  for (i in 1:1000) {
  
  if (counter_new==20) {
    break
  }
  
  if (counter_new==19) {
    donnees$group_allocation[is.na(donnees$group_allocation)] <- 20
    break
  }
  
  if (counter<19) {
    
    donnees_sorted<- donnees[!is.na(donnees$group_allocation), ]
    donnees_left_to_sort<- donnees[is.na(donnees$group_allocation), ]
    
    assigning_groups_by_buckets(no, donnees_left_to_sort)
    donnees_left_to_sort<-donnees
    
    donnees_left_to_sort$group_allocation<-donnees_left_to_sort$group_allocation + counter_new
    donnees <- rbind(donnees_sorted, donnees_left_to_sort)
    
    counter_new=counter+counter_new
  }
  }
  donnees <<- donnees
  }
}