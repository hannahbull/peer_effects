group_mean_var <- function(donnees, x) {
    
  group_allocation <-c(1:20)
  
  groupchar <-data.frame(group_allocation)
  
 # groupchar$group_effects <- rnorm(20,0,1)
  
  mean_group_allocation <- aggregate(donnees$x, list(group_allocation=donnees$group_allocation), mean)
  colnames(mean_group_allocation)[2] <- "mean_group_allocation"
  
  var_group_allocation <- aggregate(donnees$x, list(group_allocation=donnees$group_allocation), var)
  colnames(var_group_allocation)[2] <- "var_group_allocation"
  
  groupchar <- merge(groupchar, mean_group_allocation, by="group_allocation")
  groupchar <- merge(groupchar, var_group_allocation, by="group_allocation")
  
  donnees <- merge(donnees, groupchar, by="group_allocation")
  
  # plot(groupchar$mean_group_allocation, groupchar$var_group_allocation, main=paste0("Mean vs Variance",s),
  #      xlab="mean", ylab="variance", pch=19)
  
  
  #compute leave-out means
  
  leave_out_mean <- function(mean,x) {
    result <- (20*mean-x)/(20-1)
    return(result)
  }
  
  leave_out_variance <- function(var,x, mean, left_out_mean) {
    result <- ((var+ mean^2)*20-x^2)/(20-1) - left_out_mean^2
    return(result)
  }
  
  donnees$left_out_mean <- leave_out_mean(donnees$mean_group_allocation,donnees$x)
  donnees$left_out_var <- leave_out_variance(donnees$var_group_allocation,donnees$x, donnees$mean_group_allocation,donnees$left_out_mean)

  donnees <<- donnees
  groupchar <<- groupchar
  
  }