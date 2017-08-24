
random_algo <- function(no, donnees, x) {
  donnees$random_sorting_variable <- rnorm(400,0,1)
  donnees <- donnees[order(donnees$random_sorting_variable),]
  donnees$group_allocation <- rep(c(1:20),each=20)
  donnees <- select(donnees, -random_sorting_variable)
  
  donnees <<- donnees
}

#random_algo(no, donnees, x)