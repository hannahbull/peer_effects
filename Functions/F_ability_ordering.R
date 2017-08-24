
ability_ordering <- function(no, donnees, x) {
  donnees <- donnees[order(donnees$x),]
  donnees$group_allocation <- rep(c(1:20),each=20)
  donnees <<- donnees 
}
