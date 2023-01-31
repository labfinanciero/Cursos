cartas <- c(as.character(2:10), "J", "Q", "K", "A")
cartas <- rep(cartas, each = 4)

un_par <- function (iter, cartas) {
 mano <- sample(cartas, 52, replace = FALSE)[1:5]
 mano <- unique(mano)
 if (length(mano) == 4) {
  return(1)
 } else {
  return(0)
 }
}

it <- 2000000
probabilidad_par <- sum(sapply(1:it, un_par, cartas)) / it * 100

# simul_par <- function (a, un_par, cartas) {
#  it <- 100000
#  p <-  sum(sapply(1:it, un_par, cartas)) / it * 100
#  return(p)
# }
# probabilidad_par <- sapply(1:1000, simul_par, un_par, cartas)
