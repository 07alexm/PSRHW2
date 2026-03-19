# Problem 1
#a) We have \lambda|Y1,...,Yn~Gam(a+n,a+\sumYi).
n <-10
a <- 1
b <- 1
data <- rexp(n, rate = 2)
quan = qgamma()
lambdadist <- function(data, a, b) {
  a = a + length(data)
  b = b + sum(data)
  post <- function(lambda) {
    b ^ a / gamma(a) * lambda ^ (a - 1) * exp(-lambda * b)
  }
  return(post)
}
post = lambdadist(data, a, b)
post(1)

# Problem 2
monteint <- function(int, f, n) {
  X <- runif(n, min = int[1], max = int[2])
  fX = f(X)
  1 / n * sum(fX * (int[2] - int[1]))
}
x2 <- function(x) {
  x * x
}
monteint(10000)

# Problem 3
montecircle <- function(n) {
  circle <- 0
  square <- 0
  Xline <- 1:n
  Yline <- 1:n
  for (i in 1:n){
    x <- runif(2, min = 0, max = 1)
    x = x * x
    x = sum(x)
    if (x < 1) {
      circle = circle + 1
      square = square + 1
    }else {
      square = square + 1
    }
    piapprox = circle / square * 4
    Yline[i] = piapprox
  }
  plot(Xline, Yline)
  piapprox
}
montecircle(10000)
