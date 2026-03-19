
fib <- function(x){
    n = 1:x
    for (j in 1:x){
        b = 0
        c = sum(n[(j-1):j])
        print(c)
        n[j+1]=c
    }
return(n)
}

gaussian <- function(mu,sigma){
    size = 10^3
    range = 1.5*sigma
    Xline = (-3*size/2):(3*size/2)
    Xline = Xline/size*2+mu
    Yline = dnorm(Xline, mean = mu, sd = sigma)
    plot(Xline, Yline)
}

doublesum <- function(n1,n2){
    sum = 0
    for (i in 1:n1){
        for(j in 1:n2){
            sum = sum+i^4/(3+j)
        }
    }
    return(sum)
}

doublesum2 <- function(n){
    sum = 0
    for (i in 1:n){
        for(j in 1:i){
            sum = sum+i^4/(3+i*j)
        }
    }
    return(sum)
}

SRW <- function(n){
    output = 0:(n-1)
    for (i in 1:(n-1)){
        output[i+1] = output[i]+1
    }
    return(output)
}
