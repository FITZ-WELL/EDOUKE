 dft <- function(x){
   n <- length(x)
   X <- complex(real=rep(0,n),imaginary = rep(0,n))

   for (k in 0:(n-1)){
     for(t in 0:(n-1)){
       angle <- -2*pi*k*t/n
       X[k+1] <- X[k+1] + x[t+1]*complex(real = cos(angle),imaginary = sin(angle))
     }
   }
   return(X)
 }

signal <- c(1,2,3,4)
dft_result <- dft(signal)
print(dft_result)



signal <- c(1,2,3,4)
dft_result <- dft(signal)

magnitude <- Mod(dft_result)

#  Ajout des éléments esthétiques:
plot(magnitude,type="h",main="Magnitude des coefficients de fourier", xlab="Frequence",ylab="Magnitude")

grid()
points(magnitude,pch=19, col="red")
lines(magnitude, col="green", lwd=1)



