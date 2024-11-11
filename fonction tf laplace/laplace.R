# fonction pour le calcul de la transformée de laplace:

laplace_transform <- function(f,s){
  integrate(function(t){f(t) * exp(-s*t)}, 0 ,Inf) $ value
}
#  Illustration de la transformation de laplace d'une fonction:
f <- function(t) {exp(-2*t)}

#  Calcul de la transformation de laplace pour differentes valeurs de s
s_values <- seq(0 , 10 , by=0.1)
result_TL <- sapply(s_values, function(s) laplace_transform(f,s))

#  Tracé du graphe:
plot(s_values,result_TL, type="l",lwd=2,col="blue",main="Transformation de laplace de f(t)",xlab="s",ylab="L(f)(s)")
grid()
points(s_values,result_TL,pch=19,col="red")
lines(s_values,result_TL,col="green",lwd=1)




