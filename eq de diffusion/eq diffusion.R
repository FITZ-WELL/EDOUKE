   #  Cette fonction calcule l'équation de diffusion.


 #  Cette fonction





#  Eciture des parametres:

nx <- 50        #  Nombre de points dans l'espace
nt <- 100       #  nombre de points temporels
L <- 10         #  Longueur de la tige
T <-  1         #  Temps Total
D <-  1         #  Coefficient de diffusion

dx <- L/(nx-1)
dt <- T/(nt-1)

# Initialisation:

u <- matrix(0,nrow=nx,ncol=nt)
x <- seq(0,L,length.out=nx)
t <- seq(0,T,length.out=nt)

  #  Condition d'initialisaton:

  u[,1] <- exp(-((x-L/2)^2)/0.1)
  #   Boucle temporelle:

for (n in 1:(nt-1)) {
  for(i in 2:(nx-1)){
    u[i,n+1] <- u[i,n] + D * dt / dx^2*(u[i+1,n]-2*u[i,n]+u[i-1,n])
  }

}

   #  Graphique:

persp3D <- function(x,y,z,...){
  zlim <- range(z,finite=TRUE)
  z[is.infinite(z)] <- zlim[1]
  persp(x,y,z,zlim=zlim,...)
}

persp3D(x,t,u,theta=30,phi=30,col="lightblue",main="Equation de Diffusion",xlab="Position",ylab="temps",zlab="Concentration")


