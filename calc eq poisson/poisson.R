  #  Creation d'une fonction qui calcul les EDP comme l'equation de poisson

solve_poisson <- function(f,nx,ny,Lx,Ly,tol=1e-5){
  dx <- Lx/(nx - 1)
  dy <- Ly/(ny - 1)
  u <- matrix(0, nrow=nx, ncol = ny)  # Source uniforme
  error <- 1

  while(error>tol){
    u_new <- u
    for(i in 2:(nx - 1)) {
      for(j in 2:(ny - 1)) {
        u_new[i, j] <- 0.25*(u[i+1, j] + u[i-1, j] + u[i, j+1] + u[i, j-1] - dx * dy *f[i, j])
      }
    }
    error <- max(abs(u_new - u))
    u <- u_new
  }
  return(u)
}

  #  Exemple d'application pour l'equation de poisson:
nx <- 50
ny <- 50
Lx <- 10
Ly <- 10


f <-  matrix(1,nrow = nx, ncol = ny)

u_poisson <-  solve_poisson(f,nx,ny,Lx,Ly)

 #  Graphique de la solution:

persp3D <- function(x,y,z, ...){
  zlim <- range(z,finite=TRUE)
  z[is.infinite(z)] <- zlim[1]
  persp(x,y,z,zlim=zlim, ...)
}

x <- seq(0,Lx,length.out=nx)
y <- seq(0,Ly,length.out=ny)

grid()
persp3D(x,y,u_poisson, theta=48,phi=30,col="yellow",main="Equation de poisson",xlab="Position X",ylab="position Y",zlab="solution")





