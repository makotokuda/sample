source("poss_bild.R")

c0d <- matrix(1,19,19)
c01d <- matrix(sample(c(0,1),361,replace=TRUE),19,19)
cp1d <- matrix(runif(361),19,19)
c1d <- c0d - c01d*cp1d

c02d <- matrix(sample(c(0,1),361,replace=TRUE),19,19)
cp2d <- matrix(runif(361),19,19)
c2d <- c0d - c02d*cp2d

cpd <- poss_m1(c1d,c2d)
cnd <- poss_m1(c2d,c1d)

cpx <- matrix(1,4,6) 
cnx <- matrix(runif(24),4,6)

dpx <- runif(6)
dnx <- rep(1,6)

dn <- rep(1,19)
d0 <- sample(c(0,1),19,replace=TRUE)
dp <- runif(19)


rundpie1mp <- function(n) {
	x <- c()
	for(i in 1:n){
		mpset0 <- sample(1:4,sample(1:4,1,TRUE))
		eset0 <- sample(1:6,sample(1:6,1,TRUE))
		pie1mp <- pi_e1mp(cpx,cnx,dpx,dnx,mpset0,eset0)
		x<- c(x,pie1mp)
		}
	return(x)
	}

#mpset <- sort(sample(1:4,sample(1:4,1,TRUE)))
#eset <- sort(sample(1:6,sample(1:6,1,TRUE)))
mpset <- sort(sample(1:3,sample(1:3,1,TRUE)))
eset <- sort(sample(1:3,sample(1:3,1,TRUE)))
#eset <- c(3,6)

rmpset <- function() sample(1:4,sample(1:4,1,TRUE))
reset <- function() sample(1:6,sample(1:6,1,TRUE))
ndp <- c(1,0.1,0.7)
ndn <- c(1,1,1)


mpset <- 2
eset <- 1
#				  1,1 1,2 1,3 2,1 2,2 2,3 3,1 3,2 3,3
ncp <- t(matrix(c(1.0,0.0,1.0,1.0,0.8,0.0,0.7,1.0,1.0),3,3))
ncn <- t(matrix(c(0.1,1.0,1.0,0.1,1.0,1.0,1.0,1.0,0.1),3,3))

cpx <- ncp
cnx <- ncn
dpx <- ndp
dnx <- ndn

#7 = 2^m -1


mpi_e1mp <- function() {
nmpset <- list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))
neset <- list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))
nx <- c()
 for(i in 1:7)
  for(j in 1:7){
	nx <- c(nx, pi_e1mp(cpx,cnx,dpx,dnx,nmpset[[i]],neset[[j]]) )
  }

 return(matrix(nx,7,7))
}

mpi_e2mp <- function() {
nmpset <- list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))
neset <- list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))
nx <- c()
 for(i in 1:7)
  for(j in 1:7){
	nx <- c(nx, pi_e2mp(cpx,cnx,dpx,dnx,nmpset[[i]],neset[[j]]) )
  }

 return(matrix(nx,7,7))
}

#(8)
pim1d1d2 <- pi_m1d1d2(cpx,eset)
pim2d1d2 <- pi_m2d1d2(cnx,eset) 
#(7)
pimp1e <- pi_mp1e(cpx,cnx,mpset,eset)

#(6)
pim1 <- pi_m1(cpx,dpx)
pim2 <- pi_m2(cnx,dpx,dnx)

#(5)
pimp <- pi_mp(cpx,cnx,dpx,dnx,mpset)

#(4)
pie <- pi_e(dpx,dnx,eset)

#(3) pi(M+|E) <- pim1(M+|E)
pie1mp <- pi_e1mp(cpx,cnx,dpx,dnx,mpset,eset)

pim1d2 <- pi_m1d2(cpx,dpx)
pim2d2 <- pi_m2d2(cnx,dpx,dnx)
pim1d1 <- pi_m1d1(cpx,dpx)
pim2d1 <- pi_m2d1(cnx,dpx,dnx)
#pie1mp <- pi_e1mp(cpx,cnx,dpx,dnx,mpset,eset)
pimp1d1 <- pi_mp1d1(cpx,cnx,dpx,dnx,mpset)
pimp1d2 <- pi_mp1d2(cpx,cnx,dpx,dnx,mpset)
pid1mp <- pi_d1mp(cpx,cnx,dpx,dnx,mpset)
pid2mp <- pi_d2mp(cpx,cnx,dpx,dnx,mpset)

pie2mp <- pi_e2mp(cpx,cnx,dpx,dnx,mpset,eset)


