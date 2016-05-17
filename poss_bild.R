#
is_poss_dist <- function(x){ if( min(x) >= 0 && max(x) == 1 ) return(TRUE) else return(FALSE) }

slct_mat <- function(v,mat){
	mat2 <- NULL
	for(i in 1:length(v)){ 
	tmp <- c( v[i] , mat[2,v[i]] , mat[3,v[i]]) ;
	mat2 <- cbind(mat2,tmp);
	}
	return(mat2)
}

#pi(A) = max (x)

#
slct_possM <- function(a,x){ min ( slct_mat(a,x)[2,] ) }

#
make_poss_dist2 <- function(possP){ possN = 1; return( rbind(possP,possN) ) }

#
make_poss_dist <- function(n,m,p){
	if(m < 2){id = n; possP = p; possN = 1.0; }
	else{ 
	id = 1:m + n - 1;
	possP <- p + (1:m-1)*(1-p)/(m-1);
	possN <- rep(1.0 ,length = m );
	}
	return(rbind(id,possP,possN));
}

make_poss_d2 <- function(m,p){
	possP <- p + (1:m-1)*(1-p)/(m-1);
	possN <- rep(1.0 ,length = m );
	return(rbind(possP,possN));
}

#
#
poss_m1 <- function(m1,m2){
	x <- m1/m2
	x <- apply(x,2,sapply,function(i) {if(i < 1) return(i) else return(1) })
	return(x)
}

poss_v1 <- function(v1,v2){
	x <- v1/v2
	x <- sapply(x,function(i) {if(i < 1) return(i) else return(1) })
	return(x)
}

#
possP <- function(x,vposs){
	if(x == vposs[1] ) return(vposs[2])
	else return() # NULL 
}

possN <- function(x,vposs){
	if(x == vposs[1] ) return(vposs[3])
	else return() # NULL 
}


#(8)
pi_m1d1d2 <- function(c1d,e) apply(c1d,1,function(i) max(i[e])) 
pi_m2d1d2 <- function(c2d,e) apply(c2d,1,function(i) min(i[e]))

#(7)
pi_mp1e <- function(c1d,c2d,mp,e){
		pim1dd <- pi_m1d1d2(c1d,e)
		pim2dd <- pi_m2d1d2(c2d,e)
		return( min(pim1dd[mp],pim2dd[(1:length(pim1dd))[-mp]]) )
	  }

##　n(v) == ncol(m)
mv_max <- function(m,v) t(apply(m,1,function(i,y){ mapply(max,i,y) },v))
mv_min <- function(m,v) t(apply(m,1,function(i,y){ mapply(min,i,y) },v))

#(6)
pi_m1 <- function(c1d,d1) apply(mv_min(c1d,d1),1,max)
pi_m2 <- function(c2d,d1,d2) apply( mv_max(mv_min(c2d,d1),d2),1,min)


#(5)
pi_mp <- function(c1d,c2d,d1,d2,mp) min(pi_m1(c1d,d1)[mp],pi_m2(c2d,d1,d2)[ (1:nrow(c1d))[-mp] ] )
pi_mp_s <- function(c1d,d1) min(pi_m1(c1d,d1)[mp])

#(4)
pi_e <- function(d1,d2,e) min(d1[e],d2[-e])
pi_e_s <- function(d1,e) min(d1[e])

#(3)
#pi(E|M+)=pi(M+|E)^pi(E),if pi(M+)>pi(M+|E)^pi(E)
#          1                ,otherwise
bayes_poss <- function(w,x,y){ 
	o <- min(x,y)
	if(w <= o)  o <- 1
	return(o)
}

pi_e1mp <- function(c1d,c2d,d1,d2,mp,e){
	w <- pi_mp(c1d,c2d,d1,d2,mp)
	x <- pi_mp1e(c1d,c2d,mp,e)
	y <- pi_e(d1,d2,e)
	return( bayes_poss(w,x,y) )
	}


#(12)
pi_m1d2i <- function(c1d,d1,i) apply( mv_max(c1d,d1)[,-i],1,max)
pi_m2d2i <- function(c2d,d1,d2,i) apply( mv_max(mv_min(c2d,d1),d2)[,-i],1,min)

pi_m1d2 <- function(c1d,d1) {
        x <- c()
	for(i in 1:length(d1) ) {x <- cbind(x, pi_m1d2i(c1d,d1,i) )}
	return(x)
	}

pi_m2d2 <- function(c2d,d1,d2) { 
	x <- c()
	for(i in 1:length(d1) ){x <- cbind(x, pi_m2d2i(c2d,d1,d2,i) )}
	return(x)
	}

#
#(11)
pi_m1d1 <- function(c1d,d1) matrix(mapply(max,c1d,pi_m1d2(c1d,d1)),nrow(c1d),ncol(c1d))
pi_m2d1 <- function(c2d,d1,d2) matrix(mapply(min,c2d,pi_m2d2(c2d,d1,d2)),nrow(c2d),ncol(c2d))

#(10)
pi_mp1d1i <- function(c1d,c2d,d1,d2,mp,i) min( pi_m1d1(c1d,d1)[mp,i],pi_m2d1(c2d,d1,d2)[-mp,i] )
pi_mp1d2i <- function(c1d,c2d,d1,d2,mp,i) min( pi_m1d2(c1d,d1)[mp,i],pi_m2d2(c2d,d1,d2)[-mp,i] )

pi_mp1d1 <- function(c1d,c2d,d1,d2,mp) {
        x <- c()
	for(i in 1:length(d1) ) { x <- c(x, pi_mp1d1i(c1d,c2d,d1,d2,mp,i) ) }
	return(x)
	}

pi_mp1d2 <- function(c1d,c2d,d1,d2,mp) {
        x <- c()
	for(i in 1:length(d1) ) { x <- c(x, pi_mp1d2i(c1d,c2d,d1,d2,mp,i) ) }
	return(x)
	}


#bayes_poss <- function(w,x,y){ 
#	o <- min(x,y)
#	if(w <= o)  o <- 1
#	return(o)
#}

#pi(di|M+)=pi(M+|di)^pi(di),if pi(M+)>pi(M+|di)^pi(di)
#          1                ,otherwise
pi_d1mp <- function(c1d,c2d,d1,d2,mp){
	 pid2mp <- pi_mp(c1d,c2d,d1,d2,mp)
	 pimp1d1 <- pi_mp1d1(c1d,c2d,d1,d2,mp)
	 x <- c()
	 for(i in 1:length(d1)) x <- c(x , bayes_poss(pid2mp,pimp1d1[i],d1[i]) )
	 return(x)
	}

#pi(/di|M+)=pi(M+|/di)^pi(/di),if pi(M+)>pi(M+|/di)^pi(/di)
#          1                ,otherwise
pi_d2mp <- function(c1d,c2d,d1,d2,mp){
	 pid2mp <- pi_mp(c1d,c2d,d1,d2,mp)
	 pimp1d2 <- pi_mp1d2(c1d,c2d,d1,d2,mp)
	 x <- c()
	 for(i in 1:length(d2)) x <- c(x , bayes_poss(pid2mp,pimp1d2[i],d2[i]) )
	 return(x)
	}

#pi(/E|M+)
pi_e2mp <- function(c1d,c2d,d1,d2,mp,e) max(pi_d2mp(c1d,c2d,d1,d2,mp)[e],pi_d1mp(c1d,c2d,d1,d2,mp)[-e] )

pi_emp_dist0 <- function(c1d,c2d,d1,d2,mp,e) c(pi_e1mp(c1d,c2d,d1,d2,mp,e),pi_e2mp(c1d,c2d,d1,d2,mp,e)) 

pi_emp_dist <- function(cp,dp,mp,e) c(pi_e1mp(cp[[1]],cp[[2]],dp[[1]],dp[[2]],mp,e),pi_e2mp(cp[[1]],cp[[2]],dp[[1]],dp[[2]],mp,e)) 


check1pi_e2mp <- function(p1d,p2d,d1,d2){
x1d <- p1d
for(i in 1:nrow(p1d)) for(j in 1:ncol(p1d)) x1d[i,j] <- pi_e2mp(p1d,p2d,d1,d2,i,j)
return(x1d)
}

check1pi_e1mp <- function(p1d,p2d,d1,d2){
x1d <- p1d
for(i in 1:nrow(p1d)) for(j in 1:ncol(p1d)) x1d[i,j] <- pi_e1mp(p1d,p2d,d1,d2,i,j)
return(x1d)
}

checkn1pi_e2mp <- function(p1d,p2d,d1,d2){
x1d <- p1d
for(i in 1:nrow(p1d)) for(j in 1:ncol(p1d)) x1d[i,j] <- pi_e2mp(p1d,p2d,d1,d2,i,j)
return(x1d)
}

checkn1pi_e1mp <- function(p1d,p2d,d1,d2){
x1d <- p1d
for(i in 1:nrow(p1d)) for(j in 1:ncol(p1d)) x1d[i,j] <- pi_e1mp(p1d,p2d,d1,d2,-i,-j)
return(x1d)
}

check1pimp1e <- function(p1d,p2d){
x1d <- p1d
for(i in 1:nrow(p1d)) for(j in 1:ncol(p1d)) x1d[i,j] <- pimp1e(p1d,p2d,-i,-j)
return(x1d)
}



