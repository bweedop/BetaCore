#'The following is calculating the total sum of squares and the 
#' index of beta diversity.
sqrd.diffs<- function(soil.system) {
  #Legendre(1)
  yj.bar<-colMeans(soil.system)
  for(i in 1:nrow(soil.system)){
    for(j in 1:ncol(soil.system)){
      soil.system[i,j]<-(soil.system[i,j]-yj.bar[j])^2
    }
  }
  return(soil.system)
}

<<<<<<< HEAD
total.sum.sqrs<-function(system.sum.sqrs){
=======
# Will says: this needs to be done on the square differences, not the
# input matrix. Thus this function isn't "wrong", but it's not *right*
# unless you use it with the output from sqrd.diffs ...which I now see
# that you new judging by your wrapper. Carry on...
total.sum.sqrs<-function(soil.system){
>>>>>>> 9a1d9a8dd5870b9fc487e1f8a22f5720f64e43d6
  #Legendre(2)
  p.sum<-colSums(system.sum.sqrs)
  ss.tot<-(sum(p.sum))
  return(ss.tot)
}
#'This calculates the total beta diversity (BD.total) or the variance of the 
#'  entire system.
b.div<-function(soil.system){
  #Legendre(3)
  bd.tot<-(total.sum.sqrs(soil.system)/(nrow(soil.system)-1))
  return(bd.tot)
}

species.ss<-function(system.sum.sqrs){
  #Legendre(4a)
  species.sum.sqrs<-colSums(system.sum.sqrs)
  return(species.sum.sqrs)
}

species.cont.bdiv<-function(system.sum.sqrs){
  #Legendre(4b)
  ss.j<-species.ss(system.sum.sqrs)
  scbd<-(ss.j/total.sum.sqrs(system.sum.sqrs))
  return(scbd)
}

<<<<<<< HEAD
sample.ss<-function(system.sum.sqrs){
  #Legendre(5a)
  sample.sum.sqrs<-rowSums(system.sum.sqrs)
=======
# Will:s you should calculate this for all the cores, and return the
# contributions for all the cores
sample.ss<-function(soil.system, core){
  #Legendre(5a)
  sample.sum.sqrs<-sum(soil.system[core]) # Will: should this be ",core"? (a bit moot now anyway :p)
>>>>>>> 9a1d9a8dd5870b9fc487e1f8a22f5720f64e43d6
  return(sample.sum.sqrs)
}

local.cont.bdiv<-function(system.sum.sqrs){
  #Legendre(5b)
  ss.i<-sample.ss(system.sum.sqrs)
  lcbd<-(ss.i/total.sum.sqrs(system.sum.sqrs))
  return(lcbd)
}

<<<<<<< HEAD
b.div.wrap<-function(soil.system){
  system.sum.sqrs<-sqrd.diffs(soil.system)
  total.var<-b.div(soil.system)
  species.cont<-species.cont.bdiv(system.sum.sqrs)
  sample.cont<-local.cont.bdiv(system.sum.sqrs)
  return(list(total=total.var,species=species.cont,sites=sample.cont))
}

=======
## Will: give me a nicer name please :D
## ...and make three functions:
## (1) calc. species contributions
## (2) calc. site contributions
## (3) calc. total variance in system.
## ... you could write some sort of nice wrapper for all these things
## if you wished; I don't really mind :D
wrapper<-function(soil.system){
  soil.system<-sqrd.diffs(soil.system)
  ss.tot<-total.sum.sqrs(soil.system)
  return(ss.tot)
}

## ...just one more thing...
## Write unit tests for this, that include:
## (1) a stated dataset with values for it (something small and with
##     just 1s and zeroes that isn't random)
## (2) that use the way that the three arrows in equations 2, 4, and 5
##     should converge to build tests
>>>>>>> 9a1d9a8dd5870b9fc487e1f8a22f5720f64e43d6
