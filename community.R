#'initializing the community data table (Y) with any number of samples
#'   contains any amount of species or OTUs. For now this is just an
#'   example with random data until I figure out how to use the  
#'   sequence data to represent whether the otu is present or absent.
sys.comp<-function(cores, otu){
  comm.tbl<-matrix(sample(0:1,cores*otu,replace=TRUE),nrow = cores, ncol = otu)
  return(comm.tbl)
}
#'The following is calculating the total sum of squares and the 
#' index of beta diversity. This may have to altered in the future
#' but for the time being, I think this will function for it's 
#' purposes.
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

# Will says: this needs to be done on the square differences, not the
# input matrix. Thus this function isn't "wrong", but it's not *right*
# unless you use it with the output from sqrd.diffs ...which I now see
# that you new judging by your wrapper. Carry on...
total.sum.sqrs<-function(soil.system){
  #Legendre(2)
  n.sum<-rowSums(soil.system)
  p.sum<-colSums(soil.system)
  ss.tot<-(sum(n.sum)+sum(p.sum))
  return(ss.tot)
}

b.div<-function(soil.system){
  #Legendre(3)
  bd.tot<-(ss.tot/(nrow(soil.system)-1))
  return(bd.tot)
}

species.ss<-function(soil.system){
  #Legendre(4a)
  species.sum.sqrs<-colSums(soil.system)
  return(species.sum.sqrs)
}

species.cont.bdiv<-function(soil.system){
  #Legendre(4b)
  ss.j<-species.ss(soil.system)
  scbd<-(ss.j/total.sum.sqrs(soil.system))
  return(scbd)
}

# Will:s you should calculate this for all the cores, and return the
# contributions for all the cores
sample.ss<-function(soil.system, core){
  #Legendre(5a)
  sample.sum.sqrs<-sum(soil.system[core]) # Will: should this be ",core"? (a bit moot now anyway :p)
  return(sample.sum.sqrs)
}

local.cont.bdiv<-function(soil.system, core){
  #Legendre(5b)
  ss.i<-sample.ss(soil.system, core)
  lcbd<-(ss.i/total.sum.sqrs(soil.system))
  return(lcbd)
}

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
