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

total.sum.sqrs<-function(system.sum.sqrs){
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

sample.ss<-function(system.sum.sqrs){
  #Legendre(5a)
  sample.sum.sqrs<-rowSums(system.sum.sqrs)
  return(sample.sum.sqrs)
}

local.cont.bdiv<-function(system.sum.sqrs){
  #Legendre(5b)
  ss.i<-sample.ss(system.sum.sqrs)
  lcbd<-(ss.i/total.sum.sqrs(system.sum.sqrs))
  return(lcbd)
}

#'Wraps the functions in order to return (1) total variance in the system, (2) the species contributions
#'   to beta diversity, and (3) the local contributions to beta diversity. 
b.div.wrap<-function(soil.system){
  system.sum.sqrs<-sqrd.diffs(soil.system)
  total.var<-b.div(system.sum.sqrs)
  species.cont<-species.cont.bdiv(system.sum.sqrs)
  sample.cont<-local.cont.bdiv(system.sum.sqrs)
  return(list(total=total.var,species=species.cont,sites=sample.cont))
}

#'Calculates the beta diversity using the three different methods presented by Legendre
b.div.forked<-function(soil.system){
  system.sum.sqrs<-sqrd.diffs(soil.system)
  #Legendre(3[species])
  species.fork<-sum(species.ss(system.sum.sqrs))/(nrow(system.sum.sqrs)-1)
  #Legendre(3[samples])
  sample.fork<-sum(sample.ss(system.sum.sqrs))/(nrow(system.sum.sqrs)-1)
  #Legendre(3[system])
  total.var<-b.div(system.sum.sqrs)
  return(list(total=total.var,species=species.fork,sites=sample.fork))
}



