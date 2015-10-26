classify<-function(X,pars){
  
  #X here is just a single vector
  
  sign(as.numeric(pars[3])*(X[as.numeric(pars[1])]-as.numeric(pars[2])))

}
