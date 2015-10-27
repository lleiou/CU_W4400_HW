AdaBoost<-function(X,y,B){
#note here that y is a Xframe instead of a vector
#use as.vector to transform it, so that it will be a lot more easier...


n<-length(y)
dim<-length(X[1,])
#use the allPars to store the the (j, theta, m) 
#which specify the decision stump that we get in every iteration...
allPars<-data.frame(j=rep(0, B),theta=rep(0, B),m=rep(0, B))

#use the Xframe "final" to store the optimal value,..
#the orientation of the classier, and so on in every axis j
final<-data.frame(j=c(1:dim),cum=rep(0,dim),theta=rep(0,dim),m=rep(0,dim))
alpha<-rep(0,B)
w<-rep(1/n,n)

for(b in 1:B){
  
  pars<-train(X,w,y)
  
  for(z in 1:3){
  allPars[b,z]<-pars[z]
  }
  
  
  indicator<-1*(y!=sign(pars[3]*(X[,pars[1]]-pars[2])))
  #indicator[which(sign(pars$m[b]*(X[,pars$j[b]]-pars$theta[b]))==0)]<-0
 
  error<-sum(w*indicator)/sum(w)
  alpha[b]<-log((1-error)/error)
  
  for(k in 1:n){
    
    w[k]<-w[k]*exp(alpha[b]*indicator[k])
    
  }
}

return(list(alpha=alpha,allPars=allPars))


}

