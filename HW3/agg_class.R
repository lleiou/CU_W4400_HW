agg_class<-function(X,alpha,allPars){
  
 

  n<-length(X[,1])
  #RESULT IS USED TO STORE THE CLASSIFY RESULT IN X
  result<-rep(0,n)
  
  
  for(i in 1:n){
    sum<-0
    
    for(j in 1:length(allPars[,1])){
    
    sum<-sum+alpha[j]*classify(X[i,],allPars[j,])
    
    }
  
 result[i]<-sign(sum) 
  }
  
  return(as.vector(result))
  
  
}
