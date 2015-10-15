###################This is the complete code for solving the machine learning HW2 Question2, Columbia University.
###################Cannot guarantee the accuracy of the whole answer.
###################Whoever wants to use this code, please check the code by yourselves. Thank you very much!

####################################################
################### This R file contains four parts:
################### fakedata(w,n)
################### classify(S,z)
################### perceptrain(S,y)
################### the implementation of the functios
#####################################################


#########################################
################### PART I: fakedata(w,n)
#########################################


#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){
  
  if(! require(MASS))
  {
    install.packages("MASS")
  }
  if(! require(mvtnorm))
  {
    install.packages("mvtnorm")
  }
  
  require(MASS)
  require(mvtnorm)
  
  # obtain dimension
  d <- length(w)-1
  
  # compute the offset vector and a Basis consisting of w and its nullspace
  offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
  Basis <- cbind(Null(w[1:d]), w[1:d])	 
  
  # Create samples, correct for offset, and extend
  # rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
  S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
  S <- S + matrix(rep(offset,n),n,d,byrow=T)
  S <- cbind(S,1)
  
  # compute the class assignments
  y <- as.vector(sign(S %*% w))
  
  # add corrective factors to points that lie on the hyperplane.
  S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
  y = as.vector(sign(S %*% w))
  return(list(S=S, y=y))
  
} # end function fakedata



#########################################
################### PART II classify(S,z)
##########################################


classify<-function(S,z){
  
  n<-length(S[,1])
  fy<-c(1:n)
  #fy is used to store the estimate label of every data point 
  for(i in 1:n){
    fy[i]<-sign(z%*%S[i,])
    if(fy[i]==0)
    {
      fy[i]<-1
    }
  }	
  fy
  
}





#############################################
################### PART III perceptrain(S,y)
#############################################


perceptrain<-function(S,y){
  
  Z_history<-matrix(, nrow<-0, ncol<-d+1)
  plot(S[,2]~S[,1], col=(y+2), pch=(y+2))
  
  
  for(k in 1: 10000){
    
    

###### first calculate the gradient of the cost function  
    
    #create a vector grad to store the sum of every single value used to calculate the gradient
    grad<-0
    
    for(i in 1:n)
    {
      if(y[i]==classify(S,z)[i]){
        grad<-grad+0
      }
      
      else{
        grad<-grad+sign(z%*%S[i,])*S[i,]
      }
    }
    
    
    
    z<-z-(1/k)*grad
    
    ##########
    #this space is left blank for adding the plot function for the each of the history of z.
    
    
    abline(a=-z[3]/z[2],b=-z[1]/z[2])
    
    
    
    ##########
    
    
    Z_history<-rbind(Z_history, z)
    
    #test if the cost function equals 0 or not, if so, terminate the function.
    
    if (sum(abs(y-classify(S,z)))==0){break}
    
  }
  
  
  return(list(z=z, Z_history=Z_history))
  
  
}


##############################################################
################### PART IV the implementation of the functios
##############################################################


n<-100
d<-2

z<-c(1,1,1)

fakedata<-fakedata(z, 100)

S<-fakedata$S
y<-fakedata$y


z<-runif(3,0,2)

result<-perceptrain(S,y)


z<-c(1,1,1)


#########################################
#########################################
#########################################
#Why do we need to run the fakedata again before using it for the second time?
#No idea......
#########################################
#########################################
#########################################


#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){
  
  if(! require(MASS))
  {
    install.packages("MASS")
  }
  if(! require(mvtnorm))
  {
    install.packages("mvtnorm")
  }
  
  require(MASS)
  require(mvtnorm)
  
  # obtain dimension
  d <- length(w)-1
  
  # compute the offset vector and a Basis consisting of w and its nullspace
  offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
  Basis <- cbind(Null(w[1:d]), w[1:d])	 
  
  # Create samples, correct for offset, and extend
  # rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
  S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
  S <- S + matrix(rep(offset,n),n,d,byrow=T)
  S <- cbind(S,1)
  
  # compute the class assignments
  y <- as.vector(sign(S %*% w))
  
  # add corrective factors to points that lie on the hyperplane.
  S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
  y = as.vector(sign(S %*% w))
  return(list(S=S, y=y))
  
} # end function fakedata

########################################################
########################################################
########################################################


fakedata.test<-fakedata(z, 50)
S.test<-fakedata.test$S
y.test<-fakedata.test$y

#test the correctness of the classifier
classify(S.test,z)-y.test

plot(S.test[,2]~S.test[,1], col=ifelse(y.test==1,"red","black"),pch=(y.test+2))
#It's better that we could also change the shape of different dots...


abline(a=-z[3]/z[2],b=-z[1]/z[2])

