train<-function(X,w,y){
  #par function gives a vector (j, theta, m), which is the optimal parameters for the Xwy giveN
  n<-length(y)
  dim<-length(X[1,])
  #use the par to store the the (j, theta, m) 
  #which specify the decision stump that we get in every iteration...
  pars<-c(0,0,0)
  
  #use the Xframe "final" to store the optimal value,..
  #the orientation of the classier, and so on in every axis j
  final<-data.frame(j=c(1:dim),cum=rep(0,dim),theta=rep(0,dim),m=rep(0,dim))
  
 

    
    for (j in 1:dim) {
      
      x<-rep(0,n)
      
      for (g in 1:n){
        x[g]<- min(X[,j])+g*(max(X[,j])-min(X[,j]))/(n+1)
      }

      #use Xframe to help calculate the sum of the first several 
      #which is the method introduce bt the TA (which I still think is 
      #not the way to find the parameters which generates the least mieclassify error...)
      Xframe<-data.frame(x=x, y=y, product=rep(0,n), m=rep(0,n))
      
      for (i in 1:n) {
        
        #mframe temporarily stores the value for both m=+1and-1, which is used to find the best m under theta
        mframe<-data.frame(indicator=c(0,0),m=c(-1,1))
        #the indicator is the error when we select theta=x[i] and m
        mframe$indicator[1]<-sum(w*(1*(y!=-1*sign(X[,j]-x[i]))))/sum(w)
        mframe$indicator[2]<-sum(w*(1*(y!=sign(X[,j]-x[i]))))/sum(w)
        
        #m=-1: regard the right hand side of the line is negative, whereas the left hand side is positive
        #m=+1: regard the left hand side of the line is negative, whereas the right hand side is positive
        
        #Xframe gives the best theta for this j
        Xframe$product[i]<-min(mframe$indicator)
        Xframe$m[i]<-mframe$m[which.min(mframe$indicator)]
      
      }
      
      #final is a Xframe under b, which stores the best theta for every j
      final$theta[j]<-Xframe$x[which.min(Xframe$product)]
      final$m[j]<-Xframe$m[which.min(Xframe$product)]
      final$cum[j]<-min(Xframe$product)
      
    }
    
    pars[1]<-final$j[which.min(abs(final$cum))]
    pars[2]<-final$theta[which.min(abs(final$cum))]
    pars[3]<-final$m[which.min(abs(final$cum))]
    
    return(pars)

    }
