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

