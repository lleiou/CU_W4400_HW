library(e1071)
data<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")


n<-length(y[,1])
rand<-sample(1:200, 40)

data.train<-data[-rand,]
y.train<-y[-rand,]

data.test<-data[rand,]
y.test<-y[rand,]


svm.linear<-tune(svm,data.train,y.train,ranges=list(cost=c(0.001,0.01,0.1,1,10)),kernel="linear")



test.linear<-svm(data.test,y.test,kernal="linear",cost=0.01)
predict.linear<-sign(predict(test.linear,data.test))
error.linear<-sum(abs(y.test-predict.linear))/2
rate.linear<-error.linear/40





plot(svm.linear$performances[,2]~svm.linear$performances[,1])
lines(lowess(svm.linear$performances[,2]~svm.linear$performances[,1]))



svm.RBF<-tune(svm,data.train,y.train,ranges=list(cost=c(0.001,0.01,0.1,1,10),gamma=c(0.001,0.01,0.1,1,10)))


test.RBF<-svm(data.test,y.test,gamma=0.01, cost=10)
predict.RBF<-sign(predict(test.RBF,data.test))
error.RBF<-sum(abs(y.test-predict.RBF))/2
rate.RBF<-error.RBF/40


library(scatterplot3d)
scatterplot3d(svm.RBF$performances[,1],svm.RBF$performances[,2],svm.RBF$performances[,3])


