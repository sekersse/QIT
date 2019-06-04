#Aufbauend
#nur Raum = rm
library(neuralnet)
#Datensatz verändern oder rückspeichern, manuell
da<-data
data<-da #Auf Original zurücksetzen
data<-subset(data,medv>20)
nrow(data)
#Correlationen
C<-function(x){
  cor(x,data[,14])
}
apply(data[,1:13],2,C)

#Test und Traningsdaten
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#Normierung
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]


n <- names(train_)

f <- as.formula(paste("medv ~ crim+ rm+tax+ptratio+lstat"))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T, threshold=0.005, lifesign = "minimal",rep=5)

#plot(nn)

pr.nn <- compute(nn,rep=4,test_[,c(1,6,10,11,13)])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv) #Normierung auflösen
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)     #Normierung auflösen
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
MSE.nn

#print(paste(MSE.lm,MSE.nn))

#Grafik Ausgabe 
plot(test.r,pr.nn_,col='red',main='Real vs predicted nn',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n', cex=.95)
abline(3,1,lwd=2,col=4) #3000$ Grenze
abline(-3,1,lwd=2,col=4) #3000$ Grenze
abline(0,1.1,lwd=2,col=3) #10% Grenze
abline(0,0.9,lwd=2,col=3) #10% Grenze

#Check Zielerreichung
testobjects<-length(test.r)
testobjects
#Check Ziel 1 75% < 3000
KleinerDreitausend<-sum(abs(test.r-pr.nn_)<3)
Ziel1<-KleinerDreitausend/testobjects
Ziel1

#Check Ziel 2 Anzahl der Abweichungen >10% == Null
Ziel2<-sum((abs(test.r-pr.nn_)/test.r)>0.1)
Ziel2

#Sichere Bereiche bzgl. des Preises?
ResSort<-cbind(test.r,pr.nn_, abs(test.r  -pr.nn_)/test.r  )
ResSort<-ResSort[order(ResSort[,1]),]
plot(x=ResSort[,1],y=ResSort[,3])
abline(0.1,0)
N=5
lines(filter(ResSort[,3],rep(1/N,N),sides = 1),col=4)

#Correlationen
C<-function(x){
  cor(x,data[,14])
}
apply(data[,1:13],2,C)

test#Grafik
par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
