library(neuralnet)
set.seed(7896129)

##### Ergebnis
XOR <- c(0,1,1,0)  

##### Input und Ergebnis als data.frame
xor.data <- data.frame(expand.grid(c(0,1), c(0,1)), XOR) 

##### Neuronales Netz berechnen
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=5, lifesign="minimal")

print(net.xor)

plot(net.xor, rep="best")

########################
## Netz testen
########################

res<-compute(net.xor, rep=5, xor.data[,1:2])
resrounded<-round(res$net.result[,1],0)
table(XOR, resrounded)
resroudeddf<-data.frame(resrounded)
attach(resroudeddf)
#testresult<- ifelse(net.xor$net.result[[1]]>0.5,1,0)
testresult<- ifelse(res$net.result>0.5,1,0)
#complres<-cbind(xor.data,trd)
complres<-cbind(xor.data[,3],trd)
complres
mean(complres[,1]-complres[,2])

########################
## Weitere Experimente
########################

net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=2, rep=10, lifesign="minimal")
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=1, rep=10, lifesign="minimal")
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=3, rep=10, lifesign="minimal")
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=c(2,1), rep=10, lifesign="minimal")
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=c(2,2), rep=10, lifesign="minimal")
net.xor <- neuralnet( XOR~Var1+Var2, xor.data, hidden=c(2,2), rep=100, lifesign="minimal")
