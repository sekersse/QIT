library(neuralnet)
set.seed(7896129)

########################
### Ergebnisvektor
########################
AND <- c(0,0,0,1)  

##### Input und Ergebnis als data.frame
data.frame(Var1=c(0,1,0,1), Var2=c(0,0,0,1), AND)
and.data <- data.frame(expand.grid(c(0,1), c(0,1)), AND) 
and.data

##### Neuronales Netz berechnen
## Zielfunktion festlegen
f<- AND~Var1+Var2

## Netz berechnen hiddenlayers=0, Wiederholungen eine, 
net.and <- neuralnet( f, and.data, hidden=0, rep=1, lifesign="minimal")

#Textausgabe des KNN
print(net.and)

#plot den KNN
plot(net.and, rep="best")

########################
## Netz testen
########################

res<-compute(net.and, rep=1, and.data[,1:2])
res
resrounded<-round(res$net.result[,1],0)
resrounded

#AND=        (0,0,0,1)
#resrounded= (0,0,0,1)
table(AND, resrounded)

########################
## Weitere Experimente
########################

net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=2, rep=10, lifesign="minimal")
net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=1, rep=10, lifesign="minimal")
net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=3, rep=10, lifesign="minimal")
net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=c(2,1), rep=10, lifesign="minimal")
net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=c(2,2), rep=10, lifesign="minimal")
net.and <- neuralnet( AND~Var1+Var2, and.data, hidden=c(2,2), rep=100, lifesign="minimal")
