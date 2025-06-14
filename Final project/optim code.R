library(caret)
library(boot)
library(mlbench)
#data("PimaIndiansDiabetes")
data=read.csv("winequality-red.csv")
#data=PimaIndiansDiabetes
#data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
#data$class=ifelse(data$class=="Iris-setosa",1,0)
data$quality=ifelse(data$quality<6,1,0)
n_features=ncol(data)

dt = sort(sample(nrow(data), nrow(data)*.7))
train<-data[dt,]
test<-data[-dt,]
x=as.matrix(train[,1:n_features-1])
y=train[,n_features]
xtest=test[,1:n_features-1]
ytest=test[,n_features]

weightInitialization=function(x,y){
  init_weights=solve(t(x)%*%x)%*%t(x)%*%y
  init_intercept = mean(y) - (colMeans(x)%*%init_weights)
  init=c(init_intercept,init_weights)
  names(init)=c("intercept",colnames(x))
  return(init)
}
init=weightInitialization(x,y)

sigmoid=function(result){
  final_result = 1/(1+exp(-result))
  return(final_result)
}

cost.glm <- function(theta,X) {
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
}

x1 <- cbind(1, x)
predict=optim(par=init, fn = cost.glm, method='CG',
              X=x1)

coeff=unlist(predict[1])
names(coeff)=c("intercept",colnames(x))
cost=unlist(predict[2])

w=coeff[2:length(coeff)]
b=coeff[1]
final_train_pred = sigmoid(w%*%t(x)+b)
final_test_pred = sigmoid(w%*%t(xtest)+b)

m_tr =  nrow(x)
m_ts =  nrow(xtest)


predictor=function(final_pred, m, cutoff=0.5){
  y_pred = rep(0,m)
  for(i in 1:length(final_pred)){
    if(final_pred[i] > cutoff)
      y_pred[i] = 1
    else
      y_pred[i] = 0
  }
  return(y_pred)
}


y_tr_pred = predictor(final_train_pred, m_tr,cutoff=0.5)
y_ts_pred = predictor(final_test_pred, m_ts)


#plot(c(1:length(costs)),costs,type="l")
table(ytest)
table(y_ts_pred)
newdata <- data.frame(prob=seq(min(ytest), max(ytest),length=length(ytest)))
newdata$class=predictor(newdata$prob, m_ts)

plot(class~prob, data=newdata, col="steelblue")
lines(class~prob, newdata, lwd=2)

ggplot(newdata, aes(x=prob, y=class)) +
  geom_point(alpha=.5) +
  geom_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
#install.packages("caret")



bootstrapCI=function(data,alpha,n=20){
  n_features=ncol(data)
  beta=matrix(nrow=n,ncol=n_features)
  for(i in 1:n){
    n_features=ncol(data)
    dt = sort(sample(nrow(data), nrow(data)*.7))
    train<-data[dt,]
    x=as.matrix(train[,1:n_features-1])
    y=train[,n_features]

    init= weightInitialization(x,y)
    x1 <- cbind(1, x)
    predict=optim(par=init, fn = cost.glm, method='CG',
                  X=x1)

    coeff=unlist(predict[1])
    beta[i,]=coeff
  }
  CI=matrix(nrow=n_features,ncol=3)
  for(i in 1:n_features){
    b1<-boot(beta[i,],function(u,i) mean(u[i]),R=n)
    g=boot.ci(b1,type=c("norm","basic","perc"),conf=1-alpha)$norm
    CI[i,]=as.vector(g)
  }
  rownames(CI)=c("intercept",colnames(x))
  colnames(CI)=c("Level","Lower","Upper")
  return(CI)
}

CI=bootstrapCI(data,0.1)
CI

metricplot=function(y_prob,ytest){
  m_tr =  nrow(y_prob)
  metrics=matrix(nrow=9,ncol=7)
  colnames(metrics)=c("Prevalence","Accuracy","Sensitivity","Specificity","False Discovery Rate","Diagnostic Odds Ratio","cutoff")
  for(i in 1:9){
    y_ts_pred = predictor(y_prob, m_tr,cutoff=i/10)
    confusion_matrix=confusionMatrix(as.factor(ytest), as.factor(y_ts_pred))
    metrics[i,1]=as.vector(confusion_matrix$byClass[8])
    metrics[i,2]=as.vector(confusion_matrix$overall[1])
    metrics[i,3]=as.vector(confusion_matrix$byClass[1])
    metrics[i,4]=as.vector(confusion_matrix$byClass[2])
    metrics[i,5]=1-as.vector(confusion_matrix$byClass[3])
    metrics[i,6]=metrics[i,2]*metrics[i,3]/((1-metrics[i,2])*(1-metrics[i,3]))
    metrics[i,7]=i/10
  }
  return(metrics)
}
metric_data=metricplot(final_test_pred,ytest)
i
