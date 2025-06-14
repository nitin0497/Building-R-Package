data=read.csv("iris_csv.csv")
data=data[data$class=="Iris-setosa" | data$class=="Iris-virginica",]
data$class=ifelse(data$class==c("Iris-setosa"),1,0)
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
weightInitialization(x,y)

sigmoid=function(result){
  final_result = 1/(1+exp(-result))
  return(final_result)
}

model_optimize=function(init, x, y){
  m = nrow(x)
  w=init[2:length(init)]
  b=init[1]
  #Prediction
  final_result = sigmoid(w%*%t(x)+b)
  Y_T = t(y)
  cost = (-1/m)*(sum((log(final_result)*Y_T) + (log(1-final_result)*(1-Y_T))))
  #Gradient calculation
  dw = (1/m)*((t(x)%*%t((final_result-t(y)))))
  db = (1/m)*(sum(final_result-t(y)))
  
  grads = c(db,dw)
  
  return(c(cost,grads))
}

model_predict=function(init, x, y, learning_rate=0.05, no_iterations=1500){
  costs = c()
  w=init[2:length(init)]
  b=init[1]
  
  for(i in 1:no_iterations){
    optim = model_optimize(init,x,y)
    grads = c(optim[2:length(optim)])
    cost = optim[1]
    
    dw = c(grads[2:length(grads)])
    db = grads[1]
    #weight update
    w = w - (dw*learning_rate)
    b = b - (db*learning_rate)
    init=c(b,w)
    costs=c(costs,cost)
  }

  #final parameters
  
  coeff = c(b,w)
  gradient = c(db,dw)
  
  return(list(coeff,gradient,costs))
}

predictor=function(final_pred, m, cut_off=0.5){
  y_pred = rep(0,m)
  for(i in 1:length(final_pred)){
    if(final_pred[i] > cut_off)
      y_pred[i] = 1
    else
      y_pred[i] = 0
  }
  return(y_pred)
}

init= weightInitialization(x,y)
#Gradient Descent
predict=model_predict(init, x, y)

coeff=unlist(predict[1])
names(coeff)=c("intercept",colnames(x))
gradient=unlist(predict[2])
costs = unlist(predict[3])
w=coeff[2:length(init)]
b=coeff[1]
final_train_pred = sigmoid(w%*%t(x)+b)
final_test_pred = sigmoid(w%*%t(xtest)+b)

m_tr =  nrow(x)
m_ts =  nrow(xtest)

y_tr_pred = predictor(final_train_pred, m_tr,cut_off=0.5)
y_ts_pred = predictor(final_test_pred, m_ts)

coeff
min(costs)
plot(c(1:length(costs)),costs,type="l")
table(ytest)
table(y_ts_pred)
df=data.frame(y_ts_pred,ytest)

#install.packages("caret")
#library(caret)
confusion_matrix=confusionMatrix(as.factor(df$ytest), as.factor(df$y_ts_pred))
confusion_matrix


ggplot(df, aes(x=ytest, y=final_test_pred)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))


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
    predict=model_predict(init, x, y)
    
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

CI=bootstrapCI(data,0.15,n=100)
CI
#library(boot)
x1<-rnorm(50,2,0.25)
b1<-boot(x1,function(u,i) mean(u[i]),R=1000)
CI=boot.ci(b1,type=c("norm"))
CI$norm
b1
?boot
n=20


CI
g
