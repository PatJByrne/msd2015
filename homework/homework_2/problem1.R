require(data.table)
require(dplyr)
set.seed(20)

data <- data.table(read.table('polyfit.tsv',header = TRUE,sep = '\t',row.names=NULL))
data <- arrange(data,by = x)
randrows <- sort(sample(nrow(data),nrow(data)*.5))
randrows_compl = !(seq(nrow(data)) %in% randrows)

train_data <- data[randrows,]

test_data <- data[randrows_compl,]

test_error = numeric()
train_error = numeric()
best_fit_error = Inf
best_fit_degree = Inf
best_fit = NULL
best_prdic = NULL
degree = seq(10)
plot(data)

for(i in degree){

  fit = lm(train_data$y~poly(train_data$x,i,raw = TRUE))
  prdic = 0
  for(j in seq(i+1)){
    prdic <- prdic + fit$coefficients[j]*test_data$x^(j-1)
    
  }

  degree_error_train = sqrt(sum((fitted(fit)-train_data$y)**2)/nrow(train_data))
  degree_error_test = sqrt(sum((prdic-test_data$y)**2)/nrow(test_data) )
  
  test_error<-append(test_error,degree_error_test)
  train_error<-append(train_error,degree_error_train)

  if(tail(test_error,n=1) < best_fit_error){
    best_fit_error <- tail(test_error,n=1)
    best_fit_degree <- i
    best_fit <- fit
    best_prdic <- prdic
  }
}
i = best_fit_degree

png(filename = 'Error_v_poly_degree.png')
plot(degree,test_error,ylim = c(0,.05),ylab = 'RMS Error',xlab = 'Polynomial Degree',col = 'blue')
points(degree,train_error,col = 'red')
lines(rep(best_fit_degree,2),c(0,.1),col = 'black')
text(3,.1,pos = 4,'best fit degree: 4')
legend('topright',c('training dataset','test dataset'),col = c('red','blue'), pch = c(1,1))
dev.off()

png(filename = 'Prob_1_best_fit.png')
plot(data)
points(train_data$x,fitted(best_fit),col = 'red')
points(test_data$x,best_prdic,col = 'red')
legend('topright',c('raw data','data fitted to model'),col = c('black','red'),pch=c(1,1))
title('Best fit to data')
dev.off()

#points(test_data$x,prdic,col='blue')