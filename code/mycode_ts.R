library(datasets)
library(outliers)
library(astsa)
library(forecast)
library(readxl)

dataset= read_excel('data.xlsx')

#Extracting tourist data from the dataset
tdata = ts(dataset$TouristNumber, frequency=12, start=c(1992,1))
plot(tdata)

#Test for correlation 
Box.test(tdata)

#Plotting of differenced data
data_notrend= diff(tdata)
plot(data_notrend, main= ' Differenced Data(No Trend)')

#Checking ACF and PACF
par(mfrow=c(2,1))
acf(data_notrend, main='acf')
pacf(data_notrend, main='pacf')

#Plotting of deseasoned data
data_noseason= diff(diff(tdata), 12)
plot(data_noseason, main= ' Differenced Data(No Season)')

#Checking ACF and PACF again
par(mfrow=c(2,1))
acf(data_noseason, main='acf')
pacf(data_noseason, main='pacf')

#This implies AR order may be 0,1,2,3,4,5,6,7  and MA order 1 or 2

#Q-Stats
Box.test(data_noseason)


#Checking Various Models
library(astsa)

d=1
DD=1

per=12

for(p in 1:3){
  for(q in 1:3){
    for(i in 1:3){
      for(j in 1:3){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(tdata), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

#Auto Arima Check
auto.arima(tdata)

#Fitting best Model from above based on lowest AIC, SSE and high P-value
#Here fitting (1,1,1,0,1,1) period=12
modelf= arima(x=tdata, order=c(1,1,1), seasonal = list(order=c(0,1,1), period= 12))
summary(modelf)

#Forecasting
values= forecast(object=tdata, model= modelf, h=24)
df= as.data.frame(values$mean)

plot(values)




