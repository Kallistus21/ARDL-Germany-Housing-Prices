Sys.setenv(LANG = "en")
options(scipen=100)
library(lmtest)
library(tseries)
library(sandwich)
library(zoo) #as.yearqtr is a part of zoo package
library(dplyr)
library(urca)
library(dynlm)
library(car)
library(forecast)
library(ARDL)
data<-read.csv('database ARDL.csv')
source("function_testdf2.R")

####DATA PROCESSING#####
data$Date<-as.Date(as.yearqtr(data$Date,format='%Y-Q%q'))
data<-data%>%
  rename("GDP_per_capita"="GDP.PER.CAPITA",
         "Unemployment"="UNEMPLOYMENT.15.or.more.",
         "Inflation_rate"="inflation.rate..CPI.",
         "Interest_rate"="interest.rate...long.term.",
         "Population"="Population..in.thousands.",
         "Real_House_Prices"="real.house.prices",
         "Permits_issued"="permits.issued..index.")
#also, Population and GDP per capita need to be converted into numerical
#data$GDP.PER.CAPITA<-as.numeric(gsub(",","",data$GDP.PER.CAPITA))
#data$Population..in.thousands.<-as.numeric(gsub(",","",data$Population..in.thousands.))
#or, using dplyr
data <- data %>%
  mutate(
    GDP_per_capita= as.numeric(gsub(",", "", GDP_per_capita)),
    Population = as.numeric(gsub(",", "", Population)),
    Unemployment=Unemployment/100, #multiply inflation rate, interest rate and unemployment rate by 100 so it is actually a percentage
    Inflation_rate=Inflation_rate/100,
    Interest_rate=Interest_rate/100,
    Population=Population*1000 #multiply by 1000 as OECD stores population in thousands
  ) %>%
  relocate(Real_House_Prices, .after=Date)#move the dependent variable after date so that it is first 
#We will get "rid" of date in the "ts" function

#Convert the dataframe to a time series
data2<-ts(data=data[,-1],start=c(1994,2),frequency=4)
#take logs of all the variables
log_data2<-data2
#Due to some variables having negative values (although very close to 0), we will mitigate this by adding a small constant so that we can take logs
small_constant<-1e-2
for (i in 1:ncol(log_data2)) {
  log_data2[, i] <- log(log_data2[, i] + small_constant)
}
#GRAPHS OF LOGS
plot_logs<-list()
for(i in 1:ncol(log_data2)){
  plot(data2[,i],
       main=colnames(log_data2)[i],
       ylab=colnames(log_data2)[i],
       col="blue",
       pch=16,
       cex.axis=0.6)
  
  plot_logs[[i]]<-recordPlot()
}
names(plot_logs)<-colnames(log_data2)
plot_logs[["Real_House_Prices"]]
plot_logs[["GDP_per_capita"]]
plot_logs[["Unemployment"]]
plot_logs[["Population"]]
plot_logs[["Interest_rate"]]
plot_logs[["Permits_issued"]]
plot_logs[["Inflation_rate"]]
#Let's take the first differences of our logarithmised variables
data_diff<-ts(data=log_data2[-1,],start=c(1994,2),frequency=4)
for (i in 1:ncol(log_data2)){
  data_diff[,i]<-diff(log_data2[,i])
}
#GRAPHS OF DIFFERENTIATED TIME SERIES
plot_diff<-list()
for(i in 1:ncol(data_diff)){
  plot(data_diff[,i],
       main=colnames(data_diff)[i],
       ylab=colnames(data_diff)[i],
       col="blue",
       pch=16,
       cex.axis=0.6)
  
  plot_diff[[i]]<-recordPlot()
}
names(plot_diff)<-colnames(data_diff)
plot_diff[["Real_House_Prices"]]
plot_diff[["GDP_per_capita"]]
plot_diff[["Unemployment"]]
plot_diff[["Population"]]
plot_diff[["Interest_rate"]]
plot_diff[["Permits_issued"]]
plot_diff[["Inflation_rate"]]

######STATIONARITY TESTING######
#ADF and BG TEST Dr. Wozniaka
source("function_testdf2.R")

####REAL HOUSE PRICES (dependent variable)####
testdf2(variable = log_data2[,"Real_House_Prices"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Real_House_Prices"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
#at least 1 augmentation needed REAL HOUSE PRICES
# PHILIPS PERRON TEST
pp_test_Real_House_Prices<-ur.pp(log_data2[,1],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_Real_House_Prices)
pp_test_Real_House_Prices_diff<-ur.pp(data_diff[,1],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_Real_House_Prices_diff)
#KPSS TEST
kpss.test_house <- ur.kpss(log_data2[,1], 
                           type = c("mu")) 
summary(kpss.test_house)
kpss.test_house_diff <- ur.kpss(data_diff[,1], 
                                type = c("mu")) 
summary(kpss.test_house_diff)

###GDP PER CAPITA###
##ADF and BG TEST##
testdf2(variable = log_data2[,"GDP_per_capita"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  # 
#needs diff
testdf2(variable = data_diff[,"GDP_per_capita"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
pp_test_GDP_per_capita<-ur.pp(log_data2[,2],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_GDP_per_capita)
pp_test_GDP_per_capita_diff<-ur.pp(data_diff[,2],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_GDP_per_capita_diff)
kpss.test_GDP <- ur.kpss(log_data2[,2], 
                           type = c("mu")) 
summary(kpss.test_GDP)
kpss.test_GDP_diff <- ur.kpss(data_diff[,2], 
                                type = c("mu")) 
summary(kpss.test_GDP_diff)

#0 AUGMENTATIONS NEEDED GDP PER CAPITA
###Unemployment###
#ADF BG##
testdf2(variable = log_data2[,"Unemployment"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Unemployment"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
#needs diff p value 0.01
#PP TEST
pp_test_Unemployment<-ur.pp(log_data2[,3],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_Unemployment)
pp_test_Unemployment_diff<-ur.pp(data_diff[,3],
                                      type = "Z-tau", 
                                      model="constant")
#KPSS
summary(pp_test_Unemployment_diff)
kpss.test_Unemployment <- ur.kpss(log_data2[,3], 
                           type = c("mu")) 
summary(kpss.test_Unemployment)
kpss.test_Unemployment_diff <- ur.kpss(data_diff[,3], 
                                type = c("mu")) 
summary(kpss.test_Unemployment_diff)
#NO AUGM NEEDED FOR UNEMPLOYMENT

####Population (for diff, we need 2 augmentations to get rid of autocorr)
#ADF BG
testdf2(variable = log_data2[,"Population"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Population"],
        test.type = "nc", 
        max.augmentations = 5,
        max.order=5)  
#PPTEST
pp_test_Population<-ur.pp(log_data2[,4],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_Population)
pp_test_Population_diff<-ur.pp(data_diff[,4],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_Population_diff)
#KPSS
kpss.test_pop <- ur.kpss(log_data2[,4], 
                           type = c("mu")) 
summary(kpss.test_pop)
kpss.test_pop_diff <- ur.kpss(data_diff[,4], 
                                type = c("mu")) 
summary(kpss.test_pop_diff)
#AT LEAST 2 AUGM NEEDED POPULATION


####Interest rate###, no augmentations needed for the difference
#ADF BG
testdf2(variable = log_data2[,"Interest_rate"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Interest_rate"], 
        test.type = "nc", # test type
        max.augmentations = 5, 
        max.order=5)  
#NO AUGM NEEDED INTEREST RATE
#PP TEST
pp_test_interest<-ur.pp(log_data2[,5],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_interest)
pp_test_interest_diff<-ur.pp(data_diff[,5],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_interest_diff)
#KPSS
kpss.test_interest <- ur.kpss(log_data2[,5], 
                           type = c("mu")) 
summary(kpss.test_interest)
kpss.test_interest_diff <- ur.kpss(data_diff[,5], 
                                type = c("mu")) 
summary(kpss.test_interest_diff)

####Permits_issued###
#ADF BG
testdf2(variable = log_data2[,"Permits_issued"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Permits_issued"], 
        test.type = "nc", 
        max.augmentations = 5, 
        max.order=5)  
#PP TEST
pp_test_permits<-ur.pp(log_data2[,6],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_permits)
pp_test_permits_diff<-ur.pp(data_diff[,6],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_permits_diff)
#KPSS
kpss.test_permits<- ur.kpss(log_data2[,6], 
                           type = c("mu")) 
summary(kpss.test_permits)
kpss.test_permits_diff <- ur.kpss(data_diff[,6], 
                                type = c("mu")) 
summary(kpss.test_permits)
#NO AUGM NEEDED PERMITS ISSUED

####inflation rate###, apparently 1 augmentations needed if we want bg tests p value>0.05 
testdf2(variable = log_data2[,"Inflation_rate"], 
        test.type = "nc", # test type
        max.augmentations = 5, 
        max.order=5)  
testdf2(variable = data_diff[,"Inflation_rate"], 
        test.type = "nc", # test type
        max.augmentations = 5, 
        max.order=5)  
#PP TEST
pp_test_inflation<-ur.pp(log_data2[,7],
                                 type = "Z-tau", 
                                 model="constant")
summary(pp_test_inflation)
pp_test_inflation_diff<-ur.pp(data_diff[,7],
                                      type = "Z-tau", 
                                      model="constant")
summary(pp_test_inflation_diff)
#KPSS
kpss.test_inflation <- ur.kpss(log_data2[,7], 
                           type = c("mu")) 
summary(kpss.test_inflation)
kpss.test_inflation_diff <- ur.kpss(data_diff[,7], 
                                type = c("mu")) 
summary(kpss.test_inflation_diff)


# PHILIPS PERRON TEST
#############just a cool loop i wanted to make, ended up not being useful in the end#########################
#results_PP<-list()
#for (i in 1:ncol(log_data2)){
#  PP_test_results<-ur.pp(log_data2[,i],
#                         type = "Z-tau", 
#                         model="constant")
#  results_PP[[colnames(log_data2)[i]]]<-summary(PP_test_results)
#}
#
#post the results of each variable
# for (i in 1:length(results)){
#  print(results[i])
# }
#or
#for(i in 1:length(results_PP)){
#  result_PP<-results_PP[[i]]
#  cat("\nThe test statistic for", names(results_PP)[i],"is: ",result_PP@teststat)
#  cat("\nThe critical values",names(results_PP)[i] ,"are: ",result_PP@cval)
#  cat("\n")
#}
#result_PP[[1]]
##################################end ###########################################




#after testing we use auto_ardl function to see what our general model should look like (in terms of lags)
BIC_Comparison <- auto_ardl(
  Real_House_Prices ~ GDP_per_capita + Unemployment +
    Population + Interest_rate  +
    Permits_issued  + Inflation_rate ,
  data = data_diff,
  selection="BIC",
  max_order = c(10, 10, 10, 10, 10,10,10))
BIC_Comparison
summary(BIC_Comparison)
#the best model according to BIC criterion turned out to be -657.0305(the lowest) is:
#permits issued 1 lag, 
#population 1 lag, 
#real house prices 2 lags
#no lags for the rest of the variables

#therefore let's construct the general model
general_BIC_model <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    Unemployment+
    Population+L(Population, 1)+  
    Interest_rate +  
    Permits_issued + L(Permits_issued, 1)+  
    Inflation_rate, 
  data = data_diff,
)
summary(general_BIC_model)

####GENERAL TO SPECIFIC PROCEDURE
#REMOVE UNEMPLOYMENT (highest p value of insignificant variables)
step1 <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    Population+L(Population, 1)+  
    Interest_rate +  
    Permits_issued + L(Permits_issued, 1)+  
    Inflation_rate,  
  data = data_diff,
)
summary(step1)
#remove population but before we do that we have to do th walt test of joint significance:
linearHypothesis(general_BIC_model,c("Unemployment=0","Population=0")) #to test joint significance
#p value higher than0.05 => we can remove it
#step 2 without population and unemployment
step2 <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    L(Population, 1)+  
    Interest_rate +  
    Permits_issued + L(Permits_issued, 1)+  
    Inflation_rate,  # Lags of Inflation_rate
  data = data_diff,
)
summary(step2)
#L(Population, 1) 0.46124 has the highest, again wald test to see if we can remove it
linearHypothesis(general_BIC_model,c("Unemployment=0","Population=0","L(Population, 1)=0")) #to test joint significance
#0.8567 p value, we can remove first lag of population
step3 <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    Interest_rate +  
    Permits_issued + L(Permits_issued, 1)+
    Inflation_rate,
  data = data_diff,
)
summary(step3)
#Inflation_rate  0.255430  
#wald test
linearHypothesis(general_BIC_model,c("Unemployment=0","Population=0","L(Population, 1)=0","Inflation_rate=0")) #to test joint significance
#0.7268 wald test = remove inflation rate

#Step 4
step4 <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    Interest_rate +  
    Permits_issued+L(Permits_issued, 1),
  data = data_diff,
)
summary(step4)
#Permits_issued 0.073327
#slightly above 0.05 but enough for us. Wald test.
linearHypothesis(general_BIC_model,c("Unemployment=0","Population=0","L(Population, 1)=0","Inflation_rate=0","Permits_issued=0")) #to test joint significance
#0.3912 p value of wald test => remove it
#Step 5
step5 <- dynlm(
  Real_House_Prices ~ 
    L(Real_House_Prices, 1) + L(Real_House_Prices, 2) +  
    GDP_per_capita +  
    Interest_rate +  
+L(Permits_issued, 1),
  data = data_diff,
)
summary(step5)
#everything is significant - this is our final model
Final_ARDL<-step5
summary(Final_ARDL)
#LT MULTIS
#i actually calculated this by hand using the formula because i had problems with a function

#Reset test
reset_test <- resettest(Final_ARDL, power = 2:3)
print(reset_test)

#heteroscedasticity - Breusch Pagan test
bp_test<-bptest(Final_ARDL)
print(bp_test)

#Jarque bera test
Jarque_Bera<-jarque.bera.test(residuals(Final_ARDL))
Jarque_Bera
# ##### ARIMA MODEL#####

arima.best.AIC <- auto.arima(data_diff[,"Real_House_Prices"],
                             d = 1,             
                             max.p = 10,        
                             max.q = 10,        
                             max.order = 11,    
                             start.p = 1,       
                             start.q = 1,       
                             ic = "aic",        
                             stepwise = FALSE,  
                             allowdrift = TRUE, 
                             trace = TRUE,
                             seasonal=FALSE)     # we dont have seasonality so we set it to FALSE 

#Best model: ARIMA(3,1,4) using AIC criterion
coeftest(arima.best.AIC)
#let's construct our arima model with the parameters we got through AIC criterion 
arima_model <- arima(data_diff[,"Real_House_Prices"], order=c(3,1,4))
#see BIC and AIC 
AIC(arima.best.AIC)
BIC(arima.best.AIC)
#Ljung Box's test of autocorrelation (specifically for time series)
Box.test(resid(arima_model),
         type = "Ljung-Box", lag = 10)

#let's compare the models to see which one has a better performance
AIC(arima_model)
BIC(arima_model)
AIC(Final_ARDL)
BIC(Final_ARDL)
summary(Final_ARDL)
summary(arima_model) # arima doesn't have R squared because its a prediction


#przed dodaniem na gita:
#1. dodac functiondf2 na git
#2. dodac .csv na git
#3. moze opisac jakos inaczej komentarze? Albo opisac readme zamiast w komentarzach?
#4. dodać R squared i adjusted r squared oprocz aic i bic
#5. poprawic interpretacje lagów
# https://www.youtube.com/watch?v=VtijB_eFp5o 51:25

#ok a wiec tak z tego co rozumiem: 
  
#Long-Term Multiplier for L(Real_House_Prices, 2) ≈0.9638 ZLE, jak mopze byc lt multi dla 1 laga?
Long-Term Multiplier for L(Real_House_Prices, 1) ≈0.4930# to samo co wyzej  
Long-Term Multiplier for GDP_per_capita: ≈0.4405 # to niby dobrze le to jest po prostu ST=LT bo nie ma lagow
Long-Term Multiplier for Interest_rate: ≈−0.0775\approx -0.0775≈−0.0775# to samo
Long-Term Multiplier for L(Permits_issued, 1): ≈0.0715  #zle, nie ma LTM dla laga
  
#6. na gita wstawic long term multiplier, bo jest w dokumencie ale nie tutaj