library(readxl)
library(quantreg)
library(tseries)



###CAVIAR-BTC, tao = 0.05
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
BTC=mydata$`Bitcoin (BTC)`
tau = 0.05
ret_test_BTC = mydata[809:(nrow(mydata)), c(1,2)]
ret_train_BTC = mydata[1:808, c(1,2)]
Time = ret_train_BTC$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
BTC = unlist(ret_train_BTC$`Bitcoin (BTC)`)
ret_B = coredata(BTC)
n.start = 10
ret_test_BTC = unlist(ret_test_BTC[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_B = est.CAViaR(ret = ret_B, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_B = est.CAViaR(ret = ret_B, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_B = est.CAViaR(ret = ret_B, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_B = est.CAViaR(ret = ret_B, tau = tau, model = "AS", n.start = n.start) 
fit_IG_B = est.CAViaR(ret = ret_B, tau = tau, model = "IG", n.start = n.start)
fit_GJR_B =  est.CAViaR(ret = ret_B, tau = tau, model = "GJR", n.start = n.start)
fit_SAVu_B = est.CAViaR(ret = ret_B, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_B = est.CAViaR(ret = ret_B, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_B = est.CAViaR(ret = ret_B, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_B = est.CAViaR(ret = ret_B, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_BTC = CAViaR.fun(beta = fit_AD_10_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 1012 precedenti
forecast_AD_1e10_BTC = CAViaR.fun(beta = fit_AD_1e10_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AD", G = 1e10)
forecast_SAV_BTC = CAViaR.fun(beta = fit_SAV_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="SAV", G = 10)
forecast_AS_BTC = CAViaR.fun(beta = fit_AS_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AS", G = 10)
forecast_IG_BTC = CAViaR.fun(beta = fit_IG_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="IG", G = 10)
forecast_GJR_BTC = CAViaR.fun(beta = fit_GJR_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="GJR", G = 10)
forecast_SAVu_BTC = CAViaR.fun(beta = fit_SAVu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="SAVu", G = 10)
forecast_IGu_BTC = CAViaR.fun(beta = fit_IGu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="IGu", G = 10)
forecast_ASu_BTC = CAViaR.fun(beta = fit_ASu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="ASu", G = 10)
forecast_GJRu_BTC = CAViaR.fun(beta = fit_GJRu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="GJRu", G = 10)



forecast_AD_10_BTC = as.vector(forecast_AD_10_BTC)
forecast_AD_1e10_BTC = as.vector(forecast_AD_1e10_BTC)
forecast_SAV_BTC = as.vector(forecast_SAV_BTC)
forecast_AS_BTC = as.vector(forecast_AS_BTC)
forecast_IG_BTC = as.vector(forecast_IG_BTC)
forecast_GJR_BTC = as.vector(forecast_GJR_BTC)
forecast_SAVu_BTC = as.vector(forecast_SAVu_BTC)
forecast_IGu_BTC = as.vector(forecast_IGu_BTC)
forecast_ASu_BTC = as.vector(forecast_ASu_BTC)
forecast_GJRu_BTC = as.vector(forecast_GJRu_BTC)


#queste somme devono fare 0
sum(is.na(ret_test_BTC))
sum(is.na(forecast_AD_10_BTC))
sum(is.infinite(ret_test_BTC))
sum(is.infinite((forecast_AD_10_BTC)))

class(ret_test_BTC)
ret_test_BTC = unlist(ret_test_BTC[,2])
class(ret_test_BTC)
anyNA(ret_test_BTC)
library(GAS) #non sono uguali le lunghezze
Backtest_BTC_AD_10 = BacktestVaR(data =ret_test_BTC, VaR = forecast_AD_10_BTC, alpha = 0.05)
Backtest_BTC_AD_1e10 = BacktestVaR(ret_test_BTC, forecast_AD_1e10_BTC, 0.05)
Backtest_BTC_SAV = BacktestVaR(ret_test_BTC, forecast_SAV_BTC, 0.05)
Backtest_BTC_AS = BacktestVaR(ret_test_BTC, forecast_AS_BTC, 0.05)
Backtest_BTC_IG = BacktestVaR(ret_test_BTC, forecast_IG_BTC, 0.05)
Backtest_BTC_GJR = BacktestVaR(ret_test_BTC, forecast_GJR_BTC, 0.05)
Backtest_BTC_SAVu = BacktestVaR(ret_test_BTC, forecast_SAVu_BTC, 0.05)
Backtest_BTC_IGu = BacktestVaR(ret_test_BTC, forecast_IGu_BTC, 0.05)
Backtest_BTC_ASu = BacktestVaR(ret_test_BTC, forecast_ASu_BTC, 0.05)
Backtest_BTC_GJRu = BacktestVaR(ret_test_BTC, forecast_GJRu_BTC, 0.05)

###CAVIAR-BTC, tao = 0.01
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
BTC=mydata$`Bitcoin (BTC)`
tau = 0.01
ret_test_BTC = mydata[809:(nrow(mydata)), c(1,2)]
ret_train_BTC = mydata[1:(nrow(mydata)*0.60), c(1,2)]
Time = ret_train_BTC$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
BTC = unlist(ret_train_BTC$`Bitcoin (BTC)`)
ret_B = coredata(BTC)
n.start = 10
ret_test_BTC = unlist(ret_test_BTC[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_B = est.CAViaR(ret = ret_B, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_B = est.CAViaR(ret = ret_B, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_B = est.CAViaR(ret = ret_B, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_B = est.CAViaR(ret = ret_B, tau = tau, model = "AS", n.start = n.start) 
fit_IG_B = est.CAViaR(ret = ret_B, tau = tau, model = "IG", n.start = n.start)
fit_GJR_B =  est.CAViaR(ret = ret_B, tau = tau, model = "GJR", n.start = n.start)
fit_SAVu_B = est.CAViaR(ret = ret_B, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_B = est.CAViaR(ret = ret_B, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_B = est.CAViaR(ret = ret_B, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_B = est.CAViaR(ret = ret_B, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_BTC = CAViaR.fun(beta = fit_AD_10_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 800 precedenti
forecast_AD_1e10_BTC = CAViaR.fun(beta = fit_AD_1e10_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AD", G = 1e10)
forecast_SAV_BTC = CAViaR.fun(beta = fit_SAV_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="SAV", G = 10)
forecast_AS_BTC = CAViaR.fun(beta = fit_AS_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="AS", G = 10)
forecast_IG_BTC = CAViaR.fun(beta = fit_IG_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="IG", G = 10)
forecast_GJR_BTC = CAViaR.fun(beta = fit_GJR_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="GJR", G = 10)
forecast_SAVu_BTC = CAViaR.fun(beta = fit_SAVu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="SAVu", G = 10)
forecast_IGu_BTC = CAViaR.fun(beta = fit_IGu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="IGu", G = 10)
forecast_ASu_BTC = CAViaR.fun(beta = fit_ASu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="ASu", G = 10)
forecast_GJRu_BTC = CAViaR.fun(beta = fit_GJRu_B[["pars"]], u=u, ret=ret_test_BTC, tau = tau, model ="GJRu", G = 10)



forecast_AD_10_BTC = as.vector(forecast_AD_10_BTC)
forecast_AD_1e10_BTC = as.vector(forecast_AD_1e10_BTC)
forecast_SAV_BTC = as.vector(forecast_SAV_BTC)
forecast_AS_BTC = as.vector(forecast_AS_BTC)
forecast_IG_BTC = as.vector(forecast_IG_BTC)
forecast_GJR_BTC = as.vector(forecast_GJR_BTC)
forecast_SAVu_BTC = as.vector(forecast_SAVu_BTC)
forecast_IGu_BTC = as.vector(forecast_IGu_BTC)
forecast_ASu_BTC = as.vector(forecast_ASu_BTC)
forecast_GJRu_BTC = as.vector(forecast_GJRu_BTC)


#queste somme devono fare 0
sum(is.na(ret_test_BTC))
sum(is.na(forecast_AD_10_BTC))
sum(is.infinite(ret_test_BTC))
sum(is.infinite((forecast_AD_10_BTC)))

library(GAS) #non sono uguali le lunghezze
Backtest_BTC_AD_10 = BacktestVaR(data =ret_test_BTC, VaR = forecast_AD_10_BTC, alpha = 0.01)
Backtest_BTC_AD_1e10 = BacktestVaR(ret_test_BTC, forecast_AD_1e10_BTC, 0.01)
Backtest_BTC_SAV = BacktestVaR(ret_test_BTC, forecast_SAV_BTC, 0.01)
Backtest_BTC_AS = BacktestVaR(ret_test_BTC, forecast_AS_BTC, 0.01)
Backtest_BTC_IG = BacktestVaR(ret_test_BTC, forecast_IG_BTC, 0.01)
Backtest_BTC_GJR = BacktestVaR(ret_test_BTC, forecast_GJR_BTC, 0.01)
Backtest_BTC_SAVu = BacktestVaR(ret_test_BTC, forecast_SAVu_BTC, 0.01)
Backtest_BTC_IGu = BacktestVaR(ret_test_BTC, forecast_IGu_BTC, 0.01)
Backtest_BTC_ASu = BacktestVaR(ret_test_BTC, forecast_ASu_BTC, 0.01)
Backtest_BTC_GJRu = BacktestVaR(ret_test_BTC, forecast_GJRu_BTC, 0.01)
