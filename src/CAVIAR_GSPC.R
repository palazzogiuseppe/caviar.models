library(readxl)
library(quantreg)
library(tseries)



###CAVIAR-S$P500, tau = 0.05
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
GSPC=mydata$`S&P500 (GSPC)`
tau = 0.05
ret_test_GSPC = mydata[1012:(nrow(mydata)), c(1,7)]
ret_train_GSPC = mydata[1:(nrow(mydata)*0.75), c(1,7)]
Time = ret_train_GSPC$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
GSPC = unlist(ret_train_GSPC$`S&P500 (GSPC)`)
ret_S = coredata(GSPC)
n.start = 10
ret_test_GSPC = unlist(ret_test_GSPC[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_S = est.CAViaR(ret = ret_S, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_S = est.CAViaR(ret = ret_S, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_S = est.CAViaR(ret = ret_S, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_S = est.CAViaR(ret = ret_S, tau = tau, model = "AS", n.start = n.start) 
fit_IG_S = est.CAViaR(ret = ret_S, tau = tau, model = "IG", n.start = n.start)
fit_GJR_S =  est.CAViaR(ret = ret_S, tau = tau, model = "GJR", n.start = n.start)
fit_SAVu_S = est.CAViaR(ret = ret_S, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_S = est.CAViaR(ret = ret_S, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_S = est.CAViaR(ret = ret_S, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_S = est.CAViaR(ret = ret_S, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_GSPC = CAViaR.fun(beta = fit_AD_10_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 800 precedenti
forecast_AD_1e10_GSPC = CAViaR.fun(beta = fit_AD_1e10_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AD", G = 1e10)
forecast_SAV_GSPC = CAViaR.fun(beta = fit_SAV_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="SAV", G = 10)
forecast_AS_GSPC = CAViar.fun(beta = fit_AS_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AS", G = 10)
forecast_IG_GSPC = CAViaR.fun(beta = fit_IG_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="IG", G = 10)
forecast_GJR_GSPC = CAViaR.fun(beta = fit_GJR_S[["pars"]], u=u, ret=tet_, tau = tau, model ="GJR", G = 10)
forecast_SAVu_GSPC = CAViaR.fun(beta = fit_SAVu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="SAVu", G = 10)
forecast_IGu_GSPC = CAViaR.fun(beta = fit_IGu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="IGu", G = 10)
forecast_ASu_GSPC = CAViaR.fun(beta = fit_ASu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="ASu", G = 10)
forecast_GJRu_GSPC = CAViaR.fun(beta = fit_GJRu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="GJRu", G = 10)


forecast_AD_10_GSPC = as.vector(forecast_AD_10_GSPC)
forecast_AD_1e10_GSPC = as.vector(forecast_AD_1e10_GSPC)
forecast_SAV_GSPC = as.vector(forecast_SAV_GSPC)
forecast_AS_GSPC = as.vector(forecast_AS_GSPC)
forecast_IG_GSPC = as.vector(forecast_IG_GSPC)
forecast_GJR_GSPC = as.vector(forecast_GJR_GSPC)
forecast_SAVu_GSPC = as.vector(forecast_SAVu_GSPC)
forecast_IGu_GSPC = as.vector(forecast_IGu_GSPC)
forecast_ASu_GSPC = as.vector(forecast_ASu_GSPC)
forecast_GJRu_GSPC = as.vector(forecast_GJRu_GSPC)


#queste somme devono fare 0
sum(is.na(ret_test_GSPC))
sum(is.na(forecast_AD_10_GSPC))
sum(is.infinite(ret_test_GSPC))
sum(is.infinite((forecast_AD_10_GSPC)))

library(GAS) #non sono uguali le lunghezze
Backtest_GSPC_AD_10 = BacktestVaR(data =ret_test_GSPC, VaR = forecast_AD_10_GSPC, alpha = 0.05)
Backtest_GSPC_AD_1e10 = BacktestVaR(ret_test_GSPC, forecast_AD_1e10_GSPC, 0.05)
Backtest_GSPC_SAV = BacktestVaR(ret_test_GSPC, forecast_SAV_GSPC, 0.05)
Backtest_GSPC_AS = BacktestVaR(ret_test_GSPC, forecast_AS_GSPC, 0.05)
Backtest_GSPC_IG = BacktestVaR(ret_test_GSPC, forecast_IG_GSPC, 0.05)
Backtest_GSPC_GJR = BacktestVaR(ret_test_GSPC, forecast_GJR_GSPC, 0.05)
Backtest_GSPC_SAVu = BacktestVaR(ret_test_GSPC, forecast_SAVu_GSPC, 0.05)
Backtest_GSPC_IGu = BacktestVaR(ret_test_GSPC, forecast_IGu_GSPC, 0.05)
Backtest_GSPC_ASu = BacktestVaR(ret_test_GSPC, forecast_ASu_GSPC, 0.05)
Backtest_GSPC_GJRu = BacktestVaR(ret_test_GSPC, forecast_GJRu_GSPC, 0.05)

###CAVIAR-S$P500, tau = 0.01
rm(list=ls())
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
GSPC=mydata$`S&P500 (GSPC)`
tau = 0.01
ret_test_GSPC = mydata[1012:(nrow(mydata)), c(1,7)]
ret_train_GSPC = mydata[1:(nrow(mydata)*0.75), c(1,7)]
Time = ret_train_GSPC$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
GSPC = unlist(ret_train_GSPC$`S&P500 (GSPC)`)
ret_S = coredata(GSPC)
n.start = 10
ret_test_GSPC = unlist(ret_test_GSPC[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_S = est.CAViaR(ret = ret_S, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_S = est.CAViaR(ret = ret_S, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_S = est.CAViaR(ret = ret_S, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_S = est.CAViaR(ret = ret_S, tau = tau, model = "AS", n.start = n.start) 
fit_IG_S = est.CAViaR(ret = ret_S, tau = tau, model = "IG", n.start = n.start)
fit_GJR_S =  est.CAViaR(ret = ret_S, tau = tau, model = "GJR", n.start = n.start)
fit_SAVu_S = est.CAViaR(ret = ret_S, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_S = est.CAViaR(ret = ret_S, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_S = est.CAViaR(ret = ret_S, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_S = est.CAViaR(ret = ret_S, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_GSPC = CAViaR.fun(beta = fit_AD_10_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 800 precedenti
forecast_AD_1e10_GSPC = CAViaR.fun(beta = fit_AD_1e10_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AD", G = 1e10)
forecast_SAV_GSPC = CAViaR.fun(beta = fit_SAV_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="SAV", G = 10)
forecast_AS_GSPC = CAViar.fun(beta = fit_AS_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="AS", G = 10)
forecast_IG_GSPC = CAViaR.fun(beta = fit_IG_S[["pars"]], u=u, ret=ret_test_GSPC, tau = tau, model ="IG", G = 10)
forecast_GJR_GSPC = CAViaR.fun(beta = fit_GJR_S[["pars"]], u=u, ret=tet_, tau = tau, model ="GJR", G = 10)
forecast_SAVu_GSPC = CAViaR.fun(beta = fit_SAVu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="SAVu", G = 10)
forecast_IGu_GSPC = CAViaR.fun(beta = fit_IGu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="IGu", G = 10)
forecast_ASu_GSPC = CAViaR.fun(beta = fit_ASu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="ASu", G = 10)
forecast_GJRu_GSPC = CAViaR.fun(beta = fit_GJRu_S[["pars"]], u=u, ret=BTC, tau = tau, model ="GJRu", G = 10)


forecast_AD_10_GSPC = as.vector(forecast_AD_10_GSPC)
forecast_AD_1e10_GSPC = as.vector(forecast_AD_1e10_GSPC)
forecast_SAV_GSPC = as.vector(forecast_SAV_GSPC)
forecast_AS_GSPC = as.vector(forecast_AS_GSPC)
forecast_IG_GSPC = as.vector(forecast_IG_GSPC)
forecast_GJR_GSPC = as.vector(forecast_GJR_GSPC)
forecast_SAVu_GSPC = as.vector(forecast_SAVu_GSPC)
forecast_IGu_GSPC = as.vector(forecast_IGu_GSPC)
forecast_ASu_GSPC = as.vector(forecast_ASu_GSPC)
forecast_GJRu_GSPC = as.vector(forecast_GJRu_GSPC)


#queste somme devono fare 0
sum(is.na(ret_test_GSPC))
sum(is.na(forecast_AD_10_GSPC))
sum(is.infinite(ret_test_GSPC))
sum(is.infinite((forecast_AD_10_GSPC)))

library(GAS) #non sono uguali le lunghezze
Backtest_GSPC_AD_10 = BacktestVaR(data =ret_test_GSPC, VaR = forecast_AD_10_GSPC, alpha = 0.01)
Backtest_GSPC_AD_1e10 = BacktestVaR(ret_test_GSPC, forecast_AD_1e10_GSPC, 0.01)
Backtest_GSPC_SAV = BacktestVaR(ret_test_GSPC, forecast_SAV_GSPC, 0.01)
Backtest_GSPC_AS = BacktestVaR(ret_test_GSPC, forecast_AS_GSPC, 0.01)
Backtest_GSPC_IG = BacktestVaR(ret_test_GSPC, forecast_IG_GSPC, 0.01)
Backtest_GSPC_GJR = BacktestVaR(ret_test_GSPC, forecast_GJR_GSPC, 0.01)
Backtest_GSPC_SAVu = BacktestVaR(ret_test_GSPC, forecast_SAVu_GSPC, 0.01)
Backtest_GSPC_IGu = BacktestVaR(ret_test_GSPC, forecast_IGu_GSPC, 0.01)
Backtest_GSPC_ASu = BacktestVaR(ret_test_GSPC, forecast_ASu_GSPC, 0.01)
Backtest_GSPC_GJRu = BacktestVaR(ret_test_GSPC, forecast_GJRu_GSPC, 0.01)
