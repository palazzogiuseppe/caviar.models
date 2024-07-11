library(readxl)
library(quantreg)
library(tseries)



###CAVIAR-GOLD, tao = 0.05
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
GOLD=mydata$Gold
tau = 0.05
ret_test_GOLD = mydata[809:(nrow(mydata)), c(1,11)]
ret_train_GOLD = mydata[1:(nrow(mydata)*0.60), c(1,11)]
Time = ret_train_GOLD$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
GOLD = unlist(ret_train_GOLD$Gold)
ret_G = coredata(GOLD)
n.start = 10
ret_test_GOLD = unlist(ret_test_GOLD[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_G = est.CAViaR(ret = ret_G, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_G = est.CAViaR(ret = ret_G, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_G = est.CAViaR(ret = ret_G, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_G = est.CAViaR(ret = ret_G, tau = tau, model = "AS", n.start = n.start) 
fit_IG_G = est.CAViaR(ret = ret_G, tau = tau, model = "IG", n.start = n.start)
fit_GJR_G =  est.CAViaR(ret = ret_G, tau = tau, model = "GJR", n.start = n.start)
fit_GAVu_G = est.CAViaR(ret = ret_G, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_G = est.CAViaR(ret = ret_G, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_G = est.CAViaR(ret = ret_G, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_G = est.CAViaR(ret = ret_G, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_GOLD = CAViaR.fun(beta = fit_AD_10_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 800 precedenti
forecast_AD_1e10_GOLD = CAViaR.fun(beta = fit_AD_1e10_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AD", G = 1e10)
forecast_SAV_GOLD = CAViaR.fun(beta = fit_SAV_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="SAV", G = 10)
forecast_AS_GOLD = CAViaR.fun(beta = fit_AS_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AS", G = 10)
forecast_IG_GOLD = CAViaR.fun(beta = fit_IG_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="IG", G = 10)
forecast_GJR_GOLD = CAViaR.fun(beta = fit_GJR_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="GJR", G = 10)
forecast_SAVu_GOLD = CAViaR.fun(beta = fit_SAVu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="SAVu", G = 10)
forecast_IGu_GOLD = CAViaR.fun(beta = fit_IGu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="IGu", G = 10)
forecast_ASu_GOLD = CAViaR.fun(beta = fit_ASu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="ASu", G = 10)
forecast_GJRu_GOLD = CAViaR.fun(beta = fit_GJRu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="GJRu", G = 10)



forecast_AD_10_GOLD = as.vector(forecast_AD_10_GOLD)
forecast_AD_1e10_GOLD = as.vector(forecast_AD_1e10_GOLD)
forecast_SAV_GOLD = as.vector(forecast_SAV_GOLD)
forecast_AS_GOLD = as.vector(forecast_AS_GOLD)
forecast_IG_GOLD = as.vector(forecast_IG_GOLD)
forecast_GJR_GOLD = as.vector(forecast_GJR_GOLD)
forecast_SAVu_GOLD = as.vector(forecast_SAVu_GOLD)
forecast_IGu_GOLD = as.vector(forecast_IGu_GOLD)
forecast_ASu_GOLD = as.vector(forecast_ASu_GOLD)
forecast_GJRu_GOLD = as.vector(forecast_GJRu_GOLD)


#queste somme devono fare 0
sum(is.na(ret_test_GOLD))
sum(is.na(forecast_AD_10_GOLD))
sum(is.infinite(ret_test_GOLD))
sum(is.infinite((forecast_AD_10_GOLD)))

library(GAS) #non sono uguali le lunghezze
Backtest_GOLD_AD_10 = BacktestVaR(data =ret_test_GOLD, VaR = forecast_AD_10_GOLD, alpha = 0.05)
Backtest_GOLD_AD_1e10 = BacktestVaR(ret_test_GOLD, forecast_AD_1e10_GOLD, 0.05)
Backtest_GOLD_SAV = BacktestVaR(ret_test_GOLD, forecast_SAV_GOLD, 0.05)
Backtest_GOLD_AS = BacktestVaR(ret_test_GOLD, forecast_AS_GOLD, 0.05)
Backtest_GOLD_IG = BacktestVaR(ret_test_GOLD, forecast_IG_GOLD, 0.05)
Backtest_GOLD_GJR = BacktestVaR(ret_test_GOLD, forecast_GJR_GOLD, 0.05)
Backtest_GOLD_SAVu = BacktestVaR(ret_test_GOLD, forecast_SAVu_GOLD, 0.05)
Backtest_GOLD_IGu = BacktestVaR(ret_test_GOLD, forecast_IGu_GOLD, 0.05)
Backtest_GOLD_ASu = BacktestVaR(ret_test_GOLD, forecast_ASu_GOLD, 0.05)
Backtest_GOLD_GJRu = BacktestVaR(ret_test_GOLD, forecast_GJRu_GOLD, 0.05)


###CAVIAR-GOLD, tao = 0.01
require(quantmod)
source("CAViaR.R")
mydata <- read_excel("ret_crypto_index_comm.xlsx")
GOLD=mydata$`S&P500 (GOLD)`
tau = 0.01
ret_test_GOLD = mydata[1012:(nrow(mydata)), c(1,11)]
ret_train_GOLD = mydata[1:(nrow(mydata)*0.75), c(1,11)]
Time = ret_train_GOLD$...1
Time = as.Date(unlist(Time), format = "%Y-%m-%d")
GOLD = unlist(ret_train_GOLD$`S&P500 (GOLD)`)
ret_S = coredata(GOLD)
n.start = 10
ret_test_GOLD = unlist(ret_test_GOLD[,2]) #comando unlist va fatto girare una sola volta

fit_AD_10_G = est.CAViaR(ret = ret_G, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10_G = est.CAViaR(ret = ret_G, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV_G = est.CAViaR(ret = ret_G, tau = tau ,model = "SAV", n.start = n.start) 
fit_AS_G = est.CAViaR(ret = ret_G, tau = tau, model = "AS", n.start = n.start) 
fit_IG_G = est.CAViaR(ret = ret_G, tau = tau, model = "IG", n.start = n.start)
fit_GJR_G =  est.CAViaR(ret = ret_G, tau = tau, model = "GJR", n.start = n.start)
fit_GAVu_G = est.CAViaR(ret = ret_G, tau = tau , model = "SAVu", n.start = n.start, u= u)
fit_IGu_G = est.CAViaR(ret = ret_G, tau = tau , model = "IGu", n.start = n.start, u= u)
fit_ASu_G = est.CAViaR(ret = ret_G, tau = tau , model = "ASu", n.start = n.start, u= u)
fit_GJRu_G = est.CAViaR(ret = ret_G, tau = tau , model = "GJRu", n.start = n.start, u= u)


forecast_AD_10_GOLD = CAViaR.fun(beta = fit_AD_10_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AD", G = 10) #questi sono i VaR predetti sulla base degli 800 precedenti
forecast_AD_1e10_GOLD = CAViaR.fun(beta = fit_AD_1e10_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AD", G = 1e10)
forecast_SAV_GOLD = CAViaR.fun(beta = fit_SAV_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="SAV", G = 10)
forecast_AS_GOLD = CAViaR.fun(beta = fit_AS_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="AS", G = 10)
forecast_IG_GOLD = CAViaR.fun(beta = fit_IG_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="IG", G = 10)
forecast_GJR_GOLD = CAViaR.fun(beta = fit_GJR_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="GJR", G = 10)
forecast_SAVu_GOLD = CAViaR.fun(beta = fit_SAVu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="SAVu", G = 10)
forecast_IGu_GOLD = CAViaR.fun(beta = fit_IGu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="IGu", G = 10)
forecast_ASu_GOLD = CAViaR.fun(beta = fit_ASu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="ASu", G = 10)
forecast_GJRu_GOLD = CAViaR.fun(beta = fit_GJRu_G[["pars"]], u=u, ret=ret_test_GOLD, tau = tau, model ="GJRu", G = 10)



forecast_AD_10_GOLD = as.vector(forecast_AD_10_GOLD)
forecast_AD_1e10_GOLD = as.vector(forecast_AD_1e10_GOLD)
forecast_SAV_GOLD = as.vector(forecast_SAV_GOLD)
forecast_AS_GOLD = as.vector(forecast_AS_GOLD)
forecast_IG_GOLD = as.vector(forecast_IG_GOLD)
forecast_GJR_GOLD = as.vector(forecast_GJR_GOLD)
forecast_SAVu_GOLD = as.vector(forecast_SAVu_GOLD)
forecast_IGu_GOLD = as.vector(forecast_IGu_GOLD)
forecast_ASu_GOLD = as.vector(forecast_ASu_GOLD)
forecast_GJRu_GOLD = as.vector(forecast_GJRu_GOLD)


#queste somme devono fare 0
sum(is.na(ret_test_GOLD))
sum(is.na(forecast_AD_10_GOLD))
sum(is.infinite(ret_test_GOLD))
sum(is.infinite((forecast_AD_10_GOLD)))

library(GAS) #non sono uguali le lunghezze
Backtest_GOLD_AD_10 = BacktestVaR(data =ret_test_GOLD, VaR = forecast_AD_10_GOLD, alpha = 0.01)
Backtest_GOLD_AD_1e10 = BacktestVaR(ret_test_GOLD, forecast_AD_1e10_GOLD, 0.01)
Backtest_GOLD_SAV = BacktestVaR(ret_test_GOLD, forecast_SAV_GOLD, 0.01)
Backtest_GOLD_AS = BacktestVaR(ret_test_GOLD, forecast_AS_GOLD, 0.01)
Backtest_GOLD_IG = BacktestVaR(ret_test_GOLD, forecast_IG_GOLD, 0.01)
Backtest_GOLD_GJR = BacktestVaR(ret_test_GOLD, forecast_GJR_GOLD, 0.01)
Backtest_GOLD_SAVu = BacktestVaR(ret_test_GOLD, forecast_SAVu_GOLD, 0.01)
Backtest_GOLD_IGu = BacktestVaR(ret_test_GOLD, forecast_IGu_GOLD, 0.01)
Backtest_GOLD_ASu = BacktestVaR(ret_test_GOLD, forecast_ASu_GOLD, 0.01)
Backtest_GOLD_GJRu = BacktestVaR(ret_test_GOLD, forecast_GJRu_GOLD, 0.01)

