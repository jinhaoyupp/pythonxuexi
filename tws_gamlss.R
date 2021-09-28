library(gamlss)

annual.ws = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/annual_WS.csv', header = T)
time = annual.ws$time
water = annual.ws$yearly_w
sand = annual.ws$yearly_s
tws = cbind(time,water,sand)
colnames(tws) = c('x', 'water','y')
tws = data.frame(tws)

tws.gamlss.weibull = gamlss(y~cs(x), data = tws, family = WEI)
tws.gamlss.gamma = gamlss(y~cs(x), data = tws, family = GA)
tws.gamlss.logno = gamlss(y~cs(x), data = tws, family = LOGNO)
tws.gamlss.logistic = gamlss(y~cs(x), data = tws, family = LO)
tws.gamlss.gumbel = gamlss(y~cs(x), data = tws, family = GU)

tws.weibull.aic = AIC(tws.gamlss.weibull)
tws.gamma.aic = AIC(tws.gamlss.gamma)
tws.logno.aic = AIC(tws.gamlss.logno)
tws.logistic.aic = AIC(tws.gamlss.logistic)
tws.gumbel.aic = AIC(tws.gamlss.gumbel)
total.aic = c(tws.weibull.aic,tws.gamma.aic,tws.logno.aic,
              tws.logistic.aic,tws.gumbel.aic)

tws.weibull.bic = BIC(tws.gamlss.weibull)
tws.gamma.bic = BIC(tws.gamlss.gamma)
tws.logno.bic = BIC(tws.gamlss.logno)
tws.logistic.bic = BIC(tws.gamlss.logistic)
tws.gumbel.bic = BIC(tws.gamlss.gumbel)
total.bic = c(tws.weibull.bic,tws.gamma.bic,tws.logno.bic,
              tws.logistic.bic,tws.gumbel.bic)

setaxis = annual.ws$time
plot(setaxis, tws$y, type = 'l',col=1)
lines(setaxis, fitted(tws.gamlss.weibull),col=2)
lines(setaxis, fitted(tws.gamlss.gamma),col=3)
lines(setaxis, exp(fitted(tws.gamlss.logno)), col=4, lty=4)
lines(setaxis, fitted(tws.gamlss.logistic), col=5)
lines(setaxis, fitted(tws.gamlss.gumbel), col=6)





