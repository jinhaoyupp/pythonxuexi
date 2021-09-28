library(gamlss)

annual.ws = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/annual_WS.csv', header = T)
water = annual.ws$yearly_w
sand = annual.ws$yearly_s
ws = cbind(water,sand)
colnames(ws) = c('x','y')
ws = data.frame(ws)
ws.gamlss.weibull = gamlss(y~cs(x), data = ws, family = WEI)
ws.gamlss.gamma = gamlss(y~cs(x), data = ws, family = GA)
ws.gamlss.logno = gamlss(y~cs(x), data = ws, family = LOGNO)
ws.gamlss.logistic = gamlss(y~cs(x), data = ws, family = LO)
ws.gamlss.gumbel = gamlss(y~cs(x), data = ws, family = GU)

ws.weibull.aic = AIC(ws.gamlss.weibull)
ws.gamma.aic = AIC(ws.gamlss.gamma)
ws.logno.aic = AIC(ws.gamlss.logno)
ws.logistic.aic = AIC(ws.gamlss.logistic)
ws.gumbel.aic = AIC(ws.gamlss.gumbel)
total.aic = c(ws.weibull.aic,ws.gamma.aic,ws.logno.aic,
              ws.logistic.aic,ws.gumbel.aic)

ws.weibull.bic = BIC(ws.gamlss.weibull)
ws.gamma.bic = BIC(ws.gamlss.gamma)
ws.logno.bic = BIC(ws.gamlss.logno)
ws.logistic.bic = BIC(ws.gamlss.logistic)
ws.gumbel.bic = BIC(ws.gamlss.gumbel)
total.bic = c(ws.weibull.bic,ws.gamma.bic,ws.logno.bic,
              ws.logistic.bic,ws.gumbel.bic)

setaxis = annual.ws$time
plot(setaxis, ws$y, type = 'l',col=1)
lines(setaxis, fitted(ws.gamlss.weibull),col=2)
lines(setaxis, fitted(ws.gamlss.gamma),col=3)
lines(setaxis, exp(fitted(ws.gamlss.logno)), col=4, lty=4)
lines(setaxis, fitted(ws.gamlss.logistic), col=5)
lines(setaxis, fitted(ws.gamlss.gumbel), col=6)

# 分位数
centiles(ws.gamlss.logistic,setaxis,cent = c(5,95))
lines(setaxis, fitted(ws.gamlss.logistic), col=5)
# Worm plots
wp(ws.gamlss.logistic)
# plot
newpar<-par(mfrow=c(2,2), mar=par("mar")+c(0,1,0,0), col.axis="blue4",
            col="blue4",
            col.main="blue4",col.lab="blue4",pch="+",cex=.45,
            cex.lab=1.2, cex.axis=1, cex.main=1.2 )
plot(ws.gamlss.logistic, par=newpar)





