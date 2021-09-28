library(gamlss)

data = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/jsj_pre_w_sand.csv',header = T)

time = data$time
water = data$yearly_w
pre = data$yearly_pre
sand = data$yearly_s

twps = cbind(time,water,pre,sand)
colnames(twps) = c('x', 'water','pre','y')
twps = data.frame(twps)


m.wei0 = gamlss(y~cs(water)+cs(pre), sigma.formula = ~1, data = twps, family = WEI)
m.wei1 = gamlss(y~1, sigma.formula = ~cs(water)+cs(pre), data = twps, family = WEI)
m.wei2 = gamlss(y~cs(water)+pre, sigma.formula = ~cs(water)+pre, data = twps, family = WEI)
m.ga0 = gamlss(y~cs(water)+cs(pre), sigma.formula = ~1, data = twps, family = GA)
m.ga1 = gamlss(y~1, sigma.formula = ~cs(water)+cs(pre), data = twps, family = GA)
m.ga2 = gamlss(y~cs(water)+pre, sigma.formula = ~cs(water)+pre, data = twps, family = GA)
m.logno0 = gamlss(y~cs(water)+cs(pre), sigma.formula = ~1, data = twps, family = LOGNO)
m.logno1 = gamlss(y~1, sigma.formula = ~cs(water)+cs(pre), data = twps, family = LOGNO)
m.logno2 = gamlss(y~cs(water)+pre, sigma.formula = ~water+pre, data = twps, family = LOGNO)
m.lo0 = gamlss(y~cs(water)+cs(pre), sigma.formula = ~1, data = twps, family = LO)
m.lo1 = gamlss(y~1, sigma.formula = ~cs(water)+cs(pre), data = twps, family = LO)
m.lo2 = gamlss(y~water+pre, sigma.formula = ~water+pre, data = twps, family = LO)
m.gu0 = gamlss(y~cs(water)+cs(pre), sigma.formula = ~1, data = twps, family = GU)
m.gu1 = gamlss(y~1, sigma.formula = ~cs(water)+cs(pre), data = twps, family = GU)
m.gu2 = gamlss(y~cs(water)+pre, sigma.formula = ~water+pre, data = twps, family = GU)

# 选出最优mode2是m.lo2
# par(bg="white")
# plot(m.gu,xvar = twps$x, parameters = par(bg='white', col='green')) #修改背景颜色
newpar<-par(mfrow=c(2,2), mar=par("mar")+c(0,1,0,0), col.axis="blue4",
            col="blue4",
            col.main="blue4",col.lab="blue4",pch="+",cex=.45,
            cex.lab=1.2, cex.axis=1, cex.main=1.2 )
plot(m.lo2,par=newpar)
wp(m.lo2, col=5, pch=18)

# 分位数线
centiles(m.lo2, xvar= twps$x,cent = c(5,50,95),
         xleg = 2001, yleg = 3200, ylim = c(1000, 9500),
         pch = 17, cex = 1, col = 'purple',
         col.centiles = c('blue', 'red','green'),
         xlab = 'Year', ylab = 'Sediment load(10^4 t)')
# lines(twps$x, fitted(m.gu), col='red', lwd=1.2)

# 预测

twps13 = twps[1:13,]
m.lo13 = gamlss(y~water+pre,sigma.formula = ~water+pre, data = twps13, family = LO)
mat1418 = centiles.pred(m.lo13, xname = 'x', xvalues = seq(2014,2018,1),
                        cent = c(5,50,95))
centiles(m.lo13,xvar = twps13$x, cent = c(5,50,95),
         xleg = 2001, yleg = 2800, ylim = c(500, 9500),
         xlim = c(2001,2018),
         pch = 17, cex = 1, col = 'purple',
         col.centiles = c('blue', 'red','green'),
         xlab = 'Year', ylab = 'Sediment load(10^4 t)')
# lines(twps13$x, fitted(m.gu13), col='red', lwd=1.2)
lines(twps$x[14:18], mat1418[,2], col='blue',lty=2)
lines(twps$x[14:18], mat1418[,3], col='red',lty=2)
lines(twps$x[14:18], mat1418[,4], col='green',lty=2)
points(twps$x[14:18], twps$y[14:18],pch=18,col='orange')


