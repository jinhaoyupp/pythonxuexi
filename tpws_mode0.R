library(gamlss)

data = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/jsj_pre_w_sand.csv',header = T)

time = data$time
water = data$yearly_w
pre = data$yearly_pre
sand = data$yearly_s

twps = cbind(time,water,pre,sand)
colnames(twps) = c('x', 'water','pre','y')
twps = data.frame(twps)


m.wei = gamlss(y~1, sigma.formula = ~1, data = twps, family = WEI)
m.ga = gamlss(y~1, sigma.formula = ~1, data = twps, family = GA)
m.logno = gamlss(y~1, sigma.formula = ~1, data = twps, family = LOGNO)
m.lo = gamlss(y~1, sigma.formula = ~1, data = twps, family = LO)
m.gu = gamlss(y~1, sigma.formula = ~1, data = twps, family = GU)

# 选出最优mode0是Gumbel
# par(bg="white")
# plot(m.gu,xvar = twps$x, parameters = par(bg='white', col='green')) #修改背景颜色
newpar<-par(mfrow=c(2,2), mar=par("mar")+c(0,1,0,0), col.axis="blue4",
             col="blue4",
             col.main="blue4",col.lab="blue4",pch="+",cex=.45,
             cex.lab=1.2, cex.axis=1, cex.main=1.2 )
plot(m.gu,par=newpar)
wp(m.gu, col=5, pch=18)

# 分位数线
centiles(m.gu, xvar= twps$x,cent = c(5,50,95),
         xleg = 2001, yleg = 4200, ylim = c(2500, 8500),
         pch = 17, cex = 1, col = 'purple',
         col.centiles = c('blue', 'red','green'),
         xlab = 'Year', ylab = 'Sediment load(10^4 t)')
# lines(twps$x, fitted(m.gu), col='red', lwd=1.2)

# 预测

twps13 = twps[1:13,]
m.gu13 = gamlss(y~1,sigma.formula = ~1, data = twps13, family = GU)
mat1418 = centiles.pred(m.gu13, xname = 'x', xvalues = seq(14,18,1),
                        cent = c(5,50,95))
centiles(m.gu13,xvar = twps13$x, cent = c(5,50,95),
         xleg = 2001, yleg = 3800, ylim = c(1500, 8500),
         xlim = c(2001,2018),
         pch = 17, cex = 1, col = 'purple',
         col.centiles = c('blue', 'red','green'),
         xlab = 'Year', ylab = 'Sediment load(10^4 t)')
# lines(twps13$x, fitted(m.gu13), col='red', lwd=1.2)
lines(twps$x[14:18], mat1418[,2], col='blue',lty=2)
lines(twps$x[14:18], mat1418[,3], col='red',lty=2)
lines(twps$x[14:18], mat1418[,4], col='green',lty=2)
points(twps$x[14:18], twps$y[14:18],pch=18,col='orange')


