library(gamlss)

data = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/jsj_pre_w_sand.csv',header = T)

time = data$time
water = data$yearly_w
pre = data$yearly_pre
sand = data$yearly_s

twps = cbind(time,water,pre,sand)
colnames(twps) = c('x', 'water','pre','y')
twps = data.frame(twps)

m.gu = gamlss(y~1, sigma.formula = ~1, data = twps, family = GU)
m.wei2 = gamlss(y~cs(x), sigma.formula = ~cs(x), data = twps, family = WEI)
m.lo2 = gamlss(y~water, sigma.formula = ~water, data = twps, family = LO)

# # 模型对比
# centiles.com(m.gu,m.wei2,m.lo2, xvar = twps$x,
#              cent = c(5,50,95),
#              ylim = c(100, 8500),
#              xleg = 2001,yleg = 2500,
#              xlab = 'Year',ylab = 'Sediment load(10^4 t)')
# 
# # 10, 20, 50, 100年设计值
mat.gu = centiles.pred(m.gu,xname = 'x',xvalues = seq(2001,2018,1),
                       cent = c(90,95,98,99),plot = F)
mat.wei2 = centiles.pred(m.wei2,xname = 'x',xvalues = seq(2001,2018,1),
                       cent = c(90,95,98,99),plot = T)
newvalue = cbind(water, pre)
newvalue = data.frame(newvalue)

mat.lo2 = centiles.pred(m.lo2,xname = 'water',xvalues = newvalue$water,
                        data=twps,
                         cent = c(90,95,98,99),plot = T)

twps13 = twps[1:13,]
m.lo13 = gamlss(y~water+pre,sigma.formula = ~water+pre, data = twps13, family = LO)
mat1418 = centiles.pred(m.lo13, xname = 'x', xvalues = seq(2014,2018,1),
                        cent = c(90,95,98,99))



