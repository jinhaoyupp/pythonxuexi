library(gamlss)

tpws = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/jsj_pre_w_sand.csv', header = T)
# mu固定不变
tpws.gamlss.logistic.fix = gamlss(yearly_s~1, data = tpws, family = LO)
plot(tpws$time, tpws$yearly_s, type = 'l',col=1)
lines(tpws$time, fitted(tpws.gamlss.logistic.fix),col=2)

sand.fix = fitted(tpws.gamlss.logistic.fix)


tpws.gamlss.logistic.time = gamlss(yearly_s~time, data = tpws, family = LO)
plot(tpws$time, tpws$yearly_s, type = 'l',col=1)
lines(tpws$time, fitted(tpws.gamlss.logistic.time),col=2)
sand.time = fitted(tpws.gamlss.logistic.time)

tpws.gamlss.logistic.time.sigma = gamlss(yearly_s~cs(time), sigma.formula = ~cs(time),
                                         data = tpws, family = LO)
plot(tpws$time, tpws$yearly_s, type = 'l',col=1)
lines(tpws$time, fitted(tpws.gamlss.logistic.time.sigma),col=2)
sand.time.sigma = fitted(tpws.gamlss.logistic.time.sigma)




