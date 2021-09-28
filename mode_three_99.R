library(gamlss)

data_mode3 = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/mode3_99.csv',header = T)

plot(data_mode3$year, data_mode3$agu90, type = 'l', lty=1,col='red',
     xlab = 'Year', ylab = 'Sediment load(10^4 t)')
lines(data_mode3$year, data_mode3$awei90, type = 'l',lty=1, col='green')
lines(data_mode3$year, data_mode3$alo90, type = 'l',lty=1, col='blue')
points(data_mode3$year, data_mode3$yearly_s,pch=7,col='purple')
# 有线有点时添加图例
legend(x=2001,y=10000,legend = c('Mode0-10','Mode1-10','Mode2-10','Obs'),
       lty = c(1,1,1,-1),pch = c(-1,-1,-1,7),col = c('red','green','blue','purple'))


plot(data_mode3$year, data_mode3$agu95, type = 'l', lty=1,col='red',
     xlab = 'Year', ylab = 'Sediment load(10^4 t)')
lines(data_mode3$year, data_mode3$awei95, type = 'l',lty=1, col='green')
lines(data_mode3$year, data_mode3$alo95, type = 'l',lty=1, col='blue')
points(data_mode3$year, data_mode3$yearly_s,pch=7,col='purple')
legend(x=2001,y=10500,legend = c('Mode0-20','Mode1-20','Mode2-20','Obs'),
       lty = c(1,1,1,-1),pch = c(-1,-1,-1,7),col = c('red','green','blue','purple'))
# GU-20','WEI-20','LO-20','Obs'
plot(data_mode3$year, data_mode3$agu98, type = 'l', lty=1,col='red',
     xlab = 'Year', ylab = 'Sediment load(10^4 t)')
lines(data_mode3$year, data_mode3$awei98, type = 'l',lty=1, col='green')
lines(data_mode3$year, data_mode3$alo98, type = 'l',lty=1, col='blue')
points(data_mode3$year, data_mode3$yearly_s,pch=7,col='purple')
legend(x=2001,y=11000,legend = c('Mode0-50','Mode1-50','Mode2-50','Obs'),
       lty = c(1,1,1,-1),pch = c(-1,-1,-1,7),col = c('red','green','blue','purple'))

plot(data_mode3$year, data_mode3$agu99, type = 'l', lty=1,col='red',
     xlab = 'Year', ylab = 'Sediment load(10^4 t)')
lines(data_mode3$year, data_mode3$awei99, type = 'l',lty=1, col='green')
lines(data_mode3$year, data_mode3$alo99, type = 'l',lty=1, col='blue')
points(data_mode3$year, data_mode3$yearly_s,pch=7,col='purple')
legend(x=2001,y=11000,legend = c('Mode0-100','Mode1-100','Mode2-100','Obs'),
       lty = c(1,1,1,-1),pch = c(-1,-1,-1,7),col = c('red','green','blue','purple'))



