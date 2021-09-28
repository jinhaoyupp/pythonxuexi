library(scatterplot3d)
library(plotrix)
library(ggplot2)

data = read.csv('/Users/jinhaoyu/Downloads/jinshajiang_WS/jsj_pre_w_sand.csv',header = T)

time = data$time
water = data$yearly_w
pre = data$yearly_pre
sand = data$yearly_s

twps = cbind(time,water,pre,sand)
colnames(twps) = c('x', 'water','pre','y')
twps = data.frame(twps)

twoord.plot(twps$x, twps$y, twps$x, twps$water,
            ylab = 'Sediment load',lytickpos = seq(2000, 9000, 1000),
            mar = c(4,4,4,4), lylab = seq(2000, 9000, 1000))

p <- ggplot(twps, aes(x = twps$x))
p <- p + geom_line(aes(y=twps$y, colour = 'sediment'))
p <- p + geom_line(aes(y=twps$water, colour = 'water'))
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = 'Relative humidity [%]'))
p <- p + scale_colour_manual(values = c('blue', 'red'))
p <- p + labs(y = 'Air temperature [°C]',x = '“Date and time”',colour = '“Parameter”')
p <- p + theme(legend.position = c(0.8, 0.9))
p
# 沙与年径流量
par(mar = c(5, 5, 3, 5))
plot(twps$x,twps$y, type ="o",ylab = '',
     xlab = "Year",
     col = "blue", pch=6, col.ylab='blue')
axis(side = 2, col.ticks = 'blue', col.axis='blue')
mtext("Sediment load(10^4 t)", side = 2, line = 3, col = 'blue')
par(new = TRUE)
plot(twps$x,twps$water, type = "o", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2, pch=4
    )

axis(side = 4,col.ticks = 'red', col.axis='red')
mtext("Flow(10^8 m^3)", side = 4, line = 3, col = 'red')

legend("bottomright", c("Sediment load", "Flow"),
       col = c("blue", "red"), lty = c(1, 2), 
       pch = c(6,4),
       bty = 'n')
# 沙与降水
par(mar = c(5, 5, 3, 5))
plot(twps$x,twps$y, type ="o",ylab = '',
     xlab = "Year",
     col = "blue", pch=6, col.ylab='blue')
axis(side = 2, col.ticks = 'blue', col.axis='blue')
mtext("Sediment load(10^4 t)", side = 2, line = 3, col = 'blue')
par(new = TRUE)
plot(twps$x, pre, type = "o", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "green", lty = 2, pch=4
)

axis(side = 4,col.ticks = 'green', col.axis='green')
mtext("Precipitation(mm)", side = 4, line = 3, col = 'green')

legend("bottomright", c("Sediment load", "Precipitation"),
       col = c("blue", "green"), lty = c(1, 2), 
       pch = c(6,4),
       bty = 'n')

# 散点图
smoothScatter(water, twps$y, cex = 0.7,col='blue',
              xlab = 'Flow(10^8 m^3)',
              ylab = 'Sediment load(10^4 t)',
              colramp = colorRampPalette(c("white", 'salmon')),
              pch = 5)

corcoef = cor(twps$y,water)
fit <- glm(twps$y~water)
co <- coef(fit)
k = round(co[2],3)
b = round(co[1],2)
corcoef = round(corcoef,2)
abline(fit, col='magenta', lwd=1.5)
text(500,5700,paste('y=',k,'x',b,'\nR^2=', 0.78))
# 沙与降雨
smoothScatter(pre, twps$y, cex = 0.7,col='purple',
              xlab = 'Flow(10^8 m^3)',
              ylab = 'Precipitation(mm)',
              colramp = colorRampPalette(c("white", 'royalblue1')),
              pch = 5)

corcoef = cor(twps$y,pre)
fit <- glm(twps$y~pre)
co <- coef(fit)
k = round(co[2],3)
b = round(co[1],2)
corcoef = round(corcoef,2)
abline(fit, col='seagreen1', lwd=1.5)
text(700,5700,paste('y=',k,'x',b,'\nR^2=', 0.73))


