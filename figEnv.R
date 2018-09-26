x = seq(0, 32, 0.1);
y = rep(1 / 16, length(x))
y[x > 16] = 0
plotData = data.frame(x, y)
library('ggplot2')
ggplot(plotData, aes(x, y)) + geom_line() + geom_vline(xintercept = 2)