require(ggplot2)
require(cowplot)

theme_set(theme_cowplot())


xlabs <- c(-2.2, -1.6, -0.5, 0, 3)
yends <- dlogis(xlabs)
cuts  <- c(expression(c[1]), 
           expression(c[2]), 
           expression(c[3]), 
           expression(...), 
           expression(c[K-1])
)

ggplot(data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dlogis, xlim = c(-5, xlabs[1]), geom = 'area', fill = 'red') + 
  stat_function(fun = dlogis, xlim = c(xlabs[1], xlabs[2]), geom = 'area', fill = 'orange') +
  stat_function(fun = dlogis, xlim = c(xlabs[2], xlabs[3]), geom = 'area', fill = 'yellow') +
  stat_function(fun = dlogis, xlim = c(xlabs[5], 5), geom = 'area', fill = 'grey') +
  stat_function(fun = dlogis, geom = 'line') +
  scale_x_continuous(breaks = xlabs, labels = cuts) +
  xlab('y*') +
  ylab('Density') +
  ggtitle('Standard Logistic Distribution') +
  theme(axis.text = element_text(size =  28),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30))
  

ggsave('Figures/0_Logit.png', width = 14, height = 10)









