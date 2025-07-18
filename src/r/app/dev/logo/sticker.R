library(hexSticker)
library(ggplot2)
library(dplyr)



p <- data.frame(x = 1:100, y = rnorm(100)) %>%
  ggplot(aes(x, y))+
  geom_line(color = 'darkorange')+
  theme_classic()


p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point(color = 'darkorange')
p <- p + theme_void() + theme_transparent()

sticker(p, package="ADATA VIS", p_size=9, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        h_fill = 'white', h_color='#2d8a44', p_color = "grey50",
        filename="dev/logo/logo.png")
