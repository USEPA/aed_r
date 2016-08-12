# Going over example from gganimate

# Get packages: gganimate not on CRAN
# Use devtools to get gganimate
#install.packages("devtools")
#install.packages("gapminder")
library(devtools) #Need for install_github
install_github("dgrtwo/gganimate") #installing
library(gganimate)
library(ggplot2)
library(gapminder)

#setting to black and with theme
theme_set(theme_bw())

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, 
                           size = pop, color = continent, 
                           frame = year)) +
  geom_point() +
  scale_x_log10()

# Notice we added frame = year and saved the plot as p. 
# We then display it as an animation with the gg_animate function:
p_anim <- gg_animate(p)

# Save to gif
gg_animate_save(p_anim,"animation.gif")

#Some Iris Examples
x <- ggplot(iris,aes(Sepal.Length,Sepal.Width)) + 
  geom_point() + 
  stat_smooth(method="lm")
x
xlm <- lm(Sepal.Width ~ Sepal.Length , data=iris)

#Get Help:
#http://docs.ggplot2.org/
#http://www.cookbook-r.com/Graphs/
