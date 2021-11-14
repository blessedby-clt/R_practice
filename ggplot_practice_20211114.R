library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(GGally)

data(Titanic)
titanic <- data.frame(Titanic)
data(mtcars)

# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors


str(titanic)
str(mtcars)
summary(mtcars)

mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

ggplot(data = titanic, aes(x= Sex, y = Freq, fill = Survived)) + 
  geom_bar(stat = "identity", position = "fill", show.legend = T) +
  labs(title = "성별에 따른 생존자") + 
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = mtcars, aes(x= hp, y = wt)) + geom_point(aes(colour = vs)) +
  ggrepel::geom_text_repel(aes(label = wt, size = 0.4))
  
a <- ggplot(data = mtcars, aes(x =am, y = hp)) + geom_boxplot(aes(fill = am)) + theme_bw()
b <- ggplot(data = mtcars, aes(x =am, y = hp)) + geom_violin(aes(fill = am)) + theme_bw()

c <- ggplot(data = mtcars, aes(x =hp, fill = am)) + geom_histogram(aes(alpha = 0.5)) + theme_bw()

grid.arrange(a, b, c)

ggplot(data = titanic, aes(x = Survived, y = Freq, fill = Age)) + geom_bar(stat = "identity") +
  facet_wrap(.~Age, scales = "free") 


ggpairs(mtcars)
g = ggplot(data = select_if(mtcars, is.numeric) %>% pivot_longer(cols = everything()))
g + geom_histogram(aes(x = value, fill = name), bins = 10) + facet_wrap(~name, scales = "free_x")
