library(ggplot2)
str(mtcars)
# create factors with value labels
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears"))
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl"))
# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am,
       size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")
library(dplyr)
mtcars$mpg
mtcarsnew <- mutate(mtcars, if (mtcars$mpg < 20){pom = 1}
                 else if (mtcars$mpg < 25){pom = 2}
                 else {pom = 3})
mtcarsnew = mtcars %>% mutate(pom = ifelse(mtcars$mpg < 20, 1, ifelse(mtcars$mpg < 25, 2, 3)))
 mtcarsnewer =  mtcarsnew %>%  mutate(tom = ifelse(mtcarsnew$hp < 150, 1, ifelse(mtcarsnew$hp < 250, 2, 3)))
qplot(hp, mpg, data=mtcarsnewer, shape=am, color=am,
      facets=tom~pom, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon")
