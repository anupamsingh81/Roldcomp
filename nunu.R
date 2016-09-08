A = data.frame(
    x = c(1,1,1),
    y = c(10,20,30),
    z = c(60,70,80),
    a = c(2,2,2),
    b = c(30,70,80),
    m = c(80,90,100)
)
library(tidyr)
A %>% gather(A,Amount,value,-x)
x.row10 <- setNames(data.frame(letters[1:3],1:3,2:4,3:5,4:6,5:7,6:8,7:9),
                    c("names",2004:2009,2012))
x.row10  %>% gather(Year, Val, -names)



A %>% gather("variable","value",2:3)
library("reshape2")
A = melt(A, id  = c("x", "a"))
A$variable = NULL
A = melt(A,id = c("value"))
A[,3] = NULL
library(dplyr)
A = rename(A,trial = variable )