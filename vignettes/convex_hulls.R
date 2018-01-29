## ----preambule-----------------------------------------------------------
library(ggplot2)
library(PTCA4CATA)

## ------------------------------------------------------------------------
set.seed(123)
df <- data.frame(x = rnorm(10), y=rnorm(10))

ggplot(data=df, aes(x, y)) + 
  geom_point() +
  ggConvexHull(df)

## ------------------------------------------------------------------------
df2 <- rbind(df, 
             data.frame(x = rnorm(10, 1), 
                        y = rnorm(10, -3)))
df2$group <- factor(paste0("Group", rep(1:2, e=10)))
  
ggplot(data=df2, aes(x, y, color=group)) + 
  geom_point() +
  ggConvexHull(df2, names.of.factors = (df2$group))


