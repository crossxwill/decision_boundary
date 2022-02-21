library(tidyverse)
library(splines)


### simulate data ---------

set.seed(1)

x1 <- runif(300, 0, 10)
x2 <- runif(300, 0, 10)

y <- as.factor(ifelse(sqrt((x1-5)^2 + (x2-5)^2) < 4.5, 1, 0))

df <- data.frame(y=y, x1=x1, x2=x2)

rm(x1, x2, y)

head(df)

ggplot(df, aes(x=x1, y=x2, color=y)) + geom_point()

### logistic regression with natural cubic splines -------

logit_mod <- glm(y ~ ns(x1,2)*ns(x2,2), data=df, family=binomial)
## p-values are unreliable due to perfect separation

gam::plot.Gam(logit_mod)

summary(logit_mod)

### decision boundary ------

x1 <- seq(min(df$x1), max(df$x1), length.out=300)
x2 <- seq(min(df$x2), max(df$x2), length.out=300)

X_complete <- expand.grid(x1=x1, x2=x2)

X_complete$logit_pred <- predict(logit_mod, newdata=X_complete, type="response")

ggplot(df, aes(x=x1, y=x2, color=y)) + geom_point() +
  geom_contour(data=X_complete, aes(x=x1, y=x2, z=logit_pred),
               inherit.aes = FALSE, breaks=c(0.5))

