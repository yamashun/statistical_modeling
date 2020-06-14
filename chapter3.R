DF <- read.table( "stat_data/data.csv", 
                  sep = ",",                #カンマ区切りのファイル
                  header = TRUE,            #1行目はヘッダー(列名)
                  fileEncoding="UTF-8")     #文字コードはUTF-8
DF
DF$f = as.factor(DF$f)
DF$f
class(DF$x)

summary(DF)

plot(DF$x, DF$y, pch = c(21,19)[DF$f])
legend("topleft", legend = c("C", "T"), pch = c(21, 19))

plot(DF$f, DF$y)

fit <- glm(y ~ x, data = DF, family = poisson)
fit
summary(fit)
logLik(fit)

plot(DF$x, DF$y, pch = c(21, 19)[DF$f])
xx <- seq(min(DF$x), max(DF$x), length = 100)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)

fit.f <- glm(y ~ f, data = DF, family = poisson)

## 複数の説明変数
fit.all <- glm(y ~ x + f, data = DF, family = poisson)
logLik(fit.all)

