# Regressão simples e múltipla
dados <- mtcars

# Simples
lm1 <- lm(mpg ~ wt, data = dados)
summary(lm1)

# Múltipla
lm2 <- lm(mpg ~ wt + hp + drat, data = dados)
summary(lm2)













# Centralizando as variáveis (removendo as médias) para trabalhar sem intercepto
X <- as.matrix(scale(dados[, c("wt","hp","drat")], center = TRUE, scale = FALSE))
y <- as.numeric(dados$mpg - mean(dados$mpg))

# Beta usando MQO
beta_MQO <- solve(t(X) %*% X) %*% t(X) %*% y
beta_MQO

# Beta via matrizes de covariância
S_xx <- (t(X) %*% X) / (nrow(X) - 1)
S_xy <- (t(X) %*% y) / (nrow(X) - 1)

beta_cov <- solve(S_xx) %*% S_xy
beta_cov

# Beta usando lm()
lm_fit <- lm(y ~ X - 1)  # -1 para sem intercepto
summary(lm_fit)
coef(lm_fit)
