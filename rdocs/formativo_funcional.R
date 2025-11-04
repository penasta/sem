if(!require(pacman)) install.packages(pacman)
p_load(lavaan,
       MASS, # Para gerar valores de uma normal multivariada
       lavaanPlot,
       semPlot)

set.seed(1)

n <- 2000   
# Em geral, requer amostra maior.

# Gerando 3 fatores exógenos correlacionados (F1, F2, F3)
Sigma_lat <- matrix(c(1.0, 0.25, 0.20,
                      0.25, 1.0, 0.15,
                      0.20, 0.15, 1.0),
                    3, 3)

lat <- mvrnorm(n = n, mu = rep(0,3), Sigma = Sigma_lat)

F1 <- lat[,1]
F2 <- lat[,2]
F3 <- lat[,3]

# Gerando indicadores reflexivos para F1, F2, F3 (q1..q9)
q1 <- 0.8*F1 + rnorm(n,0,0.6)
q2 <- 0.7*F1 + rnorm(n,0,0.6)
q3 <- 0.9*F1 + rnorm(n,0,0.6)
q4 <- 0.7*F2 + rnorm(n,0,0.6)
q5 <- 0.8*F2 + rnorm(n,0,0.6)
q6 <- 0.6*F2 + rnorm(n,0,0.6)
q7 <- 0.9*F3 + rnorm(n,0,0.6)
q8 <- 0.8*F3 + rnorm(n,0,0.6)
q9 <- 0.7*F3 + rnorm(n,0,0.6)

# Gerando indicadores formativos observados (f1,f2,f3). 
# Eles são observáveis e formarão o latente formativo no modelo (<~).
# Simulamos com algum vínculo aos fatores exógenos para realismo.
f1 <- 0.3*F1 + rnorm(n,0,1.0)
f2 <- 0.2*F2 + rnorm(n,0,1.0)
f3 <- 0.15*F3 + rnorm(n,0,1.0)
# NOTE: NÃO vamos criar uma coluna 'Form'
# observada nos dados que entram no modelo.

Form_true <- 0.6*f1 + 0.3*f2 + 0.1*f3 + rnorm(n,0,0.2)
# usado internamente para gerar outcomes

# Gerando latentes endógenos (verdadeiros) dependentes de Form_true e fatores
ETA_true <- 0.5*F1 + 0.4*Form_true + 0.3*F3 + rnorm(n,0,0.7)
Y2_true  <- 0.45*Form_true + 0.35*F2 + rnorm(n,0,0.7)

# Gerando indicadores reflexivos do ETA e Y2 (q10..q15)
q10 <- 0.8*ETA_true + rnorm(n,0,0.6)
q11 <- 0.7*ETA_true + rnorm(n,0,0.6)
q12 <- 0.9*ETA_true + rnorm(n,0,0.6)
q13 <- 0.7*Y2_true + rnorm(n,0,0.6)
q14 <- 0.6*Y2_true + rnorm(n,0,0.6)
q15 <- 0.8*Y2_true + rnorm(n,0,0.6)

dados <- data.frame(q1,q2,q3, q4,q5,q6, q7,q8,q9,
                    f1,f2,f3,
                    q10,q11,q12, q13,q14,q15)

# Modelo para ESTIMAÇÃO no lavaan: formativo com '<~'

model_est <- '
  F1 =~ q1 + q2 + q3
  F2 =~ q4 + q5 + q6
  F3 =~ q7 + q8 + q9

  ETA =~ q10 + q11 + q12
  Y2  =~ q13 + q14 + q15

  Form <~ 1*f1 + f2 + f3

  ETA ~ F1 + Form + F3
  Y2  ~ Form + F2

  F1 ~~ F2
  F1 ~~ F3
  F2 ~~ F3
'
# Notem que precisei fixar 1*F1, para ancorar o modelo.

fit_form <- sem(model_est, data = dados, estimator = "MLR")

summary(fit_form, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Verificar parâmetros do bloco formativo
pe <- parameterEstimates(fit_form, standardized = TRUE)
pe[pe$op %in% c("<~","~"), ]

lavaanPlot(model = fit_form)

semPaths(fit_form,
         whatLabels = "est",
         nCharNodes = 0,
         sizeMan = 6,
         sizeLat = 8,
         mar = c(4,4,4,4))



# Conclusão: Se possível, fuja!!
