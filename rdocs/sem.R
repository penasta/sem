if(!require(pacman)) install.packages(pacman)
p_load(lavaan,lavaanPlot,semPlot)

# 1) Exemplo canônico ----

model <- ' 
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sem(model, data = PoliticalDemocracy)
summary(fit, fit.measures = TRUE)
lavaanPlot(model = fit, coefs = TRUE)



# 2) Exemplo gerado ----

modelo_sim <- '
  x1 =~ 0.8*q1 + 0.7*q2 + 0.9*q3
  x2 =~ 0.2*q4 + 0.8*q5 + 0.6*q6
  x3 =~ 0.9*q7 + 0.8*q8 + 0.7*q9
  x4 =~ 0.8*q10 + 0.7*q11 + 0.6*q12
  x5 =~ 0.9*q13 + 0.8*q14 

  x4 ~ 0.5*x1 + 0.4*x2
  x5 ~ 0.6*x3 + 0.5*x4
  
  GD ~ 0.7*x1 + 0.3*x2 + 0.2*x3 + 0.8*x4 + 0.1*x5
  GC ~ 1.4*GD + 0.7*q15 + 0.3*x4 + 0.7*x5
  
  x1 ~~ 0.2*x2

'

data <- simulateData(modelo_sim)

cor(data)

model_fit <- '
  x1 =~ q1 + q2 + q3
  x2 =~ q4 + q5 + q6
  x3 =~ q7 + q8 + q9
  x4 =~ q10 + q11 + q12
  x5 =~ q13 + q14 

  x4 ~ x1 + x2
  x5 ~ x3 + x4

  GD ~ x1 + x2 + x3 + x4 + x5
  GC ~ GD + q15 + x4 + x5
' 

# deslocamento de q4 e q5; supressão de x4/x5 em GC.

fit_sem <- sem(model_fit, data = data)

summary(fit_sem, fit.measures = TRUE)

lavaanPlot(model = fit_sem, coefs = TRUE)
