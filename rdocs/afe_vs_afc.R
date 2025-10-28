if(!require(pacman)) install.packages(pacman)
p_load(lavaan,lavaanPlot,semPlot)

df <- HolzingerSwineford1939

modelo_efa <- '
  efa("block1")*F1 + efa("block1")*F2 + efa("block1")*F3 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'

fit_efa <- sem(modelo_efa, data = df, estimator = "MLR", missing = "fiml")

lavaanPlot(model = fit_efa, coefs = TRUE)

?HolzingerSwineford1939

modelo_afc <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

fit_afc <- cfa(modelo_afc, data = df, estimator = "MLR", missing = "fiml")

lavaanPlot(model = fit_afc, coefs = TRUE)

# summary(fit_afc, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitMeasures(fit_afc, c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))

# Cargas fatoriais padronizadas
inspect(fit_afc, "std")$lambda


