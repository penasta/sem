if(!require(pacman)) install.packages(pacman)
p_load(psych)

dados <- Harman74.cor$cov
n <- Harman74.cor$n.obs

fa.parallel(dados, n.obs = n, fa = "fa") 

# KMO: medida de adequação de amostragem;
# Referência: valores > .80 muito bons, .7 aceitáveis, < .6 problemáticos.
KMO(dados)

# Testa se existem covariâncias. Hipótese nula: Não há.
cortest.bartlett(dados, n = n)

fa_fit1 <- fa(dados, nfactors = 2, rotate = "varimax", fm = "ml")

fa_fit1

fa.diagram(fa_fit1, simple = F)

fa_fit2 <- fa(dados, nfactors = 6, rotate = "promax", fm = "ml")

fa_fit2

fa.diagram(fa_fit2, simple = F)
