if(!require(pacman)) install.packages(pacman)
p_load(psych,GGally,corrplot,dplyr)

dados <- diamonds %>%
  select(carat,depth,table,price,x,y,z)

# Matriz de covariância
cov_matrix <- cov(dados)
cov_matrix

# Matriz de correlação
cor_matrix <- cor(dados)
cor_matrix

# Note:
cov(scale(dados))

# PCA vs FA

par(mfrow = c(1, 2))

# Análise de componentes principais
pca <- psych::principal(dados, nfactors = 3, rotate = "promax")
psych::fa.diagram(pca)

# Análise fatorial exploratória
fa <- psych::fa(dados, nfactors = 3, rotate = "promax")

# Visualização da análise fatorial
fa.diagram(fa)

par(mfrow = c(1, 1))

# E aí?

# Outros dados - visualizando a diferença

df = bfi

par(mfrow = c(1, 2))
pca <- psych::principal(df, nfactors = 3, rotate = "promax")
psych::fa.diagram(pca)
fa <- psych::fa(df, nfactors = 3, rotate = "promax")
fa.diagram(fa)
par(mfrow = c(1, 1))

# E aí?









# Comunalidade!