if(!require(pacman))install.packages(pacman)
p_load(plspm,readr,pwr,reshape,ggplot2,car,boot)

# 0.1. Tamanho amostral ----

## 0.1.1. Via OLS ----
n <- pwr.f2.test(u=17, # argmax #preditores(GD,GC)
            f2 = .15, # Tamanho de efeito. Médio = .15
            sig.level = .05, # Significância
            power = .8) # Poder

round(n[["v"]]+n[["u"]]+1)
# Tamanho mínimo da amostra (real!) para obter os parâmetros fixados acima.

# Observação: o cálculo de poder é baseado em regressão linear (OLS).
# Em PLS-SEM, é usado aqui como referência conservadora.

## 0.1.2. Via Inverse Square Root & Gamma-exponential

z <- 1.96
power <- .8

### 0.1.2.1. ISR ----
ceiling((z/(1-power))^2)

### 0.1.2.2. GE ----
ceiling((z^2)/((1-power)^2)*exp(1-power))

# Usar ISR e GE como limites inferior e superior para n.


# 1.0. Dados ----
df <- read_csv("dados/base_simulada_GD_GC.csv")

summary(df)

# 1.1. Matriz de caminhos ----
# Aqui, devemos indicar os caminhos estruturais do modelo
GD <- c(0,0)
GC <- c(1,0)

paths <- rbind(GD,GC)
colnames(paths) <- rownames(paths)

# Visualizar
paths
innerplot(paths)

# 1.2. Blocos ----
# Aqui, devemos indicar quais manifestas impactam quais "latentes" (tanto se reflexivo quanto se formativo.)
# Temos:
# GC: 1.1, 1.2, 1,3, 1.8, 1.9, 2.1, 2.4, 2.5, 2.6,2.7, 2.8, 2.9, 3.2, 3.4, 3.5, 3.7, 3.9 
# GD: 1.4,1.5, 1.6, 1.7, 2.2, 2.3, 3.1, 3.3, 3.6, 3.8

#               GD    GC
blocks <- list(1:10, 11:27)

# 1.3. Modos ----
# Aqui, devemos indicar se construtos são reflexivos ou formativos.
# A: Reflexivo. B: Formativo.
# Ordem: Dos BLOCOS

#           GD  GC
modes <- c("A","A")

# 2.0. Ajuste do modelo.
modelo <- plspm(Data = df,
                path_matrix = paths,
                blocks = blocks,
                scaling = NULL,
                modes = modes,
                scheme = "path", # Esquema: Modelo de caminhos.
                scaled = NULL,
                tol = 1e-05,
                maxiter = 300,
                boot.val = TRUE,
                br = 100 # Quantidade de reamostragens bootstrap para validar modelo e IC (aumentar este valor! ~ 5000)
                )

# 3. Avaliação do modelo ----
summary(modelo)

## 3.1. Modelos REFLEXIVOS ----

### 3.1.1. Avaliação unidimensional ----
# (Alfa de Cronbach e Medida de Confiabilidade Composta)
modelo$unidim
# Esperamos C.alpha,DG.rho > .7
# 1 < eig.1st >> eig.2nd

### 3.1.2. Cargas e comunalidade (validade convergente) ----
modelo$outer_model[,-c(3,6)]
# Esperamos liading > .708 e/ou communality > .5

### 3.1.3. AVE (Validade convergente) ----
modelo$inner_summary[,5, drop = F]
# Esperamos > 0.5

### 3.1.4. Cargas cruzadas (Validade discriminante) ----
modelo$crossloadings
# Esperamos valores maiores para as latentes "corretas".
xloads <- melt(modelo$crossloadings,
               id.vars = c("name","block"),
               variable_name = "LV")

ggplot(data = xloads, aes(x = name, y = value, fill = block)) +
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = .5, color = "gray70", linetype = 2) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(block ~ LV)+
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  ggtitle("Cross loadings")

### 3.1.5. Critério de Fornell-Larcker ----
sqrt(modelo$inner_summary$AVE)
# 0.6351391 0.6281412

summary(modelo) # Buscar "CORRELATIONS BETWEEN LVs"
# CORRELATIONS BETWEEN LVs 
#       GD     GC
# GD  1.000  0.589
# GC  0.589  1.000

# Comparar resultados:

# 0.6351391 > 0.589 -> Tudo certo!
# 0.6281412 > 0.589 -> Tudo certo!


### 3.1.6. Heterotrait-Monotrait Ratio (HTMT) ----
# Obs: Não vem implementado no pacote.

itens_GD <- df[, 1:10]
itens_GC <- df[, 11:27]

cor_mat <- cor(df, use = "pairwise.complete.obs")

calc_htmt <- function(cor_mat, idx_A, idx_B) {
  hetero <- abs(cor_mat[idx_A, idx_B])
  mono_A <- abs(cor_mat[idx_A, idx_A])
  mono_B <- abs(cor_mat[idx_B, idx_B])
  mono_A <- mono_A[lower.tri(mono_A)]
  mono_B <- mono_B[lower.tri(mono_B)]
  htmt <- mean(hetero) / sqrt(mean(mono_A) * mean(mono_B))
  return(htmt)
}

htmt_boot <- function(data, indices, idx_A, idx_B) {
  d <- data[indices, ]
  cor_mat <- cor(d, use = "pairwise.complete.obs")
  calc_htmt(cor_mat, idx_A, idx_B)
}

boot_htmt <- boot(
  data = df,
  statistic = htmt_boot,
  R = 100,
  idx_A = 1:10,
  idx_B = 11:27
)

boot.ci(boot_htmt, type = "perc")

# < 0.85 -> validade discriminante adequada
# IC não inclui 1 -> Validade discriminante confirmada

## 3.2. Modelos FORMATIVOS ----

# Trocando um dos modos, apenas para verificarmos como seria a avaliação no caso formativo.
modes <- c("A","B")

modelo <- plspm(Data = df,path_matrix = paths,blocks = blocks,scaling = NULL,modes = modes,scheme = "path",scaled = NULL,tol = 1e-05,maxiter = 300,boot.val = TRUE,br = 100)

### 3.2.0. Análise de redundância ----

# Não faremos de fato este critério, pois ele é duvidoso.
# Mas uma forma é simplesmente analisar se as cargas (loadings) dos itens no caso reflexivo são > .7.

### 3.2.1. Multicolinearidade ----
scores <- as.data.frame(modelo$scores)
fit <- lm(scores$GC ~ df$q1_1 + df$q1_2 + df$q1_3 + df$q1_8 + df$q1_9 + df$q2_1 + df$q2_4 + df$q2_5 + df$q2_6 + df$q2_7 + df$q2_8 + df$q2_9 + df$q3_2 + df$q3_4 + df$q3_5 + df$q3_7 + df$q3_9)
vif(fit) # <5, sem grandes problemas.

### 3.2.2. Significância dos pesos ----

modelo$boot$weights[11:27,] 
# Queremos também, preferencialmente, que sejam estritamente positivas.
# Critério: IC não passou pelo 0 -> significante.
# Demais: Olhar a carga:

modelo$boot$loadings[11:27,]
# Cargas significantes (e, preferencialmente, se a carga for > 0,5) -> Pode manter a carga.

# Avaliando tudo, poderíamos remover q2_5 e q2_7, rodar novamente o modelo e analisar.

# 4. Avaliação estrutural do modelo
modes <- c("A","A")
modelo <- plspm(Data = df,path_matrix = paths,blocks = blocks,scaling = NULL,modes = modes,scheme = "path",scaled = NULL,tol = 1e-05,maxiter = 300,boot.val = TRUE,br = 100)
summary(modelo)

## 4.1. Multicolinearidade ----
scores <- as.data.frame(modelo$scores)
fit <- lm(scores$GC ~ scores$GD)
vif(fit) # Como comentado, não teria como haver multicolinearidade mesmo.

## 4.2. Coeficiente de caminho (relevância e significância) ----

modelo$inner_model

# p-valor significante, estimativa relevante e positivo.

## 4.3. Efeitos (Diretos, Indiretos, Totais) ----

modelo$effects
# (Neste caso particular, é simplesmente a estimativa vista na análise anterior.)
# GD -> GC. Efeito total = 0.5891717

### 4.3.1 Tamanho do efeito f² (o quanto explica) ----

modelo$inner_summary

# R² 'com', R² 'sem'. Como GC não recebe mais nenhum preditor, R² 'sem' = 0 por definição.
(0.3471233 - 0) / (1 - 0.3471233)

# < 0.02	Desprezível
# 0.02–0.15	Pequeno
# 0.15–0.35	Médio
# ≥ 0.35	Grande

## 4.4. Coeficiente de determinação (R²), e média do índice de redundância (Q²) ----

modelo$inner_summary[,-5]
# A variação de GD explica a variação em GC em 34,71%.
# Variação das manifestas da variável exógena que explcam a variação das variáveis manifestas indicadoras da variável endógena.

# Q² ≤ 0	Sem relevância preditiva
# 0 < Q² ≤ 0.02	Pequena
# 0.02 < Q² ≤ 0.15	Média
# Q² > 0.15	Alta

# Calculado: 0.1369615 (Ok!)

### 4.4.1 Tamanho do efeito q² (o quanto prediz) ----

modelo$inner_summary

0.1369615 / (1-0.1369615)

# < 0.02	Desprezível
# 0.02–0.15	Pequeno
# 0.15–0.35	Médio
# ≥ 0.35	Grande

## 4.5. GoF ----
modelo$gof

# 5. Validação ----

## 5.1. Bootstrap (Se vire) ----

modelo$boot

## 5.2. Convergência ----
# Olhar o primeiro quadro, "MODEL SPECIFICATION", item 9.
summary(modelo)
# Convergência em 3 iterações.

