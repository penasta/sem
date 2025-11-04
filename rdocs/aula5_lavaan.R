if(!require(pacman)) install.packages(pacman) # Gerenciador de pacotes no R
p_load(lavaan, # O pacote que de fato utilizaremos
       lavaanPlot, # Para gráficos
       semPlot # Alternativa para gráficos
       )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Exemplo: SEM (mensuração + estrutura) ----

# Modelo estrutural simples: F2 ~ F1
model_sim <- '
  # measurement
  F1 =~ 0.8*q1 + 0.7*q2 + 0.9*q3
  F2 =~ 0.7*q4 + 0.8*q5 + 0.6*q6

  # structural
  F2 ~ 0.6*F1

  # variâncias e covariâncias
  F1 ~~ 1*F1
  F2 ~~ 1*F2
'

data <- simulateData(model_sim)

model_est <- '
  F1 =~ q1 + q2 + q3
  F2 =~ q4 + q5 + q6
  F2 ~ F1
'

fit <- sem(model_est, data = data, estimator = "MLR")
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Ao executar uma AFC, estamos mensurando um construto que não tem mensuração direta.
# Logo, também não tem um domínio bem definido.

# Para estabelecer este domínio, a técnica possui três alternativas possíveis.
# A primeira seria fixar a carga latente do primeiro construto observado em 1,
# e deixar os pesos dos demais construtos variarem.
# Na tabela, estes valores serão o `Estimate`.

# Outra solução possível seria fixar a variância do construto latente em 1,
# mantento a escala original das variáveis observadas.
# Na tabela, esta técnica será o valor `Std.lv`.

# A última solução seria padronizar tanto latentes quanto observadas.
# Neste caso, jogamos ambas para a variância unitária.
# Na tabela, esta técnica será o valor `Std.all`.
# Útil quando queremos interpretar os coeficientes em função do desvio padrão 
# (ex: Std.all = 0.66, significa que 1 desvio padrão no fator
# corresponde a 0.66 desvio padrão na observada).


# R2 dos construtos endógenos
print(inspect(fit,"r2"))
# OBS: Não será necessariamente utilizado.

lavaanPlot(model = fit)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Problemas típicos ----

## Caso Heywood (variância negativa) ----
# Vamos forçar carga muito alta & n pequeno para provocar problema.

set.seed(1)
model_sim_hey <- '
  F1 =~ 0.99*q1 + 0.99*q2 + 0.99*q3
'

data_hey <- simulateData(model_sim_hey, sample.nobs = 10)

model_est_hey <- 'F1 =~ q1 + q2 + q3'
fit_hey <- cfa(model_est_hey, data = data_hey, estimator = "MLR")

summary(fit_hey, fit.measures = TRUE, standardized = TRUE)

# Possíveis soluções:
# - Aumentar n;
# - Simplificar o modelo;
# - Ultra-Heywood (Laissez faire, laissez passer...);
# - Alternativas bayesianas.

##--##--##--##

## Identificação (modelo não identificável) ----
# Exemplo: latente definida por 2 indicadores e sem escala definida

# Modelo propositalmente mal identificado:
# dois fatores, cada um com dois indicadores, sem correlação 
# e sem fixar escala

model_bad <- '
  A =~ x1 + x2
  B =~ x3 + x4
'

model_bad_sim <- '
  A =~ 0.8*x1 + 0.8*x2
  B =~ 0.8*x3 + 0.8*x4
'

set.seed(1)
data_bad <- simulateData(model_bad_sim, sample.nobs = 200)
fit_bad <- cfa(model_bad, data = data_bad)

# Possíveis soluções (lembrar das regras heurísticas)
# - >= 3 indicadores por fator.
# - Fixar variância unitária (nem sempre funciona...)

model_bad <- '
  A =~ 1*x1 + x2
  B =~ 1*x3 + x4
'
fit_bad <- cfa(model_bad, data = data_bad)
# Neste caso, mesmo que:
fit_bad <- sem(model_bad, data = data_bad)

# Forçar pesos das observadas reflexivas para 1
# Em geral, irá funcionar. 
# Mas, não estaremos estimando (quase) nada!

model_bad <- '
  A =~ 1*x1 + 1*x2
  B =~ 1*x3 + 1*x4
'
fit_bad <- cfa(model_bad, data = data_bad)
summary(fit_bad)

# OBS: O aporte qualitativo aqui é fundamental.
# Se faz sentido teórico, então tudo bem!

# - Permitir correlação entre fatores (nem sempre funciona...)

model_bad <- '
  A =~ x1 + x2
  B =~ x3 + x4
  A ~~ B
'
fit_bad <- cfa(model_bad, data = data_bad)

# Problemas qualitativos: Se temos apenas 1 ou 2 variáveis 
# reflexivas ao fator, possivelmente não temos fator!
# -> relação direta da va. observada com a estrutura.

# Lembrem-se: O modelo estrutural "pede" sobre-identificação!

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Diagnósticos locais ----

## Modification Indices (MIs) ----

# Lembrando o modelo declarado anteriormente...

# model_est <- '
#   F1 =~ q1 + q2 + q3
#   F2 =~ q4 + q5 + q6
#   F2 ~ F1
# '

mi <- modificationIndices(fit)
mi[order(-mi$mi),]

# Tudo que NÃO especificamos no modelo,
# o lavaan irá tentar ao máximo respeitar pressupostos, como:

# - erros de medida independentes;
# - cargas não especificadas serão fixadas em 0;
# - covariâncias entre os erros não declarados serão fixados (teoricamente) em 0;
# - caminhos de regressão não incluídos não serão computados.

# Os índices de modificação irão sugerir, numericamente,
# especificações adicionais ao modelo, que (novamente, numericamente),
# melhorariam seu resultado — pois para cada parâmetro extra que a gente
# deixa de fixar e permite o lavaan estimar, o modelo ganha 1 grau de liberdade!

# Estas alterações PRECISAM fazer sentido qualitativo,
# mas servem como "argumento" para o pesquisador.
# (podemos, erroneamente, estar ajustando ruído!)

# Em outras palavras, MI é sugestão estatística — 
# só adicionar se há justificativa teórica.

# escala: Ele está tratando da diminuição do qui-quadrado do ajuste global,
# caso a estrutura proposta fosse incluída!

# Regra de "bolso": MI > 10.

# Exemplo prático: modelo com erro proposital (covariância omitida)

modelo_sim <- '
  F1 =~ 0.8*y1 + 0.7*y2 + 0.9*y3
  F2 =~ 0.7*y4 + 0.8*y5 + 0.9*y6
  F1 ~~ 0.9*F2
  y1 ~~ 0.36*y1
  y2 ~~ 0.51*y2
  y3 ~~ 0.19*y3
  y4 ~~ 0.36*y4
  y5 ~~ 0.36*y5
  y6 ~~ 0.36*y6
'

# Notem que existe uma correlação forte (0.9) entre F1 e F2
# na especificação populacional (geradora de dados).

modelo <- '
  F1 =~ y1 + y2 + y3
  F2 =~ y4 + y5 + y6
'

# Note que a correlação F1 ~~ F2 populacional não foi especificada.

set.seed(1)
dados <- simulateData(modelo_sim, sample.nobs = 300)

fit <- sem(modelo, data = dados)
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Índices de modificação
mi <- modificationIndices(fit)
mi[order(-mi$mi), ][c("lhs","op","rhs","mi")]

##--##--##--##

## Resíduos ----

# Matriz de resíduos (observed - predicted)
residuals(fit)$cov
# Queremos algo o mais próximo de 0 mesmo
# (o que deixou de ser explicado pelo nosso modelo!)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Valores ausentes (FIML) e estimação robusta (MLR) ----

# Criar alguns missings MCAR

# "Missing Completely at Random":
# (Desaparecido Completamente ao Acaso).
# Situação em que a ausência de dados é totalmente aleatória,
# sem qualquer relação com os dados observados
# ou com os próprios valores ausentes.

data_miss <- data
idx <- sample.int(nrow(data_miss), size = 0.1*nrow(data_miss))
data_miss$q1[idx] <- NA   # 10% missing MCAR em q1

df <- as.data.frame(data_miss)

# Ajustar com FIML (missing="fiml")
fit_miss <- sem(model_est,
                data = data_miss,
                estimator = "MLR",
                missing = "fiml")

summary(fit_miss, fit.measures = TRUE)

# O método "fiml" do lavaan utiliza a informação disponível SEM realizar imputação.
# Para um n suficiente e MCAR, costuma funcionar muito bem.
# Casos MAR (Missing at random) pode funcionar também, mas a teoria é mais dinâmica.
# Casos MNAR(Missing NOT at random) pode funcionar também, mas quase certamente os
# resultados serão viesados, caso existam.

# O estimador MLR (Máxima verossimilhança modificada para ser robusta) 
# pode ser usado quando temos dados contínuos com outliers
# ou leve não-normalidade, por exemplo.

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Dados ordinais (variáveis Likert) — WLSMV  ----

# Simulando itens ordinais (Likert 1:5) a partir de latentes (reflexivo!)

n <- 500
F1_lat <- rnorm(n)
y1 <- cut(0.8*F1_lat + rnorm(n,0,1), breaks = quantile(0.8*F1_lat + rnorm(n,0,1), probs = seq(0,1,by=.2)), labels = FALSE)
y2 <- cut(0.7*F1_lat + rnorm(n,0,1), breaks = quantile(0.7*F1_lat + rnorm(n,0,1), probs = seq(0,1,by=.2)), labels = FALSE)
y3 <- cut(0.9*F1_lat + rnorm(n,0,1), breaks = quantile(0.9*F1_lat + rnorm(n,0,1), probs = seq(0,1,by=.2)), labels = FALSE)
ord_data <- data.frame(y1 = factor(y1, ordered = TRUE), y2 = factor(y2, ordered = TRUE), y3 = factor(y3, ordered = TRUE))

model_ord <- 'F1 =~ y1 + y2 + y3'

fit_ord <- cfa(model_ord,
               data = ord_data,
               estimator = "WLSMV", # Padrão-ouro para dados Likert em SEM e AFC!
               ordered = c("y1","y2","y3"))

# Irá utilizar correlação policórica e introduzir thresholds. Não exige normalidade!

# Irá criar e ajustar thresholds tal que separam as categorias de escala likert
# para uma variável latente contínua assumidamente ~ N(0,1)

summary(fit_ord, fit.measures = TRUE, standardized = TRUE)

# Notem as métricas usuais -> discrepância não mensurável.

# Qui quadrado zero: Modelo saturado. \Sigma = S
# Não há como avaliar o ajuste global neste caso.

# Obs: Não interpretem os p-valores dos thresholds. Eles só testam a diferença de 0 (trivial).

# Em thresholds: Valor do ponto de corte na escala da variável latente subjacente y

# Em latente: "Um aumento de 1 dp em F1 aumenta em .656 o dp a v. reflexiva y2,
# levando a respostas mais altas na escala likert (+ correlações com y1 e y2)"

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Bootstrap ----

# Ajuste com bootstrap para inferência robusta

fit_boot <- sem(model_est, data = data, se = "bootstrap", bootstrap = 100)
summary(fit_boot, standardized = TRUE)

# Vamos fazer isso no nosso modelo final (demora mais a rodar; B = 100 é ínfimo).

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Multigrupos / Invariância (exemplo compacto) ----

# Inserindo grupos fictícios
data$group <- sample(c("A","B"), nrow(data), replace = TRUE)

# Configurando o modelo (Ajuste grupo A != Ajuste grupo B)
fit_mult0 <- cfa(model_est, data = data, group = "group")
summary(fit_mult0)

# Configurando o modelo (Ajuste grupo A = Ajuste grupo B)
fit_mult1 <- cfa(model_est, data = data, group = "group", group.equal = c("loadings"))
summary(fit_mult1)

# Comparar se faz sentido diferenciar grupos ou não, por exemplo (escolha de modelo)

fitMeasures(fit_mult0, c("cfi","rmsea","srmr"))
fitMeasures(fit_mult1, c("cfi","rmsea","srmr"))

# Regra de "bolso": ΔCFI < .01 -> irrelevante (ignorar numericamente os grupos;
# i.é: A = B).
fitMeasures(fit_mult0, c("cfi")) - fitMeasures(fit_mult1, c("cfi"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Construtos formativos ----

model_sim_form <- '
  # Parte reflexiva
  F1 =~ 0.8*q1 + 0.7*q2 + 0.9*q3
  F2 =~ 0.7*q4 + 0.8*q5 + 0.6*q6

  # Parte formativa
  Form <~ 0.6*f1 + 0.3*f2 + 0.1*f3

  # Saídas, buscando robustez
  ETA =~ 0.8*q7 + 0.7*q8 + 0.9*q9
  Y2  =~ 0.7*q10 + 0.6*q11 + 0.8*q12

  ETA ~ 0.4*Form + 0.5*F1
  Y2  ~ 0.45*Form + 0.3*F2
'

set.seed(1)

data_form <- simulateData(model_sim_form, sample.nobs = 2000)

model_est_form <- '
  F1 =~ q1 + q2 + q3
  F2 =~ q4 + q5 + q6

  Form <~ f1 + f2 + f3

  ETA =~ q7 + q8 + q9
  Y2  =~ q10 + q11 + q12

  ETA ~ Form + F1
  Y2  ~ Form + F2

  F1 ~~ F2
'

fit_form <- sem(model_est_form, data = data_form, estimator = "MLR")

# Formativo funcional em outro código (Vou deixar para mostrar ao final).

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Alguns erros comuns & como debugá-los ---- 

## Nomes colidindo (latentes vs observadas) ----
colnames(data) # criar latentes com nomes diferentes (sejam criativos e explícitos!)

## Ver tabela de parâmetros, identificar NAs ----
parameterEstimates(fit)

## Verificar se SEs são calculáveis ----
lavInspect(fit, "se")
# (Necessário, por exemplo, para bootstrap e inferências).

## Usar lavInspect para explorar matriz de informação ----
# Obs: vem muita coisa de uma vez; como o summary. Use com moderação.
lavInspect(fit, "information")

## Demais informações do modelo ----
?lavInspect

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Salvar gráficos para exportar ----

# semPlot salva direto em device gráfico; para PNG:
png("sem_plot.png", width = 2000, height = 1200, res = 150)
semPaths(fit,
         whatLabels = "std",
         edge.label.cex = 0.9,
         layout = "tree",
         nCharNodes = 7)
dev.off()

# Dúvidas e testes:
?semPaths
?lavaanPlot

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Resumo - funções mais utilizadas ----

# summary(fit, fit.measures=TRUE, standardized=TRUE)
# parameterEstimates(fit)
# fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
# modificationIndices(fit)
# residuals(fit)          # matrizes de resíduos
# fitted(fit)             # Sigma estimada
# lavPredict(fit)         # escores fatoriais estimados
# lavInspect(fit, "r2")   # R-squared
# lavInspect(fit, "npar") # número de parâmetros livres

# Utilizem o help!!

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#