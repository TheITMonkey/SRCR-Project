library(neuralnet)
library(hydroGOF)
library(arules)
library(leaps)

set.seed(10)

# ler dataset do ficheiro csv
dados <- read.csv("C:\\Users\\Ricardo\\Desktop\\Universidade\\SRCR\\TP3\\bank-additional\\bank-additional-full.csv", header=TRUE, sep=";")

dados$job <- as.numeric(dados$job)
dados$marital <- as.numeric(dados$marital)
dados$education <- as.numeric(dados$education)
dados$default <- as.numeric(dados$default)
dados$housing <- as.numeric(dados$housing)
dados$loan <- as.numeric(dados$loan)
dados$contact <- as.numeric(dados$contact)
dados$month <- as.numeric(dados$month)
dados$day_of_week <- as.numeric(dados$day_of_week)
dados$poutcome <- as.numeric(dados$poutcome)
dados$y <- as.numeric(dados$y)

# mostrar a cabeca do dataset
head(dados)

# dividir os dados iniciais em casos para treino
treino <- dados[1:700, ]

# dividir os dados iniciais em casos para teste
teste <- dados[701:41188, ]

# definicao das camadas de entrada e saida da RNA
formulatp3 <- y ~ age + education + duration
#formulatp3 <- y ~ month + duration + pdays + nr.employed
#formulatp3 <- y ~ month + duration + pdays + nr.employed + euribor3m

# treinar a rede neuronal para usar as variaveis age, education e duration como input e y como output
rnabanco <- neuralnet( formulatp3, treino, hidden=c(3),lifesign = "full", linear.output = TRUE, threshold = 0.1)

# desenhar rede neuronal
plot(rnabanco, rep = "best")

# definir variaveis de input para teste
teste.01 <- subset(teste, select = c("age", "education", "duration"))
#teste.01 <- subset(teste, select = c("month", "duration", "pdays", "nr.employed"))
#teste.01 <- subset(teste, select = c("month", "duration", "pdays", "nr.employed", "euribor3m"))

# testar a rede com os novos casos
rnabanco.resultados <- compute(rnabanco, teste.01)

# imprimir resultados
resultados <- data.frame(atual = teste$y, previsao = rnabanco.resultados$net.result)

# imprimir resultados arredondados
resultados$previsao <- round(resultados$previsao, digits=8)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

# calcular o RMSE
rmse(c(teste$y),c(resultados$previsao))

0# seleção de variáveis mais significativas
funcao <- y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + euribor3m + nr.employed
selecao <- regsubsets(funcao,dados,nvmax=4)
summary(selecao)

selecao <- regsubsets(funcao,dados,method="backward")
summary(selecao)

