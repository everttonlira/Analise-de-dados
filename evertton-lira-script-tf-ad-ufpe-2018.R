# Carregando pacotes

if(require(rio) == F) install.packages('rio'); require(rio) # Algumas versões do R pedem este comando duas vezes
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(car) == F) install.packages('car'); require(car)
if(require(MASS) == F) install.packages('MASS'); require(MASS)
if(require(dotwhisker) == F) install.packages('dotwhisker'); require(dotwhisker)
if(require(broom) == F) install.packages('broom'); require(broom)

# Carregar banco de dados
competicao <- import(file = "evertton-lira-bd-td-ad-ufpe-2018.dta")

# Rodando estatísticas descritivas
summary(competicao)

# Gráficos de análises descritivas

# Plot de log(PIB) e IDH x PNS
qplot(log(PIB), PNS, data = competicao)
qplot(IDH, PNS, data = competicao)

# Distribuição do PIB
qplot(PIB, data = competicao, geom = "histogram")

# Distribuição do PIB após tansformação logarítmica 
qplot(log(PIB), data = competicao, geom = "histogram")

# Violin de IDEOL x PNS
## Converter variável X para factor
competicao$IDEOL <- as.factor(competicao$IDEOL)
library(ggplot2)
# Criar histograma de violino com cores
violin2<-ggplot(competicao, aes(x=IDEOL, y=PNS, fill=IDEOL)) +
  geom_violin(trim=FALSE)
violin2
#Adicionar média
violin2 + stat_summary(fun.y=mean, geom="point", shape=23, size=2)

# Violin de CANDLEG X IDEOL
violin3 <-ggplot(competicao, aes(x=IDEOL, y=CANDLEG, fill=IDEOL)) +
  geom_violin(trim=FALSE)
violin3
# Adicionar média
violin3 + stat_summary(fun.y=mean, geom="point", shape=23, size=2)

# Pressuostos do modelo

# Pressuposto 1: o modelo de regressão é linear nos parâmetros

# Rodar os modelos
modelo1 <- lm(PNS ~ CANDLEG + CANDEXEC + IDH + IDEOL, data=competicao)
modelo2 <- lm(PNS ~ CANDLEG + CANDEXEC + log(PIB) + IDEOL, data=competicao)
modelo3 <- lm(PNS ~ CANDLEG + CANDEXEC + dummyesq*IDH, data=competicao)
modelo4 <- lm(PNS ~ CANDLEG + CANDEXEC + dummyesq*log(PIB), data=competicao)

# Pressuposto 2: a média dos resíduos é 0
mean(modelo1$residuals)
mean(modelo2$residuals)
mean(modelo3$residuals)
mean(modelo4$residuals)

# Pressuposto 3: homoscedasticidade dos resíduos ou variância igual
par(mfrow=c(2,2))
## necessário rodar o modelo depois
plot(modelo1)
plot(modelo2)
plot(modelo3)
plot(modelo4)
## transformação da variável
require(MASS)
trans_var1 <- boxcox(modelo1, lam=seq(-1, 1, 1/10), plotit = TRUE)
trans_var2 <- boxcox(modelo2, lam=seq(-1, 1, 1/10), plotit = TRUE)
trans_var3 <- boxcox(modelo3, lam=seq(-1, 1, 1/10), plotit = TRUE)
trans_var4 <- boxcox(modelo4, lam=seq(-1, 1, 1/10), plotit = TRUE)
par(mfrow=c(2,2))
trans_var1
trans_var2
trans_var3
trans_var4
PNSnovo <- (competicao$PNS^(0.98)-1)/0.98
#Rodando novos modelos com a VD transformada
modelo1 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + IDH + IDEOL, data=competicao)
modelo2 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + log(PIB) + IDEOL, data=competicao)
modelo3 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + dummyesq*IDH, data=competicao)
modelo4 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + dummyesq*log(PIB), data=competicao)
# Verificando novamente os resíduos
par(mfrow=c(2,2))
plot(modelo1)
plot(modelo2)
plot(modelo3)
plot(modelo4)

# Pressuposto 4: normalidade (verificado nos gráficos anteriores pelo teste Q-Q)

# Pressuposto 5: os resíduos não são autocorrelacionados
library(ggplot2)
par(mfrow=c(2,2))
acf(modelo1$residuals)
acf(modelo2$residuals)
par(mfrow=c(2,2))
acf(modelo3$residuals)
acf(modelo4$residuals)

# Pressuposto 6: O número de observações é maior que o número de variáveis

# Pressuposto 7: o modelo está bem especificado

# Pressuposto 8: Sem multicolinearidade perfeita
library(car)
vif(modelo1)
vif(modelo2)
vif(modelo3)
vif(modelo4)

# Regressões
modelo1 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + IDEOL + IDH, data = competicao)
modelo2 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + IDEOL + log(PIB), data = competicao)
modelo3 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + IDH*dummyesq, data = competicao)
modelo4 <- lm(PNSnovo ~ CANDLEG + CANDEXEC + log(PIB)*dummyesq, data = competicao)
# Coeficientes
summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)
# Carregar pacotes
library(dotwhisker)
library(broom)
# Gerar gráficos
dwplot(modelo1, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
dwplot(modelo2, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
dwplot(modelo3, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
dwplot(modelo4, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))


