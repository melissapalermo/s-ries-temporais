#leitura banco de dados

#para ex 1: (import dataset)

vendas <- ex1

#para ex 2: (import dataset)
soi=ex22

#formatar data:
install.packages("lubridate")
library(lubridate)

vendas$V1 = dmy(vendas$V1) #acho que nao precisa disso


#formatar serie temporal:

serie_1=ts(vendas$V2, frequency = 12, start = c(2000, 1, calendar = TRUE)) 
serie_2=ts(soi, frequency = 12, start =c(1866, calendar = TRUE) )


#analises descritivas:
install.packages("dygraphs")
library(dygraphs)
dygraph(serie_1, main = "Volume de vendas no varejo de tecidos, vestuários e calçados") %>% dyRangeSelector(dateWindow = c("2000-01-01", "2011-12-01"))

dygraph(serie_2, main="Diferença de pressão normalizada entre Tahiti e Darwin") %>% dyRangeSelector

#salvar como html que continua dinâmico.
par(mfrow=c(1,2))
acf(serie_1, main="Função de Autocorrelação")#ex1
pacf(serie_1, main="Função de Autocorrelação Parcial")#ex1

par(mfrow=c(1,2))
pacf(serie_2, main="Função de Autocorrelação Parcial")#ex2
acf(serie_2, main="Função de Autocorrelação")#ex2

#ex1 precisa tirar a tendencia, mas nao sera estacionario, tem ciclos sugrito SARIMA(0,0,1)(0,0,1)12 porque é anual
#ex2 esta com cara de um AR(1)

# AR(1) com média diferente de zero: arima(serie_2, order = c(1,0,0)) 

# Análise dos resíduos do modelo2
modelo2 <- arima(serie_2, order = c(2,0,0))
par(mfrow=c(1,2))
acf(residuals(modelo2))
pacf(residuals(modelo2))