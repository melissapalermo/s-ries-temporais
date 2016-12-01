install.packages("chron")
install.packages("h2o")
install.packages("lubridate")

library(lubridate)
library(h2o)
library(chron)

bikes = read.csv("C:/Users/Melissa/Desktop/Melissa/201402_status_data.csv") #banco de dados

 #só os da estacao de san francisco
x=rep(1:260491)
dados = bikes[x,]

dados$time = ymd_hms(dados$time) #formatei var time
dados$year = year(dados$time) #criei var ano
dados$month = month(dados$time) #criei var mes
dados$day = day(dados$time) #criei var dia
dados$hour = hour(dados$time) #criei var hora
dados$minute = minute(dados$time) #criei var minuto
dados$weekday = weekdays(dados$time) # var dia da semana
dados$time = NULL #apaguei a var time

#de int/num para fator
for (campo in c('station_id', 'month', 'day', 'weekday'))
  dados[[campo]] = factor(dados[[campo]])


set.seed(9)
rows = sample(nrow(dados), .7*nrow(dados))
train = dados[rows,]
valid = dados[-rows,]


#converter para o h2o
h2o.init()
training_frame = as.h2o(train) 
validation_frame = as.h2o(valid)


preds=c("day","month", "hour", "year", "minute") #variáveis preditoras

y1 = "bikes_available"

hp = list(hidden= list(c(512, 512, 512, 300), c(800, 650, 879, 120), c(980, 55, 980, 55),
c(400, 400, 400, 400), c(350, 512, 350, 512), c(55, 99), c(10, 10, 10, 10)
, c(512, 512, 512) , c(400, 400), c(300, 300), c(1500, 300)))

  
#fazer com mais camadas do relatorio

fitGrid = h2o.grid('deeplearning', x=preds, y=y1, training_frame=training_frame, validation_frame=validation_frame, hyper_params=hp)

model_ids = fitGrid@model_ids[[1]]
melhor_model = h2o.getModel(model_ids)

dias=c(7:30, 1:31, 1:31, 1:14)
mes=NULL
mes[1:24]=11
mes[25:56]=12
mes[57:88]=1
mes[89:100]=2

hora=c(7,8,9)
min=c(0,30)

y=list()
pre=list()
x=dados[32,]

for(i in 1:24){
  x[4]=2016 #ano
  x[5]=mes[i] #mes
  x[6]=dias[i] #dia
  x[7]=7 #hora
  x[8]=0 #min
  y[[i]]=x
  z=as.h2o(y[[i]])
  pre[[i]]=h2o.predict(melhor_model, z)
  
}

for(i in 25:56){
  x[4]=2016 #ano
  x[5]=mes[i] #mes
  x[6]=dias[i] #dia
  x[7]=7 #hora
  x[8]=0 #min
  y[[i]]=x
  z=as.h2o(y[[i]])
  pre[[i]]=h2o.predict(melhor_model, z)
  
}

for(i in 57:88){
  x[4]=2017 #ano
  x[5]=mes[i] #mes
  x[6]=dias[i] #dia
  x[7]=7 #hora
  x[8]=0 #min
  y[[i]]=x
  z=as.h2o(y[[i]])
  pre[[i]]=h2o.predict(melhor_model, z)
  
}


z=as.h2o(x)
pre=h2o.predict(melhor_model, z)


library(dygraphs)
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))