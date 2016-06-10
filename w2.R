f<-function(x){
  res<-x^2
  return (res)
}

f(2)


fs<-function(x,stepen=2){
  res<-x^stepen
  return (res)
}

fs(4,stepen=0.5)


for (i in 5:10){
  k<-i^2
  cat("i=",i, ",i^2=",k,"\n")
} 

#i может быть не только числовым, например можно for(fname=c("data0.csv","data1.csv"))
#rbind()-аналог set в sase

library ("sandwich") #vcovHC vcovHAC
library ("broom")
library ("car")
library ("plyr")
library ("lmtest")
library ("ggplot2")

augment(model,data=) #остатки модели .-resid /-.fitted прогнозноне значение


vcovHC(model)#оценки ковариационной матрицы гетроскедастичности

coeftest(model, vcov.=vcovHC(model)) # тест для коэф b с учетом скорректированной матрицы ковариации

bptest(model) #тест уайта

bptest(model,data=,varformula = ~totsp+I(totsp^2)) #тест уайта,классика, с учетом 2 степеней
#он же по другому записанный
bptest(model,data=,varformula = ~poly(totsp,2)) #тест уайта
#тест гольдфринда-квиндта
gqtest(model,order.by=~totsp,data=, fraction=0.2) #выкидывание 20% проц 



#борьба с гетероскедастичностью логарифмированием
# в начале нужно теоретическое предположение о наличии гетероскедастичности и использование робастных поправок, только потом тестирование

#автокорреляция
library("devtools")
install_github("dqrtwo/broom")
install_github("cran/bstats")
install_github("bdemeshev/rusquant")
install_github("bdemeshev/sophisthse")


library("lubridate")#работа с датами
library("bstats")# тесты
library("zoo")#временные ряды
library("xts")#еще
library ("sandwich") #vcovHC vcovHAC
library ("broom")
library ("car")#тесты
library ("dplyr")
library ("lmtest")#тесты
library ("ggplot2")



library ("quantmod")# данные google 
library ("rusquant")# данные finam.ru
library ("sophisthse")# данные sophist.hse.ru
library("Quandl")#данные с quandl

x<-c("2015-10-02","2016-01-10")
y<-ymd(x)
y+days(20)
y+months(20)
y-years(20)

vignette("lubridate")

x<-rnorm(5)
d<-c("2000-01-01")
y<-ymd(d)+days(0:4)
ts<-zoo(x,order.by=y)
ts
lag(ts,1)
diff(ts)
z<-(lag(ts,-3)+lag(ts,-2)+lag(ts,-1))/3
z
lag(ts,-3)

ts2<-zooreg(x,start=as.yearqtr("2014-01"),freq=4)#квартальные данные по 4 кв
ts2
ts3<-zooreg(x,start=as.yearmon("2014-01"),freq=12)#месячные показатели
ts3

data("Investment")
help("Investment")
start(data)
end(data)
time(data)#инфа по времени распределения
coredata(data)#сами данные без времени


#для заполнения пропусков
#среднее
na.aprox(data)

#предыдущим
na.locf(data)

#источники данных
#finance.google.com
#finance.yahoo.com
#quandl.com
#finam.ru
#sophist.hse.ru

b<-Quandl("FRED/GNP")

#finance.google.com

Sys.setlocale("LC_TIME","C")
#получение данных
getSymbols(Symbols="AAPL",from="2010-01-01",to="2014-02-03",src="google")#акции apple c гугл /src=yahoo, данные с яху
head(AAPL)
tail(AAPL)

#графики
autoplot(AAPL)
autoplot(AAPL[,1:4])
autoplot(AAPL[,1:4],facets=NULL)

chartSeries(AAPL) #внизу объемы торгов








#тест
library ("Ecdat")
h0<-Griliches
model_test6<-lm(data=h0,lw80~age80+iq+school80+expr80)
covv<-vcov(model_test6)
covv2<-vcovHC(model_test6)
head(covv)
covv[3,4]
abs(covv[3,4]-covv2[3,4])

help(vcovHC)

covHC0<-vcovHC(model_test6,type = "HC0")
covHC1<-vcovHC(model_test6,type = "HC2")
covHC2<-vcovHC(model_test6,type="HC4")
covHC3<-vcovHC(model_test6,type="HC5")
ocenki<-c(covHC0[4,4],covHC1[4,4],covHC2[4,4],covHC3[4,4])
min(ocenki)

bptest(model_test6,data=h0,varformula=~iq)
gqtest(model_test6,order.by=~age80,data=h0, fraction=0.2)
