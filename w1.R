x<-qt(0.95,df=28)
#-qt(0.05,df=28)
y1<-2-5.48*x
y2<-2+5.48*x
round(y1,digits = 2)
round(y2,digits = 2)
qt(0.95,df=28)
qt(0.05,df=28)

z1<- -3-(0.5/(sqrt(30)))*(qt(0.95,df=27))
round(z1,digits = 2)
z2<- -3+(0.5/(sqrt(30)))*(qt(0.95,df=27))
round(z2,digits=2)

t<-round(pchisq(9,df=10),digits = 2)
t
qplot(rchisq(100,df=10))
qt(0.95,df=27)


library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("devtools")
library("vcd") #пакет для построения графиков для качественных/категориальных переменных mosaic()
#library("rlms")

#devtools::install_github("bdemeshev/rlms")



help(pnorm)

pnorm(9, mean=7,sd=2)

h<-swiss
head(h,n=10)
help(head)

z<-lm(data=h,Fertility~Agriculture+Catholic)

coef(z)
summary(z)
View(z)

library("ggplot2") 
help(diamonds)
df <- diamonds
g<-max(df$price)

g
help(table)
p<-table(df$cut)
p
help(lm)



model <- glm(data=df, price ~ carat)
summary(model)
model0<-lm(data=df,price~0+carat)
summary(model0)


#сохранение результатов
stuff<-list(data=df,model=model)
saveRDS(file="mydata.RDS", stuff)
#csv - comma separated value
f1<-read.csv("deep.csv",sep=";",dec=",",header=TRUE)
#glimpse()-не работает почему то
head(f1)


help(diamonds)

model1<-lm(data=df, price ~ carat+x+y+table)
coef(model1)
summary(model1)

model2<-lm(data=df, price ~ carat+x+y)
confint(model2,level=0.9)#доверительный интервал
help(confint)




model <- lm(data=df, price ~ carat+table)

sjp.lm(model)
head(df,n=10)
model0 <- lm(data=df, price ~ carat+x+y+z)
coef(model0)
summary(model0)
model0 <- lm(data=df, price ~ y)
model0 <- lm(data=df, price ~ carat+x+y)
y0<-confint(model0,level=0.9)
str(y0)

help(diamonds)





#-----01.06.2016

h<-diamonds
qplot(data=h,carat,price)#зависимость нелинейна
bg<-qplot(data=h,log(carat),log(price))
bg+geom_hex() #светлые пятна-стандартные формы огранки
glimpse(h)


mutate_each(data=..,"factor",#список переменных)
head(h)
qplot(data=h,depth,fill=color)
qplot(data=h,depth,fill=color,position="dodge")
table(h$cut)
table(h$color)
table(h$clarity)#прозрачность
table(h$carat)
qplot(data=h,log(depth),fill=cut,position="dodge")
q2<-qplot(data=h,log(depth),fill=cut,geom="density",alpha=0.5)
q2+facet_grid(clarity~color)
q2+facet_grid(~color)

waldtest()#для оценки значимоссти различия ограниченной и неограниченной моделей

model_0<-lm(data=h,log(carat)~log(price))
bg+stat_smooth(method="lm")
bg+stat_smooth(method="lm")+facet_grid(~cut) # facet_grid дает возможность оценить зависимость размера брилианта(лог) от цены(лог) с разбивкой по степени огранки 
bg+aes(col=color)+stat_smooth(method="lm")+facet_grid(~cut) #через aes выделяем цветом "цвет" бриллианта

h$nocut<-memisc::recode(h$cut,"Fair"<-"Ideal","Ideal"<-"Fair")# перекодировка значений
table(h$nocut)
table(h$cut)
mtable() #выдает значения R2_adj, AIC BIC
resettest() #тест рамсея

resettest(model_0)

nrow(h)
mtable(model_0)
model_1=lm(data=h,log(price)~log(carat))
mtable(model_1)
model_2=lm(data=h,price~carat+y)
summary(model_2)
waldtest(model_2)
model_3=lm(data=h,price~carat+clarity)
model_4=lm(data=h,price~carat)
mtable(model_3,model_4)
mtable(model_4)
model_5=lm(data=h,price~carat+depth)
model_6=lm(data=h,price~carat+depth+cut)
mtable(model_4,model_5,model_6)
waldtest(model_5,model_6)
resettest(model_5)
resettest(model_6)

qplot(data=h,log(price),fill=clarity,geom="density",alpha=0.5)+facet_wrap(~clarity)
qplot(data=h,log(carat),log(price),color=clarity)+facet_wrap(~cut)
qplot(data=h,carat,price,color=clarity)+facet_wrap(~cut)
summary(h)
str(h)

help(read.table)
station20<-read.table(file.choose(), sep=";" , header=TRUE,fill=TRUE)
head(station20)
model_st20<-lm(data=station20,freq~dr*zn1_)
summary(model_st20)

resettest(model_st20)
qplot(data=station20, freq)
table(station20$freq)



#03.06--мультиколлинеарность

library("HSAUR")
library("plyr")
library("psych")
library("lmtest")
library("glmnet")
library("ggplot2")
library("car")

library("fmsb")


h<-cars
h<-mutate(h,speed2=speed^2,speed3=speed^3)
model<-lm(data=h,dist~speed)
model_mk<-lm(data=h,dist~speed+speed2+speed3)
summary(model_mk)#каждый из speed не значим,но F-stat p-value: 3.074e-11 - регрессия вцелом значима, т.е. зависимость от какого регрессора есть  

#пример мультиколиннеарности

vif(model_mk) #коэффициенты вздутия дисперсии
x0<-model.matrix(data=h,dist~0+speed+speed2+speed3)
head(x0)
cor(x0)# correlation

#используем модель для прогнозирования-мультиколлинеарность не проблема-прооверим?
nd<-data.frame(speed=10,speed2=100,speed3=1000)
#доверительные интервалы для прогнозных значений
predict(model,newdata=nd,interval="prediction")
predict(model_mk,newdata=nd,interval="prediction")#доверительные интервалы не сильно изменились

#дов интервалы для коэффициентов
confint(model)
confint(model_mk)



#ридж /лассо
y<-h$dist
x0<-model.matrix(data=h,dist~0+speed+speed2+speed3)

#lasso
lambdas<-seq(50,0.1,length=30)#vector of lambdas
m_lasso<-glmnet(x0,y,alpha=1,lambda=lambdas) #дял ридж - альфа=0
m_coef<-coef(m_lasso,s=c(0.1,1)) # чему равны коэффициенты для лямбда 0.1 и 1
m_coef



#PCA

h<-heptathlon
head(h) #javelin-метание копья


h<-select(h,-score) # select из пакета dplyr и он должен был удалить столбец score, но пакет не ставится

h$score<-NULL # но можно и так :)
head(h)
describe(h) # разные единицы измерения, разный разброс-НУЖНА СТАНДАРТИЗАЦИЯ!


h.pca<-prcomp(h,scale=TRUE)

pca1<-h.pca$x[,1]# первая гл компонента, новые x
v1<-h.pca$rotation[,1] #веса 1 гл компоненты, коэффициенты
v1

summary(h.pca) # разложение по компонентам

plot(h.pca)
biplot(h.pca,xlim=c(-1,1))

head(h.pca)



#06.06 - промежуточный экзамен

h<- ChickWeight
head(h)
table(h)
table(h$time)
c1<-h[c("Time", "weight")]
c2<-c1["Time"]==10 #создаем массив true false по условию Time=10
c3<-c1[c2,]#выдергиваем из исходного массива с1 только те поля, где True
table(c3$Time)
round(mean(c3$weight),digits = 2)

#цыплята какой диеты в среднем больше всего весят на 21 день
c_1<-h["Time"]==21
c_2<-h[c_1,]
head(c_2)
table(c_2$Diet)
help(mean)
aggregate(c_2["weight"],list(c_2$Diet),mean)


#оценить регрессию веса цыпленка на его возраст и тип диеты, вытащить r2
model_chick<-lm(data=h,weight~Time+Diet)
summary(model_chick)
help(lm)
round(summary(model_chick)$r.squared,digits = 2)

help(diamonds)
h<-diamonds
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),fill=cut)+facet_grid(~cut)
qplot(data = diamonds, price,fill=cut)+facet_grid(~clarity)
qplot(data = diamonds, price,fill=cut)+facet_wrap(~clarity)


model_price<-lm(data=h,price~carat+table+x+y+z+depth)
summary(model_price)
nrow(h)
model_price2<-lm(data=h,price~carat+table+x+y+depth)
confint(model_price2,level=0.9)


#PCA
h<-mtcars
nrow(mtcars)
model_cars<-lm(data=h,mpg~disp+hp+wt)
vif(model_cars)
f<-model.matrix(data=h,mpg~0+disp+hp+wt)
typeof(f)
pc_cars<-prcomp(f,scale=TRUE)
round(max(pc_cars$x[,1]),digits = 2)
mpg<-h$mpg
model_cars_pc<-lm(h$mpg~pc_cars$x[,1]+pc_cars$x[,2])
summary(model_cars_pc)
model_cars_pc2<-lm(h$mpg~pc_cars$x[,1]+pc_cars$x[,2]+pc_cars$x[,3])
dif<-summary(model_cars_pc)$r.squared-summary(model_cars_pc2)$r.squared
dif


library(Ecdat)
food<-BudgetFood
head(food)
model_food<-lm(data=food,wfood~totexp+size)
fd<-data.frame(totexp=700000,size=4)
predict(model_food,newdata=fd,interval="prediction",level=0.9)
resettest(model_food)

model_food2<-lm(data=food,wfood~totexp+size+sex)
food<-na.omit(food)
model_food<-lm(data=food,wfood~totexp+size)
model_food2<-lm(data=food,wfood~sex*(totexp+size))
summary(model_food2)
waldtest(model_food,model_food2)


