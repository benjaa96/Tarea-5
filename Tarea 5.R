#Tarea 5 


#Pregunta 2
if(!require("pacman")) install.packages("pacman")
library(tidyverse)
library(tidyquant)
library(ggthemes)
library(ggplot2)
library(foreign)
library(pdfetch)
library(moments)
library(gridExtra)

#Pregunta 2
#a
aaple <- tq_get("AAPL",
                get = "stock.prices",
                from = "2000-01-01",
                to = "2018-08-31",
                periodicity = "monthly")

msft <- tq_get("MSFT",
               get = "stock.prices",
               from = "2000-01-01",
               to = "2018-08-31",
               periodicity = "monthly")

ap <- function(t){
  aaple_o <- select(aaple, open)
  aaple_c <- select(aaple, close)
  fecha <- select(aaple, date)
  n <- (aaple_c[t,]-aaple_o[t,])/aaple_o[t,]
  aaple1 <- as.matrix(aaple_o[t,])
  Skew <- (sum((aaple1 - mean(aaple1))^3)/length(aaple1))/(sum((aaple1 - mean(aaple1))^2)/length(aaple1))^(3/2)
  Kur <- length(aaple1) * sum((aaple1 - mean(aaple1))^4)/(sum((aaple1 - mean(aaple1))^2)^2)
  Jb <- (length(aaple1)/6) * (Skew^2 + 0.25 * ((Kur - 3)^2))
  pval <- 1 - pchisq(Jb, df = 2)
  n2 <- retorno_fecha <- cbind(fecha[t,], n)             
  freq_Acum <- cumsum(n$close)
  i2 <- data.matrix(freq_Acum, rownames.force = NA)
  retorno_fecha2 <- cbind(fecha[t,], i2)
  retorno_fecha <- cbind(fecha[t,], n)
  u <- log(aaple_c[t,]/aaple_o[t,])
  retorno_fecha3 <- cbind(fecha[t,], u)                  
  return(list(Retorno_variación=n2,
              Retorno_log=retorno_fecha3,
              ggplot(retorno_fecha2, aes(x = date, y = i2)) + geom_line() ,
              ggplot(retorno_fecha, aes(x = date, y = close)) + geom_line(), 
              Jarque_Bera=if(pval < 0.05){"Se rechaza la hipotesis nula"}else{"No se rechaza la hipotesis nula"})) 
}

y(1:110)






#pregunta 3

#Podemos ver que en el modelo que no que omite la variable relevante no hay presencia de sesgo ya que el valor esperado del estimador b1 
# es igual a 2.5 por lo que el estimador es insesgado. por otro lado podemos ver que para el modelo que omite la variable relevante el valor esperado del estimador
#esta alejado del valor del parametro poblacional por lo que podemos ver presencia de sesgo. 

#Tambien podemos notar que en el modelo que omite la variable relevante a medida que la muestra
#aumenta de tamaño el sesgo NO desaparece, sin embargo, la varianza del estimador disminuye. 
#3.A

set.seed(1234)
Rep = 10000
betas = matrix(NA ,nrow = Rep, ncol = 8)

b0 = 2
b1 = 2.5
b2 = 1

N = c(50, 100, 500, 1000)

for (j in 1:length(N)){
  x1 = rnorm(N[j],20,1)
  x2 = (0.8*x1) + rnorm(N[j],0,1)
  x2.2 = runif(N[j],0,1)
  
  for (i in 1:Rep) {
    
    u = rnorm(N[j],0,1)
    v = b2*x2 + u
    
    YSESGADA = b0 + b1*x1 + v
    
    YNOSESGADA = b0 + b1*x1 + b2*x2 + u
    
    REGRESION_SESGADA = lm(YSESGADA ~ x1)
    
    betas[i,j] = REGRESION_SESGADA$coef[2]
    
    
    REGRESION_NOSESGADA = lm(YNOSESGADA ~ x1 + x2)
    
    betas[i,j+4] = REGRESION_NOSESGADA$coef[2]
    
  }
}

betas_data = data.frame(betas)

apply(betas_data, 2, mean)



#VARIANZAS

apply(betas_data, 2, var)


#3.B
library(gridExtra)

REGSESGADA50 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,1],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,1]), sd=sd(betas_data[,1])),
                geom = "line", color="blue", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1 con modelo incompleto  con N=50") +xlab("Estimador b1") +
  theme_economist_white()

REGSESGADA50





REGSINSESGO50 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,5],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,5]), sd=sd(betas_data[,5])),
                geom = "line", color="blue", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=50") +xlab("Estimador b1") + theme_economist_white()

REGSINSESGO50




REGSESGADA100 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,2],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,2]), sd=sd(betas_data[,2])),
                geom = "line", color="BLUE", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1 con modelo incompleto  con N=100") +xlab("Estimador b1") +
  theme_economist_white()


REGSESGADA100


REGSINSESGO100 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,6],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,6]), sd=sd(betas_data[,6])),
                geom = "line", color="blue", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=100") +xlab("Estimador b1") +
  theme_economist_white()


REGSINSESGO100




REGSESGADA500 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,3],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,3]), sd=sd(betas_data[,3])),
                geom = "line", color="BLUE", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1 con modelo incompleto  con N=500") +xlab("Estimador b1") +
  theme_economist_white()

REGSESGADA500



REGSINSESGO500 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,7],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,7]), sd=sd(betas_data[,7])),
                geom = "line", color="BLUE", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=500") +xlab("Estimador b1") +
  theme_economist_white()

REGSINSESGO500




REGSESGADA1000 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,4],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,4]), sd=sd(betas_data[,4])),
                geom = "line", color="BLUE", size=1) +
  ylab("Densidad") +ggtitle("Distribucion estimador b1 con modelo incompleto  con N=1000") +xlab("Estimador b1") +
  theme_economist_white()

REGSESGADA1000



REGSINSESGO1000 = ggplot(betas_data) + 
  geom_histogram(aes(betas_data[,8],y=..density..), col="black", bins = 50) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data[,8]), sd=sd(betas_data[,8])),
                geom = "line", color="BLUE", size=1) +
  ylab("Densidad") +ggtitle("Distribucion del estimador b1 modelo completo con N=1000") +xlab("Estimador b1") +
  theme_economist_white()

REGSINSESGO1000


grid.arrange(REGSESGADA50, REGSINSESGO50,REGSESGADA100, REGSINSESGO100,REGSESGADA500, REGSINSESGO500,REGSESGADA1000, REGSINSESGO1000,  nrow=4, ncol=2)


#3.C

#Si ahora x2 se distribuye uniformemente entre 0 y 1 podemos notar que se elimina el sesgo y el valor esperado 
#de los estimadores tiende al valor del parametro poblacional (2.5) en ambos modelos, tambien cabe
#mencionar que los betas en ambos modelos aun siguen una distribucion normal. Finalmente al aumentar el tamaño de la 
#muestra al igual que en los resultados anteriores la varianza de los estimadores disminuye. 



set.seed(1234)
Rep = 10000
betas = matrix(NA ,nrow = Rep, ncol = 8)

b0 = 2
b1 = 2.5
b2 = 1

N = c(50, 100, 500, 1000)

for (j in 1:length(N)){
  x1 = rnorm(N[j],20,1)
  x2 = runif(N[j],0,1)
  x2.2 = (0.8*x1) + rnorm(N[j],0,1)
  
  for (i in 1:Rep) {
    
    u = rnorm(N[j],0,1)
    v = b2*x2 + u
    
    Y_CON_SESGO = b0 + b1*x1 + v
    
    Y_SIN_SESGO = b0 + b1*x1 + b2*x2 + u
    
    REGRESION_CON_SESGO = lm(Y_CON_SESGO ~ x1)
    
    betas[i,j] = REGRESION_CON_SESGO$coef[2]
    
    
    REGRESION_SIN_SESGO = lm(Y_SIN_SESGO ~ x1 + x2)
    
    betas[i,j+4] = REGRESION_SIN_SESGO$coef[2]
    
  }
}

betas_data_unif = data.frame(betas)

apply(betas_data_unif, 2, mean)



#varianzas
apply(betas_data_unif, 2, var)


#GRAFICOS CON x2 ∼ U [0, 1]



REG_U_50_CS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,1],y=..density..), col="black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,1]), sd=sd(betas_data_unif[,1])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1 con modelo incompleto  con N=50", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_50_CS



REG_U_50_SS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,5],y=..density..), col="black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,5]), sd=sd(betas_data_unif[,5])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=50", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_50_SS




REG_U_100_CS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,2],y=..density..), col="black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,2]), sd=sd(betas_data_unif[,2])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1  con modelo incompleto  con N==100", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()


REG_U_100_CS



REG_U_100_SS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,6],y=..density..), col="black", bins = 25) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,6]), sd=sd(betas_data_unif[,6])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=100", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()


REG_U_100_SS




REG_U_500_CS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,3],y=..density..), col="black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,3]), sd=sd(betas_data_unif[,3])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion estimador b1 con modelo incompleto  con N=500", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_500_CS


REG_U_500_SS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,7],y=..density..), col="black", bins = 30) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,7]), sd=sd(betas_data_unif[,7])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") + ggtitle("Distribucion del estimador b1 modelo completo con N=500", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_500_SS


REG_U_1000_CS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,4],y=..density..), col="black", bins = 25) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,4]), sd=sd(betas_data_unif[,4])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") +ggtitle("Distribucion estimador b1 con modelo incompleto  con N=1000", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_1000_CS


REG_U_1000_SS = ggplot(betas_data_unif) + 
  geom_histogram(aes(betas_data_unif[,8],y=..density..), col="black", bins = 25) +
  stat_function(fun = dnorm, args = list(mean=mean(betas_data_unif[,8]), sd=sd(betas_data_unif[,8])),
                geom = "line", color="red", size=1) +
  ylab("Densidad") +ggtitle("Distribucion del estimador b1 modelo completo con N=1000", subtitle ="con  x2 ∼ U [0, 1]") +xlab("Estimador b1") +
  theme_economist_white()

REG_U_1000_SS

grid.arrange(REG_U_50_CS, REG_U_50_SS,REG_U_100_CS, REG_U_100_SS,REG_U_500_CS,REG_U_500_SS,REG_U_1000_CS, REG_U_1000_SS, nrow=4, ncol=2)