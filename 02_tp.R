

# Home --------------------------------------------------------------------

#1:03 interptreacion de modleo posson
#1:21 riesgo relativo

library(tidyverse)
library(skimr)
library(tidymodels)
library(jtools)

Data_set_modelos <- readRDS("Data_set_modelos.rds")


dataset <- Data_set_modelos %>% 
  filter(pliegue == 24) %>% 
  select(-c(pliegue, delitos_next_3))


# EDA general -------------------------------------------------------------

dataset$delitos %>% table()

dataset %>% skim()


# EDA seleccion de variables ----------------------------------------------

Receta <- recipe(delitos ~ ., data = dataset) %>% 
  
  step_mutate(bancos = atm + bancos) %>% 
  
  step_select(-atm) %>% 
  
  step_nzv(all_predictors(), freq_cut = 75/25)


dataset_def <- Receta %>% prep() %>% bake(dataset)

dataset_def %>% skim()


# EDA target --------------------------------------------------------------







# Modelado ----------------------------------------------------------------

#prueba <- dataset_def %>% select(-c(id, lat, long))
prueba <- dataset_def %>% select(delitos,
                                 bancos                             ,
                                 meses_sin_delitos                  ,
                                 colectivo                          ,
                                 garajes                            ,
                                 educativos                         )


#Poisson
m_poisson <-glm(delitos~.,family=poisson(link="log"),data=prueba)
summary(m_poisson)


check_model(m_poisson)

check_zeroinflation(m_poisson)

check_overdispersion(m_poisson)





# Comparacion de modelo ---------------------------------------------------

#m_poisson <- m_poisson %>% select_parameters()


#BN
library(MASS)
m_binomialn <-glm.nb(delitos~.,link=log, data = prueba)
summary(m_binomialn)

#quasipoisson
m_qpoisson <- glm(delitos~.,quasipoisson(link = "log"), data = prueba)


# Comparacion de modelos con tablas

table_poisson <- function(y, target){
  
  #options(digits=3)
  
  counts <- unique(target$delitos) %>% sort()
  
  tabla <- tibble(counts = counts) %>% 
    mutate(table1p = map_dbl(counts,function(.x){sum(dpois(.x,y))}))
  
  
  actual <- data.frame(table(target$delitos))[,2]
  diff <- actual-tabla$table1p
  PearsonG = sum(diff*diff/tabla$table1p)
  
  #cbind(table1p,PearsonG)}
  
  list(tabla = tabla, PearsonG = PearsonG)
}

table_quasipoisson <- table_poisson(m_qpoisson$fitted.values,prueba)

table_poisson <- table_poisson(m_poisson$fitted.values, prueba)

table_binomialn <- function(y, target){
  
  options(digits=3)
  
  counts <- unique(target$delitos) %>% sort()
  
  tabla <- tibble(counts = counts) %>% 
    mutate(table1p = map_dbl(counts,function(.x){sum(dnbinom(.x,size=y$theta,mu=y$fitted))}))
  
  
  actual <- data.frame(table(target$delitos))[,2]
  diff <- actual-tabla$table1p
  PearsonG = sum(diff*diff/tabla$table1p)
  
  #cbind(table1p,PearsonG)}
  
  list(tabla = tabla, PearsonG = PearsonG)
}

table_binomialn <- table_binomialn(m_binomialn, prueba)


actual <- data.frame(table(prueba$delitos))[,2]

comp<-cbind(
  delitos = table_poisson$tabla$counts,
  observado = actual,
  poisson = table_poisson$tabla$table1p,
  qpoisson = table_quasipoisson$tabla$table1p,
  binomialn = table_binomialn$tabla$table1p
  ) %>% as_tibble()


## Modelos con inflacion en 0

library(pscl)

#ZIBN
m_zinb <- zeroinfl(delitos~.,data = prueba, dist = "negbin")
summary(m_zinb)


table_zinb <- table_binomialn(m_zinb, prueba)

comp<-comp %>% bind_cols(zinb = table_zinb$tabla$table1p)


#Hurdle
m_hurdle<- hurdle(delitos~.,data = prueba, dist = "negbin")
summary(m_hurdle)


#Comparacion AIC y BIC
compare_performance(m_binomialn, m_zinb, m_hurdle, metrics = "AIC")

plot(compare_performance(m_binomialn, m_zinb, m_hurdle, metrics = c("AIC", "BIC")))

vuongtest(m_binomialn, m_hurdle)
vuongtest(m_binomialn, m_zinb)
vuongtest(m_zinb, m_hurdle)


check_zeroinflation(m_binomialn)



# Rootograma --------------------------------------------------------------

library(countreg)

root<-rootogram(m_hurdle,main="ZIP")


root.pois <- rootogram(m_poisson, style = "hanging", plot = FALSE)
root.nb   <- rootogram(m_binomialn, style = "hanging", plot = FALSE)
root.zinb <- rootogram(m_zinb, style = "hanging", plot = FALSE)
root.hurdle <- rootogram(m_hurdle, style = "hanging", plot = FALSE)

ylims <- ylim(-2, 20)  # common scale for comparison
cowplot:: plot_grid(autoplot(root.pois) + ylims,
          autoplot(root.nb) + ylims, 
          autoplot(root.zinb) + ylims, 
          autoplot(root.hurdle) + ylims, 
          ncol = 3, labels = "auto")


autoplot(c(root.nb, root.zinb))
autoplot(c(root.pois, root.zinb))












library(nonnest2)
vuongtest(m_binomialn,m_zinb)

vuongtest(m_zinb, m_hurdle)

vuongtest(m_binomialn,m_hurdle)



#Rootgrama

library(countreg)

root<-rootogram(m_zip,main="ZIP")







library(performance)



plot(compare_performance(m_poisson, m_zip, m_binomialn, m_zinb, rank = TRUE))


compare_performance(m_poisson, m_zip, m_binomialn, m_zinb,m_hurdle)

check_model(m_poisson)

check_zeroinflation(m_poisson)

check_overdispersion(a)



##overdispersion##

#grafico##
plot(log(fitted(a)),log((prueba$delitos-fitted(a))^2),xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)

#test sobredispersion##
library(AER)
dispersiontest(a)




library(parameters)

model_parameters(m_poisson)


m_poisson %>% 
  select_parameters() %>% 
  model_parameters()

a <- 
m_poisson %>% 
  select_parameters()





compare_performance(a, m_poisson, m_hurdle)

describe_distribution(iris)


plot(parameters(a))


test_performance(a, m_poisson)

plot(compare_performance(a, m_poisson))



export_summs(a,m_poisson)

summary(a)
coeftest(a)
logLik(a)


