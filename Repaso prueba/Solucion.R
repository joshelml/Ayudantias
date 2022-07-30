
# Librerias ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyverse)


# 1 -----------------------------------------------------------------------

auto_mpg  <- readr::read_csv("Repaso prueba/datos/auto-mpg.csv")
View(auto_mpg)

auto_mpg %>% glimpse()

## Reviando NAN

auto_mpg %>% summary()

auto_mpg <- auto_mpg %>% 
  janitor::clean_names()

## Re-codificacion variable origin, y transformacion de horsepower

auto_mpg <- auto_mpg %>% 
  mutate(origin = case_when(origin == 1 ~ 'EEUU',
                            origin == 2 ~ 'Europa',
                            origin == 3 ~ 'Japón'),
         horsepower = as.numeric(horsepower))


auto_mpg %>% summary()

## Se eliminan
auto_mpg <- drop_na(auto_mpg) 
auto_mpg %>% summary()


# 2 -----------------------------------------------------------------------

auto_mpg %>% 
  ggplot(aes(x = weight, y = mpg)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

# Se observa que existe una relación negativa, es decir, a mayor 
# millas por galon de consumo menor es el peso del automovil.
#los vehiculos livianos recorren más millas por galon.

auto_mpg %>% 
  ggplot(aes(x = weight, y = mpg, col = origin)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

auto_mpg$origin %>% table()


# 3 -----------------------------------------------------------------------

auto_mpg %>% glimpse()
auto_mpg <- auto_mpg %>%  
  select(-car_name)

modelo_lm <- lm(mpg ~., data = auto_mpg )
summary(modelo_lm)


# method = both -----------------------------------------------------------

modelo_both <- step(modelo_lm, method = 'both',
                    trace = 0)
summary(modelo_both)


# method = backward -------------------------------------------------------


modelo_back <- step(modelo_lm, 
                    direction = 'backward',
                    trace = 0)
summary(modelo_back)


# method = forward --------------------------------------------------------

modelo_full = formula(lm(mpg ~., data = auto_mpg))

modelo_for <- step(object = lm(mpg ~ 1, data = auto_mpg),
                   scope = modelo_full,
                   direction = 'forward',
                   trace = 0)
summary(modelo_for)


# 4 -----------------------------------------------------------------------

## Analizar supuesto de normalidad de los residuos

### grafico
ggplot(data = NULL, aes(sample = modelo_both$residuals)) +
  stat_qq() + stat_qq_line(col = 'purple')

### test
### H0: los residuos siguen una distribución normal
### H1: los residuos no siguen una distribución normal

nortest::lillie.test(modelo_both$residuals)

# los residuos no siguen una distibución normal.


## Independecia de los residuos

### Test Durbin Watson

### H0: los residuos no estan autocorrelacionados
### H1: los residuos estan autocorrelacionados

lmtest::dwtest(modelo_both)
plot(modelo_both) #graficos del modelo
# Rechazamos la hipotesis nula, por lo que los residuos no son
#independientes.

## Homocedasticidad
### H0: Homocedasticidad
### H1: Heterocedasticidad

library(ggfortify)
#plot(modelo_both, 1)
#plot(modelo_both, 3)
autoplot(modelo_both, which = 1:6, ncol = 2, label.size = 3)[c(1, 3)]

### Test Breusch Pagan
### H0: Homocedasticidad
### H1: Heterocedasticidad
lmtest::bptest(modelo_both)

# En general nuestro modelo no es bueno.


# 5 -----------------------------------------------------------------------

informacion <- data.frame(cylinders= 6, displacement= 390, horsepower= 110, 
                         weight = 3021, accceleration = 13.5, model_year = 80, 
                         origin = 'EEUU')

informacion
# mpg = b0 + cylinders*x1 + ....
predict(modelo_both, informacion)


summary(modelo_both)

#Weight:HorsePower interacciones dentro del modelo


# Pregunta 2 --------------------------------------------------------------


# 1 -----------------------------------------------------------------------

datos <- readr::read_csv("Repaso prueba/datos/rrhh.csv")
datos %>% summary()

datos %>% glimpse()

datos <- datos %>% 
  mutate(Estado.Civil = case_when(Estado.Civil == 1 ~ 'Divorciado',
                                  Estado.Civil == 2 ~ 'Casado',
                                  Estado.Civil == 3 ~ 'Separado',
                                  Estado.Civil == 4 ~ 'Soltero',
                                  Estado.Civil == 5 ~ 'Viudo'))

datos <- datos %>% 
  mutate_if(is.character, as.factor)

datos %>% glimpse()


# 2 -----------------------------------------------------------------------

set.seed(2022)
id <- sample(1:nrow(datos), size = 0.75*nrow(datos), replace = FALSE)

#Forma 1
train <- datos %>% 
  slice(id)

test <- datos %>% 
  slice(-id)

#Forma 2
train <- datos[id, ]
test <- datos[-id, ]


# 3 -----------------------------------------------------------------------

## primero veamos la variable desempeño
train$Desempeño %>% unique()

##--
#cambiar la categoria basal
stats::relevel(train$Desempeño, ref = '90-day meets')
##--

modelo1 <- glm(Estado ~ Edad + Desempeño, data = train, 
               family = binomial(link = 'logit'))
summary(modelo1)

# b > 0 agravante 
# b < 0 protector

# Calcular OR

broom::tidy(modelo1) %>% 
  mutate(OR = exp(estimate))

#OR > 1 factor de riesgo de la desvinculación
# Edad: por cada aumento en la edad la chance de desvinculación aumenta en un 4%
# (90-day meets) Desempeño: Aumenta el chance de desvinculación de un 67% si la
#categoria es PIP frente a 90-day meets.

#Si la variable es de tipo factor, entonces el aumento o disminucion de chance
#es frente a la categoria que "quedo afuera" o la categoria basal.

#Desempeño 2: Disminuye el chance de desvinculación en un 99% si la categoría es
#Exceptional frente a 90-day meets.

# 4 -----------------------------------------------------------------------

modelo_completo <- glm(Estado ~ ., data = train,
                       family = binomial(link = 'logit'))

modelo_back <- step(modelo_completo,
                    direction = 'backward',
                    trace = 0)

summary(modelo_back)
#modelo final
modelo_back$formula

broom::tidy(modelo_back) %>%
  mutate(OR = exp(estimate))


# 5 -----------------------------------------------------------------------



