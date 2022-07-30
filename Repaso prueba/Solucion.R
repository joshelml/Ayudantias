
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





















