#Desarrollo Local
library(MatchIt)
library(lmtest)
library(sandwich)
library(ggplot2)

#abrir datos de FUNDESA.
Datos_fundesa <- read.csv("Data\\datos_fundesa.csv", header=TRUE, stringsAsFactors=FALSE,fileEncoding="LATIN1")


ggplot(Datos_fundesa, aes(x=WES_SCORE_mean,y=Turismo_pp)) + 
  geom_point()
# +geom_label(
#     label=Datos_fundesa$MUNICIPIO, 
#     nudge_x = 0.25, nudge_y = 0.25, 
#     check_overlap = T
#   )

ggplot(Datos_fundesa, aes(x=GINI,y=WES_SCORE_mean)) + 
  geom_point()

ggplot(Datos_fundesa, aes(x=GINI,y=tasa.global.de.participacion.mujeres)) + 
  geom_point()
ggplot(Datos_fundesa, aes(x=WES_SCORE_mean,y=tasa.global.de.participacion.mujeres)) + 
  geom_point()
ggplot(Datos_fundesa, aes(x=Turismo_pp,y=tasa.global.de.participacion.mujeres)) + 
  geom_point()

sggplot(Datos_fundesa, aes(x=Ladinos_porcentaje,y=WES_SCORE_mean)) + 
  geom_point()

LR=lm(WES_SCORE_mean ~ tasa.global.de.participacion.mujeres, data =Datos_fundesa)
summary(LR)

LR=lm(tasa.global.de.participacion.mujeres ~ Turismo_pp, data =Datos_fundesa)
summary(LR)

# Propensity score matching
match_obj <- matchit(Turistico ~ Poblacion + MIGRA_VIDA_pout + AGRICULTURA + Ladinos_porcentaje,
                     data = Datos_fundesa, method = "nearest", distance ="glm",
                     ratio = 1,
                     replace = FALSE)
summary(match_obj)


plot(match_obj, type = "jitter", interactive = FALSE)
plot(summary(match_obj), abs = FALSE)


matched_data <- match.data(match_obj)

res <- lm(ICL.2023 ~ Turistico, data = matched_data, weights = weights)
#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

res <- lm(WES_SCORE_mean ~ Turistico, data = matched_data, weights = weights)
#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

res <- lm(GINI ~ Turistico, data = matched_data, weights = weights)
#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

res <- lm(tasa.global.de.participacion ~ Turistico, data = matched_data, weights = weights)
#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

res <- lm(Pobreza ~ Turistico, data = matched_data, weights = weights)
#Test the coefficient using cluster robust standard error
coeftest(res, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)
