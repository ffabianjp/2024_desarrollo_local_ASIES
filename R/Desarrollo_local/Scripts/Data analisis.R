#Desarrollo Local

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
