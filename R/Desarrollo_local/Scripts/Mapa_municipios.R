library(ggplot2)
library(sf)
library(geodata)
mapa <- st_read("gadm41_GTM.gpkg",layer="GID_2")


mapa=geodata::gadm('GTM', level = 2, path = 'maps')
plot(mapa)

ggplot()+                # a ggplot function
  geom_sf(                # precise that it will be a spatial geometry
    aes(                  # provide some aesthetics
      geometry = mapa$geom,    # the geometry column (usually auto detected)
      fill = Datos_fundesa$BRECHA_TGP)       # we want the polygon color to change following the count
  ) -> g # store it in g

mapa_sf<-sf::st_as_sf(mapa)
Datos_fundesa <- read.csv("Data\\datos_fundesa.csv", header=TRUE, stringsAsFactors=FALSE,fileEncoding="LATIN1")
mapa_sf$No_corr<-1:nrow(mapa_sf)
mapa_sf_datos<-merge(mapa_sf,Datos_fundesa, by='No_corr')

ggplot(data = mapa_sf_datos) +
  geom_sf(aes(fill = WES_SCORE_mean)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, "RdYlGn"),
                       na.value = "darkgrey")


