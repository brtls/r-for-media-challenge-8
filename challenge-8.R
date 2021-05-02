# Challenge #8: Inzidenzzahlen der Hamburger Bezirke

# Lade den Datensatz stadtteile_wsg84.RDS
# Recherchiere die Fallzahlen der letzten sieben Tage für die Hamburger Bezirke
# https://www.hamburg.de/corona-zahlen/
# Erstelle eine leaflet Map und visualisiere die Inzidenzzahlen (Achtung: Nicht die Fallzahlen)
# Nutze dafür Shapes, Legende, Hovereffekte und Labels
# Exportiere die Map als HTML file

library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(tidyverse)
library(htmltools)

hamburg_districts <- readRDS("data/hamburg_districts.rds")

bezirke <- readRDS("data/bezirke_wsg84.RDS") %>% 
  st_as_sf() %>% 
  st_transform("+proj=longlat +datum=WGS84")

bezirke <- bezirke %>% 
  left_join(hamburg_districts, by = c("Bezirk_Name" = "bezirk"))

vergangene_7_tage <- c(690, 320, 208, 286, 587, 177, 327)
bezirke_name <- c("Hamburg-Mitte", "Altona", "Eimsbüttel", "Hamburg-Nord", "Wandsbek", "Bergedorf", "Harburg")

vergangene_7_tage_df <- data.frame(bezirke_name, vergangene_7_tage)

bezirke <- bezirke %>% 
  left_join(vergangene_7_tage_df, by = c("Bezirk_Name" = "bezirke_name"))

bezirke <- bezirke %>% 
  mutate(inzidenz = round(vergangene_7_tage / einwohner * 100000))

#bins und pal definieren
cuts1 <- c(0, 50, 100, 150, 200, 250)

pal1 <- colorBin("YlOrRd", domain = bezirke$inzidenz, bins = cuts1)

#labels definieren
labels1 <- sprintf("<strong>%s</strong><br>Inzidenz: %g<br>Neuinfektionen: %g ",
                   bezirke$Bezirk_Name,
                   bezirke$inzidenz,
                   bezirke$vergangene_7_tage) %>% 
  map(HTML)

hamburg_bezirke <- leaflet(data = bezirke) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  setView(lng = 9.993682, lat = 53.551086, zoom = 10) %>% 
  addPolygons(
    fillColor = ~pal1(inzidenz),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.6,
    highlight = highlightOptions(
      weight = 3,
      color = "grey",
      bringToFront = TRUE,
      fillOpacity = 0.8),
    label = labels1) %>% 
  addLegend(pal = pal1, values = ~inzidenz, 
            opacity = 0.5, title = "Corona Inzidenz letzte 7 Tage", position = "bottomright")

hamburg_bezirke

htmlwidgets::saveWidget(as_widget(hamburg_bezirke), "corona_inzidenz_hamburg.html")
