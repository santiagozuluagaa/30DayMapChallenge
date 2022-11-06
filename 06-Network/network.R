library(geojsonsf)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(rnaturalearthdata)
library(rnaturalearth)
library(RColorBrewer)
library(magick)

# Data source (cable-geo and landing-point-geo): https://github.com/telegeography/www.submarinecablemap.com

# Reading geo information about cables and their landing points.

cables <- geojson_sf("cable-geo.json")
# You also can use https://raw.githubusercontent.com/telegeography/www.submarinecablemap.com/master/web/public/api/v3/cable/cable-geo.json

landing_point <- geojson_sf("landing-point-geo.json")
# You also can use https://raw.githubusercontent.com/telegeography/www.submarinecablemap.com/master/web/public/api/v3/landing-point/landing-point-geo.json

# Cables landing in Colombia
cables_landing_in_Colombia <- c(
  "South American Crossing (SAC)", 
  "Maya-1",
  "Colombian Festoon",
  "ARCOS",
  "Caribbean Express (CX)",
  "Pacific Caribbean Cable System (PCCS)",
  "America Movil Submarine Cable System-1 (AMX-1)",
  "Colombia-Florida Subsea Fiber (CFX-1)",
  "San Andres Isla Tolu Submarine Cable (SAIT)",
  "Carnival Submarine Network-1 (CSN-1)",
  "GlobeNet",
  "South America-1 (SAm-1)"
)

cables_Colombia <- cables %>% 
  dplyr::filter(name %in% cables_landing_in_Colombia)


# Landing Points Colombia
landing_point_Colombia <- landing_point %>% 
  dplyr::filter(str_detect(name, "Colombia"))

# Countries map
world_map <- ne_countries(scale = 50, returnclass = 'sf')

# Getting countries from the Americas, except Canada
countries_Americas <- world_map %>% 
  dplyr::filter(continent %in% c("North America", "South America"),
                name != "Canada"
  )


# CREATING ANIMATION

# 12 colors for the 12 cables
colors <- rep(brewer.pal(5,"Set1"), 3)[1:12]

for (cable in cables_landing_in_Colombia){
  
  cable_Colombia_filtered <- cables_Colombia %>% dplyr::filter(name == cable)
  
  color_cable <- colors[which(cables_landing_in_Colombia == cable)]
  
  p <- ggplot() +
    geom_sf(data = countries_Americas, size = 0.2) +
    geom_sf(data = cable_Colombia_filtered, color = color_cable, size = 0.8) +
    geom_sf(data = landing_point_Colombia, size = 0.3) +
    coord_sf(xlim = c(-100,-25), ylim = c(-60, 50), expand = FALSE) +
    scale_x_continuous(breaks=seq(-100, -25, 25)) +
    labs(title = cable,
         caption = "Source: TeleGeography. Submarine Cable Map. Graphic: Santiago Zuluaga @santizuluayal | 2022") +
    theme_bw() +
    theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
          plot.caption = element_text(size = 5.6, hjust = 0.5, face = "bold"))
  
  fp <- paste0(cable, ".png")
  
  ggsave(plot = p, 
         filename = fp, 
         device = "png",
         width = 4,
         height = 4)
  
}

# GIF CREATION
# For more info about gif creation: https://www.nagraj.net/notes/gifs-in-r/
imgs <- dir()[str_detect(dir(), "png")]
cable_list <- lapply(imgs, image_read)

## join the images together
cable_joined <- image_join(cable_list)

## animate at 2 frames per second
cable_animated <- image_animate(cable_joined, fps = 2)

## view animated image
cable_animated

## save to disk
image_write(image = cable_animated,
            path = "cables_Colombia.gif")
