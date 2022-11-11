library(sf)
library(ggplot2)
library(readxl)
library(dplyr)

# División política de Colombia (territorial division in Colombia) Shapefile
# Source: https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/
Colombia <- read_sf("MGN_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")

# Necesidades básica insatisfechas (Unmet basic needs)
# Source: https://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/necesidades-basicas-insatisfechas-nbi
NBI <- readxl::read_xlsx(
                         path = "CNPV-2018-NBI-DIVIPOLA-2021.xlsx",
                         sheet = "Departamento",
                         range = "A11:C43",
                         col_names = c("DPTO_CCDGO", "Departamento", "NBI")
                        )

# Merge (Join) Colombia and NBI
ColombiaNBI <- merge(Colombia, NBI, by = "DPTO_CCDGO")


# Transformation to NBI (get those departments with NBI > 50) and exluding San Andrés
ColombiaNBI <- ColombiaNBI %>% 
                  mutate(NBI_gt_50 = ifelse(NBI > 50, Departamento, NA)) %>% 
                  filter(Departamento != "ARCHIPIÉLAGO DE SAN ANDRÉS")


# Plot
p <- 
  ggplot(data = ColombiaNBI) +
  geom_sf(aes(fill = NBI)) +
  geom_sf_label(aes(label = NBI_gt_50), size = 2.5) +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15") +
  labs(
       title = "% de Población con \n Necesidades Básicas Insatisfechas (NBI)",
       caption = "Source: DANE - Censo nacional de población y vivienda 2018 \n Graphic: Santiago Zuluaga @santizuluayal | 2022",
       fill = "NBI (%)"
       ) +
  theme_bw() +
  theme(
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 7, hjust = 0.5, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
        )

# Save plot  
ggsave(plot = p, 
       filename = "NBI_Colombia.png", 
       device = "png",
       width = 4.5,
       height = 4.5)  

