
library(tidyverse); library(magrittr); library(MetBrewer)

anoph <- read_csv('C:/Users/cjcar/Dropbox/AnophElev/clean/Anopheles clean.csv')
anoph %<>% select(-X, -X1)

anoph %>%
  select(Lat, Long, Year) %>%
  distinct() %>%
  group_by(Lat, Long) %>%
  summarize(Years = diff(range(Year))+1) -> coverage

coverage %<>% arrange(Years)

world <- map_data("world")
africa <- subset(world, region %in% c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
                                      "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
                                      "Democratic Republic of the Congo","Republic of Congo","Ivory Coast",
                                      "Djibouti","Egypt","Equatorial Guinea","Eritrea","Swaziland","Ethiopia",
                                      "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
                                      "Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco",
                                      "Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
                                      "Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan",
                                      "Sudan","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"))
# Borrowed from https://warin.ca/posts/rcourse-datavisualizationwithr-maps/
coverage %>%
  ggplot(aes(x = Long, y = Lat, color = Years)) + 
  geom_point(size = 2) + 
  scale_color_gradientn(colors = met.brewer("Hokusai3")) + 
  geom_polygon(data = africa, aes(x = long, y = lat, group = group), 
               fill = NA, color = "grey50", alpha = 0.3) + 
  theme_void() + theme(legend.position = 'right',
                       plot.margin = margin(1,1,0,0, "cm"))

########### ELEVATION AND REGIONS

library(raster)
library(sf)

elev <- raster("C:/Users/cjcar/Dropbox/AnophElev/Data/Africa.asc")
elev %<>% aggregate(fact = 10, fun = mean)

gbd <- st_read(dsn = "C:/Users/cjcar/Dropbox/continents/GBD-Retool-DECIMALS/OriginalGBD",
               layer = "WorldRegions")

gbd %<>% filter(SmllRgn %in% c("Sub-Saharan Africa (East)",
                               "Sub-Saharan Africa (West)",
                               "Sub-Saharan Africa (Central)",
                               "Sub-Saharan Africa (Southern)"))
# gbd %<>% select()

# gbd %<>% group_by(SmllRgn) %>% summarize()

elev %<>% as.data.frame(xy = TRUE) %>%
  drop_na() %>% rename(Elevation = africa)

ggplot(elev) + 
  geom_raster(aes(x = x, y = y, fill = Elevation)) + 
  scale_fill_gradientn(colors = met.brewer("Tam"), name = 'Elevation (m)') + 
  theme_void() +   
  xlim(range(africa$long)) + ylim(-35, 38) + 
  geom_polygon(data = africa, aes(x = long, y = lat, group = group), 
               fill = NA, color = "grey50", alpha = 0.3) + 
  geom_sf(data = gbd, fill = NA, color = NA, lwd = 1) + # turn back on for outlines
  theme(legend.position = 'right') -> g1

ggplot(coverage) + 
  geom_polygon(data = africa, aes(x = long, y = lat, group = group), 
               fill = NA, color = "grey50", alpha = 0.3) + 
  theme_void() + 
  geom_point(aes(x = Long, y = Lat, color = Years), size = 2) + 
  scale_color_gradientn(colors = met.brewer("Hokusai3")) + 
  # theme(legend.position = 'right',
  #       plot.margin = margin(1,1,0,0, "cm")) + 
  #xlim(range(africa$long)) + 
  ylim(-35, 38) + 
  geom_sf(data = gbd, fill = NA, color = NA, lwd = 1) + 
  theme(legend.position = 'right') -> g2

library(patchwork)
g1 + g2 + 
  plot_layout(guides = 'collect') + plot_annotation(tag_levels = c("A","B")) & 
  theme(plot.tag = element_text(size = 19))
