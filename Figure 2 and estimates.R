
library(tidyverse); library(magrittr); library(patchwork); library(MetBrewer)

anoph <- read_csv('C:/Users/cjcar/Dropbox/AnophElev/clean/Anopheles clean.csv')
anoph %<>% dplyr::select(-X, -X1)

anoph %<>% mutate(Species = str_replace(Species,"\\..*","")) %>%
  filter(!is.na(elev)) 

splist <- anoph %>% pull(Species) %>% unique() %>% sort()

# Generate a list of models

mods <- lapply(splist, function(sp) {  
  
  spdf <- anoph %>% filter(Species == sp)
  latdf <- spdf %>% 
    filter(Lat < 0) %>% # Only southern hemisphere
    group_by(Year) %>% summarize(Lat = min(Lat)) %>% mutate(Lat = -1*Lat)
  elevdf <- spdf %>% group_by(Year) %>% summarize(elev = max(elev))
  spyrs <- range(spdf$Year)
  
  if(length(unique(latdf$Lat)) > 4) {
    latlm <- lm(Lat ~ Year, data = latdf)
    
    predlat <- data.frame(species = sp,
                          latslope = coef(latlm)[[2]],
                          latsig = coefficients(summary(latlm))[2,4])
  } else {
    predlat <- data.frame(species = sp,
                          latslope = NA,
                          latsig = NA)
  }
  
  if(length(unique(elevdf$elev)) > 4) {
    elevlm <- lm(elev ~ Year, data = elevdf)
    predelev <- data.frame(species = sp,
                          elevslope = coef(elevlm)[[2]],
                          elevsig = coefficients(summary(elevlm))[2,4])
  } else {
    predelev <- data.frame(species = sp,
                           elevslope = NA,
                           elevsig = NA)
  }
  
  sppred <- left_join(predlat,predelev)
  sppred$Species <- sp
  return(sppred)
  
})

mods <- bind_rows(mods)

# Some of the simpler analyses in the text

mean(mods$latslope, na.rm = TRUE)*111
sd(mods$latslope*111, na.rm = TRUE)
table(mods$latsig < 0.05)

mean(mods$elevslope, na.rm = TRUE)
sd(mods$elevslope, na.rm = TRUE)
table(mods$elevsig < 0.05)

cor.test(mods$latslope, mods$elevslope, method = 'pearson')

# Generate the figures

spdfs <- lapply(splist, function(sp) {  
  
  spdf <- anoph %>% filter(Species == sp)
  latdf <- spdf %>% 
    filter(Lat < 0) %>% 
    group_by(Year) %>% summarize(Lat = min(Lat)) %>% mutate(Lat = -1*Lat)
  elevdf <- spdf %>% group_by(Year) %>% summarize(elev = max(elev))
  spyrs <- range(spdf$Year)
  
  if(length(unique(latdf$Lat)) > 4) {
    latlm <- lm(Lat ~ Year, data = latdf)
    predlat <- data.frame(Year = c(1898:2016),
                        Lat = predict(latlm, data.frame(Year = c(1898:2016))),
                        LatSig = (summary(latlm)$coefficients[2,4] < 0.05/22))
  } else {
    predlat <- data.frame(Year = c(1898:2016),
                          Lat = NA,
                          LatSig = NA)
  }
  
  if(length(unique(elevdf$elev)) > 4) {
    elevlm <- lm(elev ~ Year, data = elevdf)
    predelev <- data.frame(Year = c(1898:2016),
                          Elev = predict(elevlm, data.frame(Year = c(1898:2016))),
                          ElevSig = (summary(elevlm)$coefficients[2,4] < 0.05/22))
  } else {
    predelev <- data.frame(Year = c(1898:2016),
                          Elev = NA,
                          ElevSig = NA)
  }
  
  sppred <- left_join(predlat,predelev)
  sppred$Species <- sp
  return(sppred)
  
})

models <- bind_rows(spdfs) %>% as_tibble()
models %<>% mutate(LatSig = factor(LatSig, levels = c("TRUE", "FALSE")))
models %<>% mutate(ElevSig = factor(ElevSig, levels = c("TRUE", "FALSE")))

ggplot(models, aes(x = Year, y = Lat, group = Species)) + 
  geom_line(aes(color = Species, linetype = LatSig), size = 1) + 
  theme_bw() + 
  guides(linetype = "none") +  
  ylab("Latitude (°S)") +
  scale_color_manual(values = met.brewer("Renoir", n = 22)) + 
  xlab("") -> lat

ggplot(models, aes(x = Year, y = Elev, group = Species)) + 
  geom_line(aes(color = Species, linetype = ElevSig), size = 1) + 
  theme_bw() + 
  guides(linetype = "none") + 
  ylab("Elevation (m)") +
  scale_color_manual(values = met.brewer("Renoir", n = 22)) + 
  xlab("") -> elev

models

lat / elev 

#
#
#
#

models %>%
  group_by(Species) %>%
  filter(Year==1900) %>% 
  dplyr::select(Species, Lat) %>%
  rename(Lat1900 = Lat) -> df1900

models %>%
  group_by(Species) %>%
  filter(Year==2000) %>%
  dplyr::select(Species, Lat) %>%
  rename(Lat2000 = Lat) -> df2000

models %>%
  dplyr::select(Species, LatSig) %>%
  distinct() -> latsigs

anoph %>%
  filter(Lat < 0, Year %in% c(1900:1950)) %>%
  group_by(Species) %>%
  slice(which.min(Lat)) %>%
  left_join(df1900) %>% 
  left_join(df2000) %>% 
  left_join(latsigs) -> mappy



world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot(mappy) + 
  geom_sf(data = world, fill = NA, color = "black", alpha = 0.3, size = 0.2) + 
  geom_segment(aes(x = Long, y = -1*Lat1900, xend = Long, yend = -1*Lat2000, color = LatSig),
               lineend = 'round', linejoin = 'round', 
               size = 1,
               arrow = arrow(length = unit(0.1, "inches")),
               position = position_jitter(height = 0, width = 1.1)) + 
  xlim(5, 40) + ylim(-30,5) + theme_bw() + xlab("") + ylab("") +
  scale_color_manual(values = c('indianred2','grey30')) + 
  guides(color = "none") -> g1
 

models %>% 
  filter(Year == 1900) %>%
  dplyr::select(Species, Elev) %>%
  rename(Elev1900 = Elev) -> df1900

models %>% 
  filter(Year == 2000) %>%
  dplyr::select(Species, Elev) %>%
  rename(Elev2000 = Elev) -> df2000

models %>%
  dplyr::select(Species, ElevSig) %>%
  distinct() -> elevsigs

anoph %>%
  filter(Year == 1900) %>% 
  group_by(Species) %>%
  slice(which.max(elev)) %>%
  dplyr::select(Species, elev) %>%
  left_join(df1900) %>% 
  left_join(df2000) %>% 
  left_join(elevsigs) -> mappy2


tri <- data.frame(x = c(0,  3000, 3000),
                  y = c(0, 0,     3000))
ggplot(mappy2) +
  geom_polygon(data = tri, aes(x = x, y = y), alpha = .75, fill = 'gray90', color = 'black') + 
  geom_segment(aes(x = Elev1900, y = Elev1900, xend = Elev1900, yend = Elev2000, color = ElevSig),
               lineend = 'round', linejoin = 'round', 
               size = 0.8,
               arrow = arrow(length = unit(0.1, "inches")),
               position = position_jitter(height = 0, width = 1.1)) + 
  xlab("") + ylab("") +
  scale_color_manual(values = c('indianred2','grey30'))  + 
  guides(color = "none") + theme_bw() -> g2

g1 + g2 + plot_annotation(tag_levels = c("A","B")) & 
  theme(plot.tag = element_text(size = 16))

######

# Generate supplement tables for reviewer 2

tables2 <- lapply(splist, function(sp) {  
  
  spdf <- anoph %>% filter(Species == sp)
  latdf <- spdf %>% 
    filter(Lat < 0) %>% # Only southern hemisphere
    group_by(Year) %>% summarize(Lat = min(Lat)) %>% mutate(Lat = -1*Lat)
  elevdf <- spdf %>% group_by(Year) %>% summarize(elev = max(elev))
  spyrs <- range(spdf$Year)
  
  if(length(unique(latdf$Lat)) > 4) {
    latlm <- lm(Lat ~ Year, data = latdf)
    s1 <- summary(latlm)
    df <- data.frame(species = sp,
                     sigma = round(s1$sigma, 3),
                     f = round(s1$fstatistic[1], 2),
                     df = s1$fstatistic[3],
                     adjr2 = round(s1$adj.r.squared,3))
  } else {
    df <- data.frame(species = sp,
                     sigma = NA,
                     f = NA,
                     df = NA,
                     adjr2 = NA)
  }
  
  return(df)
  
})

bind_rows(tables2) %>% View()

tables3 <- lapply(splist, function(sp) {  
  
  spdf <- anoph %>% filter(Species == sp)
  latdf <- spdf %>% 
    filter(Lat < 0) %>% # Only southern hemisphere
    group_by(Year) %>% summarize(Lat = min(Lat)) %>% mutate(Lat = -1*Lat)
  elevdf <- spdf %>% group_by(Year) %>% summarize(elev = max(elev))
  spyrs <- range(spdf$Year)
  
  if(length(unique(elevdf$elev)) > 4) {
    elevlm <- lm(elev ~ Year, data = elevdf)
    s1 <- summary(elevlm)
    df <- data.frame(species = sp,
                     sigma = round(s1$sigma, 3),
                     f = round(s1$fstatistic[1], 2),
                     df = s1$fstatistic[3],
                     adjr2 = round(s1$adj.r.squared,3))
  } else {
    df <- data.frame(species = sp,
                     sigma = NA,
                     f = NA,
                     df = NA,
                     adjr2 = NA)
  }
  
  return(df)
  
})

bind_rows(tables3) %>% View()

