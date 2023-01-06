
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
    group_by(Year) %>% summarize(Lat = min(Lat), Sampling = n()) %>% 
    filter(Lat < 0) %>% # Only southern hemisphere
    mutate(Lat = -1*Lat)
  elevdf <- spdf %>% group_by(Year) %>% summarize(elev = max(elev), Sampling = n())
  spyrs <- range(spdf$Year)
  
  if(length(unique(latdf$Lat)) > 4) {
    latlm <- lm(Lat ~ Year + Sampling, data = latdf)
    
    predlat <- data.frame(species = sp,
                          latslope = coef(latlm)[[2]],
                          latsig = coefficients(summary(latlm))[2,4],
                          nslope = coef(latlm)[[3]],
                          nsig = coefficients(summary(latlm))[3,4])
  } else {
    predlat <- data.frame(species = sp,
                          latslope = NA,
                          latsig = NA,
                          nslope = NA,
                          nsig = NA)
  }
  
  if(length(unique(elevdf$elev)) > 4) {
    elevlm <- lm(elev ~ Year + Sampling, data = elevdf)
    predelev <- data.frame(species = sp,
                           elevslope = coef(elevlm)[[2]],
                           elevsig = coefficients(summary(elevlm))[2,4],
                           nslope = coef(elevlm)[[3]],
                           nsig = coefficients(summary(elevlm))[3,4])
  } else {
    predelev <- data.frame(species = sp,
                           elevslope = NA,
                           elevsig = NA,
                           nslope = NA,
                           nsig = NA)
  }
  
  sppred <- left_join(predlat,predelev,by = 'species')
  sppred$Species <- sp
  return(sppred)
  
})

mods <- bind_rows(mods)

# Some of the simpler analyses in the text

mean(mods$latslope, na.rm = TRUE)*111
sd(mods$latslope*111, na.rm = TRUE)/sqrt(length(na.omit(mods$latslope)))
table(mods$latsig < 0.05)
table(mods$nsig.x < 0.05)

mean(mods$elevslope, na.rm = TRUE)
sd(mods$elevslope, na.rm = TRUE)/sqrt(length(na.omit(mods$elevslope)))
table(mods$elevsig < 0.05)
table(mods$nsig.y < 0.05)

# Check species correlations between time and sampling


mods <- lapply(splist, function(sp) {  
  spdf <- anoph %>% filter(Species == sp)
  spdf %<>% group_by(Year) %>% summarize(n = n())
  s <- summary(lm(n ~ Year, data = spdf))
  s$coefficients[2,4] < 0.001
})
table(unlist(mods))