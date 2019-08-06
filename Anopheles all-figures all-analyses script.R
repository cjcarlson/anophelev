library(rangeshifts)
library(ggplot2)

anoph <- read.csv('C:/Users/cjcar/Dropbox/AnophElev/clean/Anopheles clean.csv')
colnames(anoph)[colnames(anoph)=='Region'] <- 'BigRegion'
#anoph$Region <- ''

as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(3, elev) %>% 
  ggplot(aes(y=elev, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Max elevation (meters)') -> g3

as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(-3, Lat) %>% 
  ggplot(aes(y=-1*Lat, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Max latitude (degrees)') -> g4

cowplot::plot_grid(g3,g4,labels=c("A","B"))

as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(3, elev) %>% dplyr::rename(Response = elev) -> ae
as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(-3, Lat) %>% dplyr::rename(Response = Lat) -> al

al$Response <- al$Response*(-1)

ae$var <- 'Max elevation (meters)'
al$var <- 'Max latitude (degrees)'
aboth <- rbind(ae,al)

ggplot(aboth, aes(y=Response, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm) + facet_wrap(~var, scale='free') + theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=7))


#### REGRESSION

lat.reg <- evreg(anoph, resp="Lat", n.pts=3,
                 south=TRUE, latConv=TRUE)
table(lat.reg[[2]][,1] > 0, lat.reg[[2]][,2] < 0.05)

elev.reg <- evreg(anoph, resp="elev", n.pts=3,
                 byRegion='Country', units='meters')
table(elev.reg[[2]][,1] > 0, elev.reg[[2]][,2] < 0.05)

std <- function(x) sd(x)/sqrt(length(x))

# Maximum elevations 1.58 +- 0.38 meters per year
mean(elev.reg[[2]][,1])
std(elev.reg[[2]][,1])

mean(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])
std(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])


# Maximum latitudes 5.19 +- 0.89 km per year
mean(lat.reg[[2]][,1])
std(lat.reg[[2]][,1])

mean(lat.reg[[2]][elev.reg[[2]][,2] < 0.05,1])
std(lat.reg[[2]][elev.reg[[2]][,2] < 0.05,1])



maxshifts <- data.frame(lat=lat.reg[[2]][,1],
                        elev=elev.reg[[2]][,1])
ggplot(maxshifts, aes(x=elev, y=lat)) + geom_point() + theme_bw() + 
  xlab('Elevational shifts (m/year)') + ylab('Latitudinal shifts (km/year)') + 
  geom_smooth(method='lm')
summary(lm(elev~lat, data=maxshifts))






### MANN WHITNEY TESTS

mw1 <- mw.2period(anoph, resp='Lat', pre.years=c(1800,1954), 
                  post.years=c(1979,2020), n.mw=-10)

mw2 <- mw.2period(anoph, resp='elev', pre.years=c(1800,1954), 
                  post.years=c(1979,2020), n.mw=10) 

mw.south <- mw.2period(anoph[anoph$BigRegion=='Southern Africa',], resp='elev', pre.years=c(1800,1954), 
                      post.years=c(1979,2020), n.mw=10) 

mw.east <- mw.2period(anoph[anoph$BigRegion=='East Africa',], resp='elev', pre.years=c(1800,1954), 
                       post.years=c(1979,2020), n.mw=10) 

mw.west <- mw.2period(anoph[anoph$BigRegion=='West Africa',], resp='elev', pre.years=c(1800,1954), 
                      post.years=c(1979,2020), n.mw=10) 

mw.c <- mw.2period(anoph[anoph$BigRegion=='Central Africa',], resp='elev', pre.years=c(1800,1954), 
                      post.years=c(1979,2020), n.mw=10) 































################ SUPPLEMENT FIGURE 1

as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(10, elev) %>% 
  ggplot(aes(y=elev, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Max elevation (meters)') -> g3

as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(-10, Lat) %>% 
  ggplot(aes(y=-1*Lat, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Max latitude (degrees)') -> g4

cowplot::plot_grid(g3,g4,labels=c("A","B"))




as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(10, elev) %>% dplyr::rename(Response = elev) -> ae
as.tbl(anoph) %>% group_by(Year,Species) %>% top_n(-10, Lat) %>% dplyr::rename(Response = Lat) -> al

al$Response <- al$Response*(-1)

ae$var <- 'Max elevation (meters)'
al$var <- 'Max latitude (degrees)'
aboth <- rbind(ae,al)

ggplot(aboth, aes(y=Response, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm) + facet_wrap(~var, scale='free') + theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=7))




lat.reg <- evreg(anoph, resp="Lat", n.pts=10, 
                 south=TRUE, latConv=TRUE)
table(lat.reg[[2]][,1] > 0, lat.reg[[2]][,2] < 0.05)

elev.reg <- evreg(anoph, resp="elev", n.pts=10,
                  byRegion='Country', units='meters')
table(elev.reg[[2]][,1] > 0, elev.reg[[2]][,2] < 0.05)

std <- function(x) sd(x)/sqrt(length(x))

# Maximum elevations 1.58 +- 0.38 meters per year
mean(elev.reg[[2]][,1])
std(elev.reg[[2]][,1])

mean(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])
std(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])


# Maximum latitudes 5.19 +- 0.89 km per year
mean(lat.reg[[2]][,1])
std(lat.reg[[2]][,1])

mean(lat.reg[[2]][elev.reg[[2]][,2] < 0.05,1])
std(lat.reg[[2]][elev.reg[[2]][,2] < 0.05,1])



maxshifts <- data.frame(lat=lat.reg[[2]][,1],
                        elev=elev.reg[[2]][,1])
ggplot(maxshifts, aes(x=elev, y=lat)) + geom_point() + theme_bw() + 
  xlab('Elevational shifts (m/year)') + ylab('Latitudinal shifts (km/year)') + 
  geom_smooth(method='lm')
summary(lm(elev~lat, data=maxshifts))





#############


ole2 <- function(raw) {
  if(is.na(max(raw))) {return(NA)}
  raw <- raw[raw>0]
  if(length(unique(raw))<10) {return(NA)} else {
    data <- data.frame(vals=sort(unique(raw), decreasing=TRUE)[1:10],
                       ones=1)
    return(sExtinct::OLE(data,alpha=0.05)[[1]])
  }
}


#### ELEVATION

el.ole <- na.omit(plyr::ddply(anoph, c("Species",'Year','Country'), summarise,
                      max.elev=ole2(elev)))
el.ole <- el.ole[el.ole$Species %in% unique(el.ole$Species)[table(na.omit(el.ole)$Species)>5],]

elev.reg <- evreg(el.ole, resp="max.elev", n.pts=1,
                  byRegion='Country', units='meters')

table(elev.reg[[2]][,1] > 0, elev.reg[[2]][,2] < 0.05)

std <- function(x) sd(x)/sqrt(length(x))

# Maximum elevations 1.58 +- 0.38 meters per year
mean(elev.reg[[2]][,1])
std(elev.reg[[2]][,1])

mean(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])
std(elev.reg[[2]][elev.reg[[2]][,2] < 0.05,1])

###### LAT


lat.ole <- na.omit(plyr::ddply(anoph, c("Species",'Year','Country'), summarise,
                              max.lat=-1*ole2(-1*Lat)))
lat.ole <- lat.ole[lat.ole$Species %in% unique(lat.ole$Species)[table(na.omit(lat.ole)$Species)>5],]

lat.reg <- evreg(lat.ole, resp="max.lat", south=TRUE, latConv=TRUE)

table(lat.reg[[2]][,1] > 0, lat.reg[[2]][,2] < 0.05)

std <- function(x) sd(x)/sqrt(length(x))

# Maximum latations 1.58 +- 0.38 meters per year
mean(lat.reg[[2]][,1])
std(lat.reg[[2]][,1])

mean(lat.reg[[2]][lat.reg[[2]][,2] < 0.05,1])
std(lat.reg[[2]][lat.reg[[2]][,2] < 0.05,1])


el.ole %>% ggplot(aes(y=max.elev, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Est. max elevation (meters)') -> g3

lat.ole %>% ggplot(aes(y=max.lat, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm, show.legend=FALSE) + 
  ylab('Est. max latitude (degrees)') -> g4

cowplot::plot_grid(g3,g4,labels=c("A","B"))


el.ole %>% dplyr::rename(Response = max.elev) -> ae
lat.ole %>% mutate(max.lat = -1*max.lat) %>% dplyr::rename(Response = max.lat) -> al

ae$var <- 'Max elevation (meters)'
al$var <- 'Max latitude (degrees)'
aboth <- rbind(ae,al)

ggplot(aboth, aes(y=Response, x=Year, color=Species, fill=Species)) + theme_bw()  + 
  geom_smooth(method=lm) + facet_wrap(~var, scale='free') + theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=7))



