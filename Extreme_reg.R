#install.packages("ismev")               # if not installed!
library(ismev)
library(magrittr)
library(mgcv)
library(tidyverse)

# Read in the data
anoph <- read_csv('C:/Users/cjcar/Dropbox/AnophElev/clean/Anopheles clean.csv')[,-c(1:2)]
anoph

# Recenter years, and eliminate recent years which are data deficient (relatively)

anoph %<>% mutate(Year = (Year - 1900))

hist(anoph$Year)
table(anoph$Year)
anoph %<>% filter(Year < 115)

# Pull out wellcomei and check it out

anoph %>% filter(Species == 'wellcomei') -> well
ggplot(well, aes(x = Year, y = Lat)) + #geom_point(alpha = 0.3) + 
  stat_binhex() + scale_fill_gradient(low = 'blue', high = 'red')

# Pull out Southern latitudes (reverse sign), one max per year, and run a regression just as a check

well %>% mutate(AbsLat = -1*Lat) %>% 
  group_by(Year) %>% top_n(AbsLat, n = 1) -> well.max

well.lm<- lm(AbsLat ~ Year, data = well.max)
summary(well.lm)

plot(AbsLat ~ Year, well.max)
abline(well.lm)

# Now... how to do this as a GAM 

m1 <- gam(list(AbsLat ~ s(Year),
               ~ s(Year),
               ~ 1),
          data = well.max, method = "REML",
          family = gevlss(link = list("identity", "identity", "identity")))

summary(m1)
plot(m1, pages = 1, all.terms = TRUE)

# Is this correct? 

mu <- fitted(m1)[,1]
rho <- fitted(m1)[,2]
xi <- fitted(m1)[,3]
fv <- mu + exp(rho)*(gamma(1-xi)-1)/xi

plot(fv ~ well.max$AbsLat)

plot(well.max$AbsLat ~ well.max$Year)
points(fv ~ well.max$Year, col='red')



