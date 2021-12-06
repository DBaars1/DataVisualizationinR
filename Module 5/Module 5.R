 ### Practice Statistical models

# Start with the tidyverse
library(tidyverse)

data("Boston", package = "MASS")

boston2 <- mutate(
  Boston,
  chas = factor(chas, 0:1, c("yes", "no"))
)

library(GGally)
ggpairs(Boston)

glimpse(Boston)
head(Boston)

fit <- lm(crim ~ medv + rm + chas, data = boston2)
library(broom)

#Q1
glance(fit)

#Q2
aug <- augment(fit)

aug %>%
ggplot(aes(.fitted, .std.resid)) +
  geom_hline(yintercept = 0, 
             lty = 2) +
  geom_point()
         
plot(fit)

#Q3
broom::tidy(fit, conf.int = T, conf.level = 0.89) %>%
  filter (term != "(Intercept)") %>%
  ggplot(aes(estimate, term)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high))

#Q4
expand.fit <-
  tidyr::expand(
    boston2,
    chas,
    medv,
    rm = mean(rm)
  )

aug.fit <- 
  broom::augment(fit, newdata = expand.fit, se_fit = T)

aug.fit %>%
ggplot(aes(medv, .fitted, 
           fill = chas, 
           color = chas)) +
  geom_ribbon(aes(ymin = .fitted - .se.fit, 
                  ymax = .fitted + .se.fit), 
              color = "transparent", 
              alpha = 0.2) +
  geom_line()

#Q5
summarize(boston2, across(c("medv", "rm"), range))

expand.fit2 <-
  tidyr::expand(
    boston2,
    chas,
    medv = full_seq(medv, period = 2, tol = 1),
    rm = full_seq(rm, period = 0.5, tol = 1)
  )

aug.fit2 <- 
  broom::augment(fit, newdata = expand.fit2, se_fit = T)

aug.fit2 %>%
  ggplot(aes(medv, rm, 
             fill = .fitted)) +
  geom_tile() +
  facet_wrap(vars(chas)) +
  scale_fill_viridis_c(name = "Crime Rate") +
  labs(
    x = "median house value", 
    y = "rooms per house", 
    title = "Prediction heatmaps for crime rate across variables Median house value, rooms per house and proximity to the Charles River", 
    fill = "crim")
  
ggsave("Q5.png")


## Worked Example: Heavy Metal 

library(sp)
data(meuse)
head(meuse)

library(tidyverse)
library(ggthemes) # install.packages("ggthemes")
theme_set(theme_map(base_size = 11))

ggplot(meuse) +
  aes(x,y, size = lead) +
  geom_point() +
  coord_equal() +
  theme(legend.position = "right")

library(ggmap) # install.packages("ggmap")
library(rgdal)

rdh <- select(meuse, x, y)
coordinates(rdh) <- ~ x + y
proj4string(rdh) <- CRS("+init=epsg:28992")

meuse2 <- as_tibble(lonlat) %>%
  bind_cols(select(meuse, -x, -y))

lonlat <- spTransform(rdh, CRS("+init=epsg:4326"))

rng <-
  meuse2 %>%
  summarize(x = extendrange(x), # extendrange() extends the range
            y = extendrange(y))

mp <- get_stamenmap(
  c(left = rng$x[1],
    right = rng$x[2],
    top = rng$y[2],
    bottom = rng$y[1]),
  zoom = 14,
  maptype = "terrain"
)

ggmap(mp) +
  geom_point(aes(x, y, size = lead), data = meuse2) +
  theme(legend.position = c(1, 0),
        legend.justification = c("right", "bottom"))

meuse3 <-
  meuse2 %>%
  pivot_longer(cadmium:zinc, names_to = "metal", values_to = "concentration")

ggmap(mp) +
  geom_point(aes(x, y, size = concentration),
             alpha = 0.5,
             data = meuse3) +
  facet_wrap("metal") +
  theme(legend.position = "right")

library(tidygeocoder)

addresses <- tribble(
  ~ name,          ~ address,
  "Kasteel Stein", "Kasteel Stein, Netherlands"
)

kasteel_stein <-
  addresses %>%
  tidygeocoder::geocode(address,        # map to address column
                        method = "osm") # for Nominatim API
kasteel_stein

# Practice Maps

install.packages(c(
  "maps",
  "mapproj",
  "ggmap",
  "rworldmap",
  "tidygeocoder"
))

library(tidyverse)
theme_set(theme_void())

nj <- map_data("county", "new jersey")

#Q1

Trenton <- data.frame(long = -74.742935, lang = 40.217052)

  ggplot(data = nj) +
           geom_point(aes(long, lat)) +
  geom_point(data = Trenton, aes(long, lang, size = 4, col = 'red')) +
    coord_map()



#Q2
swe_cities <- read_csv(
  "https://raw.githubusercontent.com/stat-lu/STAE04/master/data/swe-cities.csv"
)

swe_map <- read_csv(
  "https://raw.githubusercontent.com/stat-lu/STAE04/master/data/swe-map.csv"
)

ggplot() + 
  geom_polygon(aes(x, y, group = group), 
               fill = 'white', color = 'black',
 data = swe_map) +
  coord_fixed() +
  geom_point(aes(x, y, alpha = 0.1),
             stroke = 0.001,
             data = swe_cities) +
  facet("year")
  
#Q3
swe_cities2 <-
  swe_cities %>%
  select(-population_density) %>%
  pivot_wider(
    names_from = "year",
    values_from = "population",
    names_prefix = "population_"
  ) %>%
  mutate(
    population_diff = population_2018 - population_2015,
    direction = ifelse(population_diff < 0, "decay", "growth"),
    abs_population_diff = abs(population_diff)
  ) %>%
  select(-starts_with("population_20"))

ggplot() + 
  geom_polygon(aes(x, y, group = group), 
               fill = 'white', color = 'black',
               data = swe_map) +
  coord_fixed() +
  geom_point(aes(x, y, alpha = 2, size = abs_population_diff),
             stroke = 0.001,
             data = drop_na(swe_cities2)) +
  facet_wrap("direction") 

#Q4
munic_dens1 <-
  swe_cities %>%
  group_by(municipality_name, year) %>%
  summarise(population_density = mean(population_density)) 

swe_map_munic <- left_join(swe_map, munic_dens1)

drop_na(swe_map_munic) %>%
ggplot() +
  geom_polygon(aes(x, y, group = group, color = population_density), inherit.aes = F, fill = 'grey') +
  coord_fixed() +
  facet_wrap("year")

#Q5

munic_dens_diff <-
  swe_map_munic %>%
  drop_na() %>%
  pivot_wider(names_from = "year",
              values_from = "population_density",
              names_prefix = "density_",
              values_fn = mean) %>%
  mutate(density_diff = density_2018 - density_2015) %>%
  select(-density_2015, -density_2018)

drop_na(munic_dens_diff) %>%
  ggplot(aes(x, y, group = group, fill = density_diff)) +
  geom_polygon() +
  scale_fill_distiller(direction = 1, palette = "Greens") +
  coord_fixed() 

#Q6
bbox <- c(left = 13.19321,
          right = 55.70584,
          bottom = 0.08,
          top = 0.04)

map <- get_stamenmap(
  bbox, 
  maptype = "toner-lite", 
  zoom = 14
)

ggmap(map)

#Q8
library(rworldmap)
world <- map_data(getMap())

world %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon() +
  coord_map("gilbert")

#Q9

data.Corona <- read.csv("data.csv")
covid <- read_csv(
  "https://raw.githubusercontent.com/stat-lu/STAE04/master/data/covid.csv"
)

world <- map_data(getMap("coarse"))

covid1 <-
  covid %>%
  filter(indicator == 'deaths') %>%
  mutate(hundredk.death = (weekly_count/population)*100000) %>%
  group_by(region, year, month) %>%
  summarise(hundredk.month= sum(hundredk.death, na.rm =T))

world2 <- inner_join(world, covid1)

  ggplot() +
  geom_polygon(aes(long, lat, group = group), fill = "lightgrey", data = world) +
  geom_polygon(aes(long, lat, group = group, fill = hundredk.month), data = world2) +
  coord_map("mollweide", xlim= c(-180,180), ylim = c(-52, 83.6)) +
    scale_fill_distiller(direction = 1, palette = "Reds") +
    facet_grid(vars(month), vars (year)) +
     labs(fill='Deaths per 100k') 
  
  ggsave("Q10.png", weight = 15, height = 10)






  