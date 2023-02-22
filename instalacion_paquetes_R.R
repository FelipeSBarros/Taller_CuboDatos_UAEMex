#install.packages("stars")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("sp")
#install.packages("raster")
#install.packages("cubelyr") 

library(stars)
library(dplyr)

# Estados (provincias) de amazonia
uf <- readRDS('./data/tidy/uf.rds')
# cubo de datos material particulado < 2.5
(tseries <- read_stars("./data/CAMS/pm25_daily_mean_2020.nc"))

sf::st_crs(tseries) <- st_crs(uf)
names(tseries) <- "ppm < 2.5"
tseries

st_get_dimension_values(tseries, "time")

tseries %>% 
  slice(index = 1:10, along = "time")

tseries %>% 
  slice(index = 1:10, along = "time") %>% 
  #as("Raster") %>% 
  plot(breaks="equal")


tseries %>% 
  slice(index = 1:10, along = "time") %>% 
  as("Raster") %>% 
  raster::plot()

plot_hook = function() plot(st_geometry(uf), border = 'red', add = TRUE)

tseries %>% 
  slice(index = 1:10, along = "time") %>% 
  plot(hook = plot_hook)

tseries %>% 
  filter(
    time < lubridate::ymd('2020-02-01')
  ) %>% plot(hook = plot_hook)

tseries %>% filter(
  time >= lubridate::ymd('2020-07-01'),
  time < lubridate::ymd('2020-08-01')
) %>% plot(hook = plot_hook)

tseries %>% filter(
  time >= lubridate::ymd('2020-07-01'),
  time < lubridate::ymd('2020-11-01')
) %>% aggregate(by = "1 months", FUN = max) %>%
  plot(hook=plot_hook)

tseries %>% filter(
  time >= lubridate::ymd('2020-07-01'),
  time < lubridate::ymd('2020-11-01')
) %>% aggregate(by = "1 months", FUN = max) %>%
  aggregate(by = uf, FUN = mean) %>% 
  plot()

(datos_agregados_uf <- tseries %>% 
    aggregate(by = uf, FUN = mean))

datos_agregados_uf %>% filter(
  time >= lubridate::ymd('2020-07-01'),
  time < lubridate::ymd('2020-11-01')
) %>% aggregate(by = "1 months", FUN = mean)

datos_agregados_uf %>% filter(
  time >= lubridate::ymd('2020-07-01'),
  time < lubridate::ymd('2020-11-01')
) %>% #aggregate(by = "1 months", FUN = mean) %>% 
  plot(breaks='equal')


(r <- tseries %>% slice(index = 1:30, along = "time") %>% as("SpatRaster"))

(s <- st_as_stars(r))

names(s) <- "Polución"
st_get_dimension_values(s, 'band')

(s <- st_set_dimensions(s,
                        # which = 3,
                        which = 'band',
                        values = lubridate::date('2020-01-01') + 
                          c(0:30),
                        names = 'fechas'))

st_get_dimension_values(s, 'fechas')

datos_agregados_uf[,1,]
s['Polución', , , 1:7 ]

write_stars(#s["Polución",,,], 
            adrop(s[1]),
            "./cubo_polucion.nc",
            overwrite=TRUE, 
            driver='netCDF',
)

(s_ = read_ncdf(.x = './cubo_polucion.nc', 
                make_time = TRUE
))
