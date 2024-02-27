# satelite ----
library(stars)
# Loading required package: abind
(r <- read_stars("./data/raster/rj_2010.tif"))
st_dimensions(r)

b <- st_bbox(r) %>% 
  st_as_sfc()  %>% 
  st_centroid()  %>% 
  st_buffer(units::set_units(9000, 'm'))



plot(r[b][,,,1], reset = FALSE)
# The defaults include:
#   
#   the plots receive a joint legend, rather than a legend for each layer; where raster considers the bands as independent layers, stars treats them as a single variable that varies over the dimension band;
# the plot layout (rows × columns) is chosen such that the plotting space is filled maximally with sub-plots;
# a legend is placed on the side where the most white space was left;
# color breaks are chosen by classInt::classIntervals using the quantile method, to get maximum spread of colors;
# a grey color pallete is used;
# grey lines separate the sub-plots.
plot(b, border = 'brown', lwd = 2, col = NA, add = TRUE)

plot(r[b, crop = FALSE][,,,1])

(rs <- split(r))
st_dimensions(rs)

merge(rs, name = "band")  %>%  setNames("L8")

# Multiple stars object with identical dimensions can be combined using c. By default, the arrays are combined as additional attributes, but by specifying an along argument, the arrays are merged along a new dimension:

(r2 <- c(r, r, along = "new_dim"))

circles <- st_sample(st_as_sfc(st_bbox(r)), 3) %>% 
  st_buffer(500)

aggregate(r, circles, mean)

# which gives a data cube with 3 geometries and 6 bands. Aggregation over a temporal dimension is done by passing a time variable as the second argument to aggregate, as

#a set of time stamps indicating the start of time intervals or
#a time period like "weeks", "5 days" or "years"
plot(r)
plot(r, rgb = c(4,3,2), main = "False colour (NIR-R-G)")    # rgb

# masking certin values
r2[r < 50] <- NA

# Dimension-wise, we can apply functions to selected array dimensions (Section 6.3.3)) of stars objects similar to how apply does this to arrays. For instance, we can compute for each pixel the mean of the 6 band values by
st_apply(r, c("x", "y"), mean)
ndvi <- function(b1, b2, b3, b4, b5, b6) (b4 - b3)/(b4 + b3)
ndvi <- st_apply(r, c("x", "y"), ndvi)
r <- merge(c(split(r), ndvi), name = 'band')
plot(r, rgb=c(5,4,3))
plot(r[,,,5])

# Dados de polucion ----
library(stars)
library(dplyr)

municipios <- readRDS('./data/tidy/municipios.rds')
uf <- readRDS('./data/tidy/uf.rds')
plot(st_geometry(uf))

(tseries <- read_stars("./data/CAMS/pm25_daily_mean_2020.nc"))

sf::st_crs(tseries) <- st_crs(uf)
names(tseries) <- "ppm2_5"

st_get_dimension_values(tseries, "time")

tseries %>% dplyr::slice(index = 9, along = "time") %>% as("Raster") %>% raster::plot()
plot(st_geometry(uf), add=T)

t10 <- tseries %>% 
  slice(index = 1:10, along = "time")

st_get_dimension_values(t10, "time")

# Using filter
tseries %>% filter(time < lubridate::ymd('2020-02-01')) %>% plot()

# plot with polygon
plot_hook = function() plot(st_geometry(uf), border = 'red', add = TRUE)

t10 %>%
  plot(hook = plot_hook)

aggregate(t10, by = "5 days", FUN = mean) %>%
  plot(hook=plot_hook)

st_get_dimension_values(t10, "time")

# selecionando por data
tseries %>%
  slice(index =
          grep("2020-08-01", 
            st_get_dimension_values(tseries, "time")),
        along = "time")

periodo_incendios <- tseries %>%
  slice(index =
          grep(
            paste("2020-08","2020-09" , 
                  sep = '|'), 
            st_get_dimension_values(tseries, "time")),
        along = "time")

st_get_dimension_values(periodo_incendios, "time")

# estimando valor medio por semana e por UF
aggregate(periodo_incendios,
          by = 'weeks', 
          FUN = mean) %>% 
  aggregate(by = uf, FUN = mean) %>% 
  plot()

# removendo NA
aggregate(periodo_incendios,
          by = 'weeks', 
          FUN = mean) %>% 
  aggregate(by = uf, FUN = mean, na.rm=TRUE) %>% 
  plot()


# We can integrate over (reduce) time, for instance to find out when the maximum precipitation occurred. The following code finds the time index, and then the corresponding time value:
index_max = function(x) ifelse(all(is.na(x)), NA, which.max(x))

daily_mean_values <- aggregate(periodo_incendios, 
                               by = uf, 
                               FUN = max)

st_apply(daily_mean_values, 
         "geometry", 
         index_max) %>%
  mutate(
    when = 
      st_get_dimension_values(
        daily_mean_values, "time")[.$index_max]) %>%
  select(when) %>%
  plot(key.pos = 1, main = "Day with max exposures")

# In contrast to stars objects, tbl_cube objects
# do not explicitly handle regular dimensions (offset, delta)
# do not register reference systems (unless they are a single dimension property)
# do not cope with affine grids (see below)
# do not cope with dimensions represented by a list (e.g. simple features, see below)
# do not register whether attribute values (measurements) refer to point or interval values, on each dimension
library(ggplot2)
require(ggforce)
daily_mean_values %>% stars::as.tbl_cube.stars() %>%
  as_tibble() %>% 
  ggplot() +  
  geom_line(aes(x=time, y=ppm2_5, group=geometry, color=as.factor(geometry)))

## Loading required package: ggforce
ggplot() +  
  geom_line(data=b, aes(x=hour, y=traffic))
# creando ----
library(stars)
library(dplyr)

# creando un raster aleatorio
uf <- readRDS('./data/tidy/uf.rds')

(tseries <- read_stars("./data/CAMS/pm25_daily_mean_2020.nc"))

# r <- raster::raster(
#   nrow=62, ncol=78,
#   xmn=-74.59, xmx=-43.39, 
#   ymn=-18.64, ymx=6.16, res=0.4,
#   crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", vals=NULL)
sf::st_crs(tseries) <- st_crs(uf)
(r <- tseries %>% slice(index = 1:30, along = "time") %>% as("SpatRaster"))

(s <- st_as_stars(r))
# dandole alguna correlacion esacial
# set.seed(0)
# raster::values(r) <- runif(raster::ncell(r), min=1, max=10)
# fw <- raster::focalWeight(r, 3, type='circle')

# r.focal <- raster::focal(r, w=fw, fun=mean, na.rm=TRUE) 
# raster::plot(r)
# raster::plot(r.focal)

# creando nuevas bandas y transformando en stars
# s <- st_as_stars(
#   raster::stack(
#     lapply(1:14, function(i) r.focal*(2*i))))
# 
# s
plot(s)

# definiendo el nombre del atributo:
names(s) <- "Polución"
st_get_dimension_values(s, 'band')
s <- st_set_dimensions(s,
                  # which = 3,
                  which = 'band',
                  values = lubridate::date('2020-06-30') + 
                    c(0:30),
                  names = 'fechas')

s
st_get_dimension_values(s, 'fechas')
s
plot(s)
# guardando dato

# raster::writeRaster(s,
#             "./cubo_polucion.nc", overwrite=TRUE, 
#             format="CDF", 
#             # varname="polución", 
#             # varunit="ug", 
#             # longname="Polución atmosferica", 
#             xname="Longitude",   
#             yname="Latitude", 
#             zname="fechas")
write_stars(adrop(s[1]),
             "./cubo_polucion.nc", 
            overwrite=TRUE, driver='netCDF',
            # format="CDF"
            )

# reading
s_ = read_ncdf(.x = './cubo_polucion.nc', 
          make_time = TRUE, 
          # var="Polución"
          )
s <- merge(s_)  %>% 
  setNames(names(s)) %>% 
  st_set_dimensions(3, 
                    values = lubridate::date('2020-06-30') + 
                      c(0:13))  %>% 
  st_set_dimensions(names = c("x", "y", "fechas"))
