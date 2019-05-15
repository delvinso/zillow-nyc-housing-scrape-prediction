library(tidyverse)
library(sf)
library(rgdal)
library(RColorBrewer)
library(broom)
library(GGally)
library(corrplot)
library(tidycensus)


source("analysis/zllw_helpers.R")
# read in data ----
zllw <- read_csv("data/zllw_clean_04_25_2019_only.csv")

# zllw <- read_csv("data/zllw_clean_04_25_2019_pluto.csv") # pluto

# eda ----

glimpse(zllw)
zllw$zpid <- as.factor(zllw$zpid)
zllw$type <- as.factor(zllw$type)
zllw$city <- as.factor(zllw$city)
zllw$zip <- as.factor(zllw$zip)
zllw$fips <- NULL
summary(zllw)

#
quant_vars <- names(select_if(zllw, is.numeric))
quant_vars

cat_vars <- names(select_if(zllw, is.factor))
cat_vars 



# ---- quantitative variables ----
png(filename = "figures/quant_relationship.png", height = 8, width = 12, res = 400, units = "in")
zllw[, quant_vars] %>%
  ggpairs(., aes(alpha = 0.3),
          upper = list(continuous = GGally::wrap("cor", size = 3)),
          lower = list(continuous = corLine)) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 7))
dev.off()

# assessed tax value, finished sqft, lot_size_sqft, zestimate, and sold price must be log transformed
png(filename = "figures/quant_relationship_v2.png", height = 8, width = 12, res = 400, units = "in")
zllw[, quant_vars] %>%
  mutate_at(vars(tax_assess_value, finished_sqft, zestimate, last_sold_price),# lot_size_sqft),
            ~ log(.)) %>%
  ggpairs(., aes(alpha = 0.3),
          upper = list(continuous = GGally::wrap("cor", size = 3)),
          lower = list(continuous = corLine)) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 7))
dev.off()
# they look a lot more normally distributed now, which
# has also increased their correlation with sold price 

# correlation plot

zllw[, quant_vars] %>%
  select(-zestimate) %>%
  mutate_at(vars(tax_assess_value, finished_sqft, last_sold_price),# lot_size_sqft),
            ~ log(.))  %>%
  cor(., use = "pairwise") %>%
  corrplot(type = "upper", method = "color", diag = FALSE, addCoef.col = "black", 
           mar=c(0,0,1,0))
# year built could be converted into a factor...

zllw %>%
  scttrCor(aes(y = log(last_sold_price), x = year_built)) +
  scale_x_continuous(breaks = seq(1700, 2020, by = 10)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# looks like it could be divided into 3 categories?

zllw %>%
  mutate(period_built = ifelse(year_built <= 1930, "one", "two")) %>%
  group_by(period_built) %>%
  summarize(mean_sale = mean(log(last_sold_price)))
  # ggplot(aes(x = period_built, y = log(last_sold_price))) + 
  # geom_boxplot(aes(fill = period_built))

# ---- beds and baths ----

# these may capture more variation as factors

zllw %>% 
  mutate_at(vars(baths, beds,
                 # baths_ngbr_mean,  beds_ngbr_mean,
  ),
  ~ as.factor(case_when(
    . > 5 ~ "5",
    . >= 4 ~ "4",
    . >= 3 ~ "3",
    . >= 2 ~ "2",
    . >= 1 ~ "1",
    # need to figure out how to account for when beds/baths is near mean,
    # or less than..
    . >= 0 ~ "0",
    # . <= 0 ~ "negative"
      )
    )
  ) %>%
  select(beds, baths, last_sold_price) %>%
  gather(key = "vars", value = "num", c(beds, baths)) %>%
  mutate(num = factor(num, levels = c("0", "1", "2", "3", "4", "5", ">6"))) %>%
  ggplot(aes(x = num, y = log(last_sold_price), colour = num)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 0.5) + 
  geom_boxplot(width = 0.5) + 
  facet_wrap(~ vars, scales = "free") +
  scale_color_brewer(palette = "Dark2") 


# ---- gis stuff ----

ggplot(zllw) +
  geom_point(aes(x = lon, y = lat, colour = log(last_sold_price)), alpha = 0.6, size = 1.5) +
  scale_colour_viridis_c()
# ---- categorical variables ----
  
  
  # ---- postal code ----
  # is there a relationship between postal code and price sold?
  
  zllw %>%
    ggplot(aes(x = zip, y = log(last_sold_price))) + 
    geom_boxplot() +
    coord_flip()
  
  # variability in zip code may be better captured by using first 3 digits
  # https://www.reddit.com/r/datascience/comments/9hdz86/predicting_house_price_what_to_do_with_zip_code/
  
  zllw %>% 
    mutate(zip2 = str_sub(zip, 1, 3 )) %>%
    ggplot(aes(x = zip2, log(last_sold_price))) + 
    geom_boxplot() + 
    coord_flip()
  
  
  zllw %>%  mutate(zip2 = str_sub(zip, 1, 3 )) %>% 
    count(zip2, sort = TRUE)
  
  # drop postal codes with low counts...
  
  
  # ---- city ----
  zllw[, c(cat_vars, "last_sold_price")] %>%
    ggplot(aes(x = city, y = log(last_sold_price))) + 
    geom_boxplot() + 
    coord_flip()
  
  # will have to remove this as it doesnn't look to be informative
  zllw %>% count(city, sort = TRUE) %>% arrange(n)
  
  # ---- type of property ----
  zllw %>% count(type, sort = TRUE) 
  
  zllw[, c(cat_vars, "last_sold_price")] %>%
    ggplot(aes(x = type, y = log(last_sold_price))) + 
    geom_boxplot() + 
    coord_flip()
  # there does seem to be variation among house types

  
  
  
  # ---- putting it together ----
  
  # leave beds and baths for later as we will use neighbouring properties as a feature
  zllw <- zllw %>% 
    mutate_at(vars(tax_assess_value, finished_sqft, #lot_size_sqft,
                   last_sold_price),
              list(log = ~log(.))) %>%
    select(-c(tax_assess_value, finished_sqft,#lot_size_sqft,
              last_sold_price, zestimate, city)) %>%
    mutate(zip = as.factor(str_sub(zip, 1, 3))) %>%
    filter(!zip %in% c(107, 108, 10))
  
  zllw # 12899 obs
  vis_miss(zllw)
  #12702 observations remaininng
  
  
  
# GIS Feature Engineering ----


# --- boroughs ----

# uses spatial points class
# i could use sf's st_intersection() but it takes forever relative to over()

# https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm
# https://stackoverflow.com/questions/45950770/map-lat-lon-points-to-a-shape-file-in-r

# first convert latitude and longitude into coordinates 
# by way of transforming into a SpatialPointsClass



zllw_coords <- zllw[, c("lon", "lat")] %>% rename("long" = "lon")
# convert data into a SpatialPointsClass

coordinates(zllw_coords) <- ~long+lat

# read in borough shapefile
boro <- readOGR("data/shapefiles/Borough Boundaries/geo_export_30da3225-047d-4fc3-bb2e-41dd71a6a257.shp",
                layer = "geo_export_30da3225-047d-4fc3-bb2e-41dd71a6a257") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null")) # from .prj file


# ensure the two files have the same CRS
zllw_coords@proj4string <- boro@proj4string
# overlaying the coordinates on the shapefiles
points_overlaid_boro <- as_tibble(over(zllw_coords, boro))

# sannity check
bind_cols(zllw, points_overlaid_boro)  %>%
  ggplot(aes(x = lon, y = lat)) + 
  geom_point(aes(colour = boro_name), size = 0.7, alpha = 0.7) + 
  scale_color_brewer(palette = "Set1")


# ---- census blocks and tracts (2010)----
#read in tracts
tracts <- readOGR("data/shapefiles/nyct2010_19a/nyct2010.shp",
                  layer = "nyct2010") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null")) # from .prj file

# overlaying the coordinates on the shapefiles
points_overlaid_tracts <- as_tibble(over(zllw_coords, tracts))

# read in blocks
blocks <-  readOGR("data/shapefiles/nycb2010_19a/nycb2010.shp",
                   layer = "nycb2010") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null")) # from .prj file

points_overlaid_blocks <- as_tibble(over(zllw_coords, blocks))


# ---- community districts ----

districts <-  readOGR("data/shapefiles/nycd_19a/nycd.shp",
                      layer = "nycd") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null")) # from .prj file

points_overlaid_cd <- as_tibble(over(zllw_coords, districts))

table(points_overlaid_cd$BoroCD )


# ---- zip code (2014) ----

zip <-  readOGR("data/shapefiles/ZIP_CODE_040114/ZIP_CODE_040114.shp",
                layer = "ZIP_CODE_040114") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs +nadgrids=@null"))

points_overlaid_zip <- as_tibble(over(zllw_coords, zip))

points_overlaid_zip %>% count(ZIPCODE, sort = TRUE)

# ---- median income and population ----
# register my API key
library(tidycensus)
census_api_key("xxxx")
# look at available variables
v17 <- load_variables(2017, "acs5", cache = TRUE)
# as_tibble(v17) %>% filter(str_detect(label, "")) 

# retrieve median income for ny 
ny <- get_acs(geography = "tract", 
              keep_geo_vars = TRUE,
              variables = c(med_income = "B19013_001",
                            poverty = "B05010_001",
                            pop = "B00001_001"), 
              state = "NY",
              geometry = TRUE) %>%
  filter(str_detect(NAME.y, "Queens County|New York County|Bronx County|Kings County|Richmond County"))
# 
# ggplot() + 
#   geom_sf(data = ny) +
#   geom_sf(data = st_as_sf(zllw_coords), size = 0.1, alpha = 0.8)



# transform to WGS84
ny <- ny %>% st_transform(crs = 4326)

# visualizing - should normalize before visualizing?
# ny %>%
#   group_by(variable) %>%
#   mutate(estimate = scale(estimate, center = TRUE, scale = TRUE)) %>%
#   ggplot(aes(fill = estimate)) + 
#     geom_sf(color = NA) + 
#     scale_fill_viridis_c(option = "magma")  +
#     facet_wrap(~variable)

# trannsform coordinates to SF and also WGS84 coordinate system
zllw_coords_census <- st_as_sf(zllw_coords) %>% 
  st_transform(crs = 4326) %>% mutate(id = row_number())


coords_overlaid_census <- st_intersection(ny, zllw_coords_census)


coords_overlaid_census  %>% count(id)
# ???
# coords_overlaid_census %>%
#   # geom_polygon(size = 0.1, alpha = 0.5, aes(fill = estimate)) + 
#   ggplot(aes(fill = estimate)) + 
#   geom_sf(color = NA) + 
#   coord_sf(crs = 4326) +
#   scale_fill_viridis_c(option = "magma") 
# join variables with coordinates + id, and converting to 
# wide format
acs_vars <- coords_overlaid_census$geometry %>% 
  st_coordinates() %>%
  cbind(coords_overlaid_census[, c("id","variable", "estimate")]) %>%
  as_tibble() %>%
  select(-geometry) %>%
  spread(variable, estimate) %>%
  rename(lon = X, lat = Y)
  
acs_vars
# vars <- coords_overlaid_census$geometry %>% 
#     st_coordinates() %>%
#     cbind(coords_overlaid_census[, c("id", "estimate")]) %>%
#     rename(lon = X, lat = Y, med_income = estimate) %>%
#     as_tibble() %>%
#     select(-geometry)  


  

zllw %>% mutate(id = row_number()) %>%
  left_join(acs_vars, by = c("id"))    %>%
  # ggplot() +
  # geom_histogram(aes(x = pop))
  scttrCor(aes(x = med_income, y = last_sold_price_log))



# --- neighbouring properties buffers ----

# take the average of neighbouring homes features

# convert to sf
zllw_coords_sf <- st_as_sf(zllw_coords)

# set our coordinates to dublin crs, which is in meters
zllw_coords_sf<- st_transform(zllw_coords_sf, 29902) 
# create buffer 300m around each coordinate
zllw_buff <- st_buffer(zllw_coords_sf, 300)
zllw_buff

# mapping points into buffer zones
coords_mapped <- st_intersects(zllw_coords_sf, zllw_buff)  %>%
  as_tibble() %>%
  # remove properties which the buffer was created around so
  # its features are not included in the aggregate
  filter(row.id != col.id)


coords_mapped


# subset houses correspondinng to each house identified by col.id and bind
# with groupings in coords_mapped
neighbours <- bind_cols(coords_mapped, zllw[coords_mapped$col.id, ])
neighbours
# sanity check
# zllw[c(237, 417, 497), ] # looks good

# how many nearby properties are there are on average within the 750m buffer?

neighbours %>% count(row.id) %>% summarize(mean(n)) # 5.78

# take mean of nearby numerical features
ngbr_means <- neighbours %>% 
  group_by(row.id) %>%
  summarize_at(vars(last_sold_price_log, beds, baths, finished_sqft_log, year_built,
                    tax_assess_value_log), 
               list(ngbr_mean = ~ mean(., na.rm = TRUE)))

# take mode of nearby categorical features ?

ngbr_means

# combine with original data, can't use bind cols because row count of the neighbour
# aggregate data is not the same as og data 
# this is because not all houses have neighbours in the buffer zone and as such werent included

zllw %>%
  mutate(row_num = row_number()) %>%
  select(row_num, everything()) %>%
  left_join(ngbr_means, by = c("row_num" = "row.id"))  

# ----- Distance from Midtown  ----

midtown_lat <- 40.750470
midtown_lon <- -73.989610

# calculate "crow-fly" distance between mid-town and property location 
zllw %>% 
  mutate(distance = geosphere::distHaversine(.[, c("lon", "lat")], 
                                             c(midtown_lon, midtown_lat)))  %>% 
  scttrCor( aes(x = sqrt(distance), y = last_sold_price_log))

  ggplot(aes(x = log(distance))) +
  geom_histogram()





# ---- TBD: neighbouring properties characteristics as features ----
# what about variables with neighbours?

# dist <- st_distance(zllw_coords_sf, zllw_coords_sf)
# https://stackoverflow.com/questions/32618956/find-the-nearest-x-y-coordinate-using-r
closest <- RANN::nn2(data=as_tibble(zllw_coords), k = 20)[[1]]

zllw[closest[1, ][-1], ] %>%
  rename_all(~ paste0("ngbr_one", "_", .)) 

# sanity check for neighbours
zllw_closest <- zllw %>% 
  mutate(row_num = row_number()) %>%
  left_join(closest_long, by = c("row_num" = "neighbours"))

zllw_closest
closest_long <- tidy(closest) %>% #filter(X1 == 1) %>%
  gather(key = "property", value = "neighbours", -one_of("X1")) %>%
  arrange(X1)

# sanity check
closest_long
closest_long[closest_long$X1 == 1,]$neighbours
zllw%>%
  mutate(marks = as.factor(ifelse(row_number() %in% closest_long[closest_long$X1 == 1,]$neighbours, 1, 0))) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_point(aes(colour = marks, size = marks), alpha = 0.9, size = 0.4)

closest_long

# ---- putting it all together ----

zllw2 <- bind_cols(
  zllw,
  # borough names
  points_overlaid_boro["boro_name"] %>% rename("boro" = "boro_name"),
  # tract, boro + tract, neighbourhood tabulation areas
  points_overlaid_tracts[c("CT2010", "NTAName")], 
  # blocks
  points_overlaid_blocks["CB2010"],
  # community districts
  points_overlaid_cd["BoroCD"],
  # zip and fips
  points_overlaid_zip[c("ZIPCODE")]) %>%
  filter(!is.na(boro)) %>% 
  # distance to midtown 
  mutate(dist_midtown = geosphere::distHaversine(.[, c("lon", "lat")], 
                                             c(midtown_lon, midtown_lat))) %>%
  # join with nearby mean property features and acs
  mutate(row_num = row_number()) %>%
  select(row_num, everything()) %>%
  left_join(acs_vars %>% select(-c(lon, lat)), by = c("row_num" = "id"))  %>%   
  left_join(ngbr_means, by = c("row_num" = "row.id")) %>%
  mutate(BoroCD = as.factor(BoroCD)) %>%
  select(-zip)

# sum(as.character(zllw2$zip) == str_sub(as.character(zllw2$ZIPCODE), 1, 3), na.rm = TRUE)


# clean up
rm(blocks, tracts, districts, zip, zllw_buff, zllw_coords, zllw_coords_census, zllw_coords_sf)

apply(zllw2[c("CT2010", "NTAName", "CB2010", "BoroCD")], 2, table)
# census block and census tract does not seem to be informative as 
# they are too sparse, so they will be dropped
# may be worthwhile to look at median income in the tract/block
zllw2 <- zllw2 %>% select(-c("CT2010", "CB2010"))

miss_var_summary(zllw2)


## ---- some final cleaning ----

# removing unnecessary columns, could eventually look at these 
zllw2 <- zllw2 %>% 
  select(-c(row_num, zpid, last_sold_date)) %>%# %>%
  # take log of status and sqft variables
  # mutate_at(vars(last_sold_price, finished_sqft,
  #                tax_assess_value,
  #                last_sold_price_ngbr_mean, finished_sqft_ngbr_mean), list(log = ~ log(.))) %>%
  # remove old vars
  # select(-c(last_sold_price, finished_sqft,
  #           last_sold_price_ngbr_mean, finished_sqft_ngbr_mean,
  #           tax_assess_value)) %>%
  # baths and beds
  mutate_at(vars(baths, beds,
                 baths_ngbr_mean,  beds_ngbr_mean
  ),
  ~ as.factor(case_when(
    . >= 5 ~ "5",
    . >= 4 ~ "4",
    . >= 3 ~ "3",
    . >= 2 ~ "2",
    . >= 1 ~ "1",
    # need to figure out how to account for when beds/baths is near mean,
    # or less than..
    . >= 0 ~ "0",
    # . <= 0 ~ "negative"
  )
  )
  ) %>%
  mutate(zip = as.factor(str_sub(ZIPCODE, 1, 3))) %>%
  # age of the house and period built
  mutate(age = 2019 - year_built,
         age_ngbr_mean = 2019 - year_built_ngbr_mean) %>%
  mutate(period_built = as.factor(ifelse(year_built <= 1930, "pre-1930","post-1930")),
         period_built_ngbr_mean = as.factor(ifelse(year_built_ngbr_mean <= 1930, "pre-1930","post-1930")) )  %>%
  select(-c(ZIPCODE, contains("year_built")))

zllw2  # 12899 
names(zllw2)

table(zllw2$beds)
table(zllw2$baths)
zllw2 <- zllw2[-which(zllw2$baths == 0), ]

write_csv(zllw2, "data/zllw_model_ready_04_27_2019.csv")

# ---- final visualizations of data ----

zllw2 %>% count(boro)
# histogram of response
zllw2 %>% 
  ggplot() + 
  geom_histogram(aes(x = last_sold_price_log, fill = ..count..), show.legend = FALSE) +
  scale_fill_viridis_c()

# lat/lon of sold price
zllw2 %>% 
  ggplot() + 
  geom_polygon(data = tidy(boro), aes(x = long,
                                      y = lat,
                                      group = group),
               colour = "black", 
               fill = "white") +
  geom_point(aes(x = lon, y = lat, colour  = last_sold_price_log),
             size = 2, alpha = 0.5) +

  theme(legend.direction = "horizontal",
        legend.position = "top") +
  scale_colour_viridis_c(option = "viridis", direction = 1,
                         begin = 0.1,
                         limits = c(min(zllw2$last_sold_price_log), ceiling(max(zllw2$last_sold_price_log))),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(2, units = "mm"),
                           barwidth = unit(50, units = "mm"),
                           draw.ulim = F,
                           title.position = "top",
                           title.hjust = 0.5,
                           label.hjust = 0.5))  +
  coord_quickmap()


# correlation plot of baseline features
zllw2[, c("tax_assess_year", "lat", "lon", "tax_assess_value_log",
               "finished_sqft_log", "last_sold_price_log"
               # "poverty", "med_income", "pop"
)] %>%
  cor(., use = "pairwise") %>%
  corrplot(type = "upper", method = "color", diag = FALSE, addCoef.col = "black", 
           mar=c(0,0,1,0))

# violin plot of baseline 

zllw2 %>% #select_if(is.factor) %>% 
  select(type,
         baths, beds,
         period_built, last_sold_price_log) %>%
  gather(key = "var", value = value, -one_of("last_sold_price_log")) %>%
  ggplot(aes(fill= var, x = value, y = last_sold_price_log)) + 
  geom_violin(draw_quantiles = 0.5, show.legend = FALSE) + 
  geom_boxplot(width = 0.2, fill = "white") + 
  facet_wrap(~ var, scales = "free") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# correlation plot of engineered features

zllw2 %>% 
  select(poverty, med_income, pop, contains("mean"), dist_midtown, 
         last_sold_price_log) %>% 
  select_if(is.numeric) %>%
  cor(., use = "pairwise") %>%
  corrplot(type = "upper", method = "color", diag = FALSE, addCoef.col = "black", 
           mar=c(0,0,1,0))


# violin plot of engineered categorical variables

zllw2 %>% #select_if(is.factor) %>% 
  select(boro, zip, last_sold_price_log,         
         period_built_ngbr_mean, baths_ngbr_mean, beds_ngbr_mean) %>%
  gather(key = "var", value = value, -one_of("last_sold_price_log")) %>%
  ggplot(aes(fill= var, x = value, y = last_sold_price_log)) + 
  geom_violin(draw_quantiles = 0.5) + 
  geom_boxplot(width = 0.2, fill = "white") + 
  facet_wrap(~ var, scales = "free") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

# violin plot of baseline + gis categorical variables

zllw2 %>% #select_if(is.factor) %>% 
  select(type, baths, boro, beds, period_built, zip, last_sold_price_log,
         period_built_ngbr_mean, baths_ngbr_mean, beds_ngbr_mean) %>%
  gather(key = "var", value = value, -one_of("last_sold_price_log")) %>%
  ggplot(aes(fill= var)) + 
  geom_violin(aes(x = value, y = last_sold_price_log), 
              show.legend = FALSE, draw_quantiles = 0.5) + 
  facet_wrap(~ var, scales = "free") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
# missingness

miss_var_summary(zllw2) %>%
  ggplot(aes(x = reorder(variable, n_miss), y = n_miss, fill = n_miss)) +
  geom_col(colour = "black") + 
  geom_label(aes(x = variable, y = n_miss, label = paste0(round(pct_miss, 2), "%"), hjust = -0.05), size = 2.5) +
  scale_fill_viridis_c(begin = 0.2) + 
  scale_y_continuous(limits = c(0, 2250)) + 
  coord_flip() + 
  guides(fill = FALSE)


   
