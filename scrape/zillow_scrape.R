library(rvest)
library(tidyverse)

source("zillow_scrape_funs2.R")
# https://stackoverflow.com/questions/49834051/web-scraping-with-rvest-filtering-through-paginanation

# to be realistic as possible, property should have the following information
# removed lots, lands
# 50k +
# 1 square foot +
# year built
# 1 bed +
# 1 bath + 

# between_pages_sleep_timer  - 
# definitely_sleep_timer - found in scraping details

# for each borough in new york, we create 5 urls correspondinng to different property types, and scrape all 20 pages corresponding to those results (based on above search parameters)
# we create a different 'starting' url for each property type to maximize the number of results we get, as zillow restricts results to only 20 pages


property_types <- c("house_type", "apartment_duplex_type", "condo_type", "townhouse_type")#, "house,condo,apartment_duplex,townhouse_type")

scrape_zillow_urls <- function(first_page, # first page
                               save_file = TRUE,
                               filename,
                               # scrapes all 20 pages by default
                               page_range = c(1:20),
                               between_pages_sleep_timer = 3, definitely_sleep_timer = 0.5){
  
  # url is a given page with search results, eg. https://www.zillow.com/homes/recently_sold/Manhattan-New-York-NY/condo_type/12530_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/40.958641,-73.726159,40.600398,-74.220543_rect/10_zm/
  
  # create the 20 urls that will be scraped
  
  
  urls <- paste0(first_page, page_range, "_p/")
  
  # scrape the 20 urls
  zillow_res <- urls %>% map_dfr(~ read_html(.x) %>% 
                               scrape_zillow(., between_pages_sleep_timer, definitely_sleep_timer))
  
  # remove duplicate results
  zillow_res <- zillow_res %>% distinct()
  
  # save file to my output folder if filename exists
  if(save_file == TRUE & exists("filename")) write_results(zillow_res, filename = filename) 
  
  return(zillow_res)
}



# ---- recently sold in manhattan ---- 
# https://www.zillow.com/homes/recently_sold/Manhattan-New-York-NY/house,condo,apartment_duplex,townhouse_type/12530_rid/1-_beds/1-_baths/50000-_price/200-_mp/1-_size/1-_built/40.958641,-73.726159,40.600398,-74.220543_rect/10_zm/
nyc_base_url <- c("https://www.zillow.com/homes/recently_sold/Manhattan-New-York-NY/",
                  "house,condo,apartment_duplex,townhouse_type",
                  "/12530_rid/1-_beds/1-_baths/50000-_price/200-_mp/1-_size/1-_built/40.958641,-73.726159,40.600398,-74.220543_rect/10_zm/")

zillow_nyc <- property_types %>% 
  map_dfr(~ paste0(nyc_base_url[1], .x, nyc_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:6),
                           definitely_sleep_timer = 3,
                           save_file = FALSE,
                           between_pages_sleep_timer = 3
                           # filename = paste0("zillow_nyc_", .x))
        )
  )
# 1.5, 7.5

zillow_nyc %>% 
  mutate(borough = "manhattan") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  filter(row_number() == 1) %>%
  spread(label, value) %>% 
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%
  
  write_results(., filename = "fixed/zillow_nyc_NEW")

# ---- recently sold in brooklyn ---------

# all properties +  each individual
bklyn_base_url <- c( "https://www.zillow.com/homes/recently_sold/Brooklyn-New-York-NY/",
                      "house,condo,apartment_duplex,mobile,townhouse_type",
                      "/37607_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/40.769621,-73.690453,40.540938,-74.184838_rect/10_zm/")

zillow_bklyn <- property_types %>% 
  map_dfr(~ paste0(bklyn_base_url[1], .x, bklyn_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:10),
                           definitely_sleep_timer = 1.5,
                           save_file = FALSE,
                           between_pages_sleep_timer = 1
                           # filename = paste0("zillow_bklyn_", .x))
                           )
        )

zillow_bklyn %>% 
  mutate(borough = "brooklyn") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  filter(row_number() == 1) %>%
  spread(label, value) %>%
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%

  
  write_results(., filename = "fixed/zillow_brooklyn_NEW")

# ---- recently sold in queens ----

# all properties +  each individual

queens_base_url <- c("https://www.zillow.com/homes/recently_sold/Queens-New-York-NY/",
                     "house,condo,apartment_duplex,townhouse_type",
                     "/270915_rid/1-_beds/1-_baths/50000-_price/200-_mp/1-_size/1-_built/40.850696,-73.584023,40.49187,-74.078408_rect/10_zm/")

zillow_queens <- property_types %>% 
  map_df(~ paste0(queens_base_url[1], .x, queens_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:10),
                           definitely_sleep_timer = 1.5,
                           save_file = FALSE,
                           between_pages_sleep_timer = 5
                           # filename = paste0("zillow_queens_", .x)
                           )
        )


zillow_queens2 <- property_types %>% 
  map_df(~ paste0(queens_base_url[1], .x, queens_base_url[3]) %>% 
           scrape_zillow_urls(first_page = .,
                              page_range = c(11:20),
                              definitely_sleep_timer = 10,
                              save_file = FALSE,
                              between_pages_sleep_timer = 20
                              # filename = paste0("zillow_queens_", .x)
           )
  )

zillow_queens %>% 
  mutate(borough = "queens") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  filter(row_number() == 1) %>%
  spread(label, value)# %>% 
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%
  write_results(filename = "zillow_queens")
  

# ---- recently sold in bronx ----


bronx_base_url <- c("https://www.zillow.com/homes/recently_sold/Bronx-New-York-NY/",
                     "house,condo,apartment_duplex,mobile,townhouse_type",
                     "/17182_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/40.964344,-73.601876,40.736331,-74.096261_rect/10_zm/")



zillow_bronx <- property_types %>% 
  map_dfr(~ paste0(bronx_base_url[1], .x, bronx_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:10),
                           definitely_sleep_timer = 15,
                           save_file = FALSE,
                           between_pages_sleep_timer = 20
                           # filename = paste0("zillow_bronx_", .x)
                           )
        )


zillow_bronx %>% 
  mutate(borough = "bronx") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  filter(row_number() == 1) %>%
  spread(label, value) %>% 
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%
  write_results(filename = "zillow_bronx")


# ---- recently sold in staten island ----

staten_base_url <- c("https://www.zillow.com/homes/recently_sold/Staten-Island-New-York-NY/",
                     "house,condo,apartment_duplex,mobile,townhouse_type",
                     "/27252_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/40.686886,-73.906746,40.457919,-74.401131_rect/10_zm/")

zillow_staten <- property_types %>% 
  map_dfr(~ paste0(staten_base_url[1], .x, staten_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:20),
                           definitely_sleep_timer = 10,
                           save_file = FALSE,
                           between_pages_sleep_timer = 15
                           # filename = paste0("zillow_staten_", .x))
        ))

zillow_staten %>% 
  mutate(borough = "staten") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  filter(row_number() == 1) %>%
  spread(label, value)%>% 
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%
  write_results(filename = "zillow_staten")


# ---- recently sold across ny ----


ny_base_url  <- c("https://www.zillow.com/homes/recently_sold/New-York-NY/",
                  "house,condo,apartment_duplex,mobile,townhouse_type",
                  "/6181_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/41.063821,-73.484116,40.346544,-74.472886_rect/9_zm/u1_sch/")

zillow_ny <- property_types %>% 
  map_dfr(~ paste0(ny_base_url[1], .x, ny_base_url[3]) %>% 
        scrape_zillow_urls(first_page = .,
                           page_range = c(1:20),
                           definitely_sleep_timer = 3,
                           save_file = FALSE,
                           between_pages_sleep_timer = 5
                           # filename = paste0("zillow_ny_", .x))
        ))

zillow_ny %>% 
  mutate(borough = "ny") %>% 
  distinct() %>%
  group_by(z_id, label) %>%
  # .[c(190, 4903, 194, 4909, 192, 4904, 188, 4901, 189, 4902, 144, 4899, 1224, 4896, 1241, 4895, 2502, 4897, 2570, 4898, 2621, 4900, 3678, 4944), ]
  filter(row_number() == 1) %>%
  spread(label, value)#%>% 
  # mutate(date_sold = lubridate::mdy(date_sold))  %>%
  # filter(date_sold >= "2019-01-29") %>%
  # 
  write_results(filename = "fixed/zillow_ny_NEW")


# ---- single borough example ----
ny_urls  <- paste0("https://www.zillow.com/homes/recently_sold/New-York-NY/house,condo,apartment_duplex,mobile,townhouse_type/6181_rid/1-_beds/1-_baths/50000-_price/201-_mp/1-_size/1-_built/globalrelevanceex_sort/41.063821,-73.484116,40.346544,-74.472886_rect/9_zm/u1_sch/",
             1:20, "_p/")

zillow_ny <- ny_urls %>%
  map_dfr(~ read_html(.x) %>% 
            scrape_zillow(., between_pages_sleep_timer = 3, definitely_sleep_timer = 0.25))

write_results(zillow_ny, filename = "zillow_ny")



test_dtls_link <- read_html("https://www.zillow.com/homes/for_sale/2086484737_zpid/40.796333,-73.895159,40.739258,-74.018755_rect/12_zm/1_fr/")

test_dtls_link %>% html_nodes(".zsg-icon-recently-sold") %>%
  html_text()
