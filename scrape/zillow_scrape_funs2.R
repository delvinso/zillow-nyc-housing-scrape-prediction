library(rvest)
library(tidyverse)



# ---- for testing purposes -----

test <- read_html("https://www.zillow.com/homes/recently_sold/Queens-County-NY/_type/1347_rid/1-_beds/50000-_price/197-_mp/1-_size/1-_built/globalrelevanceex_sort/40.850696,-73.584023,40.49187,-74.078408_rect/10_zm/")
test <- read_html("https://www.zillow.com/homes/recently_sold/New-York-NY/6181_rid/2-_beds/365000-84457000_price/1417-327888_mp/globalrelevanceex_sort/41.063821,-73.484116,40.346544,-74.472886_rect/9_zm/")

# test <- read_html("https://www.zillow.com/homes/for_sale/Manhattan-New-York-NY/12530_rid/1-_size/globalrelevanceex_sort/40.958641,-73.726159,40.600398,-74.220543_rect/10_zm/")
# test <- read_html("https://www.zillow.com/homes/recently_sold/Manhattan-New-York-NY/12530_rid/1-_size/globalrelevanceex_sort/40.958641,-73.726159,40.600398,-74.220543_rect/10_zm/1_p/")
# test <- read_html(brooklyn_urls[3])

html <- test %>%
  html_nodes(".photo-cards li article")

cards <- html

cards



# ---- individual functions -----

# ---- from cards ----

get_status <- function(html){
  raw_prices <- html %>% 
    html_nodes(".zsg-photo-card-caption") %>%
    html_node(".zsg-photo-card-status")  %>%
    html_text()%>%
   str_replace_all(. , "SOLD: \\$", "") # %>%
   # remove commas and periods
  #  str_replace(., "\\.|\\,", "") %>%
  #   # replace M with million
  #   str_replace(., "M", "000000") # 100 times too big
  # # str_replace(., "M", "0000") 
  
  # 1.65M becomes 165 000 000 instead of 1 650 000 - MOST DATA
  # 1.65M becomes 1 650 000
  
  #   # 12.8M becomes 128 000 000 instead of 1 65 000
  
} 

get_id <- function(html){
  # get id
  html %>%
    html_attr("id")
  
}

get_lat_lon <- function(html){
  # latitude and longitude
  geo <- html %>% 
    html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
    # html_node(".itemprop span")
    html_node("[itemprop=geo]") 
  
  lon <- geo %>%
    html_node("[itemprop=longitude]") %>% 
    html_attr("content") %>%
    as.numeric()
  lat <- geo %>%
    html_node("[itemprop=latitude]") %>% 
    html_attr("content") %>%
    as.numeric()
  
  return(list(lat, lon))
}


get_prop_sold <- function(html){
  # params
  
  house_prop <- html %>% 
    html_node(".zsg-photo-card-info") %>%
    html_text() %>%
    strsplit(" · ")
  
  pc_sqft <- house_prop %>%
    map_chr(1) %>%
    str_extract(., '\\(?[0-9,.]+\\)?') %>%
    str_replace(.,",", "")
  
  beds <- house_prop %>% 
    map_chr(2)   %>%
    str_extract(., '\\(?[0-9,.]+\\)?') 
  
  
  baths <- house_prop %>% 
    map_chr(3) %>%
    str_extract(., '\\(?[0-9,.]+\\)?') 
  
  area <- house_prop %>% 
    map_chr(4) %>% 
    str_extract(., '\\(?[0-9,.]+\\)?') %>%
    str_replace(.,",", "")
  
  
  res <- list(pc_sqft, beds, baths, area)
  
  return(res)
}

get_address <- function(html){
  
  
  
  
  st_address <- html %>% 
    html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
    html_node("span.hide")  %>%
    html_nodes(xpath = '//*[@itemprop="streetAddress"]')  %>%
    html_text() %>%
    str_trim() %>%
    str_to_lower()
  
  locality <- html %>%
    html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
    html_node("span.hide")  %>%
    html_nodes(xpath = '//*[@itemprop="addressLocality"]') %>%
    html_text() %>%
    str_trim() %>%
    str_to_lower()
  
  
  
  region <- html %>%
    html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
    html_node("span.hide")  %>%
    html_nodes(xpath = '//*[@itemprop="addressRegion"]') %>%
    html_text() %>%
    str_trim() %>%
    str_to_lower()
  
  
  postal_code <- html %>%
    html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
    html_node("span.hide")  %>%
    html_nodes(xpath = '//*[@itemprop="postalCode"]') %>%
    html_text() %>%
    str_trim() 
  
  
  return(list(st_address, locality, region, postal_code))
}

get_date_sold <- function(html){
  
  html %>% 
    html_nodes(".zsg-photo-card-img") %>%
    html_text() %>%
    gsub("Sold ", "", .)
}

# ----vfrom card URL - quick facts and features ----

get_school_info <- function(html){
  
  nearby_school_names <- html %>%
    html_nodes(".nearby-schools-name") %>%
    html_text() %>%
    str_trim() %>% 
    .[-1]
  
  nearby_schools_grades <- html %>%
    html_nodes(".nearby-schools-grades") %>%
    html_text() %>%
    str_trim() %>% 
    .[-1] 
  
  nearby_schools_rating <- html %>%
    html_nodes(".nearby-schools-rating") %>%
    html_text() %>%
    .[-1] %>%
    str_split_fixed(., " ", n = 3) %>%
    .[, 1]
  
  # account for less than 3 schools
  # 
  # if(length(nearby_school_names) < 3){nearby_school_names[3] <- "NA"}
  # if(length(nearby_schools_grades) < 3){nearby_school_grades[3] <- "NA"}
  # if(length(nearby_schools_rating) < 3){nearby_school_rating[3] <- "NA"}
  # 

  
  
  return(list(nearby_school_names, nearby_schools_grades, nearby_schools_rating))
}

get_neighbourhood <- function(html){
  html %>%
    html_nodes(".zsg-h2.hdp-collapsible-title") %>%
    html_text() %>%
    .[1] %>% # first element is neighborhood %>%
    gsub("Neighborhood: ", "", .)
}

get_quick_facts <- function(html){
  
  facts_nodes <- html %>%
    html_nodes(".home-facts-at-a-glance-section") 
  
  facts <- facts_nodes %>%
    html_nodes(".fact-label") %>%
    html_text() %>%
    str_trim()
  
  facts_values <- facts_nodes %>%
    html_nodes(".fact-value") %>%
    html_text() %>%
    str_trim()
  
  facts_values <- replace(facts_values, facts_values == "No Data", NA)
  
  return(data.frame(facts, facts_values, stringsAsFactors = FALSE))
}

get_zestimate <- function(html){
  zestimate <- html %>%
    html_nodes(".zestimate.primary-quote") %>%
    html_text() %>%
    str_extract(., '\\(?[0-9,.]+\\)?')  %>%
    str_replace_all(., ",", "")
  if(length(zestimate) == 0) zestimate <- NA
  
  return(zestimate)
}

get_neighbourhood_zestimate_median <- function(html){
  
  nb_zestimate_med <- html %>% 
    html_nodes(".zsg-content_collapsed") %>%
    html_text() %>%
    .[1] %>%
    str_replace_all(. ,"\\$|,", "")
  
  if(length(nb_zestimate_med) == 0) zestimate <- NA
  
  return(nb_zestimate_med)
}

# from card URL - features WIP


# ---- high-level functions ----

# parses cards for general overview, such as price sold, address,, lat/longitude, etc
get_parse_cards <- function(cards){

  status <- get_status(cards)
  
  z_id <- get_id(cards)
  
  lat_lon <- get_lat_lon(cards)
    
    lat <- lat_lon[[1]]
    lon <- lat_lon[[2]]
    
  prop <- get_prop_sold(cards)
  # prop <- get_prop(cards)
  
    pc_sqft <- prop[[1]]
    beds <- prop[[2]]
    baths <- prop[[3]]
    area <- prop[[4]]
  
  # address
  address <- get_address(cards)
  
    st_address <- address[[1]]
    locality <- address[[2]]
    region <- address[[3]]
    postal_code <- address[[4]]
  
  # date
  date_scraped <- Sys.Date()
  
  # date sold
  
  date_sold <- get_date_sold(cards)
  
  df <- tibble(z_id, 
               # price, 
               status,
               beds, 
               baths, 
               # pc_sqft,
               "sqft" = area, 
               lat, 
               lon,
               st_address,
               locality,
               region,
               postal_code,
               date_scraped,
               date_sold
               # details_link
  )
  return(df)
}


# gets facts and other characteristics for each property, found when clicking onto a link
# TODO: account for NAs outside or inside function
get_facts <- function(zillow_link2, z_id){
  
  # quick facts

  
  url <- paste0("https://www.zillow.com", zillow_link2)
  
  zillow_link2 <-read_html(url)
  
  # zp id 
  z_id <- str_split_fixed(zillow_link2, "/", n = 5)[, 4]
  z_id <- gsub("_zpid", "", z_id) %>% paste0("zpid_", .)
  z_id
  
  
  facts <- tryCatch(get_quick_facts(zillow_link2),
                    error = function(err) NA)
  
  if(nrow(facts) == 0){
    facts <- data.frame(#category = rep("facts", 6),
                        facts = c("type", "year_built", "heating", "cooling", "parking", "hoa"),
                        facts_values = rep(NA, 6),
                        z_id = z_id,
                        stringsAsFactors = FALSE)
  }


  # nearby school

  # account for NAs or no schools

  school_info <- tryCatch( get_school_info(zillow_link2),
                           error = function(err) NA)

  if(any(is.na(school_info)) | is.na(school_info)){
    nearby_school_names <- rep(NA, 3)
    nearby_schools_grades <-  rep(NA, 3)
    nearby_schools_rating <-  rep(NA, 3)

  } else{

    nearby_school_names <- school_info[[1]]
    nearby_schools_grades <- school_info[[2]]
    nearby_schools_rating <- school_info[[3]]

  }

  # neighbourhood
  
  neighbourhood <- get_neighbourhood(zillow_link2)
  
  # zestimate, although not all properties will have a zestimate.
  zestimate <- get_zestimate(zillow_link2)
  
  # neighbourhood median zestimate
  
  nb_zestimate_med <- get_neighbourhood_zestimate_median(zillow_link2)
  
  
  # zillow_test <- read_html("https://www.zillow.com/homedetails/659-E-52nd-St-Brooklyn-NY-11203/30656281_zpid/")
  
  # putting it all together into a dataframe
  
  facts_tbl <- data.frame("label" = c(facts[, 1], 
                                      "neighbourhood", 
                                      "zestimate"),
                                      # paste0(rep(c("nearby_school_names", "nearby_school_grades", "nearby_schools_rating"),
                                      #            # repeat for length of the school info vector up to 3
                                      #            each = length(school_info[[1]])), c(1:length(school_info[[1]])))),
                          "value" = c(facts[, 2],
                                      neighbourhood,
                                      zestimate
                                      # nearby_school_names,
                                      # nearby_schools_rating,
                                      # nearby_schools_grades
                                      ),
                          "z_id" = z_id) %>% 
    mutate(label =  str_to_lower(label) %>%
             str_replace_all( " ", "_"),
           category = "facts") %>% 
    select(category, label, value, z_id) %>%
    as_tibble()
  
  
  return(facts_tbl)
}



# wrapper around url which extracts from a cards URL for characteristics + features 
get_more_details <- function(zillow_link){
  
  # zillow_link <- read_html("https://www.zillow.com/homedetails/432-Park-Ave-PH95-New-York-NY-10022/2096657162_zpid/")
  
  url <- paste0("https://www.zillow.com", zillow_link)
  
  zillow_link2 <-read_html(url)
  
  # zp id 
  z_id <- str_split_fixed(zillow_link2, "/", n = 5)[, 4]
  z_id <- gsub("_zpid", "", z_id) %>% paste0("zpid_", .)
  z_id

  
  # facts #
  facts <- get_facts(zillow_link2, z_id)
  #debug
  # print(facts)

  return(facts)
  
}


definitely_get_data = function(func = get_facts, n_tries = 10, definitely_sleep_timer = 0, x){
  #http://www.brodrigues.co/blog/2018-03-12-keep_trying/
  
  
  try_number = 1
  # most of the time, this won't return otherwise because most errors result in a 'char' object, nonetheless it
  # is still a safeguard
  result = "NO DESCR"
  possibly_func = purrr::possibly(func, otherwise = "NO DESCR", quiet = FALSE)
  
  # sometimes description returns nothing (length of 1) or an error (NA)
  # implements a fix to iterate until we get the description. until n tries
  while(nchar(result[1])  <= 10 && try_number <= n_tries || is.na(result[1]) && try_number <= n_tries ){
    
    # prints out url if try exceeds 3, also puts a sleep timer (arbitrary)
    if(try_number >= 3){
      print(paste("Try number:", try_number, "for url", x))
      sleep <- 1
    }
    try_number <- try_number + 1
    result <- possibly_func(x) # uses description as the check, this assumes that if the description is good, so is the job id 
    # Sys.sleep(definitely_sleep_timer)
  }
  
  definitely_sleep_timer <- sample.int(c(definitely_sleep_timer), size = 1)
  
  print(paste0("Between Properties Sleep Timer: ", definitely_sleep_timer, "s"))
  
  Sys.sleep(definitely_sleep_timer)
  
  
  return(result)
}

# ---- main function -----
scrape_zillow <- function(html, between_pages_sleep_timer = 0, definitely_sleep_timer = 3){
  
  # each house is a card under the photo-cards class and article node
  print(definitely_sleep_timer)
  
  # parse overview of cards and then individual card quick facts + features
  cards <- html %>% 
    html_nodes(".photo-cards li article")
  
  cards
  
  property_overview <- get_parse_cards(cards)
  
  property_overview
  
  # from cards, extract each properties URL 
  # details_link <- cards %>%
  #   html_nodes(".zsg-photo-card-content.zsg-aspect-ratio-content") %>%
  #   html_node("a") %>% 
  #   html_attr("href") 
  
  # use URL to extract quick facts
  # details_res <- details_link %>%
  #   map_dfr(~ definitely_get_data(x = .x, 
  #                                 n_tries = 3, 
  #                                 definitely_sleep_timer = definitely_sleep_timer ))
  # 
  # # join back facts with card parsed data
  # res <- property_overview %>% 
  #   mutate("dtls_link" = details_link) %>% 
  #   left_join(details_res, by = "z_id")
  # 
  # debug
  # print(res)
  
  # TIMER BEFORE NEXT PAGE 
  
  between_pages_sleep_timer <- sample.int(c(between_pages_sleep_timer), size = 1)
  print(paste0("Between Pages Sleep Timer: ", between_pages_sleep_timer, "s"))
  Sys.sleep(between_pages_sleep_timer)
  
  
  res <- property_overview
  print(res)
  
  return(res)
}


# ---- for testing purposes 2 -----
test_out <- scrape_zillow(test, between_pages_sleep_timer = 1, definitely_sleep_timer = 1)
test_out %>% spread(label, value)


# ---- saving results to project directory ----
write_results <- function(results, filename, directory = "~/Documents/Projects/zillow/output/"){
  
  # res_name <- deparse(substitute(results))
  current_date <- Sys.Date()
  
  file <- paste0(directory, filename, "_", current_date, ".csv" )
  write_csv(results, path = file)
  
  print(file)
}




