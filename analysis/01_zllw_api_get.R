library(ZillowR)
library(XML)
library(broom)
library(tidyverse)
library(naniar)


# ---- Extracting Data from API ----

# first I read in the files that I scraped from zillow.
# there are roughly ~13k unique properties.
# here I use these 13k unique properties and obtain 'more accurate' 
# characteristics for each house by mining Zillow's GetDeepSearchResults endpoint


# ---- read in files ----


setwd("~/Documents/Projects/zillow/")


# get all file names from the 'OLD' scraped results (with sold price error)

infiles <- list.files(path = "output", 
                      # pattern = "FIX",
                      full.names = TRUE)

infiles
# read in files
zllw_dat <- infiles[-1] %>% 
  map(~ read_csv(.x, col_names = TRUE ) %>%
        mutate(status = as.character(status)))  %>% 
  map(~ .x %>% 
        select(-(`mls_#`)) %>%
        mutate(postal_code = as.character(postal_code))) %>% 
  bind_rows()

zllw_dat
# get all file names NEW

infiles_new <- list.files(path = "output/fixed", 
                      pattern = "NEW",
                      full.names = TRUE)

infiles_new
# read in files
zllw_dat_new <- infiles_new %>% 
  map(~ read_csv(.x, col_names = TRUE ) %>%
        mutate(status = as.character(status)))  %>% 
  map(~ .x %>% 
        select(-(`mls_#`)) %>%
        mutate(postal_code = as.character(postal_code))) %>% 
  bind_rows()


# bound to be duplicates which we need to remove
unique_ids <- zllw_dat %>% distinct(z_id) %>% pull(z_id)
unique_ids
length(unique_ids)

# sort by date scraped as the earlier dates have more information. (as of 12/19/2017 a lot of schools are missing), then take the first observation

zllw_dat2 <- zllw_dat %>% 
  arrange(date_scraped) %>%
  group_by(z_id) %>%
  filter(row_number() == 1) %>%
  ungroup()


# for getdeepsearchres ----

# combine and get distinct addresses, postal codes for api scraping
zllw_dat_api <- bind_rows(zllw_dat2, zllw_dat_new)  %>%
  select(z_id, st_address, postal_code) %>%
  distinct()

zllw_dat_api # 13521 

# how many distinct addresses are there
zllw_dat_api %>% distinct(st_address) # 13394

# for property stuff ----

# total # of unique zids
unique_zid <- bind_rows(zllw_dat_new, zllw_dat2) %>% distinct(z_id) %>% pull(z_id)

# there are some school ratings that were empty and thus parsed as a new line.
zllw_dat2 <- zllw_dat2 %>% mutate_at(vars(contains("school")), ~ funs(gsub("\n", NA, .))) 

# zids not in zllw dat2 (old zillow data)
new_zid <- unique_zid[!unique_zid %in% zllw_dat2$z_id]
length(new_zid)

# are the # of unique ids and the # of rows in the data the same?
nrow(zllw_dat2) == length(unique_ids)

# clean up
rm(zllw_dat2, zllw_dat_new, zllw_dat)


# ---- pluto ----
# use pluto to obtain addresses for querying against zillow
# https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page

pluto <- read_csv("data/nyc_pluto_18v2_1_csv/pluto_18v2_1.csv") %>% mutate(id = row_number())

# remove property without a zipcode or address as they can't be queried agaisnt api
# also filter for only  two family, walk up apartment, elevator apartment and condos
pluto_filt <- pluto %>%
  filter(!is.na(zipcode) & !is.na(address)) %>%
  filter(str_detect(bldgclass, "^(A|B|C|D|R)")) %>%
  mutate(address = str_to_lower(address)) %>%
  select(bldgclass, everything()) 

rm(pluto)
# sample by borough 
set.seed(1)
pluto_first <- pluto_filt  %>%
  mutate(address = str_to_lower(address)) %>%
  select(id, address, zipcode, borough) %>%
  group_by(borough) %>% sample_n(size = 800, replace = FALSE) 

pluto_first$id

# filter out properties we already sampled from to make sure there 
# are no duplicates, take a sample of roughly 3.75k properties/borough
# 
set.seed(1)
pluto_second <- pluto_filt %>%
  filter(!id %in% pluto_first$id) %>%
  select(id, address, zipcode, borough) %>%
  group_by(borough) %>% sample_n(size = 3750, replace = FALSE) 


set.seed(1)
pluto_third<- pluto_filt %>%
  filter(!id %in% pluto_first$id, !id %in% pluto_second$id) %>%
  select(id, address, zipcode, borough) %>%
  group_by(borough) %>% sample_n(size = 3750, replace = FALSE)  %>%
  ungroup()

pluto_third

set.seed(1)
pluto_four<- pluto_filt %>%
  filter(!id %in% pluto_first$id, !id %in% pluto_second$id, !id %in% pluto_third$id) %>%
  select(id, address, zipcode, borough) %>%
  group_by(borough) %>% sample_n(size = 3750, replace = FALSE)  %>%
  ungroup()

pluto_four %>%   distinct(address, zipcode, borough)

# ---- GetDeepSearchResults ----

#set up API key
keys <- c("YOUR KEYS HERE")
set_zillow_web_service_id(keys[1])  

# debug ----
# pluto_third
zllw_xml<- GetDeepSearchResults(address = "640 w 237th st aparment 8b", citystatezip = "new york",
                     rentzestimate = TRUE, zws_id = getOption("ZillowR-zws_id"),
                     url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm")

xml_test <- saveXML(zllw_xml$response[["results"]]) %>% 
  xmlParse()

xml_test

xmlValue(xml_test[["//result/finishedSqFt"]])
xmlValue(xml_test[["//result/lotSizeSqFt"]])
xmlValue(xml_test[["//result/useCode"]])
xmlValue(xml_test[["//result/taxAssessmentYear"]])
xmlValue(xml_test[["//result/taxAssessment"]])
xmlValue(xml_test[["//result/yearBuilt"]])
xmlValue(xml_test[["//result/lastSoldPrice"]])
xmlValue(xml_test[["//result/lastSoldDate"]])
xmlValue(xml_test[["//result//address/zipcode"]])
xmlValue(xml_test[["//result//bathrooms"]])


xml_extract(xml_test, address = "3246 Fairmount Ave", api = "deep")
# define zillow function to parse and extract relevant attributes from zillow api calls
# deep = GetDeepSearchResults
# details = GetUpdatedPropertyDetails

xml_extract <-function(xmldata, address, api){
  
  if (api == "deep"){
    do.call(rbind, xpathApply(xmldata, "//result", function(node) {
    

      # initialize dataframe
      dat <- data.frame(zpid = "NA",
                        # fips = "NA",
                        type = "NA",
                        st = "NA",
                        address = address,
                        tax_assess_year = "NA",
                        tax_assess_value = "NA",
                        year_built = "NA",
                        lot_size_sqft = "NA",
                        finished_sqft = "NA",
                        baths = "NA",
                        beds = "NA",
                        zip = "NA",
                        lat = "NA",
                        lon = "NA",
                        zestimate = "NA",
                        last_sold_date = "NA",
                        last_sold_price = "NA",
                        stringsAsFactors = FALSE)
      
      dat$zpid<-xmlValue(node[["zpid"]])
      dat$fips <- xmlValue(node[["FIPScounty"]])
      dat$type <-xmlValue(node[["useCode"]])
      dat$st <- xpathSApply(node,"./address/street", xmlValue)
      dat$tax_assess_year <- xmlValue(node[["taxAssessmentYear"]])
      dat$tax_assess_value <- xmlValue(node[["taxAssessment"]]) #%>% as.numeric()
      dat$year_built <- xmlValue(node[["yearBuilt"]])# %>% as.numeric()
      dat$finished_sqft<-xmlValue(node[["finishedSqFt"]])# %>% as.numeric()
      dat$lot_size_sqft <- xmlValue(node[["lotSizeSqFt"]])# %>% as.numeric()
      dat$baths <- xmlValue(node[["bathrooms"]]) #%>% as.numeric()
      dat$beds <- xmlValue(node[["bedrooms"]])# %>% as.numeric()
      dat$zip <- xpathSApply(node,"./address/zipcode", xmlValue)
      dat$city<-xpathSApply(node,"./address/city", xmlValue)
      # state<-xpathSApply(node,"./address/state", xmlValue) 
      dat$lat<-xpathSApply(node,"./address/latitude", xmlValue)# %>% as.numeric()
      dat$lon<-xpathSApply(node,"./address/longitude", xmlValue) #%>% as.numeric()
      dat$zestimate<-xpathSApply(node,"./zestimate/amount", xmlValue)# %>% as.numeric()
      dat$last_sold_date <- xmlValue(node[["lastSoldDate"]]) 
      dat$last_sold_price <- xmlValue(node[["lastSoldPrice"]]) #%>% as.numeric()
      # lowval<-xpathSApply(node,"./zestimate/valuationRange/low", xmlValue)
      # highval<-xpathSApply(node,"./zestimate/valuationRange/high", xmlValue)
      nb<-xpathSApply(node,"//result/localRealEstate/region",xmlAttrs)[[1]]
      nbid<-xpathSApply(node,"//result/localRealEstate/region",xmlAttrs)[[2]]
      
      return(as_tibble(dat))
    }))

  }
    else if(api == "details"){
      do.call(rbind, xpathApply(xmldata, "//editedFacts", function(node) {
        
      dat <- data.frame(zpid = "NA",
                        address = address, 
                        finished_sqft = "NA",
                        lot_size_sqft = "NA",
                        parking_type = "NA",
                        heat_source = "NA",
                        heat_system = "NA",
                        cool_system = "NA",
                        architecture = "NA",
                        num_floors = "NA",
                        num_rooms = "NA",
                        floor_covering = "NA",
                        # school_district = "NA",
                        exterior_material = "NA",
                        # descr = "NA",
                        stringsAsFactors = FALSE)
      # initialize datafframe
      dat$z_id <- 
      dat$finished_sqft <- xmlValue(node[["finishedSqFt"]])
      dat$lot_size_sqft <- xmlValue(node[["lotSizeSqFt"]])
      dat$parking_type <- xmlValue(node[["parkingType"]])
      dat$heat_source <- xmlValue(node[["heatingSources"]])
      dat$heat_system <- xmlValue(node[["heatingSystem"]])
      dat$cool_system <- xmlValue(node[["coolingSystem"]])
      dat$architecture <- xmlValue(node[["architecture"]])
      dat$num_floors <- xmlValue(node[["numFloor"]])
      dat$num_rooms <- xmlValue(node[["numRooms"]])
      dat$floor_covering <- xmlValue(node[["floorCovering"]])
      # dat$school_district <- xmlValue(node[["//schoolDistrict"]])
      # dat$descr <- xmlValue(node[["//homeDescription"]])
      dat$exterior_material <- xmlValue(node[["exteriorMaterial"]])
      
      return(as_tibble(dat))
      }))
    }
}
# call API (for loop) ----


# iterates through all possible addresses in zllw_dat_api, calling DeepSearchResult 
# on each one and retrieving the properties characteristics
# due to zillows limit on 1000 alls per day, the loop keeps track of the iteration
# number using a counter and uses the next available key when 950 calls are made


iter <- 1 # for api limits
deep_res # make sure doesn't exist
zllw_dat_api

for (i in 7500:nrow(zllw_dat_api)){

  # print(i)}
  if (i %% 990 == 0){ #API limits
    iter = iter + 1
    key = keys[iter]
    set_zillow_web_service_id(key)  #  didnt run 2,9, 10
    print("########################################################")
    print(paste("############  Property", i, "Key", iter, ":", key, "  ############"))
    print("########################################################")
    
  }
# }
  
  # print(paste("Property", i, "Key", iter, ":", key))
  
  # using previously scraped zillow data
  dat <- zllw_dat_api[i, ]
  address <- dat$st_address
  postal_code <- dat$postal_code
  city_state_zip <- paste("ny", postal_code)
  

  # address <- dat$address
  # postal_code <- as.character(dat$zipcode)
  # borough <- dat$borough
  # city_state_zip <- paste("ny", postal_code)
  
  print(i);print(address);print(city_state_zip) # debug
  
  # call zillow API for deep search results
  zllw_xml <- GetDeepSearchResults(address = address, citystatezip = city_state_zip,
                                   rentzestimate = TRUE, zws_id = getOption("ZillowR-zws_id"),
                                   url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm")
  
  
  # only extract data if zllw_xml did not fail (ie. error code = 0 )
  if(zllw_xml$message$code == 0){
    
    xml_test <- saveXML(zllw_xml$response[["results"]]) %>% 
      xmlParse()
    
    
    temp_res <- as_tibble(xml_extract(xml_test, address, api = "deep"))
    
  }
  else{
    print(paste(address, city_state_zip, "Failed\n", "Error Code:", zllw_xml$message$code,
                "Error Message:", zllw_xml$message))
    
    temp_res <- data.frame(zpid = "NA",
                      # fips = "NA",
                      type = "NA",
                      st = "NA",
                      address = address,
                      tax_assess_year = "NA",
                      tax_assess_value = "NA",
                      year_built = "NA",
                      lot_size_sqft = "NA",
                      finished_sqft = "NA",
                      baths = "NA",
                      beds = "NA",
                      zip = postal_code,
                      lat = "NA",
                      lon = "NA",
                      zestimate = "NA",
                      last_sold_date = "NA",
                      last_sold_price = "NA", stringsAsFactors = FALSE)
    
  }
  
  if(exists("deep_res")){
    deep_res <- bind_rows(temp_res, deep_res)
  }else{
    deep_res <- temp_res
  }
  
  print(temp_res)
  
}

# there are a lot of results that end up missing, are they in a specific area?
no_res <- deep_res_copy  %>% 
  mutate_all(~ ifelse(. == "NA", NA, .)) %>%
  filter(is.na(zpid)) %>%
  mutate(address = str_to_lower(address))
# join on original data with boroughs
no_res %>%
  left_join(pluto_third, by = c("address")) %>%
  mutate(zip = str_sub(zip, 1, 3)) %>%
  count(zip)
# they seem to belong to brooklyn and queens..


deep_res %>% 
  as_tibble() %>%
  filter(zpid != "NA") %>% 
  filter(!is.na(last_sold_price)) %>%
  filter(!is.na(beds) | !is.na(baths)) %>%
  miss_var_summary()
  
# write_csv(deep_res %>%  filter(zpid != "NA"), "data/pluto_four_api_04_30_2019.csv")


# ---- some quick eda and cleaning ----

# compare number of non-na results and # of our original addresses
nrow(zllw_dat_api) # 13521

deep_res %>% filter(!is.na(last_sold_price)) %>% count() #14386

zpid_na <- deep_res %>% filter(type == "NA")
zpid_na # 794
write_csv(zpid_na, "data/zllw_zpid_no_results_04_22_2019.csv")

# remove NA houses and NA sold prices and save to csv
deep_res <- deep_res %>% filter(type != "NA" | !is.na(last_sold_price)) %>% distinct()# %>%


#########
apply(res[c("type", "baths", "beds", "zip")], 2, table)

# cast to long format
res_long <- deep_res %>%
  mutate_at(vars(tax_assess_value, lot_size_sqft, baths, beds, year_built,
                 finished_sqft, zestimate, last_sold_price), ~ ((as.numeric(.)))) %>%
  mutate_at(vars(tax_assess_value, lot_size_sqft,
                 finished_sqft, zestimate, last_sold_price), ~ (log(.))) %>% 
  select(tax_assess_value, lot_size_sqft, baths, beds, year_built,
         finished_sqft, zestimate, last_sold_price)# %>%


cor(res$baths, log(res$last_sold_price), na.rm = TRUE)


res %>% arrange(-baths)





zllw_test_fail <- GetDeepSearchResults(address = "4477 douglas ave #14", citystatezip = "ny 10471",
                                 rentzestimate = TRUE, zws_id = getOption("ZillowR-zws_id"),
                                 url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm")



if(zllw_test_fail$message$code == 0){
  #do stuff
}
else{
  print(zllw_test_fail$message$code)
  print(zllw_test_fail$message)
  
}

# ---- updated property details ----


# ---- debug  -----
updated_dtls <-GetUpdatedPropertyDetails(zpid = gsub("zpid_", "", zllw_dat_api$z_id[22]), zws_id = getOption("ZillowR-zws_id"),
                                         url = "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm")

updated_dtls_xml <- saveXML(updated_dtls$response) %>% 
  xmlParse()

xmlValue(updated_dtls_xml[["//zpid"]])

xmlValue(updated_dtls_xml[["//editedFacts/finishedSqFt"]])
xmlValue(updated_dtls_xml[["//editedFacts/lotSizeSqFt"]])
xmlValue(updated_dtls_xml[["//editedFacts/parkingType"]])
xmlValue(updated_dtls_xml[["//editedFacts/heatingSources"]])
xmlValue(updated_dtls_xml[["//editedFacts/heatingSystem"]])
xmlValue(updated_dtls_xml[["//editedFacts/coolingSystem"]])
xmlValue(updated_dtls_xml[["//editedFacts/architecture"]])
xmlValue(updated_dtls_xml[["//editedFacts/numFloor"]])
xmlValue(updated_dtls_xml[["//editedFacts/numRooms"]])
xmlValue(updated_dtls_xml[["//editedFacts/floorCovering"]])
# xmlValue(updated_dtls_xml[["//schoolDistrict"]])
# xmlValue(updated_dtls_xml[["//homeDescription"]])
xmlValue(updated_dtls_xml[["//editedFacts/exteriorMaterial"]])



xmlValue(updated_dtls_xml[["//homeDescription"]])
updated_dtls_xml

xml_extract(updated_dtls_xml, address = "", api = "details")
# GetUpdatedPropetyDetails, call API (for loop) ----


iter <- 2 # for api limits
res # make sure doesn't exist
zllw_dat_api


# for (i in 1:5){

for (i in 1:nrow(zllw_dat_api)){
  # print(i)
  
  if (i %% 950 == 0){ #API limits
    iter = iter + 1
    key = keys[iter]
    set_zillow_web_service_id(key)  #  didnt run 2,9, 10
    print("########################################################")
    print(paste("############  Property", i, "Key", iter, ":", key, "  ############"))
    print("########################################################")
    
  }
  # }
  
  # print(paste("Property", i, "Key", iter, ":", key))
  
  dat <- zllw_dat_api[i, ]

  # get zid 
  
  zid <- dat$z_id
  zid <- gsub("zpid_", "", zllw_dat_api$z_id[i])
  address <- dat$st_address
  
  # print(i);print(address);print(city_state_zip) # debug
  
  # call zillow API for deep search results
  zllw_xml <- GetUpdatedPropertyDetails(zpid = zid, zws_id = getOption("ZillowR-zws_id"),
                                       url = "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm")
  
  
  # only extract data if zllw_xml did not fail (ie. error code = 0 )
  if(zllw_xml$message$code == 0){
    
    xml_test <- saveXML(zllw_xml$response) %>% 
      xmlParse()
    
    
    temp_res <- as_tibble(xml_extract(xml_test, address, api = "details"))
    
    # temp_res <- as_tibble(xml_extract(xml_test, address)[1, ])
    
    # print(res)
    
    
    
  }
  else{
    print(paste(address, city_state_zip, "Failed\n", "Error Code:", zllw_xml$message$code,
                "Error Message:", zllw_xml$message))
    
    
    temp_res <- dat <- data.frame(zpid = zid,
                                  address = address, 
                                  finished_sqft = "NA",
                                  lot_size_sqft = "NA",
                                  parking_type = "NA",
                                  heat_source = "NA",
                                  heat_system = "NA",
                                  cool_system = "NA",
                                  architecture = "NA",
                                  num_floors = "NA",
                                  num_rooms = "NA",
                                  floor_covering = "NA",
                                  # school_district = "NA",
                                  exterior_material = "NA",
                                  # descr = "NA",
                                  stringsAsFactors = FALSE)
    
  }
  
  temp_res$zpid <- zid
  
  
  if(exists("res")){
    res <- bind_rows(res, temp_res)
  }else{
    res <- temp_res
  }
  
  print(temp_res)
  
}

# save csv
# write_csv(res, "zllw_api_details_04_23_2019.csv")#res
# missingness
miss_var_summary(res)


