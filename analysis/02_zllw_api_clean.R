library(tidyverse)
library(naniar)
library(RColorBrewer)
library(lubridate)
source("analysis/zllw_helpers.R")
theme_set(theme_minimal(base_size = 14))
options(tibble.max_extra_cols = 5)

# ---- read in files ----
# zllw <- read_csv("~/Documents/Projects/zillow/data/zllw_api_04_22_2019_v2.csv")
zllw <- read_csv("~/Documents/Projects/zillow/data/zllw_api_04_23_2019.csv")


zllw$zpid <- as.factor(zllw$zpid)
zllw$city <- as.factor(zllw$city)
zllw$type <- as.factor(zllw$type)
zllw$fips <- as.factor(zllw$fips)
zllw$city <- as.factor(str_to_lower(as.character((zllw$city))))
zllw$zip <- as.factor(zllw$zip)
zllw$last_sold_date <- mdy(zllw$last_sold_date)

# ---- exploratory data analysis and cleaning ----

glimpse(zllw) # 13352

# ---- year last sold ----
zllw %>%
  mutate(year_sold = year(last_sold_date)) %>%
  count(year_sold, sort = TRUE) %>%
  ggplot(aes(x = year_sold, y = n)) + 
  geom_col() +
  coord_flip()

# remove properties which do not have a known sold date and those sold before 2010
zllw <- zllw %>% mutate(year_sold = year(last_sold_date)) %>%
  filter(!is.na(last_sold_date) & !is.na(last_sold_price)) %>%
  filter(year_sold >= 2010) %>%
  select(-year_sold)

# checking for duplicate properties
zllw %>% 
  count(zpid, sort = TRUE)  %>%
  filter(n > 1)

zllw %>% filter(zpid %in% c("122198068", "2111815001")) %>%
  distinct(last_sold_price, last_sold_date)
# looks to be the same property

# remove duplicate properties 
zllw <- zllw %>%
  group_by(zpid) %>% 
  arrange(desc(tax_assess_year)) %>%
  filter(row_number() == 1) %>% ungroup() 

# drop address
zllw <- zllw %>% select(-c(st, address))

# ---- fips ----
# these are boroughs i believe
ggplot(zllw) +
  geom_point(aes(x = lon, y = lat, colour = fips), show.legend = FALSE, alpha = 0.5, size = 1) +
  scale_colour_brewer(palette = "Set1")

zllw %>% count(fips)

# ---- look at quick summary of the remainder of our data ----

summary(zllw)

# some issues that immediately come to my mind are
# values of 1 for finished and lot sqft
# properties with 0 bed
# last sold price of 1

# ---- year built ----

zllw %>% count(year_built) %>%
  ggplot(aes(x = year_built, y = n)) + 
  geom_col()

zllw %>%
  ggplot(aes(x = year_built)) + 
  geom_histogram()


# ---- last sold price ----

zllw %>% 
  ggplot() + 
  geom_histogram(aes(x = log(last_sold_price)))
# definitely some funky stuff going on on the lower end

# remove houses less than 100k
zllw %>% 
  filter(last_sold_price < 100000) %>%
  # filter(last_sold_price < quantile(last_sold_price, 0.01, na.rm = TRUE)) %>%
  arrange(-last_sold_price) 
# 71 houses

# ---- lot square foot ----

zllw %>% 
  ggplot() + 
  geom_histogram(aes(x = log10(lot_size_sqft)))

# look at the bottom and top percentile
zllw %>%
  filter(lot_size_sqft < quantile(lot_size_sqft, 0.01, na.rm = TRUE))  %>%
  arrange(-lot_size_sqft)

zllw %>%
  filter(lot_size_sqft > quantile(lot_size_sqft, 0.99, na.rm = TRUE))  %>%
  arrange(-lot_size_sqft) 
# remove the bottom and top percentile as they seem to be ridiculous values

lot_sqft_1 <- quantile(zllw$lot_size_sqft, probs = c(0.01), na.rm = TRUE)[1]
lot_sqft_99 <- quantile(zllw$lot_size_sqft, probs = c(0.99), na.rm = TRUE)[1]
zllw %>%
  filter(lot_size_sqft > lot_sqft_1, lot_size_sqft < lot_sqft_99) %>%
  arrange(-lot_size_sqft)   %>% 
  ggplot() + 
  geom_histogram(aes(x = log10(lot_size_sqft)))


# zllw <- zllw[-which(zllw$lot_size_sqft < lot_sqft_1 |  zllw$lot_size_sqft > lot_sqft_99),] 
 zllw$lot_size_sqft <- NULL
# ---- property square feet ----
zllw %>% 
  ggplot() + 
  geom_histogram(aes(x = log10(finished_sqft)))

# a quick google search shows the the avg apartment size is ~ 700sqft, with a minimum square feet of 400 required after 1947.
# so cap it to 300ft?

zllw %>%
  filter(finished_sqft < 300) %>%
  arrange(-finished_sqft) 

# 35 observations
zllw %>%
  filter(finished_sqft >= 300) %>%
  arrange(-finished_sqft) %>%
  ggplot() + 
  geom_histogram(aes(x = log10(finished_sqft)))

# top percentile looks fishhy,

zllw %>% filter(finished_sqft >= quantile(finished_sqft, na.rm = TRUE, probs = 0.99))

zllw %>% group_by(type) %>% 
  summarize(mean_sqft = mean(finished_sqft, na.rm = TRUE), n = n())
# ---- beds ----
zllw %>% 
  count(beds) %>% 
  ggplot() + 
  geom_col(aes(x = beds, y = n)) 

# assess missingness for properties with 0 bedrooms
zllw %>% filter(beds == 0) %>% vis_miss()
# may be best just to remove these


# ---- baths ----

zllw %>% count(baths) %>% arrange(-baths)
# one property has 2149
zllw %>% filter(baths == 2149) 
# looks fine to remove


zllw %>% 
  count(baths, sort = TRUE) %>% 
  filter(baths != 2149) %>%
  ggplot() + 
  geom_col(aes(x = baths, y = n)) 



# top and bottom percentile
quantile(zllw$baths, na.rm = TRUE, probs = c(0.01, 0.999))


zllw %>% filter(baths > quantile(baths, na.rm = TRUE, probs = 0.999))

# ---- housing types ----

table(zllw$type)

# misc, duplex, triplex, quadruplex, unknown and vacantn residential land has low # of levels so we will remove
# also a lot of missing values
zllw <- filter(zllw, !type %in% c("Mobile", "Unknown", "Miscellaneous",
                                  "VacantResidentialLand", "Duplex", "Triplex",
                                  "Quadruplex"))


# 14 observations
# zllw <- zllw %>% filtfinished sqft < 300
# last_sold_price < 100000
quantile(zllw$last_sold_price, probs = c(0.01, 0.99))
beds_001 <- (quantile(zllw$beds, probs = c(0.999), na.rm = TRUE))
baths_001 <- (quantile(zllw$baths, probs = c(0.999), na.rm = TRUE))

zllw <- zllw[-which(zllw$beds > beds_001 | zllw$baths > baths_001),] 
zllw <- zllw[-which(zllw$beds == 0),]
zllw <- zllw[-which(zllw$last_sold_price < 100000),] 
zllw <- zllw[-which(zllw$finished_sqft < 300), ]

zllw # 12900

vis_miss(zllw, warn_large_data = FALSE)



# ---- look at missingness of our data -----
miss_var_summary(zllw) 
miss_var_summary(zllw) %>%
  ggplot(aes(x = reorder(variable, n_miss), y = n_miss, fill = n_miss)) +
  geom_col() + 
  scale_fill_viridis_c() + 
  coord_flip() + 
  guides(fill = FALSE)

# looks like beds, baths, tax assessed value and lot size are highly missing

# first we should examine whether there is any pattern to variables with missing data?
zllw %>% filter(is.na(tax_assess_value)) %>% vis_miss()
zllw %>% filter(is.na(beds) & is.na(baths)) %>% vis_miss()

# so properties missing the # of beds are also missing the # of bathrooms, so let's just drop these
# zllw <- filter(zllw, !(is.na(beds) &is.na(baths))) 

# properties with missing lot size may not be facing the street or is a type of property that doesn't have a lot..
# so let's remove it for now
zllw$lot_size_sqft <- NULL

# also drop properties lacking a tax assessment year
# zllw <- zllw %>% filter(!is.na(tax_assess_year))

# examine relationship between log assessed value and sold price
scttrCor(zllw, aes(x = log10(finished_sqft), y = log10(last_sold_price)))
scttrCor(zllw, aes(x = log10(tax_assess_value), y = log10(last_sold_price)))
# scttrCor(zllw, aes(x = log(lot_size_sqft), y = log(last_sold_price))) # no relationship

# overall the relationship seems moderately strong

# reassess missingness
zllw %>% vis_miss()

zllw #12900

# save as csv
# write_csv(zllw, "data/zllw_clean_04_25_2019_only.csv")



# ---- quick and dirty RF with imputated values ----

library(randomForest)
library(doParallel)
# imputation - https://stats.stackexchange.com/questions/226803/what-is-the-proper-way-to-use-rfimpute-imputation-by-random-forest-in-r
library(missForest)
library(mice)
set.seed(1)
# zllw_mat <- model.matrix(data = zllw[, -c(1, 11)], last_sold_price ~ .)[, -1]
zllw2 <- zllw %>%
  select(-zpid, -zestimate) %>%
  # data.frame() %>%
  mutate(zip = as.factor(as.character(str_sub(zip, 1, 3)))) %>%
  mutate_at(vars(tax_assess_value, finished_sqft, last_sold_price), ~ log10(.)) %>%
  mutate(type = droplevels(type)) 
table(zllw2$city)


# registerDoParallel(cores = 4)
# zllw_impute <- rfImpute(x = zllw2, y = zllw$last_sold_price)
# stopImplicitCluster()

# split our data
set.seed(1)
trainRows <- sample.int(nrow(zllw2), nrow(zllw2) * 0.8, replace = FALSE) 


train_x <- zllw2[trainRows, ]
test_x <- zllw2[-trainRows,] 

train_y <- log(zllw$last_sold_price[trainRows])
test_y <- log(zllw$last_sold_price[-trainRows])

set.seed(1)

dim(train_x);dim(test_x)
length(train_y);length(test_y)



# system.time(imp_train_x <- parlmice(data = train_x[1:100,], m = 5, 
#                                     cluster.seed = 1234, n.core = 4, 
#                                     defaultMethod = c("mean", "rf")))


# train imputation
system.time(imp_train_x_rf <- mice(train_x, m = 10, maxit = 20, printFlag = TRUE, seed = 1234,
                                   method = c("pmm")))
xyplot(imp_train_x_rf,  tax_assess_value ~ last_sold_price)
summary(imp_train_x_rf)

train_x_complete <- complete(imp_train_x_rf)

# test imputation
imp_test_x_rf <- mice(test_x, m = 10, maxit = 20, printFlag = TRUE, seed = 1234,
                      method = c("pmm"))
xyplot(imp_test_x_rf, baths~ last_sold_price)
summary(imp_train_x_rf)

test_x_complete <- complete(imp_test_x_rf)



# quick and dirty random forest

# remove 
quick_rf <- randomForest(x = train_x_complete[, -c(10, 11)],
                         y = train_y,
                         seed = 1234,
                         importance = TRUE)
quick_rf

rf_fit <- predict(quick_rf, train_x_complete)

tibble(obs = train_y, pred = rf_fit) %>% 
  scttrCor(aes(x = pred, y = obs))

levels(test_x_complete$type) <- levels(train_x_complete$type)

rf_preds <- predict(quick_rf, 
                    test_x_complete[, -c(10, 11)])

tibble(obs = test_y, pred = rf_preds) %>% 
  scttrCor(aes(x = pred, y = obs))

ape(obs = test_y, pred = rf_preds) 
# 13.8% without tax assessed value
# 13.0% with RF imputation

# variable importance
data.frame(importance(quick_rf)) %>% 
  rownames_to_column("preds") %>% 
  arrange(desc(X.IncMSE)) %>%
  # .[c(1:1), ] %>%
  ggplot( aes(x = reorder(preds, X.IncMSE), y = X.IncMSE, fill = X.IncMSE)) +
  geom_bar(stat = 'identity', colour = "black") + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + 
  theme(legend.position="none")

