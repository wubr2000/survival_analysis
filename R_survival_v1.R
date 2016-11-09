library(dplyr)
library(data.table)
library(survival)

#Example
url <- 'http://socserv.mcmaster.ca/jfox/Books/Companion/data/Rossi.txt'
Rossi <- read.table(url, header = TRUE)
Rossi[1:5,1:10]

mod.allison <- coxph( Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio, data=Rossi)
summary(mod.allison)

#Data from csv
listings_df <- read.table('/Users/bwu/testing/survival_analysis/sf_listings_TE.csv', header = TRUE, sep=",")
listings_df$mprid[1:10]
colnames(listings_df)

mod.sf <- coxph( Surv(T,E) ~ bedrooms + photo_count + price_psf + bedrooms*sqft, data=listings_df)
summary(mod.sf)

mod2.sf <- coxph( Surv(T,E) ~ bedrooms + photo_count + price_psf + bedrooms*sqft
                              + ldp_views_day1 + ldp_views_day3 + ldp_views_day7, data=listings_df)

summary(mod2.sf)




