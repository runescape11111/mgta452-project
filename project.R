library(tidyverse)
library(forcats)

athletes <- read_csv("olympic_athletes.csv")
hosts <- read_csv("olympic_hosts.csv")
medals <- read_csv("olympic_medals.csv")

results <- read_csv("olympic_results.csv")
results$medal_type <- fct_relevel(as.factor(results$medal_type), c('GOLD','SILVER', 'BRONZE'))

# summarize medal counts by country
medals_by_country <- results[complete.cases(results$medal_type),] %>%
  group_by(country_3_letter_code, medal_type) %>%
  summarize(sum = n())
medals_by_country

# gdp clean up
gdp <- read_csv("GDP.csv")
gdp[gdp == '..'] <- NA
colnames(gdp) <- c('name','code','series','series_code','1960',
                   '1962','1964','1966','1968','1970',
                   '1972','1974','1976','1978','1980',
                   '1982','1984','1986','1988','1990',
                   '1992','1994','1996','1998','2000',
                   '2002','2004','2006','2008','2010',
                   '2012','2014','2016','2018','2020')
gdp_long <- pivot_longer(gdp, cols= starts_with(c('1','2')), names_to = 'year', values_to = 'value')

gdp_long$series <- as.factor(gdp_long$series)
levels(gdp_long$series)

gdp_long$year <- as.numeric(gdp_long$year)
gdp_long$value <- as.numeric(gdp_long$value)
gdp_long[complete.cases(gdp_long),]