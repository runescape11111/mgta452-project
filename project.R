library(tidyverse)
library(forcats)

library(rvest)

url <- 'https://en.wikipedia.org/wiki/List_of_participating_nations_at_the_Summer_Olympic_Games'

# scraping on wikipedia for attendance
attendance_data <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div[1]/table[3]') %>%
  html_table()
attendance_processing <- attendance_data[[1]]
attendance <- attendance_processing[,c('A','Code','Total')]
attendance[attendance == 'Soviet Union [^]'] <- 'Soviet Union'
colnames(attendance) <- c('country_name','code', 'attendances')
attendance$attendances <- as.numeric(attendance$attendances)

athletes <- read_csv("olympic_athletes.csv")
hosts <- read_csv("olympic_hosts.csv")
medals <- read_csv("olympic_medals.csv")

results <- read_csv("olympic_results.csv")
results$medal_type <- fct_relevel(as.factor(results$medal_type), c('GOLD','SILVER', 'BRONZE'))

# summarize medal counts by country
medals_by_country <- results[complete.cases(results$medal_type),] %>%
  group_by(country_3_letter_code, medal_type) %>%
  summarize(total_each = n()) %>% 
  pivot_wider(names_from = medal_type, values_from = total_each) %>%
  replace_na(list(GOLD = 0, SILVER = 0, BRONZE = 0)) %>%
  mutate(total = BRONZE + SILVER + GOLD) %>%
  arrange(desc(total)) %>%
  pivot_longer(cols = c(GOLD, SILVER, BRONZE), names_to = "medal_type", values_to = "count_by_medal") %>%
  inner_join(attendance, by = c(country_3_letter_code = 'code')) %>%
  mutate(medals_per_attendance = count_by_medal / attendances) %>%
  group_by(country_3_letter_code) %>%
  mutate(total_per_attendance = sum(medals_per_attendance))
  
medals_by_country

ggplot(medals_by_country[1:30,], aes(reorder(country_3_letter_code, -total), count_by_medal, fill = medal_type)) +
  geom_bar( stat='identity') +
  scale_fill_manual("legend", values = c("GOLD" = "yellow", "SILVER" = "gray", "BRONZE" = "brown"))

# plotting for medal counts per attendance
ggplot(medals_by_country[1:30,], aes(reorder(country_3_letter_code, -total_per_attendance), medals_per_attendance, fill = medal_type)) +
  geom_bar( stat='identity') +
  scale_fill_manual("legend", values = c("GOLD" = "yellow", "SILVER" = "gray", "BRONZE" = "brown")) +
  geom_text(aes(label = signif(medals_per_attendance, digits = 3)), size = 3, position = position_stack(vjust = 0.5)) +
  geom_text(aes(country_3_letter_code, signif(total_per_attendance, digits = 3), label = signif(total_per_attendance, digits = 3), vjust = -0.25, group = country_3_letter_code))

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
gdp_long <- pivot_longer(gdp, cols= starts_with(c('1','2')), names_to = 'year', values_to = 'gdp_per_capita') %>%
  select(-c(series,series_code))

gdp_long$year <- as.numeric(gdp_long$year)
gdp_long$value <- as.numeric(gdp_long$value)
gdp_long[complete.cases(gdp_long),]

try <- results %>%
  inner_join(hosts[,c("game_slug","game_year")], by = c("slug_game"="game_slug")) %>%
  drop_na(medal_type) %>%
  select(-c(athletes, rank_equal, country_name, athlete_url, athlete_full_name, value_unit, value_type, rank_position))

# gdp_long[gdp_long$code == "RUS",] %>% .[complete.cases(.),]

# curing data for # of participation by country
event_participation <- results %>%
  group_by(slug_game, country_name) %>%
  summarize(competed_in = n()) %>%
  inner_join(hosts[,c(1,7)], by = c(slug_game = 'game_slug')) %>%
  arrange(country_name, game_year)

event_participation$slug_game <- as.factor(event_participation$slug_game)
event_participation$country_name <- as.factor(event_participation$country_name)

