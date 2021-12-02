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
results$country_3_letter_code[results$country_3_letter_code == 'ROC'] <- 'RUS'
results$country_3_letter_code[results$country_3_letter_code == 'OAR'] <- 'RUS'
results$country_3_letter_code[results$country_3_letter_code == 'EUN'] <- 'URS'
results$country_3_letter_code[results$country_3_letter_code == 'FRG'] <- 'GER'
results$country_3_letter_code[results$country_3_letter_code == 'GDR'] <- 'GER'
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
  mutate(medal_type = fct_relevel(as.factor(medal_type), c('GOLD','SILVER', 'BRONZE'))) %>%
  inner_join(attendance, by = c(country_3_letter_code = 'code')) %>%
  mutate(medals_per_attendance = count_by_medal / attendances) %>%
  group_by(country_3_letter_code) %>%
  mutate(total_per_attendance = sum(medals_per_attendance))
  
medals_by_country

gg_medals_by_country <- ggplot(medals_by_country[1:30,], aes(reorder(country_3_letter_code, -total), count_by_medal, fill = medal_type)) +
  geom_bar( stat='identity') +
  scale_fill_manual("Medals", values = c("GOLD" = "gold2", "SILVER" = "gray", "BRONZE" = "#CC6600")) +
  geom_text(aes(label = signif(count_by_medal, digits = 3)), size = 3, position = position_stack(vjust = 0.5)) +
  geom_text(aes(country_3_letter_code,
                signif(total, digits = 3),
                label = signif(total, digits = 3),
                vjust = -0.25,
                group = country_3_letter_code)) +
  ggtitle("Medal Count for Top 10 Countries") +
  xlab("Countries") +
  ylab("Medal Count") +
  theme_minimal() 

gg_medals_by_country + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                             axis.text.x = element_text(angle=50, size=10, vjust=0.5),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())

 
# plotting for medal counts per attendance
medals_by_country_attendance <- medals_by_country %>%
  arrange(desc(total_per_attendance))

gg_medals_by_country_attendance <- ggplot(medals_by_country_attendance[1:30,], aes(reorder(country_3_letter_code, -total_per_attendance), medals_per_attendance, fill = medal_type)) +
  geom_bar( stat='identity') +
  scale_fill_manual("Medals", values = c("GOLD" = "gold2", "SILVER" = "gray", "BRONZE" = "#CC6600")) +
  geom_text(aes(label = signif(medals_per_attendance, digits = 3)), size = 3, position = position_stack(vjust = 0.5)) +
  geom_text(aes(country_3_letter_code,
                signif(total_per_attendance, digits = 3),
                label = signif(total_per_attendance, digits = 3),
                vjust = -0.25,
                group = country_3_letter_code)) +
  ggtitle("Medal Count for Top 10 Countries by Attendance") +
  xlab("Countries") +
  ylab("Medal Count") +
  theme_minimal() 

gg_medals_by_country_attendance + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                             axis.text.x = element_text(angle=50, size=10, vjust=0.5),
                             axis.ticks.y = element_blank(),
                             axis.text.y = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())

# gdp clean up
gdp_per <- read_csv("GDP_per_capita.csv")
gdp_per[gdp_per == '..'] <- NA
colnames(gdp_per) <- c('name','code','series','series_code','1960',
                   '1962','1964','1966','1968','1970',
                   '1972','1974','1976','1978','1980',
                   '1982','1984','1986','1988','1990',
                   '1992','1994','1996','1998','2000',
                   '2002','2004','2006','2008','2010',
                   '2012','2014','2016','2018','2020')
gdp_per_long <- pivot_longer(gdp_per, cols= starts_with(c('1','2')), names_to = 'year', values_to = 'gdp_per_capita') %>%
  select(-c(series,series_code))

gdp_per_long$year <- as.numeric(gdp_per_long$year)
gdp_per_long$gdp_per_capita <- as.numeric(gdp_per_long$gdp_per_capita)
gdp_per_long <- gdp_per_long %>%
  replace_na(list(gdp_per_capita = 0))

gdp_tot <- read_csv("GDP.csv")
gdp_tot[gdp_tot == '..'] <- NA
gdp_tot <- gdp_tot[,1:65]
gdp_tot_long <- pivot_longer(gdp_tot, cols = starts_with(c('1','2')), names_to = 'year', values_to = 'gdp_total') %>%
  select(-c('Indicator Name','Indicator Code'))
colnames(gdp_tot_long) <- c('name', 'code', 'year', 'gdp_total')
gdp_tot_long$year <- as.numeric(gdp_tot_long$year)
gdp_tot_long <- gdp_tot_long %>%
  replace_na(list(gdp_total = 0))

military <- read_csv("Military Spending.csv")
military[military == '..'] <- NA
military <- military[,1:65]
military <- pivot_longer(military, cols = starts_with(c('1','2')), names_to = 'year', values_to = 'military_percent_gdp') %>%
  select(-c('Indicator Name','Indicator Code'))
colnames(military) <- c('name', 'code', 'year', 'military_percent_gdp')
military$year <- as.numeric(military$year)
military <- military %>%
  replace_na(list(military_percent_gdp = 0))


# gdp_long[gdp_long$code == "RUS",] %>% .[complete.cases(.),]

# curing data for # of participation by country
event_participation <- results %>%
  group_by(slug_game, country_3_letter_code, country_name, .drop = FALSE) %>%
  summarize(competed_in = n()) %>%
  right_join(hosts[,c(1,4,6,7)], by = c(slug_game = 'game_slug')) %>%
  arrange(country_3_letter_code, game_year)

event_participation$game_location[event_participation$game_location == "Australia, Sweden"] <- "Australia"
event_participation$country_3_letter_code[event_participation$country_3_letter_code == "BRN"] <- "BHR"
event_participation$country_3_letter_code[event_participation$country_3_letter_code == "BRU"] <- "BRN"

event_participation$slug_game <- as.factor(event_participation$slug_game)
event_participation$country_3_letter_code <- as.factor(event_participation$country_3_letter_code)

event_participation$game_location[event_participation$game_location == "China"] <- "People's Republic of China"
event_participation$game_location[event_participation$game_location == "United States"] <- "United States of America"
event_participation$game_location[event_participation$game_location == "USSR"] <- "Soviet Union"

#country_list <- as.factor(event_participation$country_name)
#levels(country_list)

#host_list <- as.factor(event_participation$game_location)
#levels(host_list)

event_participation <- event_participation %>%
  mutate(host = if_else(game_location == country_name, "Y", "N")) %>%
  mutate(host = as.factor(host))

medals_by_country_event <- results %>%
  group_by(country_3_letter_code, slug_game, medal_type) %>%
  summarize(won = sum(!is.na(medal_type))) %>%
  drop_na() %>%
  right_join(event_participation, by = c("slug_game", "country_3_letter_code")) %>%
  arrange(country_3_letter_code, game_year, medal_type) %>%
  replace_na(list(bronze_won = 0, silver_won = 0, gold_won = 0)) %>%
  inner_join(hosts[,c("game_season", "game_year", "game_slug")], by = c("slug_game" = "game_slug", "game_year", "game_season"))

medals_by_country_event_total <- medals_by_country_event %>%
  group_by(country_3_letter_code, country_name, game_year, game_season, game_location) %>%
  summarize(total = sum(won), host = host, slug_game = slug_game, competed_in = competed_in) %>%
  replace_na(list(total = 0)) %>%
  distinct()

# plot for participation vs won titles
png(filename="participation_vs_won.png", width=5000, height=5000)
ggplot(medals_by_country_event_total, aes(competed_in, total, label = country_3_letter_code, color = game_season)) +
  geom_label(aes(size = 10)) +
  facet_wrap(~game_year + game_season, scales = 'free') +
  scale_color_manual("legend", values = c("Summer" = "red", "Winter" = "blue"))
dev.off()

png(filename="participation_vs_log_won.png", width=5000, height=5000)
ggplot(medals_by_country_event_total, aes(log(competed_in), log(total), label = country_3_letter_code, color = game_season)) +
  geom_label(aes(size = 10)) +
  facet_wrap(~game_year + game_season, scales = 'free') +
  scale_color_manual("legend", values = c("Summer" = "red", "Winter" = "blue"))
dev.off()

gg_medals_by_country_event_total_USA <- ggplot(medals_by_country_event_total[medals_by_country_event_total$country_3_letter_code == 'USA',]) +
  geom_point(aes(competed_in, total, color = host)) +
  facet_wrap(~game_season, scales = 'free') +
  scale_color_manual("Legend", values = c("Y" = "green", "N" = "red")) +
  ggtitle("Total Medals for Competing Nations") +
  xlab("Competed In") +
  ylab("Total Medals")
gg_medals_by_country_event_total_USA + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                             panel.grid.major = element_blank())

# country code clean up
gdp_per_long$code <- as.factor(gdp_per_long$code)
gdp_tot_long$code <- as.factor(gdp_tot_long$code)

gdp_per_levels <- levels(gdp_per_long$code)
gdp_tot_levels <- levels(gdp_tot_long$code)

medals_by_country_event_total$country_3_letter_code <- as.factor(medals_by_country_event_total$country_3_letter_code)
medals_levels <- levels(medals_by_country_event_total$country_3_letter_code)

setdiff(medals_levels, gdp_per_levels)
setdiff(gdp_per_levels,medals_levels)

gdp_per_long$code <- as.character(gdp_per_long$code)
gdp_tot_long$code <- as.character(gdp_tot_long$code)

gdp_per_long$ISO3 <- gdp_per_long$code

corrections <- read_csv('corrections.csv')

for (i in 1:nrow(corrections)) {
  before <- corrections$from[i]
  after <- corrections$to[i]
  country <- corrections$name[i]
  if ((country == 'Channel Islands') | ((before %in% gdp_per_long$code) & (after %in% medals_by_country_event_total$country_3_letter_code))) {
    gdp_per_long$code[gdp_per_long$code == before] <- after
    gdp_tot_long$code[gdp_tot_long$code == before] <- after
    military$code[military$code == before] <- after
  } else {
    print(country)
    print(i)
  }
}






joined_for_predict <- medals_by_country_event_total %>%
  inner_join(gdp_per_long, by = c('country_3_letter_code' = 'code', 'game_year' = 'year')) %>%
  mutate(gdp_per_capita = as.numeric(gdp_per_capita)) %>%
  filter(gdp_per_capita != 0) %>%
  inner_join(gdp_tot_long, by = c('country_3_letter_code' = 'code', 'game_year' = 'year', 'name' = 'name')) %>%
  mutate(population = floor(gdp_total / gdp_per_capita)) %>%
  inner_join(military, by = c('country_3_letter_code' = 'code', 'game_year' = 'year', 'name' = 'name')) %>%
  mutate(military_total = military_percent_gdp / 100 * gdp_total)

check <- joined_for_predict[joined_for_predict$country_name != joined_for_predict$name,]

gg_total_medals_per_capita <- ggplot(joined_for_predict, aes(gdp_per_capita, total, label = country_3_letter_code)) +
  geom_label() +
  ggtitle("Total Medals Won by GDP per Capita") +
  xlab("GDP per Capita") +
  ylab("Total Medals")
gg_total_medals_per_capita + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank())

gg_log_total_medals_total_gdp <- ggplot(joined_for_predict, aes(log10(gdp_total), total, label = country_3_letter_code, fill = game_season, alpha = 0.2)) +
  geom_label(label.r = unit(0.5, "lines")) +
  facet_wrap(~game_season, scales = 'free') +
  ggtitle("Total Medals Won by Log Total GDP") +
  ylab("Total Medals") + 
  xlab("Log Total GDP")
gg_log_total_medals_total_gdp + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank()) +
  labs(fill = "Season")

gg_total_medals_pop <- ggplot(joined_for_predict, aes(log(population), total, label = country_3_letter_code)) +
  geom_label() +
  ggtitle("Total Medals by Country Population") +
  xlab("Country Population") +
  ylab("Total Medals")
gg_total_medals_pop + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())

#needs to be updated
ggplot(joined_for_predict, aes(total, label = country_3_letter_code, fill = game_season)) +
  geom_bar() +
  facet_wrap(~host, scales = 'free') 

#needs to be updated (title)
gg_competed_total_medals <- ggplot(joined_for_predict, aes(competed_in, total, label = country_3_letter_code, fill = game_season)) +
  geom_label() +
  ggtitle("Total Medals Won in Different Seasons") +
  xlab("Competed In") +
  ylab("Total Medals")
gg_competed_total_medals + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank()) +
  labs(fill = "Season")


gg_military_total_medals <- ggplot(joined_for_predict, aes(military_percent_gdp, total, label = country_3_letter_code, fill = game_season)) +
  geom_label() +
  ggtitle("Total Medals Won by Military Spending Percentage of GDP") +
  xlab("Military Percentage of GDP") +
  ylab("Total Medals")
gg_military_total_medals + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank()) +
  labs(fill = "Season")

gg_total_military_total_medals <- ggplot(joined_for_predict, aes(military_total, total, label = country_3_letter_code, fill = game_season)) +
  geom_label() +
  facet_wrap(~game_season, scales = 'free') +
  ggtitle("Total Medals Won by Total Military Spending") +
  xlab("Total Military Spending") +
  ylab("Total Medals")
gg_total_military_total_medals + theme(plot.title = element_text(size = 15, lineheight = 0.8, hjust = 0.5, face = "bold"), 
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank()) +
  labs(fill = "Season")

joined_for_predict <- joined_for_predict %>%
  mutate(country_3_letter_code = as.factor(country_3_letter_code),
         game_year = as.factor(game_year),
         game_season = as.factor(game_season)
         )

library(ggmap)
register_google('AIzaSyBOyePqErRX7KawzZmPmamEYcAXdBGsODc')

countries <- data.frame(levels(as.factor(joined_for_predict$country_name)))
colnames(countries) <- c('name')

#coords <- mutate_geocode(countries, name)
#write_csv(coords,'coords.csv')
coords <- read_csv('coords.csv')

joined_for_predict <- joined_for_predict %>%
  inner_join(coords, by = c('country_name' = 'name')) %>%
  mutate(abs_lat = abs(lat)) %>%
  left_join(distinct(medals_by_country[,1:2]), by = 'country_3_letter_code')

joined_for_predict$ISO3[joined_for_predict$ISO3 == 'XKX'] <- 'KOS'
names(joined_for_predict)[names(joined_for_predict) == 'total.x'] <- 'total_at_event'
names(joined_for_predict)[names(joined_for_predict) == 'total.y'] <- 'total_all_time'

joined_for_predict <- joined_for_predict %>%
  mutate(gdp_ln = log(gdp_total)) %>%
  group_by(slug_game) %>%
  mutate(part_norm = competed_in / sum(competed_in))

joined_for_predict$total_at_event_sqrt <- sqrt(joined_for_predict$total_at_event)

set.seed(101)
sample <- sample.int(n = nrow(joined_for_predict), size = floor(.75*nrow(joined_for_predict)), replace = F)
train <- joined_for_predict[sample, ]
valid  <- joined_for_predict[-sample, ]

write_csv(train,'train.csv')
write_csv(valid,'valid.csv')

install.packages('rworldmap')
library(rworldmap)
library(RColorBrewer)
install.packages('GISTools')
library(GISTools)

joinData <- joinCountryData2Map( joined_for_predict,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "ISO3")

png(filename="world_map_all_time.png", width=5000, height=2500)
theMap <- mapCountryData(joinData,
                         nameColumnToPlot="total_all_time",
                         addLegend=F,
                         colourPalette=brewer.pal(9,"Set3"),#'palette',
                         #Paired, Set3, Pastel1
                         catMethod=c(0,5,25,50,250,500,750,1000,3000),
                         #catMethod='logFixedWidth',
                         missingCountryCol = 'grey',
                         xlim=c(-80,-20),
                         ylim=c(0,60),
                         #mapRegion='africa',
                         borderCol='black',
                         mapTitle = 'All Time Medals Won')
do.call(addMapLegend, c(theMap, legendWidth=1, legendMar = 2,legendLabels = "all"))
labelCountries(joinData, nameCountryColumn = "total_all_time", col = "black", cex = 0.8)
dev.off()

mapBubbles(joinData,
           nameZSize="total_all_time",
           symbolSize=2,
           nameZColour="total_all_time",
           catMethod=c(0,5,25,50,250,500,750,1000,3000),
           colourPalette=add.alpha(brewer.pal(5,'Spectral'),0.8),
           borderCol='black',
           mapRegion='europe',
           plotZeroVals=T,
           addLegend=F)
labelCountries(joinData, nameCountryColumn = "total_all_time", col = "black", cex = 0.8)

png(filename="total_all_time.png", width=1000, height=800)
mapBubbles(joinData,
           nameZSize="total_all_time",
           symbolSize=20,
           nameZColour="total_all_time",
           catMethod=c(0,5,25,50,250,500,750,1000,3000),
           colourPalette=add.alpha(brewer.pal(5,'BrBG'),0.8),
           borderCol='grey',
           xlim=c(80,140),
           ylim=c(-50,50),
           #mapRegion='europe',
           plotZeroVals=T,
           lwd=1,
           addLegend=F,
           addColourLegend=T,
           colourLegendPos='topright',
           colourLegendTitle='Medal Count',
           landCol = "dark gray")
labelCountries(joinData, nameCountryColumn = "total_all_time", col = "black", cex = sqrt(joinData$total_all_time)/3)
dev.off()


labelCountries(joinData, nameCountryColumn = "total_all_time", col = "black", cex = sqrt(joinData$total_all_time) / 20)