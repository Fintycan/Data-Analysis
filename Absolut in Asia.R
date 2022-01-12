library(utils)
library(tidyverse)
library(readxl)
library(reshape2)
library(directlabels)
library(mefa4)
library(ggrepel)
library(gridExtra)


#Pre Processing

#Import CSV File
vodka <- read_xlsx('C:\\Users\\Finta\\Downloads\\vodka asia (4).xlsx')

#Factorise variables with less than 10 values
unique_vals <- apply(vodka, 2, function(x) {
  length(unique(x))
})

vodka$`Price Band` <- as.factor(vodka$`Price Band`)
vodka$Country <- as.factor(vodka$Country)
vodka$International <- as.factor(vodka$International)
vodka$`Local/Imported`<- as.factor(vodka$`Local/Imported`)

# Clean up and prepare data for analysis
vodka <- vodka %>% rename('2019' = `2019...14`,'2020' = `2019...15`)

vodka <- vodka %>% gather('year','value',colnames(vodka[11:15]))
vodka <- vodka %>% na.omit(value)
vodka$value <- as.numeric(vodka$value)
imported_vodka <- vodka %>% filter(`Local/Imported`== 'Imported')

# Remove non pertinent markets 
vodka <- vodka %>% filter(Country %notin% c('Australia','New Zealand','Laos','Macau','India','Sri Lanka'))

#Combine flavored variants into one variable
flavored_absolut <- c('Absolut Lime Vodka','Absolut Mandrin Vodka','Absolut Raspberri Vodka','Absolut Vanilia Vodka',
                      'Absolut Citron Vodka','Absolut Berri','Acai Vodka','Absolut Apeach Vodka','Absolut Apple Flav. Vodka',
                      'Absolut Berri Acai Vodka', 'Absolut Cherrys Vodka','Absolut Extrakt Flav. Vodka', 'Absolut Flavoured Vodka',
                      'Absolut Grapefruit Vodka', 'Absolut Juice Flav. Vodka', 'Absolut Korea Flav. Vodka', 'Absolut Mango Vodka',
                      'Absolut Pears Vodka', 'Absolut Wild Tea Vodka') 

indexes <- which(imported_vodka$`Brand Line` %in% flavored_absolut)
imported_vodka$`Brand Line`[indexes] <- 'Absolut Flavored Vodka'

# Imported vodka volumes for 2021
imported_vodka %>% filter(year == '2020-12-31') %>%
  group_by(Country,year) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value))

# Volumes in the 'standard' value segment
standard_country_vols <- imported_vodka %>%
  mutate(year = as.factor(year)) %>%
  filter(`Price Band` == 'Standard') %>%
  group_by(Country,year) %>%
  summarise(value = sum(value))%>% 
  ggplot(aes(year,value,group = Country,color = Country)) +
  scale_y_continuous(trans = 'log10') +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  xlab('Year') +
  ylab(' Volumes 000 9lc') +
  ggtitle('Vodka Volumes in Asia(Standard Price Brand)') 

# Absolut volumes by country
absolute_Country_vols <- imported_vodka %>%
  filter(Brand == 'Absolut Vodka', Country %notin% c('China','Myanmar','Cambodia')) %>%
  mutate(year = as.factor(year)) %>%
  group_by(Country,year) %>%
  summarise(value = sum(value))%>% 
  ggplot(aes(year,value,group = Country,color = Country)) +
  scale_y_continuous(trans = 'log2') +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1),guide = guide_axis(check.overlap = TRUE)) +
  geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points")), cex = .8) +
  xlab('Year') +
  ylab(' Volumes 000 9lc') +
  ggtitle('Absolut Volumes in Asia') 


# Absolute volumes by brand
Absolut_Brand_Line <- imported_vodka %>%
  filter(Brand == 'Absolut Vodka', Country %notin% c('China','Myanmar'), Country %in% selected_countries) %>%
  mutate(year = as.factor(year)) %>%
  group_by(Country,year,`Brand Line`) %>%
  ggplot(aes(year,value,fill = `Brand Line`)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country) +
  theme(legend.position="bottom") +
  xlab('Year') +
  ylab('Volumes') +
  ggtitle('Absolut Brand Lines')
  

#Absolute vs competitors in 2020 Standard price band
standard_price_band_2020 <- imported_vodka %>% 
  filter(year == '2020' & `Price Band` == 'Standard', Country != 'China') %>%
  mutate(Brand = ifelse(Brand == 'Absolut Vodka', 'Absolut','Other')) %>%
  group_by(Country,Brand) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ggplot(aes(x = reorder(Country,value),value,fill = Brand)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab('Volumes 000 9lc') +
  ylab('') +
  ggtitle('Absolut vs Rest 2020')

#Absolute vs competitors in 2019 Standard price band
standard_price_band_2019 <- imported_vodka %>% 
  filter(year == '2019' & `Price Band` == 'Standard', Country != 'China') %>%
  mutate(Brand = ifelse(Brand == 'Absolut Vodka', 'Absolut','Other')) %>%
  group_by(Country,Brand) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  ggplot(aes(x = reorder(Country,value),value,fill = Brand)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab('Volumes 000 9lc') +
  ylab('') +
  ggtitle('Absolut vs Rest 2019')

grid.arrange(standard_price_band_2020,standard_price_band_2019)

# Vodka volumes by price band
category_price_bands <- imported_vodka %>%
  filter(`Price Band` %in% c('Value','Standard')) %>%
  group_by(year,`Price Band`) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = `Price Band`)) +
  geom_bar(stat="identity",position="dodge")

selected_countries <- c('South Korea', 'Thailand','Vietnam','Taiwan','Philippines','Indonesia','Singapore','Japan','Cambodia')


# Vodka volumes by price band
standard_vs_value_by_country <- vodka %>% filter(Country %in% selected_countries & `Price Band` %in% c('Value','Standard')) %>%
  group_by(year,`Price Band`,Country) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = `Price Band`)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country) +
  xlab('Year') +
  ylab('Volumes') +
  ggtitle('Standard vs Value Price Band') +
  theme(legend.position="bottom")

  
# Absolut vs Competitors broken down by competitor
absolute_vs_rest <- imported_vodka %>% 
  filter(`Price Band` == 'Standard' & Country %in% selected_countries) %>%
  mutate(Brand = ifelse(Brand == 'Absolut Vodka', 'Absolute','Other')) %>%
  group_by(Country,Brand,year) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = Brand)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country)

# Absolut vs Smirnoff
absolute_vs_smirnoff <- 
  imported_vodka %>%
  filter(Brand %in% c('Absolut Vodka','Smirnoff Vodka') & Country %in% selected_countries) %>%
  group_by(Brand,Country,year) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = Brand)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country)

#Premium+ Price Bands
above_standard_band <- c('Premium','Super Premium', 'Prestige','Ultra Premium') 
above_standard_vols <- imported_vodka %>%
  filter(`Price Band` %in% above_standard_band, Country != 'Myanmar') %>%
  group_by(`Price Band`,Country,year) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = `Price Band`)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country) +
  theme(legend.position="bottom")

#Elyx vs other premium 
elyx_vs_rest <- imported_vodka %>%
  filter(`Price Band` %in% c('Super Premium','Ultra Premium') & Country %in% selected_countries & Country %notin% c('Hong Kong','Singapore')) %>%
  mutate(`Brand Line` = ifelse(`Brand Line` == 'Absolut Elyx Vodka','Absolut Elyx','Competitor')) %>%
  group_by(Country,`Brand Line`,year) %>% 
  summarise(value = sum(value)) %>%
  ggplot(aes(year,value,fill = `Brand Line`)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Country) +
  xlab('Year') +
  ylab('Volumes') +
  ggtitle('Absolut Elyx vs Price Band') 
  
  