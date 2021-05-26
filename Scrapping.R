library(rvest)#Easily Harvest (Scrape) Web Pages. html_nodes
library(tidyverse)#Designed to make it easy to install and load multiple 'tidyverse' packages in a single step.
library(magrittr)#A Forward-Pipe Operator for R
library(scales)
library(knitr)
library(lubridate)#Lubridate provides tools that make it easier to parse and manipulate dates.
library(ggrepel)#This package contains extra geoms for ggplot2.

library(sjmisc) #string contains 
library(countrycode)
library(stringr)
# Verbose regular expressions
library(rebus)



#So I thought this would be a quick, fun side project - ofcourse it is never sooo easy cleaning up data
# scrapped from the internet is never fun!!! Sad face. 
##Overview of visa requirements/exemptions for entry into the Federal Republic of Germany

url <- "https://www.auswaertiges-amt.de/de/service/visa-und-aufenthalt/staatenliste-zur-visumpflicht/207820"


country <- url %>% read_html() %>% html_nodes('table') %>% html_table()


dt <- country[[1]]

colnames(dt) <- dt[1,]
dt <- dt[-1,]
dt <- dt[1:198,1:2]


#I decided to just remove the information inside the brackets to make the country coding easiers remove the information in the brackets
dt_re <- gsub("\\s*\\([^\\)]+\\)","", dt$`Country/ territorial community`)



dt <- cbind(dt_re, dt[,2])
dt <-dt[-1,]
colnames(dt)
dt <-  dt %>% dplyr::rename(countries = "dt_re",
                              visum = "Entry visa required no/yes")


# getting the country names to match universial standards
code_country_4 <- lapply(dt5[[1]], function(x) countrycode(x, origin = 'country.name.en', destination = 'country.name.en')) %>% unlist()

#code_country_4 <- map_df(dt5[[1]], function(x) countrycode(x, origin = 'country.name.de', destination = 'country.name.en'))


#putting it together                        
visum_yes_no <- cbind(code_country_4, dt[,2]) %>%  as.data.frame()
#visum_yes_no <- visum_yes_no %>% dplyr::rename(countries = "code_country_4")
colnames(visum_yes_no) <- c("countries", "yes/no")
 #replacing the & with and 
visum_yes_no <- gsub("&","and", visum_yes_no)


# merging my shp df and visum_yes_no
# Ah shit need to find out where I got the file from- at least I have a rough idea what i googled 
file <- unzip("World_Countries_(Generalized).zip")
countries_map <- st_read("World_Countries__Generalized_.shp")

colnames(visum_yes_no)
merged_countries <- merge(countries_map, visum_yes_no, by.x = "COUNTRYAFF", by.y = "countries", all.x = T)

View(merged_countries)

#checking for missing values
missing_values <- subset(merged_countries, is.na(merged_countries$`yes/no`))
# AH still a lot but managable 
View(missing_values)

colnames(merged_countries)
#lets do Congo 
which(grepl("Congo", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[45:46] <- "yes"
#Germany
which(grepl("Germany", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[87] <- "no"

#Russia
which(grepl("Russian Federation", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[175] <- "yes"
View(country[[1]])
#Czech Republic
which(grepl("Czech Republic", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[52] <- "no" 

#Trinidad and Tobago
which(grepl("Trinidad and Tobago", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[211] <- "no (1)" 

#Bosnia and Herzegovina
which(grepl("Bosnia and Herzegovina", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[28] <- "no (1, 5)" 

#Palestinian Territory
which(grepl("Palestinian Territory", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[165] <- "yes" 

#Myanmar
which(grepl("Myanmar", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[140] <- "yes" 

#Côte d'Ivoire
which(grepl("Côte d'Ivoire", merged_countries$COUNTRYAFF))
merged_countries$`yes/no`[48] <- "yes" 
colnames(visum_yes_no)


# more  information what do the numbers mean?
urlEn <- "https://www.auswaertiges-amt.de/en/einreiseundaufenthalt/-/231148"
#para <- urlEn %>% read_html() %>% html_nodes('table') %>% html_text() %>% as.data.frame()
para <- urlEn %>% read_html() %>% html_nodes('table') %>% html_table()
para <- para[[1]]

colnames(para) <- para[1,]
para <- para[200:208,1]

View(para)
colnames(para) <- "numbs_explained"

no_vis_numb <- para


#empty column
merged_countries$no_vis_num <- NA

#check what different combinations there are
unique_combo <- lapply(merged_countries$`yes/no`, function (x) str_extract_all(x, "\\([^()]+\\)")[[1]]) %>% unique() %>% unlist()                       

#Use unique combination for case_when - look at better way to do this!

#case_when better than nested ifelse
merged_countries <-  merged_countries %>% mutate(
                  no_vis_num = lapply(merged_countries$`yes/no`, function(x) 
                      case_when( 
                                (str_contains(x, '1') & str_contains(x, '3') & str_contains(x, '7')) ~ paste0(no_vis_numb[1,],"\n",no_vis_numb[3,],"\n",no_vis_numb[7,]),
                                (str_contains(x, '1') & str_contains(x, '3')) ~ paste0(no_vis_numb[1,], "\n", no_vis_numb[3,]),
                                (str_contains(x, '1') & str_contains(x, '4')) ~ paste0(no_vis_numb[1,],"\n",no_vis_numb[4,]),
                                 (str_contains(x, '1') & str_contains(x, '5')) ~paste0(no_vis_numb[1,],"\n",no_vis_numb[5,]),
                                (str_contains(x, '1') & str_contains(x, '8')) ~ paste0(no_vis_numb[1,],"\n",no_vis_numb[8,]),
                                 str_contains(x, '1') ~ paste0(no_vis_numb[1,]),
                                 str_contains(x, '2') ~ paste0(no_vis_numb[2,]),
                                 str_contains(x, '3') ~ paste0(no_vis_numb[3,]),
                                  str_contains(x, '4') ~ paste0(no_vis_numb[4,]),
                                  str_contains(x, '5') ~ paste0(no_vis_numb[5,]),
                                  str_contains(x, '6') ~ paste0(no_vis_numb[6,]),
                                  str_contains(x, '7') ~ paste0(no_vis_numb[7,]),
                                  str_contains(x, '8') ~ paste0(no_vis_numb[8,]),
                                  str_contains(x, '9') ~ paste0(no_vis_numb[9,]),
                                   TRUE ~ "NA"))) 


merged_countries$no_vis_num <- merged_countries$no_vis_num %>% unlist() %>% as.character()


merged_countries$no_vis_num[merged_countries$no_vis_num  == "NA"] <- ""


saveRDS(merged_countries, file = "visum_data.rds")

########################

###### Second panel - from Germany to other countries

#######################


###  https://www.solo-urlaub.de/alle-visafreien-laender-im-ueberblick/
## Scrapping table from  2019 which countries you can travel without a visum 

url_table <- "https://www.solo-urlaub.de/alle-visafreien-laender-im-ueberblick/"

table <- url_table %>% read_html() %>% html_node('table') %>% html_table()
visum_ger <- table[,2:3]
colnames(visum_ger) <- c("Laender", "visum")


#change the countries names to  english for merging of  the maps
code_country_en <- map_df(visum_ger[,1], function(x) countrycode(x, origin = 'country.name.de', destination = 'country.name.en'))
table(is.na(code_country_en))
visum_ger_en <- cbind(visum_ger, code_country_en)


colnames(visum_ger_en) <- c("Laender", "visum", "country")

#see which rows have missing values
new_DF <- visum_ger_en[rowSums(is.na(visum_ger_en)) > 0,]
View(new_DF)
as.numeric(rownames(new_DF))

#Äquatorial Guinea
visum_ger_en[8,3] <- "Equatorial Guinea"
#Jamaica
visum_ger_en[67,3] <- "Jamaica"
#Kirgistan
visum_ger_en[77,3] <- "Kyrgyzstan"
#Marschall Inseln
visum_ger_en[105,3] <- "Marshall Islands"
#Moldawien
visum_ger_en[110,3] <- "Moldova"
#Papua New Guinea
visum_ger_en[133,3] <- "Papua New Guinea"
#Weißrussland
visum_ger_en[196,3] <- "Belarus"

visum_ger_en <- visum_ger_en[,2:3]


#merging the dfs
merged_countries_ger <- merge(countries_map, visum_ger_en, by.x = "COUNTRYAFF", by.y = "country", all.x = T)
missing_val <- merged_countries_ger[rowSums(is.na(merged_countries_ger)) > 0,]
View(missing_val)

#checking for missing values
missing_values <- subset(merged_countries_ger, is.na(merged_countries_ger$visum))
# AH still a lot but managable 
View(missing_values)

colnames(merged_countries_ger)
#lets do Congo 
which(grepl("Congo", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[45:46] <- "Visum benötigt"
#Germany
which(grepl("Germany", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[87] <- "visafrei"

#Russia
which(grepl("Russian Federation", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[175] <- "Visum benötigt"

#Czech Republic
which(grepl("Czech Republic", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[52] <- "visafrei" 

#Trinidad and Tobago
which(grepl("Trinidad and Tobago", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[210] <- "visafrei 90 Tage" 

#Bosnia and Herzegovina
which(grepl("Bosnia and Herzegovina", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[28] <- "visafrei 90 Tage" 

#Palestinian Territory
which(grepl("Palestinian Territory", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[165] <- "visafrei 90 Tage" 

#Myanmar
which(grepl("Myanmar", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[140] <- "Visum benötigt / eVisa" 

#Côte d'Ivoire
which(grepl("Côte d'Ivoire", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[48] <- "Visum benötigt" 
colnames(visum_yes_no)
#saint Lucia
which(grepl("Saint Lucia", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[178] <- "visafrei 90 Tage" 
#Sao Tome and Principe
which(grepl("Sao Tome and Principe", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[182] <- "visafrei 15 Tage" 
#Antigua and Barbuda
which(grepl("Antigua and Barbuda", merged_countries_ger$COUNTRYAFF))
merged_countries_ger$visum[7] <- "visafrei 180 Tage" 

merged_countries_ger$visum[merged_countries_ger$visum  == "NA"] <- ""

saveRDS(merged_countries_ger, file = "visum_data_ger.rds")

##############
######## https://www.passportindex.org/passport/germany/ -COVID-19  -Update
#################

url_table_2 <- "https://www.passportindex.org/passport/germany/"

table_2 <- url_table_2 %>% read_html() %>% html_node('table') %>% html_table() %>% as.data.frame()
colnames(table_2) <- c("country","visum")
View(table_2)

merged_countries_en <- merge(countries_map, table_2, by.x = "COUNTRYAFF", by.y = "country", all.x = T)

missing_values <- merged_countries_en[rowSums(is.na(merged_countries_en)) > 0,]
View(missing_values)

which(grepl("United States", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[234:240] <- "eTA/90 days"

#
which(grepl("Vietnam", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[246] <- "COVID-19 ban"
#Myanmar
which(grepl("Myanmar", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[140] <- "visa on arrival / eVisa/30 days"

#Germany
which(grepl("Germany", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[87] <- "visa-free"

#Palestinian Territory
which(grepl("Palestinian Territory", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[165] <- "visa-free"

#Congo DRC
which(grepl("Congo", merged_countries_en$COUNTRYAFF))
merged_countries_en$visum[46] <- "visa required"

merged_countries_en$visum[merged_countries_en$visum  == "NA"] <- ""

saveRDS(merged_countries_en, file = "visum_data_en.rds")

