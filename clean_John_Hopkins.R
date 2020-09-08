jhfolder <- "~/ownCloud/coronavirus/lockdown/data/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports/"

# data from John Hopkins
lf <- list.files(jhfolder)
lf <- lf[lf!="README.md"]
dates <- substring(lf, 1, 10)

# rbind all data and slight cleaning:
all_df <- c()

for(ff in lf){
  df <- read.csv(paste0(masterdir, "data/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports/", ff))
  df$date <- substring(ff, 1, 10)
  names(df)[which(names(df) == "Country_Region")] <- "Country.Region"
  names(df)[which(names(df) == "Province_State")] <- "Province.State"
  names(df)[which(names(df) == "Last_Update")] <- "Last.Update"
  df$Country.Region <- as.character(df$Country.Region)
  df$Country.Region[which(df$Country.Region=="Mainland China")] <- "China"
  all_df <- rbind(all_df, df[,c("Province.State", "Country.Region", "Last.Update", "Confirmed", "Deaths", "Recovered", "date")])
}

all_df$date <- as.Date.character(all_df$date, format = "%m-%d-%Y")

# clean countries
library(plyr)
all_df$Country.Region <- as.character(all_df$Country.Region)
all_df$Province.State <- as.character(all_df$Province.State)
all_df$Province.State[all_df$Province.State %in% c("None", "")] <- all_df$Country.Region[all_df$Province.State %in% c("None", "")] 

# correct a few countries / province for consistency
all_df$Country.Region[all_df$Country.Region == "Iran (Islamic Republic of)"] <- "Iran"
all_df$Province.State[all_df$Province.State == "Iran (Islamic Republic of)"] <- "Iran"
all_df$Country.Region[all_df$Country.Region == "Czechia"] <- "Czech Republic"
all_df$Province.State[all_df$Province.State == "Cruise Ship"] <- "Diamond Princess cruise ship"
all_df$Province.State[all_df$Province.State == "Diamond Princess"] <- "Diamond Princess cruise ship"
all_df$Country.Region[all_df$Province.State == "Diamond Princess cruise ship"] <- "Others"
all_df$Province.State[all_df$Province.State == "Fench Guiana"] <- "French Guiana"
all_df$Country.Region[all_df$Province.State == "French Guiana"] <- "France"
all_df$Province.State[all_df$Province.State == "Grand Princess"] <- "Grand Princess Cruise Ship"
all_df$Country.Region[all_df$Province.State == "Greenland"] <- "Denmark"
all_df$Country.Region[all_df$Province.State == "Guadeloupe"] <- "France"
all_df$Country.Region[all_df$Province.State == "Guam"] <- "US"
all_df$Country.Region[all_df$Province.State == "Hong Kong"] <- "China"
all_df$Country.Region[all_df$Province.State == "Macau"] <- "China"
all_df$Country.Region[all_df$Province.State == "Mayotte"] <- "France"
all_df$Province.State[all_df$Province.State == "occupied Palestinian territory"] <- "Palestine"
all_df$Country.Region[all_df$Country.Region == "occupied Palestinian territory"] <- "Palestine"
all_df$Country.Region[all_df$Province.State == "Puerto Rico"] <- "US"
all_df$Country.Region[all_df$Province.State == "Reunion"] <- "France"
all_df$Province.State[all_df$Province.State == "Russian Federation"] <- "Russia"
all_df$Country.Region[all_df$Country.Region == "Russian Federation"] <- "Russia"
all_df$Country.Region[all_df$Province.State == "Saint Barthelemy"] <- "France"
all_df$Province.State[all_df$Province.State == "Korea, South"] <- "South Korea"
all_df$Country.Region[all_df$Country.Region == "Korea, South"] <- "South Korea"
all_df$Province.State[all_df$Province.State == "Republic of Korea"] <- "South Korea"
all_df$Country.Region[all_df$Country.Region == "Republic of Korea"] <- "South Korea"
all_df$Country.Region[all_df$Province.State == "St. Martin"] <- "France"
all_df$Province.State[all_df$Province.State == "Taiwan*"] <- "Taiwan"
all_df$Country.Region[all_df$Country.Region == "Taiwan*"] <- "Taiwan"
all_df$Country.Region[all_df$Province.State == "UK"] <- "UK"
all_df$Province.State[all_df$Province.State == "United Kingdom"] <- "UK"
all_df$Country.Region[all_df$Country.Region == "United Kingdom"] <- "UK"
all_df$Province.State[all_df$Province.State == "French Polynesia" & all_df$date == "2020-03-23"] <- "France" # this one was mistakenly attributed to French Polynesia
all_df$Province.State[all_df$Province.State == "Santa Clara County, CA"] <- "Santa Clara, CA"
# deal with duplicates TODO!!!
#View(all_df[which(duplicated(all_df[, c("Province.State", "Country.Region", "date", "Confirmed", "Deaths", "Recovered")])),])


# deal with the USA which have multiple data lines per date, so sum that:
all_df_USA <- all_df[all_df$Country.Region == "US",]
all_df_USA <- ddply(all_df_USA, .(Province.State, Country.Region, date), summarise, Last.Update = NA, Confirmed = sum(Confirmed, na.rm = T), Deaths = sum(Deaths, na.rm = T), Recovered = sum(Recovered, na.rm = T))
all_df_USA <- all_df_USA[names(all_df)
                         ]
all_df_noUSA <- all_df[!all_df$Country.Region == "US",]

all_df <- rbind(all_df_noUSA, all_df_USA)

# to check country names, etc.
#all_df_max <- ddply(all_df, .(Province.State), summarise, max_cases = max(Confirmed, na.rm = T), max_deaths = max(Deaths, na.rm = T), n_provinces = length(unique(Province.State)))
#all_df_max[which(all_df_max$max_cases > 1000),]

all_df$Province_Country <- paste(all_df$Province.State, all_df$Country.Region, sep = "_")
n <- nrow(all_df)
all_df <- all_df[
  with(all_df, order(Province_Country, date)),
  ]
all_df$incidence <- NA
all_df$death_incidence <- NA

#View(all_df)

for(cc in unique(all_df$Province_Country)){
  sub <- which(all_df$Province_Country == cc)
  n_sub <- length(sub)
  if(n_sub > 1){
    for(i in 2:n_sub){
      if(difftime(all_df[sub[i-1],"date"], all_df[sub[i],"date"]) == -1){
        all_df[sub[i], "incidence"] <- all_df[sub[i], "Confirmed"] - all_df[sub[i-1], "Confirmed"]
        all_df[sub[i], "death_incidence"] <- all_df[sub[i], "Deaths"] - all_df[sub[i-1], "Deaths"]
      }
    }
  }
}

# create numeric date (days in 2020)
process_date <- function(dd){
  dd <- strsplit(as.character(dd), split = "\\-")[[1]]
  if(dd[1]!="2020")stop("date in 2020 only")
  if(dd[2] == "01") return(as.numeric(dd[3]))
  if(dd[2] == "02") return(31 + as.numeric(dd[3]))
  if(dd[2] == "03") return(31 + 29 + as.numeric(dd[3]))
  if(dd[2] == "04") return(31 + 29 + 31 + as.numeric(dd[3]))
  if(dd[2] == "05") return(31 + 29 + 31 + 30 + as.numeric(dd[3]))
  if(dd[2] == "06") return(31 + 29 + 31 + 30 + 31 + as.numeric(dd[3]))
  stop("Jan, Feb, Mar, April, May, June only")
}
all_df$date1 <- all_df$date
all_df$date <- sapply(all_df$date, process_date)
all_df$Province_Country <- gsub(pattern = "\\, ", replacement = "_", x = all_df$Province_Country)
rm(df, cc, ff, i, n, n_sub, sub, all_df_noUSA, all_df_USA, dates, lf)

write.csv(x = all_df, file = paste0(masterdir, "data/jh.csv"), row.names = F)


