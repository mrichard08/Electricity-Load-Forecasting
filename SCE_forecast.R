library(fable)
library(tidyverse)
library(tsibble)
library(tsibbledata)
library(riem)
library(plotly)
library(lubridate)

setwd("~/Grid Modernization/Johanna/Johanna")

# Import load and generation data---------------------------------------------------

filenames <- Sys.glob(file.path("~/Grid Modernization/Johanna/Johanna", "A-Bank-JOHANNA-JOHANNA_220_66_MW_*"))

count <- 0

for (i in filenames){
  if (count == 0){
    ts_data <- read_csv(i)
    count <- count + 1
    }
  else if (count > 0 & count <5) {
    temp <- read_csv(i)
    ts_data <- ts_data %>% bind_rows(temp)
    count <- count + 1
  }
}

ts_data <- ts_data %>% mutate(pv_gen = `Gross MW`-`Cleansed MW`)

# Convert load data to tsibble------------------------------------------------------

load_data <- ts_data[!duplicated(ts_data$Date, fromLast = TRUE),] %>% 
  rename(Demand =`Cleansed MW`) %>% 
  select(Date, Demand) %>% 
  as_tsibble() %>% 
  fill_gaps(Demand = 150)

autoplot(load_data)

# Retrieve temperature data---------------------------------------------------------

sna <- riem_measures("SNA", date_start = "2011-01-01", date_end = "2016-01-01")

sna <- sna %>% mutate(valid = floor_date(valid, unit = "hours")) %>%
  select(Time = valid, Temp = tmpf) %>%
  replace_na(list(Temp = 55.0))

sna <- sna[!duplicated(sna$Time, fromLast = TRUE),]

#write.table(temp_data, file = "~/Grid Modernization/Johanna/Johanna/temp_data.csv")

#temp_data <- read_csv("~/Grid Modernization/Johanna/Johanna/temp_data.csv") %>% as_tsibble() 

temp_data <- sna %>% as_tsibble()

autoplot(temp_data)

# Combine data into single tsibble-------------------------------------------------

comb <- bind_cols(load_data, temp_data$Temp)

# Create forecast-------------------------------------------------------------------

fit <- load_data %>% model(ETS(Demand)) %>% forecast(h=8760) %>% autoplot(load_data)

fit
