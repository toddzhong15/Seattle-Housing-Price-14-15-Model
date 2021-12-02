

rm(list = ls()) # Clear environment
gc()            # Clear memory
cat("\f")       # Clear the console
options(scipen = 4) # Remove scientific notation for numbers

packages <- c("ggplot2" # Best plotting
              , "readr"
              , "tidyr" # To load RAND dataset
              , "ggstatsplot"
)

#load library
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggstatsplot)
#load csv
housing <- read_csv("/Users/yanli/Desktop/cleaned_data1.csv")
head(housing)
housing %>% select(-sqft_basement)


reg <- lm(price~yr_dummy+bedrooms+bathrooms+sqft_living+sqft_lot+floors+
            waterfront+view+condition+grade+sqft_above+
            yr_built+sqft_living15+sqft_lot15, data = housing)
summary(reg)

m <- housing %>% select(month)
y <- housing %>% select(yr_dummy)
p <- housing %>% select(price)

cor(m, y)
cor(p,m)
cor(p,y)

library(car)
linearHypothesis(reg, c("month","yr_dummy"))

