

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
#seperate date variable
housing <- housing %>%
  separate(date, c('year','month',"day"), '-')
head(housing)
#variales change to numeric
housing<- housing %>%
  mutate(year= as.numeric(year),
         month= as.numeric(month),
         day= as.numeric(day),
         price= as.numeric(price),
         bedrooms= as.numeric(bedrooms),
         bathrooms= as.numeric(bathrooms),
         sqft_living= as.numeric(sqft_living),
         sqft_lot=as.numeric(sqft_lot),
         floors=as.numeric(floors), 
         waterfront=as.numeric(waterfront),
         view=as.numeric(view),
         condition=as.numeric(condition),
         grade=as.numeric(grade),
         sqft_above=as.numeric(sqft_above),
         sqft_basement=as.numeric(sqft_basement),
         yr_built=as.numeric(yr_built),
         yr_renovated=as.numeric(yr_renovated),
         lat=as.numeric(lat),
         long=as.numeric(long),
         sqft_living15=as.numeric(sqft_living15),
         sqft_lot15=as.numeric(sqft_lot15))
head(housing)
#create dataset with only variables with possibility of outlier
housing2<-housing %>%
  select(price, sqft_living, sqft_lot, sqft_above, sqft_basement, sqft_living15, sqft_lot15)
#visualize outliers
boxplot(housing2)$out
priceviz<-ggbetweenstats(housing,
               year, price, outlier.tagging = TRUE)
priceviz
sqft_lotviz<-ggbetweenstats(housing,
               year, sqft_lot, outlier.tagging = TRUE)
sqft_lotviz
sqft_lot15viz<-ggbetweenstats(housing,
               year, sqft_lot15, outlier.tagging = TRUE)
sqft_lot15viz
#identify outliers
Q1 <- quantile(housing$price, probs=c(.25, .75), na.rm = FALSE)
iqr1 <- IQR(housing$price)
up1 <-  Q1[2]+1.5*iqr1 # Upper Range  
low1<- Q1[1]-1.5*iqr1 # Lower Range
Q2 <- quantile(housing$sqft_lot, probs=c(.25, .75), na.rm = FALSE)
iqr2 <- IQR(housing$sqft_lot)
up2 <-  Q2[2]+1.5*iqr2 # Upper Range  
low2<- Q2[1]-1.5*iqr2 # Lower Range
Q3 <- quantile(housing$sqft_lot15, probs=c(.25, .75), na.rm = FALSE)
iqr3 <- IQR(housing$sqft_lot15)
up33 <-  Q3[2]+1.5*iqr3 # Upper Range  
low3<- Q3[1]-1.5*iqr3 # Lower Range
#remove outliers
eliminated <- subset(housing, housing$price > (Q1[1] - 1.5*iqr1) & housing$price < (Q1[2]+1.5*iqr1))
eliminated <- subset(eliminated, eliminated$sqft_lot > (Q2[1] - 1.5*iqr2) & eliminated$sqft_lot < (Q2[2]+1.5*iqr2))
eliminated <- subset(eliminated, eliminated$sqft_lot15 > (Q3[1] - 1.5*iqr3) & eliminated$sqft_lot15 < (Q3[2]+1.5*iqr3))
#check new distribution
ggbetweenstats(eliminated, year, price, outlier.tagging = TRUE) 
ggbetweenstats(eliminated, year, sqft_lot, outlier.tagging = TRUE) 
ggbetweenstats(eliminated, year, sqft_lot15, outlier.tagging = TRUE) 
housing2 <- eliminated %>%
  select(price, sqft_living, sqft_lot, sqft_above, sqft_basement, sqft_living15, sqft_lot15)
boxplot(housing2)$out
#remove unimportant variables
cleaned_data <- eliminated %>%
  select(-id, -day, -yr_renovated, -zipcode, -lat, -long)
#save as housing
housing <-cleaned_data
#remove the variable that is not helpful
housing %>% 
  select(-sqft_basement)
#run mlr to identify potentially insignificant variables
reg <- lm(price~yr_dummy+bedrooms+bathrooms+sqft_living+sqft_lot+floors+
            waterfront+view+condition+grade+sqft_above+
            yr_built+sqft_living15+sqft_lot15, data = housing)
summary(reg)
#check correlation of month, yr_dummy, and price
m <- housing %>% select(month)
y <- housing %>% select(yr_dummy)
p <- housing %>% select(price)
cor(m, y)
cor(p,m)
cor(p,y)
#check linear Hypothesis between month and yr_dummy
library(car)
linearHypothesis(reg, c("month","yr_dummy"))

