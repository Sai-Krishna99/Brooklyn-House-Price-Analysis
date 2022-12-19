# Loading all the packages useful for this analysis
require(ggplot2)
require(dplyr)
require(readr)
require(MASS)
require(stats)
require(leaps)
require(tidyverse)
require(lmtest)
require(corrplot)

# Eliminate this step if the working directory is already set
setwd("C:/Users/saikr/Downloads")

## 1.1 Reading the data into R ##
#reading datasets individually and skipping the first few rows accordingly
data.2016 <- read.csv("2016_brooklyn.csv", skip = 4)
data.2017 <- read.csv("2017_brooklyn.csv", skip = 4)
data.2018 <- read.csv("2018_brooklyn.csv", skip = 4)
data.2019 <- read.csv("2019_brooklyn.csv", skip = 4)
data.2020 <- read.csv("2020_brooklyn.csv", skip = 6)
# data.2020 <- data.2020[c(2:19900),] #removed the first row and also the extra NULL rows picked up by r 
data.2020 <- data.2020[-1,]
#modifying the column names as per the instructions
column.names <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot',
                  'easement','bldclasscurr','address','aptnum','zip','resunits','comunits',
                  'totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                  'bldclasssale','price','date')
colnames(data.2016) <- column.names
colnames(data.2017) <- column.names
colnames(data.2018) <- column.names
colnames(data.2019) <- column.names
colnames(data.2020) <- column.names
summary(data.2017)
#need a lot of data type conversion and cleaning before modeling

####################################################################################################################

## 1.2 Data cleaning and joining ##

# Started the cleaning from the last column to first column
#size of data before cleaning - 
nrow(data.2016)+nrow(data.2017)+nrow(data.2018)+nrow(data.2019)+nrow(data.2020)

# Converting the data type of date from character to Date
data.2016$date <- as.Date(data.2016$date, "%m/%d/%Y")
data.2017$date <- as.Date(data.2017$date, "%m/%d/%y")
data.2018$date <- as.Date(data.2018$date, "%m/%d/%y")
data.2019$date <- as.Date(data.2019$date, "%m/%d/%y")
data.2020$date <- as.Date(data.2020$date, "%m/%d/%y")

## Cleaning the price column to remove any "$","-" or "," signs
# removing those and converting the column from character to numeric values
str(data.2020$price)
data.2016$price <- gsub(",|[$]|-","",data.2016$price)
data.2016$price <- as.numeric(data.2016$price)
data.2017$price <- gsub(",|[$]|-","",data.2017$price)
data.2017$price <- as.numeric(data.2017$price)
data.2018$price <- gsub(",|[$]|-","",data.2018$price)
data.2018$price <- as.numeric(data.2018$price)
data.2019$price <- gsub(",|[$]|-","",data.2019$price)
data.2019$price <- as.numeric(data.2019$price)
data.2020$price <- gsub(",|[$]|-","",data.2020$price)
data.2020$price <- as.numeric(data.2020$price)

## Converting the bldclasssale from character to factor
#removing spaces from the column
unique(data.2018$bldclasssale)
#need to remove leading and trailing spaces in 2018 dataset, other datasets looks fine
data.2018$bldclasssale <- trimws(data.2018$bldclasssale, which="both")

## Convert the taxclasssale from integer to factor in the combined dataset
unique(data.2019$taxclasssale)
data.2019[is.na(data.2019$taxclasssale),]

## Convert the yrbuilt column from integer to factor in the combined dataset
str(data.2016$yrbuilt)
#observed around ~2000 to ~3000 zeros or NAs in each dataset for yrbuilt
#since yrbuilt is a probable predictor, its better to not let any NA's or Zeros affect this predictor

## Cleaning the grosssft column to remove any "," signs or spaces
# removing those and converting the column from character to numeric values
unique(data.2016$grosssqft)
data.2016$grosssqft <- gsub(",| ","",data.2016$grosssqft)
data.2016$grosssqft <- as.numeric(data.2016$grosssqft)
data.2017$grosssqft <- gsub(",| ","",data.2017$grosssqft)
data.2017$grosssqft <- as.numeric(data.2017$grosssqft)
data.2018$grosssqft <- gsub(",| ","",data.2018$grosssqft)
data.2018$grosssqft <- as.numeric(data.2018$grosssqft)
data.2019$grosssqft <- gsub(",| ","",data.2019$grosssqft)
data.2019$grosssqft <- as.numeric(data.2019$grosssqft)
data.2020$grosssqft <- gsub(",| ","",data.2020$grosssqft)
data.2020$grosssqft <- as.numeric(data.2020$grosssqft)

## Cleaning the landsft column to remove any "," signs or spaces
# removing those and converting the column from character to numeric values
unique(data.2016$landsqft)
data.2016$landsqft <- gsub(",| ","",data.2016$landsqft)
data.2016$landsqft <- as.numeric(data.2016$landsqft)
data.2017$landsqft <- gsub(",| ","",data.2017$landsqft)
data.2017$landsqft <- as.numeric(data.2017$landsqft)
data.2018$landsqft <- gsub(",| ","",data.2018$landsqft)
data.2018$landsqft <- as.numeric(data.2018$landsqft)
data.2019$landsqft <- gsub(",| ","",data.2019$landsqft)
data.2019$landsqft <- as.numeric(data.2019$landsqft)
data.2020$landsqft <- gsub(",| ","",data.2020$landsqft)
data.2020$landsqft <- as.numeric(data.2020$landsqft)

## Cleaning the totunits column to remove spaces in 2016 and 2018 datasets
# converting the column from character to numeric in 2016 and 2018, int to numeric in 2017,2019 and 2020
unique(data.2018$totunits)
data.2016$totunits <- gsub(" | -   ","",data.2016$totunits)
data.2016$totunits <- as.numeric(data.2016$totunits)
data.2018$totunits <- gsub(" ","",data.2018$totunits)
data.2018$totunits <- as.numeric(data.2018$totunits)
data.2017$totunits <- as.numeric(data.2017$totunits)
data.2019$totunits <- as.numeric(data.2019$totunits)
data.2020$totunits <- as.numeric(data.2020$totunits)

## Cleaning comunits column to remove any spaces or "-" signs
# then converting the column from character to numeric in 2016, integer to numeric in 2017,2018,2019,2020
str(data.2018$comunits)
data.2016$comunits <- gsub(" -   | ","",data.2016$comunits)
data.2016$comunits <- as.numeric(data.2016$comunits)
data.2017$comunits <- as.numeric(data.2017$comunits)
data.2018$comunits <- as.numeric(data.2018$comunits)
data.2019$comunits <- as.numeric(data.2019$comunits)
data.2020$comunits <- as.numeric(data.2020$comunits)

## Cleaning resunits column to remove any spaces and converting the column from character to numeric in 2016&2018, 
# integer to numeric in 2017,2019,2020
unique(data.2019$resunits)
data.2016$resunits <- gsub(" | -   ","",data.2016$resunits)
data.2016$resunits <- as.numeric(data.2016$resunits)
data.2017$resunits <- as.numeric(data.2017$resunits)
data.2018$resunits <- as.numeric(data.2018$resunits)
data.2019$resunits <- as.numeric(data.2019$resunits)
data.2020$resunits <- as.numeric(data.2020$resunits)

## Zip column is consistent with data types across all the datasets and doesnt need any cleaning
unique(data.2020$zip)
# I think it is better to use zip as predictor, remove zeros and converting them to NA's in the combined dataset

## aptnum column is also consistent across all datasets and it will not be used as predictor,
unique(data.2016$aptnum)

## address can be left as is and it will not be used as predictor
unique(data.2016$address)

## Converting the bldclasscurr from character to factor and identify the nulls
#clearing out the spaces
unique(data.2017$bldclasscurr)
data.2016$bldclasscurr <- gsub("  ","",data.2016$bldclasscurr)
data.2017$bldclasscurr <- gsub(" ","",data.2017$bldclasscurr)

## easement column can be dropped after joining the data
str(data.2016$easement)

## Convert the lot from integer to factor in the combined dataset 
str(data.2016$lot)

## Convert the block from integer to factor in the combined dataset
unique(data.2017$block) 

## Converting the taxclasscurr from character to factor in the final dataset
unique(data.2017$taxclasscurr)
data.2016$taxclasscurr <- gsub("  ","",data.2016$taxclasscurr)
data.2017$taxclasscurr <- gsub(" ","",data.2017$taxclasscurr)

## Converting the bldclasscat from character to factor
unique(data.2016$bldclasscat)
#spaces are different in 2016, making it consistent and removing leading and trailing spaces in all
data.2016$bldclasscat <- gsub("  "," ",data.2016$bldclasscat)
data.2016$bldclasscat <- trimws(data.2016$bldclasscat, which="both")
data.2017$bldclasscat <- trimws(data.2017$bldclasscat, which="both")
data.2018$bldclasscat <- trimws(data.2018$bldclasscat, which="both")
data.2019$bldclasscat <- trimws(data.2019$bldclasscat, which="both")
data.2020$bldclasscat <- trimws(data.2020$bldclasscat, which="both")

## Convert the neighborhood from character to factor in the final dataset
str(data.2016$neighborhood)

## borough is same across all datasets since we are looking at just one area, we can remove it once joined
## Same goes for easement as it has no data, we can remove easement column
data.2016 <- data.2016[,c(-1,-7)]
data.2017 <- data.2017[,c(-1,-7)]
data.2018 <- data.2018[,c(-1,-7)]
data.2019 <- data.2019[,c(-1,-7)]
data.2020 <- data.2020[,c(-1,-7)]


# Joining the cleaned data
final.data <- rbind(data.2016,data.2017,data.2018,data.2019,data.2020)
dim(final.data)
# Met the requirement of 119k rows, got 119.3k rows


#################################################################################################################
## 1.3 Filtering the data

my_df <- final.data

# Data type handling
my_df$neighborhood <- as.factor(my_df$neighborhood)
my_df$bldclasscat <- as.character(my_df$bldclasscat)
my_df$taxclasscurr <- as.character(my_df$taxclasscurr)
my_df$block <- as.factor(my_df$block)
my_df$lot <- as.factor(my_df$lot)
my_df$bldclasscurr <- as.character(my_df$bldclasscurr)
my_df$zip <- as.factor(my_df$zip)
my_df$landsqft <- as.numeric(my_df$landsqft)
my_df$grosssqft <- as.numeric(my_df$grosssqft)
my_df$yrbuilt <- as.numeric(my_df$yrbuilt)
# my_df$time_period <- as.factor(my_df$time_period)
my_df$taxclasssale <- as.character(my_df$taxclasssale)
my_df$bldclasssale <- as.character(my_df$bldclasssale)
my_df$price <- as.numeric(my_df$price)
my_df$date <- as.Date(my_df$date)
str(my_df)

# left the bldclass and taxclass columns in character for now, so that filtering can be easy. 
# will convert them to factor after filtering the data

# dropping columns adress, aptnum for now
my_df <- subset(my_df, select = -c(address,aptnum))

# creating a new dataframe just to have a recovery point
# filtering data to contain only building classes staring with A or R at the time of sale
new.df <- my_df
new.df <- new.df[substr(new.df$bldclasssale,1,1) == "A" | substr(new.df$bldclasssale,1,1)=="R",]
dim(new.df)
new.df<-subset(new.df, subset = bldclasscat!='11 SPECIAL CONDO BILLING LOTS')
unique(new.df$bldclasscat)
levels(as.factor(new.df$bldclasssale))

# filtering to have both total and residential units to be 1, gross sqft >0 and price != 0
new.df <- new.df[new.df$totunits == 1 & new.df$resunits == 1,]
new.df <- new.df[new.df$grosssqft>0,]
new.df <- new.df[!is.na(new.df$price),]
# removing the zero valued observations from price column
new.df <- new.df[new.df$price!=0,]
# filtering price further, to not let outliers interfere with the model
new.df <- new.df[new.df$price > 5000 & new.df$price < 5000000, ]
dim(new.df)
# Met the requirement for the overall filtered data dimensions, needed >13k rows, have ~13.5k rows

#converting the character columns to factor
new.df$bldclasssale <- as.factor(new.df$bldclasssale)
new.df$bldclasscat <- as.factor(new.df$bldclasscat)
new.df$bldclasscurr <- as.factor(new.df$bldclasscurr)
new.df$taxclasscurr <- as.factor(new.df$taxclasscurr)
new.df$taxclasssale <- as.factor(new.df$taxclasssale)
str(new.df)

# removing the columns resunits, comunits, totunits as the filtering has already been done for them
new.df <- subset(new.df, select = -c(resunits, comunits, totunits))
str(new.df)
######################################################################################################################

## 2.1 EDA and Feature Engineering

# Adding a new column time_period based on yrbuilt (part of 2.1)
# possible recommendations - yrbuilt have zeros in it, so for all zeros it will take as 19th century home which is imperfect!
# missing values might be replaced with median or mode in this case, but doing that doesnt account for uncertainity
# filling it with mode for now in the new column
new.df$time_period <- new.df$yrbuilt
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
new.df$time_period[is.na(new.df$time_period)|new.df$time_period==0] <- 
  find_mode(new.df$time_period[new.df$time_period!=0])

new.df$time_period[new.df$time_period<=1900] <- "19th Century"
new.df$time_period[as.numeric(new.df$time_period)>1900 & as.numeric(new.df$time_period)<=1940] <- "War and Depression"
new.df$time_period[as.numeric(new.df$time_period)>1940] <- "WW2 and post war"
new.df$time_period[as.numeric(new.df$time_period)>=2000] <- "21st Century"
unique(new.df$time_period)

## Checking the correlation of each numeric variable with other
str(new.df)
corrplot(cor(new.df[,c(8:10,13)]), title = "Correlation between the numerical predictors", 
         method = "circle", 
         order = "hclust", 
         hclust.method = "ward.D",
         addrect = 3,
         rect.col = 3,
         rect.lwd = 3)

## Checking the distribution of price, landsqft, grosssqft
ggplot(new.df, aes(price))+
  geom_histogram(bins = 50, alpha=0.7, color="white", fill= "black")+
  labs(title = "Histogram of Price",
       x = "Price in $",
       y = "Frequency")
ggplot(new.df, aes(landsqft))+
  geom_histogram(bins = 50, alpha=0.7, color="white", fill = "black")+
  labs(title = "Histogram of Land Sqft",
       x = "SQFT",
       y = "Frequency")+
  scale_x_continuous(limits = c(100,10000), breaks = seq(100,10000,1000))
ggplot(new.df, aes(grosssqft))+
  geom_histogram(bins = 50, alpha=0.7, color="white", fill = "black")+
  labs(title = "Histogram of Gross Sqft",
       x = "SQFT",
       y = "Frequency")+
  scale_x_continuous(limits = c(100,6000), breaks = seq(100,6000,500))
# Checking if the year built is having any significant impact on sale prices
ggplot(new.df, aes(x=time_period, y=price))+geom_boxplot()
ggplot(new.df, aes(x=yrbuilt, y=price))+geom_point()

# sale date can be further limited to quarter of the year
require(zoo)
new.df$quarter <- as.yearqtr(new.df$date, format = "%Y-%m-%d")
new.df$quarter <- as.factor(new.df$quarter)

# To better control the model degrees of freedom:
# zip can be bucketed further based on group median prices
plot(new.df$zip, new.df$price)
zip_means <- aggregate(new.df$price, list(new.df$zip), FUN=mean) #with mean
colnames(zip_means) <- c("zip","mean_price")
plot(zip_means$zip, (zip_means$mean_price), type = 'l') #with mean
ggplot(zip_means, aes(x = zip, y = mean_price))+
  geom_point()

zip_med <- aggregate(new.df$price, list(new.df$zip), FUN=median) #with median
colnames(zip_med) <- c("zip","median_price")
plot(zip_med$zip, (zip_med$median_price), type = 'l') #with median
ggplot(zip_med, aes(x = zip, y = sort(median_price)))+
  geom_point()

# ranking zips based on median prices
zip.rk1 <- c('11202','11231')
zip.rk2 <- c('11215')
zip.rk3 <- c('11238')
zip.rk4 <- c('11225')
zip.rk5 <- c('11218','11222')
zip.rk6 <- c('11209','11211')
zip.rk7 <- c('11230','11241')
zip.rk8 <- c('11205','11217','11228')
zip.rk9 <- c('11204','11220','11223')
zip.rk10 <- c('11210','11213','11219','11221')
zip.rk11 <- c('11206','11235','11237')
zip.rk12 <- c('11229','11233')
zip.rk13 <- c('11234')
zip.rk14 <- c('11203','11224')
zip.rk15 <- c('11236')
zip.rk16 <- c('11207','11208','11212','11239')

zip.rk17 <- c('11232')
zip.rk18 <- c('11216')
zip.rk19 <- c('11214','11226')

# creating a new variable zip.rk to further categorize zips based on the mean price for each zip
new.df$zip.rk <- "zip1"
#rank 2
new.df$zip.rk[new.df$zip=='11215'] <- "zip2"
#rank 3
new.df$zip.rk[new.df$zip=='11238'] <- "zip3"
#rank 4
new.df$zip.rk[new.df$zip=='11225'] <- "zip4"
#rank 5
new.df$zip.rk[new.df$zip=='11218'|new.df$zip=='11222'] <- "zip5"
#rank 6
new.df$zip.rk[new.df$zip=="11209"|new.df$zip=="11211"] <- "zip6"
#rank 7
new.df$zip.rk[new.df$zip=='11230'|new.df$zip=='11241'] <- "zip7"
#rank 8
new.df$zip.rk[new.df$zip=='11205'|new.df$zip=='11217'|new.df$zip=='11228'] <- "zip8"
#rank 9
new.df$zip.rk[new.df$zip=='11204'|new.df$zip=='11220'|new.df$zip=='11223'] <- "zip9"
#rank 10
new.df$zip.rk[new.df$zip=='11210'|new.df$zip=='11213'|new.df$zip=='11219'|new.df$zip=='11221'] <- "zip10"
#rank 11
new.df$zip.rk[new.df$zip=='11206'|new.df$zip=='11235'|new.df$zip=='11237'] <- "zip11"
#rank 12
new.df$zip.rk[new.df$zip=='11229'|new.df$zip=='11233'] <- "zip12"
#rank 13
new.df$zip.rk[new.df$zip=='11234'] <- "zip13"
#rank 14
new.df$zip.rk[new.df$zip=='11203'|new.df$zip=='11224'] <- "zip14"
#rank 15
new.df$zip.rk[new.df$zip=='11236'] <- "zip15"
#rank 16
new.df$zip.rk[new.df$zip=='11207'|new.df$zip=='11208'|new.df$zip=='11212'|new.df$zip=='11239'] <- "zip16"

#rank 17
new.df$zip.rk[new.df$zip=='11232'] <- "zip17"
#rank 18
new.df$zip.rk[new.df$zip=='11216'] <- "zip18"
#rank 19
new.df$zip.rk[new.df$zip=='11214'|new.df$zip=='11226'] <- "zip19"


new.df$zip.rk <- as.factor(new.df$zip.rk)
unique(new.df$zip.rk)

#test zip_rk columns
summary(lm(price~zip.rk,data = new.df))

## Controlling the bldclasscat to have only 2 levels
unique(new.df$bldclasscat)
new.df$bldclasscat <- as.character(new.df$bldclasscat)
# tax 1 class condos to one family dwellings
new.df$bldclasscat[which(new.df$bldclasscat == "04 TAX CLASS 1 CONDOS")] <- "01 ONE FAMILY DWELLINGS"
# condos 2-10 residential to condos -elevator aprtments
new.df$bldclasscat[which(new.df$bldclasscat == "15 CONDOS - 2-10 UNIT RESIDENTIAL")] <- "13 CONDOS - ELEVATOR APARTMENTS"
# condos walkup apartments to one family dwellings
new.df$bldclasscat[which(new.df$bldclasscat == "12 CONDOS - WALKUP APARTMENTS")] <- "01 ONE FAMILY DWELLINGS"
new.df$bldclasscat <- as.factor(new.df$bldclasscat)

##################################################################################################################

## Building the Model and Validating : Model Selection

# building basline model
# let the base model not have the time_period, neighborhood, block and lot too
new.df$yrbuilt <- as.numeric(new.df$yrbuilt)
base.model.df <- subset(new.df, select=-c(neighborhood,block,lot,time_period,quarter,zip.rk))
base.lm <- lm(price~.,data = base.model.df)
summary(base.lm)
# Model DF - 63, adjusted r squared - 0.5913, and RMSE of 428039, good start.
summary(base.lm)$adj.r.squared
(base.rmse <- sqrt(mean((new.df$price - base.lm$fitted.values)^2)))

# new model with time_period instead of year
colnames(new.df)
summary(lm(sqrt(price)~taxclasscurr+zip+sqrt(landsqft)+taxclasssale*sqrt(grosssqft)+date+time_period, data = new.df))
# time period did not help much in improving the performance which was already expected from EDA

lm1 <- lm(price~taxclasscurr+zip+sqrt(landsqft)+date+time_period*sqrt(grosssqft), data = new.df)
summary(lm1)
sqrt(mean((new.df$price - lm1$fitted.values)^2))
#adj r squared - 0.5903, df - 48, RMSE - 428848

lm2 <- lm(price~taxclasscurr*sqrt(grosssqft)+zip+sqrt(landsqft)+date+yrbuilt, data = new.df)
summary(lm2)
sqrt(mean((new.df$price - lm2$fitted.values)^2))
#adj r squared - 0.5968, df - 49, RMSE - 425388

lm3 <- lm(price~taxclasscurr+zip.rk*sqrt(grosssqft)+sqrt(landsqft)*sqrt(grosssqft)+date+yrbuilt, data = new.df)
summary(lm3)
sqrt(mean((new.df$price - lm3$fitted.values)^2))
#adj r squared 0.6291, df - 45, RMSE - 408058

lm4 <- lm(price~ taxclasssale+yrbuilt+sqrt(landsqft)+grosssqft*zip.rk+date, data = new.df)
summary(lm4)
sqrt(mean((new.df$price - lm4$fitted.values)^2))
#adj r squared 0.6152, df - 41, RMSE - 415682 - pretty good but still higher DF

lm5 <- lm(price~bldclasscat+date+zip.rk*sqrt(grosssqft)+landsqft , new.df)
summary(lm5)
sqrt(mean((new.df$price - lm5$fitted.values)^2))
#adj r squared <- 0.6199, df - 40, RMSE - 413159 - best model satisfying all the conditions

best.model <- lm5

## Checking the fit for OLS Assumptions
plot(best.model$fitted.values, best.model$residuals) # shows heteroskedasticity

# Normality Check
ggplot( mapping=  aes(best.model$residuals))+
  geom_histogram(aes(y = stat(density)),alpha=0.7, color="white", fill = "black")+
  geom_density(col = 'red',lwd=0.7)+
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")+
  geom_vline(xintercept = 0, color='black')
#plot looks very close to normal distribution, checking with ks test to confirm
ks.test(best.model$residuals/summary(best.model)$sigma, 'pnorm')
# but the test shows that the residuals are not normally distributed 
bptest(best.model)
# heteroskedasticity is present
dwtest(best.model)
# serial correlation is present
acf(best.model$residuals)

# Part 1 results:
#Data Coverage - expected: >=13k rows, result: 13482 rows
dim(new.df)
#Explanatory power - expected: >=0.6, result: 0.6199
summary(best.model)$adj.r.squared
#Predictive power - expected: <=$450,000 , result: $413,151
sqrt(mean((new.df$price - best.model$fitted.values)^2))
#Model parsimony - expected: <= 40 Model DF, result: 40 DF
summary(best.model)

saveRDS(list(model=best.model,data=new.df),file='saikrishna.RDS')
