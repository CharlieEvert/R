#import the dataset
install.packages("tidyverse")
library(readxl)
z <- read_excel("Project Data Set.xlsx")
View(z)
attach(z)
z

#set average lat and long to replace all NAs (to use for visualizing later)
avglat <- 40.74324868 #this value was taken from the XL doc since it would not compute bcs of NAs (which we are replacing)
avglong <- -73.90827755 #same as above
z$`LATITUDE`[is.na(z$`LATITUDE`)] <- avglat
z$`LONGITUDE`[is.na(z$`LONGITUDE`)] <- avglong

#visualize the distributions of Lat and Long
par(mfrow=c(1,2))
hist(LATITUDE, col="green") #there seems to be a more consistent spread from West to East NYC
hist(LONGITUDE, col="blue") #There is a more rightward skew and a more pronounce peak around the average

#replace NAs with values that work
z$`ZIP CODE`[is.na(z$`ZIP CODE`)] <- 0
z$BOROUGH[is.na(z$BOROUGH)] <- 'UNSPECIFIED'
z$'ON STREET NAME'[is.na(z$'ON STREET NAME')] <- 'UNSPECIFIED'
z$'CROSS STREET NAME'[is.na(z$'CROSS STREET NAME')] <- 'UNSPECIFIED'
z$'OFF STREET NAME'[is.na(z$'OFF STREET NAME')] <- 'UNSPECIFIED'

#These function makes cleaning NAs a lot easier...
#input the table (z) and the column to clean text based NAs
clean_na <- function(z,x) {
  z$x[is.na(z$x)] <- 'UNSPECIFIED'
}

#Convert all columns to propercase so there is no inconsistency
#install.packages("stringr")
library("stringr")
z$`CROSS STREET NAME` <- str_to_title(z$`CROSS STREET NAME`)
z$`ON STREET NAME` <- str_to_title(z$`ON STREET NAME`)
z$`BOROUGH` <- str_to_title(z$`BOROUGH`)
z$`OFF STREET NAME` <- str_to_title(z$`OFF STREET NAME`)
z$`CONTRIBUTING FACTOR VEHICLE 1` <- str_to_title(z$`CONTRIBUTING FACTOR VEHICLE 1`)
z$`CONTRIBUTING FACTOR VEHICLE 2` <- str_to_title(z$`CONTRIBUTING FACTOR VEHICLE 2`)
z$`CONTRIBUTING FACTOR VEHICLE 3` <- str_to_title(z$`CONTRIBUTING FACTOR VEHICLE 3`)
z$`CONTRIBUTING FACTOR VEHICLE 4` <- str_to_title(z$`CONTRIBUTING FACTOR VEHICLE 4`)
z$`CONTRIBUTING FACTOR VEHICLE 5` <- str_to_title(z$`CONTRIBUTING FACTOR VEHICLE 5`)
z$`VEHICLE TYPE CODE 1` <- str_to_title(z$`VEHICLE TYPE CODE 1`)
z$`VEHICLE TYPE CODE 2` <- str_to_title(z$`VEHICLE TYPE CODE 2`)
z$`VEHICLE TYPE CODE 3` <- str_to_title(z$`VEHICLE TYPE CODE 3`)
z$`VEHICLE TYPE CODE 4` <- str_to_title(z$`VEHICLE TYPE CODE 4`)
z$`VEHICLE TYPE CODE 5` <- str_to_title(z$`VEHICLE TYPE CODE 5`)

#to create seasonal variables to measure winter/summer accidents
library(lubridate)
z$'MONTH' <- month(as.POSIXlt(z$`CRASH DATE`, format="%Y-%m-%d"))
z$SUMMER <- 0
z$SUMMER[z$MONTH==6] <- 1
z$SUMMER[z$MONTH==7] <- 1
z$SUMMER[z$MONTH==8] <- 1
z$WINTER <- 0
z$WINTER[z$MONTH==12] <- 1
z$WINTER[z$MONTH==1] <- 1
z$WINTER[z$MONTH==2] <- 1

#See which season has more accidents (happens to be Winter) and the difference
#between them (28) in addition to the % difference (23%)
Winter <- table(z$WINTER)
Winter
Summer <- table(z$SUMMER)
Summer
Difference <- Winter - Summer
Difference
PercentageChange <- Difference/Winter
PercentageChange

#pie charts to demonstrate that winter accidents make up a higher % of total accidents than summer
pie(Winter, main="Winter Accidents over Total", col=rainbow(2))
pie(Summer, main="Summer Accidents over Total", col=rainbow(5))

#visualize the top 5 most crashed vehicle types
library(dplyr)
library(ggplot2)

#set up vehicle type to merge df for a full count by type, and rename the type to be the same
#also rename the frequency to make merging easier
a = data.frame(table(`VEHICLE TYPE CODE 1`))
a = rename(a, Type = VEHICLE.TYPE.CODE.1)
a = rename(a, Freq1 = Freq)
b = data.frame(table(`VEHICLE TYPE CODE 2`)) 
b = rename(b, Type = VEHICLE.TYPE.CODE.2)
b = rename(b, Freq2 = Freq)
c = data.frame(table(`VEHICLE TYPE CODE 3`))
c = rename(c, Type = VEHICLE.TYPE.CODE.3)
c = rename(c, Freq3 = Freq)
d = data.frame(table(`VEHICLE TYPE CODE 4`))
d = rename(d, Type = VEHICLE.TYPE.CODE.4)
d = rename(d, Freq4 = Freq)
e = data.frame(table(`VEHICLE TYPE CODE 5`))
e = rename(e, Type = VEHICLE.TYPE.CODE.5)
e = rename(e, Freq5 = Freq)

#merging all columns
VehicleType = merge(a,b,by="Type")
VehicleType = merge(VehicleType,c,by="Type")
VehicleType = merge(VehicleType,d,by="Type")
VehicleType = merge(VehicleType,e,by="Type")

#creating sum of all frequencies
VehicleType$RealFreq =  VehicleType$Freq1 + 
                        VehicleType$Freq2 + 
                        VehicleType$Freq3 +
                        VehicleType$Freq4 + 
                        VehicleType$Freq5

#Drop columns we don't need
drops <- c("Freq1","Freq2","Freq3","Freq4","Freq5")
VehicleType <- VehicleType[ , !(names(VehicleType) %in% drops)]

#Convert RealFreq to numeric
VehicleType$RealFreq <- as.integer(VehicleType$RealFreq) 
typeof(VehicleType$RealFreq)
attach(VehicleType)

#Convert type to propercase
VehicleType$Type <- str_to_title(VehicleType$Type)

#Show the top 3 most frequent cars in 5 car accidents
v <- VehicleType %>% top_n(3)
v

#Look at the top 3 most accident-prone vehicles in barchart
p<-ggplot(v, aes(x=reorder(Type, -RealFreq), y=RealFreq)) +
  geom_bar(stat="identity", fill = rainbow(3)) +
  ggtitle("Which Vehicle Types Have the Most 5-Car Pileups", "A Look at NYC's Traffic Accidents") +
  labs(y= "Number of Accidents", x = "Type of Vehicle") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) 
p

#Creating Model to predict injuries based on attributes we have looked at
summary(lm(`NUMBER OF PERSONS INJURED` ~ z$SUMMER + 
             z$WINTER +
             z$LATITUDE +
             z$LONGITUDE
))

#Second iteration- still insignificant
summary(lm(`NUMBER OF PERSONS INJURED` ~ z$SUMMER + 
             z$WINTER
))

#Add in dummy coded columns for contr factor of driver 1
install.packages('fastDummies')
library('fastDummies')
z <- dummy_cols(z, select_columns = 'CONTRIBUTING FACTOR VEHICLE 1')
attach(z)

#Predictive model based on contributing factors of car 1; most are significant
summary(lm(`NUMBER OF PERSONS INJURED` ~ 
             `CONTRIBUTING FACTOR VEHICLE 1_Aggressive Driving/Road Rage` +
             `CONTRIBUTING FACTOR VEHICLE 1_Alcohol Involvement` +
             `CONTRIBUTING FACTOR VEHICLE 1_Animals Action` +
             `CONTRIBUTING FACTOR VEHICLE 1_Backing Unsafely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Brakes Defective` +
             `CONTRIBUTING FACTOR VEHICLE 1_Cell Phone (Hand-Held)` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driver Inattention/Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driver Inexperience` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driverless/Runaway Vehicle` +
             `CONTRIBUTING FACTOR VEHICLE 1_Drugs (Illegal)` +
             `CONTRIBUTING FACTOR VEHICLE 1_Failure To Keep Right` +
             `CONTRIBUTING FACTOR VEHICLE 1_Failure To Yield Right-Of-Way` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fatigued/Drowsy` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fell Asleep` +
             `CONTRIBUTING FACTOR VEHICLE 1_Following Too Closely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Glare` +
             `CONTRIBUTING FACTOR VEHICLE 1_Lost Consciousness` +
             `CONTRIBUTING FACTOR VEHICLE 1_Obstruction/Debris` +
             `CONTRIBUTING FACTOR VEHICLE 1_Other Electronic Device` +
             `CONTRIBUTING FACTOR VEHICLE 1_Other Vehicular` +
             `CONTRIBUTING FACTOR VEHICLE 1_Outside Car Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Passing Or Lane Usage Improper` +
             `CONTRIBUTING FACTOR VEHICLE 1_Passing Too Closely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pavement Defective` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pavement Slippery` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pedestrian/Bicyclist/Other Pedestrian Error/Confusion` +
             `CONTRIBUTING FACTOR VEHICLE 1_Prescription Medication` +
             `CONTRIBUTING FACTOR VEHICLE 1_Reaction To Uninvolved Vehicle` +
             `CONTRIBUTING FACTOR VEHICLE 1_Steering Failure` +
             `CONTRIBUTING FACTOR VEHICLE 1_Tire Failure/Inadequate` +
             `CONTRIBUTING FACTOR VEHICLE 1_Traffic Control Device Improper/Non-Working` +
             `CONTRIBUTING FACTOR VEHICLE 1_Traffic Control Disregarded` +
             `CONTRIBUTING FACTOR VEHICLE 1_Turning Improperly` +
             `CONTRIBUTING FACTOR VEHICLE 1_Unsafe Lane Changing` +
             `CONTRIBUTING FACTOR VEHICLE 1_Unsafe Speed` +
             `CONTRIBUTING FACTOR VEHICLE 1_View Obstructed/Limited`
))

#This is the predictive model after 2 iterations of removing insig factors
summary(lm(`NUMBER OF PERSONS INJURED` ~ 
             `CONTRIBUTING FACTOR VEHICLE 1_Driver Inattention/Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fatigued/Drowsy` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fell Asleep` +
             `CONTRIBUTING FACTOR VEHICLE 1_Following Too Closely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Other Vehicular` +
             `CONTRIBUTING FACTOR VEHICLE 1_Outside Car Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pavement Slippery` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pedestrian/Bicyclist/Other Pedestrian Error/Confusion` +
             `CONTRIBUTING FACTOR VEHICLE 1_Steering Failure` +
             `CONTRIBUTING FACTOR VEHICLE 1_Traffic Control Disregarded` +
             `CONTRIBUTING FACTOR VEHICLE 1_Unsafe Speed` 
))

#shows plots of residuals- definitely a pretty weak model!
plot(lm(`NUMBER OF PERSONS INJURED` ~ 
             `CONTRIBUTING FACTOR VEHICLE 1_Aggressive Driving/Road Rage` +
             `CONTRIBUTING FACTOR VEHICLE 1_Alcohol Involvement` +
             `CONTRIBUTING FACTOR VEHICLE 1_Animals Action` +
             `CONTRIBUTING FACTOR VEHICLE 1_Backing Unsafely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Brakes Defective` +
             `CONTRIBUTING FACTOR VEHICLE 1_Cell Phone (Hand-Held)` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driver Inattention/Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driver Inexperience` +
             `CONTRIBUTING FACTOR VEHICLE 1_Driverless/Runaway Vehicle` +
             `CONTRIBUTING FACTOR VEHICLE 1_Drugs (Illegal)` +
             `CONTRIBUTING FACTOR VEHICLE 1_Failure To Keep Right` +
             `CONTRIBUTING FACTOR VEHICLE 1_Failure To Yield Right-Of-Way` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fatigued/Drowsy` +
             `CONTRIBUTING FACTOR VEHICLE 1_Fell Asleep` +
             `CONTRIBUTING FACTOR VEHICLE 1_Following Too Closely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Glare` +
             `CONTRIBUTING FACTOR VEHICLE 1_Lost Consciousness` +
             `CONTRIBUTING FACTOR VEHICLE 1_Obstruction/Debris` +
             `CONTRIBUTING FACTOR VEHICLE 1_Other Electronic Device` +
             `CONTRIBUTING FACTOR VEHICLE 1_Other Vehicular` +
             `CONTRIBUTING FACTOR VEHICLE 1_Outside Car Distraction` +
             `CONTRIBUTING FACTOR VEHICLE 1_Passing Or Lane Usage Improper` +
             `CONTRIBUTING FACTOR VEHICLE 1_Passing Too Closely` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pavement Defective` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pavement Slippery` +
             `CONTRIBUTING FACTOR VEHICLE 1_Pedestrian/Bicyclist/Other Pedestrian Error/Confusion` +
             `CONTRIBUTING FACTOR VEHICLE 1_Prescription Medication` +
             `CONTRIBUTING FACTOR VEHICLE 1_Reaction To Uninvolved Vehicle` +
             `CONTRIBUTING FACTOR VEHICLE 1_Steering Failure` +
             `CONTRIBUTING FACTOR VEHICLE 1_Tire Failure/Inadequate` +
             `CONTRIBUTING FACTOR VEHICLE 1_Traffic Control Device Improper/Non-Working` +
             `CONTRIBUTING FACTOR VEHICLE 1_Traffic Control Disregarded` +
             `CONTRIBUTING FACTOR VEHICLE 1_Turning Improperly` +
             `CONTRIBUTING FACTOR VEHICLE 1_Unsafe Lane Changing` +
             `CONTRIBUTING FACTOR VEHICLE 1_Unsafe Speed` +
             `CONTRIBUTING FACTOR VEHICLE 1_View Obstructed/Limited`
))