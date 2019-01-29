# Load essential libraries
library(tidyr)
library(stringr)

#Loading the data file to be modelled
car_price<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
#Structure of loan data file
str(car_price)
#View loan data file
View(car_price)

#                      /*******/

#EDA and DATA PREPARATION

#Checking for duplicate entries
car_price$id[duplicated(car_price$car_ID)]

#As we need to consider only company name as the independent variable 
#for the model building,removing the model name
car_price<-separate(car_price,CarName,into=c("CarName","Model"),sep=" ")

#Removing car id and model as it is not required
car_price<- subset(car_price,select=-c(car_ID,Model))

#checking for errors in Carname
levels(as.factor(car_price$CarName))

#mazda is mispelled as maxda,nissan as Nissan,porsche as porcshce
#toyota as toyouta, volkswagen as vw,vokswagen
#Correcting the spelling
car_price$CarName<-str_replace(car_price$CarName,"maxda","mazda")
car_price$CarName<-str_replace(car_price$CarName,"Nissan","nissan")
car_price$CarName<-str_replace(car_price$CarName,"porcshce","porsche")
car_price$CarName<-str_replace(car_price$CarName,"toyouta","toyota")
car_price$CarName<-str_replace(car_price$CarName,"vokswagen","volkswagen")
car_price$CarName<-str_replace(car_price$CarName,"vw","volkswagen")

levels(as.factor(car_price$CarName))




#character varaible, we will first convert them into factor variable for better analysis

chr_to_factor <-
  function(x) {
    for (i in 1:ncol(x)) {
      if (typeof(x[, i]) == "character") {
        x[, i] <- as.factor(x[, i])
      }
    } 
    return(x)
  }

car_price <- chr_to_factor(car_price)

str(car_price)

#Converting variable symboling to factors as it is categorical in nature

car_price$symboling<-as.factor(car_price$symboling)
str(car_price$symboling)

#Creating Derived Matrice: length to width ratio
car_price$lwratio=car_price$carlength/car_price$carwidth

# convert factors with 2 levels to numerical variables

#Converting Fuel type gas to 0 and diesel to 1
levels(car_price$fueltype)<-c(1,0)
car_price$fueltype<- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

#Converting aspiration std to 1 and turbo to 0
levels(car_price$aspiration)<-c(1,0)
car_price$aspiration<- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

#Converting doornumber four to 1 and two to 0
levels(car_price$doornumber)<-c(1,0)
car_price$doornumber<- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

#Converting enginelocation front to 1 and rear to 0
levels(car_price$enginelocation)<-c(1,0)
car_price$enginelocation<- as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]

# Create the dummy variable for factors with more than 2 levels
#long to wide format
# CarName
dummy_1 <- data.frame(model.matrix( ~CarName, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#carbody
dummy_1 <- data.frame(model.matrix( ~carbody, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#enginetype
dummy_1 <- data.frame(model.matrix( ~enginetype, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#fuelsystem
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#cylindernumber
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#drivewheel

dummy_1 <- data.frame(model.matrix( ~drivewheel, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)

#symboling
dummy_1 <- data.frame(model.matrix( ~symboling, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price <- cbind(car_price, dummy_1)


#removing the duplicated variables
car_price_final<-subset(car_price,select= -c(symboling,CarName,carbody,enginetype,fuelsystem,cylindernumber,drivewheel))

#                   /*********/


#MODELLING

#Loading Essential libraries
library(MASS)
library(car)
# separate training and testing data
set.seed(50)
sampledata= sample(1:nrow(car_price_final), 0.7*nrow(car_price_final))
train = car_price_final[sampledata,]
test = car_price_final[-sampledata,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#stepAIC
step <- stepAIC(model_1, direction="both")

#Variables treated as significant by stepAIC method
step

#Executing the models with significant variables
model_2<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + citympg + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour + drivewheelfwd + drivewheelrwd + symboling2 + 
              symboling3, data = train)

summary(model_2)

#Check for multicollinearity 
vif(model_2)

#Removing citympg as it have high VIF and pvalue
model_3<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour + drivewheelfwd + drivewheelrwd + symboling2 + 
              symboling3, data = train)

summary(model_3)
vif(model_3)

#Removing drivewheelrwd as it have high VIF and p value
model_4<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour + drivewheelfwd + symboling2 + 
              symboling3, data = train)

summary(model_4)
vif(model_4)

#Removing drivewheelfwd as it have high VIF and p value

model_5<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour + symboling2 + 
              symboling3, data = train)
summary(model_5)
vif(model_5)

#Removing symboling2 as it have high p value

model_6<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour + symboling3, data = train)

summary(model_6)
vif(model_6)

#Removing symboling3 as it has high p value

model_7<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamenissan + CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour, data = train)

summary(model_7)
vif(model_7)

#Removing CarNamenissan as it has high p value

model_8<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemazda + CarNamemitsubishi + 
              CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour, data = train)

summary(model_8)
vif(model_8)

#Removing carnamemazda having p value

model_9<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
              wheelbase + carheight + curbweight + enginesize + boreratio + 
              stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
              CarNamebuick + CarNamedodge + CarNamemitsubishi + 
              CarNamepeugeot + CarNameplymouth + CarNameporsche + 
              CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + enginetypel + enginetypeohc + 
              enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
              cylindernumberfour, data = train)

summary(model_9)
vif(model_9)

#Removing enginetypeohc having high p value and vif

model_10<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + CarNamesubaru + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypel +  
               enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
               cylindernumberfour, data = train)

summary(model_10)
vif(model_10)

#Removing CarNameSubaru having high p value 
model_11<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + enginetypel +  
               enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
               cylindernumberfour, data = train)

summary(model_11)
vif(model_11)

#Removing carbodywagon having high p value and VIF value

model_12<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + carbodyhardtop + carbodyhatchback + 
               carbodysedan + enginetypel +  
               enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
               cylindernumberfour, data = train)

summary(model_12)
vif(model_12)

#Removing carbodysedan having high p value 

model_13<-lm(formula = price ~ fueltype + aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + carbodyhardtop + carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
               cylindernumberfour, data = train)

summary(model_13)
vif(model_13)

#Removing fueltype having high p value 

model_14<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + carbodyhardtop + carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
               cylindernumberfour, data = train)

summary(model_14)
vif(model_14)

#Removing fuelsystemmpfi having high p value 

model_15<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab + carbodyhardtop + carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl +
               cylindernumberfour, data = train)

summary(model_15)
vif(model_15)

#Removing carbodyhardtop as having less significance 

model_16<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl +
               cylindernumberfour, data = train)

summary(model_16)
vif(model_16)


#Removing CarNamePeugeot as it has high VIF

model_17<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + carheight + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl +
               cylindernumberfour, data = train)

summary(model_17)
vif(model_17)



#Removing carheight as it has high p value

model_18<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + fuelsystem2bbl +
               cylindernumberfour, data = train)

summary(model_18)
vif(model_18)

#Removing fuelsystem2bbl as it has high p value

model_19<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfour, data = train)

summary(model_19)
vif(model_19)

#Removing cylindernumberfour as it has high p value

model_20<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypel + enginetypeohcv + enginetyperotor, data = train)

summary(model_20)
vif(model_20)

#Removing enginetypel as it has high p value

model_21<-lm(formula = price ~aspiration + enginelocation + 
               wheelbase + curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_21)
vif(model_21)

#Removing wheelbase as it has high p value

model_22<-lm(formula = price ~aspiration + enginelocation + 
                curbweight + enginesize + boreratio + 
               stroke + peakrpm + lwratio + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_22)
vif(model_22)

#Removing lwratio as it has high p value

model_23<-lm(formula = price ~aspiration + enginelocation + 
               curbweight + enginesize + boreratio + 
               stroke + peakrpm + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + 
               CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_23)
vif(model_23)

#Removing enginelocation as it has high p value

model_24<-lm(formula = price ~aspiration +  
               curbweight + enginesize + boreratio + 
               stroke + peakrpm + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamedodge + CarNamemitsubishi + CarNameplymouth + 
               CarNameporsche + CarNamesaab +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_24)
vif(model_24)

#Removing CarNamedodge as it has high p value

model_25<-lm(formula = price ~aspiration +  
               curbweight + enginesize + boreratio + 
               stroke + peakrpm + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
               CarNamesaab +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_25)
vif(model_25)

#Removing CarNamesaab as it has high p value

model_26<-lm(formula = price ~aspiration + curbweight + enginesize + boreratio + 
               stroke + peakrpm + CarNameaudi + CarNamebmw + 
               CarNamebuick + CarNamemitsubishi + CarNameplymouth + CarNameporsche + 
               +carbodyhatchback + enginetypeohcv + enginetyperotor, data = train)

summary(model_26)
vif(model_26)

#Removing CarNameplymouth as it has high p value

model_27<-lm(formula = price ~aspiration + curbweight + enginesize + boreratio + 
               stroke + peakrpm + CarNameaudi + CarNamebmw + CarNamebuick + 
               CarNamemitsubishi + CarNameporsche + carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_27)
vif(model_27)

#Removing CarNameaudi as it is less significant

model_28<-lm(formula = price ~aspiration +  
                enginesize + boreratio + stroke + peakrpm + curbweight + CarNamebmw + 
               CarNamebuick + CarNamemitsubishi + CarNameporsche +carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_28)
vif(model_28)

#Removing boreratio as it has high VIF value
model_29<-lm(formula = price ~aspiration +enginesize + curbweight + 
               stroke + peakrpm + CarNamebmw + CarNamebuick + CarNamemitsubishi + 
               + CarNameporsche + carbodyhatchback + 
               enginetypeohcv + enginetyperotor, data = train)

summary(model_29)
vif(model_29)

#Removing enginetypeohcv as it has high  p value
model_30<-lm(formula = price ~aspiration + enginesize+ curbweight +    
               stroke + peakrpm + CarNamebmw + CarNamebuick + CarNamemitsubishi + 
               CarNameporsche + carbodyhatchback + 
                enginetyperotor, data = train)

summary(model_30)
vif(model_30)

#Removing curbweight as it has high VIF value

model_31<-lm(formula = price ~aspiration + enginesize+   
               stroke + peakrpm + CarNamebmw + 
               CarNamebuick + CarNamemitsubishi  
               + CarNameporsche + carbodyhatchback + 
               enginetyperotor, data = train)

summary(model_31)
vif(model_31)

#Peakrpm coefficient seems very low compared to others,hence removing it 

model_32<-lm(formula = price ~aspiration + enginesize+  
               stroke + CarNamebmw + CarNamebuick + CarNamemitsubishi + 
               CarNameporsche + carbodyhatchback + 
               enginetyperotor, data = train)

summary(model_32)
vif(model_32) 

#CarNameMitsubishi seems less significant hence removing it
model_33<-lm(formula = price ~aspiration + enginesize+ 
               stroke + CarNamebmw + CarNamebuick +  
               CarNameporsche +carbodyhatchback + 
               enginetyperotor, data = train)

summary(model_33)
vif(model_33)

#Stroke seems less significant compared to others hence removing it
model_34<-lm(formula = price ~aspiration +enginesize + CarNamebmw + 
               CarNamebuick + CarNameporsche + carbodyhatchback + 
               enginetyperotor, data = train)

summary(model_34)
vif(model_34)
#                  /***********/

#TESTING on test data

Predict_1 <- predict(model_34,test)
test$test_price <- Predict_1

#we need to test the r square between actual and predicted sales. 

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#Adjusted R squared:0.8873 and tested R square: 0.8857

