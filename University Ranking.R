library(RSQLite)
library(RODBC)
odbcDataSources(type = c("all", "user", "system"))

#Create connection
db <- odbcConnect("Example", uid = "", pwd = "")
#Query a database (select statement)
UniversityRank <- sqlQuery(db, "SELECT * FROM WorldUniversityRanking.dbo.timesData")
sqlBasic <- sqlQuery(db, "SELECT 
              	      world_rank
                     ,university_name
                     ,country 
                     ,teaching
                     ,international 
                     ,research
                     ,citations 
                     ,income 
                     ,total_score 
                     ,num_students 
                     ,student_staff_ratio 
                     ,international_students 
                     ,female_male_ratio 
                     ,year
                     from WorldUniversityRanking.dbo.timesData")

library(dplyr)

# the summary of the obs
sqlsummary<-sqlQuery(db," Select Count(*) AS TotabObs, Avg(total_score) AS Avgscore
                     From WorldUniversityRanking.dbo.timesData")
sqlsummary

#List the top 1 Universities from 2011 to 2016 */
TOP1<-sqlQuery(db,"select *
               from ( select ROW_NUMBER() over(partition by year order by world_rank ASC ) worldRank,
               university_name,country,year 
               from WorldUniversityRanking.dbo.timesData
               where world_rank<201) a 
               where worldRank<2")  
TOP1

# rank of UC
UC<-sqlQuery(db," Select world_rank,university_name,total_score,year
             From WorldUniversityRanking.dbo.timesData
             where university_name like '%Cincinnati'")
UC


##Plot
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(plotly)
univer<-read.csv("C:/6045 R&SAS/final/data/timesData.csv",stringsAsFactors=FALSE,header=T);
str(univer)
boxplot(univer$teaching)
#take Duke,Harvard,Princeton University as an example to show the change of rank
duke<-"Duke University"
duke.university<-univer[univer$university_name==duke,]

Harvard<-"Harvard University"
Harvard.University<-univer[univer$university_name==Harvard,]

Princeton<-"Princeton University"
Princeton.University<-univer[univer$university_name==Princeton,]

total <- rbind(duke.university,Harvard.University,Princeton.University)

str(total)
#converting world_rank

# converting world_rank in times to numeric 

duke.university[,1]=as.numeric(duke.university$world_rank)
Harvard.University[,1]=as.numeric(Harvard.University$world_rank)
Princeton.University[,1]=as.numeric(Princeton.University$world_rank)
#https://plot.ly/r/line-and-scatter/
#Plotting rankings of the top 3 universities over the years
library(magrittr)
plot_ly(data= duke.university, x= ~year, y = ~world_rank,name ='duke', type='scatter', mode='lines+markers')%>%
  add_trace(data = Harvard.University,name= 'harvard')%>%
  add_trace(data=Princeton.University,name = 'princeton')%>%
  layout(title = 'University Ranking Over Time')

#read the data for the year of 2011
uni<-univer[univer$year==2011,]

# country VS university count 
country_VS_uni<-uni %>%
  na.omit() %>%
  group_by(country)%>% 
  summarize(count = n())

# List top 10 country
top_10_country <- country_VS_uni %>%
  arrange(desc(count)) %>%
  head(10)
top_10_country



# Plot top 10 country as per university count
ggplot(top_10_country, 
       aes(x=reorder(country, -count), y=count, fill=country)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(x="Count",y="Country") +
  ggtitle("Top 10 Country by number of university ")

#mapping
library(rworldmap)
gtdMap <- joinCountryData2Map( country_VS_uni, nameJoinColumn="country", joinCode="NAME" )
mapParams <- mapCountryData(gtdMap,
                            nameColumnToPlot="count", 
                            catMethod="fixedWidth",
                            numCats=4,colourPalette="rainbow",
                            mapTitle="The number of university VS Country")

#Regression
#due to the 2016 has the less missing value
uni_2016 <- univer[univer$year==2016,]
uni2016 <- uni_2016[1:100,4:12]
str(uni2016)
#output data
getwd()
library(RODBC)
write.csv(d, file = "C:/6045 R&SAS/final/data/tdclean2.csv", row.names = F, quote = F)
write.table(uni2016, file = 'C:/6045 R&SAS/final/data/tdclean.txt',quote = F)

#from the output, then it can be learned that some variable need to be cleaned.
#clean data
uni2016$international <- as.numeric(as.character(uni2016$international))
uni2016$income <- sub('-','0',uni2016$income)
uni2016$income <- as.numeric(as.character(uni2016$income))
uni2016$total_score <- as.numeric(as.character(uni2016$total_score))
uni2016$num_students <- gsub(',','',uni2016$num_students)
uni2016$num_students <- as.numeric(as.character(uni2016$num_students))
uni2016$international_students <- as.numeric(as.character(gsub('%','',uni2016$international_students)))/100
str(uni2016)
d1 <- na.omit(uni2016)
d1
summary(d1)

#Correlation
cor(d1[,c(1:5,7:9)],method="pearson")
#http://www.statmethods.net/stats/regression.html
#regression
model<- lm(total_score~., d1)
print(summary(model))


# Stepwise Regression
library(MASS)
model<- lm(total_score~teaching + international + research + citations + 
             income + num_students +student_staff_ratio,d1)
print(summary(model))

#stepwise selection
step <- stepAIC(model, direction="both")
step$anova # display results


#Finnal Model
model1<-lm(total_score ~ teaching + international + research + citations + 
             income + student_staff_ratio,d1)
print(summary(model1))


#Evaluate Collinearity
library(car)
vif(model1) # variance inflation factors 


# Test for Autocorrelated Errors
library(lmtest)
durbinWatsonTest(model1)

#residual diagoues
#QQ plot 
outlierTest(model1)# Bonferonni p-value for most extreme obs
qqPlot(model1, main="QQ Plot")
qqnorm(model1$residuals)

#Constant Variance
ncvTest(model1)
spreadLevelPlot(model1,main="Residual Plot",ylab="Residual")

plot(model1$fitted.values,model1$residuals,main="Fitted Value vs Residuals",xlab="Fitted Values",ylab="Residuals",col="red")

av.Plots(model1)
cutoff <- 4/((nrow(mtcars)-length(model1$coefficients)-2)) 
plot(model1, which=4, cook.levels=cutoff)

#diagouse
windows()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
qqnorm(model1$residuals)
plot(model1$fitted.values,model1$residuals,main="Fitted Value vs Residuals",xlab="Fitted Values",ylab="Residuals",col="red")
hist(model1$residuals,col="red",main = "Hisogram of Residuals")
plot(uni2016$total_score[1:99],model1$residuals,main="Observations vs Residuals",xlab="Observations",ylab="Residuals",col="blue")



