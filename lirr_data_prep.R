#Load the packages
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(RColorBrewer)


#Load Data
Performance_DF = read.csv("./Performance_LIRR.csv",stringsAsFactors = FALSE)
stops_df = read.csv("./stops.csv",stringsAsFactors = FALSE)

#This function cleans text
clean_text <- function(x,y) {
  z = gsub(y,'', x)
  return(z)
}


#Clean Data
Performance_DF_Clean = Performance_DF%>%
  filter(., Parent.Sequence == 20421 | Indicator.Sequence == 20421)%>%
  select(., Indicator.Name, Period.Month, Period.Year, 
         YTD.Target, YTD.Actual, Monthly.Target, Monthly.Actual, Period)%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Branch - OTP"))%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Branch OTP"))%>%
  mutate(., Indicator.Name = str_trim(Indicator.Name))%>%
  mutate(., Month_Name = month.name[Period.Month])

#write.csv(Performance_DF_Clean, file = "Performance_DF_Clean.csv",row.names=FALSE)


#Analyze average branch performance by month
df = Performance_DF_Clean%>%
  filter(., Indicator.Name == "Oyster Bay")%>%
  group_by(., Period.Month)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))

g = ggplot(df, aes(x = month.abb[Period.Month], y = Average_OTP, group = 1)) 
g + 
  #geom_bar(stat = 'identity', position = 'dodge', fill = 'Blue') + 
  geom_point() +
  geom_line() +
  xlab("Month") + ylab('Percentage On-Time, (%)') + scale_x_discrete(labels=month.abb) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Branch On-Time Performance By Month Since 2008')


#Analyze average branch performance since 2008
df1 = Performance_DF_Clean%>%
  filter(., Indicator.Name != 'On-Time Performance')%>%
  group_by(Indicator.Name)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(.,(Average_OTP))

#df$Average_OTP = format(round(df$Average_OTP, 2), nsmall = 2)
df1$Indicator.Name <- factor(df1$Indicator.Name, levels = df1$Indicator.Name)
g1 = ggplot(df, aes(x = Indicator.Name, y = Average_OTP, fill = Indicator.Name )) 
g1 + 
  geom_col() + coord_flip() + scale_fill_brewer(palette="Spectral") +
  xlab("Branch") + ylab("Average OTP (%)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Average On-Time Performance Since 2008') +
  geom_text(aes(label=format(round(df1$Average_OTP, 2), nsmall = 2)), hjust = 1.5, size=3.0)

#Overall OTP by month
df = Performance_DF_Clean%>%
  filter(., Indicator.Name == "On-Time Performance")%>%
  group_by(., Period.Month)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))


top1 <- head(df, 1)
bot1 <- tail(df, 1)

ggplot(df, aes(x = month.abb[Period.Month], y = Average_OTP, group = 1) ) +
  geom_point() +
  geom_line() +
  xlab("Month") + ylab('Percentage On-Time, (%)') + scale_x_discrete(labels=month.abb) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Branch On-Time Performance By Month Since 2008')
  
#Overall OTP by month comparison
df = Performance_DF_Clean%>%
  filter(., Indicator.Name == "Port Jefferson")%>%
  group_by(., Indicator.Name, Period.Year)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))

ggplot(df, aes(x = factor(Period.Year), y = Average_OTP, group = 1)) +
  geom_line(aes(color = 'red')) +
  xlab("Year") + ylab('Percentage On-Time, (%)') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Branch On-Time Performance By Year') +
  theme(legend.title = element_blank()) +
  scale_color_manual(labels = c(df$Indicator.Name), values = c("red"))

temp_df = Performance_DF_Clean%>%
  filter(., Indicator.Name != "On-Time Performance")%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"Hicksville/"))%>%
  mutate(., Indicator.Name = clean_text(Indicator.Name,"/Ronkonkoma"))
  
branches = as.data.frame(unique(temp_df$Indicator.Name))
branches = select(branches, Indicator.Name = 'unique(temp_df$Indicator.Name)')
stops_df2 = select(stops_df, Indicator.Name = 'stop_name', stop_lat, stop_lon)

stops_df3 = left_join(branches,stops_df2,by = 'Indicator.Name')
stops_df4 = filter(stops_df, stop_name == 'Penn Station')


list2 <- rep(stops_df4$stop_lat,NROW(stop_df3))
list3 <- cbind(list2, list1)


##################################
#Branch Distance and #of Stations
num_of_stations = c(16,7,10,5,9,14,14,6,10,13)
distance = c(117,22.8,59.4,22.5,22.6,96.1,32.3,24.9,34.7,19.9)
Indicator.Name = c('Montauk', 'Far Rockaway','Port Jefferson','West Hempstead',
                 'Hempstead','Greenport/Ronkonkoma','Babylon','Long Beach','Oyster Bay', 'Port Washington')

assoc_df = data.frame(Indicator.Name, num_of_stations, distance)

#combine with overall delay 
test_df = Performance_DF_Clean%>%
  filter(., Indicator.Name != 'On-Time Performance')%>%
  filter(., Indicator.Name != 'Hicksville/Huntington')%>%
  group_by(., Indicator.Name)%>%
  summarise(., Average_OTP = mean(Monthly.Actual))%>%
  arrange(., desc(Average_OTP))

assoc_df1 = left_join(assoc_df, test_df, by = 'Indicator.Name' )
write.csv(assoc_df1, file = "assoc_df.csv",row.names=FALSE)




