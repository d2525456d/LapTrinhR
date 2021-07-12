library(tidyverse)
library(dplyr)
Confi = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
Dea = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
Reco = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
#Group
Confirmed_nn = Confi %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths_nn = Dea %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered_nn = Reco %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
Cdr_join = full_join(Confirmed_nn, deaths_nn) %>% full_join(recovered_nn)
Cdr_join$date = Cdr_join$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
Cdr_join = Cdr_join %>% group_by(Country.Region) %>% mutate( day = date - first(date) + 1)
allworld = Cdr_join %>% group_by(date) %>% summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>% mutate(day = date - first(date) + 1)
country = Cdr_join %>% group_by(Country.Region) %>% summarize(cumconfirmed = max(confirmed), cumdeaths = max(deaths), cumrecovered = max(recovered))
#plot1
library(tmap)
data(World)
country$Country.Region[!country$Country.Region %in% World$name]
lst = which(!country$Country.Region %in% World$name)
country$country = as.character(country$Country.Region)
country$country[lst] =
  c("Andorra"            ,              "Antigua and Barbuda"   ,           "Bahrain" ,                        
    "Barbados"            ,             "Bosnia and Herzegovina"  ,         "Burma"    ,                       
    "Cabo Verde"           ,            "Central African Republic"  ,       "Comoros"   ,                      
    "Congo (Brazzaville)"   ,           "Congo (Kinshasa)"      ,           "Czechia"    ,                     
    "Diamond Princess"      ,           "Dominica"              ,           "Dominican Republic"     ,         
    "Equatorial Guinea"      ,          "Eswatini"               ,          "Grenada"                ,         
    "Holy See"                ,         "Kiribati"                ,         "Korea, South"           ,         
    "Laos"                     ,        "Liechtenstein"            ,        "Maldives"           ,             
    "Malta"                     ,       "Marshall Islands"          ,       "Mauritius"           ,            
    "Micronesia"                 ,      "Monaco"                     ,      "MS Zaandam"          ,            
    "North Macedonia"             ,     "Palau"                       ,     "Saint Kitts and Nevis"  ,         
    "Saint Lucia"                  ,    "Saint Vincent and the Grenadines", "Samoa"        ,                   
    "San Marino"                 ,      "Sao Tome and Principe"  ,          "Seychelles"    ,                  
    "Singapore"                    ,    "Solomon Islands"          ,        "South Sudan"     ,                
    "Summer Olympics 2020"         ,    "Taiwan*"                  ,        "United States",
    "West Bank and Gaza" )
World$country = World$name
map = left_join(World, country, by="country")
map$cumconfirmed[is.na(map$cumconfirmed)] <- 0
ggplot(map)+ 
  geom_sf(aes(fill=cumconfirmed), color="green") +
  ggtitle("Bản đồ tình hình nhiễm covid trên thế giới")+
  theme_classic()
#plot2
vietnam =  Cdr_join %>% filter(Country.Region=="Vietnam")
ggplot()+
  geom_line(data = vietnam, aes(x= date, y= confirmed ),size = 2,color = 'red') + 
  geom_line(data = vietnam, aes(x= date, y= deaths),size = 2,color = "gray")+ 
  geom_line(data = vietnam, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid tại việt nam")+
  theme_light()
#plot3
us = Cdr_join %>% filter(Country.Region =="US")
ggplot()+
  geom_line(data = us, aes(x= date, y= confirmed ),size = 2,color = 'red') + 
  geom_line(data = us, aes(x= date, y= deaths),size = 2,color = "gray")+ 
  geom_line(data = us, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid tại Mỹ")+
  theme_light()
#plot4
map$cumdeaths[is.na(map$cumdeaths)] <- 0
ggplot(map)+ 
  geom_sf(aes(fill=cumdeaths), color="blue") +
  ggtitle("Bản đồ tình hình tử vong vì covid trên thế giới")+
  theme_light()
#Plot5
map$cumrecovered[is.na(map$cumrecovered)] <- 0
ggplot(map)+ 
  geom_sf(aes(fill=cumrecovered), color="black") +
  ggtitle("Bản đồ tình hình chữa khỏi covid trên thế giới")+
  theme_classic()
#plot6
Cdrjoin = Cdr_join[order(-Cdr_join$day),]
Cdrjoin1 = Cdrjoin %>% group_by(date) %>% summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>% mutate(day = date - first(date) + 1)
ggplot()+
  geom_line(data = Cdrjoin1, aes(x= date, y= confirmed ),size = 2,color = 'red') + 
  geom_line(data = Cdrjoin1, aes(x= date, y= deaths),size = 2,color = "gray")+ 
  geom_line(data = Cdrjoin1, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid trên thế giới")+
  theme_light()
#plot7
ok =  Cdr_join %>% filter(Country.Region == "China")
ok1 =  Cdr_join %>% filter(Country.Region == "US")
ok2 =  Cdr_join %>% filter(Country.Region == "India")
ok3 =  Cdr_join %>% filter(Country.Region == "Brazil")
ok4 = Cdr_join %>% filter(Country.Region == "France")
ggplot()+
  scale_colour_manual(
    values=c(China="red", US="blue", India="green",Brazil = "Black",France = "grey"))+
  geom_line(data = ok, aes(x = date, y = confirmed, size = 2,color = "China"))+
  geom_line(data = ok1, aes(x = date, y = confirmed, size = 2,color = "US"))+
  geom_line(data = ok2, aes(x = date, y = confirmed, size = 2,color = "India"))+
  geom_line(data = ok3, aes(x = date, y = confirmed, size = 2,color = "Brazil"))+
  geom_line(data = ok4, aes(x = date, y = confirmed, size = 2,color = "France"))+
  
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid của China,US,India,Brazil,France")+
  theme_light()



df = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-31-2020.csv',header = TRUE)
View(df)

#Group
dt = df %>% group_by(Country_Region)
#plot8
dtt = dt %>% summarise(Confirmed = sum(Confirmed),Deaths = sum(Deaths),Recovered = sum(Recovered))
dtt = dtt[order(-dtt$Confirmed),]
for (i in 1:7){
  dtt$Recovered[i] = dtt$Recovered[i] - dtt$Deaths[i]
  dtt$Confirmed[i] = dtt$Confirmed[i] - dtt$Deaths[i] - dtt$Recovered[i]
}
cot1 = c(rep(dtt$Country_Region[1],3),
         rep(dtt$Country_Region[2],3),
         rep(dtt$Country_Region[3],3),
         rep(dtt$Country_Region[4],3),
         rep(dtt$Country_Region[5],3),
         rep(dtt$Country_Region[6],3),
         rep(dtt$Country_Region[7],3)
)
cot2 = rep(c("Số ca còn nhiễm","Số ca chữa khỏi","Số ca tử vong"),7)
cot3 = c(dtt$Confirmed[1],dtt$Recovered[1],dtt$Deaths[1],
         dtt$Confirmed[2],dtt$Recovered[2],dtt$Deaths[2],
         dtt$Confirmed[3],dtt$Recovered[3],dtt$Deaths[3],
         dtt$Confirmed[4],dtt$Recovered[4],dtt$Deaths[4],
         dtt$Confirmed[5],dtt$Recovered[5],dtt$Deaths[5],
         dtt$Confirmed[6],dtt$Recovered[6],dtt$Deaths[6],
         dtt$Confirmed[7],dtt$Recovered[7],dtt$Deaths[7])
dtt_n = data.frame(cot1,cot2,cot3)
ggplot(dtt_n, aes(fill=cot2, y=cot3, x=cot1)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Quốc gia")+
  ylab("số ca")+
  ggtitle("Tổng số ca tử vong, chữa khỏi, còn nhiễm covid 19 của 7 quốc gia có số ca nhiễm nhiều nhất")+
  theme(legend.position = "bottom")
#plot9
dt_Confirmed = dt %>% summarise(Confirmed = sum(Confirmed))
dt_Confirmed = dt_Confirmed[order(-dt_Confirmed$Confirmed),]
dt_Confirmed_n = dt_Confirmed[1:10,]
others_Confirmed=sum(dt_Confirmed$Confirmed)-sum(dt_Confirmed_n$Confirmed)
others_Confirmed=data.frame("Others", others_Confirmed)
names(others_Confirmed)<-c("Country_Region","Confirmed")
dt_Confirmed <- rbind(dt_Confirmed_n, others_Confirmed)
dt_Confirmed$prop = sprintf((dt_Confirmed$Confirmed / sum(dt_Confirmed$Confirmed) *100), fmt = '%#.2f')
ggplot(dt_Confirmed,aes(x="",y = Confirmed,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ nhiễm covid của các nước") +
  theme_void()
#plot10
dt_Deaths = dt %>% summarise(Deaths = sum(Deaths))
dt_Deaths = dt_Deaths[order(-dt_Deaths$Deaths),]
dt_Deaths_n = dt_Deaths[1:10,]
others_Deaths=sum(dt_Deaths$Deaths)-sum(dt_Deaths_n$Deaths)
others_Deaths=data.frame("Others", others_Deaths)
names(others_Deaths)<-c("Country_Region","Deaths")
dt_Deaths <- rbind(dt_Deaths_n, others_Deaths)
dt_Deaths$prop = sprintf((dt_Deaths$Deaths / sum(dt_Deaths$Deaths) *100), fmt = '%#.2f')
ggplot(dt_Deaths,aes(x="",y = Deaths,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ tử vong vì covid của các nước") +
  theme_void()
#plot11
dt_Recovered = dt %>% summarise(Recovered = sum(Recovered))
dt_Recovered = dt_Recovered[order(-dt_Recovered$Recovered),]
dt_Recovered_n = dt_Recovered[1:10,]
others_Recovered=sum(dt_Recovered$Recovered)-sum(dt_Recovered_n$Recovered)
others_Recovered=data.frame("Others", others_Recovered)
names(others_Recovered)<-c("Country_Region","Recovered")
dt_Recovered <- rbind(dt_Recovered_n, others_Recovered)
dt_Recovered$prop = sprintf((dt_Recovered$Recovered / sum(dt_Recovered$Recovered) *100), fmt = '%#.2f')
ggplot(dt_Recovered,aes(x="",y = Recovered,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ chữa khỏi covid của các nước") +
  theme_void()

#plot 12
dt_Deaths = dt %>% summarise(Deaths = sum(Deaths))
dt_Deaths = dt_Deaths[order(-dt_Deaths$Deaths),]
dt_Deaths_n = dt_Deaths[1:10,]
ggplot(dt_Deaths_n,aes(x =reorder(Country_Region,-Deaths),y = Deaths))+
  geom_col(color = 'orange',fill = "orange")+
  labs(title = "Top 10 nước có số ca tử vong nhiều nhất theo thứ tự giảmm dần vào 31/3/2020")
#plot13
dt_Confirmed = dt %>% summarise(Confirmed = sum(Confirmed))
dt_Confirmed = dt_Confirmed[order(-dt_Confirmed$Confirmed),]
dt_Confirmed_n = dt_Confirmed[1:10,]
View(dt_Confirmed_n)


ggplot(dt_Confirmed_n,aes(x =reorder(Country_Region,-Confirmed),y = Confirmed))+
  geom_col(color = 'red',fill = "black")+
  xlab("quốc gia")+
  ylab("số ca mắc")+
  labs(title = "Top 10 nước có số ca nhiễm nhiều nhất theo thứ tự giảm dần vào 31/3/2020")
#plot14 
dt_Recovered = dt %>% summarise(Recovered = sum(Recovered))
dt_Recovered = dt_Recovered[order(-dt_Recovered$Recovered),]
dt_Recovered_n = dt_Recovered[1:10,]
View(dt_Recovered_n)


ggplot(dt_Recovered_n,aes(x =reorder(Country_Region,-Recovered),y = Recovered))+
  geom_col(color = 'green',fill = "blue")+
  xlab("quốc gia")+
  ylab("số ca chữa khỏi")+
  labs(title = "Top 10 nước có số ca chữa khỏi nhiều nhất theo thứ tự giảm dần vào 31/3/2020")
#plot15
dt_Confirmed_Recovered = dt %>% summarise(Recovered = sum(Recovered),Confirmed = sum(Confirmed))
dt_Confirmed_Recovered = dt_Confirmed_Recovered[order(-dt_Confirmed_Recovered$Confirmed),]
dt_Confirmed_Recovered_n = dt_Confirmed_Recovered[1:10,]
View(dt_Confirmed_Recovered_n)
ggplot(dt_Confirmed_Recovered_n,aes(x = Confirmed, y = Recovered,fill = Country_Region))+
  geom_point(size = 3,aes(col = Country_Region))+
  xlab("Số ca nhiễm")+ylab("Số ca chưa khỏi")+
  labs(tilte = "Biểu đồ thể hiện tương quan giữa số ca nhiễm và ca chữa khỏi của 10 nước có số ca nhiễm nhiều nhất vào 31/3/2020")
#plot16
dt_Confirmed_Deaths = dt %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths))
dt_Confirmed_Deaths = dt_Confirmed_Deaths[order(-dt_Confirmed_Deaths$Confirmed),]
dt_Confirmed_Deaths_n = dt_Confirmed_Deaths[1:10,]
View(dt_Confirmed_Deaths_n)
ggplot(dt_Confirmed_Deaths_n,aes(x = Confirmed, y = Deaths,fill = Country_Region))+
  geom_point(size = 3.5 , aes(col = Country_Region))+
  xlab("Số ca nhiễm")+ylab("số ca tử vong")+
  labs(title = "Biểu đồ thể hiện tương quan giữa số ca nhiễm và ca tử vong của 10 nước có số ca nhiễm nhiều nhất vào 31/3/2020 ")+
  theme(legend.position = "bottom")
#plot7
dt_Deaths_Recovered = dt %>% summarise(Deaths = sum(Deaths), Recovered = sum(Recovered),Confirmed = sum(Confirmed))
dt_Deaths_Recovered = dt_Deaths_Recovered[order(-dt_Deaths_Recovered$Confirmed),]
dt_Deaths_Recovered_n = dt_Deaths_Recovered[1:10,]
View(dt_Deaths_Recovered_n)
ggplot(dt_Deaths_Recovered_n,aes(x = Deaths, y = Recovered,fill = Country_Region))+
  geom_point(size = 6 , aes(col = Country_Region))+
  xlab("Số ca tử vong")+ylab("số ca chữaa khỏi")+
  labs(title = "Biểu đồ thể hiện tương quan giữa số ca tử vong và ca chữa khỏi của 10 nước có số ca nhiễm nhiều nhất vào 31/3/2020")+
  theme(legend.position = "bottom")+
  theme(legend.background = element_rect(fill="green",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"))
#plot18
dt_Confirmed_Deaths2 = dt %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths))
View(dt_Confirmed_Deaths2)
ggplot(dt_Confirmed_Deaths2,aes(x = Confirmed, y = Deaths))+
  geom_point(size = 3, color = "blue")+
  labs(tilte = "Biểu đồ tương quan giữa số ca mắc và tử vong vì covid vào 31/3/2020")
#plot19
dt_Confirmed_Recovered2 = dt %>% summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered))
View(dt_Confirmed_Recovered2)

ggplot(dt_Confirmed_Recovered2,aes(x = Confirmed, y = Recovered))+
  geom_point(size = 2, color = "#FF0000")+
  labs("Biểu đồ tương quan giữa số chữa khỏi và nhiễm covid vào 31/3/2020")
#plot20
dt_Deaths_Recovered2 = dt %>% summarise(Deaths = sum(Deaths), Recovered = sum(Recovered))

View(dt_Deaths_Recovered2)
ggplot(dt_Deaths_Recovered2,aes(x = Deaths, y = Recovered))+
  geom_point(size = 1.5, color = "#3FBF00")+
  labs("Biểu đồ tương quan giữa số chữa khỏi và tử vong vì covid vào 31/3/2020")
