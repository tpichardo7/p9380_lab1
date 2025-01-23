### Lab goal is to create a county level map of Quality of Life Index Ranking 
### from the Robert Wood Johnson Foundation 
### (obtained from here: https://www.countyhealthrankings.org/)

#setwd("")
#
#install.packages("data.table")
#install.packages("tidycensus")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("sf")



require(data.table)
require(tidycensus)
require(ggplot2)
require(ggpubr)
require(sf)
census_api_key("aa512886c5449a582d837da8d3a07af66a043fe5")

census_data = load_variables(2010, "sf1", cache=T)
View(census_data)
fwrite(census_data, "census_variables.csv")

vars = c(tpop = 'P001001',
          medage = 'P013001',
          wpop = 'P003002',
          bpop = 'P003003',
          apop = 'P003005',
          hpop = 'P004003')
View(vars)

FL_df = get_decennial(state = "FL", 
                       geography = "county",
                       variables = vars,
                       year = 2010,
                       geometry = T,
                       output = "wide")
View(FL_df)

plot(FL_df["tpop"])
plot(FL_df["bpop"])
plot(FL_df["medage"])

FL_df$wpct = (FL_df$wpop / FL_df$tpop) *100
FL_df$bpct = (FL_df$bpop / FL_df$tpop) *100
FL_df$apct = (FL_df$apop / FL_df$tpop) *100
FL_df$hpct = (FL_df$hpop / FL_df$tpop) *100

###Compute Simpson's Diversity Index **D = ((SUM n(n-1))/N(N-1))**
###Higher value indicates higher diversity

FL_df$race_div = 1 - (((FL_df$wpop*(FL_df$wpop-1))+
                          (FL_df$bpop*(FL_df$bpop-1))+
                          (FL_df$hpop*(FL_df$hpop-1))+
                          (FL_df$apop*(FL_df$apop-1)))/
                         (FL_df$tpop*(FL_df$tpop-1)))

head(FL_df)
summary(FL_df$race_div)
plot(FL_df["race_div"])

rwj = fread("rwj_rank.csv", 
             stringsAsFactors = F, 
             data.table = F, 
             colClasses=list(character=c("FIPS")))
head(rwj)
FL_rwj = subset(rwj, State == "Florida")
head(FL_rwj, n=62)

FL_rwj_df = merge(FL_df, 
                   FL_rwj,
                   by.x = "GEOID",
                   by.y = "FIPS")

summary(FL_rwj_df)
summary(FL_rwj_df$QL.Rank)
FL_rwj_df$QL.Rank = as.numeric(FL_rwj_df$QL.Rank)
summary(FL_rwj_df$QL.Rank)

### use ggplot to visualize data in map
map1 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill = cut_number(QL.Rank, 5)))
map1

map2 = ggplot(FL_rwj_df, aes(fill = QL.Rank)) +
  geom_sf() +
  #scale_fill_continuous(low = "#34E8EB", high = "#3D34EB") +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen") +
  ggtitle("County Level Quality of Life Rank") +
  theme(line = element_blank(),                          
        axis.text=element_blank(),                       
        axis.title=element_blank(),                      
        panel.background = element_blank()) 
map2

summary(FL_rwj_df$QL.Rank)
FL_rwj_df$QL.Rank = (FL_rwj_df$QL.Rank - 63) * -1

map3 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill=QL.Rank)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen") +
  ggtitle("County Level Quality of Life Rank") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(hjust = 0.5))
map3

map4 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill=QL.Rank)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen") +
  ggtitle("County Level Quality of Life Rank") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=12,hjust = 0.5)) 
map4

map5 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill=QL.Rank)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen") +
  ggtitle("County Level Quality of Life Rank") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=12,hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9,0.5)) 
map5

map6 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill=QL.Rank)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen",
                        breaks = c(12, 24, 36, 48, 60),
                        name = "Quality of Life",
                        labels = c("low",
                                   "",
                                   "",
                                   "",
                                   "high")) +
  ggtitle("County Level Quality of Life Rank") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=12,hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9,0.5)) 
map6

map7 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill=QL.Rank)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen",
                        breaks = c(3, 12, 24, 36, 48, 60),
                        name = "Quality\nof\nLife",
                        labels = c("low",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "high")) +
  ggtitle("County Level Quality of Life Rank") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=12,hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9,0.5),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) 
map7

names(FL_rwj_df)
summary(FL_rwj_df$race_div)

map8 = ggplot(FL_rwj_df) +
  geom_sf(aes(fill = race_div)) +
  scale_fill_continuous(low = "lightgreen", high = "darkgreen",
                        breaks = c(0.05, 0.1, 0.16, 0.31, 0.65),
                        name = "Racial\nDiversity",
                        labels = c("low",
                                   "",
                                   "",
                                   "",
                                   "high")) +
  ggtitle("County Level Racial Diversity") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=12,hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.9,0.5),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) 
map8


FL_rwj_df$race_div_bin = ifelse(FL_rwj_df$race_div > mean(FL_rwj_df$race_div),
                                 "High Diversity",
                                 "Low Diversity")
FL_rwj_df$Qual_Life_bin = ifelse(FL_rwj_df$QL.Rank > mean(FL_rwj_df$QL.Rank),
                                  "High Quality of Life",
                                  "Low Quality of Life")

plot1 = ggplot(FL_rwj_df, 
                     aes(x = race_div_bin, 
                         y = QL.Rank, 
                         group = race_div_bin, 
                         color = race_div_bin, 
                         fill = race_div_bin)) + 
  geom_boxplot() + 
  labs(title="Boxplots", 
       subtitle= "Quality of Life by Racial Diversity",
       y="Quality of Life", 
       x="Racial Diversity") +
  scale_colour_brewer(palette = "Set3") +
  theme(legend.position = "none")
plot1

#require(RColorBrewer)
#display.brewer.all()

plot2 = ggplot(FL_rwj_df, 
                aes(x = Qual_Life_bin, 
                    y = race_div, 
                    group = Qual_Life_bin, 
                    color = Qual_Life_bin, 
                    fill = Qual_Life_bin)) + 
  geom_boxplot() + 
  labs(title="Boxplots", 
       subtitle= "Racial Diversity by Quality of Life",
       y="Racial Diversity", 
       x="Quality of Life") +
  scale_colour_brewer(palette = "Set3") +
  theme(legend.position = "none")
plot2

#install.packages("ggpubr")
require(ggpubr)

full_plot = ggarrange(map7,
                       map8,
                       plot1,
                       plot2,
                       ncol = 2,
                       nrow = 2)
full_plot


### Please submit the result of the lab script by replicating the
### process for the state of Florida in fulfillment of your 
### assignment 1 requirement





