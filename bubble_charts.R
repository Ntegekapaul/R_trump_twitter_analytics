# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

#import Donald Trump 2016 tweets dataset
#dt <- read.csv("C:/{path_to_csv_file}/dt.csv")
#view imported file
#view(dt)glimp

#dt <- read.csv

#convert chr column into date
library(anytime)
library(lubridate)

dt$date <- anydate(dt$date)

library(ggplot2)
library(dplyr)
library(ggthemes)


data <- dt

glimpse(data)

# Most basic bubble plot
data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=data$date, y=data$favorites, size=data$replies)) +
  geom_point(alpha=0.2) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme( axis.line = element_line(colour = "black", 
                                  size = 1, linetype = "solid"))+
  labs(title="Italy Confirmed cases",y ="Number of likes", x = "Dates")+
  geom_vline(xintercept = as.numeric(ymd("2016-07-05")),color = "blue", size=1.5)

data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=data$date, y=data$favorites, size=data$replies)) +
  geom_point(alpha=0.2) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Trump's tweets (July,2016)",y ="Number of likes", x = "Dates")+
  geom_vline(xintercept = as.numeric(ymd("2016-07-05")),color = "black", 
                                  size=0.3)

data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=data$date, y=data$favorites, size=data$replies)) +
  geom_point(alpha=0.2) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Trump's tweets (July,2016)",y ="Number of likes", x = "Dates")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-05", "2016-07-15"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-05", "2016-07-15")),
             labels = c("Day Trump was shot\nby lee", "Day Trump was burried\nin Washington DC"))

ggsave(
  "graph5.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE,
)

dt$mentions_clinton <- str_detect(dt$text, 'Hillary', negate = FALSE)
dt$mentions_crooked <- str_detect(dt$text, 'crooked', negate = FALSE)
dt$mentions_crooked <- str_detect(dt$text, 'Crooked', negate = FALSE)


library(stringr)

dt$mentions_hillary <- str_detect(dt$text, regex('hillary', ignore_case = T))
dt$mentions_crook <- str_detect(dt$text, regex('crooked', ignore_case = T))


glimpse(dt)

data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=dt$date, y=dt$favorites, size=dt$replies,color=dt$mentions_hillary)) +
  geom_point(alpha=0.4) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Trump's tweets (July,2016)",y ="Number of likes", x = "Events")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28")),
               labels = c("Attorney General Loretta\n leaves it up to FBI\n to decide wether to charge Clinton", 
                          "Donald Trump announces \n Mike Pence as his running mate",
                          "First Day \nof the DNC",
                          "Last Day \n of the DNC"))+
  scale_color_manual(values=c('#F38C75','#35B5ED'),
                     labels = c("No mention", "mentioned"),
                     name='Clinton mentioned \n in tweet')
  

ggsave(
  "graph6.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 900,
  limitsize = TRUE,
)

data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=dt$date, y=dt$favorites, size=dt$replies)) +
  geom_point(alpha=0.3) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Trump's tweets (July,2016)",y ="Number of likes", x = "Events")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28")),
               labels = c("Attorney General Loretta\n leaves it up to FBI\n to decide wether to charge Clinton", 
                          "Donald Trump announces \n Mike Pence as his running mate",
                          "First Day \nof the DNC",
                          "Last Day \n of the DNC"))

ggsave(
  "graph8.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 900,
  limitsize = TRUE,
)

################################################################################
#Hillary clinton analysis

#hc <- read.csv("{path_to_dataset}/hc.csv")

library(anytime)
library(lubridate)

hc$date <- anydate(hc$date)

library(ggplot2)
library(dplyr)
library(ggthemes)


data <- hc

glimpse(data)

# Most basic bubble plot
data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=hc$date, y=hc$favorites, size=hc$replies)) +
  geom_point(alpha=0.3) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Clinton's tweets (July,2016)",y ="Number of likes", x = "Events")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-01", "2016-07-16", "2016-07-25","2016-07-28")),
               labels = c("Attorney General Loretta\n leaves it up to FBI\n to decide wether to charge Clinton", 
                          "Donald Trump announces \n Mike Pence as his running mate",
                          "First Day \nof the DNC",
                          "Last Day \n of the DNC"))

ggsave(
  "graph1.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 900,
  limitsize = TRUE,
)

library(stringr)

hc$mentions_trump <- str_detect(hc$text, regex('trump', ignore_case = T))

data %>%
  arrange(desc(date)) %>%
  ggplot(aes(x=hc$date, y=hc$favorites, size=hc$replies,color=hc$mentions_trump)) +
  geom_point(alpha=0.4) +
  scale_size(range = c(.1, 24), name="Replies")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Engagement of Clinton's tweets (July,2016)",y ="Number of likes", x = "Events")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-01", "2016-07-16", "2016-07-22","2016-07-28"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-01", "2016-07-16", "2016-07-22","2016-07-28")),
               labels = c("Attorney General Loretta\n leaves it up to FBI\n to decide wether to charge Clinton", 
                          "Clinton announces Tim Kaine\n as her running mate.",
                          "First Day \nof the DNC",
                          "Last Day \n of the DNC"))+
  scale_color_manual(values=c('#56B4E9','#E69F00'),
                     labels = c("No mention", "mentioned"),
                     name='Donald Trump\n mentioned \n in the tweet')


ggsave(
  "graph2.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 900,
  limitsize = TRUE,
)

## count number of tweets per day. for both Clinton and Trump.


library(anytime)
library(lubridate)

dt$date <- anydate(dt$date)
hc$date <- anydate(hc$date)

library(ggplot2)
library(dplyr)
library(ggthemes)


num_of_tweets_dt <- data.frame(table(dt$date))
glimpse(num_of_tweets_dt)
num_of_tweets_dt$Var1 <- anydate(num_of_tweets_dt$Var1)

colnames(num_of_tweets_dt)[1] <- "Dates"

num_of_tweets_hc <- data.frame(table(hc$date))
glimpse(num_of_tweets_hc)
num_of_tweets_hc$Var1 <- anydate(num_of_tweets_hc$Var1)

colnames(num_of_tweets_hc)[1] <- "Dates"

daily_comparison <- merge(num_of_tweets_dt,num_of_tweets_hc,by="Dates")
colnames(daily_comparison)[2] <- "Trump_daily"
colnames(daily_comparison)[3] <- "Clinton_daily"
glimpse(daily_comparison)

daily_comparison$abitrary_y_value_c <- 15
daily_comparison$abitrary_y_value_d <- 25


glimpse(daily_comparison)

library(ggplot2)

data <- daily_comparison

glimpse(data)

# Most basic bubble plot
data %>%
  arrange(desc(Dates)) %>%
  ggplot(aes(x=data$Dates, y=data$abitrary_y_value_c, size=data$Clinton_daily, color="#5ab2ed")) +
  ylim(10, 30)+
  geom_point(alpha=0.4)+
  scale_size(range = c(.1, 30), name="number of tweets\n per day")+theme_tufte()+
  theme(plot.title = element_text(hjust = 0.5, family = "Helvetica", face = "bold", size = (15)),
        legend.title = element_text(hjust = 0.5, face = "bold", family = "Helvetica", size = (12)), 
        legend.text = element_text(face = "bold", family = "Helvetica",size = (12)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(family = "Helvetica", face = "bold"),
        axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  labs(title="Clinton and trump number of tweets per day (July,2016)",y ="Clinton & Trump", x = "Events")+
  geom_vline(xintercept = as.numeric(ymd(c("2016-07-01", "2016-07-16", "2016-07-22","2016-07-28"))),color = "black", size=0.3)+
  scale_x_date(breaks = as.Date(c("2016-07-01", "2016-07-16", "2016-07-22","2016-07-28")),
               labels = c("Attorney General \nLoretta leaves it up \nto FBI to decide \nwether to charge Clinton", 
                          "Clinton announces Tim Kaine\n as her running mate.",
                          "First Day \nof the DNC",
                          "Last Day \n of the DNC"))+
  scale_color_manual(values=c('#56B4E9','red'),
                     labels = c("Clinton", "trump"),
                     name='')+
  geom_point(alpha=0.4,aes(x=data$Dates, y=data$abitrary_y_value_d, size=data$Trump_daily, color="red"))


ggsave(
  "graph2.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 900,
  limitsize = TRUE,
)
