# Examples from https://github.com/hrbrmstr/newsflash
#devtools::install_github("hrbrmstr/newsflash")
#options(width=120)
library(newsflash)
library(tidyverse)
library(ggalt)
library(hrbrthemes) 
library(ggthemes)

#list_networks(widget=FALSE)
#query_tv("clinton", "email", "AFFMARKALL")
#mex <- query_tv("mexican president", filter_network="NATIONAL")
#top_text(mex)
#head(top_text(mex, tidy=FALSE))

# *********************************************************
# Timeline Trump
# *********************************************************

keyword <- "trump"
orange <- query_tv(keyword)

arrange(orange$station_histogram, value) %>% 
  mutate(station=factor(station, levels=station)) %>% 
  ggplot(aes(value, station)) +
  geom_lollipop(horizontal=TRUE, size=0.75,
                color=ggthemes::tableau_color_pal()(10)[2]) +
  scale_x_continuous(expand=c(0,0), label=scales::comma, limits=c(0,400000)) +
  labs(y=NULL, x="# Mentions",
       title=paste("Station Histogram:", keyword)) +
  theme_ipsum_rc(grid="X")

mutate(orange$timeline, date_start=as.Date(date_start)) %>% 
  filter(date_start >= as.Date("2015-01-01")) %>% 
  ggplot(aes(date_start, value)) +
  geom_area(aes(group=station, fill=station), position="stack") +
  scale_x_date(name=NULL, expand=c(0,0)) +
  scale_y_continuous(name="# Mentions", label=scales::comma, limits=c(0, 8000), expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  labs(title=paste("Timeline:", keyword)) +
  theme_ipsum_rc(grid="XY") +
  theme(legend.position="bottom") +
  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 0.5, 0.5)))
