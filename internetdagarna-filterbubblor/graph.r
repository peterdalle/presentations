library(lubridate)
library(tidyverse)
library(gridExtra)

# **************************************************************************
# Filterbubblor.
# **************************************************************************

# Läs in data till data frame.
text <- readLines("filterbubbl.txt")
text <- strsplit(text, "\t")
df <- data.frame(monthyear=text[[1]], results=as.numeric(text[[2]]))

# Vill bara ha resultat för år 2001 och framåt.
df <- df[325:NROW(df), ]

# Gör om mm/yy till yyyy-mm-dd.
df$date <- as.POSIXct(paste0("20", substr(df$monthyear, 4, 5), "-", substr(df$monthyear, 1, 2), "-01"))

# Markera november 2016.
df$highlight <- if_else(ymd(df$date) == ymd("2016-11-01"), TRUE, FALSE)

# Gör plot.
gg.filt <- df %>%
  ggplot(aes(date, results, fill=highlight)) +
  geom_bar(stat="identity", fill="#6C6EA0") +
  scale_x_datetime(date_breaks="3 month", date_labels="%b %Y") +
  labs(title="Filterbubblor i svensk tryckt press",
       subtitle="Från 2011 till november 2017",
       caption="Data: Retriever, 19 nov 2017, sökord: filterbubbl*",
       x="Månad",
       y="Antal träffar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
  
# **************************************************************************
# Algoritmer
# **************************************************************************

# Läs in data till data frame.
text <- readLines("algoritm.txt")
text <- strsplit(text, "\t")
df <- data.frame(monthyear=as.character(text[[1]]), results=as.numeric(text[[2]]))

# Gör om mm/yy till yyyy-mm-dd för 1900-talet.
df$date <- as.POSIXct(strptime("1800-01-01 01:01:01", "%Y-%m-%d %H:%M:%S"))
df[1:193,]$date <- as.POSIXct(paste0("19", substr(df$monthyear[1:193], 4, 5), "-", substr(df$monthyear[1:193], 1, 2), "-01"))

# Gör om mm/yy till yyyy-mm-dd för 2000-talet.
df[193:NROW(df),]$date <- as.POSIXct(paste0("20", substr(df$monthyear[193:NROW(df)], 4, 5), "-", substr(df$monthyear[193:NROW(df)], 1, 2), "-01"))

# Vill bara ha resultat för år 2011 och framåt.
df <- df %>% filter(date > as.Date("2011-01-01"))

# Markera november 2016.
df$highlight <- if_else(ymd(df$date) == ymd("2016-11-01"), TRUE, FALSE)

# Gör plot.
gg.algo <- df %>%
  ggplot(aes(date, results, fill=highlight)) +
  geom_bar(stat="identity", fill="#FF1053") +
  scale_x_datetime(date_breaks="6 month", date_labels="%b %Y") +
  labs(title="Algoritmer i svensk tryckt press",
       subtitle="Från 2011 till november 2017",
       caption="Data: Retriever, 19 nov 2017, sökord: algoritm*",
       x="Månad",
       y="Antal träffar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# **************************************************************************
# Faktaresistens
# **************************************************************************

# Läs in data till data frame.
text <- readLines("faktaresisten.txt")
text <- strsplit(text, "\t")
df <- data.frame(monthyear=as.character(text[[1]]), results=as.numeric(text[[2]]))

# Gör om mm/yy till yyyy-mm-dd för 2000-talet.
df$date <- as.POSIXct(paste0("20", substr(df$monthyear, 4, 5), "-", substr(df$monthyear, 1, 2), "-01"))

# Vill bara ha resultat för år 2015 och framåt.
df <- df %>% filter(date > as.Date("2015-01-01"))

# Markera november 2016.
df$highlight <- if_else(ymd(df$date) == ymd("2016-11-01"), TRUE, FALSE)

# Gör plot.
gg.fact <- df %>%
  ggplot(aes(date, results, fill=highlight)) +
  geom_bar(stat="identity", fill="#66C7F4") +
  scale_x_datetime(date_breaks="2 month", date_labels="%b %Y") +
  labs(title="Faktaresistens i svensk tryckt press",
       subtitle="Från 2015 till november 2017",
       caption="Data: Retriever, 19 nov 2017, sökord: faktaresisten*",
       x="Månad",
       y="Antal träffar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# **************************************************************************
# Falska nyheter
# **************************************************************************

# Läs in data till data frame.
text <- readLines("falskanyheter.txt")
text <- strsplit(text, "\t")
df <- data.frame(monthyear=as.character(text[[1]]), results=as.numeric(text[[2]]))

# Gör om mm/yy till yyyy-mm-dd för 1900-talet.
df$date <- as.POSIXct(strptime("1800-01-01 01:01:01", "%Y-%m-%d %H:%M:%S"))
df[1:193,]$date <- as.POSIXct(paste0("19", substr(df$monthyear[1:193], 4, 5), "-", substr(df$monthyear[1:193], 1, 2), "-01"))

# Gör om mm/yy till yyyy-mm-dd för 2000-talet.
df[193:NROW(df),]$date <- as.POSIXct(paste0("20", substr(df$monthyear[193:NROW(df)], 4, 5), "-", substr(df$monthyear[193:NROW(df)], 1, 2), "-01"))

# Vill bara ha resultat för år 2015 och framåt.
df <- df %>% filter(date > as.Date("2015-01-01"))

# Markera november 2016.
df$highlight <- if_else(ymd(df$date) == ymd("2016-11-01"), TRUE, FALSE)

# Gör plot.
gg.fake <- df %>%
  ggplot(aes(date, results, fill=highlight)) +
  geom_bar(stat="identity", fill="#519E8A") +
  scale_x_datetime(date_breaks="2 month", date_labels="%b %Y") +
  labs(title="Falska nyheter i svensk tryckt press",
       subtitle="Från 2011 till november 2017",
       caption='Data: Retriever, 19 nov 2017, sökord: "fejkade nyheter"|"fejkad nyhet"|"falska nyheter"|"falsk nyhet"|"fake news"',
       x="Månad",
       y="Antal träffar") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# **************************************************************************
# Slå ihop till en enda graf
# **************************************************************************

# 4x4 alla samma storlek.
grid.arrange(gg.filt, gg.algo, gg.fact, gg.fake, layout_matrix=rbind(c(1,1,2,2),
                                                                     c(3,3,4,4)))

# Powerpoint-version: Större text + mer avskalad + annan layout.
pp_theme <- theme_minimal(base_size=16) +
              theme(axis.text.x = element_text(angle = 90),
                    axis.title.x = element_blank(),
                    plot.subtitle = element_blank(),
                    plot.caption = element_blank()) 

gg.filt <- gg.filt + pp_theme + labs(title="Filterbubblor")
gg.algo <- gg.algo + pp_theme + labs(title="Algoritmer")
gg.fact <- gg.fact + pp_theme + labs(title="Faktaresistens")
gg.fake <- gg.fake + pp_theme + labs(title="Fake news")

grid.arrange(gg.filt, gg.algo, gg.fact, gg.fake, layout_matrix=rbind(c(1,1,1,1,2,2),
                                                                     c(1,1,1,1,3,3),
                                                                     c(1,1,1,1,4,4)))
