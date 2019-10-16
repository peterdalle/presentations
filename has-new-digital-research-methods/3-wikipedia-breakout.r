#install.packages("devtools")
#devtools::install_github("twitter/BreakoutDetection")

library(pageviews)
library(dplyr)
library(ggplot2)
library(BreakoutDetection)
library(magrittr)

# ***********************************************
# Project + top articles
# ***********************************************

# Views per language.
perproject <- project_pageviews(start = as.Date("2017-04-07"), end=as.Date("2017-04-09"), project = c("sv.wikipedia", "en.wikipedia"))
perproject

# Top articles.
toparticles <- top_articles(start = as.Date("2017-04-07"), end=as.Date("2017-04-09"), project = "sv.wikipedia")
head(toparticles, 20)

# ***********************************************
# Page views
# ***********************************************

# Page views - 1 article.
pageviews <- article_pageviews(project = "sv.wikipedia",
                               article = "Terrorism",
                               start = as.Date('2016-01-01'),
                               end = as.Date("2017-04-17"))
pageviews %>%
  ggplot(aes(date, views)) +
  geom_line() +
  theme_bw() +
  labs(title=paste("Wikipedia page views:", pageviews$article[1]))


# Page views - comparison.
pageviews.comp <- article_pageviews(project = c("sv.wikipedia"),
                                    article = c("Terrorism", "Bombdåden_i_Stockholm_2010", "Attentatet_i_Nice_2016", "Tomahawk_(robot)", "Islamiska_staten", "Terrordåden_i_Paris_i_november_2015", "Stabsläge"),
                                    start = as.Date('2017-01-01'),
                                    end = as.Date("2017-04-09"))
pageviews.comp %>%
  ggplot(aes(date, views, color=factor(article))) +
  geom_line(size=1.2) +
  theme_bw() +
  labs(title="Wikipedia page views")


# Page views - comparison.
pageviews.comp <- article_pageviews(project = c("sv.wikipedia"),
                               article = c("Stefan_Löfven", "Donald_Trump", "Barack_Obama"),
                               start = as.Date('2017-01-01'),
                               end = as.Date("2017-04-07"))
pageviews.comp %>%
  ggplot(aes(date, views, color=factor(article))) +
  geom_line(size=1.2) +
  theme_bw() +
  labs(title="Wikipedia page views")

# ***********************************************
# Breakout detection
# ***********************************************

# Breakout detection (mean shift detection).
df <- pageviews %>% select(date, views) %>% mutate(timestamp = date, count = views)
breaks <- breakout(df, min.size=10, method="multi", percent=0.05, plot=TRUE)
breaks

# Add mean spans from breakout detection.
breaks <- df[breaks$loc,]
df$span <- 0
for (d in breaks$date) {
  df$span[df$date > d ] %<>% add(1)
}

df$mcount <- 0
for (s in unique(df$span) ) {
  df$mcount[df$span == s] <- mean(df$count[df$span == s])
}

spans <- df %>% 
  as_data_frame() %>% 
  group_by(span) %>% 
  summarize(start = min(date), end = max(date), length = end - start, mean_count = round(mean(count)),
    min_count = min(count), max_count = max(count), var_count = var(count))

spans

# Plot mean shifts.
ggplot(df, aes(x=date, y=count) ) + 
  geom_line(alpha=0.5, color="steelblue") + 
  geom_line(aes(y=mcount), alpha=0.5, color="red2", size=1.2) + 
  theme_bw() + 
  labs(title="Mean shifts spans")

# Plot loess smooth to compare.
#ggplot(df, aes(x=date, y=count) ) + 
 # geom_line(alpha=1.0) + 
  #geom_smooth(method="loess") +
  #theme_bw() + 
  #labs(title="Loess")
