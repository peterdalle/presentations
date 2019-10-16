# Microsoft Cognitive Services (före detta Oxford Project)
# - Face
# - Emotion
# - Vision

#install.packages(c("httr", "jpeg", "reshape2", "ggplot2", "gridExtra"))
#require(devtools)
#install_github("flovv/Roxford")

# We need httr for HTTP requests and jpeg for plotting images.
library(httr)
library(jsonlite)
library(jpeg)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(Roxford)

# ***********************************************************
# Image
# ***********************************************************

 Politicians
img.url <- "http://www.feliciasliv.se/mm/bilder/lofven-150x200.jpg" # Löfven
img.url <- "http://gfx.aftonbladet-cdn.se/image/22452895/800/normal/0190032aaa466/anna-kinberg-batra.jpg" # Ann Kinberg Batra

get_grob <- function(img.url) {
  img.raw <- GET(img.url)
  img.jpg <- readJPEG(img.raw$content, native=TRUE)
  img.rast <- grid::rasterGrob(img.jpg)
  return(img.rast)
}

# ***********************************************************
# Face
# ***********************************************************

# Project Oxford API Key
OxfordFaceApiKey <- "XXX"
OxfordFaceUrl <- "https://westus.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair"

# HTTP request
httpbody <- list(url=img.url)
face.response <- POST(url=OxfordFaceUrl, content_type('application/json'), add_headers(.headers=c('Ocp-Apim-Subscription-Key'=OxfordFaceApiKey)), body=httpbody, encode='json')

# Convert JSON object
face <- content(face.response)[[1]]

# Print raw output
face

# Read JPG from web into R and display
img.raw <- GET(img.url)
img.jpg <- readJPEG(img.raw$content, native=T)
img.res <- dim(img.jpg)[1:2]
plot(1, 1, xlim=c(1,img.res[1]), ylim=c(1,img.res[2]), type='n', xaxs='i', yaxs='i', xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
rasterImage(img.jpg, 1, 1, img.res[1], img.res[2])

# Add text to plot
title(paste(face$faceAttributes$gender, " ",
            face$faceAttributes$age, " years old ", 
            face$faceAttributes$smile * 100, "% smile\n",
            face$faceAttributes$facialHair$beard * 100, "% beard ",
            face$faceAttributes$facialHair$moustache * 100, "% moustache ",
            face$faceAttributes$facialHair$sideburns * 100, "% sideburns ",
            sep=""))

# ***********************************************************
# Emotions
# ***********************************************************

OxfordEmotionsApiKey <- "XXX"
OxfordEmotionsUrl <- "https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognize"

# HTTP request
httpbody <- list(url = img.url)
emotions.response <- POST(url=OxfordEmotionsUrl, content_type('application/json'), add_headers(.headers=c('Ocp-Apim-Subscription-Key'=OxfordEmotionsApiKey)), body=httpbody, encode='json')

# Convert JSON object
emotions <- content(emotions.response)[[1]]

# Print raw output
emotions

# Plot scores
scores <- melt(as.data.frame(emotions$scores))
gg <- ggplot(data=scores, aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  labs(title="Emotions in picture", x="", y="") +
  theme_bw() + 
  theme(legend.position = "none") + 
  coord_flip()

# Combine face + scores
grid.arrange(gg, grid::rasterGrob(img.jpg))

# ***********************************************************
# Vision
# ***********************************************************

library(Roxford)
OxfordVisionKey <- "XXX"

img.url <- "https://www.svtstatic.se/image-cms/svtse/1492523418/nyheter/lokalt/orebro/article13328000.svt/alternates/extralarge/olycka-jpg" # Olycka, ambulanser

# Get description + tags
vision.desc <- getDescriptionResponseURL(img.url, OxfordVisionKey)
vision.tags <- getTaggingResponseURL(img.url, OxfordVisionKey)

# Show pic
img.grob <- get_grob(img.url)
grid.arrange(img.grob)

vision.desc
vision.tags
  
as.character(vision.desc$captions.text[1])

# ***********************************************************
# Text analysis
# ***********************************************************

df <- data.frame(text = c("Skriv texten här.", "Write text here."))

# Setup
OxfordTextAnalysisKey <- "XXX"
OxfordTextAnalysisUrl <- "https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages"

# Use svpol.
text <- c(df$text %>% head(10))
df <- data.frame(id = 1:NROW(text), text)

# HTTP request: language
httpbody <- list(documents = df)
lang.response <- POST(OxfordTextAnalysisUrl, content_type('application/json'), add_headers(.headers=c('Ocp-Apim-Subscription-Key'=OxfordTextAnalysisKey)), body=toJSON(httpbody))
lang <- content(lang.response)

# Language
for(i in 1:NROW(text)) 
{
  curr.lang <- lang$documents[[i]]$detectedLanguages[[1]]$name
  curr.tweet <- text[i]
  print(paste(curr.lang, " = ", substr(curr.tweet, 1, 75), sep=""))
}
