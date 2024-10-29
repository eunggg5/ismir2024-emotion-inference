library(Hmisc)
library(corrplot)
library(plotly)

setwd("~/Library/CloudStorage/GoogleDrive-oheunji97@gmail.com/내 드라이브/Document/2KAIST/2024/conference/ISMIR")

eat <- read.csv("target_rate.csv")

eat <- subset(eat, select = -X)

eat$id <- as.factor(eat$id)
eat$emotion <- as.factor(eat$emotion)
eat$type <- as.factor(eat$type)

emotion <- c("joy", "sad", "anger")
types <- c("music", "social")
id_t <- unique(eat$id)


for (i in seq_len(nrow(eat))){
  if (eat[i, c("emotion")] == "joy"){
    eat[i, c("pn")] <- "positive"
  }
  else if (eat[i, c("emotion")] != "joy"){
    eat[i, c("pn")] <- "negative"
  }
}

eat$pn <- as.factor(eat$pn)

summary(eat[eat$emotion=="sad",])
sd(eat[eat$emotion=="sad","dominance"])

df_eat <- eat[,c("id","emotion","valence","arousal","dominance")]



# 3-d AVD model

fig <- plot_ly(df_eat, x = ~valence, y = ~arousal, z = ~dominance,
        mode="markers", 
        symbol = ~emotion, symbols = c('square', 'cross', 'circle'), 
        color = ~emotion, colors = c('#fb4f4f', '#fbc93d', '#6cc0e5')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = '<b> Valence </b>',
                                   titlefont = list(size = 20),
                                   tickfont = list(size = 15)),
                      yaxis = list(title = '<b> Arousal </b>',
                                   titlefont = list(size = 20),
                                   tickfont = list(size = 15)),
                      zaxis = list(title = '<b> Dominance </b>',
                                   titlefont = list(size = 20),
                                   tickfont = list(size = 15))),
        legend = list(title=list(text='<b> Emotion </b>',
                                 font = list(size = 16))))

fig


