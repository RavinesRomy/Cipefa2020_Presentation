
train<-readRDS(file = "~/GitHub/Cipefa_2020/Siniestros_MixDir/output/datasets/textos_train_21.rds")
train_tidy = readRDS(file = "~/GitHub/Cipefa_2020/Siniestros_MixDir/output/datasets/textos_tidy_21.rds")


#--------------------------------------------------------------
# Wordcloud
#--------------------------------------------------------------

library(wordcloud2)


df_token = train_tidy %>% group_by(token) %>% count(token)
df_grouped <- train_tidy %>% count(token) %>%
  mutate(frecuencia = n / n()) %>%
  arrange(desc(frecuencia)) # %>% nest()
demoFreq=data.frame(word = df_grouped$token, freq = df_grouped$frecuencia)

wc1<-wordcloud2(demoFreq[which(demoFreq$word != "vehiculo"),])
wc1
htmlwidgets::saveWidget(wc1,'wc1.html',selfcontained = F)

wc2 <- wordcloud2(demoFreq, size=1.5, color='random-dark') #cuidado que no muestra vehículo
wc2
htmlwidgets::saveWidget(wc2,'wc2.html',selfcontained = F)
#```



#```{r textos-descriptive, include=FALSE}


#--------------------------------------------------------------
# Palabras distintas utilizadas
#--------------------------------------------------------------

train_tidy %>%
  select(grupo, token) %>%
  distinct() %>%
  group_by(grupo) %>%
  summarise(palabras_distintas = n())

# Longitud media de los textos

train_tidy %>%
  group_by(id) %>%
  summarise(longitud = n()) %>%
  summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud))

library(plotly)
#library(plyr)
#detach(package:plyr)

data <- train_tidy %>% 
  filter(!grupo == "otros") %>%
  group_by(grupo, id) %>%
    summarise(longitud = 
             n()) %>%
  group_by(grupo) %>%
  summarise(mean_words = mean(longitud), sd_words = sd(longitud))


data <- as.data.frame(data)
data$grupo <- as.factor(data$grupo)

fig <- plot_ly(data = data, 
               y = ~grupo, 
               x = ~mean_words, 
               type = 'bar', 
               orientation = 'h',
               name = 'n_words',
               error_x = ~list(array = sd_words,
                               color = '#C19434'),
               marker = list(color = '#E9E7DA',
                             line = list(color = '#41541e',
                                         width = 3)))
fig <- fig  %>%    
  layout(title = list(text = "Textos Cortos",pad = list(b = 90, l = 130, r = 50 ))) %>%
  layout(xaxis = list(title = "Longitud = Número de Palabras"),
         yaxis = list(title = " ")) %>%
  layout(plot_bgcolor='#F1F1EF')

#%>%  layout(paper_bgcolor='#F1F1EF')

fig

library(htmlwidgets)
saveWidget(fig, "n_words.html", selfcontained = F)


#--------------------------------------------------------------
# Correlación entre usuarios por palabras utilizadas
#--------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)

#grupo_in_muestra = c("columna","granizo", "lluvia", "lunas", "roce", "robo","semaforo","stop")

train_spread <- train_tidy %>% group_by(grupo, token) %>% count(token) %>%
  spread(key = grupo, value = n, fill = NA, drop = TRUE)

p1 <- ggplot(train_spread, aes(columna, granizo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "#C19434") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(plot.background = element_rect(fill = "#F1F1EF"))

p2 <- ggplot(train_spread, aes(stop, semaforo)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "#C19434") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.background = element_rect(fill = "#F1F1EF"))

grid.arrange(p1, p2, nrow = 1)


# ggsave("imgs/mtcars.png", width = 700, height = 450, units = "px")


png("imgs/textos_correla01.png", width = 700, height = 450, units = "px")
print(grid.arrange(p1, p2, nrow = 1))
dev.off()

png("imgs/textos_correla01.png", width = 894, height = 760, units = "px")
print(grid.arrange(p1, p2, nrow = 2))
dev.off()

#--------------------------------------------------------------
# Palabras más utilizadas por grupo
#--------------------------------------------------------------

train_tidy %>% group_by(id) %>% count(token) %>%
  top_n(10, n) %>% arrange(desc(n)) %>% print(n=30)

train_tidy %>%
  group_by(grupo, token) %>%
  count(token) %>%
  group_by(grupo) %>%
  top_n(10, n) %>%
  arrange(grupo, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = grupo)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~grupo,scales = "free", ncol = 3, drop = TRUE)



#```





#--------------------------------------------------------------
# Networks and Visualization
#--------------------------------------------------------------
library(ngram)
library(tm)

# Text Mining
library(tidytext)

# Network Analysis
library(igraph)
library(ggraph)
# Data Wrangling and Visualization
library(glue)

bi.gram.words <- train %>%
  select(texto) %>%
  unnest_tokens(
    input = texto,
    output = bigram,
    token = 'ngrams',
    n = 2
  ) %>%
  filter(! is.na(bigram))


bi.gram.words %>%
  select(bigram) %>%
  head(10)

#filter for stop words and remove white spaces.
lista_stopwords <-tm::stopwords(kind = "es")

bi.gram.words %<>%
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>%
  filter(! word1 %in% lista_stopwords) %>%
  filter(! word2 %in% lista_stopwords) %>%
  filter(! is.na(word1)) %>%
  filter(! is.na(word2))

bi.gram.count <- bi.gram.words %>%
  count(word1, word2, sort = TRUE) %>%
  # We rename the weight column so that the
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count %>% head()

# Weight Distribution
bi.gram.count %>%
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

# Network representation

# For visualization purposes we scale by a global factor.
ScaleWeight <- function(x, lambda) {
  x / lambda
}


threshold <- 20

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>%
  graph_from_data_frame(directed = FALSE)

#summary(network)
#is.weighted(network)

aa<- plot(
  network,
  vertex.size = 1,
  vertex.label.color = '#41541e',
  vertex.label.cex = 1.2,
  vertex.label.dist = 1,
  edge.color = 'gray',
  #main = 'Bigram Count Network',
  #sub = glue('Weight Threshold: {threshold}'),
  alpha = 50
)

png("imgs/textos_bigram.png", width = 894, height = 760, units = "px",  bg = "white")
plot(
  network,
  vertex.size = 3,
  vertex.label.color = '#41541e',
  vertex.label.cex = 1.5,
  vertex.label.dist = 1,
  edge.color = 'gray',
  #main = 'Bigram Count Network',
  #sub = glue('Weight Threshold: {threshold}'),
  alpha = 50
)
dev.off()


###########################
  
devtools::install_github("briatte/ggnet")
library(ggnet)
install.packages("intergraph")
library(GGally)


zz <- bi.gram.count %>%  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3))
head(zz)
ggnet2(zz)


library(network)
library(sna)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)
str(net)

ggnet(network)


library(network)
library(sna)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)

###########################

#---------------
# Gracias
#---------------

datos=data.frame(
word=c(rep("Gracias",50)),
freq=c(rpois(50,1))
)

head(demoFreqC)
demoFreqGracias=demoFreq
demoFreqGracias$word="Gracias"

wordcloud2:letterCloud( datos, word = "R", color='random-light' , backgroundColor="black")

letterCloud(datos, word = "Cipefa", wordSize = 1)



wc_gracias<-wordcloud2::wordcloud2(demoFreqGracias, 
                       shape = "circle", 
                       size = 1.5,  
                       color=rep_len( c("#41541e","#C19434","#903163","#4d1434"), 
                                      nrow(demoFreqGracias) ),
                       backgroundColor = "#E9E7DA")

wc_gracias
htmlwidgets::saveWidget(wc_gracias,'~/GitHub/Cipefa_2020/Presentation/charts/wc_gracias.html',selfcontained = F)


figPath = system.file(file="~/GitHub/Cipefa_2020/Presentation/imgs/mask.png",package = "wordcloud2")
wordcloud2(datos, figPath = figPath, size = 1.5,color = "skyblue")
figPath = "~/GitHub/Cipefa_2020/Presentation/imgs/mask3.png"

