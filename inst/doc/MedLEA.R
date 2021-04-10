## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MedLEA)

## ----example3, warning=FALSE, message=FALSE-----------------------------------
library(tidyverse)
library(wordcloud2)
library(patchwork)
library(tm)

#unique(medlea$Family_Name)

text1 <- medlea$Family_Name
docs <- Corpus(VectorSource(text1))
docs <- docs%>% tm_map(stripWhitespace)
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
p1 <- wordcloud2(data = df, size = 0.9,color = 'random-dark', shape = 'pentagon')
p1

## ----example6, warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE-----------
#  d11 <- as.data.frame(table(medlea$Arrangements))
#  names(d11) <- c('Arrangements_of_the_leaf', 'No_of_leaves')
#  d11 <- d11 %>% mutate(Percentage = round(No_of_leaves*100/sum(No_of_leaves),2))
#  
#  ggplot(d11, aes(x= reorder(Arrangements_of_the_leaf, Percentage), y=Percentage)) + labs(y="Percentage", x="Arrangements of the leaf") + geom_bar(stat = "identity", width = 0.3) + ggtitle("Composition of sample by the Leaf Arrangement")+ geom_label(aes(label = paste0(Percentage, "%")), nudge_y = -6, size = 4, label.padding = unit(0.175,"lines")) + coord_flip()

## ----example2, warning=FALSE, message=FALSE-----------------------------------
medlea <- filter(medlea, Arrangements == "Simple")

d11 <- as.data.frame(table(medlea$Shape))
names(d11) <- c('Shape_of_the_leaf', 'No_of_leaves')

p2 <- ggplot(d11, aes(x= reorder(Shape_of_the_leaf, No_of_leaves), y=No_of_leaves)) + labs(y="Number of leaves", x="Shape of the leaf") + geom_bar(stat = "identity", width = 0.6) + ggtitle("Composition of the Sample by the Shape Label") + coord_flip()


## ----example5, warning=FALSE, message=FALSE-----------------------------------
d11 <- as.data.frame(table(medlea$Edges))
names(d11) <- c('Edges', 'No_of_leaves')
#d11 <- d11 %>% mutate(Percentage = round(No_of_leaves*100/sum(No_of_leaves),0))
#ggplot(d11, aes(x= reorder(Shape_of_the_leaf, Percentage), y=Percentage)) + labs(y="Percentage", x="Shape of the leaf") + geom_bar(stat = "identity", width = 0.5) + geom_label(aes(label = paste0(Percentage, "%")), nudge_y = -3, size = 3.25, label.padding = unit(0.175,"lines")) + ggtitle("Composition of the Sample by the Shape Label") + coord_flip()

p3 <- ggplot(d11, aes(x= reorder(Edges, No_of_leaves), y=No_of_leaves)) + labs(y="Number of leaves", x="Edge type of the leaf") + geom_bar(stat = "identity", width = 0.6) + ggtitle("Composition of the Sample by the Edge Type") + coord_flip()

p2 + p3 + plot_layout(ncol = 1)

## ----example4, warning=FALSE, message=FALSE-----------------------------------
medlea <- filter(medlea, Shape != "Scale-like shaped")

d29 <- as.data.frame(table(medlea$Shape,medlea$Edges))
names(d29) <- c('Shape','Edges','No_of_leaves')


ggplot(d29, aes(fill = Edges, x=Shape , y=No_of_leaves)) + labs(y="Number of leaves", x="Shape of the leaf") + geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + coord_flip() + ggtitle("Composition of the sample by Shape Label and Edge type") + scale_fill_brewer(palette = "Set1") + coord_flip() 

