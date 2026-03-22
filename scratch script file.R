## scratch script file
## TEMP SCRATCH FILES FOR DRAWING DIAGRAMS

# Load required libraries
library(ggplot2)
library(ggrepel)

# Define coordinates for the two objects
object1 <- c(1, 2)
object2 <- c(6, 5)

# Calculate Euclidean distance
euclidean_distance <- sqrt(sum((object2 - object1)^2))
distance_formula <- paste("Euclidean Distance =", round(euclidean_distance, 2))


# Create data frame for plotting
df <- data.frame(x = c(object1[1], object2[1]),
                 y = c(object1[2], object2[2]),
                 label = c("Object 1", "Object 2"),
                 coords = c(paste("(", object1[1], ",", object1[2], ")"),
                            paste("(", object2[1], ",", object2[2], ")")))

## Euclidean distance graphic -- concept and formula
euclidean1<-ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 3) +
#  geom_label_repel(aes(label = label), size = 5, box.padding = 0.5) +
#  geom_text_repel(aes(label = coords), size = 3.5, box.padding = 0.1) +
  geom_segment(aes(xend = object2[1], yend = object1[2]), linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(xend = object1[1], yend = object1[2]), linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(x = object1[1], y = object1[2], xend = object2[1], yend = object2[2])) +
  xlim(0, 7) +
  ylim(0, 6)+
 # annotate("text", x = (object1[1] + object2[1])/2, y = (object1[2] + object2[2])/2,
 # label = distance_formula, fontface = "bold", size = 5, hjust = "left") +
  theme_minimal() +
  labs(title = "Euclidean Distance between two points, measured on two variables",
       subtitle="Panel A: concept",
       x = "X",
       y = "Y") +
annotate("text", x = 2.7, y = 4.2, label = expression(paste ("d = ", sqrt(({x[2]} - {x[1]})^2 + ({y[2]} - {y[1]})^2)))) +
annotate("text", x = 4, y = 1.7, label = expression({x[2]}-{x[1]})) +
annotate("text", x = 6.4, y = 4, label = expression({y[2]}-{y[1]})) +
annotate("text", x=.8, y=2.25, label=expression(paste( ,  , "(", ~ x[1], ",", ~ y[1], ")"))) +
annotate("text", x=6, y=5.25, label=expression(paste( ,  , "(", ~ x[2], ",", ~ y[2], ")")))


## Euclidean distance graphic -- specific example
euclidean2<-ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 3) +
  #  geom_label_repel(aes(label = label), size = 5, box.padding = 0.5) +
  #  geom_text_repel(aes(label = coords), size = 3.5, box.padding = 0.1) +
  geom_segment(aes(xend = object2[1], yend = object1[2]), linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(xend = object1[1], yend = object1[2]), linetype = "dashed", alpha = 0.5) +
  geom_segment(aes(x = object1[1], y = object1[2], xend = object2[1], yend = object2[2])) +
  xlim(0, 7) +
  ylim(0, 6)+
  # annotate("text", x = (object1[1] + object2[1])/2, y = (object1[2] + object2[2])/2,
  # label = distance_formula, fontface = "bold", size = 5, hjust = "left") +
  theme_minimal() +
  labs(subtitle="Panel B: example",
       x = "X",
       y = "Y") +
  annotate("text", x = 2.7, y = 4.2, label = expression(paste ("distance = ", sqrt(5^2 + 3^2), " = 5.83"))) +
  annotate("text", x = 4, y = 1.7, label = "6 - 1 = 5") +
  annotate("text", x = 6.4, y = 4, label = "5 - 2 = 3") +
  annotate("text", x=6, y=5.25, label="(6,5)") +
  annotate("text", x=1, y=1.7, label="(1,2)")

library(gridExtra)
grid.arrange(euclidean1, euclidean2, ncol = 1)



#  annotation_custom(img, xmin=3, xmax=4, ymin=4, ymax=5) +
#  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


inset_element(p = img,
              left = 0.5,
              bottom = 0.55,
              right = 0.95,
              top = 0.95)


install.packages("patchwork")       # Install patchwork package
library("patchwork")




library(png)
library(ggplot2)
library(grid)

# Load the image
img <- readPNG("./addfigures/tempfigures/euclideanformula.png", native=TRUE)

# Create a ggplot2 plot
plot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3)

# Add the image to the plot
plot + annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")))


knitr::write_bib(c("WDI"), "packages.bib", width = 60)

texts <- c(Fed10 = "If a faction consists of less than a majority relief is supplied by the republican principle republican principle",
          Fed51 = "In republican government a the than relief by legislative for for authority necessarily predominates",
          Fed78 = "The standard of good behavior good behavior good behavior  for the continuance by  in office by of the judicial magistracy")
corp <- corpus(texts)

# proportional term frquencies
dfm_texts_prop<-dfm_weight(dfm_texts, scheme = "prop")

# the TF*IDF statistic for each term
tfidf_texts_prop<-dfm_tfidf(dfm_texts, scheme_tf="prop")

dfm_texts<-dfm(corp)


as.data.frame(dfm_texts)
dfm_texts2 <- dfm_tfidf(dfm_texts)

as.data.frame(dfm_texts2)
dfm_texts_idf <- convert(dfm_texts2, to = "data.frame")

## IDF
dfm_texts_idf


# term frequency?
dfm_weight(dfm_texts, scheme = "count")

#### THESE ARE TERM FREQUENCIES
## a proportional count??
dfm_textsPROP<-dfm_weight(dfm_texts, scheme = "prop")

as.matrix(dfm_textsPROP)

############################################# QUANTEDA PART HERE #############################################
### TF-IDF for proportional or relative frequency, base 10
mat1default<-dfm_tfidf(dfm_texts, scheme_tf="prop", base=10)
mat1default<-as.matrix(mat1default)

mat1default
as.data.frame(dfm_texts)

###### PROPORTIONAL BUT BASE 2
mat1default<-dfm_tfidf(dfm_texts, scheme_tf="prop", base=2)
mat1default<-as.matrix(mat1default)

mat1default
as.data.frame(dfm_texts)


### TF-IDF for raw count, base 10
mat1default<-dfm_tfidf(dfm_texts, scheme_tf="count", base=10)
mat1default<-as.matrix(mat1default)

mat1default
as.data.frame(dfm_texts)



mat1default<-dfm_tfidf(dfm_texts, base=10)
mat1default<-as.matrix(mat1default)

mat1default
as.data.frame(dfm_texts)



# might need relative frequency here below
as.data.frame(dfm_texts)
rowSums(as.matrix(dfm_texts))

dfm_texts2*dfm_texts


mat1default<-dfm_tfidf(dfm_texts)
mat1default<-as.matrix(mat1default)

mat2<-dfm_tfidf(dfm_texts, scheme_tf = "count", scheme_df = "inverse")

mat2<-as.matrix(mat2)

mat2




library(tm)
# create a vector source
source <- VectorSource(texts)
# create a corpus
corpustm <- Corpus(source)

dtm_corpustm<-DocumentTermMatrix(corpustm)
dtm_corpustm
print(dtm_corpustm)


as.matrix(dtm_corpustm)

# 'normalized' tfidf, base2 logarithm
comparisontfidf<-weightTfIdf(dtm_corpustm)
comparisontfidfMAT<-as.matrix(comparisontfidf)

## TFIDF FOR TM PACKAGE.
comparisontfidfMAT

row.names(comparisontfidf)

head(sort(comparisontfidf[1, ], decreasing=TRUE), n=15)

## Here is the TFIDF from the TM package
as.matrix(comparisontfidf)




## scratch for FEC

# committee ID
BIDEN FOR PRESIDENT (C00703975)
