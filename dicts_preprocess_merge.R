{
library(stringr)    # title case
library(tidyverse)  # piping and plotting with ggplot2
library(cluster)    # clustering
library(ggcorrplot) # visualising the intercorrelations
library(ggrepel)    # prevent labels overlapping in ggplots
library(clValid)    # cluster validation
library(RankAggreg) # rank aggregation for evaluation results
}
source("functions/rescale_function.R")    # custom rescale function
source("functions/dend_custom_function.R")# custom dendorgram function

# set ggplot theme globally
theme_set(theme_minimal())

# de-activate scientific notation
options(scipen = 999)


# Loading the dictionaries ----

# setwd("/Users/tinchen/Documents/DYSEN/git/dysen-processing/dictionaries/01_merging/dictionaries")
# 
# # create object with file names you want imported
# {
#   datasets <- list.files(pattern = "txt$")
# 
# # extract file names without extension to name the data frames in the loop
# dataset_names <- sub(x = datasets, pattern = "[.]txt", replacement = "")
# 
# # loop to read in the data and fill the list
# for (i in 1:length(datasets)){
#   df <- read.delim(datasets[i], stringsAsFactors = FALSE)
#   x <- dataset_names[i]
#   assign(x = x, value = df)
# }
# 
# }
# ## Load the tsv files (German Polarity Clues) ----
# 
# # create object with file names you want imported
# {
# datasets2 <- list.files(pattern = "tsv$")
# 
# # extract file names without extension to name the data frames in the loop
# dataset_names2 <- sub(x = datasets2, pattern = "[.]tsv", replacement = "")
# 
# 
# # loop to read in the data and fill the list
# for (i in 1:length(datasets2)){
#   df <- read.delim(datasets2[i], 
#                    stringsAsFactors = FALSE, 
#                    header = FALSE)
#   x <- dataset_names2[i]
#   assign(x = x, value = df)
# }
# }
# 
# 
# 
# angst <- read.csv(file = "/Users/tinchen/Documents/DYSEN/git/dysen-processing/dictionaries/01_merging/dictionaries/ANGST.csv", 
#                   dec = ".", 
#                   stringsAsFactors = FALSE, 
#                   encoding = "UTF-8")
# bawlr <- read.csv(file = "BAWL-R.csv", 
#                  sep = ",", 
#                  dec = ".", 
#                  header = TRUE, 
#                  stringsAsFactors = FALSE)
# 
# wordnorms <- read.csv("Wordnorms.csv")
# setwd("/Users/tinchen/Documents/DYSEN/git/dysen-processing/dictionaries")



# Preprocessing the dictionaries ----


## Affective Dictionary (Schröder, 2011) ----
affdict <- read.csv(file = "input_data/AffDict.csv", header = TRUE, stringsAsFactors = FALSE)

head(affdict)
affdict <- affdict[, c("German.Word", "E.unisex")] # evaluation of both sexes

affdict$German.Word <-  gsub(x = affdict$German.Word, pattern = "_", replacement = " ")
affdict <- affdict[!is.na(affdict$E.unisex), ] # remove empty rows on bottom of document

summary(affdict) # scaled [-4, 4]

names(affdict)[names(affdict) == "German.Word"] <- "Word"
names(affdict)[names(affdict) == "E.unisex"] <- "AffDict"

# remove placeholders 
affdict$Word <- gsub(affdict$Word, pattern = " [(]etwas[)]", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern =  " von", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " mit", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " bei", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " gegen", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " nach", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " auf", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " um", replacement = "")
affdict$Word <- gsub(affdict$Word, pattern = " ", replacement = "")
# NOTE: Data cleaned by hand: all ae etc. very replaced by ä etc. manually in the csv file


## Affective Meaning (Schröder, 2011) ----
affmeaning <- read.csv(file = "input_data/AffMeaning.csv", header = TRUE, stringsAsFactors = FALSE)

head(affmeaning)
affmeaning <- affmeaning[, c("wort", "mean_eva")] # evaluation (good-bad)
affmeaning <- affmeaning[!is.na(affmeaning$mean_eva), ] # at the end of the document there are some empty rows in the file
summary(affmeaning) # sentiment values scaled [1, 8]

names(affmeaning)[names(affmeaning) == "wort"] <- "Word"
names(affmeaning)[names(affmeaning) == "mean_eva"] <- "AffMeaning"
head(affmeaning)

# remove placeholders to fit the other dictionaries
affmeaning$Word <- gsub(affmeaning$Word, pattern = "jmd[.] ", replacement = "")
affmeaning <- affmeaning[affmeaning$Word != "Ich selbst, wie ich wirklich bin", ]


# last row contains only a rating, but no word -> remove it
affmeaning[nrow(affmeaning), ]
affmeaning <- affmeaning[-nrow(affmeaning), ]
tail(affmeaning)



## Affective Norms (Schulte im Walde, 350 000 words) ----
affnorms <- read.table(file = "input_data/AffNorms.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

head(affnorms) # middle 3 columns are not needed
affnorms <- affNorms[c(1, 5)] # select Word and Sentiment column

names(affnorms)[names(affnorms) =="Sentiment_score"] <- "AffNorms" # rename sentiment column

affnorms <- affnorms[ , c("Word", "AffNorms")]

affnorms <- affnorms[!is.na(affnorms$Word), ]
any(is.na(affnorms$Word))

# no duplicate words
n_occur <- data.frame(table(affnorms$Word))
n_occur[n_occur$Freq > 1, ]


## Affective Norms for German Sentiment Terms (ANGST) ----
angst <- read.csv("input_data/ANGST.csv", header = TRUE, stringsAsFactors = FALSE)
head(angst)

angst <- angst[, c("G.word", "VAL_Mean")] # select relevant columns
summary(angst)

names(angst)[names(angst) == "VAL_Mean"] <- "ANGST" # give more clear name for the merging
names(angst)[names(angst) == "G.word"]   <- "Word"

# get words that occur more than once using the table() function
n_occur <- data.frame(table(angst$Word))
dups <- n_occur[n_occur$Freq > 1,]

# remove factor
dups <- as.vector.factor(dups$Var1)

# subset to check out duplicates
angst[angst$Word %in% dups, ] 
# the duplicate words have in fact the same sentiment value, so we can just remove them

angst <- unique(angst)



## Austrian Language Plarity for Sentiment Anaylsis (alpin) ----
alpin <- read.table(file = "input_data/ALPIN.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

str(alpin)
# rename columns
names(alpin)[names(alpin) == "D"] <- "ALPIN"
names(alpin)[names(alpin) == "word"] <- "Word"
# drop columns with PoS tag:
alpin <- alpin[ , c("Word", "ALPIN")]

# 94 entries have a vertical bar plus plural form
alpin[grepl(x = alpin$Word, pattern = "\\|"), ] 
# keep only the word that comes before the vertical bar
alpin$Word <- gsub(x = alpin$Word, pattern = "\\|*", replacement = "")

# 70 entries are abbreviations or website links 
alpin[!grepl(x = alpin$Word, pattern = "\\."), ] 
# remove entries containing abbreviations and website links
alpin <- alpin[!grepl(x = alpin$Word, pattern = "\\."), ]

# 4 identical entries: @card@
alpin[grepl(x = alpin$Word, pattern = "@"), ] 
# seems like gibberish, so they are removed:
alpin <- alpin[!grepl(x = alpin$Word, pattern = "@"), ] 
  
# 124 entries contain only numbers
alpin[grepl(x = alpin$Word, pattern = "\\d$"), ]
# remove those
alpin <- alpin[!grepl(x = alpin$Word, pattern = "\\d$"), ]  

# frequency table to identify duplicate words
word_frq_alpin <- table(alpin$Word)
dup_words_alpin <- word_frq_alpin[word_frq_alpin > 1]

# 618 duplicated entries
length(dup_words_alpin) 

# vector containing the dupliates
dup_words_alpin <- names(dup_words_alpin)

# get the row indices for the duplicate words in alpin by checking alpin against the vector containing the duplicates:
dup_row_ind <- lapply(dup_words_alpin, function(x){
  which(alpin$Word %in% x)
})

# average the sentiment values of the rows that contain duplicates
dups_avg_alpin <- lapply(dup_row_ind, function(x){
  mean(alpin$ALPIN[x])
})

# bind the words and the averaged sentiment values
dups_replacement_alpin <- data.frame(Word = dup_words_alpin, 
                               ALPIN = unlist(dups_avg_alpin))
nrow(dups_replacement_alpin)


# remove the duplicate rows
alpin <- alpin[-unlist(dup_row_ind), ] 

# replace them with averaged sentiment values
alpin <- rbind(alpin, dups_replacement_alpin) # 14 005 words

nrow(alpin)


## Berlin Affective Word List Reloaded (BAWL-R) ----
bawlr <- read.csv(file = "input_data/BAWL-R.csv", header = TRUE, stringsAsFactors = FALSE)

str(bawlr) # The words are either all caps or all lower case
# Therefore: Take the lower case column and put every word to upper case if WORDCLASS == N
library(stringr)
bawlr$Word <- vector(mode = "character", length = nrow(bawlr))
for (i in 1:nrow(bawlr)){
  if (bawlr$WORD_CLASS[i] == "N") {
    bawlr$Word[i] <- str_to_title(bawlr$WORD_LOWER[i])
  } else if (bawlr$WORD_CLASS[i] != "N") {
    bawlr$Word[i] <- bawlr$WORD_LOWER[i]
  }
}
bawlr <- bawlr[ , c("Word", "EMO_MEAN")] # now select the relevant columns

summary(bawlr$EMO_MEAN) # scaled [-3, 3]

names(bawlr)[names(bawlr) == "EMO_MEAN"] <-  "BAWL-R"
head(bawlr)




## Leipzig Affective Norms German (LANG) ----
lang <- read.table(file = "input_data/LANG.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

lang <- lang[c("word", "valence_mean")]
head(lang)

# Problem: LANG ist ISO-8859-1 encoded, not UTF-8.
lang$word <- iconv(x = lang$word, from = "ISO_8859-1", to = "UTF-8") # fix encoding

names(lang)[names(lang) == "word"] <- "Word" # rename columns
names(lang)[names(lang) == "valence_mean"] <- "LANG"

lang$LANG <- as.numeric(lang$LANG) # valence column needs to be converted to numeric
summary(lang$LANG) # done!


## Polart Lexicon ----
polart <- read.table(file = "input_data/Polart.txt", header = TRUE, stringsAsFactors = FALSE, sep = " ")

head(polart)
summary(polart)
is.character(polart$Sentiment_cat_score)

temp5 <- strsplit(x = polart$Sentiment_cat_score, split = "=") # split column into sentiment label and score
mat5 <- do.call(rbind, temp5) # bind split columns into matrix
polart$label <-  mat5[ ,1] # subset matrix to extract labels
polart$score <-  mat5[ ,2] # subset matrix to extract scores

polart$Sentiment_cat_score <- NULL # remove old column

# Converting to numerical introduces NAs. It gets clear why when we take a closer look:
polart[polart$Word == "stöhnen", ]
polart[polart$Word == "dickhäutig", ]
# The sentiment scores are supposed to be 0.7 and 0.5, but there are some additional characters that shouldn't be there

# Replace them:
polart$score[polart$Word == "stöhnen"] <- 0.7
polart$score[polart$Word == "dickhäutig"] <- 0.5

# Convert column to numeric:
polart$score <- as.numeric(polart$score)
sum(is.na(polart$score)) # no more NAs 


summary(polart) # min and max much too high / low
polart[polart$score == -7, ] # should be - 0.7
polart[polart$score == -7, ] <- - 0.7
polart[polart$score ==  5, ] # should be - 0.5
polart[polart$score ==  5, ] <- - 0.5
polart[polart$score ==  7, ] <- - 0.7
polart[polart$score ==  3, ] # INT = Intensifiers
polart[polart$score ==  3, ] <- 0 # Intensifiers are set to 0
polart[polart$score ==  2, ]

# For loop to assign a negative value to sentiment scores with NEG label and to assign intensifiers, shifters and neutral words the value 0
for (i in 1:nrow(polart)){ 
  if (polart$label[i] == "NEG") { 
    polart$score[i] <- polart$score[i] * (- 1)
  } else if (polart$label[i] %in% c("NEU", "INT", "SHI")) {
    polart$score[i] <- 0
  }
}

summary(polart) # values looking neat now, no outliers

polart$label <- NULL # this column  is not needed anymore
polart$Wordtype <- NULL
polart[c(3936, 7518), ] # these two rows contins numbers, not words
polart <- polart[-c(3936, 7518), ] # remove them

polart$Word<- gsub(polart$Word, pattern = ":", replacement = "")
names(polart)[names(polart) == "score"] <- "Polart"
head(polart)

polart$Word <- gsub(polart$Word, pattern = "_", replacement = " ")



## Sentimerge ----
sentimerge <- read.delim(file= "input_data/Sentimerge.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")
  
str(sentimerge)
summary(sentimerge)
head(sentimerge$lemma, 20)

# Put nouns to uppercase using the stringr package:
Word <- vector(mode = "character", length = nrow(sentimerge))
for (i in 1:nrow(sentimerge)){
  if (sentimerge$PoS[i] == "N") {
    Word[i] <- str_to_title(sentimerge$lemma[i])
  } else if (sentimerge$PoS[i] != "N") {
    Word[i] <- sentimerge$lemma[i]
  }
}

# replace old all-lowercase word column with the new one from the loop: 
sentimerge$Word <- Word
sentimerge$lemma <- NULL 

# There are a lot of nouns that have XY as PoS code, not N:
nrow(sentimerge[sentimerge$PoS == "XY", ])
# View(sentimerge[sentimerge$PoS == "XY", ])
# They will create unnecessary rows, but there is nothing to be done about it (unless changing the entries manually, and it's too many for that)


# some words have several entries because of different PoS tags:
sentimerge[sentimerge$Word == "absolut", ]
sentimerge[sentimerge$PoS == "ADJA", ] # 733 adjectives labelled ADJA
sentimerge[sentimerge$PoS == "AJ", ] # 13 652 adjectives labelled AJ

# drop unneeded columns before taking care of the double entries
sentimerge$PoS <- NULL
sentimerge$weight <- NULL

# order dictionary alphabetically 
word_order <- order(sentimerge$Word)
sentimerge <- sentimerge[word_order, ]

# frequency table to identify duplicate words
word_frq <- table(sentimerge$Word)
nrow(dup_words_tab <- word_frq[word_frq > 1])
length(dup_words_tab) # 1805 have two or more entries

# vector containing the dupliates
dup_words <- names(dup_words_tab)

# get the row indices for the duplicate words in sentimerge by checking sentimerge against the vector containing the duplicates:
dup_rowind <- lapply(dup_words, function(x){
  which(sentimerge$Word %in% x)
})

# average the sentiment values of the rows that contain duplicates
dups_avg <- lapply(dup_rowind, function(x){
  mean(sentimerge$sentiment[x])
})

# bind the words and the averaged sentiment values
dups_replacement <- data.frame(Word = dup_words, 
                               sentiment = unlist(dups_avg))


# remove the duplicate rows from sentimerge
sentimerge <- sentimerge[-unlist(dup_rowind), ] # previously 99701 rows, now 96 023

# replace them with averaged sentiment values
sentimerge <- rbind(sentimerge, dups_replacement) # 97 828 rows

# as mentioned before, double rows that were introduced by the mysterious "XY" part-of-speech tag were not affected by this and still persist.

# entries that are not a legit word:
sentimerge[sentimerge$Word == "bakelit&amp;#174;", ] 
sentimerge[sentimerge$Word == "abb&eacute;-neuber-lippenplastik", ]
sentimerge[grepl(x = sentimerge$Word, pattern = "In[+]Akk[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "wiss[.]:"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Ugs[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Form[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Sl[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Norddt[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Obs[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Österr[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = ";"), ] # heavy gibberish


# remove the rows containing gibberish
sentimerge <- sentimerge[sentimerge$Word != "bakelit&amp;#174;", ]
sentimerge <- sentimerge[sentimerge$Word != "abb&eacute;-neuber-lippenplastik", ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "In[+]Akk[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Sich[+]Akk[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Teespenpl[.]:"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "wiss[.]:"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Ugs[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Form[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Sl[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Norddt[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Obs[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Österr[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Abk[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = ";"), ]



# Entries that contain abbreviations:
sentimerge[grepl(x = sentimerge$Word, pattern = "jmd"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "etw[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Etw[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Pej[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Dekofachspr[.]:"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "Abk[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "[.]"), ]
sentimerge[grepl(x = sentimerge$Word, pattern = "[.][.][.]"), ]
# Remove rows with abbreviations:
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "jmd"), ]
sentimerge$Word[sentimerge$Word == "etw.fantasieren"] <- "fantasieren"
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "etw[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Etw[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Pej[.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "Dekofachspr[.]:"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "[.][.][.]"), ]
sentimerge <- sentimerge[!grepl(x = sentimerge$Word, pattern = "[.]"), ]

names(sentimerge)[names(sentimerge) == "sentiment"] <- "SentiMerge"

summary(sentimerge$SentiMerge)
tail(sentimerge)
nrow(sentimerge) # 97 188 entries after cleaning

## SentiWS ----
sentiws_pos <- read.delim(file = "input_data/SentiWS_Positive.txt", header = FALSE, stringsAsFactors = FALSE, sep = "\t")

sentiws_neg <- read.delim(file = "input_data/SentiWS_Negative.txt", header = FALSE, stringsAsFactors = FALSE)
head(sentiws_neg)

# First, clean up the positive entries
head(sentiws_pos)
str(sentiws_pos)

names(sentiws_pos)[names(sentiws_pos) == "V1"] <-  "Word"
names(sentiws_pos)[names(sentiws_pos) == "V2"] <- "SentiWS" # rename sentiment score column
sentiws_pos$V3 <- NULL # inflections not needed

# remove the part-of-speech tags          
sentiws_pos$Word <- gsub(x = sentiws_pos$Word, replacement = "", pattern = "[|]*[A-Z]{1,5}$")
grep(x = sentiws_pos$Word, pattern = "[|]*[A-Z]{1,5}$", value = TRUE)

head(sentiws_pos) # done!




# Next, the negative ones
names(sentiws_neg)[names(sentiws_neg) == "V1"] <- "Word" # rename Word column
names(sentiws_neg)[names(sentiws_neg) == "V2"] <- "SentiWS" # rename Word column
sentiws_neg$V3 <- NULL # remove unneeded column

# remove the part-of-speech tags
sentiws_neg$Word <- gsub(x = sentiws_neg$Word, replacement = "", pattern = "[|]*[A-Z]{1,5}$") # clean up word column
 

# bind the two SentiWS dictionaries together:
sentiws <- rbind(sentiws_pos, sentiws_neg)

head(sentiws, 50) # looking good
str(sentiws) 

nrow(sentiws)


## Sentiment Phrase List (SePL) -----
sepl <- read.delim(file = "input_data/SePL.txt", dec = ".", sep = ";")

head(sepl, 20)
names(sepl)[names(sepl) =="phrase"] <- "Word" # rename the Word column

sepl <- sepl[ , c("Word", "opinion_value")]

names(sepl)[names(sepl) =="opinion_value"] <- "SePL"

head(sepl, 20) # done!
str(sepl)



## word norms ----
wordnorms <- read.csv(file = "input_data/Wordnorms.csv", header = TRUE, stringsAsFactors = FALSE)

head(wordnorms)

wordnorms <- wordnorms[ , c("Word", "Valence_M")] # select needed columns

sum(is.na(wordnorms$Valence_M)) # 5 missings in the sentiment score column
wordnorms[is.na(wordnorms$Valence_M), ] # these seem to be empty rows in the csv file
wordnorms <- wordnorms[!is.na(wordnorms$Valence_M), ] # remove rows with NA

summary(wordnorms$Valence_M) # scale [0, 5]


names(wordnorms)[names(wordnorms) == "Valence_M"] <- "WordNorms"
summary(wordnorms)
head(wordnorms)


# Merge I ----

# Dictionaries with numerical values
numdict <- merge(affdict, affmeaning,      by = "Word", all = TRUE)
numdict <- merge(numdict, alpin,           by = "Word", all = TRUE)
numdict <- merge(numdict, angst,           by = "Word", all = TRUE)
numdict <- merge(numdict, bawlr,            by = "Word", all = TRUE)
numdict <- merge(numdict, lang,            by = "Word", all = TRUE)
numdict <- merge(numdict, wordnorms,       by = "Word", all = TRUE)
numdict <- merge(numdict, sentimerge,      by = "Word", all = TRUE)
numdict <- merge(numdict, sentiws,         by = "Word", all = TRUE)
numdict <- merge(numdict, sepl,            by = "Word", all = TRUE)
numdict <- merge(numdict, polart,          by = "Word", all = TRUE)
numdict <- merge(numdict, affnorms,        by = "Word", all = TRUE)
colnames(numdict)

# Rescale all the dictionaries to [-1, 1]
numdict[ ,-1] <- apply(X = numdict[ ,-1], MARGIN = 2, FUN = ReScale, first = -1, last = 1)
summary(numdict[ , -1])
str(numdict)


# get mean sentiment for positive words
allnums <- numdict[ , -1] # select all but the word column
# select all non-NA
bool <- apply(allnums, 2, is.na)
bool <- !bool
#select all non-NA
nona <- allnums[bool]

# select all positive NAs
impute.pos <- mean(nona[nona > 0]) 
# 0.1762399


# get mean sentiment for negative words
impute.neg <- mean(nona[nona < 0]) 
# -0.2281867



# Dictionaries with categorical sentiment labels

## German Emotion Dictionaries ----


### Anger ("Wut") ----
anger <- read.delim(file = "input_data/EmotionDict_Anger.txt", header = FALSE, stringsAsFactors = FALSE)

# name Word column
names(anger) <- "Word" 
# create sentiment values
sentiment <- rep(x = impute.neg, times = nrow(anger))
anger <- data.frame(Word = anger, 
                    Sentiment = sentiment)
head(anger)


### Contempt ("Verachtung") ----
contempt <- read.delim(file = "input_data/EmotionDict_Contempt.txt", header = FALSE, stringsAsFactors = FALSE)

names(contempt) <- "Word"
dim(contempt)
sentiment <- rep(x = impute.neg, times = nrow(contempt))
contempt <- data.frame(Word = contempt, 
                       Sentiment = sentiment)
head(contempt)


### Disgust ("Ekel") ----
disgust <- read.delim(file = "input_data/EmotionDict_Disgust.txt", header = FALSE, stringsAsFactors = FALSE)

names(disgust) <- "Word"
dim(disgust)
sentiment <- as.numeric(rep(x = impute.neg, times = nrow(disgust)))
disgust <- data.frame(Word = disgust, 
                      Sentiment = sentiment)
head(disgust)

### Enjoyment ("Freude") ----
enjoyment <- read.delim(file = "input_data/EmotionDict_Enjoyment.txt", header = FALSE, stringsAsFactors = FALSE)

names(enjoyment) <- "Word"
sentiment <- as.numeric(rep(x = impute.pos, times = nrow(enjoyment)))
enjoyment <- data.frame(Word = enjoyment, 
                        Sentiment = sentiment)
head(enjoyment)


### Fear ("Furcht") ----
fear <- read.delim(file = "input_data/EmotionDict_Fear.txt", header = FALSE, stringsAsFactors = FALSE)

names(fear) <- "Word"
dim(fear)
sentiment <- rep(x = impute.neg, times = nrow(fear))
fear <- data.frame(Word = fear, 
                   Sentiment = sentiment)
head(fear)


### Sadness ("Trauer") ----
sadness <- read.delim(file = "input_data/EmotionDict_Sadness.txt", header = FALSE, stringsAsFactors = FALSE)

names(sadness) <- "Word"
dim(sadness)
sentiment <- as.numeric(rep(x = impute.neg, times = nrow(sadness)))
sadness <- data.frame(Word = sadness, 
                      Sentiment = sentiment)
head(sadness)


## rbind the German Emotion Dictionaries ----
EmotionDict <-  rbind(anger, contempt, disgust, enjoyment, fear, sadness)
head(EmotionDict)

is.numeric(EmotionDict$Sentiment)
EmotionDict$Sentiment <- as.numeric(EmotionDict$Sentiment)
names(EmotionDict)[names(EmotionDict) == "Sentiment" ] <-  "EmotionDict"
str(EmotionDict)

summary(EmotionDict)
# There is a comma in one column that will cause problems later
# Remove it, because the lemma "doof" is one row above, so we don't need the duplicate
EmotionDict[EmotionDict$Word == "doof,doofe", ] 
EmotionDict <- EmotionDict[EmotionDict$Word != "doof,doofe", ]
str(EmotionDict)



## German Polarity Clues (GPC) ----
negGPC     <- read.delim(file = "input_data/PolarityClues_Negative.tsv", header = FALSE, stringsAsFactors = FALSE, sep = "\t") 

neutralGPC <- read.delim(file = "input_data/PolarityClues_Neutral.tsv", header = FALSE, stringsAsFactors = FALSE, sep = "\t")

posGPC     <- read.delim(file = "input_data/PolarityClues_Positive.tsv", header = FALSE, stringsAsFactors = FALSE, sep = "\t")

head(negGPC)
head(posGPC)
head(neutralGPC)
# posGPC and negGPC have the same structure and columns names -> rbind them
negposGPC <- rbind(negGPC, posGPC)

# rename the columns more clearly
names(negposGPC)[names(negposGPC) == "V1"] <- "Word"
names(negposGPC)[names(negposGPC) == "V4"] <- "PolarityClues"
negposGPC[c("V2","V3", "V5", "V6")]        <- NULL # remove columns that are not needed

head(neutralGPC, 20) # Drop the first 19 rows as they contain only numbers and symbols
neutralGPC <- neutralGPC[-c(1:19), ]

# renaming the columns more clearly
names(neutralGPC)[names(neutralGPC) == "V1"] <- "Word"
names(neutralGPC)[names(neutralGPC) == "V4"] <- "PolarityClues"

# drop columns that are not needed
neutralGPC[ ,c("V2", "V3", "V5", "V6")]  <- NULL 

# Now, negposGPC and neutralGPC have the same structure and column names. rbind them and do the rest of the data transformation on all of them at the same time:
PolClues <- rbind(negposGPC, neutralGPC)

# assign numerical values to sentiment labels
for (i in 1:nrow(PolClues)){
  if (PolClues$PolarityClues[i] == "negative") {
    PolClues$PolarityClues[i] <- impute.neg
  } else if (PolClues$PolarityClues[i] == "positive") {
    PolClues$PolarityClues[i] <- impute.pos
  } else if (PolClues$PolarityClues[i] == "neutral") {
    PolClues$PolarityClues[i] <- 0
  }
}

PolClues$PolarityClues <-  as.numeric(PolClues$PolarityClues)
head(PolClues)
summary(PolClues$PolarityClues)

# PolClues is ready!

## Morph Comp ----

### morph ----

morph <- read.delim(file = "input_data/Morph_Dev.txt", header = TRUE, stringsAsFactors = FALSE)

head(morph, 20)
names(morph)[names(morph) == "Sentiment_cat"] <- "Sentiment" # renaming the sentiment column

# remove part-of-speech tags
morph$Word <- gsub(x = morph$Word, pattern = "_[A-Z$]", replacement = "")


for (i in 1:nrow(morph)){
  if (morph$Sentiment[i] == "POS") { # map numerical values to sentiment labels
    morph$Sentiment[i] <- impute.pos
  } else if (morph$Sentiment[i] %in% c("NEU", "INT")){
    morph$Sentiment[i] <- 0
  } else if (morph$Sentiment[i] == "NEG"){
    morph$Sentiment[i] <- - impute.neg
  }
}
morph$Sentiment <-  as.numeric(morph$Sentiment) # convert to numeric

head(morph, 20)

### morph: Wiktionary data ----

wiki <- read.delim(file = "input_data/Morph_Wiktionary.txt", header = TRUE, stringsAsFactors = FALSE)

head(wiki)
# rename sentiment column
names(wiki)[names(wiki) == "Sentiment_cat"] <-  "Sentiment" 

sum(wiki$Sentiment == "POS") # only 3 positive words

# remove the part-of-speech tags
wiki$Word <- gsub(x = wiki$Word, pattern = "_[A-Z]$", replacement = "")
head(wiki, 50)

for (i in 1:nrow(wiki)){
  if (wiki$Sentiment[i] == "POS"){ # map numerical values to labels
    wiki$Sentiment[i] <- impute.pos
  } else if (wiki$Sentiment[i] == "NEU"){
    wiki$Sentiment[i] <- 0
  } else if (wiki$Sentiment[i] == "NEG"){
    wiki$Sentiment[i] <-  impute.neg
  }
}

class(wiki$Sentiment)
wiki$Sentiment <-  as.numeric(wiki$Sentiment) 
is.numeric(wiki$Sentiment)

summary(wiki$Sentiment)
head(wiki)


### morph: Wortwarte data ----

wortw <- read.delim(file = "input_data/Morph_Wortwarte.txt")
head(wortw)

# rename sentiment column
names(wortw)[names(wortw) == "Sentiment_cat"] <- "Sentiment" 

# remove part-of-speech tags
wortw$Word <- gsub(x = wortw$Word, pattern = "_[A-Z]$", replacement = "")
head(wortw)

# for loop 
for (i in 1:nrow(wortw)){
  if (wortw$Sentiment[i] == "POS"){ # replace labels with num. values
    wortw$Sentiment[i] <- impute.pos
  } else if (wortw$Sentiment[i] == "NEU"){
    wortw$Sentiment[i] <- 0
  } else if (wortw$Sentiment[i] == "NEG"){
    wortw$Sentiment[i] <- impute.neg
  }
}

wortw$Sentiment <- as.numeric(wortw$Sentiment)
is.numeric(wortw$Sentiment)


### morph: Train-test data ----
ttdata <- read.delim(file = "input_data/Morph_Traintest.txt", header = TRUE, stringsAsFactors = FALSE)

head(ttdata, 50)


names(ttdata)[names(ttdata) == "Sentiment_cat"] <-  "Sentiment" # rename Sentiment col

# remove part-of-speech tags
ttdata$Word <- gsub(ttdata$Word, pattern = "_[A-Z]$", replacement = "")

# for loop 
for (i in 1:nrow(ttdata)){
  if (ttdata$Sentiment[i] == "POS"){ # replace labels with num. values
    ttdata$Sentiment[i] <- impute.pos
  } else if (ttdata$Sentiment[i] %in% c("NEU", "INT", "SHI")){
    ttdata$Sentiment[i] <- 0
  } else if (ttdata$Sentiment[i] == "NEG"){
    ttdata$Sentiment[i] <-  impute.neg
  }
}

ttdata$Sentiment <- as.numeric(ttdata$Sentiment)
is.numeric(ttdata$Sentiment)
head(ttdata)


# rbind the morph dictionaries:
morph <- rbind(morph, wiki, wortw, ttdata)
names(morph)[names(morph) == "Sentiment"] <- "Morph"






## Merge II ----
## Dictionaries wi hcategorical labels (now imputed with mean values from the numerical dictionaries)
alldict <- merge(numdict, EmotionDict, by = "Word", all = TRUE)
alldict <- merge(alldict, morph,       by = "Word", all = TRUE)
alldict <- merge(alldict, PolClues,    by = "Word", all = TRUE)

# Arrange columns alphabetically (= according to dictionary name)
alldict <- alldict[, c("Word", sort(setdiff(names(alldict), "Word")))]



# Get number of words in each dictionary
n_words <- apply(alldict[ ,-1], 2, function(x) sum(!is.na(x)))
n_words
mean(n_words)
median(n_words)


length(unique(alldict$Word)) == nrow(alldict)
length(unique(alldict$Word)) # 415 061 unique words
length(alldict$Word) # 415 545, so there are some duplicate entries

# duplicate rows
n_occur <- data.frame(table(alldict$Word))
nrow(n_occur[n_occur$Freq > 1, ]) # there are still 427 duplicate rows
alldict[n_occur$Var1[n_occur$Freq > 1], ]


# Count in how many dictionaries each word is
n_dictionaries <- rowSums(!is.na(alldict[ ,-1])) 


# Make subset dictionaries
in2dicts <- alldict[n_dictionaries >= 2, ] # 55 701 words appear in at least 2 dicts
in3dicts <- alldict[n_dictionaries >= 3, ] # 19 460 words appear in at least 3
in4dicts <- alldict[n_dictionaries >= 4, ] # 11 556 words appear in at least 4
in5dicts <- alldict[n_dictionaries >= 5, ] #  6 368 words appear in at least 5
in6dicts <- alldict[n_dictionaries >= 6, ] #  3 737 words appear in at least 6
in7dicts <- alldict[n_dictionaries >= 7, ] #  2 101 words appear in at least 7


## BEFORE adding the categorical dictionaries: 
numdict$mean_sentiment <- rowMeans(numdict[ ,-1], na.rm = TRUE)
pdf(file = "output_plots/Histogram_mean_sentiment.pdf")
ggplot(data = numdict, mapping = aes(x = mean_sentiment)) +
  geom_histogram(fill = "grey", col = "darkgrey")+
  theme_bw() +
  labs(title = "Histogram of mean sentiment per word",
       x = " Mean Sentiment",
       y = "Count")
dev.off()

## AFTER adding the categorical dictionaries with imputed values
alldict$mean_sentiment <- rowMeans(alldict[ ,-1], na.rm = TRUE)
pdf(file = "output_plots/Histogram_mean_sentiment.pdf")
ggplot(data = alldict, mapping = aes(x = mean_sentiment)) +
  geom_histogram(fill = "grey", col = "darkgrey")+
  theme_bw() +
  labs(title = "Histogram of Mean Sentiment per Word",
       x = " Mean Sentiment",
       y = "Count")+ 
  theme(plot.title = element_text(size = 14),
        axis.title = element_text(size = 12))

dev.off()
# mostly neutral words



## Write files ----

# SePl dictionary contains spaces in the word column. Thus, tab-separated file is out of the question, going for comma-separated instead.
any(grepl(alldict$Word, pattern = " "))
alldict$Word[grepl(alldict$Word, pattern = " ")]

# write txt file for merged dictionaries, with mean sentiment column:
write.table(alldict, file = "output_other/all_sentdictionaries.txt", sep = ",", col.names = TRUE, row.names = FALSE, na = "NA", quote = TRUE)

# write txt file for the words that appear in 2 or more dictionaries:
write.table(in2dicts, file = "output_other/in2ormoredicts.txt", sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)

# write txt file for words that appear in 3 or more dictionaries:
write.table(in3dicts, file = "output_other/in3ormoredicts.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)

# write txt file for words that appear in 4 or more dictionaries:
write.table(in4dicts, file = "output_other/in4ormoredicts.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)

# write txt file for words that appear in 5 or more dictionaries:
write.table(in5dicts, file = "output_other/in5ormoredicts.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)

# write txt file for words that appear in 6 or more dictionaries:
write.table(in6dicts, file = "output_other/in6ormoredicts.txt", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)




# Cluster ----                                             

## preparations and assumptions ----

# Features should have the same means and SDs
colMeans(alldict[ , -1], na.rm = TRUE)
apply(X = alldict[ , -1], FUN = sd, MARGIN = 2, na.rm = TRUE)
# should I normalise the SD? -> Andreas


# Look at correlations
# --------------------------------------------------
correlations <- cor(alldict[ ,-c(1, ncol(alldict))], use = "pairwise.complete.obs")
# Method "pairwise complete observations" had to be used here, as there are no  complete cases: There is at least one NA in each row. 



ggcorrplot(correlations, 
           type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_bw,
           title = "Correlations between sentiment dictionaries",
           colors = c("#6D9EC1", "white", "#E46726"),
           method = "circle",
           lab = TRUE,
           lab_size = 3)



# transpose dictionaries: put dictionaries in the rows for clustering
transposed.dicts <- t(alldict[ ,-c(1, ncol(alldict))])

## Find optimal parameters for clustering  ----
# Evaluate Ward
ward_eucl <- clValid(obj = transposed.dicts, 
                         nClust = c(2, 3, 4, 5),  
                         clMethods = c("pam", "agnes", "diana"), 
                         validation = "internal", 
                         metric = "euclidean", 
                         method = "ward")

summary(ward_eucl)
optimalScores(ward_eucl)

ranks <- getRanksWeights(ward_eucl)
RankAggreg(ranks$ranks, k = 3, weights = ranks$weights, seed = 123)
#  diana-3 diana-2 agnes-3


# evaluate average linkage 
al_eucl <- clValid(obj = transposed.dicts, 
                        nClust = c(2, 3, 4, 5),  
                        clMethods = c("pam", "agnes", "diana"), 
                        validation = "internal", 
                        metric = "euclidean", 
                        method = "average")
summary(al_eucl)
optimalScores(al_eucl)

ranks_al_eucl <- getRanksWeights(al_eucl)
RankAggreg(ranks_al_eucl$ranks, k = 3, weights = ranks_al_eucl$weights, seed = 456)
#  pam-2 agnes-3 diana-2



# evaluate complete linkage
cl_eucl <- clValid(obj = transposed.dicts, 
                        nClust = c(2, 3, 4, 5),  
                        clMethods = c("pam", "agnes", "diana"), 
                        validation = "internal", 
                        metric = "euclidean", 
                        method = "complete")
summary(cl_eucl)
optimalScores(cl_eucl)

ranks_cl_eucl <- getRanksWeights(cl_eucl)
RankAggreg(ranks_cl_eucl$ranks, k = 3, weights = ranks_cl_eucl$weights, seed = 789)
# diana-3 pam-2 diana-2

# results of weighted aggregated ramks:
# Diana with 3 clusters (two-times winner), pam with 2 clusters (one-time winner)

par(mfrow = c(1,1))



## Distances ----
eucl.dist <- dist(transposed.dicts, method = "euclidean")


## Clustering ----
diana.x <- diana(x = eucl.dist, metric = "euclidean")


## Plotting ----
### diana ----
pdf(file = "output_plots/diana3_banner.pdf")
plot(diana.x, main = "", which.plot = 1, max.strlen = 13, col = c(adjustcolor(col = c("cornflowerblue","lightgrey"), alpha.f = 0.6)))
title(main = "Banner Plot of Divisive Analysis Clustering")
dev.off()

pdf(file = "output_plots/diana3_dend_new.pdf")
fviz_dend_custom(diana.x, k = 3, rect = TRUE, rect_fill = TRUE,
          ggtheme = theme_minimal(),
          lwd = 0.5, k_colors = "black",
          labels_track_height = 120, rect_border = 3:5,
          type = "rectangle", offset_labels = -10,
          main = "Dendrogram of Divisive Analysis Clustering for k = 3",
          sub = "distance metric = euclidean")
dev.off()


