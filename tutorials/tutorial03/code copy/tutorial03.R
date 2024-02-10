##################################
# Tutorial 3: Textual Statistics #
##################################

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem", # an alternative method for lemmatizing
         "lubridate" # working with dates
), pkgTest)

## Read in our data from last time
ukr22 <- readRDS("df2022")
ukr23 <- readRDS("df2023")
ukr24 <- readRDS("df2024")

## 1. Revising last week's class: creating the corpus
#  By the end of last week's class we had processed our text and created 
#  three objects:
#     a) a corpus
#     b) a tokens list
#     c) a dfm (document-feature matrix)

# Let's do the same now, repeating the same steps for each dataframe. 

# First, we'll subset again on the kind of articles we're interested in:
ukr22 <- ukr22[ukr22$type == "article" & ukr22$section_id == "world",]
ukr23 <- ukr23[ukr23$type == "article" & ukr23$section_id == "world",]
ukr24 <- ukr24[ukr24$type == "article" & ukr24$section_id == "world",]

# First, we'll tidy up our initial dataframes.
tidy22 <- ukr22 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

tidy23 <- ukr23 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

tidy24 <- ukr24 %>%
  select(headline,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

# Next, we'll remove the initial (large) dataframes from memory
rm(ukr22)
rm(ukr23)
rm(ukr24)

# Now we have a more wieldy tibble. 
head(tidy22)
head(tidy23)
head(tidy24)

# Let's check for duplicates again:
which(duplicated(tidy22$headline))
which(duplicated(tidy23$headline))
which(duplicated(tidy24$headline))

# We can use the same code to drop duplicated headlines:
tidy22 <- tidy22[-which(duplicated(tidy22$headline)),]
tidy23 <- tidy23[-which(duplicated(tidy23$headline)),]
tidy24 <- tidy24[-which(duplicated(tidy24$headline)),]

# Let's also tidy the body_text column before we transform into a corpus
tidy22$body_text <- str_replace(tidy22$body_text, "\u2022.+$", "")
tidy23$body_text <- str_replace(tidy23$body_text, "\u2022.+$", "")
tidy24$body_text <- str_replace(tidy24$body_text, "\u2022.+$", "")

# Creating a corpus object
corp22 <- corpus(tidy22, 
                 docid_field = "headline",
                 text_field = "body_text")

corp23 <- corpus(tidy23, 
                 docid_field = "headline",
                 text_field = "body_text")

corp24 <- corpus(tidy24, 
                 docid_field = "headline",
                 text_field = "body_text")

# Creating a useful summary object of our corpus
corpSum22 <- summary(corp22, 
                     n = nrow(docvars(corp22)) #note: the default is n=100
) 

corpSum23 <- summary(corp23, 
                     n = nrow(docvars(corp23)) #note: the default is n=100
)

corpSum24 <- summary(corp24, 
                     n = nrow(docvars(corp24)) #note: the default is n=100
)


head(corpSum22[,-8])
head(corpSum23[,-8])
head(corpSum24[,-8])

## 2. Corpus statistics
#  We can use the corpus summary object to start creating statistical 
#  plots of our text data, for instance a histogram of articles over time:
corpSum22 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

corpSum23 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

corpSum24 %>%
  ggplot(aes(date)) +
  geom_histogram() # note, we can use geom_density() to give the pdf

# We can also plot these simultaneously using lubridate
ggplot(data = NULL) +
  geom_density(aes(yday(corpSum22$date)), color = "red") +
  geom_density(aes(yday(corpSum23$date)), color = "blue") +
  geom_density(aes(yday(corpSum24$date)), color = "green")
  
# We can calculate additional statistics using the summary object. 
# For example, the TTR is the ratio of types to tokens:
corpSum22$ttr <- corpSum22$Types / corpSum22$Tokens
corpSum23$ttr <- corpSum23$Types / corpSum23$Tokens
corpSum24$ttr <- corpSum24$Types / corpSum24$Tokens

#type / token = lexical complexity of the text.
#count different words check relation to total of words
#tkens = total number of words etc.

# We can plot this over time as well:
ggplot(data = NULL) +
  geom_point(aes(yday(corpSum22$date), corpSum22$ttr), col = "red") +
  geom_point(aes(yday(corpSum23$date), corpSum23$ttr), col = "blue") +
  geom_point(aes(yday(corpSum24$date), corpSum24$ttr), col = "green") +
  geom_smooth(aes(yday(corpSum22$date), corpSum22$ttr), col = "red") +
  geom_smooth(aes(yday(corpSum23$date), corpSum23$ttr), col = "blue") +
  geom_smooth(aes(yday(corpSum24$date), corpSum24$ttr), col = "green")
  
# How would you interpret this plot?

## Exercise: Readability
# "Readability" is a qualitative metric. A common scale is Flesch-Kincaid, 
# which estimates readability in terms of school grade (year). Try using 
# the function textstat_readability() to calculate the F-K scale and assign
# it as a new column to the corpus_sum object.
?textstat_readability

# Once you've done that, try plotting the F-K scale according to byline.
# Let's compare three correspondents: Luke Harding, Jennifer Rankin and
# Julian Borger. Look at the code below and fill in the necessary arguments.


#adding new column
corpSum22$fk <- textstat_readability(corp22, measure = "Flesch.Kincaid")
corpSum23$fk <- textstat_readability(corp23, measure = "Flesch.Kincaid")
corpSum24$fk <- textstat_readability(corp24, measure = "Flesch.Kincaid")

grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
      corpSum22$byline, ignore.case = TRUE)

corpSum22 %>%
  filter(grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
               byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Jennifer Rankin|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

corpSum23 %>%
  filter(grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
               byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Jennifer Rankin|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

corpSum24 %>%
  filter(grepl("Luke Harding|Jennifer Rankin|Julian Borger", 
               byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Jennifer Rankin|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")


# Try the same for 2023 and 2024 as well - what do we find? (Note: Jennifer Rankin
# is not currently writing for the Guardian in 2024)

## 3. Creating the tokens list and the dfm
# Let's move on to creating our other data objects: the tokens list and 
# the dfm. Here are the steps we followed last week:
toks22 <- quanteda::tokens(corp22, 
                         include_docvars = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

toks23 <- quanteda::tokens(corp23, 
                         include_docvars = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

toks24 <- quanteda::tokens(corp24, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

# Find collocations
colc22 <- textstat_collocations(toks22, size = 2, min_count = 10) 
colc23 <- textstat_collocations(toks23, size = 2, min_count = 10) 
colc24 <- textstat_collocations(toks24, size = 2, min_count = 10)

# This time, let's look at the z scores to see what cut-off to use
?textstat_collocations

toks22 <- tokens_compound(toks22, pattern = colc22[colc22$z > 17,])
toks23 <- tokens_compound(toks23, pattern = colc23[colc23$z > 17,])
toks24 <- tokens_compound(toks24, pattern = colc24[colc24$z > 17,])

# Remove whitespace
toks22 <- tokens_remove(quanteda::tokens(toks22), "") 
toks23 <- tokens_remove(quanteda::tokens(toks23), "") 
toks24 <- tokens_remove(quanteda::tokens(toks24), "") 

# Stem tokens?
toks22 <- tokens_wordstem(toks22)
toks23 <- tokens_wordstem(toks23)
toks24 <- tokens_wordstem(toks24)

dfm22 <- dfm(toks22)
dfm23 <- dfm(toks23)
dfm24 <- dfm(toks24)

topfeatures(dfm22)
topfeatures(dfm23)
topfeatures(dfm24)

super_stops <- c("said",
                 "say",
                 "also",
                 "ukrain*",
                 "russi*")

toks22 <- tokens_remove(toks22, super_stops,
                        valuetype = "glob")

toks23 <- tokens_remove(toks23, super_stops,
                        valuetype = "glob")

toks24 <- tokens_remove(toks24, super_stops,
                        valuetype = "glob")
# run again
dfm22 <- dfm(toks22)
dfm23 <- dfm(toks23)
dfm24 <- dfm(toks24)

# Removing extra stopwords
# Last week we used the topfeatures() function to check our dfm
# for features that should have been removed. Let's do that again
# this time: create a dfm for both tokens objects, then go back to
# remove the necessary stopwords.




#### 4. Statistics with the dfm 
# Let's compare the relative frequency of features from our three years.
dfm22_frq <- textstat_frequency(dfm22, n = 20)
dfm23_frq <- textstat_frequency(dfm23, n = 20)
dfm24_frq <- textstat_frequency(dfm24, n = 20)

dfm22_frq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")

dfm23_frq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")

dfm24_frq %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")

# We can also bind together our three dfms
dfm_ukr <- rbind(dfm22, dfm23, dfm24)

# This allows us to analyse and compare keyness across years
# 
set.seed(2023)
dfm_by_date <- dfm_group(dfm_ukr, fill = TRUE, groups = year(dfm_ukr$date))
keyness <- textstat_keyness(dfm_by_date, target = "2022")
textplot_keyness(keyness, labelsize = 2)

#reference compared to everything in the dfm

keyness <- textstat_keyness(dfm_by_date, target = "2024")
textplot_keyness(keyness, labelsize = 2)

# Try changing this code to compare keyness of 2024 against the previous two years

# Finally, let's see if sentiment analysis can detect a change in tone 
# over time. To do this, we need to use a dictionary object. We only
# want positive and negative sentiments, so we'll just use the first two 
# elements.
dfm_sentiment <- dfm_lookup(dfm_ukr, data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups = date)

# Once we have the frequency for positive and negative sentiment, we can 
# use a useful feature of R - vectorisation - to calculate net sentiment 
# across each day and plot it.
docvars(dfm_sentiment, "prop_negative") <- as.numeric(dfm_sentiment[,1] / 
                                                        ntoken(dfm_sentiment))
docvars(dfm_sentiment, "prop_positive") <- as.numeric(dfm_sentiment[,2] / 
                                                        ntoken(dfm_sentiment))
docvars(dfm_sentiment, "net_sentiment") <- docvars(dfm_sentiment, "prop_positive") 
                                                        - docvars(dfm_sentiment,"prop_negative")

docvars(dfm_sentiment) %>%
  ggplot(aes(x = yday(date), y = net_sentiment, group = year(date))) +
  geom_smooth(aes(colour = as.character(year(date)))) +
  labs(title = "Sentiment over time", 
       x = "day of year", y = "net sentiment", 
       colour = "year")

# Having performed these analyses, is there anything you would change in 
# the initial corpus? Try changing a few things and see how it affects 
# the results.

# save our data for next time
saveRDS(dfm22, "data/dfm22")
saveRDS(dfm23, "data/dfm23")
saveRDS(dfm24, "data/dfm24")
