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
  filter(grepl("Luke Harding|Julian Borger", 
               byline, ignore.case = TRUE)) %>%
  group_by(grp = str_extract(byline, "Luke Harding|Julian Borger")) %>%
  summarise(av = mean(fk$Flesch.Kincaid)) %>%
  ggplot(aes(x = reorder(grp, -av), y = av)) +
  geom_col() +
  ggtitle(label = "FK Readability by Correspondent") +
  xlab(label = NULL) +
  ylab("Mean Flesch-Kincaid")

summary(colc22$z) # Look at the z scores and see what cut-off to use
toks22 <- tokens_compound(toks22, pattern = colc22[colc22$z > 17,])
toks23 <- tokens_compound(toks23, pattern = colc23[colc23$z > 17,])
toks24 <- tokens_compound(toks24, pattern = colc24[colc24$z > 17,])

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

keyness <- textstat_keyness(dfm_by_date, target = "2024")
textplot_keyness(keyness, labelsize = 3)
