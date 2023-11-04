library(tidyverse)
library(ggplot2)
library(stm)
library(stminsights)


# save objects in .RData file
save.image('USdissNew.RData')
# export file

# Re-upload saved RData file
load(file = "USdissNew.RData")


# Upload file
usdiss4 <- read_delim("usdiss4.csv", delim = ";",
escape_double = FALSE, col_types = cols(Period_Zh = col_skip()),
trim_ws = TRUE)


# Note: usdiss4 is the usdiss3 to which I have added the dissertations for 2018-2022


# List the columns
colnames(usdiss4)


# Select only the dissertation filed in a US university
usdiss4 <- usdiss4 %>% filter(Country == "United States")

# Number of dissertations by university
usdiss4_School <- usdiss4 %>% select(School_Name, Title) %>% group_by(School_Name) %>% count()
write_csv(usdiss4_School, "usdiss4_School.csv")   

# Number of dissertations by year
usdiss4_Year <- usdiss4 %>% select(Year, Title) %>% group_by(Year) %>% count()
write_csv(usdiss4_Year, "usdiss4_Year.csv") 


# Number of dissertations by degree
usdiss4_Deg <- usdiss4 %>% select(Degree, Title) %>% group_by(Degree) %>% count()
write_csv(usdiss4_Deg, "usdiss4_Deg.csv") 

# Number of dissertations by period
usdiss4_Period <- usdiss4 %>% select(Period, Title) %>% group_by(Period) %>% count()
write_csv(usdiss4_Period, "usdiss4_Period.csv") 

# Add percentage
# Calculate the sum of the 'number' column
total_sum <- sum(usdiss4_Period$n)
# Create a new column with the percentage
usdiss4_Period$percentage <- (usdiss4_Period$n / total_sum) * 100

# Number of dissertations by department
usdiss4_Dpt <- usdiss4 %>% select(Department_Strd, Title) %>% group_by(Department_Strd) %>% count()


write_csv(usdiss4_School, "usdiss4_School.csv") 
write_csv(usdiss4_Year, "usdiss4_Year.csv") 
write_csv(usdiss4_Deg, "usdiss4_Deg.csv") 
write_csv(usdiss4_Period, "usdiss4_Period.csv") 
write_csv(usdiss4_Dpt, "usdiss4_Dpt.csv") 


# Number of dissertations by department
usdiss4_Dept <- usdiss4 %>% select(Department_Strd, Title) %>% group_by(Department_Strd) %>% count()
# This is not conclusive because the share of missing data (841) is too important
usdiss4_Hist <- usdiss4 %>% filter(str_detect(Department_Strd, "History"))
# In the available data, history is mentioned 179 times

# Number of dissertations by university
usdiss4_UnivDiss <- usdiss4 %>% select(School_Name, Title) %>% group_by(School_Name) %>% count()
write_csv(usdiss4_UnivDiss, "usdiss4_UnivDiss.csv") 


# Plot number of dissertations per year 
ggplot(data = usdiss4) + 
  geom_bar(mapping = aes(x = Year), fill="darkblue")+ 
  labs(title = "Number of dissertations per year (1932-2019)", 
       subtitle = "Dissertations per year", 
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")

# The number of dissertation is insignificant for a long period
# I filter with at least 5 dissertations per year

usdiss4_Yearfil <- usdiss4_Year %>% filter(n>9)



# Plot number of dissertations per year 

ggplot(usdiss4_Yearfil, aes(x = reorder(Year, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  labs(title = "Number of dissertations per year",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")


# Plot number of dissertations per university 
ggplot(usdiss4_School, aes(x = reorder(School_Name, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per university",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")

# It also produces too many results to be visualized 

usdiss4_Schoolfil <- usdiss4_School %>% filter(n>14)


ggplot(usdiss4_Schoolfil, aes(x = reorder(School_Name, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per university",
       subtitle = "15 dissertations or more",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")


# I select the dissertations for certain universities
# Harvard
usdiss4_Harvard <- usdiss4 %>% filter(str_detect(School_Name, "Harvard"))
# Stanford
usdiss4_Stanford <- usdiss4 %>% filter(str_detect(School_Name, "Stanford"))
# Princeton
usdiss4_Princeton <- usdiss4 %>% filter(str_detect(School_Name, "Princeton"))
# Chicago
usdiss4_Chicago <- usdiss4 %>% filter(str_detect(School_Name, "Chicago"))
# Columbia
usdiss4_Columbia <- usdiss4 %>% filter(str_detect(School_Name, "Columbia"))
# UCIrvine
usdiss4_UCIrvine <- usdiss4 %>% filter(str_detect(School_Name, "Irvine"))
# UCBerkeley
usdiss4_UCBerkeley <- usdiss4 %>% filter(str_detect(School_Name, "Berkeley"))
# Yale
usdiss4_Yale <- usdiss4 %>% filter(str_detect(School_Name, "Yale"))
# Michigan
usdiss4_Michigan<- usdiss4 %>% filter(str_detect(School_Name, "Michigan"))


write_csv(usdiss4_Harvard, "usdiss4_Harvard.csv")
write_csv(usdiss4_Stanford, "usdiss4_Stanford.csv")
write_csv(usdiss4_Princeton, "usdiss4_Princeton.csv")
write_csv(usdiss4_Chicago, "usdiss4_Chicago.csv")
write_csv(usdiss4_Columbia, "usdiss4_Columbia.csv")
write_csv(usdiss4_UCIrvine, "usdiss4_UCIrvine.csv")
write_csv(usdiss4_UCBerkeley, "usdiss4_UCBerkeley.csv")
write_csv(usdiss4_Yale, "usdiss4_Yale.csv")
write_csv(usdiss4_Michigan, "usdiss4_Michigan.csv")


# Plot number of Harvard dissertations per year 
ggplot(data = usdiss4_Columbia) + 
  geom_bar(mapping = aes(x = Year), fill="darkblue")+ 
  labs(title = "Columbia dissertations per year (1988-2022)", 
       subtitle = "Dissertations per year", 
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")

# Plot number of Columbia dissertations per year 
ggplot(data = usdiss4_Harvard) + 
  geom_bar(mapping = aes(x = Year), fill="darkblue")+ 
  labs(title = "Harvard dissertations per year (1988-2022)", 
       subtitle = "Dissertations per year", 
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")


# Select the universities with 15 dissertations or less
library(forcats)
usdiss4_SchoolLump <- usdiss4_School %>%
  mutate(School_Name = fct_lump(School_Name, n = 15)) %>%
  count(School_Name, sort = TRUE)


### Mapping universities

# I add the location of universities
# Upload file with list of universities and location
US_universities_LocCoord <- read_csv("US_universities_LocCoord.csv")

# I do a left_join to add the location
usdiss4_SchoolLoc <- left_join(usdiss4_School, US_universities_LocCoord)


# I add the locations to the original usdiss4 file
usdiss4Loc <- left_join(usdiss4, usdiss4_SchoolLoc)
write_csv(usdiss4Loc, "usdiss4Loc.csv")

# I plot the number of dissertations per city
# I chose to have a horizontal bar chart with decreasing values
ggplot(usdiss4Loc, aes(x = reorder(City, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per city",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")

# The plot above is not satisfying as the script does not work to produce a hierarchical ranking
# It also produces too many results to be visualized 

usdiss4Loc2 <- usdiss4Loc %>% filter(n>14)

ggplot(usdiss4Loc2, aes(x = reorder(City, n), y = n)) + geom_bar(stat = "identity", fill="darkblue")+ 
  coord_flip() +
  labs(title = "Number of dissertations per city",
       subtitle = "15 dissertations or more",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")


# I want to map the distribution of dissertations 
library(leaflet)
library(readxl)

# I remove Hawaii
usdiss4LocUSA <- usdiss4Loc %>% filter(!str_detect(State, "Hawaii"))
write_csv(usdiss4LocUSA, "usdiss4LocUSA.csv")
us_uni <- usdiss4LocUSA

# Create the map of university cities with dissertations
leaflet(data = us_uni) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, popup = ~School_Name)


# Create the map of universities 
leaflet(data = us_uni) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, radius = ~n/50,
                   popup = ~paste(School_Name, ":", n, "dissertations"),
                   fill = TRUE, fillOpacity = 0.5, color = "green")



leaflet(data = us_uni) %>%
  addTiles() %>%
  addCircleMarkers(~lng, ~lat, radius = ~sqrt(n),
                   popup = ~paste(School_Name, ":", n, "dissertations"),
                   fill = TRUE, fillOpacity = 0.5, color = "green")



# I want to explore the content of dissertation abstracts
# I filter out the dissertations that have no abstract
usdiss4tk <- usdiss4 %>% filter(!str_detect(Abstract, "Abstract not available"))
# I remove all the quotation marks in Abstracts
usdiss4tk <- usdiss4tk %>% mutate(name = str_remove_all(Abstract, "\""))


# pre-processing
usdiss4tkt <- usdiss4tk %>% select(StoreId, Abstract, Title, Year, School_Name, Keywords_Ext)
meta <- usdiss4tkt %>% transmute(StoreId, Title, Year, School_Name, Keywords_Ext)
corpus <- stm::textProcessor(usdiss4tk$Abstract, 
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(4, Inf), 
                             customstopwords = c("part", "among", "many", "within", "study", "used", "well", "explain", "however", "china", "toward", "chinas", "china's", "chinese", "dissertation", "chapter", "chapters", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "also", "dissertation", "argue", "even", "rather", "examine", "examines", "argues", "explores", "thus"))

# Filtering
out <- stm::prepDocuments(corpus$documents, corpus$vocab, corpus$meta, lower.thresh = 10)

#Removing 18029 of 20671 terms (39883 of 142122 tokens) due to frequency 
#Your corpus now has 1109 documents, 2642 terms and 102239 tokens.

# select model 
Ksearch <- stm::searchK(out$documents, out$vocab, c(5, 6, 7, 10), cores = 1)
plot(Ksearch)

# Ksearch point to an optimal model at 6 or 7 topics. The 6-topic model presents the most optimal parameters
# I choose to calculate only tree models fot he sake of comparison

# Build model with 6 topics
mod.6 <- stm::stm(out$documents, out$vocab, K=6, prevalence =~ School_Name + Year, data=out$meta)
# Build model with 7 topics
mod.7 <- stm::stm(out$documents, out$vocab, K=7, prevalence =~ School_Name + Year, data=out$meta)
# Build model with 10 topics
mod.10 <- stm::stm(out$documents, out$vocab, K=10, prevalence =~ School_Name + Year, data=out$meta)


# estimate effect for year and school
effect_6 <- stm::estimateEffect(1:6 ~ School_Name + Year, mod.6, meta=out$meta)
effect_7 <- stm::estimateEffect(1:7 ~ School_Name + Year, mod.7, meta=out$meta)
effect_10 <- stm::estimateEffect(1:10 ~ School_Name + Year, mod.10, meta=out$meta)

# View the proportions of topics for each document, along with their metadata

topicprop6<-make.dt(mod.6, meta)
topicprop6

topicprop7<-make.dt(mod.7, meta)
topicprop7

topicprop10<-make.dt(mod.10, meta)
topicprop10

write_csv(topicprop6, "topicprop6.csv")
write_csv(topicprop7, "topicprop7.csv")
write_csv(topicprop10, "topicprop10.csv")

# Visualize the estimates of document-topic proportions
plot.STM(mod.6, "hist")
plot.STM(mod.7, "hist")
plot.STM(mod.10, "hist")


#  plot the topic distribution per document, using tidytext and ggplot

library(tidytext)
# the tidytext package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta6 <- tidytext::tidy(mod.6, matrix = "theta")
td_theta7 <- tidytext::tidy(mod.7, matrix = "theta")
td_theta10 <- tidytext::tidy(mod.10, matrix = "theta")

selectiontdthteta6<-td_theta6[td_theta6$document%in%c(1:15),] 
selectiontdthteta7<-td_theta7[td_theta7$document%in%c(1:15),] 
selectiontdthteta10<-td_theta10[td_theta10$document%in%c(1:15),] 
#select the first 15 documents. be careful to select a sensible interval, 
#as attempting to load a very huge corpus might crash the kernel


thetaplot6<-ggplot(selectiontdthteta6, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (first 15 documents)",
       y = expression(theta), x = "Topic")

thetaplot6

thetaplot7<-ggplot(selectiontdthteta7, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (first 15 documents)",
       y = expression(theta), x = "Topic")

thetaplot7


#select the last 15 documents
selectiontdthteta6l<-td_theta6[td_theta6$document%in%c(1015:1026),] 

thetaplot6l<-ggplot(selectiontdthteta6l, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (bottom list)",
       y = expression(theta), x = "Topic")

thetaplot6l


#Next, we want to understand more about each topic – what are they really about
# If we go back to the β matrix, we can have a more analytical look at the word frequencies per topic
# The matrix stores the log of the word probabilities for each topic, and plotting it can give us a good overall understanding of the distribution of words per topic

td_beta6 <- tidytext::tidy(mod.6) 
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 
td_beta6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic (6 topics)",
       subtitle = "Different words are associated with different topics")

td_beta7 <- tidytext::tidy(mod.7) 
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 
td_beta7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic (7 topics)",
       subtitle = "Different words are associated with different topics")


td_beta <- tidytext::tidy(mod.10) 
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic (10 topics)",
       subtitle = "Different words are associated with different topics")

# save objects in .RData file
save.image('USdissNew.RData')

# In this case I found pretty helpful to have a more detailed look at the word distribution within each topic
# The plot above focus only on the top 10 words for the first topic):

# 6-Topic model

# Topic 1  
beta6T1<-td_beta6 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") #beta values for topic 1

beta6plotT1<-ggplot(beta6T1[beta6T1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1") #plot word probabilities higher than 0.003 for topic 1

beta6plotT1

# Topic 2  
beta6T2<-td_beta6 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 2") #beta values for topic 2

beta6plotT2<-ggplot(beta6T2[beta6T2$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 2") #plot word probabilities higher than 0.003 for topic 1
beta6plotT2


# Topic 3  
beta6T3<-td_beta6 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 3") #beta values for topic 3

beta6plotT3<-ggplot(beta6T3[beta6T3$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 3") #plot word probabilities higher than 0.003 for topic 1

beta6plotT3



# Topic 4  
beta6T4<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 4") #beta values for topic 4

beta6plotT4<-ggplot(beta6T4[beta6T4$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 4") #plot word probabilities higher than 0.003 for topic 1
beta6plotT4

# Topic 5  
beta6T5<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 5") #beta values for topic 5

beta6plotT5<-ggplot(beta6T5[beta6T5$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 5") #plot word probabilities higher than 0.003 for topic 1
betaplotT5


# Topic 6  
beta6T6<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 3") #beta values for topic 3

beta6plotT6<-ggplot(beta6T6[beta6T6$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 6") #plot word probabilities higher than 0.003 for topic 1

beta6plotT6


# 7-topic model


# Topic 1 in mod.7  
beta7T1<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") #beta values for topic 1

beta7plotT1<-ggplot(beta7T1[beta7T1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1") #plot word probabilities higher than 0.003 for topic 1

beta7plotT1

# Topic 2 in mod.7  
beta7T2<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 2") #beta values for topic 2

beta7plotT2<-ggplot(beta7T2[beta7T2$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 2") #plot word probabilities higher than 0.003 for topic 1

beta7plotT2


# Topic 3 in mod.7  
beta7T3<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 3") #beta values for topic 3

beta7plotT3<-ggplot(beta7T3[beta7T3$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 3") #plot word probabilities higher than 0.003 for topic 1

beta7plotT3

# Topic 4 in mod.7  
beta7T4<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 4") #beta values for topic 4

beta7plotT4<-ggplot(beta7T4[beta7T4$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 4") #plot word probabilities higher than 0.003 for topic 1
beta7plotT4

# Topic 5 in mod.7  
beta7T5<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 5") #beta values for topic 5

beta7plotT5<-ggplot(beta7T5[beta7T5$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 5") #plot word probabilities higher than 0.003 for topic 1

beta7plotT5

# Topic 6 in mod.7  
beta7T6<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 6") #beta values for topic 6

beta7plotT6<-ggplot(beta7T6[beta7T6$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 6") #plot word probabilities higher than 0.003 for topic 1

beta7plotT6

# Topic 7 in mod.7  
beta7T7<-td_beta7 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 7") #beta values for topic 7

beta7plotT7<-ggplot(beta7T7[beta7T7$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 7") #plot word probabilities higher than 0.003 for topic 1

beta7plotT7


# I use plot.STM with argument “summary” to visualize the topic distribution (which topics are overall more common) with the most common words for each topic 

plot.STM(mod.6, "summary", n=5) # distribution and top 5 words per topic
plot.STM(mod.7, "summary", n=5) # distribution and top 5 words per topic
plot.STM(mod.10, "summary", n=5) # distribution and top 5 words per topic


# The function labelTopics (or sageLabels) gives us more detailed insights on the popular words in each topic
# As mentioned above, other than the highest probability, we can visualize more distribution patterns:
# FREX words (FREX weights words by frequency and exclusivity to the topic), lift words (frequency divided by frequency in other topics), and score (similar to lift, but with log frequencies)
# Visualization in the console only
labelTopics(mod.6, n=10) # complete list of top 10 words per topic
labelTopics(mod.7, n=10) # complete list of top 10 words per topic
labelTopics(mod.10, n=10) # complete list of top 10 words per topic


# Examine selected topics
labelTopics(mod.6, topics=c(1,2,3,4,5,6), n=10) # complete list of top 10 words per topics 1,2,3,4,5,6
labelTopics(mod.7, topics=c(1,2,3,4,5,6,7), n=10) # complete list of top 10 words per topics 1,2,3,4,5,6
labelTopics(mod.10, topics=c(1,2,5,10), n=10) # complete list of top 10 words per topics 1,2,5,10

# Visualize the same as above in a table
plot.STM(mod.6, topics=c(1,2,3,4,5,6), n=10) # complete list of top 10 words per topics 1,2,3,4,5,6
plot.STM(mod.7, topics=c(1,2,3,4,5,6,7), n=10) # complete list of top 10 words per topics 1,2,3,4,5,6
plot.STM(mod.10, "labels", topics=c(1,2,3,4,5,6), label="frex", n=10, width=60)#top 10 FREX words per topics 1,2,5,10


# We can further have a glimpse at highly representative documents per each topic with findThoughts
# and plot them with plotQuote (this might give best results with shorter documents):
# Representative documents for topics 2 and 5:

thoughts2 <- findThoughts(mod.6,texts=usdiss4tk$Abstract, topics=2, n=3)$docs[[1]]# select 3 representative documents per topic 2
thoughts5 <- findThoughts(mod.6,texts=usdiss4tk$Abstract, topics=5, n=3)$docs[[1]]# select 3 representative documents per topic 5

par(mfrow=c(1,2), mar=c(0,0,2,2))
plotQuote(thoughts2, width=50, maxwidth=500, text.cex=0.5, main="Topic 2")
plotQuote(thoughts5, width=50, maxwidth=500, text.cex=0.5, main="Topic 5")

par(mfrow=c(1,1))


# Topic correlation shows relations between topics based on the proportions of words they have in common:

mod6.out.corr <- topicCorr(mod.6)
plot(mod6.out.corr)

mod7.out.corr <- topicCorr(mod.7)
plot(mod7.out.corr)

mod.out.corr <- topicCorr(mod.10)
plot(mod.out.corr)



# Stm offers two options for estimating topic correlations - simple and huge (more complex). The “simple” method simply thresholds the covariances whereas “huge” uses the semiparametric procedure in the package. Let’s compare the two procedures:

corrsimple6 <- topicCorr(mod.6, method = "simple", verbose = FALSE)
corrhuge <- topicCorr(mod.15, method = "huge", verbose = FALSE)
par(mfrow=c(1,2), mar=c(0,0,2,2))
plot(corrsimple6, main = "Simple method")
plot(corrhuge, main = "Huge method")

corrsimple7 <- topicCorr(mod.7, method = "simple", verbose = FALSE)
corrhuge <- topicCorr(mod.15, method = "huge", verbose = FALSE)
par(mfrow=c(1,2), mar=c(0,0,2,2))
plot(corrsimple7, main = "Simple method")
plot(corrhuge, main = "Huge method")

corrsimple10 <- topicCorr(mod.10, method = "simple", verbose = FALSE)
corrhuge <- topicCorr(mod.10, method = "huge", verbose = FALSE)
par(mfrow=c(1,2), mar=c(0,0,2,2))
plot(corrsimple10, main = "Simple method")
plot(corrhuge, main = "Huge method")

par(mfrow=c(1,1))



# We use gggraph to produce a more accurate visualization reflecting the topic proportion and importance of correlation
# Simple method:
# This will work only if there are sufficient correlations
# With the 6- or 7- topic models, it does not produce anything relevant

# 10-topic model
# extract network 
stm_corrs <- get_network(model = mod.10,
                         method = 'simple',
                         labels = paste('Topic', 1:10),
                         cutoff = 0.001,
                         cutiso = TRUE)

# plot network with ggraph
library(ggraph)

ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  labs(size = 1.0,  edge_width = 1.0, title = "Simple method") +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()


# Control script
# This is to make sure that the stm_corrs object is a valid graph object that can be processed by ggraph
class(stm_corrs)
# Start with a minimal ggraph call
ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_label(aes(label = name))
#Check Layout: use a different layout, such as 'kk' (Kamada-Kawai):
ggraph(stm_corrs, layout = 'kk') +
  geom_edge_link(aes(edge_width = weight)) +
  geom_node_point(size = 4)  +
  geom_node_label(aes(label = name, size = props), repel = TRUE, alpha = 0.85)
#Check Layout:  return to 'fr'layout:
ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_label(aes(label = name)) +
  theme_graph()

ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(aes(edge_width = weight), 
                    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(aes(label = name, size = props),
                  colour = 'black', repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()
##End of script control




# Huge method:
# extract network 

stm_corrs_huge <- get_network(model = mod.10,
                              method = 'huge',
                              labels = paste('Topic', 1:10),
                              cutoff = 0.001,
                              cutiso = TRUE)

# plot network 

ggraph(stm_corrs_huge, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  scale_edge_width(range = c(1, 3)) +
  labs(size = 1.0,  edge_width = 1.0, title = "Huge method") +
  theme_graph()



# The “perspective” argument enables to compare topics two by two. For instance, we can compare the two “brevities” topics:

plot(mod.6, type="perspectives", topics=c(1, 5))
plot(mod.7, type="perspectives", topics=c(2, 6))


# Word clouds provide an intuitive, though less rigorous way of visualizing word prevalence in topics.

# First line of the script prepares for splitting into two rows to display two word clouds
par(mfrow=c(1,2), mar=c(0,0,2,2))
cloud(mod.6, topic = 1, scale = c(4, 0.4))
cloud(mod.7, topic = 5, scale = c(4, 0.4))

par(mfrow=c(1,2), mar=c(0,0,2,2))
cloud(mod.7, topic = 2, scale = c(4, 0.4))
cloud(mod.7, topic = 7, scale = c(4, 0.4))

par(mfrow=c(1,1))

# First line of the script prepares for splitting into four rows to display four word clouds
par(mfrow=c(2,4), mar=c(0,0,4,4))
cloud(mod.6, topic = 1, scale = c(4, 0.4))
cloud(mod.6, topic = 3, scale = c(4, 0.4))
cloud(mod.6, topic = 5, scale = c(4, 0.4))
cloud(mod.6, topic = 6, scale = c(4, 0.4))

# interactively visualize an LDA topic model
stm::toLDAvis(mod.6, doc=out$documents)
stm::toLDAvis(mod.7, doc=out$documents)
stm::toLDAvis(mod.10, doc=out$documents)



library(reshape)
library(pals)
library(ggplot2)
library(tidyverse)

# Topic proportion over time

# topic proportion per year 

# I changed the original script because it created a Year.1 column that I could not get rid of

# Remove unwanted columns  from topicprop10
topicprop10s <- topicprop10 %>% select(-c(Title, School_Name, Keywords_Ext, Year))

# Join the two data frames by the new StoreId column
combined_data <- inner_join(topicprop10s, usdiss4tkt, by = c("StoreId" = "StoreId"))

# Use dplyr to group and summarise
topic_proportion_per_year10 <- combined_data %>%
  group_by(Year) %>%
  summarise(across(starts_with("Topic"), mean, na.rm = TRUE))

# Save
write.csv(topic_proportion_per_year10, "topic_proportion_per_year10.csv")

# reshape data frame
vizDataFrame10y <- topic_proportion_per_year10 %>% pivot_longer(!Year, names_to = "variable", values_to = "value")

# plot topic proportions per year as bar plot

require(pals)
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Topics over time in US dissertations", 
       subtitle = "Topic proportion over time", 
       caption = "10-topic stm model")

library(ggplot2)
library(dplyr)
library(pals)

# Assuming topic_proportion_per_year10 is already loaded and contains the correct data

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Change color palette
color_palette <- brewer.pal(10, "Set3")  # Adjust the number if needed

# Plot topic proportions per year as bar plot with new color palette
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + 
  ylab("proportion") + 
  scale_fill_manual(values=color_palette, name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Topics over time in US dissertations", 
       subtitle = "Topic proportion over time", 
       caption = "10-topic stm model")



# Plot topic proportions per year as line plot
ggplot(vizDataFrame10y, aes(x=Year, y=value, group=variable, color=variable)) + 
  geom_line() + 
  ylab("proportion") + 
  scale_color_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Topics over time in US dissertations", 
       subtitle = "Topic proportion over time", 
       caption = "10-topic stm model")


# interactively visualize with LDA stminsignhts
run_stminsights()

# save objects in .RData file
save.image('USdissNew.RData')



