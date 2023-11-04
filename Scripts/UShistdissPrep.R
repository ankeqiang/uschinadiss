#US dissertations in history on China: data processing of metadata extracted from ProQuest Dissertations


library(tidyverse)
library(ggplot2)
library(stm)
library(stminsights)

run_stminsights()

# save objects in .RData file
save.image('USdiss.RData')
# export file

# Re-upload saved RData file
load(file = "USdiss.RData")


### Initial Script
# Upload file
usdiss <- read_delim("China_PlaceHist_Wrk3 Henriot v2.csv",
                     ";", escape_double = FALSE, trim_ws = TRUE)

# Note: I removed all the semi-column and quotation marks in the Abstract and Title fields. They create issues in R.


# List the columns
colnames(usdiss)

# Reorder the columns
usdiss2 <-usdiss[,c(1,3,4,12,13,11,7,8,9,5,10,6,15,16,17,2)]

# Select only the dissertation filed in a US university
usdiss2 <- usdiss2 %>% filter(Country == "United States")

# Homogenizing names of institutions
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Harvard Divinity School", "Harvard University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Columbia University in the City of New York", "Columbia University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Brandeis University, The Heller School for Social Policy and Management", "Brandeis University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Fuller Theological Seminary, Doctor of Ministry Program", "Fuller Theological Seminary"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Fuller Theological Seminary, School of World Mission", "Fuller Theological Seminary"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Princeton Theological Seminary", "Princeton University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "Rutgers The State University of New Jersey, School of Graduate Studies", "Rutgers The State University of New Jersey"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The Claremont Graduate University", "Claremont Graduate University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The Florida State University", "Florida State University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The George Washington University", "George Washington University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The Johns Hopkins University", "Johns Hopkins University"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of Akron", "University of Akron"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of Chicago", "University of Chicago"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of Iowa", "University of Iowa"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of New Mexico", "University of New Mexico"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of North Carolina at Chapel Hill", "University of North Carolina at Chapel Hill"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of Wisconsin - Madison", "University of Wisconsin - Madison"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "The University of Wisconsin - Milwaukee", "University of Wisconsin - Milwaukee"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Berkeley", "University of California Berkeley"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Davis", "University of California Davis"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Irvine", "University of California Irvine"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Los Angeles", "University of California Irvine"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Merced", "University of California Merced"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Riverside", "University of California Riverside"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, San Diego", "University of California San Diego"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Santa Barbara", "University of California Santa Barbara"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of California, Santa Cruz", "University of California Santa Cruz"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of Colorado at Boulder", "University of Colorado Boulder"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of Hawai'i at Manoa", "University of Hawaii at Manoa"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of Maryland, Baltimore County", "University of Maryland Baltimore"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of Maryland, College Park", "University of Maryland"))
usdiss2 <- usdiss2 %>% mutate(School_Name = str_replace(School_Name, "University of Missouri - Columbia", "University of Missouri"))



write_csv(usdiss2, "usdiss2.csv")
# Note: last two rows corrected manually. Need to find exact expression to take care of removing the parenthesis

# Upload curated usdiss2.csv
usdiss2 <- read_csv("usdiss2.csv")


# Filter out the degrees that are not doctorates 
usdiss2_FilDeg <- usdiss2 %>% filter(!Degree == "A.M.")
usdiss2_FilDeg2 <- usdiss2_FilDeg %>% filter(!Degree == "M.A.")
usdiss2_FilDeg3 <- usdiss2_FilDeg2 %>% filter(!Degree == "M.A.L.S.")
usdiss2_FilDeg4 <- usdiss2_FilDeg3 %>% filter(!Degree == "M.Arch.")
usdiss2_FilDeg5 <- usdiss2_FilDeg4 %>% filter(!Degree == "M.B.A.")
usdiss2_FilDeg6 <- usdiss2_FilDeg5 %>% filter(!Degree == "M.F.A.")
usdiss2_FilDeg7 <- usdiss2_FilDeg6 %>% filter(!Degree == "M.F.S.")
usdiss2_FilDeg8 <- usdiss2_FilDeg7 %>% filter(!Degree == "M.S.")
usdiss2_FilDeg9 <- usdiss2_FilDeg8 %>% filter(!Degree == "M.Sc.")
usdiss2_FilDeg10 <- usdiss2_FilDeg9 %>% filter(!Degree == "Th.M.")
usdiss2_FilDeg11 <- usdiss2_FilDeg10 %>% filter(!Degree == "A.L.M.")

# Simplify file name
usdiss3 <- usdiss2_FilDeg11 
write_csv(usdiss3, "usdiss3.csv")  

# Number of dissertations by university
usdiss3_School <- usdiss3 %>% select(School_Name, Title) %>% group_by(School_Name) %>% count()
write_csv(usdiss3_School, "usdiss3_School.csv")   

# Number of dissertations by year
usdiss3_Year <- usdiss3 %>% select(Year, Title) %>% group_by(Year) %>% count()
write_csv(usdiss3_Year, "usdiss3_Year.csv") 

# Number of dissertations by degree
usdiss3_Deg <- usdiss3 %>% select(Degree, Title) %>% group_by(Degree) %>% count()
write_csv(usdiss3_Deg, "usdiss3_Deg.csv") 


# Number of dissertations by department
usdiss3_Dept <- usdiss3 %>% select(Department, Title) %>% group_by(Department) %>% count()
# This is not conclusive because the share of missing data (841) is too important
usdiss3_Hist <- usdiss3 %>% filter(str_detect(Department, "History"))
# In the available data, history is mentioned 111 times

# Number of dissertations by university
usdiss3_UnivDiss <- usdiss3 %>% select(School_Name, Title) %>% group_by(School_Name) %>% count()
write_csv(usdiss3_UnivDiss, "usdiss3_UnivDiss.csv") 



# Plot number of dissertations per year 
ggplot(data = usdiss3) + 
  geom_bar(mapping = aes(x = Year), fill="darkblue")+ 
  labs(title = "Number of dissertations per year (1932-2019)", 
       subtitle = "Dissertations per year", 
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")

# The number of dissertation is insignificant for a long period
# I filter with at least 5 dissertations per year

usdiss3_Yearfil <- usdiss3_Year %>% filter(n>9)



# Plot number of dissertations per year 

ggplot(usdiss3_Yearfil, aes(x = reorder(Year, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  labs(title = "Number of dissertations per year",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")


# Plot number of dissertations per university 
ggplot(usdiss3_School, aes(x = reorder(School_Name, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per university",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")

# It also produces too many results to be visualized 

usdiss3_Schoolfil <- usdiss3_School %>% filter(n>14)


ggplot(usdiss3_Schoolfil, aes(x = reorder(School_Name, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per university",
       subtitle = "15 dissertations or more",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")


# I select the dissertations for certain universities
# Harvard
usdiss3_Harvard <- usdiss3 %>% filter(str_detect(School_Name, "Harvard"))
# Stanford
usdiss3_Stanford <- usdiss3 %>% filter(str_detect(School_Name, "Stanford"))
# Princeton
usdiss3_Princeton <- usdiss3 %>% filter(str_detect(School_Name, "Princeton"))
# Chicago
usdiss3_Chicago <- usdiss3 %>% filter(str_detect(School_Name, "Chicago"))
# Columbia
usdiss3_Columbia <- usdiss3 %>% filter(str_detect(School_Name, "Columbia"))
# UCIrvine
usdiss3_UCIrvine <- usdiss3 %>% filter(str_detect(School_Name, "Irvine"))
# UCBerkeley
usdiss3_UCBerkeley <- usdiss3 %>% filter(str_detect(School_Name, "Berkeley"))
# Yale
usdiss3_Yale <- usdiss3 %>% filter(str_detect(School_Name, "Yale"))
# Michigan
usdiss3_Michigan<- usdiss3 %>% filter(str_detect(School_Name, "Michigan"))


write_csv(usdiss3_Harvard, "usdiss3_Harvard.csv")
write_csv(usdiss3_Stanford, "usdiss3_Stanford.csv")
write_csv(usdiss3_Princeton, "usdiss3_Princeton.csv")
write_csv(usdiss3_Chicago, "usdiss3_Chicago.csv")
write_csv(usdiss3_Columbia, "usdiss3_Columbia.csv")
write_csv(usdiss3_UCIrvine, "usdiss3_UCIrvine.csv")
write_csv(usdiss3_UCBerkeley, "usdiss3_UCBerkeley.csv")
write_csv(usdiss3_Yale, "usdiss3_Yale.csv")
write_csv(usdiss3_Michigan, "usdiss3_Michigan.csv")


# Plot number of Harvard dissertations per year 
ggplot(data = usdiss3_Yale) + 
  geom_bar(mapping = aes(x = Year), fill="darkblue")+ 
  labs(title = "Columbia dissertations per year (1960-2019)", 
       subtitle = "Dissertations per year", 
       caption = "based on data extracted from ProQuest Dissertations",
       x = "Year",
       y = "Number of dissertations")


###To be revised - formula below did not work - "Other" category added manually in "School" column

#_min :
usdiss3_SchoolLump <- usdiss3_School %>%
  mutate(School_Name = fct_lump_min(School_Name, min = 15)) %>%
  count(School_Name, sort = TRUE)

###

# I add the location of universities
# Upload file with list of universities and location
US_universities_Locations <- read_csv("US_universities_Locations.csv")

# I remove erroneous or duplicated universities
US_universities_Locations <- US_universities_Locations %>% filter(!str_detect(City, "Manoa"))

# I do a left_join to add the location
usdiss3_SchoolLoc <- left_join(usdiss3_School, US_universities_Locations)

# I add the locations to the original usdiss3 file
usdiss3Loc <- left_join(usdiss3, usdiss3_SchoolLoc)
# I count the number of dissertations by city
usdiss3Loc <- usdiss3Loc %>% select(City, Title) %>% group_by(City) %>% count()

# I plot the number of dissertations per city
# I chose to have a horizontal bar chart with decreasing values
ggplot(usdiss3Loc, aes(x = reorder(City, n), y = n)) + geom_bar(stat = "identity", fill="palegreen4")+ 
  coord_flip() +
  labs(title = "Number of dissertations per city",
       subtitle = "(1988-2018)",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")

# The plot above is not satisfying as the script does not work to produce a hierarchical ranking
# It also produces too many results to be visualized 

usdiss3Loc2 <- usdiss3Loc %>% filter(n>14)

ggplot(usdiss3Loc2, aes(x = reorder(City, n), y = n)) + geom_bar(stat = "identity", fill="darkblue")+ 
  coord_flip() +
  labs(title = "Number of dissertations per city",
       subtitle = "15 dissertations or more",
       caption = "based on data extracted from ProQuest Dissertations",
       x = "University",
       y = "Number of dissertations")



# I want to explore the content of dissertation abstracts
# I filter out the dissertations that have no abstract
usdiss3tk <- usdiss3 %>% filter(!is.na(Abstract))
# I filter out the dissertations that state "no abstract available"
usdiss3tk <- usdiss3tk %>% filter(!str_detect(Abstract, "Abstract not available."))
# I remove all the quotation marks in Abstracts
usdiss3tk <- usdiss3tk %>% mutate(name = str_remove_all(Abstract, "\""))


# pre-processing
usdiss3tkt <- usdiss3tk %>% select(StoreId, Abstract, Title, Year, School_Name, Subjects1, Keyword2)
meta <- usdiss3tkt %>% transmute(StoreId, Title, Year, School_Name, Subjects1, Keyword2)
corpus <- stm::textProcessor(usdiss3tk$Abstract, 
                             metadata = meta, 
                             stem = FALSE, 
                             wordLengths = c(4, Inf), 
                             customstopwords = c("part", "among", "many", "within", "study", "used", "well", "explain", "however", "china", "toward", "chinas", "china's", "chinese", "dissertation", "chapter", "chapters", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "also", "dissertation", "argue", "even", "rather", "examines", "argues", "explores", "thus"))

# Filtering
out <- stm::prepDocuments(corpus$documents, corpus$vocab, corpus$meta, lower.thresh = 10)

#Removing 17036 of 19476 terms (37960 of 129078 tokens) due to frequency 
#Your corpus now has 1024 documents, 2440 terms and 91118 tokens.

# select model 
Ksearch <- stm::searchK(out$documents, out$vocab, c(10,15,20,25,30), cores = 1)
plot(Ksearch)


# Build model with 10 topics
mod.10 <- stm::stm(out$documents, out$vocab, K=10, prevalence =~ School_Name + Year, data=out$meta)
# Build model with 15 topics
mod.15 <- stm::stm(out$documents, out$vocab, K=15, prevalence =~ School_Name + Year, data=out$meta)


# estimate effect for year and school
effect_10 <- stm::estimateEffect(1:10 ~ School_Name + Year, mod.20, meta=out$meta)
effect_15 <- stm::estimateEffect(1:15 ~ School_Name + Year, mod.15, meta=out$meta)




# View the proportions of topics for each document, along with their metadata

topicprop10<-make.dt(mod.10, meta)
topicprop10

topicprop15<-make.dt(mod.15, meta)
topicprop15


# Visualize the estimates of document-topic proportions
plot.STM(mod.10, "hist")
plot.STM(mod.15, "hist")


#  plot the topic distribution per document, using tidytext and ggplot

library(tidytext)
# the tidytext package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta10 <- tidytext::tidy(mod.10, matrix = "theta")
td_theta15 <- tidytext::tidy(mod.15, matrix = "theta")


selectiontdthteta<-td_theta10[td_theta10$document%in%c(1:15),] #select the first 15 documents. be careful to select a sensible interval, 
#as attempting to load a very huge corpus might crash the kernel
selectiontdthteta15<-td_theta15[td_theta15$document%in%c(1:15),]


thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (first 15 documents)",
       y = expression(theta), x = "Topic")

thetaplot1

thetaplot151<-ggplot(selectiontdthteta15, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (first 15 documents)",
       y = expression(theta), x = "Topic")

thetaplot151


#select the last 15 documents
selectiontdthteta2<-td_theta[td_theta$document%in%c(1015:1026),] 

thetaplot2<-ggplot(selectiontdthteta2, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document (bottom list)",
       y = expression(theta), x = "Topic")

thetaplot2


#Next, we want to understand more about each topic – what are they really about. If we go back to the β matrix, we can have a more analytical look at the word frequencies per topic. The matrix stores the log of the word probabilities for each topic, and plotting it can give us a good overall understanding of the distribution of words per topic

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
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

td_beta15 <- tidytext::tidy(mod.15) 
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 
td_beta15 %>%
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
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



# In this case I found pretty helpful to have a more detailed look at the word distribution within each topic (the plot above focus only on the top 10 words for the first topic):

# Topic 1  
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") #beta values for topic 1

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1") #plot word probabilities higher than 0.003 for topic 1

betaplotT1

# Topic 115  
betaT115<-td_beta15 %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") #beta values for topic 1

betaplotT115<-ggplot(betaT115[betaT115$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1") #plot word probabilities higher than 0.003 for topic 1

betaplotT115


# Topic 2  
betaT2<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 2") #beta values for topic 2

betaplotT2<-ggplot(betaT2[betaT2$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 2") #plot word probabilities higher than 0.003 for topic 1

betaplotT2

# Topic 3  
betaT3<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 3") #beta values for topic 3

betaplotT3<-ggplot(betaT3[betaT3$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 3") #plot word probabilities higher than 0.003 for topic 1

betaplotT3

# Topic 4  
betaT4<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 4") #beta values for topic 4

betaplotT4<-ggplot(betaT4[betaT4$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 4") #plot word probabilities higher than 0.003 for topic 1
betaplotT4

# Topic 5  
betaT5<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 5") #beta values for topic 5

betaplotT5<-ggplot(betaT5[betaT5$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 5") #plot word probabilities higher than 0.003 for topic 1
betaplotT5


# Topic 15
betaT15<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 15") #beta values for topic 15

betaplotT15<-ggplot(betaT15[betaT15$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 15") #plot word probabilities higher than 0.003 for topic 1

betaplotT15


# Topic 10  
betaT10<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 10") #beta values for topic 10

betaplotT10<-ggplot(betaT10[betaT10$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 10") #plot word probabilities higher than 0.003 for topic 1

betaplotT10

# I use plot.STM with argument “summary” to visualize the topic distribution (which topics are overall more common) with the most common words for each topic 

plot.STM(mod.10, "summary", n=5) # distribution and top 5 words per topic
plot.STM(mod.15, "summary", n=5) # distribution and top 5 words per topic


# The function labelTopics (or sageLabels) gives us more detailed insights on the popular words in each topic. As mentioned above, other than the highest probability, we can visualize the FREX words (FREX weights words by frequency and exclusivity to the topic), lift words (frequency divided by frequency in other topics), and score (similar to lift, but with log frequencies)
# Visualization in the console only
labelTopics(mod.10, n=10) # complete list of top 10 words per topic
labelTopics(mod.15, n=10) # complete list of top 10 words per topic

# Examine selected topics
labelTopics(mod.10, topics=c(1,2,5,10), n=10) # complete list of top 10 words per topics 1,2,5,10
labelTopics(mod.15, topics=c(1,2,5,10,12), n=10) # complete list of top 10 words per topics 1,2,5,10,12

# Visualize the same as above in a table
plot.STM(mod.10, "labels", topics=c(1,2,5,10), label="frex", n=10, width=60)#top 10 FREX words per topics 1,2,5,10
plot.STM(mod.15, "labels", topics=c(1,2,5,10,15), label="frex", n=10, width=60)#top 10 FREX words per topics 1,2,5,10,15


# We can further have a glimpse at highly representative documents per each topic with findThoughts, and plot them with plotQuote (this might give best results with shorter documents):
# Representative documents for topics 2 and 5:

thoughts2 <- findThoughts(mod.10,texts=usdiss3tk$Abstract, topics=2, n=3)$docs[[1]]# select 3 representative documents per topic 2
thoughts5 <- findThoughts(mod.10,texts=usdiss3tk$Abstract, topics=5, n=3)$docs[[1]]# select 3 representative documents per topic 5

par(mfrow=c(1,2), mar=c(0,0,2,2))
plotQuote(thoughts2, width=50, maxwidth=500, text.cex=0.5, main="Topic 2")
plotQuote(thoughts5, width=50, maxwidth=500, text.cex=0.5, main="Topic 5")


thoughts215 <- findThoughts(mod.15,texts=usdiss3tk$Abstract, topics=6, n=3)$docs[[1]]# select 3 representative documents per topic 2
thoughts515 <- findThoughts(mod.15,texts=usdiss3tk$Abstract, topics=8, n=3)$docs[[1]]# select 3 representative documents per topic 5

par(mfrow=c(1,2), mar=c(0,0,2,2))
plotQuote(thoughts215, width=50, maxwidth=500, text.cex=0.5, main="Topic 6")
plotQuote(thoughts515, width=50, maxwidth=500, text.cex=0.5, main="Topic 8")




# Topic correlation shows relations between topics based on the proportions of words they have in common:

mod.out.corr <- topicCorr(mod.10)
plot(mod.out.corr)

mod15.out.corr <- topicCorr(mod.15)
plot(mod15.out.corr)



# Stm offers two options for estimating topic correlations - simple and huge (more complex). The “simple” method simply thresholds the covariances whereas “huge” uses the semiparametric procedure in the package. Let’s compare the two procedures:

corrsimple <- topicCorr(mod.10, method = "simple", verbose = FALSE)
corrhuge <- topicCorr(mod.10, method = "huge", verbose = FALSE)
par(mfrow=c(1,2), mar=c(0,0,2,2))
plot(corrsimple, main = "Simple method")
plot(corrhuge, main = "Huge method")

corrsimple15 <- topicCorr(mod.15, method = "simple", verbose = FALSE)
corrhuge <- topicCorr(mod.15, method = "huge", verbose = FALSE)
par(mfrow=c(1,2), mar=c(0,0,2,2))
plot(corrsimple, main = "Simple method")
plot(corrhuge, main = "Huge method")



# We use gggraph to produce a more accurate visualization reflecting the topic proportion and importance of correlation
# Simple method:

# extract network 
stm_corrs <- get_network(model = mod.10,
                         method = 'simple',
                         labels = paste('Topic', 1:10),
                         cutoff = 0.001,
                         cutiso = TRUE)

#[This does not work; I get only topic 1 & and 9 & 10]
# Sin e extraction does not work, ggraph does not work either

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
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation', title = "Simple method") + 
  scale_edge_width(range = c(1, 3)) +
  theme_graph()

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
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation', title = "Huge method") +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()


# The “perspective” argument enables to compare topics two by two. For instance, we can compare the two “brevities” topics:

plot(mod.10, type="perspectives", topics=c(3, 10))
plot(mod.15, type="perspectives", topics=c(3, 10))
# Not ideal because words get cut

# Word clouds provide an intuitive, though less rigorous way of visualizing word prevalence in topics.

# First line of the script prepares for splitting into two rows to display two word clouds
par(mfrow=c(1,2), mar=c(0,0,2,2))
cloud(mod.10, topic = 1, scale = c(4, 0.4))
cloud(mod.10, topic = 5, scale = c(4, 0.4))

par(mfrow=c(1,2), mar=c(0,0,2,2))
cloud(mod.10, topic = 2, scale = c(4, 0.4))
cloud(mod.10, topic = 7, scale = c(4, 0.4))

par(mfrow=c(1,2), mar=c(0,0,2,2))
cloud(mod.15, topic = 3, scale = c(4, 0.4))
cloud(mod.15, topic = 10, scale = c(4, 0.4))

# First line of the script prepares for splitting into four rows to display four word clouds
par(mfrow=c(2,4), mar=c(0,0,4,4))
cloud(mod.10, topic = 1, scale = c(4, 0.4))
cloud(mod.10, topic = 5, scale = c(4, 0.4))
cloud(mod.10, topic = 6, scale = c(4, 0.4))
cloud(mod.10, topic = 10, scale = c(4, 0.4))

# interactively visualize an LDA topic model
stm::toLDAvis(mod.10, doc=out$documents)
stm::toLDAvis(mod.15, doc=out$documents)

# save objects in .RData file
save.image('USdiss.RData')
