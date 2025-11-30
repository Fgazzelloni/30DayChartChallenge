

words<-df1$occupation
d <- lapply(words, function(x) words[agrep(x, words,
                                           max.distance=list(all=2, insertions=0, deletions=0, substitutions=2))])
names(d) <- words

library("stringdist") 
words<-tolower(words)
words<-words[1:20]
sdm = stringdistmatrix(words, words, useNames=T,method='jaccard')

sdm_dist = as.dist(sdm) # convert to a dist object (you essentially already have distances calculated)
plot(hclust(sdm_dist))        

library(tidygraph)

#df2 <-df1%>%filter(!is.na(education))
hs_graph <- as_tbl_graph(df1, directed = FALSE)
hs_graph

library(ggraph)
ggraph(hs_graph) + 
  geom_edge_link() + 
  geom_node_point()


ggraph(hs_graph, layout = "drl") + 
  geom_edge_link() + 
  geom_node_point()

ggraph(hs_graph, layout = "stress") + 
  geom_edge_link() + 
  geom_node_point(
    aes(filter = centrality_degree() > 2, 
        colour = centrality_power()),
    size = 4
  )

df1%>%
  mutate(occupation=as.character(occupation), 
         occupation=case_when(occupation=str_detect(occupation,"Computer")~"Computer",
                              occupation=str_detect(occupation,"Aerospace")~"Aerospace",
                              occupation=str_detect(occupation,"Acc")~"Accounting",
                              occupation=str_detect(occupation,"Agri")~"Agricultural",
                              occupation=str_detect(occupation,"Anthro")~"Anthropology",
                              occupation=str_detect(occupation,fixed("mathematics",ignore_case=TRUE))~"Mathematics",
                              occupation=str_detect(occupation,fixed("Archit",ignore_case=TRUE))~"Architectural",
                              occupation=str_detect(occupation,"Art")~"Arts",
                              occupation=str_detect(occupation,"Engineering")~"Engineering",
                              occupation=str_detect(occupation,fixed("physics",ignore_case=TRUE))~"Physics",
                              occupation=str_detect(occupation,fixed("Atmospheric",ignore_case=TRUE))~"Atmospheric",
                              occupation=str_detect(occupation,fixed("Bioengineer",ignore_case=TRUE))~"Bioengineering",
                              occupation=str_detect(occupation,fixed("Bio",ignore_case=TRUE))~"Biological",
                              occupation=str_detect(occupation,"Bioengineering")~"Biological",
                              occupation=str_detect(occupation,fixed("Chem",ignore_case=TRUE))~"Chemical",
                              TRUE~occupation)) %>% count(occupation) #%>%View
#filter(str_detect(occupation,fixed("Chem",ignore_case=TRUE))) %>% count(occupation)

library(tidytext)
library(wordcloud)
set.seed(123)
df1%>%
  mutate(occupation=as.character(occupation))%>%
  unnest_tokens(word, occupation) %>%  
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE) %>%
  #filter(!str_detect(word,"[A-z]")) %>%
  #count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100,
                 color="violet"))


df1%>%
  mutate(occupation=as.character(occupation))%>%
  unnest_tokens(word, occupation)%>%  
  anti_join(get_stopwords())%>%
  count(word, sort = TRUE)


academia <- readxl::read_excel(here::here("~/Documents/R/WomenInSTEM/nsb20212-tabslbr-026_academia.xlsx"), 
                               na = "0", 
                               skip = 3)


academia1 <- academia %>%
  mutate(across(-`Position, sex, and field`,as.numeric)) %>%
  pivot_longer(cols = 2:22,names_to="year",values_to="values")%>%
  distinct() %>%
  mutate(year=as.integer(year)) %>%
  janitor::clean_names() %>%
  pivot_wider(names_from=position_sex_and_field,
              values_from=values,
              values_fn = {mean}) 


academia1[is.na(academia1)]<-0

academia1%>%DataExplorer::profile_missing()

academia1%>%head

order_education <- df1 %>%
  group_by(education) %>%
  summarise(avg_tot_edu = mean(tot_edu)) %>%
  ungroup() %>%
  arrange(-avg_tot_edu) %>%
  distinct(education) %>%
  mutate(index_edu = row_number())

order_occupation <- df1 %>%
  group_by(occupation) %>%
  summarise(avg_tot_occ = mean(tot_occ)) %>%
  ungroup() %>%
  arrange(-avg_tot_occ) %>%
  distinct(occupation) %>%
  mutate(index_occ = row_number())


df2 <- df1 %>%
  left_join(order_education)%>%
  left_join(order_occupation)%>%
  mutate(index=row_number())%>%
  relocate(index)

df2%>%glimpse

library(modelr)
df2%>%
  select(occupation,female_occ,male_occ)%>%
  mutate()
mutate(female_occ=-female_occ)%>%
  arrange(-male_occ)%>%
  slice(1:10)%>%
  pivot_longer(cols=c("female_occ","male_occ"),names_to="gender",values_to="values_occ") %>%
  ggplot(aes(x=values_occ, y=occupation))+
  geom_col()+
  modelr::geom_ref_line(h = 0,size = 2, colour = "white") +
  geom_vline(aes(xintercept=0))
