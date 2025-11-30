employed2 <-employed1%>%
arrange(education)%>%
  mutate(occupation=education,.after=education)


df <- scientists1 %>%
  full_join(employed2,by="occupation") %>%
  distinct()%>%
  relocate(education) 

df1 <- df%>%
  filter(!str_detect(occupation,"All"))%>%
  rename(tot_occ=total.x,
         female_occ=female.x,
         female_occ_pct=female_perc.x,
         male_occ=male.x,
         male_occ_pct=male_perc.x,
         
         tot_edu=total.y,
         female_edu=female.y,
         female_edu_pct=female_perc.y,
         male_edu=male.y,
         male_edu_pct=male_perc.y) %>%
  mutate(occupation=as.factor(occupation),
         education=as.factor(education),
         occupation=fct_reorder(occupation,-tot_occ),
         education=fct_reorder(education,-tot_edu))

order_education <- df1 %>% #dim
  group_by(education) %>%
  summarise(avg_tot_edu = mean(tot_edu)) %>%
  ungroup() %>% #dim
  arrange(-avg_tot_edu) %>%
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

df3 <- df2%>%
  filter(!occupation=="Prekindergarten and kindergarten",
         !occupation=="Elementary",
         !str_detect(occupation,"Other"))%>%
  mutate(occupation=as.character(occupation))%>%
  mutate(occupation=case_when(occupation=="Secretaries, receptionists, typists"~"Secretaries",
                              str_detect(occupation,"Registered")~"Nurses",
                              str_detect(occupation,"Counselors")~"Counselors",
                              TRUE~occupation)) %>%
  select(occupation,female_occ_pct,male_occ_pct)%>%
  mutate(female_occ_pct=-female_occ_pct)%>%
  mutate(occupation=as.factor(occupation))

library(extrafont)
# loadfonts()

# barplot1 ---------
library(modelr)
df4 <- df3%>%
  arrange(female_occ_pct)%>%
  slice(1:10)%>%
  pivot_longer(cols=c("female_occ_pct","male_occ_pct"),
               names_to="gender",values_to="values_occ_pct") %>%
  mutate(label=as.character(values_occ_pct),
         label=gsub("-","",label),
         label=paste0(label,"%"))



  
 barplot1 <-  df4%>%
  ggplot(aes(x=values_occ_pct, y=occupation))+
  geom_col(width = 0.4)+
  modelr::geom_ref_line(v = 0,size = 2, colour = "white") +
  geom_text(aes(label=label),
            nudge_x = 0.5 ,vjust=-0.8,hjust=1.2)+
  scale_x_continuous(expand = c(0.15,1))+
    labs(x="Occupational by gender")+
  theme_classic()+
    theme(text = element_text(family = "Roboto Condensed"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank())

# barplot2 --------
 df5 <-df3%>%
   select(occupation,female_occ_pct,male_occ_pct)%>%
   mutate(female_occ_pct=-female_occ_pct)%>%
   arrange(-male_occ_pct)%>%
   slice(1:10)%>%
   pivot_longer(cols=c("female_occ_pct","male_occ_pct"),
                names_to="gender",values_to="values_occ_pct") %>%
   mutate(label=as.character(values_occ_pct),
          label=gsub("-","",label),
          label=paste0(label,"%"))
 
 
 
 barplot2 
 
 df5%>%
   ggplot(aes(x=values_occ_pct, y=occupation))+
   geom_col(width = 0.4,fill="grey9")+
   modelr::geom_ref_line(v = 0,size = 2, colour = "white") +
   geom_text(aes(label=label),
             family = "Roboto Condensed",
             #color="grey80",
             nudge_x = 0.5 ,vjust=-0.8,hjust=1.2)+
   scale_x_continuous(expand = c(0.9,1))+
   labs(x="Occupational by gender")
   theme_classic()+
   theme(text = element_text(family = "Roboto Condensed",
                             color="grey80"),
         axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_text(family = "Roboto Condensed",
                                    color="grey80"),
         axis.title.y = element_blank(),
         plot.background = element_rect(fill="grey20",color="grey20"),
         panel.background = element_rect(fill="grey20",color="grey20"))


library(patchwork)

 barplot1/barplot2
