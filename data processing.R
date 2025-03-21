library(tidyverse)

raw<-read.table("data/birdwatch.txt",sep = "\t")
lad<-read.csv("data/georef-united-kingdom-local-authority-district.csv",sep=";")
lad$Official.Name.Local.authority.district<-str_replace_all(lad$Official.Name.Local.authority.district,",",", ")






text_remove<-str_split_fixed(str_split_fixed(raw$V1,"[0-9]",2)[,1]," ",2)
colnames(text_remove)<-c("Species","County")

number_remove<-str_split_fixed(str_remove(raw$V1,pattern = str_split_fixed(raw$V1,"[0-9]",2)[,1])," ",9)
colnames(number_remove)<-c("Mean_2024","Rank_2024","Per_2024","Mean_2023","Rank_2023","Per_2023","Percent_Change","ChangeRank","ChangePer")

bird_data<-data.frame(text_remove,number_remove) %>%
  mutate(County=trimws(County),Species=trimws(Species)) %>%
  mutate(    Mean_2024=as.numeric(Mean_2024),
             Rank_2024=as.numeric(Rank_2024),
             Per_2024 =as.numeric(Per_2024),
             Mean_2023=as.numeric(Mean_2023),
             Rank_2023=as.numeric(Rank_2023),
             Per_2023 =as.numeric(Per_2023)) %>%
  filter(County!="Lincolnshire (part of)")





birds_2024<-bird_data %>% 
  dplyr::select(Species:Mean_2024) %>%
  spread(Species,Mean_2024)

county<-lad %>%
  distinct(Official.Name.County.Unitary.district,Official.Name.Region,Official.Name.Country)

scotland<-inner_join(birds_2024,county,by=c("County"="Official.Name.County.Unitary.district")) %>%
  filter(Official.Name.Country=="Scotland")

saveRDS(list(scotland=scotland,birds_2024=birds_2024,bird_data=bird_data),"data/data.RDS")

###

all_data<-read.csv("C:/Users/SamDumble/Downloads/archive(4)/BioTIMEQuery_24_06_2021.csv")
study_56<-dplyr::filter(all_data,STUDY_ID==56)

rm(all_data)

library(tidyverse)
study_56<-study_56 %>%
            group_by(YEAR,GENUS,SPECIES) %>%
              summarise(ABUNDANCE=sum(sum.allrawdata.ABUNDANCE),
                        BIOMASS=sum(sum.allrawdata.BIOMASS)) %>%
  filter(GENUS!="remi")


diversity_56<-study_56 %>% 
  group_by(YEAR) %>%
  summarise("Species Richness"=sum(ABUNDANCE>0),"Total Abundance"=sum(ABUNDANCE),
            "Dominance"=max(ABUNDANCE/sum(ABUNDANCE)),
            Shannon=vegan::diversity(as.numeric(ABUNDANCE),index="shannon"),
            Simpson=vegan::diversity(as.numeric(ABUNDANCE),index="simpson"),
            PilouJ=Shannon/log(`Species Richness`))



diversity_56<-study_56 %>% 
  group_by(YEAR) %>%
  summarise("Species Richness"=sum(BIOMASS>0),"Total Biomass"=sum(BIOMASS),
            "Dominance"=max(BIOMASS/sum(BIOMASS)),
            Shannon=vegan::diversity(as.numeric(BIOMASS),index="shannon"),
            Simpson=vegan::diversity(as.numeric(BIOMASS),index="simpson"),
            PilouJ=Shannon/log(`Species Richness`))

                        
ggplot(diversity_56,aes(y=Simpson,x=YEAR )) +
    geom_point()+
    geom_line()


ggplot(diversity_56,aes(y=PilouJ,x=YEAR )) +
  geom_point()+
  geom_line()


saveRDS(list(scotland=scotland,birds_2024=birds_2024,bird_data=bird_data,study_56=study_56),"data/data.RDS")

#Small Mammal Mark-Recapture Population Dynamics at Core Research Sites
