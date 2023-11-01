#---------------------------------------------------#
# Pipeline                                          #
#---------------------------------------------------#

colitions_list <- function(dados){
  dados <- dados %>% 
    janitor::clean_names() %>%
    drop_na(applicants) %>% 
    mutate(applicants = gsub(";;", ";", applicants)) %>% 
    select(x,jurisdiction,publication_date,publication_year,
           application_date,title,applicants,inventors,
           cites_patent_count, cited_by_patent_count)
  
  val <- strsplit(dados$applicants, ";")
  val <- lapply(val, unique)
  
  first_player <-sapply(val,function(x) x[1])
  second_player <-sapply(val,function(x) x[2])
  third_player <- sapply(val,function(x) x[3])
  
  total <- data.frame(cbind(first_player,second_player,third_player))
  
  one_colition <- total %>% 
    filter(!complete.cases(total$second_player))
  
  grand_colition <- total %>% 
    filter(complete.cases(total$second_player)) 
  
  two_colition <- grand_colition %>% 
    filter(!complete.cases(grand_colition$third_player))
  
  grand_colition <- grand_colition %>% 
    filter(complete.cases(grand_colition$third_player))
  
  return(list(one_colition,two_colition,grand_colition))
  
}

classification <- function(data){
  
  colitions <- colitions_list(data)
  
  one_colition <- colitions [[1]] %>% 
    mutate(helix = ifelse(str_detect(first_player, univ),"University", "Industry")) %>% 
    mutate(helix = ifelse(str_detect(first_player, gov),"Goverment", helix)) %>% 
    mutate(helix = ifelse(str_detect(first_player, ind), "Industry", helix))
  
  two_colition <- colitions [[2]] %>% 
    mutate(helix = ifelse((str_detect(first_player,"UNIV"))& (str_detect(second_player, "UNIV")), "University", "Industry")) %>% 
    mutate(helix = ifelse((str_detect(first_player,gov))& (str_detect(second_player, gov)), "Goverment", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player,ind))& (str_detect(second_player, ind)), "Industry", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind))& (str_detect(second_player, univ)), "UI", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ))& (str_detect(second_player, ind)), "UI", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind))& (str_detect(second_player, gov)), "IG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, ind)),"IG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, univ)),"UG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ))& (str_detect(second_player, gov)),"UG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player,"UNIV"))& (str_detect(second_player, "UNIV")), "University", helix)) 
  
  grand_colition <- colitions[[3]] %>% 
    mutate(helix = ifelse((str_detect(first_player, 'UNIV')) & (str_detect(second_player, "UNIV")) &  (str_detect(third_player, "UNIV")), "University", "Industry")) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov)) & (str_detect(second_player, gov)) &  (str_detect(third_player, gov)), "Goverment", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind)) & (str_detect(second_player, ind)) &  (str_detect(third_player, ind)), "Industry", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ)) & (str_detect(second_player, univ)) &  (str_detect(third_player, gov)), "UG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ))& (str_detect(second_player, gov)) &  (str_detect(third_player, univ)), "UG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, univ)) &  (str_detect(third_player, univ)), "UG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind)) & (str_detect(second_player, univ)) &  (str_detect(third_player, univ)), "UI", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ)) & (str_detect(second_player, ind)) &  (str_detect(third_player, univ)), "UI", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ)) & (str_detect(second_player, univ)) &  (str_detect(third_player, ind)), "UI", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind)) & (str_detect(second_player, ind)) &  (str_detect(third_player, gov)), "IG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind))& (str_detect(second_player, gov)) &  (str_detect(third_player, ind)), "IG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, ind)) &  (str_detect(third_player, ind)), "IG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ))& (str_detect(second_player, gov)) &  (str_detect(third_player, ind)), "UIG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, univ)) &  (str_detect(third_player, ind)), "UIG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, univ))& (str_detect(second_player, ind)) &  (str_detect(third_player, gov)), "UIG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind))& (str_detect(second_player, univ)) &  (str_detect(third_player, gov)), "UIG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, gov))& (str_detect(second_player, ind)) &  (str_detect(third_player, univ)), "UIG", helix)) %>% 
    mutate(helix = ifelse((str_detect(first_player, ind))& (str_detect(second_player, gov)) &  (str_detect(third_player, univ)), "UIG", helix)) 
  
  prueba <- data %>% 
    janitor::clean_names() %>% 
    drop_na(applicants) %>% 
    mutate(applicants = gsub(";;", ";", applicants)) %>% 
    mutate(applicants = gsub("^([^;]*;[^;]*;[^;]*).*$", "\\1", applicants)) %>% 
    arrange(applicants)
  
  grand_colition %>% 
    unite(applicants,first_player,second_player,third_player, sep = ";") -> grand_colition
  
  two_colition %>% 
    unite(applicants, first_player, second_player, sep = ";") %>% 
    select(applicants, helix) -> two_colition
  
  one_colition %>% 
    select(first_player, helix) %>% 
    rename(applicants  = first_player) -> one_colition
  
  colitions_total <- bind_rows(one_colition, two_colition, grand_colition) %>% 
    arrange(applicants)
  
  colitions_total$x <- prueba$x
  
  prueba <- prueba %>% 
    left_join(colitions_total, by = c("x", "applicants")) 
  
  
  return(prueba)
  
}


#---------------------------------------------------#
# Brazil                                            #
#---------------------------------------------------#

univ <- "UNIV|UNICAMP|UBEA|UNIAO BRASILEIRA|FACULDADES|FUNDACAO UNIV|FUNDACAO UNIVERSIDADES|FUNDACAO UNIVERSIDADEFUNDACAO UNIV|FUNDACAO UNIVERSIDADES|ASSOCIACAO EDUCACIONAL NOVE DE JULHO"
gov <- "PETROLEO|INST|FUNDACAO|CBPF|CELPE|EMBRAPA|PETROBRAS|CAPES|COMISSAO|SIDERURGICA|SECRETARIA DO ESTADO|BUTANTAN|CENTRAIS ELETRICAS|APC"
ind <- "LTDA|INC|SA|CO|CORP|LTD"


BRAZIL <- bind_rows(BR2010_2012,BR2013_2014, BR2015, BR2016, BR2017_primeira_parte, BR2017_segunda_parte, BR2018, BR2019, BR2020)
rm(BR2010_2012,BR2013_2014, BR2015, BR2016, BR2017_primeira_parte, BR2017_segunda_parte, BR2018, BR2019, BR2020)

BRAZIL <- classification(BRAZIL)

save(BRAZIL, file = "BRAZIL.RData")


#---------------------------------------------------#
# Mexico                                            #
#---------------------------------------------------#

univ <- "UNIV"
gov <- "INST|SECRETARIA|COMISION"
ind <- "LTDA|INC|SA|CO|CORP|LTD"

MEXICO <- bind_rows(MX2010_2012,MX2013_2015,MX2016_2018,MX2019_2020)
rm(MX2010_2012,MX2013_2015,MX2016_2018,MX2019_2020)

MEXICO <- classification(MEXICO)


MEXICO %>% 
  group_by(helix) %>% 
  summarise(n=n())


MEXICO  %>% 
  filter(grepl(";", applicants)) -> MX


save(MEXICO, file="MEXICO.RData")

#---------------------------------------------------#
# Argentina                                         #
#---------------------------------------------------#

univ <- "UNIV"
gov <- "INST|INTI|COMISION"
ind <- "LTDA|INC|SA|CO|CORP|LTD"

ARG2010_2020 <- classification(ARG2010_2020) 

ARG2010_2020 %>% 
  group_by(helix) %>% 
  summarise(n=n())

save(ARG2010_2020, file = "ARGENTINA.RData")

#---------------------------------------------------#
# Chile                                             #
#---------------------------------------------------#

univ <- "UNIV"
gov <- "INST|INTA"
ind <- "LTDA|INC|SA|CO|CORP|LTD"

CH2010_2020 <- classification(CH2010_2020) 

CH2010_2020 %>% 
  group_by(helix) %>% 
  summarise(n=n())


CH2010_2020 %>% 
  filter(grepl(";", Applicants)) ->CH


save(CH2010_2020, file = "CHILE.RData")
