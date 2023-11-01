
paper_populacion <- function(numero_patente, poblacion, indice){
  resumen <- (numero_patente/poblacion)*indice
  
  return(resumen)
}


PaperGDP_PPP <- function(numero_patente, GDP_PPP, indice){
  resultado <- (numero_patente/GDP_PPP) * indice
  
  return(resultado)
}

info_paper <- tribble(~Paper_Indicador ,~Brazil ,~Chile ,~Argentina, ~Mexico,
        "População", patente_populacion(sum(BRA$Paper),211000000, 100000), 
        patente_populacion(sum(CH$Paper),18000000, 100000), patente_populacion(sum(ARG$Paper), 44940000, 100000), 
        patente_populacion(sum(MX$Paper),127600000, 100000), "GDP_PPP", PatenteGDP_PPP(sum(BRA$Paper), 3154000000000, 1000000000), 
        PatenteGDP_PPP(sum(CH$Paper), 478199000000, 1000000000) , PatenteGDP_PPP(sum(ARG$Paper), 942367000000, 1000000000), 
        PatenteGDP_PPP(sum(MX$Paper), 2428000000000, 1000000000))

info_paper <- info_paper %>% 
  gather(Brazil:Mexico, key= "Paises", value= "valor") %>% 
  mutate(Paises = fct_reorder(Paises, desc(valor)))


colores <- c("#feb24c", "#7fcdbb")
indicadores <- ggplot(data = info_paper)+
  geom_col(aes(x = Paises, y = valor , fill = Paper_Indicador), position = "dodge")+
  labs(x= "", y="")+
  scale_fill_manual(name = "", values = colores, labels= c("GDP PPP", "População"))+
  geom_text(aes(x = Paises, y = valor, label = round(valor), group = Paper_Indicador),
            position=position_dodge(width=0.9), size = 4,vjust=-0.5)+
  theme_classic()

indicadores
