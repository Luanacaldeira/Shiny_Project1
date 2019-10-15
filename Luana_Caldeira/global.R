
# Libraries ####


library(readxl)
library(stringr)
library(plyr)
library(dplyr)

library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)

library(shiny)
library(shinydashboard)



# Reads excel worksheets ####


read_sheets_excel = function(filepath, sheet_names) {
  x = lapply(sheet_names, function (x) readxl::read_excel(filepath,sheet = x,
                                                          col_names = FALSE,
                                                          skip = 7))
  names(x) = sheet_names
  x = lapply(x, as.data.frame)
  x

}




lst_snames = c("creche_regiao",
               "creche_sexoraca",
               "creche_idade",
               "pre_escola_regiao",
               "pre_escola_sexoraca",
               "pre_escola_idade",
               "fund_inic_regiao",
               "fund_inic_sexoraca",
               "fund_inic_idade",
               "fund_final_regiao",
               "fund_final_sexoraca",
               "fund_final_idade",
               "medio_regiao",
               "medio_sexoraca",
               "medio_idade")




lst_filepath = c("./data/sinop_basica_2018.xlsx",
                 "./data/sinop_basica_2017.xlsx",
                 "./data/sinop_basica_2016.xlsx",
                 "./data/sinop_basica_2015.xlsx",
                 "./data/sinop_basica_2014.xlsx",
                 "./data/sinop_basica_2013.xlsx",
                 "./data/sinop_basica_2012.xlsx",
                 "./data/sinop_basica_2011.xlsx",
                 "./data/sinop_basica_2010.xlsx")



sinop_basica = list()

for (path in lst_filepath){
  year = str_extract(path, "\\d{4}")
  sinop_basica[[year]] = read_sheets_excel(path, lst_snames)
}





# ID/Year and join dfs ####

basica = list()

for(path in lst_filepath){
  year = str_extract(path, "\\d{4}")

  for (sname in lst_snames){

    basica[[year]][[sname]] = sinop_basica[[year]][[sname]] %>%
      mutate(., col_id = paste(...1, ...2, ...3, sep = "_")) %>%
      select(., col_id, reg = ...1, est = ...2, mun = ...3, everything()) %>%
      mutate_each(funs(as.numeric), starts_with("...")) %>%
      filter(., !is.na(mun))

  }
}



join = list()

for (path in lst_filepath){
  year = str_extract(path, "\\d{4}")

  join[[year]] = join_all(basica[[year]],
                          by = c("col_id", "reg", "est", "mun"),
                          match = "all")
}




# Rename colunms ####


col_name_l1 = c("Creche",
                "Preescola",
                "Inicial",
                "Final",
                "Medio")

rep_l1 = c(30, 30, 33, 33, 32)



col_name_l2 = c("Total",
                "Urbana",
                "Rural",
                "Sexoraca_Total",
                "Sexoraca_Fem",
                "Sexoraca_Masc")

rep_l2 = c(1, 5, 5, 1, 7, 7)



col_name_l3 = c("Geral",
                "Total,Federal,Estadual,Municipal,Privada",
                "Geral",
                "Total,NaoDeclarada,Branca,Preta,Parda,Amarela,Indigina")

rep_l3 = c(1, 2, 1, 2)



col_name_idade = c("Total",
                   "Ate3anos",
                   "4a5anos",
                   "6a10anos",
                   "11a14anos",
                   "15a17anos",
                   "18a19anos",
                   "20a24anos",
                   "25anosoumais")

idade1 = paste("Idade",
               c(col_name_idade[1:4]),
               sep = "_")

idade2 = paste("Idade",
               c(col_name_idade[1], col_name_idade[3:8]),
               sep = "_")

idade3 = paste("Idade",
               c(col_name_idade[1], col_name_idade[4:9]),
               sep = "_")

idade4 = paste("Idade",
               c(col_name_idade[1], col_name_idade[5:9]),
               sep = "_")




colnamesaux = paste(rep(col_name_l2, rep_l2),
                    unlist(strsplit(rep(col_name_l3, rep_l3, sep = "_"), ",")),
                    sep = "_")

col_names = c("ID", "Regiao", "Estado", "Municipio",
              paste(rep(col_name_l1, rep_l1),
                    c(colnamesaux, idade1, colnamesaux, idade1, colnamesaux,
                      idade2,colnamesaux, idade3, colnamesaux, idade4),
                    sep = "_"))


for (path in lst_filepath){
  year = str_extract(path, "\\d{4}")
  colnames(join[[year]]) = col_names
}


basic_education = bind_rows(join, .id = "Year")


populacao = readxl::read_excel("./data/brasil_populacao.xls",sheet = "Populacao",
                    col_names = TRUE,
                    skip = 0)


Populacao_municipio = readxl::read_excel("./data/PIB_por_Regiao.xls",
                                         sheet = "Populacao_Municipio",
                                         col_names = TRUE,
                                         skip = 0)




# Choices List ####


group_list = list("Nursery" = "Creche_Total_Geral",
                  "Preschool" = "Preescola_Total_Geral",
                  "Elementary" = "Inicial_Total_Geral",
                  "Middle School" = "Final_Total_Geral",
                  "High School" = "Medio_Total_Geral")


education_stages_limit = c("Creche",
                           "Preescola",
                           "Inicial",
                           "Final",
                           "Medio")

education_stages_limit2 = c("Creche_Total_Geral",
                           "Preescola_Total_Geral",
                           "Inicial_Total_Geral",
                           "Final_Total_Geral",
                           "Medio_Total_Geral")



education_stages_label = c("Nursery",
                           "Preschool",
                           "Elementary",
                           "Middle School",
                           "High School")


region_list = list("Brazil" = "all",
                   "North" = "Norte",
                   "Northeast" = "Nordeste",
                   "Central-West" = "Centro-Oeste",
                   "South-East" = "Sudeste",
                   "South" = "Sul")

region_limit = c("Norte",
                 "Nordeste",
                 "Centro-Oeste",
                 "Sudeste",
                 "Sul")

region_label = c("North",
                 "Northeast",
                 "Central-West",
                 "South-East",
                 "South")

age_label = c("0-3",
               "4-5",
               "6-10",
               "11-14",
               "15-17",
               "18-19",
               "20-24",
               "25-40")


race_limit = c("Creche_Sexoraca_NaoDeclarada",
               "Creche_Sexoraca_Branca",
               "Creche_Sexoraca_Parda",
               "Creche_Sexoraca_Preta",
               "Creche_Sexoraca_Amarela",
               "Creche_Sexoraca_Indigina")


race_label = c("ND",
               "White",
               "Mestizo",
               "Black",
               "Asian",
               "Amerindian")


year_list = sort((unique(basic_education$Year)))
names(year_list) = sort(unique(basic_education$Year))
year_list = as.list(year_list)










# Data Plot 4 ####

byRegionAge_df = basic_education %>% 
  select(., Year, Regiao, Estado, Municipio,
         contains("Idade"), -contains("Total")) %>% 
  reshape2::melt(., id.vars = c("Year", "Regiao", "Estado", "Municipio")) %>% 
  mutate(., age = str_extract(variable, "([^_]+)$"),
         age = ifelse(age %in% c( "Ate3anos", "4a5anos", "6a10anos",
                                  "11a14anos", "15a17anos"), "ate18anos",
                      "18anosoumais"),
         Year = as.numeric(Year)) %>% 
  filter(., age == "ate18anos", Year <= 2016) %>% 
  select(., -variable, -age) %>% 
  group_by(., Year, Municipio) %>% 
  summarise(., student = sum(value))


Populacao_municipio_selec = as.data.frame(Populacao_municipio) %>% 
  filter(., !is.na(Municipio))






