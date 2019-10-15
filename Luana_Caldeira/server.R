


shinyServer(function(input, output){
    
  
  
  
  # Reactive Expressions ####
  
  region_selection = reactive({
    
    basic_education %>% 
      filter(., if (input$region == "all") Regiao %in% unique(Regiao) else Regiao == input$region) %>%
      group_by(., Year)
    
  })
  
  
  
  datalag = reactive({
    
    lag_df = basic_education %>%
      filter(., if (input$region == "all") Regiao %in% unique(Regiao) else Regiao == input$region) %>%
      select(., Year, Regiao, Municipio,
             contains("Urbana_Total"), contains("Rural_Total")) %>%
      group_by(., Year, Regiao) %>% 
      summarise_if(., is.numeric, sum) 
    
    
    crechelag = lag_df %>% 
      filter(., Year == input$startyear) %>% 
      group_by(., Year) %>% 
      summarise(., Urbana = sum(Creche_Urbana_Total), Rural = sum(Creche_Rural_Total)) %>% 
      mutate(., Group = "Creche")
    
    preescolalag = lag_df %>% 
      filter(., Year == (as.numeric(input$startyear) + as.numeric(input$gap1))) %>% 
      group_by(., Year) %>% 
      summarise(., Urbana = sum(Preescola_Urbana_Total), Rural = sum(Preescola_Rural_Total)) %>% 
      mutate(., Group = "Preescola")
    
    iniciallag = lag_df %>% 
      filter(., Year == (as.numeric(input$startyear) + as.numeric(input$gap1) +
                           as.numeric(input$gap2))) %>% 
      group_by(., Year) %>% 
      summarise(., Urbana = sum(Inicial_Urbana_Total), Rural = sum(Inicial_Rural_Total)) %>% 
      mutate(., Group = "Inicial")
    
    finallag = lag_df %>% 
      filter(., Year == (as.numeric(input$startyear) + as.numeric(input$gap1) +
                           as.numeric(input$gap2) + as.numeric(input$gap3))) %>% 
      group_by(., Year) %>% 
      summarise(., Urbana = sum(Final_Urbana_Total), Rural = sum(Final_Rural_Total)) %>% 
      mutate(., Group = "Final")
    
    mediolag = lag_df %>% 
      filter(., Year == (as.numeric(input$startyear) + as.numeric(input$gap1) +
                           as.numeric(input$gap2) + as.numeric(input$gap3) +
                           as.numeric(input$gap4))) %>% 
      group_by(., Year) %>% 
      summarise(., Urbana = sum(Medio_Urbana_Total), Rural = sum(Medio_Rural_Total)) %>% 
      mutate(., Group = "Medio")
    
    
    dadoslag = rbind(crechelag, preescolalag, iniciallag, finallag, mediolag) %>% 
      reshape2::melt(., id.vars = c("Year", "Group")) 
    
  })
  

  
  # Plot 1 ####
  
    output$plot1 = renderPlot({
    
    
      region_selection() %>% 
        filter(., Year >= input$slider[1] & Year <= input$slider[2]) %>% 
        
        summarise(., Creche_Total_Geral = sum(Creche_Total_Geral),
                    Preescola_Total_Geral = sum(Preescola_Total_Geral),
                    Inicial_Total_Geral = sum(Inicial_Total_Geral),
                    Final_Total_Geral = sum(Final_Total_Geral),
                    Medio_Total_Geral = sum(Medio_Total_Geral)) %>% 
        select(., Year, input$checkGroup) %>%
        reshape2::melt(., id.vars = "Year") %>% 
        ggplot(., aes(x = Year, y = as.numeric(value))) +
        geom_bar(aes(fill = variable),
                 position = "dodge", stat = "identity") +
        theme(axis.text.x = element_text(angle = 90,
                                         size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(), legend.position = "right",
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4"),
              plot.title = element_text(size = 18, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 0),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Enrollment") +
        scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6),
                           limits = c(0,18000000), expand = c(0, 0)) +
        scale_fill_brewer(palette = "OrRd", limits = education_stages_limit2,
                          label = education_stages_label)
    
      
  })
    

    
    
    
    
    
    # Plot 2 ####
    
    output$plot2 = renderPlot({
      
      
      region_selection() %>%  
        select(., Year, contains("Sexoraca"), -contains("Total")) %>% 
        summarise_if(., is.numeric, sum) %>% 
        select(., Year, contains(str_extract(input$selecgroup, "[^_]+"))) %>%
        filter(., Year == input$year1 | Year == input$year2) %>% 
        reshape2::melt(., id.vars = "Year") %>% 
        mutate(., gender = ifelse(grepl("Fem", variable), "Female", "Male")) %>%
        mutate(., variable = gsub(c("_Masc_|_Fem_"), "_", variable)) %>% 
        ggplot(., 
               mapping = aes(x = variable, 
                             y = ifelse(test = gender == "Male",
                                        yes = -as.numeric(value),
                                        no = as.numeric(value)), 
                             fill = gender)) +
        geom_bar(stat = "identity", position = "identity",
                 aes(color = Year), alpha = 0.3, size = 0.8) +
        coord_flip() +
        theme(axis.text.x = element_text(angle = 90,
                                         size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.position = "right",
              plot.title = element_text(size = 18, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 0),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Enrollment by Race/Ethnicity") +
        scale_x_discrete( limits = gsub("Creche",
                                        str_extract(input$selecgroup, "[^_]+"),
                                        race_limit),
                          labels = race_label) +
        scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3)) +
        scale_fill_brewer(palette = "Set1")
      

        
     
      
      
    })
    

    # Plot 3 ####
    
    output$plot3 = renderPlot({
      
      
      region_selection() %>%
        select(., Year, contains("Idade"), -contains("Total")) %>% 
        summarise_if(., is.numeric, sum) %>% 
        filter(., Year == 2018) %>%  
        reshape2::melt(., id.vars = "Year") %>% 
        mutate(.,group = str_extract(variable, "([^_]+)"),
               age = str_extract(variable, "([^_]+)$")) %>%
        filter (., group %in% str_extract(input$checkGroup, "([^_]+)")) %>%
        ggplot(aes(x = age, y = value)) +
        geom_col(aes(fill = group))+ 
        theme(axis.text.x = element_text(angle = 90,
                                         size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(), legend.position = "right",
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4"),
              plot.title = element_text(size = 18, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 0),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Enrollment by Age (2018)") +
        scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6),
                           expand = c(0, 0)) +
        scale_x_discrete(name = NULL, limits = col_name_idade[2:9],
                         label = age_label) +
        scale_fill_brewer(palette = "OrRd", limits = education_stages_limit,
                          labels = education_stages_label)
      
        
        
        
      
      
    })
    
    
    
    
    #Plot 4 #### 
    
    output$plot4 = renderPlot({
      
      
      
      
      left_join(byRegionAge_df, Populacao_municipio_selec,
                           by = c("Year" = ("Ano"), "Municipio")) %>% 
        mutate(., student_ratio = student/Populaco_ate18anos,
               Nome_Grande_Regiao = ifelse(Nome_Grande_Regiao == "Centro-oeste",
                                           "Centro-Oeste", Nome_Grande_Regiao)) %>% 
        filter(., Year == 2010,
               student_ratio < 1, student_ratio > 0.5,
               if (input$region == "all") Nome_Grande_Regiao %in% unique(Nome_Grande_Regiao) else Nome_Grande_Regiao == input$region) %>% 
        ggplot(aes(x = student_ratio, y = PIB_per_Capita)) + 
        geom_point(aes(color = Nome_Grande_Regiao)) +
        geom_smooth(method = "lm") +
        theme(axis.text.x = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.position = "right",
              plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 1),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Enrollment Ratio vs. GDP per Capita (BRL)") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3),
                           expand = c(0, 0), limits = c(0, 100000)) +
        scale_color_brewer(palette = "Accent", labels =  c("Central-West",
                                                           "Northeast",
                                                           "North",
                                                           "South-East",
                                                           "South"),
                           limits = c("Centro-Oeste",
                                      "Nordeste",
                                      "Norte",
                                      "Sudeste",
                                      "Sul"))
      
        
       # scale_color_brewer(palette = "Accent")
      
      
      
      
    })
    
    
    
    #Plot 5 ####
    
    
    output$plot5 = renderPlot({
      
      
      basic_education %>% 
        filter(., if (input$region == "all") Regiao %in% unique(Regiao)
               else Regiao == input$region) %>% 
        select(., Year, Regiao, Municipio,
               contains("Urbana_Total"), contains("Rural_Total")) %>% 
        reshape2::melt(., id.vars = c("Year", "Regiao", "Municipio")) %>%
        mutate(., Group = str_extract(variable, "([^_]+)"),
               Local = str_extract(variable, "_(.*?)_"),
               Rural = ifelse(Local == "_Rural_", 1, 0)) %>%
        group_by(., Year, Regiao, Municipio) %>% 
        summarise(., ratio_Rural_Urbana = sum(value * Rural)/ sum(value)) %>% 
        ggplot(aes(x = Regiao, y = ratio_Rural_Urbana, fill = Regiao)) + 
        geom_boxplot(outlier.colour = "deepskyblue4", outlier.shape = 3,
                     outlier.size = 1, notch = FALSE) +
        stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "dodgerblue4") +
        theme(axis.text.x = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 1),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Students Attending School in Rural Areas") +
        scale_x_discrete(limits = region_limit, labels = region_label) +
        scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
        scale_fill_brewer(palette = "Spectral")
      
      
      
    })
    
    
    
    
    
    
    #Plot 6 ####
    
    
    output$plot6 = renderPlot({
      
     
      
      datalag() %>% 
        ggplot(., aes(x = Group, y = value)) +
        geom_bar(aes(fill = variable), stat = "identity", position = "stack") +
        theme(axis.text.x = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.position = c(0.15, 0.83),
              plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 1),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Dropout Estimation") +
        scale_x_discrete(limits = col_name_l1, labels = education_stages_label) +
        scale_y_continuous(label = unit_format(unit = "m", scale = 1e-6),
                           limits = c(0,18000000), expand = c(0, 0)) +
        scale_fill_brewer(palette = "OrRd", labels = c("Urban Area", "Rural Area"))
      
      
      
      
      
      
        
      
    })
    
    
    
    
    #Output Plot 7
    
    
    output$plot7 = renderPlot({
      
      datalag() %>% 
        ggplot(., aes(x = Group, y = value)) +
          geom_bar(aes(fill = variable), stat = "identity", position = "fill") +
        theme(axis.text.x = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              axis.text.y = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 10,
                                         color = "deepskyblue4",
                                         face = "bold"),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold",
                                        hjust = 0.5, color = "deepskyblue4"),
              plot.caption = element_text(color = "gray", face = "italic", hjust = 1),
              panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.minor.y = element_line(colour = "gray", linetype = "dashed"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "deepskyblue4"),
              axis.ticks.x = element_blank()) +
        labs(x = NULL, y = NULL, caption = "Data Source: INEP") +
        ggtitle("Dropout Estimation - by Area Type") +
        scale_x_discrete(limits = col_name_l1, labels = education_stages_label) +
        scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
        scale_fill_brewer(palette = "OrRd", labels = c("Urban Area", "Rural Area"))
      
        
      
    })
    
    
    
        
})





# ,
# labels = paste0(as.character(c(8:0, 1:8)), "k")


