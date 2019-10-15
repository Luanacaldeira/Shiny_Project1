


shinyUI(dashboardPage(skin = "blue",
    
    dashboardHeader(title = "A Glimpse at Primary and Secondary Education in Brazil",
                    titleWidth = 1150),

    
    
    dashboardSidebar(
        width = 200,
        sidebarUserPanel("Luana Stamato Caldeira"),
        
        
        
        sidebarMenu(
            menuItem("Enrollment by Ethnicity", tabName = "tab_1", icon = icon("chart-bar")),
            menuItem("Enrollment by Location", tabName = "tab_2", icon = icon("chart-bar"))
        ),
        selectizeInput("region",
                       "Select Region to Dispaly",
                       choices = region_list)
        
    ),
    
    
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "tab_1",
                    fluidRow(
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 10,
                            height = 270,
                            plotOutput("plot1", height = 250)),
                        
                        box(
                            status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            background = "navy",
                            width = 2,
                            height = 270,
                            tags$div(align = 'left', 
                                     class = 'multicol',
                            
                                     sliderInput(inputId = "slider",
                                                 label = "Year Range",
                                                 sep = "", ticks = FALSE,
                                                 min = 2010, max = 2018, step = 1,
                                                 value = c(2010, 2018),
                                                 width = "90%"),
                                     
                                     hr(),
                                     
                                     
                                     checkboxGroupInput(inputId = "checkGroup",
                                                        label = "Education Stage", 
                                                        choices = group_list,
                                                        selected = group_list,
                                                        inline = FALSE)
                                     
                                     
                            ))),
                    
                    
                    fluidRow(
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 5,
                            height = 270,
                            plotOutput("plot2", height = 250)
                            ),

                        box(status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = 2,
                            height = 270,
                            align = "center",
                            title = "Inputs",
                            selectizeInput("selecgroup",
                                           label = ("Select Education Stage and Years to Compare"),
                                           choices = group_list),
                            selectizeInput("year1",
                                           label = NULL,
                                           choices = year_list,
                                           selected = 2010),
                            selectizeInput("year2",
                                           label = NULL,
                                           choices = year_list,
                                           selected = 2011)

                            
                            ),

                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 5,
                            height = 270,
                            plotOutput("plot3", height = 250)
                            )

                    )),

           
            tabItem(tabName = "tab_2",
                    fluidRow(
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 6,
                            height = 270,
                            plotOutput("plot4", height = 250)
                            ),
                        
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 6,
                            height = 270,
                            plotOutput("plot5", height = 250))
                        
                        
                        ),
                    
                    
                    
                    
                
                    fluidRow(
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 5,
                            height = 270,
                            plotOutput("plot6", height = 250)
                            
                        ),
                        
                        box(status = "primary",
                            solidHeader = FALSE,
                            collapsible = FALSE,
                            width = 5,
                            height = 270,
                            plotOutput("plot7", height = 250)
                            ),
                        
                        
                        
                        box(status = "info",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = 2,
                            height = 270,
                            title = "Input",
                            align = "center",
                            background = "light-blue",
                            div(style = "display:inline-block",
                                selectizeInput("startyear",
                                               label = "Start Year and Step",
                                               choices = year_list[1:5])),
                            div(style = "display:inline-block",
                                selectizeInput("gap1",
                                               label = NULL,
                                               choices = c(1:5))),
                            div(style = "display:inline-block",
                                selectizeInput("gap2",
                                               label = NULL,
                                               choices = c(1:5))),
                            div(style = "display:inline-block",
                                selectizeInput("gap3",
                                               label = NULL,
                                               choices = c(1:5))),
                            div(style = "display:inline-block",
                                selectizeInput("gap4",
                                               label = NULL,
                                               choices = c(1:5)))
                            )
                            
                            
                            
     ))))))