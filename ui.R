
# header ----

header <- dashboardHeader(title = "DigitAF tree Advice")


# sidebar ----

sidebar <- dashboardSidebar(
  
  radioButtons(
    inputId="in_language",
    label="Language",
    choices = languages,
    selected = "en",
    inline = TRUE),
  sidebarMenu(
    
    menuItem("Informations", tabName = "infos"),
    menuItem("Tool", tabName = "tool")
  )
)



# Main panel ----

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "infos", 
            h1("Welcome! please go to Tool in the menu"),
            img(src = "logoDigitAF_HR_500.png")
    ),#fin infos
    tabItem(tabName = "tool", 
            fluidRow(column(width=6,
                            box(title = "Your site",
                                solidHeader = TRUE,
                                status="danger",
                                width=NULL,
                                uiOutput("dynamicControlsResponse")
                            )),
                     column(width=6,
                            box(title = "Your objectives",
                                solidHeader = TRUE,
                                status="primary",
                                width=NULL,
                                fluidRow(uiOutput("dynamicControlsEffect"))
                            ))
            ),
            fluidRow(wellPanel(
              class = "custom-well-panel5",
              style = "display: flex; justify-content: center;",
              # Fourth panel (full width)
              actionButton(inputId = "ab_compute", label="Compare trees !")
            )),
            box(title = "All trees",
                solidHeader = TRUE,
                status="warning",
                width=NULL,
                fluidRow(
                  column(width=12,
                         radioButtons(inputId="orderby", label="Order By", 
                                      choices=c(Adaptation="responsetrait", Efficiency="effecttrait"),
                                      selected="responsetrait", inline=TRUE),
                         plotOutput("barplot_suitability")
                         
                  ),
                  column(width=12,
                         DTOutput(outputId = "DTSuitability")
                  ))
            )
            
            
            # A completer: page tutoriel video, page pdf backend
    )#fin tool
  ) #fin pages
) #fin dashboard


# Complete page ----

dashboardPage(
  header,
  sidebar,
  body
)

