
# Fichier de définition de l'interface utilisateur pour l'application Shiny

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
            tabsetPanel(
              
              tabPanel("Flanders Tree Advisor (DENTRO)", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), # module selecmap avec les données de l'Afrique
              
              tabPanel("Shade Tree Advice (coffee and cocoa)", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), # module selecmap avec les données de l'Amérique
              
              tabPanel("Deciduous (fruit trees in France)", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)) # module selecmap avec les données de l'Asie
              
            )
            
            # A completer: page tutoriel video, page pdf backend
    )#fin tool
  ) #fin pages
) #fin dashboardbody


# Complete page ----

dashboardPage(
  header,
  sidebar,
  body
)



