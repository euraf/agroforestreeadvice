
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
    
    menuItem("Informations", tabName = "Welcome"),
    menuItem("Tool", tabName = "tool"),
    menuItem("Databases", tabName = "databases")
  )
)

# Main panel ----

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "Welcome", 
            h1("Welcome! please go to Tool in the menu to find the best tree for you! or to the Databases to learn where all this comes from"),
            h1("This tool is under development (to do: make this welcome page look nicer!) within the DigitAF European project"),
            img(src = "logoDigitAF_HR_500.png"),
            h1("Contributors:"),
            h2("Contributors to code"),
            h2("Marie Gosme; Clément Rigal; Raphael Paut"),
            h2("Contributors to databases"),
            h2("DENTRO: Sarah Carton; Paul Pardon; Bert Reubens"),
            h2("Deciduous: Raphael Paut; François Warlop"),
            h2("Shade Tree Advice: Clément Rigal; Philippe Vaast; Laurence Jassogne; Just van der Wolf; Gilles Gram; Mathilde LEPINE; Anais CARPENTE; Mai Phuong NGUYEN; Sigrun WAGNER; Sophie GRAEFE; Baptiste CAMUS; Juan Carlos Villalba Malaver")
    ),#fin infos
    tabItem(tabName = "tool", 
            tabsetPanel(
              
              tabPanel("Flanders Tree Advisor (DENTRO)", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), # module selecmap avec les données de l'Afrique
              
              tabPanel("Shade Tree Advice (coffee and cocoa)", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), # module selecmap avec les données de l'Amérique
              
              tabPanel("Deciduous (fruit trees in France)", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)), # module selecmap avec les données de l'Asie
              
              tabPanel("SCSM (species climate suitability model)", moduleTabInterface_UI(id = "SCSM", data=dataSCSM, interface=interfaceSCSM)) # module selecmap avec les données de l'Asie
              
            )
    ),#fin tool
    tabItem(tabName = "databases", 
            h1("Coming soon! Inventory of all (known to us) tools for tree selection in agroforestry, with:"),
            h2("link to the original tool"),
            h2("filtering by usefull features"),
            h2("symbol indicating if the tool is integrated in the unified tool")
    
            # A completer: page tutoriel video, page pdf backend
    )#fin databases
  ) #fin pages
) #fin dashboardbody


# Complete page ----

dashboardPage(
  header,
  sidebar,
  body
)



