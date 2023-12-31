
# Fichier de définition de l'interface utilisateur pour l'application Shiny

# header ----

header <- dashboardHeader(title = "AgroforesTreeAdvice")


# sidebar ----

sidebar <- dashboardSidebar(
  
  radioButtons(
    inputId="in_language",
    label="Language",
    choices = languages,
    selected = "en",
    inline = TRUE),
  sidebarMenu(
    id="sidemenu",
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
            h2("Marie Gosme; Clément Rigal; Raphael Paut ; Birk Skyum"),
            h2("Contributors to databases"),
            h2("DENTRO: Sarah Carton; Paul Pardon; Bert Reubens"),
            h2("Deciduous: Raphael Paut; François Warlop"),
            h2("Shade Tree Advice: Clément Rigal; Philippe Vaast; Laurence Jassogne; Just van der Wolf; Gilles Gram; Mathilde LEPINE; Anais CARPENTE; Mai Phuong NGUYEN; Sigrun WAGNER; Sophie GRAEFE; Baptiste CAMUS; Juan Carlos Villalba Malaver"),
            h2("SCSM : Birk Skyum Kristoffer Ronn-Anderson"),
            h2("Czech database : Jan Weger (VUKOZ Průhonice), Lubos Úradníček and Antonín Martiník (MENDELU Brno)"),
            h2("If you want to report bugs, or have suggestions for improvement, or want to contribute, please do so on github: https://github.com/euraf/agroforestreeadvice")
    ),#fin infos
    tabItem(tabName = "tool", 
            tabsetPanel(
              id = "toolsTabset",
              tabPanel("Flanders Tree Advisor (DENTRO)", value="DENTRO", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), 
              
              tabPanel("Shade Tree Advice (coffee and cocoa)", value="STA", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), 
              
              tabPanel("Deciduous (fruit trees in France)", value="DECIDUOUS", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)), 
              
              tabPanel("SCSM (species climate suitability model)", value="SCSM", moduleTabInterface_UI(id = "SCSM", data=dataSCSM, interface=interfaceSCSM)), 
              
              tabPanel("Czech tree selection tool", value="Czech", moduleTabInterface_UI(id = "Czech", data = dataCzech, interface= interfaceCzech))
              
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



