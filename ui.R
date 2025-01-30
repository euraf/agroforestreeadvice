
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
            h2("Shade Tree Advice: Clément Rigal; Philippe Vaast; Laurence Jassogne; Just van der Wolf; Gilles Gram; Mathilde LEPINE; Anais CARPENTE; Mai Phuong NGUYEN; Sigrun WAGNER; Sophie GRAEFE; Baptiste CAMUS; Juan Carlos Villalba Malaver"),
            h2("Deciduous: Raphael Paut; François Warlop"),
            h2("SCSM : Birk Skyum Kristoffer Ronn-Anderson"),
            h2("Czech database : Jan Weger (VUKOZ Průhonice); Lubos Úradníček and Antonín Martiník (MENDELU Brno)"),
            h2("Juiste Boom op de Juiste Plek: Jordy van Eijk (St. ReGeneration & Van Eijk : Consultantree); Jade Koop (Jade Reforestry);Euridice Leyequien Abarca (HVHL); Jerke de Vries (HVHL); Evert Prins (Louis Bolk Institute) "),
            h2("GoÖko : Tsonkova, P., C. Böhm, R. Hübner and J. Ehritt (2019). Managing hedgerows to optimise ecosystem services in agroforestry systems. Agroforestry for sustainable agriculture. M. R. Mosquera-Losada and R. Prabhu. Cambridge, Burleigh Dodds Science Publishing. 1: 39-88. DOI 10.19103/as.2018.0041.03 using among others data from Gaida W., Grothe H., 2000. Gehölze: Handbuch für Planung und Ausführung (Woody Species: Handbook for Planning and Planting). Berlin, Hannover: Patzer Verlag: 319"),
            h2("Finnish tree suitability:  Ujula, Juha & Mattila, Iris. (2023). Suomessa menestyviä puu- ja pensaslajeja agrometsäkäyttöön. Teoksessa I. Mattila (toim.). Puustoinen maatalous Suomessa - opas suunnitteluun, katsaus kulttuurinmuutokseen (liite 1). Kilpiän tila. https://doi.org/10.5281/zenodo.8418298"),
            h2("If you want to report bugs, or have suggestions for improvement, or want to contribute, please do so on github: https://github.com/euraf/agroforestreeadvice")
    ),#fin infos
    tabItem(tabName = "tool", 
            tabsetPanel(
              id = "toolsTabset",
              tabPanel("Flanders Tree Advisor (DENTRO)", value="DENTRO", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), 
              
              tabPanel("Shade Tree Advice (coffee and cocoa)", value="STA", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), 
              
              tabPanel("Deciduous (fruit trees in France)", value="DECIDUOUS", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)), 
              
              tabPanel("SCSM (species climate suitability model)", value="SCSM", moduleTabInterface_UI(id = "SCSM", data=dataSCSM, interface=interfaceSCSM)), 
              
              tabPanel("Czech tree selection tool", value="Czech", moduleTabInterface_UI(id = "Czech", data = dataCzech, interface= interfaceCzech)),
              
              tabPanel("Juiste Boom op de Juiste Plek", value="JBOJP", moduleTabInterface_UI(id = "JBOJP", data = dataJBOJP, interface= interfaceJBOJP)),
              
              tabPanel("GoÖko (German Hedgerow manager)", value="DEHM", moduleTabInterface_UI(id = "DEHM", data = dataDEHM, interface= interfaceDEHM)),
              
              tabPanel("Finnish tree suitability", value="SUOMI", moduleTabInterface_UI(id = "SUOMI", data = dataSUOMI, interface= interfaceSUOMI)),
              
              tabPanel("UK Guide", value="UKguide", moduleTabInterface_UI(id = "UKguide", data = dataUKguide, interface= interfaceUKguide))
              
            )
    ),#fin tool
    tabItem(tabName = "databases", 
            h1("Coming soon! Inventory of all (known to us) tools for tree selection in agroforestry, with:"),
            h2("link to the original tool"),
            h2("filtering by usefull features"),
            h2("symbol indicating if the tool is integrated in the unified tool"),
            h1("Czech AgroforesTree Selection Tool:"),
            p("Data and other know-how for this tree selection tool in conditions of the Czech Republic were provided from publication (certified methodology) Practices and components of agroforestry systems recommended for the restoration and strengthening of environmental functions of landscape which was main result of the research project EPSILON TH04030409 of TACR (2019-2022). The input database was updated and adapted for use in the on-line tool AgroforetsTreeAdvice by following authors: Jan Weger, Luboš Úradníček, Antonín Martiník, Tadeáš Staněk and Marie Gosme."),
            a("Link for the pdf explaining the methodology", href="https://www.vukoz.cz/wp-content/uploads/2023/03/Metodika-ALS-Epsilon-fin-3.pdf")

    
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



