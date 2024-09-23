
# Fichier de définition de l'interface utilisateur pour l'application Shiny

# header ----

header <- dashboardHeader(title = "AgroforesTreeAdvice")


# sidebar ----

sidebar <- dashboardSidebar(
  
  shiny.i18n::usei18n(i18n),
  selectInput(inputId = 'selected_language',
              i18n$t("Change language"),
              choices = i18n$get_languages(),
              selected = "cz"),

  sidebarMenu(
    id="sidemenu",
    menuItem(i18n$t("Tools"), tabName = "tool"),
    menuItem(i18n$t("Information"), tabName = "Welcome"),
    menuItem(i18n$t("Databases"), tabName = "databases"),
    menuItem(i18n$t("Scoring"), tabName = "Scoring")
  )
)

# Main panel ----

body <- dashboardBody(
  # tags$head(
  #     tags$style(HTML("
  #       /* Custom styles for tabs */
  #       .nav-tabs > li > a {
  #         background-color: #f8f9fa; /* Light grey background */
  #         border: 1px solid #dee2e6;
  #         color: #495057; /* Dark grey text */
  #       }
  #       .nav-tabs > li.active > a {
  #         background-color: #007bff; /* Blue background for active tab */
  #         color: white; /* White text for active tab */
  #       }
  #       .tab-content {
  #         padding: 20px;
  #         border: 1px solid #dee2e6;
  #         border-top: none;
  #       }
  #     "))
  #   ),
  tags$head(
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
      }
    "))
  ),
  # style for github link
  tags$style(HTML("
      .spaced-text {
        margin-top: 50px;  /* Adjust the value to add more space */
      }
      .text-and-link {
        margin-bottom: 0px;  /* Remove bottom margin */
      }
    ")),

  tabItems(
    tabItem(tabName = "Welcome", 
            h1("Welcome! please go to Tool in the menu to find the best tree for you! or to the Databases to learn where all this comes from"),
            h1("This tool is under development (to do: make this welcome page look nicer!) within the DigitAF European project"),
            div(class = "centered-image-container",
                img(src = "logoDigitAF_HR_500.png")),

            h3("Contributors to code"),
            tags$ul(
              tags$li("Marie Gosme"),
              tags$li("Clément Rigal"),
              tags$li("Raphael Paut"),
              tags$li("Birk Skyum")
            ),

            h3("Contributors to databases"),
            h4("DENTRO"),
            tags$ul(
              tags$li("Sarah Carton"),
              tags$li("Paul Pardon"),
              tags$li("Bert Reubens")
            ),
            h4("Deciduous"),
            tags$ul(
              tags$li("Raphael Paut"),
              tags$li("François Warlop")
            ),
            h4("Shade Tree Advice"),
            tags$ul(
              tags$li("Clément Rigal"),
              tags$li("Philippe Vaast"),
              tags$li("Laurence Jassogne"),
              tags$li("Just van der Wolf"),
              tags$li("Gilles Gram"),
              tags$li("Mathilde LEPINE"),
              tags$li("Anais CARPENTE"),
              tags$li("Mai Phuong NGUYEN"),
              tags$li("Sigrun WAGNER"),
              tags$li("Sophie GRAEFE"),
              tags$li("Baptiste CAMUS"),
              tags$li("Juan Carlos Villalba Malaver")
            ),
            h4("SCSM"),
            tags$ul(
              tags$li("Birk Skyum Kristoffer Ronn-Anderson")
            ),
            h4("Czech database"),
            tags$ul(
              tags$li("Jan Weger (VUKOZ Průhonice)"),
              tags$li("Lubos Úradníček (MENDELU Brno)"),
              tags$li("Antonín Martiník (MENDELU Brno)")
            ),
            h4("Juiste Boom op de Juiste Plek"),
            tags$ul(
              tags$li("Jordy van Eijk (St. ReGeneration & Van Eijk : Consultantree)"),
              tags$li("Jade Koop (Jade Reforestry)"),
              tags$li("Euridice Leyequien Abarca (HVHL)"),
              tags$li("Jerke de Vries (HVHL)"),
              tags$li("Evert Prins (Louis Bolk Institute)")
            ),
            h4("German Hedgerow manager"),
            tags$ul(
              tags$li("Gaida W., Grothe H., 2000. Gehölze: Handbuch für Planung und Ausführung (Woody Species: Handbook for Planning and Planting). Berlin, Hannover: Patzer Verlag: 319")
            ),
            h4("Finnish tree suitability"),
            tags$ul(
              tags$li("Ujula, Juha & Mattila, Iris. (2023). Suomessa menestyviä puu- ja pensaslajeja agrometsäkäyttöön. Teoksessa I. Mattila (toim.). Puustoinen maatalous Suomessa - opas suunnitteluun, katsaus kulttuurinmuutokseen (liite 1). Kilpiän tila. https://doi.org/10.5281/zenodo.8418298")
            ),

            div(class = "spaced-text",
              h4("If you want to report bugs, or have suggestions for improvement, or want to contribute, please do so on github:"),
              a("https://github.com/euraf/agroforestreeadvice", href="https://github.com/euraf/agroforestreeadvice")
            )

    ),#fin infos
    tabItem(tabName = "tool", 
            tabsetPanel(
              id = "toolsTabset",
              tabPanel(i18n$t("Czech tree selection tool"), value="Czech", moduleTabInterface_UI(id = "Czech", data = dataCzech, interface= interfaceCzech)),
              
              tabPanel("Flanders Tree Advisor (DENTRO)", value="DENTRO", moduleTabInterface_UI(id = "DENTRO", data = dataDENTRO, interface= interfaceDENTRO)), 
              
              tabPanel(i18n$t("Shade Tree Advice (coffee and cocoa)"), value="STA", moduleTabInterface_UI(id = "STA", data=dataSTA, interface=interfaceSTA)), 
              
              tabPanel(i18n$t("Deciduous (fruit trees in France)"), value="DECIDUOUS", moduleTabInterface_UI(id = "DECIDUOUS", data=dataDECIDUOUS, interface=interfaceDECIDUOUS)), 
              
              tabPanel(i18n$t("SCSM (species climate suitability model)"), value="SCSM", moduleTabInterface_UI(id = "SCSM", data=dataSCSM, interface=interfaceSCSM)), 
              
              tabPanel("Juiste Boom op de Juiste Plek", value="JBOJP", moduleTabInterface_UI(id = "JBOJP", data = dataJBOJP, interface= interfaceJBOJP)),
              
              tabPanel("German Hedgerow manager", value="DEHM", moduleTabInterface_UI(id = "DEHM", data = dataDEHM, interface= interfaceDEHM)),
              
              tabPanel("Finnish tree suitability", value="SUOMI", moduleTabInterface_UI(id = "SUOMI", data = dataSUOMI, interface= interfaceSUOMI))
              
              
              
              
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
    ), #fin databases

    tabItem(tabName = "Scoring", 
            h1("Scoring table of the tools:"),
            DTOutput("scoringTable")  # Placeholder for the scoring DataTable
  
    ) #fin scoring
  ) #fin pages
    ,tags$style(HTML("
        /* first box, red */
        .box.box-solid.box-danger>.box-header {
                  background-color: #e35a56; 
                  color: white;}
        .box.box-solid.box-danger {border-color: #e35a56;}

        /* second box, green */
        .box.box-solid.box-primary>.box-header {
                  background-color: #6dab57; 
                  color: white;}
        .box.box-solid.box-primary {border-color: #6dab57;}

        /* third box, blue */
        .box.box-solid.box-info>.box-header {
                  background-color: #3c8dbc; 
                  color: white;}
        .box.box-solid.box-info {border-color: #3c8dbc;}
        ")),
    
    # center the image
    tags$head(
      tags$style(HTML("
          .centered-image-container {
            display: flex;
            justify-content: center;
            align-items: center;
          }
      "))
  ),

  # style for the buttons on download modal
  tags$head(
    tags$style(HTML("
      .download-button {
        width: 220px;
      }
    "))
  ),
          
) #fin dashboardbody


# Complete page ----

dashboardPage(
  header,
  sidebar,
  body
)



