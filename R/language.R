Site_box_lang <- function(lang) {
    # set the language of all variables of website

    Site_box_lang = "Your Site"
    if (as.character(lang) == "fr") {Site_box_lang = "Votre Site"}
    if (as.character(lang) == "es") {Site_box_lang = "Su Sitio"}
    if (as.character(lang) == "de") {Site_box_lang = "Ihre Seite"}
    if (as.character(lang) == "nl") {Site_box_lang = "Uw Site"}
    if (as.character(lang) == "cz") {Site_box_lang = "Vaše Stanoviště"}
    #print(as.character(lang))

    return(Site_box_lang)
}

Objectives_box_lang <- function(lang) {
    # set the language of all variables of website

    Objectives_box_lang = "Your Objectives"
    if (as.character(lang) == "fr") {Objectives_box_lang = "Vos Objectifs"}
    if (as.character(lang) == "es") {Objectives_box_lang = "Sus Objetivos"}
    if (as.character(lang) == "de") {Objectives_box_lang = "Ihre Ziele"}
    if (as.character(lang) == "nl") {Objectives_box_lang = "Uw Doelstellingen"}
    if (as.character(lang) == "cz") {Objectives_box_lang = "Vaše požadavky"}
    #print(as.character(lang))

    return(Objectives_box_lang)
}

Compare_trees_button <- function(lang) {
    # set the language of all variables of website

    Compere_trees_button = "Compare Trees"
    if (as.character(lang) == "fr") {Compere_trees_button = "Comparer les Arbres"}
    if (as.character(lang) == "es") {Compere_trees_button = "Comparar Árboles"}
    if (as.character(lang) == "de") {Compere_trees_button = "Bäume vergleichen"}
    if (as.character(lang) == "nl") {Compere_trees_button = "Bomen vergelijken"}
    if (as.character(lang) == "cz") {Compere_trees_button = "Porovnat stromy"}
    #print(as.character(lang))

    return(Compere_trees_button)
}

Graph_legend_lang <- function(lang) {
    # set the language of graph legend header

    Graph_legend_lang = "Main Criteria"
    if (as.character(lang) == "fr") {Graph_legend_lang = "Critères Principaux"}
    if (as.character(lang) == "es") {Graph_legend_lang = "Criterios Principales"}
    if (as.character(lang) == "de") {Graph_legend_lang = "Hauptkriterien"}
    if (as.character(lang) == "nl") {Graph_legend_lang = "Belangrijkste Criteria"}
    if (as.character(lang) == "cz") {Graph_legend_lang = "Hlavní Kritéria"}

    return(Graph_legend_lang)
}

Plot_legend_lang <- function(lang) {
    # changes names of X axis , actually used:
    # climate, equipment, soil, species, resources, height, production, legislation, growth, understory_tree, food,
    # wood, animals, biomass, timber, fruits, biodiversity, windbreak, shade, fertility, bioticenvironment, cropproduction,
    # economic, firewood, microclimate, otherProducts, pestdiseasecontrol, treegrowth, work, crop, location, size, lifespan
    # default for English
    translations <- c("climate" ,"equipment", "soil", "species", "resources", "height", "production", 
    "legislation", "growth", "understory_tree", "food", "wood", "animals", "biomass", "timber", "fruits", 
    "biodiversity", "windbreak", "shade", "fertility", "bioticenvironment", "cropproduction", "economic", 
    "firewood", "microclimate", "otherProducts", "pestdiseasecontrol", "treegrowth", "work", "crop", 
    "location", "size", "lifespan")

    if (as.character(lang) == "fr") {
        translations <- c("climate" = "climat", "equipment" = "équipement", "soil" = "sol", 
        "species" = "espèces", "resources" = "ressources", "height" = "hauteur", "production" = "production", 
        "legislation" = "législation", "growth" = "croissance", "understory_tree" = "sous-étage", 
        "food" = "nourriture", "wood" = "bois", "animals" = "animaux", "biomass" = "biomasse", 
        "timber" = "bois", "fruits" = "fruits", "biodiversity" = "biodiversité", 
        "windbreak" = "brise-vent", "shade" = "ombre", "fertility" = "fertilité", 
        "bioticenvironment" = "environnement biotique", "cropproduction" = "production de cultures", 
        "economic" = "économique", "firewood" = "bois de chauffage", "microclimate" = "microclimat", 
        "otherProducts" = "autres produits", "pestdiseasecontrol" = "contrôle des ravageurs et des maladies", 
        "treegrowth" = "croissance des arbres", "work" = "travail", "crop" = "culture", "location" = "emplacement", 
        "size" = "taille", "lifespan" = "durée de vie")}

    if (as.character(lang) == "es") {
        translations <- c("climate" = "clima", "equipment" = "equipo", "soil" = "suelo",
        "species" = "especies", "resources" = "recursos", "height" = "altura", "production" = "producción",
        "legislation" = "legislación", "growth" = "crecimiento", "understory_tree" = "sotobosque",
        "food" = "comida", "wood" = "madera", "animals" = "animales", "biomass" = "biomasa",
        "timber" = "madera", "fruits" = "frutas", "biodiversity" = "biodiversidad",
        "windbreak" = "rompevientos", "shade" = "sombra", "fertility" = "fertilidad",
        "bioticenvironment" = "entorno biótico", "cropproduction" = "producción de cultivos",
        "economic" = "económico", "firewood" = "leña", "microclimate" = "microclima",
        "otherProducts" = "otros productos", "pestdiseasecontrol" = "control de plagas y enfermedades",
        "treegrowth" = "crecimiento de árboles", "work" = "trabajo", "crop" = "cultivo", "location" = "ubicación",
        "size" = "tamaño", "lifespan" = "vida útil")}

    if (as.character(lang) == "de") {
        translations <- c("climate" = "Klima", "equipment" = "Ausrüstung", "soil" = "Boden", 
        "species" = "Arten", "resources" = "Ressourcen", "height" = "Höhe", "production" = "Produktion", 
        "legislation" = "Gesetzgebung", "growth" = "Wachstum", "understory_tree" = "Unterholz",
        "food" = "Nahrung", "wood" = "Holz", "animals" = "Tiere", "biomass" = "Biomasse",
        "timber" = "Holz", "fruits" = "Früchte", "biodiversity" = "Biodiversität",
        "windbreak" = "Windschutz", "shade" = "Schatten", "fertility" = "Fruchtbarkeit",
        "bioticenvironment" = "Biotische Umwelt", "cropproduction" = "Ernteerzeugung",
        "economic" = "Wirtschaftlich", "firewood" = "Brennholz", "microclimate" = "Mikroklima",
        "otherProducts" = "andere Produkte", "pestdiseasecontrol" = "Schädlings- und Krankheitsbekämpfung",
        "treegrowth" = "Baumwachstum", "work" = "Arbeit", "crop" = "Ernte", "location" = "Ort",
        "size" = "Größe", "lifespan" = "Lebensdauer")}

    if (as.character(lang) == "cz") {
        translations <- c("climate" = "klima", "equipment" = "vybavení", "soil" = "půda", 
        "species" = "druh", "resources" = "zdroje", "height" = "výška", "production" = "produkce", 
        "legislation" = "legislativa", "growth" = "růst", "understory_tree" = "podrost", 
        "food" = "potrava", "wood" = "dřevo", "animals" = "zvířata", "biomass" = "biomasa", 
        "timber" = "dřevo", "fruits" = "ovoce", "biodiversity" = "biodiverzita", 
        "windbreak" = "protivětrný pás", "shade" = "stín", "fertility" = "plodnost", 
        "bioticenvironment" = "biotické prostředí", "cropproduction" = "plodiny", 
        "economic" = "ekonomické", "firewood" = "palivo", "microclimate" = "mikroklima", 
        "otherProducts" = "jiné produkty", "pestdiseasecontrol" = "kontrola škůdců a nemocí", 
        "treegrowth" = "růst stromů", "work" = "práce", "crop" = "plodina", "location" = "umístění", 
        "size" = "velikost", "lifespan" = "doba života")}

    return(translations)
}

Graph_labels_lang <- function(lang) {
    # generates labes for graph

    labels = c("Adaptation", "Efficiency")
    if (as.character(lang) == "fr") {labels = c("Adaptation", "Efficacité")}
    if (as.character(lang) == "es") {labels = c("Adaptación", "Eficiencia")}
    if (as.character(lang) == "de") {labels = c("Anpassung", "Effizienz")}
    if (as.character(lang) == "nl") {labels = c("Aanpassing", "Efficiëntie")}
    if (as.character(lang) == "cz") {labels = c("Adaptace", "Efektivita")}
    #print(as.character(lang))

    return(labels)
}

Order_by_lang <- function(lang) {
    # set the language of Graph controls
    label = "Order by"
    if (as.character(lang) == "fr") {label = "Trier par"}
    if (as.character(lang) == "es") {label = "Ordenar por"}
    if (as.character(lang) == "de") {label = "Sortieren nach"}
    if (as.character(lang) == "nl") {label = "Sorteer op"}
    if (as.character(lang) == "cz") {label = "Seřadit podle"}

    return(label)
}