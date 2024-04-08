Plot_legend_lang <- function(lang) {
    # changes names of X axis , actually used:
    # climate, equipment, soil, species, resources, height, production, legislation, growth, understory_tree, food,
    # wood, animals, biomass, timber, fruits, biodiversity, windbreak, shade, fertility, bioticenvironment, cropproduction,
    # economic, firewood, microclimate, otherProducts, pestdiseasecontrol, treegrowth, work, crop, location, size, lifespan
  all_translations <- list(
    en = c("climate" = "climate", "equipment" = "equipment", "soil" = "soil", 
           "species" = "species", "resources" = "resources", "height" = "height", 
           "production" = "production", "legislation" = "legislation", "growth" = "growth", 
           "understory_tree" = "understory tree", "food" = "food", "wood" = "wood", 
           "animals" = "animals", "biomass" = "biomass", "timber" = "timber", 
           "fruits" = "fruits", "biodiversity" = "biodiversity", "windbreak" = "windbreak", 
           "shade" = "shade", "fertility" = "fertility", "bioticenvironment" = "biotic environment", 
           "cropproduction" = "crop production", "economic" = "economic", "firewood" = "firewood", 
           "microclimate" = "microclimate", "otherProducts" = "other products", 
           "pestdiseasecontrol" = "pest and disease control", "treegrowth" = "tree growth", 
           "work" = "work", "crop" = "crop", "location" = "location", "size" = "size", 
           "lifespan" = "lifespan"),
    fr = c("climate" = "climat", "equipment" = "équipement", "soil" = "sol", 
           "species" = "espèces", "resources" = "ressources", "height" = "hauteur", 
           "production" = "production", "legislation" = "législation", "growth" = "croissance", 
           "understory_tree" = "sous-étage", "food" = "nourriture", "wood" = "bois", 
           "animals" = "animaux", "biomass" = "biomasse", "timber" = "bois", "fruits" = "fruits", 
           "biodiversity" = "biodiversité", "windbreak" = "brise-vent", "shade" = "ombre", 
           "fertility" = "fertilité", "bioticenvironment" = "environnement biotique", 
           "cropproduction" = "production de cultures", "economic" = "économique", 
           "firewood" = "bois de chauffage", "microclimate" = "microclimat", 
           "otherProducts" = "autres produits", "pestdiseasecontrol" = "contrôle des ravageurs et des maladies", 
           "treegrowth" = "croissance des arbres", "work" = "travail", "crop" = "culture", 
           "location" = "emplacement", "size" = "taille", "lifespan" = "durée de vie"),

    es = c("climate" = "clima", "equipment" = "equipo", "soil" = "suelo",
              "species" = "especies", "resources" = "recursos", "height" = "altura", 
              "production" = "producción", "legislation" = "legislación", "growth" = "crecimiento", 
              "understory_tree" = "sotobosque", "food" = "comida", "wood" = "madera", 
              "animals" = "animales", "biomass" = "biomasa", "timber" = "madera", 
              "fruits" = "frutas", "biodiversity" = "biodiversidad", "windbreak" = "rompevientos", 
              "shade" = "sombra", "fertility" = "fertilidad", "bioticenvironment" = "entorno biótico", 
              "cropproduction" = "producción de cultivos", "economic" = "económico", 
              "firewood" = "leña", "microclimate" = "microclima", "otherProducts" = "otros productos", 
              "pestdiseasecontrol" = "control de plagas y enfermedades", "treegrowth" = "crecimiento de árboles", 
              "work" = "trabajo", "crop" = "cultivo", "location" = "ubicación", "size" = "tamaño", 
              "lifespan" = "vida útil"),

    de = c("climate" = "Klima", "equipment" = "Ausrüstung", "soil" = "Boden",
                "species" = "Arten", "resources" = "Ressourcen", "height" = "Höhe", 
                "production" = "Produktion", "legislation" = "Gesetzgebung", "growth" = "Wachstum", 
                "understory_tree" = "Unterholz", "food" = "Nahrung", "wood" = "Holz", 
                "animals" = "Tiere", "biomass" = "Biomasse", "timber" = "Holz", 
                "fruits" = "Früchte", "biodiversity" = "Biodiversität", "windbreak" = "Windschutz", 
                "shade" = "Schatten", "fertility" = "Fruchtbarkeit", "bioticenvironment" = "Biotische Umwelt", 
                "cropproduction" = "Ernteerzeugung", "economic" = "Wirtschaftlich", 
                "firewood" = "Brennholz", "microclimate" = "Mikroklima", "otherProducts" = "andere Produkte", 
                "pestdiseasecontrol" = "Schädlings- und Krankheitsbekämpfung", "treegrowth" = "Baumwachstum", 
                "work" = "Arbeit", "crop" = "Ernte", "location" = "Ort", "size" = "Größe", 
                "lifespan" = "Lebensdauer"),

    nl = c("climate" = "klimaat", "equipment" = "uitrusting", "soil" = "bodem",
                "species" = "soorten", "resources" = "hulpbronnen", "height" = "hoogte", 
                "production" = "productie", "legislation" = "wetgeving", "growth" = "groei", 
                "understory_tree" = "ondergroei", "food" = "voedsel", "wood" = "hout", 
                "animals" = "dieren", "biomass" = "biomassa", "timber" = "hout", 
                "fruits" = "vruchten", "biodiversity" = "biodiversiteit", "windbreak" = "windbreker", 
                "shade" = "schaduw", "fertility" = "vruchtbaarheid", "bioticenvironment" = "biotische omgeving", 
                "cropproduction" = "gewasproductie", "economic" = "economisch", 
                "firewood" = "brandhout", "microclimate" = "microklimaat", "otherProducts" = "andere producten", 
                "pestdiseasecontrol" = "ongedierte- en ziektebestrijding", "treegrowth" = "boomgroei", 
                "work" = "werk", "crop" = "gewas", "location" = "locatie", "size" = "grootte", 
                "lifespan" = "levensduur"),

    cz = c("climate" = "klima", "equipment" = "vybavení", "soil" = "půda",
                "species" = "druh", "resources" = "zdroje", "height" = "výška", 
                "production" = "produkce", "legislation" = "legislativa", "growth" = "růst", 
                "understory_tree" = "podrost", "food" = "potrava", "wood" = "dřevo", 
                "animals" = "zvířata", "biomass" = "biomasa", "timber" = "dřevo", 
                "fruits" = "ovoce", "biodiversity" = "biodiverzita", "windbreak" = "protivětrný pás", 
                "shade" = "stín", "fertility" = "plodnost", "bioticenvironment" = "biotické prostředí", 
                "cropproduction" = "plodiny", "economic" = "ekonomické", 
                "firewood" = "palivo", "microclimate" = "mikroklima", "otherProducts" = "jiné produkty", 
                "pestdiseasecontrol" = "kontrola škůdců a nemocí", "treegrowth" = "růst stromů", 
                "work" = "práce", "crop" = "plodina", "location" = "umístění", "size" = "velikost", 
                "lifespan" = "doba života")
    # Add more languages here...
  )

  # Default to English if the specified language is not found
  translations <- all_translations[[as.character(lang)]] %||% all_translations[["en"]]
  
  print("XXXXXXXX")
  print(as.character(lang))

  return(translations)
}