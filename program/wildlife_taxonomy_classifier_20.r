# Enhanced Queensland Wildlife Classifier - Updated with Brush Turkey
# 增强版昆士兰野生动物分类器 - 更新刷火鸡

# Load required packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(readxl)) install.packages("readxl") 
if (!require(writexl)) install.packages("writexl")
if (!require(stringr)) install.packages("stringr")
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(tcltk)) install.packages("tcltk")

library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(httr)
library(jsonlite)
library(tcltk)

# Taxonomic validation database
create_taxonomic_validation_db <- function() {
  # Major animal groups with their valid higher taxonomies
  taxonomic_groups <- list(
    # Birds
    birds = list(
      class = "Aves",
      valid_families = c("Meliphagidae", "Psittaculidae", "Artamidae", "Halcyonidae", 
                        "Rhipiduridae", "Cacatuidae", "Maluridae", "Cracticidae", 
                        "Threskiornithidae", "Phalacrocoracidae", "Rallidae", "Columbidae",
                        "Corvidae", "Pardalotidae", "Acanthizidae", "Petroicidae",
                        "Megapodiidae"),  # Added for Brush Turkey
      exclude_families = c("Papilionidae", "Lepidoptera", "Canidae", "Felidae", "Bovidae")
    ),
    
    # Mammals
    mammals = list(
      class = "Mammalia",
      valid_families = c("Macropodidae", "Phalangeridae", "Phascolarctidae", "Vombatidae",
                        "Canidae", "Leporidae", "Cervidae", "Bovidae", "Felidae"),
      exclude_families = c("Papilionidae", "Psittaculidae", "Meliphagidae")
    ),
    
    # Reptiles
    reptiles = list(
      class = "Reptilia",
      valid_families = c("Pythonidae", "Agamidae", "Scincidae", "Varanidae", "Gekkonidae"),
      exclude_families = c("Papilionidae", "Canidae", "Psittaculidae")
    ),
    
    # Amphibians
    amphibians = list(
      class = "Amphibia",
      valid_families = c("Bufonidae", "Hylidae", "Myobatrachidae"),
      exclude_families = c("Papilionidae", "Canidae", "Psittaculidae")
    ),
    
    # Monotremes
    monotremes = list(
      class = "Mammalia",
      valid_families = c("Tachyglossidae", "Ornithorhynchidae"),
      exclude_families = c("Papilionidae", "Psittaculidae")
    )
  )
  
  return(taxonomic_groups)
}

# Create Queensland wildlife database
create_qld_database <- function() {
  animals_data <- list(
    # Native Birds
    list(names = c("Noisy Minor", "Minor Noisy", "Noisy Miner", "Miner Noisy"),
         scientific = "Manorina melanocephala", genus = "Manorina", family = "Meliphagidae", region = "Native", group = "birds"),
    
    list(names = c("Rainbow Lorikeet", "Lorikeet Rainbow", "Lorikeet"),
         scientific = "Trichoglossus moluccanus", genus = "Trichoglossus", family = "Psittaculidae", region = "Native", group = "birds"),
    
    list(names = c("Australian Magpie", "Magpie", "Magpie Australian"),
         scientific = "Gymnorhina tibicen", genus = "Gymnorhina", family = "Artamidae", region = "Native", group = "birds"),
    
    list(names = c("Kookaburra", "Laughing Kookaburra", "Kookaburra Laughing"),
         scientific = "Dacelo novaeguineae", genus = "Dacelo", family = "Halcyonidae", region = "Native", group = "birds"),
    
    list(names = c("Willie Wagtail", "Wagtail Willie", "Wagtail"),
         scientific = "Rhipidura leucophrys", genus = "Rhipidura", family = "Rhipiduridae", region = "Native", group = "birds"),
    
    list(names = c("Galah", "Rose-breasted Cockatoo", "Pink Cockatoo"),
         scientific = "Eolophus roseicapilla", genus = "Eolophus", family = "Cacatuidae", region = "Native", group = "birds"),
    
    list(names = c("Sulphur-crested Cockatoo", "Cockatoo Sulphur-crested", "Cockatoo"),
         scientific = "Cacatua galerita", genus = "Cacatua", family = "Cacatuidae", region = "Native", group = "birds"),
    
    list(names = c("Australian King Parrot", "King Parrot", "Parrot King"),
         scientific = "Alisterus scapularis", genus = "Alisterus", family = "Psittaculidae", region = "Native", group = "birds"),
    
    list(names = c("Crimson Rosella", "Rosella Crimson", "Rosella"),
         scientific = "Platycercus elegans", genus = "Platycercus", family = "Psittaculidae", region = "Native", group = "birds"),
    
    list(names = c("Superb Fairy-wren", "Fairy-wren Superb", "Fairy-wren"),
         scientific = "Malurus cyaneus", genus = "Malurus", family = "Maluridae", region = "Native", group = "birds"),
    
    list(names = c("Blue-faced Honeyeater", "Honeyeater Blue-faced", "Honeyeater"),
         scientific = "Entomyzon cyanotis", genus = "Entomyzon", family = "Meliphagidae", region = "Native", group = "birds"),
    
    list(names = c("Pied Butcherbird", "Butcherbird Pied", "Butcherbird"),
         scientific = "Cracticus nigrogularis", genus = "Cracticus", family = "Cracticidae", region = "Native", group = "birds"),
    
    list(names = c("Ibis", "Australian White Ibis", "White Ibis"),
         scientific = "Threskiornis molucca", genus = "Threskiornis", family = "Threskiornithidae", region = "Native", group = "birds"),
    
    list(names = c("Cormorant", "Little Pied Cormorant", "Pied Cormorant"),
         scientific = "Microcarbo melanoleucos", genus = "Microcarbo", family = "Phalacrocoracidae", region = "Native", group = "birds"),
    
    list(names = c("Purple Swamp Hen", "Hen Purple Swamp", "Swamp Hen"),
         scientific = "Porphyrio porphyrio", genus = "Porphyrio", family = "Rallidae", region = "Native", group = "birds"),
    
    # Added Australian Brush Turkey
    list(names = c("Australian Brush Turkey", "Brush Turkey", "Turkey Brush", "Scrub Turkey", "Bush Turkey"),
         scientific = "Alectura lathami", genus = "Alectura", family = "Megapodiidae", region = "Native", group = "birds"),
    
    # Marsupials
    list(names = c("Eastern Grey Kangaroo", "Kangaroo Eastern Grey", "Grey Kangaroo", "Kangaroo"),
         scientific = "Osphranter robustus", genus = "Osphranter", family = "Macropodidae", region = "Native", group = "mammals"),
    
    list(names = c("Red Kangaroo", "Kangaroo Red"),
         scientific = "Osphranter rufus", genus = "Osphranter", family = "Macropodidae", region = "Native", group = "mammals"),
    
    list(names = c("Wallaby", "Rock Wallaby", "Swamp Wallaby", "Red-necked Wallaby"),
         scientific = "Notamacropus rufogriseus", genus = "Notamacropus", family = "Macropodidae", region = "Native", group = "mammals"),
    
    list(names = c("Common Brushtail Possum", "Brushtail Possum", "Possum Brushtail", "Possum"),
         scientific = "Trichosurus vulpecula", genus = "Trichosurus", family = "Phalangeridae", region = "Native", group = "mammals"),
    
    list(names = c("Koala", "Koala Bear"),
         scientific = "Phascolarctos cinereus", genus = "Phascolarctos", family = "Phascolarctidae", region = "Native", group = "mammals"),
    
    list(names = c("Common Wombat", "Wombat Common", "Wombat"),
         scientific = "Vombatus ursinus", genus = "Vombatus", family = "Vombatidae", region = "Native", group = "mammals"),
    
    # Monotremes
    list(names = c("Short-beaked Echidna", "Echidna Short-beaked", "Echidna"),
         scientific = "Tachyglossus aculeatus", genus = "Tachyglossus", family = "Tachyglossidae", region = "Native", group = "monotremes"),
    
    list(names = c("Platypus", "Duck-billed Platypus"),
         scientific = "Ornithorhynchus anatinus", genus = "Ornithorhynchus", family = "Ornithorhynchidae", region = "Native", group = "monotremes"),
    
    # Reptiles
    list(names = c("Carpet Python", "Python Carpet", "Snake Carpet Python"),
         scientific = "Morelia spilota", genus = "Morelia", family = "Pythonidae", region = "Native", group = "reptiles"),
    
    list(names = c("Eastern Water Dragon", "Water Dragon Eastern", "Water Dragon"),
         scientific = "Intellagama lesueurii", genus = "Intellagama", family = "Agamidae", region = "Native", group = "reptiles"),
    
    list(names = c("Bearded Dragon", "Dragon Bearded"),
         scientific = "Pogona vitticeps", genus = "Pogona", family = "Agamidae", region = "Native", group = "reptiles"),
    
    list(names = c("Blue-tongue Lizard", "Blue-tongued Skink", "Bluetongue"),
         scientific = "Tiliqua scincoides", genus = "Tiliqua", family = "Scincidae", region = "Native", group = "reptiles"),
    
    # Introduced Species
    list(names = c("European Fox", "Fox European", "Red Fox", "Fox Red", "Fox"),
         scientific = "Vulpes vulpes", genus = "Vulpes", family = "Canidae", region = "Introduced", group = "mammals"),
    
    list(names = c("European Rabbit", "Rabbit European", "Rabbit"),
         scientific = "Oryctolagus cuniculus", genus = "Oryctolagus", family = "Leporidae", region = "Introduced", group = "mammals"),
    
    list(names = c("Cane Toad", "Toad Cane"),
         scientific = "Rhinella marina", genus = "Rhinella", family = "Bufonidae", region = "Introduced", group = "amphibians"),
    
    list(names = c("Feral Pigeon", "Pigeon Feral", "Rock Pigeon", "City Pigeon"),
         scientific = "Columba livia", genus = "Columba", family = "Columbidae", region = "Introduced", group = "birds"),
    
    list(names = c("Deer", "Fallow Deer", "Red Deer", "Chital Deer"),
         scientific = "Cervus elaphus", genus = "Cervus", family = "Cervidae", region = "Introduced", group = "mammals")
  )
  
  # Convert to data frame
  all_names <- c()
  all_scientific <- c()
  all_genus <- c()
  all_family <- c()
  all_region <- c()
  all_group <- c()
  
  for (animal in animals_data) {
    for (name in animal$names) {
      all_names <- c(all_names, name)
      all_scientific <- c(all_scientific, animal$scientific)
      all_genus <- c(all_genus, animal$genus)
      all_family <- c(all_family, animal$family)
      all_region <- c(all_region, animal$region)
      all_group <- c(all_group, animal$group)
    }
  }
  
  data.frame(
    name = all_names,
    scientific = all_scientific,
    genus = all_genus,
    family = all_family,
    region = all_region,
    group = all_group,
    stringsAsFactors = FALSE
  )
}

# Create databases
qld_db <- create_qld_database()
taxonomic_db <- create_taxonomic_validation_db()

# Information sufficiency checker
check_information_sufficiency <- function(animal_type, animal_breed) {
  type_clean <- clean_name(animal_type)
  breed_clean <- clean_name(animal_breed)
  
  # Check for "unspecified" or similar terms
  unspecified_terms <- c("unspecified", "unknown", "not specified", "unclear", "generic", "general")
  
  type_is_unspecified <- any(sapply(unspecified_terms, function(x) grepl(x, type_clean, ignore.case = TRUE)))
  breed_is_unspecified <- any(sapply(unspecified_terms, function(x) grepl(x, breed_clean, ignore.case = TRUE)))
  
  # Check if we have meaningful information
  type_meaningful <- type_clean != "" && !type_is_unspecified && nchar(type_clean) > 3
  breed_meaningful <- breed_clean != "" && !breed_is_unspecified && nchar(breed_clean) > 3
  
  # Determine information level
  if (!type_meaningful && !breed_meaningful) {
    return(list(sufficient = FALSE, level = "insufficient", reason = "No meaningful information"))
  }
  
  if (type_meaningful && breed_meaningful) {
    return(list(sufficient = TRUE, level = "detailed", reason = "Both type and breed specified"))
  }
  
  if (breed_meaningful) {
    return(list(sufficient = TRUE, level = "moderate", reason = "Breed specified"))
  }
  
  if (type_meaningful) {
    # Check if type is very general
    general_terms <- c("bird", "mammal", "reptile", "animal", "wildlife")
    if (any(sapply(general_terms, function(x) grepl(paste0("^", x, "$"), type_clean, ignore.case = TRUE)))) {
      return(list(sufficient = FALSE, level = "too_general", reason = "Type too general"))
    }
    return(list(sufficient = TRUE, level = "basic", reason = "Type specified"))
  }
  
  return(list(sufficient = FALSE, level = "insufficient", reason = "Insufficient information"))
}

# Taxonomic validation function
validate_taxonomy <- function(family, genus, species, animal_group = NULL) {
  if (is.na(family) || family == "Unknown" || family == "") {
    return(list(valid = FALSE, reason = "No family information", corrected = NULL))
  }
  
  # Detect animal group from family if not provided
  if (is.null(animal_group) || animal_group == "") {
    for (group_name in names(taxonomic_db)) {
      if (family %in% taxonomic_db[[group_name]]$valid_families) {
        animal_group <- group_name
        break
      }
    }
  }
  
  # If we still don't have a group, try to infer from the data
  if (is.null(animal_group) || animal_group == "") {
    # Check against known Queensland species
    qld_match <- qld_db[qld_db$family == family, ]
    if (nrow(qld_match) > 0) {
      animal_group <- qld_match$group[1]
    }
  }
  
  if (is.null(animal_group) || animal_group == "" || !animal_group %in% names(taxonomic_db)) {
    return(list(valid = TRUE, reason = "Cannot validate", corrected = NULL))
  }
  
  # Check if family is in exclusion list
  if (family %in% taxonomic_db[[animal_group]]$exclude_families) {
    return(list(valid = FALSE, reason = paste("Invalid family for", animal_group), corrected = NULL))
  }
  
  # Check if family is in valid list
  if (!family %in% taxonomic_db[[animal_group]]$valid_families) {
    return(list(valid = FALSE, reason = paste("Unrecognized family for", animal_group), corrected = NULL))
  }
  
  # Additional cross-validation with Queensland database
  qld_family_check <- qld_db[qld_db$family == family, ]
  if (nrow(qld_family_check) > 0) {
    expected_group <- qld_family_check$group[1]
    if (expected_group != animal_group) {
      return(list(valid = FALSE, reason = paste("Family belongs to", expected_group), corrected = expected_group))
    }
  }
  
  return(list(valid = TRUE, reason = "Taxonomy validated", corrected = NULL))
}

# Determine animal group from input
determine_animal_group <- function(animal_type, animal_breed) {
  type_clean <- tolower(clean_name(animal_type))
  breed_clean <- tolower(clean_name(animal_breed))
  
  combined_text <- paste(type_clean, breed_clean)
  
  # Bird indicators (updated with turkey)
  if (grepl("bird|pigeon|parrot|cockatoo|lorikeet|magpie|kookaburra|wagtail|galah|rosella|wren|honeyeater|butcherbird|ibis|cormorant|hen|chicken|duck|goose|swan|eagle|hawk|owl|crow|raven|turkey|brush", combined_text)) {
    return("birds")
  }
  
  # Mammal indicators
  if (grepl("mammal|kangaroo|wallaby|possum|koala|wombat|fox|rabbit|deer|bat|mouse|rat|cat|dog", combined_text)) {
    return("mammals")
  }
  
  # Reptile indicators
  if (grepl("reptile|snake|python|lizard|dragon|gecko|skink|turtle|tortoise|crocodile", combined_text)) {
    return("reptiles")
  }
  
  # Amphibian indicators
  if (grepl("amphibian|frog|toad|salamander", combined_text)) {
    return("amphibians")
  }
  
  # Monotreme indicators
  if (grepl("echidna|platypus|monotreme", combined_text)) {
    return("monotremes")
  }
  
  return(NULL)
}

# Clean name function
clean_name <- function(name) {
  if (is.na(name) || is.null(name) || name == "") return("")
  cleaned <- str_trim(str_squish(as.character(name)))
  cleaned <- str_replace_all(cleaned, "[\r\n]+", " ")
  cleaned <- str_to_title(cleaned)
  cleaned <- str_replace_all(cleaned, "\\b(Pet|Wild|Adult|Juvenile|Male|Female|Unspecified)\\b", "")
  cleaned <- str_trim(str_squish(cleaned))
  return(cleaned)
}

# Generate search variations
generate_variations <- function(name) {
  if (name == "") return(character(0))
  
  variations <- c(name)
  
  # Handle hyphen format "A - B" -> "B A"
  if (str_detect(name, " - ")) {
    parts <- str_split(name, " - ")[[1]]
    if (length(parts) == 2) {
      part1 <- str_trim(parts[1])
      part2 <- str_trim(parts[2])
      variations <- c(variations, paste(part2, part1), part2)
    }
  }
  
  # Remove modifiers for fuzzy matching
  base_name <- str_replace_all(name, "\\b(Feral|Wild|Common|Little|Large|Big|Small)\\b", "")
  base_name <- str_trim(str_squish(base_name))
  if (base_name != "" && base_name != name) {
    variations <- c(variations, base_name)
  }
  
  # Add Australian prefix
  if (!str_detect(name, "Australian|Australia")) {
    variations <- c(variations, paste("Australian", name))
  }
  
  return(unique(variations[variations != ""]))
}

# GBIF query function
query_gbif <- function(query_name) {
  if (query_name == "") return(NULL)
  
  tryCatch({
    Sys.sleep(0.3)
    base_url <- "https://api.gbif.org/v1/species/match"
    response <- GET(base_url, query = list(name = query_name))
    
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed")
      if (!is.null(result$genus) && !is.na(result$genus)) {
        confidence_level <- if (!is.null(result$species) && !is.na(result$species)) {
          "Species-GBIF"
        } else if (!is.null(result$genus) && !is.na(result$genus)) {
          "Genus-GBIF"
        } else {
          "Family-GBIF"
        }
        
        return(list(
          family = ifelse(is.null(result$family), NA, result$family),
          genus = ifelse(is.null(result$genus), NA, result$genus),
          species = ifelse(is.null(result$scientificName), NA, result$scientificName),
          confidence = confidence_level,
          method = paste("GBIF", query_name, sep = "-"),
          match_level = if (!is.null(result$species)) "species" else if (!is.null(result$genus)) "genus" else "family"
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# iNaturalist query function
query_inat <- function(query_name) {
  if (query_name == "") return(NULL)
  
  tryCatch({
    Sys.sleep(0.2)
    base_url <- "https://api.inaturalist.org/v1/taxa"
    response <- GET(base_url, query = list(q = query_name, rank = "species,genus,family", per_page = 5))
    
    if (status_code(response) == 200) {
      result <- content(response, as = "parsed")
      if (length(result$results) > 0) {
        best_match <- result$results[[1]]
        
        confidence_level <- switch(best_match$rank,
                                 "species" = "Species-iNat",
                                 "genus" = "Genus-iNat",
                                 "family" = "Family-iNat",
                                 "Medium-iNat")
        
        # Get higher taxonomy
        family_name <- NA
        genus_name <- NA
        
        if (!is.null(best_match$ancestors)) {
          for (ancestor in best_match$ancestors) {
            if (ancestor$rank == "family") family_name <- ancestor$name
            if (ancestor$rank == "genus") genus_name <- ancestor$name
          }
        }
        
        if (best_match$rank == "family") family_name <- best_match$name
        if (best_match$rank == "genus") genus_name <- best_match$name
        
        return(list(
          family = family_name,
          genus = genus_name,
          species = ifelse(best_match$rank == "species", best_match$name, NA),
          confidence = confidence_level,
          method = paste("iNat", query_name, sep = "-"),
          match_level = best_match$rank
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# Match Queensland database
match_qld_db <- function(query_name, expected_group = NULL) {
  if (query_name == "") return(NULL)
  
  # Priority: Native species
  native_matches <- qld_db[qld_db$region == "Native", ]
  
  # Filter by group if specified
  if (!is.null(expected_group) && expected_group %in% qld_db$group) {
    native_matches <- native_matches[native_matches$group == expected_group, ]
  }
  
  # Exact match (native)
  exact <- native_matches[tolower(native_matches$name) == tolower(query_name), ]
  if (nrow(exact) > 0) {
    match <- exact[1, ]
    return(list(
      family = match$family,
      genus = match$genus,
      species = match$scientific,
      confidence = "Species-Native",
      method = paste("QLD-Native", query_name, sep = "-"),
      match_level = "species",
      needs_review = FALSE,
      group = match$group
    ))
  }
  
  # Partial match (native)
  for (i in 1:nrow(native_matches)) {
    if (grepl(native_matches$name[i], query_name, ignore.case = TRUE) ||
        grepl(query_name, native_matches$name[i], ignore.case = TRUE)) {
      match <- native_matches[i, ]
      return(list(
        family = match$family,
        genus = match$genus,
        species = match$scientific,
        confidence = "Species-Native-Partial",
        method = paste("QLD-Native-Partial", query_name, sep = "-"),
        match_level = "species",
        needs_review = FALSE,
        group = match$group
      ))
    }
  }
  
  # Check introduced species
  intro_matches <- qld_db[qld_db$region == "Introduced", ]
  
  # Filter by group if specified
  if (!is.null(expected_group) && expected_group %in% qld_db$group) {
    intro_matches <- intro_matches[intro_matches$group == expected_group, ]
  }
  
  # Exact match (introduced)
  exact <- intro_matches[tolower(intro_matches$name) == tolower(query_name), ]
  if (nrow(exact) > 0) {
    match <- exact[1, ]
    return(list(
      family = match$family,
      genus = match$genus,
      species = match$scientific,
      confidence = "Species-Invasive",
      method = paste("QLD-Invasive", query_name, sep = "-"),
      match_level = "species",
      needs_review = FALSE,
      group = match$group
    ))
  }
  
  # Partial match (introduced)
  for (i in 1:nrow(intro_matches)) {
    if (grepl(intro_matches$name[i], query_name, ignore.case = TRUE) ||
        grepl(query_name, intro_matches$name[i], ignore.case = TRUE)) {
      match <- intro_matches[i, ]
      return(list(
        family = match$family,
        genus = match$genus,
        species = match$scientific,
        confidence = "Species-Invasive-Partial",
        method = paste("QLD-Invasive-Partial", query_name, sep = "-"),
        match_level = "species",
        needs_review = TRUE,
        group = match$group
      ))
    }
  }
  
  return(NULL)
}

# Aggregate multiple results with validation
aggregate_results <- function(all_results, expected_group = NULL) {
  if (length(all_results) == 0) return(NULL)
  
  valid_results <- all_results[!sapply(all_results, is.null)]
  if (length(valid_results) == 0) return(NULL)
  
  # Validate each result
  validated_results <- list()
  for (result in valid_results) {
    validation <- validate_taxonomy(result$family, result$genus, result$species, expected_group)
    if (validation$valid) {
      validated_results[[length(validated_results) + 1]] <- result
    } else {
      cat("    Rejected result:", result$family, "-", validation$reason, "\n")
    }
  }
  
  if (length(validated_results) == 0) return(NULL)
  
  # Count frequencies from validated results
  fam_counts <- table(sapply(validated_results, function(x) ifelse(is.na(x$family), "NA", x$family)))
  gen_counts <- table(sapply(validated_results, function(x) ifelse(is.na(x$genus), "NA", x$genus)))
  spp_counts <- table(sapply(validated_results, function(x) ifelse(is.na(x$species), "NA", x$species)))
  
  # Remove NA counts
  fam_counts <- fam_counts[names(fam_counts) != "NA"]
  gen_counts <- gen_counts[names(gen_counts) != "NA"]
  spp_counts <- spp_counts[names(spp_counts) != "NA"]
  
  # Get most common
  most_fam <- if (length(fam_counts) > 0) names(fam_counts)[which.max(fam_counts)] else NA
  most_gen <- if (length(gen_counts) > 0) names(gen_counts)[which.max(gen_counts)] else NA
  most_spp <- if (length(spp_counts) > 0) names(spp_counts)[which.max(spp_counts)] else NA
  
  # Calculate agreement
  total <- length(validated_results)
  fam_agree <- if (length(fam_counts) > 0) max(fam_counts) / total else 0
  gen_agree <- if (length(gen_counts) > 0) max(gen_counts) / total else 0
  spp_agree <- if (length(spp_counts) > 0) max(spp_counts) / total else 0
  
  # Determine match level
  match_level <- "family"
  conf_level <- "Family"
  
  if (!is.na(most_spp) && spp_agree >= 0.5) {
    match_level <- "species"
    conf_level <- "Species"
  } else if (!is.na(most_gen) && gen_agree >= 0.5) {
    match_level <- "genus"
    conf_level <- "Genus"
  }
  
  # Adjust confidence based on agreement
  max_agree <- max(fam_agree, gen_agree, spp_agree)
  if (max_agree < 0.5) {
    conf_level <- paste(conf_level, "Low")
    needs_review <- TRUE
  } else if (max_agree >= 0.8) {
    conf_level <- paste(conf_level, "High")
    needs_review <- FALSE
  } else {
    conf_level <- paste(conf_level, "Med")
    needs_review <- max_agree < 0.6
  }
  
  # Final validation of the aggregated result
  final_validation <- validate_taxonomy(most_fam, most_gen, most_spp, expected_group)
  if (!final_validation$valid) {
    cat("    Final result failed validation:", final_validation$reason, "\n")
    return(NULL)
  }
  
  return(list(
    family = most_fam,
    genus = if (match_level %in% c("genus", "species")) most_gen else "Unknown",
    species = if (match_level == "species") most_spp else "Unknown",
    confidence = paste(conf_level, "Validated"),
    method = paste("MultiDB", total, sep = "-"),
    agreement_ratio = max_agree,
    total_searches = total,
    match_level = match_level,
    needs_review = needs_review
  ))
}

# Standardize result format
standardize_result <- function(result, info_check = NULL) {
  # Handle insufficient information cases
  if (!is.null(info_check) && !info_check$sufficient) {
    return(data.frame(
      family = "InsufficientInfo",
      genus = "CannotClassify", 
      species = "CannotClassify",
      confidence = "InsufficientInfo",
      method = "InfoCheck",
      agreement_ratio = NA,
      total_searches = 0,
      match_level = "insufficient",
      needs_review = TRUE,
      validation_status = "InsufficientInfo",
      stringsAsFactors = FALSE
    ))
  }
  
  if (is.null(result)) {
    return(data.frame(
      family = "Unknown",
      genus = "Unknown", 
      species = "Unknown",
      confidence = "Failed",
      method = "NoMatches",
      agreement_ratio = NA,
      total_searches = 0,
      match_level = "none",
      needs_review = TRUE,
      validation_status = "NoResults",
      stringsAsFactors = FALSE
    ))
  }
  
  # Format display
  fam_display <- if (is.na(result$family) || result$family == "") "Unknown" else result$family
  gen_display <- if (is.na(result$genus) || result$genus == "" || result$genus == "Unknown") "Unknown" else result$genus
  spp_display <- if (is.na(result$species) || result$species == "" || result$species == "Unknown") "Unknown" else result$species
  
  return(data.frame(
    family = fam_display,
    genus = gen_display,
    species = spp_display,
    confidence = ifelse(is.null(result$confidence), "Failed", result$confidence),
    method = ifelse(is.null(result$method), "Unknown", result$method),
    agreement_ratio = ifelse(is.null(result$agreement_ratio), NA, result$agreement_ratio),
    total_searches = ifelse(is.null(result$total_searches), 1, result$total_searches),
    match_level = ifelse(is.null(result$match_level), "unknown", result$match_level),
    needs_review = ifelse(is.null(result$needs_review), TRUE, result$needs_review),
    validation_status = "Validated",
    stringsAsFactors = FALSE
  ))
}

# Interactive output directory selector
select_output_directory <- function() {
  if (interactive()) {
    tryCatch({
      # Try using tcltk directory chooser
      output_dir <- tkchooseDirectory(title = "Select Output Directory")
      output_dir <- as.character(output_dir)
      
      if (output_dir == "" || is.na(output_dir)) {
        cat("No directory selected, using current working directory\n")
        return(getwd())
      }
      
      # Verify directory exists and is writable
      if (!dir.exists(output_dir)) {
        cat("Selected directory does not exist\n")
        return(getwd())
      }
      
      # Test write permissions
      test_file <- file.path(output_dir, "test_write_permission.tmp")
      tryCatch({
        writeLines("test", test_file)
        file.remove(test_file)
        cat("Output directory selected:", output_dir, "\n")
        return(output_dir)
      }, error = function(e) {
        cat("No write permission, using current working directory\n")
        return(getwd())
      })
      
    }, error = function(e) {
      cat("Directory selector error, using current working directory\n")
      return(getwd())
    })
  } else {
    return(getwd())
  }
}

# Main enhanced classification function
classify_animal <- function(animal_type, animal_breed) {
  # Clean inputs
  type_clean <- clean_name(animal_type)
  breed_clean <- clean_name(animal_breed)
  
  cat("Raw input:", animal_type, "|", animal_breed, "\n")
  cat("Cleaned:", type_clean, "|", breed_clean, "\n")
  
  # Check information sufficiency
  info_check <- check_information_sufficiency(animal_type, animal_breed)
  cat("Information sufficiency:", info_check$sufficient, "-", info_check$reason, "\n")
  
  if (!info_check$sufficient) {
    return(standardize_result(NULL, info_check))
  }
  
  # Determine expected animal group
  expected_group <- determine_animal_group(animal_type, animal_breed)
  cat("Expected group:", ifelse(is.null(expected_group), "Unknown", expected_group), "\n")
  
  # Determine search priority
  primary_search <- ""
  secondary_search <- ""
  
  if (breed_clean != "" && breed_clean != type_clean) {
    primary_search <- breed_clean
  }
  
  if (type_clean != "") {
    if (primary_search == "") {
      primary_search <- type_clean
    } else {
      secondary_search <- type_clean
    }
  }
  
  if (primary_search == "") {
    return(standardize_result(NULL, info_check))
  }
  
  cat("Primary search:", primary_search, "\n")
  
  # Search primary variations
  primary_vars <- generate_variations(primary_search)
  cat("Variations:", paste(primary_vars, collapse = ", "), "\n")
  
  all_results <- list()
  
  for (i in seq_along(primary_vars)) {
    var <- primary_vars[i]
    cat("  Searching variant", i, ":", var, "\n")
    
    # Priority: Queensland database with group filtering
    qld_result <- match_qld_db(var, expected_group)
    if (!is.null(qld_result)) {
      cat("    QLD match found\n")
      all_results[[length(all_results) + 1]] <- qld_result
    }
    
    # GBIF search
    gbif_result <- query_gbif(var)
    if (!is.null(gbif_result)) {
      cat("    GBIF match found\n")
      all_results[[length(all_results) + 1]] <- gbif_result
    }
    
    # iNaturalist search
    inat_result <- query_inat(var)
    if (!is.null(inat_result)) {
      cat("    iNat match found\n")
      all_results[[length(all_results) + 1]] <- inat_result
    }
    
    # Stop if enough high-quality results
    if (length(all_results) >= 3) {
      high_quality <- sum(sapply(all_results, function(x) {
        grepl("Species|Native", x$confidence)
      }))
      if (high_quality >= 2) {
        cat("    Sufficient results found\n")
        break
      }
    }
  }
  
  # Aggregate primary results with validation
  if (length(all_results) > 0) {
    cat("Primary search found", length(all_results), "results, aggregating...\n")
    aggregated <- aggregate_results(all_results, expected_group)
    if (!is.null(aggregated)) {
      return(standardize_result(aggregated))
    }
  }
  
  # Search secondary if available
  if (secondary_search != "") {
    cat("Secondary search:", secondary_search, "\n")
    secondary_vars <- generate_variations(secondary_search)
    secondary_results <- list()
    
    for (var in secondary_vars) {
      qld_result <- match_qld_db(var, expected_group)
      if (!is.null(qld_result)) {
        secondary_results[[length(secondary_results) + 1]] <- qld_result
      }
      
      gbif_result <- query_gbif(var)
      if (!is.null(gbif_result)) {
        secondary_results[[length(secondary_results) + 1]] <- gbif_result
      }
      
      inat_result <- query_inat(var)
      if (!is.null(inat_result)) {
        secondary_results[[length(secondary_results) + 1]] <- inat_result
      }
    }
    
    if (length(secondary_results) > 0) {
      cat("Secondary search found", length(secondary_results), "results, aggregating...\n")
      aggregated <- aggregate_results(secondary_results, expected_group)
      if (!is.null(aggregated)) {
        return(standardize_result(aggregated))
      }
    }
  }
  
  cat("No matches found\n")
  return(standardize_result(NULL))
}

# Enhanced test function
test_animal <- function(type = "Native Birds", breed = "Minor - Noisy") {
  cat("=== Enhanced Queensland Wildlife Classifier Test ===\n")
  cat("Animal Type:", type, "\n")
  cat("Animal Breed:", breed, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  result <- classify_animal(type, breed)
  
  cat("\n=== Classification Results ===\n")
  cat("Family:", result$family, "\n")
  cat("Genus:", result$genus, "\n")
  cat("Species:", result$species, "\n")
  cat("Confidence:", result$confidence, "\n")
  cat("Method:", result$method, "\n")
  cat("Match Level:", result$match_level, "\n")
  cat("Needs Review:", ifelse(result$needs_review, "Yes", "No"), "\n")
  cat("Validation Status:", result$validation_status, "\n")
  if (!is.na(result$agreement_ratio)) {
    cat("Agreement:", round(result$agreement_ratio * 100, 1), "%\n")
    cat("Searches:", result$total_searches, "\n")
  }
  
  return(result)
}

# Enhanced Excel processing function
process_excel <- function(input_path = NULL, output_dir = NULL) {
  cat("=== Enhanced Queensland Wildlife Classifier ===\n\n")
  
  # Input file selection
  if (is.null(input_path)) {
    if (interactive()) {
      input_path <- choose.files(caption = "Select Excel input file")
      if (length(input_path) == 0) {
        stop("No input file selected")
      }
    } else {
      stop("Please provide input file path")
    }
  }
  
  # Output directory selection
  if (is.null(output_dir)) {
    output_dir <- select_output_directory()
  }
  
  # Read Excel file
  cat("Reading Excel file:", basename(input_path), "\n")
  data <- read_excel(input_path)
  names(data) <- str_squish(names(data))
  
  # Auto-detect columns
  type_cols <- names(data)[grepl("animal.*type|type|reported.*animal.*type", names(data), ignore.case = TRUE)]
  breed_cols <- names(data)[grepl("animal.*breed|breed|reported.*animal.*breed", names(data), ignore.case = TRUE)]
  
  if (length(type_cols) == 0) {
    cat("Available columns:\n")
    print(names(data))
    stop("No animal type column found")
  }
  
  if (length(breed_cols) == 0) {
    cat("Available columns:\n")
    print(names(data))
    stop("No animal breed column found")
  }
  
  animal_type_col <- type_cols[1]
  animal_breed_col <- breed_cols[1]
  
  cat("Using columns:\n")
  cat("  Animal Type:", animal_type_col, "\n")
  cat("  Animal Breed:", animal_breed_col, "\n\n")
  
  # Data preview
  cat("Data preview:\n")
  preview <- data %>% 
    select(all_of(c(animal_type_col, animal_breed_col))) %>% 
    head(5)
  print(preview)
  
  # Set output path
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_filename <- paste0("qld_wildlife_validated_", timestamp, ".xlsx")
  output_path <- file.path(output_dir, output_filename)
  
  cat("\nOutput file:", output_path, "\n")
  
  # Process data
  cat("\nProcessing", nrow(data), "records...\n")
  start_time <- Sys.time()
  
  # Create results list
  results_list <- list()
  
  for (i in 1:nrow(data)) {
    if (i %% 5 == 0) cat("Progress:", i, "/", nrow(data), "\n")
    
    result <- classify_animal(data[[animal_type_col]][i], data[[animal_breed_col]][i])
    results_list[[i]] <- result
  }
  
  # Combine results
  results_df <- do.call(rbind, results_list)
  
  # Merge original data with results
  final_data <- cbind(data, results_df)
  
  # Rename columns (simple names)
  names(final_data)[names(final_data) == "family"] <- "Family"
  names(final_data)[names(final_data) == "genus"] <- "Genus"
  names(final_data)[names(final_data) == "species"] <- "Species"
  names(final_data)[names(final_data) == "confidence"] <- "Confidence"
  names(final_data)[names(final_data) == "method"] <- "Method"
  names(final_data)[names(final_data) == "agreement_ratio"] <- "Agreement"
  names(final_data)[names(final_data) == "total_searches"] <- "Searches"
  names(final_data)[names(final_data) == "match_level"] <- "Level"
  names(final_data)[names(final_data) == "needs_review"] <- "Review"
  names(final_data)[names(final_data) == "validation_status"] <- "Validation"
  
  # Format review column
  final_data$Review <- ifelse(final_data$Review, "Yes", "No")
  
  # Save results
  write_xlsx(final_data, output_path)
  
  # Statistics
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  
  successful <- sum(!grepl("Unknown|InsufficientInfo", final_data$Family))
  species_match <- sum(grepl("Species", final_data$Confidence), na.rm = TRUE)
  genus_match <- sum(grepl("Genus", final_data$Confidence), na.rm = TRUE)
  family_match <- sum(grepl("Family", final_data$Confidence), na.rm = TRUE)
  native_species <- sum(grepl("Native", final_data$Confidence), na.rm = TRUE)
  invasive_species <- sum(grepl("Invasive", final_data$Confidence), na.rm = TRUE)
  needs_review <- sum(final_data$Review == "Yes", na.rm = TRUE)
  insufficient_info <- sum(grepl("InsufficientInfo", final_data$Family), na.rm = TRUE)
  validated <- sum(grepl("Validated", final_data$Validation), na.rm = TRUE)
  
  cat("\n=== Processing Complete ===\n")
  cat("Total records:", nrow(final_data), "\n")
  cat("Successful:", successful, "(", round(100*successful/nrow(final_data), 1), "%)\n")
  cat("Species matches:", species_match, "(", round(100*species_match/nrow(final_data), 1), "%)\n")
  cat("Genus matches:", genus_match, "(", round(100*genus_match/nrow(final_data), 1), "%)\n")
  cat("Family matches:", family_match, "(", round(100*family_match/nrow(final_data), 1), "%)\n")
  cat("Native species:", native_species, "(", round(100*native_species/nrow(final_data), 1), "%)\n")
  cat("Invasive species:", invasive_species, "(", round(100*invasive_species/nrow(final_data), 1), "%)\n")
  cat("Need review:", needs_review, "(", round(100*needs_review/nrow(final_data), 1), "%)\n")
  cat("Insufficient info:", insufficient_info, "(", round(100*insufficient_info/nrow(final_data), 1), "%)\n")
  cat("Validated results:", validated, "(", round(100*validated/nrow(final_data), 1), "%)\n")
  cat("Processing time:", round(processing_time, 2), "minutes\n")
  cat("Results saved to:", output_path, "\n")
  
  return(final_data)
}

# Test problematic species including Brush Turkey
test_problematic <- function() {
  cat("=== Testing Problematic Species ===\n\n")
  
  problem_animals <- list(
    list(type = "Birds", breed = "Unspecified"),
    list(type = "Bird", breed = "Unspecified"),
    list(type = "Native Birds", breed = "Minor - Noisy"),
    list(type = "Native Birds", breed = "Turkey - Brush"),  # Added Brush Turkey test
    list(type = "Introduced Mammals", breed = "Deer"),
    list(type = "Birds", breed = "Pigeon - Feral"),
    list(type = "Native Birds", breed = "Ibis"),
    list(type = "Unspecified", breed = "Unknown"),
    list(type = "", breed = "")
  )
  
  results <- list()
  for (i in seq_along(problem_animals)) {
    animal <- problem_animals[[i]]
    cat("Testing", i, ":", animal$type, "-", animal$breed, "\n")
    result <- test_animal(animal$type, animal$breed)
    results[[i]] <- result
    cat("\n", paste(rep("-", 40), collapse = ""), "\n\n")
  }
  
  return(results)
}

# Test specifically for Turkey - Brush
test_brush_turkey <- function() {
  cat("=== Testing Brush Turkey Classification ===\n\n")
  
  turkey_tests <- list(
    list(type = "Native Birds", breed = "Turkey - Brush"),
    list(type = "Birds", breed = "Brush Turkey"),
    list(type = "Native Birds", breed = "Australian Brush Turkey"),
    list(type = "Birds", breed = "Scrub Turkey"),
    list(type = "Native Birds", breed = "Bush Turkey")
  )
  
  results <- list()
  for (i in seq_along(turkey_tests)) {
    test <- turkey_tests[[i]]
    cat("Testing", i, ":", test$type, "-", test$breed, "\n")
    result <- test_animal(test$type, test$breed)
    results[[i]] <- result
    cat("\n", paste(rep("-", 40), collapse = ""), "\n\n")
  }
  
  return(results)
}

# Usage instructions
cat("=== Enhanced Queensland Wildlife Classifier Usage ===\n\n")

cat("1. Test single animal:\n")
cat('   test_animal("Native Birds", "Minor - Noisy")\n')
cat('   test_animal("Native Birds", "Turkey - Brush")\n')
cat('   test_animal("Marsupial", "Wallaby")\n\n')

cat("2. Test Brush Turkey specifically:\n")
cat('   test_brush_turkey()\n\n')

cat("3. Process Excel file:\n")
cat('   results <- process_excel()\n')
cat('   # Or specify paths:\n')
cat('   results <- process_excel("input.xlsx", "C:/output/")\n\n')

cat("4. Test problematic species (including Brush Turkey):\n")
cat('   test_results <- test_problematic()\n\n')

cat("5. Enhanced Features:\n")
cat("   - Added Australian Brush Turkey (Alectura lathami, Megapodiidae)\n")
cat("   - Strict taxonomic validation\n")
cat("   - Information sufficiency checking\n")
cat("   - Interactive output directory selection\n")
cat("   - Intelligent classification level handling\n\n")

cat("Database Preview (with Brush Turkey):\n")
print(head(qld_db[qld_db$name %in% c("Australian Brush Turkey", "Brush Turkey", "Turkey Brush", "Scrub Turkey", "Bush Turkey"), ], 5))

cat("\nEnhanced Queensland Wildlife Classification System Ready!\n")
cat("Now includes Australian Brush Turkey with proper taxonomic classification.\n")