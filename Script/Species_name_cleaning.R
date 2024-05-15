
library(tidyverse)
library(bdc)
library(rgbif)
library(rfishbase)
# library(algaeClassify)

observation_raw <- readxl::read_xlsx(path = "Land-use-and-freshwater-database.xlsx", sheet = 3)

observdata <- observation_raw |> 
  select(Dataset_id, Sample_id,Plot_id,Replicate_id,Taxon,Abundance)


##remove duplicate species name##
species_name <- observdata[!duplicated(observdata$Taxon), ]

#name cleaning
species_name <- species_name |> 
  mutate(
    Taxon = Taxon |> 
      # Erasing development stage information in species names
      base::gsub(
        pattern = "_|\\.instar\\.I\\+II|\\.nymph|Larvae|\\.Other|\\. Adults|\\.Pupae| taxon|(?<!sp\\.)\\.|(?<=sp)$|\\.", 
        replacement = " ", 
        ignore.case = TRUE, perl = TRUE) |> 
      # erasing everything between brackets and years
      stringi::stri_replace_all_regex(
        pattern = "\\(.*\\)|[0-9]{4}",
        replacement = "",
        vectorize_all = FALSE) |> 
      # putting back the missing dots
      stringi::stri_replace_all_regex(
        pattern = c("sp ","aff ","cf ","spp ", " {2,9}","-"),
        replacement = c("sp.", "aff.", "cf.", "spp."," "," "),
        vectorize_all = FALSE) |> 
      base::trimws() |> 
      # adding missing spaces such as in c("Elmidaesp.1",  "Orthocladiinae1")
      base::gsub(pattern = "([a-z]*)(sp\\.+)", 
                 replacement = "\\1 \\2", perl = TRUE) |>
      base::gsub(pattern = "([a-z]*)([0-9]{1,2})",
                 replacement = "\\1 \\2", perl = TRUE)  |> 
      # Because the base pipe does not work when functions are nested like here
      # several functions nested inside if_else, we use an anonymous function (R > 4.1.0)
      (\(x) dplyr::if_else(
        condition = !stringr::str_detect(string = x, pattern = " sp\\.| sp | sp[0-9]| sp$") & 
          stringr::str_detect(string = x, pattern = "[0-9]"),
        true = base::gsub(x = x, pattern = "([A-Za-z]* ?)([0-9]{1,2})",
                          replacement = "\\1 sp. \\2", perl = TRUE),
        false = x))() |> 
      base::gsub(pattern = "( sub) (sp\\.| )",
                 replacement = "\\1\\2", perl = TRUE)|>
      # 2 spaces or more -> 1 space
      stringi::stri_replace_all_regex(pattern = " {2,9}", replacement = " ") |> 
      stringi::stri_replace_all_regex( # incomplete genus names
        pattern = c("L cyanellus","L macrochirus","L gibbosus","N crysoleucas",
                    "N analostanus","E collis","N procne"),
        replacement = c("Lepomis cyanellus","Lepomis macrochirus",
                        "Lepomis gibbosus", "Notemigonus crysoleucas",
                        "Notropis analostanus","Notropis procne", 
                        "Etheostoma collis"),
        vectorize_all = FALSE) |> 
      stringi::stri_replace_all_regex( # incomplete genus names
        pattern = c("B foveolatum","B ribeiroi","B costalimai"),
        replacement = c("Belostoma foveolatum","Belostoma ribeiroi",
                        "Belostoma costalimai"),
        vectorize_all = FALSE) |> 
      stringi::stri_replace_all_regex( # incomplete order name
        pattern = c("Ephem "),
        replacement = c("Ephemeroptera "),
        vectorize_all = FALSE) |> 
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 7
        pattern = c("B divaricatus","G perakiensis"),
        replacement = c("Burmagomphus divaricatus",
                        "Gomphidictinus perakiensis"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 57
        pattern = c("M oligolepis","C proteus","K gamma"), 
        replacement = c("Moenkhausia oligolepis","Crenicichla proteus",
                        "Knodus gamma"),
        vectorize_all = FALSE)|> 
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 63
        pattern = c("A diversus","A cruentata","C humeralis","O albiceps",
                    "K chapmani","T funebris","P antipodarum"),
        replacement = c("Archichauliodes diversus","Acanthophlebia cruentata",
                        "Coloburiscus humeralis","oxyethira albiceps",
                        "Kaniwhaniwhanus chapmani ","Tanytarsus funebris",
                        "Potamopyrgus antipodarum"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 93
        pattern = c("T tenuis"), 
        replacement = c("Triplophysa tenuis"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 152
        pattern = c("H vulnerata","A lacrimans","A extranea","A percellulata",
                    "E agkistrodon","B tepeaca"), 
        replacement = c("Hetaerina vulnerata","Argia lacrimans","Argia extranea",
                        "Argia percellulata","Erpetogomphus agkistrodon",
                        "Brechmorhoga tepeaca"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 190
        pattern = c("L cf. parvidens"), 
        replacement = c("Lethrinops cf. parvidens"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 206
        pattern = c("O hornorum X O niloticus"), 
        replacement = c("Oreochromis hornorum X Oreochromis niloticus"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 207
        pattern = c("H maeaeri Brundin","H marcidus","H subpilosus",
                    "P sordidellus gr"), 
        replacement = c("Heterotrissocladius maeaeri Brund","Heterotrissocladius marcidus",
                        "Heterotrissocladius subpilosus","Psectrocladius sordidellus gr"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # incomplete order name in Dataset 240
        pattern = c("A ustrosimulium australense"), 
        replacement = c("Austrosimulium australense"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # change for dataset 33, according to author
        pattern = c("characium sp 2 1","characium sp 3 2","cloroficea sp 15 14",
                    "cloroficea sp 3 2","cloroficea sp 4 3","cloroficea sp 5 4",
                    "cloroficea sp 6 5","cloroficea sp 7 6","cloroficea sp 8 7",
                    "closterium sp 5 4","closterium sp 6 5","closterium sp 7 6",
                    "Closterium sp 8 7","closterium sp 9 8","Cosmarium sp 17 16",
                    "cosmarium sp 19 18","euastrum cosmarium 23 ver",
                    "aphanocapsa cf nubilum colonia sp 9","cosmarium sp. 8",
                    "encyonema sp 4 3","encyonema sp6 4","eunotia sp4 3",
                    "eunotia sp5 4","eunotia sp 6 5","eunotia sp 7 6",
                    "gomphonema sp 3 2","gomphonema sp 4 3","gomphonema sp 5 4",
                    "gomphonema sp 6 5","navicula sp 4 3","navicula sp 5 4",
                    "navicula sp 6 5","navicula sp 7 6","navicula sp 8 7",
                    "navicula sp 9 8","pseudoanabaena sp. 1","pseudoanabaena sp. 2",
                    "pseudoanabaena sp. 3","pseudoanabaena sp. 4","pseudoanabaena sp. 5",
                    "pseudoanabaena sp. 6","pseudoanabaena sp. 7","pseudoanabaena sp 9 8",
                    "Pseudoanabaena cf. voronichinii"), 
        replacement = c("characium sp. 1","characium sp. 2","chlorophyceae sp. 14",
                        "chlorophyceae sp. 2","chlorophyceae sp. 3","chlorophyceae sp. 4",
                        "chlorophyceae sp. 5","chlorophyceae sp. 6","chlorophyceae sp. 7",
                        "closterium sp. 4","closterium sp. 5","closterium sp. 6",
                        "closterium sp. 7","closterium sp. 8","cosmarium sp. 16",
                        "cosmarium sp. 18","cosmarium sp. 23","aphanocapsa cf nubilum",
                        "cosmarium sp. 8","encyonema sp. 3","encyonema sp. 4",
                        "eunotia sp. 3","eunotia sp. 4","eunotia sp. 5","eunotia sp. 6",
                        "gomphonema sp. 2","gomphonema sp. 3","gomphonema sp. 4",
                        "gomphonema sp. 5","navicula sp. 3","navicula sp. 4",
                        "navicula sp. 5","navicula sp. 6","navicula sp. 7",
                        "navicula sp. 8","pseudanabaena sp. 1","pseudanabaena sp. 2",
                        "pseudanabaena sp. 3","pseudanabaena sp. 4","pseudanabaena sp. 5",
                        "pseudanabaena sp. 6","pseudanabaena sp. 7","pseudanabaena sp. 8",
                        "Pseudanabaena cf. voronichinii"),
        vectorize_all = FALSE) |>
      stringi::stri_replace_all_regex( # abbreviation common name in Dataset 248
        pattern = c("BLUEGILL","CREEKCHUB","LONGDACE",
                    "REDBRSUN","ROSYSDACE","WHITSUCKR",
                    "PUMPSEED","MUMICHOG","AMEREEL",
                    "COMMSHIN","BRKTROUT","FALLFISH",
                    "ROCKBASS","SPFINSHIN","SPTLSHIN",
                    "BRNTROUT","LGMTHBAS","MARGMDTM",
                    "BANDKILL","GOLDFISH","GLDNSHNR",
                    "COMLYSHIN","PEARLDACE","LNGEARSU",
                    "BRWNBULL","SATFINSH","RIVERCHUB",
                    "SEALAMPR","BLSPSUNF","ESILVMIN",
                    "LNGNSGAR","PIRPERCH","ROSYFSHIN",
                    "WHITPERCH","BLUNMINN","CENSTROL",
                    "FANTDART","GRSUNFSH","SJAWMINW",
                    "RNBTROUT","TESSDART","NHOGSUKR",
                    "FATHMINW","CUTLMINW","SMMTHBAS",
                    "POTSCULP"), 
        replacement = c("Lepomis macrochirus","Semotilus atromaculatus","Rhinichthys cataractae",
                        "Redbreast sunfish","Clinostomus funduloides","Catostomus commersonii",
                        "Lepomis gibbosus","Fundulus heteroclitus","Anguilla rostrata",
                        "Luxilus cornutus","Salvelinus fontinalis","Semotilus corporalis",
                        "Ambloplites rupestris","Cyprinella spiloptera","Notropis hudsonius",
                        "Salmo trutta","Micropterus salmoides","Noturus insignis",
                        "Fundulus diaphanus","Carassius auratus","Notemigonus crysoleucas",
                        "Notropis amoenus","Margariscus margarita","Lepomis megalotis",
                        "Ameiurus nebulosus","Cyprinella analostana","Nocomis micropogon",
                        "Petromyzon marinus","Enneacanthus gloriosus","Hybognathus regius",
                        "Lepisosteus osseus","Aphredoderus sayanus","Notropis rubellus",
                        "Morone americana","Pimephales notatus","Campostoma anomalum",
                        "Etheostoma flabellare","Lepomis cyanellus","Notropis buccatus",
                        "Oncorhynchus mykiss","Etheostoma olmstedi","Hypentelium nigricans",
                        "Pimephales promelas","Exoglossum maxillingua","Micropterus dolomieu",
                        "Cottus girardi"),
        vectorize_all = FALSE) 
    ) 
# |> 
#   dplyr::filter(Dataset_id == 33) |> 
#   dplyr::mutate(Taxon = gsub('(?<=[0-9]) (?=[0-9])', "-", Taxon, perl = TRUE)
              

# Remark:
#  - "Haliplus Ruficollis Group" 
#  - development stage in the name such as instar or nymph
#  - we need to replace "_" and "." except when sp. cf. spp. aff.

unique(species_name$Taxon)

###This function can help us keep number after sp.
# parse_name<-rgbif::name_parse(scientificname = species_name$Taxon)


# rgbif::name_backbone(name="Gundlachia",kingdom = "Animalia")

#Dataset 33 got weird species name--Emailed author to ask what's sp 3 2, sp2=1.
#  She is on maternity. So I assume they are different species, because they are
#  in different columns, and I gave them the new name (replace '=' between numbers,
#  with '-')
#Dataset 48,49 didn't add '.' after sp and also got weird name--It includes type1
#  and type2 after species, and it should be the same species? And it also includes
#  BR1, BR1.4. Seems we can remove all BRxx and combined type1 and type2.
#Dataset 52 got letters after species name
#Dataset 94 got lots unknown species
#Dataset 139 got 'Genero A (B,C...)"
#Dataset 144 got special characters--Done, changed in original dataset, put the 
# spectial characters, which were from author's name, into brackets ().
#Dataset 200 got ABCD after species name
#Dataset 238 got sp.sp.1 (2,3...) 
#Emailed dataset 15


# #select capital genus name
# captial_change<-species_name |> 
#   filter(grepl("^[A-Za-z]{1} ", Taxon)) |> select(Dataset_id,Taxon)


check_species_name <-
  bdc_scientificName_empty(
    data = species_name,
    sci_name = "Taxon")
check_species_name |>
  dplyr::filter(!.scientificName_empty) |>
  dplyr::select(Taxon, .scientificName_empty) |> dplyr::distinct()
###This is the function in bdc
parse_names <-
  bdc_clean_names(sci_names = check_species_name$Taxon, save_outputs = FALSE)

parse_names <-
  parse_names %>%
  dplyr::select(.uncer_terms, names_clean)

database <- dplyr::bind_cols(check_species_name, parse_names)

query_names <- bdc_query_names_taxadb(
  sci_name            = database$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Animalia", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 6, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)


database <-
  database %>%
  dplyr::rename(verbatim_scientificName = Taxon)  %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., query_names)


database %>%
  readr::write_csv(., "scientific_name.csv")



#####count species density##########
#records
count_recoreds<-left_join(observation_raw,meta_data,by="Dataset_id")
count_recoreds %>% 
  group_by(Taxa) %>% 
  summarise(Count = n())

database<-left_join(database,meta_data,by="Dataset_id")

#species
count_species<-database %>%
  # filter(taxonRank=='species') %>%
  group_by(Taxa,acceptedNameUsageID) %>% 
  summarise(Count = n()) 

count_species %>% 
  group_by(Taxa) %>% 
  summarise(Count = n())

#genus
count_genus<-database %>%
  group_by(Taxa,genus) %>% 
  summarise(Count = n()) 

count_genus %>% 
  group_by(Taxa) %>% 
  summarise(Count = n())

#family
count_family<-database %>%
  group_by(Taxa,family) %>% 
  summarise(Count = n()) 

count_family %>% 
  group_by(Taxa) %>% 
  summarise(Count = n())

#order
count_order<-database %>%
  group_by(Taxa,order) %>% 
  summarise(Count = n()) 

count_order %>% 
  group_by(Taxa) %>% 
  summarise(Count = n())

