

get_element_dataset<-function(){
  # read in element data
  leaf<-read_delim("/Users/katieemelianova/Desktop/Diospyros/diospyros_elements/leaf_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
  soil<-read_delim("/Users/katieemelianova/Desktop/Diospyros/diospyros_elements/soil_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
  species_localities<-read_delim("/Users/katieemelianova/Desktop/Diospyros/diospyros_elements/species_localities.tsv")
  
  #   label colnames according to tissue and join datasets
  leaf_soil<-inner_join(leaf, soil, by="demandeur")
  new_colnames<-leaf_soil %>% colnames() %>% str_replace(".x", "_leaf") %>% str_replace(".y", "_soil")
  leaf_soil %<>% set_colnames(new_colnames)
  leaf_soil_species<-inner_join(leaf_soil, species_localities, by=c("demandeur"="Ind ID"))
  
  # clean up inconsistent values
  leaf_soil_species %<>% mutate(demandeur=demandeur %>% str_replace("1023h", "1023"),
                                Soil = Soil %>% str_replace("Sedimentary \\(Black clays\\)", "Sedimentary"),
                                Soil = Soil %>% str_replace("ultramafic", "Ultramafic"),
                                Soil = na_if(Soil, "?"))
  
  
  # remove NA columns with no data
  leaf_soil_species %<>% dplyr::select(-c(`_13C_leaf`, `_13C_soil`, `_15N_leaf`, `_15N_soil`, `...8`))
  
  # scale all measurements to run from zero to one
  scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
  leaf_soil_species %<>% mutate_at(vars(contains(c('_leaf', '_soil'))), scale_values)
  
  return(leaf_soil_species)
  }

