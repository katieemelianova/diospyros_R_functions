
# get orthogroups in long format to enable easy identification of a gene's orthogroup


get_long_orthogroups<-function(){


  orthogroups<-read_delim("/Users/katieemelianova/Desktop/Diospyros/diospyros_gene_family_analysis/diospyros_gene_family_analysis/fastas/OrthoFinder/Results_Feb17/Orthogroups/Orthogroups.tsv")
  strings_to_remove<-c("_braker.aa", ".aa")
  for (strings in strings_to_remove){
    colnames(orthogroups)<-str_replace(colnames(orthogroups), strings, "")
  }
  orthogroups_long_list <- lapply(orthogroups %>% dplyr::select(-Orthogroup) %>% colnames(), 
                                  function(x) orthogroups %>% 
                                    dplyr::select(Orthogroup, x) %>% 
                                    separate_longer_delim(c(x), delim = ", ") %>%
                                    set_colnames(c("Orthogroup", "gene")) %>%
                                    mutate(species=x))
  orthogroups_long<-data.frame(do.call(rbind, orthogroups_long_list))
  orthogroups_long %<>% filter(species != "doleifera") %>% 
    mutate(gene = str_replace(gene, ".t1", "")) %>%
    mutate(species=case_when(species == "impolita" ~ "D. impolita",
                             species == "pancheri" ~ "D. pancheri",
                             species == "doleifera" ~ "D. doleifera",
                             species == "revolutissima" ~ "D. revolutissima",
                             species == "sandwicensis" ~ "D. sandwicensis",
                             species == "vieillardii" ~ "D. vieillardii",
                             species == "yahouensis" ~ "D. yahouensis"))
}
