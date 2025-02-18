
############################################################
#                   TEsorter TE annotations                #
############################################################

read_tesorter_annotation <- function(gff3){
  read_delim(gff3, col_names = FALSE) %>% 
    dplyr::select("X1", "X9") %>% 
    separate(X9, sep=";", c("detail", "name", "gene", "clade", "coverage", "evalue", "prob")) %>%
    separate(X1, sep="#", c("TE", "superclass")) %>%
    separate(clade, sep="=", c("placeholder", "clade")) %>%
    dplyr::select(-placeholder)
}

get_tesorter_annotation <- function(){
  tesorter_annotation <- list(impolita_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"),
                              pancheri_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"),
                              revolutissima_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"),
                              sandwicensis_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"),
                              vieillardii_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"),
                              yahouensis_tesorter=read_tesorter_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3")
  )
  return(tesorter_annotation)
}


############################################################
#                  Braker gene annotations                 #
############################################################

read_braker_annotation <- function(gtf){
  read_delim(gtf, col_names = FALSE) %>%
    set_colnames(c("seqname", "method", "feature", "start", "end", "ph1", "ph2", "ph3", "annotation")) %>%
    dplyr::select(-c("ph1", "ph2", "ph3"))
}

get_braker_annotation <- function(){
  braker_annotation <- list(impolita_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita_braker.gtf"),
                              pancheri_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri_braker.gtf"),
                              revolutissima_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima_braker.gtf"),
                              sandwicensis_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis_braker.gtf"),
                              vieillardii_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii_braker.gtf"),
                              yahouensis_braker=read_braker_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis_braker.gtf")
  )
  return(braker_annotation)
}



############################################################
#                  Full EDTA TE annotations                #
############################################################

read_edta_annotation <- function(gff3){
  read_delim(gff3, col_names = FALSE) %>%
    set_colnames(c("seqname", "method", "feature", "start", "end", "score", "strand", "ph3", "annotation")) %>%
    dplyr::select(-ph3)
}



get_edta_full_annotation <- function(){
    edta_full_annotation <- list(impolita_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.TEanno.noheader.gff3"),
                                 pancheri_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.TEanno.noheader.gff3"),
                                 revolutissima_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.TEanno.noheader.gff3"),
                                 sandwicensis_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.TEanno.noheader.gff3"),
                                 vieillardii_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.TEanno.noheader.gff3"),
                                 yahouensis_edta_full=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.TEanno.noheader.gff3"))
    return(edta_full_annotation)
}


get_edta_intact_annotation <- function(){
  edta_intact_annotation <- list(impolita_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.intact.noheader.gff3"),
                                 pancheri_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.intact.noheader.gff3"),
                                 revolutissima_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.intact.noheader.gff3"),
                                 sandwicensis_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.intact.noheader.gff3"),
                                 vieillardii_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.intact.noheader.gff3"),
                                 yahouensis_edta_intact=read_edta_annotation("/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.intact.noheader.gff3"))
  return(edta_intact_annotation)
}














