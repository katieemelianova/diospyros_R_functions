


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



"/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.intact.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.intact.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.intact.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.intact.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.intact.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.intact.gff3"


"/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita_braker.gtf"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis_braker.gtf"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri_braker.gtf"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii_braker.gtf"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima_braker.gtf"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis_braker.gtf"


"/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.TEanno.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.TEanno.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.TEanno.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.TEanno.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.TEanno.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.TEanno.gff3"



"/Users/katieemelianova/Desktop/Diospyros/IGVdata/impolita/impolita.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/pancheri/pancheri.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/revolutissima/revolutissima.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/sandwicensis/sandwicensis.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/vieillardii/vieillardii.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"
"/Users/katieemelianova/Desktop/Diospyros/IGVdata/yahouensis/yahouensis.fasta.mod.EDTA.TElib.fa.rexdb.dom.gff3"


