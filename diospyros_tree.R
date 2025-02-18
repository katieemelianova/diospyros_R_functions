library(ggtree)
library(ape)




diosypros_tree<-function(){
  # read in tree
  species_tree<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/TeernaDiospyrosTrees/short_names_378_hetfil.contree")
  
  # define tips to drop
  tip_to_drop<-c(species_tree$tip.label[(startsWith(species_tree$tip.label, "cal"))][-1], 
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "unk"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "ine"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "che"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "vei"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "fas"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "sam"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "mac"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "oub"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "Mau"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "fer"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "gfer"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "cffer"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "hil"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "san"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "fol"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "sub"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "vie"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "fla"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "umb"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "lab"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "tris"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "par"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "ptpar"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "gla"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "ruf"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "pan"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "per"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "yah"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "eru"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "heq"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "rev"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "cfpus"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "pus"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "imp"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "ctpar"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "min"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "trid"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "afmin"))],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "spn"))][-1],
                 species_tree$tip.label[(startsWith(species_tree$tip.label, "Mad"))])
  
  # drop tips
  species_tree<-drop.tip(species_tree, tip_to_drop)
  
  # relabel tips by species
  species_tree$tip.label<-case_when(startsWith(species_tree$tip.label, "cal") ~"calciphila",
                                    startsWith(species_tree$tip.label, "unk") ~"unknown",
                                    startsWith(species_tree$tip.label, "ine") ~"inexplorata",
                                    startsWith(species_tree$tip.label, "che") ~"cherleria",
                                    startsWith(species_tree$tip.label, "vei") ~"veillonii",
                                    startsWith(species_tree$tip.label, "fas") ~"fasciculata",
                                    startsWith(species_tree$tip.label, "sam") ~"samoensis",
                                    startsWith(species_tree$tip.label, "mac") ~"macrocarpa",
                                    startsWith(species_tree$tip.label, "oub") ~"oubatchensis",
                                    startsWith(species_tree$tip.label, "Mau") ~"mauritius",
                                    startsWith(species_tree$tip.label, "fer") ~"ferrea",
                                    startsWith(species_tree$tip.label, "gfer") ~"ferrea",
                                    startsWith(species_tree$tip.label, "cffer") ~"ferrea",
                                    startsWith(species_tree$tip.label, "hil") ~"hillebrandii",
                                    startsWith(species_tree$tip.label, "san") ~"sandwicensis",
                                    startsWith(species_tree$tip.label, "fol") ~"foliosa",
                                    startsWith(species_tree$tip.label, "sub") ~"subsessilis",
                                    startsWith(species_tree$tip.label, "vie") ~"vieillardii",
                                    startsWith(species_tree$tip.label, "fla") ~"flavocarpa",
                                    startsWith(species_tree$tip.label, "umb") ~"umbrosa",
                                    startsWith(species_tree$tip.label, "lab") ~"labillardierei",
                                    startsWith(species_tree$tip.label, "tris") ~"trisulca",
                                    startsWith(species_tree$tip.label, "par") ~"parviflora",
                                    startsWith(species_tree$tip.label, "ptpar") ~"parviflora",
                                    startsWith(species_tree$tip.label, "gla") ~"glans",
                                    startsWith(species_tree$tip.label, "ruf") ~"rufutomentosa",
                                    startsWith(species_tree$tip.label, "pan") ~"pancheri",
                                    startsWith(species_tree$tip.label, "per") ~"perplexa",
                                    startsWith(species_tree$tip.label, "yah") ~"yahouensis",
                                    startsWith(species_tree$tip.label, "eru") ~"erudita",
                                    startsWith(species_tree$tip.label, "heq") ~"hequetiae",
                                    startsWith(species_tree$tip.label, "rev") ~"revolutissima",
                                    startsWith(species_tree$tip.label, "cfpus") ~"pustulata",
                                    startsWith(species_tree$tip.label, "pus") ~"pustulata",
                                    startsWith(species_tree$tip.label, "imp") ~"impolita",
                                    startsWith(species_tree$tip.label, "ctpar") ~"parviflora",
                                    startsWith(species_tree$tip.label, "min") ~"minimifolia",
                                    startsWith(species_tree$tip.label, "trid") ~"tridentata",
                                    startsWith(species_tree$tip.label, "afmin") ~"minimifolia",
                                    startsWith(species_tree$tip.label, "spn") ~"sp. Pic N'ga")
  # root tree
  species_tree <- root(species_tree, which(species_tree$tip.label == "sandwicensis"))
  
  
  
  
}


######################################################
#                read in new species tree            #
######################################################

species_tree<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/TeernaDiospyrosTrees/short_names_378_hetfil.contree")
#species_tree.rooted <- root(species_tree, which(species_tree$tip.label == "D.sandwicensis"))
#tip<-c("D.olen", "D.fasciculosa", "D.macrocarpa", "D.ferrea")
#species_tree.rooted<-drop.tip(species_tree.rooted, tip)
#species_tree.rooted$species <- species_tree.rooted$tip.label
#species_tree.rooted$tip.label
#species_tree.rooted$tip.label<-str_replace(species_tree.rooted$tip.label, "D.", "D. ")

#outgroup<-c("D. sandwicensis")
#ultramafic<-c("D. vieillardii", "D. pancheri", "D. revolutissima")
#volcanic<-c("D. impolita", "D. yahouensis")

#########################################################################################################
#          Define tips to drop, dropping all but first individual of species you want to keep           #
#########################################################################################################




######################################
#          drop defined tips         #
######################################




#####################################################
#         make tip labels full species names        #
#####################################################


