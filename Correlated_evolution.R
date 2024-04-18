###CorHMM TE-Migration###

library(corHMM)
library(ape)
library(phytools)
library(readxl)
library(corrplot)
library(readxl)
library(tidyverse)
library(ggtree)

n <- 10

h1 <- hcl.colors(n, palette = "Dynamic")
h2 <- hcl.colors(n, palette = "Earth")
h3 <- hcl.colors(n, palette = "Berlin")
h4 <- hcl.colors(n, palette = "Fall")
h5 <- hcl.colors(n, palette = "Sunset")

library(unikn)
seecol(list(h1, h2, h3, h4, h5), 
       col_brd = "white", lwd_brd = 4, 
       title = "Example palettes from hcl.colors(n = 10)", 
       pal_names = c("Dynamic", "Earth", "Berlin", "Fall",  "Sunset"))

contr.col <- c(h2[6], h2[2], h1[3], h4[10], h2[9])

tree <- read.tree("~/Dropbox/Research/Macro_SSDI/data/birds_mcc.tre")

tips<- as.data.frame(tree$tip.label)

Fitch.data <- read.csv("data/Fitch_TE_migration.csv")

avonet <- read_excel("~/Dropbox/Data_repo/ELEData/TraitData/AVONET3_BirdTree.xlsx", sheet=2)

avonet <- avonet%>%
  mutate(Species = gsub(" ", "_", Species3),
         Migration = as.numeric(Migration),
         Migration = if_else(Migration == 3, 2, if_else(Migration == 2, 1, 0)))%>%
  filter(Family3 %in% c("Paradisaeidae", "Anatidae", "Cracidae", "Gruidae", "Aramidae", "Rostratulidae", "Ciconiidae", "Threskiornithidae", "Anseranidae", "Phasianidae", "Numididae"))%>%
  dplyr::select(Species, Migration)%>%
  na.omit()

not.in.tree <- Fitch.data%>%
  filter(!Species %in% tips$`tree$tip.label`)


#remove "Cygnus_bewickii" &     Repkace Guttera_edouardi with Guttera_plumifera, not same spcies but tip placement should work considering eduardi not in tree.	

Fitch.data <- Fitch.data %>%
  mutate(Species = if_else(
    Species == "Grus_rubricundus",
    "Grus_rubicunda",
    if_else(
      Species == "Grus_monachus",
      "Grus_monacha",
      if_else(
        Species == "Grus_leucogernus",
        "Grus_leucogeranus",
        if_else(
          Species == "Anthropoides_para",
          "Grus_paradisea",
          if_else(
            Species == "Anthropoides_virgo",
            "Grus_virgo",
            if_else(
              Species == "Penelope_arygyrotis",
              "Penelope_argyrotis",
                if_else(
                  Species == "Ortalis_cinericeps",
                  "Ortalis_cinereiceps",
                  if_else(
                    Species == "Crax_unicornis",
                    "Pauxi_unicornis",
                    if_else(
                      Species == "Crax_pauxi",
                      "Pauxi_pauxi",
                      if_else(
                        Species == "Crax_salvini",
                        "Mitu_salvini",
                        if_else(
                          Species == "Crax_mitu",
                          "Mitu_mitu",
                          if_else(
                            Species == "Crax_tomentosa",
                            "Mitu_tomentosum",
                            if_else(
                              Species == "Manucodia_keraudrennii",
                              "Manucodia_keraudrenii",
                              if_else(Species == "Guttera_edouardi", "Guttera_plumifera",
                                      Species)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
  )) %>%
  filter(!Species %in% c("Cygnus_bewickii", "Penelope_jacuaca")) %>%
  mutate(
    TE = if_else(TE %in% c("M", "F"), 2, 1),
    #1 = monomorphic; 2 = dimorphic
    TE_type_binary = if_else(TE_type %in% c("Subder"), 1, 0),
    #1 = interior (subdermal, thoracic, intracalvicular); 2 = external (subdermal)
    Migration = if_else(
      Migration == "Long", 2, if_else(Migration == "Short", 1, 0))
    )



data <- avonet %>%
  left_join(., Fitch.data) %>%
  mutate(
    TE = if_else(is.na(TE), 0, TE),
    TE_type_binary = if_else(is.na(TE_type_binary), 0, TE_type_binary),
    Migration.binary = if_else(Migration %in% c(1:2), 1, 0),
    TE_type = if_else(is.na(TE_type), "?", if_else(
      TE_type == "Subder", "0", if_else(TE_type %in% c("Clav", "Thor"), "1", "2")
    ))
  )
  


tree.trimmed <- keep.tip(tree, data$Species)


plot(tree.trimmed,show.tip.label = FALSE, type="fan")


set.seed(1982)
require(ape)
require(expm)
require(corHMM)

phy <- multi2di(tree.trimmed)
temp <- as.data.frame(data)
plot(phy, show.tip.label = FALSE, type="fan")
data.sort <- data.frame(temp[, 2], temp[,3],temp[,5], row.names = temp[,1])
data.sort <- data.sort[phy$tip.label, ]
tiplabels(pch = 16, col = data.sort[,1]+1, cex = 0.5)
tiplabels(pch = 16, col = data.sort[,2]+5, cex = 0.5, offset = 1)

# Migration and TE type testing whether external TE tends to be associated with loss or gain of migration
#ARD model no hidden rates
MK_3state.ARD <- corHMM(phy = phy, data = data[,c(1,6,4,3)], model="ARD", rate.cat = 1)

MK_3state.ARD
plotMKmodel(MK_3state.ARD)
corrplot(MK_3state.ARD$solution, is.corr = FALSE) # by default, method = 'circle'

#ER model no hidden rates
MK_3state.ER <- corHMM(phy = phy, data = data[,c(1,6,5)],model="ER", rate.cat = 1)

MK_3state.ER
plotMKmodel(MK_3state.ER)
corrplot(MK_3state.ER$solution, is.corr = FALSE) # by default, method = 'circle'


#SYM model no hidden rates
MK_3state.SYM <- corHMM(phy = phy, data = data[,c(1,6,5)],model="SYM", rate.cat = 1)

MK_3state.SYM
plotMKmodel(MK_3state.SYM)
corrplot(MK_3state.SYM$solution, is.corr = FALSE) # by default, method = 'circle'


#ARD model 2 rates  (hidden rates)
MK_3state.ARD.hr2 <- corHMM(phy = phy, data = data[,c(1,6,5)], model="ARD", rate.cat = 2)

MK_3state.ARD.hr2
plotMKmodel(MK_3state.ARD.hr2)
corrplot(log(MK_3state.ARD.hr2$solution), is.corr = FALSE) # by default, method = 'circle'

#ER model 2 rates (hidden rates)
MK_3state.ER.hr2 <- corHMM(phy = phy, data = data[,c(1,6,5)],model="ER", rate.cat = 2)

MK_3state.ER.hr2
plotMKmodel(MK_3state.ER.hr2)
corrplot(MK_3state.ER.hr2$solution, is.corr = FALSE) # by default, method = 'circle'


#SYM model 2 rates (hidden rates)
MK_3state.SYM.hr2 <- corHMM(phy = phy, data = data[,c(1,6,5)],model="SYM", rate.cat = 2)

MK_3state.SYM.hr2
plotMKmodel(MK_3state.SYM.hr2)
corrplot(MK_3state.SYM.hr2$solution, is.corr = FALSE) # by default, method = 'circle'



# complex models with all three traits

MK_7state <- corHMM(phy = phy, data = data[,c(1,2,3,5)], rate.cat = 1)

MK_7state
plotMKmodel(MK_7state)

#7 -> 5 dimorphic TE migratory internal symmetrical migratory internal
#5->4 short migration symmetrical TE to short migration no TE
#7 -> 5 dimorphic TE migratory internal symmetrical migratory internal
#5->4 short migration symmetrical TE to short migration no TE


phy = MK_7state$phy
data = MK_7state$data
model = MK_7state$solution
model[is.na(model)] <- 0
diag(model) <- -rowSums(model)
# run get simmap (can be plotted using phytools)
simmap <- makeSimmap(tree=phy, data=data, model=model, rate.cat=1, nSim=1, nCores=1)
# we import phytools plotSimmap for plotting
phytools::plotSimmap(simmap[[1]], fsize = 0.1, type = "arc")


#HMM model 2 rate cats
HMM_7state.SYM <- corHMM(phy = phy, data = data, rate.cat = 2, model = "SYM", get.tip.states = TRUE)

HMM_7state.SYM

plotMKmodel(HMM_7state.SYM)


## HMM all rates different 2 rate categories
HMM_7state.ARD <- corHMM(phy = phy, data = data, rate.cat = 2, model = "ARD", get.tip.states = TRUE)

HMM_7state.ARD

plotMKmodel(HMM_7state.ARD)

plot(HMM_7state.ARD,color=TRUE,lwd=3)
title(main="Transition rates between ecomorph states\nunder an ARD model",
      font.main=3,line=-1)

corrplot(log10(as.data.frame(HMM_7state.ARD$solution[2])), is.corr = FALSE,) # by default, method = 'circle'

#Map to tree
# extract character of interest
te <- data%>%
  dplyr::select(Species, TE_type)%>%
  na.omit()%>%
  column_to_rownames(var="Species")%>%
  pull(TE_type)%>%
  as.factor()

te <- setNames(te, data%>%
                      dplyr::select(Species, TE_type)%>%
                      na.omit()%>%
                      pull(Species))


#set names for colors
cols<-setNames(palette(cols)[1:length(levels(te))],
               levels(te))

#reorder trait data
te<-ReorderData(tree.trimmed, te, taxa.names="row names")

## then plot it:
plotTree(tree.trimmed,type = "arc", ftype="i",fsize=0.5,color="darkgrey",
         offset=4.5)
tiplabels(pie=to.matrix(te[tree.trimmed$tip.label],
                        levels(te)),piecol=cols,cex=0.05, col = "transparent")
legend(x="bottomleft",levels(te),pch=22,
       pt.bg=cols,pt.cex=1.5,bty="n",cex=0.9)


map.trees<-make.simmap(tree.trimmed,te,model="ER",
                       nsim=100)


## next plot an outline tree:
plotTree(tree.trimmed,ftype="i",fsize=0.1, type="arc", offset=0.5,
         lwd=1.5, color="black")
par(fg="transparent",lend=1)
plotTree(tree.trimmed, ftype="i", fsize=0.1, type="arc", offset=0.5,
         lwd=1.2, color="white", add=TRUE)
## now plot our 100 stochastic map trees
## with 99% transparency
for(i in 1:length(map.trees)) plot(map.trees[[i]],
                                   colors=sapply(cols,make.transparent,alpha=0.01),
                                   add=TRUE,lwd=1.2,ftype="i", type="arc", fsize=0.1,offset=0.5)
par(fg="black")

legend(x="bottomleft",levels(te),pch=22,
       pt.bg=cols,pt.cex=1.5,bty="n",cex=0.7)


circ <- ggtree(tree.trimmed, data.sort, layout = "circular")
  
circ

rownames() <- tree$tip.label

p1 <- gheatmap(circ, data.sort, offset=.8, width=.2,
               colnames_angle=95, colnames_offset_y = .25) +
  scale_fill_viridis_d(option="D", name="discrete\nvalue")
