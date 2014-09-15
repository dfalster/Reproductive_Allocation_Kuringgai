library(igraph)
GraphMaps=list()

LEES.graph=graph.formula("bud_tiny"-"bud_mid"-"bud_big"-"flower_calyx"-"finished_flower"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_mature","bud_tiny"-"bud_aborted","bud_big"-"flower_petals","fruit_young"-"calyx_fruit","fruit_young"-"fruit_aborted")
from="bud_tiny"
to="fruit_mature"
Paths=data.frame(from=from,to=to)
#Setting colours indicating main progression line together with its XORs. The accesorry tissues are to have different colour, mark only the final
#element of the accesory line! Accesorries to the same line are to be marked in their own colour of value 1 larger than the path itself
LEES.graph=set.vertex.attribute(LEES.graph,name="col",index=V(LEES.graph),value=3)
LEES.graph=set.vertex.attribute(LEES.graph,name="col",index=c(11,12),value=4)
#Setting edge weight which defines how much of carbon is beeing directed from each part to each part. Especially important for species
#with accessories
LEES.graph=set.edge.attribute(LEES.graph,name="weight",value=1)
LEES.graph=set.edge.attribute(LEES.graph,name="weight",index=c(4,5,9,10),value=c(0.6,0.4,0.81,0.19))
GraphMaps[["LEES"]]=list(graph=LEES.graph,Paths=Paths)
rm(LEES.graph)



GRBU.graph=graph.formula("bud_small"-"bud_mid"-"bud_big"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_large_immature_02"-"fruit_large_immature_03"-"fruit_large_immature_04"-"fruit_large_immature_05"-"fruit_large_immature_06"-"seed","inflorescence_bud_small"-"inflorescence_stalk","bud_big"-"flower_petals","fruit_young"-"pedicel","fruit_large_immature_06"-"seed_pod","fruit_large_immature_06"-"seed_aborted")
from=c("bud_small","inflorescence_bud_small")
to=c("seed","inflorescence_stalk")
Paths=data.frame(from=from,to=to)
GRBU.graph=set.vertex.attribute(GRBU.graph,name="col",index=c(1:14,20),value=3)
GRBU.graph=set.vertex.attribute(GRBU.graph,name="col",index=17:19,value=4)
GRBU.graph=set.vertex.attribute(GRBU.graph,name="col",index=15:16,value=5)
#Setting edge weight which defines how much of carbon is beeing directed from each part to each part. Especially important for species
#with accessories
GRBU.graph=set.edge.attribute(GRBU.graph,name="weight",value=1)
GRBU.graph=set.edge.attribute(GRBU.graph,name="weight",index=c(3,4,8,9,15,16,17),value=c(0.63,0.37,0.85,0.15,0.08,0.92,0.08))
# Saving the graph
GraphMaps[["GRBU"]]=list(graph=GRBU.graph,Paths=Paths)
rm(GRBU.graph)


GRSP.graph=graph.formula("bud_tiny"-"bud_small"-"bud_mid"-"bud_big"-"bud_just_opening"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_large_immature_02"-"fruit_large_immature_03"-"fruit_large_immature_04"-"fruit_large_immature_05"-"fruit_large_immature_06"-"seed","inflorescence_bud_small"-"inflorescence_stalk"-"inflorescence_stalk_in_fruit","bud_just_opening"-"flower_petals","fruit_young"-"pedicel","fruit_large_immature_06"-"seed_pod","fruit_large_immature_06"-"seed_aborted")
from=c("bud_tiny","inflorescence_bud_small")
to=c("seed","inflorescence_stalk_in_fruit")
Paths=data.frame(from=from,to=to)
#Set colors
GRSP.graph=set.vertex.attribute(GRSP.graph,name="col",index=c(1:16,23),value=3)
GRSP.graph=set.vertex.attribute(GRSP.graph,name="col",index=20:22,value=4)
GRSP.graph=set.vertex.attribute(GRSP.graph,name="col",index=17:19,value=5)
#Set weights
GRSP.graph=set.edge.attribute(GRSP.graph,name="weight",value=1)
GRSP.graph=set.edge.attribute(GRSP.graph,name="weight",index=c(5,6,10,11,17,18,19),value=c(0.4,0.6,0.92,0.08,0.1,0.9,0.1))
GraphMaps[["GRSP"]]=list(graph=GRSP.graph,Paths=Paths)
rm(GRSP.graph)


# Temporary changes.
EPMI.graph=graph.formula("bud_tiny"-"bud_mid"-"bud_big"-"flower_calyx"-"finished_flower"-"fruit_young"-"fruit_large_immature_01"-"fruit_mature","bud_big"-"flower_petals")
from="bud_tiny"
to="fruit_mature"
Paths=data.frame(from=from,to=to)
#Set vertex color
EPMI.graph=set.vertex.attribute(EPMI.graph,name="col",index=c(1:8),value=3)
EPMI.graph=set.vertex.attribute(EPMI.graph,name="col",index=9,value=4)
#Set edge color
EPMI.graph=set.edge.attribute(EPMI.graph,name="weight",value=1)
EPMI.graph=set.edge.attribute(EPMI.graph,name="weight",index=c(3,4),value=c(0.53,0.47))
GraphMaps[["EPMI"]]=list(graph=EPMI.graph,Paths=Paths)
rm(EPMI.graph)


COER.graph=graph.formula("bud_big"-"flower_stigma"-"finished_flower"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_mature","inflorescence_bud_tiny"-"inflorescence_bud_mid"-"inflorescence_stalk"-"inflorescence_stalk_in_fruit","inflorescence_stalk"-"inflorescence_stalk_in_fruit_large","inflorescence_stalk"-"inflorescence_stalk_in_fruit_very_large","bud_big"-"flower_petals","fruit_just_starting"-"bract_fruit")
from=c("bud_big","inflorescence_bud_tiny")
to=c("fruit_mature","inflorescence_stalk_in_fruit_very_large")
Paths=data.frame(from=from,to=to)
#First progression with accessories
COER.graph=set.vertex.attribute(COER.graph,name="col",index=c(1:7),value=3)
COER.graph=set.vertex.attribute(COER.graph,name="col",index=14:15,value=4)
#Second progression without accessories
COER.graph=set.vertex.attribute(COER.graph,name="col",index=8:13,value=5)
#Set the weights for all edges
COER.graph=set.edge.attribute(COER.graph,name="weight",value=1)
COER.graph=set.edge.attribute(COER.graph,name="weight",index=c(1,2,5,6),value=c(0.32,0.68,0.53,0.47))
GraphMaps[["COER"]]=list(graph=COER.graph,Paths=Paths)
rm(COER.graph)



PUTU.graph=graph.formula("bud_big"-"flower_stigma"-"finished_flower_stigma"-"fruit_large_immature_01"-"seed","bud_big"-"flower_petals","bud_big"-"flower_calyx","bud_big"-"bract_flower_or_finished_flower","flower_aborted","flower_stigma"-"fruit_aborted","fruit_large_immature_01"-"seed_pod","fruit_large_immature_01"-"seed_aborted")
from=c("bud_big","flower_aborted")
to=c("seed","flower_aborted")
Paths=data.frame(from=from,to=to)
#Set colors to vertices
PUTU.graph=set.vertex.attribute(PUTU.graph,name="col",index=c(1:5,10,12),value=3)
PUTU.graph=set.vertex.attribute(PUTU.graph,name="col",index=c(6:8,11),value=4)
PUTU.graph=set.vertex.attribute(PUTU.graph,name="col",index=c(9),value=5)
#Set weights to all edges
PUTU.graph=set.edge.attribute(PUTU.graph,name="weight",value=1)
PUTU.graph=set.edge.attribute(PUTU.graph,name="weight",index=c(1,2,3,4,8,9,10),value=c(0.1596,0.5122,0.1557,0.1725,0.19,0.81,0.19))
GraphMaps[["PUTU"]]=list(graph=PUTU.graph,Paths=Paths)
rm(PUTU.graph)




BAER.graph=graph.formula("cone_base_green_01"-"cone_base_green_02"-"cone_base_green_03"-"cone_base_green_04"-"cone_base_brown","cone_young_01"-"cone_young_02"-"cone_young_03"-"cone_young_04"-"cone_green_01"-"cone_green_02"-"cone_green_03"-"cone_green_04"-"cone_brown","cone_green_04"-"cone_brown_no_expanded_follicles","cone_green_04"-"cone_aborted","bud_tiny"-"bud_small"-"bud_mid"-"bud_big"-"bud_just_opening"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"seed","bud_just_opening"-"flower_petals","bud_just_opening"-"flower_style","fruit_large_immature_01"-"seed_pod","fruit_large_immature_01"-"seed_aborted")
from=c("bud_tiny","cone_young_01","cone_base_green_01")
to=c("seed","cone_brown","cone_base_brown")
Paths=data.frame(from=from,to=to)
#Set colors
BAER.graph=set.vertex.attribute(BAER.graph,name="col",index=c(1:5),value=1)
BAER.graph=set.vertex.attribute(BAER.graph,name="col",index=c(6:16),value=5)
BAER.graph=set.vertex.attribute(BAER.graph,name="col",index=c(17:27,31),value=3)
BAER.graph=set.vertex.attribute(BAER.graph,name="col",index=c(28:30),value=4)
#Set weights
BAER.graph=set.edge.attribute(BAER.graph,name="weight",value=1)
BAER.graph=set.edge.attribute(BAER.graph,name="weight",index=c(19,20,21,26,27,28),value=c(0.28,0.44,0.28,0.07,0.93,0.07))
GraphMaps[["BAER"]]=list(graph=BAER.graph,Paths=Paths)
rm(BAER.graph)




BOLE.graph=graph.formula("bud_tiny"-"bud_small"-"bud_mid"-"bud_big"-"flower_calyx"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"seed","bud_big"-"pedicel","bud_big"-"flower_petals","flower_calyx"-"finished_flower"-"late_finished_flower","fruit_large_immature_01"-"seed_pod")
from=c("bud_tiny")
to=c("seed")
Paths=data.frame(from=from,to=to)
#Set vertex colors to define paths and their accessories
BOLE.graph=set.vertex.attribute(BOLE.graph,name="col",index=c(1:10),value=3)
BOLE.graph=set.vertex.attribute(BOLE.graph,name="col",index=c(11,12,13,14,15),value=4)
#Set edge weight to define carbon flow
BOLE.graph=set.edge.attribute(BOLE.graph,name="weight",value=1)
BOLE.graph=set.edge.attribute(BOLE.graph,name="weight",index=c(4,5,6,7,8,12,13),value=c(0.486,0.116,0.4,0.03,0.97,0.48,0.52))
GraphMaps[["BOLE"]]=list(graph=BOLE.graph,Paths=Paths)
rm(BOLE.graph)



HEPU.graph=graph.formula("bud_small"-"bud_big"-"flower_calyx"-"finished_flower"-"fruit_young"-"fruit_large_immature_01"-"fruit_mature","bud_big"-"flower_petals","flower_calyx"-"fruit_aborted","finished_flower"-"calyx_fruit")
from=c("bud_small")
to=c("fruit_mature")
Paths=data.frame(from=from,to=to)
#Set vertex colors
HEPU.graph=set.vertex.attribute(HEPU.graph,name="col",index=c(1:7,9),value=3)
HEPU.graph=set.vertex.attribute(HEPU.graph,name="col",index=c(8,10),value=4)
#Set egde weights
HEPU.graph=set.edge.attribute(HEPU.graph,name="weight",value=1)
HEPU.graph=set.edge.attribute(HEPU.graph,name="weight",index=c(2,3,6,7),value=c(0.4433,0.5567,0.4658,0.5342))
GraphMaps[["HEPU"]]=list(graph=HEPU.graph,Paths=Paths)
rm(HEPU.graph)



PILI.graph=graph.formula("bud_big"-"flower_stigma"-"fruit_young"-"fruit_large_immature_01"-"seed","inflorescence_bud_mid"-"inflorescence_stalk","inflorescence_bud_mid"-"bract_flower_or_finished_flower","bud_big"-"flower_calyx","bud_big"-"flower_petals","fruit_young"-"fruit_aborted","fruit_large_immature_01"-"seed_pod")
from=c("bud_big","inflorescence_bud_mid")
to=c("seed","inflorescence_stalk")
Paths=data.frame(from=from,to=to)
#Set vertex colors
PILI.graph=set.vertex.attribute(PILI.graph,name="col",index=c(1:5,11),value=2)
PILI.graph=set.vertex.attribute(PILI.graph,name="col",index=c(9,10,12),value=3)
PILI.graph=set.vertex.attribute(PILI.graph,name="col",index=c(6:7),value=4)
PILI.graph=set.vertex.attribute(PILI.graph,name="col",index=8,value=5)
#Set egde weights
PILI.graph=set.edge.attribute(PILI.graph,name="weight",value=1)
PILI.graph=set.edge.attribute(PILI.graph,name="weight",index=c(1,2,3,7,8,9,10),value=c(0.02,0.2,0.78,0.61,0.39,0.22,0.78))
GraphMaps[["PILI"]]=list(graph=PILI.graph,Paths=Paths)
rm(PILI.graph)



PHPH.graph=graph.formula("bud_small"-"bud_mid"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"seed","bud_mid"-"flower_petals_small"-"flower_petals","bud_mid"-"bract_flower_or_finished_flower","bud_mid"-"flower_calyx","bud_small"-"flower_aborted","flower_aborted_without_petals","fruit_young"-"fruit_aborted","fruit_large_immature_01"-"seed_pod","fruit_large_immature_01"-"seed_aborted")
from=c("bud_small","flower_aborted_without_petals")
to=c("seed","flower_aborted_without_petals")
Paths=data.frame(from=from,to=to)
#Set vertex color
PHPH.graph=set.vertex.attribute(PHPH.graph,name="col",index=c(1:8,13,15,17),value=3)
PHPH.graph=set.vertex.attribute(PHPH.graph,name="col",index=c(9,10,11,12,16),value=4)
PHPH.graph=set.vertex.attribute(PHPH.graph,name="col",index=c(14),value=5)
#Set egde weights
PHPH.graph=set.edge.attribute(PHPH.graph,name="weight",value=1)
PHPH.graph=set.edge.attribute(PHPH.graph,name="weight",index=c(3,4,5,6,12,13,14),value=c(0.1269,0.3441,0.3413,0.1876,0.24,0.76,0.24))
GraphMaps[["PHPH"]]=list(graph=PHPH.graph,Paths=Paths)
rm(PHPH.graph)



PELA.graph=graph.formula("bud_small"-"bud_mid"-"bud_big"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_large_immature_02"-"fruit_large_immature_03"-"fruit_large_immature_04"-"fruit_large_immature_05"-"fruit_large_immature_06"-"seed","bud_big"-"flower_petals","bud_big"-"pedicel","fruit_large_immature_06"-"seed_pod")
from=c("bud_small")
to=c("seed")
Paths=data.frame(from=from,to=to)
#Set vertex colors
PELA.graph=set.vertex.attribute(PELA.graph,name="col",index=c(1:14),value=3)
PELA.graph=set.vertex.attribute(PELA.graph,name="col",index=15:17,value=4)
#Set edge color
PELA.graph=set.edge.attribute(PELA.graph,name="weight",value=1)
PELA.graph=set.edge.attribute(PELA.graph,name="weight",index=c(3,4,5,15,16),value=c(0.08,0.78,0.14,0.42,0.58))
GraphMaps[["PELA"]]=list(graph=PELA.graph,Paths=Paths)
rm(PELA.graph)

HATE.graph=graph.formula("inflorescence_bud_tiny"-"inflorescence_bud_small"-"inflorescence_bud_mid"-"inflorescence_bud_big_flowers"-"flower_stigma"-"finished_flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"seed_immature"-"seed","inflorescence_bud_mid"-"inflorescence_bud_big_bracts","inflorescence_bud_big_flowers"-"flower_petals","fruit_young"-"fruit_aborted","fruit_large_immature_01"-"seed_pod","fruit_large_immature_01"-"seed_aborted")
from=c("inflorescence_bud_tiny")
to=c("seed")
Paths=data.frame(from=from,to=to)
#Set vertex colors
HATE.graph=set.vertex.attribute(HATE.graph,name="col",index=c(1:11,14,16),value=3)
HATE.graph=set.vertex.attribute(HATE.graph,name="col",index=c(12,13,15),value=4)
#Set edge color
HATE.graph=set.edge.attribute(HATE.graph,name="weight",value=1)
HATE.graph=set.edge.attribute(HATE.graph,name="weight",index=c(3,4,5,6,12,13,14),value=c(0.44,0.56,0.45,0.55,0.01,0.99,0.01))
GraphMaps[["HATE"]]=list(graph=HATE.graph,Paths=Paths)
rm(HATE.graph)


PEPU.graph=graph.formula("bud_tiny"-"bud_big"-"flower_stigma"-"fruit_just_starting"-"fruit_young"-"fruit_large_immature_01"-"fruit_mature","cone_just_starting_01"-"cone_just_starting_02"-"cone_just_starting_03"-"cone_just_starting_04"-"cone_just_starting_05"-"cone_young_01"-"cone_young_02"-"cone_young_03"-"cone_young_04"-"cone_green_01"-"cone_green_02"-"cone_green_03"-"cone_green_04"-"cone_brown","cone_green_04"-"cone_aborted","bud_tiny"-"bud_aborted","bud_big"-"flower_petals","bud_big"-"flower_calyx","fruit_large_immature_01"-"fruit_empty","fruit_just_starting"-"fruit_aborted")
from=c("bud_tiny","cone_just_starting_01")
to=c("fruit_mature","cone_brown")
Paths=data.frame(from=from,to=to)
#Set vertex colors
PEPU.graph=set.vertex.attribute(PEPU.graph,name="col",index=c(1:7,23,26,27),value=3)
PEPU.graph=set.vertex.attribute(PEPU.graph,name="col",index=c(24,25),value=4)
PEPU.graph=set.vertex.attribute(PEPU.graph,name="col",index=c(8:22),value=5)
#Set edge color
PEPU.graph=set.edge.attribute(PEPU.graph,name="weight",value=1)
PEPU.graph=set.edge.attribute(PEPU.graph,name="weight",index=c(3,4,5),value=c(0.22,0.66,0.12))
GraphMaps[["PEPU"]]=list(graph=PEPU.graph,Paths=Paths)
rm(PEPU.graph)


# Example plot
# tkplot(GraphMaps[["LEES"]]$graph,vertex.color=V(GraphMaps[["LEES"]]$graph)$col,edge.label=E(GraphMaps[["LEES"]]$graph)$weight)
