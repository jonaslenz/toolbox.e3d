
# run prepocessor
system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", '/r "C:/Users/Jonas Lenz/Desktop/lisem/no_pond_rel.par"', wait=TRUE)

# write Soilparams
a <- read.csv("C:/Users/Jonas Lenz/Desktop/lisem/soil_params.csv")
a$BLKDENSITY <- 1500
a$CORG <- 1.7
a$SKINFACTOR <- 0.18
a$INITMOIST <- 20
a$ROUGHNESS <- 0.015
a$ERODIBIL <- 0.015
a$COVER <- 0
a$FT <- 0
a$MT <- 19
a$GT <- 0
a$FU <- 14
a$MU <- 23
a$GU <- 20
a$FS <- 7
a$MS <- 7
a$GS <- 10
a$KS_GW <- -9999
a$ROCKFRAGM <- -9999
a$THETA_R <- -9999
a$THETA_S <- -9999
a$ALPHA <- -9999
a$NORDPOL <- -9999
a$KS <- -9999
write.csv(a, "C:/Users/Jonas Lenz/Desktop/lisem/soil/soil_params.csv", row.names = FALSE, quote = FALSE)

# run prepocessor
#system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", '/s "C:/Users/Jonas Lenz/Desktop/lisem/no_pond_rel.par"', wait=TRUE)


#run model
system2("C:/Program Files (x86)/Erosion-3D_32-320/e3d", '/c "C:/Users/Jonas Lenz/Desktop/lisem/pond.par"', wait=TRUE)
max(values(raster(read.asciigrid("C:/Users/Jonas Lenz/Desktop/lisem/result/sum_q.asc"))))
