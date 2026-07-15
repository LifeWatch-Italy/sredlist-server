

# sRL_fragmentation -----------------------------------------------------

sRL_fragmentation<-function(aoh, aoh_type, dispersion, density_sp){

    ### Create patches and remove unique cells
    patch<-patches(aoh, zeroAsNA=T) %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(crs=CRSMOLL)
    patch$Area<-st_area(patch) %>% as.numeric(.)/1000000


    ### Buffer by dispersal distance and cluster
    buffer<-st_buffer(patch, (dispersion/2)) # Dispersal should be divided by 2 as buffers are applied to all polygons
    buffer$Cluster<-st_intersects(buffer, st_cast(st_union(buffer),"POLYGON")) %>% unlist(.) %>% as.factor(.) # Gives unique identifier to chains of overlapping polygons
    clusters<-buffer %>% dplyr::group_by(Cluster) %>% dplyr::summarise(N=n(), aoh_sum=sum(Area))


    ### Calculate density per cluster + Remove clusters with too few individuals.
    clusters$pop<-as.numeric(clusters$aoh_sum)*density_sp[1] ; clusters$pop<-replace(clusters$pop, clusters$pop<2, 0)
    if(length(density_sp)>1){clusters$pop2<-as.numeric(clusters$aoh_sum)*density_sp[2] ; clusters$pop2<-replace(clusters$pop2, clusters$pop2<2, 0)}


    ### Calculate cumulative sum depending on minimum viable range
    prop.fragm<-clusters[order(clusters$pop, decreasing=F),]
    df_to_add<-data.frame(Cluster=0, N=NA, aoh_sum=0, geometry=NA, pop=0) ; if(length(density_sp)>1){df_to_add$pop2=0}
    prop.fragm<-rbind(df_to_add, prop.fragm)
    prop.fragm$prop.pop=prop.fragm$pop/sum(prop.fragm$pop, na.rm=T)
    prop.fragm$CumSum=cumsum(prop.fragm$prop.pop)
    prop.fragm[(nrow(prop.fragm)+1),c("pop", "CumSum")]<-c(2*max(prop.fragm$pop, na.rm=T), 1)
    if(length(density_sp)>1){prop.fragm$prop.pop2=prop.fragm$pop2/sum(prop.fragm$pop2, na.rm=T)
                             prop.fragm$CumSum2=cumsum(prop.fragm$prop.pop2)
                             prop.fragm[(nrow(prop.fragm)),c("pop", "pop2", "CumSum2")]<-c(2*max(prop.fragm$pop2, na.rm=T), 2*max(prop.fragm$pop2, na.rm=T), 1)
    }

    # Return
    return(list(clusters=clusters, prop.fragm=prop.fragm))

}




# sRL_CalcHumandensity
sRL_CalcHumandensity<-function(scientific_name, username, distSP, GL, AOO_path){
  
  ### Charge recent human layer
  Year2<-2020
  human2<-rast(gsub("XXXX", Year2, config$Human_density_path))

  ### Charge old human layer
  Year1_theo<-min(Year2 - 3*GL, Year2-10) # Takes the year that is 3 GL or 10 years before
  Year1<-c(2000, 2005, 2010, 2015, 2020)[which(abs(Year1_theo-c(2000, 2005, 2010, 2015, 2020))==min(abs(Year1_theo-c(2000, 2005, 2010, 2015, 2020))))][1]
  human1<-rast(gsub("XXXX", Year1, config$Human_density_path))

  ### Mask
  distSP<-st_transform(distSP, st_crs(human1))
  human1_crop<-crop(human1, distSP, snap="out") %>% mask(., distSP)
  human2_crop<-crop(human2, distSP, snap="out") %>% mask(., distSP)
  human_change<-human2_crop-human1_crop

  ### Save rasters
  terra::writeRaster(human2_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Human_density_Current.tif"), overwrite=T)
  terra::writeRaster(human_change, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Human_density_Change.tif"), overwrite=T)
  
  ### Plots
  RS_name="Human population density"
  
  GG_RS=cowplot::plot_grid(
    
    gplot(human2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="Ind/km2")+
      ggtitle(paste0("In ", Year2)) +
      sRLTheme_maps,
    
    gplot(human_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#018571", mid="azure2", midpoint=0, high="#8c510a", name="Ind/km2", na.value="white")+
      ggtitle(paste0("Change ", Year1, "-", Year2)) +
      sRLTheme_maps
    
    ,ncol=2
  )
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_humandensity.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_humandensity.png"), mime = "image/png", encoding = "base64") # nolint

  ### Calculate outputs
  RS_current<-exact_extract(human2_crop, distSP, "median") 
  RS_old<-exact_extract(human1_crop, distSP, "median") # Would be nice to give an absolute number of individuals instead
  RS_timewindow<-paste(Year1, Year2, sep="-")
  RS_trendsABS<-RS_current-RS_old
  RS_trendsREL<-(RS_current-RS_old)/RS_old
  
  LIST_RS <- list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " (median Ind/km2)"),
    RS_trendsABS=paste0(round(RS_trendsABS), " (Ind/km2)"),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change in median"),
    RS_timewindow=RS_timewindow
  )

  ### Add stat within AOO if calculated
  if(is.null(AOO_path)==F){
    AOO_rast <- rast(AOO_path) ; names(AOO_rast)[1] <- "lyr1"
    AOO_map <- AOO_rast %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(., st_crs(human2)) %>% subset(., lyr1==1)
    human2_aoo <- mask(human2_crop, AOO_map)
    LIST_RS$RS_currentAOO <- paste0(round(exact_extract(human2_aoo, distSP, "median")), " (median Ind/km2 within AOO)") 
  }

  ### Return
  return(LIST_RS)
  
}




# sRL_CalcForestchange
sRL_CalcForestchange<-function(scientific_name, username, distSP, GL){

  ### Calculate year for forest 1
  Year2=2025
  Year1<-min(round(Year2 - 3*GL), Year2-10) %>% max(., 2000) # Takes the year that is 3 GL or 10 years before, not before 2000

  ### Charge forest layers
  forest1<-rast(sub("XXXX", Year1, config$ForestAgg_path))
  forest2<-rast(sub("XXXX", Year2, config$ForestAgg_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(forest1))
  forest1_crop<-crop(forest1, distSP, snap="out") %>% mask(., distSP)
  forest2_crop<-crop(forest2, distSP, snap="out") %>% mask(., distSP)
  forest_change<-forest2_crop-forest1_crop
  
  ### Save rasters
  terra::writeRaster(forest2_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Forest_cover_Current.tif"), overwrite=T)
  terra::writeRaster(forest_change, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Forest_cover_Change.tif"), overwrite=T)
  
  ### Plots
  RS_name="Forest cover"
  
  GG_RS=cowplot::plot_grid(
    
    gplot(forest2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="%", limits=c(0,100))+
      ggtitle(paste0("In ", Year2)) +
      sRLTheme_maps,
    
    gplot(forest_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="%", na.value="white")+
      ggtitle(paste0("Change ", Year1, "-", Year2)) +
      sRLTheme_maps,
    
    ncol=2
  )
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forest.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forest.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(forest2_crop, distSP, "mean") 
  rast_area <- cellSize(forest2_crop) / 10^6 # go from m2 to km2
  RS_currentArea<-exact_extract(0.01*rast_area*forest2_crop, distSP, "sum") 
  RS_oldArea<-exact_extract(0.01*rast_area*forest1_crop, distSP, "sum") 
  RS_trendsABS<-(RS_currentArea-RS_oldArea)
  RS_trendsREL<-(RS_currentArea-RS_oldArea)/RS_oldArea
  RS_timewindow<-paste0(Year1, "-", Year2)
  
  ### Return
  return(list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " % (", round(RS_currentArea), " km2)"),
    RS_trendsABS=paste0(round(RS_trendsABS,1), " km2"),
    RS_trendsREL=paste0(100*round(RS_trendsREL,3), " %"),
    RS_timewindow=RS_timewindow
  ))
  
}

# sRL_CalcForestIntegrity
sRL_CalcForestIntegrity<-function(scientific_name, username, distSP, AOO_path){
  
  print("t0")
  print(config$ForestIntegrity_path)
  print(file.exists(config$ForestIntegrity_path))
  
  ### Charge FLII raster
  flii <- rast(config$ForestIntegrity_path)
  print("t1")
  print(summary(flii))
  
  ### Mask
  distSP <- st_transform(distSP, st_crs(flii))
  print("t2")
  flii_crop <- crop(flii, distSP, snap="out") %>% mask(., distSP)/1000
  print("t3")
  print(summary(flii_crop))
  
  ### Save rasters
  terra::writeRaster(flii_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Forest_integrity.tif"), overwrite=T)
  print("t4")
  
  ### Categorise
  flii_cat <- terra::classify(flii_crop, rcl=c(0,6,9.6,10), include.lowest=TRUE)
  print("t5")
  print(summary(flii_cat))
  COL_rast <- levels(droplevels(flii_cat))[[1]]$flii_earth %>% revalue(., c("[0 - 6]"="#8c510a", "(6 - 9.6]"="lightgreen", "(9.6 - 10]"="darkgreen"), warn_missing=F) %>% c(., "white")
  CAT_rast <- levels(droplevels(flii_cat))[[1]]$flii_earth %>% revalue(., c("[0 - 6]"="Low", "(6 - 9.6]"="Medium", "(9.6 - 10]"="High"), warn_missing=F) %>% c(., "")
  
  ### Plots
  RS_name="Forest Landscape Integrity Index"
  print("t6")
  G1 <- gplot(flii_crop)+
    coord_fixed()+
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option="viridis", na.value = "white", name="FLII", limits = c(0, 10))+
    ggtitle("In 2019 (continuous)") +
    sRLTheme_maps
  
  print("t6b")
  G2_delete <- gplot(flii_cat)+
    coord_fixed()+
    geom_tile(aes(fill = value)) +
    scale_fill_manual(values=COL_rast, label=CAT_rast, name="Integrity")+
    ggtitle("In 2019 (categorised)") +
    sRLTheme_maps
  print("t6c")
  
  G2 <- gplot(flii_cat)+
    coord_fixed()+
    geom_tile(aes(fill = factor(value))) +
    scale_fill_manual(values=COL_rast, label=CAT_rast, name="Integrity")+
    ggtitle("In 2019 (categorised)") +
    sRLTheme_maps
  print("t6d")
  
  GG_RS=cowplot::plot_grid(
    
    G1,
    
    G2,
    
    ncol=2
  )
  print("t7")
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forestintegrity.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forestintegrity.png"), mime = "image/png", encoding = "base64") # nolint
  print("t8")
  
  ### Calculate outputs
  RS_current <- exact_extract(flii_crop, distSP, "mean") 
  RS_prop <- summary(flii_cat, size=10^7) %>% as.data.frame(.) %>% mutate(Freq=gsub(" ", "", .$Freq)) %>% separate(Freq, c("Cat", "N"), sep=":") %>% subset(., .$Cat != "NA's") %>% mutate(Percent = round(100*as.numeric(N)/sum(as.numeric(N)), 1))
  RS_detail <- paste0(RS_prop$Percent[RS_prop$Cat=="(9.6-10]"], "% of high integrity; ", RS_prop$Percent[RS_prop$Cat=="(6-9.6]"], "% of medium integrity; ", RS_prop$Percent[RS_prop$Cat=="[0-6]"], "% of low integrity")
  RS_timewindow<-"2019 (data not temporal)"
  print("t9")
  
  LIST_RS <- list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current,1), " (mean)"),
    RS_detail=RS_detail,
    RS_timewindow=RS_timewindow
  )
  
  ### Add stat within AOO if calculated
  print("t10a")
  if(is.null(AOO_path)==F){
    print("t10b")
    AOO_rast <- rast(AOO_path) ; names(AOO_rast)[1] <- "lyr1"
    print("t11")
    AOO_map <- AOO_rast %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(., st_crs(flii)) %>% subset(., lyr1==1)
    print("t12")
    flii_aoo <- mask(flii_crop, AOO_map)
    print("t13")
    flii_aoo_cat <- classify(flii_aoo, rcl=c(0,6,9.6,10), include.lowest=TRUE)
    print("t14")
    
    LIST_RS$RS_currentAOO <- paste0(round(exact_extract(flii_aoo, distSP, "mean") ,1), " (mean)")
    print("t15")
    RS_prop_aoo <- summary(flii_aoo_cat, size=10^7) %>% as.data.frame(.) %>% mutate(Freq=gsub(" ", "", .$Freq)) %>% separate(Freq, c("Cat", "N"), sep=":") %>% subset(., .$Cat != "NA's") %>% mutate(Percent = round(100*as.numeric(N)/sum(as.numeric(N)), 1))
    print("t16")
    LIST_RS$RS_detailAOO <- paste0("Within AOO, ", RS_prop_aoo$Percent[RS_prop_aoo$Cat=="(9.6-10]"], "% of high integrity; ", RS_prop_aoo$Percent[RS_prop_aoo$Cat=="(6-9.6]"], "% of medium integrity; ", RS_prop_aoo$Percent[RS_prop_aoo$Cat=="[0-6]"], "% of low integrity")
  }
  print("t17")
  print(LIST_RS)
  
  ### Return
  return(LIST_RS)
  
}


# sRL_CalcModification
sRL_CalcModification<-function(scientific_name, username, distSP, AOO_path){
  
  ### Charge human modification layers
  human1<-rast(gsub("XXXX", 1990, config$Human_modification_path))
  human2<-rast(gsub("XXXX", 2015, config$Human_modification_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(human1))
  human1_crop<-crop(human1, distSP, snap="out") %>% mask(., distSP)
  human2_crop<-crop(human2, distSP, snap="out") %>% mask(., distSP)
  human1_crop <- human1_crop / 65536 # To get back to original scale (0-1), see ZENODO repo
  human2_crop <- human2_crop / 65536
  human_change<-human2_crop-human1_crop
  
  ### Save rasters
  terra::writeRaster(human2_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Human_modification_Current.tif"), overwrite=T)
  terra::writeRaster(human_change, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Human_modification_Change.tif"), overwrite=T)
  
  ### Plots
  RS_name="Human modification index"
  
  GG_RS=cowplot::plot_grid(
    
    gplot(human2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="")+
      ggtitle(paste0("In ", 2015)) +
      sRLTheme_maps,
    
    gplot(human_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#018571", mid="azure2", midpoint=0, high="#8c510a", name="", na.value="white")+
      ggtitle(paste0("Change ", 1990, "-", 2015)) +
      sRLTheme_maps
    
    ,ncol=2
  )
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_modif.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_modif.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(human2_crop, distSP, "mean") 
  RS_old<-exact_extract(human1_crop, distSP, "mean")
  RS_timewindow<-"1990-2015"
  RS_trendsABS<-RS_current-RS_old
  RS_trendsREL<-(RS_current-RS_old)/RS_old
  
  LIST_RS <- list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current,4), " (mean)"),
    RS_trendsABS=round(RS_trendsABS,4),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change"),
    RS_timewindow=RS_timewindow
  )
  
  ### Add stat within AOO if calculated
  if(is.null(AOO_path)==F){
    AOO_rast <- rast(AOO_path) ; names(AOO_rast)[1] <- "lyr1"
    AOO_map <- AOO_rast %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(., st_crs(human2)) %>% subset(., lyr1==1)
    human2_aoo <- mask(human2_crop, AOO_map)
    LIST_RS$RS_currentAOO <- paste0(round(exact_extract(human2_aoo, distSP, "mean")), " (mean within AOO)") 
  }
  
  ### Return
  return(LIST_RS)
  
}

# sRL_CalcModification: calculate trends in Water Availability
sRL_CalcWater<-function(scientific_name, username, distSP, AOO_path){
  
  ### Charge human modification layers
  water1<-rast(gsub("XXXX", "1984_1999", config$Water_availability_path))
  water2<-rast(gsub("XXXX", "2000_2021", config$Water_availability_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(water1))
  water1_crop<-crop(water1, distSP, snap="out") %>% mask(., distSP)
  water2_crop<-crop(water2, distSP, snap="out") %>% mask(., distSP)
  water_change<-water2_crop-water1_crop
  
  ### Save rasters
  terra::writeRaster(water2_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Water_availability_Current.tif"), overwrite=T)
  terra::writeRaster(water_change, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Water_availability_Change.tif"), overwrite=T)
  
  ### Plots
  RS_name="Water availability"
  
  GG_RS=cowplot::plot_grid(
    
    gplot(water2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="%")+
      ggtitle("Average 2000-2021") +
      sRLTheme_maps,
    
    gplot(water_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="%", na.value="white")+
      ggtitle("Change from (1984-1999) to (2000-2021)") +
      sRLTheme_maps

    ,ncol=2
  )
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_water.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_water.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  rast_area <- cellSize(water2_crop) / 10^6 # go from m2 to km2
  water2_area <- 0.01*rast_area*water2_crop
  RS_current<-exact_extract(water2_crop, distSP, "mean") 
  RS_currentArea<-exact_extract(water2_area, distSP, "sum") 
  RS_oldArea<-exact_extract(0.01*water1_crop*rast_area, distSP, "sum")
  RS_timewindow<-"(1984-1999) to (2000-2021)"
  RS_trendsABS<-RS_currentArea-RS_oldArea
  RS_trendsREL<-(RS_currentArea-RS_oldArea)/RS_oldArea
  
  LIST_RS <- list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " % (", round(RS_currentArea), " km2)"),
    RS_trendsABS=paste0(round(RS_trendsABS,1), " km2"),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change"),
    RS_timewindow=RS_timewindow
  )
  
  ### Add stat within AOO if calculated
  if(is.null(AOO_path)==F){
    AOO_rast <- rast(AOO_path) ; names(AOO_rast)[1] <- "lyr1"
    AOO_map <- AOO_rast %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(., st_crs(water2)) %>% subset(., lyr1==1)
    water2_aoo <- mask(water2_area, AOO_map)
    LIST_RS$RS_currentAOO <- paste0(round(exact_extract(water2_aoo, distSP, "sum")), " (km2 within AOO)") 
  }
  ### Return
  return(LIST_RS)
  
}


# sRL_CalcMarineImpact: calculate trends in marine impact
sRL_CalcMarineImpact<-function(scientific_name, username, distSP){
  
  ### Charge human modification layers
  marine2<-rast(sub("Change-2008-", "", config$Marine_Impact_Halpern_path))
  marine_change<-rast(config$Marine_Impact_Halpern_path)
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(marine2))
  marine2_crop<-crop(marine2, distSP, snap="out") %>% mask(., distSP)/10
  marinechange_crop<-crop(marine_change, distSP, snap="out") %>% mask(., distSP)/10
  
  ### Save rasters
  terra::writeRaster(marine2_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Marine_Impact_Current.tif"), overwrite=T)
  terra::writeRaster(marinechange_crop, paste0("resources/AOH_stored/", gsub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Marine_Impact_Change.tif"), overwrite=T)
  
  ### Plots
  RS_name="Human impact to marine ecosystems"
  
  GG_RS=cowplot::plot_grid(
    
    gplot(marine2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="")+
      ggtitle("Cumulative impact in 2013") +
      sRLTheme_maps,
    
    gplot(marinechange_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#018571", mid="azure2", midpoint=0, high="#8c510a", name="", na.value="white")+
      ggtitle("Change from 2008 to 2013") +
      sRLTheme_maps
    
    ,ncol=2
  )
  
  EXT <- extent(distSP) ; size_scale <- (EXT[2]-EXT[1])/(EXT[4]-EXT[3])
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_marineimpact.png"), plot = GG_RS, bg="white", width=12, height=6/size_scale)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_marineimpact.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  rast_area <- cellSize(marine2_crop) / 10^6 # go from m2 to km2
  marine2_area <- rast_area*marine2_crop
  RS_current<-exact_extract(marine2_crop, distSP, "mean") 
  RS_timewindow<-"2008 to 2013"
  RS_trendsABS<-exact_extract(marinechange_crop, distSP, "mean") 

  LIST_RS <- list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=round(RS_current,2),
    RS_trendsABS=round(RS_trendsABS,2),
    RS_timewindow=RS_timewindow
  )
  
  ### Return
  return(LIST_RS)
  
}
