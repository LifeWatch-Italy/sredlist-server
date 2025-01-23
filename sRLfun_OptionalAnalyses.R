

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
sRL_CalcHumandensity<-function(scientific_name, username, distSP, GL){
  
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
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_humandensity.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_humandensity.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(human2_crop, distSP, "median") 
  RS_old<-exact_extract(human1_crop, distSP, "median") # Would be nice to give an absolute number of individuals instead
  RS_timewindow<-paste(Year1, Year2, sep="-")
  RS_trendsABS<-RS_current-RS_old
  RS_trendsREL<-(RS_current-RS_old)/RS_current
  
  ### Return
  return(list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " (median Ind/km2)"),
    RS_trendsABS=paste0(round(RS_trendsABS), " (Ind/km2)"),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change in median"),
    RS_timewindow=RS_timewindow
  ))
  
}




# sRL_CalcForestchange
sRL_CalcForestchange<-function(scientific_name, username, distSP, GL){

  ### Calculate year for forest 1
  Year2=2022
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
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forest.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_forest.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(forest2_crop, distSP, "mean") 
  RS_old<-exact_extract(forest1_crop, distSP, "mean")
  RS_trendsABS<-(RS_current-RS_old)
  RS_trendsREL<-(RS_current-RS_old)/RS_old
  RS_timewindow<-paste0(Year1, "-", Year2)
  
  
  ### Return
  return(list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current,1), "%"),
    RS_trendsABS=paste0(round(RS_trendsABS,1), " %"),
    RS_trendsREL=paste0(100*round(RS_trendsREL,3), " %"),
    RS_timewindow=RS_timewindow
  ))
  
}


# sRL_CalcModification
sRL_CalcModification<-function(scientific_name, username, distSP){
  
  ### Charge human modification layers
  human1<-rast(gsub("XXXX", 1990, config$Human_modification_path))
  human2<-rast(gsub("XXXX", 2015, config$Human_modification_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(human1))
  human1_crop<-crop(human1, distSP, snap="out") %>% mask(., distSP)
  human2_crop<-crop(human2, distSP, snap="out") %>% mask(., distSP)
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
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_modif.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_modif.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(human2_crop, distSP, "mean") 
  RS_old<-exact_extract(human1_crop, distSP, "mean")
  RS_timewindow<-"1990-2015"
  RS_trendsABS<-RS_current-RS_old
  RS_trendsREL<-(RS_current-RS_old)/RS_current
  
  ### Return
  return(list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " (mean)"),
    RS_trendsABS=round(RS_trendsABS),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change"),
    RS_timewindow=RS_timewindow
  ))
  
}

# sRL_CalcModification: calculate trends in Water Availability
sRL_CalcWater<-function(scientific_name, username, distSP){
  
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
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_water.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "_", sRL_userdecode(username), "/Plots/RS_plot_water.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(water2_crop, distSP, "mean") 
  RS_old<-exact_extract(water1_crop, distSP, "mean")
  RS_timewindow<-"(1984-1999) to (2000-2021)"
  RS_trendsABS<-RS_current-RS_old
  RS_trendsREL<-(RS_current-RS_old)/RS_current
  
  ### Return
  return(list(
    RS_prodname=RS_name,
    RS_plot=RS_plot,
    RS_current=paste0(round(RS_current), " (mean)"),
    RS_trendsABS=round(RS_trendsABS),
    RS_trendsREL=paste0(100*round(RS_trendsREL, 3), " % change"),
    RS_timewindow=RS_timewindow
  ))
  
}
