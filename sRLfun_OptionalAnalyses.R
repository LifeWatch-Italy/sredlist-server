

# sRL_fragmentation -----------------------------------------------------

sRL_fragmentation<-function(aoh, aoh_type, dispersion, density_sp){

    ### If large range, I have to binarize the suitable habitat
    if(aoh_type=="Large"){aoh<-ifel(aoh<=90, 0, 1)}

    ### Create patches and remove unique cells
    patch<-patches(aoh, zeroAsNA=T) %>% as.polygons(.) %>% st_as_sf(.) %>% st_transform(crs=CRSMOLL)
    patch$Area<-st_area(patch) %>% as.numeric(.)/1000000


    ### Buffer by dispersal distance and cluster
    buffer<-st_buffer(patch, (dispersion/2)) # Dispersal should be divided by 2 as buffers are applied to all polygons
    buffer$Cluster<-st_intersects(buffer, st_cast(st_union(buffer),"POLYGON")) %>% unlist(.) %>% as.factor(.) # Gives unique identifier to chains of overlapping polygons
    clusters<-buffer %>% dplyr::group_by(Cluster) %>% dplyr::summarise(N=n(), aoh_sum=sum(Area))


    ### Calculate density per cluster
    clusters$pop<-as.numeric(clusters$aoh_sum)*density_sp
    clusters<-subset(clusters, clusters$pop>2) # Remove clusters with too few individuals. TO DECIDE WITH LUCA


    ### Calculate cumulative sum depending on minimum viable range
    prop.fragm<-clusters[order(clusters$pop, decreasing=F),]
    prop.fragm<-rbind(data.frame(Cluster=0, N=NA, aoh_sum=0, geometry=NA, pop=0), prop.fragm)
    prop.fragm$prop.pop=prop.fragm$pop/sum(prop.fragm$pop)
    prop.fragm$CumSum=cumsum(prop.fragm$prop.pop)
    prop.fragm[(nrow(prop.fragm)+1),c("pop", "CumSum")]<-c(2*max(prop.fragm$pop, na.rm=T), 1)

    # Return
    return(list(clusters=clusters, prop.fragm=prop.fragm))

}




# sRL_CalcHumandensity
sRL_CalcHumandensity<-function(scientific_name, distSP, GL){
  
  ### Charge recent human layer
  Year2<-2020
  human2<-rast(gsub("XXXX", Year2, config$Human_density_path))
  
  ### Charge old human layer
  Year1_theo<-min(Year2 - 3*GL, Year2-10) # Takes the year that is 3 GL or 10 years before
  Year1<-c(2000, 2005, 2010, 2015, 2020)[which(abs(Year1_theo-c(2000, 2005, 2010, 2015, 2020))==min(abs(Year1_theo-c(2000, 2005, 2010, 2015, 2020))))][1]
  human1<-rast(gsub("XXXX", Year1, config$Human_density_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(human1))
  human1_crop<-crop(human1, distSP) %>% mask(., distSP)
  human2_crop<-crop(human2, distSP) %>% mask(., distSP)
  human_change<-human2_crop-human1_crop
  
  ### Plots
  RS_name="Human population density"
  
  GG_RS=grid.arrange(
    
    gplot(human2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="Ind/km2")+
      ggtitle(paste0("In ", Year2)) +
      sRLTheme_maps,
    
    gplot(human_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="Ind/km2", na.value="white")+
      ggtitle(paste0("Change ", Year1, "-", Year2)) +
      sRLTheme_maps
    
    ,ncol=2, top=RS_name
  )
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/RS_plot.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/RS_plot.png"), mime = "image/png", encoding = "base64") # nolint
  
  
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
sRL_CalcForestchange<-function(scientific_name, distSP){

  ### Charge forest layers
  forest1<-rast(sub("XXXX", 2000, config$ForestAgg_path))
  forest2<-rast(sub("XXXX", 2022, config$ForestAgg_path))
  
  ### Mask
  distSP<-st_transform(distSP, st_crs(forest1))
  forest1_crop<-crop(forest1, distSP) %>% mask(., distSP)
  forest2_crop<-crop(forest2, distSP) %>% mask(., distSP)
  forest_change<-forest2_crop-forest1_crop
  
  ### Plots
  RS_name="Forest cover"
  
  GG_RS=grid.arrange(
    
    gplot(forest2_crop)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_viridis_c(option="viridis", na.value = "white", name="%", limits=c(0,100))+
      ggtitle(paste0("In 2022")) +
      sRLTheme_maps,
    
    gplot(forest_change)+
      coord_fixed()+
      geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low="#8c510a", mid="azure2", midpoint=0, high="#018571", name="%", na.value="white")+
      ggtitle(paste0("Change 2000-2022")) +
      sRLTheme_maps
    
    ,ncol=2, top=RS_name
  )
  
  ggsave(filename = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/RS_plot.png"), plot = GG_RS, width=10, height=6)
  RS_plot <- base64enc::dataURI(file = paste0("resources/AOH_stored/", sub(" ", "_", scientific_name), "/Plots/RS_plot.png"), mime = "image/png", encoding = "base64") # nolint
  
  
  ### Calculate outputs
  RS_current<-exact_extract(forest2_crop, distSP, "mean") 
  RS_old<-exact_extract(forest1_crop, distSP, "mean")
  RS_trendsABS<-(RS_current-RS_old)
  RS_trendsREL<-(RS_current-RS_old)/RS_old
  RS_timewindow<-"2000-2022"
  
  
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


# sRL_CalcNDVIchange
sRL_CalcNDVIchange<-function(scientific_name, distSP, GL){
  
  ### Return
  return(list(
    RS_plot=NULL,
    RS_current=NA,
    RS_trends=NA,
    RS_timewindow=NA
  ))
  
}



