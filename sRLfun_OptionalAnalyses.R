

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
