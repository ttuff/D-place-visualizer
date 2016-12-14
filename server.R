require(shiny)
require(shinydashboard)
require(raster)
require(ape)
require(spdep)
require(leaflet)
require(RColorBrewer)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
 
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  
  
  pal <- colorFactor(c("gold", "red"), domain = c(0,1), na.color="transparent")
  
  output$mymap_1 <- renderLeaflet({
    
    plant_potential <- raster("www/richnobuf.asc")
    
    load('www/ClimateDataRaster_ElevDownscaled.2016.Rdata')
   
    load('www/TempHarsh_raster.Rdata')
    load('www/XeroHarsh_raster.Rdata')
    
    objects()
    
    raster_list <-  list( Biomes_raster, Biomes_raster, Biomes_raster, EcoRegions_raster, OlsonEcoregions_raster, AmphibianDiv_raster, BirdDiv_raster, 
                          MammalDiv_raster, VascPlant_CoKrig_KreftJetz2007_raster, plant_potential,Elev_raster, Slope_raster, 
                          Glaciation, Sand_raster, LocalHeterogeneity_raster, MeanN_raster, MeanP_raster, MeanP_raster1950,
                          MeanT_raster, MeanT_raster1950, VarN_raster, VarP_raster, VarP_raster1950,  VarT_raster ,                     
                          VarT_raster1950 , TempHarsh_raster, XeroHarsh_raster, Pn_raster, Pp_raster, Pp_raster1950,                    
                          Pt_raster, Pt_raster1950, Cn_raster, Cp_raster , Cp_raster1950, Ct_raster ,  Ct_raster1950,
                          Mp_raster, Mp_raster1950, Mt_raster, Mt_raster1950)
    
    r <- raster_list[[as.numeric(input$raster_layer_1)]]
    
    colfunc  <- colorRampPalette(c("#f0f0f0",
                                   "#969696",
                                   "#000000"))
    
    
    
    crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    #leaflet() %>% addTiles() %>%
    # addRasterImage(r, colors = pal, opacity = 0.8) %>%
    #  addLegend(pal = pal, values = values(r),
    #          title = "Surface temp")
    
    if("Full" == input$tree_choose_1){
      load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_FULL_trimmed.Rdata')
      tree_choice <- full_tree
      binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
    if("Uto" == input$tree_choose_1){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Uto_trimmed.RData')
      tree_choice <- uto_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Bantu" == input$tree_choose_1){
      load('www/BANTU_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Bantu_trimmed.RData')
      tree_choice <- bantu_tree
      binary_traits <- BANTU_Society_data_with_binary_conversions}
    if("Indo-european" == input$tree_choose_1){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_IE.RData')
      tree_choice <-  IE_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Pama-Nyungan" == input$tree_choose_1){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Pama_Nyungan.RData')
      tree_choice <- pama_nyungan_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Austronesian" == input$tree_choose_1){
      load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_austronisian_trimmed.RData')
      tree_choice <- aus_tree
      binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
    
    
    dim(binary_traits)
    binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
    dim(binary_traits)
    
    legend_label <-  c(
      
      "no layer",
      "    " ,
      "Biomes"  , 
      "EcoRegions"   ,
      "Olson Ecoregions"  ,            
      "Amphibian diversity" ,                 
      "Bird diversity"  , 
      "Mammal diversiy"  ,
      "Vascular Plant diversity from Kreft and Jetz 2007",
      "Potential domesticable species" ,
      "Elevation" , 
      "Slope"    ,
      "Glaciation"  ,  
      "Sand type"   ,                       
      "Local heterogeneity"  , 
      "Mean NPP"  ,                       
      "Mean precipitation"  ,                       
      "Mean precipitation 1950"   ,                  
      "Mean temperature"  ,                       
      "Mean temperature 1950"    ,   
      "Variance NPP"     ,                     
      "Variance precipitation"     ,                     
      "Variance precipitation 1950"  ,                     
      "Variance temperature"     ,                     
      "Variance temperature 1950"   , 
      "Temperature harshness"     ,                
      "Xero harshness"       ,
      
      "Predicability NPP"   ,                          
      "Predicability precipitation"  ,                           
      "Predicability precipitation 1950"   ,                  
      "Predicability temperature"   ,                          
      "Predicability temperature 1950"   ,    
      "Constancy NPP"  ,                          
      "Constancy precipitation"  ,                           
      "Constancy precipitation 1950" ,                       
      "Constancy temperature"   ,                          
      "Constancy temperature 1950"   ,       
      "Contingency NPP"  ,                          
      "Contingency precipitation"   ,                          
      "Contingency precipitation 1950"   ,                     
      "Contingency temperature"      ,                     
      "Contingency temperature 1950"                         
      
    )
    
    pall <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                         na.color = "transparent")
    
   
    
  leaf <- leaflet(data = binary_traits)  %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>% 
      addCircleMarkers(~Revised.longitude.x, ~Revised.latitude.x, 
                       radius = 4,
                       color = ~pal(binary_traits[,as.numeric(input$trait_choose_1)]),
                       stroke = FALSE, fillOpacity = 1,
                       popup = ~popup_text
      ) 
  
  #%>% addMouseCoordinates()
  
 
  
  if(as.numeric(input$raster_layer_1) == 1 || as.numeric(input$raster_layer_1) == 2){leaf} else {leaf %>%  addRasterImage(r, opacity = 0.8, colors=pall) %>%
    addLegend(pal = pall, position="bottomleft" ,values = values(r),
              title = legend_label[as.numeric(input$raster_layer_1)]) %>% setView(0,10,1)}  
  })
  
  
  
  
  
  output$mymap_2 <- renderLeaflet({
    
    plant_potential <- raster("www/richnobuf.asc")
    
    load('www/ClimateDataRaster_ElevDownscaled.2016.Rdata')
    
    load('www/TempHarsh_raster.Rdata')
    load('www/XeroHarsh_raster.Rdata')
    
    objects()
    
    raster_list <-  list( Biomes_raster, Biomes_raster, Biomes_raster, EcoRegions_raster, OlsonEcoregions_raster, AmphibianDiv_raster, BirdDiv_raster, 
                          MammalDiv_raster, VascPlant_CoKrig_KreftJetz2007_raster, plant_potential,Elev_raster, Slope_raster, 
                          Glaciation, Sand_raster, LocalHeterogeneity_raster, MeanN_raster, MeanP_raster, MeanP_raster1950,
                          MeanT_raster, MeanT_raster1950, VarN_raster, VarP_raster, VarP_raster1950,  VarT_raster ,                     
                          VarT_raster1950 , TempHarsh_raster, XeroHarsh_raster, Pn_raster, Pp_raster, Pp_raster1950,                    
                          Pt_raster, Pt_raster1950, Cn_raster, Cp_raster , Cp_raster1950, Ct_raster ,  Ct_raster1950,
                          Mp_raster, Mp_raster1950, Mt_raster, Mt_raster1950)
    
    
    
    r <- raster_list[[as.numeric(input$raster_layer_2)]]
    
    colfunc  <- colorRampPalette(c("#f0f0f0",
                                   "#969696",
                                   "#000000"))
    
    
    
    crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    #leaflet() %>% addTiles() %>%
    # addRasterImage(r, colors = pal, opacity = 0.8) %>%
    #  addLegend(pal = pal, values = values(r),
    #          title = "Surface temp")
    
    if("Full" == input$tree_choose_2){
      load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_FULL_trimmed.Rdata')
      tree_choice <- full_tree
      binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
    if("Uto" == input$tree_choose_2){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Uto_trimmed.RData')
      tree_choice <- uto_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Bantu" == input$tree_choose_2){
      load('www/BANTU_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Bantu_trimmed.RData')
      tree_choice <- bantu_tree
      binary_traits <- BANTU_Society_data_with_binary_conversions}
    if("Indo-european" == input$tree_choose_2){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_IE.RData')
      tree_choice <-  IE_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Pama-Nyungan" == input$tree_choose_2){
      load('www/UTO_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_Pama_Nyungan.RData')
      tree_choice <- pama_nyungan_tree
      binary_traits <- UTO_Society_data_with_binary_conversions}
    if("Austronesian" == input$tree_choose_2){
      load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
      load('www/Tree_austronisian_trimmed.RData')
      tree_choice <- aus_tree
      binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
    
    
    dim(binary_traits)
    binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
    dim(binary_traits)
    
    legend_label <-  c(
      
      "no layer",
      "    " ,
      "Biomes"  , 
      "EcoRegions"   ,
      "Olson Ecoregions"  ,            
      "Amphibian diversity" ,                 
      "Bird diversity"  , 
      "Mammal diversiy"  ,
      "Vascular Plant diversity from Kreft and Jetz 2007",
      "Potential domesticable species" ,
      "Elevation" , 
      "Slope"    ,
      "Glaciation"  ,  
      "Sand type"   ,                       
      "Local heterogeneity"  , 
      "Mean NPP"  ,                       
      "Mean precipitation"  ,                       
      "Mean precipitation 1950"   ,                  
      "Mean temperature"  ,                       
      "Mean temperature 1950"    ,   
      "Variance NPP"     ,                     
      "Variance precipitation"     ,                     
      "Variance precipitation 1950"  ,                     
      "Variance temperature"     ,                     
      "Variance temperature 1950"   , 
      "Temperature harshness"     ,                
      "Xero harshness"       ,
      
      "Predicability NPP"   ,                          
      "Predicability precipitation"  ,                           
      "Predicability precipitation 1950"   ,                  
      "Predicability temperature"   ,                          
      "Predicability temperature 1950"   ,    
      "Constancy NPP"  ,                          
      "Constancy precipitation"  ,                           
      "Constancy precipitation 1950" ,                       
      "Constancy temperature"   ,                          
      "Constancy temperature 1950"   ,       
      "Contingency NPP"  ,                          
      "Contingency precipitation"   ,                          
      "Contingency precipitation 1950"   ,                     
      "Contingency temperature"      ,                     
      "Contingency temperature 1950"                         
      
    )
    
    pall <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                         na.color = "transparent")
    
    
    
    leaf <- leaflet(data = binary_traits)  %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>% 
      addCircleMarkers(~Revised.longitude.x, ~Revised.latitude.x, 
                       radius = 4,
                       color = ~pal(binary_traits[,as.numeric(input$trait_choose_2)]),
                       stroke = FALSE, fillOpacity = 1,
                       popup = ~popup_text
      ) 
    #%>% addMouseCoordinates()
    
    
    
    if(as.numeric(input$raster_layer_2) == 1 || as.numeric(input$raster_layer_2) == 2){leaf} else {leaf %>%  addRasterImage(r, opacity = 0.8, colors=pall) %>%
        addLegend(pal = pall, position="bottomright" ,values = values(r),
                  title = legend_label[as.numeric(input$raster_layer_2)]) %>% setView(0,10,1)}  
  })
  
  
  
 
  
  
  
  
  
  
    
    
    load("www/binary_cultures_tip_ordered.Rdata")
    
    
    output$trait_binary_bar_1 <- renderPlot({
      
      if("Full" == input$tree_choose_1){
        load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_FULL_trimmed.Rdata')
        tree_choice <- full_tree
        binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
      if("Uto" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Uto_trimmed.RData')
        tree_choice <- uto_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Bantu" == input$tree_choose_1){
        load('www/BANTU_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Bantu_trimmed.RData')
        tree_choice <- bantu_tree
        binary_traits <- BANTU_Society_data_with_binary_conversions}
      if("Indo-european" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_IE.RData')
        tree_choice <-  IE_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Pama-Nyungan" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Pama_Nyungan.RData')
        tree_choice <- pama_nyungan_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Austronesian" == input$tree_choose_1){
        load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_austronisian_trimmed.RData')
        tree_choice <- aus_tree
        binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
      
      dim(binary_traits)
      binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
      dim(binary_traits)
      
      zeros <- length(which(as.numeric(binary_traits[, as.numeric(input$trait_choose_1)]) == 0))
      ones <- length(which(as.numeric(binary_traits[, as.numeric(input$trait_choose_1)]) == 1))
      no <- length(which(is.na(binary_traits[,as.numeric(input$trait_choose_1)])))
      
      par(mar=c(4,5,4,4))
      barplot(c(no, zeros, ones), col=c( adjustcolor("lightblue", alpha=.5), "gold", "red"), main="Trait frequency", xaxt="n", xlab="", ylim=c(0.5,3.5), horiz=TRUE)
      axis(2, at=c(1,2,3), label=c("No Data", "Absent", "Present"), las=2)
      axis(1, line=2)
    })
    
    
    
    output$Traits_on_trees_1 <- renderPlot({
      
      if("Full" == input$tree_choose_1){
        load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_FULL_trimmed.Rdata')
        tree_choice <- full_tree
        binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
      if("Uto" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Uto_trimmed.RData')
        tree_choice <- uto_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Bantu" == input$tree_choose_1){
        load('www/BANTU_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Bantu_trimmed.RData')
        tree_choice <- bantu_tree
        binary_traits <- BANTU_Society_data_with_binary_conversions}
      if("Indo-european" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_IE.RData')
        tree_choice <-  IE_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Pama-Nyungan" == input$tree_choose_1){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Pama_Nyungan.RData')
        tree_choice <- pama_nyungan_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Austronesian" == input$tree_choose_1){
        load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_austronisian_trimmed.RData')
        tree_choice <- aus_tree
        binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
      
      dim(binary_traits)
      binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
      dim(binary_traits)
      
      length(8:36)
      
      length(tree_choice$tip.label)
      col_vector <- c("gold", "red", "white")[as.factor(as.character(binary_traits[, as.numeric(input$trait_choose_1)]))]
      length(col_vector)
      
      #p <- ggtree(tree_choice, layout="circular", size=0.25) + geom_tippoint(color= col_vector, shape=19, size=2) + geom_tiplab(aes(angle=angle), color= col_vector, size=.5) 
      par(mar=c(0,0,0,0))
      plot(tree_choice, type="fan",  show.tip.label=FALSE)
      tiplabels(pch=19, col=col_vector)
       
      #open_tree(p, angle=50)
      #rotate_tree(p, angle=80)
      
      
    }) 
    
    
    
    
    
    
    output$trait_binary_bar_2 <- renderPlot({
      if("Full" == input$tree_choose_2){
        load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_FULL_trimmed.Rdata')
        tree_choice <- full_tree
        binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
      if("Uto" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Uto_trimmed.RData')
        tree_choice <- uto_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Bantu" == input$tree_choose_2){
        load('www/BANTU_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Bantu_trimmed.RData')
        tree_choice <- bantu_tree
        binary_traits <- BANTU_Society_data_with_binary_conversions}
      if("Indo-european" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_IE.RData')
        tree_choice <-  IE_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Pama-Nyungan" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Pama_Nyungan.RData')
        tree_choice <- pama_nyungan_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Austronesian" == input$tree_choose_2){
        load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_austronisian_trimmed.RData')
        tree_choice <- aus_tree
        binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
      
      dim(binary_traits)
      binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
      dim(binary_traits)
      
      zeros <- length(which(as.numeric(binary_traits[, as.numeric(input$trait_choose_2)]) == 0))
      ones <- length(which(as.numeric(binary_traits[, as.numeric(input$trait_choose_2)]) == 1))
      no <- length(which(is.na(binary_traits[,as.numeric(input$trait_choose_2)])))
      
      par(mar=c(4,5,4,4))
      barplot(c(no, zeros, ones), col=c( adjustcolor("lightblue", alpha=.5), "gold", "red"), main="Trait frequency", xaxt="n", xlab="", ylim=c(0.5,3.5), horiz=TRUE)
      axis(2, at=c(1,2,3), label=c("No Data", "Absent", "Present"), las=2)
      axis(1, line=2)
    })
    
 
    output$Traits_on_trees_2 <- renderPlot({
      
      if("Full" == input$tree_choose_2){
        load('www/FULL_TREE_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_FULL_trimmed.Rdata')
        tree_choice <- full_tree
        binary_traits <- FULL_TREE_Society_data_with_binary_conversions}
      if("Uto" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Uto_trimmed.RData')
        tree_choice <- uto_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Bantu" == input$tree_choose_2){
        load('www/BANTU_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Bantu_trimmed.RData')
        tree_choice <- bantu_tree
        binary_traits <- BANTU_Society_data_with_binary_conversions}
      if("Indo-european" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_IE.RData')
        tree_choice <-  IE_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Pama-Nyungan" == input$tree_choose_2){
        load('www/UTO_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_Pama_Nyungan.RData')
        tree_choice <- pama_nyungan_tree
        binary_traits <- UTO_Society_data_with_binary_conversions}
      if("Austronesian" == input$tree_choose_2){
        load('www/AUSTRONESIAN_Society_data_with_binary_conversions.Rdata')
        load('www/Tree_austronisian_trimmed.RData')
        tree_choice <- aus_tree
        binary_traits <- AUSTRONESIAN_Society_data_with_binary_conversions}
      
      dim(binary_traits)
      binary_traits <- cbind(binary_traits, rep(NA, length(binary_traits[,1])), rep(NA, length(binary_traits[,1])))
      dim(binary_traits)
      
      length(8:36)
      
      length(tree_choice$tip.label)
      col_vector <- c("gold", "red", "white")[as.factor(as.character(binary_traits[, as.numeric(input$trait_choose_2)]))]
      length(col_vector)
      
     # p <- ggtree(tree_choice, layout="circular", size=0.25) + geom_tippoint(color= col_vector, shape=19, size=2) + geom_tiplab(aes(angle=angle), color= col_vector, size=.5) 
      
      par(mar=c(0,0,0,0))
      plot(tree_choice, type="fan",  show.tip.label=FALSE)
      tiplabels(pch=19, col=col_vector)
      
      
     # open_tree(p, angle=50)
     # rotate_tree(p, angle=80)
      
      
    })
    
    
    
    

   
    
    
})
    

    



#####

