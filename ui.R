options(rgl.useNULL=TRUE)
   
require(shiny)
require(shinydashboard)
require(raster)
require(ape)
require(phytools)
#require(diversitree)
#source("https://bioconductor.org/biocLite.R")
#biocLite("ggtree")
#biocLite("ggtree", type = "source")
#library(ggtree)
require(spdep)
require(mapview)
require(leaflet)
require(RColorBrewer)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

shinyUI(dashboardPage( skin="black", 
  dashboardHeader(title = "D-place visualizer"),
  dashboardSidebar(
    sidebarMenu(disable=TRUE,
                menuItem("Traits as binary", tabName = "leaflet", icon = icon("book"))
      
     
      
    )
  ),
  dashboardBody(

    tabItems( 

      tabItem(tabName ="leaflet",
              fluidRow(
                tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        
                column(6,box(leafletOutput("mymap_1", width="100%", height=300), 
                             #plotOutput("Traits_on_trees_1", width = "100%", height = "300px"),
                             #plotOutput("trait_binary_bar_1", width = "100%", height = "300px")
                              width=12)),
                column(6,box(leafletOutput("mymap_2", width="100%", height=300),
                             #plotOutput("Traits_on_trees_2", width = "100%", height = "300px"),
                             #plotOutput("trait_binary_bar_2", width = "100%", height = "300px")
                              width=12))
              ),
                fluidRow(
                  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),  
                column(6, column(4, selectInput("trait_choose_1", label=h3("Choose a trait"), choices = list(
                                "Subsistence on Hunting and Gathering" = 19, 
                                "Subsistence on Fish" = 20,
                                "Subsistence on Husbandry" = 21,           
                                "Subsistence Agriculture" = 22,              
                                "Marriage Exchange" = 23,           
                                "Extended Family" = 24,            
                                "Polygyny Common"  = 25,            
                                "Patrilocal"    = 26,              
                                "Exogamy"    = 27,                
                                "Kin Terms"  = 28,                  
                                "Intensity of Agriculture" = 29,       
                                "Cereals"   = 30,                 
                                "Mobility"  = 31,                  
                                "Community Size"   = 32,            
                                "Political Complexity" = 33,       
                                "High Moral Gods"  = 34,             
                                "Domesticates"   = 35,             
                                "Patrilineal Descent"   = 36,      
                                "Matrilineal Descent"  = 37,        
                                "Cognatic Descent"    = 38,         
                                "Female Biased Pottery" = 39,       
                                "Male Biased House Construction" = 40,
                                "Egalitarian"          = 41,       
                                "Complex Stratification"   = 42,   
                                "Slavery"       = 43,              
                                "Land Inheritance"    = 44,         
                                "Movable Property inheritance" =45,
                                "House Shape" = 46,
                                "no points" = 48,
                                "     " = 49
                              ), selected=49),
                              
                              
                             
                              selectInput("raster_layer_1", label=h3("Choose an overlay"), choices = list(
                              
                                "AmphibianDiv_raster" =  1,                  
                                "Biomes_raster" = 2 ,                       
                                "BirdDiv_raster"  = 3 ,                     
                                "Cn_raster"  =  4,                          
                                "complete" = 5 ,                             
                                "Continents" = 6 ,                          
                                "Cp_raster"  = 7 ,                           
                                "Cp_raster1950" = 8 ,                       
                                "Ct_raster"   =  9,                          
                                "Ct_raster1950"  = 10 ,                      
                                "EcoRegions_raster"   = 11 ,                  
                                "Elev_raster"  =  12,                        
                                "Glaciation" = 13 ,                          
                                "LocalHeterogeneity_raster" = 14 ,           
                                "MammalDiv_raster"   = 15 ,                   
                                "MeanN_raster"  =  16,                       
                                "MeanP_raster"  = 17 ,                       
                                "MeanP_raster1950"   = 18 ,                  
                                "MeanT_raster"   =  19,                       
                                "MeanT_raster1950"   = 20 ,                  
                                "Mn_raster"  =  21,                          
                                "Mp_raster"  = 22 ,                          
                                "Mp_raster1950"    =  23,                     
                                "Mt_raster"       =  24,                     
                                "Mt_raster1950"  = 25 ,                       
                                "OlsonEcoregions_raster"   =  26,            
                                "OlsonEcoregions.CategoryNames"   = 27 ,      
                                "Pn_raster"  = 28 ,                          
                                "Pp_raster"  =  29,                           
                                "Pp_raster1950"      =  30,                  
                                "Pt_raster"   =  31,                          
                                "Pt_raster1950"   =  32,                                                   
                                "Sand_raster"   =  33,                       
                                "Slope_raster"    =  34,                     
                                "TempHarsh_raster"     = 35 ,                
                                "VarN_raster"      =  36,                     
                                "VarP_raster"     =  37,                     
                                "VarP_raster1950"  =  38,                     
                                "VarT_raster"     = 39 ,                     
                                "VarT_raster1950"  = 40 ,                     
                                "VascPlant_CoKrig_KreftJetz2007_raster"=  41,
                                "XeroHarsh_raster"     =   42  ,
                                "potential ag plants" =  43, 
                                "no layer" = 44,
                                "    " = 45
                              ), selected=45),
                              selectInput("tree_choose_1", label=h3("Choose a tree"), choices = list(
                                "Full", 
                                "Uto",
                                "Bantu",
                                "Indo-european",
                                "Austronisian"
                              ), selected="Full")
                              
                              ),
                              column(8, plotOutput("Traits_on_trees_1", width = "100%", height = "350px"),
                                     plotOutput("trait_binary_bar_1", width = "100%", height = "150px"))
                              
                              ) ,
                             # plotOutput("trait_binary_bar_2", width = "100%", height = "300px")
                             
                            
                             column(6, column(8, plotOutput("Traits_on_trees_2", width = "100%", height = "350px"),
                                           plotOutput("trait_binary_bar_2", width = "100%", height = "150px")),
                                    column(4, selectInput("trait_choose_2", label=h3("Choose a trait"), choices = list(
                                      "Subsistence on Hunting and Gathering" = 19, 
                                      "Subsistence on Fish" = 20,
                                      "Subsistence on Husbandry" = 21,           
                                      "Subsistence Agriculture" = 22,              
                                      "Marriage Exchange" = 23,           
                                      "Extended Family" = 24,            
                                      "Polygyny Common"  = 25,            
                                      "Patrilocal"    = 26,              
                                      "Exogamy"    = 27,                
                                      "Kin Terms"  = 28,                  
                                      "Intensity of Agriculture" = 29,       
                                      "Cereals"   = 30,                 
                                      "Mobility"  = 31,                  
                                      "Community Size"   = 32,            
                                      "Political Complexity" = 33,       
                                      "High Moral Gods"  = 34,             
                                      "Domesticates"   = 35,             
                                      "Patrilineal Descent"   = 36,      
                                      "Matrilineal Descent"  = 37,        
                                      "Cognatic Descent"    = 38,         
                                      "Female Biased Pottery" = 39,       
                                      "Male Biased House Construction" = 40,
                                      "Egalitarian"          = 41,       
                                      "Complex Stratification"   = 42,   
                                      "Slavery"       = 43,              
                                      "Land Inheritance"    = 44,         
                                      "Movable Property inheritance" =45,
                                      "House Shape" = 46,
                                      "no points" = 48,
                                      "     " = 49
                                    ), selected=49),
                                    
                                    
                                    
                                    selectInput("raster_layer_2", label=h3("Choose an overlay"), choices = list(
                                      
                                      "AmphibianDiv_raster" =  1,                  
                                      "Biomes_raster" = 2 ,                       
                                      "BirdDiv_raster"  = 3 ,                     
                                      "Cn_raster"  =  4,                          
                                      "complete" = 5 ,                             
                                      "Continents" = 6 ,                          
                                      "Cp_raster"  = 7 ,                           
                                      "Cp_raster1950" = 8 ,                       
                                      "Ct_raster"   =  9,                          
                                      "Ct_raster1950"  = 10 ,                      
                                      "EcoRegions_raster"   = 11 ,                  
                                      "Elev_raster"  =  12,                        
                                      "Glaciation" = 13 ,                          
                                      "LocalHeterogeneity_raster" = 14 ,           
                                      "MammalDiv_raster"   = 15 ,                   
                                      "MeanN_raster"  =  16,                       
                                      "MeanP_raster"  = 17 ,                       
                                      "MeanP_raster1950"   = 18 ,                  
                                      "MeanT_raster"   =  19,                       
                                      "MeanT_raster1950"   = 20 ,                  
                                      "Mn_raster"  =  21,                          
                                      "Mp_raster"  = 22 ,                          
                                      "Mp_raster1950"    =  23,                     
                                      "Mt_raster"       =  24,                     
                                      "Mt_raster1950"  = 25 ,                       
                                      "OlsonEcoregions_raster"   =  26,            
                                      "OlsonEcoregions.CategoryNames"   = 27 ,      
                                      "Pn_raster"  = 28 ,                          
                                      "Pp_raster"  =  29,                           
                                      "Pp_raster1950"      =  30,                  
                                      "Pt_raster"   =  31,                          
                                      "Pt_raster1950"   =  32,                                                   
                                      "Sand_raster"   =  33,                       
                                      "Slope_raster"    =  34,                     
                                      "TempHarsh_raster"     = 35 ,                
                                      "VarN_raster"      =  36,                     
                                      "VarP_raster"     =  37,                     
                                      "VarP_raster1950"  =  38,                     
                                      "VarT_raster"     = 39 ,                     
                                      "VarT_raster1950"  = 40 ,                     
                                      "VascPlant_CoKrig_KreftJetz2007_raster"=  41,
                                      "XeroHarsh_raster"     =   42  ,
                                      "potential ag plants" =  43, 
                                      "no layer" = 44,
                                      "    " = 45
                                    ), selected=45),
                                    selectInput("tree_choose_2", label=h3("Choose a tree"), choices = list(
                                      "Full", 
                                      "Uto",
                                      "Bantu",
                                      "Indo-european",
                                      "Austronisian"
                                    ), selected="Full")
                                    
                                    )
                             
                             )
        
        
                             )
      )
            ) # end tab items
        ) # end dashboard body
) # end dashboard page
) # end shiny UI


