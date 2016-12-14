require(shiny)
require(shinydashboard)
require(raster)
require(ape)
require(phytools)
require(spdep)
require(mapview)
require(leaflet)
require(RColorBrewer)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

shinyUI(dashboardPage( skin="black", 
  dashboardHeader(title = "p-place visualizer"),
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
                  tags$style(type = "text/css", "html, body {width:100%;height:150%}"),  
                column(6, column(4, selectInput("trait_choose_1", label=h3("Choose a trait"), choices = list(
                               "no points" = 48,
                                "     " = 49,
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
                                "House Shape" = 46
                                
                              ), selected=49),
                              
                              
                             
                              selectInput("raster_layer_1", label=h3("Choose an overlay"), choices = list(
                              
                                "no layer" = 1,
                                "    " = 2,
                                "Biomes" = 3 , 
                                "EcoRegions"   = 4 ,
                                "Olson Ecoregions"   =  5,            
                                "Amphibian diversity" =  6,                 
                                "Bird diversity"  = 7 , 
                                "Mammal Diversiy"   = 8 ,
                                "Vascular Plant diversity from Kreft and Jetz 2007"=  9,
                                "Potential domesticable speciess" =  10,
                                "Elevation"  =  11, 
                                "Slope"    =  12,
                                "Glaciation" = 13 ,  
                                "Sand type"   =  14,                       
                                "Local Heterogeneity" = 15 , 
                                "Mean NPP"  =  16,                       
                                "Mean Precipitation"  = 17 ,                       
                                "Mean P_1950"   = 18 ,                  
                                "Mean T"   =  19,                       
                                "Mean T_1950"   = 20 ,   
                                "Variance NPP"      =  21,                     
                                "Variance precipitation"     =  22,                     
                                "Variance precipitation 1950"  =  23,                     
                                "Variance temperature"     = 24 ,                     
                                "Variance temperature 1950"  = 25 , 
                                "Temperature Harshness"     = 26 ,                
                                "Xero Harshness"     =   27  ,
                                
                                "Predicability NPP"  = 28 ,                          
                                "Predicability precipitation"  =  29,                           
                                "Predicability precipitation 1950"      =  30,                  
                                "Predicability temperature"   =  31,                          
                                "Predicability temperature 1950"   =  32,    
                                "Constancy NPP"  =  33,                          
                                "Constancy precipitation"  =34 ,                           
                                "Constancy precipitation 1950" =35 ,                       
                                "Constancy temperature"   = 36,                          
                                "Constancy temperature 1950"  = 37 ,       
                                "Contingency NPP"  =  38,                          
                                "Contingency precipitation"  = 39 ,                          
                                "Contingency precipitation 1950"    =  40,                     
                                "Contingency temperature"       =  41,                     
                                "Contingency temperature 1950"  = 42                        
                                
                              ), selected=2),
                              selectInput("tree_choose_1", label=h3("Choose a tree"), choices = list(
                                "Full", 
                                "Bantu",
                                "Austronesian",
                                "Uto"
                              ), selected="Full")
                              
                              ),
                              column(8, plotOutput("Traits_on_trees_1", width = "100%", height = "350px"),
                                     plotOutput("trait_binary_bar_1", width = "100%", height = "150px"))
                              
                              ) ,
                             # plotOutput("trait_binary_bar_2", width = "100%", height = "300px")
                             
                            
                             column(6, column(8, plotOutput("Traits_on_trees_2", width = "100%", height = "350px"),
                                           plotOutput("trait_binary_bar_2", width = "100%", height = "150px")),
                                    column(4, selectInput("trait_choose_2", label=h3("Choose a trait"), choices = list(
                                      "no points" = 48,
                                      "     " = 49,
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
                                      "House Shape" = 46
                                    ), selected=49),
                                    
                                    
                                    
                                    selectInput("raster_layer_2", label=h3("Choose an overlay"), choices = list(
                                      
                                      "no layer" = 1,
                                      "    " = 2,
                                      "Biomes" = 3 , 
                                      "EcoRegions"   = 4 ,
                                      "Olson Ecoregions"   =  5,            
                                      "Amphibian diversity" =  6,                 
                                      "Bird diversity"  = 7 , 
                                      "Mammal Diversiy"   = 8 ,
                                      "Vascular Plant diversity from Kreft and Jetz 2007"=  9,
                                      "Potential domesticable speciess" =  10,
                                      "Elevation"  =  11, 
                                      "Slope"    =  12,
                                      "Glaciation" = 13 ,  
                                      "Sand type"   =  14,                       
                                      "Local Heterogeneity" = 15 , 
                                      "Mean NPP"  =  16,                       
                                      "Mean Precipitation"  = 17 ,                       
                                      "Mean P_1950"   = 18 ,                  
                                      "Mean T"   =  19,                       
                                      "Mean T_1950"   = 20 ,   
                                      "Variance NPP"      =  21,                     
                                      "Variance precipitation"     =  22,                     
                                      "Variance precipitation 1950"  =  23,                     
                                      "Variance temperature"     = 24 ,                     
                                      "Variance temperature 1950"  = 25 , 
                                      "Temperature Harshness"     = 26 ,                
                                      "Xero Harshness"     =   27  ,
                                      
                                      "Predicability NPP"  = 28 ,                          
                                      "Predicability precipitation"  =  29,                           
                                      "Predicability precipitation 1950"      =  30,                  
                                      "Predicability temperature"   =  31,                          
                                      "Predicability temperature 1950"   =  32,    
                                      "Constancy NPP"  =  33,                          
                                      "Constancy precipitation"  =34 ,                           
                                      "Constancy precipitation 1950" =35 ,                       
                                      "Constancy temperature"   = 36,                          
                                      "Constancy temperature 1950"  = 37 ,       
                                      "Contingency NPP"  =  38,                          
                                      "Contingency precipitation"  = 39 ,                          
                                      "Contingency precipitation 1950"    =  40,                     
                                      "Contingency temperature"       =  41,                     
                                      "Contingency temperature 1950"  = 42                        
                                      
                                    ), selected=2),
                                    selectInput("tree_choose_2", label=h3("Choose a tree"), choices = list(
                                      "Full", 
                                      "Bantu",
                                      "Austronesian",
                                      "Uto"
                                    ), selected="Full")
                                    
                                    )
                             
                             )
        
        
                             )
      )
            ) # end tab items
        ) # end dashboard body
) # end dashboard page
) # end shiny UI


