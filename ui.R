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
                              
                                "Amphibian diversity" =  1,                  
                                "Biomes" = 2 ,                       
                                "Bird diversity"  = 3 ,                     
                                "Cn"  =  4,                          
                                "complete" = 5 ,                             
                                "Continents" = 6 ,                          
                                "Cp"  = 7 ,                           
                                "Cp_1950" = 8 ,                       
                                "Ct"   =  9,                          
                                "Ct1950"  = 10 ,                      
                                "EcoRegions"   = 11 ,                  
                                "Elevation"  =  12,                        
                                "Glaciation" = 13 ,                          
                                "Local Heterogeneity" = 14 ,           
                                "MammalDiversiy"   = 15 ,                   
                                "Mean N"  =  16,                       
                                "Mean P"  = 17 ,                       
                                "Mean P_1950"   = 18 ,                  
                                "Mean T"   =  19,                       
                                "Mean T_1950"   = 20 ,                  
                                "Mn"  =  21,                          
                                "Mp"  = 22 ,                          
                                "Mp_1950"    =  23,                     
                                "Mt"       =  24,                     
                                "Mt_1950"  = 25 ,                       
                                "Olson Ecoregions"   =  26,            
                                "Olson Ecoregions Names"   = 27 ,      
                                "Pn"  = 28 ,                          
                                "Pp"  =  29,                           
                                "Pp_1950"      =  30,                  
                                "Pt"   =  31,                          
                                "Pt_1950"   =  32,                                                   
                                "Sand type"   =  33,                       
                                "Slope"    =  34,                     
                                "Temperature Harshness"     = 35 ,                
                                "VarN"      =  36,                     
                                "VarP"     =  37,                     
                                "VarP_1950"  =  38,                     
                                "VarT"     = 39 ,                     
                                "VarT_1950"  = 40 ,                     
                                "Vascular Plants from Kreft and Jetz 2007"=  41,
                                "Xero Harshness"     =   42  ,
                                "potential domesticable speciess" =  43, 
                                "no layer" = 44,
                                "    " = 45
                              ), selected=45),
                              selectInput("tree_choose_1", label=h3("Choose a tree"), choices = list(
                                "Full", 
                                "Bantu",
                                "Austronisian",
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
                                      
                                      "Amphibian diversity" =  1,                  
                                      "Biomes" = 2 ,                       
                                      "Bird diversity"  = 3 ,                     
                                      "Cn"  =  4,                          
                                      "complete" = 5 ,                             
                                      "Continents" = 6 ,                          
                                      "Cp"  = 7 ,                           
                                      "Cp_1950" = 8 ,                       
                                      "Ct"   =  9,                          
                                      "Ct1950"  = 10 ,                      
                                      "EcoRegions"   = 11 ,                  
                                      "Elevation"  =  12,                        
                                      "Glaciation" = 13 ,                          
                                      "Local Heterogeneity" = 14 ,           
                                      "MammalDiversiy"   = 15 ,                   
                                      "Mean N"  =  16,                       
                                      "Mean P"  = 17 ,                       
                                      "Mean P_1950"   = 18 ,                  
                                      "Mean T"   =  19,                       
                                      "Mean T_1950"   = 20 ,                  
                                      "Mn"  =  21,                          
                                      "Mp"  = 22 ,                          
                                      "Mp_1950"    =  23,                     
                                      "Mt"       =  24,                     
                                      "Mt_1950"  = 25 ,                       
                                      "Olson Ecoregions"   =  26,            
                                      "Olson Ecoregions Names"   = 27 ,      
                                      "Pn"  = 28 ,                          
                                      "Pp"  =  29,                           
                                      "Pp_1950"      =  30,                  
                                      "Pt"   =  31,                          
                                      "Pt_1950"   =  32,                                                   
                                      "Sand type"   =  33,                       
                                      "Slope"    =  34,                     
                                      "Temperature Harshness"     = 35 ,                
                                      "VarN"      =  36,                     
                                      "VarP"     =  37,                     
                                      "VarP_1950"  =  38,                     
                                      "VarT"     = 39 ,                     
                                      "VarT_1950"  = 40 ,                     
                                      "Vascular Plants from Kreft and Jetz 2007"=  41,
                                      "Xero Harshness"     =   42  ,
                                      "potential domesticable speciess" =  43, 
                                      "no layer" = 44,
                                      "    " = 45
                                    ), selected=45),
                                    selectInput("tree_choose_2", label=h3("Choose a tree"), choices = list(
                                      "Full", 
                                      "Bantu",
                                      "Austronisian",
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


