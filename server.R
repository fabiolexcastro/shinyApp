
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(grid)){install.packages('grid'); library(grid)} else {library(grid)})
suppressMessages(if(!require(gtable)){install.packages('gtable'); library(gtable)} else {library(gtable)})
suppressMessages(if(!require(rsconnect)){install.packages('rsconnect'); library(rsconnect)} else {library(rsconnect)})
suppressMessages(if(!require(shiny)){install.packages('shiny'); library(shiny)} else {library(shiny)})
suppressMessages(if(!require(shiny)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(leaflet)){install.packages('leaflet'); library(leaflet)} else {library(leaflet)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(rgeos)){install.packages('rgeos'); library(rgeos)} else {library(rgeos)})

function(input, output, session) {
  
  
  #### leaflet Map
  
  shp <- shapefile("www/crn_pol_dss.shp")
  crs(shp) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  # pal <- colorNumeric(c("#1E803A", "#1E803A", "#FFFFCC"), values(rast),
  #                     na.color = "transparent")
  output$mymap <- renderLeaflet({
    
    leaflet() %>% addTiles() %>% addPolygons(data = shp , opacity = 0.5, fillOpacity = 0.5, stroke = F, color = "green")
  })
  
  
  lat_lng <- list(lng = c(), lat = c())
  
observeEvent(input$mymap_shape_click, {
    
  lat_lng$lng <- input$mymap_shape_click$lng
  lat_lng$lat <- input$mymap_shape_click$lat
  
   my_data <<- readRDS('www/myData.rds')  %>% as.tibble() %>%filter(., 
                                                              Lon == filter(., dplyr::row_number() == which.min(sqrt((lat_lng$lng- Lon)^2 + (lat_lng$lat - Lat)^2)) ) %>%
                                                                dplyr::pull(., Lon),
                                                              Lat ==  filter(., dplyr::row_number() == which.min(sqrt((lat_lng$lng- Lon)^2 + (lat_lng$lat - Lat)^2)) ) %>%
                                                                dplyr::pull(., Lat) 
                                                              
  ) 
   
 updateNumericInput(session, "Lon", value = unique(my_data$Lon))
 updateNumericInput(session, "Lat", value = unique(my_data$Lat))
 
  })

  data <- reactive({
    
data <- my_data
print(data)
#      data <- fl_data
   
     #filter(., Lon == input$Lon & Lat == input$Lat)
    #rm(datam); g <- gc(); rm(g)
    # data <- datam %>% filter(., Lon == -159.583 & Lat == 21.917)
    crn <- data %>% 
      mutate(month = factor(month, levels = month.abb)) %>% 
      filter(period == 'Current')
    med <- data %>% 
      filter(period == '2050') %>% 
      group_by(month) %>% 
      summarize(median = median(prec))
    ftr <- data %>% dplyr::filter(period == '2050')
    crn.ftr <- data %>% 
      group_by(period, month) %>%
      summarize(prec = mean(prec), 
                tmin = mean(tmin), 
                tmean = mean(tmean), 
                tmax = mean(tmax)) %>%
      ungroup()
    # ln <- unique(crn.ftr[,1])
    # lt <- input$Lat
    # return(list(crn = crn, crn.ftr = crn.ftr, ftr = ftr, med = med))
    return(list(crn = crn, crn.ftr = crn.ftr, ftr = ftr, med = med))
  })
  
  ggpls <- reactive({
    
    # gg <- ggplot(data = crn, aes(x = month)) +
    gg <- ggplot(data = data()[[1]], aes(x = month)) +
      # geom_bar(aes(y = prec), stat = 'identity', position = 'dodge', colour = '#63BE5C', fill = '#63BE5C') +
      geom_bar(aes(y = prec, fill = 'C'), stat = 'identity', position = 'dodge') +
      # geom_point(data = med, aes(x = month, y = median, size = "A"), colour = 'black') +
      # geom_point(data = ftr, aes(x = month, y = prec, size = "B"), colour = 'black') +
      geom_point(data = data()[[4]], aes(x = month, y = median, size = 'A'), colour = 'black') +
      geom_point(data = data()[[3]], aes(x = month, y = prec, size = 'B'), colour = 'black') +
      scale_size_manual(name = ' ',
                        values = c('A' = 6, 'B' = 2),
                        labels = c('A' = 'Future (Median)', 'B' = "Future (GCM)"),
                        breaks = c('B', 'A')) +
      scale_fill_manual(name = 'Precipitation', 
                        values = c('C' = '#63BE5C'),
                        labels = c('C' = 'Prec.')) +
      ylab('Precipitation')
    
    pr <- pull(data()[[2]], prec)
    tm <- pull(data()[[2]], tmean)
    rlc <- mean(pr) / mean(tm) * 2
    
    gg <- gg +
      # geom_line(data = crn.ftr, aes(y = tmin*rlc, group = period, colour = period, linetype = "D"), size = 1.2) +
      geom_line(data = data()[[2]], aes(y = tmin*rlc, colour = period, group = period, linetype = 'D'), size = 1.2) +
      # geom_line(data = crn.ftr, aes(y = tmean*rlc, colour = period, group = period, linetype = 'E'), size = 1.2) +
      geom_line(data = data()[[2]], aes(y = tmean*rlc, colour = period, group = period, linetype = 'E'), size = 1.2) +
      # geom_line(data = crn.ftr, aes(y = tmax*rlc, colour = period, group = period, linetype = 'D'), size = 1.2) +
      geom_line(data = data()[[2]], aes(y = tmax*rlc, colour = period, group = period, linetype = 'D'), size = 1.2) +
      scale_y_continuous(sec.axis = sec_axis(~./rlc, name = 'Temperature ºC')) +
      scale_color_manual(name = 'Temperature',
                         values = c('2050' = '#63BE5C', 'Current' = '#009933'),
                         labels = c('2050' = 'Future', 'Current' = 'Current'),
                         breaks = c('Current', '2050')) +
      scale_linetype_manual(name = ' ', 
                            values = c("D" = 2, 'E' = 1), 
                            labels = c("D" = "Min. and Max.", 'E' = 'Mean')) +
      # ggtitle(label = paste0('Climate graph for ', ' / '), 
      ggtitle(label = paste0('Climate graph for ', input$Lon, ' / ', input$Lat),
              subtitle = 'Past and projected 2050 conditions') +
      theme_bw() + 
      theme(legend.position = 'bottom',
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 20, hjust = 0.5),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 18, face = 'bold'),
            legend.key = element_rect(size = 4),
            legend.key.size = unit(2, 'lines'))  +
      guides(linetype = guide_legend(nrow = 2, keywidth = 3, order = 4, title.position = 'top', size = 15),
             color = guide_legend(nrow = 2, keywidth = 3, order = 3, title.position = 'top', size = 15),
             fill = guide_legend(order = 1, title.position = 'top', size = 15),
             size = guide_legend(order = 2, nrow = 2, title.position = 'top', size = 15)) +
      xlab('')
    gg
    return(gg)
  })
  output$plot1 <- renderPlot({
    ggpls()
  }, width = 1000, height = 800) # 800 - 700   
 
}
