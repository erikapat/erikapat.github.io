

# Graph points and its mean as a line (or other line that you want.
#Input:
#dt1: data set with the points for the scatter plot (X, Y, Z, TITLE)
#dt2: data set with the points for the line (X, Y2, Z, TITLE2)
# xlabel, ylabe, ylegend, zlegend: strings for the titles of the plot

graph_points_lines <- function(dt1, dt2, xlabel = "Hour", ylabel = "Number of dispositives", ylegend ='Frequency', zlegend ='Day'){
  
  
  H <- hchart(dt1, "scatter", hcaes(y = Y, 
                              x = X, #for the axis
                              size = 2,
                              g = Z, #for the legend
                              w = W, #for the legend (month)
                              h = H,          #       (year)
                              group = TITLE))
              
  
  H <- H %>% hc_xAxis(title = list(text= xlabel, style = list(fontWeight = "bold" , allowDecimals = T, fontSize = "20px"))) %>%
             hc_yAxis(title = list(text= ylabel, style = list(fontWeight = "bold" , allowDecimals = T, fontSize = "20px")))
  
  H <- H %>% hc_add_series(data = dt2,
                            type = "line",
                           hcaes(x = X,
                              y = Y2,         # 
                              g = Z, #for the legend
                              w = W, #for the legend (month)
                              h = H, 
                              group = TITLE2))
  #H <-  H %>% hc_colors("cyan")  
  H <- H %>% hc_legend(align = "right", verticalAlign = "top")
  
  H <- H %>% hc_tooltip(useHTML = TRUE,
                        headerFormat = "<table>",
                        pointFormat = paste(
                          #"<tr><th colspan=\"1\"><b>{point.label}</b></th></tr>",
                          paste0("<tr><th>", 'DESCRIPTION:' , "</th><td></td></tr>"),
                          paste0("<tr><th>", xlabel, "</th><td>{point.x} hours</td></tr>"),
                          paste0("<tr><th>", ylegend, "</th><td>{point.y} dispositives</td></tr>"),
                          paste0("<tr><th>", zlegend, "</th><td>{point.g}</td></tr>"),
                          paste0("<tr><th>", 'Month', "</th><td>{point.w}</td></tr>"),
                          paste0("<tr><th>", 'Year', "</th><td>{point.h}</td></tr>")
                          #"cluster: {point.z}" #con unidades
                            ),
                        #"<tr><th>Drat</th><td>{point.z} </td></tr>",
                        #"<tr><th>HP</th><td>{point.valuecolor} hp</td></tr>"),
                        footerFormat = "</table>")
  #H <- H  %>% hc_add_theme(hc_theme_sandsignika())
  #THEMES: http://jkunst.com/highcharter/themes.html
  
  H <- H %>%  hc_rangeSelector(inputEnabled = T) 
  H <- H %>%  hc_scrollbar(enabled = F)
  H
  
  saveWidget(H, "H1.html", selfcontained = TRUE, libdir = NULL, background = "white")
  return(H)
 
}

graph_lines <- function(dt, xlabel = "Hour", ylabel = "Number of dispositives", title = 'Number of detected dispositives', ylegend ='Frequency', zlegend ='Day'){
  
  H <- hchart(dt, "line", hcaes(y = Y, x = X), color = 'black') %>% 
       hc_add_series(data = dt,
                     type = "point",
                     hcaes(x = X, #DAY,
                     y = Y), #FREQ_DAY_HOUR,
                     color = "black")  
  H <-  H %>% hc_colors("black")   
  H <-  H %>%  hc_xAxis(
    title = list(text = xlabel, style = list(fontWeight = "bold" , allowDecimals = T, fontSize = "20px"))) %>% 
    hc_yAxis(
      title = list(text = title, style = list(fontWeight = "bold" , allowDecimals = T, fontSize = "20px")),
      #opposite = TRUE,
      plotLines = list(
        list(label = list(text = ""),
             color = "#'FF0000",
             width = 100,
             value = 5.5)))
  
  H <- H %>%  hc_title(text = title, style = list(fontWeight = "bold")) %>% 
    hc_exporting(enabled = TRUE, filename = "threat_extract") 
  
  H <- H %>% hc_add_theme(hc_theme_538())
  
  H <- H %>% hc_tooltip(useHTML = TRUE,
                        headerFormat = "<table>",
                        pointFormat = paste( #"<tr><th colspan=\"1\"><b>{point.label}</b></th></tr>",
                          paste0("<tr><th>", xlabel, "</th><td>{point.x} </td></tr>"),
                          paste0("<tr><th>", ylegend, "</th><td>{point.y} dispositives</td></tr>")),
                         # paste0("<tr><th>", zlegend, "</th><td>{point.z}</td></tr>")), #con unidades
                        #"<tr><th>Drat</th><td>{point.z} </td></tr>",
                        #"<tr><th>HP</th><td>{point.valuecolor} hp</td></tr>"),
                        footerFormat = "</table>")
  
  saveWidget(H, "H2.html", selfcontained = TRUE, libdir = NULL, background = "white")
  return(H)
  
}