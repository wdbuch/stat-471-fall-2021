plot_grayscale = function(image_array){
  image_array %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "row") %>% 
    pivot_longer(-row, 
                 values_to = "intensity", 
                 names_to = "col", 
                 names_prefix = "V", 
                 names_transform = list(col = as.integer)) %>% 
    mutate(row = as.integer(row)) %>% 
    ggplot(aes(x = col, y = - row, fill = intensity)) + 
    geom_tile() + 
    coord_fixed() + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_gradient(low = "white", high = "black") + 
    theme_void() + 
    theme(panel.border = element_rect(colour = "black", fill=NA))  
}
