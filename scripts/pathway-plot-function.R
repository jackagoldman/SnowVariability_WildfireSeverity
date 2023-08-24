# indirect plot function

pathway.plot <- function(df1, df2, response, region){
  
  
  df <-  df1 %>% 
    left_join(df2, by = c("Pathway", "Response")) 
  df$`Indirect Effect` <- round(df$`Indirect Effect`, digits = 3)
  df$`Total Causal Effect` <- round(df$`Total Causal Effect`, digits = 3)
  
df_long <- df %>% select(-c(Response)) %>% 
    pivot_longer(!Pathway, names_to = "type", values_to = "effect")
  
if (response == "median"){
  if (region == "east"){
    
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Median Burn Severity - East") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
    
    
    
  }else if (region == "west"){
    
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Median Burn Severity - West") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
  }else if(region == "NA"){
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Median Burn Severity - All of Ontario") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
  }
  
  
}else if (response == "extreme"){
  if(region == "east"){
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Extreme Burn Severity - East") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
  }else if (region == "west"){
    
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Extreme Burn Severity - West") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
    
    
    
    
  }else if(region == "NA"){
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Median Burn Severity - All of Ontario") + 
      scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
  }
}else if (response == "heterogeneity"){
  #plot
  plt <- ggplot()+
    geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
    theme_bw()+
    theme(axis.title = element_text(size = 14, family = "Helvetica"), 
          axis.text = element_text(size = 10, family = "Helvetica"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
    theme(legend.position ="bottom") + xlab("Standardized Effects")+
    scale_colour_viridis_d("Effect Type", option = "viridis") +
    labs(title = "Burn Severity Heterogeneity") + 
    scale_y_discrete(position = "right") #y to the right to make proper pannel of plots
  
  
  
}
  
 
  
  return(plt)
}







