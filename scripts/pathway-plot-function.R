#' Effects plots for SEM
#' 
#' This functions creates plots that compare the indirect, direct and total causal effects of between two variables
#' from and piecewiseSEM.
#' @param df1 dataframe consisting of indirect effects and direct effects. 
#'            The dataframe should have a column each for the pathway and response variable as well as...
#' @param df2 Optional. data frame consisting of total causal effects structured in same way as df1
#' @param response one of median, extreme or heterogeneity
#' @param region  one of east or west, or null for entire boreal shield
#'
#' @return returns a effects plot
#' 
#'
#' @examples pathway.plot(cv_ind_eff, cv_tot_eff, response = "heterogeneity")
#' 
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
      geom_vline(xintercept = 0, linetype = 2)+
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
      geom_vline(xintercept = 0, linetype = 2)+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Median Burn Severity - West") + 
      scale_y_discrete(position = "left") #y to the right to make proper pannel of plots
  }else if(region == "NA"){
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      geom_vline(xintercept = 0), linetype = 2+
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
      geom_vline(xintercept = 0, linetype = 2)+
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
      geom_vline(xintercept = 0, linetype = 2)+
      theme(axis.title = element_text(size = 14, family = "Helvetica"), 
            axis.text = element_text(size = 10, family = "Helvetica"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
      theme(legend.position ="bottom") + xlab("Standardized Effects")+
      scale_colour_viridis_d("Effect Type", option = "viridis") +
      labs(title = "Extreme Burn Severity - West") + 
      scale_y_discrete(position = "left") #y to the right to make proper pannel of plots
    
    
    
    
  }else if(region == "NA"){
    #plot
    plt <- ggplot()+
      geom_point(data = df_long, aes(x = effect, y = Pathway, colour = type), size = 5)+
      theme_bw()+
      geom_vline(xintercept = 0, linetype = 2)+
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
    geom_vline(xintercept = 0, linetype = 2)+
    theme(axis.title = element_text(size = 14, family = "Helvetica"), 
          axis.text = element_text(size = 10, family = "Helvetica"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
    theme(legend.position ="bottom") + xlab("Standardized Effects")+
    scale_colour_viridis_d("Effect Type", option = "viridis") +
    labs(title = "Burn Severity Heterogeneity") + 
    scale_y_discrete(position = "left") #y to the right to make proper pannel of plots
  
  
  
}
  
 
  
  return(plt)
}



grouped_path <- function(df1, df2, df3, df4){
  require(patchwork)
  #remove legends 
  df1 <- df1 + theme(legend.position = "none")
  df2 <- df2 + theme(legend.position = "none")
  df3 <- df3 +theme(legend.position = "none")
  
  #change title
  df1 <- df1 + labs(title = "West", subtitle = "Extreme") +   theme(plot.subtitle=element_text(hjust=0.5))
  df2 <- df2 + labs(title = "East", subtitle = "Extreme") +   theme(plot.subtitle=element_text(hjust=0.5)) + theme(axis.title.y = element_blank())
  df3 <-df3 + labs(title = "West", subtitle = "Median") +   theme(plot.subtitle=element_text(hjust=0.5))
  df4 <- df4 + labs(title = "East", subtitle = "Median") +   theme(plot.subtitle=element_text(hjust=0.5))+ theme(axis.title.y = element_blank())
  
  #make plot
  plot <- df1 + df2 + df3 + df4 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
  return(plot)
  
  
}



