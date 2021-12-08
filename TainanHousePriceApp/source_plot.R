rentdist_fig <- function(df){
  ggplot(df, aes(x= 總額元,y= 鄉鎮市區,fill = ..x..)) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = T,
                        quantile_lines = T, quantiles = 2, alpha = 0.90, scale = 0.75, size = 1.05) +
    scale_fill_gradient2(limits = c(0, 50000), 
                         low = "red", 
                         mid = "white", 
                         high = "springgreen4", 
                         midpoint = 25000, guide = 'none')+#, guide = "colourbar") +
    scale_y_discrete(expand = expansion(add = c(1, 1.75))) +
    scale_x_continuous(breaks = seq(0, 50000, 10000), limits = c(0, 50000), oob = rescale_none) +
    labs(x = 'Renting Price', y = "", 
         #title = paste(num_players, "Lowest Career Scorers"), 
         #subtitle = paste0("Among players with at least ", career_rounds, " rounds played between ", years[1],
         #                  " and ", years[2], ".\n"),
         caption = '內政部全國法規資料庫') +
    #theme_teeter() +
    theme_bw()+        #####################################################ADD==================
  theme(plot.margin = unit(c(rep(0.5, 3), 0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 1))
}