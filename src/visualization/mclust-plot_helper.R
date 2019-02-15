# test[[1]]
# 
# group_colours <- c("blue", "green", "red", "yellow")
# group_shapes <- 1:length(group_colours)

plot_mclust_formants <- function(mclust_result, group_colours, group_shapes) {
    df <-
        mclust_result$classifications %>%
        ungroup() %>% 
        mutate(weight = 1/n())
    
    centres <-
        mclust_result$centres %>%
        mutate(label_text = paste(max(mclust_result$centres), "G", clust, sep = ""))
    
    title <- paste0("Formant tracker, ", str_to_title(df$tracker[1]), " = (", str_to_upper(str_extract(df$tracker[1], "^[f|p]")), ")")
        
    f1_f2_scatterplot <- df %>%
            ggplot(aes(x = f2_z, y = f1_z, color = as.factor(clust), shape = as.factor(clust))) + 
            geom_point(aes(size = uncertainty), alpha = 0.25) +
            geom_point(data = mclust_result$centres, size = 2, color = "black", shape = 16) +
            stat_ellipse() +
            ggrepel::geom_label_repel(data = centres, aes(label = label_text), size = 6, color = "black") +
            xlab("Mid-point F2 (z-normalized)") +
            ylab("Mid-point F1 (z-normalized)") +
            scale_y_reverse() +
            scale_x_reverse() +
            scale_color_manual(values = group_colours) +
            scale_shape_manual(values = group_shapes)
    
    f2_density <- df %>%
        ggplot(aes(x = f2_z, color = as.factor(clust))) + 
        geom_density(data = df, aes(x = f2_z), color = "grey", alpha = 0.1, linetype = 6) +
        geom_density(aes(weight = weight, , linetype = as.factor(clust)), alpha = 0.1) +
        scale_x_reverse() +
        scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
        ylab("Density") +
        xlab("") +
        scale_color_manual(values = group_colours)
    
    f1_density <- df %>%
        ggplot(aes(x = f1_z, color = as.factor(clust))) + 
        geom_density(data = df, aes(x = f1_z), color = "grey", alpha = 0.1, linetype = 6) +
        geom_density(aes(weight = weight, linetype = as.factor(clust)), alpha = 0.1) +
        scale_x_reverse() +
        scale_y_continuous(limits = c(0,0.6), breaks = c(0, 0.2, 0.4, 0.6)) +
        ylab("Density") +
        xlab("") +
        coord_flip() +
        scale_color_manual(values = group_colours)
    
    arrangeGrob(
        f2_density, f1_f2_scatterplot, f1_density,
        layout_matrix = rbind(
            c(1,1,NA),
            c(2,2,3),
            c(2,2,3)
        )
    )
}

# plot_mclust_formants(test[[4]], c("red", "blue", "green", "purple", "orange"), 1:5) %>% grid.arrange()

# grid.arrange(
#     plot_mclust_formants(mclust_plot, "forest", c('#e41a1c','#377eb8','#4daf4a','#984ea3'), 1:4),
#     plot_mclust_formants(mclust_plot, "praat", c('#377eb8', '#984ea3','#4daf4a', '#e41a1c'), c(2,4,3,1)),
#     ncol = 2,
#     top = grid::textGrob("Unsupervised clustering of first and second formant values using finite Gaussian Mixture Models (GMMs)\n", gp=grid::gpar(fontsize=18,font=8))
# )