# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



plot_mean_length = function(data, colores, labels, name_plot, output_path, heigh_plot, width_plot, size_axis_text = 12, angle_text_x = 90, size_title_axis = 14){

  tmedia_sex <- data %>%
    mutate(SEX = factor(SEX, levels = c("Hembra","Macho","Indet"))) %>%
    group_by(season, SEX) %>%
    reframe(tmedia = mean(TL_CM, na.rm = TRUE))

  tmedia_total <- data %>%
    mutate(SEX = factor(SEX, levels = c("Hembra","Macho","Indet"))) %>%
    group_by(season) %>%
    reframe(tmedia = mean(TL_CM, na.rm = TRUE))

  p = tmedia_sex %>%
    ggplot() +
    geom_line(aes(x = season, y = tmedia, group = SEX, col = SEX)) +
    geom_point(aes(x = season, y = tmedia, group = SEX, col = SEX)) +
    geom_line(data = tmedia_total, aes(x = season, y = tmedia, group = "Total", col = "Total")) +
    geom_point(data = tmedia_total, aes(x = season, y = tmedia, group = "Total", col = "Total")) +
    scale_color_manual(values = colores, labels = labels) +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme_bw() +
    labs(y = "Longitud Total Promedio (cm)\n", x = "\nTemporada", col = "") +
    theme(axis.text = element_text(colour = "black", size = size_axis_text),
          axis.text.x = element_text(angle = angle_text_x),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.8,"cm"),
          axis.title = element_text(size = size_title_axis))

  print(p)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)

}



plot_tallas_gridges_sex = function(data, order_years = "decreasing", colores, labels, name_plot, output_path, heigh_plot, width_plot, size_axis_text = 12, angle_text_x = 0, size_title_axis = 14){


  if(order_years %in% "decreasing") {
    lvls = unique(rev(data$season))
  } else if (order_years %in% "increasing") {
    lvls = unique(data$season)
  } else{
    stop("Use only decreasing or increasing")
  }

  p_sex = data %>%
    mutate(season = factor(season, levels = lvls),
           SEX = factor(SEX, levels = c("Hembra","Macho","Indet"), labels = labels)) %>%
    ggplot() +
    geom_density_ridges(aes(x = TL_CM, y = season, fill = SEX, scale = 1)) +
    scale_fill_manual(values = colores, labels = labels) +
    labs(x = "Longitud Total (cm)", y = "", fill = "") +
    theme_bw()  +
    theme(axis.text = element_text(colour = "black", size = size_axis_text),
          axis.text.x = element_text(angle = angle_text_x),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.8,"cm"),
          axis.title = element_text(size = size_title_axis))

  print(p_sex)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)

}


plot_tallas_gridges_total = function(data, order_years = "decreasing", color_total, name_plot, output_path, heigh_plot, width_plot, size_axis_text = 12, angle_text_x = 0, size_title_axis = 14){


  if(order_years %in% "decreasing") {
    lvls = unique(rev(data$season))
  } else if (order_years %in% "increasing") {
    lvls = unique(data$season)
  } else{
    stop("Use only decreasing or increasing")
  }

  p_total = data %>%
    mutate(season = factor(season, levels = lvls)) %>%
    ggplot() +
    geom_density_ridges(aes(x = TL_CM, y = season, fill = "Total", scale = 1)) +
    scale_fill_manual(values = color_total) +
    labs(x = "Longitud Total (cm)", y = "", fill = "") +
    theme_bw()  +
    theme(axis.text = element_text(colour = "black", size = size_axis_text),
          axis.text.x = element_text(angle = angle_text_x),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.8,"cm"),
          axis.title = element_text(size = size_title_axis))

  print(p_total)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)

}



plot_boxplot_sex = function(data, colores, labels, name_plot, output_path, heigh_plot, width_plot){

  plot_bx_sex = data  %>%
    mutate(SEX = factor(SEX, levels = c("Hembra","Macho","Indet"), labels = labels)) %>%
    ggplot() +
    geom_boxplot(aes(x = interaction(SEX, season, sep = "&"), y = TL_CM, fill = SEX))  +
    scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
    labs(y = "Longitud Total (cm)", x = "", size = "n", fill = "Sexo") +
    scale_fill_manual(values = colores, labels = labels) +
    theme_bw() +
    theme(axis.text = element_text(color = "black", size = 10))

  print(plot_bx_sex)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)


}



plot_boxplot_igs_sex = function(data, colores, labels, name_plot, output_path, heigh_plot, width_plot, size_axis_text = 10, size_title_axis = 14) {

  lvls_sex = unique(data$SEX)

  if(length(lvls_sex) != length(labels)) {
    stop("verificar los labels, los levels son ", paste0(lvls_sex," "))
  }

  bxp_sex_season = data %>%
    mutate(mes = factor(MONTH, levels = c(9, 10,11, 12, 1, 2, 3)),
           SEX = factor(SEX, levels = lvls_sex, labels = labels)) %>%
    ggplot(aes(x = interaction(mes, SEX, season, sep = "&"), y = IGS, fill = SEX)) +
    geom_boxplot() +
    scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
    scale_fill_manual(values = colores, labels = labels) +
    labs(x = "") +
    theme_bw() +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.height = unit(0.4, "cm"),
          legend.key.width = unit(0.3, "cm"),
          axis.title = element_text(size = size_title_axis),
          axis.text = element_text(color = "black", size = size_axis_text)) +
    guides(fill = guide_legend(ncol = 1))

  print(bxp_sex_season)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)

}



plot_desembarques_n_facet = function(data, ylab, step_days = 5, name_plot, output_path, heigh_plot, width_plot, angle_text_x = 90, size_title_axis = 14, size_axis_text = 10, size_title_facet = 14){

  plot_line = data %>%
    group_by(season) %>%
    mutate(t = 1:n()) %>%
    ggplot(aes(x = t, y = var)) +
    geom_line() +
    geom_point() +
    facet_wrap(~season) +
    scale_x_continuous(breaks = seq(1, nrow(data), step_days),
                       labels = data$fecha[seq(1, nrow(data), step_days)]) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = angle_text_x, vjust = 0.5),
          axis.text = element_text(color = "black", size = size_axis_text),
          axis.title = element_text(size = size_title_axis),
          strip.text = element_text(size = size_title_facet)) +
    labs(x = "", y = ylab)

  print(plot_line)

  ggsave(file.path(output_path, name_plot), width = width_plot, height = heigh_plot)


}
