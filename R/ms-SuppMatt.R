

print_table_species <- function(species, data) {
  data <- data[data$Species == species, -c(1)]
  print(xtable(data,
            hline.after=c(1),
            align='p{0.1cm}p{4.5cm}p{6cm}p{2cm}p{1cm}p{1cm}',
            label=sprintf("tab:parts_%s", gsub(" ", "_", species)),
            caption=sprintf("Table of parts for \\emph{%s}.", species)),
      sanitize.text.function=I,
      , table.placement = "h"
      , caption.placement = "top"
      , include.rownames = FALSE
      , include.colnames = TRUE
      , size = "small"
      , tabular.environment = 'longtable'
      , floating = FALSE)
}

print_table_metadata <- function(path, align = NULL) {

  data <-  read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  file <- gsub("_", "\\_", basename(path), fixed = TRUE)

  label <- sprintf("tab:%s_meta", strsplit(basename(path), "_")[[1]][1])

  print(xtable(data,
            align=align,
            hline.after=c(1),
            label= label,
            caption=
              sprintf("Description of variables within the file \\texttt{data/%s}. These definitions can also be found within the file \\texttt{data/%s}.",
              gsub("\\_meta", "", file, fixed = TRUE), file)),
      , table.placement = "h"
      , caption.placement = "top"
      , include.rownames = FALSE
      , include.colnames = TRUE
      , size = "small"
      , tabular.environment = 'longtable'
      , floating = FALSE)
}



figure_site_map <- function(file) {

  # fetch map
  basemap <- get_map('Kuringai National Park', zoom = 12, maptype = "hybrid")

  # Load site data 
  data <-  read.csv(file, stringsAsFactors=FALSE, check.names=FALSE) %>% 
    filter(site != "Bobbin2010")

  ## Add nudges to data
  nudge_x <- c(-0.002, 0.03, -0.002, -0.002, 0.035, -0.002)
  nudge_y <- c(0.002, 0, -0.002, 0.001, -0.003, 0.003)


  ggmap(basemap) + 
    labs(x = "Longitude", y = "Latitude") +
  geom_point(data = data, aes(x=Longitude, y=Latitude), 
             colour = "orange") +
  geom_text(data = data, 
            aes(x=Longitude, y=Latitude, label=site),
            size = 2, 
            fontface = "bold",
            colour = "orange",
            hjust = "right",
            nudge_x = nudge_x, nudge_y = nudge_y)
}
