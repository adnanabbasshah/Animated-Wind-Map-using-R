# Animated-Wind-Map-using-R
idea:  Milos Popovic
install.packages("pacman")
pacman::p_load(
  ecmwfr, terra, tidyverse, metR,
  classInt, scico, gganimate,
  lubridate, hms, sf, gifski
)

folder <- "D:/cv/dates"
files <- list.files(path = folder, pattern = "\\.nc$", full.names = TRUE)

proc <- function(f) {
  u <- terra::rast(f, subds = "u10")
  v <- terra::rast(f, subds = "v10")
  
  dt <- str_extract(basename(f), "\\d{4}-\\d{1,2}-\\d{1,2}")
  dt <- lubridate::ymd_hms(paste0(dt, " 00:00:00"))
  
  names(u) <- paste0("u_", seq(0, 21, 3))
  names(v) <- paste0("v_", seq(0, 21, 3))
  
  c(u, v) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE) %>%
    pivot_longer(
      cols = starts_with(c("u", "v")),
      names_to = c("var", "hour"),
      names_pattern = "^(u|v)_(\\d+)$",
      values_to = "spd"
    ) %>%
    pivot_wider(names_from = var, values_from = spd) %>%
    mutate(time = dt + hours(as.numeric(hour))) %>%
    group_by(x, y, time) %>%
    summarise(u = mean(u, na.rm = TRUE), v = mean(v, na.rm = TRUE), .groups = "drop") %>%
    as_tibble()
}

safe_proc <- purrr::safely(proc)
res <- purrr::map(files, safe_proc)
all_data <- purrr::map_dfr(res, "result")

dates <- seq(min(all_data$time), max(all_data$time), by = "3 days")
samp <- all_data %>% filter(as.Date(time) %in% as.Date(dates))

strm <- samp %>%
  split(.$time) %>%
  purrr::map_dfr(function(df) {
    tm <- df$time[1]
    p <- ggplot(df, aes(
      x = x, y = y, dx = u, dy = v,
      color = sqrt(after_stat(dx)^2 + after_stat(dy)^2),
      alpha = sqrt(after_stat(dx)^2 + after_stat(dy)^2),
      linewidth = sqrt(after_stat(dx)^2 + after_stat(dy)^2)
    )) +
      metR::stat_streamline(L = 7, res = 2, geom = "path")
    ld <- layer_data(p)
    ld$speed <- sqrt(ld$dx^2 + ld$dy^2)
    ld$time <- tm
    ld
  })

brks <- classInt::classIntervals(strm$speed, n = 6, style = "equal")$brks

pak <- st_read("D:/Pakistan shape file with kashmir/Pakistan_with_Kashmir.shp")

plt <- ggplot() +
  geom_sf(data = pak, fill = NA, color = "black", linewidth = 1) +
  geom_path(
    data = strm,
    aes(x = x, y = y, group = group, color = speed, linewidth = speed),
    lineend = "round", show.legend = TRUE
  ) +
  scico::scale_color_scico(
    palette = "imola", direction = -1,
    name = "Wind Speed (m/s)",
    breaks = brks,
    labels = round(brks, 1)
  ) +
  scale_linewidth(range = c(.1, 1), guide = "none") +
  coord_sf(xlim = c(60.5, 80.5), ylim = c(20.5, 38.0), expand = FALSE) +
  labs(
    title = "Wind Patterns Over Pakistan",
    subtitle = "Date: {format(frame_time, '%B %d, %Y %H:%M')}",
    caption = "Data: ERA5 Reanalysis | Visualization: Adnan Abbas Shah"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position.inside = c(.92, .25),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank()
  ) +
  transition_time(time) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

anim <- animate(
  plt,
  nframes = length(unique(strm$time)) * 3,
  fps = 10,
  width = 900,
  height = 700,
  renderer = gifski_renderer(),
  detail = 2
)

anim_save("pakistan_wind_animation_april_may1.gif", anim)

animate(
  plt,
  nframes = length(unique(strm$time)) * 2,
  fps = 5,
  width = 600,
  height = 450,
  renderer = gifski_renderer()
) %>% anim_save("pakistan_wind_animation_small.gif")
