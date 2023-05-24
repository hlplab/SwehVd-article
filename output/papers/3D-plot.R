make_ellipse <- function(mu, Sigma, cues) {
  ellipse = rgl::ellipse3d(Sigma, subdivide = 5)
  
  x = as.numeric(ellipse$vb[1,] + mu[1])
  y = as.numeric(ellipse$vb[2,] + mu[2])
  z = as.numeric(ellipse$vb[3,] + mu[3])
  
  t = tibble(x, y, z)
  names(t) = cues
  return(t)
}

plot_vowel3D <- function(
    .data = d.SwehVd,
    quantity = "long",
    cues = c("F1", "F2", "F3"),
    axis.expansion = 1.1
) {
  require(rlang)
  
  .data %<>%
    filter(Quantity %in% quantity) %>%
    filter(Transcribed_vowel == "targeted", Word != "hädd") %>%
    select(category, Quantity, !!! syms(cues), Location) %>%
    pivot_longer(
      cols = Location,
      values_to = "Location") %>%
    select(-name)
  
  
  axx <- list(
    title = cues[1],
    nticks = 4,
    range = range(.data[, cues[1]]) * axis.expansion,
    gridwidth = 2,
    autorange="reversed"
  )
  axy <- list(
    title = cues[2],
    nticks = 4,
    range = range(.data[, cues[2]]) * axis.expansion,
    gridwidth = 2,
    autorange="reversed"
  )
  axz <- list(
    title = cues[3],
    nticks = 4,
    range =  range(.data[, cues[3]]) * axis.expansion,
    gridwidth = 2
  )
  
  axes <- list(axx, axy, axz)
  
  m <-
    make_MVG_ideal_observer_from_data(.data, group = "Location", cues = cues) %>%
    mutate(e = map2(mu, Sigma, ~ make_ellipse(.x, .y, cues = cues))) %>%
    unnest(e)
  
  .data %>%
    plot_ly(
      x= ~F1, y= ~F2, z= ~F3,
      color= ~category, # here we could add *what* colors to use (in addition to the color argument): colors = colors.Category[.data.IO$Model.Sound],
      frame = ~Location,
      opacity = .2,
      size = 1,
      showlegend = T,
      type="scatter3d", mode="markers") %>%
    add_trace(
      data = m,
      type = 'mesh3d',
      x= ~F1, y= ~F2, z= ~F3,
      alphahull = 0,
      frame = ~Location,
      opacity = .1,
      color = ~category, # here we could add *what* colors to use (in addition to the color argument): vertexcolor = colors.Category[.data.IO$Model.Sound],
      hoverinfo = 'text',
      text = ~paste("Category:", category,
                    "<br>F1:", signif(F1, 4),
                    "<br>F2:", signif(F2, 4),
                    "<br>F3:", signif(F3, 4)),
      inherit = F,
      showlegend = F) %>%
    layout(scene = list(xaxis = axes[[1]], yaxis = axes[[2]], zaxis = axes[[3]], aspectmode='cube'))
  
  # Talk to Xin to set perspective on 3d plot that puts x = F2 (reverted)  and y = F1 (reverted)
}

plot_vowel3D(quantity = "long")
plot_vowel3D(quantity = "short")
plot_vowel3D(quantity = c("short", "long"))
