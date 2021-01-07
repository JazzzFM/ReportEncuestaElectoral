# Diseño Muestral 

gglevantamiento <- function(BD) {
  data <- BD %>%
    select(modoLevantamiento) %>% 
    mutate(n = 1) %>% 
    group_by(modoLevantamiento) %>% 
    summarise(across(n, sum)) %>%
    tibble(x = 1:3)
  
  Annotations <- tibble(data %>% select(modoLevantamiento), x = 1:3)
  
  Graph <- ggplot(data, aes(x = x, y = 0, xend = x, yend = n, fill = modoLevantamiento, colour = modoLevantamiento, label = modoLevantamiento))+
    scale_y_continuous(name = "Stopping distance", limits = c(-1, max(data$n) + 1)) +  
    geom_segment(lineend = "round", linejoin = "round", size = 30, arrow = arrow(length = unit(.0001, "inches"))) +
    annotate("text", hjust = 0.5, vjust = 0.5, label = Annotations$modoLevantamiento, x = Annotations$x, y = -1, size = 5, colour = "#13384D") +
    # geom_fit_text(position = "dodge", grow = FALSE, reflow = TRUE, 
    #               place = "left", color = "white") +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    labs(title = "Modo de levantamiento", caption = "") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Avenir Next", size = 15),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  
  return(Graph)
}

ggbarrasLevantamiento <- function(DB) {
  
  data <- DB %>% select("modoLevantamiento") %>%
    mutate(n = 1) %>%
    group_by(modoLevantamiento) %>%
    summarise(across(n, sum))
  
  
  Graph <- ggplot(data, aes(x = modoLevantamiento, y = n,
                            fill = as.factor(modoLevantamiento),
                            label = as.factor(modoLevantamiento))) +
    geom_bar(stat = "identity") +
    geom_bar_text(position = "stack", reflow = TRUE, grow = TRUE, contrast = TRUE) +
    labs(y = "frecuencia", title = "Modo de levantamiento", caption = "") +
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    theme(
      axis.title.y = element_text(family = "Open Sans", colour = "#751438", size = 15),
      text = element_text(family = "Open Sans", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Open Sans", size = 15),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  
  return(Graph)
}

WordCldTV <- function(BD){
  # Cuidar que no haya na
  
  word <- select(BD, marcoMuestral) %>% na.omit() # Pregunta
  #titulo <- "Marco Muestral"
  
  # Frecuencias
  palabras <- word %>%
    tidytext::unnest_tokens(input = marcoMuestral, output = palabra, token = "words") %>%
    count(palabra)
  
  # Graficar
  
  wrdcld <- ggwordcloud(palabras$palabra, freq = 1,
                        #scale = c(4, 0.5),
                        max.words = 30, color = palabras$palabra,
                        random.order = TRUE, random.color = FALSE) +
    scale_size_area(max_size = 20) +
    scale_color_brewer(palette="Spectral") +
    theme_minimal() +
    labs(title = "Marco Muestral") +
    theme(
      text = element_text(family = "Open Sans", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Avenir Next", size = 15),
    )
  
  
  return(wrdcld)
}

ggAleatoria <- function(BD){
  
  tot <- BD %>% select(aleatoria) %>% nrow()
  mid <- round(tot/2)
  
  aux <- BD %>% 
    select(aleatoria) %>% 
    filter(aleatoria == "Sí") %>% 
    count() %>% 
    mutate(color= case_when(n >= mid ~"#C0D294", F ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = n),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = round(n, digits = 1)), color = aux$color,
              x=-5, y=5, size=60/.pt)+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    labs(title = "Total de aleatorias") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}

ggNoaleatoria <- function(BD){
  
  tot <- BD %>% select(aleatoria) %>% nrow()
  tot <- round(tot/2)
  aux <- BD %>% 
    select(aleatoria) %>% 
    filter(aleatoria == "No") %>% 
    count() %>% 
    mutate(color= case_when(n >= tot ~"#C0D294", F ~"#5E4EA2"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = n),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = round(n, digits = 1)), color = aux$color,
              x=-5, y=5, size=60/.pt)+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10)) +
    theme_minimal() +
    labs(title = "Total de no aleatorias") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
    )
  return(Graph)
}

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ggEstratificada <- function(BD) {
  data <- BD %>%
    select(estratificada, nivielEstratificada) %>%
    mutate(n = 1) %>%
    group_by(nivielEstratificada) %>%
    summarise(across(n, sum))
  
  Graph <- ggplot(data, aes(x = nivielEstratificada, y = n, label = nivielEstratificada, fill = as.factor(nivielEstratificada), colour = as.factor(nivielEstratificada))) +
    geom_bar(stat = "identity") +
    geom_bar_text(position = "stack", reflow = TRUE, grow = FALSE, contrast = TRUE) +
    labs(title = "Estratificada por niveles", x = "nivel", y = "frecuencia") +
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    #scale_color_brewer(palette="Spectral") +
    theme(
      axis.title.y = element_text(colour = "#751438", family = "Open Sans", size = 20),
      axis.title.x =  element_text(colour = "#751438", family = "Open Sans", size = 20),
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Open Sans", size = 15),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  return(Graph)
}

ggPoliEtapa <- function(BD) {
  data <- BD %>%
    select(poliEtapa, nivelpoliEtapa) %>%
    mutate(n = 1) %>%
    group_by(nivelpoliEtapa) %>%
    summarise(across(n, sum))
  
  Graph <- ggplot(data, aes(x = nivelpoliEtapa, y = n, label = as.factor(nivelpoliEtapa), fill = as.factor(nivelpoliEtapa), colour = as.factor(nivelpoliEtapa))) +
    geom_bar(stat = "identity") +
    geom_bar_text(position = "stack", reflow = TRUE, grow = FALSE, contrast = TRUE) +
    labs(title = "Polietápica por niveles", x = "nivel", y = "frecuencia") +
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    theme(
      axis.title.y = element_text(colour =  "#751438", family = "Open Sans", size = 20),
      axis.title.x = element_text(colour =  "#751438", family = "Open Sans", size = 20),
      text = element_text(family = "Open Sans", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Open Sans", size = 15),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  return(Graph)
}

ggConglomerados <- function(BD) {
  data <- BD %>%
    select(conglomerados, nivielConglomerados) %>%
    mutate(n = 1) %>%
    group_by(nivielConglomerados) %>%
    summarise(across(n, sum))
  
  Graph <- ggplot(data, aes(x = nivielConglomerados, y = n, label = as.factor(nivielConglomerados), fill = as.factor(nivielConglomerados))) +
    geom_bar(stat = "identity") +
    geom_bar_text(position = "stack", reflow = TRUE, grow = FALSE, contrast = TRUE) +
    labs(title = "Conglomerados por niveles", x = "nivel", y = "frecuencia") +
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    scale_fill_brewer(palette="Spectral") +
    theme(
      axis.title.y = element_text(colour = "#751438", family = "Open Sans", size = 20),
      axis.title.x =  element_text(colour = "#751438", family = "Open Sans", size = 20),
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Open Sans", size = 15),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid = element_blank()
    )
  return(Graph)
}

ggUnidadMuestral <- function(BD) {
  
  data <- BD %>% 
    select(unidadMuestral) %>% 
    mutate(n = 1) %>% 
    group_by(unidadMuestral) %>%
    summarise(across(n, sum))
  
  Graph <- ggplot(data, aes(y = n, x = unidadMuestral,
                            fill = unidadMuestral,
                            label = unidadMuestral)) +
    geom_bar(stat = "identity") +
    coord_polar() +
    geom_fit_text(position = "stack", place = "topleft", min.size = 0, grow = TRUE, contrast = TRUE) +
    scale_fill_brewer(palette="Spectral") +
    ylim(0, NA) +
    labs(title = "Unidades muestrales") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(family = "Open Sans", size = 15),
      plot.title = element_text(size = 22,
                                colour =  "#751438",
                                hjust = 0),
      axis.text.y = element_text(family = "Open Sans", size = 10),
      axis.text.x = element_blank(),
      # axis.line.x = element_blank(),
      # panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      #panel.grid.major.x = element_blank(),
      #panel.grid = element_blank()
    )
  return(Graph)
}


ggMinNivelConfianza <- function(BD){
  
  tot <- BD %>%nrow()
  mid <- round(tot/2)
  
  aux_2 <- BD %>% 
    select(nivelConfianza) %>%
    pull(nivelConfianza) %>%
    min()
  
  aux <- BD %>% 
    select(nivelConfianza) %>% 
    filter(nivelConfianza == aux_2) %>% 
    mutate(color= case_when(nivelConfianza >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=tot, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(nivelConfianza, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    labs(title = "Nivel de confianza minimo") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}

ggModaNivelConfianza <- function(BD){
  tot <- BD %>% nrow()
  mid <- round(tot/2)
  aux_2 <- BD  %>% 
    select(nivelConfianza) %>%
    pull(nivelConfianza) %>% 
    getmoda()
  
  aux <- BD %>% 
    select(nivelConfianza) %>% 
    filter(nivelConfianza == aux_2) %>% 
    mutate(color= case_when(nivelConfianza >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=tot, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(nivelConfianza, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    labs(title = "Nivel de confianza frecuente") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}

ggMaxNivelConfianza <- function(BD){
  tot <- BD %>% nrow()
  mid <- round(tot/2)
  aux_2 <- BD %>% 
    select(nivelConfianza) %>% max()
  
  aux <- BD %>% 
    select(nivelConfianza) %>% 
    filter(nivelConfianza == aux_2) %>% 
    mutate(color= case_when(nivelConfianza >= mid ~"#C0D294", T ~"#9D1742"))
  
  Graph <- aux %>% ggplot() +
    annotate(x=1, xend=1, y=0, yend=tot, size=10*1.1, color = aux$color,
             geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                 color = aux$color,
                 lineend = "round", linejoin = "round",
                 size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
    geom_text(aes(label = paste0(round(nivelConfianza, digits = 1)), "%"), color = aux$color,
              x=-5, y=5, size=60/.pt, fontface="bold")+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    theme_minimal() +
    scale_color_brewer(palette="Spectral") +
    labs(title = "Nivel de confianza máximo") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          text = element_text(family = "Open Sans", size = 20),
          plot.title = element_text(size = 22,
                                    colour =  "#751438",
                                    hjust = 0),
          axis.line.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
          
    )
  return(Graph)
}

ggClaridadObjetivos <- function(DB){
    
    DB <- DB %>% 
      mutate(n = 1) %>% 
      group_by(nivelClaridad) %>% 
      summarise(across(n, sum)) %>% 
      ungroup()
    
    n <- DB %>% nrow()
    
    barras <- data.frame(DB, y = n:1)
    
    Annotations <- data.frame(DB %>% select(n), y = n:1)
    niveles <- tibble(DB %>% select(nivelClaridad), y = n:1)
    
    Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = as.factor(nivelClaridad), group = nivelClaridad, color = as.factor(nivelClaridad))) +
      geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                   arrow = arrow(length = unit(.0001, "inches"))) + 
      scale_color_brewer(palette="Spectral") +
      scale_color_brewer(palette="Spectral") +
      annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
      theme_minimal() +
      labs(title = "Nivel de claridad", subtitle = "", caption = "", x = "", y = "") +
      annotate("text", label = niveles$nivelClaridad, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#751438"),
        text = element_text(family = "Open Sans", size = 20),
        plot.title = element_text(size = 22,
                                  colour =  "#751438",
                                  hjust = 0,),
        axis.text.y = element_blank(),
        axis.text.x = element_text( color = "#751438",family = "Avenir Next", size = 15),
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank()
      )
    
    return(Graph)
  }
  
  #BD <- procesamiento("nivelClaridad") 
  #ggClaridadObjetivos(BD)
  
  ggOperacionalizacion <- function(DB){
    
    DB <- DB %>%
      mutate(n = 1) %>% 
      group_by(operacionalizacion) %>%
      summarise(across(n, sum)) %>%
      ungroup()
    
    DB <- DB %>%
      mutate(opera = case_when(
        operacionalizacion != "No" ~ paste("Sí", sapply(strsplit(operacionalizacion, ", "),"[", 2), sep = " "),
        operacionalizacion == "No" ~ "No"))
    
    n = DB %>% nrow()
    
    barras <- data.frame(DB, y = n:1)
    
    Annotations <- data.frame(DB %>% select(n), y = n:1)
    niveles <- tibble(DB %>% select(opera), y = n:1)
    
    Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = as.factor(operacionalizacion), group = operacionalizacion, color = as.factor(operacionalizacion))) +
      geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                   arrow = arrow(length = unit(.0001, "inches"))) +
      scale_color_brewer(palette="Spectral") +
      scale_fill_brewer(palette="Spectral") +
      annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
      theme_minimal() +
      labs(title = "Operacionalización", subtitle = "", caption = "", x = "", y = "") +
      annotate("text", label = niveles$opera, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#751438"),
        text = element_text(family = "Open Sans", size = 20),
        plot.title = element_text(size = 22,
                                  colour =  "#751438",
                                  hjust = 0),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Open Sans", size = 15),
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank()
      )
    
    return(Graph)
  }
  
  ggPoblacionObjetivo <- function(DB){
    
    DB <- DB %>%
      mutate(n = 1) %>% 
      group_by(poblacionObjetivo) %>%
      summarise(across(n, sum)) %>%
      ungroup()
    
    DB <- DB %>%
      mutate(opera = case_when(
        poblacionObjetivo != "No" ~ paste("Sí", sapply(strsplit(poblacionObjetivo, ", "),"[", 2), sep = " "),
        poblacionObjetivo == "No" ~ "No"))
    
    n = DB %>% nrow()
    
    barras <- data.frame(DB, y = n:1)
    
    Annotations <- data.frame(DB %>% select(n), y = n:1)
    niveles <- tibble(DB %>% select(opera), y = n:1)
    
    Graph <- ggplot(barras, aes(x = 0, y = y, xend = n, yend = y, fill = poblacionObjetivo , group = poblacionObjetivo, color = poblacionObjetivo)) +
      geom_segment(lineend = "round", linejoin = "round", size = 9.5,
                   arrow = arrow(length = unit(.0001, "inches"))) +
      scale_color_brewer(palette="Spectral") +
      annotate("text", hjust = 1, label = Annotations$n, x = Annotations$n, y = Annotations$y, size = 6, colour = "white") +
      theme_minimal() +
      labs(title = "Población objetivo", subtitle = "", caption = "", x = "", y = "") +
      annotate("text", label = niveles$opera, vjust = 0, hjust = 0, x = 0, y = niveles$y + 0.3, size = 5, colour = "#8b878d") +
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "#751438"),
        text = element_text(family = "Open Sans", size = 20),
        plot.title = element_text(size = 22,
                                  colour =  "#751438",
                                  hjust = 0),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Open Sans", size = 15),
        axis.line.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank()
      )
    
    return(Graph)
  }
  
  #DB <- procesamiento("poblacionObjetivo")
  #ggPoblacionObjetivo(DB)
  
  ggCantidadBloquesMin <- function(BD){
    tot <- BD %>% nrow()
    mid <- round(tot/2)
    aux_2 <- BD %>% 
      select(cantidadBloques) %>% min()
    
    aux <- BD %>% 
      select(cantidadBloques) %>% 
      filter(cantidadBloques == aux_2) %>% 
      mutate(color = case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
    
    Graph <- aux %>% ggplot() +
      annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
               geom = "segment", alpha= 0.3)+
      geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                   color = aux$color,
                   lineend = "round", linejoin = "round",
                   size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
      geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
                x=-5, y=5, size=60/.pt, fontface="bold")+
      coord_polar(theta = "y") +
      scale_x_continuous(limits = c(-5,2)) +
      scale_y_continuous(limits = c(0, 10))+
      theme_minimal() +
      labs(title = "Cantidad mínima de bloques", caption = "") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            text = element_text(family = "Open Sans", size = 20),
            plot.title = element_text(size = 22,
                                      colour =  "#751438",
                                      hjust = 0),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
      )
    
    return(Graph)
  }
  
  getmoda <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  ggCantidadBloquesModa <- function(BD){
    tot <- BD %>% nrow()
    mid <- round(tot/2)
    aux_2 <- BD  %>% 
      select(cantidadBloques) %>%
      pull(cantidadBloques) %>% 
      getmoda()
    
    aux <- BD %>% 
      select(cantidadBloques) %>% 
      filter(cantidadBloques == aux_2) %>% 
      mutate(color= case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
    
    Graph <- aux %>% ggplot() +
      annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
               geom = "segment", alpha=.5)+
      geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                   color = aux$color,
                   lineend = "round", linejoin = "round",
                   size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
      geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
                x=-5, y=5, size=60/.pt, fontface="bold")+
      coord_polar(theta = "y") +
      scale_x_continuous(limits = c(-5,2)) +
      scale_y_continuous(limits = c(0, 10))+
      theme_minimal() +
      labs(title = "Cantidad frecuente de bloques") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            text = element_text(family = "Open Sans", size = 20),
            plot.title = element_text(size = 22,
                                      colour =  "#751438",
                                      hjust = 0),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
            
      )
    return(Graph)
  }
  
ggCantidadBloquesMax <- function(BD){
    tot <- BD %>% nrow()
    mid <- round(tot/2)
    
    aux_2 <- BD %>% 
      select(cantidadBloques) %>% max()
    
    aux <- BD %>% 
      select(cantidadBloques) %>% 
      filter(cantidadBloques == aux_2) %>% 
      mutate(color= case_when(cantidadBloques >= mid ~"#C0D294", T ~"#9D1742"))
    
    Graph <- aux %>% ggplot() +
      annotate(x=1, xend=1, y=0, yend=10, size=10*1.1, color = aux$color,
               geom = "segment", alpha=.5)+
      geom_segment(aes(x = 1, y = 0, xend = 1, yend = aux_2),
                   color = aux$color,
                   lineend = "round", linejoin = "round",
                   size =  10, arrow = arrow(length = unit(0, "inches"))  ) +
      geom_text(aes(label = paste0(round(cantidadBloques, digits = 1)), "%"), color = aux$color,
                x=-5, y=5, size=60/.pt, fontface="bold")+
      coord_polar(theta = "y") +
      scale_x_continuous(limits = c(-5,2)) +
      scale_y_continuous(limits = c(0, 10))+
      theme_minimal() +
      labs(title = "Cantidad máxima de bloques") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            text = element_text(family = "Open Sans", size = 20),
            plot.title = element_text(size = 22,
                                      colour =  "#751438",
                                      hjust = 0),
            axis.line.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none")
    
      return(Graph)
}
