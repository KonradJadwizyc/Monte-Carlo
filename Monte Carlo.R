library(dplyr)
library(Matrix)
library(tidyverse)
library(plotly)
library(ggplot2)



liczba_pi <- function(losowanie,ile_pot_2) {
  # ramka danych dla koła 
  kolo <- data_frame(x = numeric(), y = numeric(), k = logical())
  # ramka danych dla losowania potęki oraz ilość rzutów 
  df <- data_frame(nr_losowania = numeric(), potęga_2 = numeric(), pi = numeric())
  # domyślnie ustawiam wartość na zero oraz resetuje wartość l z pamięci
  l <- 0
  # losuje potęgę liczby 2
  for(j in 1:ile_pot_2) {
    
    max <- 2^j
    
    cat(paste(max, " "))
    
    # promień koła
    r <- 100
    for(w in 1:losowanie){
      # rzucamy 5000 razy ołówkiem
      for(i in 1:max) {
        
        # losowe współrzędne punktu
        x <- sample(c(-1,1),1) * sample(0:r, 1)
        y <- sample(c(-1,1),1) * sample(0:r, 1)
        
        # w jakim miejscu upadł ołówek?
        kolo[i, "x"] <- x
        kolo[i, "y"] <- y
        
        # czy wylosowany punkt wypadł w kole?
        kolo[i, "k"] <- ifelse(x^2 + y^2 <= r^2, TRUE, FALSE)
        
        
      }
      # licze ile punktów wpadło a ile nie
      a <- kolo %>% group_by(k) %>% count(k) 
      # sumuje wszystkie punkty
      c <- sum(a$n)
      # licze przybliżenie liczby pi
      f = 4 * (a[2,2] / c) %>% mutate(liczba_pi = n)
      # przeskok o jesden watości l 
      l <- l + 1
      # przypisanie do tablicy wyników
      
      
      df[l, "nr_losowania"] <- w
      df[l, "potęga_2"] <- max
      df[l, "pi"] <- f
    }
  }
  # wykres wyników
  p <- kolo %>%
    plot_ly(
      x = ~x, 
      y = ~y, 
      color = ~k,
      type = 'scatter',
      mode = 'markers'
    ) 
  
  box <- boxplot(df$pi ~ df$potęga_2, 
                 alpha = 0.5, 
                 col = "green", 
                 ylab = "przybliżenie liczby Pi",
                 xlab = "kolejne potęgi liczby 2",
                 main = "boxplot losowań",
                 ylim = c(0,4.5))
  abline(h = 3.14159265, col = "red")
  
  return(p)
  return(df)
  return(f)
  
}
