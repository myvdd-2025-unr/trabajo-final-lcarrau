
# Paquetes necesarios
library(shiny)
library(readr)
library(tidyverse)
library(plotly)
# library(shinydashboard)
# library(shinythemes)
library(bslib)
library(shinyWidgets)

# Importación de datos
books_app <- read_delim(file = "books.txt", delim ="\t", show_col_types = FALSE)


# Crear la función para el gráfico


# Puntuación y cantidad de votos, por genero y por autor

lista_generos <- books_app %>% 
  separate_rows(genres, sep = ", ") %>% 
  pull(genres) %>% 
  unique() %>% 
  sort()

lista_autores <- books_app %>%
  pull(author) %>%
  unique() %>%
  sort()

grafico <- function(autor, tiempo) {
  
filtrado <- books_app %>% 
  filter(
    author == autor,
    between(first_publish_year, tiempo[1], tiempo[2])
  )
  
  a <- ggplot(data = filtrado) +
  aes(x = rating_count, y = average_rating, text = title) +
  scale_x_continuous(limits = range(books_app$rating_count)) +
  geom_point(color = "skyblue") +
  theme_classic()

ggplotly(a)

}

# Los 10 más (podría cambiar entre autor o libro y por año: 
  # autores y libros con puntajes más altos

# autor - puntaje

books_app %>%
  group_by(author) %>%
  summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>% #por si tengo varias filas por autor
  arrange(desc(average_rating)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = reorder(author, average_rating), x = average_rating)) +
  geom_bar(stat = "identity", fill = "#3498DB") +  
  labs(
    title = "Top 10 Autores por Puntuación Promedio",
    y = "Autor",
    x = "Puntuación Promedio"
  ) +
  theme_minimal()
  
top_10_autores <- books_app %>%
  group_by(author) %>%
  summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>%
  arrange(desc(average_rating)) %>%
  slice_head(n = 10)

# libro - puntaje

books_app %>%
  group_by(title) %>%
  summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>%
  arrange(desc(average_rating)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = reorder(title, average_rating), x = average_rating)) +
  geom_bar(stat = "identity", fill = "#3498DB") +  
  labs(
    title = "Top 10 Libros por Puntuación Promedio",
    y = "Libro",
    x = "Puntuación Promedio"
  ) +
  theme_minimal()

top_10_libros <- books_app %>%
  group_by(title) %>%
  summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>%
  arrange(desc(average_rating)) %>%
  slice_head(n = 10)

  # autores y libros con mayor cantidad de ediciones

# autor - ediciones

books_app %>%
  group_by(author) %>%
  summarise(editions = mean(editions, na.rm = TRUE)) %>%
  arrange(desc(editions)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = reorder(author, editions), x = editions)) +
  geom_bar(stat = "identity", fill = "#3498DB") +  
  labs(
    title = "Top 10 Autores por Cantidad de ediciones",
    y = "Autor",
    x = "Cantidad de ediciones"
  ) +
  theme_minimal()

# libro - ediciones
books_app %>%
  group_by(title) %>%
  summarise(editions = mean(editions, na.rm = TRUE)) %>%
  arrange(desc(editions)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = reorder(title, editions), x = editions)) +
  geom_bar(stat = "identity", fill = "#3498DB") +  
  labs(
    title = "Top 10 Libros por Cantidad de ediciones",
    y = "Libro",
    x = "Puntuación Promedio"
  ) +
  theme_minimal()

# UI

MiInterfaz <- fluidPage(
  title = "Libros de Open Library",
  titlePanel("Catálogo de libros de Open Library"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  h1("Valoración de libros según autor y año de primera publicación"),
  br(), # line break
  HTML("Catálogo de libros"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("slider2",
                  label = h3("Rango de años"), 
                  min = min(books_app$first_publish_year),
                  max = max(books_app$first_publish_year), 
                  value = c(1500, 2000)),
      
      multiInput(
        inputId = "autor1",
        label = "Autor :", 
        choices = lista_autores,
        selected = "A. A. Milne", 
        width = "100%"
        )
      
  ),
  mainPanel(plotlyOutput(outputId = "graf1"))
 )
)

# Servidor

MiServidor <- function(input, output) {

  # Expresión reactiva para el data frame filtrado
  # Se ejecuta cada vez que 'autor1' o 'slider2' cambian
  
  data_filtrada <- reactive({
    # req() previene que el código se ejecute si las entradas son NULL
    req(input$autor1, input$slider2)
    
    books_app %>%
      filter(
        author %in% input$autor1,
        between(first_publish_year, input$slider2[1], input$slider2[2])
      )
  })

  # Renderiza el gráfico
  output$graf1 <- renderPlotly({
    a <- ggplot(data = data_filtrada()) +
      aes(x = rating_count, 
          y = average_rating, 
          text = paste("Título:", title,
                       "<br>Autor:", author,
                       "<br>Año:", first_publish_year)) +
      geom_point(color = "#98d8c9") +
      theme_classic()
    
    ggplotly(a)
  })
  }

shinyApp(ui = MiInterfaz, server = MiServidor)


