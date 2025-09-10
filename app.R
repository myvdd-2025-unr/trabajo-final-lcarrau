
# Paquetes necesarios
library(shiny)
library(readr)
library(tidyverse)
library(plotly)
library(patchwork)
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

# UI

MiInterfaz <- fluidPage(
  title = "Libros de Open Library",
  titlePanel("Catálogo de libros de Open Library"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  h1("Valoración de libros según autor y año de primera publicación"),
  br(), # line break
  HTML("Open Library es un proyecto de Internet Archive que busca crear una página web para cada libro 
       publicado. Además de ofrecer fichas bibliográficas completas, permite acceder a millones de textos: 
       algunos disponibles para descarga libre por ser de dominio público y otros en préstamo digital, 
       emulando el funcionamiento de una biblioteca tradicional. También es colaborativa, ya que los usuarios 
       pueden contribuir agregando o editando información sobre los libros."),
 br(),
 br(),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("slider2",
                  label = h5("Año de primera publicación"), 
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
  
  # Agregar pestañas
  
  mainPanel(
    tabsetPanel(
      # Pestaña 1: Gráfico de valoración de libros
      tabPanel("Gráfico de Valoración", 
               plotlyOutput(outputId = "graf1")),
      
      # Pestaña 2: Gráficos de Top 10
      tabPanel("Top 10",
               h3("Principales Autores y Libros"),
               plotOutput(outputId = "top10_cuadricula", height = "800px"))
    )
  )
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
                       "<br>Año:", first_publish_year,
                       "<br>Cantidad de votos:", rating_count,
                       "<br>Valoración:", average_rating)) +
      geom_point(color = "#98d8c9") +
      labs(x = "Cantidad de votos", y = "Valoración") +
      theme_classic()

    ggplotly(a, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = list(standoff = 20), # Agrega 20px de espacio al título del eje X
          ticks = "outside" # Mueve las marcas del eje hacia afuera
        ),
        yaxis = list(
          title = list(standoff = 20) # Agrega 20px de espacio al título del eje Y
        )
      )
    
  })
  # --- Gráficos del TOP 10 ---
  
  # Gráfico de Top 10 autores por rating
  grafico_autores_rating <- reactive({
    books_app %>%
      group_by(author) %>%
      summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>%
      arrange(desc(average_rating)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(y = reorder(author, average_rating), x = average_rating)) +
      geom_bar(stat = "identity", fill = "#98d8c9") +
      labs(title = "Top 10 Autores según su puntuación", y = NULL, x = "Puntuación Promedio") +
      theme_minimal() +
      theme(
        # Aumenta el tamaño del título de los ejes
        axis.title = element_text(size = 14), 
        # Aumenta el tamaño de los números y etiquetas de los ejes
        axis.text = element_text(size = 12),
        # Ajusta el tamaño del título del gráfico
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })
  
  # Gráfico de Top 10 libros por rating
  grafico_libros_rating <- reactive({
    books_app %>%
      group_by(title) %>%
      summarise(average_rating = mean(average_rating, na.rm = TRUE)) %>%
      arrange(desc(average_rating)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(y = reorder(title, average_rating), x = average_rating)) +
      geom_bar(stat = "identity", fill = "#98d8c9") +
      labs(title = "Top 10 Libros según su puntuación", y = NULL, x = "Puntuación Promedio") +
      theme_minimal() +
      theme(
        # Aumenta el tamaño del título de los ejes
        axis.title = element_text(size = 14), 
        # Aumenta el tamaño de los números y etiquetas de los ejes
        axis.text = element_text(size = 12),
        # Ajusta el tamaño del título del gráfico
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })
  
  # Gráfico de Top 10 autores por ediciones
  grafico_autores_ediciones <- reactive({
    books_app %>%
      group_by(author) %>%
      summarise(editions = mean(editions, na.rm = TRUE)) %>%
      arrange(desc(editions)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(y = reorder(author, editions), x = editions)) +
      geom_bar(stat = "identity", fill = "#98d8c9") +
      labs(title = "Top 10 Autores con más ediciones", y = NULL, x = "Cantidad de Ediciones") +
      theme_minimal() +
      theme(
        # Aumenta el tamaño del título de los ejes
        axis.title = element_text(size = 14), 
        # Aumenta el tamaño de los números y etiquetas de los ejes
        axis.text = element_text(size = 12),
        # Ajusta el tamaño del título del gráfico
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })
  
  # Gráfico de Top 10 libros por ediciones
  grafico_libros_ediciones <- reactive({
    books_app %>%
      group_by(title) %>%
      summarise(editions = mean(editions, na.rm = TRUE)) %>%
      arrange(desc(editions)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(y = reorder(title, editions), x = editions)) +
      geom_bar(stat = "identity", fill = "#98d8c9") +
      labs(title = "Top 10 Libros con más ediciones", y = NULL, x = "Cantidad de Ediciones") +
      theme_minimal() +
      theme(
        # Aumenta el tamaño del título de los ejes
        axis.title = element_text(size = 14), 
        # Aumenta el tamaño de los números y etiquetas de los ejes
        axis.text = element_text(size = 12),
        # Ajusta el tamaño del título del gráfico
        plot.title = element_text(size = 16, hjust = 0.5)
      )
  })
  
  # Renderiza la cuadrícula de 2x2 en un solo `renderPlot`
  output$top10_cuadricula <- renderPlot({
    library(patchwork)
    
    # Combina los gráficos y crea el diseño 2x2
    (grafico_autores_rating() + grafico_libros_rating()) /
      (grafico_autores_ediciones() + grafico_libros_ediciones()) +
      plot_layout(guides = "collect") # Recopila las guías para que no se dupliquen
  })
}

shinyApp(ui = MiInterfaz, server = MiServidor)


