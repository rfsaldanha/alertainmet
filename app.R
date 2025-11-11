# Packages
library(shiny)
library(bslib)
library(arrow)
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(inmetrss)

# INMET parquet file address
parquet_url <- "https://inmetalerts.nyc3.cdn.digitaloceanspaces.com/inmetalerts.parquet"

# INMET dataset
ds <- read_parquet(parquet_url)
ds <- parse_mun(ds)
ds$mun_codes <- substr(ds$mun_codes, 0, 6)

# Read municipality seats data
mun_seats <- readRDS("data/mun_seats.rds")

# Municipality list for selector
mun_names <- mun_seats$code_muni
names(mun_names) <- paste(mun_seats$name_muni, "-", mun_seats$abbrev_state)

# Municipality codes and names
ref_mun_names <- mun_seats |>
  st_drop_geometry() |>
  select(code_muni, name_muni, abbrev_state) |>
  mutate(name_muni = paste(name_muni, "-", abbrev_state)) |>
  select(-abbrev_state) |>
  as_tibble()

# Interface
ui <- page_navbar(
  title = "Alertas INMET",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'selo_obs_h.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Alertas meteorológicos",

    # Sidebar
    layout_sidebar(
      sidebar = sidebar(
        uiOutput(outputId = "sel_year_UI"),
        uiOutput(outputId = "sel_mun_UI")
      ),

      # Card
      card(
        full_screen = TRUE,
        card_body(
          plotOutput(outputId = "graph")
        )
      )
    )
  ),

  # About page
  nav_panel(
    title = "Sobre o projeto",
    card(
      p(
        "Acompanhamento de dados de alertas meteorológicos coletados diariamente no sistema de alertas do INMET."
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Year selector
  output$sel_year_UI <- renderUI({
    res <- ds |>
      mutate(year = year(onset)) |>
      select(year) |>
      distinct(year) |>
      arrange(-year) |>
      pull(year)

    selectInput(
      inputId = "sel_year",
      choices = res,
      label = "Ano"
    )
  })

  # Municipality selector
  output$sel_mun_UI <- renderUI({
    selectInput(
      inputId = "sel_mun",
      choices = mun_names,
      label = "Município"
    )
  })

  output$graph <- renderPlot({
    req(input$sel_mun)

    name <- ref_mun_names |>
      filter(code_muni == input$sel_mun) |>
      select(name_muni)

    ds |>
      filter(mun_codes == input$sel_mun) |>
      mutate(year = year(onset)) |>
      filter(year == 2025) |>
      ggplot(aes(
        x = onset,
        xend = expires,
        y = event,
        yend = event,
        col = event
      )) +
      geom_segment(size = 20) +
      labs(
        title = "Alertas meteorológicos INMET",
        subtitle = name$name_muni[1],
        x = "Duração",
        y = NULL,
        caption = "Elaboração: LIS/ICICT/Fiocruz"
      ) +
      theme_bw() +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
