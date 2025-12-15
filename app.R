# Packages
library(shiny)
library(bslib)
library(arrow)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(ggplot2)
library(inmetrss)
library(DT)
library(tmap)
library(shinybusy)
busy_bar_color <- "brown"
busy_bar_height <- "4px"

# INMET parquet file address
parquet_url <- "https://inmetalerts.nyc3.cdn.digitaloceanspaces.com/inmetalerts.parquet"

# INMET dataset
ds <- read_parquet(parquet_url) |>
  distinct()
ds <- parse_mun(ds)

# Read municipality seats data
mun <- readRDS("data/mun.rds")

# Municipality list for selector
mun_names <- mun$code_muni
names(mun_names) <- mun$name_muni

# Read municipality polygon geometry
br.shp <- readRDS("data/br_mun_2024.rds")

# Interface
ui <- page_navbar(
  title = "Alertas INMET",
  theme = bs_theme(bootswatch = "shiny"),

  sidebar = sidebar(
    uiOutput(outputId = "sel_year_UI"),
    uiOutput(outputId = "sel_mun_UI")
  ),

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

  # Graph page
  nav_panel(
    title = "Alertas meteorológicos",

    card(
      full_screen = TRUE,
      card_body(
        plotOutput(outputId = "graph"),
        add_busy_bar(color = busy_bar_color, height = busy_bar_height)
      )
    )
  ),

  # Table page
  nav_panel(
    title = "Tabelas",

    accordion(
      multiple = FALSE,
      accordion_panel(
        title = "Alertas mais recentes",
        dataTableOutput(outputId = "table_recent"),
        add_busy_bar(color = busy_bar_color, height = busy_bar_height)
      ),
      accordion_panel(
        title = "Quantidade de alertas",
        dataTableOutput(outputId = "table_qtd"),
        add_busy_bar(color = busy_bar_color, height = busy_bar_height)
      ),
      accordion_panel(
        title = "Duração de alertas (dias)",
        dataTableOutput(outputId = "table_duration"),
        add_busy_bar(color = busy_bar_color, height = busy_bar_height)
      )
    )
  ),

  # Map page
  nav_panel(
    title = "Mapa",

    card(
      full_screen = TRUE,
      card_body(
        tmapOutput(outputId = "map"),
        add_busy_bar(color = busy_bar_color, height = busy_bar_height)
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

    name <- mun |>
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
      geom_segment(size = 25) +
      labs(
        title = paste(
          "Alertas meteorológicos emitidos pelo INMET -",
          input$sel_year
        ),
        subtitle = name$name_muni[1],
        x = "Duração",
        y = NULL,
        caption = "Elaboração: Observatório de Clima e Saúde/LIS/ICICT/Fiocruz"
      ) +
      theme_bw() +
      theme(legend.position = "none", text = element_text(size = 12))
  })

  output$table_recent <- renderDT({
    res <- ds |>
      mutate(sent = as_datetime(sent)) |>
      group_by(identifier, event, sent) |>
      summarise(freq = n()) |>
      ungroup() |>
      arrange(desc(sent)) |>
      rename(
        `Identificador` = identifier,
        `Evento` = event,
        `Envio` = sent,
        `Municípios afetados` = freq
      )

    datatable(data = res, rownames = FALSE, options = list(dom = 'ftp'))
  })

  output$table_qtd <- renderDT({
    res <- ds |>
      mutate(mun_codes = as.numeric(mun_codes)) |>
      left_join(mun, by = c("mun_codes" = "code_muni")) |>
      mutate(year = year(onset)) |>
      group_by(year, name_muni, event) |>
      summarise(freq = n()) |>
      ungroup() |>
      pivot_wider(names_from = event, values_from = freq) |>
      mutate(year = as.character(year)) |>
      mutate(
        `Total` = as.integer(rowSums(across(where(is.numeric)), na.rm = TRUE))
      ) |>
      arrange(-`Total`) |>
      rename(`Ano` = year, `Município` = name_muni)

    datatable(data = res, rownames = FALSE, options = list(dom = 'ftp'))
  })

  output$table_duration <- renderDT({
    res <- ds |>
      mutate(mun_codes = as.numeric(mun_codes)) |>
      left_join(mun, by = c("mun_codes" = "code_muni")) |>
      mutate(year = year(onset)) |>
      group_by(year, name_muni, event) |>
      summarise(
        dur = round(
          sum(as.numeric(expires - onset, "days"), na.rm = TRUE),
          digits = 0
        )
      ) |>
      ungroup() |>
      pivot_wider(names_from = event, values_from = dur) |>
      mutate(year = as.character(year)) |>
      mutate(
        `Total` = as.integer(rowSums(across(where(is.numeric)), na.rm = TRUE))
      ) |>
      arrange(-`Total`) |>
      rename(`Ano` = year, `Município` = name_muni)

    datatable(data = res, rownames = FALSE, options = list(dom = 'ftp'))
  })

  output$map <- renderTmap({
    req(input$sel_year)

    res <- ds |>
      mutate(year = year(onset)) |>
      filter(year == input$sel_year) |>
      mutate(mun_codes = as.numeric(mun_codes)) |>
      group_by(year, mun_codes, event) |>
      summarise(freq = n()) |>
      ungroup() |>
      pivot_wider(names_from = event, values_from = freq) |>
      mutate(across(everything(), ~ replace_na(.x, 0))) |>
      pivot_longer(cols = 3:9)

    res <- left_join(br.shp, res, by = c("code_muni" = "mun_codes")) |>
      na.omit()

    tm_shape(res) +
      tm_title(paste(
        "Alertas meteorológicos emitidos pelo INMET -",
        input$sel_year
      )) +
      tm_polygons(
        fill = "value",
        lwd = 0,
        fill.free = TRUE,
        fill.legend = tm_legend(show = FALSE),
        fill.scale = tm_scale_intervals(values = "brewer.or_rd")
      ) +
      tm_facets(by = "name")
  })
}

shinyApp(ui, server)
