# appearance_module.R

appearanceUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    appearanceStyles(),
    
    shiny::strong("Appearance"),
    
    shiny::checkboxInput(
      ns("dark_mode"),
      "Dark mode",
      value = FALSE
    ),
    
    colourpicker::colourInput(
      ns("stable_colour"),
      "Stable equilibria color",
      value = "#93c5fd",
      allowTransparent = FALSE
    ),
    
    colourpicker::colourInput(
      ns("unstable_colour"),
      "Unstable equilibria color",
      value = "#fdba74",
      allowTransparent = FALSE
    ),
    
    shiny::hr()
  )
}


appearanceServer <- function(
    id,
    update_plot,
    stable_trace = 0L,
    unstable_trace = 1L,
    fold_trace = 2L
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    set_surface_colour <- function(colour, trace) {
      scale <- list(
        c(0, colour),
        c(1, colour)
      )
      
      update_plot(
        "restyle",
        list(colorscale = list(scale)),
        trace
      )
    }
    
    shiny::observeEvent(input$stable_colour, {
      shiny::req(input$stable_colour)
      
      set_surface_colour(
        input$stable_colour,
        stable_trace
      )
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$unstable_colour, {
      shiny::req(input$unstable_colour)
      
      set_surface_colour(
        input$unstable_colour,
        unstable_trace
      )
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$dark_mode, {
      shiny::req(!is.null(input$dark_mode))
      
      dark <- isTRUE(input$dark_mode)
      
      shinyjs::toggleClass(
        selector = "body",
        class = "dark-mode",
        condition = dark
      )
      
      palette <- if (dark) {
        list(
          page = "#0f172a",
          panel = "#1e293b",
          text = "#e2e8f0",
          grid = "#475569",
          zero = "#64748b",
          fold = "#f1f5f9"
        )
      } else {
        list(
          page = "#f8fafc",
          panel = "#ffffff",
          text = "#172033",
          grid = "#cbd5e1",
          zero = "#94a3b8",
          fold = "#334155"
        )
      }
      
      changes <- list(
        paper_bgcolor = palette$page,
        plot_bgcolor = palette$page,
        "scene.bgcolor" = palette$page,
        "font.color" = palette$text
      )
      
      for (axis in c("xaxis", "yaxis", "zaxis")) {
        prefix <- paste0("scene.", axis, ".")
        
        changes[[paste0(prefix, "color")]] <- palette$text
        changes[[paste0(prefix, "tickfont.color")]] <- palette$text
        changes[[paste0(prefix, "title.font.color")]] <- palette$text
        changes[[paste0(prefix, "gridcolor")]] <- palette$grid
        changes[[paste0(prefix, "zerolinecolor")]] <- palette$zero
        changes[[paste0(prefix, "backgroundcolor")]] <- palette$panel
        changes[[paste0(prefix, "showbackground")]] <- TRUE
      }
      
      update_plot("relayout", changes)
      
      update_plot(
        "restyle",
        list("line.color" = palette$fold),
        fold_trace
      )
    })
  })
}


appearanceStyles <- function() {
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
          body {
            --page: #f8fafc;
            --panel: #ffffff;
            --input: #ffffff;
            --text: #172033;
            --muted: #526176;
            --border: #cbd5e1;
            --active: #dbeafe;
            background: var(--page);
            color: var(--text);
            color-scheme: light;
          }

          body.dark-mode {
            --page: #0f172a;
            --panel: #1e293b;
            --input: #111827;
            --text: #e2e8f0;
            --muted: #b6c2d2;
            --border: #475569;
            --active: #334155;
            color-scheme: dark;
          }

          .well {
            background: var(--panel);
            border-color: var(--border);
          }

          .note {
            color: var(--muted);
            font-size: 13px;
          }

          hr { border-top-color: var(--border); }

          .form-control,
          .selectize-input,
          .selectize-input.full,
          .selectize-control.multi .selectize-input.has-items,
          .selectize-dropdown,
          pre {
            background: var(--input) !important;
            color: var(--text) !important;
            border-color: var(--border) !important;
          }

          .selectize-input input {
            color: var(--text) !important;
          }

          .selectize-control.multi .selectize-input > div,
          .selectize-dropdown .active {
            background: var(--active) !important;
            color: var(--text) !important;
            border-color: var(--border) !important;
          }

          .btn-default {
            background: var(--panel);
            color: var(--text);
            border-color: var(--border);
          }

          .btn-default:hover,
          .btn-default:focus,
          .btn-default:active {
            background: var(--active) !important;
            color: var(--text) !important;
            border-color: var(--border) !important;
          }

          .irs--shiny .irs-line {
            background: var(--border);
          }

          .irs--shiny .irs-min,
          .irs--shiny .irs-max {
            background: var(--active);
            color: var(--text);
          }

          .irs--shiny .irs-grid-text {
            color: var(--muted);
          }

          body.dark-mode .modebar-btn path {
            fill: #b6c2d2 !important;
          }

          body.dark-mode .shiny-notification {
            background: var(--panel);
            color: var(--text);
            border-color: var(--border);
          }
        ")
      )
    )
  )
}