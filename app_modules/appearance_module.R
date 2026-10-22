# appearance_module.R

appearanceUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    appearanceStyles(),
    
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::checkboxInput(
          ns("dark_mode"),
          "Dark mode",
          value = FALSE
        )
      ),
      
      shiny::column(
        width = 4,
        colourpicker::colourInput(
          ns("stable_colour"),
          "Stable equilibria color",
          value = "#93c5fd",
          allowTransparent = FALSE
        )
      ),
      
      shiny::column(
        width = 4,
        colourpicker::colourInput(
          ns("unstable_colour"),
          "Unstable equilibria color",
          value = "#fdba74",
          allowTransparent = FALSE
        )
      )
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

#----------re-loading parts from base/stats that need changes----------
#re-load ui
ui <- fluidPage(
  tags$head(tags$style(HTML("body { background: #f8fafc; color: #172033; }
    .well { background: white; border-color: #e2e8f0; }
    .note { color: #526176; font-size: 13px; }"))),
  titlePanel("Interactive Cusp Catastrophe Tool: Visualisation & Data-Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "person", "Person to move",
        choices = c("Select a person" = "", "Person 1" = "1"),
        selected = "1", multiple = FALSE
      ),
      uiOutput("controls"), uiOutput("selection_note"),
      actionButton("add_person", "Add person"),
      actionButton("remove_people", "Remove selected person", class = "btn-danger"),
      p(class = "note", "Removing a person permanently deletes them from the plot and list. Switching selection does not delete anyone. New people are assigned the lowest available number."),
      actionButton("reset", "Reset everyone's controls"), hr(),
      numericInput("speed", "Control movement speed (units / second)", value = 1, min = 0.1, max = 5, step = 0.1),
      strong("Selected person's position"), verbatimTextOutput("coordinates"),
      actionButton("switch_branch", "Switch selected person's branch"),
      p(class = "note", "Switching acts at the selected person's current controls when two stable equilibria exist. Other people remain unchanged."), hr(),
      strong("Rename axes"),
      textInput("axis_normal", "Normal-control axis name", default_labels$a),
      textInput("axis_splitting", "Splitting-control axis name", default_labels$b),
      textInput("axis_response", "Behavioural axis name", default_labels$response),
      p(class = "note", "Use this option to make your theory-driven visualisations easier to interpret. Changing names only changes the labels."), hr(),
      h4("We need to talk about the cusp…"),
      p(class = "note", "The surface of the cusp is divided into two sections: stable and unstable equilibria (in different colours). The latter is also referred to as the inaccessible region as people can only occupy these positions momentarily. The dark lines mark the fold."),
      p(class = "note", "At a fold each person jumps directly to the other stable branch. We hope that this app helps you visualise psychological phenomena whose dynamics theory implies is cusp-like. One such example is the cusp model of attitudes (van der Maas et al., 2003)."),
      strong("Reference"),
      p(class = "note", "van der Maas, H. L. J., Kolstein, R., & van der Pligt, J. (2003). Sudden transitions in attitudes. ",
        tags$em("Sociological Methods & Research, 32"), "(2), 125–152. ",
        tags$a(href = "https://doi.org/10.1177/0049124103253773",
               "https://doi.org/10.1177/0049124103253773", target = "_blank"))),
    mainPanel(
      appearanceUI("appearance"),
      plotlyOutput("cusp", height = "720px"),
      p(class = "note", "Model: V(x; a, b) = x⁴/4 − bx²/2 − ax. Equilibrium: x³ − bx − a = 0. Stability: 3x² − b > 0. Motion uses quasi-static stable-branch continuation, not physical time-domain dynamics."),
      dataAnalysisUI("analysis"))))
#end


#mysterious AI code piece
server <- local({
  base_server <- server
  
  function(input, output, session) {
    # Run the existing app logic unchanged
    base_server(input, output, session)
    
    # Update the existing cusp plot
    update_plot <- function(method, ...) {
      proxy <- plotly::plotlyProxy(
        "cusp",
        session,
        deferUntilFlush = TRUE
      )
      
      plotly::plotlyProxyInvoke(proxy, method, ...)
    }
    
    appearanceServer(
      "appearance",
      update_plot = update_plot
    )
  }
})
#end
#----------end----------
