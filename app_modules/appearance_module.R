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

#re-load ui
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("People on a cusp catastrophe"),
  sidebarLayout(
    sidebarPanel(
      appearanceUI("appearance"),
      selectInput("person", "People to move", choices = c("Person 1" = "1"), selected = "1", multiple = TRUE),
      actionButton("select_all", "Select all"), actionButton("select_none", "Select none"),
      uiOutput("controls"), uiOutput("selection_note"),
      actionButton("add_person", "Add person"),
      actionButton("remove_people", "Remove selected people", class = "btn-danger"),
      p(class = "note", "Remove deletes everyone selected. New people reuse the lowest available number. If everyone is removed, the next person is Person 1."),
      actionButton("reset", "Reset everyone's controls"), hr(),
      numericInput("speed", "Control movement speed (units / second)", value = 1, min = 0.1, max = 5, step = 0.1),
      p(class = "note", "Sliders set targets for everyone selected. Current controls approach their targets in small increments, solving the equilibrium each time. Speed is constant in the control plane, not along the curved surface."),
      strong("Selected people's positions"), verbatimTextOutput("coordinates"),
      actionButton("switch_branch", "Switch selected people's branches"),
      p(class = "note", "Switching acts at each person's current controls when two stable equilibria exist. Other people remain unchanged."), hr(),
      strong("Rename axes"),
      textInput("axis_normal", "Normal-control axis name", default_labels$a),
      textInput("axis_splitting", "Splitting-control axis name", default_labels$b),
      textInput("axis_response", "Behavioural axis name", default_labels$response),
      p(class = "note", "Names update automatically. Blank names use defaults. Renaming changes labels only, not mathematics or camera."), hr(),
      p("Blue: stable equilibria. Orange: unstable equilibria. Dark curves: folds."),
      p(class = "note", "The central foot marker follows the surface; the figure is decorative. At a fold each person jumps directly to the other stable branch, retaining their own branch history."),
      p(class = "note", "Drag the plot to rotate; scroll to zoom. Adding/removing people preserves the camera. Colours are random. Deselecting does not cancel motion toward an assigned target."),
      p(class = "note", "For hysteresis, set splitting to 2, then normal to 2. Let people arrive, then return normal to -2.")),
    mainPanel(plotlyOutput("cusp", height = "720px"),
              p(class = "note", "Model: V(x; a, b) = x⁴/4 − bx²/2 − ax. Equilibrium: x³ − bx − a = 0. Stability: 3x² − b > 0. Motion uses quasi-static stable-branch continuation, not physical time-domain dynamics."),
              dataAnalysisUI("analysis"))))
#end
