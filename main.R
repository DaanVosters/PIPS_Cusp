# app.R
# Install once: install.packages(c("shiny", "plotly"))
# R >= 4.1. Save as app.R and click Run App in RStudio.
# No custom JavaScript; standard R Shiny/Plotly APIs only.
library(shiny)
library(plotly)

normal_limits <- c(-2.5, 2.5)
splitting_limits <- c(-1, 3)
frame_ms <- 40
max_control_step <- 0.005
default_labels <- list(a = "Normal axis (a)", b = "Splitting axis (b)", response = "Response (x)")
# Model: V(x;a,b) = x^4/4 - b*x^2/2 - a*x.
# Equilibrium: x^3 - b*x - a = 0. Stability: 3*x^2 - b > 0.
# Plotly API fields map as: spatial x <- a, spatial y <- b, spatial z <- x.
random_colour <- function() hsv(runif(1), runif(1, 0.65, 0.95), runif(1, 0.65, 0.90))

# Reuse the lowest available positive person number.
next_available_number <- function(ids) {
  used <- as.integer(ids)
  number <- 1L
  while (number %in% used) number <- number+1L
  number
}
# Sort only the dropdown choices, NOT the stored rows. Stored row order
# must stay aligned with Plotly's body/head/anchor trace order.
person_choices <- function(records) {
  index <- order(as.integer(records$id))
  setNames(records$id[index], records$name[index])
}

real_roots <- function(a, b) {
  roots <- polyroot(c(-a, -b, 0, 1))
  sort(Re(roots[abs(Im(roots)) < 1e-6]))
}
choose_equilibrium <- function(a, b, previous_x) {
  roots <- real_roots(a, b)
  candidates <- roots[3*roots^2-b >= -1e-7]
  if (!length(candidates)) candidates <- roots
  candidates[which.min(abs(candidates-previous_x))]
}
x_grid <- seq(-2.6, 2.6, length.out = 181)
b_grid <- seq(-1, 3, length.out = 111)
A <- outer(x_grid, b_grid, function(x, b) x^3-b*x)
B <- matrix(rep(b_grid, each = length(x_grid)), nrow = length(x_grid))
X <- matrix(rep(x_grid, times = length(b_grid)), nrow = length(x_grid))
stability <- 3*X^2-B
X_stable <- X
X_unstable <- X
X_stable[stability < 0 | abs(A) > 3.5] <- NA_real_
X_unstable[stability >= 0 | abs(A) > 3.5] <- NA_real_
fold_x <- seq(-1, 1, length.out = 300)
fold_a <- -2*fold_x^3
fold_b <- 3*fold_x^2
figure_geometry <- function(a, b, x) {
  h <- 0.30
  w <- 0.09
  list(a = c(a, a, NA, a-w, a+w, NA, a, a-w, NA, a, a+w), b = rep(b, 11),
       x = c(x+h*0.82, x+h*0.35, NA, x+h*0.62, x+h*0.62,
             NA, x+h*0.35, x, NA, x+h*0.35, x), head_x = x+h)
}
hover_coordinates <- function(labels) {
  safe <- lapply(labels, function(s) as.character(htmltools::htmlEscape(s)))
  paste0(safe$a, ": %{x:.3f}<br>", safe$b, ": %{y:.3f}<br>", safe$response, ": %{z:.3f}")
}
anchor_hover <- function(name, labels) {
  paste0(as.character(htmltools::htmlEscape(name)), "<br>", hover_coordinates(labels), "<extra></extra>")
}
person_traces <- function(person, position, selected = FALSE, labels = default_labels) {
  a <- position$a
  b <- position$b
  x <- position$x
  g <- figure_geometry(a, b, x)
  list(
    list(type = "scatter3d", mode = "lines", x = g$a, y = g$b, z = g$x,
         line = list(color = person$color, width = if (selected) 8 else 5),
         name = person$name, showlegend = FALSE, hoverinfo = "skip"),
    list(type = "scatter3d", mode = "markers+text", x = list(a), y = list(b), z = list(g$head_x),
         marker = list(color = person$color, size = if (selected) 7 else 5),
         text = list(person$name), textposition = "top center",
         textfont = list(color = person$color, size = 12), name = person$name,
         showlegend = FALSE, hovertemplate = paste0(person$name, "<extra></extra>")),
    list(type = "scatter3d", mode = "markers", x = list(a), y = list(b), z = list(x),
         marker = list(color = person$color, size = 4), name = person$name,
         showlegend = FALSE, hovertemplate = anchor_hover(person$name, labels)))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("body { background: #f8fafc; color: #172033; }
    .well { background: white; border-color: #e2e8f0; }
    .note { color: #526176; font-size: 13px; }"))),
  titlePanel("People on a cusp catastrophe"),
  sidebarLayout(
    sidebarPanel(
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
              p(class = "note", "Model: V(x; a, b) = x⁴/4 − bx²/2 − ax. Equilibrium: x³ − bx − a = 0. Stability: 3x² − b > 0. Motion uses quasi-static stable-branch continuation, not physical time-domain dynamics."))))

server <- function(input, output, session) {
  initial_people <- data.frame(id = "1", name = "Person 1", a = -1, b = 2, color = random_colour(), stringsAsFactors = FALSE)
  initial_positions <- data.frame(id = "1", a = -1, b = 2, x = choose_equilibrium(-1, 2, -2), stringsAsFactors = FALSE)
  people <- reactiveVal(initial_people)
  positions <- reactiveVal(initial_positions)
  available_ids <- reactiveVal("1") # Changes only on addition/removal, not target updates.
  person_revision <- reactiveVal(0L)
  control_generation <- reactiveVal(0L)
  last_controls <- reactiveVal(NULL)
  last_tick <- Sys.time()
  selected_ids <- reactive({
    ids <- input$person
    if (is.null(ids)) return(character())
    ids <- as.character(ids)
    ids[ids %in% available_ids()]
  })
  label_or_default <- function(value, fallback) {
    if (is.null(value) || !nzchar(trimws(value))) fallback else trimws(value)
  }
  axis_labels <- debounce(reactive({
    list(a = label_or_default(input$axis_normal, default_labels$a),
         b = label_or_default(input$axis_splitting, default_labels$b),
         response = label_or_default(input$axis_response, default_labels$response))
  }), 250)
  observeEvent(selected_ids(), {
    control_generation(isolate(control_generation())+1L)
  }, ignoreInit = TRUE, priority = 100)
  control_setup <- reactive({
    ids <- selected_ids()
    generation <- control_generation()
    req(length(ids) > 0)
    records <- isolate(people())
    ref <- records[match(ids[1], records$id), , drop = FALSE]
    req(nrow(ref) == 1, !is.na(ref$id))
    list(generation = generation, a_id = paste0("normal_", generation),
         b_id = paste0("splitting_", generation), a = ref$a, b = ref$b)
  })
  output$controls <- renderUI({
    if (!length(selected_ids())) return(p("Select at least one person to show the sliders."))
    setup <- control_setup()
    labels <- isolate(axis_labels())
    tagList(
      sliderInput(setup$a_id, paste0(labels$a, " — selected targets"), min = normal_limits[1], max = normal_limits[2], value = setup$a, step = 0.01),
      sliderInput(setup$b_id, paste0(labels$b, " — selected targets"), min = splitting_limits[1], max = splitting_limits[2], value = setup$b, step = 0.01))
  })
  output$selection_note <- renderUI({
    ids <- selected_ids()
    if (!length(ids)) return(NULL)
    records <- people()
    ref <- records[match(ids[1], records$id), , drop = FALSE]
    p(class = "note", paste0(length(ids), " selected. On selection, sliders load ", ref$name,
                             "'s targets. Moving one slider assigns its value to everyone selected; their other target remains unchanged. Selection alone never moves people."))
  })
  observeEvent(list(axis_labels(), control_setup()), {
    labels <- axis_labels()
    setup <- control_setup()
    updateSliderInput(session, setup$a_id, label = paste0(labels$a, " — selected targets"))
    updateSliderInput(session, setup$b_id, label = paste0(labels$b, " — selected targets"))
  })
  observe({
    setup <- control_setup()
    a <- input[[setup$a_id]]
    b <- input[[setup$b_id]]
    req(!is.null(a), !is.null(b))
    previous <- isolate(last_controls())
    if (is.null(previous) || previous$generation != setup$generation) previous <- list(generation = setup$generation, a = setup$a, b = setup$b)
    changed_a <- !isTRUE(all.equal(a, previous$a))
    changed_b <- !isTRUE(all.equal(b, previous$b))
    last_controls(list(generation = setup$generation, a = a, b = b))
    if (!changed_a && !changed_b) return()
    records <- isolate(people())
    chosen <- records$id %in% isolate(selected_ids())
    if (changed_a) records$a[chosen] <- a
    if (changed_b) records$b[chosen] <- b
    people(records)
  })
  observeEvent(input$select_all, updateSelectInput(session, "person", selected = sort(as.integer(available_ids()))))
  observeEvent(input$select_none, updateSelectInput(session, "person", selected = character()))
  observeEvent(input$add_person, {
    records <- people()
    displayed <- positions()
    ids <- selected_ids()
    # Reuse a gap in the current numbering instead of a lifetime counter.
    id <- as.character(next_available_number(records$id))
    if (nrow(records)) {
      source_id <- if (length(ids)) ids[1] else records$id[1]
      source <- records[match(source_id, records$id), , drop = FALSE]
      start <- displayed[match(source_id, displayed$id), , drop = FALSE]
    } else {
      source <- initial_people
      start <- initial_positions
    }
    source$id <- id
    source$name <- paste("Person", id)
    source$color <- random_colour()
    start$id <- id
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, "addTraces", person_traces(source, start, labels = axis_labels()))
    # Append stored rows to match appended traces; sort the dropdown only.
    records <- rbind(records, source)
    people(records)
    positions(rbind(displayed, start))
    available_ids(records$id)
    person_revision(person_revision()+1L)
    updateSelectInput(session, "person", choices = person_choices(records), selected = unique(c(ids, id)))
  })
  observeEvent(input$remove_people, {
    ids <- selected_ids()
    if (!length(ids)) {
      showNotification("Select at least one person to remove.", type = "message")
      return()
    }
    records <- people()
    displayed <- positions()
    rows <- which(records$id %in% ids)
    if (!length(rows)) return()
    # Zero-based trace indices: surfaces 0/1, folds 2, then body/head/anchor.
    trace_indices <- unlist(lapply(rows, function(i) {
      first <- 3L+3L*(i-1L)
      c(first, first+1L, first+2L)
    }), use.names = FALSE)
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    # Queue deletion before subsequent updates. Remaining trace order
    # automatically compacts to match the remaining stored rows.
    plotlyProxyInvoke(proxy, "deleteTraces", sort(trace_indices, decreasing = TRUE))
    records <- records[!(records$id %in% ids), , drop = FALSE]
    displayed <- displayed[!(displayed$id %in% ids), , drop = FALSE]
    people(records)
    positions(displayed)
    available_ids(records$id)
    person_revision(person_revision()+1L)
    updateSelectInput(session, "person", choices = person_choices(records), selected = character())
    showNotification(paste("Removed", length(ids), "selected people."), type = "message")
  })
  observeEvent(input$reset, {
    records <- people()
    if (!nrow(records)) return()
    records$a <- -1
    records$b <- 2
    people(records)
    control_generation(control_generation()+1L)
  })
  observeEvent(input$switch_branch, {
    ids <- selected_ids()
    if (!length(ids)) {
      showNotification("Select at least one person first.", type = "message")
      return()
    }
    current <- positions()
    switched <- 0L
    for (i in which(current$id %in% ids)) {
      roots <- real_roots(current$a[i], current$b[i])
      stable <- roots[3*roots^2-current$b[i] > 1e-7]
      if (length(stable) < 2) next
      current$x[i] <- stable[which.max(abs(stable-current$x[i]))]
      switched <- switched+1L
    }
    if (switched) positions(current)
    showNotification(paste("Switched", switched, "of", length(ids), "selected people."), type = "message")
  })
  observe({
    invalidateLater(frame_ms, session)
    now <- Sys.time()
    dt <- min(as.numeric(difftime(now, last_tick, units = "secs")), 0.15)
    last_tick <<- now
    speed <- isolate(input$speed)
    if (is.null(speed) || !is.finite(speed) || speed <= 0 || dt <= 0) return()
    target <- isolate(people())
    current <- isolate(positions())
    changed <- FALSE
    for (i in seq_len(nrow(current))) {
      j <- match(current$id[i], target$id)
      if (is.na(j)) next
      from <- c(current$a[i], current$b[i])
      to <- c(target$a[j], target$b[j])
      delta <- to-from
      distance <- sqrt(sum(delta^2))
      if (distance < 1e-10) next
      travel <- min(distance, speed*dt)
      endpoint <- if (travel >= distance) to else from+(travel/distance)*delta
      n_steps <- max(1L, as.integer(ceiling(travel/max_control_step)))
      x <- current$x[i]
      for (k in seq_len(n_steps)) {
        controls <- from+(k/n_steps)*(endpoint-from)
        x <- choose_equilibrium(controls[1], controls[2], x)
      }
      current$a[i] <- endpoint[1]
      current$b[i] <- endpoint[2]
      current$x[i] <- x
      changed <- TRUE
    }
    if (changed) positions(current)
  })
  output$coordinates <- renderText({
    ids <- selected_ids()
    if (!length(ids)) return("No people selected.")
    records <- people()
    current <- positions()
    labels <- axis_labels()
    blocks <- lapply(ids, function(id) {
      i <- match(id, records$id)
      j <- match(id, current$id)
      if (is.na(i) || is.na(j)) return(NULL)
      paste0(records$name[i], "\n", labels$a, ": ", sprintf("%.3f", current$a[j]),
             " (target ", sprintf("%.2f", records$a[i]), ")\n", labels$b, ": ",
             sprintf("%.3f", current$b[j]), " (target ", sprintf("%.2f", records$b[i]), ")\n",
             labels$response, ": ", sprintf("%.3f", current$x[j]))
    })
    paste(unlist(blocks), collapse = "\n\n")
  })
  # Create once. All changes use proxy calls to preserve the camera.
  output$cusp <- renderPlotly({
    hover <- hover_coordinates(default_labels)
    fig <- plot_ly() |>
      add_surface(x = A, y = B, z = X_stable, name = "Stable equilibria", opacity = 0.62, showscale = FALSE,
                  colorscale = list(c(0, "#93c5fd"), c(1, "#93c5fd")), hovertemplate = paste0(hover, "<extra>Stable sheet</extra>")) |>
      add_surface(x = A, y = B, z = X_unstable, name = "Unstable equilibria", opacity = 0.28, showscale = FALSE,
                  colorscale = list(c(0, "#fdba74"), c(1, "#fdba74")), hovertemplate = paste0(hover, "<extra>Unstable sheet</extra>")) |>
      add_trace(x = fold_a, y = fold_b, z = fold_x, type = "scatter3d", mode = "lines", name = "Folds",
                line = list(color = "#334155", width = 5), hoverinfo = "skip") |>
      layout(uirevision = "keep-camera", showlegend = FALSE,
             scene = list(uirevision = "keep-camera",
                          xaxis = list(title = list(text = default_labels$a), range = c(-3.5, 3.5)),
                          yaxis = list(title = list(text = default_labels$b), range = c(-1, 3)),
                          zaxis = list(title = list(text = default_labels$response), range = c(-2.6, 3)),
                          aspectmode = "manual", aspectratio = list(x = 1.25, y = 1, z = 1),
                          camera = list(eye = list(x = 1.6, y = 1.6, z = 1.1))), margin = list(l = 0, r = 0, b = 0, t = 20)) |>
      config(displaylogo = FALSE)
    fig <- plotly_build(fig)
    fig$x$data <- c(fig$x$data, person_traces(initial_people, initial_positions, TRUE))
    fig
  })
  observeEvent(list(positions(), selected_ids()), {
    displayed <- positions()
    records <- isolate(people())
    req(nrow(displayed) == nrow(records))
    if (!nrow(records)) return()
    a_values <- list()
    b_values <- list()
    x_values <- list()
    indices <- integer()
    for (i in seq_len(nrow(records))) {
      j <- match(records$id[i], displayed$id)
      a <- displayed$a[j]
      b <- displayed$b[j]
      x <- displayed$x[j]
      g <- figure_geometry(a, b, x)
      a_values <- c(a_values, list(g$a, list(a), list(a)))
      b_values <- c(b_values, list(g$b, list(b), list(b)))
      x_values <- c(x_values, list(g$x, list(g$head_x), list(x)))
      first <- 3L+3L*(i-1L)
      indices <- c(indices, first, first+1L, first+2L)
    }
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, "restyle", list(x = a_values, y = b_values, z = x_values), indices)
  }, ignoreInit = TRUE)
  observeEvent(list(selected_ids(), person_revision()), {
    records <- isolate(people())
    if (!nrow(records)) return()
    body_indices <- 3L+3L*(seq_len(nrow(records))-1L)
    chosen <- records$id %in% selected_ids()
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, "restyle", list("line.width" = as.list(ifelse(chosen, 8, 5))), body_indices)
    plotlyProxyInvoke(proxy, "restyle", list("marker.size" = as.list(ifelse(chosen, 7, 5))), body_indices+1L)
  }, ignoreInit = TRUE)
  observeEvent(axis_labels(), {
    labels <- axis_labels()
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, "relayout", list("scene.xaxis.title.text" = labels$a,
                                              "scene.yaxis.title.text" = labels$b, "scene.zaxis.title.text" = labels$response))
    hover <- hover_coordinates(labels)
    plotlyProxyInvoke(proxy, "restyle", list(hovertemplate = list(paste0(hover, "<extra>Stable sheet</extra>"),
                                                                  paste0(hover, "<extra>Unstable sheet</extra>"))), c(0L, 1L))
    records <- isolate(people())
    if (!nrow(records)) return()
    plotlyProxyInvoke(proxy, "restyle", list(hovertemplate = lapply(records$name, anchor_hover, labels = labels)),
                      5L+3L*(seq_len(nrow(records))-1L))
  }, ignoreInit = TRUE)
}
shinyApp(ui, server)
