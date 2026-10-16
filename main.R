library(shiny)
library(plotly)


normal_limits <- c(-2.5, 2.5)
splitting_limits <- c(-1, 3) #changes the in-app sliders but not the 3d grid
frame_ms <- 35 #should change the plot framerate according to miss AI. 35 seems to look slightly smoother than 40 for me while 30 makes it worse.
max_control_step <- 0.005 #should have something to do with the tracking over the surface according to miss AI
default_labels <- list(a = "Normal axis (a)", b = "Splitting axis (b)", response = "Response (x)")


#picks a random, fully saturated and maximally bright, color.
random_colour <- function() hsv(runif(1), 1, 1)

# Find the next lowest available person number (important for after having deleted persons).
next_available_number <- function(ids) {
  used <- as.integer(ids)
  number <- 1L
  while (number %in% used) number <- number+1L
  return(number)
}

# Function for creating the person selection list; attaching the right name to the right underlying ID.
person_choices <- function(records) {
  index <- order(as.integer(records$id))
  return(setNames(records$id[index], records$name[index]))
}

# For finding the equilibria of the cusp, for a given a and b parameters.
real_roots <- function(a, b) {
  roots <- polyroot(c(-a, -b, 0, 1)) #finds roots of the first derivative
  return(sort(Re(roots[abs(Im(roots)) < 1e-6]))) #Imaginary part need to stay otherwise more roots are found (I think)
}

# Returns next position at a stable equilibrium for a person
choose_equilibrium <- function(a, b, previous_x) {
  roots <- real_roots(a, b)
  candidates <- roots[3*roots^2-b >= -1e-7] #if second derivative at roots is bigger than 0...; slightly negative to allow for rounding error apparently.
  return(candidates[which.min(abs(candidates - previous_x))])
}



x_grid <- seq(-2.6, 2.6, length.out = 181)
b_grid <- seq(-1, 3, length.out = 111) #check length.out values here and above; might be arbitrary.
A <- outer(x_grid, b_grid, function(x, b) x^3-b*x) #creates a matrix of the slope at x for a value of b, with a = 0. Perhaps this is for finding the required A to make the first derivative 0?
B <- matrix(rep(b_grid, each = length(x_grid)), nrow = length(x_grid))
X <- matrix(rep(x_grid, times = length(b_grid)), nrow = length(x_grid))
stability <- 3*X^2-B #second derivative of the cusp function; checks whether a certain x value is stable or unstable for a given b.
X_stable <- X
X_unstable <- X
X_stable[stability < 0 | abs(A) > 3.5] <- NA_real_ #why abs(A) here though and on the next line? also shouldn't stability be > 0 for a stable equilibrium?
X_unstable[stability >= 0 | abs(A) > 3.5] <- NA_real_ #should this be > instead of >=?

#for creating black line separating stable from unstable area in plot:
fold_x <- seq(-1, 1, length.out = 300)
fold_a <- -2*fold_x^3
fold_b <- 3*fold_x^2
#end

           
#Next three functions perhaps for the plotly part?
figure_geometry <- function(a, b, x) {
  h <- 0.30
  w <- 0.09
  return(list(a = c(a, a, NA, a-w, a+w, NA, a, a-w, NA, a, a+w), b = rep(b, 11),
       x = c(x+h*0.82, x+h*0.35, NA, x+h*0.62, x+h*0.62,
             NA, x+h*0.35, x, NA, x+h*0.35, x), head_x = x+h))
}       
hover_coordinates <- function(labels) {
  safe <- lapply(labels, function(s) as.character(htmltools::htmlEscape(s)))
  return(paste0(safe$a, ": %{x:.3f}<br>", safe$b, ": %{y:.3f}<br>", safe$response, ": %{z:.3f}"))
}            
anchor_hover <- function(name, labels) {
  return(paste0(as.character(htmltools::htmlEscape(name)), "<br>", hover_coordinates(labels), "<extra></extra>"))
}
#end of mysterious three functions

                 
#t.b.d. what this does                
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
#end


#text editing part for the app I believe                     
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
#end

                     
#the most mysterious function the world has ever seen 
server <- function(input, output, session) {
  
  # ====================================================================
  # 1. State: target controls, current positions, and selected people
  # ====================================================================
  
  initial_targets <- data.frame(
    id = "1", name = "Person 1", a = -1, b = 2,
    color = random_colour(), stringsAsFactors = FALSE
  )
  initial_positions <- data.frame(
    id = "1", a = -1, b = 2,
    x = choose_equilibrium(-1, 2, -2), stringsAsFactors = FALSE
  )
  
  # Both tables stay in Plotly trace order, NOT numerical person-ID order.
  # Only dropdown choices are sorted numerically.
  person_targets <- reactiveVal(initial_targets)
  person_positions <- reactiveVal(initial_positions)
  person_ids <- reactiveVal("1")
  selected_people <- reactiveVal("1")
  
  # Fresh slider IDs prevent old input values affecting a new selection.
  controls_version <- reactiveVal(0L)
  previous_slider_values <- reactiveVal(NULL)
  previous_update_time <- Sys.time()
  
  # ====================================================================
  # 2. Small helpers
  # ====================================================================
  
  # Every plot change is made in place, preserving the camera.
  update_plot <- function(method, ...) {
    proxy <- plotlyProxy("cusp", session, deferUntilFlush = TRUE)
    plotlyProxyInvoke(proxy, method, ...)
  }
  
  # Plotly uses zero-based indices. The first three traces are the surface
  # sheets and folds; each subsequent person has body, head, and anchor.
  body_indices <- function(rows) 3L + 3L * (rows - 1L)
  
  all_person_trace_indices <- function(rows) {
    first <- body_indices(rows)
    as.vector(rbind(first, first + 1L, first + 2L))
  }
  
  refresh_person_choices <- function(targets, selected) {
    updateSelectInput(
      session, "person",
      choices = person_choices(targets), selected = selected
    )
  }
  
  refresh_controls <- function() {
    controls_version(isolate(controls_version()) + 1L)
  }
  
  # Blank axis labels fall back to the standard model labels.
  use_default_if_blank <- function(value, default) {
    if (is.null(value) || !nzchar(trimws(value))) default else trimws(value)
  }
  
  axis_labels <- debounce(reactive({
    list(
      a = use_default_if_blank(input$axis_normal, default_labels$a),
      b = use_default_if_blank(input$axis_splitting, default_labels$b),
      response = use_default_if_blank(input$axis_response, default_labels$response)
    )
  }), 250)
  
  # Advance one person toward their target. Intermediate response values
  # are solved internally; only the endpoint is sent to the plot.
  advance_person <- function(a, b, x, target_a, target_b, travel, distance) {
    start <- c(a, b)
    target <- c(target_a, target_b)
    
    endpoint <- if (travel >= distance) {
      target
    } else {
      start + (travel / distance) * (target - start)
    }
    
    number_of_steps <- max(1L, as.integer(ceiling(travel / max_control_step)))
    
    for (step in seq_len(number_of_steps)) {
      controls <- start + (step / number_of_steps) * (endpoint - start)
      x <- choose_equilibrium(controls[1], controls[2], x)
    }
    
    list(a = endpoint[1], b = endpoint[2], x = x)
  }
  
  # ====================================================================
  # 3. Selection
  # ====================================================================
  
  # Observe membership only, not target changes. reactiveVal also avoids
  # refreshing controls if an input event leaves the selection unchanged.
  observeEvent(list(input$person, person_ids()), {
    requested_ids <- as.character(input$person)
    valid_ids <- requested_ids[requested_ids %in% person_ids()]
    
    if (!identical(valid_ids, selected_people())) {
      selected_people(valid_ids)
      refresh_controls()
    }
  }, priority = 100)
  
  observeEvent(input$select_all, {
    selected <- as.character(sort(as.integer(person_ids())))
    updateSelectInput(session, "person", selected = selected)
  })
  
  observeEvent(input$select_none, {
    updateSelectInput(session, "person", selected = character())
  })
  
  # ====================================================================
  # 4. Shared sliders
  # ====================================================================
  
  # On selection/reset, take initial slider values from the first selected
  # person. Merely loading these values must NOT change other targets.
  slider_setup <- reactive({
    selected <- selected_people()
    version <- controls_version()
    req(length(selected) > 0)
    
    targets <- isolate(person_targets())
    reference_row <- match(selected[1], targets$id)
    req(!is.na(reference_row))
    
    list(
      version = version,
      a_input = paste0("normal_", version),
      b_input = paste0("splitting_", version),
      initial_a = targets$a[reference_row],
      initial_b = targets$b[reference_row]
    )
  })
  
  output$controls <- renderUI({
    if (!length(selected_people())) {
      return(p("Select at least one person to show the sliders."))
    }
    
    setup <- slider_setup()
    labels <- isolate(axis_labels())
    
    tagList(
      sliderInput(
        setup$a_input, paste0(labels$a, " — selected targets"),
        min = normal_limits[1], max = normal_limits[2],
        value = setup$initial_a, step = 0.01
      ),
      sliderInput(
        setup$b_input, paste0(labels$b, " — selected targets"),
        min = splitting_limits[1], max = splitting_limits[2],
        value = setup$initial_b, step = 0.01
      )
    )
  })
  
  output$selection_note <- renderUI({
    selected <- selected_people()
    if (!length(selected)) return(NULL)
    
    targets <- isolate(person_targets())
    reference_name <- targets$name[match(selected[1], targets$id)]
    
    p(class = "note", paste0(
      length(selected), " selected. On selection, sliders load ",
      reference_name, "'s targets. Moving one slider assigns its value to ",
      "everyone selected; their other target remains unchanged. ",
      "Selection alone never moves people."
    ))
  })
  
  # Rename existing sliders without resetting their current values.
  observeEvent(list(axis_labels(), slider_setup()), {
    labels <- axis_labels()
    setup <- slider_setup()
    
    updateSliderInput(session, setup$a_input,
                      label = paste0(labels$a, " — selected targets"))
    updateSliderInput(session, setup$b_input,
                      label = paste0(labels$b, " — selected targets"))
  })
  
  observe({
    setup <- slider_setup()
    new_a <- input[[setup$a_input]]
    new_b <- input[[setup$b_input]]
    req(!is.null(new_a), !is.null(new_b))
    
    previous <- isolate(previous_slider_values())
    
    if (is.null(previous) || previous$version != setup$version) {
      previous <- list(
        version = setup$version,
        a = setup$initial_a, b = setup$initial_b
      )
    }
    
    a_changed <- !isTRUE(all.equal(new_a, previous$a))
    b_changed <- !isTRUE(all.equal(new_b, previous$b))
    
    previous_slider_values(list(version = setup$version, a = new_a, b = new_b))
    if (!a_changed && !b_changed) return()
    
    targets <- isolate(person_targets())
    selected_rows <- targets$id %in% isolate(selected_people())
    
    if (a_changed) targets$a[selected_rows] <- new_a
    if (b_changed) targets$b[selected_rows] <- new_b
    
    person_targets(targets)
  })
  
  # ====================================================================
  # 5. Add, remove, reset, and switch branches
  # ====================================================================
  
  observeEvent(input$add_person, {
    targets <- person_targets()
    positions <- person_positions()
    selected <- selected_people()
    new_id <- as.character(next_available_number(targets$id))
    
    if (nrow(targets) > 0) {
      reference_id <- if (length(selected)) selected[1] else targets$id[1]
      new_target <- targets[match(reference_id, targets$id), , drop = FALSE]
      new_position <- positions[match(reference_id, positions$id), , drop = FALSE]
    } else {
      new_target <- initial_targets
      new_position <- initial_positions
    }
    
    new_target$id <- new_id
    new_target$name <- paste("Person", new_id)
    new_target$color <- random_colour()
    new_position$id <- new_id
    
    # Queue trace addition before any updates involving its new indices.
    update_plot("addTraces", person_traces(
      new_target, new_position, labels = axis_labels()
    ))
    
    targets <- rbind(targets, new_target)
    person_targets(targets)
    person_positions(rbind(positions, new_position))
    person_ids(targets$id)
    
    refresh_person_choices(targets, unique(c(selected, new_id)))
  })
  
  observeEvent(input$remove_people, {
    selected <- selected_people()
    if (!length(selected)) {
      showNotification("Select at least one person to remove.", type = "message")
      return()
    }
    
    targets <- person_targets()
    positions <- person_positions()
    removed_rows <- which(targets$id %in% selected)
    if (!length(removed_rows)) return()
    
    # Delete all three traces for each removed person. Surviving traces
    # compact automatically, matching the order of the remaining rows.
    removed_traces <- all_person_trace_indices(removed_rows)
    update_plot("deleteTraces", sort(removed_traces, decreasing = TRUE))
    
    targets <- targets[!(targets$id %in% selected), , drop = FALSE]
    positions <- positions[!(positions$id %in% selected), , drop = FALSE]
    
    person_targets(targets)
    person_positions(positions)
    person_ids(targets$id)
    refresh_person_choices(targets, character())
    
    showNotification(paste("Removed", length(removed_rows), "selected people."),
                     type = "message")
  })
  
  observeEvent(input$reset, {
    targets <- person_targets()
    if (!nrow(targets)) return()
    
    targets$a <- -1
    targets$b <- 2
    person_targets(targets)
    refresh_controls()
  })
  
  observeEvent(input$switch_branch, {
    selected <- selected_people()
    if (!length(selected)) {
      showNotification("Select at least one person first.", type = "message")
      return()
    }
    
    positions <- person_positions()
    switched <- 0L
    
    for (row in which(positions$id %in% selected)) {
      roots <- real_roots(positions$a[row], positions$b[row])
      stable_roots <- roots[3 * roots^2 - positions$b[row] > 1e-7]
      if (length(stable_roots) < 2) next
      
      furthest_root <- which.max(abs(stable_roots - positions$x[row]))
      positions$x[row] <- stable_roots[furthest_root]
      switched <- switched + 1L
    }
    
    if (switched > 0) person_positions(positions)
    
    showNotification(paste("Switched", switched, "of", length(selected),
                           "selected people."), type = "message")
  })
  
  # ====================================================================
  # 6. Movement timer
  # ====================================================================
  
  observe({
    invalidateLater(frame_ms, session)
    
    now <- Sys.time()
    elapsed_seconds <- as.numeric(difftime(now, previous_update_time, units = "secs"))
    previous_update_time <<- now
    
    # Retain the previous app's safeguard against very large delayed steps.
    elapsed_seconds <- min(elapsed_seconds, 0.15)
    speed <- isolate(input$speed)
    
    if (is.null(speed) || !is.finite(speed) || speed <= 0 || elapsed_seconds <= 0) return()
    
    targets <- isolate(person_targets())
    positions <- isolate(person_positions())
    if (!nrow(positions)) return()
    
    # The tables share row order. Calculate distances for everyone at once,
    # then solve equilibria ONLY for people who still need to move.
    difference_a <- targets$a - positions$a
    difference_b <- targets$b - positions$b
    distances <- sqrt(difference_a^2 + difference_b^2)
    moving_rows <- which(distances >= 1e-10)
    if (!length(moving_rows)) return()
    
    for (row in moving_rows) {
      travel <- min(distances[row], speed * elapsed_seconds)
      
      next_position <- advance_person(
        a = positions$a[row], b = positions$b[row], x = positions$x[row],
        target_a = targets$a[row], target_b = targets$b[row],
        travel = travel, distance = distances[row]
      )
      
      positions$a[row] <- next_position$a
      positions$b[row] <- next_position$b
      positions$x[row] <- next_position$x
    }
    
    person_positions(positions)
  })
  
  # ====================================================================
  # 7. Position readout
  # ====================================================================
  
  output$coordinates <- renderText({
    selected <- selected_people()
    if (!length(selected)) return("No people selected.")
    
    targets <- person_targets()
    positions <- person_positions()
    labels <- axis_labels()
    
    blocks <- lapply(selected, function(id) {
      target_row <- match(id, targets$id)
      position_row <- match(id, positions$id)
      if (is.na(target_row) || is.na(position_row)) return(NULL)
      
      paste0(
        targets$name[target_row], "\n",
        labels$a, ": ", sprintf("%.3f", positions$a[position_row]),
        " (target ", sprintf("%.2f", targets$a[target_row]), ")\n",
        labels$b, ": ", sprintf("%.3f", positions$b[position_row]),
        " (target ", sprintf("%.2f", targets$b[target_row]), ")\n",
        labels$response, ": ", sprintf("%.3f", positions$x[position_row])
      )
    })
    
    paste(unlist(blocks), collapse = "\n\n")
  })
  
  # ====================================================================
  # 8. Initial plot: rendered once
  # ====================================================================
  
  output$cusp <- renderPlotly({
    hover <- hover_coordinates(default_labels)
    
    plot <- plot_ly() |>
      add_surface(
        x = A, y = B, z = X_stable, name = "Stable equilibria",
        opacity = 0.62, showscale = FALSE,
        colorscale = list(c(0, "#93c5fd"), c(1, "#93c5fd")),
        hovertemplate = paste0(hover, "<extra>Stable sheet</extra>")
      ) |>
      add_surface(
        x = A, y = B, z = X_unstable, name = "Unstable equilibria",
        opacity = 0.28, showscale = FALSE,
        colorscale = list(c(0, "#fdba74"), c(1, "#fdba74")),
        hovertemplate = paste0(hover, "<extra>Unstable sheet</extra>")
      ) |>
      add_trace(
        x = fold_a, y = fold_b, z = fold_x,
        type = "scatter3d", mode = "lines", name = "Folds",
        line = list(color = "#334155", width = 5), hoverinfo = "skip"
      ) |>
      layout(
        uirevision = "keep-camera", showlegend = FALSE,
        scene = list(
          uirevision = "keep-camera",
          xaxis = list(title = list(text = default_labels$a), range = c(-3.5, 3.5)),
          yaxis = list(title = list(text = default_labels$b), range = c(-1, 3)),
          zaxis = list(title = list(text = default_labels$response), range = c(-2.6, 3)),
          aspectmode = "manual", aspectratio = list(x = 1.25, y = 1, z = 1),
          camera = list(eye = list(x = 1.6, y = 1.6, z = 1.1))
        ),
        margin = list(l = 0, r = 0, b = 0, t = 20)
      ) |>
      config(displaylogo = FALSE)
    
    plot <- plotly_build(plot)
    plot$x$data <- c(plot$x$data, person_traces(initial_targets, initial_positions, TRUE))
    plot
  })
  
  # ====================================================================
  # 9. In-place plot updates: coordinates, selection, axis labels
  # ====================================================================
  
  # Selection alone does not change coordinates, so it does not trigger
  # this observer. All intermediate surface steps remain internal.
  observeEvent(person_positions(), {
    positions <- person_positions()
    if (!nrow(positions)) return()
    
    geometries <- lapply(seq_len(nrow(positions)), function(row) {
      figure_geometry(positions$a[row], positions$b[row], positions$x[row])
    })
    
    a_values <- list()
    b_values <- list()
    x_values <- list()
    
    for (row in seq_len(nrow(positions))) {
      geometry <- geometries[[row]]
      a <- positions$a[row]
      b <- positions$b[row]
      x <- positions$x[row]
      
      # Explicit inner lists preserve single-point head and anchor traces.
      a_values <- c(a_values, list(geometry$a, list(a), list(a)))
      b_values <- c(b_values, list(geometry$b, list(b), list(b)))
      x_values <- c(x_values, list(geometry$x, list(geometry$head_x), list(x)))
    }
    
    update_plot(
      "restyle", list(x = a_values, y = b_values, z = x_values),
      all_person_trace_indices(seq_len(nrow(positions)))
    )
  }, ignoreInit = TRUE)
  
  observeEvent(list(selected_people(), person_ids()), {
    ids <- person_ids()
    if (!length(ids)) return()
    
    selected <- ids %in% selected_people()
    bodies <- body_indices(seq_along(ids))
    
    update_plot("restyle", list("line.width" = as.list(ifelse(selected, 8, 5))), bodies)
    update_plot("restyle", list("marker.size" = as.list(ifelse(selected, 7, 5))), bodies + 1L)
  }, ignoreInit = TRUE)
  
  observeEvent(axis_labels(), {
    labels <- axis_labels()
    
    # Fixed Plotly field names; only their visible titles are renamed.
    update_plot("relayout", list(
      "scene.xaxis.title.text" = labels$a,
      "scene.yaxis.title.text" = labels$b,
      "scene.zaxis.title.text" = labels$response
    ))
    
    hover <- hover_coordinates(labels)
    update_plot("restyle", list(hovertemplate = list(
      paste0(hover, "<extra>Stable sheet</extra>"),
      paste0(hover, "<extra>Unstable sheet</extra>")
    )), c(0L, 1L))
    
    targets <- isolate(person_targets())
    if (!nrow(targets)) return()
    
    templates <- lapply(targets$name, anchor_hover, labels = labels)
    anchors <- body_indices(seq_len(nrow(targets))) + 2L
    update_plot("restyle", list(hovertemplate = templates), anchors)
  }, ignoreInit = TRUE)
}
#end of BIG mysterious function
                     
shinyApp(ui, server)
