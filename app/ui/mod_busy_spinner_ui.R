mod_busy_spinner_ui <- function(id) {
  div(class="busy-spinner",
      add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
}