# see https://rconsortium.github.io/S7/articles/packages.html#method-registration
.onLoad <- function(...) {
  S7::methods_register()
}
