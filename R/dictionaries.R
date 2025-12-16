#' Dictionary for discussions
#'
#' Creates a data frame dictionary of different versions of wording to use in
#' usecase posts depending on the language of the post.
#'
#' @returns Data frame.
#'
#' @export
#' @examples
#' dict_usecases()

dict_usecases <- function() {
  dplyr::tibble(
    # Spanish and English
    category = c("Casos de Uso", "Use Cases"),
    l_using = c("Usando", "Using"),
    l_by = c("por", "by"),
    l_maintained = c("mantenido por", "maintained by"),
    l_intro_mult = c(
      "[casos de uso] ¡Nuevos casos de uso de rOpenSci!",
      "[use cases] New rOpenSci usecases!"
    ),
    l_intro_single = c(
      "[casos de uso] ¡Nuevo caso de uso de rOpenSci!",
      "[use cases] Usecase of an rOpenSci resource:"
    ),
    l_share = c(
      "¡Comparte tu caso de uso!",
      "Share your usecase too!"
    )
  )
}
