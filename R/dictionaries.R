#' Dictionary for usecases
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
      "[casos de uso] \u00A1Nuevos casos de uso de rOpenSci!",
      "[use cases] New rOpenSci usecases!"
    ),
    l_intro_single = c(
      "[casos de uso] \u00A1Nuevo caso de uso de rOpenSci!",
      "[use cases] Usecase of an rOpenSci resource:"
    ),
    l_share = c(
      "\u00A1Comparte tu caso de uso!",
      "Share your usecase too!"
    )
  )
}

#' Dictionary for help-wanted
#'
#' Creates a data frame dictionary of different versions of wording to use in
#' help-wanted posts depending on the language of the post.
#'
#' @returns Data frame.
#'
#' @export
#' @examples
#' dict_helpwanted()

dict_helpwanted <- function() {
  # TODO UPDATE!
  dplyr::tibble(
    # Spanish and English
    category = c("Se busca colaboración", "Help wanted"),
    l_first = c("", "A great way to learn with this 'good first issue'!"),
    l_maintainer = c("", "New Maintainer Wanted"),
    l_maintained = c("mantenido por", "maintained by"),
    l_intro_mult = c(
      "[casos de uso] \u00A1Nuevos casos de uso de rOpenSci!",
      "[use cases] New rOpenSci usecases!"
    ),
    l_intro_single = c(
      "[casos de uso] \u00A1Nuevo caso de uso de rOpenSci!",
      "[use cases] Usecase of an rOpenSci resource:"
    ),
    l_share = c(
      "\u00A1Comparte tu caso de uso!",
      "Share your usecase too!"
    )
  )
}
