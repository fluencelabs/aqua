package aqua.backend

/**
 * Compilation result
 *
 * @param suffix extension or another info that will be added to a resulted file
 * @param content compiled code
 */
case class Generated(suffix: String, content: String)
