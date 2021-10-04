package aqua.backend

import aqua.model.transform.res.FuncRes

/**
 * Compilation result
 *
 * @param extension extension or another info that will be added to a resulted file
 * @param content compiled code
 * @param suffix info (func name for Air backend) that will be added to a resulted file name
 */
case class Generated(extension: String, content: String, func: Option[FuncRes] = None)
