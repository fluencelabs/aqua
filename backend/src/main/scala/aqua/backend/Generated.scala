package aqua.backend

import aqua.definitions.ServiceDef

/**
 * Compilation result
 *
 * @param suffix extension or another info that will be added to a resulted file
 * @param content compiled code
 */
case class Generated(suffix: String, content: String, air: List[AirFunction], services: List[ServiceDef] = Nil)
