package aqua.linker

// HACK: here E is a FileNotFound error with Focus that the code will 'throw'
// if not found it in the list of loaded modules in `Modules` class.
// Essentially this error is a container with import information
// and a future error if the file for this import is not found
// TODO: fix it
case class AquaModule[I, E, T](id: I, dependsOn: Map[I, E], body: T => T)
