# ##
# Anything needed for the functioning of the namespace should 
# be handled at load/unload times by the .onLoad and .onUnload 
# hooks. For example, DLLs can be loaded (unless done by a 
#                                         useDynLib directive in 
#                                         the NAMESPACE file) 
# and initialized in .onLoad and unloaded in .onUnload. 
# Use .onAttach only for actions that are needed only when the 
# package becomes visible to the user (for example a start-up message) 
# or need to be run after the package environment has been created.

.onAttach <- function(...) {
  ##
  pack <- "pborepo"
  packageStartupMessage(sprintf("Loaded %s (%s) -- PBO data retrieval and metadata.", 
                                pack,
                                utils::packageVersion(pack)))
  #lsf.str(paste0("package:",pack))
}
