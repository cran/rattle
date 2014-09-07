normVarNames <- function(vars, sep="_")
{
  if (sep == ".") sep <- "\\."
    
  # Replace all _ and . and ' ' with the nominated separator.
  
  pat  <- '_|\\.| '
  rep  <- sep
  vars <- stringr::str_replace_all(vars, pat, rep)
  
  # Replace any all capitals words with Initial capitals
  
  pat  <- stringr::perl('(?<![[:upper:]])([[:upper:]])([[:upper:]]*)')
  rep  <- '\\1\\L\\2'
  vars <- stringr::str_replace_all(vars, pat, rep)
  
  # Replace any capitals not at the beginning of the string with _ 
  # and then the lowercase letter.
  
  pat  <- stringr::perl('(?<!^)([[:upper:]])')
  rep  <- paste0(sep, '\\L\\1')
  vars <- stringr::str_replace_all(vars, pat, rep)
  
  # WHY DO THIS? Replace any number sequences not preceded by an
  # underscore, with it preceded by an underscore. The (?<!...) is a
  # lookbehind operator.
  
  pat  <- stringr::perl(paste0('(?<![', sep, '[[:digit:]]])([[:digit:]]+)'))
  rep  <- paste0(sep, '\\1')
  vars <- stringr::str_replace_all(vars, pat, rep)

  # Remove any resulting initial or trailing underscore or multiples:
  #
  # _2level -> 2level

  vars <- stringr::str_replace(vars, "^_+", "")
  vars <- stringr::str_replace(vars, "_+$", "")
  vars <- stringr::str_replace(vars, "__+", "_")
  
  # Convert to lowercase
  
  vars <- tolower(vars)
    
  # Remove repeated separators.
  
  pat  <- paste0(sep, "+")
  rep  <- sep
  vars <- stringr::str_replace_all(vars, pat, rep)
  
  return(vars)
}
