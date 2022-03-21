normVarNames <- function(vars, sep="_")
{
  return(janitor::make_clean_names(vars, numerals="right"))
}
