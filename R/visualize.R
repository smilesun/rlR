visualize = function(tabular, env = c("cliff"), latex = FALSE) {
  
  left  = if (latex) "$\\leftarrow$"             else "<"
  right = if (latex) "$\\rightarrow$"            else ">"
  up    = if (latex) "$\\,\\,\\uparrow\\,\\,$"   else "^"
  down  = if (latex) "$\\,\\,\\downarrow\\,\\,$" else "v"
  
  parser_lake = function(x) {
    if (x == 0) left
    else if (x == 1) down
    else if (x == 2) right
    else if (x == 3) up
  }
  
  parser_cliff = function(x) {
    if (x == 0) up
    else if (x == 1) right
    else if (x == 2) down
    else if (x == 3) left
  }

  policy = data.frame(position = 1:nrow(tabular))
  policy$action = sapply(policy$position, function(x) which.max(tabular[x, ]) - 1, USE.NAMES = FALSE)
  policy$action = sapply(policy$action, if (env == "lake") parser_lake else parser_cliff, USE.NAMES = FALSE)
  
  if (env == "lake" && latex)
    cat( "\\hline \n",
         paste(policy$action[1:4],  collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[5:8],  collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[9:12],  collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[13:16],  collapse = " & "), "\\\\ \\hline \n"
    )
  else if (env == "cliff" && latex)
    cat( "\\hline \n",
         paste(policy$action[1:12],  collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[13:24], collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[25:36], collapse = " & "), "\\\\ \\hline \n",
         paste(policy$action[37:48], collapse = " & "), "\\\\ \\hline \n"
    )
  else if (env == "lake")
    cat( "\n",
         policy$action[1], policy$action[2], policy$action[3], policy$action[4], "\n",
         policy$action[5], policy$action[6], policy$action[7], policy$action[8], "\n",
         policy$action[9], policy$action[10], policy$action[11], policy$action[12], "\n",
         policy$action[13], policy$action[14], policy$action[15], policy$action[16], "\n"
    )
  else
    cat( "\n",
         paste(policy$action[1:12],  collapse = " "), "\n",
         paste(policy$action[13:24], collapse = " "), "\n",
         paste(policy$action[25:36], collapse = " "), "\n",
         paste(policy$action[37:48], collapse = " "), "\n"
    )
}
