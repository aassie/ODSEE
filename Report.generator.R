Report.generator<-function(x){
  cat("#' ---\n#' title: \"Competition referee v0.1 report\"\n#' output:\n#'   html_document:\n#'     toc: true \n#'     toc_depth: 2 \n#' ---\n")
  cat("#+ echo=FALSE\n") 
  cat("knitr::opts_chunk$set(echo = TRUE)\n")
  cat("#' This report summarize the data from photometer data.\n#'\n#' # Experiment Summary\n")
  cat("#'There are", Exp.num, "experiments\n#'\n#'\n")
  
  for(n in 1:Exp.num){
    cat("#' * Experiment", n, "has", length(Exp.samples.list[[n]]), "plate(s)","\n")
  }
  
  cat("#'\n")
  
  for(n in 1:Exp.num){
    cat("#'\n")
    cat("#' # Experiment",n,"\n")
    cat("#' Experiment", n, "has", length(Exp.samples.list[[n]]), "plate(s)","\n#'\n")

        cat("#' ## Tested bacteria \n")
    cat("#' <style>\n")
    cat("#'.col-container {display: table; width: 100%; }\n")
    cat("#'.col {display: table-cell; }\n")
    cat("#' </style>\n")
    cat("#'<div class=\"col-container\">\n#'\n")
    cat("#'<div class=\"col\">\n")
    cat("#' This experiment tested the following bacteria\n#'<ul>\n")
    for (b in unique(Result.auc[[n]]$`Tested Bacteria`)){
    cat("#' <li>",b,"</li>\n")
    }
    cat("#'</ul>\n")
    cat("#'</div>\n")
    cat("#'<div class=\"col\">\n")
    cat("#'against the following bacteria\n#'<ul>\n")
    for (b in unique(Result.auc[[n]]$Competitor)){
      cat("#' <li>",b,"</li>\n")
    }
    cat("#'</ul>\n")
    cat("#'</div>\n")
    cat("#'</div>\n")
    cat("#'\n")
    cat("#' ## Details \n")
    cat("#'\n")
    
    cat("#' ## Results \n")
    cat("#'\n")
    cat("#' The table below summarize all the different combination found based on the Manifest file. Those combination take account of well results accross different plates\n")
    
    cat("#+ echo=FALSE, fig.width=7\n")
    cat("Result.auc[[",n,"]]%>%")
    cat("kbl(align=c(rep('c',times=6))) %>%")
    cat("kable_styling(bootstrap_options = \"striped\", full_width = F, position = \"center\")\n")
    
 
    for (s in 1:length(Exp.samples.list[[n]])){
      cat("#' ### Plate",s,"\n")
      n.blank<-nrow(Ex.sample.data[[n]][[s]][["Metadata"]][Ex.sample.data[[n]][[s]][["Metadata"]]$Isolate=="Blank",1])
      cat("#' Plate",names(Ex.sample.data[[n]])[s] ,"has",n.blank, "blank wells\n")
      if(n.blank==0){
        cat("#' Please considere using Blank control in your next experiment\n")
      }
      cat("#'\n")
      cat("#+ echo=FALSE, fig.width=7\n")
      p2<-gsub("\\.xls.*$","",names(Ex.sample.data[[n]])[s])
      pplot<-paste(cwd,"/outputs/plots/Experiment-",n,".plate.",p2,".png",sep="")
      cat("knitr::include_graphics(\"",pplot,"\")\n", sep = "")
    }
    cat("#'\n")
  }
}

