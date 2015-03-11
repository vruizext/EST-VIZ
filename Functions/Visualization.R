# plot AR data from AR results.csv
output$VisualizeAR <- renderPlot({
  estData <- collectData()
  if (is.null(estData)) 
    return()
  
  if (is.null(input)) 
    return()
  
  if (input$typeOfReport != "AR")
    return()
  
  if (is.null(input$ARFirms))
    return()
  
  shiny::validate(need(length(input$ARFirms) <= 6, "Please choose max. 6 Firms!"))
  
  plType <- input$arOrT
  
  wGet <- ifelse(plType == "AR", "AR", "value")
  idAR <- which(str_detect(names(estData), wGet))
  namAR <- names(estData)[idAR]
  namAR <- str_replace_all(namAR, "[.]", "")
  arTime <- as.numeric(unlist(lapply(str_split(namAR, wGet), function(x) x[[2]])))
  idNull <- which(arTime == 0)
  if (length(idNull) > 0)
    arTime[1:idNull] <- -1 * arTime[1:idNull]
  
  idFirms <- which(input$ARFirms %in% estData$Event.ID)
  plData <- estData[idFirms, c(1, idAR)]
  names(plData) <- c("firms", arTime)
  plData <- melt(plData, id.vars = "firms")
  plData$variable <- as.numeric(as.character(plData$variable))
  
  # plData$firms <- as.character(plData$firms)
  p <- ggplot(plData, aes(x=variable, y=value, group=firms)) +
    geom_vline(xintercept=0, linetype="dashed", colour="grey") +
    geom_hline(yintercept=0, linetype="dashed", colour="grey") +
    geom_line(aes(color=firms)) + 
    scale_color_manual(name="", values = stat.color)  +
    theme(legend.position="bottom") +
    scale_x_continuous(expand = c(0, 0.01)) +
    xlab("")
  if (plType == "AR") {
    p <- p + scale_y_continuous(labels=percent) +
      ylab("Abnormal Return")
  } else {
    p <- p + ylab("AR t-Statistic")
  }
  
  return(p)
})

# plot AAR data from AAR results.csv
output$VisualizeAAR <- renderPlot({
  estData <- collectData()
  if (is.null(estData)) 
    return()
  
  if (is.null(input)) 
    return()
  
  if (input$typeOfReport != "AAR")
    return()
  
  if (is.null(input$ARGroups))
    return()
  
  shiny::validate(need(length(input$ARGroups) <= 6, "Please choose max. 6 Firms!"))
  
  idnam <- which(str_detect(estData$Grouping.Variable.N, "N[(]"))
  nam <- estData$Grouping.Variable.N[idnam-1]
  
  plType <- "AAR"
  
  wGet <- "AAR"
  idAR <- which(str_detect(names(estData), wGet))
  namAR <- names(estData)[idAR]
  namAR <- str_replace_all(namAR, "[.]", "")
  arTime <- as.numeric(unlist(lapply(str_split(namAR, wGet), function(x) x[[2]])))
  idNull <- which(arTime == 0)
  if (length(idNull) > 0)
    arTime[1:idNull] <- -1 * arTime[1:idNull]
  
  idFirms <- which(input$ARGroups %in% estData$Grouping.Variable.N)
  plData <- estData[idFirms, c(1, idAR)]
  names(plData) <- c("firms", arTime)
  plData <- melt(plData, id.vars = "firms")
  plData$variable <- as.numeric(as.character(plData$variable))
  plData$value <- as.numeric(as.character(plData$value))
  # plData$firms <- as.character(plData$firms)
  p <- ggplot(plData, aes(x=variable, y=value, group=firms)) +
    geom_vline(xintercept=0, linetype="dashed", colour="grey") +
    geom_hline(yintercept=0, linetype="dashed", colour="grey") +
    geom_line(aes(color=firms)) + 
    scale_color_manual(name="", values = stat.color)  +
    theme(legend.position="bottom") +
    scale_x_continuous(expand = c(0, 0.01), breaks=arTime, labels=arTime) +
    xlab("")
  if (plType == "AAR") {
    p <- p + scale_y_continuous(labels=percent) +
      ylab("Averaged Abnormal Return")
  } else {
    p <- p + ylab("AR t-Statistic")
  }
  
  return(p)
})
