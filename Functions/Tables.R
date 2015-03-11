# AR Table ----
output$dtAR <- renderDataTable({
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
  tt <- apply(plData[,-1], 2, 
              function(x, perc=FALSE) {
                if(perc) x <- 100 * x
                x <- round(x, 2)
                }, perc=ifelse(plType == "AR", TRUE, FALSE)
              )
  tt <- matrix(tt, length(idFirms), length(arTime))
  tt <- as.data.frame(tt)
  plData <- cbind(plData[, 1], tt)
  names(plData) <- c("firms", arTime)
  return(plData)
}, options = list(paging = FALSE, 
                  searching = FALSE, 
                  searchable = FALSE, 
                  info=FALSE, 
                  ordering=FALSE,
                  rowCallback = I(
                    "function(row, data) {
                    $('td', row).css('text-align', 'center');
                    $('td:eq(0)', row).css('text-align', 'left');
                    $('td:eq(0)', row).css('font-weight', 'bold');
                    }"
                  ),
                  headerCallback = I(
                    "function(thead, data, start, end, display) {
                        $('th').css('text-align', 'center');
                        $('th').css('color', '#333333');
                        $('th').css('border-bottom', '3px double #333333');
                    }"
                  ))
)
