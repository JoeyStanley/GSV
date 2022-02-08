observeEvent(input$hide_help_button, {
  
  # Show the other button
  output$show_help_button <- renderUI({
    actionButton("show_help_button",
                 label="Toggle Help",
                 icon = icon("question-circle-o"),
                 class="toggle-help")
  })
  
  # Hide this button
  output$hide_help_button <- renderUI({ return() })
  
  # Remove the popovers for the left and right sides (I can' believe this loop works!)
  for (side in c("left", "right")) {
    removePopover(session, paste0("ethnicity_", side))
    removePopover(session, paste0("sex_", side))
    removePopover(session, paste0("education_", side))
    removePopover(session, paste0("classification_", side))
    removePopover(session, paste0("norm_", side))
    removePopover(session, paste0("trans_", side))
    
    removePopover(session, paste0("points_", side))
    removePopover(session, paste0("pointsAlpha_", side))
    removePopover(session, paste0("pointsSize_", side))
    removePopover(session, paste0("ellipses_", side))
    removePopover(session, paste0("ellipsesAlpha_", side))
    removePopover(session, paste0("ellipsesSize_", side))
    removePopover(session, paste0("means_", side))
    removePopover(session, paste0("meansAlpha_", side))
    removePopover(session, paste0("meansSize_", side))
    removePopover(session, paste0("words_", side))
    removePopover(session, paste0("wordsAlpha_", side))
    removePopover(session, paste0("wordsSize_", side))
    removePopover(session, paste0("colors_", side))
  }
})