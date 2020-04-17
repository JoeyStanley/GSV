observeEvent(input$show_help_button, {

  # Show the other button
  output$hide_help_button <- renderUI({
    actionButton("hide_help_button",
                 label="Toggle Help",
                 icon = icon("question-circle"),
                 class="toggle-help")
  })
  
  # Hide this button
  output$show_help_button <- renderUI({ return() })
  
  
  # Add the popovers for the left and right sides (I can' believe this loop works!)
  for (side in c("left", "right", "traj", "grid")) {
    
    # ________ Speakers -------------------------------------------------------------------
    addPopover(session, id = paste0("ethnicity_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Select Ethnicity",
               content = paste0("These are labels assigned by the original fieldworkers."))
    addPopover(session, id = paste0("sex_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Select Sex",
               content = paste0("These are labels assigned by the original fieldworkers."))
    addPopover(session, id = paste0("education_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Select Education",
               content = paste0("These represent the number of years of formal education completed."))
    addPopover(session, id = paste0("classification_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Select Classification Types",
               content = paste0("These are the original LAGS classifications, as explained in Pederson <i>et al.</i> (1986), Vol. 1 pg. 33&ndash;40.<br/><br/>",
                                "<i>Type I</i>: Little formal education, little reading and restricted social contacts.<br/>",
                                "<i>Type II</i>: Better formal education (usually high school) and/or wider reading and social contacts.<br/>",
                                "<i>Type III</i>: Superior education (usually college), cultured background, wide reading and/or extensive social contacts.<br/>",
                                "<br/>",
                                "<i>Type A</i>: Aged, and/or regarded by the field worker as old-fashioned.<br/>",
                                "<i>Type B</i>: Middle-aged or younger, and/or regarded by the field worker as more modern.<br/>",
                                "<br/>",
                                "The classes <i>Folk</i>, <i>Common</i>, and <i>Cultivated</i> are functional classifcations based on cumulative social experiences of each of the informants."))
    addPopover(session, id = paste0("yob_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Filter by Year of Birth",
               content = paste0("Filter the speakers by their year of birth."))
    # ________ Words -------------------------------------------------------------------
    addPopover(session, id = paste0("wordlist_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "Include/Exclude Words",
               content = paste0("By default, the words in this box are a standard set of function words and are exluded from the analysis.",
                                "<br/>",
                                "You can switch this so that <i>only</i> these words are included using the radio buttons to the right.",
                                "<br/>",
                                "You can change this list by typing additional words, or by putting your curser after a word and hitting backspace."))
    addPopover(session, id = paste0("stopwords_btn_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Stop Words",
               content = paste0("Click this button to populate the box to the right with a default list of stop words. Note, this will erase any words that are already there."))
    addPopover(session, id = paste0("clear_words_btn_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Clear List",
               content = paste0("Click this button to clear the box to the right."))
    
    
    # ________ Vowels -------------------------------------------------------------------
    addPopover(session, id = paste0("vowel_", side), placement = "right", trigger = "hover",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Vowel Selection",
               content = paste0("Choose one or more vowels to display in the plot below. Note that these are lexically assigned vowel qualities as determined by the CMU Pronouncing Dictionary and are no indication of actual pronunciation."))
    addPopover(session, id = paste0("place_", side), placement = "right", trigger = "hover",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Place of Articlation Selection",
               content = paste0("Filter the selected vowels by place of articulation of the following consonant."))
    addPopover(session, id = paste0("voice_", side), placement = "right", trigger = "hover",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Voicing Selection",
               content = paste0("Filter the selected vowels by voicing of the following consonant."))
    addPopover(session, id = paste0("manner_", side), placement = "left", trigger = "hover",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Manner of Articulation Selection",
               content = paste0("Filter the selected vowels by manner of articulation of the following consonant. This is particularly useful for studying conditioned mergers such as the <i>PIN-PEN merger</i>."))
    addPopover(session, id = paste0("filter_", side), placement = "left", trigger = "focus",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Filtering Technique",
               content = paste0("Because this data was extracted from very noisy recordings, there will be erroneous formant measurements. A filtering technique is therefore recommended to remove bad data from the visualization. The following options are provided:",
                                "<br/><br/>",
                                "<i>No filter</i>: No tokens are excluded based on their formant measurements.<br /><br />",
                                "<i>z-score</i>: For each vowel for each speaker, the data transformed into a <i>z</i> score for both F1 and F2. This means the mean for each cluster is (0,0) and all other points are measured in standard deviation units from that mean. Any observation further than 3 standard deviations from the mean is excluded.<br /><br />",
                                "<i>Mahalanobis distance</i>: Like the <i>z</i>-score technique, the F1 and F2 measurements are scaled, but instead of filtering on F1 and F2, outliers are determined by measuring the Euclidean distance from the center of the distribution. The furthest 5% of observations are removed. This method has the effect of producing perfectly ellipsoid distributions. <br /><br />",
                                "<i>Joey's method</i>: Joey Stanley is working on an alternative method for filtering out vowel data that is like the Mahalanobis distance but less sensitive to outliers. Specific details on how this is calculated will not be said here because the procedure is still being refined."))
    addPopover(session, id = paste0("stress_", side), placement = "left", trigger = "hover",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Stress Selection",
               content = paste0("Filter the selected vowels based on their stress. Note that stress is lexically assigned as determined by the CMU Pronouncing Dictionary and are no indication of actual pronunciation."))
    addPopover(session, id = paste0("norm_", side), placement = "left", trigger = "focus",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Normalization Procedures",
               content = paste0("For more information on vowel normalization and the procedures listed here, please visit <a href=\'http://lingtools.uoregon.edu/norm/\' target=\'_blank\'>NORM: The Vowel Normalization and Plotting Suite</a> produced by Erik Thomas and Tyler Kendall."))
    addPopover(session, id = paste0("trans_", side), placement = "left", trigger = "focus",
               options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
               title = "Transcription Systems",
               content = paste0("<i>ARPABET</i>: The transcription system used in the <a href=\'http://www.speech.cs.cmu.edu/cgi-bin/cmudict\' target=\'_blank\'>CMU Pronouncing Dictionary</a> and the default used in forced aligners like <a href=\'http://fave.ling.upenn.edu/\' target=\'_blank\'>FAVE</a>.<br /><br />",
                                "<i><a href=\'http://www.phon.ucl.ac.uk/home/sampa/\' target=\'_blank\'>SAMPA</a></i>: Computer readable phonetic alphabet where IPA symbols are mapped into ACII codes.<br /><br />",
                                "<i>Plotnik</i>: The system used by the <a href=\'http://www.ling.upenn.edu/~wlabov/Plotnik.html\' target=\'_blank\'>Plotnik</a> vowel analysis program from UPenn. <br /><br />",
                                "<i>Wells\' Lexical Sets</i>: Traditional labels assigned to historical vowel classes (<a href=\'http://www.cambridge.org/us/academic/subjects/languages-linguistics/phonetics-and-phonology/accents-english-volume-1?format=PB#YHYv6ERH82cSs4KZ.97\' target=\'_blank\'>Wells 1982</a>)."))
    
    # ________ Plot -------------------------------------------------------------------
    addPopover(session, id = paste0("points_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Hide/Show Points",
               content = paste0("Check this to display each observation as points in a scatterplot.<br /><br/>",
                                "If points are displayed, it\'s advisible to not display words at the same time."))
    addPopover(session, id = paste0("pointsAlpha_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Opacity and Transparency (Points)",
               content = "0 = completely transparent and 1=fully shaded in. For plots with many points, a smaller value is usually better.")
    addPopover(session, id = paste0("pointsSize_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Point Size",
               content = "Default=1. Generally the size is inversely proprotional to the amount of data displayed.")
    addPopover(session, id = paste0("ellipses_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Hide/Show Ellipses",
               content = "These ellipses capture observations within some range of each vowel\'s mean.")
    addPopover(session, id = paste0("ellipsesAlpha_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Opacity and Transparency (Ellipses)",
               content = "0 = completely transparent and 1=fully shaded in. Usually, shaded ellipses are better.")
    addPopover(session, id = paste0("ellipsesSize_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Ellipses Size",
               content = paste0("What percentage of each vowel should be contained within the ellipses?<br /><br />",
                                "67% = approx. 1 standard deviation (default)<br />",
                                "95% = approx. 2 standard deviations<br/>",
                                "99% = approx. 3 standard deviations"))
    addPopover(session, id = paste0("means_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Hide/Show Vowel Means",
               content = "Check this to display the mean of each vowel superimposed on the plot.")
    addPopover(session, id = paste0("meansAlpha_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Opacity and Transparency (Means)",
               content = "0 = completely transparent and 1=fully shaded in. You\'ll usually want the means to be darker than the points so they stand out more.")
    addPopover(session, id = paste0("meansSize_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Means Size",
               content = "Default=10. Generally, you want the means to be larger than the points so they stand out more.")
    addPopover(session, id = paste0("words_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Hide/Show Words",
               content = paste0("Check this to plot words from which the formant measurements came from.<br /><br />",
                                "If words are displayed, it\'s advisible to not display points at the same time.<br /><br />",
                                "<i>Warning</i>: this may take some time for the plot to render. Please be patient for large datasets."))
    addPopover(session, id = paste0("wordsAlpha_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Opacity and Transparency (Words)",
               content = "0 = completely transparent and 1=fully shaded in. For plots with many words, more transparency is usually better.")
    addPopover(session, id = paste0("wordsSize_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Word Size",
               content = "Default=3. Generally the size is inversely proprotional to the amount of data displayed.")
    
    
    # ________ Plot Options -------------------------------------------------------------------
    addPopover(session, id = paste0("colors_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Color Options",
               content = paste0("<i>Black</i>: Everything is black. This is the default on the Point Pattern plots.<br /><br />",
                                "<i>Multicolored (Variable)</i>: This uses the default R color scheme. The colors are determined by the number of vowels you have displayed and are maximally distinct from each other. This is best for saving the plots, but can be confusing when comparing different subsets because the vowel that is, say, blue, is not consistent.<br /><br />",
                                "<i>Multicolored (Persistent)</i>: Each vowel has a specific color assigned to it. When this option is selected, the colors will always be used regardless of what vowels are being displayed at a time. This is useful for when you want to look at different subsets of vowels because /o/ is always blue, for example. This is the default on all the plots except for the Point Pattern page.<br /><br />"))
    addPopover(session, id = paste0("F1_range_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "F1 Range",
               content = paste0("Set the minimum and maximum F1 values (the <i>y</i>-axis) that will be displayed on the plot (the top and bottom boundaries). This is automatically set to a default value when new data is displayed."))
    addPopover(session, id = paste0("F2_range_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "F2 Range",
               content = paste0("Set the minimum and maximum values for F2 (the <i>x</i>-axis) that will be displayed on the plot (the let and right boundaries). This is automatically set to a default value when new data is displayed."))
    addPopover(session, id = paste0("ratio_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Set aspect ratio",
               content = paste0("Change the aspect ratio of the plot. The default setting is determined by the normalization procedure, and aims for a 2:1 ratio. This means that 100Hz along the <i>x</i>-axis is twice the length of 100Hz along the <i>y</i>-axis. A ratio of 1 means that a unit of measurement along the <i>x</i>-axis is the same size as it is along the <i>y</i>-axis."))
    addPopover(session, id = paste0("zoom_out_max_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "Fit Vowel Space",
               content = paste0("This changes the axes of the plot so that it comfortably fits a vowel space. This is good when you want to compare two different vowels and want to keep the axes consistent."))
    addPopover(session, id = paste0("zoom_in_max_", side), placement = "left", trigger = "hover",
               options = list(container = "body"),
               title = "Fit Displayed Data",
               content = paste0("This changes the axes of the plot such that only the selected data will be displayed."))
    
    
    
    # ________ Customization -------------------------------------------------------------------
    addPopover(session, id = paste0("height_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Image Height",
               content = paste0("When downloading this image, this is how tall, in inches, the image will be."))
    addPopover(session, id = paste0("width_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "Image Width",
               content = paste0("When downloading this image, this is how wide, in inches, the image will be."))
    addPopover(session, id = paste0("dpi_", side), placement = "right", trigger = "hover",
               options = list(container = "body"),
               title = "DPI (Image Quality)",
               content = paste0("DPI or <i>dots per inch</i> is a measure of image quality. The default is 300, which is magazine quality, but you can try something like 150 for lower-quality images or as high as 600 for very high resolution plots. This greatly affects how big the file is, so if you find it taking a long time, maybe set it to something lower.",
                                "<br/>",
                                "Note that this is irrelevant (and thus silently ignored) when saving a PDF."))
    addPopover(session, id = paste0("filename_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "File Name",
               content = paste0("This name will become the file name when you download an image."))
    addPopover(session, id = paste0("filetype_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "File Type",
               content = paste0("This determines what type of image you'll download."))
    addPopover(session, id = paste0("download_", side), placement = "bottom", trigger = "hover",
               options = list(container = "body"),
               title = "Download Button",
               content = paste0("Click this to download the currently displayed plot. Use the controls on this tab to change the size, quality, and type of file you download."))
  }
  
  # ________ Grid Options ------------------------------
  addPopover(session, id = "grid_rows", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Number of Grid Rows",
             content = paste0("This number determines how many rows should be used in the underlaid grid.",
                              "<br/><br/>",
                              "Note: The grid area only covers what we've determined to be a reasonably large vowel space, regardless of the subset actually displayed or filter being used here. This means that you may find points being plotted outside of the grid: most of those are outliers."))
  addPopover(session, id = "grid_cols", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Number of Grid Columns",
             content = paste0("This number determines how many columns should be used in the underlaid grid.",
                              "<br/><br/>",
                              "Note: The grid area includes the full vowel space of <i>all</i> data, regardless of the filter used or the subset actually displayed here. Since some points are gross outliers, cell A1 is very far to the bottom left of the vowel space. Hint: click the \'Fit all data\' button to show the full vowel space for all data."))
  
  addPopover(session, id = "grid_cell_labels", placement = "bottom", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Show Cell Labels",
             content = paste0("Check this box if you would like the underlaid grid to display the names of the cells. Rows are numbered sequentially with '1' at the bottom of the vowel space. Columns are letters starting with 'A' at the far left of the vowel space."))
  addPopover(session, id = "grid_label_alpha", placement = "bottom", trigger = "hover",
             options = list(container = "body"),
             title = "Opacity and Transparency (Grid Labels)",
             content = "0 = completely transparent and 1=fully shaded in.")
  addPopover(session, id = "grid_label_size", placement = "bottom", trigger = "hover",
             options = list(container = "body"),
             title = "Grid Label Size",
             content = "Default=7. Try a smaller size if the cells labels overlap when exporting the image.")
  
  addPopover(session, id = "shading_type", placement = "left", trigger = "hover",
             options = list(container = "body"),
             title = "Grid Shading Type",
             content = paste0("These options control how the cells in the grid are shaded. <br/><br/>",
                              "<i>Discrete</i> Cells are binned and colored by those bins. This is the default because it's easier to interpret them (catographers do this too). The bins represent quantiles, meaning if you have four bins, the darkest cells collectively contain the top 25% of the data. Importantly, they are <i>not</i> equal-interval breaks, so the range of values within each bin will not necessarily be the same.<br/><br/>", 
                              "<i>Continuous</i> Cells are shaded using a continuous scale.<br/><br/>",
                              "<i>None (white)</i> No shading, but the grid remains. This is useful if you want to display the grid for explanatory purposes without the distraction of shading."))
  addPopover(session, id = "grid_shades", placement = "left", trigger = "hover",
             options = list(container = "body"),
             title = "Number of Discrete Shades",
             content = "When using discrete shading, this number determines how many shades should be used. By default, it is four because the data appears to follow a 75-25 distribution.")
  addPopover(session, id = "shade_color", placement = "left", trigger = "hover",
             options = list(container = "body"),
             title = "Darkest Shade",
             content = paste0("You can change the color of the darkest cell. The color should be in hexidecimal format, with the pound sign before it.<br/><br/>",
                              "Here are some recommended colors from <a href=\'http://colorbrewer2.org/\' target=\'_blank\'>ColorBrewer2.org</a>:<br/><br/>",
                              "<ul>",
                              "<li><i>\"#084594\"</i> Blue</li>",
                              "<li><i>\"#005a32\"</i> Green</li>",
                              "<li><i>\"#252525\"</i> Dark gray</li>",
                              "<li><i>\"#8c2d04\"</i> Orange</li>",
                              "<li><i>\"#4a1486\"</i> Purple</li>",
                              "<li><i>\"#99000d\"</i> Red</li>",
                              "</ul><br/>",
                              "By default, the color is \"#144387\", which is just a particular shade of blue I (Joey) like. You can also go to <a href=\'http://color.adobe.com/\' target=\'_blank\'>color.adobe.com</a> to explore other colors and get their hexidecimal values.</li>"))
  
  
  # ________ Other UI elements ------------------------------
  addPopover(session, id = "display_n_rows", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Number of Rows to Display",
             content = paste0("This number determines how many rows should be displayed in the table to the right."))
  addPopover(session, id = "click_info", placement = "bottom", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Number of Rows to Display",
             content = paste0("Click anywhere on the plot and this table will be populated with the the closest points. The number of points that are displayed is determined by the selector to the left. The table gives information about the speaker, word, and vowel associated with that token"))
  addPopover(session, id = "scatterplot_grid", placement = "bottom", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Grid Chart",
             content = paste0("This plot displays points in the normal F1-F2 vowel space, but with a grid underlaid. The number of rows and columns is determined by options on the 'Plot Options' tab. Darker cells indicate more data in that space."))
  addPopover(session, id = "scatterplot_grid", placement = "top", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Grid Chart",
             content = paste0("This plot displays points in the normal F1-F2 vowel space, but with a grid underlaid. The number of rows and columns is determined by options on the 'Plot Options' tab. Darker cells indicate more data in that space."))
  addPopover(session, id = "a_curve", placement = "top", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "The A-Curve",
             content = paste0("This plot shows the number of observations in each cell in the grid above, in order of frequency. The result is predicted to always follow a Zipfian distribution (aka an \"A-Curve\")."))
  
  addPopover(session, id = "map_color", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Color of Points",
             content = paste0("Select which variable should be used to color the points on the map to the right."))
  addPopover(session, id = "map_shape", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Shape of Points",
             content = paste0("Select which variable should be used to determine the shape of the points on the map to the right."))
  addPopover(session, id = "speaker_nums_map", placement = "right", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "Show Speaker Numbers?",
             content = paste0("Check this box to display speaker numbers on the map below their points on the map."))
  addPopover(session, id = "map", placement = "bottom", trigger = "hover",
             options = list(container = "body"), # this is requird for right placement: https://github.com/ebailey78/shinyBS/issues/32
             title = "DASS Speakers",
             content = paste0("This map displays the location for all 64 speakers in the DASS corpus. The color and shape of the points can be changed using the menus to the left. The numbers are the original numbers assigned to the speakers in the original LAGS dataset."))
  
  
  
})