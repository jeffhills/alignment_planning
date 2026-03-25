library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
library(svglite)
library(glue)
library(cowplot)
library(janitor)
# library(rms)
# library(Hmisc)
library(cowplot)
# library(assertr)
library(lubridate)
library(shinydashboard)
library(magick)
# library(ggforce)
# library(plotly)
library(redcapAPI)
library(gt)
library(blastula)
library(officer)
library(rvg)


options(shiny.maxRequestSize = 25*1024^2)

source("shiny_functions.R", local = TRUE)
source("jh_calculation_functions.R", local = TRUE)
source("jh_spine_coordinate_geometry_functions.R", local = TRUE)

source("jh_build_spine_from_coordinates_functions.R", local = TRUE)

source("jh_constructing_spine_from_coordinates_new.R", local = TRUE)


# In global.R
# email_credentials <- if(Sys.getenv("SHINY_ENV") == "production"){
#   blastula::creds_file("/srv/shiny-server/alignment_planning/.blastula_email_creds")
# } else {
#   blastula::creds_key("email_creds")
# }
email_credentials <- if(Sys.getenv("SHINY_ENV") == "production"){
  blastula::creds_file("/etc/shiny-apps/secrets/.blastula_email_creds")
} else {
  blastula::creds_key("email_creds")
}


# all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")

# reactlog_enable()


ui <- dashboardPage(
  
  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(title = "SolaSpine"),
  
  # ── Sidebar ─────────────────────────────────────────────────────────────────
  dashboardSidebar(
    
    tags$style(HTML("
      .segment-input {
        display: flex;
        align-items: center;
        justify-content: end;
        margin-bottom: 0px;
        font-size: 14px;
        color: black;
      }
      .segment-label {
        margin-right: 5px;
        white-space: nowrap;
        font-size: 12px;
        color: black;
      }
      .segment-input .form-group {
        margin-bottom: 1px;
        color: black;
      }
      .custom-numeric-input {
        padding: 0;
        margin: 0;
        text-align: -webkit-left;
      }
      .custom-numeric-input .form-control {
        padding: 2px 5px;
        margin-bottom: 0px;
        text-align: -webkit-left;
        width: 50px;
      }
    ")),
    
    br(),
    
    # X-ray orientation button (shown after upload)
    conditionalPanel(
      condition = "input.xray_file_uploaded == true",
      h4("Xray Orientation:"),
      actionBttn(
        inputId = "spine_orientation_button",
        label   = "Facing LEFT",
        style   = "material-flat",
        color   = "primary",
        icon    = icon("arrow-left")
      )
    ),
    
    br(),
    
    # Surgical planning box
    fluidRow(
      box(
        title      = "Surgical Planning:",
        status     = "info",
        width      = 12,
        collapsible = FALSE,
        conditionalPanel(
          condition = "input.xray_file_uploaded == true",
          conditionalPanel(
            condition = "input.all_points_recorded == true",
            actionBttn(
              inputId = "calibrate_button",
              label   = "Calibrate",
              size    = "sm",
              style   = "pill",
              color   = "primary"
            )
          ),
          hr(),
          uiOutput(outputId = "preop_xray_rigid_segments_ui")
        )
      )
    ),
    
    textOutput("calibration_status_text"),
    
    # Hidden state switches
    div(
      style = "display: none;",
      switchInput(inputId = "xray_file_uploaded", size = "mini", label = NULL,
                  value = FALSE, onLabel = "Y", offLabel = "N"),
      switchInput(inputId = "all_points_recorded", size = "mini", label = NULL,
                  value = FALSE, onLabel = "Y", offLabel = "N")
    )
  ),
  
  # ── Body ────────────────────────────────────────────────────────────────────
  dashboardBody(
    
    # Loading overlay
    tags$div(
      id    = "loading-overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%;
               background-color: #fff; z-index: 9999;
               display: flex; justify-content: center; align-items: center;",
      tags$div(tags$p("Loading..."))
    ),
    tags$script("
      $(document).on('shiny:idle', function() {
        $('#loading-overlay').fadeOut(500);
      });
    "),
    
    # ── Global styles ──────────────────────────────────────────────────────────
    tags$head(
      tags$style(HTML("

        /* File upload drop zone */
        .file-upload-container {
          border: 4px dashed #c5c5c5;
          border-radius: 10px;
          width: 80%;
          margin: 50px auto;
          padding: 50px;
          text-align: center;
          background-color: #f7f9fb;
          cursor: pointer;
          font-size: 18px;
          color: #333;
          transition: background-color 0.3s ease;
          position: relative;
        }
        .file-upload-container.dragover { background-color: #edf2f7; }
        .browse-btn {
          color: #007bff;
          font-weight: bold;
          cursor: pointer;
          display: inline-block;
          font-size: 20px;
          margin-top: 10px;
        }
        .upload-icon {
          font-size: 50px;
          color: #007bff;
          margin-bottom: 10px;
        }
        #image {
          position: absolute;
          width: 1px; height: 1px;
          padding: 0; margin: -1px;
          overflow: hidden;
          clip: rect(0,0,0,0);
          border: 0;
          opacity: 0;
        }

        /* Tab styles */
        .nav-tabs-custom > .nav-tabs { background-color: #0073e6; }
        .nav-tabs-custom > .nav-tabs > li.active > a,
        .nav-tabs-custom > .nav-tabs > li.active > a:hover {
          background-color: #005bb5;
          color: white;
          font-size: 18px;
          font-weight: bold;
        }
        .nav-tabs-custom > .nav-tabs > li > a { color: white; }

        /* Alignment box padding */
        #alignment_adjustment_box { padding: 0px !important; }

        /* Dynamic column resizing */
        .col-main { transition: width 0.3s ease; }
        .col-width-6 { width: 50% !important; }
        .col-width-4 { width: 33.33% !important; }
        .col-width-8 { width: 66.67% !important; }

      "))
    ),
    
    # ── Dynamic column resize handler ──────────────────────────────────────────
    tags$script(HTML("
      Shiny.addCustomMessageHandler('resize-columns', function(data) {
        if (data.expand) {
          $('#col-left').removeClass('col-width-6').addClass('col-width-4');
          $('#col-right').removeClass('col-width-6').addClass('col-width-8');
        } else {
          $('#col-left').removeClass('col-width-4').addClass('col-width-6');
          $('#col-right').removeClass('col-width-8').addClass('col-width-6');
        }
      });
    ")),
    
    # ── Drag-and-drop file upload (shown before upload) ────────────────────────
    conditionalPanel(
      condition = "input.xray_file_uploaded == false",
      fluidRow(
        column(
          width = 12, align = "center",
          div(
            class = "file-upload-container",
            fluidRow(
              align = "center",
              tags$span(icon("upload", class = "upload-icon",
                             style = "font-size: 60px; color: #007bff; cursor: pointer;")),
              br(),
              span("Drag & Drop or ", span("Choose an X-ray", class = "browse-btn"))
            )
          ),
          div(style = "display: none;",
              fileInput("image", label = NULL, accept = "image/", width = "100%")),
          div(class = "shiny-file-input-progress")
        )
      )
    ),
    
    # ── Drag-and-drop JS ───────────────────────────────────────────────────────
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var container  = $('.file-upload-container');
        var fileInput  = $('#image');

        container.on('dragover', function(e) {
          e.preventDefault(); e.stopPropagation();
          container.addClass('dragover');
        });
        container.on('dragleave', function(e) {
          e.preventDefault(); e.stopPropagation();
          container.removeClass('dragover');
        });
        container.on('drop', function(e) {
          e.preventDefault(); e.stopPropagation();
          container.removeClass('dragover');
          var files = e.originalEvent.dataTransfer.files;
          fileInput[0].files = files;
          fileInput.trigger('change');
        });

        fileInput.css({
          'position'       : 'absolute',
          'top'            : '-9999px',
          'opacity'        : '0',
          'visibility'     : 'hidden',
          'pointer-events' : 'none'
        });

        container.on('click', function() {
          fileInput.off('click');
          setTimeout(function() { fileInput.trigger('click'); }, 50);
        });
      });
    ")),
    
    # ── Main workspace (shown after upload) ────────────────────────────────────
    conditionalPanel(
      condition = "input.xray_file_uploaded == true",
      fluidRow(
        
        # ════════════════════════════════════════════════════════════════════════
        # COLUMN 1 — X-ray image panel
        # ════════════════════════════════════════════════════════════════════════
        tags$div(
          id    = "col-left",
          class = "col-sm-6 col-main",
          
          box(
            width = 12,
            
            # Instruction banner
            fluidRow(
              class = "d-flex justify-content-center",
              tags$div(
                style = "font-size: 20px; font-weight: bold; color: yellow;
                         font-family: arial; font-style: italic; text-align: center;
                         background-color: black; padding: 3px; border-radius: 12px;
                         display: block; margin-left: 10px; margin-right: 10px;
                         box-sizing: border-box;",
                htmlOutput(outputId = "xray_click_instructions")
              ),
              conditionalPanel(
                condition = "input.xray_file_uploaded == true",
                fluidRow(
                  tags$div(
                    "Zoom with scroll wheel; Pan with right click",
                    style = "font-size: 10pt; font-style: italic; color: #555; text-align: center;"
                  )
                )
              )
            ),
            
            # ── Point-collection view (before all points recorded) ─────────────
            conditionalPanel(
              condition = "input.xray_file_uploaded == true & input.all_points_recorded == false",
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    id    = "image-container",
                    style = "position: relative; width: auto; height: 700px;
                             overflow: hidden; border: 0px solid #ccc;",
                    tags$img(id = "uploadedImage", src = "",
                             style = "position: absolute; top: 0; left: 0; cursor: crosshair")
                  ),
                  tags$script(HTML("
                    $(document).ready(function() {
                      let scale = 1, panX = 0, panY = 0;
                      let isPanning = false, startX, startY;
                      let imageHeight = null;

                      function updateImageTransform() {
                        $('#uploadedImage').css({
                          'transform-origin': 'top left',
                          'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
                        });
                        $('.dot').each(function() {
                          const ox = $(this).data('orig-x');
                          const oy = $(this).data('orig-y');
                          $(this).css({
                            left: (ox * scale + panX) + 'px',
                            top:  ((imageHeight - oy) * scale + panY) + 'px'
                          });
                        });
                      }

                      Shiny.addCustomMessageHandler('load-image', function(data) {
                        var img = document.getElementById('uploadedImage');
                        img.src = data.src;
                        img.onload = function() {
                          imageHeight = img.naturalHeight;
                          scale = 1; panX = 0; panY = 0;
                          updateImageTransform();
                          $('.dot').remove();
                        };
                      });

                      Shiny.addCustomMessageHandler('plot-coordinates', function(data) {
                        $('.dot').remove();
                        if (!imageHeight) return;
                        data.coords.forEach(function(coord) {
                          const dot     = $('<div class=\"dot\"></div>');
                          const dotSize = 10;
                          dot.css({
                            position        : 'absolute',
                            top             : ((imageHeight - coord.y) * scale + panY - dotSize/2 + 0.5) + 'px',
                            left            : (coord.x * scale + panX - dotSize/2 + 0.5) + 'px',
                            width           : dotSize + 'px',
                            height          : dotSize + 'px',
                            'background-color' : 'red',
                            'border-radius' : '50%',
                            'pointer-events': 'none',
                            'z-index'       : 10
                          });
                          dot.data('orig-x', coord.x);
                          dot.data('orig-y', coord.y);
                          $('#image-container').append(dot);
                        });
                        updateImageTransform();
                      });

                      Shiny.addCustomMessageHandler('apply-filters', function(cfg) {
                        const img = document.getElementById('uploadedImage');
                        if (!img) return;
                        const b  = (cfg.brightness || 100) / 100.0;
                        const c  = (cfg.contrast   || 100) / 100.0;
                        const g  = (cfg.gamma      || 100) / 100.0;
                        const gc = Math.max(0.5, Math.min(2.0, g));
                        const ec = 0.5 * (gc - 1) + 1;
                        const inv = cfg.invert ? ' invert(1)' : '';
                        img.style.filter = `contrast(${c * ec}) brightness(${b})` + inv;
                      });

                      $('#image-container').on('wheel', function(e) {
                        e.preventDefault();
                        const zi       = 0.1;
                        const delta    = e.originalEvent.deltaY > 0 ? -1 : 1;
                        const prevScale = scale;
                        scale = Math.min(Math.max(0.5, scale * (1 + delta * zi)), 5);
                        const mx = e.pageX - $(this).offset().left;
                        const my = e.pageY - $(this).offset().top;
                        panX = mx - (mx - panX) * (scale / prevScale);
                        panY = my - (my - panY) * (scale / prevScale);
                        updateImageTransform();
                      });

                      $('#image-container').on('mousedown', function(e) {
                        if (e.which === 3) {
                          isPanning = true;
                          startX = e.pageX - panX; startY = e.pageY - panY;
                          $(this).css('cursor', 'grabbing');
                          return false;
                        }
                      });
                      $(document).on('mouseup', function() {
                        isPanning = false;
                        $('#image-container').css('cursor', 'crosshair');
                      });
                      $(document).on('mousemove', function(e) {
                        if (!isPanning) return;
                        panX = e.pageX - startX; panY = e.pageY - startY;
                        updateImageTransform();
                      });
                      $('#image-container').on('contextmenu', function() { return false; });

                      $('#image-container').on('click', function(e) {
                        if (e.which !== 1) return;
                        const img  = document.getElementById('uploadedImage');
                        const rect = img.getBoundingClientRect();
                        const ax   = (e.clientX - rect.left)  / scale;
                        const ay   = (e.clientY - rect.top)   / scale;
                        Shiny.setInputValue('xray_click',
                          { x: ax - 1, y: imageHeight - ay + 1 },
                          { priority: 'event' });
                      });
                    });
                  "))
                )
              )
            ),
            
            # ── Final image view (after all points recorded) ───────────────────
            conditionalPanel(
              condition = "input.xray_file_uploaded == true & input.all_points_recorded == true",
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    id    = "image-plot-container",
                    style = "position: relative; width: auto; height: 700px;
                             overflow: hidden; border: 0px solid #ccc;",
                    tags$img(id = "uploadedImagePlot", src = "",
                             style = "position: absolute; top: 0; left: 0; cursor: crosshair")
                  ),
                  tags$script(HTML("
                    $(document).ready(function() {
                      let scale = 1, panX = 0, panY = 0;
                      let isPanning = false, startX, startY;
                      let imageHeight = null;

                      function updateImageTransform() {
                        $('#uploadedImagePlot').css({
                          'transform-origin': 'top left',
                          'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
                        });
                      }

                      Shiny.addCustomMessageHandler('load-plot-image', function(data) {
                        var img = document.getElementById('uploadedImagePlot');
                        img.src = data.src;
                        img.onload = function() {
                          const cw = $('#image-plot-container').width();
                          const ch = $('#image-plot-container').height();
                          imageHeight = img.naturalHeight;
                          scale  = Math.min(cw / img.naturalWidth, ch / imageHeight);
                          panX   = (cw - img.naturalWidth  * scale) / 2;
                          panY   = (ch - imageHeight        * scale) / 2;
                          $('#uploadedImagePlot').css({
                            'transform-origin': 'top left',
                            'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
                          });
                          $('.dot').remove();
                        };
                      });

                      $('#image-plot-container').on('wheel', function(e) {
                        e.preventDefault();
                        const zi       = 0.1;
                        const delta    = e.originalEvent.deltaY > 0 ? -1 : 1;
                        const prevScale = scale;
                        scale = Math.min(Math.max(0.5, scale * (1 + delta * zi)), 5);
                        const mx = e.pageX - $(this).offset().left;
                        const my = e.pageY - $(this).offset().top;
                        panX = mx - (mx - panX) * (scale / prevScale);
                        panY = my - (my - panY) * (scale / prevScale);
                        updateImageTransform();
                      });

                      $('#image-plot-container').on('mousedown', function(e) {
                        if (e.which === 3) {
                          isPanning = true;
                          startX = e.pageX - panX; startY = e.pageY - panY;
                          $(this).css('cursor', 'grabbing');
                          return false;
                        }
                      });
                      $(document).on('mouseup', function() {
                        isPanning = false;
                        $('#image-plot-container').css('cursor', 'crosshair');
                      });
                      $(document).on('mousemove', function(e) {
                        if (!isPanning) return;
                        panX = e.pageX - startX; panY = e.pageY - startY;
                        updateImageTransform();
                      });
                      $('#image-plot-container').on('contextmenu', function() { return false; });

                      $('#image-plot-container').on('click', function(e) {
                        if (e.which !== 1) return;
                        const img  = document.getElementById('uploadedImagePlot');
                        const rect = img.getBoundingClientRect();
                        const ax   = (e.clientX - rect.left)  / scale;
                        const ay   = (e.clientY - rect.top)   / scale;
                        Shiny.setInputValue('xray_plot_click',
                          { x: ax - 1, y: imageHeight - ay + 1 },
                          { priority: 'event' });
                      });
                    });
                  "))
                )
              )
            ),
            
            br(),
            
            # Image filter controls
            fluidRow(
              div(
                class = "inline-row",
                switchInput(inputId = "img_invert",
                            label = tagList(icon("circle-half-stroke"), "Invert"),
                            value = FALSE, size = "small", labelWidth = "80px"),
                actionButton("img_reset_filters", "Reset",
                             class = "btn btn-default btn-sm")
              ),
              box(
                title = "Adjust Image", collapsible = TRUE, collapsed = TRUE, width = 12,
                div(
                  class = "inline-sliders",
                  div(class = "inline-slider",
                      span(class = "inline-label", "Brightness"),
                      sliderInput("img_brightness", label = NULL,
                                  min = 50, max = 200, value = 100, post = "%", width = "220px")),
                  div(class = "inline-slider",
                      span(class = "inline-label", "Contrast"),
                      sliderInput("img_contrast", label = NULL,
                                  min = 50, max = 200, value = 100, post = "%", width = "220px")),
                  div(class = "inline-slider",
                      span(class = "inline-label", "Gamma"),
                      sliderInput("img_gamma", label = NULL,
                                  min = 50, max = 200, value = 100, post = "%", width = "220px"))
                )
              )
            ),
            
            # Delete / Reset point buttons
            fluidRow(
              column(6,
                     actionBttn(inputId = "xray_delete_last_point", block = TRUE, size = "md",
                                label = "Delete Last", style = "jelly", color = "success",
                                icon = icon("delete-left"))),
              column(6,
                     actionBttn(inputId = "xray_reset_points", block = TRUE, size = "md",
                                label = "Reset", style = "unite", color = "danger",
                                icon = icon("trash-can")))
            )
          )
        ),
        
        # ════════════════════════════════════════════════════════════════════════
        # COLUMN 2 — Planning & alignment panel
        # ════════════════════════════════════════════════════════════════════════
        tags$div(
          id    = "col-right",
          class = "col-sm-6 col-main",
          
          conditionalPanel(
            condition = "input.all_points_recorded == true",
            box(
              width = 12,
              
              # ── Export actions (only when rod is added) ───────────────────────
              conditionalPanel(
                condition = "input.add_rod == true",
                useSweetAlert(),
                fluidRow(
                  column(6,
                         div(style = "text-align: center;",
                             actionButton(
                               inputId = "email_plan", label = "Email Plan",
                               icon = icon("envelope"), 
                               # disabled = TRUE,
                               style = "font-size: 18px; padding: 12px 25px;
                         background-color: #28A745; color: white;
                         border: none; border-radius: 6px;
                         font-weight: bold; display: inline-block;"
                             )
                         )
                  ),
                  column(6,
                         div(style = "text-align: center;",
                             downloadButton(
                               outputId = "download_rod_template", label = "Download Rod Template",
                               icon = icon("download"),
                               style = "font-size: 18px; padding: 12px 25px;
                         background-color: #007BFF; color: white;
                         border: none; border-radius: 6px; font-weight: bold;
                         display: block; margin-left: 10px; margin-right: 10px;"
                             ),
                             tags$script(HTML("
                $(document).ready(function() {
                  $('#download_rod_template').on('click', function() {
                    setTimeout(function() {
                      Shiny.setInputValue('downloadFinished', Date.now());
                    }, 1000);
                  });
                });
              ")),
                             h5(p(em("Open in Adobe Acrobat & Print as 'Poster' at 100% Scale.")))
                         )
                  )
                )
              ),
              
              # ── Main content row ──────────────────────────────────────────────
              fluidRow(
                
                # Left: plot + table + downloads
                column(6,
                       h3("Preop & Planned Alignment"),
                       plotOutput(outputId = "preop_spine_simulation_plot",
                                  width = "auto", height = "750px"),
                       fluidRow(
                         column(7,
                                gt_output(outputId = "planning_parameters_table"),
                                br(), hr(),
                                box(
                                  width = 12,
                                  downloadButton("download_figure", "Download the Figure"),
                                  br(), br(),
                                  actionBttn(inputId = "add_to_powerpoint_button",
                                             label = "Add to Powerpoint", style = "gradient",
                                             size = "sm", color = "primary", icon = icon("plus")),
                                  br(), br(),
                                  downloadButton("download_ppt_slide", "Download PPT slide"),
                                  br(),
                                  textOutput("ppt_slide_counter")
                                )
                         ),
                         column(5,
                                div(style = "text-align: center;",
                                    switchInput(inputId = "add_rod", label = "Add Rod?",
                                                value = FALSE, onLabel = "Yes", offLabel = "No")
                                ),
                                conditionalPanel(
                                  condition = "input.add_rod == true",
                                  fluidRow(
                                    pickerInput(inputId = "rod_uiv", label = "UIV:",
                                                choices  = c("C2","C3","C4","C5","C6","C7",
                                                             "T1","T2","T3","T4","T5","T6",
                                                             "T7","T8","T9","T10","T11","T12",
                                                             "L1","L2","L3","L4"),
                                                selected = "T4", multiple = FALSE,
                                                options  = pickerOptions(container = "body"),
                                                width    = "100%"),
                                    pickerInput(inputId = "rod_liv", label = "LIV:",
                                                choices  = c("C6","C7","T1","T2","T3","T4","T5",
                                                             "T6","T7","T8","T9","T10","T11","T12",
                                                             "L1","L2","L3","L4","L5","S1","Pelvis"),
                                                selected = "Pelvis", multiple = FALSE,
                                                options  = pickerOptions(container = "body"),
                                                width    = "100%")
                                  ),
                                  fluidRow(
                                    # column(6, div(style = "display: flex; align-items: right; height: 100%;",
                                    #               strong("Rod Contouring:"))),
                                    column(12, 
                                           sliderInput(
                                             inputId = "rod_contouring",
                                             label   = "Rod Contouring:",
                                             min     = 0,
                                             max     = 100,
                                             value   = 80,
                                             step    = 5,
                                             post    = "%"
                                           )
                                           # numericInput("rod_knots", label = NULL, value = 6, min = 2)
                                           )
                                  )
                                )
                         )
                       )
                ),
                
                # Right: segmental adjustment controls
                column(6,
                       tags$script(HTML("
            $(document).ready(function() {
              $('.btn-group button').on('click', function() {
                Shiny.setInputValue($(this).attr('id'), Date.now());
              });
            });
          ")),
                       
                       box(title = "Cervical", width = 12, collapsible = TRUE, collapsed = TRUE,
                           id = "alignment_adjustment_box-box",
                           tags$table(width = "100%",
                                      map(.x = jh_spine_levels_factors_df$interspace[1:7],
                                          .f = ~ generate_spine_level_controls(spine_level = .x))
                           ),
                           fluidRow(
                             column(6,
                                    pickerInput(inputId = "cervical_pso", label = "Cervical PSO/VCR",
                                                choices  = c("C3","C4","C5","C6","C7"),
                                                multiple = TRUE,
                                                options  = pickerOptions(container = "body"),
                                                width    = "100%")),
                             column(6)
                           )
                       ),
                       
                       box(title = "Thoracic", width = 12, collapsible = TRUE, collapsed = FALSE,
                           id = "alignment_adjustment_box",
                           tags$table(width = "100%",
                                      map(.x = jh_spine_levels_factors_df$interspace[8:19],
                                          .f = ~ generate_spine_level_controls(spine_level = .x))
                           ),
                           fluidRow(
                             column(6,
                                    pickerInput(inputId = "thoracic_pso", label = "Thoracic PSO/VCR",
                                                choices  = c("T1","T2","T3","T4","T5","T6",
                                                             "T7","T8","T9","T10","T11","T12"),
                                                multiple = TRUE,
                                                options  = pickerOptions(container = "body"),
                                                width    = "100%")),
                             column(6)
                           )
                       ),
                       
                       box(title = "Lumbar", width = 12, collapsible = TRUE, collapsed = FALSE,
                           id = "alignment_adjustment_box",
                           tags$table(width = "100%",
                                      map(.x = jh_spine_levels_factors_df$interspace[20:24],
                                          .f = ~ generate_spine_level_controls(spine_level = .x))
                           ),
                           fluidRow(
                             column(6,
                                    pickerInput(inputId = "lumbar_pso", label = "Lumbar PSO",
                                                choices  = c("L1","L2","L3","L4","L5"),
                                                multiple = TRUE,
                                                options  = pickerOptions(container = "body"),
                                                width    = "100%")),
                             column(6, uiOutput("lumbar_pso_inputs"))
                           )
                       ),
                       
                       actionBttn(inputId = "segmental_planning_reset", block = TRUE, size = "md",
                                  label = "Reset", style = "unite", color = "danger",
                                  icon = icon("arrow-rotate-left")),
                       br(),
                       tableOutput("spine_segmental_planning_df"),
                       br(),
                       tableOutput("spine_segmental_planning_pso_df")
                )
              )
            )
          ),
          
          # ── Raw coordinate data debug box ─────────────────────────────────────────
          conditionalPanel(
            condition = "input.all_points_recorded == true",
            box(
              title = "Coordinate Data:", width = 12,
              collapsible = TRUE, collapsed = TRUE,
              tableOutput("alignment_parameters_df"),
              hr(),
              tableOutput("click_coordinates_df"),
              br(),
              textOutput("click_coordinates_text"),
              br(), hr(),
              h4("Centroid Coordinates:"),
              tableOutput("centroid_coordinates_df"),
              br(),
              h4("Spine Preop Alignment vert_df"),
              verbatimTextOutput("spine_preop_coord_df"),
              br(),
              h4("Spine Planned Alignment: 'spine_build_list_reactivevalues$planned_spine_list$vert_coord_df'"),
              # verbatimTextOutput("xray_centroid_tibble_text"),
              # spine_build_list_reactivevalues$planned_spine_list$vert_coord_df
              # hr(),
              verbatimTextOutput("spine_plan_tibble_text")
            )
          )
        ) # end col-right
      ) # end fluidRow
    ) # end conditionalPanel (xray uploaded)
  ) # end dashboardBody
) # end dashboardPage


############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################

# Server logic
server <- function(input, output, session) {
  
  observeEvent(input$all_points_recorded, {
    session$sendCustomMessage("resize-columns", list(expand = isTRUE(input$all_points_recorded)))
  })
  
  observeEvent(input$image, {
    req(input$image)
    updateSwitchInput(session = session, inputId = "xray_file_uploaded", value = TRUE)
  })
  
  # Helper to send current filter state
  send_filters <- function(){
    session$sendCustomMessage("apply-filters", list(
      brightness = input$img_brightness %||% 100,
      contrast   = input$img_contrast   %||% 100,
      gamma      = input$img_gamma      %||% 100,
      invert     = isTRUE(input$img_invert)
    ))
  }
  
  observe({
    req(input$xray_file_uploaded)  # only after image is up
    # Re-send whenever any control changes
    junk <- list(input$img_brightness, input$img_contrast, input$img_gamma, input$img_invert)
    send_filters()
  })
  
  observeEvent(input$img_reset_filters, {
    updateSliderInput(session, "img_brightness", value = 100)
    updateSliderInput(session, "img_contrast",   value = 100)
    updateSliderInput(session, "img_gamma",      value = 100)
    updateCheckboxInput(session, "img_invert",   value = FALSE)
    send_filters()
  })
  
  # After a new image loads, also apply the current filters (so state persists)
  observeEvent(input$xray_file_uploaded, {
    send_filters()
  }, ignoreInit = TRUE)

  
  spine_orientation <- reactiveVal("left")
  
  observeEvent(input$spine_orientation_button, ignoreInit = TRUE, {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      spine_orientation("right")
    } else {
      spine_orientation("left")
    }
  })
  
  observeEvent(spine_orientation(), {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
    } else {
      updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
    }
  })
  
  click_coord_reactive_list <- reactiveValues(coords = list(), index = 1)
  
  observeEvent(click_coord_reactive_list$coords, {
    if(length(click_coord_reactive_list$coords)==3){
      fem_head_x <- click_coord_reactive_list$coords$fem_head_center$x
      s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior$x
      s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior$x
      
      if(s1_anterior_superior_x < s1_posterior_superior_x){
        spine_orientation("left")
      }else{
        spine_orientation("right")
      }
    }
    
  }
  )
  
  
  
  
  observe({
    req(input$image)
    if(isTRUE(input$all_points_recorded) && 
          length(spine_build_list_reactivevalues$spine_build_list) > 0){
      req(input$image)  # Ensure there's an image uploaded
      
      xray <- image_scale(image_read(path = input$image$datapath), "400x")
      
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      
      # Generate the plot
      plot <- xray_reactive_plot()
      
      # Save the plot as a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      ggsave(temp_file, plot = plot, width = xray_width, height = xray_height, units = "px")
      
      
      img_scaled <- image_scale(image_read(temp_file), "400x")  # Scale to 400px width
      
      # Write the scaled image to a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      image_write(img_scaled, path = temp_file, format = "jpeg")
      
      # Encode the scaled image to base64
      img_base64 <- base64enc::dataURI(file = temp_file, mime = "image/jpeg")
      
      # print(paste("Sending load-plot-image message", "Xray height is ", xray_height, "Width is ", xray_width))  # Debugging log
      session$sendCustomMessage('load-plot-image', list(src = img_base64)) 
      
    }else{
      img_scaled <- image_scale(image_read(input$image$datapath), "400x")  # Scale to 400px width
      
      # Write the scaled image to a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      image_write(img_scaled, path = temp_file, format = "jpeg")
      
      # Encode the scaled image to base64
      image_src <- base64enc::dataURI(file = temp_file, mime = "image/jpeg")
      
      # Send the image URI to the UI
      session$sendCustomMessage('load-image', list(src = image_src)) 
    }
  })
  

  # Reset button to clear all points
  observeEvent(input$xray_reset_points, ignoreInit = TRUE, {
    click_coord_reactive_list$coords <- list()
    click_coord_reactive_list$index <- 1
  })
  
  # Button to remove the last recorded point
  observeEvent(input$xray_delete_last_point, ignoreInit = TRUE, {
    if (click_coord_reactive_list$index > 1) {
      click_coord_reactive_list$coords[[click_coord_reactive_list$index - 1]] <- NULL
      click_coord_reactive_list$index <- click_coord_reactive_list$index - 1
    }
    
    if(length(plot_points_coordinates_reactiveval())>0){
      plot_points_list <- plot_points_coordinates_reactiveval()
      
      plot_points_list <- plot_points_list[-length(plot_points_list)]
      
      plot_points_coordinates_reactiveval(plot_points_list)
      
    }
  })
  
  
  plot_points_coordinates_reactiveval <- reactiveVal()
  
  # Store clicks and assign them to the correct label
  observeEvent(list(input$xray_click), ignoreInit = TRUE, {
    spine_input_labels <- get_spine_labels(FALSE)
    
    # Only proceed if there's a label available for the current index
    if (click_coord_reactive_list$index <= length(spine_input_labels)) {
      target_name <- spine_input_labels[click_coord_reactive_list$index]
      # Create a named list with the click coordinates
      new_click <- list(x = input$xray_click$x, y = input$xray_click$y)
      
      # Append the new click as a named list with the label
      click_coord_reactive_list$coords[[target_name]] <- new_click
      click_coord_reactive_list$index <- click_coord_reactive_list$index + 1
    }
    
    # Debugging step to print the structure of click_coord_reactive_list$coords
    # print(str(click_coord_reactive_list$coords))
    # print("test")
    # print(names(click_coord_reactive_list$coords))
    
    # Convert the named list to a list of lists
    coords_for_js <- unname(lapply(click_coord_reactive_list$coords, function(coord) {
      list(x = coord$x, y = coord$y)
    }))
    
    # Debugging step to print coords_for_js
    # print(coords_for_js)
    
    plot_points_coordinates_reactiveval(coords_for_js)
    
  })
  
  observeEvent(list(input$xray_click, input$xray_delete_last_point, input$xray_reset_points), {
    
    session$sendCustomMessage('plot-coordinates', list(coords = plot_points_coordinates_reactiveval()))
  })
  
  
  
  xray_instructions_reactiveval <- reactiveVal("x")
  
  # Render instructions dynamically based on the number of recorded clicks
  output$xray_click_instructions <- renderText({
    spine_input_labels <- get_spine_labels()
    click_count <- length(click_coord_reactive_list$coords)

    if (click_count < length(spine_input_labels)) {
      instruction <- spine_input_labels[click_count + 1]
      
      instruction <- str_replace_all(instruction, "fem_head_center", "Center of Hips")
      instruction <- str_replace_all(instruction, "_superior", "_superior Corner")
      
      instruction <- str_to_title(str_replace_all(instruction, "_", " "))
      
      instruction <- glue("Click:<br>{instruction}")
      
      xray_instructions_reactiveval("x")
      
    } else {
      # print("incorrect instructions")
      
      instruction <- "All points recorded."
      xray_instructions_reactiveval("Completed")
      
      # print(paste(xray_instructions_reactiveval()))
    }
    
    HTML("<div>", instruction, "</div>")
  })
  
  ############ CALIBRATION #####################
  ############ CALIBRATION #####################
  ############ CALIBRATION #####################
  ############ CALIBRATION #####################
  
  # Show modal when calibration_2 is clicked or calibrate button is pressed
  observeEvent(list(input$xray_click, input$calibrate_button), ignoreInit = TRUE, {
    calibration_length_modal_function <- function(calibration_length = 100) {
      modalDialog(
        easyClose = TRUE,  
        size = "l",
        h3("Enter Length in mm"),
        fluidRow(
          column(width = 2),
          column(width = 6,
                 div(
                   id = "calibration-input-container",
                   numericInput(
                     inputId = "calibration_length", 
                     label   = "Length (mm):", 
                     value   = calibration_length,
                     min     = 10, 
                     max     = 1000
                   )
                 )
          )
        ),
        fluidRow(
          actionButton(
            inputId = "use_average_calibration",
            label   = "Use Average Height",
            icon    = icon("calculator"),
            style   = "background-color: #6C757D; color: white; border-radius: 6px;"
          )
        ),
        tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            if ($('#calibration_length').length > 0) {
              $('#calibration_length').focus();
              $('#calibration_length').on('keydown', function(e) {
                if(e.key === 'Enter') {
                  Shiny.setInputValue('calibration_enter_press', true, {priority: 'event'});
                }
              });
            } else {
              console.error('Element #calibration_length not found');
            }
          }, 300);
        });
      "))
      )
    }
    
    if (any(names(click_coord_reactive_list$coords) == "calibration_2")) {
      calibration_length <- if (length(input$calibration_length) == 0) 100 else input$calibration_length
      showModal(calibration_length_modal_function(calibration_length = calibration_length))
    }
  })
  
  
  # Close modal on Enter key
  observeEvent(input$calibration_enter_press, {
    removeModal()
  })
  
  
  
  calibration_list <- reactiveValues(calibration_modifier = NA)
  
  # Compute calibration modifier whenever the two points or entered length change
  observeEvent(list(input$calibration_length, click_coord_reactive_list$coords), ignoreInit = TRUE, {
    req(!is.null(input$calibration_length), !is.na(input$calibration_length))
    
    if (any(names(click_coord_reactive_list$coords) == "calibration_2")) {
      calibration_pixel_distance <- jh_calculate_distance_between_2_points_function(
        point_1 = c(click_coord_reactive_list$coords$calibration_1$x,
                    click_coord_reactive_list$coords$calibration_1$y),
        point_2 = c(click_coord_reactive_list$coords$calibration_2$x,
                    click_coord_reactive_list$coords$calibration_2$y)
      )
      calibration_list$calibration_modifier <- as.double(input$calibration_length) / calibration_pixel_distance
    }
  })
  
  observeEvent(input$use_average_calibration, {
    req(
      any(names(click_coord_reactive_list$coords) == "c2_centroid"),
      any(names(click_coord_reactive_list$coords) == "fem_head_center")
    )
    
    calibration_pixel_distance <- jh_calculate_distance_between_2_points_function(
      point_1 = c(click_coord_reactive_list$coords$c2_centroid$x,
                  click_coord_reactive_list$coords$c2_centroid$y),
      point_2 = c(click_coord_reactive_list$coords$fem_head_center$x,
                  click_coord_reactive_list$coords$fem_head_center$y)
    )
    
    calibration_list$calibration_modifier <- 610 / calibration_pixel_distance
  })
  
  output$calibration_status_text <- renderText({
    if (any(names(click_coord_reactive_list$coords) == "calibration_2")) {
      calibration_pixel_distance <- round(jh_calculate_distance_between_2_points_function(
        point_1 = c(click_coord_reactive_list$coords$calibration_1$x,
                    click_coord_reactive_list$coords$calibration_1$y),
        point_2 = c(click_coord_reactive_list$coords$calibration_2$x,
                    click_coord_reactive_list$coords$calibration_2$y)
      ), 4)
      
      paste0(
        "Pixel Distance = ", calibration_pixel_distance,
        " | Length Entered: ", input$calibration_length, " mm",
        " | Modifier: ", round(calibration_list$calibration_modifier, 4)
      )
    } else {
      "Calibration not yet recorded."
    }
  })
  
 
  
  
  
  
  ########### CALIBRATION END #######
  ########### CALIBRATION END #######
  ########### CALIBRATION END #######
  
  
  ###################### Generate Tables for viewing and copying the clicked coordinates as unscaled, unmodified coordinates  ###################### 
  ###################### Generate Tables for viewing and copying the clicked coordinates as unscaled, unmodified coordinates  ###################### 
  #### RECALL: obtain 'clicked' coordinate labels with 'get_spine_labels(FALSE)'
  
  # Create a reactive table to display coordinates
  click_coordinates_df_reactive <- reactive({
    if (length(click_coord_reactive_list$coords) > 0) {
      # Convert the list to a tibble
      click_coordinates_df_reactive_df <- tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(click_coord_reactive_list$coords, "x"),
        y = map_dbl(click_coord_reactive_list$coords, "y")
      )
      
      # print(dput(click_coordinates_df_reactive_df))

    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_df <- renderTable({
    plotting_coord_list <- plot_points_coordinates_reactiveval()
    
    if (length(plotting_coord_list) > 0) {
      # Convert the list to a tibble
      plotting_coord_df <- tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(plotting_coord_list, "x"),
        y = map_dbl(plotting_coord_list, "y")
      ) 

      plotting_coord_df 

    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_text <- renderText({

    if (length(plot_points_coordinates_reactiveval()) > 0) {
      # Convert the list to a tibble
      s1_center <- jh_get_point_along_line_function(coord_a = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y), 
                                                    coord_b = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y), 
                                                    percent_a_to_b = 0.5)
      
      # click_df <- tibble(spine_point = "s1_center", x = s1_center[1], y = s1_center[2]) %>%
      #   union_all(click_coordinates_df_reactive())
      
      click_df <- click_coordinates_df_reactive()
      
      spine_point_labels <- glue_collapse(click_df$spine_point, sep = "', '")
      
      x_values <- glue_collapse(click_df$x, sep = ", ")
      y_values <- glue_collapse(click_df$y, sep = ", ")
      
      glue("click_df <- tibble(spine_point = c('{spine_point_labels}'), x = c({x_values}), y = c({y_values}))")
      
      
    } else {
      glue("click_df <- tibble(spine_point)")
    }
  })
  
  
  
  ################## FIT SPLINE TO COMPUTE ALL VERTEBRAL CENTROIDS AND ALL VERTEBRAL BODY COORDINATES ######################
  ################## FIT SPLINE TO COMPUTE ALL VERTEBRAL CENTROIDS AND ALL VERTEBRAL BODY COORDINATES ######################
  ################## FIT SPLINE TO COMPUTE ALL VERTEBRAL CENTROIDS AND ALL VERTEBRAL BODY COORDINATES ######################
  ################## FIT SPLINE TO COMPUTE ALL VERTEBRAL CENTROIDS AND ALL VERTEBRAL BODY COORDINATES ######################

  spine_build_list_reactivevalues <- reactiveValues(
    spine_build_list = list(),
    planned_spine_list = list()
    # planned_spine_list = list(
    #   centroids_df = tibble(),
    #   vert_coord_df = tibble(),
    #   vert_coord_list = list(),
    #   interspace_coord_df = tibble(), #old
    #   interspace_list = list(), #old
    #   centroid_coord_list = list(), #old
    #   centroids_coord_list = list(),
    #   sa_adjustment_current_df = tibble()
    # )
  )
  
  # ── Build spine_build_list once calibration and points are ready ─────────────
  observeEvent(list(input$all_points_recorded, calibration_list$calibration_modifier), ignoreInit = TRUE, {
    req(input$all_points_recorded, !is.na(calibration_list$calibration_modifier))
    
    spine_build_list_reactivevalues$spine_build_list <- jh_calculate_scaled_centered_spine_coordinates_from_centroids_function(
      clicked_coord_df = click_coordinates_df_reactive(),
      spine_orientation = spine_orientation(),
      calibration_modifier = calibration_list$calibration_modifier
    )
    spine_build_list_reactivevalues$planned_spine_list <- spine_build_list_reactivevalues$spine_build_list
    
    spine_simulation_planning_reactive_list$c2_tilt_normal_df <- tibble(
      x = c(0),
      y = c(max(spine_build_list_reactivevalues$spine_build_list$vert_coord_df$y))
    )
    
  })
  
  # spine_coord_list_reactive <- reactive({
  #   
  #   req(input$all_points_recorded, !is.na(calibration_list$calibration_modifier))
  #   
  #   xray_clicked_coord_df <- click_coordinates_df_reactive()
  #   
  #   spine_coord_list <- jh_calculate_scaled_centered_spine_coordinates_from_centroids_function(
  #     clicked_coord_df = xray_clicked_coord_df,
  #     spine_orientation = spine_orientation(),
  #     calibration_modifier = calibration_list$calibration_modifier
  #   )
  #   # this list has the named objects: "centroids_df"    "vert_coord_list" "vert_coord_df"  
  #   # spine_coord_list$vert_coord_list has the columns: spine_level, vert_point, x, y
  #   ##### vert_point has the values in order: sp, ip, ia, sa, mid, mid_post
  #   
  #   return(spine_coord_list)
  # })
  
  
  # xray_centroid_coordinates_reactive_df <- reactive({
  # the above should be replaced with:
  # click_coordinates_df_reactive()
  
  
  output$centroid_coordinates_df <- renderTable({
    # spine_coord_list <- spine_coord_list_reactive()
    
    if(nrow(spine_build_list_reactivevalues$spine_build_list$centroids_df)>0){
      spine_build_list_reactivevalues$spine_build_list$centroids_df
    }
  })
  
  # output$xray_centroid_tibble_text <- renderText({

  
  output$spine_preop_coord_df <- renderText({
    
    req(spine_build_list_reactivevalues$spine_build_list$vert_coord_df)
    jh_format_text_to_print_tibble_in_shiny_function(df = spine_build_list_reactivevalues$spine_build_list$vert_coord_df)
    
  })
  
  output$spine_plan_tibble_text <- renderText({
    
    req(spine_build_list_reactivevalues$planned_spine_list$vert_coord_df)
    jh_format_text_to_print_tibble_in_shiny_function(df = spine_build_list_reactivevalues$planned_spine_list$vert_coord_df)
    
  })
  
  alignment_parameters_reactivevalues_list <- reactiveValues()
  
  actively_computing_parameters_reactive_list <- reactiveValues(alignment_df = tibble())
  
  
  
  observeEvent(list(click_coord_reactive_list$coords, spine_orientation()), ignoreInit = TRUE, {
    clicked_coord_df <- click_coordinates_df_reactive()
    
    clicked_coord_list <- mutate(clicked_coord_df, coord = map2(.x = x, .y = y, .f = ~ c(.x, .y)))$coord 
    names(clicked_coord_list) <- clicked_coord_df$spine_point
    
    # print(names(clicked_coord_list))
    
    if(all(c("s1_anterior_superior", "s1_posterior_superior") %in% names(clicked_coord_list))){
      s1_superior_center <- jh_get_point_along_line_function(coord_a = clicked_coord_list$s1_anterior_superior, coord_b = clicked_coord_list$s1_posterior_superior, percent_a_to_b = 0.5)
      
      s1_inferior_mid_point <- jh_find_sacrum_inf_point_function(s1_posterior_sup = clicked_coord_list$s1_posterior_superior,
                                                                 s1_anterior_sup = clicked_coord_list$s1_anterior_superior,
                                                                 inf_length_multiplier = 1,
                                                                 spine_facing =  spine_orientation()
      )
      
      pelvic_incidence_value <- jh_calculate_vertex_angle(vertex_coord = s1_superior_center, 
                                                          posterior_point_coord = s1_inferior_mid_point, 
                                                          ventral_point_coord = clicked_coord_list$fem_head_center,
                                                          spine_orientation = spine_orientation()
      )
      
      pelvic_tilt_value <- jh_calculate_vertex_angle(vertex_coord = clicked_coord_list$fem_head_center, 
                                                     posterior_point_coord = s1_superior_center, 
                                                     ventral_point_coord = c(clicked_coord_list$fem_head_center[1], s1_superior_center[2]),
                                                     spine_orientation = spine_orientation()
                                                     )
      
      actively_computing_parameters_reactive_list$alignment_df  <- tibble(measure = c("PI", "PT"), value = c(pelvic_incidence_value, pelvic_tilt_value))
      
    }
    
    
    vert_body_coordinates_df <- clicked_coord_df %>%
      filter(str_detect(spine_point, "centroid"))
    
    if(nrow(vert_body_coordinates_df)>0){
      
      v_tilt_df <- vert_body_coordinates_df %>%
        mutate(v_tilt = map2(.x = x, .y = y, 
                             .f = ~ jh_calculate_vertex_angle(posterior_point_coord = c(.x, .y),
                                                              ventral_point_coord = c(clicked_coord_list$fem_head_center[1], .y),
                                                              vertex_coord = clicked_coord_list$fem_head_center,
                                                              spine_orientation = spine_orientation()
                             )
        )) %>%
        unnest() 
      
      if(str_to_lower(spine_orientation()) == "left"){
        vpa_df <- v_tilt_df %>%
          mutate(v_tilt = if_else(x < clicked_coord_list$fem_head_center[1], abs(v_tilt), abs(v_tilt)*-1)) %>%
          mutate(value = pelvic_tilt_value + v_tilt) %>%
          mutate(measure = str_to_upper(str_replace_all(spine_point, "_centroid", "pa"))) %>%
          select(measure, value)
      }else{
        vpa_df <- v_tilt_df %>%
          mutate(v_tilt = if_else(x < clicked_coord_list$fem_head_center[1], abs(v_tilt)*-1, abs(v_tilt)))%>%
          mutate(value = pelvic_tilt_value + v_tilt) %>%
          mutate(measure = str_to_upper(str_replace_all(spine_point, "_centroid", "pa"))) %>%
          select(measure, value)
      }
      
      actively_computing_parameters_reactive_list$alignment_df <- actively_computing_parameters_reactive_list$alignment_df %>%
        union_all(vpa_df)
      
      
    }
  })
  
  observeEvent(list(input$xray_click,
                    spine_orientation()), ignoreInit = TRUE, {
                      
                      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
                      
                      if((any(names(alignment_parameters_list) == "pelvic_incidence") == FALSE) & nrow(filter(click_coordinates_df_reactive(), str_detect(spine_point, "s1")))>1){
                        fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
                        
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        #   ### COMPUTE PT ###
                        fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                 point_2 = s1_center) ## hypotenuse
                        
                        fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                   point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
                        
                        # pt_orientation_modifier <- case_when(
                        #   spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
                        #   spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
                        #   spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
                        #   spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
                        # )
                        pt_orientation_modifier <- case_when(
                          spine_orientation() == "left"  & fem_head_center[[1]] > s1_center[[1]] ~ 1,   # facing left, normal PT
                          spine_orientation() == "left"  & fem_head_center[[1]] < s1_center[[1]] ~ -1,  # facing left, anteverted
                          spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ 1,   # facing right, normal PT
                          spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ -1   # facing right, anteverted
                        )
                        
                        alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
                        
                        print(paste("PT: ", alignment_parameters_reactivevalues_list$pelvic_tilt, "and.... PT Modifier: ", pt_orientation_modifier))
                        
                        ### COMPUTE SS ###
                        s1_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                                                     point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x,
                                                                                                   click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                                                       point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
                        
                        ### COMPUTE PI ###
                        alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
                        
                      }
                      
                      ## COMPUTE ALL VPAs ##
                      if(any(click_coordinates_df_reactive()$spine_point == "c2_centroid")){
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        vpa_df <- click_coordinates_df_reactive() %>%
                          filter(spine_point != "s1_center") %>%
                          mutate(vpa = map2(.x = x, .y = y, 
                                            .f = ~ jh_calculate_vertex_angle(posterior_point_coord = s1_center,
                                                                             ventral_point_coord = c(.x, .y),
                                                                             vertex_coord = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                                             spine_orientation = spine_orientation()
                                            )
                          )) %>%
                          unnest() %>%
                          mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
                          select(vpa_label, vpa)
                        
                        vpa_list <- as.list(vpa_df$vpa)
                        names(vpa_list) <- vpa_df$vpa_label
                        
                        for (name in names(vpa_list)) {
                          alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
                        }
                      }
                      
                    }
  )
  
  
  
  # ### MAYBE IS OK:
  # observeEvent(spine_orientation(), ignoreInit = TRUE, {
  # 
  #   alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
  # 
  #   if((any(names(alignment_parameters_list) == "pelvic_incidence") == TRUE)){
  #     fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
  # 
  #     s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
  #                    (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
  # 
  #     #   ### COMPUTE PT ###
  #     fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
  #                                                                              point_2 = s1_center) ## hypotenuse
  # 
  #     fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
  #                                                                                point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
  # 
  #     pt_orientation_modifier <- case_when(
  #       spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
  #       spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
  #       spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
  #       spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
  #     )
  # 
  #     alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
  # 
  #     ### COMPUTE SS ###
  #     s1_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
  #                                                                  point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
  # 
  #     s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x,
  #                                                                                click_coord_reactive_list$coords$s1_posterior_superior$y),
  #                                                                    point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
  # 
  #     alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
  # 
  #     ### COMPUTE PI ###
  #     alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
  # 
  #   }
  # 
  #   ## COMPUTE ALL VPAs ##
  #   if(any(click_coordinates_df_reactive()$spine_point == "c2_centroid")){
  #     s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
  #                    (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
  # 
  #     vpa_df <- click_coordinates_df_reactive() %>%
  #       filter(spine_point != "s1_center") %>%
  #       mutate(vpa = map2(.x = x, .y = y,
  #                         .f = ~ jh_calculate_vertex_angle(posterior_point_coord = s1_center,
  #                                                          ventral_point_coord = c(.x, .y),
  #                                                          vertex_coord = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
  #                                                          spine_orientation = spine_orientation()
  #                         )
  #       ))%>%
  #       unnest() %>%
  #       mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
  #       select(vpa_label, vpa)
  # 
  #     vpa_list <- as.list(vpa_df$vpa)
  #     names(vpa_list) <- vpa_df$vpa_label
  # 
  #     for (name in names(vpa_list)) {
  #       alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
  #     }
  #   }
  # 
  # }
  # )
  
  output$alignment_parameters_df_text <- renderText({

    glue_collapse(reactiveValuesToList(alignment_parameters_reactivevalues_list), sep = " \n")

  })
  
  output$alignment_parameters_df <- renderTable({
    
    enframe(reactiveValuesToList(alignment_parameters_reactivevalues_list)) %>%
      mutate(name = str_replace_all(name, "pelvic_tilt", "PT")) %>%
      mutate(name = str_replace_all(name, "pelvic_incidence", "PI")) %>%
      mutate(name = str_replace_all(name, "sacral_slope", "SS")) %>%
      mutate(name = str_to_upper(name))
    
  })
  
  observeEvent(list(click_coordinates_df_reactive(), calibration_list$calibration_modifier), ignoreInit = TRUE, {
    
    required_points <- get_spine_labels()
    clicked_coord_df <- click_coordinates_df_reactive()
    
    all_points_recorded <- length(required_points) > 0 &&
      all(required_points %in% clicked_coord_df$spine_point) &&
      !anyNA(clicked_coord_df$x[clicked_coord_df$spine_point %in% required_points]) &&
      !anyNA(clicked_coord_df$y[clicked_coord_df$spine_point %in% required_points]) &&
      !is.na(calibration_list$calibration_modifier)
    
    updateSwitchInput(session = session,
                      inputId = "all_points_recorded",
                      value = all_points_recorded)
  })
  
  
  
  # spine_build_list_reactivevalues <- reactiveValues(spine_build_list = list(),
  #                                                   planned_spine_list = list(vert_coord_df = tibble(),
  #                                                                             vert_coord_list = list(),
  #                                                                             interspace_coord_df = tibble(),
  #                                                                             interspace_list = list(),
  #                                                                             centroid_coord_list = list(),
  #                                                                             sa_adjustment_current_df = tibble()))
  # 

  # observeEvent(spine_coord_list_reactive(), once = TRUE, {
  #   spine_build_list_reactivevalues$spine_build_list <- spine_coord_list_reactive()
  # })
  
 
  
  
  ############ THIS IS THE ANNOTATED XRAY THAT GETS SHOWN ONCE input$all_points_recorded is TRUE.IT gets sent via: session$sendCustomMessage('load-plot-image', list(src = img_base64))  ##############
  xray_reactive_plot <- reactive({

    req(
      input$all_points_recorded,
      length(spine_build_list_reactivevalues$spine_build_list) > 0,
      !is.null(spine_build_list_reactivevalues$spine_build_list$centroids_df)
    )
      
      xray <- image_scale(image_read(path = input$image$datapath), "400x")
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      # The transformation is: new = (original * calibration_modifier) - fem_head_original_coord * calibration_modifier
      # Equivalently: new = original * calibration_modifier + translation
      # So: scaled = original * k + t
      
      # ORIGINAL:
      fem_head_original_coord <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
      c2_centroid_original_coord <-  c(click_coord_reactive_list$coords$c2_centroid$x, click_coord_reactive_list$coords$c2_centroid$y)
      # NEW SCALED:
      fem_head_scaled_coord <- c(0,0)
      c2_centroid_scaled_coord <- spine_build_list_reactivevalues$spine_build_list$centroids_coord_list$c2_centroid
      
      
      # Compute k (scale factor) from any two known point pairs
      k <- sqrt(sum((c2_centroid_scaled_coord - fem_head_scaled_coord)^2)) / 
        sqrt(sum((c2_centroid_original_coord - fem_head_original_coord)^2))
      
      # Compute translation: t = scaled - original * k
      t_vec <- fem_head_scaled_coord - fem_head_original_coord * k
      
      
      # Image corners in original pixel coords
      img_origin_original <- c(0, 0)
      
      # Image corners mapped to scaled coordinate space
      img_x_scaled <- c(0 * k + t_vec[1], xray_width * k + t_vec[1])
      img_y_scaled <- c(0 * k + t_vec[2], xray_height * k + t_vec[2])
      

      
      spine_colors_df <-  spine_build_list_reactivevalues$spine_build_list$centroids_df %>%
        mutate(spine_color = case_when(
          str_detect(spine_level, "c") ~ "lightblue",
          str_detect(spine_level, "t") ~ "lightgreen",
          str_detect(spine_level, "l") ~ "darkblue",
          str_detect(spine_level, "s") ~ "purple"
        )
        )
      

      s1_center_coord <- spine_build_list_reactivevalues$spine_build_list$centroids_coord_list$s1_superior_center
      l1_coord <- spine_build_list_reactivevalues$spine_build_list$centroids_coord_list$l1_centroid
      t4_coord <- spine_build_list_reactivevalues$spine_build_list$centroids_coord_list$t4_centroid
      
      # print(paste("s1_center_coord:", s1_center_coord))
      # print(paste("l1_coord:", l1_coord))
      # print(paste("t4_coord:", t4_coord))
      
      req(!is.null(s1_center_coord), !is.null(l1_coord), !is.null(t4_coord))
      
      
      s1_inf_pi_point <- jh_get_point_along_line_function(coord_a = s1_center_coord, 
                                                          coord_b = spine_build_list_reactivevalues$spine_build_list$vert_coord_list$s1$ia,
                                                          percent_a_to_b = 3)
      

      pi_df <- tibble(spine_point = c("fem_head", 
                                      's1_center', 
                                      's1_inferior_mid'),
                      x = c(0, 
                            s1_center_coord[1], 
                            s1_inf_pi_point[1]
                            ),
                      y = c(0, 
                            s1_center_coord[2], 
                            s1_inf_pi_point[2]))
      
      l1pa_df <- tibble(spine_point = c("s1_center", 
                                        'fem_head',
                                        'l1_centroid'),
                        x = c(s1_center_coord[1], 
                              0, 
                              l1_coord[1]),
                        y = c(spine_build_list_reactivevalues$spine_build_list$centroids_coord_list$s1_superior_center[2], 
                              0,
                              l1_coord[2]))
      
      t4pa_df <- tibble(spine_point = c("s1_center", 'fem_head', 't4_centroid'),
                        x = c(s1_center_coord[1], 
                              0, 
                              t4_coord[1]),
                        y = c(s1_center_coord[2], 
                              0,
                              t4_coord[2])
                        )
      
      
      
      xray_plot <- ggdraw(xlim = img_x_scaled, ylim = img_y_scaled) +
        draw_image(
          xray,
          x     = img_x_scaled[1],
          y     = img_y_scaled[1],
          width = diff(img_x_scaled),
          height = diff(img_y_scaled), clip = FALSE
        )+
        geom_path(data = spine_build_list_reactivevalues$spine_build_list$centroids_df,
                  aes(x = x, y = y), color = "lightblue", size = 0.2) +
        geom_point(data = spine_colors_df,
                   aes(x = x, y = y, color = spine_color, fill = spine_color),size = 0.2) +
        geom_path(data = pi_df,
                  aes(x = x, y = y), color = "darkgreen", size = 0.25) +
        geom_path(data = l1pa_df,
                  aes(x = x, y = y),
                  color = "darkblue", size = 0.25)+
        geom_path(data = t4pa_df,
                  aes(x = x, y = y),
                  color = "purple", size = 0.25) +
        scale_fill_identity() +
        scale_color_identity()+
        coord_fixed(
          xlim   = img_x_scaled,
          ylim   = img_y_scaled,
          expand = FALSE,
          clip   = "off"
        )
      xray_plot

    
    }
  )

  
  
  
  # observeEvent(input$xray_plot_click, {
  #   spine_xr_build_list <-  spine_build_list_reactivevalues$spine_build_list
  #   
  #   # Assume these are the clicked coordinates from the plot
  #   clicked_x <- input$xray_plot_click$x
  #   clicked_y <- input$xray_plot_click$y
  #   
  #   
  #   if(length(spine_xr_build_list)>0){
  #     
  #     nearest_point <- spine_xr_build_list$spine_coord_df %>%
  #       rowwise() %>%
  #       mutate(distance = sqrt((x - clicked_x)^2 + (y - clicked_y)^2)) %>%
  #       ungroup() %>%
  #       arrange(distance) %>%
  #       slice(1) 
  #     
  #     spine_level_to_mod <- nearest_point$spine_level[[1]]
  #     spine_point_to_mod <- nearest_point$vert_point[[1]]
  #     # if(nrow())
  #     spine_xr_build_list$vert_coord_list[[spine_level_to_mod]][[spine_point_to_mod]] <- c(clicked_x,clicked_y)
  #     
  #     # spine_build_list_reactivevalues$spine_build_list$vert_coord_list[[nearest_point$spine_level[[1]]]][[nearest_point$vert_point[[1]]]] <- c(nearest_point$x[[1]], nearest_point$y[[1]])
  #     
  #     new_geom <- jh_construct_vert_polygon_from_coordinates_list_function(vert_list = spine_xr_build_list$vert_coord_list[[spine_level_to_mod]], 
  #                                                                          buffer_amount = spine_build_list_reactivevalues$spine_build_list$buffer_amount)
  #     
  #     spine_xr_build_list$vert_geom_list[[spine_level_to_mod]] <- new_geom 
  #     spine_build_list_reactivevalues$spine_build_list <- spine_xr_build_list
  #     
  #     new_vert_point_coord_df <- tibble(spine_level = spine_level_to_mod, vert_point = spine_point_to_mod, x = clicked_x, y = clicked_y)
  #     
  #     spine_build_list_reactivevalues$spine_build_list$spine_coord_df <- spine_build_list_reactivevalues$spine_build_list$spine_coord_df %>% 
  #       rows_upsert(new_vert_point_coord_df, by = c("spine_level", "vert_point"))
  #     
  #   }else{
  #     # print(names(spine_build_list), "Reactive values to list results with names: names(reactiveValuesToList(spine_build_list_reactivevalues))", names(reactiveValuesToList(spine_build_list_reactivevalues)))
  #   }
  #   
  # })
  
  
  output$xray_plot_click_coordinates <- renderTable({
    spine_xr_build_list <-  spine_build_list_reactivevalues$spine_build_list
    # print(paste0(input$xray_plot_click))
    # coord_clicked <- paste("X:", click_coords$x, "Y:", click_coords$y)
    
    xray_click_tibble <- tibble(contents_of_list = names(spine_xr_build_list))
    
    if(length(spine_xr_build_list)>0){
      # Assume these are the clicked coordinates from the plot
      
      clicked_x <- input$xray_plot_click$x
      clicked_y <- input$xray_plot_click$y
      
      nearest_point <- spine_xr_build_list$spine_coord_df %>%
        rowwise() %>%
        mutate(distance = sqrt((x - clicked_x)^2 + (y - clicked_y)^2)) %>%
        ungroup() %>%
        arrange(distance) %>%
        slice(1)
      
      xray_click_tibble <- nearest_point %>%
        select(spine_level, vert_point, x, y) %>%
        mutate(x_click = clicked_x,
               y_click = clicked_y) %>%
        pivot_longer(cols = c(x, y, x_click, y_click), names_to = "coord", values_to = "val")
      # xray_click_tibble <- nearest_point
    }
    
    # print(xray_click_tibble)
    xray_click_tibble
  }) %>%
    bindEvent(input$xray_plot_click)
  
  
  
  
  output$preop_xray_rigid_segments_ui <- renderUI({
    # preop_segment_angles_list <- preop_segment_angles_list_reactive()
    
    segment_angles_input_list <- rev(map(spinal_segments_labels_vector, function(label) {
      create_spine_rigid_level_input_function(segment_input_label = label)
    }))
    # create_spine_rigid_level_input_function
    
    # column(width = 5,
    fluidRow(
      column(width = 12,
             tags$div(
               "Select Any Fused Levels:",
               style = "font-size: 12pt; font-style: italic; color: black; text-align: center;"
             ),
             # h4("Select Any Fused Levels:"),
             box(width = 12,
                 title = "Cervical", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 # h5("Check box if Rigid Level"),
                 segment_angles_input_list[1:6] %>% tagList()
             ),
             box(width = 12,title = "Thoracic", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 # h5("Check box if Rigid Level"),
                 segment_angles_input_list[7:19] %>% tagList()
             ),
             box(width = 12,title = "Lumbar", 
                 collapsible = TRUE, 
                 collapsed = FALSE,
                 # h5("Check box if Rigid Level"),
                 segment_angles_input_list[20:24]%>% tagList()
             )
      )
    )
  })
  
  preop_rigid_levels_vector_reactive_xray <- reactive({
    # segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_rigid")
    rigid_levels <- c("na")
    if(input$preop_l5_s1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L5-S1")} 
    if(input$preop_l4_l5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L4-L5")} 
    if(input$preop_l3_l4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L3-L4")} 
    if(input$preop_l2_l3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L2-L3")} 
    if(input$preop_l1_l2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L1-L2")} 
    if(input$preop_t12_l1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T12-L1")} 
    if(input$preop_t11_t12_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T11-T12")} 
    if(input$preop_t10_t11_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T11")} 
    if(input$preop_t9_t10_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T9")} 
    if(input$preop_t8_t9_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T8-T9")} 
    if(input$preop_t7_t8_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T7-T8")} 
    if(input$preop_t6_t7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T6-T7")} 
    if(input$preop_t5_t6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T5-T6")} 
    if(input$preop_t4_t5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T4-T5")} 
    if(input$preop_t3_t4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T3-T4")} 
    if(input$preop_t2_t3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T2-T3")} 
    if(input$preop_t1_t2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T1-T2")} 
    if(input$preop_c7_t1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C7-T1")} 
    if(input$preop_c6_c7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C6-C7")} 
    if(input$preop_c5_c6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C5-C6")} 
    if(input$preop_c4_c5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C4-C5")} 
    if(input$preop_c3_c4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C3-C4")} 
    if(input$preop_c2_c3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C2-C3")} 
    if(input$preop_c1_c2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C1-C2")} 
    # set_names(map_chr(spinal_segments_labels_vector, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment_angle")))
    if(length(rigid_levels)>0){
      rigid_levels <- map_chr(rigid_levels, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment"))
    }
    rigid_levels
  })
  
  
  
  # Initialize a reactiveValues dataframe
  spine_segmental_planning_df <- reactiveValues(
    df = tibble(
      spine_interspace = rev(jh_spine_levels_factors_df$interspace),
      adjustment = rep(0, length(jh_spine_levels_factors_df$interspace))
    ), 
    pso_df = tibble(level = character(), adjustment = numeric())
  )
  
  map(.x = jh_spine_levels_factors_df$interspace, 
      .f = ~ update_spine_segmental_planning_table_observe_button_function(spine_segmental_planning_df,
                                                                           spine_interspace = .x, session = session))
  
  
  # Optional: Display the updated table in UI for debugging
  output$spine_segmental_planning_df <- renderTable({
    
    adjustment_df <- spine_segmental_planning_df$df %>%
      filter(adjustment != 0) %>%
      select("Level" = spine_interspace,
             "Adjustment" = adjustment)

    adjustment_df
    
    
  })
  
  output$spine_segmental_planning_pso_df <- renderTable({
    
    if(nrow(spine_segmental_planning_df$pso_df)>0){
      spine_segmental_planning_df$pso_df %>%
      select("PSO Level" = level, "Angle" = adjustment)
    }
    
    
    
  })
  
  
  observeEvent(input$segmental_planning_reset, ignoreInit = TRUE, {
    spine_segmental_planning_df$df <- spine_segmental_planning_df$df %>%
      mutate(adjustment = 0)
    
    spine_build_list_reactivevalues$planned_spine_list  <- spine_build_list_reactivevalues$spine_build_list
    
    # spine_simulation_planning_reactive_list$planned_geom <- NULL
    
  })
  
  output$lumbar_pso_inputs <- renderUI({
    req(input$lumbar_pso)  # Ensure there's a selection before rendering
    
    # Generate numericInputs dynamically using purrr::map
    map(input$lumbar_pso, ~ numericInput(
      inputId = paste0(tolower(.x), "_pso_value"), 
      label = paste(.x, "PSO Value"), 
      value = 30, 
      min = 0, 
      max = 50
    ))
  })
  
  ####### PSO LIST ###########

  pso_values <- reactive({
    pso_vector <- c(input$lumbar_pso, input$thoracic_pso, input$cervical_pso)
    
    if (length(pso_vector) == 0) return(NULL)
    
    # Extract values dynamically from input
    setNames(
      map(pso_vector, ~ input[[paste0(tolower(.x), "_pso_value")]] %||% 30), 
      pso_vector
    )
  })
  
  # Observe both changes in selections and numeric input values
  observe({
    req(pso_values())  # Ensure there's data before updating
    
    spine_segmental_planning_df$pso_df <- enframe(pso_values()) %>%
      unnest() %>%
      select(level = name, adjustment = value)
  })


  
  
  spine_simulation_planning_reactive_list <- reactiveValues(
    predicted_pt      = NULL,
    rod_plot          = NULL,
    rod_coord_df      = tibble(),
    c2_tilt_normal_df = tibble(x = c(0), y = c(0))
  )
  
  
  
  # ── Build planned spine when segmental adjustments change ────────────────────
  observeEvent(list(spine_segmental_planning_df$df, spine_segmental_planning_df$pso_df), ignoreInit = TRUE, {
    req(
      input$all_points_recorded,
      length(spine_build_list_reactivevalues$spine_build_list) > 0
    )

    has_adjustments <- any(spine_segmental_planning_df$df$adjustment != 0) ||
      nrow(spine_segmental_planning_df$pso_df) > 0
    
    if(has_adjustments){
      pso_adjustment_list <- if(nrow(filter(spine_segmental_planning_df$pso_df, !is.na(adjustment)))>0){
        lst <- as.list(spine_segmental_planning_df$pso_df$adjustment)
        names(lst) <- str_to_lower(spine_segmental_planning_df$pso_df$level)
        lst
      } else {
        list()
      }
      
      # pso_adjustment_list <- as.list(spine_segmental_planning_df$pso_df$adjustment)
      # names(pso_adjustment_list) <- str_to_lower(spine_segmental_planning_df$pso_df$level)
      
      spine_build_list_reactivevalues$planned_spine_list <- jh_construct_adjusted_spine_list_function(
        segment_angle_adjustment_df = spine_segmental_planning_df$df,
        pso_list                    = pso_adjustment_list,
        spine_list                  = spine_build_list_reactivevalues$spine_build_list,
        spine_orientation           = spine_orientation(),
        adjust_for_pt_change        = TRUE
      )
      # print("made it to 2143")
      
      hypotenuse_length <- max(spine_build_list_reactivevalues$planned_spine_list$vert_coord_df$y)
      
      spine_simulation_planning_reactive_list$c2_tilt_normal_df <- tibble(
        x = c(0, sin(4*pi/180)*hypotenuse_length, sin(1*pi/180)*hypotenuse_length),
        y = c(0, hypotenuse_length, hypotenuse_length)
      )
    } else {
      spine_build_list_reactivevalues$planned_spine_list <- list(
        vert_coord_df      = tibble(),
        vert_coord_list    = list(),
        interspace_coord_df = tibble(),
        interspace_list    = list(),
        centroids_coord_list = list(),
        sa_adjustment_current_df = tibble()
      )
      
      spine_simulation_planning_reactive_list$c2_tilt_normal_df <- tibble(
        x = c(0),
        y = c(max(spine_build_list_reactivevalues$spine_build_list$vert_coord_df$y))
      )
    }
  })
  
  
  # ── Rod construction ─────────────────────────────────────────────────────────
  observeEvent(list(input$add_rod, input$rod_uiv, input$rod_liv, input$rod_contouring), ignoreInit = TRUE, {
    if(input$add_rod){
      if(nrow(spine_segmental_planning_df$pso_df) > 0){
        pso_vec <- spine_segmental_planning_df$pso_df$level
      }else{
        pso_vec <- character()
      }
      # print(pso_vec)
      
      spine_simulation_planning_reactive_list$rod_coord_df <- jh_construct_rod_coordinates_function(
        planned_spine_coord_df = spine_build_list_reactivevalues$planned_spine_list$vert_coord_df,
        uiv                    = input$rod_uiv,
        liv                    = input$rod_liv,
        pso_vector = pso_vec,
        spine_orientation      = spine_orientation(),
        contouring_percent        = input$rod_contouring
      )
      # print(paste(dput(spine_simulation_planning_reactive_list$rod_coord_df)))
    } else {
      spine_simulation_planning_reactive_list$rod_coord_df <- tibble()
    }
  })
  
  # ── Reset ────────────────────────────────────────────────────────────────────
  observeEvent(input$segmental_planning_reset, ignoreInit = TRUE, {
    spine_segmental_planning_df$df <- spine_segmental_planning_df$df %>%
      mutate(adjustment = 0)
    spine_segmental_planning_df$pso_df <- tibble(level = character(), adjustment = numeric())
  })
  
  # observeEvent(list(input$add_rod, input$rod_uiv, input$rod_liv, input$rod_contouring), ignoreInit = TRUE, {
  #   if(input$add_rod){
  #     print("reacted correctly")
  #     spine_simulation_planning_reactive_list$rod_coord_df <- jh_construct_rod_coordinates_function(planned_spine_coord_df = spine_build_list_reactivevalues$planned_spine_list$vert_coord_df,
  #                                                                                                   uiv = input$rod_uiv, 
  #                                                                                                   liv = input$rod_liv,
  #                                                                                                   spine_orientation = spine_orientation(),
  #                                                                                                   number_of_knots = input$rod_contouring)
  #     
  #     print("completed the function")
  #     
  #     spine_simulation_planning_reactive_list$rod_geom <- geom_path(data = spine_simulation_planning_reactive_list$rod_coord_df,
  #                                                                   aes(x = x, y = y),
  #                                                                   color = "blue",
  #                                                                   size = 2,
  #                                                                   lineend = "round",
  #                                                                   linejoin = "round")
  #     
  #     
  #     spine_simulation_planning_reactive_list$rod_plot <- ggplot() +
  #       spine_simulation_planning_reactive_list$rod_geom +
  #       theme_minimal_grid()
      
      # ############## generate rod plot for printing ###################
      # 
      # spine_levels <- c("pelvis", "sacrum", "l5", "l4", "l3", "l2", "l1",
      #                   "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
      #                   "c7", "c6", "c5", "c4", "c3", "c2", "c1")
      # 
      # # Ensure spine_level is a factor with the correct order
      # instrumented_vert_centered_df <- adjusted_spine_list$vert_coord_df %>%
      #   mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
      #   group_by(spine_level) %>%
      #   filter(spine_level >= str_to_lower(liv) & spine_level <= str_to_lower(uiv)) %>%
      #   mutate(x = x - test_rod$x[[1]])%>%
      #   mutate(y = y - test_rod$y[[1]])
      # 
      # rod_coord_centered_df <- spine_simulation_planning_reactive_list$rod_coord_df %>%
      #   mutate(x = x - spine_simulation_planning_reactive_list$rod_coord_df$x[[1]])%>%
      #   mutate(y = y - spine_simulation_planning_reactive_list$rod_coord_df$y[[1]])
      # 
      # spine_simulation_planning_reactive_list$rod_coord_centered_for_template_df <- rod_coord_centered_df
      # 
      # x_range <- range(rod_coord_centered_df$x)
      # if(diff(x_range) > 49){
      #   x_grid_breaks <- 50
      # }else{
      #   x_grid_breaks <- 25
      # }
      # 
      # if(diff(range(rod_coord_centered_df$y)) > 199){
      #   y_grid_breaks <- 100
      # }else{
      #   y_grid_breaks <- 50
      # }
      # 
      # spine_simulation_planning_reactive_list$rod_plot_template <- ggplot() +
      #   geom_path(data = rod_coord_centered_df,
      #             aes(x = x, y = y),
      #             color = "blue",
      #             size = 2,
      #             lineend = "round",
      #             linejoin = "round") +
      #   jh_theme_function(axis_text_size = 9, axis_title_size = 10) +
      #   coord_fixed() +
      #   ylab("mm") + 
      #   xlab("mm") +
      #   scale_x_continuous(breaks = seq(floor(x_range[1] / x_grid_breaks) * x_grid_breaks, ceiling(x_range[2] / x_grid_breaks) * x_grid_breaks, by = x_grid_breaks)) +
      #   scale_y_continuous(breaks = seq(0, max(rod_coord_centered_df$y), by = y_grid_breaks)) +
      #   theme(
      #     panel.grid.major = element_line(linetype = "dashed", color = "grey50"),
      #     panel.grid.minor = element_blank()
      #   )
  #     
  #   }else{
  #     spine_simulation_planning_reactive_list$rod_geom <- NULL
  #     
  #     spine_simulation_planning_reactive_list$rod_plot <- ggplot() +
  #       spine_simulation_planning_reactive_list$rod_geom +
  #       theme_minimal_grid()
  #     
  #   }
  #   
  # })
  
  
  # ── Main plot reactive ────────────────────────────────────────────────────────
  preop_spine_simulation_plot_reactive <- reactive({
    req(
      length(spine_build_list_reactivevalues$spine_build_list) > 0,
      !is.null(spine_build_list_reactivevalues$spine_build_list$vert_coord_df),
      nrow(spine_build_list_reactivevalues$spine_build_list$vert_coord_df) > 0
    )
    
    spine_build_list   <- spine_build_list_reactivevalues$spine_build_list
    planned_spine_list <- spine_build_list_reactivevalues$planned_spine_list
    sim_list           <- spine_simulation_planning_reactive_list
    c2_tilt_df         <- sim_list$c2_tilt_normal_df
    rod_coord_df       <- sim_list$rod_coord_df
    
    has_adjustments <- any(spine_segmental_planning_df$df$adjustment != 0) ||
      nrow(spine_segmental_planning_df$pso_df) > 0
    
    # has_planned <- nrow(planned_spine_list$vert_coord_df) > 0
    # has_planned <- planned_spine_list$vert_coord_df == spine_build_list$vert_coord_df
    has_rod     <- nrow(rod_coord_df) > 0
    
    # print(paste("has_adjustments", has_adjustments))
    # print(paste("has_planned", has_planned))
    # print(paste("has_rod", has_rod))
    # has_planned <- FALSE
    # has_rod <- FALSE
    
    # ── Compute geoms locally ───────────────────────────────────────────────────
    preop_geom <- if(has_adjustments){
      jh_construct_spine_geom_sf_function(
        vert_coord_list = spine_build_list$vert_coord_list,
        baseline_spine  = TRUE,
        fade_baseline_spine = TRUE
      )$spine_geom_sf
    } else {
      jh_construct_spine_geom_sf_function(
        vert_coord_list = spine_build_list$vert_coord_list,
        baseline_spine  = TRUE
      )$spine_geom_sf
    }
    
    planned_geom <- if(has_adjustments){
      jh_construct_spine_geom_sf_function(
        vert_coord_list = planned_spine_list$vert_coord_list,
        baseline_spine  = FALSE
      )$spine_geom_sf
    } else NULL
    
    c2_tilt_geom <- if(nrow(c2_tilt_df) > 1){
      geom_polygon(data = c2_tilt_df, aes(x = x, y = y), fill = "green", alpha = 0.2)
    } else NULL
    
    rod_geom <- if(has_rod){
      geom_path(data = rod_coord_df, aes(x = x, y = y),
                color = "blue", size = 2, lineend = "round", linejoin = "round")
    } else NULL
    
    l1pa_geom <- if(has_adjustments){
      jh_construct_vpa_line_geoms_from_vert_coord_function(
        vertebral_level    = "l1",
        centroid_coord_list = planned_spine_list$centroids_coord_list,
        line_color = "blue", line_size = 0.5)
    } else NULL
    
    # print(paste("l1pa_geom class:", class(l1pa_geom)))
    # print(paste("l1pa_geom length:", length(l1pa_geom)))
    
    t4pa_geom <- if(has_adjustments){
      jh_construct_vpa_line_geoms_from_vert_coord_function(
        vertebral_level    = "t4",
        centroid_coord_list = planned_spine_list$centroids_coord_list,
        line_color = "purple", line_size = 0.5)
    } else NULL
    
    c2pa_geom <- if(has_adjustments){
      jh_construct_vpa_line_geoms_from_vert_coord_function(
        vertebral_level    = "c2",
        centroid_coord_list = planned_spine_list$centroids_coord_list,
        line_color = "darkgreen", line_size = 0.5)
    } else NULL
    
    # ── Limits ──────────────────────────────────────────────────────────────────
    xrange     <- diff(range(spine_build_list$vert_coord_df$x))
    xmin_limit <- min(spine_build_list$vert_coord_df$x) - xrange * 0.1
    xmax_limit <- max(spine_build_list$vert_coord_df$x) + xrange * 0.1
    
    if(has_adjustments){
      xmin_limit <- min(xmin_limit, min(planned_spine_list$vert_coord_df$x) - xrange * 0.1)
      xmax_limit <- max(xmax_limit, max(planned_spine_list$vert_coord_df$x) + xrange * 0.1)
    }
    if(has_rod){
      xmin_limit <- min(xmin_limit, min(rod_coord_df$x) - xrange * 0.1)
      xmax_limit <- max(xmax_limit, max(rod_coord_df$x) + xrange * 0.1)
    }
    
    xlimits <- c(xmin_limit, xmax_limit)
    
    # ── Femoral head ─────────────────────────────────────────────────────────────
    fem_head_radius <- jh_calculate_distance_between_2_points_function(
      point_1 = spine_build_list$vert_coord_list$s1$sa,
      point_2 = spine_build_list$vert_coord_list$s1$sp
    ) * 0.5
    femoral_head_circle_sf <- st_buffer(st_sfc(st_point(c(0, 0))), dist = fem_head_radius)
    
    # ── Labels ───────────────────────────────────────────────────────────────────

    if(has_adjustments){
      vert_labels_df <- enframe(planned_spine_list$centroids_coord_list) %>%
        unnest_wider(value, names_sep = "_") %>%
        select(spine_level = name,  x = value_1, y = value_2) %>%
        filter(!spine_level %in% c("c2", "s1", "femoral_head")) %>%
        mutate(spine_level = str_to_upper(spine_level))
    }else{
    vert_labels_df <- spine_build_list$centroids_df %>%
      filter(!spine_level %in% c("c2", "s1")) %>%
      mutate(spine_level = str_to_upper(spine_level))
    }
    
    
    # ── Shared theme ──────────────────────────────────────────────────────────────
    add_if <- function(geom) if(!is.null(geom)) geom else NULL
    
    shared_theme <- list(
      theme_void(),
      xlim(xlimits),
      coord_sf(),
      # coord_fixed(),
      theme(
        axis.title     = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)
      )
    )
    
    # ── plot_with_preop_faded ─────────────────────────────────────────────────────
    plot_faded <- ggplot() +
      add_if(c2_tilt_geom) +
      preop_geom +
      add_if(planned_geom) +
      geom_sf(data = femoral_head_circle_sf, fill = "grey90") +
      add_if(l1pa_geom) +
      add_if(t4pa_geom) +
      add_if(c2pa_geom) +
      add_if(rod_geom) +
      shared_theme
    
    if(nrow(vert_labels_df) > 0){
      plot_faded <- plot_faded +
        draw_text(text = vert_labels_df$spine_level,
                  x = vert_labels_df$x, y = vert_labels_df$y,
                  size = 9, hjust = 0.5)
    }
    
    # ── plot_simple ───────────────────────────────────────────────────────────────
    plot_simple <- ggplot() +
      geom_sf(data = femoral_head_circle_sf, fill = "grey90") +
      shared_theme
    
    if(!has_adjustments){
      plot_simple <- plot_simple +
        add_if(c2_tilt_geom) +
        preop_geom
    } else {
      plot_simple <- plot_simple +
        add_if(planned_geom) +
        add_if(l1pa_geom) +
        add_if(t4pa_geom) +
        add_if(c2pa_geom) +
        add_if(rod_geom)
    }
    
    list(
      plot_with_preop_faded = plot_faded,
      plot_simple           = plot_simple
    )
  })
  
  output$preop_spine_simulation_plot <- renderPlot({
    preop_spine_simulation_plot_reactive()$plot_with_preop_faded
  })
  
  
  ### alignment planning table ###
  
  output$planning_parameters_table <- render_gt({
    req(
      length(spine_build_list_reactivevalues$spine_build_list) > 0,
      !is.null(alignment_parameters_reactivevalues_list$pelvic_incidence)
    )
    alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)

      preop_vector <- c(alignment_parameters_list$pelvic_incidence, 
                        alignment_parameters_list$pelvic_tilt, 
                        alignment_parameters_list$l1pa, 
                        alignment_parameters_list$t9pa,
                        alignment_parameters_list$t4pa, 
                        alignment_parameters_list$c2pa)
      
      
      
      # planned_vector <- if(length(spine_build_list_reactivevalues$planned_spine_list$centroids_coord_list) > 0){
      #   vpa_list <- jh_calculate_vertebral_tilt_and_vpas_from_coordinates_function(
      #     full_centroid_coord_list = spine_build_list_reactivevalues$planned_spine_list$centroids_coord_list,
      #     spine_orientation        = spine_orientation()
      #   )
      #   paste0(round(c(
      #     alignment_parameters_list$pelvic_incidence,
      #     vpa_list$pt_computed,
      #     vpa_list$vpa_list$l1,
      #     vpa_list$vpa_list$t9,
      #     vpa_list$vpa_list$t4,
      #     vpa_list$vpa_list$c2
      #   ), 1), "º")
      # } else {
      #   rep("-", 6)
      # }
      planned_vector <- if(
        !is.null(spine_build_list_reactivevalues$planned_spine_list$centroids_coord_list) &&
        length(spine_build_list_reactivevalues$planned_spine_list$centroids_coord_list) > 0 &&
        any(spine_segmental_planning_df$df$adjustment != 0) || 
        nrow(spine_segmental_planning_df$pso_df) > 0
      ){
        vpa_list <- jh_calculate_vertebral_tilt_and_vpas_from_coordinates_function(
          full_centroid_coord_list = spine_build_list_reactivevalues$planned_spine_list$centroids_coord_list,
          spine_orientation        = spine_orientation()
        )
        paste0(round(c(
          alignment_parameters_list$pelvic_incidence,
          vpa_list$pt_computed,
          vpa_list$vpa_list$l1,
          vpa_list$vpa_list$t9,
          vpa_list$vpa_list$t4,
          vpa_list$vpa_list$c2
        ), 1), "º")
      } else {
        rep("-", 6)
      }
      
      tibble(
        Measure = c("PI", "PT", "L1PA", "T9PA", "T4PA", "C2PA"),
        Preop   = paste0(round(preop_vector, 1), "º"),
        Planned = planned_vector
      ) %>%
        gt() %>%
        tab_options(table.width = px(200), table.font.size = px(12), data_row.padding = px(0)) %>%
        cols_label(Measure = md("**Measure**"), Preop = "Preop", Planned = "Planned") %>%
        tab_style(
          style     = list(cell_text(weight = "bold", align = "center")),
          locations = cells_column_labels(columns = everything())
        ) %>%
        tab_style(
          style     = list(cell_text(align = "center")),
          locations = cells_body(columns = c("Preop", "Planned"))
        ) %>%
        tab_style(
          style     = list(cell_text(weight = "bold", align = "right")),
          locations = cells_body(columns = "Measure")
        )
      
  })
  
  
  
  output$download_rod_template <- downloadHandler(
    filename = function() "rod_plot_to_scale.pdf",
    content  = function(file) {
      generate_rod_template_pdf_function(
        planned_vert_coord_df = spine_build_list_reactivevalues$planned_spine_list$vert_coord_df,
        rod_coord_df          = spine_simulation_planning_reactive_list$rod_coord_df,
        rod_uiv               = input$rod_uiv,
        rod_liv               = input$rod_liv,
        file                  = file
      )
    }
  )
  
  # output$download_rod_template <- downloadHandler(
  # 
  #   filename = function() {
  #     "rod_plot_to_scale.pdf"
  #   },
  # 
  #   content = function(file) {
  #     # Get the reactive plot
  # 
  #     ############## generate rod plot for printing ###################
  # 
  #     spine_levels <- c("pelvis", "s1", "l5", "l4", "l3", "l2", "l1",
  #                       "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
  #                       "c7", "c6", "c5", "c4", "c3", "c2", "c1")
  # 
  #     # Ensure spine_level is a factor with the correct order
  #     instrumented_vert_centered_df <- spine_build_list_reactivevalues$planned_spine_list$vert_coord_df  %>%
  #       mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
  #       group_by(spine_level) %>%
  #       filter(spine_level >= str_to_lower(input$rod_liv) & spine_level <= str_to_lower(input$rod_uiv)) %>%
  #       mutate(x = x - spine_simulation_planning_reactive_list$rod_coord_df$x[[1]])%>%
  #       mutate(y = y - spine_simulation_planning_reactive_list$rod_coord_df$y[[1]])
  # 
  #     rod_coord_centered_df <- spine_simulation_planning_reactive_list$rod_coord_df %>%
  #       mutate(x = x - spine_simulation_planning_reactive_list$rod_coord_df$x[[1]])%>%
  #       mutate(y = y - spine_simulation_planning_reactive_list$rod_coord_df$y[[1]])
  # 
  #     # # Calculate the actual plot range after centering
  #     x_range_centered <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
  #     y_range_centered <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
  #     
  #     y_grid_breaks <- 50
  #     
  #     y_grid_max <- ceiling(y_range_centered[[2]]/ y_grid_breaks) * y_grid_breaks
  #     y_grid_min <- floor(y_range_centered[[1]]/ y_grid_breaks) * y_grid_breaks
  #     x_grid_max <- ceiling(x_range_centered[[2]]/ 20) * 20
  #     x_grid_min <- floor(x_range_centered[[1]]/ 20) * 20
  # 
  #     grid_corners_df <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
  #     
  #     y_gridline_tibble_df <- tibble(x_min = x_grid_min, x_max = x_grid_max, y = seq(from = y_grid_min, to = y_grid_max, by = 50))
  # 
  #     x_gridline_tibble_df <- tibble(x = seq(from = x_grid_min, to = x_grid_max, by = 40), y_min = y_grid_min, y_max = y_grid_min + 25)%>%
  #       slice(-1)
  #     
  #     spine_levels_labels_df <- instrumented_vert_centered_df %>%
  #       select(spine_level, x, y) %>%
  #       group_by(spine_level) %>%
  #       mutate(center_x = mean(x), center_y = mean(y)) %>%
  #       mutate(spine_level = str_to_upper(spine_level)) %>%
  #       select(spine_level, center_x, center_y) %>%
  #       ungroup() %>%
  #       distinct() 
  #     
  #     rod_plot_template <- ggplot() +
  #       geom_polygon(data = instrumented_vert_centered_df, aes(x = x, y = y, group = spine_level), color = "grey50", fill = NA, alpha = 0.3) +
  #       geom_segment(data = y_gridline_tibble_df, aes(x = x_min, xend = x_max, y = y, yend = y), color = "grey75", linetype = "dashed")+ 
  #       draw_text(text = paste0(as.character(y_gridline_tibble_df$y), "mm"), x = y_gridline_tibble_df$x_min + 20, y = y_gridline_tibble_df$y, size = 7) +
  #       geom_segment(data = x_gridline_tibble_df, aes(x = x, xend = x, y = y_min, yend = y_max), color = "grey75", linetype = "dashed")+ 
  #       draw_text(text = paste0(as.character(x_gridline_tibble_df$x), "mm"),
  #                 x = x_gridline_tibble_df$x, y = x_gridline_tibble_df$y_max, size = 7) +
  #       draw_text(text = spine_levels_labels_df$spine_level, x = spine_levels_labels_df$center_x, y = spine_levels_labels_df$center_y, size = 8) +
  #       geom_path(data = rod_coord_centered_df,
  #                 aes(x = x, y = y),
  #                 color = "blue",
  #                 size = 2,
  #                 lineend = "round",
  #                 linejoin = "round") +
  #       geom_point(data = grid_corners_df, aes(x =x, y = y), color = "red") +
  #       coord_fixed(expand = FALSE, xlim = c(x_grid_min, x_grid_max), ylim = c(y_grid_min, y_grid_max)) +
  #       theme_void()
  #     
  #     plot_width_mm <- diff(c(x_grid_min, x_grid_max))  # Width in mm
  #     plot_height_mm <- diff(c(y_grid_min, y_grid_max)) # Height in mm
  #     
  #     # Convert mm to inches (1 inch = 25.4 mm)
  #     plot_width_in <- plot_width_mm / 25.4
  #     plot_height_in <- plot_height_mm / 25.4
  # 
  #     # Save as PDF with exact dimensions
  #     ggsave(file,
  #            plot = rod_plot_template,
  #            device = cairo_pdf,
  #            width = plot_width_in,  # Convert mm to cm
  #            height = plot_height_in,
  #            units = "in",
  #            dpi = 300,
  #            limitsize = FALSE)
  # 
  #   }
  # )
  


  # 2) When the browser signals "downloadFinished", show a SweetAlert
  observeEvent(input$downloadFinished, {
    sendSweetAlert(
      session = session,
      title = "Download Complete",
      text = h2(p(em("To Print: Open in Adobe Acrobat & Print as 'Poster' at 100% Scale."))),
      type = "success"
    )
  })
  
  
  
  #### EMAIL PLAN #####
  
  observeEvent(input$email_plan, {
    showModal(modalDialog(
      title = "Email Your Rod Template",
      textInput("email_address", "Enter Email Address:", placeholder = "example@domain.com"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("send_email", "Send Email", icon = icon("paper-plane"),
                     style = "background-color: #17A2B8; color: white;")
      )
    ))
  })
  
  observeEvent(input$send_email, {
    req(input$email_address)
    
    pdf_file <- tempfile(fileext = ".pdf")
    
    generate_rod_template_pdf_function(
      planned_vert_coord_df = spine_build_list_reactivevalues$planned_spine_list$vert_coord_df,
      rod_coord_df          = spine_simulation_planning_reactive_list$rod_coord_df,
      rod_uiv               = input$rod_uiv,
      rod_liv               = input$rod_liv,
      file                  = pdf_file
    )
    
    tryCatch({
      blastula::compose_email(body = "Your requested rod template is attached.") %>%
        blastula::add_attachment(file = pdf_file) %>%
        blastula::smtp_send(
          to          = input$email_address,
          from        = "align@solaspine.com",
          subject     = "Rod Template from SOLASpine",
          credentials = email_credentials
        )
      removeModal()
      showNotification("Email successfully sent!", type = "message", duration = 5)
    }, error = function(e){
      showNotification(paste("Email failed:", e$message), type = "error", duration = 10)
    })
  })
  
  # observeEvent(input$send_email, {
  #   req(input$email_address)
  #   
  #   # Temporary PDF file creation
  #   pdf_file <- tempfile(fileext = ".pdf")
  # 
  #   # YOUR PDF generation code here
  #   spine_levels <- c("pelvis", "sacrum", "l5", "l4", "l3", "l2", "l1",
  #                     "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
  #                     "c7", "c6", "c5", "c4", "c3", "c2", "c1")
  #   
  #   instrumented_vert_centered_df <- spine_build_list_reactivevalues$planned_spine_list$vert_coord_df  %>%
  #     mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
  #     group_by(spine_level) %>%
  #     filter(spine_level >= str_to_lower(input$rod_liv) & spine_level <= str_to_lower(input$rod_uiv)) %>%
  #     mutate(x = x - spine_simulation_planning_reactive_list$rod_coord_df$x[[1]]) %>%
  #     mutate(y = y - spine_simulation_planning_reactive_list$rod_coord_df$y[[1]])
  #   
  #   rod_coord_centered_df <- spine_simulation_planning_reactive_list$rod_coord_df %>%
  #     mutate(x = x - spine_simulation_planning_reactive_list$rod_coord_df$x[[1]]) %>%
  #     mutate(y = y - spine_simulation_planning_reactive_list$rod_coord_df$y[[1]])
  #   
  #   x_range_centered <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
  #   y_range_centered <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
  #   
  #   y_grid_breaks <- 50
  #   
  #   y_grid_max <- ceiling(y_range_centered[[2]]/ y_grid_breaks) * y_grid_breaks
  #   y_grid_min <- floor(y_range_centered[[1]]/ y_grid_breaks) * y_grid_breaks
  #   x_grid_max <- ceiling(x_range_centered[[2]]/ 20) * 20
  #   x_grid_min <- floor(x_range_centered[[1]]/ 20) * 20
  #   
  #   grid_corners_df <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
  #   
  #   y_gridline_tibble_df <- tibble(x_min = x_grid_min, x_max = x_grid_max, y = seq(from = y_grid_min, to = y_grid_max, by = 50))
  #   
  #   x_gridline_tibble_df <- tibble(x = seq(from = x_grid_min, to = x_grid_max, by = 40), y_min = y_grid_min, y_max = y_grid_min + 25) %>%
  #     slice(-1)
  #   
  #   rod_plot_template <- ggplot() +
  #     geom_polygon(data = instrumented_vert_centered_df, aes(x = x, y = y, group = spine_level), color = "grey50", fill = NA, alpha = 0.3) +
  #     geom_segment(data = y_gridline_tibble_df, aes(x = x_min, xend = x_max, y = y, yend = y), color = "grey75", linetype = "dashed")+ 
  #     draw_text(text = paste0(as.character(y_gridline_tibble_df$y), "mm"), x = y_gridline_tibble_df$x_min + 20, y = y_gridline_tibble_df$y, size = 7) +
  #     geom_segment(data = x_gridline_tibble_df, aes(x = x, xend = x, y = y_min, yend = y_max), color = "grey75", linetype = "dashed")+ 
  #     draw_text(text = paste0(as.character(x_gridline_tibble_df$x), "mm"),
  #               x = x_gridline_tibble_df$x, y = x_gridline_tibble_df$y_max, size = 7) +
  #     geom_path(data = rod_coord_centered_df,
  #               aes(x = x, y = y),
  #               color = "blue",
  #               size = 2,
  #               lineend = "round",
  #               linejoin = "round") +
  #     geom_point(data = grid_corners_df, aes(x =x, y = y), color = "red") +
  #     coord_fixed(expand = FALSE, xlim = c(x_grid_min, x_grid_max), ylim = c(y_grid_min, y_grid_max)) +
  #     theme_void()
  #   
  #   plot_width_mm <- diff(c(x_grid_min, x_grid_max))  
  #   plot_height_mm <- diff(c(y_grid_min, y_grid_max))
  #   
  #   plot_width_in <- plot_width_mm / 25.4
  #   plot_height_in <- plot_height_mm / 25.4
  #   
  #   ggsave(pdf_file,
  #          plot = rod_plot_template,
  #          device = cairo_pdf,
  #          width = plot_width_in,
  #          height = plot_height_in,
  #          units = "in",
  #          dpi = 300,
  #          limitsize = FALSE)
  #   
  #   # # Compose email
  #   # email <- blastula::compose_email(
  #   #   body = "Your requested rod template is attached.",
  #   #   attachments = pdf_file
  #   # )
  #   email <- blastula::compose_email(
  #     body = "Your requested rod template is attached."
  #   ) %>%
  #     blastula::add_attachment(file = pdf_file)
  #   
  #   # create_smtp_creds_key(
  #   #   id = "email_creds",
  #   #   user = "align@solaspine.com",
  #   #   host = "smtp.zoho.com", 
  #   #   port = 465, 
  #   #   use_ssl = TRUE
  #   # )
  #   
  #   # Send email
  #   blastula::smtp_send(
  #     email,
  #     to = input$email_address,
  #     from = "align@solaspine.com",
  #     subject = "Rod Template from SOLASpine",
  #     # credentials = blastula::creds_file("/srv/shiny-server/alignment_planning/.blastula_email_creds")
  #     credentials = blastula::creds_key("email_creds")
  #     # credentials = blastula::creds_file("/home/ubuntu/.blastula_email_creds")
  #     # credentials = blastula::creds_file("/srv/shiny-server/alignment_planning/.blastula_email_creds")
  #     
  #   )
  #   # blastula::smtp_send(
  #   #   email,
  #   #   to = input$email_address,
  #   #   from = "align@solaspine.com",
  #   #   subject = "Rod Template from SOLA Spine",
  #   #   credentials = blastula::creds_file("/home/ubuntu/.blastula_email_creds")
  #   # )
  #   
  #   removeModal()
  #   
  #   showNotification("Email successfully sent!", type = "message", duration = 5)
  # })
  
  
  
  ####   #### CREATE POWERPOINT OF FIGURES   ####   #### 
  ####   #### CREATE POWERPOINT OF FIGURES   ####   #### 
  ####   #### CREATE POWERPOINT OF FIGURES   ####   #### 
  
  spine_powerpoint_list <- reactiveVal(list())
  
  observeEvent(input$add_to_powerpoint_button, {
    
    plot_to_add <- preop_spine_simulation_plot_reactive()$plot_simple
    
    if (inherits(plot_to_add, "ggplot")) {
      saved_plots <- spine_powerpoint_list()
      saved_plots[[length(saved_plots) + 1]] <- plot_to_add
      spine_powerpoint_list(saved_plots)
    }
    
    # spine_plots_saved <- spine_powerpoint_list()
    # spine_plots_saved[[length(spine_plots_saved) + 1]] <- preop_spine_simulation_plot_reactive()$plot_simple
    # 
    # spine_powerpoint_list(spine_plots_saved)
    
  })
  
  output$ppt_slide_counter <-   renderText({
    paste("PPT Slides = ", length(spine_powerpoint_list()))
  })
  
  
  
  output$download_ppt_slide <- downloadHandler(
    filename = function() { paste("spine_figure.pptx") },
    content = function(file) {
      ppt <- read_pptx()
      
      # Loop through saved plots and add each as a separate slide
      # for (plot in spine_powerpoint_list()) {
      #   ppt <- ppt %>%
      #     add_slide(layout = "Blank", master = "Office Theme") %>%
      #     ph_with(dml(ggobj = plot), location = ph_location_fullsize())
      # }
      for (plot in spine_powerpoint_list()) {
        if (inherits(plot, "ggplot")) {
          ppt <- ppt %>%
            add_slide(layout = "Blank", master = "Office Theme") %>%
            ph_with(dml(ggobj = plot), location = ph_location_fullsize())
        }
      }
      
      # Save PowerPoint file
      print(ppt, target = file)
    }
  )
  
  output$download_figure <- downloadHandler(filename = function(){paste("spine_figure.svg")},
                                            content = function(figure){
                                              ggsave(filename = figure, 
                                                     plot = preop_spine_simulation_plot_reactive()$plot_simple +theme_void(),
                                                     units = "in",
                                                     width = 8,
                                                     height = 14,
                                                     dpi = 300,
                                                     device = "svg")
                                            })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
