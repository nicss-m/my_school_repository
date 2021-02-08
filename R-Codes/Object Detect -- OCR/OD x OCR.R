library(shiny)
library(magick)
library(shinyjs)
library(shinybusy)
library(tesseract)
library(shinythemes)
library(image.darknet)

havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

writeLines("Executing program...")

writeLines("checking packages...")
# check and install packages if unavailable
needed.packages = c('image.darknet','magick','shiny','shinythemes','tesseract','shinybusy','shinyjs')
unavailable.packages = needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]

# loop to individually check unavailable packages since image.darknet is not available on cran we will use a repository link
if(length(unavailable.packages)){
  writeLines(c("Some missing packages detected...",paste("missing package: ",unavailable.packages)))
  confirmation = readline(prompt = "Install missing packages? type 'yes' for confirmation: ")
  if(confirmation=='yes' || confirmation=='YES' || confirmation=='Yes'){
    confirm_connection = havingIP()
    if(havingIP()==T){
      writeLines("installing packages...")
      for( package in unavailable.packages){
        if (package=='image.darknet'){
          install.packages(package, repos = "https://bnosac.github.io/drat")
        }
        install.packages(package)
      }
      writeLines("Packages confirmed")
    }else{
      stop("Sorry, your not connected to the internet, please connect to the internet and try again.\nsystem exits..")
    }
  }else{
    stop("unconfirmed.. package missing.. system exits..")
  }
}else{
  writeLines("Packages confirmed")
}

writeLines("Launching App...")

# where data is shown and visualization are served
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel("DIP LEARNING EVIDENCE"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1 || input.tabselected==2 || input.tabselected==3", fileInput("file","Please Upload an Image", multiple = T),h6("Default max. file size is 5MB")),
      conditionalPanel(condition = "input.tabselected==3", selectInput("slcInput", "Select Language for Text Extraction", choices = tesseract_info()$available),actionButton("add_lang", "Add Language"),actionButton("save_text", "Save Text")),
      conditionalPanel(condition = "input.tabselected==1 || input.tabselected==2 || input.tabselected==3",h6("Powered by:"),tags$img(src='RStudio-Ball.png', height=50, width=50))
      
      ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Original Image", value = 1, uiOutput("imgOutput")),
                  tabPanel("Object Detection", value = 2, uiOutput("objectDetect")),
                  tabPanel("Text Extraction", value = 3, uiOutput("processingOutput")),
                  id = "tabselected"
      
      )
      
  )
)
)

# where data is manipulated and visualizations are prepared
server <- shinyServer(function(input, output, session){
  
  md <- reactiveValues(
    names = tesseract_info()$available
  )
  
  observe({
    updateSelectInput(session, "slcInput",
                      label = "Select Language for Text Extraction",
                      choices = md$names)
  })
  
  # file saving confirm dialog
  observeEvent(input$save_text, {
    showModal(modalDialog(
      tagList(
        textInput("NewTextFile", label = "Filename", placeholder = "e.g. my_text_file")
      ), 
      title="Save File",
      footer = tagList(actionButton("confirmSave", "Save"),
                       modalButton("Cancel")
      )
    ))
  })
  
  # download new language
  observeEvent(input$add_lang, {
    url_tesseract <- a("Tesseract OCR", href="https://github.com/tesseract-ocr/tessdata")
    showModal(modalDialog(
      tagList(tags$h5("Note: Adding language will refresh the page")),
      tagList("Please refer to this link to see the available languages in tesseract OCR",url_tesseract),
      tagList(
        textInput("language", label = "Language name is in 3 letter ISO code", placeholder = "e.g. eng")
      ), 
      title="Download Language",
      footer = tagList(actionButton("confirmDownload", "Download"),
                       modalButton("Cancel")
      )
    ))
  })
  
  # download language confirm dialog
  observeEvent(input$confirmSave, {
    req(input$NewTextFile)
    if(is.null(input$file$datapath)){
      showNotification("No file to save", type = 'warning')
    }
    else{
      file_name = input$file$datapath
      img = image_read(file_name)
      lang = tesseract(input$slcInput)
      text_lang = ocr(img,engine = lang)
      write(" ", file = paste0(input$NewTextFile,".txt"))
      writeLines(text_lang, paste0(input$NewTextFile,".txt"), useBytes = T)
      print(paste0("File saved At: ",getwd(),input$NewTextFile,".txt"))
      showNotification(paste0("File saved At: ",getwd(),"/",input$NewTextFile,".txt"), type = 'message')
      removeModal()
    }
    
  })
  # download language tesseract
  observeEvent(input$confirmDownload, {
    req(input$language)
    show_modal_spinner() 
    tryCatch({
      tesseract_download(input$language)
      remove_modal_spinner()
      showNotification(paste0("Language ",input$language," added"), duration = 3, type = 'message')
      showNotification(paste0("Refreshing Page To Add Language.."), duration = 5, type = 'message')
      delay(7000, refresh())
      
    },
    warning = function(warn){
      remove_modal_spinner()
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      remove_modal_spinner()
      showNotification(paste0(err), type = 'err')
    })
    removeModal()
  })
  
  # view original image
  output$imgOutput <- renderUI(
    if(is.null(input$file)){}
    else{
      file_name = input$file$datapath
      if(grepl("\\.pdf$", file_name) || grepl("\\.ai$", file_name)){
        img = image_read_pdf(file_name)
        image_write(img, path=paste0("www/",tools::file_path_sans_ext(input$file$name),".png"),format = 'png')
      }else if(grepl("\\.svg$", file_name) || grepl("\\.psd$", file_name) || grepl("\\.gif$", file_name) || grepl("\\.tiff$", file_name) || grepl("\\.eps$", file_name) || grepl("\\.jpg$", file_name) || grepl("\\.png$", file_name) || grepl("\\.bmp$", file_name)){
        img = image_read(file_name)
        image_write(img, path=paste0("www/",tools::file_path_sans_ext(input$file$name),".png"),format = 'png')
      }
      tags$img(src = paste0(tools::file_path_sans_ext(input$file$name),".png"), height = 500, width = 590)
    }
  )
  # text output
  output$processingOutput <- renderUI({
    if(is.null(input$file)){}
    else{
        
      n = 2
      
      withProgress(message = 'Processing', value = 0,{
        file_name = input$file$datapath
        img = image_read(file_name)
        lang = tesseract(input$slcInput)
        incProgress(1/n, detail = paste("Extracting text..."))
        # convert the text image into machine coded text using ocr()
        text_lang = ocr(img,engine = lang)
        incProgress(1/n, detail = paste("Printing text..."))
        
        return(text_lang)
      })
    }
  })
  
  # object detect output
  output$objectDetect <- renderUI(
    
    if(is.null(input$file)){}
    else{
      
      n = 3
      
      withProgress(message = 'Processing', value = 0, {
        
        file_name = input$file$datapath
        pdf_ai_file = F
        svg_psd_gif_tiff_file = F
        
        if(grepl("\\.pdf$", file_name) || grepl("\\.ai$", file_name)){
          img = image_read_pdf(file_name)
          image_write(img, path ="image.png",format = 'png')
          pdf_ai_file = T
        }else if(grepl("\\.svg$", file_name) || grepl("\\.psd$", file_name) || grepl("\\.gif$", file_name) || grepl("\\.tiff$", file_name) || grepl("\\.eps$", file_name)){
          img = image_read(file_name)
          image_write(img, path ="image.png",format = 'png')
          svg_psd_gif_tiff_file = T
        }
        
        incProgress(1/n, detail = paste("Creating Model..."))
        
        writeLines("creating model...")
        # create yolo model
        yolo_tiny_voc <- image_darknet_model(type = 'detect', model = "tiny-yolo-voc.cfg", weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))
        
        incProgress(1/n, detail = paste("Detecting Objects..."))
        
        writeLines("detecting objects...")
        # detect object using model yolo model
        if(pdf_ai_file==T || svg_psd_gif_tiff_file==T){
          x <- image_darknet_detect(file = file.path(getwd(),'image.png'), object = yolo_tiny_voc, threshold = 0.19)
        }else{
          x <- image_darknet_detect(file = file_name, object = yolo_tiny_voc, threshold = 0.19)
        }
        
        incProgress(1/n, detail = paste("Writing Image..."))

        img = image_read('predictions.png')
        image_write(img, path=paste0("www/",tools::file_path_sans_ext(input$file$name),"_predictions.png"),format = 'png')
        
      })
      
      tags$img(src = paste0(tools::file_path_sans_ext(input$file$name),"_predictions.png"), height = 500, width = 590)
      
    }
  )
  
})

# launching codes
shinyApp(ui = ui, server = server)
