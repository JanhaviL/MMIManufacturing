# Names: Anvitha Ghanta and Janhavi Lonari 
# Last Updated: 07/08/2025
# Details: This is the UI file for the Shiny app made for MMI Textiles

# call the needed libraries 
library(shiny)
library(DT)
library(bslib)

# this defines the UI 
ui <- navbarPage(
  title=div(tags$img(src = "logo.png", height = "100px", style = "margin-right:15px;"),#Adds the logo
            "MMI Textiles Manufacturing Summary"  # names the navigation bar
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css") #links our style file which gives font to text and headers
  ),
  theme= bs_theme( #adds themes and color makes the front end look good
    version=5, 
    bootswatch='flatly',
    primary ="#C8102E",
    base_font=font_google('Gelasio'),
    heading_font=font_google("Lora")
  ),
  
  
  # this creates tab 1 "Introduction"
  tabPanel("Introduction",
           mainPanel(
             h3("Overview of MMI Textiles Manufacturing Summary:"),
             p("This app has been created to provide real time updates and visualizations on MMI Textiles Manufacturing Summary. The user is able to look at an analysis of different styles and looms and their accuracy."),
             p("The app has 5 different tabs that can be accesed at the top by clicking on the name. We will dive deeper into what each tab holds and how to use it."),
             h3("Overview of Tabs:"),
             
             tags$div(
               style = "
        display: flex;
        align-items: stretch;
        gap: 30px;
        margin-top: 20px;
      ",
               
               # Red box 
               tags$div(
                 style = "
          background-color: #C8102E;
          padding: 20px;
          border-radius: 10px;
          display: flex;
          justify-content: center;
          align-items: center;
          height: 380px;
          width: 570px;
        ",
                 # image
                 tags$img(
                   src = "Form.png",
                   height = "300px",
                   width = '550px',
                   style = "border-radius: 8px; border: 1px solid #6AA84F;"
                 )
               ),
               
               # Text next to the red box
               tags$div(
                 style = "flex: 1; max-width: 400px;",
                 h3("Google Form Tab"),
                 p("The first tab reffered to as Google Form consists of a form for data collection. This form mirrors the paper data collection where you enter the edge widths, Heat Set MO #, and more. After submitting entries into the 
                   form, the user has to press on the button in the sidebar called reload google sheet. This button then takes the entries entered into the form formats them and enters into the data sheet which can create updated graphs on other tabs when the reload button is
                   pressed.")
               )
             ),
             
             
             
             # Flex row for red box + side text
             tags$div(
               style = "
        display: flex;
        align-items: stretch;
        gap: 30px;
        margin-top: 20px;
      ",
               
               # Red box 
               tags$div(
                 style = "
          background-color: #C8102E;
          padding: 20px;
          border-radius: 10px;
          display: flex;
          justify-content: center;
          align-items: center;
          height: 380px;
          width: 570px;
        ",
                 # image
                 tags$img(
                   src = "Tolerance.png",
                   height = "300px",
                   style = "border-radius: 8px; border: 1px solid #6AA84F;"
                 )
               ),
               
               # Text next to the red box
               tags$div(
                 style = "flex: 1; max-width: 400px;",
                 h3("Tolerance Graphs Tab"),
                 p("This tab specifically creates on demand tolerance graphs which show the distribution of the data points in and around the minimum and maximum. This tab also has features 
                   like a dropdown that allows you to specify which style's tolerance graph you want to see. Additionaly, the user also has the choice to filter the data by the loom allowing you to see the intersection of loom and style. 
                   another feature that is provided on this tab is the reload button which when pressed on creates updated versions of the graph if the data set has been changed.")
             )
           ),
           tags$div(
             style = "
        display: flex;
        align-items: stretch;
        gap: 30px;
        margin-top: 20px;
      ",
             
             # Red box 
             tags$div(
               style = "
          background-color: #C8102E;
          padding: 20px;
          border-radius: 10px;
          display: flex;
          justify-content: center;
          align-items: center;
          height: 380px;
          width: 570px;
        ",
               # image
               tags$img(
                 src = "cpk.png",
                 height = "300px",
                 width = '550px',
                 style = "border-radius: 8px; border: 1px solid #6AA84F;"
               )
             ),
             
             # Text next to the red box
             tags$div(
               style = "flex: 1; max-width: 400px;",
               h3("CPK Graphs Tab"),
               p("The CPK Graphs Tab provides reactive graphs and a calculation of the CPK - metric that is used to measure how good a manufacturing process is- for products. This allows for the user
                 to have a numerical value that clearly shows which products need to be fixed. These graphs and values can be customized by the sidebar on the right hand side which allows you to 
                 choose which style you are intrested at seeing. In the sidebar there is also a feature called data range wich filters the data to that range so that if you want to see the cpk value after making changes you can avoid 
                 the data before the changes were made to the process. Lastly, there is the reload button which when pressed on creates updated versions of the graph if the data set has been changed. ")
             )
           ),
         
           tags$div(
             style = "
        display: flex;
        align-items: stretch;
        gap: 30px;
        margin-top: 20px;
      ",
             
             # Red box 
             tags$div(
               style = "
          background-color: #C8102E;
          padding: 20px;
          border-radius: 10px;
          display: flex;
          justify-content: center;
          align-items: center;
          height: 380px;
          width: 570px;
        ",
               # image
               tags$img(
                 src = "CPKtable.png",
                 height = "300px",
                 width = '550px',
                 style = "border-radius: 8px; border: 1px solid #6AA84F;"
               )
             ),
             
             # Text next to the red box
             tags$div(
               style = "flex: 1; max-width: 400px;",
               h3("CPK Table Tab"),
               p("The last tab on the app is the CPK Table Tab. This tab provides a table with all the different styles and their CPK value in a descending order so that you can easily find the lowest and highest performing styles.
                 On this tab there are features like a search bar that allow for the user to type in any style and get it CPK value. There is also a dropdown that you can customize which lets you choose 
                 how many entries you want to see on any page. Additionaly, there is also a previous and next button that lets you navigate all the values. Finally, there is the reload button which when pressed on creates updated versions of the table if the data set has been changed. ")
             )
           ),
           )
  ),
  
  tabPanel("Google Form",
           sidebarLayout(
             sidebarPanel(
               div(
                 style = "text-align: center;",
                 actionButton("reload_sheet", "Reload Google Sheet")
               )
             ),
             mainPanel(
               div(
                 style = "text-align: center; transform: scale(0.9); transform-origin: center;",
                 tags$iframe(
                   src = "https://tally.so/r/w4l7LA",
                   width = "100%",  # Compensates for scale shrinkage
                   height = "750px",
                   frameborder = "4"
                   
                 )
               )
             )


             
           )
  ),
             
  
  # this creates tab 2 "Tolerance Graphs"
  tabPanel("Tolerance Graphs",
           sidebarLayout(
             sidebarPanel(
               actionButton("reload_tolerance", "Reload Data"),
               selectInput(
                 inputId = "styleChoice_tolerance",
                 label = "Style:",
                 choices = "loading...",
                 selected = "loading..."
               ),
               selectInput(
                 inputId = "loom_tolerance",
                 label = "Loom:",
                 choices = "loading...",
                 selected = "loading..."
               )
             ),
             mainPanel(
               plotOutput("distPlot"),
               h4(textOutput("outCount")) # prints how many total points and how many out of range points
             )
           )
  ),
  
  # this creates tab 3 "CPK Graphs"
  tabPanel("CPK Graphs",
           sidebarLayout(
             sidebarPanel(
               actionButton("reload_cpk", "Reload Data"),
               selectInput(
                 inputId = "styleChoice_cpk",
                 label = "Style:",
                 choices = "loading...",
                 selected = "loading..."
               ),
               dateRangeInput("date_range",
                              label = "Select Date Range:",
                              start = Sys.Date() - 30,
                              end = Sys.Date(),
                              min = "2020-01-01",
                              max = Sys.Date())
             ),
             
             mainPanel(
               plotOutput("mistPlot")  # You can define this output in server if needed
             )
           )
  ),
  
  

  # this creates tab 7 "CPK Data"
  tabPanel("CPK Data",
           sidebarLayout(
             sidebarPanel(
               div(
                 style = "text-align: center;",
               actionButton("reload_cpk_table", "Reload Data")
               ),
             ),
             mainPanel(
               DTOutput("tableMaker")
             )
           )
  )
  
  
  
  
 
)


