ui <- dashboardPage(skin = "blue",
                    header = dashboardHeader(
                      title = tags$a(href='https://skyone.group/portal',
                                     tags$img(src = logo_header, width = 120)),
                      tags$li(uiOutput("eventos_bttn_ui"), class= 'dropdown'),
                      tags$li(uiOutput("alarmas_bttn_ui"), class= 'dropdown'), 
                      tags$li(uiOutput("llamadas_bttn_ui"), class= 'dropdown'),
                      tags$li(uiOutput("perfil_bttn_ui"), class= 'dropdown')
                    ),
                    
                    sidebar = dashboardSidebar(width = 200,
                                             div(
                                               br(),
                                               br(),
                                               sidebarMenuOutput("menu_ui"),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               textOutput("keepAlive")
                                             )
                    ),
                    
                    body = dashboardBody(shinyjs::useShinyjs(), 
                                          
                                          rclipboardSetup(),
                                          
                                          extendShinyjs(text = "shinyjs.resetCRM = function() {history.go(0)}", functions = "resetCRM"),
                                          
                                          tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = logo_browser)),
                                          
                                          tags$script(HTML('
                                    <div id="fb-root"></div>
                                    <script async defer crossorigin="anonymous" src="https://connect.facebook.net/es_ES/sdk.js#xfbml=1&version=v13.0&appId=288335385666465&autoLogAppEvents=1" nonce="YuFugR4n"></script>
                      ')),
                                          
                                          tags$script(HTML(paste0('
                      
                      $(document).ready(function() {document.title = "', browser_name, '";});
                      
                      
                      $("body").addClass("fixed");
                      
                      
                      $(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});});
                      
                      
                      var dimension = [0, 0];
                      $(document).on("shiny:connected", function(e) {
                            dimension[0] = window.innerWidth;
                            dimension[1] = window.innerHeight;
                            Shiny.onInputChange("dimension", dimension);
                      });
                      $(window).resize(function(e) {
                            dimension[0] = window.innerWidth;
                            dimension[1] = window.innerHeight;
                            Shiny.onInputChange("dimension", dimension);
                      });
                      
                      
                      Shiny.addCustomMessageHandler("resetValue", function(variableName) {
                        Shiny.onInputChange(variableName, null);
                      });
                      
                      
                      $(document).ready(function () {
                          function getLocation(callback){
                          var options = {
                            enableHighAccuracy: false,
                            timeout: 5000,
                            maximumAge: 0
                          };
                          navigator.geolocation.getCurrentPosition(onSuccess, onError);
                          function onError (err) {
                            Shiny.onInputChange("geolocation", false);
                          }
                          function onSuccess (position) {
                            setTimeout(function () {
                              var coords = position.coords;
                              var timestamp = new Date();
                              console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
                              Shiny.onInputChange("geolocation", true);
                              Shiny.onInputChange("lat", coords.latitude);
                              Shiny.onInputChange("lng", coords.longitude);
                              Shiny.onInputChange("accuracy", coords.accuracy);
                              Shiny.onInputChange("time", timestamp)
                              console.log(timestamp);
                              if (callback) {
                                callback();
                              }
                            }, 1100)
                          }
                        }
                        var TIMEOUT = 2000; //SPECIFY
                        var started = false;
                        function getLocationRepeat(){
                          //first time only - no delay needed
                          if (!started) {
                            started = true;
                            getLocation(getLocationRepeat);
                            return;
                          }
                          setTimeout(function () {
                            getLocation(getLocationRepeat);
                          }, TIMEOUT);
                        };
                        getLocationRepeat();
                      });
                      
                      
                      var socket_timeout_interval
                      var n = 0
                      $(document).on("shiny:connected", function(event) {
                        socket_timeout_interval = setInterval(function(){
                          Shiny.onInputChange("count", n++)
                        }, 60000)
                      });
                      $(document).on("shiny:disconnected", function(event) {
                        clearInterval(socket_timeout_interval)
                      });
                      
                      
                      '))),
                                          
                                          tags$style(type = "text/css", paste0('

                                 #inline input{border-radius: 10px;}
                                 #inline .selectize-input{border-radius: 10px;}
                                 #inline textarea{border-radius: 10px;}
                                 #inline label{font-weight: normal}
                                 #inline button{border-radius: 10px; background-color: white}

                                 #form_noticia {padding: 0px}
                                
                                
                                .marker-custom-small {
                                  background-color: rgba(157, 160, 168, 1);
                                    }
                                .marker-custom-small div {
                                    background-color: rgba(116, 119, 125, 1);
                                    }
                                .marker-custom-small div {
                                    color: white;
                                    }
                                
                                .marker-custom-medium {
                                    background-color: rgba(134, 149, 188, 1);
                                    }
                                .marker-custom-medium div {
                                    background-color: rgba(68, 88, 139, 1);
                                    }
                                .marker-custom-medium div {
                                    color: white;
                                    }
                                
                                .marker-custom-large {
                                    background-color: rgba(172, 192, 241, 1);
                                    }
                                .marker-custom-large div {
                                    background-color: rgba(105, 144, 240, 1);
                                    }
                                .marker-custom-large div {
                                    color: white;
                                }
                                
                                table.dataTable tr.selected td, table.dataTable td.selected {background-color: ', lighten(main_color, 0.30), ' !important;
                                                                                            color: white !important;}
                                
                                 #text_transp textarea{border-color: transparent; background-color: transparent}
                                 #text_transp input{border-color: transparent; background-color: transparent}
                                 
                                 #minube button{border-radius: 10px; background-color: white;
                                                width: 250px; height: 70px;
                                                font-size: 120%; text-align: center;
                                                line-height: 1em;
                                                border: 2px solid ", main_color, ";}
                                 .minube_down{border-radius: 10px !important; background-color: white; padding-top: 20px;
                                                width: 250px; height: 70px; z-index: 1; position: absolute;
                                                font-size: 100%; margin-right: 0px;
                                                border: 2px solid ", main_color, ";}
                                  #minube_del button{border-radius: 5px; background-color: red; border: 0px;
                                                  width: 20px; height: 20px; margin-left: 0px;  z-index: 3;
                                                  position: absolute; color: white; padding: 0px; font-size: 80%;
                                                  margin-bottom: -25px; margin-left: 230px;}
                                   #minube_share button{border-radius: 5px; background-color: black; border: 0px;
                                                  width: 20px; height: 20px; margin-left: 0px;  z-index: 3;
                                                  position: absolute; color: white; padding: 0px; font-size: 80%;
                                                  margin-bottom: -25px; margin-left: 205px;}

                                  .btn-group:after {display: table;}
                                  
                                  .btn-file {height: 35px; padding: 2px; font-size: 80%}
                                 
                                  .btn-file{border-radius: 10px; background-color: white; font-size: 18px;}
                                                           
                                   #gal_del button{border-radius: 5px; background-color: red; border: 0px;
                                                  width: 20px; height: 20px; margin-left: 0px;  z-index: 3;
                                                  position: absolute; color: white; padding: 0px;
                                                  margin-bottom: -20px; margin-left: 185px;}
                                   #gal_del button:hover{background: red; color: white}  
                                                           
                                         
                                  .skin-blue .main-header .logo {background-color: ', main_color, ';}
        
                                  .skin-blue .main-header .logo:hover {background-color: ', darken(main_color, 0.40), ';}

                                  .skin-blue .main-header .navbar {background-color: ', main_color, ';}        

                                  .skin-blue .main-sidebar {background-color: ', main_color, ';}

                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: ', lighten(main_color, 0.20), ';}

                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: ', main_color, ';
                                                color: #ffffff; font-size: 110%;}

                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: ', darken(main_color, 0.40), ';}
                  
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ', darken(main_color, 0.40), ';}
                  
                                  .content-wrapper, .right-side {background-color: #d1d3cf}
                                
                                  .box.box.bg-orange {background-color: #666666 !important; color: #000000 !important; }
                                
                                  #box_ubi .box {background-color: white; color: #888888 !important;
                                                 border-color: ', main_color, '; border-width: 2px;
                                                 border-style: solid;
                                                 padding: 0px; border-radius: 10px;}
                                
                                  .content {padding-right: 0px; padding-left: 0px}
                    
                                  #modal_login_sky .modal-sm  {width:300px; left: 50%; margin-left: -150px;}
                                  #modal_login_sky .modal-body {padding: 0px;
                                                     background: 
                                                     linear-gradient(to bottom, ', main_color, ', 70%, #fed3a3);
                                                     background-size: cover, 300px 50px;
                                                     background-position: top, 0px 300px;
                                                     background-repeat: no-repeat;
                                                     border-bottom-right-radius: 10px; 
                                                     border-bottom-left-radius: 10px;
                                                     border-top-left-radius: 10px; 
                                                     border-top-right-radius: 10px;
                                                     margin-bottom:0;padding-bottom:0;
                                                     box-shadow: rgba(0, 0, 0, 0.6) 10px 20px 30px 0px,
                                                                 inset 5px 5px 5px rgba(255,255,255,.4), 
                                                                 inset -5px -5px 5px rgba(0,0,0,.3);}
                                

                                  #modal_inmo_double .modal-content  {max-width: 717px; left: 50%; transform: translate(-50%);}
                                  #modal_inmo_double .modal-body  {padding: 15px;}
                                
                                  #modal_inmo_single .modal-content  {max-width: 372px; left: 50%; transform: translate(-50%);}
                                  #modal_inmo_single .modal-body  {padding: 15px;}
                                
                                  #modal_alarma .modal {
                                                    display: none; /* Hidden by default */
                                                    position: fixed; /* Stay in place */
                                                    z-index: 102; /* Sit on top */
                                                    left: 0;
                                                    top: 0;
                                                    width: 100%; /* Full width */
                                                    height: 100%; /* Full height */
                                                    overflow: auto; /* Enable scroll if needed */
                                                    background-color: rgb(0,0,0); /* Fallback color */
                                                    background-color: rgba(0,0,0,0.4); /* Black w/ opacity */
                                  }
                                
                                  #modal_alarma .modal-content {
                                                    background-color: #fefefe;
                                                    margin: 15% auto; /* 15% from the top and centered */
                                                    padding: 20px;
                                                    border: 1px solid #888;
                                                    width: 250px; /* Could be more or less, depending on screen size */
                                                    z-indez: 101
                                  }                            
                                
                                  #modal_planes .modal-sm  {max-width:520px;}
                                
                                  .modal-content  {-webkit-border-radius: 10px;
                                                    box-shadow: rgba(0, 0, 0, 0.6) 10px 20px 30px 0px;
                                                    font-weight: 400
                                                    weight: light}
                                  .modal-header  {background-color: ', main_color,'; 
                                                    border: 0px;
                                                    border-top-left-radius: 10px; 
                                                    border-top-right-radius: 10px;
                                                    color: white;
                                                    height: 50px}
                                  .modal-body {background-color: #d1d3cf;
                                                    border: 0px;
                                                    border-bottom-left-radius: 10px; 
                                                    border-bottom-right-radius: 10px;
                                                    font-weight: 400}
                                                 
                                  .small-box {-webkit-border-radius: 10px;
                                                    box-shadow: inset 3px 3px 3px rgba(255,255,255,.4), 
                                                    inset -3px -3px 3px rgba(0,0,0,.3)}
                                
                                  .small-box.bg-blue {', gradient_color("#479ec9"), '}
                                
                                  .popupbutton {
                                                    height: 40px;
                                                    width: 40px;
                                                    border:2px solid #6495ED;
                                                    background-color: #BCD2EE;
                                                    border-radius: 10px;
                                                    display:block;}
                                              
                                  #bttn_modal button {background-color: ', main_color, '; color: white;
                                                    font-size: 130%; border-radius: 20px; border: 0px}
 
                                  #icon_info {width: 30px; transition: ease-in-out all .4s;}
                                  .icon_label {opacity: 0; position: absolute; margin-top: -10px;}
                                   #icon_info:hover .icon_label {opacity: 1; width: 120px}
                                   
                                   #back_hover a {background-color: transparent;}
                                   #back_hover:hover {background-color: ', lighten(main_color, 0.20), ';}
                                   #back_hover:hover a {background-color: ', lighten(main_color, 0.20), ';}     
                                                           
                      ')),
                                          
                                          setShadow(class = "box"),
                                          setShadow(id = "ingresar"),
                                          setShadow(id = "registro"),
                                          setShadow(id = "login_user"),
                                          setShadow(id = "login_pass"),
                                          
                                          add_busy_gif(src = "pin_rot_small.gif", position = "full-page", 
                                                       overlay_color = "transparent", height = 60, width = 60),
                                          fluidPage(width=12, 
                                                    fluidRow(width = 1,
                                                             uiOutput("logo_inmo_ui")    
                                                    ),
                                                    
                                                    inputIp("ipid"),
                                                    inputUserid("fingerprint"),
                                                    
                                                    useShinyalert(), 
                                                    uiOutput("alarmas_ui"),
                                                    uiOutput("task_ui"),
                                                    uiOutput("box_eventos_ui"),
                                                    uiOutput("home_ui"),
                                                    uiOutput("office_agen_ui"),
                                                    uiOutput("box_office_prop_ui"),
                                                    uiOutput("box_office_cont_ui"),
                                                    uiOutput("box_estado_ui"),
                                                    uiOutput("box_office_trans_ui"),
                                                    uiOutput("new_inmo_ui"),
                                                    uiOutput("edit_inmo_ui"),
                                                    uiOutput("new_busqueda_ui"),
                                                    uiOutput("box_clien_ui"),
                                                    uiOutput("box_busqueda_ui"),
                                                    uiOutput("box_inmo_ui"),
                                                    uiOutput("mi_nube_ui"),
                                                    uiOutput("capacitacion_ui"),
                                                    uiOutput("prop_todas_ui"),
                                                    uiOutput("bus_todas_ui"),
                                                    uiOutput("myplace_ui"),
                                                    uiOutput("place_view_ui"),
                                                    uiOutput("place_ui"),
                                                    uiOutput("notificacion_generando_ui"),
                                                    uiOutput("notificacion_saldo_ui")
                                          )    
                    )
)  
