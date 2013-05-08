# change the following line since R v.2.15.3 as .onAttach [2013/3/4 AM 06:27:15]

.onAttach <- function(lib, pkg)  {

packageStartupMessage("
....................................
                          
   bear (BE/BA for R) 
   v2.5.5
                                                                   
   Please type 'go()' to run; or
   'about.bear()' to read more; or
   'demo(bear.demo)' to run demo.               

....................................")
}