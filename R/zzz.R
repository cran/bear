# change the following line since R v.2.15.3 as .onAttach [2013/3/4 AM 06:27:15]
# .First.lib <- function(...) {  <-- should not use again.

.onAttach <- function(lib, pkg)  {

# echo output to screen

packageStartupMessage("
..........................................

   .b                  
   88                 
   888oooo    .ooooo.   eoooo.  oooood8b
   d88    88 d8(   )8b      )8b  888  8)
   888    88 888ooo88b o8o89888  888
   888    88 888       88(   88  888
     o8ooo8   88bod8P   doooo8Q d888b 
                          
   bear (BE/BA for R)
                                                                   
   Please type 'go()' to run; or
   'about.bear()' to read more.                

..........................................")
}