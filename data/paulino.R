require("hyperdirichlet" , quietly=TRUE, save=FALSE)

"paulino" <-
  hyperdirichlet(
                 x       = c(0,21,18,18,15,0,28,0) ,
                 NC      = 8.6554760591751e-34 ,
                 pnames  = c("low" , "medium" , "high")
                 )
