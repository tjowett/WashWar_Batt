WWBatt <- function() {

  #Attacker and Defender
  a <- readline("Is the American army attacking?")

  if(a=="y"){Att <- "Am"; Def <- "Br"}
  if(a=="n"){Att <- "Br"; Def <- "Am"}

  # Winter Offensive(Am only):
  Br.WO.DRM <- NA
  Am.WO.DRM <- 0

  if(Att=="Am") {

    w <- readline("Is the American army commanded by Washington?")

    if(w=="y"){
      c <- readline("Was the battle activated by the last card in the Strategy Phase?")
    }

    if(c=="y"){
      Am.WO.DRM <- 1
    }
  }

  # Port DRM(Br only):
  Br.Port.DRM <- 0
  Am.Port.DRM <- NA

  p <- readline("Is the battle taking place in a non-blockaded Port?")

    if(p=="y"){
      fp <- readline("Is the port fortified?")
      if(fp=="n"){Br.Port.DRM <- 1}
        if(fp=="y"){
          fp.br.pcm <- readline("Does the space contain a Br PCM?")
          if(fp.br.pcm=="y"){Br.Port.DRM <- 1}
        }
    }

return(c(Am.WO.DRM,Br.Port.DRM))

}

WWBatt()