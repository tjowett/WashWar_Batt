WWBatt <- function() {

  # PART 1 RETREAT -------------------------------------------------------------
  writeLines("Part 1: Pre-battle retreat by Americans")

  #Americans commanded by Washington
  w <- readline("Is the American army commanded by Washington?")

  #Attacker and Defender
  a <- readline("Is the American army attacking?")
  if(a=="y"){Att <- "Am"; Def <- "Br"}
  if(a=="n"){Att <- "Br"; Def <- "Am"}

  if(Def=="Am"){
    ar <- readline("Does the American army want to retreat?")
    if(ar=="y"){
       if(w=="n"){
         writeLines("Conduct American retreat without die roll modifier (see 7.9A, page 13).")
       }
       if(w=="y"){
         writeLines("Conduct American retreat with +2 die roll modifier from Washington (see 7.9A, page 13).")
       }

      sr <- readline("Was the American retreat successful?")
      if(sr=="y"){writeLines("Battle avoided due to successful American retreat")}
    }
  }

  if(sr=="n"|ar=="n"){

  # Part 2 Enter Battle details -------------------------------------------------------------

  writeLines("Part 12: Enter Battle details")

  #Interception
  Br.Int.DRM <- NA
  Am.Int.DRM <- 0
  if(a=="y"){
    I <- readline("Did the American army conduct a successful Intercept?")
    if(I=="y"){Am.Int.DRM <- 1}
  }

  # Winter Offensive(Am only):
  Br.WO.DRM <- NA
  Am.WO.DRM <- 0

  if(Att=="Am") {

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
}

WWBatt()