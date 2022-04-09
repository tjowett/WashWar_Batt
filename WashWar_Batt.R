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
  writeLines("Part 2: Enter Battle details")

    #General's Battle rating
    Br.GR <- readline("Enter the battle rating of the British General:")
    Am.GR <- readline("Enter the battle rating of the American General:")

    #CU's
    Am.CU <- readline("Enter the American CU's in the battle:")
    Br.CU <- readline("Enter the British CU's in the battle:")

  #British Regulars Advantage (Br only) (9.41)
    Br.BR.DRM <- 0
    Am.BR.DRM <- NA

    br <- readline("Do the British regulars have the Advantage?")
    if(br=="y"){Br.BR.DRM <- 1}

    #Royal Navy Support (Br only) (9.42)

    Br.RNS.DRM <- 0
    Am.RNS.DRM <- NA

  p <- readline("Is the battle taking place in a non-blockaded Port?")

    if(p=="y"){
      fp <- readline("Is the port fortified?")
      if(fp=="n"){Br.RNS.DRM <- 1}
        if(fp=="y"){
          fp.br.pcm <- readline("Does the space contain a Br PCM?")
          if(fp.br.pcm=="y"){Br.RNS.DRM <- 1}
        }
    }

     # Militia support (9.43):
    Br.MS.DRM <- 0
    Am.MS.DRM <- 0

    bc <- readline("Is the battle taking place in Canada?")

    if(bc=="y"){
      cc <- readline("Does one side control Canada, Montreal and Quebec?")
      if(cc=="y"){scc <- redline("Which side controls Canada, Mpntreal and Quebec?")
        if(scc=="Am"){Am.MS.DRM <- 1}
        if(scc=="Br"){Br.MS.DRM <- 1}
      }
    }

    if(bc=="n"){
      co <- readline("Is the battle space within a controlled Colony?")
      if(co=="y"){ccc <- redline("Which side controls the Colony?")
        if(ccc=="Am"){Am.MS.DRM <- 1}
        if(ccc=="Br"){Br.MS.DRM <- 1}
      }
    }

 # Winter Offensive(Am only) (9.44):
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

  #Interception (Am only) (9.46):
  Br.Int.DRM <- NA
  Am.Int.DRM <- 0
  if(a=="y"){
    I <- readline("Did the American army conduct a successful Intercept?")
    if(I=="y"){Am.Int.DRM <- 1}
  }

    # Part 3 Battle Resolution -------------------------------------------------------------
  writeLines("Part 3: Battle Resolution")

    writeLines("Step 1: Play Cards")

      # Battle card bonus (9.45):
    Br.BC.DRM <- 0
    Am.BC.DRM <- 0

if(a=="y"){
  pd.aad <- readline("Did the attacker(America) discard an event strategy card?")
if(pd.aad=="n"){
  pd.aap <- readline("Did the attacker(America) play a Battle card?")
}
if(pd.aad=="y"){Am.BC.DRM <- 1 }
if(pd.aap=="y"){Am.BC.DRM <- readline("Enter the DRM from the American Battle card:") }

  pd.db <- readline("Did the defender(Britain) discard an event strategy card?")
if(pd.db=="n"){
  pd.dbp <- readline("Did the defender(Britain) play a Battle card?")
}
if(pd.db=="y"){Br.BC.DRM <- 1 }
if(pd.aap=="y"){Br.BC.DRM <- readline("Enter the DRM from the Bristish Battle card:") }
}

if(a=="n"){
  pd.aad <- readline("Did the attacker(Britain discard an event strategy card?")
if(pd.aad=="n"){
  pd.aap <- readline("Did the attacker(Britain) play a Battle card?")
}
if(pd.aad=="y"){Br.BC.DRM <- 1 }
if(pd.aap=="y"){Br.BC.DRM <- readline("Enter the DRM from the British Battle card:") }

  pd.db <- readline("Did the defender(America) discard an event strategy card?")
if(pd.db=="n"){
  pd.dbp <- readline("Did the defender(America) play a Battle card?")
}
if(pd.db=="y"){Am.BC.DRM <- 1 }
if(pd.aap=="y"){Am.BC.DRM <- readline("Enter the DRM from the American Battle card:") }
}

    # Step 2: Determine actual General's battle rating---------
writeLines("Step 2: Determine Actual Battle Rating of each General")

    Am.DR <- readline("American die roll:")
    Br.DR <- readline("Bristish die roll:")

    if(Am.DR <=3){Am.GR <- round(Am.GR/2)
      if(Am.GR > Am.CU){Am.GR <- Am.CU}
    }
    if(Am.DR > 3){
      if(Am.GR > Am.CU){Am.GR <- Am.CU}
    }

    if(Br.DR <=3){Br.GR <- round(Br.GR/2)
      if(Br.GR > Br.CU){Br.GR <- Br.CU}
    }
    if(Br.DR > 3){
      if(Br.GR > Br.CU){Br.GR <- Br.CU}
    }

    # Step 3: Calculate the total DRM---------
    writeLines("Step 3: Calculate the total DRM")

    Am.TDRM <- Am.CU + Am.GR + Am.MS.DRM + Am.WO.DRM + Am.Int.DRM
    Br.TDRM <- Br.CU + Br.GR + Br.BR.DRM + Br.RNS.DRM + Br.MS.DRM

    # Step 4: Determine Battle Winner---------
    writeLines("Step 4: Determine Battle Winner")

    Am.BDR <- readline("American die roll:")
    Br.BDR <- readline("Bristish die roll:")

    Am.BS <- Am.TDRM + Am.BDR
    Br.BS <- Br.TDRM + Br.BDR
    CD <- abs(Am.BS-Br.BS)

    if(Am.BS > Br.BS & Att=="Am"){
      BW <- "Am"
      writeLines("The Americans (attackers) won the Battle!")
      writeLines(paste("Margin of victory:",CD))
    }

    if(Br.BS > Am.BS & Att=="Br"){
      BW <- "Br"
      writeLines("The British (attackers) won the Battle!")
      writeLines(paste("Margin of victory:",CD))
    }

    if(Am.BS > Br.BS & Def=="Am"){
      BW <- "Am"
      writeLines("The Americans (defernders) won the Battle!")
      writeLines(paste("Margin of victory:",CD))
    }

    if(Br.BS > Am.BS & Def=="Br"){
      BW <- "Br"
      writeLines("The British (defenders) won the Battle!")
      writeLines(paste("Margin of victory:",CD))
    }

    if(Am.BS==Br.BS){
      if(Att=="Am"){
        BW <- "Am"
        writeLines("The Americans (attackers) won the Battle!")
        writeLines(paste("Margin of victory:",CD))
      }
      if(Att=="Br"){
        BW <- "Br"
        writeLines("The British (attackers) won the Battle!")
        writeLines(paste("Margin of victory:",CD))
      }
    }

    # Step 5: Determine CU losses of BOTH sides ---------
    writeLines("Step 5: Determine the CU losses of BOTH sides")

  return(c(Am.WO.DRM,Br.Port.DRM))
  }
}

WWBatt()