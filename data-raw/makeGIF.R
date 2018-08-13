# This file reads .jpg files in selected folders and merges 
# them to a .gif animation. 

require(animation)

# Capture variables in workspace:
OldVariables <- ls()


# Set parameters (folders and subfolders)
mainDir  = "D:/Data/spektrai/_5/PAP_RK_2015"
exs      = c("355","405")
klasifs  = c("HistGr_2015","ZPV_16")
figType  = c("AUC",
            "Pav 1.4 fixed maximum",
            "Pav 1.4 relative maximum",
            "Pav 1.10 STD fixed maximum",
            "Pav 1.10 STD relative maximum",
            "Pav 2.2 fixed maximum",
            "Pav 2.2 relative maximum",
            "Pav 3.3 boxplot")

# Clear console
cat("\014")


for (ex in exs)
{  
    message(paste0('==================== ',ex,' ====================='))
    # konvertuojama spektrų matrica
    wDir = file.path(mainDir, ex);
    message(wDir)
    setwd(wDir)
    shell(paste0('convert -loop 0 -delay 30 ', 
                 '"2 Spektru matrica ex',ex,', t=*s.jpg" ',
                 '"',file.path(mainDir,paste0('ex', ex,'nm spektrai.gif"'))
                )
        )
    
    # Konvertuojami kiti paveikslai
   
    for (klasif in klasifs)
    {
        message(paste0('----------------- ',klasif,' -----------------'))
        
        for (i in 1:length(figType))
        {
            fig = figType[i]
            message(paste0('Converting to gif (', i,'): "', fig,'"'))
            wDir = file.path(mainDir, ex, klasif, fig)
            # message(wDir)
            setwd(wDir)
            
            if (i==1) t<-50 else t<-30
            
            shell(paste0('convert -loop 0 -delay ', t ,' *.jpg ',
                         '"',file.path(mainDir,
                                       paste0('ex', ex, ' ', klasif,' ', fig, '.gif"'))
                         )
                  )
        }
    
    }
}
message('>>> DONE  <<<')

# Delete unnecessary variables
rm(list = setdiff(ls(),OldVariables))
