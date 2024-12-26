DEFINE VARIABLE cData AS CHARACTER NO-UNDO initial "232435-44/3+4,6*543/21-8,8*99+34+27*3+44/4-4-6/23+33*3+12+4/5*4".

DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cList1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cList2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLeft AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRight AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSliceList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReplaceResult AS CHARACTER NO-UNDO.
DEFINE VARIABLE iI AS INTEGER NO-UNDO.

DO while INDEX(cData, "*") > 0.

cList = entry(1, cData, "*").

DO WHILE INDEX(cList, "/") > 0:

        cSliceList = entry(1, cList, "/") + "/".
        //message cSliceList view-as alert-box.
    
        cList = replace(cList, cSliceList, "").
        //message cList view-as alert-box.
        cList = cList.      
END.

DO WHILE INDEX(cList, "-") > 0:

        cSliceList = entry(1, cList, "-") + "-".
        //message cSliceList view-as alert-box.
    
        cList = replace(cList, cSliceList, "").
        //message cList view-as alert-box.
        cList = cList.      
END.

DO WHILE INDEX(cList, "+") > 0:

        cSliceList = entry(1, cList, "+") + "+".
        //message cSliceList view-as alert-box.
    
        cList = replace(cList, cSliceList, "").
        //message cList view-as alert-box.
        cList = cList.      
END.
cLeft = cList.
/* ----LINKS------OBEN----------------------------------------------------------------------------------------------------------------------------------------------------------- */        
/* ----RECHTS-----UNTEN---------------------------------------------------------------------------------------------------------------------------------------------------------- */        
cList1 = Entry(2, cData, "*").

DO WHILE INDEX(cList1, "/") > 0:
    
        cSliceList = "/" + entry(2, cList1, "/").
    
        cList1 = replace(cList1, cSliceList, "").
        cList1 = cList1.        
END.

DO WHILE INDEX(cList1, "-") > 0:
    
        cSliceList = "-" + entry(2, cList1, "-").
    
        cList1 = replace(cList1, cSliceList, "").
        cList1 = cList1.        
END.

DO WHILE INDEX(cList1, "+") > 0:
    
        cSliceList = "+" + entry(2, cList1, "+").
    
        cList1 = replace(cList1, cSliceList, "").
        cList1 = cList1.        
END.
cRight = cList1.
/* ------------------------------------------------------------------------------------------------------------ */
cResult = cResult + " " + cLeft + " " + cRight.

//message "cResult Loop" cResult view-as alert-box.

cData = replace(cData, cLeft + "*" + cRight, " ").
cLeft= "".
cRight="".
cData = cData.
//message "cData Loop" cData view-as alert-box.

END. //DO iI = 1 to Num-Entries(cData, "*") - 1.
cResult = trim(cResult).
message "cResult end" cResult view-as alert-box.

DEFINE VARIABLE iNumEntries AS INTEGER NO-UNDO.
DEFINE VARIABLE cResultLen AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

ASSIGN iNumEntries = NUM-ENTRIES(cResult," ").
MESSAGE iNumEntries
VIEW-AS ALERT-BOX.

DO iLoop = 1 to iNumEntries:
    cResultLen = cResultLen + STRING(Length(ENTRY(iLoop, cResult, " "))) + " ".
END.

MESSAGE cResultLen
VIEW-AS ALERT-BOX.

