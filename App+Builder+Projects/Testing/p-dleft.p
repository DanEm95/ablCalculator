DEFINE VARIABLE cData AS CHARACTER NO-UNDO initial "2/2+2/5743+21+282*2+34+27*23+33*3+12".
DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cList1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cList2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dLeft AS DECIMAL NO-UNDO.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSliceList AS CHARACTER NO-UNDO.


iIndex = index(cData, "*").
		cList = entry(1, cData, "*").

		MESSAGE cList VIEW-AS ALERT-BOX.

DO WHILE INDEX(cList, "+") > 0:

cSliceList = entry(1, cList, "+") + "+".
		message cSliceList view-as alert-box.
	
		cList = replace(cList, cSliceList, "").
		message cList view-as alert-box.
		cList = cList.		
END.

dLeft = decimal(cList).
