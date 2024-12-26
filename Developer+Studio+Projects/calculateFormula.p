FUNCTION operatorSearch RETURNS LOGICAL  // Sucht nach Operator im String Priorisiert * und / vor + und -
  (INPUT cData AS CHARACTER, OUTPUT cFoundOperator AS CHARACTER):
  
  DEFINE VARIABLE iOperatorSearch AS INTEGER NO-UNDO.
  
  /* Suche zuerst nach * oder / */
  DO iOperatorSearch = 1 TO LENGTH(cData):
    CASE SUBSTRING(cData, iOperatorSearch, 1):
      WHEN "*" THEN
        DO:
          cFoundOperator = SUBSTRING(cData, iOperatorSearch, 1).
          RETURN TRUE.
        END.
      WHEN "/" THEN
        DO:
          cFoundOperator = SUBSTRING(cData, iOperatorSearch, 1).
          RETURN TRUE.
        END.   
    END CASE.
  END.

  /* Wenn kein * oder /, suche + oder - */
  DO iOperatorSearch = 1 TO LENGTH(cData):
    CASE SUBSTRING(cData, iOperatorSearch, 1):
      WHEN "+" THEN
        DO:
          cFoundOperator = SUBSTRING(cData, iOperatorSearch, 1).
          RETURN TRUE.
        END.
      WHEN "-" THEN // Wird speziell behandelt, wegen Neagtiven Werten
        DO:
          IF iOperatorSearch = 1 OR SUBSTRING(cData, iOperatorSearch - 1, 1) = "*" OR SUBSTRING(cData, iOperatorSearch - 1, 1) = "/" OR SUBSTRING(cData, iOperatorSearch - 1, 1) = "+" THEN NEXT.
          cFoundOperator = SUBSTRING(cData, iOperatorSearch, 1).
          RETURN TRUE.
        END.
    END CASE.
  END.
  
  RETURN FALSE.  // Kein Operator gefunden
END FUNCTION.

FUNCTION replaceResult RETURNS LOGICAL // Tauscht a + b mit Ergebnis im String an der bestimmten Position
  (INPUT cData3 AS CHARACTER, INPUT cDataToReplace3 AS CHARACTER, INPUT cReplaceWith3 AS CHARACTER, OUTPUT cResult AS CHARACTER):

  DEFINE VARIABLE iPos           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iCountOperator AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDataLeft      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDataRight     AS CHARACTER NO-UNDO.

  /* Zählt wie oft ein bestimmter Operator in cDataToReplace3 vorhanden ist */
  iCountOperator = LENGTH(cData3) - LENGTH(REPLACE(cData3, cDataToReplace3, "")).
  
  IF iCountOperator = 1 THEN 
  DO:
    iPos        = INDEX(cData3, cDataToReplace3).
    cDataLeft   = LEFT-TRIM(SUBSTRING(cData3, 1, iPos - 1)).
    cDataRight  = RIGHT-TRIM(SUBSTRING(cData3, iPos + LENGTH(cDataToReplace3), LENGTH(cData3))).
    cResult     = cDataLeft + cReplaceWith3 + cDataRight.
  END.
  ELSE 
  DO:
    cResult = REPLACE(cData3, cDataToReplace3, cReplaceWith3).
  END.
  
  RETURN TRUE.
END FUNCTION.

FUNCTION calculateFormula RETURNS LOGICAL // Logik für Punkt vor Strich, Berechnung usw.
  (INPUT cData AS CHARACTER, OUTPUT cResult AS CHARACTER):

  DEFINE VARIABLE iPos        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLeftPos    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iRightPos   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cLeftValue  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRightValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOperator   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dLeft       AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dRight      AS DECIMAL   NO-UNDO.
  
  IF INDEX(cData, "--") > 0 THEN
    cData = REPLACE(cData, "--", "+").

  /* Während Operatoren in cData vorhanden sind */
  DO WHILE operatorSearch(cData, cOperator):
    iPos = INDEX(cData, cOperator).

    /* Findet linken Operanten */
    DO iLeftPos = iPos - 1 TO 1 BY -1:
      CASE SUBSTRING(cData, iLeftPos, 1):
        WHEN "+" THEN 
          LEAVE.
        WHEN "-" THEN 
          LEAVE.
        WHEN "/" THEN 
          LEAVE.
        WHEN "*" THEN 
          LEAVE.
      END CASE.
    END.
    cLeftValue = SUBSTRING(cData, iLeftPos + 1, iPos - iLeftPos - 1).

    /* Findet rechten Operanten */
    DO iRightPos = iPos + 1 TO LENGTH(cData):
      CASE SUBSTRING(cData, iRightPos, 1):
        WHEN "+" THEN 
          LEAVE.
        WHEN "-" THEN 
          LEAVE.
        WHEN "/" THEN 
          LEAVE.
        WHEN "*" THEN 
          LEAVE.
      END CASE.
    END.
    cRightValue = SUBSTRING(cData, iPos + 1, iRightPos - iPos - 1).
    
    dLeft = DECIMAL(cLeftValue).
    dRight = DECIMAL(cRightValue).

    /* Ergebnis berechnen */
    CASE cOperator:
      WHEN "*" THEN
        cResult = STRING(dLeft * dRight).
      WHEN "/" THEN
        cResult = LEFT-TRIM(STRING(ROUND(dLeft / dRight, 2), "->>>,>>>,>>9.99")).
      WHEN "+" THEN
        cResult = STRING(dLeft + dRight).
      WHEN "-" THEN
        cResult = STRING(dLeft - dRight).
    END CASE.

    /* tauschen des Ergebnisses mit den alten Werten in dem String */
    replaceResult(cData, SUBSTRING(cData, iLeftPos + 1, iRightPos - iLeftPos - 1), cResult, cData).
  END.
  
  cResult = cData.
  RETURN TRUE.
END FUNCTION.

DEFINE VARIABLE cData   AS CHARACTER NO-UNDO INITIAL "(5+3-(5+-3))+(5*4)/0,4-3". // Eingabe String, welcher berechnet werden soll.
DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

/* Werte in den Klammern holen und mit calculateFormula berechnen */
DO WHILE INDEX(cData, ")") > 0:
  DEFINE VARIABLE iLeftPos  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iRightPos AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPos      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cSubExpr  AS CHARACTER NO-UNDO.

  /* Findet die Position der ersten geschlossenen Klammer, geht dann je einen Schritt nach links bis eine offene Klammer gefunden wird  */
  iPos = INDEX(cData, ")").
  DO iLeftPos = iPos - 1 TO 1 BY -1:
    IF SUBSTRING(cData, iLeftPos, 1) = "(" THEN LEAVE.
  END.
  cSubExpr = SUBSTRING(cData, iLeftPos + 1, iPos - iLeftPos - 1).

  /* calculateFormula berechnet die Expression innerhalb der Klammern */
  calculateFormula(cSubExpr, cSubExpr).
  cData = REPLACE(cData, SUBSTRING(cData, iLeftPos, iPos - iLeftPos + 1), cSubExpr).
END.

/* Wenn keine Klammern im String sind wird direkt calculateFormula aufgerufen */
calculateFormula(cData, cResult).
MESSAGE cResult VIEW-AS ALERT-BOX.
