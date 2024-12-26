&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE dOPERANT-A AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOperant-B AS DECIMAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME WINDOW-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FILL-IN-1 BTN-ROOT BTN-CE ~
BTN-DEL BTN-DEL1 BTN-POTENTIAL2 BTN-LEFT BTN-Right BTN-DIVIDE BTN-7 BTN-8 ~
BTN-9 BTN-MULTIPLY BTN-4 BTN-5 BTN-6 BTN-MINUS BTN-1 BTN-2 BTN-3 BTN-PLUS ~
BTN-PERCENT BTN-0 BTN-COMMA BTN-EQUAL BTN-NEGATIVE BTN-MODULO ~
BTN-POTENTIALY 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calculateFormula C-Win 
FUNCTION calculateFormula RETURNS LOGICAL
  ( INPUT cData AS CHARACTER, OUTPUT cResult AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD operatorSearch C-Win 
FUNCTION operatorSearch RETURNS LOGICAL
  ( INPUT cData AS CHARACTER, OUTPUT cFoundOperator AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD replaceResult C-Win 
FUNCTION replaceResult RETURNS LOGICAL
  ( INPUT cData3 AS CHARACTER, INPUT cDataToReplace3 AS CHARACTER, INPUT cReplaceWith3 AS CHARACTER, OUTPUT cResult AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Navigation 
       MENU-ITEM m_Btn0         LABEL "Btn0"           ACCELERATOR "0"
       MENU-ITEM m_Btn1         LABEL "Btn1"           ACCELERATOR "1"
       MENU-ITEM m_Btn2         LABEL "Btn2"           ACCELERATOR "2"
       MENU-ITEM m_Btn3         LABEL "Btn3"           ACCELERATOR "3"
       MENU-ITEM m_Btn4         LABEL "Btn4"           ACCELERATOR "4"
       MENU-ITEM m_Btn5         LABEL "Btn5"           ACCELERATOR "5"
       MENU-ITEM m_Btn6         LABEL "Btn6"           ACCELERATOR "6"
       MENU-ITEM m_Btn7         LABEL "Btn7"           ACCELERATOR "7"
       MENU-ITEM m_Btn8         LABEL "Btn8"           ACCELERATOR "8"
       MENU-ITEM m_Btn9         LABEL "Btn9"           ACCELERATOR "9"
       RULE
       MENU-ITEM m_BtnPlus      LABEL "BtnPlus"        ACCELERATOR "+"
       MENU-ITEM m_BtnMinus     LABEL "BtnMinus"       ACCELERATOR "-"
       MENU-ITEM m_BtnMultiply  LABEL "BtnMultiply"    ACCELERATOR "*"
       MENU-ITEM m_BtnDivide    LABEL "BtnDivide"      ACCELERATOR "/"
       MENU-ITEM m_BtnEqual     LABEL "BtnEqual"       ACCELERATOR "="
       RULE
       MENU-ITEM m_BtnDel1      LABEL "BtnDel1"        ACCELERATOR "BACKSPACE"
       MENU-ITEM m_BtnDel       LABEL "BtnDel"         ACCELERATOR "DEL".

DEFINE SUB-MENU m_Buttons 
       SUB-MENU  m_Navigation   LABEL "Navigation"    .

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Buttons      LABEL "Buttons"       .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-0 
     LABEL "0" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-1 
     LABEL "1" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-2 
     LABEL "2" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-3 
     LABEL "3" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-4 
     LABEL "4" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-5 
     LABEL "5" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-6 
     LABEL "6" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-7 
     LABEL "7" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-8 
     LABEL "8" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-9 
     LABEL "9" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-CE 
     LABEL "CE" 
     SIZE 9 BY 1.52
     BGCOLOR 12 .

DEFINE BUTTON BTN-COMMA 
     LABEL "," 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-DEL 
     LABEL "DEL" 
     SIZE 9 BY 1.52
     BGCOLOR 12 .

DEFINE BUTTON BTN-DEL1 
     LABEL "<--" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-DIVIDE 
     LABEL "/" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-EQUAL 
     LABEL "=" 
     SIZE 9 BY 1.52
     BGCOLOR 10 .

DEFINE BUTTON BTN-LEFT 
     LABEL "(" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-MINUS 
     LABEL "-" 
     SIZE 9 BY 1.52
     FONT 255.

DEFINE BUTTON BTN-MODULO 
     LABEL "% M" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-MULTIPLY 
     LABEL "X" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-NEGATIVE 
     LABEL "+/-" 
     SIZE 20 BY 1.52.

DEFINE BUTTON BTN-PERCENT 
     LABEL "%" 
     SIZE 9 BY 1.52 TOOLTIP "Eingabenbeispiel: von 20~"€~" % 10~"%~"".

DEFINE BUTTON BTN-PLUS 
     LABEL "+" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-POTENTIAL2 
     LABEL "x²" 
     SIZE 9 BY 1.52
     FONT 254.

DEFINE BUTTON BTN-POTENTIALY 
     LABEL "X^" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-Right 
     LABEL ")" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-ROOT 
     LABEL "SROOT" 
     SIZE 9 BY 1.52.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1.91
     FONT 255 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 13.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 16.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME WINDOW-A
     FILL-IN-1 AT ROW 1.48 COL 17 NO-LABEL WIDGET-ID 4
     BTN-ROOT AT ROW 4.1 COL 20 WIDGET-ID 40
     BTN-CE AT ROW 4.1 COL 31 WIDGET-ID 38
     BTN-DEL AT ROW 4.1 COL 42 WIDGET-ID 26
     BTN-DEL1 AT ROW 4.1 COL 53 WIDGET-ID 36
     BTN-POTENTIAL2 AT ROW 6 COL 20 WIDGET-ID 44
     BTN-LEFT AT ROW 6 COL 31 WIDGET-ID 60
     BTN-Right AT ROW 6 COL 42 WIDGET-ID 62
     BTN-DIVIDE AT ROW 6 COL 53 WIDGET-ID 34
     BTN-7 AT ROW 7.91 COL 20 WIDGET-ID 20
     BTN-8 AT ROW 7.91 COL 31 WIDGET-ID 22
     BTN-9 AT ROW 7.91 COL 42 WIDGET-ID 24
     BTN-MULTIPLY AT ROW 7.91 COL 53 WIDGET-ID 42
     BTN-4 AT ROW 9.81 COL 20 WIDGET-ID 10
     BTN-5 AT ROW 9.81 COL 31 WIDGET-ID 12
     BTN-6 AT ROW 9.81 COL 42 WIDGET-ID 14
     BTN-MINUS AT ROW 9.81 COL 53 WIDGET-ID 32
     BTN-1 AT ROW 11.71 COL 20 WIDGET-ID 2
     BTN-2 AT ROW 11.71 COL 31 WIDGET-ID 6
     BTN-3 AT ROW 11.71 COL 42 WIDGET-ID 8
     BTN-PLUS AT ROW 11.71 COL 53 WIDGET-ID 30
     BTN-PERCENT AT ROW 13.62 COL 20 WIDGET-ID 52
     BTN-0 AT ROW 13.62 COL 31 WIDGET-ID 16
     BTN-COMMA AT ROW 13.62 COL 42 WIDGET-ID 18
     BTN-EQUAL AT ROW 13.62 COL 53 WIDGET-ID 28
     BTN-NEGATIVE AT ROW 15.52 COL 20 WIDGET-ID 54
     BTN-MODULO AT ROW 15.52 COL 42 WIDGET-ID 50
     BTN-POTENTIALY AT ROW 15.52 COL 53 WIDGET-ID 48
     RECT-1 AT ROW 3.62 COL 17 WIDGET-ID 56
     RECT-2 AT ROW 1 COL 15 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.8 BY 17.67 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Rechner Beta"
         HEIGHT             = 17.67
         WIDTH              = 78.8
         MAX-HEIGHT         = 54.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 54.14
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME WINDOW-A
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME WINDOW-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME WINDOW-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Rechner Beta */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rechner Beta */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-0 C-Win
ON CHOOSE OF BTN-0 IN FRAME WINDOW-A /* 0 */
OR CHOOSE OF MENU-ITEM m_btn0  // key "0" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "0".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-1 C-Win
ON CHOOSE OF BTN-1 IN FRAME WINDOW-A /* 1 */
OR CHOOSE OF MENU-ITEM m_Btn1 // key "1" kann getippt werden 
                              // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "1".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-2 C-Win
ON CHOOSE OF BTN-2 IN FRAME WINDOW-A /* 2 */
OR CHOOSE OF MENU-ITEM m_btn2 // key "2" kann getippt werden 
                              // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "2".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-3 C-Win
ON CHOOSE OF BTN-3 IN FRAME WINDOW-A /* 3 */
OR CHOOSE OF MENU-ITEM m_btn3 // key "3" kann getippt werden 
                              // ctrl + links klick und dann Object properties/Menu Bar 

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "3".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-4 C-Win
ON CHOOSE OF BTN-4 IN FRAME WINDOW-A /* 4 */
OR CHOOSE OF MENU-ITEM m_Btn4  // key "4" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "4".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-5 C-Win
ON CHOOSE OF BTN-5 IN FRAME WINDOW-A /* 5 */
OR CHOOSE OF MENU-ITEM m_btn5  // key "5" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "5".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-6 C-Win
ON CHOOSE OF BTN-6 IN FRAME WINDOW-A /* 6 */
OR CHOOSE OF MENU-ITEM m_btn6  // key "6" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "6".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-7 C-Win
ON CHOOSE OF BTN-7 IN FRAME WINDOW-A /* 7 */
OR CHOOSE OF MENU-ITEM m_btn7  // key "7" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "7".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-8 C-Win
ON CHOOSE OF BTN-8 IN FRAME WINDOW-A /* 8 */
OR CHOOSE OF MENU-ITEM m_btn8  // key "8" kann getippt werden 
                               // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "8".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-9 C-Win
ON CHOOSE OF BTN-9 IN FRAME WINDOW-A /* 9 */
OR CHOOSE OF MENU-ITEM m_btn9 // key "9" kann getippt werden 
                              // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "9".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-CE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-CE C-Win
ON CHOOSE OF BTN-CE IN FRAME WINDOW-A /* CE */
DO:  //TODO
  FILL-IN-1:SCREEN-VALUE = ENTRY(1, FILL-IN-1:SCREEN-VALUE, "*").
  FILL-IN-1:SCREEN-VALUE = ENTRY(1, FILL-IN-1:SCREEN-VALUE, "/").
  FILL-IN-1:SCREEN-VALUE = ENTRY(1, FILL-IN-1:SCREEN-VALUE, "+").
  FILL-IN-1:SCREEN-VALUE = ENTRY(1, FILL-IN-1:SCREEN-VALUE, "-").
     
  DO:
  ASSIGN BTN-PLUS:SENSITIVE = YES
         BTN-MINUS:SENSITIVE = YES
         BTN-MULTIPLY:SENSITIVE = YES
         BTN-DIVIDE:SENSITIVE = YES
         BTN-POTENTIALY:SENSITIVE = YES
         BTN-POTENTIAL2:SENSITIVE = YES
         BTN-ROOT:SENSITIVE = YES
         BTN-MODULO:SENSITIVE = YES
         BTN-NEGATIVE:SENSITIVE = YES.
         BTN-PERCENT:SENSITIVE = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-COMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-COMMA C-Win
ON CHOOSE OF BTN-COMMA IN FRAME WINDOW-A /* , */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + ",".
    
    /* Sorgt dafür das keine doppelden commas hintereinander gesetzt werden. */
    FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "," + ",", ",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DEL C-Win
ON CHOOSE OF BTN-DEL IN FRAME WINDOW-A /* DEL */
OR CHOOSE OF MENU-ITEM m_BtnDel // key "DEL" kann getippt werden 
                                // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = "".

  DO:
    ASSIGN BTN-PLUS:SENSITIVE = YES
           BTN-MINUS:SENSITIVE = YES
           BTN-MULTIPLY:SENSITIVE = YES
           BTN-DIVIDE:SENSITIVE = YES
           BTN-POTENTIALY:SENSITIVE = YES
           BTN-POTENTIAL2:SENSITIVE = YES
           BTN-ROOT:SENSITIVE = YES
           BTN-MODULO:SENSITIVE = YES
           BTN-NEGATIVE:SENSITIVE = YES
           BTN-PERCENT:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DEL1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DEL1 C-Win
ON CHOOSE OF BTN-DEL1 IN FRAME WINDOW-A /* <-- */
OR CHOOSE OF MENU-ITEM m_BtnDel1 // key "BACKSPACE" kann getippt werden 
                                 // ctrl + links klick und dann Object properties/Menu Bar

DO:
  //FILL-IN-1:SCREEN-VALUE = substring(FILL-IN-1:SCREEN-VALUE, 1, LENGTH(FILL-IN-1:SCREEN-VALUE) - 1).
  FILL-IN-1:SCREEN-VALUE = substring(FILL-IN-1:SCREEN-VALUE, 1, LENGTH(FILL-IN-1:SCREEN-VALUE) - 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DIVIDE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DIVIDE C-Win
ON CHOOSE OF BTN-DIVIDE IN FRAME WINDOW-A /* / */
OR CHOOSE OF MENU-ITEM m_BtnDivide // key "/" kann getippt werden 
                                   // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "/".

  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "/" + "/", "/").

  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "/" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-EQUAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-EQUAL C-Win
ON CHOOSE OF BTN-EQUAL IN FRAME WINDOW-A /* = */
OR CHOOSE OF MENU-ITEM m_BtnEqual  // key "=" kann getippt werden 
                                   // ctrl + links klick und dann Object properties/Menu Bar

DO: 
  DEFINE VARIABLE cData   AS CHARACTER NO-UNDO. // Eingabe String, welcher berechnet werden soll.
  DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
  
  cData = Fill-IN-1:SCREEN-VALUE.

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
  FILL-IN-1:SCREEN-VALUE = cResult.
  
  IF INDEX(FILL-IN-1:SCREEN-VALUE, "%") > 0 THEN
  DO:
    /* Prozent Rechnung */
    dOPERANT-B = DECIMAL(ENTRY(2, FILL-IN-1:SCREEN-VALUE, "%")).
    FILL-IN-1:SCREEN-VALUE = STRING(dOPERANT-A / 100 * dOPERANT-B, "9€").
  END.
  
  DO:
  ASSIGN BTN-PLUS:SENSITIVE = YES
         BTN-MINUS:SENSITIVE = YES
         BTN-MULTIPLY:SENSITIVE = YES
         BTN-DIVIDE:SENSITIVE = YES
         BTN-POTENTIALY:SENSITIVE = YES
         BTN-POTENTIAL2:SENSITIVE = YES
         BTN-ROOT:SENSITIVE = YES
         BTN-MODULO:SENSITIVE = YES
         BTN-NEGATIVE:SENSITIVE = YES
         BTN-PERCENT:SENSITIVE = YES.
  END.
    
/*     CATCH e AS PROGRESS.Lang.Error: */
/*         /* Catching every Error */  */
/*     END CATCH.                      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-EQUAL C-Win
ON RIGHT-MOUSE-DBLCLICK OF BTN-EQUAL IN FRAME WINDOW-A /* = */
DO:
  MESSAGE "TRIGGER RIGHT MOUSE DBCLICK THREE BUTTON MOUSE EVENT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-LEFT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-LEFT C-Win
ON CHOOSE OF BTN-LEFT IN FRAME WINDOW-A /* ( */
DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "(".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-MINUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-MINUS C-Win
ON CHOOSE OF BTN-MINUS IN FRAME WINDOW-A /* - */
OR CHOOSE OF MENU-ITEM m_BtnMinus // key "-" kann getippt werden 
                                  // ctrl + links klick und dann Object properties/Menu Bar
                                                                                          
DO: 
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "-".

  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "-" + "-", "-").

  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "-" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-MODULO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-MODULO C-Win
ON CHOOSE OF BTN-MODULO IN FRAME WINDOW-A /* % M */
DO: //TODO 
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "%".
 
  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "%" + "%", "%").
  
  DO: // Die anderen Buttons werden "disabled".
  ASSIGN BTN-PLUS:SENSITIVE = NO
         BTN-MINUS:SENSITIVE = NO
         BTN-MULTIPLY:SENSITIVE = NO
         BTN-DIVIDE:SENSITIVE = NO
         BTN-POTENTIALY:SENSITIVE = NO
         BTN-POTENTIAL2:SENSITIVE = NO
         BTN-ROOT:SENSITIVE = NO
         BTN-PERCENT:SENSITIVE = NO   
         BTN-NEGATIVE:SENSITIVE = NO.   
  END.


  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "%" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-MULTIPLY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-MULTIPLY C-Win
ON CHOOSE OF BTN-MULTIPLY IN FRAME WINDOW-A /* X */
OR CHOOSE OF MENU-ITEM m_BtnMultiply // key "*" kann getippt werden 
                                     // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "*".

  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "*" + "*", "*").

  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "*" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-NEGATIVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-NEGATIVE C-Win
ON CHOOSE OF BTN-NEGATIVE IN FRAME WINDOW-A /* +/- */
DO:
  Fill-In-1:SCREEN-VALUE = "-" + FILL-in-1:SCREEN-VALUE.
  
  IF INDEX(FILL-IN-1:SCREEN-VALUE, "--") > 0 THEN
  DO:
    FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "--", "+"). 
  END.
  
  IF INDEX(FILL-IN-1:SCREEN-VALUE, "+", 1) > 0 THEN
  DO:
    FILL-IN-1:SCREEN-VALUE = LEFT-TRIM(FILL-IN-1:SCREEN-VALUE, "+"). 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-PERCENT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-PERCENT C-Win
ON CHOOSE OF BTN-PERCENT IN FRAME WINDOW-A /* % */
DO:   
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "%".
  dOPERANT-A = DECIMAL(ENTRY(1, FILL-IN-1:SCREEN-VALUE, "%")).

  
  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "%" + "%", "%").
  
  DO: // Die anderen Buttons werden "disabled".
  ASSIGN BTN-PLUS:SENSITIVE = NO
         BTN-MINUS:SENSITIVE = NO
         BTN-MULTIPLY:SENSITIVE = NO
         BTN-DIVIDE:SENSITIVE = NO
         BTN-POTENTIALY:SENSITIVE = NO
         BTN-POTENTIAL2:SENSITIVE = NO
         BTN-ROOT:SENSITIVE = NO
         BTN-MODULO:SENSITIVE = NO   
         BTN-NEGATIVE:SENSITIVE = NO.   
  END.


  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "%" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-PLUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-PLUS C-Win
ON CHOOSE OF BTN-PLUS IN FRAME WINDOW-A /* + */
OR CHOOSE OF MENU-ITEM m_BtnPlus // key "+" kann getippt werden 
                                 // ctrl + links klick und dann Object properties/Menu Bar

DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "+".

  /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
  FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "+" + "+", "+").

  /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
  IF FILL-IN-1:SCREEN-VALUE = "+" THEN
  DO:
      APPLY "CHOOSE" TO BTN-DEL.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-POTENTIAL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-POTENTIAL2 C-Win
ON CHOOSE OF BTN-POTENTIAL2 IN FRAME WINDOW-A /* x² */
DO:
  dOPERANT-A = DECIMAL(ENTRY(1, FILL-IN-1:SCREEN-VALUE)).

  /*  
     Wird hier durch Build-In "EXP()" 
     auf 2 nachkommastellen als Dezimalzahl ausgegeben.
  */
  FILL-IN-1:SCREEN-VALUE = STRING(EXP(dOPERANT-A, 2)).

  /* Wenn nur 0 die SCREEN-VALUE ist wird BTN-DEL getriggert. */
  IF FILL-IN-1:SCREEN-VALUE = "0" THEN
  DO:
     APPLY "CHOOSE" TO BTN-DEL. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-POTENTIALY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-POTENTIALY C-Win
ON CHOOSE OF BTN-POTENTIALY IN FRAME WINDOW-A /* X^ */
DO: //TODO
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "^".
    
    // Wird an "=" übergeben.
    dOPERANT-A = DECIMAL(ENTRY(1, FILL-IN-1:SCREEN-VALUE, "^")).
    
    /* Sorgt dafür das keine doppelden Operatoren hintereinander gesetzt werden. */
    FILL-IN-1:SCREEN-VALUE = REPLACE(FILL-IN-1:SCREEN-VALUE, "^" + "^", "^").
    
    DO: // Die anderen Buttons werden "disabled".
    ASSIGN BTN-PLUS:SENSITIVE = NO
           BTN-MINUS:SENSITIVE = NO
           BTN-MULTIPLY:SENSITIVE = NO
           BTN-DIVIDE:SENSITIVE = NO
           BTN-MODULO:SENSITIVE = NO
           BTN-POTENTIAL2:SENSITIVE = NO
           BTN-ROOT:SENSITIVE = NO
           BTN-PERCENT:SENSITIVE = NO   
           BTN-NEGATIVE:SENSITIVE = NO.   
    END.
    
    /* bevor keine Zahl eingegeben wird, wird kein Operator gesetzt. */
    IF FILL-IN-1:SCREEN-VALUE = "^" THEN
    DO:
        APPLY "CHOOSE" TO BTN-DEL.
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Right C-Win
ON CHOOSE OF BTN-Right IN FRAME WINDOW-A /* ) */
DO:
  FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + ")".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-ROOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-ROOT C-Win
ON CHOOSE OF BTN-ROOT IN FRAME WINDOW-A /* SROOT */
DO: 
    /* Wird hier durch Build-In "SQRT()" ausgerechnet. */
    fill-IN-1:SCREEN-VALUE = STRING(SQRT(DECIMAL(fill-IN-1:SCREEN-VALUE))).
    
    /* Wenn nur 0 die SCREEN-VALUE ist wird BTN-DEL getriggert. */
    IF FILL-IN-1:SCREEN-VALUE = "0" THEN
    DO:
       APPLY "CHOOSE" TO BTN-DEL. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-1 
      WITH FRAME WINDOW-A IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 FILL-IN-1 BTN-ROOT BTN-CE BTN-DEL BTN-DEL1 
         BTN-POTENTIAL2 BTN-LEFT BTN-Right BTN-DIVIDE BTN-7 BTN-8 BTN-9 
         BTN-MULTIPLY BTN-4 BTN-5 BTN-6 BTN-MINUS BTN-1 BTN-2 BTN-3 BTN-PLUS 
         BTN-PERCENT BTN-0 BTN-COMMA BTN-EQUAL BTN-NEGATIVE BTN-MODULO 
         BTN-POTENTIALY 
      WITH FRAME WINDOW-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-WINDOW-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calculateFormula C-Win 
FUNCTION calculateFormula RETURNS LOGICAL
  ( INPUT cData AS CHARACTER, OUTPUT cResult AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

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
        cResult = LEFT-TRIM(STRING(ROUND(dLeft / dRight, 2), "->>>,>>>,>>9.9<")).
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION operatorSearch C-Win 
FUNCTION operatorSearch RETURNS LOGICAL
  ( INPUT cData AS CHARACTER, OUTPUT cFoundOperator AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION replaceResult C-Win 
FUNCTION replaceResult RETURNS LOGICAL
  ( INPUT cData3 AS CHARACTER, INPUT cDataToReplace3 AS CHARACTER, INPUT cReplaceWith3 AS CHARACTER, OUTPUT cResult AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

