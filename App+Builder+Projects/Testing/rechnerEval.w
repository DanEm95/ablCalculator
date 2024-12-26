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
    DEFINE VARIABLE cOPERATOR AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dOPERANT-A AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOPERANT-B AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE cExpression AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dResult AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE lResult AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cResult AS CHAR       NO-UNDO.
   
    
    DEFINE TEMP-TABLE tt NO-UNDO /*dummy TEMP-TABLE*/
    FIELD f1 AS INTEGER.
    /* dummy QUERY required: 
    -- to call the DYNAMIC-FUNCTION 
    -- with the expression passed as an INPUT parameter to evaluate*/
    DEFINE QUERY q FOR tt. /* dummy QUERY */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME WINDOW-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 BTN-DEL BTN-DEL1 BTN-DIVIDE BTN-7 ~
BTN-8 BTN-9 BTN-MULTIPLY BTN-4 BTN-5 BTN-6 BTN-MINUS BTN-1 BTN-2 BTN-3 ~
BTN-PLUS BTN-NEGATIVE BTN-0 BTN-COMMA BTN-EQUAL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDecimal C-Win 
FUNCTION GetDecimal RETURNS LOGICAL
  ( INPUT dValue AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BTN-MINUS 
     LABEL "-" 
     SIZE 9 BY 1.52
     FONT 255.

DEFINE BUTTON BTN-MULTIPLY 
     LABEL "X" 
     SIZE 9 BY 1.52.

DEFINE BUTTON BTN-NEGATIVE 
     LABEL "+/-" 
     SIZE 9 BY 1.52
     BGCOLOR 12 .

DEFINE BUTTON BTN-PLUS 
     LABEL "+" 
     SIZE 9 BY 1.52.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1.91
     FONT 255 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME WINDOW-A
     FILL-IN-1 AT ROW 1.48 COL 4 NO-LABEL WIDGET-ID 4
     BTN-DEL AT ROW 4.33 COL 42 WIDGET-ID 26
     BTN-DEL1 AT ROW 4.33 COL 53 WIDGET-ID 36
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
     BTN-NEGATIVE AT ROW 13.62 COL 20 WIDGET-ID 54
     BTN-0 AT ROW 13.62 COL 31 WIDGET-ID 16
     BTN-COMMA AT ROW 13.62 COL 42 WIDGET-ID 18
     BTN-EQUAL AT ROW 13.62 COL 53 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.62 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 16.62
         WIDTH              = 80
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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
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
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-1 C-Win
ON CHOOSE OF BTN-1 IN FRAME WINDOW-A /* 1 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-2 C-Win
ON CHOOSE OF BTN-2 IN FRAME WINDOW-A /* 2 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "2".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-3 C-Win
ON CHOOSE OF BTN-3 IN FRAME WINDOW-A /* 3 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "3".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-4 C-Win
ON CHOOSE OF BTN-4 IN FRAME WINDOW-A /* 4 */
DO:                                                     
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "4".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-5 C-Win
ON CHOOSE OF BTN-5 IN FRAME WINDOW-A /* 5 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "5".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-6 C-Win
ON CHOOSE OF BTN-6 IN FRAME WINDOW-A /* 6 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "6".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-7 C-Win
ON CHOOSE OF BTN-7 IN FRAME WINDOW-A /* 7 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "7".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-8 C-Win
ON CHOOSE OF BTN-8 IN FRAME WINDOW-A /* 8 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "8".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-9 C-Win
ON CHOOSE OF BTN-9 IN FRAME WINDOW-A /* 9 */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "9".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-COMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-COMMA C-Win
ON CHOOSE OF BTN-COMMA IN FRAME WINDOW-A /* , */
DO:
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + ",".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DEL C-Win
ON CHOOSE OF BTN-DEL IN FRAME WINDOW-A /* DEL */
DO:
      FILL-IN-1:SCREEN-VALUE = "".
      dOPERANT-A = 0.00.
      dOPERANT-B = 0.00.


      cOPERATOR = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DEL1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DEL1 C-Win
ON CHOOSE OF BTN-DEL1 IN FRAME WINDOW-A /* <-- */
DO:
  FILL-IN-1:SCREEN-VALUE = substring(FILL-IN-1:SCREEN-VALUE, 1, LENGTH(FILL-IN-1:SCREEN-VALUE) - 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-DIVIDE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-DIVIDE C-Win
ON CHOOSE OF BTN-DIVIDE IN FRAME WINDOW-A /* / */
DO:
    cOPERATOR = "/".
    //iCOUNTER = iCOUNTER + 1.
  
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "/".

    
/*     IF iCOUNTER = 2 THEN                                                   */
/*     DO:                                                                    */
/*         APPLY "CHOOSE" TO BTN-EQUAL.                                       */
/*         FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "/".             */
/*         iCOUNTER = ICOUNTER + 1.                                           */
/*         dOPERANT-A = DECIMAL(ENTRY(1, FILL-IN-1:SCREEN-VALUE, cOPERATOR)). */
/*     END.                                                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-EQUAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-EQUAL C-Win
ON CHOOSE OF BTN-EQUAL IN FRAME WINDOW-A /* = */
DO:
    /* Evaluate an ABL decimal expression */
    
    cExpression = fill-in-1:SCREEN-VALUE.
    cExpression = REPLACE(cExpression, "+", " + ").
    cExpression = REPLACE(cExpression, "-", " - ").
    MESSAGE cexpression.
    
    QUERY q:QUERY-PREPARE("FOR EACH tt WHERE ~
       DYNAMIC-FUNCTION( 'GetDecimal', " + cExpression + ") = TRUE").
    QUERY q:QUERY-OPEN().
    QUERY q:QUERY-CLOSE.
    FILL-in-1:SCREEN-VALUE = STRING(dResult).

/* 
    function von dieser Webseite: 
    https://community.progress.com/s/article/P39990
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-MINUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-MINUS C-Win
ON CHOOSE OF BTN-MINUS IN FRAME WINDOW-A /* - */
DO:
    cOPERATOR = "-".
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "-".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-MULTIPLY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-MULTIPLY C-Win
ON CHOOSE OF BTN-MULTIPLY IN FRAME WINDOW-A /* X */
DO:
    cOPERATOR = "*".
    FILL-IN-1:SCREEN-VALUE = FILL-IN-1:SCREEN-VALUE + "*".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-NEGATIVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-NEGATIVE C-Win
ON CHOOSE OF BTN-NEGATIVE IN FRAME WINDOW-A /* +/- */
DO:
      FILL-IN-1:SCREEN-VALUE = "-" + FILL-IN-1:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-PLUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-PLUS C-Win
ON CHOOSE OF BTN-PLUS IN FRAME WINDOW-A /* + */
DO:
    cOperator = "+".
    FILL-IN-1:SCREEN-VALUE = STRING(FILL-IN-1:SCREEN-VALUE + "+").

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
  ENABLE FILL-IN-1 BTN-DEL BTN-DEL1 BTN-DIVIDE BTN-7 BTN-8 BTN-9 BTN-MULTIPLY 
         BTN-4 BTN-5 BTN-6 BTN-MINUS BTN-1 BTN-2 BTN-3 BTN-PLUS BTN-NEGATIVE 
         BTN-0 BTN-COMMA BTN-EQUAL 
      WITH FRAME WINDOW-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-WINDOW-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repeat-equal C-Win 
PROCEDURE repeat-equal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DEFINE INPUT-OUTPUT PARAMETER dOPERANT-B AS DECIMAL NO-UNDO.  */
/*   DEFINE INPUT PARAMETER cOPERATOR AS CHARACTER NO-UNDO.        */
/*   DEFINE INPUT-OUTPUT PARAMETER FILL-IN-1 AS CHARACTER NO-UNDO. */

  //cOPERANT-EQUAL = cOPERATOR + STRING(doperant-b).
  //MESSAGE coperant-equal.

/*   IF iCOUNTER-EQUAL = 2 THEN                        */
/*   DO:                                               */
/*     //MESSAGE coperant-equal.                       */
/*     FILL-IN-1 = STRING(FILL-IN-1 + cOPERANT-EQUAL). */
/*     ICOUNTER-EQUAL = 0.                             */
/*   END.                                              */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDecimal C-Win 
FUNCTION GetDecimal RETURNS LOGICAL
  ( INPUT dValue AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
        dResult = dValue.
        RETURN TRUE.
END FUNCTION.  /* GetDecimal*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

