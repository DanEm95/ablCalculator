
# Taschenrechner-Projekt

Willkommen zu unserem Taschenrechner! Dieser Rechner wertet mathematische Ausdrücke aus und unterstützt Addition, Subtraktion, Multiplikation, Division und Klammern. Der Code is int OpenEdge ABL geschrieben und einfach Anpassbar.


## Funktionen
 - [Multiplikation (*)](https://de.wikipedia.org/wiki/Multiplikation)
 - [Division (/)](https://de.wikipedia.org/wiki/Division_(Mathematik))
 - [Addition (+)](https://de.wikipedia.org/wiki/Addition)
 - [Subtraktion (-)](https://de.wikipedia.org/wiki/Subtraktion)
  - [Klammerausdrücke (a+(b+c)+d)](https://de.wikipedia.org/wiki/Klammer_(Zeichen)#Gruppierungsklammern_in_Termen)


## Requirements

[Progress OpenEdge ABL](https://www.progress.com/openedge/features/abl)


## Nutzung

```javascript
DEFINE VARIABLE cData   AS CHARACTER NO-UNDO INITIAL "(5+3-(5+-3))+(5*4)/0,4-3". 
// Eingabe String, welcher berechnet werden soll.
```

cData mit der zu berechnenden Logik ändern.

### Examples
"3/2+3+24*233+4"

"(5+3-(5+-3))+(5*4)/0,4-3"

"2+5"

"2,5-1,5"

und so weiter.