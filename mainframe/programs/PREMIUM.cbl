       IDENTIFICATION DIVISION.
       PROGRAM-ID. PREMIUM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CMD    PIC X(100).
       01 WS-A      PIC X(20).
       01 WS-B      PIC X(20).
       01 WS-C      PIC X(20).
       01 WS-TOTAL  PIC S9(8)V99.
       01 WS-OUT    PIC Z(7)9.99.
       PROCEDURE DIVISION.
           ACCEPT WS-CMD FROM COMMAND-LINE
           UNSTRING WS-CMD DELIMITED BY ALL SPACE
               INTO WS-A WS-B WS-C
           IF WS-C = SPACES
               DISPLAY "ERROR: PREMIUM base coverage risk"
               STOP RUN
           END-IF
           COMPUTE WS-TOTAL = FUNCTION NUMVAL(WS-A)
               + FUNCTION NUMVAL(WS-B) + FUNCTION NUMVAL(WS-C)
           IF WS-TOTAL < 0
               DISPLAY "ERROR: negative premium"
           ELSE
               MOVE WS-TOTAL TO WS-OUT
               DISPLAY "OK" FUNCTION TRIM(WS-OUT)
           END-IF
           STOP RUN.
