       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO "data/customers.dat"
               ORGANIZATION LINE SEQUENTIAL FILE STATUS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD CUST-FILE.
       01 CUST-REC.
           05 C-ID    PIC X(6).
           05 C-NAME  PIC X(20).
           05 C-CITY  PIC X(15).
       WORKING-STORAGE SECTION.
       01 WS-FS     PIC XX.
       01 WS-ID     PIC X(6).
       01 WS-FOUND  PIC X VALUE "N".
       PROCEDURE DIVISION.
           ACCEPT WS-ID FROM COMMAND-LINE
           OPEN INPUT CUST-FILE 
           IF WS-FS NOT = "00"
              DISPLAY "ERROR: CUSTOMER FILE UNAVAILABLE"
              STOP RUN
           END-IF 
           PERFORM UNTIL WS-FS NOT = "00" OR WS-FOUND = "Y"
              READ CUST-FILE 
                 AT END 
                    CONTINUE
                 NOT AT END
                    IF C-ID = WS-ID
                       MOVE "Y" TO WS-FOUND 
                    END-IF 
              END-READ
           END-PERFORM
           CLOSE CUST-FILE
           IF WS-FOUND = "Y"
              DISPLAY "OK: " FUNCTION TRIM(C-ID) "|"
                       FUNCTION TRIM(C-NAME) "|"
                       FUNCTION TRIM(C-CITY)
           ELSE
              DISPLAY "ERROR: CUSTOMER NOT FOUND"
           END-IF  
           STOP RUN.
