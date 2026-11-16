       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLICY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POL-FILE ASSIGN TO "data/policies.idx"
               ORGANIZATION INDEXED ACCESS RANDOM
               RECORD KEY P-ID FILE STATUS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD POL-FILE.
       01 POL-REC.
           05 P-ID      PIC X(6).
           05 P-CUST    PIC X(6).
           05 P-BASE    PIC 9(7)V99.
           05 P-STATUS  PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-FS    PIC XX.
       01 WS-CMD   PIC X(60).
       01 WS-ACT   PIC X(10).
       01 WS-ID    PIC X(6).
       01 WS-ARG   PIC X(10).
       01 WS-OUT   PIC Z(6)9.99.
       PROCEDURE DIVISION.
           ACCEPT WS-CMD FROM COMMAND-LINE
           UNSTRING WS-CMD DELIMITED BY ALL SPACE
               INTO WS-ACT WS-ID WS-ARG
           EVALUATE WS-ACT
               WHEN "INIT"   PERFORM DO-INIT
               WHEN "GET"    PERFORM DO-GET
               WHEN "STATUS" PERFORM DO-STATUS
               WHEN OTHER    DISPLAY "ERR|unknown action"
           END-EVALUATE
           STOP RUN.
         DO-INIT.
           OPEN OUTPUT POL-FILE
           MOVE "P0001 C0001 000120000ACTIVE    " TO POL-REC
           WRITE POL-REC
           MOVE "P0002 C0002 000085050ACTIVE    " TO POL-REC
           WRITE POL-REC
           CLOSE POL-FILE
           DISPLAY "OK|INITIALIZED".
         DO-GET.
               OPEN INPUT POL-FILE
               MOVE WS-ID TO P-ID
               READ POL-FILE
                   INVALID KEY DISPLAY "ERROR: POLICY NOT FOUND"
                   NOT INVALID KEY PERFORM SHOW-POLICY
               END-READ
               CLOSE POL-FILE.
         DO-STATUS.
               IF WS-ARG = SPACES
                   DISPLAY "ERROR: STATUS REQUIRED"
               ELSE
                   OPEN I-O POL-FILE
                   MOVE WS-ID TO P-ID
                   READ POL-FILE
                       INVALID KEY DISPLAY "ERROR: POLICY NOT FOUND"
                       NOT INVALID KEY
                           MOVE WS-ARG TO P-STATUS
                           REWRITE POL-REC
                           PERFORM SHOW-POLICY
                   END-READ
                   CLOSE POL-FILE
               END-IF.
          SHOW-POLICY.
               MOVE P-BASE TO WS-OUT
               DISPLAY "OK|" FUNCTION TRIM(P-ID) "|"
                       FUNCTION TRIM(P-CUST) "|"
                       FUNCTION TRIM(WS-OUT) "|"
                       FUNCTION TRIM(P-STATUS).
