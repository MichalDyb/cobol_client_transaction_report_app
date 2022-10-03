      *******************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENT-TRANSACTION-RAPORT-APP.
       AUTHOR. MICHAL-DYBAS. 

      *******************************
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
      *******************************         
               SELECT CLIENT ASSIGN TO 'clients.dat'
               FILE STATUS CLIENT-FILE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
      *******************************
               SELECT TRANSACTION ASSIGN TO 'clients-transactions.dat'
               FILE STATUS TRANSACTION-FILE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
      *******************************         
               SELECT RAPORT ASSIGN TO 'report.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

      *******************************
       DATA DIVISION.
           FILE SECTION.
      *******************************
           FD CLIENT.
           01 CLIENT-FILE.
               05 CL-ID PIC 9(3).
               05 CL-NAME PIC A(20).
               05 CL-ADDRESS PIC X(20).
               05 CL-NIP PIC 9(10).
      *******************************
           FD TRANSACTION.
           01 TRANSACTION-FILE.
               05 TR-COUNTRY-CODE PIC A(2).
               05 TR-CL-ID PIC X(3).
               05 TR-CURRENCY PIC A(3).
               05 TR-TYPE PIC A(1).
               05 TR-AMOUNT PIC 9(6)V9(2).
      *******************************
           FD RAPORT RECORDING MODE F.
           01 RAPORT-FILE.
               05 FILLER PIC X(2) VALUE SPACES.
               05 RA-TR-COUNTRY-CODE PIC A(2).
               05 FILLER PIC X(2) VALUE SPACES.
               05 RA-CL-ID PIC 9(3).
               05 FILLER PIC X(2) VALUE SPACES.
               05 RA-CL-NAME PIC A(20).
               05 FILLER PIC X(2) VALUE SPACES.
               05 RA-CL-ADDRESS PIC X(20).
               05 FILLER PIC X(2) VALUE SPACES.
               05 RA-CL-NIP PIC 9(10).
               05 RA-TR-SUMMARY OCCURS 10 TIMES.
                   10 RA-TR-CURRENCY PIC A(3).
                   10 RA-TR-DEBIT-SUM PIC 9(6)V9(2).
                   10 RA-TR-CREDIT-SUM PIC 9(6)V9(2).

           WORKING-STORAGE SECTION.
      *******************************
           01 WS-CLIENT.
               05 WS-CL-ID PIC 9(3).
               05 WS-CL-NAME PIC A(20).
               05 WS-CL-ADDRESS PIC X(20).
               05 WS-CL-NIP PIC 9(10).
      *******************************
           01 WS-TRANSACTION.
               05 WS-TR-COUNTRY-CODE PIC A(2).
               05 WS-TR-CL-ID PIC X(3).
               05 WS-TR-CURRENCY PIC A(3).
               05 WS-TR-TYPE PIC A(1).
               05 WS-TR-AMOUNT PIC 9(6)V9(2).
           
      *******************************
           01 WS-COUNTRY-CODE PIC A(2).
      *******************************
           01 WS-A PIC 9(2).
      *******************************
           01 CLIENT-FILE-STATUS PIC X(2).
           01 WS-CL-RECORD-FOUND PIC A(1) VALUE 'N'.
      *******************************
           01 TRANSACTION-FILE-STATUS PIC X(2).
           01 WS-TR-RECORD-FOUND PIC A(1) VALUE 'N'.

      *******************************
       PROCEDURE DIVISION.

      *******************************    
       SHOW-ENTRY-INFO-PARA.
           DISPLAY 'Enter the two-letter country code for which '
           'customers you want to get the report.'.
       
      *******************************
       READ-COUNTRY-CODE-PARA.
           ACCEPT WS-COUNTRY-CODE
       IF NOT FUNCTION LENGTH(FUNCTION TRIM(WS-COUNTRY-CODE)) = 2 THEN
           DISPLAY 'The country code provided is invalid! Enter '
           'the two-letter country code again.'
           PERFORM READ-COUNTRY-CODE-PARA
       END-IF.
           MOVE FUNCTION UPPER-CASE(WS-COUNTRY-CODE) TO WS-COUNTRY-CODE.

      *******************************
       OPEN-CLIENT-PARA.
           OPEN INPUT CLIENT.
       IF CLIENT-FILE-STATUS = 35 THEN
           DISPLAY "Clients data file doesn't exist."
           PERFORM CLOSE-CLIENT-PARA
       END-IF.
      *******************************
       OPEN-TRANSACTION-PARA.
           OPEN INPUT TRANSACTION.
       IF TRANSACTION-FILE-STATUS = 35 THEN
           DISPLAY "Transactions data file doesn't exist."
           PERFORM CLOSE-TRANSACTION-PARA
       END-IF.
      *******************************
       OPEN-RAPORT-PARA.
       OPEN OUTPUT RAPORT.

      *******************************
       READ-TRANSACTION-FIRST-PARA.
           READ TRANSACTION INTO WS-TRANSACTION
           AT END DISPLAY 'Transactions dataset is empty.'
           PERFORM CLOSE-TRANSACTION-PARA
           NOT AT END PERFORM READ-TRANSACTION-CHECK-PARA.
      *******************************
       READ-TRANSACTION-CHECK-PARA.
       IF NOT WS-COUNTRY-CODE = FUNCTION UPPER-CASE(TR-COUNTRY-CODE)
       THEN 
           PERFORM READ-TRANSACTION-NEXT-PARA
       ELSE
       IF WS-CL-RECORD-FOUND = 'N' THEN
           PERFORM READ-CLIENT-FIRST-PARA
       ELSE
           MOVE 'Y' TO WS-TR-RECORD-FOUND
       IF WS-TR-CL-ID = WS-CL-ID THEN
       IF FUNCTION UPPER-CASE(WS-TR-TYPE) = 'D' OR FUNCTION 
       UPPER-CASE(WS-TR-TYPE) = 'K' THEN
       PERFORM VARYING WS-A FROM 1 BY 1 UNTIL WS-A = 11
       IF RA-TR-CURRENCY(WS-A) = FUNCTION UPPER-CASE(WS-TR-CURRENCY) 
       THEN
       IF FUNCTION UPPER-CASE(WS-TR-TYPE) = 'D'
       ADD WS-TR-AMOUNT RA-TR-DEBIT-SUM(WS-A) TO RA-TR-DEBIT-SUM(WS-A)
       PERFORM READ-TRANSACTION-NEXT-PARA
       END-IF
       IF FUNCTION UPPER-CASE(WS-TR-TYPE) = 'K'
       ADD WS-TR-AMOUNT RA-TR-CREDIT-SUM(WS-A) TO RA-TR-CREDIT-SUM(WS-A)
       PERFORM READ-TRANSACTION-NEXT-PARA
       END-IF
       END-IF
       END-PERFORM
       PERFORM READ-TRANSACTION-NEXT-PARA
       ELSE
           PERFORM READ-TRANSACTION-NEXT-PARA
       END-IF
       ELSE
           PERFORM SAVE-RAPORT-NEXT-LINE-PARA                
       END-IF
       END-IF
       END-IF.
      *******************************
       READ-TRANSACTION-NEXT-PARA.
           READ TRANSACTION INTO WS-TRANSACTION
           AT END 
       IF WS-TR-RECORD-FOUND = 'Y' AND WS-CL-RECORD-FOUND = 'Y' THEN 
           PERFORM SAVE-RAPORT-NEXT-LINE-PARA
       ELSE
           DISPLAY 'Transaction not found.'
           PERFORM CLOSE-TRANSACTION-PARA
       END-IF
           NOT AT END PERFORM READ-TRANSACTION-CHECK-PARA.
       
      *******************************
       READ-CLIENT-FIRST-PARA.
           READ CLIENT INTO WS-CLIENT
           AT END DISPLAY 'Client dataset is empty.'
           PERFORM CLOSE-TRANSACTION-PARA
           NOT AT END PERFORM READ-CLIENT-CHECK-PARA.
      *******************************
       READ-CLIENT-CHECK-PARA.
       IF NOT WS-CL-ID = WS-TR-CL-ID THEN
           PERFORM READ-CLIENT-NEXT-PARA
       ELSE 
           MOVE 'Y' TO WS-CL-RECORD-FOUND
           MOVE WS-CL-ID TO RA-CL-ID
           MOVE WS-CL-NAME TO RA-CL-NAME
           MOVE WS-CL-ADDRESS TO RA-CL-ADDRESS
           MOVE WS-CL-NIP TO RA-CL-NIP
           MOVE WS-TR-COUNTRY-CODE TO RA-TR-COUNTRY-CODE
       PERFORM VARYING WS-A FROM 1 BY 1 UNTIL WS-A = 11
           INITIALIZE RA-TR-SUMMARY(WS-A) REPLACING 
               NUMERIC BY ZEROES
               ALPHABETIC BY SPACES
       END-PERFORM
           MOVE 'EUR' TO RA-TR-CURRENCY(1)
           MOVE 'USD' TO RA-TR-CURRENCY(2)
           MOVE 'AUD' TO RA-TR-CURRENCY(3)
           MOVE 'BIF' TO RA-TR-CURRENCY(4)
           MOVE 'CAD' TO RA-TR-CURRENCY(5)
           MOVE 'CNY' TO RA-TR-CURRENCY(6)
           MOVE 'CZK' TO RA-TR-CURRENCY(7)
           MOVE 'JPY' TO RA-TR-CURRENCY(8)
           MOVE 'PLN' TO RA-TR-CURRENCY(9)
           MOVE 'SEK' TO RA-TR-CURRENCY(10)
           PERFORM READ-TRANSACTION-CHECK-PARA
       END-IF.
      *******************************
       READ-CLIENT-NEXT-PARA.
           READ CLIENT INTO WS-CLIENT
           AT END PERFORM CLOSE-TRANSACTION-PARA
           NOT AT END PERFORM READ-CLIENT-CHECK-PARA.

      *******************************
       SAVE-RAPORT-NEXT-LINE-PARA.
       WRITE RAPORT-FILE
       END-WRITE.
       MOVE 'N' TO WS-CL-RECORD-FOUND.
       PERFORM READ-CLIENT-NEXT-PARA.

      *******************************
       CLOSE-TRANSACTION-PARA.
       CLOSE CLIENT.
       CLOSE TRANSACTION.
       CLOSE RAPORT.
       PERFORM END-PROGRAM-PARA.
      *******************************
       CLOSE-CLIENT-PARA.    
       CLOSE CLIENT.
       PERFORM END-PROGRAM-PARA.

      *******************************
       END-PROGRAM-PARA.
       STOP RUN.
