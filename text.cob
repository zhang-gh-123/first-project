       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDNUMBERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1       PIC 9(3).
       01 NUM2       PIC 9(3).
       01 SUM        PIC 9(4).

       PROCEDURE DIVISION.
           DISPLAY "Enter first number: ".
           ACCEPT NUM1
           
           DISPLAY "Enter second number: ".
           ACCEPT NUM2
           
           COMPUTE SUM = NUM1 + NUM2
           
           DISPLAY "The sum is: " SUM
           STOP RUN.
