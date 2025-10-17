       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE-CALC.
       AUTHOR.     COBOL-PROGRAMMER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1      PIC 99.
       01 NUM2      PIC 99.
       01 RESULT    PIC 999.
       01 CHOICE    PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "=== �ȒP�v�Z�@ ===".
           PERFORM UNTIL CHOICE = 5
               DISPLAY "1. �����Z"
               DISPLAY "2. �����Z"
               DISPLAY "3. �|���Z"
               DISPLAY "4. ����Z"
               DISPLAY "5. �I��"
               DISPLAY "�����I��ł�������: "
               ACCEPT CHOICE
               
               EVALUATE CHOICE
                   WHEN 1 PERFORM ADD-PARA
                   WHEN 2 PERFORM SUB-PARA
                   WHEN 3 PERFORM MUL-PARA
                   WHEN 4 PERFORM DIV-PARA
                   WHEN 5 DISPLAY "���悤�Ȃ�I"
                   WHEN OTHER DISPLAY "�����ȑI���ł��I"
               END-EVALUATE
           END-PERFORM.
           STOP RUN.

       ADD-PARA.
           DISPLAY "1�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM1.
           DISPLAY "2�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY "����: " NUM1 " + " NUM2 " = " RESULT.

       SUB-PARA.
           DISPLAY "1�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM1.
           DISPLAY "2�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 - NUM2.
           DISPLAY "����: " NUM1 " - " NUM2 " = " RESULT.

       MUL-PARA.
           DISPLAY "1�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM1.
           DISPLAY "2�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 * NUM2.
           DISPLAY "����: " NUM1 " * " NUM2 " = " RESULT.

       DIV-PARA.
           DISPLAY "1�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM1.
           DISPLAY "2�ڂ̐�������͂��Ă�������: "
           ACCEPT NUM2.
           IF NUM2 = 0 THEN
               DISPLAY "�G���[: 0�Ŋ��邱�Ƃ͂ł��܂���I"
           ELSE
               COMPUTE RESULT = NUM1 / NUM2
               DISPLAY "����: " NUM1 " / " NUM2 " = " RESULT
           END-IF.
