       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-INFO-LOGIN.
       AUTHOR. CHATGPT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMP-ID        PIC 9(5).
       01 EMP-PASS      PIC X(10).
       01 ANSWER        PIC X.
       01 FOUND-FLAG    PIC X VALUE "N".
       01 IDX           PIC 9.

       *> �������Ј��f�[�^�i3�l�j
       01 EMP-TABLE.
           05 EMP-REC OCCURS 3 TIMES.
              10 EMP-NO       PIC 9(5).
              10 EMP-NAME     PIC X(20).
              10 EMP-DEPT     PIC X(15).
              10 EMP-JOIN     PIC 9(4).
              10 EMP-PASSWORD PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARA.

           *> �����f�[�^�ݒ�
           MOVE 10001 TO EMP-NO (1)
           MOVE "�c�� ���Y" TO EMP-NAME (1)
           MOVE "�c�ƕ�"   TO EMP-DEPT (1)
           MOVE 2018 TO EMP-JOIN (1)
           MOVE "PASS100" TO EMP-PASSWORD (1)

           MOVE 10002 TO EMP-NO (2)
           MOVE "���� �Ԏq" TO EMP-NAME (2)
           MOVE "������"   TO EMP-DEPT (2)
           MOVE 2020 TO EMP-JOIN (2)
           MOVE "PASS200" TO EMP-PASSWORD (2)

           MOVE 10003 TO EMP-NO (3)
           MOVE "��� ��Y" TO EMP-NAME (3)
           MOVE "�J����"   TO EMP-DEPT (3)
           MOVE 2021 TO EMP-JOIN (3)
           MOVE "PASS300" TO EMP-PASSWORD (3)

           DISPLAY "==================================="
           DISPLAY "   �Ј����O�C���V�X�e���iCOBOL�j"
           DISPLAY "==================================="

           PERFORM LOGIN-LOOP

           PERFORM END-PROGRAM
           STOP RUN.

       LOGIN-LOOP.
           MOVE "N" TO FOUND-FLAG

           DISPLAY "�Ј��ԍ�����͂��Ă��������F"
           ACCEPT EMP-ID
           DISPLAY "�p�X���[�h����͂��Ă��������F"
           ACCEPT EMP-PASS

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               IF EMP-ID = EMP-NO (IDX)
                  IF EMP-PASS = EMP-PASSWORD (IDX)
                     MOVE "Y" TO FOUND-FLAG
                     EXIT PERFORM
                  END-IF
               END-IF
           END-PERFORM

           IF FOUND-FLAG = "Y"
              DISPLAY "-----------------------------------"
              DISPLAY "���O�C�������I�悤�����Ј��l�B"
              DISPLAY "�Ј��ԍ��F" EMP-NO (IDX)
              DISPLAY "�����F" EMP-NAME (IDX)
              DISPLAY "���������F" EMP-DEPT (IDX)
              DISPLAY "���ДN�F" EMP-JOIN (IDX)
              DISPLAY "-----------------------------------"
           ELSE
              DISPLAY "���O�C�����s�B�ԍ��܂��̓p�X���[�h���Ⴂ�܂��B"
              DISPLAY "������x�����܂����H (Y/N)�F"
              ACCEPT ANSWER
              IF ANSWER = "Y" OR ANSWER = "y"
                 PERFORM LOGIN-LOOP
              ELSE
                 DISPLAY "�V�X�e�����I�����܂��B"
           END-IF.

       END-PROGRAM.
           DISPLAY "==============================="
           DISPLAY "   �����p���肪�Ƃ��������܂����B"
           DISPLAY "==============================="
           .
