       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MANAGEMENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE ASSIGN TO "employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE.
       01  EMP-RECORD.
           05 EMP-ID         PIC 9(5).
           05 EMP-NAME       PIC X(20).
           05 EMP-DEPT       PIC X(15).
           05 EMP-AGE        PIC 99.
           05 EMP-GENDER     PIC X(1).

       WORKING-STORAGE SECTION.
       01  MENU-OPTION       PIC 9 VALUE 0.
       01  WS-SEARCH-ID      PIC 9(5) VALUE ZERO.
       01  FOUND-FLAG        PIC X VALUE "N".
       01  END-FLAG          PIC X VALUE "N".
       01  EOF               PIC X VALUE "N".

       01  WS-EMP-DATA.
           05 WS-EMP-ID      PIC 9(5).
           05 WS-EMP-NAME    PIC X(20).
           05 WS-EMP-DEPT    PIC X(15).
           05 WS-EMP-AGE     PIC 99.
           05 WS-EMP-GENDER  PIC X(1).

       01  MSG-MENU          PIC X(80)
           VALUE "1.�o�^  2.����  3.�ꗗ  9.�I�� �� ".
       01  MSG-END           PIC X(40)
           VALUE "�V�X�e�����I�����܂��B".
       01  MSG-INVALID       PIC X(40)
           VALUE "�����ȑI���ł��B�ē��͂��Ă��������B".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "        �Ј��Ǘ��V�X�e��             ".
           PERFORM MAIN-LOOP.
           STOP RUN.

       MAIN-LOOP.
           DISPLAY MSG-MENU.
           ACCEPT MENU-OPTION.

           EVALUATE MENU-OPTION
               WHEN 1
                   PERFORM REGISTER-EMPLOYEE
               WHEN 2
                   PERFORM SEARCH-EMPLOYEE
               WHEN 3
                   PERFORM LIST-EMPLOYEE
               WHEN 9
                   MOVE "Y" TO END-FLAG
               WHEN OTHER
                   DISPLAY MSG-INVALID
           END-EVALUATE

           IF END-FLAG = "Y"
               DISPLAY MSG-END
               STOP RUN
           END-IF

           GO TO MAIN-LOOP.

       REGISTER-EMPLOYEE.
           DISPLAY "�Ј��ԍ�����͂��Ă��������F".
           ACCEPT WS-EMP-ID.
           DISPLAY "��������͂��Ă��������F".
           ACCEPT WS-EMP-NAME.
           DISPLAY "��������͂��Ă��������F".
           ACCEPT WS-EMP-DEPT.
           DISPLAY "�N�����͂��Ă��������F".
           ACCEPT WS-EMP-AGE.
           DISPLAY "���ʁiM/F�j����͂��Ă��������F".
           ACCEPT WS-EMP-GENDER.

           OPEN EXTEND EMP-FILE.
           MOVE WS-EMP-ID      TO EMP-ID.
           MOVE WS-EMP-NAME    TO EMP-NAME.
           MOVE WS-EMP-DEPT    TO EMP-DEPT.
           MOVE WS-EMP-AGE     TO EMP-AGE.
           MOVE WS-EMP-GENDER  TO EMP-GENDER.
           WRITE EMP-RECORD.
           CLOSE EMP-FILE.

           DISPLAY "�o�^���������܂����B".
           EXIT.

       SEARCH-EMPLOYEE.
           DISPLAY "��������Ј��ԍ�����͂��Ă��������F".
           ACCEPT WS-SEARCH-ID.

           MOVE "N" TO FOUND-FLAG.
           MOVE "N" TO EOF.

           OPEN INPUT EMP-FILE.
           PERFORM UNTIL EOF = "Y"
               READ EMP-FILE
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       IF EMP-ID = WS-SEARCH-ID
                           MOVE "Y" TO FOUND-FLAG
                           PERFORM DISPLAY-EMPLOYEE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE EMP-FILE.

           IF FOUND-FLAG = "N"
               DISPLAY "�Y������Ј���������܂���ł����B"
           END-IF.
           EXIT.

       LIST-EMPLOYEE.
           MOVE "N" TO EOF.
           OPEN INPUT EMP-FILE.
           DISPLAY "�Ј��ԍ�  ����          ����        �N�� ����".
           PERFORM UNTIL EOF = "Y"
               READ EMP-FILE
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       DISPLAY EMP-ID SPACE EMP-NAME SPACE EMP-DEPT SPACE EMP-AGE SPACE EMP-GENDER
               END-READ
           END-PERFORM.
           CLOSE EMP-FILE.
           EXIT.

       DISPLAY-EMPLOYEE.
           DISPLAY "�Ј��ԍ��F" EMP-ID.
           DISPLAY "�����@�@�F" EMP-NAME.
           DISPLAY "�����@�@�F" EMP-DEPT.
           DISPLAY "�N��@�@�F" EMP-AGE.
           DISPLAY "���ʁ@�@�F" EMP-GENDER.
           EXIT.
