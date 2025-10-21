       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-MANAGER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE ASSIGN TO "EMP.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMP-FILE.
       01 EMP-REC-FILE.
           05 F-EMP-NO       PIC 9(5).
           05 F-EMP-NAME     PIC X(20).
           05 F-EMP-DEPT     PIC X(15).
           05 F-EMP-JOIN     PIC 9(4).
           05 F-EMP-PASSWORD PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS      PIC XX.
       01 WS-ANSWER           PIC X.
       01 WS-EMP-NO           PIC 9(5).
       01 WS-EMP-NAME         PIC X(20).
       01 WS-EMP-DEPT         PIC X(15).
       01 WS-EMP-JOIN         PIC 9(4).
       01 WS-EMP-PASSWORD     PIC X(10).
       01 FOUND-FLAG          PIC X VALUE "N".
       01 EOF-FLAG            PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "==================================="
           DISPLAY "   �Ј����Ǘ��V�X�e���iCOBOL�j"
           DISPLAY "==================================="

           OPEN I-O EMP-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "EMP.DAT �t�@�C�������݂��܂���B"
               DISPLAY "�V�K�쐬���܂��B"
               OPEN OUTPUT EMP-FILE
               CLOSE EMP-FILE
               OPEN I-O EMP-FILE
           END-IF

           PERFORM MAIN-LOOP

           CLOSE EMP-FILE
           DISPLAY "==============================="
           DISPLAY "   �����p���肪�Ƃ��������܂����B"
           DISPLAY "==============================="
           STOP RUN.

       MAIN-LOOP.
           PERFORM UNTIL WS-ANSWER = "Q"
               DISPLAY "-----------------------------------"
               DISPLAY "�����I�����Ă��������F"
               DISPLAY "1: �V�����Ј���ǉ�"
               DISPLAY "2: �����Ј������m�F"
               DISPLAY "Q: �I��"
               ACCEPT WS-ANSWER

               EVALUATE WS-ANSWER
                   WHEN "1"
                       PERFORM ADD-EMP
                   WHEN "2"
                       PERFORM QUERY-EMP
                   WHEN "Q"
                       DISPLAY "�v���O�������I�����܂��B"
                   WHEN OTHER
                       DISPLAY "�����ȑI���ł��B"
                       DISPLAY "������x���͂��Ă��������B"
               END-EVALUATE
           END-PERFORM.

       ADD-EMP.
           DISPLAY "�V���Ј��ԍ�����͂��Ă��������F"
           ACCEPT WS-EMP-NO
           DISPLAY "��������͂��Ă��������F"
           ACCEPT WS-EMP-NAME
           DISPLAY "��������͂��Ă��������F"
           ACCEPT WS-EMP-DEPT
           DISPLAY "���ДN�iYYYY�j����͂��Ă��������F"
           ACCEPT WS-EMP-JOIN
           DISPLAY "�����p�X���[�h����͂��Ă��������F"
           ACCEPT WS-EMP-PASSWORD

           MOVE WS-EMP-NO TO F-EMP-NO
           MOVE WS-EMP-NAME TO F-EMP-NAME
           MOVE WS-EMP-DEPT TO F-EMP-DEPT
           MOVE WS-EMP-JOIN TO F-EMP-JOIN
           MOVE WS-EMP-PASSWORD TO F-EMP-PASSWORD

           WRITE EMP-REC-FILE
           DISPLAY "�o�^�����I"
           DISPLAY "-----------------------------------"
           DISPLAY "�Ј��ԍ��F" F-EMP-NO
           DISPLAY "�����F" F-EMP-NAME
           DISPLAY "�����F" F-EMP-DEPT
           DISPLAY "���ДN�F" F-EMP-JOIN
           DISPLAY "-----------------------------------".

       QUERY-EMP.
           DISPLAY "�Ј��ԍ�����͂��Ă��������F"
           ACCEPT WS-EMP-NO
           DISPLAY "�p�X���[�h����͂��Ă��������F"
           ACCEPT WS-EMP-PASSWORD

           MOVE "N" TO FOUND-FLAG
           OPEN INPUT EMP-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ EMP-FILE INTO EMP-REC-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       IF F-EMP-NO = WS-EMP-NO AND
                          F-EMP-PASSWORD = WS-EMP-PASSWORD
                          DISPLAY "-----------------------------------"
                          DISPLAY "�Ј����F"
                          DISPLAY "�Ј��ԍ��F" F-EMP-NO
                          DISPLAY "�����F" F-EMP-NAME
                          DISPLAY "�����F" F-EMP-DEPT
                          DISPLAY "���ДN�F" F-EMP-JOIN
                          DISPLAY "-----------------------------------"
                          MOVE "Y" TO FOUND-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMP-FILE

           IF FOUND-FLAG = "N"
               DISPLAY "�Ј��ԍ��܂��̓p�X���[�h���Ⴂ�܂��B"
           END-IF.
