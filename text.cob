       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MANAGEMENT-DB.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DB               USAGE POINTER.
       01 STMT             USAGE POINTER.
       01 RETCODE          PIC S9(9) COMP-5.
       01 SQL              PIC X(512).
       01 MENU-OPTION      PIC 9 VALUE 0.
       01 WS-SEARCH-ID     PIC 9(5) VALUE ZERO.
       01 END-FLAG         PIC X VALUE "N".
       01 FOUND-FLAG       PIC X VALUE "N".

       01 WS-EMP-DATA.
           05 WS-EMP-ID      PIC 9(5).
           05 WS-EMP-NAME    PIC X(20).
           05 WS-EMP-DEPT    PIC X(15).
           05 WS-EMP-AGE     PIC 99.
           05 WS-EMP-GENDER  PIC X(6).

       01 VALUE-TEXT       PIC X(64).

       01 MSG-MENU          PIC X(80)
           VALUE "1.�o�^  2.����  3.�ꗗ  9.�I�� �� ".
       01 MSG-END           PIC X(40)
           VALUE "�V�X�e�����I�����܂��B".
       01 MSG-INVALID       PIC X(40)
           VALUE "�����ȑI���ł��B�ē��͂��Ă��������B".
           
       01 DB-NAME PIC X(64) VALUE "employee.db".    

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " �Ј��Ǘ��V�X�e���iSQLite�j".

           CALL "my_sqlite3_open" 
                               USING BY REFERENCE DB-NAME
                                     BY REFERENCE DB
                               RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "�f�[�^�x�[�X�ڑ��G���["
               STOP RUN
           END-IF.

           MOVE "CREATE TABLE IF NOT EXISTS employee (" &
                "emp_id INTEGER PRIMARY KEY, emp_name TEXT, emp_dept TEXT, emp_age INTEGER, emp_gender TEXT);" 
                TO SQL.
           CALL "my_sqlite3_exec" 
                              USING BY VALUE DB
                                    BY REFERENCE SQL
                                    BY VALUE 0 BY VALUE 0 BY VALUE 0
                              RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "�e�[�u���������G���["
               STOP RUN
           END-IF.

           PERFORM UNTIL END-FLAG = "Y"
               DISPLAY MSG-MENU
               ACCEPT MENU-OPTION
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
           END-PERFORM.

           DISPLAY MSG-END.
           CALL "my_sqlite3_close"   USING BY VALUE DB.
           STOP RUN.

       REGISTER-EMPLOYEE.
           DISPLAY "�Ј��ԍ�����͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-EMP-ID
           DISPLAY "��������͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-EMP-NAME
           DISPLAY "��������͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-EMP-DEPT
           DISPLAY "�N�����͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-EMP-AGE
           DISPLAY "���ʁi�j/���j����͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-EMP-GENDER

           STRING "INSERT INTO employee (emp_id, emp_name, emp_dept, emp_age, emp_gender) VALUES ("
               FUNCTION NUMVAL-C (WS-EMP-ID) DELIMITED BY SIZE
               ", '" WS-EMP-NAME "', '" WS-EMP-DEPT "', "
               FUNCTION NUMVAL-C (WS-EMP-AGE)
               ", '" WS-EMP-GENDER "');"
               DELIMITED BY SIZE INTO SQL
           END-STRING.

           CALL "my_sqlite3_exec" 
                              USING BY VALUE DB
                                    BY REFERENCE SQL
                                    BY VALUE 0 BY VALUE 0 BY VALUE 0
                              RETURNING RETCODE.
           IF RETCODE = 0
               DISPLAY "�o�^���������܂����B"
           ELSE
               DISPLAY "�o�^�G���[�B"
           END-IF.
           EXIT.

       SEARCH-EMPLOYEE.
           DISPLAY "��������Ј��ԍ�����͂��Ă��������F" WITH NO ADVANCING
           ACCEPT WS-SEARCH-ID

           MOVE "SELECT emp_id, emp_name, emp_dept, emp_age, emp_gender FROM employee WHERE emp_id=" TO SQL
           STRING SQL FUNCTION NUMVAL-C (WS-SEARCH-ID) ";" DELIMITED BY SIZE INTO SQL END-STRING

           CALL "sqlite3_prepare_v2" 
                                   USING BY VALUE DB
                                         BY REFERENCE SQL
                                         BY VALUE -1
                                         BY REFERENCE STMT
                                         BY VALUE 0
                                   RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "SQL�����G���["
               EXIT
           END-IF.

           CALL "sqlite3_step"   USING BY REFERENCE STMT RETURNING RETCODE
           IF RETCODE = 100
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 0 BY REFERENCE VALUE-TEXT
               DISPLAY "�Ј��ԍ��F" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 1 BY REFERENCE VALUE-TEXT
               DISPLAY "�����@�@�F" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 2 BY REFERENCE VALUE-TEXT
               DISPLAY "�����@�@�F" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 3 BY REFERENCE VALUE-TEXT
               DISPLAY "�N��@�@�F" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 4 BY REFERENCE VALUE-TEXT
               DISPLAY "���ʁ@�@�F" VALUE-TEXT
           ELSE
               DISPLAY "�Y������Ј���������܂���ł����B"
           END-IF.
           CALL "sqlite3_finalize"   USING BY REFERENCE STMT
           EXIT.

       LIST-EMPLOYEE.
           DISPLAY "�Ј��ꗗ��\�����܂��F"
           MOVE "SELECT emp_id, emp_name, emp_dept, emp_age, emp_gender FROM employee;" TO SQL

           CALL "sqlite3_prepare_v2" 
                                   USING BY VALUE DB
                                         BY REFERENCE SQL
                                         BY VALUE -1
                                         BY REFERENCE STMT
                                         BY VALUE 0
                                   RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "SQL�����G���["
               EXIT
           END-IF.

           PERFORM UNTIL RETCODE NOT = 100
               CALL "sqlite3_step"   USING BY REFERENCE STMT RETURNING RETCODE
               IF RETCODE = 100
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 0 BY REFERENCE VALUE-TEXT
                   DISPLAY "�Ј��ԍ��F" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 1 BY REFERENCE VALUE-TEXT
                   DISPLAY "�����@�@�F" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 2 BY REFERENCE VALUE-TEXT
                   DISPLAY "�����@�@�F" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 3 BY REFERENCE VALUE-TEXT
                   DISPLAY "�N��@�@�F" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 4 BY REFERENCE VALUE-TEXT
                   DISPLAY "���ʁ@�@�F" VALUE-TEXT
               END-IF
           END-PERFORM.

           CALL "sqlite3_finalize"   USING BY REFERENCE STMT
           EXIT.
