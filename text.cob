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
           VALUE "1.登録  2.検索  3.一覧  9.終了 → ".
       01 MSG-END           PIC X(40)
           VALUE "システムを終了します。".
       01 MSG-INVALID       PIC X(40)
           VALUE "無効な選択です。再入力してください。".
           
       01 DB-NAME PIC X(64) VALUE "employee.db".    

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " 社員管理システム（SQLite）".

           CALL "my_sqlite3_open" 
                               USING BY REFERENCE DB-NAME
                                     BY REFERENCE DB
                               RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "データベース接続エラー"
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
               DISPLAY "テーブル初期化エラー"
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
           DISPLAY "社員番号を入力してください：" WITH NO ADVANCING
           ACCEPT WS-EMP-ID
           DISPLAY "氏名を入力してください：" WITH NO ADVANCING
           ACCEPT WS-EMP-NAME
           DISPLAY "部署を入力してください：" WITH NO ADVANCING
           ACCEPT WS-EMP-DEPT
           DISPLAY "年齢を入力してください：" WITH NO ADVANCING
           ACCEPT WS-EMP-AGE
           DISPLAY "性別（男/女）を入力してください：" WITH NO ADVANCING
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
               DISPLAY "登録が完了しました。"
           ELSE
               DISPLAY "登録エラー。"
           END-IF.
           EXIT.

       SEARCH-EMPLOYEE.
           DISPLAY "検索する社員番号を入力してください：" WITH NO ADVANCING
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
               DISPLAY "SQL準備エラー"
               EXIT
           END-IF.

           CALL "sqlite3_step"   USING BY REFERENCE STMT RETURNING RETCODE
           IF RETCODE = 100
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 0 BY REFERENCE VALUE-TEXT
               DISPLAY "社員番号：" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 1 BY REFERENCE VALUE-TEXT
               DISPLAY "氏名　　：" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 2 BY REFERENCE VALUE-TEXT
               DISPLAY "部署　　：" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 3 BY REFERENCE VALUE-TEXT
               DISPLAY "年齢　　：" VALUE-TEXT
               CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 4 BY REFERENCE VALUE-TEXT
               DISPLAY "性別　　：" VALUE-TEXT
           ELSE
               DISPLAY "該当する社員が見つかりませんでした。"
           END-IF.
           CALL "sqlite3_finalize"   USING BY REFERENCE STMT
           EXIT.

       LIST-EMPLOYEE.
           DISPLAY "社員一覧を表示します："
           MOVE "SELECT emp_id, emp_name, emp_dept, emp_age, emp_gender FROM employee;" TO SQL

           CALL "sqlite3_prepare_v2" 
                                   USING BY VALUE DB
                                         BY REFERENCE SQL
                                         BY VALUE -1
                                         BY REFERENCE STMT
                                         BY VALUE 0
                                   RETURNING RETCODE.
           IF RETCODE NOT = 0
               DISPLAY "SQL準備エラー"
               EXIT
           END-IF.

           PERFORM UNTIL RETCODE NOT = 100
               CALL "sqlite3_step"   USING BY REFERENCE STMT RETURNING RETCODE
               IF RETCODE = 100
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 0 BY REFERENCE VALUE-TEXT
                   DISPLAY "社員番号：" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 1 BY REFERENCE VALUE-TEXT
                   DISPLAY "氏名　　：" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 2 BY REFERENCE VALUE-TEXT
                   DISPLAY "部署　　：" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 3 BY REFERENCE VALUE-TEXT
                   DISPLAY "年齢　　：" VALUE-TEXT
                   CALL "sqlite3_column_text"   USING BY REFERENCE STMT BY VALUE 4 BY REFERENCE VALUE-TEXT
                   DISPLAY "性別　　：" VALUE-TEXT
               END-IF
           END-PERFORM.

           CALL "sqlite3_finalize"   USING BY REFERENCE STMT
           EXIT.
