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
           DISPLAY "   社員情報管理システム（COBOL）"
           DISPLAY "==================================="

           OPEN I-O EMP-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "EMP.DAT ファイルが存在しません。"
               DISPLAY "新規作成します。"
               OPEN OUTPUT EMP-FILE
               CLOSE EMP-FILE
               OPEN I-O EMP-FILE
           END-IF

           PERFORM MAIN-LOOP

           CLOSE EMP-FILE
           DISPLAY "==============================="
           DISPLAY "   ご利用ありがとうございました。"
           DISPLAY "==============================="
           STOP RUN.

       MAIN-LOOP.
           PERFORM UNTIL WS-ANSWER = "Q"
               DISPLAY "-----------------------------------"
               DISPLAY "操作を選択してください："
               DISPLAY "1: 新しい社員を追加"
               DISPLAY "2: 既存社員情報を確認"
               DISPLAY "Q: 終了"
               ACCEPT WS-ANSWER

               EVALUATE WS-ANSWER
                   WHEN "1"
                       PERFORM ADD-EMP
                   WHEN "2"
                       PERFORM QUERY-EMP
                   WHEN "Q"
                       DISPLAY "プログラムを終了します。"
                   WHEN OTHER
                       DISPLAY "無効な選択です。"
                       DISPLAY "もう一度入力してください。"
               END-EVALUATE
           END-PERFORM.

       ADD-EMP.
           DISPLAY "新入社員番号を入力してください："
           ACCEPT WS-EMP-NO
           DISPLAY "氏名を入力してください："
           ACCEPT WS-EMP-NAME
           DISPLAY "部署を入力してください："
           ACCEPT WS-EMP-DEPT
           DISPLAY "入社年（YYYY）を入力してください："
           ACCEPT WS-EMP-JOIN
           DISPLAY "初期パスワードを入力してください："
           ACCEPT WS-EMP-PASSWORD

           MOVE WS-EMP-NO TO F-EMP-NO
           MOVE WS-EMP-NAME TO F-EMP-NAME
           MOVE WS-EMP-DEPT TO F-EMP-DEPT
           MOVE WS-EMP-JOIN TO F-EMP-JOIN
           MOVE WS-EMP-PASSWORD TO F-EMP-PASSWORD

           WRITE EMP-REC-FILE
           DISPLAY "登録完了！"
           DISPLAY "-----------------------------------"
           DISPLAY "社員番号：" F-EMP-NO
           DISPLAY "氏名：" F-EMP-NAME
           DISPLAY "部署：" F-EMP-DEPT
           DISPLAY "入社年：" F-EMP-JOIN
           DISPLAY "-----------------------------------".

       QUERY-EMP.
           DISPLAY "社員番号を入力してください："
           ACCEPT WS-EMP-NO
           DISPLAY "パスワードを入力してください："
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
                          DISPLAY "社員情報："
                          DISPLAY "社員番号：" F-EMP-NO
                          DISPLAY "氏名：" F-EMP-NAME
                          DISPLAY "部署：" F-EMP-DEPT
                          DISPLAY "入社年：" F-EMP-JOIN
                          DISPLAY "-----------------------------------"
                          MOVE "Y" TO FOUND-FLAG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE EMP-FILE

           IF FOUND-FLAG = "N"
               DISPLAY "社員番号またはパスワードが違います。"
           END-IF.
