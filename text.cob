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

       *> 正しい社員データ（3人）
       01 EMP-TABLE.
           05 EMP-REC OCCURS 3 TIMES.
              10 EMP-NO       PIC 9(5).
              10 EMP-NAME     PIC X(20).
              10 EMP-DEPT     PIC X(15).
              10 EMP-JOIN     PIC 9(4).
              10 EMP-PASSWORD PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARA.

           *> 初期データ設定
           MOVE 10001 TO EMP-NO (1)
           MOVE "田中 太郎" TO EMP-NAME (1)
           MOVE "営業部"   TO EMP-DEPT (1)
           MOVE 2018 TO EMP-JOIN (1)
           MOVE "PASS100" TO EMP-PASSWORD (1)

           MOVE 10002 TO EMP-NO (2)
           MOVE "佐藤 花子" TO EMP-NAME (2)
           MOVE "総務部"   TO EMP-DEPT (2)
           MOVE 2020 TO EMP-JOIN (2)
           MOVE "PASS200" TO EMP-PASSWORD (2)

           MOVE 10003 TO EMP-NO (3)
           MOVE "鈴木 一郎" TO EMP-NAME (3)
           MOVE "開発部"   TO EMP-DEPT (3)
           MOVE 2021 TO EMP-JOIN (3)
           MOVE "PASS300" TO EMP-PASSWORD (3)

           DISPLAY "==================================="
           DISPLAY "   社員ログインシステム（COBOL）"
           DISPLAY "==================================="

           PERFORM LOGIN-LOOP

           PERFORM END-PROGRAM
           STOP RUN.

       LOGIN-LOOP.
           MOVE "N" TO FOUND-FLAG

           DISPLAY "社員番号を入力してください："
           ACCEPT EMP-ID
           DISPLAY "パスワードを入力してください："
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
              DISPLAY "ログイン成功！ようこそ社員様。"
              DISPLAY "社員番号：" EMP-NO (IDX)
              DISPLAY "氏名：" EMP-NAME (IDX)
              DISPLAY "所属部署：" EMP-DEPT (IDX)
              DISPLAY "入社年：" EMP-JOIN (IDX)
              DISPLAY "-----------------------------------"
           ELSE
              DISPLAY "ログイン失敗。番号またはパスワードが違います。"
              DISPLAY "もう一度試しますか？ (Y/N)："
              ACCEPT ANSWER
              IF ANSWER = "Y" OR ANSWER = "y"
                 PERFORM LOGIN-LOOP
              ELSE
                 DISPLAY "システムを終了します。"
           END-IF.

       END-PROGRAM.
           DISPLAY "==============================="
           DISPLAY "   ご利用ありがとうございました。"
           DISPLAY "==============================="
           .
