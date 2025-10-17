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
           DISPLAY "=== 簡単計算機 ===".
           PERFORM UNTIL CHOICE = 5
               DISPLAY "1. 足し算"
               DISPLAY "2. 引き算"
               DISPLAY "3. 掛け算"
               DISPLAY "4. 割り算"
               DISPLAY "5. 終了"
               DISPLAY "操作を選んでください: "
               ACCEPT CHOICE
               
               EVALUATE CHOICE
                   WHEN 1 PERFORM ADD-PARA
                   WHEN 2 PERFORM SUB-PARA
                   WHEN 3 PERFORM MUL-PARA
                   WHEN 4 PERFORM DIV-PARA
                   WHEN 5 DISPLAY "さようなら！"
                   WHEN OTHER DISPLAY "無効な選択です！"
               END-EVALUATE
           END-PERFORM.
           STOP RUN.

       ADD-PARA.
           DISPLAY "1つ目の数字を入力してください: "
           ACCEPT NUM1.
           DISPLAY "2つ目の数字を入力してください: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 + NUM2.
           DISPLAY "結果: " NUM1 " + " NUM2 " = " RESULT.

       SUB-PARA.
           DISPLAY "1つ目の数字を入力してください: "
           ACCEPT NUM1.
           DISPLAY "2つ目の数字を入力してください: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 - NUM2.
           DISPLAY "結果: " NUM1 " - " NUM2 " = " RESULT.

       MUL-PARA.
           DISPLAY "1つ目の数字を入力してください: "
           ACCEPT NUM1.
           DISPLAY "2つ目の数字を入力してください: "
           ACCEPT NUM2.
           COMPUTE RESULT = NUM1 * NUM2.
           DISPLAY "結果: " NUM1 " * " NUM2 " = " RESULT.

       DIV-PARA.
           DISPLAY "1つ目の数字を入力してください: "
           ACCEPT NUM1.
           DISPLAY "2つ目の数字を入力してください: "
           ACCEPT NUM2.
           IF NUM2 = 0 THEN
               DISPLAY "エラー: 0で割ることはできません！"
           ELSE
               COMPUTE RESULT = NUM1 / NUM2
               DISPLAY "結果: " NUM1 " / " NUM2 " = " RESULT
           END-IF.
