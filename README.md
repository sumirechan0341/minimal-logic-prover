# minimal-logic-proover

## How to build

1. stackを入れる
2. stack build
3. stack exec minimal-logic-proover-exe

## Usage
  * (前提) |-? (証明したいもの) のフォーマットで問題を受け付ける


  * Tactic
    * intro [(仮説の名前)]
    * apply 仮説の名前
    * assumption
    * exact 仮説の名前


  * Command
    * log ファイル名 ... 証明のlogを./.log/ファイル名.logのファイルで吐く
    * undo ... 証明を1ステップ戻す
    * quit | :q ... 証明を終了する

## Todo
  + [x] printHypothesisやprintGoalで誤った論理式を表示してしまう（括弧を補う）
  + [ ] ログファイルが絶望的に汚い
  + [x] 空のコマンドを入れると落ちる（TacticのIDかなにかに割り振る）
  + [x] 証明すべきサブゴールの数を表示する
  + [x] 証明が終わったあとにコマンドを受け付けるようにする
