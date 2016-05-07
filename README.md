ttask
=================================================

皆さん、進捗どうですか。

これは何
-------------------------------------------------

スクラムというアジャイル開発手法で用いられるスケジュール管理手法に感銘を受けて開発された、
セルフマネジメントを行うための簡単なタスク管理ツールです。

ちゅーんさんが作ったタスク管理ツール……`tune san no task kanri tool`を縮めて`ttask`です。どうも。

スプリントやプロダクトバックログといった仕組みを、個人のプロジェクトや、自学等に応用する事を目的とし、
最小限の機能をCUI上でなるべく簡単に操作出来る事を目的として開発しています。

尚、ドッグフーディングしながらの開発のため、当面の間は後方互換を保ちながら開発予定です。

あくまでベータ版のため、不具合が残っている可能性が高い事、使い方や表示等の変更はしばらく続く事、
まだどのくらい大なデータの扱いに耐えられるのかが不明な事等、色々と不完全な状態ですので、ご了承ください。

哲学
-------------------------------------------------

* スケジュールや目標は頻繁に変更されるものなので、柔軟に対応出来る事が求められる
* 正確な見積は困難なので、相対見積もりと実績から徐々に制度を上げていく仕組みが求められる
* 作業の振り返りを行い今後の改善に繋げるため、様々な集計機能が求められる

インストール
------------------------------------------------- 

Stackをインストールした状態でこのリポジトリを`clone`して。

```
stack install
``` 

を実行してください。

正しくインストールできていれば、
以下のように`ttask -?`でｇｄｇｄな英語で書かれたヘルプが表示されるはずです。

```
$ ttask -?
Usage: ttask [OPTION...] <COMMAND> [ARGS...]
  Task management tool for yourself is inspired by scrum.

Options:
  -?     --help         display this help and exit
  -v[n]  --verbose[=n]  set verbosity level

Commands: 
  project     Switch other project. Or list all projects when project id is abbreviated.
  add         Add contents to active project
  delete      Delete contents from active project
  move        Move contents of active project
  swap        Swap contents of active project
  active      Show active projects information
  pbl         List all pbl stories
  sprint      List all sprint, Or show sprints detail when project id is abbreviated.
  story       Show target story information and all tasks
  run         Update contents status to `Running`
  finish      Update contents status to `Finish`
  wait        Update contents status to `Wait`
  nota        Update contents status to `Not Achieved`
  reject      Update contents status to `Rejected`
```

用語 
------------------------------------------------- 

スクラムの手法はやや複雑なので、簡単に用語を説明します。

以下は、本ツールを用いて、本ツールそのものの今後の改修予定を管理している例です。

```
$ ttask active
PROJECT - ttask : 11pt [ Wait ] ttaskへの永続的な機能追加／修正

Active sprint(s) :
SPRINT - 1 : 11pt [ Running ] ttaskの公開
To Wait            at 2016-05-03 17:29:30.337829
To Running         at 2016-05-03 17:39:45.119291
　　STORY - 1 : 11pt [ Running ] 作業者はセルフマネジメントのためttaskをgithubからインストール出来る
　　　　TASK - 1 : 3pt [ Finished ] サンプルのプロジェクトを作成しながら標準出力を記録
　　　　TASK - 2 : 5pt [ Running ] README作成
　　　　TASK - 3 : 3pt [ Wait ] githubにリポジトリ作成〜pushまで

Product backlog :
STORY - 4 : 0pt [ Wait ] 作業者は現在よりもより手軽にPBLを並び替える事の出来るttaskを使う事が出来る
STORY - 9 : 0pt [ Wait ] 作業者が誤入力しても大丈夫なように一度作成したコンテンツの説明を修正出来る
STORY - 2 : 0pt [ Wait ] 作業者はレビューのためttaskで記録したスプリントのログを照会出来る
STORY - 5 : 0pt [ Wait ] 作業者はswapやmoveを失敗した際に、その失敗を直ちに認知する事が出来る
STORY - 11 : 0pt [ Wait ] 作業者は作業の相対見積もりを正しく行うためにいつでも基準となるタスクを見れる
STORY - 7 : 0pt [ Wait ] 作業者は実際に作業を行った作業時間を記録する事が出来る
STORY - 8 : 0pt [ Wait ] スクラムを知らない作業者はttaskの管理の仕組みを理解するための詳細な文章を読む事が出来る
STORY - 6 : 0pt [ Wait ] 作業者は日々の進捗状況を正確に把握するためのバーンダウンチャートを見る事が出来る
STORY - 10 : 0pt [ Wait ] 忙しい作業者は正しい振り返りのため複数のプロジェクトにまたがったログを照会出来る
```

* 要求仕様を自然言語で簡潔に記したものをユーザーストーリー(`STORY`)と呼びます
    + 書き方がちょっとルール違反だねごめんね dear 識者
* ストーリーは、優先度の高いものからプロダクトバックログ(`Product backlog`)と呼ばれるリストに並べられます
* スケジュールを決める作業（プランニング）は、スプリント(`SPRINT`)と呼ばれる短い期間で区切って行います
    + だいたい一週間〜四週間くらい
    + プロダクトバックログのストーリーのうち、優先度の高いものから順にスプリントで処理します
* タスク(`TASK`)はストーリーを実現可能にする最小の作業単位です
    + タスクの作業量は時間ではなく「ポイント(`pt`)」と呼ばれる単位で相対的に見積もります

ストーリーの切り方とかポイントの見積もり方とかヴェロシティとか、
詳しい事はそのうち（上記例で言う所の`STORY - 8`）Wikiあたりに纏めます。 

あとはスクラムとかアジャイル関係の書籍とか文章を色々当たってみてください。

使い方
-------------------------------------------------

Helpだけだと多分無理ゲーなのでざっと使い方をまとめました。  
わりと大急ぎで書き上げたので誤植いっぱいだったらごめんね。

### プロジェクトの作成

`ttask add project -n [プロジェクトの説明] [プロジェクトの識別名]`で新しいプロジェクトを作成します。

```
ttask add project -n "サンプルプロジェクトです" sample
```

`ttask project` で現在のプロジェクトを一覧表示出来ます。`[ ]` で括られているのが現在作業中のプロジェクトです。

```
$ ttask project
  ttask
 [sample]
  test
```

また、作業中のプロジェクトを切り替えたい場合は`ttask project -i [プロジェクトの識別名]`です。

### ユーザーストーリーの追加／並べ替え

`ttask add story [ストーリーの内容]` でプロダクトバックログに対してストーリーを追加します。

```
$ ttask add story "Aさんは〜が出来る。それは〜だからだ。"
$ ttask add story "Bさんは〜が出来る。それは〜だからだ。"
$ ttask add story "Cさんは〜が出来る。それは〜だからだ。"
```

`ttask active`でプロジェクトの現在の状況を見る事ができます。

```
$ ttask active
PROJECT - sample : 0pt [ Wait ] サンプルプロジェクトです
Running sprint is nothing

Product backlog :
STORY - 1 : 0pt [ Wait ] Aさんは〜が出来る。それは〜だからだ。
STORY - 2 : 0pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
STORY - 3 : 0pt [ Wait ] Cさんは〜が出来る。それは〜だからだ。
```

ストーリーは優先して実現したい事が上にくるように並べ替えを行います。

`ttask move story -i [ストーリーのID]` でそのストーリーが先頭に移動します。
また、`ttask swap story -f [入れ替え元ID] -t [入れ替え先ID]`で二つのストーリーを入れ替えられます。

```
$ ttask move story -i 3
$ ttask swap story -f 2 -t 1
$ ttask active
PROJECT - sample : 0pt [ Wait ] サンプルプロジェクトです
Running sprint is nothing

Product backlog :
STORY - 3 : 0pt [ Wait ] Cさんは〜が出来る。それは〜だからだ。
STORY - 2 : 0pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
STORY - 1 : 0pt [ Wait ] Aさんは〜が出来る。それは〜だからだ。
```

プロダクトバックログは常に更新され続ける事が良いとされているのですが、
この順番を並び替える機能はまだ足りていないので今後追加して管理しやすくなる予定です。

### スプリントの作成

`ttask add sprint [スプリントの説明]`でスプリントが追加されます。
`ttask sprint`とだけ入力すると、（現在はひとつだけですが）全てのスプリントが一覧表示されます。

```
$ ttask add sprint "プロジェクト開始、重要度の高い機能をとりあえず作る"
$ ttask sprint
SPRINT - 1 : 0pt [ Wait ] プロジェクト開始、重要度の高い機能をとりあえず作る
```

新しく追加したスプリントのIDがこれで`1`だとわかりましたので、
`ttask move story -i [移動させるストーリーのID] -t [移動先スプリントのID]`というコマンドで、
このスプリントに対してプロダクトバックログの優先度の高いユーザーストーリーを移動させます。

```
$ ttask move story -i 3 -t 1
$ ttask move story -i 2 -t 1
```

スプリントの詳細を見るためには、`ttask sprint -i [見たいスプリントのID]`を入力です。

```
$ ttask sprint -i 1
SPRINT - 1 : 0pt [ Wait ] プロジェクト開始、重要度の高い機能をとりあえず作る
To Wait            at 2016-05-03 17:59:42.212454
　　STORY - 3 : 0pt [ Wait ] Cさんは〜が出来る。それは〜だからだ。
　　STORY - 2 : 0pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
```

### プランニング

`ttask add task -i [追加先のユーザーストーリー] -p [見積もりポイント] [タスクの説明]`というコマンドで、
ストーリーに対してタスクを追加して行きます。

```
$ ttask add task -i 3 -p 3 "機能Xを作る"
$ ttask add task -i 3 -p 1 "機能Yを作る"
$ ttask add task -i 2 -p 2 "機能Zを作るための技術Nについて調べる"
$ ttask add task -i 2 -p 5 "機能Zを作る"
$ ttask sprint -i 1
SPRINT - 1 : 11pt [ Wait ] プロジェクト開始、重要度の高い機能をとりあえず作る
To Wait            at 2016-05-03 17:59:42.212454
　　STORY - 3 : 4pt [ Wait ] Cさんは〜が出来る。それは〜だからだ。
　　　　TASK - 1 : 3pt [ Wait ] 機能Xを作る
　　　　TASK - 2 : 1pt [ Wait ] 機能Yを作る
　　STORY - 2 : 7pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
　　　　TASK - 3 : 2pt [ Wait ] 機能Zを作るための技術Nについて調べる
　　　　TASK - 4 : 5pt [ Wait ] 機能Zを作る
```

ちなみに、削除する時は`ttask delete task -i [削除するタスクのID]`です。

プランニングが完了したので、実際に作業を開始して行きましょう。
`ttask run [task/story/sprint] -i [それぞれのID]`でそのタスクの状態が`Wait`から`Running`に更新されます。

これで、スプリントが`Running`状態になったので、`ttask active`コマンドの結果に、
スプリントの状態が表示されるようになりました。

```
$ ttask run task -i 1
$ ttask run story -i 3
$ ttask run sprint -i 1
$ ttask active
PROJECT - sample : 11pt [ Wait ] サンプルプロジェクトです

Active sprint(s) :
SPRINT - 1 : 11pt [ Running ] プロジェクト開始、重要度の高い機能をとりあえず作る
To Wait            at 2016-05-03 17:59:42.212454
To Running         at 2016-05-03 18:09:39.96355
　　STORY - 3 : 4pt [ Running ] Cさんは〜が出来る。それは〜だからだ。
　　　　TASK - 1 : 3pt [ Running ] 機能Xを作る
　　　　TASK - 2 : 1pt [ Wait ] 機能Yを作る
　　STORY - 2 : 7pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
　　　　TASK - 3 : 2pt [ Wait ] 機能Zを作るための技術Nについて調べる
　　　　TASK - 4 : 5pt [ Wait ] 機能Zを作る

Product backlog :
STORY - 1 : 0pt [ Wait ] Aさんは〜が出来る。それは〜だからだ。
```

尚、この`run`されたタイミングを元に、今後追加されるタスクが計画内作業なのか、
予想外の追加作業なのか判別する事が出来るので、`run`するのはプランニングが終了してからの方が良いです。

実際に、`STORY`や`TASK`等のステータスが変更されたタイミングも記録されているので、
便利そうな照会機能や集計機能は思いついた時に追加されていくかもしれません。

### タスクの完了

タスクが完了した場合`ttask finish task -i [タスクのID]`コマンドでステータスを`Finished`に変更出来ます。

また、作業中に新たな追加作業が生じた場合はどんどん追加していきましょう。

```
$ ttask finish task -i 1
$ ttask run task -i 2
$ ttask active
PROJECT - sample : 14pt [ Wait ] サンプルプロジェクトです

Active sprint(s) :
SPRINT - 1 : 14pt [ Running ] プロジェクト開始、重要度の高い機能をとりあえず作る
To Wait            at 2016-05-03 17:59:42.212454
To Running         at 2016-05-03 18:09:39.96355
　　STORY - 3 : 7pt [ Running ] Cさんは〜が出来る。それは〜だからだ。
　　　　TASK - 1 : 3pt [ Finished ] 機能Xを作る
　　　　TASK - 2 : 1pt [ Running ] 機能Yを作る
　　　　TASK - 5 : 3pt [ Wait ] 機能Wを作る
　　STORY - 2 : 7pt [ Wait ] Bさんは〜が出来る。それは〜だからだ。
　　　　TASK - 3 : 2pt [ Wait ] 機能Zを作るための技術Nについて調べる
　　　　TASK - 4 : 5pt [ Wait ] 機能Zを作る

Product backlog :
STORY - 1 : 0pt [ Wait ] Aさんは〜が出来る。それは〜だからだ。
```

全てのタスクが完了したら同じ要領で`story`や`sprint`も`Finished`に変更します。これでスプリント作業完了です。

### その他

現在の所スプリント登録時に期間を定める仕組みは無いのですが、一度決めたスプリント期間は尊守し、
それまでに完了しなかった作業は次期スプリントに繰り越すのが良いとされています。

また。実際に作業してみたら不要になったタスク等が出てくる事もあるでしょう。
そのため、以下の二つのステータスが用意されています。

* `ttask nota -i [ID]` で完了出来なかった事を表す`Not Achieved`に更新
* `ttask reject -i [ID]` でタスクやストーリー等を却下した事を表す`Rejected`に更新

これらもおいおい集計材料に使えるため、上手く活用すると良いでしょう。

