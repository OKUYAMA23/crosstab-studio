# CrossTab Studio

アンケート（Google フォームの回答 CSV など）の集計・検定・図表・報告書の作成を行う Web アプリです。計算はブラウザの中で動く R（webR）が行い、データは端末の外に送りません。

- アプリ：https://okuyama23.github.io/crosstab-studio/
- ライセンス：MIT（`LICENSE`）。同梱の webR（GPL-3.0）、pdf-lib・fontkit・HarfBuzz（MIT）、字体（SIL OFL 1.1）は、それぞれのライセンスに従います。
- 引用：`CITATION.cff` を参照してください（GitHub の「Cite this repository」からも書式を得られます）。

## フォルダの構成

| 場所 | 内容 |
|---|---|
| リポジトリの直下 | 公開している Web 版の一式（GitHub Pages で配信） |
| `src/app/` | 画面（`www/`）と R のコード（`R/`）。パソコン版とWeb版で共通 |
| `src/web/` | Web 版だけの部品（webR との橋渡し、スマートフォン用の画面、PWA）と組み立てスクリプト |
| `src/*.bat`, `src/tools/` | Windows のパソコン版（R をインストールして使う）の起動用ファイル |

## Web 版の組み立て

```
npm pack webr@0.6.1 && tar xzf webr-0.6.1.tgz      # webR の一式（package/dist）
CTS_OUT=../site WEBR_DIST=package/dist python src/web/build_web.py
```

## English

CrossTab Studio is a browser-based app for tabulating and testing questionnaire data. It runs R in the browser via webR, so data stay on the user's device. Licensed under the MIT License; bundled components keep their own licenses.
