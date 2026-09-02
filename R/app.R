# app.R （プロジェクトのルート直下に配置）

# インストール済みのパッケージを読み込む
library(ENQviewpkg)

# パッケージ内の Shiny アプリ起動関数を実行する
# ※関数名が run_app() や ENQviewpkg() など、パッケージ内で定義した名前に合わせて変更してください
ENQviewpkg::run_app()
