# CrossTab Studio の初回の準備（Windows 用）。
#   このフォルダーの中（R フォルダー）に R を入れ、app\library に httpuv と jsonlite を入れる。
#   インターネット接続が必要。終わったあとは、インターネットなしで起動できる。
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$dest = Join-Path $root 'R'
$lib  = Join-Path $root 'app\library'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
$ProgressPreference = 'SilentlyContinue'
$rscript = Join-Path $dest 'bin\Rscript.exe'

if (-not (Test-Path $rscript)) {
  Write-Host '[1/3] R の入手先を確認しています…'
  $base = 'https://cloud.r-project.org/bin/windows/base/'
  $page = Invoke-WebRequest -UseBasicParsing -Uri $base
  $m = [regex]::Match($page.Content, 'R-[0-9]+\.[0-9]+\.[0-9]+-win\.exe')
  if (-not $m.Success) { throw 'R のインストーラーが見つかりませんでした。' }
  $tmp = Join-Path $env:TEMP $m.Value
  Write-Host "[2/3] $($m.Value) をダウンロードしています（約 85 MB）…"
  Invoke-WebRequest -UseBasicParsing -Uri ($base + $m.Value) -OutFile $tmp
  Write-Host "      $dest に入れています（1〜3 分）…"
  $p = Start-Process -FilePath $tmp -Wait -PassThru -ArgumentList '/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/CURRENTUSER', '/NOICONS', "/DIR=`"$dest`"", '/COMPONENTS=main,x64', '/MERGETASKS=!desktopicon,!quicklaunchicon,!associate,!recordversion'
  Remove-Item $tmp -ErrorAction SilentlyContinue
  if (-not (Test-Path $rscript)) { throw "R を入れられませんでした（終了コード $($p.ExitCode)）。" }
} else {
  Write-Host '[1/3] R はすでに入っています。'
}
Write-Host '[3/3] 部品（httpuv、jsonlite）を入れています…'
New-Item -ItemType Directory -Force -Path $lib | Out-Null
$libR = $lib -replace '\\', '/'
& $rscript --vanilla -e "install.packages(c('httpuv','jsonlite'), lib='$libR', repos='https://cloud.r-project.org', quiet=TRUE); ok <- all(c('httpuv','jsonlite') %in% rownames(installed.packages(lib.loc='$libR'))); quit(status = if (ok) 0 else 1)"
if ($LASTEXITCODE -ne 0) { throw '部品を入れられませんでした。インターネットの接続を確かめて、もう一度実行してください。' }
Write-Host ''
Write-Host '準備ができました。CrossTabStudio.bat をダブルクリックすると起動します。'
