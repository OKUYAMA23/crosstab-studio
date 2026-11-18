# CrossTab Studio を起動する（Windows 用）。
#   1. R を探す（このフォルダーの中の R → パソコンに入っている R の順）
#   2. 起動画面を開く（Edge、なければ Chrome のアプリ用の窓）
#   3. 裏で R を起動する（黒い窓は出さない）。準備ができると、起動画面がアプリの画面に替わる
param([switch]$ShowLog, [switch]$BrowserTab, [switch]$SplashShown, [string]$Token = '')
if (-not $Token) { $Token = [string](Get-Random -Minimum 100000 -Maximum 999999999) }
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$app  = Join-Path $root 'app'

function Show-Message([string]$text, [string]$buttons = 'OK') {
  Add-Type -AssemblyName System.Windows.Forms
  return [System.Windows.Forms.MessageBox]::Show($text, 'CrossTab Studio', $buttons, 'Information')
}
function Find-Rscript {
  $c = New-Object System.Collections.Generic.List[string]
  $c.Add((Join-Path $root 'R\bin\Rscript.exe'))
  foreach ($k in 'HKCU:\SOFTWARE\R-core\R', 'HKLM:\SOFTWARE\R-core\R', 'HKLM:\SOFTWARE\WOW6432Node\R-core\R') {
    try { $p = (Get-ItemProperty -Path $k -ErrorAction Stop).InstallPath; if ($p) { $c.Add((Join-Path $p 'bin\Rscript.exe')) } } catch {}
  }
  foreach ($base in (Join-Path $env:ProgramFiles 'R'), (Join-Path $env:LOCALAPPDATA 'Programs\R')) {
    if (Test-Path $base) {
      Get-ChildItem $base -Directory -Filter 'R-*' |
        Sort-Object { try { [version]($_.Name -replace '^R-', '' -replace '[^0-9.]', '') } catch { [version]'0.0' } } -Descending |
        ForEach-Object { $c.Add((Join-Path $_.FullName 'bin\Rscript.exe')) }
    }
  }
  foreach ($p in $c) { if (Test-Path $p) { return $p } }
  $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  return $null
}
function Find-Browser {
  $c = @((Join-Path ${env:ProgramFiles(x86)} 'Microsoft\Edge\Application\msedge.exe'),
         (Join-Path $env:ProgramFiles 'Microsoft\Edge\Application\msedge.exe'),
         (Join-Path $env:ProgramFiles 'Google\Chrome\Application\chrome.exe'),
         (Join-Path ${env:ProgramFiles(x86)} 'Google\Chrome\Application\chrome.exe'),
         (Join-Path $env:LOCALAPPDATA 'Google\Chrome\Application\chrome.exe'))
  foreach ($p in $c) { if ($p -and (Test-Path $p)) { return $p } }
  return $null
}
# すでに動いている CrossTab Studio を探す。待ち受けている番号だけに問い合わせるので速い
function Find-Running {
  $open = @()
  try {
    $open = [System.Net.NetworkInformation.IPGlobalProperties]::GetIPGlobalProperties().GetActiveTcpListeners() |
      Where-Object { $_.Port -ge 47815 -and $_.Port -le 47824 } | ForEach-Object { $_.Port } | Sort-Object -Unique
  } catch { $open = 47815..47824 }
  foreach ($port in $open) {
    try {
      $rq = [System.Net.HttpWebRequest]::Create("http://127.0.0.1:$port/api/ping")
      $rq.Timeout = 2000; $rq.Proxy = $null
      $rs = $rq.GetResponse(); $sr = New-Object System.IO.StreamReader($rs.GetResponseStream()); $txt = $sr.ReadToEnd(); $rs.Close()
      if ($txt -match 'CrossTabStudio') { return $port }
    } catch {}
  }
  return $null
}
# 起動画面に番号（ポート）を知らせるファイル。R が起動時に書くが、すでに動いているときはここで書く
function Write-Ready([int]$port) {
  $ms = [long]([DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds())
  try { [System.IO.File]::WriteAllText((Join-Path $app 'www\ready.js'), "window.CTS = { port: $port, t: $ms, k: '$Token' };") } catch {}
}
function Write-Log([string]$text) {
  try { Add-Content -Path (Join-Path $root 'launch.log') -Value ((Get-Date -Format 'yyyy-MM-dd HH:mm:ss') + ' ' + $text) -Encoding UTF8 } catch {}
}
function Open-Window([string]$url) {
  $b = Find-Browser
  if ($b -and -not $BrowserTab) { Start-Process -FilePath $b -ArgumentList "--app=$url", '--window-size=1320,860' }
  else { Start-Process $url }
}

# すでに起動しているときは、その画面を開くだけにする（起動画面が開いていれば、起動画面がつなぎ直す）
trap { Write-Log ('エラー: ' + $_.Exception.Message); Show-Message ('起動できませんでした。' + "`n`n" + $_.Exception.Message + "`n`n" + 'CrossTabStudio_debug.bat を実行すると、詳しい経過が表示されます。') | Out-Null; exit 1 }
try { Remove-Item (Join-Path $root 'launch.log') -ErrorAction SilentlyContinue } catch {}
Write-Log ('起動 PowerShell ' + $PSVersionTable.PSVersion + ' / ' + $root)
$port = Find-Running
if ($port) { Write-Log "すでに動いています（$port）"; Write-Ready $port; if (-not $SplashShown) { Open-Window "http://127.0.0.1:$port/" }; exit 0 }

$rscript = Find-Rscript
if (-not $rscript) {
  $a = Show-Message "計算に使う R が見つかりません。`n`nこのフォルダーの中に R を入れますか（初回のみ。インターネット接続が必要で、数分かかります）。" 'YesNo'
  if ($a -eq 'Yes') { Start-Process -FilePath 'powershell.exe' -ArgumentList '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "`"$(Join-Path $PSScriptRoot 'setup_R.ps1')`"" -Wait; $rscript = Find-Rscript }
  if (-not $rscript) { Show-Message 'R を用意できなかったため、起動をやめました。setup_R.bat を実行するか、R（4.2 以上）を入れてから、もう一度起動してください。' | Out-Null; exit 1 }
}

$runArgs = @('--vanilla', "`"$(Join-Path $app 'run.R')`"")
$browser = Find-Browser
if ($ShowLog) {
  # 経過を表示する起動：R がふだんのブラウザで画面を開く
  & $rscript --vanilla (Join-Path $app 'run.R') --browser
  exit $LASTEXITCODE
}
if ($browser -and -not $BrowserTab) {
  if (-not $SplashShown) {
    $splash = ([System.Uri](Join-Path $app 'www\splash.html')).AbsoluteUri + '?k=' + $Token
    Start-Process -FilePath $browser -ArgumentList "--app=$splash", '--window-size=1320,860'
  }
  $runArgs += @('--noopen', '--fallback', "--token=$Token")
} else {
  $runArgs += '--browser'
}
Write-Log ('R: ' + $rscript)
Start-Process -FilePath $rscript -ArgumentList $runArgs -WindowStyle Hidden -WorkingDirectory $app
Write-Log 'R を起動しました'
