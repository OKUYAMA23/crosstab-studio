# デスクトップに CrossTab Studio のショートカットを作る（アイコン付き）。
# ショートカットから起動すると、黒いコマンドの窓を出さずに起動する。
$ErrorActionPreference = 'Stop'
trap {
  Write-Host ''
  Write-Host ('ショートカットを作れませんでした：' + $_.Exception.Message) -ForegroundColor Red
  Write-Host '会社や学校のパソコンでは、ショートカットの作成やスクリプトの実行が制限されていることがあります。'
  Write-Host 'その場合は、CrossTabStudio.bat を右クリック →「その他のオプションを確認」→「送る」→「デスクトップ (ショートカットを作成)」で作れます。'
  exit 1
}
$root = Split-Path -Parent $PSScriptRoot
$icon = Join-Path $root 'app\www\CrossTabStudio.ico'
$vbs  = Join-Path $root 'tools\start_hidden.vbs'
$ws = New-Object -ComObject WScript.Shell
$made = @()
# デスクトップ・スタートメニュー・このフォルダーの 3 か所に作る（このフォルダーのものは、好きな場所へ移して使える）
foreach ($dir in @([Environment]::GetFolderPath('Desktop'), [Environment]::GetFolderPath('Programs'), $root)) {
  if (-not $dir -or -not (Test-Path $dir)) { continue }
  $path = Join-Path $dir 'CrossTab Studio.lnk'
  $lnk = $ws.CreateShortcut($path)
  $lnk.TargetPath = Join-Path $env:SystemRoot 'System32\wscript.exe'
  $lnk.Arguments = '"' + $vbs + '"'
  $lnk.WorkingDirectory = $root
  $lnk.IconLocation = $icon + ',0'
  $lnk.Description = 'CrossTab Studio（アンケートの集計と検定）'
  $lnk.Save()
  $made += $path
}
Write-Host ''
Write-Host 'ショートカットを作りました：'
$made | ForEach-Object { Write-Host ('  ' + $_) }
Write-Host ''
Write-Host 'デスクトップの「CrossTab Studio」をダブルクリックすると起動します（スタートメニューにも入れました）。'
Write-Host 'このフォルダーを別の場所に移したときは、もう一度この make_shortcut.bat を実行してください。'
