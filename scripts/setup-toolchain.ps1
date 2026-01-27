param(
    [string]$ToolsDir = (Join-Path $PSScriptRoot "..\\tools")
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

$toolsDirFull = Resolve-Path -LiteralPath $ToolsDir -ErrorAction SilentlyContinue
if (-not $toolsDirFull) {
    New-Item -ItemType Directory -Path $ToolsDir | Out-Null
    $toolsDirFull = Resolve-Path -LiteralPath $ToolsDir
}

$downloadDir = Join-Path $toolsDirFull "downloads"
if (-not (Test-Path $downloadDir)) {
    New-Item -ItemType Directory -Path $downloadDir | Out-Null
}

$zipPath = Join-Path $downloadDir "winlibs.zip"
$extractDir = Join-Path $toolsDirFull "winlibs"

$headers = @{ "User-Agent" = "gost-setup" }
$release = Invoke-RestMethod -Headers $headers -Uri "https://api.github.com/repos/brechtsanders/winlibs_mingw/releases/latest"

$asset = $release.assets |
    Where-Object { $_.name -match "^winlibs-x86_64-posix-seh-.*\.zip$" } |
    Select-Object -First 1

if (-not $asset) {
    throw "Could not find a winlibs GCC+LLVM zip in the latest release."
}

Write-Host "Downloading $($asset.name)..."
Invoke-WebRequest -Headers $headers -Uri $asset.browser_download_url -OutFile $zipPath

if (Test-Path $extractDir) {
    Remove-Item -Recurse -Force $extractDir
}
New-Item -ItemType Directory -Path $extractDir | Out-Null

Write-Host "Extracting..."
Expand-Archive -Path $zipPath -DestinationPath $extractDir

$binDir = Join-Path $extractDir "mingw64\\bin"
if (-not (Test-Path $binDir)) {
    $nested = Get-ChildItem -Path $extractDir -Recurse -Directory -Filter "mingw64" | Select-Object -First 1
    if ($nested) {
        $binDir = Join-Path $nested.FullName "bin"
    }
}

if (-not (Test-Path (Join-Path $binDir "gcc.exe"))) {
    throw "gcc.exe not found after extraction."
}

Write-Host ""
Write-Host "Toolchain installed."
Write-Host "GOST_CC  = $binDir\\gcc.exe"
Write-Host "GOST_LLC = $binDir\\llc.exe"
Write-Host "GOST_AR  = $binDir\\llvm-ar.exe"
Write-Host ""
Write-Host "You can now run: gs run samples\\hello.gs"
