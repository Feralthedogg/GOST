@echo off
setlocal
set "ROOT=%~dp0"
set "TOOLROOT=%ROOT%tools\\winlibs"
set "TOOLBIN=%TOOLROOT%\\mingw64\\bin"
if exist "%TOOLBIN%\\gcc.exe" (
  set "GOST_CC=%TOOLBIN%\\gcc.exe"
  if exist "%TOOLBIN%\\llc.exe" set "GOST_LLC=%TOOLBIN%\\llc.exe"
  if exist "%TOOLBIN%\\llvm-ar.exe" set "GOST_AR=%TOOLBIN%\\llvm-ar.exe"
)
if exist "%ROOT%Cargo.toml" (
  cargo build --bin gs
  if errorlevel 1 exit /b %errorlevel%
)
set "EXE=%ROOT%target\debug\gs.exe"
if exist "%EXE%" (
  "%EXE%" %*
  exit /b %errorlevel%
)
cargo run --bin gs -- %*
exit /b %errorlevel%
