@echo off
echo Building tinymath...
gcc -O2 -std=c99 -Wall -lm -o tinymath.exe tinymath.c
if errorlevel 1 (
  echo Failed to build tinymath.exe
  exit /b 1
)
echo Building test_runner...
gcc -O2 -std=c99 -Wall -o test_runner.exe test_runner.c
if errorlevel 1 (
  echo Failed to build test_runner.exe
  exit /b 1
)
echo Running tests...
test_runner.exe tinymath.exe
exit /b %ERRORLEVEL%