@echo off
REM Mock curl script for testing S3 operations on Windows
REM This provides the same mock behavior as the bash curl script

setlocal enabledelayedexpansion

REM Get the directory where this script is located
set SCRIPT_DIR=%~dp0
set RESPONSE_DIR=%SCRIPT_DIR%..\data\responses

REM Parse arguments
set OUTPUT_FILE=
set URL=
set IS_HEAD_REQUEST=0
set IS_PUT_REQUEST=0
set IS_DELETE_REQUEST=0
set DATA_FILE=

:parse_args
if "%~1"=="" goto args_done
if "%~1"=="-o" (
    set OUTPUT_FILE=%~2
    shift
    shift
    goto parse_args
)
if "%~1"=="-I" (
    set IS_HEAD_REQUEST=1
    shift
    goto parse_args
)
if "%~1"=="-X" (
    if "%~2"=="PUT" set IS_PUT_REQUEST=1
    if "%~2"=="DELETE" set IS_DELETE_REQUEST=1
    shift
    shift
    goto parse_args
)
if "%~1"=="--data-binary" (
    set DATA_FILE=%~2
    REM Remove @ prefix if present
    set DATA_FILE=!DATA_FILE:@=!
    shift
    shift
    goto parse_args
)
if "%~1"=="-s" (
    shift
    goto parse_args
)
if "%~1"=="-S" (
    shift
    goto parse_args
)
REM Check if it's a URL (starts with http)
echo %~1 | findstr /b /c:"http" >nul
if !errorlevel! equ 0 (
    set URL=%~1
    shift
    goto parse_args
)
shift
goto parse_args

:args_done

REM Extract key from URL
REM URL format: https://bucket.s3.amazonaws.com/key
for %%i in ("%URL:/=" "%") do set KEY=%%~nxi

REM Handle HEAD requests
if %IS_HEAD_REQUEST%==1 (
    if exist "%RESPONSE_DIR%\head_%KEY%" (
        type "%RESPONSE_DIR%\head_%KEY%"
        exit /b 0
    ) else (
        exit /b 22
    )
)

REM Handle PUT requests
if %IS_PUT_REQUEST%==1 (
    exit /b 0
)

REM Handle DELETE requests
if %IS_DELETE_REQUEST%==1 (
    if "%KEY%"=="delete_failure_test.txt" (
        exit /b 22
    )
    exit /b 0
)

REM Handle GET requests
if not "%OUTPUT_FILE%"=="" (
    REM Special test cases
    if "%KEY%"=="network_failure_test.txt" (
        exit /b 7
    )
    if "%KEY%"=="timeout_test.txt" (
        exit /b 28
    )
    if "%KEY%"=="malformed_response.txt" (
        copy /y "%RESPONSE_DIR%\malformed.xml" "%OUTPUT_FILE%" >nul
        exit /b 0
    )

    REM Check if response file exists
    if exist "%RESPONSE_DIR%\%KEY%" (
        copy /y "%RESPONSE_DIR%\%KEY%" "%OUTPUT_FILE%" >nul
        exit /b 0
    ) else (
        REM Return NoSuchKey error
        if exist "%RESPONSE_DIR%\nosuchkey.xml" (
            copy /y "%RESPONSE_DIR%\nosuchkey.xml" "%OUTPUT_FILE%" >nul
        ) else (
            REM Create fallback NoSuchKey response
            echo ^<?xml version="1.0" encoding="UTF-8"?^> > "%OUTPUT_FILE%"
            echo ^<Error^>^<Code^>NoSuchKey^</Code^>^<Message^>The specified key does not exist.^</Message^>^<Key^>%KEY%^</Key^>^</Error^> >> "%OUTPUT_FILE%"
        )
        exit /b 0
    )
)

REM Unsupported operation
echo Mock curl: Unsupported operation 1>&2
exit /b 1
