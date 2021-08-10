// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

// This is the main of FTP server
program ftpserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this },
  ftpmain,
  SysUtils,
  StrUtils,
  FileUtil,
  FtpFunctions,
  Math,
  IniFiles,
  ftpclient,
  ftptypes,
  ftpconst ;

{$IFDEF WINDOWS}{$R ftpserver.rc}{$ENDIF}

// Disabled Raise on IO
{$IOChecks off}

const
    VERSION : String = '0.1' ;
    
var
    // File to be write
    gfLogFile : TextFile ;
    // Maximum length of file
    giMaximumFileSize : Int64 ;
    // True if write to log file
    gbLogFile : Boolean ;
    // Number of file log
    giNumberLogFile : Integer ;
    // Log format
    gsLogFormat : String ;
    // Current log file name
    gsCurrentFileNameLog : String ;
    // Size of log
    giCurrentSizeOfLog : Int64 ;
    // Ini configuration for ftp server
    goMainConfig : TIniFile ;
    // Root config directory
    giRootConfigDirectory : String ;

    // Index of param count
    liIndexParamStr : Integer ;
    // Param
    lsParam : String ;
    // Length param
    liLengthParam : Integer ;
    // Multiple for size
    liMultiple : Integer ;
    // Size of file
    lsSizeOfFileInParam : String ;
    // Quit program
    lbQuit : Boolean ;
    // Size of file
    liSizeOfFile : Integer ;
    // Ftp main
    loFtpMain : TFtpMain ;

//
// Parse date pattern
//
// @param asPattern pattern of file name
function ParseDatePattern(const asPattern : String) : String ;
var
    // New pattern
    lsNewPattern : String ;
begin

    lsNewPattern := AnsiReplaceStr(asPattern, '%Y', FormatDateTime('yyyy', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%M', FormatDateTime('mm', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%d', FormatDateTime('dd', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%H', FormatDateTime('hh', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%m', FormatDateTime('nn', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%s', FormatDateTime('ss', Now())) ;

    Result := lsNewPattern ;
end ;

//
// Create file name
//
// @param asPattern pattern of file name
function CreateFileName(const asPattern : String) : String ;
begin
    Result := ParseDatePattern(asPattern) ;
end ;

//
// Error message in file
//
// @param asMessage message to log
procedure ErrorMsg(const asMessage : String) ;
begin
    WriteLn(ErrOutput, asMessage) ;
end ;

//
// Open file name
//
// @param asFileName file name to open
// @param afFile file variable
procedure OpenFileName(const asFileName : String; var afFile : TextFile) ;
var
    // Directory
    lsDirectoryName : String ;
    // Continue
    lbContinue : Boolean ;
begin
    lsDirectoryName := ExtractFilePath(asFileName) ;

    if (lsDirectoryName <> '') and (not DirectoryExists(lsDirectoryName))
    then begin
        lbContinue := ForceDirectories(lsDirectoryName) ;

        if not lbContinue
        then begin
            ErrorMsg('Can''t create directory of log file') ;
        end ;
    end
    else begin
        lbContinue := True ;
    end ;

    if lbContinue
    then begin
        FileMode := fmOpenWrite ;
        AssignFile(afFile, asFileName) ;
        ReWrite(afFile) ;

        giCurrentSizeOfLog := 0 ;
    end ;
end ;

//
// Log message in file
//
// @param asMessage message to log
procedure LogMsg(const asMessage : String) ;
var
    // File name
    lsFileName : String ;
begin
    if gbLogFile and (TTextRec(gfLogFile).Mode <> fmClosed)
    then begin
        WriteLn(gfLogFile,
            ParseDatePattern(gsLogFormat) +
            asMessage) ;

        Inc(giCurrentSizeOfLog, Length(asMessage)) ;

        // Check file size
        if (giMaximumFileSize > 0)
        then begin
            // File size is too big
            if giCurrentSizeOfLog > giMaximumFileSize
            then begin
                // Close file
                Close(gfLogFile) ;

                lsFileName := CreateFileName(gsCurrentFileNameLog) ;

                // if same filename
                if (CompareFilenames(gsCurrentFileNameLog, lsFileName) = 0)
                then begin
                    // Open new log file
                    lsFileName := gsCurrentFileNameLog + '-' + IntToStr(giNumberLogFile) ;
                end
                else begin
                    gsCurrentFileNameLog := lsFileName ;
                end ;

                // Open new log file
                OpenFileName(lsFileName, gfLogFile) ;

                // Inc next file count
                Inc(giNumberLogFile) ;
            end ;
        end ;
    end
    else begin
        WriteLn(ParseDatePattern(gsLogFormat) + asMessage) ;
    end ;
end ;

//
// Read main config
//
// @param asKey key to read
//
// @return value or ''
function ReadMainConfig(const asKey : String) : String ;
begin
    Result := goMainConfig.ReadString('main', asKey, '') ;
end ;

//
// Read user config
function ReadUserConfig(const asLoginName : String) : TUserConfig ;
var
    // Ini file
    loUserConfig : TIniFile ;
    // File to read
    lsFile : String ;
const
    USER_SECTION : String = 'user' ;
begin
    lsFile := ExpandFileName(giRootConfigDirectory + 'users' +
        DirectorySeparator + asLoginName + '.ini') ;

    Result.UserFound := FileExists(lsFile);

    loUserConfig := TIniFile.Create(lsFile) ;

    loUserConfig.CaseSensitive := False ;
    loUserConfig.StripQuotes := True ;

    Result.Password := loUserConfig.ReadString(USER_SECTION, USER_CONF_PASSWORD,
        DEFAULT_USER_PASSWORD) ;
    Result.Root := loUserConfig.ReadString(USER_SECTION, USER_CONF_ROOT,
        DEFAULT_USER_ROOT) ;
    Result.Download := loUserConfig.ReadString(USER_SECTION, USER_CONF_DOWNLOAD,
        DEFAULT_USER_DOWNLOAD) ;
    Result.Upload := loUserConfig.ReadString(USER_SECTION, USER_CONF_UPLOAD,
        DEFAULT_USER_UPLOAD);
    Result.Rename := loUserConfig.ReadString(USER_SECTION, USER_CONF_RENAME,
        DEFAULT_USER_RENAME) ;
    Result.Delete := loUserConfig.ReadString(USER_SECTION, USER_CONF_DELETE,
        DEFAULT_USER_DELETE) ;
    Result.MakeDirectory := loUserConfig.ReadString(USER_SECTION, USER_CONF_MAKE_DIRECTORY,
        DEFAULT_USER_MAKE_DIRECTORY) ;
    Result.DeleteDirectory := loUserConfig.ReadString(USER_SECTION, USER_CONF_DELETE_DIRECTORY,
        DEFAULT_USER_DELETE_DIRECTORY) ;
    Result.ListSubDir := loUserConfig.ReadString(USER_SECTION, USER_CONF_SUB_DIR,
        DEFAULT_USER_SUB_DIR) ;
    Result.Disabled := loUserConfig.ReadString(USER_SECTION, USER_CONF_DISABLED,
        DEFAULT_USER_DISABLED) ;

    loUserConfig.Free ;
end ;

//
// Main program
begin
    // Disable file size
    giMaximumFileSize := 0 ;
    // Root is current directory
    giRootConfigDirectory := '' ;
    // No log file
    gbLogFile := False ;
    // Number of file log
    giNumberLogFile := 1 ;
    // Multiple
    liMultiple := 0 ;
    // Don't quit program
    lbQuit := False ;
    // Defaut log format
    gsLogFormat := '%Y/%M/%d %H:%m:%s ' ;
    
    liIndexParamStr := 0 ;

    while (liIndexParamStr < ParamCount) and (lbQuit = False) do
    begin
        if (ParamStr(liIndexParamStr) = '-root')
        then begin
            Inc(liIndexParamStr) ;
            
            giRootConfigDirectory := AddTrailingEndSlash(
                ParamStr(liIndexParamStr)) ;
        end
        else if (ParamStr(liIndexParamStr) = '-logfile')
        then begin
            Inc(liIndexParamStr) ;

            gsCurrentFileNameLog := ParamStr(liIndexParamStr) ;

            gsCurrentFileNameLog := CreateFileName(gsCurrentFileNameLog) ;

            // Create file name
            OpenFileName(gsCurrentFileNameLog, gfLogFile) ;

            // If file not open, error
            if (TTextRec(gfLogFile).Mode = fmClosed)
            then begin
                ErrorMsg('Can''t open file log.') ;

                lbQuit := True ;
            end
            else begin
                gbLogFile := True ;
            end ;
        end
        else if (ParamStr(liIndexParamStr) = '-logsize')
        then begin
            Inc(liIndexParamStr) ;
            
            lsParam := ParamStr(liIndexParamStr) ;
            
            liLengthParam := Length(lsParam) ;
            
            if liLengthParam > 0
            then begin
                Inc(giNumberLogFile) ;
                
                if lsParam[liLengthParam] = 'k'
                then begin
                    liMultiple := 1 ;
                end
                else if lsParam[liLengthParam] = 'm'
                then begin
                    liMultiple := 2 ;
                end
                else if lsParam[liLengthParam] = 'g'
                then begin
                    liMultiple := 3 ;
                end
                else if lsParam[liLengthParam] = 't'
                then begin
                    liMultiple := 4 ;
                end
                else if lsParam[liLengthParam] in ['0'..'9']
                then begin
                    // +1 cause we cut after
                    Inc(liLengthParam) ;
                end ;

                // Copy integer value
                lsSizeOfFileInParam := Copy(lsParam, 1, liLengthParam - 1) ;

                liSizeOfFile := 0 ;

                if TryIntParse(lsSizeOfFileInParam, liSizeOfFile)
                then begin

                    giMaximumFileSize := 1 ;

                    while (liMultiple > 0) do
                    begin
                        giMaximumFileSize := giMaximumFileSize * 1024 ;

                        Dec(liMultiple) ;
                    end ;

                    giMaximumFileSize := liSizeOfFile * giMaximumFileSize ;
                end ;
            end ;
        end
        else if (ParamStr(liIndexParamStr) = '--version') or
            (ParamStr(liIndexParamStr) = '-v')
        then begin
            WriteLn(VERSION) ;
            
            lbQuit := True ;
        end
        else if ParamStr(liIndexParamStr) = '-logformat'
        then begin
            gsLogFormat := ParamStr(liIndexParamStr) ;
        end        
        else if (ParamStr(liIndexParamStr) = '--help') or
            (ParamStr(liIndexParamStr) = '-h')
        then begin
            WriteLn('Portable and Pascal FTP Server v' + VERSION) ;
            WriteLn('') ;
            WriteLn('  -root           : root directory who contain configuration') ;
            WriteLn('  -logfile        : log file name') ;
            WriteLn('                    %Y : year 4 digit') ;
            WriteLn('                    %M : month 2 digit') ;
            WriteLn('                    %d : day 2 digit') ;
            WriteLn('                    %H : hours 2 digit') ;
            WriteLn('                    %m : minutes 2 digit') ;
            WriteLn('                    %s : seconds 2 digit') ;
            WriteLn('   -logformat     : log format. Default "' + gsLogFormat + '". See -logfile') ;
            WriteLn('  -logsize        : size of log in byte or') ;
            WriteLn('                    k : kilo, g : giga, t : tetra') ;
            WriteLn('  -h or --help    : display this help') ;
            WriteLn('  -v or --version : display version number') ;
            
            lbQuit := True ;
        end ;
        
        Inc(liIndexParamStr) ;
    end ;
    
    if lbQuit = False
    then begin
        WriteLn('Portable and Pascal FTP Server v' + VERSION) ;
        WriteLn('CopyLeft (C) MARTINEAU Emeric (bubulemaster@yahoo.fr)') ;
        WriteLn('Web site : http://www.bubulemaster.fr') ;
        WriteLn('License : GNU LGPL v3') ;
        WriteLn('Powered by FreePascal/Lazarus and Synapse librairie') ;
        WriteLn('') ;

        // Create ini file reader
        goMainConfig := TIniFile.Create(giRootConfigDirectory + 'ppftpconf.ini') ;
        goMainConfig.CaseSensitive := False ;
        goMainConfig.StripQuotes := True ;

        loFtpMain := TFtpMain.Create(true) ;

        loFtpMain.OnMainConfigRead := @ReadMainConfig ;
        loFtpMain.OnLog := @LogMsg ;
        loFtpMain.OnError := @ErrorMsg ;
        loFtpMain.OnClientConfigRead := @ReadUserConfig ;

        loFtpMain.FreeOnTerminate := True ;
        // Set False if console application, else true
        loFtpMain.GuiApplication := False ;

        loFtpMain.Resume ;

        RtlEventWaitFor(loFtpMain.WaitEvent) ;

        goMainConfig.Free ;
    end ;
    
    // Close log file
    if gbLogFile and (TTextRec(gfLogFile).Mode <> fmClosed)
    then begin
        Close(gfLogFile) ;
    end ;
end.
