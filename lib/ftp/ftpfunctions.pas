// This unit contain commun functions of FTP
unit ftpfunctions;

// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, LCLProc, FtpConst, blcksock, synacode;

//
// Try parse a integer value
//
// @param asIntegerString String to be converted
// @param aiInteger result
//
// @return true if ok, false else
function TryIntParse(const asIntegerString : String; var aiInteger : Integer) : Boolean ;

//
// Add a char at end if no yet
//
// @param asString string
// @param char to add
function AddTrailing(const asString : String; const asChar : String;
    const abUtf8 : Boolean) : String ;


//
// Convert string with \n into xxx
//
// @param asString string to be converted
// @param asSeparator \n or something else
// @param asHeader start line
// @param asEndOfLine end of line
function ConvertMessage(const asString : String; const asSeparator : String;
    const asHeader : String; const asEndOfLine : String) : String ;

//
// Split String to TStringList
//
// @param asString string to split
// @param asSeparator separator to split
function StringToTStringList(const asString : String;
    const asSeparator : String) : TStringList ;

//
// Split String to TStringList
//
// @param asString string to split
// @param asSeparator separator to split
// @param aoList list to store result
procedure StringToTStringListNoCreate(const asString : String;
    const asSeparator : String; const aoList : TStrings) ;

//
// Split string with '-' separator and convert both part in integer
//
// @param asPassivePortString string to be split
// @param aiStartPort result of start port
// @param aiEndPort result of end port
//
// @return True if not error
function ConvertPassivePort(const asPassivePortString : String; var aiStartPort : Integer;
    var aiEndPort : Integer) : Boolean ;

//
// Check an ip address in list of ip address
//
// @param asIpAddress ip address to check
// @param aoList list of ip address
function CheckIpAddressInList(const asIpAddress : String; const aoList : TStringList) : Boolean ;

//
// Check if two list are equals with jocker support
//
// @param aoList1 first list
// @param aoList2 second list
function CheckTStringsWithJocker(const aoList1 : TStringList; const aoList2 : TStringList) : Boolean ;

//
// Send a string and add #10#13 at end
//
// @param aoClientSock socket to send
// @param asString string to send
procedure SendString(const aoClientSock : TTCPBlockSocket; const asString : String) ;

//
// Escape a char
//
// @param asString string to read
// @param asEscapeChar escape char
// @param asCharToEscape char to escape
function EscapeChar(const asString : String; const asEscapeChar : String;
    const asCharToEscape : String) : String ;

// Convert a FTP path to OS path
//
// @param asRootPath sys user root. Must and by DirectorySeparator and in UTF8
// @param asFtpCurrentPath current ftp path
// @param asFtpPath ftp path
// @param abUtf8 true if use Utf8 file name
function ConvertFtpPathToSysPath(const asRootPath : String;
    const asFtpCurrentPath : String; const asFtpPath : String;
    const abUtf8 : Boolean) : String ;

//
// Recherche un chaine dans une autre
//
// @param asNeedle seach string
// @param asStringToCheck string to check
// @param abUtf8 true if utf8
// @param asCaseSensitive if seach in case sensitive
function StrPos(const asNeedle : String; const asStringToCheck : String;
    const abUtf8 : Boolean; const abCaseSensitive : Boolean) : Integer ;

//
// Check if directory exists
//
// @param asDirectoryName directory to find
// @param abUtf8 True if check in utf8
function CheckDirectoryExists(const asDirectoryName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Get absolute path for ftp path
//
// @asFtpPath FTP path (/truc/gmachi/../)
function GetAbsoluteFtpPath(const asFtpPath : String) : String ;

//
// Check if root in path
//
// @param asRoot root of path. Must be UTF8
// @param asPath path to check
function IsRootInPath(asRoot : String;
    asPath : String; const abUtf8 : Boolean) : Boolean ;

//
// Get humain readable md5
//
// @param asString string to parse
function MD5(const asString : String) : String ;

//
// Translate to passive port
//
// @param asIpAddress ip address '127.0.0.1'
// @param aiPort port number
function TranslateToPassivePort(const asIpAddress : String;
    const aiPort : Integer) : String ;

//
// Check if file exists
//
// @param asFileName directory to find
// @param abUtf8 True if check in utf8
function CheckFileExists(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Return string of IOResult
// http://www.freepascal.org/docs-html/rtl/system/ioresult.html
//
// @param aiIoResult IOResult
//
// @return string
function GetIoResultString(const aiIoResult : word) : String ;

//
// Open text file in read
//
// @param asFileName file name
// @param asFile var file
procedure OpenTextFileForRead(const asFileName : String; var asFile : TextFile);

//
// Open text file in write
//
// @param asFileName file name
// @param asFile var file
procedure OpenTextFileForWrite(const asFileName : String; var asFile : TextFile);

//
// Return is file read only
//
// @param asFileName file name
// @param abUtf8 if utf8 mode is used
//
// Return true if readonly
function IsFileReadOnly(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Open text file in write
//
// @param asFileName file name
// @param asFile var file
procedure OpenTextFileForAppend(const asFileName : String; var asFile : TextFile);

//
// Rename file/directory
//
// @param asOldName old name of file
// @param asNewName new name of file
// @param abUtf8 true if utf8 mode enable
//
// @return true if ok
function InternalRenameFile(const asOldName : String; const asNewName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Delete file
//
// @param asFileName file name of file
// @param abUtf8 true if utf8 mode enable
//
// @return true if ok
function InternalDeleteFile(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Make directory
//
// @param asFileName file name of file
// @param abUtf8 true if utf8 mode enable
//
// @return true if ok
function InternalMakeDirectory(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Remove directory
//
// @param asFileName file name of file
// @param abUtf8 true if utf8 mode enable
//
// @return true if ok
function InternalRemoveDirectory(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;

//
// Copy a string like Copy() function with utf8 option
function InternalCopyStr(const asSource : string; const aiStartChar : Integer;
    const aiCount  : Integer; const abUtf8 : Boolean) : String ;

//
// Length with utf8 option
function InternalLengthStr(const asString : String;
    const abUtf8 : Boolean) : Integer ;

implementation

var
    // Lock for file mode
    poLockFileMode : TRTLCriticalSection ;

//
// Try parse a integer value
function TryIntParse(const asIntegerString : String; var aiInteger : Integer) : Boolean ;
begin
    Result := False ;

    Try
        aiInteger := StrToInt(asIntegerString) ;

        Result := True ;
    except
        on E : EConvertError do
            Result := False ;
    end;
end ;

//
// Add a char at end if no yet
function AddTrailing(const asString : String; const asChar : String;
    const abUtf8 : Boolean) : String ;
var
   liLength : Integer ;
begin
    liLength := InternalLengthStr(asString, abUtf8) ;

    if liLength > 0
    then begin
        if (InternalCopyStr(asString, liLength, 1, abUtf8) <> asChar)//  (asString[liLength] <> asChar)
        then begin
            Result := asString + asChar ;
        end
        else begin
            Result := asString ;
        end ;
    end
    else begin
        Result := asChar
    end ;
end ;

//
// Split String to TStringList
function StringToTStringList(const asString : String;
    const asSeparator : String) : TStringList ;
begin
    Result := TStringList.Create() ;

    StringToTStringListNoCreate(asString, asSeparator, Result) ;
end ;

//
// Split String to TStringList
procedure StringToTStringListNoCreate(const asString : String;
    const asSeparator : String; const aoList : TStrings) ;
var
    // Length of separator
    liLengthSeparator : Integer ;
    // Current line
    lsCurrentLine : String ;
    // Index of string
    liIndexString : Integer ;
    // Length of string
    liLengthString : Integer ;
begin
    liLengthSeparator := Length(asSeparator) ;

    lsCurrentLine := '' ;

    liIndexString := 1 ;

    liLengthString := Length(asString) ;

    while liIndexString <= liLengthString do
    begin
        // Check if separator
        if Copy(asString, liIndexString, liLengthSeparator) = asSeparator
        then begin
            aoList.Add(lsCurrentLine) ;

            lsCurrentLine := '' ;

            Inc(liIndexString, liLengthSeparator) ;
        end
        else begin
            lsCurrentLine := lsCurrentLine + asString[liIndexString] ;

            Inc(liIndexString) ;
        end ;
    end ;

    // If we are enter in loop and lsCurrentLine not empty
    if lsCurrentLine <> ''
    then begin
        aoList.Add(lsCurrentLine) ;
    end ;
end ;

//
// Convert string with \n into xxx
function ConvertMessage(const asString : String; const asSeparator : String;
    const asHeader : String; const asEndOfLine : String) : String ;
var
    // List
    loList : TStringList ;
    // Index of list
    liIndex : Integer ;
begin
    loList := StringToTStringList(asString, asSeparator) ;

    Result := '' ;

    for liIndex := 0 to loList.Count - 2 do
    begin
        Result := Result + asHeader + '-' + loList[liIndex] + asEndOfLine ;
    end ;

    Result := Result + asHeader + ' ' + loList[loList.Count - 1] + asEndOfLine ;

    loList.Free ;
end ;

//
// Split string with '-' separator and convert both part in integer
function ConvertPassivePort(const asPassivePortString : String; var aiStartPort : Integer;
    var aiEndPort : Integer) : Boolean ;
var
    // Length of input string
    liLengthOfString : Integer ;
    // Index of string
    liIndex : Integer ;
    // current port
    lsCurrentPort : String ;
    // If separator found
    lbSeparatorFound : Boolean ;
    // Port
    liPort : Integer ;
begin
    liLengthOfString := Length(asPassivePortString) ;

    liIndex := 1 ;

    lsCurrentPort := '' ;

    lbSeparatorFound := False ;

    // 1 - Copy upto '-'
    while (liIndex <= liLengthOfString) and (lbSeparatorFound = False) do
    begin
        if (asPassivePortString[liIndex] = '-')
        then begin
             lbSeparatorFound := True ;
        end
        else begin
            lsCurrentPort := lsCurrentPort + asPassivePortString[liIndex] ;
        end ;

        Inc(liIndex) ;
    end ;

    liPort := 21 ;

    Result := lbSeparatorFound and TryIntParse(lsCurrentPort, liPort) ;

    // 2 - Copy end of string
    if Result
    then begin
        aiStartPort := liPort ;

        lsCurrentPort := Copy(asPassivePortString, liIndex, liLengthOfString) ;

        Result := TryIntParse(lsCurrentPort, liPort) ;

        aiEndPort := liPort ;
    end ;
end ;

//
// Check an ip address in list of ip address
function CheckIpAddressInList(const asIpAddress : String; const aoList : TStringList) : Boolean ;
var
    // Ip address to check in TStringList
    loIpAddress : TStringList ;
    // Counter of list
    liIndexList : Integer ;
    // Current list
    loCurrentList : TStringList ;
begin
    // Splite 127.0.0.1 in ['127', '0', '0', '1']
    loIpAddress := StringToTStringList(asIpAddress, '.') ;

    Result := False ;

    if loIpAddress.Count > 0
    then begin
        loCurrentList := TStringList.Create ;

        for liIndexList := 0 to aoList.Count - 1 do
        begin
            // Split current address in TStringList
            StringToTStringListNoCreate(aoList[liIndexList], '.', loCurrentList) ;

            // To compare, same count are requiere
            if loIpAddress.Count = loCurrentList.Count
            then begin
                Result := CheckTStringsWithJocker(loIpAddress, loCurrentList) ;
            end ;
        end ;
    end ;
end ;

//
// Check if two list are equals with jocker support
function CheckTStringsWithJocker(const aoList1 : TStringList; const aoList2 : TStringList) : Boolean ;
var
    // Index of number of address
    liIndexNumber : Integer ;
    // Index of char
    liIndexOfChar : Integer ;
    // End of comparaison
    liEnd : Integer ;
    // String to compare
    lsString1 : String ;
    lsString2 : String ;
begin
    for liIndexNumber := 0 to aoList1.Count - 1 do
    begin
        lsString1 := aoList1[liIndexNumber] ;
        lsString2 := aoList2[liIndexNumber] ;

        // Get lower string
        if Length(lsString1) < Length(lsString2)
        then begin
            liEnd := Length(lsString1) ;
        end
        else begin
            liEnd := Length(lsString2) ;
        end ;

        Result := True ;

        // For each char in string
        for liIndexOfChar := 1 to liEnd do
        begin
            if (lsString1[liIndexOfChar] = '*') or (lsString2[liIndexOfChar] = '*')
            then begin
                break ;
            end
            else if (lsString1[liIndexOfChar] = '?') or (lsString2[liIndexOfChar] = '?')
            then begin
            end
            else if lsString1[liIndexOfChar] <> lsString2[liIndexOfChar]
            then begin
                Result := False ;

                break ;
            end ;
        end ;
    end ;
end ;

// Send a string and add #10#13 at end
procedure SendString(const aoClientSock : TTCPBlockSocket; const asString : String) ;
begin
    aoClientSock.SendString(asString + FTP_EOL) ;
    {
    if aoClientSock.LastError <> 0
    then begin
        writeln(aoClientSock.LastErrorDesc) ;
    end ;
    }
end ;

//
// Escape a char
function EscapeChar(const asString : String; const asEscapeChar : String;
    const asCharToEscape : String) : String ;
var
    // Index of string
    liIndex : Integer ;
begin
    Result := '' ;

    for liIndex := 1 to Length(asString) do
    begin
        if asString[liIndex] = asCharToEscape
        then begin
            Result := Result + asEscapeChar ;
        end ;

        Result := Result + asString[liIndex] ;
    end ;
end ;

//
// Convert a FTP path to OS path
function ConvertFtpPathToSysPath(const asRootPath : String;
    const asFtpCurrentPath : String; const asFtpPath : String;
    const abUtf8 : Boolean) : String ;
var
    // New path
    lsPath : String ;
    // Ftp path
    lsFtpPath : String ;
    // Length of lsPath
    liLengthPath : Integer ;
    // Ftp path
    lsFtpCurrentPath : String ;
    // Lenght of current path
    liLength : Integer ;
begin
    liLength := InternalLengthStr(asFtpCurrentPath, abUtf8) ;

    // Make sure end '/' exists
    if (liLength > 0) and
        (asFtpCurrentPath[liLength] <> '/')
    then begin
        lsFtpCurrentPath := AddTrailing(asFtpCurrentPath, '/', abUtf8) ;
    end
    else begin
        lsFtpCurrentPath := asFtpCurrentPath ;
    end ;

    lsPath := asFtpPath ;

    liLengthPath := InternalLengthStr(lsPath, abUtf8) ;

    // If lsPath start by /, don't use current directory
    if (liLengthPath > 0) and (lsPath[1] = '/')
    then begin
        lsFtpPath := lsPath ;
    end
    else begin
        lsFtpPath := lsFtpCurrentPath + lsPath ;
    end ;

    liLengthPath := InternalLengthStr(lsFtpPath, abUtf8) ;

    // If lsPath convert in lsPath start by '/', we
    // delete it.
    if (liLengthPath > 0) and (InternalCopyStr(lsFtpPath, 1, 1, abUtf8) = '/')
    then begin
        lsFtpPath := InternalCopyStr(lsFtpPath, 2, liLengthPath, abUtf8) ;
    end ;

    // Convert all '/' to '\'
    {$IFDEF WINDOWS}
    lsPath := AnsiReplaceStr(lsFtpPath, '/', DirectorySeparator) ;
    {$ELSE}
    lsPath := lsFtpPath ;
    {$ENDIF}

    if abUtf8
    then begin
        lsPath := AddTrailing(asRootPath, DirectorySeparator, false) + lsPath ;
    end
    else begin
        lsPath := UTF8ToSys(AddTrailing(asRootPath, DirectorySeparator, false)) + lsPath ;
    end ;

    if abUtf8
    then begin
        Result := ExpandFileNameUTF8(lsPath) ;
    end
    else begin
        Result := ExpandFileName(lsPath) ;
    end ;
end;

//
// Recherche un chaine dans une autre
function StrPos(const asNeedle : String; const asStringToCheck : String;
    const abUtf8 : Boolean; const abCaseSensitive : Boolean) : Integer ;
begin
    if abCaseSensitive
    then begin
        if abUtf8
        then begin
            Result := UTF8Pos(asNeedle, asStringToCheck) ;
        end
        else begin
            Result := Pos(asNeedle, asStringToCheck) ;
        end;
    end
    else begin
        if abUtf8
        then begin
            Result := UTF8Pos(UTF8LowerCase(asNeedle),
                UTF8LowerCase(asStringToCheck)) ;
        end
        else begin
            Result := Pos(LowerCase(asNeedle), LowerCase(asStringToCheck)) ;
        end;
    end ;
end ;

//
// Check if directory exists
function CheckDirectoryExists(const asDirectoryName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    if abUtf8
    then begin
        Result := DirectoryExistsUTF8(asDirectoryName) ;
    end
    else begin
        Result := DirectoryExists(asDirectoryName) ;
    end;
end;

//
// Get absolute path for ftp path
function GetAbsoluteFtpPath(const asFtpPath : String) : String ;
var
    // String list
    loStringList : TStringList ;
    // Index of list
    liIndexList : Integer ;
begin
    loStringList := StringToTStringList(asFtpPath, '/') ;

    liIndexList := 0 ;

    while liIndexList < loStringList.Count do
    begin
        if loStringList[liIndexList] = '.'
        then begin
            loStringList.Delete(liIndexList) ;
        end
        else if loStringList[liIndexList] = '..'
        then begin
            loStringList.Delete(liIndexList) ;

            if liIndexList > 0
            then begin
                loStringList.Delete(liIndexList - 1) ;

                Dec(liIndexList) ;
            end ;
        end
        else begin
            Inc(liIndexList) ;
        end ;
    end ;

    Result := '/' ;

    for liIndexList := 0 to loStringList.Count - 1 do
    begin
        // If path start by /, we have blank line
        if loStringList[liIndexList] <> ''
        then begin
            Result := Result + loStringList[liIndexList] + '/' ;
        end ;
    end ;
end ;

//
// Check if root in path
function IsRootInPath(asRoot : String; asPath : String;
    const abUtf8 : Boolean) : Boolean ;
var
    // pos string
    liPosOfRootPath : Integer ;
    // Case sensitive
    lbCaseSensitive : Boolean ;
begin
    asRoot := AddTrailing(asRoot, DirectorySeparator, true) ;
    asPath := AddTrailing(asPath, DirectorySeparator, abUtf8) ;

    if not abUtf8
    then begin
        asRoot := UTF8ToSys(asRoot) ;
    end ;

    // FileNameCaseSensitive : http://bugs.freepascal.org/view.php?id=9455
    {$IFDEF WINDOWS}
    lbCaseSensitive := False ;
    {$ELSE}
    lbCaseSensitive := True ;
    {$ENDIF}

    // Check if start by root
    liPosOfRootPath := StrPos(asRoot, asPath, abUtf8,
        lbCaseSensitive) ;

    Result := liPosOfRootPath <> 0 ;
end;

//
// Get humain readable md5
//
// @param asString string to parse
function MD5(const asString : String) : String ;
var
    //
    lsPassword : String ;
    // Counter
    liIndex : Integer ;
begin
    lsPassword := synacode.MD5(asString) ;

    Result := '' ;

    for liIndex := 1 to Length(lsPassword) do
    begin
        Result := Result + Format('%.2x', [byte(lsPassword[liIndex])]) ;
    end ;

    Result := LowerCase(Result) ;
end ;

//
// Translate to passive port
function TranslateToPassivePort(const asIpAddress : String;
    const aiPort : Integer) : String ;
begin
    Result := AnsiReplaceStr(asIpAddress, '.', ',') ;

    Result := Result + ',' + IntToStr(aiPort div 256) + ',' +
        IntToStr(aiPort and $FF) ;
end ;

//
// Check if file exists
function CheckFileExists(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    if abUtf8
    then begin
        Result := FileExistsUTF8(asFileName) ;
    end
    else begin
        Result := FileExists(asFileName) ;
    end ;
end ;

//
// Return string of IOResult
function GetIoResultString(const aiIoResult : word) : String ;
begin
    case aiIoResult of
        2 : Result := 'File not found.' ;
        3 : Result := 'Path not found.' ;
        4 : Result := 'Too many open files.' ;
        5 : Result := 'Access denied.' ;
        6 : Result := 'Invalid file handle.' ;
       12 : Result := 'Invalid file-access mode.' ;
       15 : Result := 'Invalid disk number.' ;
       16 : Result := 'Cannot remove current directory.' ;
       17 : Result := 'Cannot rename across volumes.' ;
      100 : Result := 'Error when reading from disk.' ;
      101 : Result := 'Error when writing to disk.' ;
      102 : Result := 'File not assigned.' ;
      103 : Result := 'File not open.' ;
      104 : Result := 'File not opened for input.' ;
      105 : Result := 'File not opened for output.' ;
      106 : Result := 'Invalid number.' ;
      150 : Result := 'Disk is write protected.' ;
      151 : Result := 'Unknown device.' ;
      152 : Result := 'Drive not ready.' ;
      153 : Result := 'Unknown command.' ;
      154 : Result := 'CRC check failed.' ;
      155 : Result := 'Invalid drive specified.' ;
      156 : Result := 'Seek error on disk.' ;
      157 : Result := 'Invalid media type.' ;
      158 : Result := 'Sector not found.' ;
      159 : Result := 'Printer out of paper.' ;
      160 : Result := 'Error when writing to device.' ;
      161 : Result := 'Error when reading from device.' ;
      162 : Result := 'Hardware failure.' ;
      else
          Result := 'Unknow error.' ;
    end;
end;

//
// Open text file in read
procedure OpenTextFileForRead(const asFileName : String; var asFile : TextFile);
begin
    EnterCriticalsection(poLockFileMode) ;

    Filemode := fmOpenRead ;

    AssignFile(asFile, asFileName) ;
    Reset(asFile) ;

    LeaveCriticalsection(poLockFileMode) ;
end ;

//
// Open text file in write
procedure OpenTextFileForWrite(const asFileName : String; var asFile : TextFile);
begin
    EnterCriticalsection(poLockFileMode) ;

    Filemode := fmOpenWrite ;

    AssignFile(asFile, asFileName) ;
    Rewrite(asFile) ;

    LeaveCriticalsection(poLockFileMode) ;
end ;

//
// Return file attributs
function IsFileReadOnly(const asFileName : String;
    const abUtf8 : Boolean) : boolean ;
begin
    if abUtf8
    then begin
        Result := FileIsReadOnlyUTF8(asFileName) ;
    end
    else begin
        Result := FileIsReadOnly(asFileName) ;
    end ;
end ;

//
// Open text file in write
procedure OpenTextFileForAppend(const asFileName : String; var asFile : TextFile);
begin
    EnterCriticalsection(poLockFileMode) ;

    Filemode := fmOpenWrite ;

    AssignFile(asFile, asFileName) ;
    Append(asFile) ;

    LeaveCriticalsection(poLockFileMode) ;
end ;

//
// Rename file/directory
function InternalRenameFile(const asOldName : String; const asNewName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    {$IOChecks off}
    if abUtf8
    then begin
        Result := RenameFileUTF8(asOldName, asNewName) ;
    end
    else begin
        Result := RenameFile(asOldName, asNewName) ;
    end ;
    {$IOChecks on}
end;

//
// Delete file
function InternalDeleteFile(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    {$IOChecks off}
    if abUtf8
    then begin
        Result := DeleteFileUTF8(asFileName) ;
    end
    else begin
        Result := DeleteFile(asFileName) ;
    end ;
    {$IOChecks on}
end;

//
// Make directory
function InternalMakeDirectory(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    {$IOChecks off}
    if abUtf8
    then begin
        Result := CreateDirUTF8(asFileName) ;
    end
    else begin
        Result := CreateDir(asFileName) ;
    end ;
    {$IOChecks on}
end;

//
// Remove directory
function InternalRemoveDirectory(const asFileName : String;
    const abUtf8 : Boolean) : Boolean ;
begin
    {$IOChecks off}
    if abUtf8
    then begin
        Result := RemoveDirUTF8(asFileName) ;
    end
    else begin
        Result := RemoveDir(asFileName) ;
    end ;
    {$IOChecks on}
end;

//
// Copy a string like Copy() function with utf8 option
function InternalCopyStr(const asSource : string; const aiStartChar : Integer;
    const aiCount  : Integer; const abUtf8 : Boolean) : String ;
begin
    if abUtf8
    then begin
        Result := UTF8Copy(asSource, aiStartChar, aiCount) ;
    end
    else begin
        Result := Copy(asSource, aiStartChar, aiCount) ;
    end ;
end ;

//
// Length with utf8 option
function InternalLengthStr(const asString : String;
    const abUtf8 : Boolean) : Integer ;
begin
    if abUtf8
    then begin
        Result := UTF8Length(asString) ;
    end
    else begin
        Result := Length(asString) ;
    end ;
end;

initialization
    InitCriticalSection(poLockFileMode) ;

finalization
    DoneCriticalsection(poLockFileMode) ;

end.

