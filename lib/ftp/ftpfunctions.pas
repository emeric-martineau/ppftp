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
  Classes, SysUtils, FtpConst, blcksock;

//
// Try parse a integer value
//
// @param asIntegerString String to be converted
// @param aiInteger result
//
// @return true if ok, false else
function TryIntParse(const asIntegerString : String; var aiInteger : Integer) : Boolean ;

//
// Add \ or / at end if no yet
//
// @param asString string
function AddTrailingEndSlash(const asString : String) : String ;

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
    const asSeparator : String; const aoList : TStringList) ;

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

implementation

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
// Add \ or / at end if no yet
function AddTrailingEndSlash(const asString : String) : String ;
var
   liLength : Integer ;
begin
    liLength := Length(asString) ;

    if liLength > 0
    then begin
        if (asString[liLength] <> DirectorySeparator)
        then begin
            Result := asString + DirectorySeparator ;
        end
        else begin
            Result := asString ;
        end ;
    end
    else begin
        Result := DirectorySeparator
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
    const asSeparator : String; const aoList : TStringList) ;
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
end ;

end.

