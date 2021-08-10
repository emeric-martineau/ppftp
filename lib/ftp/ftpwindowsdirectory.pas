// This file contain all type
unit ftpwindowsdirectory;

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

uses SysUtils ;

const
    // Type ftp server
    FTP_SERVER_TYPE : String = 'Windows_NT' ;

//
// Create a line for a directory
//
// @param arSearchRec directory
function GetDirectory(const arSearchRec: TSearchRec) : String ;

//
// Create a line for a file
//
// @param arSearchRec file
function GetFile(const arSearchRec: TSearchRec) : String ;

implementation

//
// Get date of file
//
// @param structure of file file
//
// @return string of date (e.g. 'May 12  2004')
function GetDate(const arSearchRec: TSearchRec) : String ;
var
    // Date time of file
    lrDateTime : TDateTime ;
    // Hour
    lwHour : Word ;
    // Minutes
    lwMinute : Word ;
    // Seconde
    lwSecond : Word ;
    // Millisecond
    lwMilliSecond : Word ;
    // AM
    lsAMPM : String ;
begin
    lrDateTime := FileDateToDateTime(arSearchRec.Time) ;

    DecodeTime(lrDateTime, lwHour, lwMinute, lwSecond, lwMilliSecond) ;

    Result := FormatDateTime('mm-dd-yy', lrDateTime) ;

    if lwHour < 12
    then begin
        lsAMPM := 'AM' ;
    end
    else begin
        Dec(lwHour, 12) ;

        lsAMPM := 'PM' ;
    end ;

    Result := Result + '  ' + Format('%.2d%s%.2d%s', [lwHour, ':', lwMinute,
        lsAMPM]) ;
end ;

//
// Create a line for a directory
function GetDirectory(const arSearchRec: TSearchRec) : String ;
begin
    //02-01-06  03:01PM       <DIR>          backoffice
    Result := GetDate(arSearchRec) + '       <DIR>          ' + arSearchRec.Name ;

    //writeln('02-01-06  03:01PM       <DIR>          backoffice') ;
    //writeln(Result) ;
end ;

//
// Create a line for a directory
function GetFile(const arSearchRec: TSearchRec) : String ;

begin
    //05-20-96  06:47PM                 1715 readme.txt
    Result := GetDate(arSearchRec) + ' ' + Format('%20.1d', [arSearchRec.Size])
        + ' ' + arSearchRec.Name ;

    //Writeln('05-20-96  06:47PM                 1715 readme.txt ') ;
    //writeln(Result) ;
end ;

end.

