// This file contain all type
unit ftpunixdirectory;

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
    FTP_SERVER_TYPE : String = 'UNIX Type: L8' ;

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

const
    LIST_OF_MONTH_FILE : array[1..12] of AnsiString = ('Jan', 'Feb', 'Mar',
        'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec') ;

//
// Get right
//
// @param structure of file file
//
// @return string of right (e.g. 'r-x')
function GetRight(const arSearchRec: TSearchRec) : String ;
begin
    if (arSearchRec.Attr and faReadOnly) > 0
    then begin
        Result := 'r-x' ;
    end
    else begin
        Result := 'rwx' ;
    end ;
end ;

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
    // Year
    lwYear : Word ;
    // Month
    lwMonth : Word ;
    // Day
    lwDay : Word ;
    // Current year
    lwCurrentYear : Word ;
    // Hour
    lwHour : Word ;
    // Minutes
    lwMinute : Word ;
    // Seconde
    lwSecond : Word ;
    // Millisecond
    lwMilliSecond : Word ;
begin
    DecodeDate(Now(), lwCurrentYear, lwMonth, lwDay) ;

    lrDateTime := FileDateToDateTime(arSearchRec.Time) ;

    DecodeDate(lrDateTime, lwYear, lwMonth, lwDay) ;

    // If same date
    if lwYear = lwCurrentYear
    then begin
        DecodeTime(lrDateTime, lwHour, lwMinute, lwSecond, lwMilliSecond) ;

        Result := Format('%s %2.1d %.2d:%.2d', [LIST_OF_MONTH_FILE[lwMonth],
            lwDay, lwHour, lwMinute]) ;
    end
    else begin
        Result := Format('%s %2.1d  %d', [LIST_OF_MONTH_FILE[lwMonth], lwDay,
          lwYear]) ;
    end ;
end ;

//
// Create a line for a directory
function GetRightAndDateAndSize(const arSearchRec: TSearchRec) : String ;
var
    lsRight : String ;
begin
    // Build right string
    lsRight := GetRight(arSearchRec) ;

    // Right and owner and group
    Result := lsRight + lsRight + lsRight + '   1 ftp ftp ' +
        Format('%9.1d', [arSearchRec.Size]) ;

    //drwxr-xr-x   2 web site      640 May 12  2004 admin
    Result := Result + ' ' + GetDate(arSearchRec) + ' ' + arSearchRec.Name ;

//    writeln('-rw-r--r--   1 uwmep    rsirgp     37440 May 16  2007 log4j.xml.ori') ;
//    writeln('-rw-r--r--   1 uwmep    rsirgp   1778840 Jan 25 08:15 OffresPrepayees.ear') ;
//    writeln(Result) ;
end ;

//
// Create a line for a directory
function GetDirectory(const arSearchRec: TSearchRec) : String ;
begin
    // Directory
    Result := 'd' + GetRightAndDateAndSize(arSearchRec) ;

//    writeln('-rw-r--r--   1 web site     4615 Aug  1  2004 utilisation.php') ;
//    writeln('-rw-r--r--   1 uwmep    rsirgp     37440 May 16  2007 log4j.xml.ori') ;
//    writeln('-rw-r--r--   1 uwmep    rsirgp   1778840 Jan 25 08:15 OffresPrepayees.ear') ;
//    writeln(Result) ;
end ;

//
// Create a line for a directory
function GetFile(const arSearchRec: TSearchRec) : String ;
begin
    // File
    Result := '-' + GetRightAndDateAndSize(arSearchRec) ;
end ;

end.

