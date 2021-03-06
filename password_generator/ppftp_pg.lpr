// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
program ppftp_pg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils
  { you can add units after this },
  ftpfunctions ;

{$IFDEF WINDOWS}{$R ppftp_pg.rc}{$ENDIF}

const
    PASSWORD_LENGTH : Integer = 10 ;
    STRING_PASSWORD : String = 'azertyuiopqsdfghjklmwxcvbn0123456789AZERTYUIOPQSDFGHJKLMWXCVBN' ;
var
    // Password
    lsPassword : String ;
    // Index of loop to generate password
    liIndexPassword : Integer ;
    // Index of password string
    liIndexPasswordString : Integer ;
    // Index
    liIndex : Integer ;
begin
    write('Enter password (blank for automatic generation) : ') ;
    readln(lsPassword) ;

    if (lsPassword = '')
    then begin
        Randomize ;

        for liIndexPassword := 0 to PASSWORD_LENGTH do
        begin
            liIndexPasswordString := Random(PASSWORD_LENGTH) ;

            lsPassword := lsPassword + STRING_PASSWORD[liIndexPasswordString + 1] ;
        end ;
    end ;

    writeln(MD5(lsPassword)) ;
end.

