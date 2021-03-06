// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
var
    lsSwitch : String ;
begin
    lsSwitch := UpperCase(asParameter) ;

    if lsSwitch = 'ON'
    then begin
        pbUtf8 := True ;

        SendAnswer(MSG_FTP_UTF8_ON) ;
    end
    else if lsSwitch = 'OFF'
    then begin
        pbUtf8 := False ;

        SendAnswer(MSG_FTP_UFT8_OFF) ;
    end
    else begin
        SendAnswer(MSG_FTP_UTF8_BAD) ;
    end ;
end ;
