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
    lsParameter : String ;
begin
    lsParameter := UpperCase(asParameter) ;

    if lsParameter = 'I'
    then begin
        pbTransfertMode := tmBinary ;
        SendAnswer(MSG_FTP_TYPE_I) ;
    end
    else if lsParameter = 'A'
    then begin
        pbTransfertMode := tmAscii ;
        SendAnswer(MSG_FTP_TYPE_A) ;
    end
    else begin
        SendAnswer(MSG_FTP_TYPE_ERROR) ;
    end ;
end ;
