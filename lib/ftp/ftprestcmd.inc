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
    liValue : Integer ;
begin
    liValue := 0 ;

    if TryIntParse(asParameter, liValue)
    then begin
        piStartTransfertFileValue := liValue ;

        SendAnswer(Format(MSG_FTP_REST_OK, [liValue])) ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_REST_FAIL, [asParameter])) ;
    end ;
end ;
