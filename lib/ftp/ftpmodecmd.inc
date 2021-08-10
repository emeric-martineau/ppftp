// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
begin
    if UpperCase(asParameter) = 'S'
    then begin
        SendAnswer(Format(MSG_FTP_MODE_OK, ['S'])) ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_MODE_FAIL, [asParameter])) ;
    end ;
end ;
