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
    SendAnswer('211-Features:') ;
    //SendAnswer(' MDTM') ;
    //SendAnswer(' SIZE') ;

    if FUtf8Support
    then begin
        SendAnswer(' UTF8 ON|OFF') ;
    end ;

    //SendAnswer(' CLNT') ;
    //SendAnswer(' MFMT') ;
    SendAnswer('211 End') ;
end ;