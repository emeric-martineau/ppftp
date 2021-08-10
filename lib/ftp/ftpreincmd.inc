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
    pbUtf8 := False ;

    ClosePassiveSocket;

    // done in ClosePassiveSocket
    //pbPassiveMode := False ;

    pbTransfertMode := tmAscii ;

    psRenameFromFile := '' ;

    psRenameFromFileSys := '' ;

    InitDefaultUser ;

    // done if not rest command
    //piStartTransfertFileValue := 0 ;

    SendAnswer(MSG_FTP_REIN_OK) ;
end ;
