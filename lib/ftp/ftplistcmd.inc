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
    // Command
    lsCommand : String ;
    // Parameter
    lsParameter : String ;
    // Data Socket
    loDataSocket : TTCPBlockSocket ;
begin
    // if "-la"
    if (InternalLengthStr(asParameter, pbUtf8) > 0) and
        (InternalCopyStr(asParameter, 1, 1, pbUtf8) = '-')
    then begin
        lsCommand := '' ;
        lsParameter := '' ;

        ExplodeCommand(asParameter, lsCommand, lsParameter) ;
    end
    else begin
        lsParameter := asParameter ;
    end ;

    loDataSocket := GetDataSocket ;

    if Assigned(loDataSocket)
    then begin
        ListDirectory(lsParameter, loDataSocket, abNlst) ;

        CloseDataConnection(loDataSocket) ;
    end ;
end ;
