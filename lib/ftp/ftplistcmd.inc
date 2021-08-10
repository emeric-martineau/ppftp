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
    if (Length(asParameter) > 0) and (asParameter[1] = '-')
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
