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
    liCount : Integer ;
begin
    pbPassiveMode := False ;

    // Close previous passive port
    ClosePassiveSocket ;

    prPassivePort := GetPassivePort ;

    if prPassivePort <> nil
    then begin
        // Open socket
        poPassivePortSock := TTCPBlockSocket.Create ;

        try
            // Check if port is used
            for liCount := 1 to 3 do
            begin
                poPassivePortSock.Bind(poClientSock.GetLocalSinIP,
                    IntToStr(prPassivePort^.Port)) ;

                if poPassivePortSock.LastError = 0
                then begin
                    break ;
                end ;

                FreePassivePort(prPassivePort) ;

                prPassivePort := GetPassivePort ;
            end ;

            if poPassivePortSock.LastError = 0
            then begin
                poPassivePortSock.SetLinger(LINGER_ENABLE, LINGER_DELAY);

                poPassivePortSock.Listen ;

                pbPassiveMode := True ;

                SendAnswer(Format(MSG_FTP_PASSIVE_PORT_OK,
                    [TranslateToPassivePort(poPassivePortSock.GetLocalSinIP,
                    poPassivePortSock.GetLocalSinPort)])) ;
            end ;

            if poPassivePortSock.LastError <> 0
            then begin
                ClosePassiveSocket ;

                SendAnswer(MSG_FTP_PASSIVE_PORT_SOCKET_FAIL) ;
            end;
        Except
            ClosePassiveSocket ;

            SendAnswer(MSG_FTP_PASSIVE_PORT_SOCKET_FAIL) ;
        end ;
    end
    else begin
        SendAnswer(MSG_FTP_PASSIVE_PORT_FAIL) ;
    end ;
end ;
