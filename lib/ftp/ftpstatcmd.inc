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
    // of connection
    lsType : String ;
begin
    if (InternalLengthStr(asParameter, pbUtf8) > 0)
    then begin
        // Display file or directory
        ListDirectory(asParameter, poClientSock, false, '    ',
            Format(MSG_FTP_STAT_FILE_START, [asParameter]),
            MSG_FTP_STAT_FILE_STOP) ;
    end
    else begin
        SendAnswer(MSG_FTP_STAT_START) ;

        SendAnswer('     Connected to ' + poClientSock.GetRemoteSinIP) ;
        SendAnswer('     Logged in as ' + prUserConfig.Login) ;

        if pbTransfertMode = tmBinary
        then begin
            lsType := 'binary' ;
        end
        else begin
            lsType := 'ascii' ;
        end ;

        SendAnswer('     Type : ' + lsType) ;

        if pbPassiveMode
        then begin
            lsType := 'yes (' + poPassivePortSock.GetLocalSinIP + ':' +
                IntToStr(poPassivePortSock.GetLocalSinPort) + ')' ;
        end
        else begin
            lsType := 'no' ;
        end ;

        SendAnswer('     Passive mode : ' + lsType) ;
        SendAnswer('     Restart transfert at : ' +
            IntToStr(piStartTransfertFileValue)) ;
        SendAnswer('     Mode : Stream') ;

        SendAnswer(MSG_FTP_STAT_STOP) ;
    end ;
end ;
