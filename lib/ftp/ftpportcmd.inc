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
    // List of value
    loValueList : TStringList ;
    // Ip address
    lsIpAddress : String ;
    // Invalide argument
    lbValide : Boolean ;
    // Temporary value
    liValue : Integer ;
    // Port
    liPort : Integer ;
begin
    loValueList := StringToTStringList(asParameter, ',') ;

    lbValide := loValueList.Count = 6 ;

    if lbValide
    then begin
        lsIpAddress := loValueList[0] + '.' + loValueList[1] + '.' +
            loValueList[2] + '.' + loValueList[3] ;

        liPort := 0 ;
        liValue := 0 ;

        lbValide := TryIntParse(loValueList[5], liPort) ;

        if lbValide
        then begin
            lbValide := TryIntParse(loValueList[4], liValue) ;

            if lbValide
            then begin
                liPort := liPort + 256 * liValue ;

                if liPort > 1023
                then begin
                    pbPassiveMode := False ;

                    psActiveModeIpAddresse := lsIpAddress ;

                    psActiveModePort := IntToStr(liPort) ;

                    SendAnswer(MSG_FTP_PORT_OK) ;

                    ClosePassiveSocket ;
                end
                else begin
                    SendAnswer(MSG_FTP_PORT_1024) ;
                end ;
            end ;
        end ;
    end ;

    if not lbValide
    then begin
        SendAnswer(MSG_FTP_PORT_INVALID) ;
    end ;
end ;
