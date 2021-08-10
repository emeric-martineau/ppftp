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
    // Sys file name
    lsSysFileName : String ;
    // File date
    liFileDate : Integer ;
    // File date
    lrFileTime : TDateTime ;
    // MDTM result
    lsMdtmResult : String ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    if IsFileProtected(lsSysFileName)
    then begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else if not CheckFileExists(lsSysFileName, pbUtf8)
    then begin
         SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else if not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else begin
        if pbUtf8
        then begin
            liFileDate := FileAgeUTF8(lsSysFileName) ;
        end
        else begin
            liFileDate := FileAge(lsSysFileName) ;
        end ;

        lrFileTime := FileDateToDateTime(liFileDate) ;

        lsMdtmResult := FormatDateTime('yyyymmddhhnnss', lrFileTime) ;

        SendAnswer(Format(MSG_FTP_FILE_MDTM, [lsMdtmResult])) ;
    end;

end ;
