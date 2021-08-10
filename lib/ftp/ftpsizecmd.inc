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
    // Search structure
    lrSearchRec : TSearchRec ;
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
            FindFirstUTF8(lsSysFileName, faanyfile, lrSearchRec) ;
        end
        else begin
            FindFirst(lsSysFileName, faanyfile, lrSearchRec) ;
        end ;

        SendAnswer(Format(MSG_FTP_FILE_SIZE, [lrSearchRec.Size])) ;
    end;
end ;
