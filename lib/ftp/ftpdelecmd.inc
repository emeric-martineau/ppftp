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
    // rename
    lbDelete : Boolean ;
    // Local config
    lsLocalConfigValue : String ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    lbDelete := true ;

    lsLocalConfigValue := '' ;

    // Check if can send file
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_DELETE, lsLocalConfigValue)
    then begin
        lbDelete := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file
    if not lbDelete or
        not prUserConfig.Delete or
        not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else if IsFileProtected(lsSysFileName)
    then begin
        // File is protected. We say not found
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else if CheckFileExists(lsSysFileName, pbUtf8)
    then begin
        if InternalDeleteFile(lsSysFileName, pbUtf8)
        then begin
            SendAnswer(Format(MSG_FTP_FILE_DELETED, [asParameter])) ;
        end
        else begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end ;
end ;
