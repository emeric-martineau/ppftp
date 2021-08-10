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
    lbRename : Boolean ;
    // Local config
    lsLocalConfigValue : String ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    lbRename := true ;

    lsLocalConfigValue := '' ;

    // Check if can send file
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_RENAME, lsLocalConfigValue)
    then begin
        lbRename := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file
    if not lbRename or
        not prUserConfig.Rename or
        not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else if IsFileProtected(lsSysFileName)
    then begin
        // File is protected. We say not found
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else if CheckFileExists(lsSysFileName, pbUtf8) or
        CheckDirectoryExists(lsSysFileName, pbUtf8)
    then begin
        psRenameFromFileSys := lsSysFileName ;
        psRenameFromFile := asParameter ;

        SendAnswer(MSG_FTP_RENAME_FROM) ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end ;
end ;
