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

    procedure ren(const asOldName : String; const asNewName : String;
        const abUtf8 : Boolean; const asMessage : String) ;
    begin
        if InternalRenameFile(asOldName, asNewName, abUtf8)
        then begin
            SendAnswer(Format(asMessage, [psRenameFromFile,
                asParameter])) ;
        end
        else begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end ;
    end ;

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
    if not lbRename or IsFileProtected(lsSysFileName) or
        not prUserConfig.Rename
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else if CheckFileExists(lsSysFileName, pbUtf8)
    then begin
        SendAnswer(Format(MSG_FTP_FILE_EXISTS, [asParameter])) ;
    end
    else if CheckDirectoryExists(lsSysFileName, pbUtf8)
    then begin
        SendAnswer(Format(MSG_FTP_DIR_EXISTS, [asParameter])) ;
    end
    else if CheckFileExists(psRenameFromFileSys, pbUtf8)
    then begin
        ren(psRenameFromFileSys, lsSysFileName, pbUtf8, MSG_FTP_FILE_RENAMED) ;
    end
    else if CheckDirectoryExists(psRenameFromFileSys, pbUtf8)
    then begin
        ren(psRenameFromFileSys, lsSysFileName, pbUtf8, MSG_FTP_DIR_RENAMED) ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [psRenameFromFile])) ;
    end ;
end ;
