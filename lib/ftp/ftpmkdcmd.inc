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
    lbMakeDirectory : Boolean ;
    // Local config
    lsLocalConfigValue : String ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    lbMakeDirectory := true ;

    lsLocalConfigValue := '' ;

    // Check if can create directory
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_MAKE_DIRECTORY, lsLocalConfigValue)
    then begin
        lbMakeDirectory := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file
    if not lbMakeDirectory or
        not prUserConfig.MakeDirectory or
        not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
{    else if IsFileProtected(lsSysFileName)
    then begin
        // File is protected. We say not found
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asFtpFolderName])) ;
    end
}
    else if CheckDirectoryExists(lsSysFileName, pbUtf8)
    then begin
        // If directory exist
        SendAnswer(Format(MSG_FTP_DIR_EXISTS, [asParameter])) ;
    end
    else if not CheckDirectoryExists(ExtractFilePath(lsSysFileName), pbUtf8)
    then begin
        // If parent directory doesn't exist
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else begin
        if InternalMakeDirectory(lsSysFileName, pbUtf8)
        then begin
            SendAnswer(Format(MSG_FTP_DIR_CREATED, [asParameter])) ;
        end
        else begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end ;
    end ;
end ;
