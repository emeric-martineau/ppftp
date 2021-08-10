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
    lbDeleteDirectory : Boolean ;
    // Local config
    lsLocalConfigValue : String ;

    //
    // Check if contain file
    //
    // @return true if aborted
    function InternalIsContainFile(const asPath : String) : Boolean ;
    var
        // Result of find
        liFindResult : Integer ;
        // Search structure
        lrSearchRec : TSearchRec ;
        // Find file
        lsFindFilePath : String ;
        // Full directory
        lsFullDir : String ;
    begin
        lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
            false) + '*.*' ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator, false) ;

        liFindResult := FindFirst(lsFindFilePath, faanyfile, lrSearchRec) ;

        Result := False ;

        while (liFindResult = 0) and not Result do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                        and prUserConfig.SubDir
                    then begin
                        Result := True ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                        Result := True ;
                    end ;
                end ;
            end ;

            liFindResult := FindNext(lrSearchRec) ;
        end ;

        FindClose(lrSearchRec) ;
    end ;

    //
    // Check if contain file
    //
    // @return true if aborted
    function InternalIsContainFileUTF8(const asPath : String) : Boolean ;
    var
        // Result of find
        liFindResult : Integer ;
        // Search structure
        lrSearchRec : TSearchRec ;
        // Find file
        lsFindFilePath : String ;
        // Full directory
        lsFullDir : String ;
    begin
        lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
            true) + '*.*' ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator, true) ;

        liFindResult := FindFirstUTF8(lsFindFilePath, faanyfile, lrSearchRec) ;

        Result := False ;

        while (liFindResult = 0) and not Result do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                        and prUserConfig.SubDir
                    then begin
                        Result := True ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                        Result := True ;
                    end ;
                end ;
            end ;

            liFindResult := FindNextUTF8(lrSearchRec) ;
        end ;

        FindCloseUTF8(lrSearchRec) ;
    end ;

    //
    // Check if directory contain file
    function InternalIsContainFile(const asPath : String;
        const abUtf8 : Boolean) : Boolean ;
    begin
        if abUtf8
        then begin
            Result := InternalIsContainFileUTF8(asPath) ;
        end
        else begin
            Result := InternalIsContainFile(asPath) ;
        end ;
    end ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    lbDeleteDirectory := true ;

    lsLocalConfigValue := '' ;

    // Check if can delete directory
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_DELETE_DIRECTORY, lsLocalConfigValue)
    then begin
        lbDeleteDirectory := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file
    if not lbDeleteDirectory or IsFileProtected(lsSysFileName) or
        not prUserConfig.DeleteDirectory or
        not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else if not CheckDirectoryExists(lsSysFileName, pbUtf8)
    then begin
        // If directory exist
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end
    else if InternalIsContainFile(lsSysFileName, pbUtf8)
    then begin
        SendAnswer(Format(MSG_FTP_DIR_CONTAIN_FILE, [asParameter])) ;
    end
    else begin
        // First, delete all protected file
        // Second, remove directory
        if RemoveFileProtected(lsSysFileName) and
            InternalRemoveDirectory(lsSysFileName, pbUtf8)
        then begin
            SendAnswer(Format(MSG_FTP_DIR_DELETED, [asParameter])) ;
        end
        else begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end ;
    end ;
end ;
