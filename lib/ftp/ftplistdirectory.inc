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
    // File exists
    lbFileExists : Boolean ;
    // Directory Exist
    lbFolderExists : Boolean ;
    // Sys file name
    lsSysFileName : String ;
    // True if aborted
    lbAborted : Boolean ;
    // If can read
    lbCanRead : Boolean ;

    //
    // List directory
    //
    // @return true if aborted
    function ShowDirectory(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) : Boolean ;
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
        if not abIsFile
        then begin
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
                false) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

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
                        aoDataSocket.SendString(asPrefixe + GetDirectory(lrSearchRec) +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(asPrefixe + GetFile(lrSearchRec) +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNext(lrSearchRec) ;

            Result := IsAborted ;
        end ;

        FindClose(lrSearchRec) ;
    end ;

    //
    // List directory UTF8
    //
    // @return true if aborted
    function ShowDirectoryUTF8(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) : Boolean ;
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
        if not abIsFile
        then begin
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
                true) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

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
                        aoDataSocket.SendString(asPrefixe + GetDirectory(lrSearchRec) +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(asPrefixe + GetFile(lrSearchRec) +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNextUTF8(lrSearchRec) ;

            Result := IsAborted ;
        end ;

        FindCloseUTF8(lrSearchRec) ;
    end ;

    //
    // List directory
    //
    // @return true if aborted
    function ShowDirectoryNlst(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) : Boolean ;
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
        if not abIsFile
        then begin
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
                false) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

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
                        aoDataSocket.SendString(asPrefixe + lrSearchRec.Name +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(asPrefixe + lrSearchRec.Name +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNext(lrSearchRec) ;

            Result := IsAborted ;
        end ;

        FindClose(lrSearchRec) ;
    end ;

    //
    // List directory UTF8
    //
    // @return true if aborted
    function ShowDirectoryNlstUTF8(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) : Boolean ;
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
        if not abIsFile
        then begin
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator,
                true) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

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
                        aoDataSocket.SendString(asPrefixe + lrSearchRec.Name +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(asPrefixe + lrSearchRec.Name +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNextUTF8(lrSearchRec) ;

            Result := IsAborted ;
        end ;

        FindCloseUTF8(lrSearchRec) ;
    end ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asFtpFolderName, pbUtf8) ;

    // If file or directory exists
    lbFolderExists := CheckDirectoryExists(lsSysFileName, pbUtf8) ;
    lbFileExists := CheckFileExists(lsSysFileName, pbUtf8) ;

    // If directory is readable or if directory who contains file is readble
    if (lbFileExists or lbFolderExists)
    then begin
        if lbFileExists
        then begin
            lbCanRead := CanReadDirectory(prUserConfig.Root,
                ExtractFileDir(lsSysFileName), pbUtf8) ;
        end
        else begin
            lbCanRead := CanReadDirectory(prUserConfig.Root, lsSysFileName,
                pbUtf8) ;
        end ;

        if not lbCanRead or not IsRootInPath(prUserConfig.Root, lsSysFileName,
            pbUtf8)
        then begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end
        else if IsFileProtected(lsSysFileName)
        then begin
            // File is protected. We say not found
            SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asFtpFolderName])) ;
        end
        else begin
            if Assigned(aoDataSocket)
            then begin
                SendAnswer(asStartMessage) ;

                if abNlst
                then begin
                    if pbUtf8
                    then begin
                        lbAborted := ShowDirectoryNlstUTF8(lsSysFileName,
                            lbFileExists, aoDataSocket) ;
                    end
                    else begin
                        lbAborted := ShowDirectoryNlst(lsSysFileName,
                            lbFileExists, aoDataSocket) ;
                    end ;
                end
                else begin
                    if pbUtf8
                    then begin
                        lbAborted := ShowDirectoryUTF8(lsSysFileName,
                            lbFileExists, aoDataSocket) ;
                    end
                    else begin
                        lbAborted := ShowDirectory(lsSysFileName, lbFileExists,
                            aoDataSocket) ;
                    end ;
                end ;

                if lbAborted
                then begin
                    SendAnswer(MSG_FTP_TRANS_ABORTED) ;
                end
                else begin
                    SendAnswer(asEndMessage) ;
                end ;
            end ;
        end ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asFtpFolderName])) ;
    end ;
end ;
