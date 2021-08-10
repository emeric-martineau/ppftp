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

    //
    // List directory
    procedure ShowDirectory(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) ;
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
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator) ;

        liFindResult := FindFirst(lsFindFilePath, faanyfile, lrSearchRec) ;

        while liFindResult = 0 do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                    then begin
                        aoDataSocket.SendString(GetDirectory(lrSearchRec) +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(GetFile(lrSearchRec) +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNext(lrSearchRec) ;
        end ;

        FindClose(lrSearchRec) ;
    end ;

    //
    // List directory UTF8
    procedure ShowDirectoryUTF8(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) ;
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
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator) ;

        liFindResult := FindFirstUTF8(lsFindFilePath, faanyfile, lrSearchRec) ;

        while liFindResult = 0 do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                    then begin
                        aoDataSocket.SendString(GetDirectory(lrSearchRec) +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(GetFile(lrSearchRec) +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNextUTF8(lrSearchRec) ;
        end ;

        FindCloseUTF8(lrSearchRec) ;
    end ;

    //
    // List directory
    procedure ShowDirectoryNlst(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) ;
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
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator) ;

        liFindResult := FindFirst(lsFindFilePath, faanyfile, lrSearchRec) ;

        while liFindResult = 0 do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                    then begin
                        aoDataSocket.SendString(lrSearchRec.Name +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(lrSearchRec.Name +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNext(lrSearchRec) ;
        end ;

        FindClose(lrSearchRec) ;
    end ;

    //
    // List directory UTF8
    procedure ShowDirectoryNlstUTF8(const asPath : String; const abIsFile : Boolean;
        const aoDataSocket : TTCPBlockSocket) ;
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
            lsFindFilePath := AddTrailing(asPath, DirectorySeparator) + '*.*' ;
        end
        else begin
            lsFindFilePath := asPath ;
        end ;

        lsFullDir := AddTrailing(asPath, DirectorySeparator) ;

        liFindResult := FindFirstUTF8(lsFindFilePath, faanyfile, lrSearchRec) ;

        while liFindResult = 0 do
        begin
            if ((lrSearchRec.Attr and faHidden) = 0)
                and ((lrSearchRec.Attr and faSysFile) = 0)
                and ((lrSearchRec.Attr and faVolumeID) = 0)
            then begin
                if (lrSearchRec.Attr and faDirectory) > 0
                then begin
                    if (lrSearchRec.Name <> '.') and (lrSearchRec.Name <> '..')
                    then begin
                        aoDataSocket.SendString(lrSearchRec.Name +
                            FTP_EOL) ;
                    end ;
                end
                else begin
                    if not IsFileProtected(lsFullDir + lrSearchRec.Name)
                    then begin
                         aoDataSocket.SendString(lrSearchRec.Name +
                            FTP_EOL) ;
                    end    ;
                end ;
            end ;

            liFindResult := FindNextUTF8(lrSearchRec) ;
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
    if lbFileExists or lbFolderExists
    then begin
        if IsFileProtected(lsSysFileName)
        then begin
            // File is protected. We say not found
            SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asFtpFolderName])) ;
        end
        else begin
            if Assigned(aoDataSocket)
            then begin
                SendAnswer(MSG_FTP_OPEN_ASCII_DATA_CONNECTION) ;

                if abNlst
                then begin
                    if pbUtf8
                    then begin
                        ShowDirectoryNlstUTF8(lsSysFileName, lbFileExists, aoDataSocket) ;
                    end
                    else begin
                        ShowDirectoryNlst(lsSysFileName, lbFileExists, aoDataSocket) ;
                    end ;
                end
                else begin
                    if pbUtf8
                    then begin
                        ShowDirectoryUTF8(lsSysFileName, lbFileExists, aoDataSocket) ;
                    end
                    else begin
                        ShowDirectory(lsSysFileName, lbFileExists, aoDataSocket) ;
                    end ;
                end ;

                SendAnswer(MSG_FTP_END_DATA_CONNECTION) ;
            end ;
        end ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asFtpFolderName])) ;
    end ;
end ;
