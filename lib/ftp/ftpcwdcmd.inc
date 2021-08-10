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
    lsPath : String ;
    // Sys path
    lsSysPath : String ;
begin
    if prUserConfig.ListSubDir
    then begin
        lsPath := asParameter ;

        if abCDUP
        then begin
            lsPath := '..' ;
        end ;

        // check if path start by '/'. If not, add current ftp path
        if (Length(lsPath) > 0) and (lsPath[1] = '/')
        then begin
            psFtpCurrentDirectory := '' ;
        end ;

        lsSysPath := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, lsPath, pbUtf8) ;

        lsSysPath := AddTrailing(lsSysPath, DirectorySeparator) ;

        // 0-> not start by
        if IsRootInPath(prUserConfig.Root, lsSysPath, pbUtf8)
        then begin
            // Check if directory exist
            if CheckDirectoryExists(lsSysPath, pbUtf8)
            then begin
                if CanReadDirectory(prUserConfig.Root, lsSysPath)
                then begin
                    if Length(psFtpCurrentDirectory) > 0
                    then begin
                        psFtpCurrentDirectory := AddTrailing(
                            psFtpCurrentDirectory, '/') ;
                    end ;

                    psFtpCurrentDirectory := GetAbsoluteFtpPath(
                        psFtpCurrentDirectory + lsPath) ;

                    SendAnswer(MSG_FTP_CWD_OK) ;
                end
                else begin
                    SendAnswer(MSG_FTP_ACCESS_DIRECTORY_DENIED) ;
                end ;
            end
            else begin
                // Delete and directory separator
                lsSysPath := Copy(lsSysPath, 1, Length(lsSysPath) - 1) ;

                if CheckFileExists(lsSysPath, pbUtf8)
                then begin
                    SendAnswer(Format(MSG_FTP_NOT_A_DIRECTORY, [asParameter]));
                end
                else begin
                    SendAnswer(MSG_FTP_FILE_OR_DIR_NOT_FOUND) ;
                end ;
            end ;
        end
        else begin
            SendAnswer(MSG_FTP_ACCESS_DIRECTORY_DENIED) ;
        end ;
    end
    else begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end ;
end ;
