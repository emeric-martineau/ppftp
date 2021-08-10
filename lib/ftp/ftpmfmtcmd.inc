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
    // Command
    lsFileDateTime : String ;
    // Parameter
    lsParameter : String ;
    // Date Time
    lsYear : String ;
    lsMonth : String ;
    lsDay : String ;
    lsHour : String ;
    lsMinute : String ;
    lsSeconde : String ;
    // Date Time
    lrDateTime : TDateTime;
    // File date
    liFileDate : Integer ;
begin
    lsFileDateTime := '' ;
    lsParameter := '' ;

    ExplodeCommand(asParameter, lsFileDateTime, lsParameter) ;

    if (InternalLengthStr(lsFileDateTime, pbUtf8) <> 14) or
       (AnsiCompareStr(lsParameter, '') = 0)
    then begin
        SendAnswer(MSG_FTP_SYNTAX_ERROR) ;
    end
    else begin

        // Translate ftp name to sys name
        lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
                psFtpCurrentDirectory, lsParameter, pbUtf8) ;

        if IsFileProtected(lsSysFileName)
        then begin
            SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [lsParameter])) ;
        end
        else if not CheckFileExists(lsSysFileName, pbUtf8)
        then begin
             SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [lsParameter])) ;
        end
        else if not IsRootInPath(prUserConfig.Root, lsSysFileName, pbUtf8)
        then begin
            SendAnswer(MSG_FTP_ACCESS_DENIED) ;
        end
        else begin
            lsYear := InternalCopyStr(lsFileDateTime, 1, 4, pbUtf8) ;
            lsMonth := InternalCopyStr(lsFileDateTime, 5, 2, pbUtf8) ;
            lsDay := InternalCopyStr(lsFileDateTime, 7, 2, pbUtf8) ;
            lsHour := InternalCopyStr(lsFileDateTime, 9, 2, pbUtf8) ;
            lsMinute := InternalCopyStr(lsFileDateTime, 11, 2, pbUtf8) ;
            lsSeconde := InternalCopyStr(lsFileDateTime, 13, 2, pbUtf8) ;

            lrDateTime := StrToDateTime(lsDay +
                DefaultFormatSettings.DateSeparator + lsMonth +
                DefaultFormatSettings.DateSeparator + lsYear +
                ' ' + lsHour + DefaultFormatSettings.TimeSeparator + lsMinute +
                DefaultFormatSettings.TimeSeparator + lsSeconde) ;

            liFileDate := DateTimeToFileDate(lrDateTime) ;

            if pbUtf8
            then begin
                FileSetDateUTF8(lsSysFileName, liFileDate) ;
            end
            else begin
                FileSetDate(lsSysFileName, liFileDate) ;
            end ;

            SendAnswer(Format(MSG_FTP_FILE_MFMT, [lsFileDateTime,
                lsParameter])) ;
        end ;

    end ;

end ;
