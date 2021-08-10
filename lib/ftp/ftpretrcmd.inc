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
    // Data Socket
    loDataSocket : TTCPBlockSocket ;
    // If no error
    lrError : TTransfertResult ;
    // Download
    lbDownload : Boolean ;
    // Local config
    lsLocalConfigValue : String ;

    //
    // Send a acii file
    //
    // @param asFileName filename to send
    // @param aoDataSocket socket to send
    //
    // @return if no error, error or abort
    function SendAsciiFile(const asFileName : String;
        const aoDataSocket : TTCPBlockSocket) : TTransfertResult ;
    var
        // File
        lrTextFile : TextFile ;
        // Aborted
        lbAborted : Boolean ;
        // Line
        lsLine : String ;
        // Sleep time
        //liSleepTime : Integer ;
    begin
        {$IOChecks off}
        OpenTextFileForRead(asFileName, lrTextFile) ;

        lbAborted := IsAborted ;

        while (IOResult = 0) and not EOF(lrTextFile)
            and not lbAborted and (aoDataSocket.LastError = 0) do
        begin
            ReadLn(lrTextFile, lsLine) ;

            if not EOF(lrTextFile)
            then begin
                lsLine := lsLine + FTP_EOL ;
            end ;

            aoDataSocket.SendString(lsLine) ;

            lbAborted := IsAborted ;

            if piWaitingTransfertTime > 0
            then begin
                //liSleepTime := piWaitingTransfertTime * Length(lsLine) div
                //    piByteRate ;

                Sleep(piWaitingTransfertTime) ;
            end ;
        end ;

        if lbAborted
        then begin
            Result := trAborted ;
        end
        else if IOResult = 0
        then begin
            Result := trNoError ;
        end
        else begin
            Error(GetIoResultString(IOResult) + ' ' + asFileName) ;
            Result := trError ;
        end ;

        CloseFile(lrTextFile) ;
        {$IOChecks on}
    end ;

    //
    // Send a binary file
    //
    // @param asFileName filename to send
    // @param aoDataSocket socket to send
    //
    // @return if no error, error or abort
    function SendBinaryFile(const asFileName : String;
        const aoDataSocket : TTCPBlockSocket) : TTransfertResult ;
    var
        loFile : TFileStream;
        // Aborted
        lbAborted : Boolean ;
        // Buffer
        lpBuffer : PByte ;
        // Count of read
        liCountRead : Integer ;
    begin
        GetMem(lpBuffer, piUserBufferSize) ;

        loFile := nil ;

        try
            loFile := TFileStream.Create(asFileName,
                fmOpenRead or fmShareDenyWrite) ;

            loFile.Position := piStartTransfertFileValue ;

            liCountRead := piUserBufferSize ;

            lbAborted := IsAborted ;

            while (liCountRead = piUserBufferSize) and not lbAborted
                and (aoDataSocket.LastError = 0) do
            begin
                liCountRead := loFile.Read(lpBuffer[0], piUserBufferSize) ;

                aoDataSocket.SendBuffer(lpBuffer, liCountRead) ;

                lbAborted := IsAborted ;

                if piWaitingTransfertTime > 0
                then begin
                    Sleep(piWaitingTransfertTime) ;
                end ;
            end ;

            if lbAborted
            then begin
                Result := trAborted ;
            end
            else begin
                Result := trNoError ;
            end ;
        except
            on e : Exception do
            begin
                Error(Format(MSG_ERROR_READ_FILE, [asFileName, e.Message])) ;

                Result := trError ;
            end ;
        end ;

        FreeMem(lpBuffer) ;

        if Assigned(loFile)
        then begin
            loFile.Free ;
        end;
    end ;
begin
    // Translate ftp name to sys name
    lsSysFileName := ConvertFtpPathToSysPath(prUserConfig.Root,
            psFtpCurrentDirectory, asParameter, pbUtf8) ;

    lbDownload := true ;

    lsLocalConfigValue := '' ;

    // Check if can send file
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_DOWNLOAD, lsLocalConfigValue)
    then begin
        lbDownload := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file
    if not lbDownload or IsFileProtected(lsSysFileName) or
        not prUserConfig.Download
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else if CheckFileExists(lsSysFileName, pbUtf8)
    then begin
        loDataSocket := GetDataSocket ;

        if Assigned(loDataSocket)
        then begin
            {$IFDEF CALL_BACK_TRANSFERT}
            SetTransfert(lsSysFileName, DOWNLOAD_FILE, START_TRANSFERT) ;
            {$ENDIF}

            if pbTransfertMode = tmAscii
            then begin
                SendAnswer(MSG_FTP_OPEN_ASCII_DATA_CONNECTION) ;

                lrError := SendAsciiFile(lsSysFileName, loDataSocket) ;
            end
            else begin
                SendAnswer(MSG_FTP_OPEN_BINARY_DATA_CONNECTION) ;

                lrError := SendBinaryFile(lsSysFileName, loDataSocket) ;
            end ;

            {$IFDEF CALL_BACK_TRANSFERT}
            SetTransfert(lsSysFileName, DOWNLOAD_FILE, STOP_TRANSFERT) ;
            {$ENDIF}

            case lrError of
                trAborted : SendAnswer(MSG_FTP_TRANS_ABORTED) ;
                trNoError : SendAnswer(MSG_FTP_END_DATA_CONNECTION) ;
                else
                    SendAnswer(MSG_FTP_TRANS_ERROR) ;
            end ;

            CloseDataConnection(loDataSocket) ;
        end ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_LIST_NO_FOUND, [asParameter])) ;
    end ;
end ;
