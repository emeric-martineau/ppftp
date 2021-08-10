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
    lbUpload : Boolean ;
    // Local config
    lsLocalConfigValue : String ;

    //
    // Send a acii file
    //
    // @param asFileName filename to send
    // @param aoDataSocket socket to send
    // @param abAppend if append file
    //
    // @return if no error, error or abort
    function ReceiveAsciiFile(const asFileName : String;
        const aoDataSocket : TTCPBlockSocket;
        const abAppend : Boolean) : TTransfertResult ;
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
        if abAppend
        then begin
            OpenTextFileForAppend(asFileName, lrTextFile) ;
        end
        else begin
            OpenTextFileForWrite(asFileName, lrTextFile) ;
        end ;

        lbAborted := IsAborted ;

        while (IOResult = 0)
            and not lbAborted and
            (aoDataSocket.LastError = 0) do
        begin
            lsLine := aoDataSocket.RecvString(WAITING_FOR_INPUT) ;

            WriteLn(lrTextFile, lsLine) ;

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
        else if (IOResult = 0) {and ((aoDataSocket.LastError = 0) or
            (aoDataSocket.LastError = WSAETIMEDOUT))}
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
    // @param abAppend if append file
    //
    // @return if no error, error or abort
    function ReceiveBinaryFile(const asFileName : String;
        const aoDataSocket : TTCPBlockSocket;
        const abAppend : Boolean) : TTransfertResult ;
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
            if ((piStartTransfertFileValue > 0) or abAppend) and
                CheckFileExists(asFileName, pbUtf8)
            then begin
                loFile := TFileStream.Create(asFileName,
                    fmAppend or fmShareExclusive) ;

                loFile.Position := piStartTransfertFileValue ;
            end
            else begin
                loFile := TFileStream.Create(asFileName,
                    fmOpenWrite or fmShareExclusive or fmCreate) ;
            end ;

            liCountRead := 1 ;

            lbAborted := IsAborted ;

            while (liCountRead > 0) and not lbAborted
                and (aoDataSocket.LastError = 0) do
            begin
                liCountRead := aoDataSocket.RecvBufferEx(lpBuffer,
                    piUserBufferSize, WAITING_FOR_INPUT) ;

                loFile.Write(lpBuffer[0], liCountRead) ;

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
            else if (aoDataSocket.LastError <> 0) and
                (aoDataSocket.LastError <> WSAETIMEDOUT)
            then begin
                Result := trNoError ;
            end
            else begin
                Result := trError ;
            end ;
        except
            on e : Exception do
            begin
                Error(Format(MSG_ERROR_WRITE_FILE, [asFileName,
                    e.Message])) ;

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

    lbUpload := true ;

    lsLocalConfigValue := '' ;

    if atMode = scmUnique
    then begin
        // Add directory separator
        lsSysFileName := AddTrailing(lsSysFileName, DirectorySeparator) ;

        lsSysFileName := GetTempFileName(lsSysFileName, 'STOU_') ;
    end ;

    // Check if can send file
    if FolderLocalConfigReader(ExtractFileDir(lsSysFileName),
         FOLDER_CONF_UPLOAD, lsLocalConfigValue)
    then begin
        lbUpload := lsLocalConfigValue <> NO_VALUE ;
    end ;

    // User can't download or download forbiden by local config or
    // is protected file or if read-only
    if not lbUpload or IsFileProtected(lsSysFileName) or
        not prUserConfig.Upload or
        (CheckFileExists(lsSysFileName, pbUtf8) and
          ((GetFileAttribut(lsSysFileName, pbUtf8) and faReadOnly) = faReadOnly))
    then begin
        SendAnswer(MSG_FTP_ACCESS_DENIED) ;
    end
    else begin
        loDataSocket := GetDataSocket ;

        if Assigned(loDataSocket)
        then begin
            {$IFDEF CALL_BACK_TRANSFERT}
            SetTransfert(lsSysFileName, UPLOAD_FILE, START_TRANSFERT) ;
            {$ENDIF}

            if atMode = scmUnique
            then begin
                SendAnswer(Format(MSG_FTP_STOU,
                    [ExtractFileName(lsSysFileName)])) ;
            end
            else begin
                if pbTransfertMode = tmAscii
                then begin
                    SendAnswer(MSG_FTP_OPEN_ASCII_DATA_CONNECTION) ;
                end
                else begin
                    SendAnswer(MSG_FTP_OPEN_BINARY_DATA_CONNECTION) ;
                end ;
            end ;

            if pbTransfertMode = tmAscii
            then begin
                lrError := ReceiveAsciiFile(lsSysFileName, loDataSocket,
                    atMode = scmAppend) ;
            end
            else begin
                lrError := ReceiveBinaryFile(lsSysFileName, loDataSocket,
                    atMode = scmAppend) ;
            end ;

            {$IFDEF CALL_BACK_TRANSFERT}
            SetTransfert(lsSysFileName, UPLOAD_FILE, STOP_TRANSFERT) ;
            {$ENDIF}

            case lrError of
                trAborted : SendAnswer(MSG_FTP_TRANS_ABORTED) ;
                trNoError : SendAnswer(MSG_FTP_END_DATA_CONNECTION) ;
                else
                    SendAnswer(MSG_FTP_TRANS_ERROR) ;
            end ;

            CloseDataConnection(loDataSocket) ;
        end ;
    end ;
end ;
