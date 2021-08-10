// This is the main unit of FTP server
unit ftpmain;

// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FtpTypes, FtpFunctions, FtpMessages, FtpConst, FtpClient,
  blcksock, synsock, ssl_openssl, contnrs ;

{$I config.inc}

type
    // Mother class of FTP server
    TFtpMain = class(TThread)
      protected
        // Log
        FOnLog : TLogProcedure ;
        // Error
        FOnError : TLogProcedure ;
        // Main config reader
        FOnMainConfigRead : TMainConfigReader ;
        // Number of current client
        FClientCount : Integer ;
        // Config reader
        FOnClientConfigRead : TClientConfigReader ;
        {$IFDEF GUI_APPLICATION_SUPPORT}
        // Gui application for non-freeze log in console mode
        FGuiApplication : Boolean ;
        {$ENDIF}
        // Callback procedure for logout
        FOnLogin : TLoginLogoutProcedure ;
        // Callback procedure for logout
        FOnLogout : TLoginLogoutProcedure ;
        // If running
        FRunning : Boolean ;
        // Ture if ok, false if an error occur
        FExitStatus : Boolean ;

        // Port
        piListenPort : Integer ;
        // Host
        psListenHost : String ;
        // Welcome
        psWelcomeMessage : String ;
        // Goodbye
        psGoodbyMessage : String ;
        // Max client
        piMaxClient : Integer ;
        // Max session per user
        piMaxUser : Integer ;
        // Full log
        pbFullLog : Boolean ;
        // Time out
        piTimeOut : Integer ;
        // Passive port
        piPassivePortStart : Integer ;
        piPassivePortStop : Integer ;
        // Deny priority
        pbDenyPriority : Boolean ;
        // Allowed ip
        poAllowedIP : TStringList ;
        // Deny ip
        poDeniedIP : TStringList ;
        // Buffer size
        piBufferSize : Integer ;
        // User byte rate
        piUserByteRate : Integer ;
        // Utf8 support
        pbUtf8Support : Boolean ;

        // Block size
        piBlockSize : Integer ;
        // Waiting time
        piWaitingTime : Integer ;

        // Critical section for count
        poLockClientCount : TRTLCriticalSection ;
        // First client
        poFirstFtpClient : TFtpClient ;
        // Shutdown in progress
        pbShutDownInProgress : Boolean ;
        // Critical section for login/logout
        poLockLoginLogout : TRTLCriticalSection ;
        // HashMap for login count
        poListLoginCount : TFPHashList ;

        // Message for log or error. Don't use directely. Use Log() or Error()
        psMessageLogOrError : String ;
        // Login for synchronization
        psLoginName : String ;
        // Count of login for synchronization
        piLoginCount : Integer ;

        // Call FOnLog if set
        procedure Log(const asMessage : String) ;
        // Call FOnError if set
        procedure Error(const asMessage : String) ;
        {$IFDEF GUI_APPLICATION_SUPPORT}
        // Synchronized Log. Don't use directely. Use Log()
        procedure SynchronizeLog ;
        // Synchronized Error. Don't use directely. Use Error()
        procedure SynchronizeError ;
        {$ENDIF}
        // Synchronized login. Don't use directely, use AddLogin
        procedure SynchronizeLogin ;
        // Synchronized logout. Don't use directely, use AddLogin
        procedure SynchronizeLogout ;
        // Read configuration
        function ReadConfig : Boolean ;
        // Convert int
        function ConvertInteger(const asKey : String; const aiDefaultValue : Integer;
            var aiConvertedValue : Integer; const asErrorMsg : String) : Boolean ;
        // Convert string
        function ConvertString(asKey : String; asDefaultValue : String) : String ;
        // Convert boolean
        function ConvertBoolean(asKey : String; abDefaultValue : Boolean) : Boolean ;
        // Listen client connection
        procedure Run ;
        // Create a new client
        procedure CreateNewClient(loClientSock : TTCPBlockSocket) ;
        // Add client
        procedure AddClient(const aoClientFtp : TFtpClient) ;
        // Shutdown all client
        procedure RemoveAllClient ;
        // Remove all login
        procedure RemoveAllLogin ;
      public
        // Number of connection
        property ClientCount : Integer read FClientCount ;
        {$IFDEF GUI_APPLICATION_SUPPORT}
        // Gui application for non-freeze log in console mode. Set define GUI_APPLICATION_SUPPORT in config.inc
        property GuiApplication : Boolean read FGuiApplication write FGuiApplication ;
        {$ENDIF}
        // If running
        property Running : Boolean read FRunning ;
        // Exit status
        property ExitStatus : Boolean read FExitStatus ;
        // Log
        property OnLog : TLogProcedure read FOnLog write FOnLog ;
        // Error
        property OnError : TLogProcedure read FOnError write FOnError  ;
        // Main config reader
        property OnMainConfigRead : TMainConfigReader read FOnMainConfigRead write FOnMainConfigRead  ;
        // Client config reader
        property OnClientConfigRead : TClientConfigReader read FOnClientConfigRead write FOnClientConfigRead  ;
        // Callback procedure for logout. Must be shortest possible
        property OnLogin : TLoginLogoutProcedure read FOnLogin write FOnLogin ;
        // Callback procedure for logout. Must be shortest possible
        property OnLogout : TLoginLogoutProcedure read FOnLogout write FOnLogout ;
        // Constructor
        constructor Create(const abCreateSuspended : Boolean) ;
        // Destructor
        destructor Free ;
        // Execute main thread
        procedure Execute; override;
        // Delete client. Internal using only
        procedure RemoveClient(const aoClientFtp : TFtpClient) ;
        // Add login. Internal using only
        function AddLogin(const asLoginName : String) : Boolean ;
        // Remove login. Internal using only
        procedure RemoveLogin(const asLoginName : String) ;
    end ;

implementation

//
// Constructor
//
// @param abCreateSuspended if start supend thread
constructor TftpMain.Create(const abCreateSuspended : Boolean) ;
begin
    inherited Create(abCreateSuspended) ;

    FOnLog := nil ;
    FOnError := nil ;

    poFirstFtpClient := nil ;

    {$IFDEF GUI_APPLICATION_SUPPORT}
    FGuiApplication := False ;
    {$ENDIF}

    pbShutDownInProgress := False ;

    InitCriticalSection(poLockClientCount) ;

    InitCriticalSection(poLockLoginLogout) ;

    poListLoginCount := TFPHashList.Create ;

    FOnLogin := nil ;

    FOnLogout := nil ;

    FRunning := False ;
end ;

//
// Destructor
destructor TftpMain.Free ;
begin
    pbShutDownInProgress := True ;

    RemoveAllClient ;

    DoneCriticalSection(poLockClientCount) ;

    RemoveAllLogin ;

    DoneCriticalsection(poLockLoginLogout) ;

    poListLoginCount.Free ;

    //inherited Free ;
end ;

//
// Call Log procedure with synchronize
//
// @param asMessage message to display
procedure TFtpMain.Log(const asMessage : String) ;
begin
    if (asMessage <> '') and (FOnLog <> nil)
    then begin
        {$IFDEF GUI_APPLICATION_SUPPORT}
        if FGuiApplication
        then begin
            psMessageLogOrError := asMessage ;

            Synchronize(@SynchronizeLog) ;
        end
        else begin
        {$ENDIF}
            FOnLog(asMessage) ;
        {$IFDEF GUI_APPLICATION_SUPPORT}
        end ;
        {$ENDIF}
    end ;
end ;

//
// Call Error procedure with synchronize
//
// @param asMessage message to display
procedure TFtpMain.Error(const asMessage : String) ;
begin
    if (asMessage <> '') and (FOnError <> nil)
    then begin
        {$IFDEF GUI_APPLICATION_SUPPORT}
        if FGuiApplication
        then begin
            psMessageLogOrError := asMessage ;

            Synchronize(@SynchronizeError) ;
        end
        else begin
        {$ENDIF}
            FOnError(asMessage) ;
        {$IFDEF GUI_APPLICATION_SUPPORT}
        end ;
        {$ENDIF}
    end ;
end ;

{$IFDEF GUI_APPLICATION_SUPPORT}
//
// Synchronized Log
// Don't use directely. Use Log()
procedure TFtpMain.SynchronizeLog ;
begin
    FOnLog(psMessageLogOrError) ;
end ;

//
// Synchronized Error
// Don't use directely. Use Error()
procedure TFtpMain.SynchronizeError ;
begin
    FOnError(psMessageLogOrError) ;
end ;
{$ENDIF}

//
// Convert integer from config
//
// @param asKey key config
// @param aiDefaultValue default value if asValue = ''
// @param aiConvertedValue value converted
// @param asErrorMsg error message
//
// @return true if ok else false
function TFtpMain.ConvertInteger(const asKey : String; const aiDefaultValue : Integer;
    var aiConvertedValue : Integer; const asErrorMsg : String) : Boolean ;
var
    lsValue : String ;
begin
    Result := True ;

    lsValue := OnMainConfigRead(asKey) ;

    if lsValue = ''
    then begin
        aiConvertedValue := aiDefaultValue ;
    end
    else begin
        Result := TryIntParse(lsValue, aiConvertedValue) ;

        if Result = False
        then begin
            OnError(asErrorMsg) ;
        end ;
    end ;
end ;

//
// Get value
//
// @param asKey key config
// @param asDefaultValue default value if empty
//
// @return value
function TFtpMain.ConvertString(asKey : String; asDefaultValue : String) : String ;
var
    lsValue : String ;
begin
    lsValue := OnMainConfigRead(asKey) ;

    if lsValue = ''
    then begin
        Result := asDefaultValue ;
    end
    else begin
        Result := lsValue ;
    end ;
end ;

//
// Get value
//
// @param asKey key config
// @param abDefaultValue default value if empty
//
// @return value
function TFtpMain.ConvertBoolean(asKey : String; abDefaultValue : Boolean) : Boolean ;
var
    lsValue : String ;
begin
    lsValue := OnMainConfigRead(asKey) ;

    if lsValue = ''
    then begin
        Result := abDefaultValue ;
    end
    else begin
        Result := (lsValue = YES_VALUE) ;
    end ;
end ;

//
// Read configuration
//
// @return false if error occur
function TFtpMain.ReadConfig : Boolean ;
var
    // Value of config parameter
    lsValue : String ;
    // Integer value
    liValue : Integer ;
    liValue2 : Integer ;
begin
    Log(MSG_LOG_READ_CONFIG) ;

    // Delete Freepascal warning message
    liValue := 0 ;
    liValue2 := 0 ;

    Result := ConvertInteger(MAIN_CONF_PORT, DEFAULT_PORT, liValue,
        MSG_ERROR_PORT_VALUE) ;

    // 1 - Port
    if Result
    then begin
        piListenPort := liValue ;
    end ;

    // 2 - Ip Adress
    if Result
    then begin
        psListenHost := ConvertString(MAIN_CONF_IP_ADRESS, DEFAULT_IP_ADDRESS) ;
    end ;

    // 3 - Welcome
    if Result
    then begin
        lsValue := ConvertString(MAIN_CONF_WELCOME_MESSAGE, DEFAULT_WELCOME_MESSAGE) ;

        psWelcomeMessage := ConvertMessage(lsValue, MESSAGE_SEPARATOR, '220', FTP_EOL) ;
    end ;

    // 3 - Goodbye
    if Result
    then begin
        lsValue := ConvertString(MAIN_CONF_GOODBYE_MESSAGE, DEFAULT_GOODBYE_MESSAGE) ;

        psGoodbyMessage := ConvertMessage(lsValue, MESSAGE_SEPARATOR, '221', FTP_EOL) ;
    end ;

    // 4 - Max client
    if Result
    then begin
        Result := ConvertInteger(MAIN_CONF_MAX_CLIENT, DEFAULT_MAX_CLIENT,
            liValue, MSG_ERROR_MAX_CLIENT) ;

        if Result
        then begin
            piMaxClient := liValue ;
        end ;
    end ;

    // 4 - Max user per session
    if Result
    then begin
        Result := ConvertInteger(MAIN_CONF_MAX_SESSION_PER_USER, DEFAULT_MAX_SESSION_USER,
            liValue, MSG_ERROR_MAX_SESSION_USER) ;

        if Result
        then begin
            piMaxUser := liValue ;
        end ;
    end ;

    // 5 - Full log
    if Result
    then begin
        pbFullLog := ConvertBoolean(MAIN_CONF_FULL_LOG, DEFAULT_FULL_LOG) ;
    end ;

    // 6 - Time out
    if Result
    then begin
        Result := ConvertInteger(MAIN_CONF_TIME_OUT, DEFAULT_TIME_OUT,
            liValue, MSG_ERROR_MAX_SESSION_USER) ;

        if Result
        then begin
            piTimeOut := liValue ;
        end ;
    end ;

    // 7 - Passive port
    if Result
    then begin
        lsValue := ConvertString(MAIN_CONF_PASSIVE_PORT, DEFAULT_PASSIVE_PORT) ;

        Result := ConvertPassivePort(lsValue, liValue, liValue2) ;

        if Result
        then begin
            piPassivePortStart := liValue ;
            piPassivePortStop := liValue2 ;

            if (piPassivePortStart < 0) or (piPassivePortStart > 65535)
            then begin
                Result := False ;

                OnError(MSG_ERROR_START_PASSIVE_PORT) ;
            end ;

            if (piPassivePortStop < 0) or (piPassivePortStop > 65535)
            then begin
                Result := False ;

                OnError(MSG_ERROR_END_PASSIVE_PORT) ;
            end ;

            if (piPassivePortStop < piPassivePortStart)
            then begin
                Result := False ;

                OnError(MSG_ERROR_CHECK_PASSIVE_PORT) ;
            end ;
        end
        else begin
            OnError(MSG_ERROR_PASSIVE_PORT) ;
        end ;
    end ;

    // 8 - Deny priority
    if Result
    then begin
        pbDenyPriority := ConvertBoolean(MAIN_CONF_DENY_PRIORITY, DEFAULT_DENY_PRIORITY) ;
    end ;

    // 9 - Denied IP
    if Result
    then begin
        lsValue := ConvertString(MAIN_CONF_DENY_IP_ADRESS, DEFAULT_DENY_ADDRESS) ;

        poDeniedIP := StringToTStringList(lsValue, ',') ;
    end ;


    // 9 - Allow IP
    if Result
    then begin
        lsValue := ConvertString(MAIN_CONF_ALLOW_IP_ADRESS, DEFAULT_ALLOW_ADDRESS) ;

        poAllowedIP := StringToTStringList(lsValue, ',') ;
    end ;

    // 10 - buffer size
    if Result
    then begin
        Result := ConvertInteger(MAIN_CONF_BUFFER_SIZE, DEFAULT_BUFFER_SIZE,
            liValue, MSG_ERROR_BUFFER_SIZE) ;

        if Result
        then begin
            piBufferSize := liValue ;
        end ;
    end ;

    // 11 - user byte rate
    if Result
    then begin
        Result := ConvertInteger(MAIN_CONF_USER_BYTE_RATE, DEFAULT_USER_BYTE_RATE,
            liValue, MSG_ERROR_USER_BYTE_RATE) ;

        if Result
        then begin

            // Calculate block size and waiting time between two send
            if liValue = 0
            then begin
                piBlockSize := DEFAULT_BLOCK_SIZE ;
                piWaitingTime := 0 ;
            end
            else if liValue <= DEFAULT_BLOCK_SIZE
            then begin
                piBlockSize := liValue ;
                piWaitingTime := 1000 ; // 1 second
            end
            else begin
                piBlockSize := DEFAULT_BLOCK_SIZE ;
                piWaitingTime := DEFAULT_BLOCK_SIZE div liValue * 1000 ;
            end ;

            piUserByteRate := liValue ;
        end ;
    end ;

    if Result
    then begin
        pbUtf8Support := ConvertBoolean(MAIN_CONF_UTF8, DEFAULT_UTF8) ;
    end ;
end ;

// Add client
//
// @param aoClientFtp client to add
procedure TFtpMain.AddClient(const aoClientFtp : TFtpClient) ;
begin
    Inc(FClientCount) ;

    if poFirstFtpClient = nil
    then begin
        poFirstFtpClient := aoClientFtp ;

        poFirstFtpClient.NextClient := aoClientFtp ;
        poFirstFtpClient.PreviousClient := aoClientFtp ;
    end
    else begin
        poFirstFtpClient.PreviousClient.NextClient := aoClientFtp ;

        aoClientFtp.PreviousClient := poFirstFtpClient.PreviousClient ;

        aoClientFtp.NextClient := poFirstFtpClient ;

        poFirstFtpClient.PreviousClient := aoClientFtp ;
    end ;
end ;

// Delete client
//
// @param aoClientFtp
procedure TFtpMain.RemoveClient(const aoClientFtp : TFtpClient) ;
begin
    EnterCriticalsection(poLockClientCount) ;

    if FClientCount > 0
    then begin
        Dec(FClientCount) ;
    end ;

    if aoClientFtp = poFirstFtpClient
    then begin
        // This is the first client to stop
        if FClientCount > 0
        then begin
            // If not alone
            poFirstFtpClient.NextClient.PreviousClient :=
                poFirstFtpClient.PreviousClient ;

            poFirstFtpClient.PreviousClient.NextClient :=
                poFirstFtpClient.NextClient ;

            poFirstFtpClient := poFirstFtpClient.NextClient ;
        end
        else begin
            // This is the only client
            poFirstFtpClient := nil ;
        end ;
    end
    else begin
        aoClientFtp.PreviousClient.NextClient := aoClientFtp.NextClient ;
        aoClientFtp.NextClient.PreviousClient := aoClientFtp.PreviousClient ;
    end ;

    LeaveCriticalsection(poLockClientCount) ;
end ;

//
// Shutdown all client
procedure TFtpMain.RemoveAllClient ;
var
    // Current client
    loCurrentClient : TFtpClient ;
begin
    EnterCriticalsection(poLockClientCount) ;

    // parcourrir la liste
    // supprimer la methode de deconnexion
    // Terminate = true
    // Que se passe-t-il si un client termine
    if poFirstFtpClient <> nil
    then begin
       // Delete disconect procedure for no call
       poFirstFtpClient.OnClientDisconnect := nil ;

       poFirstFtpClient.Terminate ;

       loCurrentClient := poFirstFtpClient.NextClient ;

       while loCurrentClient <> poFirstFtpClient do
       begin
           // Delete disconect procedure for no call
           loCurrentClient.OnClientDisconnect := nil ;

           loCurrentClient.Terminate ;

           loCurrentClient := loCurrentClient.NextClient ;
       end ;

       poFirstFtpClient := nil ;

       FClientCount := 0 ;
    end ;

    LeaveCriticalsection(poLockClientCount) ;
end ;

//
// Create a new client
//
// @param loClientSock client socket
procedure TFtpMain.CreateNewClient(loClientSock : TTCPBlockSocket) ;
var
    // Client FTP
    loClientFtp : TFtpClient ;
begin
    EnterCriticalsection(poLockClientCount) ;

    // Check number of connection
    if (FClientCount < piMaxClient) or (piMaxClient = 0)
    then begin
        loClientFtp := TFtpClient.Create(True, loClientSock, psWelcomeMessage,
            psGoodbyMessage,piTimeOut, piMaxUser) ;

        // Add client to the list
        AddClient(loClientFtp) ;

        LeaveCriticalsection(poLockClientCount) ;

        loClientFtp.OnLog := FOnLog ;
        loClientFtp.OnError := FOnError ;
        loClientFtp.OnClientConfigRead := FOnClientConfigRead ;
        loClientFtp.OnClientDisconnect := @RemoveClient ;
        loClientFtp.OnLogin := @AddLogin ;
        loClientFtp.OnLogout := @RemoveLogin ;

        loClientFtp.FullLog := pbFullLog ;
        loClientFtp.Utf8Support := pbUtf8Support ;
        loClientFtp.FreeOnTerminate := True ;

        loClientFtp.Resume ;
    end
    else begin
        LeaveCriticalsection(poLockClientCount) ;

        SendString(loClientSock, MSG_FTP_TOO_MANY_USER) ;

        loClientSock.CloseSocket ;

        loClientSock.Free ;
    end ;
end ;

//
// Remove all login
procedure TFtpMain.RemoveAllLogin ;
var
    // Index of login count
    liIndexLogin : Integer ;
    // Pointer of integer
    lpInteger : PInteger ;
begin
    EnterCriticalsection(poLockLoginLogout) ;

    for liIndexLogin := 0 to poListLoginCount.Count - 1 do
    begin
        lpInteger := PInteger(poListLoginCount[liIndexLogin]) ;

        Dispose(lpInteger) ;
    end ;

    LeaveCriticalsection(poLockLoginLogout) ;
end ;

//
// Add login
//
// @param asLoginName login name
//
// @return true if can connected
function TFtpMain.AddLogin(const asLoginName : String) : Boolean ;
var
    // Current counter
    lpCurrentCounter : PInteger ;
    // login
    lsLogin : ShortString ;
begin
    if pbShutDownInProgress = False
    then begin
        lsLogin := Copy(asLoginName, 1, 255) ;

        EnterCriticalsection(poLockLoginLogout) ;

        lpCurrentCounter := poListLoginCount.Find(lsLogin) ;

        if lpCurrentCounter = nil
        then begin
            // First connction
            New(lpCurrentCounter) ;

            lpCurrentCounter^ := 0 ;

            poListLoginCount.Add(lsLogin, lpCurrentCounter) ;
        end ;

        Result := lpCurrentCounter^ < piMaxUser ;

        if Result
        then begin
            lpCurrentCounter^ := lpCurrentCounter^ + 1 ;

            if FOnLogin <> nil
            then begin
                {$IFDEF GUI_APPLICATION_SUPPORT}
                if FGuiApplication = True
                then begin
                    psLoginName := asLoginName ;
                    piLoginCount := lpCurrentCounter^ ;

                    Synchronize(@SynchronizeLogin);
                end
                else begin
                {$ENDIF}
                    FOnLogin(asLoginName, lpCurrentCounter^) ;
                {$IFDEF GUI_APPLICATION_SUPPORT}
                end;
                {$ENDIF}
            end ;
        end ;

        LeaveCriticalsection(poLockLoginLogout) ;
    end
    else begin
        Result := False ;
    end ;
end ;

//
// Remove login
//
// @param asLoginName login name
procedure TFtpMain.RemoveLogin(const asLoginName : String) ;
var
    // Current counter
    lpCurrentCounter : PInteger ;
    // login
    lsLogin : ShortString ;
begin
    if (asLoginName <> '') and (pbShutDownInProgress = False)
    then begin
        lsLogin := Copy(asLoginName, 1, 255) ;

        EnterCriticalsection(poLockLoginLogout) ;

        lpCurrentCounter := poListLoginCount.Find(lsLogin) ;

        if lpCurrentCounter <> nil
        then begin
            lpCurrentCounter^ := lpCurrentCounter^ - 1 ;

            // Must be in critical section for syncronize
            if (FOnLogout <> nil) and (lpCurrentCounter <> nil)
            then begin
                {$IFDEF GUI_APPLICATION_SUPPORT}
                if FGuiApplication = True
                then begin
                    psLoginName := asLoginName ;
                    piLoginCount := lpCurrentCounter^ ;

                    Synchronize(@SynchronizeLogout) ;
                end
                else begin
                {$ENDIF}
                    FOnLogout(asLoginName, lpCurrentCounter^) ;
                {$IFDEF GUI_APPLICATION_SUPPORT}
                end ;
                {$ENDIF}
            end ;

            if lpCurrentCounter^ = 0
            then begin
                Dispose(lpCurrentCounter) ;

                // Remove entry for free memory
                poListLoginCount.Delete(
                    poListLoginCount.FindIndexOf(lsLogin)
                    ) ;
            end ;
        end ;

        LeaveCriticalsection(poLockLoginLogout) ;
    end ;
end ;

// Synchronized login. Don't use directely, use AddLogin
procedure TFtpMain.SynchronizeLogin ;
begin
    FOnLogin(psLoginName, piLoginCount) ;
end ;

// Synchronized logout. Don't use directely, use AddLogin
procedure TFtpMain.SynchronizeLogout ;
begin
    FOnLogout(psLoginName, piLoginCount) ;
end ;

//
// Execute ftp server
procedure TFtpMain.Execute ;
begin
    // Read configuration
    FExitStatus := ReadConfig ;

    if FExitStatus
    then begin
        FRunning := True ;

        Run ;

        FRunning := False ;

        RemoveAllClient ;
    end ;

    Log(MSG_LOG_SHUTDOWN) ;
end ;

//
// Listen client connection
procedure TFtpMain.Run ;
var
    // Client socket
    loSock : TSocket ;
    loClientSock : TTCPBlockSocket ;
    // Server socket
    loServerSock : TTCPBlockSocket ;
    // Ip address are allowed
    lbAllowedIpAddress : Boolean ;
    // Ip address ar denied
    lbDeniedIpAddress : Boolean ;
    // Allow connection
    lbAllowConnection : Boolean ;
begin
    // Create server socket
    loServerSock := TTCPBlockSocket.Create ;

    try
        loServerSock.Bind(psListenHost, IntToStr(piListenPort)) ;
        loServerSock.SetLinger(LINGER_ENABLE, LINGER_DELAY);
        //loServerSock.NonBlockMode := True ;

        loServerSock.Listen ;

        Log(Format(MSG_LOG_SERVER_START, [loServerSock.GetLocalSinIP,
           piListenPort])) ;

        if loServerSock.LastError = 0
        then begin
            while (Terminated = False) and (pbShutDownInProgress = False) do
            begin
                // I don't understand why it doesn't work
                if loServerSock.CanRead(READ_CONNECTION)
                then begin
                    loSock := loServerSock.Accept ;

                    // If
                    if loServerSock.LastError = 0
                    then begin
                        loClientSock := TTCPBlockSocket.Create ;
                        loClientSock.Socket := loSock ;

                        // Check ip address
                        lbAllowedIpAddress :=
                            CheckIpAddressInList(loClientSock.GetRemoteSinIP, poAllowedIP) ;
                        lbDeniedIpAddress :=
                            CheckIpAddressInList(loClientSock.GetRemoteSinIP, poDeniedIP) ;

                        if pbDenyPriority
                        then begin
                            lbAllowConnection := lbAllowedIpAddress and (not lbDeniedIpAddress) ;
                        end
                        else begin
                            lbAllowConnection := lbAllowedIpAddress ;
                        end ;

                        if lbAllowConnection
                        then begin
                            // If connection allowed create client
                            CreateNewClient(loClientSock) ;
                        end
                        else begin
                            SendString(loClientSock, MSG_FTP_UNAUTHORIZED) ;
                        end ;
                    end ;
                end ;
            end ;
        end
        else begin
            Error(Format(MSG_ERROR_CANT_CREATE_SOCKET, [piListenPort])) ;
        end ;
    finally
        loServerSock.Free ;
    end ;
end ;


end.
