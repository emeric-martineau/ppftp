// This unit is the client
unit ftpclient;

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

{$I ftpconfig.inc}

uses
  Classes, SysUtils, dateutils, FtpTypes, FtpFunctions, FtpMessages, FtpConst
  , blcksock, synsock, FileUtil,
  {$IFDEF TYPE_SERVER_UNIX}
  ftpunixdirectory
  {$ELSE}
      {$IFDEF TYPE_SERVER_WINDOWS}
      ftpwindowsdirectory
      {$ENDIF}
  {$ENDIF}
  ;

type
  TFtpClient = class ;

  // Call when client disconnect
  //
  // @param aoFtpClient client to do call
  TClientDisconnect = procedure(const aoFtpClient: TFtpClient) of object ;

  // Mother class of FTP server
  TFtpClient = class(TThread)
  protected
    // Log
    FOnLog : TLogProcedure ;
    // Error
    FOnError : TLogProcedure;
    // Config reader
    FOnClientConfigRead : TClientConfigReaderFunction;
    // Previous thread
    FPreviousClient : TFtpClient;
    // Next thread
    FNextClient : TFtpClient;
    // Call when connection shutdown
    FOnClientDisconnect : TClientDisconnect;
    // Login
    FOnLogin : TClientLoginFunction ;
    // Logout
    FOnLogout : TClientLogoutProcedure ;
    // FullLog
    FFullLog : Boolean ;
    // Utf8 support
    FUtf8Support : Boolean ;
    {$IFDEF GUI_APPLICATION_SUPPORT}
    // Gui application for non-freeze log in console mode
    FGuiApplication : Boolean ;
    {$ENDIF}
    // If local config exists
    FOnLocalConfigExists : TFolderLocalConfigExistsFunction ;
    // Get passive port
    FOnGetPassivePort : TGetPassivePortFunction ;
    // Free passive port
    FOnFreePassivePort : TFreePassivePortProcedure ;
    // Check password
    FOnCheckPassword : TClientCheckPasswordFunction ;
    // File protected
    FOnFileProtected : TFileProtectedFunction ;
    // Main byte rate
    FMainByteRate : Integer ;
    // Buffer size
    FBufferSize : Integer ;
    // Transfert
    FOnTransfert : TTransfertProcedure ;
    // Cancel
    FCancel : Boolean ;

    // Command socket
    poClientSock : TTCPBlockSocket;
    // Welcome message
    psWelcomeMessage : String;
    // Goodbye message
    psGoodbyeMessage : String;
    // Time out in second
    piTimeOut : Integer ;
    // Config
    prUserConfig : TFtpClientConfig ;
    // Maximum login
    piMaximumLogin : Integer ;
    // Utf8 mode enabled
    pbUtf8 : Boolean ;
    // File transfert timeout
    piFileTransfertTimeOut : Integer ;
    // Byte rate for text file
    piByteRate : Integer ;

    // Message to show
    psMessageLogOrError : String ;
    // Current directory
    psFtpCurrentDirectory : String ;
    // If passive mode
    pbPassiveMode : Boolean ;
    // Active ip addresse
    psActiveModeIpAddresse : String ;
    // Active mode port
    psActiveModePort : String ;
    // Passive socket
    poPassivePortSock : TTCPBlockSocket ;
    // Passive port
    prPassivePort : PFtpPassivePort ;
    // Binary transfert
    pbTransfertMode : TTransfertMode ;
    // Rest value
    piStartTransfertFileValue : Integer ;
    // Waiting transfert time
    piWaitingTransfertTime : Integer ;
    // Buffer size
    piUserBufferSize : Integer ;
    // Last file transfert
    prLastFileTransfert : TDateTime ;

    // Return True if can login
    function CheckLogin(const asLoginName : String) : Boolean ;
    // Logout
    procedure Logout(const asLoginName : String) ;
    // Call FOnLog if set
    procedure Log(const asMessage : String) ;
    {$IFDEF GUI_APPLICATION_SUPPORT}
    // Synchronized Log. Don't use directely. Use Log()
    procedure SynchronizeLog ;
    // Synchronized Error. Don't use directely. Use Error()
    procedure SynchronizeError ;
    {$ENDIF}
    // Call FOnError if set
    procedure Error(const asMessage : String) ;
    // Get config
    procedure ReadConfig(const asLoginName : String) ;
    // Read remote string
    function ReadRemoteString(var asString : String) : Integer ;
    // Split user string to command and parameter
    procedure ExplodeCommand(const asString : String; var asCmd : String;
        var asParameter : String) ;
    // Logout
    procedure Logout ;
    // Default user
    procedure InitDefaultUser ;
    // Send answer to the client
    procedure SendAnswer(const asString : String) ;
    // Valide user login and password
    procedure ValideUserPassword(const asPassword : String) ;
    // Read local folder config
    function FolderLocalConfigReader(const asFolderName : String;
        const asKey : String; var asValue : String) : Boolean ;
    // Execute user command
    procedure ExecuteUserCommand(const asCommand : String;
        const asParameter : String) ;
    // Connect to data socket
    function GetDataSocket : TTCPBlockSocket ;
    // Get passive port
    function GetPassivePort : PFtpPassivePort ;
    // Return passive port
    procedure FreePassivePort(const asPort : PFtpPassivePort) ;
    // Check password
    function CheckPassword(const asLoginName : String;
        const asPassword : String) : Boolean ;
    // Close and free passive port socket and port
    procedure ClosePassiveSocket ;
    // Check if directory is readable
    function CanReadDirectory(const asRoot : String;
        const asCurrentPath : String) : Boolean ;
    // List a directory
    procedure ListDirectory(const asFtpFolderName : String;
        const aoDataSocket : TTCPBlockSocket; const abNlst : Boolean) ;
    // File protected
    function IsFileProtected(const asPathAndFileName : String) : Boolean ;
    // Close data connection
    procedure CloseDataConnection(aoDataSocket : TTCPBlockSocket) ;
    // Check if client abord transfert
    function IsAborted : Boolean ;
    // Update waiting time for transfert
    procedure UpdateWaitingTransfertTime ;
    {$IFDEF CALL_BACK_TRANSFERT}
    // Transfert call back
    procedure SetTransfert(const asFileName : String;
        const asDownload : Boolean; const asStart : Boolean) ;
    {$ENDIF}

    // FEAT command
    procedure FeatCommand ;
    // UTF8 command
    procedure Utf8Command(const asParameter : String) ;
    // PWD command
    procedure PwdCommand ;
    // CWD/CDUP/XCDUP command
    procedure ChangeDirectoryCommand(const asParameter : String;
        const abCDUP : Boolean) ;
    // Port feature
    procedure PortCommand(const asParameter : String) ;
    // PASV command
    procedure PasvCommand ;
    // LIST/NLST feature
    procedure ListCommand(const asParameter : String; const abNlst : Boolean) ;
    // TYPE command
    procedure TypeCommand(const asParameter : String) ;
    // MODE feature
    procedure ModeCommand(const asParameter : String) ;
    // REST feature
    procedure RestCommand(const asParameter : String) ;
    // RETR feature
    procedure RetrCommand(const asParameter : String) ;
    // STOR/STOU/APPE feature
    procedure StorStouAppeCommand(const asParameter : String;
        const atMode : TStoreCommandeMode) ;

  public
    // Previous client
    property PreviousClient: TFtpClient read FPreviousClient write FPreviousClient;
    // Next client
    property NextClient: TFtpClient read FNextClient write FNextClient;
    // Full log
    property FullLog : Boolean read FFullLog write FFullLog ;
    // Utf8 support
    property Utf8Support : Boolean read FUtf8Support write FUtf8Support ;
    {$IFDEF GUI_APPLICATION_SUPPORT}
    // Gui application for non-freeze log in console mode. Set define GUI_APPLICATION_SUPPORT in config.inc
    property GuiApplication : Boolean read FGuiApplication write FGuiApplication ;
    {$ENDIF}
    // Main byte rate
    property MainByteRate : Integer read FMainByteRate write FMainByteRate ;
    // Buffer size
    property BufferSize : Integer read FBufferSize write FBufferSize ;
    // Cancel transfert file
    property Cancel : Boolean write FCancel ;

    // Log
    property OnLog: TLogProcedure read FOnLog write FOnLog;
    // Error
    property OnError: TLogProcedure read FOnError write FOnError;
    // Client config reader
    property OnClientConfigRead: TClientConfigReaderFunction
      read FOnClientConfigRead write FOnClientConfigRead;
    // Call when connection shutdown
    property OnClientDisconnect: TClientDisconnect
      read FOnClientDisconnect write FOnClientDisconnect;
    // Login
    property OnLogin : TClientLoginFunction read FOnLogin write FOnLogin ;
    // Logout
    property OnLogout : TClientLogoutProcedure read FOnLogout write FOnLogout ;
    // Local folder config
    property OnLocalConfigExists : TFolderLocalConfigExistsFunction read
        FOnLocalConfigExists write FOnLocalConfigExists ;
    // Get passive port
    property OnGetPassivePort : TGetPassivePortFunction read FOnGetPassivePort
        write FOnGetPassivePort ;
    // Free passive port
    property OnFreePassivePort : TFreePassivePortProcedure read
        FOnFreePassivePort write FOnFreePassivePort ;
    // Check password
    property OnCheckPassword : TClientCheckPasswordFunction read
        FOnCheckPassword write FOnCheckPassword ;
    // If file protected
    property OnFileProtected : TFileProtectedFunction read FOnFileProtected
        write FOnFileProtected ;
    {$IFDEF CALL_BACK_TRANSFERT}
    // Transfert
    property OnTransfert : TTransfertProcedure read FOnTransfert
        write FOnTransfert ;
    {$ENDIF}

    // Constructor
    constructor Create(const abCreateSuspended: boolean;
      const aoClientSock: TTCPBlockSocket; const asWelcomeMessage: String;
      const asGoodbyeMessage: String; const aiTimeOut : Integer;
      const aiMaxLogin : Integer; const aiFileTransfertTimeOut : Integer);
    // Destructor
    destructor Destroy; override;
    // Execute main thread
    procedure Execute; override;
  end;

const
    // ReadRemoteString() result function. We have read a string
    READ_REMOTE_STRING_OK : Integer = 0 ;
    // ReadRemoteString() result function. We have time out
    READ_REMOTE_STRING_TIME_OUT : Integer = 1 ;
    // ReadRemoteString() result function. Thread must be terminated
    READ_REMOTE_STRING_TERMINATED : Integer = 2 ;
    // ReadRemoteString() result function. Error
    READ_REMOTE_STRING_ERROR : Integer = 3 ;
    // ReadRemoteString() result function. We have file time out
    READ_REMOTE_STRING_FILE_TIME_OUT : Integer = 4 ;
implementation

//
// Constructor
//
// @param abCreateSuspended if start supend thread
// @param aoClientSock socket for communication
// @param asWelcomeMessage welcome message (must be end by FTP_EOL)
// @param asGoodbyeMessage goodbye message (must be end by FTP_EOL)
// @param aiTimeOut time out delay in second
// @param aiMaximumLogin maximum login per user
// @param aiFileTransfertTimeOut file transfert time out
constructor TFtpClient.Create(const abCreateSuspended: boolean;
  const aoClientSock: TTCPBlockSocket; const asWelcomeMessage: String;
  const asGoodbyeMessage: String; const aiTimeOut : Integer;
  const aiMaxLogin : Integer; const aiFileTransfertTimeOut : Integer) ;
begin
    inherited Create(abCreateSuspended);

    FreeOnTerminate := True;

    poClientSock := aoClientSock;
    // Accept CR+LF, LF, CR
    poClientSock.ConvertLineEnd := True ;

    psWelcomeMessage := asWelcomeMessage;

    psGoodbyeMessage := asGoodbyeMessage;

    piTimeOut := aiTimeOut ;

    piMaximumLogin := aiMaxLogin ;

    pbUtf8 := False ;

    piFileTransfertTimeOut := aiFileTransfertTimeOut ;

    pbPassiveMode := False ;

    psActiveModeIpAddresse := '' ;

    psActiveModePort := '' ;

    FUtf8Support := False ;

    FOnClientDisconnect := nil;

    FOnLog := nil;

    FOnError := nil;

    FOnClientConfigRead := nil;

    FOnGetPassivePort := nil ;

    FOnCheckPassword := nil ;

    FOnFileProtected := nil ;

    FOnTransfert := nil ;

    FMainByteRate := 0 ;

    FBufferSize := DEFAULT_BUFFER_SIZE ;

    poPassivePortSock := nil ;

    prPassivePort := nil ;

    pbTransfertMode := tmAscii ;

    FCancel := False ;

    InitDefaultUser ;
end;

//
// Check if can login (if no max user)
//
// @param asLoginName login name
function TFtpClient.CheckLogin(const asLoginName : String) : Boolean ;
begin
    Result := True ;

    if Assigned(FOnLogin)
    then begin
        Result := FOnLogin(asLoginName) ;
    end ;
end ;

//
// Logout
//
// @param asLoginName login name
procedure TFtpClient.Logout(const asLoginName : String) ;
begin
    if Assigned(FOnLogout)
    then begin
        FOnLogout(asLoginName) ;
    end ;
end ;

//
// Destructor
destructor TFtpClient.Destroy;
begin
    inherited Destroy;
end;

//
// Default user
procedure TFtpClient.InitDefaultUser ;
begin
    prUserConfig.Root := DEFAULT_USER_ROOT ;
    prUserConfig.Download := DEFAULT_USER_DOWNLOAD = YES_VALUE ;
    prUserConfig.Upload := DEFAULT_USER_UPLOAD = YES_VALUE ;
    prUserConfig.Rename := DEFAULT_USER_RENAME = YES_VALUE ;
    prUserConfig.Delete := DEFAULT_USER_DELETE = YES_VALUE ;
    prUserConfig.MakeDirectory := DEFAULT_USER_MAKE_DIRECTORY = YES_VALUE ;
    prUserConfig.DeleteDirectory := DEFAULT_USER_DELETE_DIRECTORY = YES_VALUE ;
    prUserConfig.SubDir := DEFAULT_USER_SUB_DIR = YES_VALUE ;
    prUserConfig.Disabled := DEFAULT_USER_DISABLED = YES_VALUE ;
    prUserConfig.UserFound := False ;
    prUserConfig.Connected := False ;
end ;

//
// Get config
//
// @param asLoginName login name
procedure TFtpClient.ReadConfig(const asLoginName : String) ;
var
    lrUserConfig : TUserConfig ;
    liByteRate : Integer ;
begin
    if Assigned(FOnClientConfigRead)
    then begin
        lrUserConfig := FOnClientConfigRead(asLoginName) ;

        prUserConfig.Root := AddTrailing(lrUserConfig.Root, DirectorySeparator) ;
        prUserConfig.Download := lrUserConfig.Download = YES_VALUE ;
        prUserConfig.Upload := lrUserConfig.Upload = YES_VALUE ;
        prUserConfig.Rename := lrUserConfig.Rename = YES_VALUE ;
        prUserConfig.Delete := lrUserConfig.Delete = YES_VALUE ;
        prUserConfig.MakeDirectory := lrUserConfig.MakeDirectory = YES_VALUE ;
        prUserConfig.DeleteDirectory := lrUserConfig.DeleteDirectory = YES_VALUE ;
        prUserConfig.SubDir := lrUserConfig.SubDir = YES_VALUE ;
        prUserConfig.Disabled := lrUserConfig.Disabled = YES_VALUE ;

        liByteRate := 0 ;

        if not TryIntParse(lrUserConfig.ByteRate, liByteRate)
        then begin
            liByteRate := 0 ;
        end ;

        prUserConfig.ByteRate := liByteRate ;
        prUserConfig.UserFound := lrUserConfig.UserFound ;
        prUserConfig.Connected := False ;
    end
    else begin
        InitDefaultUser ;
    end;

    prUserConfig.Login := asLoginName ;
end ;

//
// Read client string
//
// @param asString string from client
//
// @return READ_REMOTE_STRING_OK, READ_REMOTE_STRING_TIME_OUT, READ_REMOTE_STRING_TERMINATED, READ_REMOTE_STRING_ERROR
//
// @seealso(READ_REMOTE_STRING_OK)
// @seealso(READ_REMOTE_STRING_TIME_OUT)
// @seealso(READ_REMOTE_STRING_TERMINATED)
// @seealso(READ_REMOTE_STRING_ERROR)
function TFtpClient.ReadRemoteString(var asString : String) : Integer ;
var
    // Local time out
    liTimeOut : Integer ;
    // String for log
    lsStringForLog : String ;
begin
    if piTimeOut > 0
    then begin
        liTimeOut := piTimeOut ;
    end
    else begin
        liTimeOut := 1 ;
    end ;

    Result := READ_REMOTE_STRING_TIME_OUT ;

    while (liTimeOut > 0) and (Terminated = False) do
    begin
        asString := poClientSock.RecvString(WAITING_FOR_INPUT) ;

        if poClientSock.LastError = 0
        then begin
            Result := READ_REMOTE_STRING_OK ;

            if FFullLog
            then begin
                lsStringForLog := asString ;

                if UpperCase(Copy(asString, 1, 4)) = 'PASS'
                then begin
                    lsStringForLog := Copy(asString, 1, 4) + ' ****' ;
                end ;

                Log(Format(MSG_LOG_COMMAND,
                    [poClientSock.GetRemoteSinIP, lsStringForLog])) ;
            end ;

            break ;
        end
        else if poClientSock.LastError <> WSAETIMEDOUT
        then begin
            Error(poClientSock.LastErrorDesc) ;

            Result := READ_REMOTE_STRING_ERROR ;

            break ;
        end ;

        if (piFileTransfertTimeOut > 0) and
            (SecondsBetween(prLastFileTransfert, Now) > piFileTransfertTimeOut)
        then begin
            Result := READ_REMOTE_STRING_FILE_TIME_OUT ;

            break ;
        end ;

        if piTimeOut > 0
        then begin
            Dec(liTimeOut) ;
        end ;
    end ;

    if Terminated = True
    then begin
        Result := READ_REMOTE_STRING_TERMINATED ;
    end;
end ;

//
// Split user string to command and parameter
//
// @param asString string to split
// @param asCmd first word
// @param asParameter rest of string
procedure TFtpClient.ExplodeCommand(const asString : String; var asCmd : String;
    var asParameter : String) ;
var
    // Length of string
    liLengthString : Integer ;
    // Index of string
    liIndexString : Integer ;
begin
    asCmd := '' ;
    asParameter := '' ;

    liLengthString := Length(asString) ;

    liIndexString := 1 ;

    // 1 - Skip first blacnk
    while (liIndexString <= liLengthString) and
        ((asString[liIndexString] = ' ') or (asString[liIndexString] = #9)) do
    begin
        Inc(liIndexString) ;
    end ;

    // 2 - Get command
    while (liIndexString <= liLengthString) and
        (asString[liIndexString] <> ' ') and (asString[liIndexString] <> #9) do
    begin
        asCmd := asCmd + asString[liIndexString] ;

        Inc(liIndexString) ;
    end ;

    // 3 - Get parameter
    asParameter := Copy(asString, liIndexString, liLengthString) ;

    asParameter := Trim(asParameter) ;
end ;

//
// Logout
procedure TFtpClient.Logout ;
begin
    // Send logout to the parent
    if Assigned(FOnLogout) and prUserConfig.Connected
    then begin
        FOnLogout(prUserConfig.Login) ;
    end ;

    prUserConfig.Connected := False ;
end ;

//
// Call Log procedure with synchronize
//
// @param asMessage message to display
procedure TFtpClient.Log(const asMessage : String) ;
begin
    if (asMessage <> '') and Assigned(FOnLog)
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
procedure TFtpClient.Error(const asMessage : String) ;
begin
    if (asMessage <> '') and Assigned(FOnError)
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
procedure TFtpClient.SynchronizeLog ;
begin
    FOnLog(psMessageLogOrError) ;
end ;

//
// Synchronized Error
// Don't use directely. Use Error()
procedure TFtpClient.SynchronizeError ;
begin
    FOnError(psMessageLogOrError) ;
end ;
{$ENDIF}

//
// Send answer to the client
//
// @param asString string to send
procedure TFtpClient.SendAnswer(const asString : String) ;
begin
    SendString(poClientSock, asString) ;

    if FFullLog
    then begin
        Log(Format(MSG_LOG_COMMAND, [poClientSock.GetRemoteSinIP,
            asString])) ;
    end ;
end ;

//
// Valide user login and password
//
// @param asPassword password
procedure TFtpClient.ValideUserPassword(const asPassword : String) ;
begin
    if CheckLogin(prUserConfig.Login)
    then begin
        if prUserConfig.Login = 'anonymous'
        then begin
            // Not password requiere for anonymous
            Log(Format(MSG_LOG_ANONYMOUS_PASSWORD,
                [poClientSock.GetRemoteSinIP,
                asPassword])) ;

            prUserConfig.Connected :=
                (prUserConfig.UserFound = True) and
                (prUserConfig.Disabled = False) ;
        end
        else begin
            // Check password
            prUserConfig.Connected :=
                (prUserConfig.UserFound = True) and
                (prUserConfig.Disabled = False) and
                CheckPassword(prUserConfig.Login, asPassword) ;
        end ;

        // If not connected
        if not prUserConfig.Connected
        then begin
            SendAnswer(MSG_FTP_LOGIN_INCORRECT) ;
        end
        else begin
            // Check home directory exists
            if DirectoryExists(prUserConfig.Root)
            then begin
                SendAnswer(Format(MSG_FTP_ROOT_LOGGED, [prUserConfig.Login])) ;
            end
            else begin
                SendAnswer(MSG_FTP_ROOT_NOT_EXISTS) ;

                OnError(Format(MSG_ERROR_ROOT_USER_NOT_FOUND,
                    [prUserConfig.Login])) ;

                prUserConfig.Connected := False ;
            end ;
        end ;
    end
    else begin
        SendAnswer(Format(MSG_FTP_MAXIMUM_LOGIN, [piMaximumLogin])) ;
    end ;
end ;

//
// Read local folder config
//
// @param asFolderName name of folder
// @param abUtf8 True if utf8 folder name
// @param asKey keyname of local config to read
// @param asValue value of key
//
// @return True if local config found
function TFtpClient.FolderLocalConfigReader(const asFolderName : String;
        const asKey : String; var asValue : String) : Boolean ;
begin
    if Assigned(FOnLocalConfigExists)
    then begin
        Result := FOnLocalConfigExists(asFolderName, pbUtf8, asKey,
            asValue) ;
    end
    else begin
        Result := False ;
    end ;
end;

//
// Connect to data socket
//
// @return socket or nil if any error
function TFtpClient.GetDataSocket : TTCPBlockSocket ;
var
    // Client socket
    loSock : TSocket ;
begin
    Result := nil ;

    if pbPassiveMode
    then begin
        if poPassivePortSock.CanRead(READ_CONNECTION)
        then begin
            loSock := poPassivePortSock.Accept ;

            if poPassivePortSock.LastError = 0
            then begin
                Result := TTCPBlockSocket.Create ;
                Result.Socket := loSock ;
                Result.SetLinger(LINGER_ENABLE, LINGER_DELAY) ;
                Result.ConvertLineEnd := True ;
            end ;
        end ;

        // If any error close passive socket
        ClosePassiveSocket ;
    end
    else begin
        Result := TTCPBlockSocket.Create ;

        Result.SetLinger(LINGER_ENABLE_CLIENT, LINGER_DELAY_CLIENT) ;
        // If not good ip or port, wait time out define by set linger
        Result.Connect(psActiveModeIpAddresse, psActiveModePort) ;

        if Result.LastError <> 0
        then begin
            FreeAndNil(Result) ;
        end
        else begin
            Result.SetLinger(LINGER_ENABLE, LINGER_DELAY) ;
        end ;
    end ;

    // If no socket
    if Result = nil
    then begin
        SendAnswer(MSG_FTP_DATA_CONNECTION_FAIL) ;
    end
    else if poClientSock.GetRemoteSinIP <> Result.GetRemoteSinIP
    then begin
        // No same IP address
        Result.CloseSocket ;

        Result.Free ;

        Result := nil ;

        SendAnswer(MSG_FTP_DATA_CONNECTION_IP_FAIL) ;
    end ;
end ;

//
// Return passive port
//
// @return free port
function TFtpClient.GetPassivePort : PFtpPassivePort ;
begin
    Result := nil ;

    if Assigned(FOnGetPassivePort)
    then begin
        Result := FOnGetPassivePort() ;
    end;
end ;

//
// Free passive port
//
// @param asPort passive port
procedure TFtpClient.FreePassivePort(const asPort : PFtpPassivePort) ;
begin
    if Assigned(FOnFreePassivePort)
    then begin
        FOnFreePassivePort(asPort) ;
    end;
end;

// Check password
//
// @param asLoginName login name
// @param asPassword password
//
// @return true if ok
function TFtpClient.CheckPassword(const asLoginName : String;
    const asPassword : String) : Boolean ;
begin
    Result := False ;

    if Assigned(FOnCheckPassword)
    then begin
        Result := FOnCheckPassword(asLoginName, asPassword) ;
    end ;
end ;

//
// Close and free passive port socket and port
procedure TFtpClient.ClosePassiveSocket ;
begin
    // Passive socket
    FreeAndNil(poPassivePortSock) ;

    // Passive port
    FreePassivePort(prPassivePort) ;

    prPassivePort := nil ;

    pbPassiveMode := False ;
end;

//
// Return true if can read. asCurrentPath must be start by asRoot
//
// @param asRoot user root dir
// @param asCurrentPath path to read
function TFtpClient.CanReadDirectory(const asRoot : String;
    const asCurrentPath : String) : Boolean ;
var
    // local config
    lsLocalConfigValue : String ;
    // root dir
    lsRoot : String ;
    // current path
    lsCurrentPath : String ;
    // length of root
    liLengthRoot : Integer ;
begin
    // Check if local config exist and if we can go
    lsLocalConfigValue := NO_VALUE ;
    Result := True ;

    // Add directory separator to be sur we have same or higher size
    lsRoot := AddTrailing(asRoot, DirectorySeparator) ;
    lsCurrentPath := AddTrailing(asCurrentPath, DirectorySeparator) ;

    // Now, we cut start
    liLengthRoot := Length(lsRoot) ;

    // +1 for end directory separator of lsRoot
    // -1 for end directory separator of lsCurrentPath
    lsCurrentPath := Copy(lsCurrentPath, liLengthRoot + 1,
        Length(lsCurrentPath) - liLengthRoot - 1) ;

    while Result do
    begin
        if FolderLocalConfigReader(lsRoot + lsCurrentPath,
             FOLDER_CONF_DISABLED, lsLocalConfigValue)
        then begin
            Result := lsLocalConfigValue <> NO_VALUE ;
        end ;

        if Length(lsCurrentPath) = 0
        then begin
            break ;
        end ;

        lsCurrentPath := ExtractFileDir(lsCurrentPath) ;
    end ;
end ;

// Close data connection
//
// @param aoDataSocket : data socket
procedure TFtpClient.CloseDataConnection(aoDataSocket : TTCPBlockSocket) ;
begin
    aoDataSocket.CloseSocket ;
    aoDataSocket.Free ;
end ;

// List a directory
procedure TFtpClient.ListDirectory(const asFtpFolderName : String;
    const aoDataSocket : TTCPBlockSocket; const abNlst : Boolean) ;
{$I ftplistdirectory.inc}

// If file is protected and therefore we say doesn't exists
//
// @param asPathAndFileName complete file name with path
// @param asUtf8 utf8 mode enabled
function TFtpClient.IsFileProtected(const asPathAndFileName : String) : Boolean ;
begin
    Result := False ;

    if Assigned(FOnFileProtected)
    then begin
        Result := FOnFileProtected(asPathAndFileName, pbUtf8) ;
    end ;
end ;

//
// Check if client abord transfert
//
// @return True or False
function TFtpClient.IsAborted : Boolean ;
var
    // String for log
    lsString : String ;
    // Paramete
    lsParameter : String ;
    // Commande
    lsCommande : String ;
begin
    Result := FCancel ;

    FCancel := False ;

    if not Result
    then begin
        lsString := poClientSock.RecvString(0) ;

        if poClientSock.LastError = 0
        then begin
            lsParameter := '' ;
            lsCommande := '' ;

            ExplodeCommand(lsString, lsCommande, lsParameter) ;

            Result := UpperCase(lsCommande) = 'ABOR' ;

            if Result
            then begin
                SendAnswer(MSG_FTP_ABOR_OK) ;
            end
            else begin
                SendAnswer(MSG_FTP_ABOR_ONLY) ;
            end ;

            if FFullLog
            then begin
                if UpperCase(Copy(lsString, 1, 4)) = 'PASS'
                then begin
                    lsString := Copy(lsString, 1, 4) + ' ****' ;
                end ;

                Log(Format(MSG_LOG_COMMAND,
                    [poClientSock.GetRemoteSinIP, lsString])) ;
            end ;
        end ;
    end ;
end ;

//
// Update waiting time and buffer size
procedure TFtpClient.UpdateWaitingTransfertTime ;
begin
    if prUserConfig.ByteRate = -1
    then begin
        piByteRate := FMainByteRate ;
    end
    else begin
        piByteRate := prUserConfig.ByteRate ;
    end ;

    if piByteRate = 0
    then begin
        piWaitingTransfertTime := 0 ;
        piUserBufferSize := FBufferSize ;
    end
    else begin
        if piByteRate < FBufferSize
        then begin
            piWaitingTransfertTime := 1000 ;
            piUserBufferSize := piByteRate ;
        end
        else begin
            piWaitingTransfertTime := FBufferSize * 1000 div piByteRate ;
            piUserBufferSize := FBufferSize ;
        end ;
    end ;
end ;

{$IFDEF CALL_BACK_TRANSFERT}
//
// Transfert call back
//
// @param asFileName filename
// @param asDownload true if download, false if upload
// @param asStart true start, false stop
procedure TFtpClient.SetTransfert(const asFileName : String;
    const asDownload : Boolean; const asStart : Boolean) ;
begin
    if Assigned(FOnTransfert)
    then begin
        FOnTransfert(prUserConfig.Login, asFileName, asDownload, asStart,
            Self) ;
    end ;
end ;
{$ENDIF}

//
// Execute ftp server
procedure TFtpClient.Execute;
var
    // Remote sting
    lsRemoteString : String ;
    // Command
    lsCommand : String ;
    // Parameter
    lsParameter : String ;
    // Quit
    lbQuit : Boolean ;
    // ReadRemoteString result
    liReadRemoteStringResult : Integer ;
begin
    Log(Format(MSG_LOG_NEW_CONNECTION,
        [poClientSock.GetRemoteSinIP])) ;

    // Send welcome message
    poClientSock.SendString(psWelcomeMessage);

    lbQuit := False ;

    lsRemoteString := '' ;
    lsParameter := '' ;
    lsCommand := '' ;

    prLastFileTransfert := Now ;

    while (Terminated = False) and (lbQuit = False) do
    begin
        liReadRemoteStringResult := ReadRemoteString(lsRemoteString) ;

        // If can read
        if (liReadRemoteStringResult = READ_REMOTE_STRING_OK) and
            (Terminated = False)
        then begin
            // Split user string into command and parametter
            ExplodeCommand(lsRemoteString, lsCommand, lsParameter) ;

            lsCommand := UpperCase(lsCommand) ;

            if lsCommand = 'QUIT'
            then begin
                poClientSock.SendString(psGoodbyeMessage) ;

                lbQuit := True
            end
            else if lsCommand = 'USER'
            then begin
                // Logout if already connected
                Logout ;

                // Login name
                prUserConfig.Login := LowerCase(lsParameter) ;

                // Read configuration
                ReadConfig(prUserConfig.Login) ;

                if (prUserConfig.Login = 'anonymous')
                then begin
                    if prUserConfig.UserFound and not prUserConfig.Disabled
                    then begin
                        SendAnswer(MSG_FTP_ANONYMOUS_ALLOWED) ;
                    end
                    else begin
                        SendAnswer(MSG_FTP_ANONYMOUS_NOT_ALLOWED) ;
                    end;
                end
                else begin
                    SendAnswer(Format(MSG_FTP_SEND_PASSWORD, [prUserConfig.Login])) ;
                end;
            end
            // TODO HELP command
            // TODO REIN InitDefaultUser + psCurrentDirectory
            else if prUserConfig.Login = ''
            then begin
                // Login not send
                SendAnswer(MSG_FTP_LOGIN_FIRST) ;
            end
            else if lsCommand = 'PASS'
            then begin
                ValideUserPassword(lsParameter) ;

                if prUserConfig.Connected
                then begin
                    psFtpCurrentDirectory := '/' ;

                    UpdateWaitingTransfertTime ;
                end ;
            end
            else if not prUserConfig.Connected
            then begin
                SendAnswer(MSG_FTP_PASSWORD_REQUIERED) ;
            end
            else if prUserConfig.Connected = True
            then begin
                // Procedure for all command
                ExecuteUserCommand(lsCommand, lsParameter) ;
            end ;
        end
        else begin
            if liReadRemoteStringResult = READ_REMOTE_STRING_TIME_OUT
            then begin
                SendAnswer(Format(MSG_FTP_TIME_OUT, [piTimeOut])) ;
            end
            else if liReadRemoteStringResult = READ_REMOTE_STRING_FILE_TIME_OUT
            then begin
                SendAnswer(Format(MSG_FTP_FILE_TIME_OUT, [piFileTransfertTimeOut])) ;
            end ;

            break ;
        end ;
    end ;

    OnLog(Format(MSG_LOG_CONNECTION_CLOSE,
        [poClientSock.GetRemoteSinIP])) ;

    poClientSock.CloseSocket;

    poClientSock.Free;

    ClosePassiveSocket ;

    Logout ;

    // Send disconnect to the parent
    if Assigned(FOnClientDisconnect)
    then begin
      FOnClientDisconnect(Self) ;
    end ;
end ;

//
// Execute user command
//
// @param asCommand user command
// @param asParameter user parameter
procedure TFtpClient.ExecuteUserCommand(const asCommand : String;
    const asParameter : String) ;
var
    // Command
    lsCommand : String ;
    // Parameter
    lsParameter : String ;
begin
    if (asCommand = 'NOP') or (asCommand = 'NOOP')
    then begin
        SendAnswer(MSG_FTP_NOOP) ;
    end
    else if asCommand = 'SYST'
    then begin
        SendAnswer(MSG_FTP_SYST + FTP_SERVER_TYPE) ;
    end
    else if asCommand = 'FEAT'
    then begin
        FeatCommand ;
    end
    else if asCommand = 'OPTS'
    then begin
        lsCommand := '' ;
        lsParameter := '' ;

        ExplodeCommand(asParameter, lsCommand, lsParameter) ;

        if (lsCommand = 'UTF8') and Utf8Support
        then begin
            Utf8Command(lsParameter) ;
        end
        else begin
            SendAnswer(MSG_FTP_CMD_NOT_UNDERSTOOD) ;
        end ;
    end
    else if (asCommand = 'PWD') or (asCommand = 'XPWD')
    then begin
        PwdCommand ;
    end
    else if asCommand = 'CWD'
    then begin
        ChangeDirectoryCommand(asParameter, False) ;
    end
    else if (asCommand = 'CDUP') or (asCommand = 'XCUP')
    then begin
        ChangeDirectoryCommand(asParameter, True) ;
    end
    else if (asCommand = 'PORT')
    then begin
        PortCommand(asParameter) ;
    end
    else if asCommand = 'PASV'
    then begin
        PasvCommand ;
    end
    else if (asCommand = 'LIST')
    then begin
        ListCommand(asParameter, false) ;
    end
    else if (asCommand = 'NLST')
    then begin
        ListCommand(asParameter, true) ;
    end
    else if (asCommand = 'TYPE')
    then begin
        TypeCommand(asParameter) ;
    end
    else if (asCommand = 'MODE')
    then begin
        ModeCommand(asParameter) ;
    end
    else if (asCommand = 'REST')
    then begin
        RestCommand(asParameter) ;
    end
    else if (asCommand = 'ABOR')
    then begin
        SendAnswer(MSG_FTP_ABOR_OK) ;
    end
    else if (asCommand = 'RETR')
    then begin
        RetrCommand(asParameter) ;

        prLastFileTransfert := Now ;
    end
    else if (asCommand = 'STOR')
    then begin
        StorStouAppeCommand(asParameter, scmNormal) ;

        prLastFileTransfert := Now ;
    end
    else if (asCommand = 'APPE')
    then begin
        StorStouAppeCommand(asParameter, scmAppend) ;

        prLastFileTransfert := Now ;
    end
    else if (asCommand = 'STOU')
    then begin
        StorStouAppeCommand('', scmUnique) ;

        prLastFileTransfert := Now ;
    end
    else begin
        SendAnswer(MSG_FTP_CMD_NOT_UNDERSTOOD) ;
    end ;

    // We reinit rest value if not rest command
    if asCommand <> 'REST'
    then begin
        piStartTransfertFileValue := 0 ;
    end ;
end;

//
// Display all feature
procedure TFtpClient.FeatCommand ;
{$I ftpfeatcmd.inc}

//
// Utf8 feature
//
// @param asParameter parameter (ON or OFF)
procedure TFtpClient.Utf8Command(const asParameter : String) ;
{$I ftputf8cmd.inc}

//
// PWD/XPWD feature
procedure TFtpClient.PwdCommand ;
{$I ftppwdcmd.inc}

//
// CWD/CDUP/XCDUP command
//
// @param asParameter parameter
procedure TFtpClient.ChangeDirectoryCommand(const asParameter : String;
    const abCDUP : Boolean) ;
{$I ftpcwdcmd.inc}

//
// Port feature
//
// @param asParameter parameter 127,0,0,1,0,2
procedure TFtpClient.PortCommand(const asParameter : String) ;
{$I ftpportcmd.inc}

//
// PASV feature
procedure TFtpClient.PasvCommand ;
{$I ftppasvcmd.inc}

//
// List/Nlst feature
//
// @param asParameter file name
procedure TFtpClient.ListCommand(const asParameter : String; const abNlst : Boolean) ;
{$I ftplistcmd.inc}

//
// Type feature
//
// @param asParameter parameter (ON or OFF)
procedure TFtpClient.TypeCommand(const asParameter : String) ;
{$I ftptypecmd.inc}

//
// Mode feature
//
// @param asParameter parameter S
procedure TFtpClient.ModeCommand(const asParameter : String) ;
{$I ftpmodecmd.inc}

//
// Rest feature
//
// @param asParameter parameter as number
procedure TFtpClient.RestCommand(const asParameter : String) ;
{$I ftprestcmd.inc}

//
// Retr feature
//
// @param asParameter parameter is file name
procedure TFtpClient.RetrCommand(const asParameter : String) ;
{$I ftpretrcmd.inc}

//
// STOR/STOU/APPE feature
//
// @param asParameter file name
// @param atMode mode file
procedure TFtpClient.StorStouAppeCommand(const asParameter : String;
    const atMode : TStoreCommandeMode) ;
{$I ftpstorcmd.inc}

end.

