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

uses
  Classes, SysUtils, FtpTypes, FtpFunctions, FtpMessages, FtpConst
  , blcksock, synsock, MD5Api ;

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
    FOnClientConfigRead : TClientConfigReader;
    // Previous thread
    FPreviousClient : TFtpClient;
    // Next thread
    FNextClient : TFtpClient;
    // Call when connection shutdown
    FOnClientDisconnect : TClientDisconnect;
    // Login
    FOnLogin : TClientLogin ;
    // Logout
    FOnLogout : TClientLogout ;
    // FullLog
    FFullLog : Boolean ;
    // Utf8 support
    FUtf8Support : Boolean ;
    // Gui application for non-freeze log in console mode
    FGuiApplication : Boolean ;

    // Command socket
    poClientSock:     TTCPBlockSocket;
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

    // Message to show
    psMessageLogOrError : String ;

    // Return True if can login
    function CheckLogin(const asLoginName : String) : Boolean ;
    // Logout
    procedure Logout(const asLoginName : String) ;
    // Call FOnLog if set
    procedure Log(const asMessage : String) ;
    // Synchronized Log. Don't use directely. Use Log()
    procedure SynchronizeLog ;
    // Synchronized Error. Don't use directely. Use Error()
    procedure SynchronizeError ;
    // Call FOnError if set
    procedure Error(const asMessage : String) ;
    // Get config
    procedure ReadConfig(const asLoginName : String) ;
    // Read remote string
    function ReadRemoteString(var asString : String) : Boolean ;
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
    // Execute user command
    procedure ExecuteUserCommand(const asCommand : String;
        const asParameter : String; var asCurrentDirectory : String) ;

    // FEAT command
    procedure FeatCommand ;
    // UTF8 command
    procedure Utf8Command(const asParameter : String) ;

  public
    // Previous client
    property PreviousClient: TFtpClient read FPreviousClient write FPreviousClient;
    // Next client
    property NextClient: TFtpClient read FNextClient write FNextClient;
    // Full log
    property FullLog : Boolean read FFullLog write FFullLog ;
    // Utf8 support
    property Utf8Support : Boolean read FUtf8Support write FUtf8Support ;
    // Gui application for non-freeze log in console mode
    property GuiApplication : Boolean read FGuiApplication write FGuiApplication ;

    // Log
    property OnLog: TLogProcedure read FOnLog write FOnLog;
    // Error
    property OnError: TLogProcedure read FOnError write FOnError;
    // Client config reader
    property OnClientConfigRead: TClientConfigReader
      read FOnClientConfigRead write FOnClientConfigRead;
    // Call when connection shutdown
    property OnClientDisconnect: TClientDisconnect
      read FOnClientDisconnect write FOnClientDisconnect;
    // Login
    property OnLogin : TClientLogin read FOnLogin write FOnLogin ;
    // Logout
    property OnLogout : TClientLogout read FOnLogout write FOnLogout ;

    // Constructor
    constructor Create(const abCreateSuspended: boolean;
      const aoClientSock: TTCPBlockSocket; const asWelcomeMessage: String;
      const asGoodbyeMessage: String; const aiTimeOut : Integer;
      const aiMaxLogin : Integer);
    // Destructor
    destructor Destroy; override;
    // Execute main thread
    procedure Execute; override;
  end;

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
constructor TFtpClient.Create(const abCreateSuspended: boolean;
  const aoClientSock: TTCPBlockSocket; const asWelcomeMessage: String;
  const asGoodbyeMessage: String; const aiTimeOut : Integer;
  const aiMaxLogin : Integer) ;
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

    FOnClientDisconnect := nil;

    FOnLog := nil;

    FOnError := nil;

    FOnClientConfigRead := nil;

    InitDefaultUser ;
end;

//
// Check if can login (if no max user)
//
// @param asLoginName login name
function TFtpClient.CheckLogin(const asLoginName : String) : Boolean ;
begin
    Result := True ;

    if FOnLogin <> nil
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
    if FOnLogout <> nil
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
    prUserConfig.Password := DEFAULT_USER_PASSWORD ;
    prUserConfig.Root := DEFAULT_USER_ROOT ;
    prUserConfig.Download := DEFAULT_USER_DOWNLOAD = YES_VALUE ;
    prUserConfig.Upload := DEFAULT_USER_UPLOAD = YES_VALUE ;
    prUserConfig.Rename := DEFAULT_USER_RENAME = YES_VALUE ;
    prUserConfig.Delete := DEFAULT_USER_DELETE = YES_VALUE ;
    prUserConfig.MakeDirectory := DEFAULT_USER_MAKE_DIRECTORY = YES_VALUE ;
    prUserConfig.DeleteDirectory := DEFAULT_USER_DELETE_DIRECTORY = YES_VALUE ;
    prUserConfig.ListSubDir := DEFAULT_USER_SUB_DIR = YES_VALUE ;
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
begin
    if FOnClientConfigRead <> nil
    then begin
        lrUserConfig := FOnClientConfigRead(asLoginName) ;

        prUserConfig.Password := lrUserConfig.Password ;
        prUserConfig.Root := lrUserConfig.Root ;
        prUserConfig.Download := lrUserConfig.Download = YES_VALUE ;
        prUserConfig.Upload := lrUserConfig.Upload = YES_VALUE ;
        prUserConfig.Rename := lrUserConfig.Rename = YES_VALUE ;
        prUserConfig.Delete := lrUserConfig.Delete = YES_VALUE ;
        prUserConfig.MakeDirectory := lrUserConfig.MakeDirectory = YES_VALUE ;
        prUserConfig.DeleteDirectory := lrUserConfig.DeleteDirectory = YES_VALUE ;
        prUserConfig.ListSubDir := lrUserConfig.ListSubDir = YES_VALUE ;
        prUserConfig.Disabled := lrUserConfig.Disabled = YES_VALUE ;
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
function TFtpClient.ReadRemoteString(var asString : String) : Boolean ;
var
    // Local time out
    liTimeOut : Integer ;
begin
    liTimeOut := piTimeOut ;

    Result := False ;

    while (liTimeOut > 0) and (Terminated = False) do
    begin
        asString := poClientSock.RecvString(1000) ;

        if poClientSock.LastError = 0
        then begin
            Result := True ;

            if FFullLog
            then begin
                Log(Format(MSG_LOG_COMMAND,
                    [poClientSock.GetRemoteSinIP, asString])) ;
            end ;

            break ;
        end
        else if poClientSock.LastError <> WSAETIMEDOUT
        then begin
            break ;
        end ;

        Dec(liTimeOut) ;
    end ;
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
    if (FOnLogout <> nil) and (prUserConfig.Connected)
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
    if (asMessage <> '') and (FOnLog <> nil)
    then begin
        if FGuiApplication
        then begin
            psMessageLogOrError := asMessage ;

            Synchronize(@SynchronizeLog) ;
        end
        else begin
            FOnLog(asMessage) ;
        end ;
    end ;
end ;

//
// Call Error procedure with synchronize
//
// @param asMessage message to display
procedure TFtpClient.Error(const asMessage : String) ;
begin
    if (asMessage <> '') and (FOnError <> nil)
    then begin
        if FGuiApplication
        then begin
            psMessageLogOrError := asMessage ;

            Synchronize(@SynchronizeError) ;
        end
        else begin
            FOnError(asMessage) ;
        end ;
    end ;
end ;

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
                (MD5(asPassword) = prUserConfig.Password) ;
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

                OnError(Format(MSG_ERRO_ROOT_USER_NOT_FOUND,
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
    // Current directory
    lsCurrentDirectory : String ;
begin
    Log(Format(MSG_LOG_NEW_CONNECTION,
        [poClientSock.GetRemoteSinIP])) ;

    // Send welcome message
    poClientSock.SendString(psWelcomeMessage);

    lbQuit := False ;

    lsRemoteString := '' ;
    lsParameter := '' ;
    lsCommand := '' ;

    while (Terminated = False) and (lbQuit = False) do
    begin
        // If can read
        if ReadRemoteString(lsRemoteString) and (Terminated = False)
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
            // TODO REIN InitDefaultUser + lsCurrentDirectory
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
                    lsCurrentDirectory := prUserConfig.Root ;
                end ;
            end
            else if prUserConfig.Connected = True
            then begin
                // Procedure for all command
                ExecuteUserCommand(lsCommand, lsParameter, lsCurrentDirectory) ;
            end ;
        end
        else begin
            if not Terminated
            then begin
                SendAnswer(Format(MSG_FTP_TIME_OUT, [piTimeOut])) ;
            end ;

            break ;
        end ;
    end ;

    OnLog(Format(MSG_LOG_CONNECTION_CLOSE,
        [poClientSock.GetRemoteSinIP])) ;

    poClientSock.CloseSocket;

    poClientSock.Free;

    Logout ;

    // Send disconnect to the parent
    if FOnClientDisconnect <> nil
    then begin
      FOnClientDisconnect(Self) ;
    end ;
end ;

//
// Execute user command
//
// @param asCommand user command
// @param asParameter user parameter
// @param asCurrentDirectory current directory
procedure TFtpClient.ExecuteUserCommand(const asCommand : String; const asParameter : String;
    var asCurrentDirectory : String) ;
begin
    if (asCommand = 'NOP') or (asCommand = 'NOOP')
    then begin
        SendAnswer(MSG_FTP_NOOP) ;
    end
    else if asCommand = 'SYST'
    then begin
        SendAnswer(MSG_FTP_SYST) ;
    end
    else if asCommand = 'FEAT'
    then begin
        FeatCommand ;
    end
    else if asCommand = 'UTF8'
    then begin
        Utf8Command(asParameter) ;
    end
    else begin
        SendAnswer(MSG_FTP_CMD_NOT_UNDERSTOOD) ;
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

end.

