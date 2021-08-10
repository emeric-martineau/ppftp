unit maingui;

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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, IniFiles, StrUtils, LCLType, contnrs,
  CommandLine, aboutform, mainconfig, user, FtpFunctions, FtpMain, FtpTypes,
  FtpConst, FtpFileList, FtpClient ;

type

  { TFormMain }

  TFormMain = class(TForm)
    FileTransfertListView: TListView;
    LoginListView: TListView;
    MainMenu1: TMainMenu;
    LogMemo: TMemo;
    ErrorMemo: TMemo;
    MenuItem1: TMenuItem;
    ExitMenu: TMenuItem;
    AboutMenu: TMenuItem;
    CommandLineMenu: TMenuItem;
    MainConfigMenu: TMenuItem;
    LogMenu: TMenuItem;
    CancelTransfertMenu: TMenuItem;
    PopupMenu1: TPopupMenu;
    TransfertTabSheet: TTabSheet;
    UsersMenu: TMenuItem;
    SaveLogMenuItem: TMenuItem;
    ClearLogMenu: TMenuItem;
    LogSaveDialog: TSaveDialog;
    SetupMenu: TMenuItem;
    HelphMenu: TMenuItem;
    PageControl1: TPageControl;
    StartMenu: TMenuItem;
    StatusBar1: TStatusBar;
    StopMenu: TMenuItem;
    ServerMenu: TMenuItem;
    LogTabSheet: TTabSheet;
    ErrorTabSheet: TTabSheet;
    TrayIcon1: TTrayIcon;
    UserTabSheet: TTabSheet;
    procedure AboutMenuClick(Sender: TObject);
    procedure ClearLogMenuClick(Sender: TObject);
    procedure CommandLineMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MainConfigMenuClick(Sender: TObject);
    procedure CancelTransfertMenuClick(Sender: TObject);
    procedure SaveLogMenuItemClick(Sender: TObject);
    procedure ServerMenuClick(Sender: TObject);
    procedure StartMenuClick(Sender: TObject);
    procedure StopMenuClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure UsersMenuClick(Sender: TObject);
  protected
    // If tray icon can be enable
    pbSystray : Boolean ;
    // Activate/Desactivate start/stop menu
    procedure StartStopMenu(const abStart : Boolean) ;
    // Start server
    procedure StartServer ;
    // Stop server
    procedure StopServer ;
    // Hide form
    procedure HideForm ;
    // Clear list of login
    procedure ClearListOfLogin ;
    // Clear list of file
    procedure ClearListOfFile ;
  private
    { private declarations }
    // Minimize window on startup
    pbMinimizeOnStartup : Boolean ;
  public
    { public declarations }
    // If end query receive
    pbQueryEndSession : Boolean ;
    // Callback for end query
    procedure OnQueryEndSession(var Cancel : Boolean) ;
  end; 

var
  FormMain: TFormMain;
  // Root config directory
  giRootConfigDirectory : String ;
  // Ftp main
  goFtpMain : TFtpMain ;
  // Ini configuration for ftp server
  goMainConfig : TIniFile ;
  // Pase date
  gsLogFormat : String ;
  // If application shutdown. Use for log
  gbShutdown : Boolean ;
  // HashMap for login count on file transfert
  goListLogin : TFPHashList ;
  // Local folder conf file
  gsFolderLocalConfigName : String ;
  // HashMap for login count on file transfert
  goListFile : TFPHashList ;

const
  TIMER_DELAY : Cardinal = 500 ;
  MAIN_INI_SECTION : String = 'main' ;
  USER_SECTION : String = 'user' ;
  DEFAULT_FTP_ACCESS : String = 'ftp.access' ;
  FTP_ACCESS_KEY : String = 'FtpAccessFile' ;
  MAX_INTEGER_VALUE : Integer = 2147483647 ;
  USERS_DIRECTORY : String = 'users' ;
  FOLDER_IN_SECTION : String = 'folder' ;

procedure ErrorMsg(const asMessage : String) ;
procedure LogMsg(const asMessage : String) ;
function ReadMainConfig(const asKey : String) : String ;
function ReadUserConfig(const asLoginName : String) : TUserConfig ;
function ParseDatePattern(const asPattern : String) : String ;
procedure UpdateLoginCount(const asLogin : String; const aiNumberOfLogin : Integer) ;
function CheckPassword(const asLoginName : String;
    const asPassword : String) : Boolean ;
function FileProtected(const asPathAndFileName : String;
    const abUtf8 : Boolean) : Boolean ;
function RemoveFileProtected(const asPathAndFileName : String;
    const abUtf8 : Boolean) : Boolean ;
function FolderLocalConfigReader(const asFolderName : String;
        const abUtf8 : Boolean; const asKey : String;
        var asValue : String) : Boolean ;
procedure TransfertFile(const asLoginName : String;
        const asFileName : String; const asDownload : Boolean;
        const asStart : Boolean; const asFtpClient : Pointer) ;
procedure StopServerCallBack ;
procedure StartServerCallBack ;

implementation

{ TFormMain }

//
// Constructor
//
// @param Sender object to create
procedure TFormMain.FormCreate(Sender: TObject);
var
    // Counter of parameter
    liIndexParamStr : Integer ;
    // Lauch at startup ?
    lbLauch : Boolean ;
begin
    gbShutdown := False ;

    pbQueryEndSession := False ;

    Application.OnQueryEndSession:= @OnQueryEndSession ;

    pbSystray := True ;

    lbLauch := False ;

    pbMinimizeOnStartup := False ;

    liIndexParamStr := 1 ;

    while (liIndexParamStr <= ParamCount) do
    begin
        if (ParamStr(liIndexParamStr) = '-root')
        then begin
            Inc(liIndexParamStr) ;

            giRootConfigDirectory := AddTrailing(
                ParamStr(liIndexParamStr), DirectorySeparator) ;
        end
        else if (ParamStr(liIndexParamStr) = '-notrayicon')
        then begin
            pbSystray := False ;
        end
        else if (ParamStr(liIndexParamStr) = '-launch')
        then begin
            lbLauch := True ;
        end
        else if (ParamStr(liIndexParamStr) = '-minimize')
        then begin
            pbMinimizeOnStartup := True ;
        end ;

        Inc(liIndexParamStr) ;
    end ;

    // Defaut log format
    gsLogFormat := '%Y/%M/%d %H:%m:%s ' ;

    goListLogin := TFPHashList.Create ;

    goListFile := TFPHashList.Create ;

    goFtpMain := nil ;

    if lbLauch
    then begin
        StartServer ;
    end ;
end;

//
// If form show
procedure TFormMain.FormShow(Sender: TObject);
begin
    if pbMinimizeOnStartup
    then begin
        HideForm ;

        if pbMinimizeOnStartup and not pbSystray
        then begin
            Self.WindowState := wsMinimized ;
        end ;

        pbMinimizeOnStartup:= False ;
    end;
end;

//
// Call when window state change (i.e. minimize)
//
// @param Sender sender to send minimize
procedure TFormMain.FormWindowStateChange(Sender: TObject);
begin
    if FormMain.WindowState = wsMinimized
    then begin
        HideForm ;
    end ;
end;

procedure TFormMain.MainConfigMenuClick(Sender: TObject);
var
    loMainConfigForm : TMainConfigForm ;
begin
    loMainConfigForm := TMainConfigForm.Create(Self) ;

    loMainConfigForm.ConfigPath := giRootConfigDirectory + 'ppftpconf.ini' ;
    loMainConfigForm.FolderLocalConfigName := DEFAULT_FTP_ACCESS ;
    loMainConfigForm.LocalConfigKey := FTP_ACCESS_KEY ;
    loMainConfigForm.MainConfigSection := MAIN_INI_SECTION ;

    loMainConfigForm.ShowModal() ;

    loMainConfigForm.Free ;
end;

procedure TFormMain.CancelTransfertMenuClick(Sender: TObject);
var
    // Id ftp
    lsIdFtp : String ;
    // Record file
    prCurrentFile : PTFtpFile ;
begin
    if Assigned(FileTransfertListView.ItemFocused)
    then begin
        lsIdFtp := FileTransfertListView.ItemFocused.SubItems[0] ;

        prCurrentFile := PTFtpFile(goListFile.Find(lsIdFtp)) ;

        TFtpClient(prCurrentFile^.FtpClient).Cancel := True ;
    end;
end;

procedure TFormMain.SaveLogMenuItemClick(Sender: TObject);
begin
    if LogSaveDialog.Execute
    then begin
        LogMemo.Lines.SaveToFile(LogSaveDialog.FileName) ;
    end ;
end;

//
// Hide form
procedure TFormMain.HideForm ;
begin
    if pbSystray
    then begin
        TrayIcon1.Visible := True ;

        FormMain.ShowInTaskBar := stNever ;

        FormMain.Hide ;
    end ;
end ;

//
// When click on Server menu
procedure TFormMain.ServerMenuClick(Sender: TObject);
begin
    if goFtpMain <> nil
    then begin
        StartStopMenu(goFtpMain.Running) ;
    end
    else begin
        StartStopMenu(False) ;
    end ;
end;

//
// If click on start server
//
// @param Sender
procedure TFormMain.StartMenuClick(Sender: TObject);
begin
    StartServer ;
end;

//
// If click on stop server
//
// @param Sender
procedure TFormMain.StopMenuClick(Sender: TObject);
begin
    StopServer ;
end;

//
// If click on tray icon
//
// @param Sender
procedure TFormMain.TrayIcon1Click(Sender: TObject);
begin
    FormMain.ShowInTaskBar := stAlways ;

    FormMain.WindowState := wsNormal ;

    TrayIcon1.Visible := False ;

    FormMain.ShowOnTop ;
end;

procedure TFormMain.UsersMenuClick(Sender: TObject);
var
    loUserForm : TUserForm ;
begin
    loUserForm := TUserForm.Create(Self) ;

    loUserForm.psUsersDirectory := giRootConfigDirectory + USERS_DIRECTORY ;
    loUserForm.psUserSection := USER_SECTION ;

    loUserForm.ShowModal ;

    loUserForm.Free ;
end;

//
// If click on close window
procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    gbShutdown := True ;

    if goFtpMain <> nil
    then begin
        goFtpMain.OnMainConfigRead := nil ;
        goFtpMain.OnLog := nil ;
        goFtpMain.OnError := nil ;
        goFtpMain.OnClientConfigRead := nil ;
        goFtpMain.OnLogin := nil ;
        goFtpMain.OnLogout := nil ;
        goFtpMain.OnClientCheckPassword := nil ;
        //goFtpMain.OnFileProtected := nil ;
        goFtpMain.OnLocalConfigExists := nil ;
        goFtpMain.OnTransfert := nil ;
        goFtpMain.OnStart := nil ;
        goFtpMain.OnStop := nil ;

        //goFtpMain.FreeOnTerminate := True ;
        goFtpMain.Terminate ;

        FreeAndNil(goFtpMain) ;
    end ;

    FreeAndNil(goMainConfig) ;

    ClearListOfLogin ;

    FreeAndNil(goListLogin) ;

    ClearListOfFile ;

    FreeAndNil(goListFile) ;
end;

//
// If close window
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    if not pbQueryEndSession and (goFtpMain <> nil) and goFtpMain.Running
    then begin
        if Application.MessageBox('Server running. Do you want really exit ?',
            'Server running', MB_ICONQUESTION or MB_YESNO) = IDNO
        then begin
            CanClose := False ;
        end ;
    end ;
end;

//
// Click on exit menu
procedure TFormMain.ExitMenuClick(Sender: TObject);
begin
    Close ;
end;

//
// Show command line form
procedure TFormMain.CommandLineMenuClick(Sender: TObject);
var
    loCommandLineForm : TCommandLineForm ;
begin
    loCommandLineForm := TCommandLineForm.Create(Self) ;

    loCommandLineForm.ShowModal ;

    loCommandLineForm.Free ;
end;

//
// About menu
procedure TFormMain.AboutMenuClick(Sender: TObject);
var
    loAboutForm : TAboutForm2 ;
begin
    loAboutForm := TAboutForm2.Create(Self) ;
    loAboutForm.ShowModal ;
    loAboutForm.Free ;
end;

//
// Clear log
procedure TFormMain.ClearLogMenuClick(Sender: TObject);
begin
    if Application.MessageBox('Do you want clear current log ?',
        'Clear log', MB_ICONQUESTION or MB_YESNO) = IDYES
    then begin
        LogMemo.Lines.Clear ;
    end ;
end;

//
// Start/Stop server
procedure TFormMain.StartStopMenu(const abStart : Boolean) ;
begin
    StartMenu.Enabled := not abStart ;
    StopMenu.Enabled := abStart ;
end;

//
// Start server
procedure TFormMain.StartServer ;
begin
    StatusBar1.SimpleText := 'Starting server in progress...' ;

    if not FileExists(giRootConfigDirectory + 'ppftpconf.ini')
    then begin
        ErrorMsg('Main config file ''' + giRootConfigDirectory + 'ppftpconf.ini' + ''' not found !');
    end ;

    // Lacal config name
    gsFolderLocalConfigName := DEFAULT_FTP_ACCESS ;

    // Create ini file reader
    goMainConfig := TIniFile.Create(giRootConfigDirectory + 'ppftpconf.ini') ;
    goMainConfig.CaseSensitive := False ;
    goMainConfig.StripQuotes := True ;

    // Free ftp if not free
    if Assigned(goFtpMain)
    then begin
        goFtpMain.Free ;
    end ;

    goFtpMain := TFtpMain.Create(true) ;

    // Read folder config file
    gsFolderLocalConfigName :=
        goMainConfig.ReadString(MAIN_INI_SECTION, FTP_ACCESS_KEY,
        gsFolderLocalConfigName) ;

    goFtpMain.OnMainConfigRead := @ReadMainConfig ;
    goFtpMain.OnLog := @LogMsg ;
    goFtpMain.OnError := @ErrorMsg ;
    goFtpMain.OnClientConfigRead := @ReadUserConfig ;
    goFtpMain.OnLogin := @UpdateLoginCount ;
    goFtpMain.OnLogout := @UpdateLoginCount ;
    goFtpMain.OnClientCheckPassword := @CheckPassword ;
    goFtpMain.OnFileProtected := @FileProtected ;
    goFtpMain.OnRemoveFileProtected := @RemoveFileProtected ;
    goFtpMain.OnLocalConfigExists := @FolderLocalConfigReader ;
    goFtpMain.OnTransfert := @TransfertFile ;
    goFtpMain.OnStart := @StartServerCallBack ;
    goFtpMain.OnStop := @StopServerCallBack ;

    //goFtpMain.FreeOnTerminate := True ;
    // Set False if console application, else true
    goFtpMain.GuiApplication := True ;

    goFtpMain.Resume ;
end ;

//
// Start server
procedure TFormMain.StopServer ;
begin
    StatusBar1.SimpleText := 'Shutdown server in progress...' ;

    goFtpMain.Terminate ;
end ;

//
// Clear list of login
procedure TFormMain.ClearListOfLogin ;
var
    // Index of login
    liIndex : Integer ;
    // List viw
    loListViewLine : TListItem ;
begin
    for liIndex := 0 to goListLogin.Count - 1 do
    begin
        loListViewLine := TListItem(goListLogin[liIndex]) ;

        FreeAndNil(loListViewLine) ;
    end ;

    goListLogin.Clear;
end ;

//
// Clear list of file
procedure TFormMain.ClearListOfFile ;
var
    // Index of login
    liIndex : Integer ;
    // List viw
    lpFtpFileList : PTFtpFile ;
begin
    for liIndex := 0 to goListFile.Count - 1 do
    begin
        lpFtpFileList := PTFtpFile(goListFile[liIndex]) ;

        Dispose(lpFtpFileList) ;
    end ;

    goListFile.Clear;
end ;

////////////////////////////////////////////////////////////////////////////////
// CALL BACK FUNCTION
////////////////////////////////////////////////////////////////////////////////

//
// Error message in file
//
// @param asMessage message to log
procedure ErrorMsg(const asMessage : String) ;
begin
    if not gbShutdown
    then begin
        if FormMain.ErrorMemo.Lines.Count = MAX_INTEGER_VALUE
        then begin
            FormMain.ErrorMemo.Lines.Clear ;
        end ;

        FormMain.ErrorMemo.Lines.Add(ParseDatePattern(gsLogFormat) + asMessage) ;
    end ;
end ;

//
// Log message in file
//
// @param asMessage message to log
procedure LogMsg(const asMessage : String) ;
begin
    if not gbShutdown
    then begin
        if FormMain.LogMemo.Lines.Count = MAX_INTEGER_VALUE
        then begin
            FormMain.LogMemo.Lines.Clear ;
        end ;

        FormMain.LogMemo.Lines.Add(ParseDatePattern(gsLogFormat) + asMessage) ;
    end ;
end ;

//
// Read main config
//
// @param asKey key to read
//
// @return value or ''
function ReadMainConfig(const asKey : String) : String ;
begin
    if gbShutdown
    then begin
        Result := ''
    end
    else begin
        Result := goMainConfig.ReadString(MAIN_INI_SECTION, asKey, '') ;
    end ;
end ;

//
// Read user config
function ReadUserConfig(const asLoginName : String) : TUserConfig ;
var
    // Ini file
    loUserConfig : TIniFile ;
    // File to read
    lsFile : String ;
begin
    if gbShutdown
    then begin
        Result.UserFound := False ;
    end
    else begin
        lsFile := ExpandFileName(giRootConfigDirectory + USERS_DIRECTORY +
            DirectorySeparator + asLoginName + '.ini') ;

        Result.UserFound := FileExists(lsFile);

        if not Result.UserFound
        then begin
            ErrorMsg('User config ''' + lsFile + ''' not found ! Maybe user doesn''t exist.') ;
        end ;

        loUserConfig := TIniFile.Create(lsFile) ;

        loUserConfig.CaseSensitive := False ;
        loUserConfig.StripQuotes := True ;

        Result.Root := loUserConfig.ReadString(USER_SECTION, USER_CONF_ROOT,
            DEFAULT_USER_ROOT) ;
        Result.Download := loUserConfig.ReadString(USER_SECTION, USER_CONF_DOWNLOAD,
            DEFAULT_USER_DOWNLOAD) ;
        Result.Upload := loUserConfig.ReadString(USER_SECTION, USER_CONF_UPLOAD,
            DEFAULT_USER_UPLOAD);
        Result.Rename := loUserConfig.ReadString(USER_SECTION, USER_CONF_RENAME,
            DEFAULT_USER_RENAME) ;
        Result.Delete := loUserConfig.ReadString(USER_SECTION, USER_CONF_DELETE,
            DEFAULT_USER_DELETE) ;
        Result.MakeDirectory := loUserConfig.ReadString(USER_SECTION, USER_CONF_MAKE_DIRECTORY,
            DEFAULT_USER_MAKE_DIRECTORY) ;
        Result.DeleteDirectory := loUserConfig.ReadString(USER_SECTION, USER_CONF_DELETE_DIRECTORY,
            DEFAULT_USER_DELETE_DIRECTORY) ;
        Result.SubDir := loUserConfig.ReadString(USER_SECTION, USER_CONF_SUB_DIR,
            DEFAULT_USER_SUB_DIR) ;
        Result.ModifyFileTime := loUserConfig.ReadString(USER_SECTION, USER_CONF_MODIFY_FILE_TIME ,
            DEFAULT_USER_MODIFY_FILE_TIME) ;            
        Result.Disabled := loUserConfig.ReadString(USER_SECTION, USER_CONF_DISABLED,
            DEFAULT_USER_DISABLED) ;
        Result.ByteRate := loUserConfig.ReadString(USER_SECTION, USER_CONF_BYTE_RATE,
            DEFAULT_USER_CONF_BYTE_RATE) ;

        loUserConfig.Free ;
    end ;
end ;

//
// Parse date pattern
//
// @param asPattern pattern of file name
function ParseDatePattern(const asPattern : String) : String ;
var
    // New pattern
    lsNewPattern : String ;
begin

    lsNewPattern := AnsiReplaceStr(asPattern, '%Y', FormatDateTime('yyyy', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%M', FormatDateTime('mm', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%d', FormatDateTime('dd', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%H', FormatDateTime('hh', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%m', FormatDateTime('nn', Now())) ;
    lsNewPattern := AnsiReplaceStr(lsNewPattern, '%s', FormatDateTime('ss', Now())) ;

    Result := lsNewPattern ;
end ;

//
// End query call back. do nothing
procedure TFormMain.OnQueryEndSession(var Cancel : Boolean) ;
begin
    pbQueryEndSession := True ;
end ;

//
// Update login count
procedure UpdateLoginCount(const asLogin : String; const aiNumberOfLogin : Integer) ;
var
    loListViewLine : TListItem ;
begin
    if not gbShutdown
    then begin
        // Seach if login is already register
        loListViewLine := TListItem(goListLogin.Find(asLogin)) ;

        if loListViewLine = nil
        then begin
            // Create line
            loListViewLine := FormMain.LoginListView.Items.Add ; //TListItem.Create(FormMain.LoginListView) ;

            loListViewLine.Caption := asLogin ;

            loListViewLine.SubItems.Add(IntToStr(aiNumberOfLogin)) ;
            loListViewLine.SubItems.Add('0');

            goListLogin.Add(asLogin, loListViewLine) ;
        end
        else begin
            if aiNumberOfLogin = 0
            then begin
                goListLogin.Delete(goListLogin.FindIndexOf(asLogin)) ;

                // Remove user from list
                loListViewLine.Free ;
            end
            else begin
                loListViewLine.SubItems[0] := IntToStr(aiNumberOfLogin) ;
            end ;
        end ;
    end ;
end ;

// Check client password
function CheckPassword(const asLoginName : String;
    const asPassword : String) : Boolean ;
var
    // Ini file
    loUserConfig : TIniFile ;
    // File to read
    lsFile : String ;
    // Password
    lsIniPassword : String ;
begin
    Result := False ;

    if not gbShutdown
    then begin
        lsFile := ExpandFileName(giRootConfigDirectory + 'users' +
            DirectorySeparator + asLoginName + '.ini') ;

        if FileExists(lsFile)
        then begin
            loUserConfig := TIniFile.Create(lsFile) ;

            loUserConfig.CaseSensitive := False ;
            loUserConfig.StripQuotes := True ;

            lsIniPassword := loUserConfig.ReadString(USER_SECTION, USER_CONF_PASSWORD,
                DEFAULT_USER_PASSWORD) ;

            Result := FtpFunctions.MD5(asPassword) = LowerCase(lsIniPassword) ;
        end ;
    end ;
end ;

//
// File portected
//
function FileProtected(const asPathAndFileName : String;
    const abUtf8 : Boolean) : Boolean ;
var
    // Current file name
    lsFileName : String ;
begin
    lsFileName := ExtractFileName(asPathAndFileName) ;

    if abUtf8
    then begin
        lsFileName := UTF8ToSys(lsFileName) ;
    end ;

    {$IFDEF WINDOWS}
    Result := CompareFilenamesIgnoreCase(gsFolderLocalConfigName, lsFileName)
        = 0 ;
    {$ELSE}
    Result := CompareFilenames(gsFolderLocalConfigName, lsFileName) = 0 ;
    {$ENDIF}
end ;

//
// File portected
//
function RemoveFileProtected(const asPathAndFileName : String;
    const abUtf8 : Boolean) : Boolean ;
var
    lsProtectFileName : String ;
begin
    Result := True ;

    lsProtectFileName := AddTrailing(asPathAndFileName,
        DirectorySeparator) + gsFolderLocalConfigName ;

    if CheckFileExists(lsProtectFileName, abUtf8)
    then begin
        Result := InternalDeleteFile(lsProtectFileName , abUtf8) ;
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
function FolderLocalConfigReader(const asFolderName : String;
        const abUtf8 : Boolean; const asKey : String;
        var asValue : String) : Boolean ;
var
    // File name
    lsFileName : String ;
    // Ini file
    loFolderConfig : TIniFile ;
begin
    if abUtf8
    then begin
        lsFileName := UTF8ToSys(asFolderName) ;
    end
    else begin
        lsFileName := asFolderName ;
    end;

    lsFileName := AddTrailing(lsFileName, DirectorySeparator) +
        gsFolderLocalConfigName ;

    Result := FileExists(lsFileName) ;

    if Result
    then begin
        loFolderConfig := TIniFile.Create(lsFileName) ;

        loFolderConfig.CaseSensitive := False ;
        loFolderConfig.StripQuotes := True ;

        Result := loFolderConfig.ValueExists(FOLDER_IN_SECTION, asKey) ;

        if Result
        then begin
            asValue := loFolderConfig.ReadString(FOLDER_IN_SECTION, asKey, '');
        end ;

        loFolderConfig.Free ;
    end ;
end ;

{
// Transfert call back
//
// @param asLoginName login to trasfert
// @param asFileName filename
// @param asDownload true if download, false if upload
// @param asStart true start, false stop
// @param asFtpClient ftp client (TFtpClient)
procedure TransfertFile(const asLoginName : String;
        const asFileName : String; const asDownload : Boolean;
        const asStart : Boolean; const asFtpClient : Pointer) ;
var
    // Current download file
    lrCurrentFile : TFtpFile ;
    // Current list file
    loCurrentListFile : TFtpFileList ;
    // Item of view list for login
    loListViewLineLogin : TListItem ;
    // Item of view list of file
    loListViewLineFile : TListItem ;

    // Index of record
    liRecordIndex : Integer ;
begin
    if not gbShutdown
    then begin
        // Seach if login is already register
        loListViewLineLogin := TListItem(goListLogin.Find(asLoginName)) ;

        loCurrentListFile := TFtpFileList(goListFile.Find(asLoginName)) ;

        // Create and add line
        if not Assigned(loCurrentListFile)
        then begin
            loCurrentListFile := TFtpFileList.Create ;

            goListFile.Add(asLoginName, loCurrentListFile);
        end ;

        if  asStart
        then begin
            lrCurrentFile.FileName := asFileName ;
            lrCurrentFile.Download := asDownload ;
            lrCurrentFile.FtpClient := asFtpClient ;

            loListViewLineLogin.SubItems[1] := 'yes' ;

            // Create line
            loListViewLineFile := FormMain.FileTransfertListView.Items.Add ;

            loListViewLineFile.Caption := asLoginName ;

            loListViewLineFile.SubItems.Add(Format('%p', [asFtpClient])) ;
            loListViewLineFile.SubItems.Add(asFileName);

            if asDownload
            then begin
                loListViewLineFile.SubItems.Add('Send') ;
            end
            else begin
                loListViewLineFile.SubItems.Add('Receive') ;
            end ;

            lrCurrentFile.Extra := loListViewLineFile ;

            loCurrentListFile.Add(lrCurrentFile) ;
        end
        else begin
            // Delete record and line
            liRecordIndex := loCurrentListFile.Find(asFileName, asDownload,
                asFtpClient) ;

            if liRecordIndex <> -1
            then begin
                // Get line in list view
                loListViewLineFile := TListItem(loCurrentListFile[liRecordIndex].Extra) ;

                // Delete line in list view
                loListViewLineFile.Free ;

                // Delete record in list
                loCurrentListFile.Delete(liRecordIndex) ;
            end ;

            if loCurrentListFile.Count = 0
            then begin
                loListViewLineLogin.SubItems[1] := 'no' ;
            end ;
        end ;
    end ;
end ;
}

// Transfert call back
//
// @param asLoginName login to trasfert
// @param asFileName filename
// @param asDownload true if download, false if upload
// @param asStart true start, false stop
// @param asFtpClient ftp client (TFtpClient)
procedure TransfertFile(const asLoginName : String;
        const asFileName : String; const asDownload : Boolean;
        const asStart : Boolean; const asFtpClient : Pointer) ;
var
    // Current download file
    prCurrentFile : PTFtpFile ;
    // Item of view list for login
    loListViewLineLogin : TListItem ;
    // Item of view list of file
    loListViewLineFile : TListItem ;
    // Id ftp
    lsIdFtp : String ;
    // Index of record
    liRecordIndex : Integer ;
    // Counter of transfert
    liCounterOfTransfert : Integer ;
begin
    if not gbShutdown
    then begin
        // Seach if login is already register
        loListViewLineLogin := TListItem(goListLogin.Find(asLoginName)) ;

        lsIdFtp := Format('%p', [asFtpClient]) ;

        prCurrentFile := PTFtpFile(goListFile.Find(lsIdFtp)) ;

        // Create and add line
        if not Assigned(prCurrentFile)
        then begin
            New(prCurrentFile) ;
        end ;

        if asStart
        then begin
            prCurrentFile^.FileName := asFileName ;
            prCurrentFile^.Download := asDownload ;
            prCurrentFile^.FtpClient := asFtpClient ;

            liCounterOfTransfert := StrToInt(loListViewLineLogin.SubItems[1]) ;

            loListViewLineLogin.SubItems[1] := IntToStr(liCounterOfTransfert + 1) ;

            // Create line
            loListViewLineFile := FormMain.FileTransfertListView.Items.Add ;

            loListViewLineFile.Caption := asLoginName ;

            loListViewLineFile.SubItems.Add(lsIdFtp) ;
            loListViewLineFile.SubItems.Add(asFileName);

            if asDownload
            then begin
                loListViewLineFile.SubItems.Add('Send') ;
            end
            else begin
                loListViewLineFile.SubItems.Add('Receive') ;
            end ;

            prCurrentFile^.Extra := loListViewLineFile ;

            goListFile.Add(lsIdFtp, prCurrentFile) ;
        end
        else begin
            // Delete line in ListView
            loListViewLineFile := TListItem(prCurrentFile^.Extra) ;
            loListViewLineFile.Free ;

            // Free record
            Dispose(prCurrentFile) ;

            // Seach entry in map
            liRecordIndex := goListFile.FindIndexOf(lsIdFtp) ;

            // If found entry delete it
            if liRecordIndex <> -1
            then begin
                goListFile.Delete(liRecordIndex) ;
            end ;

            liCounterOfTransfert := StrToInt(loListViewLineLogin.SubItems[1]) ;

            Dec(liCounterOfTransfert) ;

            if liCounterOfTransfert > 0
            then begin
                loListViewLineLogin.SubItems[1] := IntToStr(liCounterOfTransfert) ;
            end
            else begin
                loListViewLineLogin.SubItems[1] := '0'
            end ;
        end ;
    end ;
end ;

//
// Start server call back
procedure StartServerCallBack ;
begin
    if not gbShutdown
    then begin
        FormMain.StatusBar1.SimpleText := 'Server running' ;
    end;
end ;

//
// Stop server call back
procedure StopServerCallBack ;
begin
    if not gbShutdown
    then begin
        if not Assigned(goFtpMain) or goFtpMain.ExitStatus = True
        then begin
            FormMain.StatusBar1.SimpleText := 'Server stopped' ;
        end
        else begin
            FormMain.StatusBar1.SimpleText := 'Server error !!!' ;
        end;
    end;
end ;

initialization
  {$I maingui.lrs}

end.

