unit mainconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, IniFiles, FtpFunctions, FtpConst;

type

  { TMainConfigForm }

  TMainConfigForm = class(TForm)
    AllowedIpLabel: TLabel;
    LocalFolderConfigNameLabeledEdit: TLabeledEdit;
    OkButton: TButton;
    CancelButton: TButton;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    WelcomeButton: TButton;
    DeniedPriorityCheckBox: TCheckBox;
    DeniedIpLabel: TLabel;
    AllowedIpMemo: TMemo;
    DeniedIpMemo: TMemo;
    DisabledLabel: TLabel;
    GoodbyeLabel: TLabel;
    GoodbyeButton: TButton;
    WelcomeMemo: TMemo;
    WelcomeLabel: TLabel;
    PortLabel: TLabel;
    PortSpinEdit: TSpinEdit;
    Utf8CheckBox: TCheckBox;
    FullLogCheckBox: TCheckBox;
    IpAddressLabeledEdit: TLabeledEdit;
    FileTransfertTimeOutLabel: TLabel;
    FiletransfertTimeOutSpinEdit: TSpinEdit;
    UserByteRateLabel: TLabel;
    PassivePortStartLabel: TLabel;
    PassivePortStopLabel: TLabel;
    PassivePortStartSpinEdit: TSpinEdit;
    PassivePortStopSpinEdit: TSpinEdit;
    UserByteRateLabel1: TLabel;
    UserByteRateSpinEdit: TSpinEdit;
    TimeOutSpinEdit: TSpinEdit;
    TimeOutLabel: TLabel;
    MaxClientLabel: TLabel;
    MaxsessionUserLabel: TLabel;
    MaxSessionUserSpinEdit: TSpinEdit;
    MaxClientSpinEdit: TSpinEdit;
    BufferSizeSpinEdit: TSpinEdit;
    GoodbyeMemo: TMemo;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GoodbyeButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure WelcomeButtonClick(Sender: TObject);
  private
    { private declarations }
    // If config is read
    pbConfigRead : Boolean ;
    // Ini file
    poMainConfig : TIniFile ;
    // Path to config
    psConfigPath : String ;
    // ftp.access
    psFolderLocalConfigName : String ;
    // Ini key
    psLocalConfigKey : String ;
    // Main config section
    psMainConfigSection : String ;
    // Read config
    procedure ReadMainConfig ;
    // Convert int
    function ConvertInteger(const asKey : String;
        const aiDefaultValue : Integer) : Integer ;
    // Convert string
    function ConvertString(asKey : String; asDefaultValue : String) : String ;
    // Convert boolean
    function ConvertBoolean(asKey : String; abDefaultValue : Boolean) : Boolean ;
    // Read main config
    function ReadMainConfig(const asKey : String) : String ;
    // Save main configuration
    procedure SaveMainConfig ;
    // Convert TStrings to string
    function ConvertTStringsToString(const aoList : TStrings ;
        const aoJoinString : String) : String ;
  public
    { public declarations }
    // Path to config
    property ConfigPath : String read psConfigPath write psConfigPath ;
    // ftp.access
    property FolderLocalConfigName : String read psFolderLocalConfigName write
        psFolderLocalConfigName ;
    // Ini key
    property LocalConfigKey : String read psLocalConfigKey write psLocalConfigKey ;
    // Main config section
    property MainConfigSection : String read psMainConfigSection write psMainConfigSection ;
  end; 

var
  MainConfigForm: TMainConfigForm;

implementation

//
// Form show
procedure TMainConfigForm.FormShow(Sender: TObject);
begin
    if not pbConfigRead
    then begin
        ReadMainConfig ;
    end ;
end;

//
// Cancel button
procedure TMainConfigForm.CancelButtonClick(Sender: TObject);
begin
    Close ;
end;

procedure TMainConfigForm.FormCreate(Sender: TObject);
begin
    pbConfigRead := False ;
end;

//
// When we click on Load from file for goodbye message
procedure TMainConfigForm.GoodbyeButtonClick(Sender: TObject);
begin
    if OpenDialog1.Execute
    then begin
        GoodbyeMemo.Lines.LoadFromFile(OpenDialog1.FileName) ;
    end;
end ;

procedure TMainConfigForm.OkButtonClick(Sender: TObject);
begin
    SaveMainConfig ;

    Close ;
end;

//
// When we click on Load from file for welcome message
procedure TMainConfigForm.WelcomeButtonClick(Sender: TObject);
begin
    if OpenDialog1.Execute
    then begin
        WelcomeMemo.Lines.LoadFromFile(OpenDialog1.FileName) ;
    end ;
end ;

//
// Read main configuration
procedure TMainConfigForm.ReadMainConfig ;
var
    liValue1 : Integer ;
    liValue2 : Integer ;
    lsValue : String ;
begin
    poMainConfig := TIniFile.Create(psConfigPath) ;
    poMainConfig.CaseSensitive := False ;
    poMainConfig.StripQuotes := True ;

    PortSpinEdit.Value := ConvertInteger(MAIN_CONF_PORT, DEFAULT_PORT) ;

    IpAddressLabeledEdit.Text := ConvertString(MAIN_CONF_IP_ADDRESS,
        DEFAULT_IP_ADDRESS) ;

    MaxSessionUserSpinEdit.Value := ConvertInteger(
        MAIN_CONF_MAX_SESSION_PER_USER, DEFAULT_MAX_SESSION_USER) ;

    MaxClientSpinEdit.Value := ConvertInteger(
        MAIN_CONF_MAX_CLIENT, DEFAULT_MAX_CLIENT) ;

    TimeOutSpinEdit.Value := ConvertInteger(MAIN_CONF_TIME_OUT,
        DEFAULT_TIME_OUT) ;

    FiletransfertTimeOutSpinEdit.Value := ConvertInteger(
        MAIN_CONF_FILE_TRANSFERT_TIME_OUT,
        DEFAULT_FILE_TRANSFERT_TIME_OUT) ;

    lsValue := ConvertString(MAIN_CONF_PASSIVE_PORT,
        DEFAULT_PASSIVE_PORT) ;

    liValue1 := 0 ;
    liValue2 := 0 ;

    ConvertPassivePort(lsValue, liValue1, liValue2) ;

    PassivePortStartSpinEdit.Value := liValue1 ;

    PassivePortStopSpinEdit.Value := liValue2 ;

    UserByteRateSpinEdit.Value := ConvertInteger(MAIN_CONF_USER_BYTE_RATE,
        DEFAULT_USER_BYTE_RATE) ;

    BufferSizeSpinEdit.Value := ConvertInteger(MAIN_CONF_BUFFER_SIZE,
        DEFAULT_BUFFER_SIZE) ;

    FullLogCheckBox.Checked := ConvertBoolean(MAIN_CONF_FULL_LOG,
        DEFAULT_FULL_LOG) ;

    Utf8CheckBox.Checked := ConvertBoolean(MAIN_CONF_UTF8, DEFAULT_UTF8) ;

    lsValue := ConvertString(MAIN_CONF_ALLOW_IP_ADRESS, DEFAULT_ALLOW_ADDRESS) ;

    StringToTStringListNoCreate(lsValue, ',', AllowedIpMemo.Lines) ;

    lsValue := ConvertString(MAIN_CONF_DENY_IP_ADRESS, DEFAULT_DENY_ADDRESS) ;

    StringToTStringListNoCreate(lsValue, IP_ADDRESS_SEPARATOR,
        DeniedIpMemo.Lines) ;

    DeniedPriorityCheckBox.Checked := ConvertBoolean(MAIN_CONF_DENY_PRIORITY,
        DEFAULT_DENY_PRIORITY) ;

    lsValue := ConvertString(MAIN_CONF_WELCOME_MESSAGE,
        DEFAULT_WELCOME_MESSAGE) ;

    StringToTStringListNoCreate(lsValue, MESSAGE_SEPARATOR, WelcomeMemo.Lines) ;

    lsValue := ConvertString(MAIN_CONF_GOODBYE_MESSAGE,
        DEFAULT_GOODBYE_MESSAGE) ;

    StringToTStringListNoCreate(lsValue, MESSAGE_SEPARATOR, GoodbyeMemo.Lines) ;

    LocalFolderConfigNameLabeledEdit.Text := ConvertString(psLocalConfigKey,
        psFolderLocalConfigName) ;

    poMainConfig.Free ;
end ;

//
// convert boolean to string
//
// @param value value
function BooleanToString(const value : Boolean) : String ;
begin
    if value
    then begin
        Result := YES_VALUE ;
    end
    else begin
        Result := NO_VALUE ;
    end;
end;

//
// Save main configuration
procedure TMainConfigForm.SaveMainConfig ;

begin
    poMainConfig := TIniFile.Create(psConfigPath) ;
    poMainConfig.CaseSensitive := False ;
    poMainConfig.StripQuotes := True ;

    poMainConfig.WriteInteger(psMainConfigSection, MAIN_CONF_PORT,
        PortSpinEdit.Value) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_IP_ADDRESS,
        IpAddressLabeledEdit.Text) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_MAX_SESSION_PER_USER, MaxSessionUserSpinEdit.Value) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_MAX_CLIENT, MaxClientSpinEdit.Value) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_TIME_OUT, TimeOutSpinEdit.Value) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_FILE_TRANSFERT_TIME_OUT, FiletransfertTimeOutSpinEdit.Value) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_PASSIVE_PORT,
        IntToStr(PassivePortStartSpinEdit.Value) + '-' +
        IntToStr(PassivePortStopSpinEdit.Value)) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_USER_BYTE_RATE, UserByteRateSpinEdit.Value) ;

    poMainConfig.WriteInteger(psMainConfigSection,
        MAIN_CONF_BUFFER_SIZE, BufferSizeSpinEdit.Value) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_FULL_LOG,
        BooleanToString(FullLogCheckBox.Checked)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_UTF8,
        BooleanToString(Utf8CheckBox.Checked)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_ALLOW_IP_ADRESS,
        ConvertTStringsToString(AllowedIpMemo.Lines, IP_ADDRESS_SEPARATOR)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_DENY_IP_ADRESS,
        ConvertTStringsToString(DeniedIpMemo.Lines, IP_ADDRESS_SEPARATOR)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_DENY_PRIORITY,
        BooleanToString(DeniedPriorityCheckBox.Checked)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_WELCOME_MESSAGE,
        ConvertTStringsToString(WelcomeMemo.Lines, MESSAGE_SEPARATOR)) ;

    poMainConfig.WriteString(psMainConfigSection, MAIN_CONF_GOODBYE_MESSAGE,
        ConvertTStringsToString(GoodbyeMemo.Lines, MESSAGE_SEPARATOR)) ;

    poMainConfig.WriteString(psMainConfigSection, psLocalConfigKey,
        LocalFolderConfigNameLabeledEdit.Text) ;

    poMainConfig.Free ;
end ;

//
// Convert TStrings to string
//
// @param aoList liste to join
// @param aoJoinString string to join
//
// @return string
function TMainConfigForm.ConvertTStringsToString(const aoList : TStrings;
    const aoJoinString : String) : String ;
var
    liCounter : Integer ;
begin
    Result := '' ;

    for liCounter := 0 to aoList.Count - 1 do
    begin
        if aoList[liCounter] <> ''
        then begin
            if liCounter <> 0
            then begin
                Result := Result + aoJoinString ;
            end ;

            Result := Result + aoList[liCounter] ;
        end ;
    end ;
end ;

//
// Convert integer from config
//
// @param asKey key config
// @param aiDefaultValue default value if asValue = ''
// @param aiConvertedValue value converted
//
// @return true if ok else false
function TMainConfigForm.ConvertInteger(const asKey : String;
    const aiDefaultValue : Integer) : Integer ;
var
    lsValue : String ;
begin
    Result := 0 ;

    lsValue := ReadMainConfig(asKey) ;

    if lsValue = ''
    then begin
        Result := aiDefaultValue ;
    end
    else begin
        if not TryIntParse(lsValue, Result)
        then begin
            Result := aiDefaultValue ;
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
function TMainConfigForm.ConvertString(asKey : String; asDefaultValue : String) : String ;
var
    lsValue : String ;
begin
    lsValue := ReadMainConfig(asKey) ;

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
function TMainConfigForm.ConvertBoolean(asKey : String; abDefaultValue : Boolean) : Boolean ;
var
    lsValue : String ;
begin
    lsValue := ReadMainConfig(asKey) ;

    if lsValue = ''
    then begin
        Result := abDefaultValue ;
    end
    else begin
        Result := (lsValue = YES_VALUE) ;
    end ;
end ;

//
// Read main config
//
// @param asKey key to read
//
// @return value or ''
function TMainConfigForm.ReadMainConfig(const asKey : String) : String ;
begin
    Result := poMainConfig.ReadString(psMainConfigSection, asKey, '') ;
end ;

initialization
  {$I mainconfig.lrs}

end.

