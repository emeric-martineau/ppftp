unit user;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditBtn, Spin, LCLType, IniFiles, FtpFunctions, FtpConst ;

type

  { TUserForm }

  TUserForm = class(TForm)
    DeleteButton: TButton;
    SaveButton: TButton;
    CancelButton: TButton;
    DisabledCheckBox: TCheckBox;
    DisabledLabel: TLabel;
    RightsCheckGroup: TCheckGroup;
    CreateDirectoryCheckBox: TCheckBox;
    DeleteCheckBox: TCheckBox;
    DeleteDirectoryCheckBox: TCheckBox;
    DownloadCheckBox: TCheckBox;
    HomeDirectoryEdit: TDirectoryEdit;
    HomeDirectoryLabel: TLabel;
    ModifyFileTimeCheckBox: TCheckBox;
    PasswordLabeledEdit: TLabeledEdit;
    RenameCheckBox: TCheckBox;
    Shape1: TShape;
    SubDirCheckBox: TCheckBox;
    UploadCheckBox: TCheckBox;
    UserByteRateLabel: TLabel;
    UserByteRateSpinEdit: TSpinEdit;
    UsersListBox: TListBox;
    UsersLabel: TLabel;
    procedure DeleteButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure UsersListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    // If config is read
    pbConfigRead : Boolean ;
    // Read list of user
    procedure ReadListUsers ;
    // Read user config
    procedure ReadConfigUser ;
    // Write user config
    procedure WriteConfigUser ;
    // Convert boolean to yes/no value
    function YesNoValue(const abValue : Boolean) : String ;
  public
    { public declarations }
    psUsersDirectory : String ;
    psUserSection : String ;
  end; 

var
  UserForm: TUserForm;

implementation

{ TUserForm }

const
    ANONYMOUS_USER : String = 'anonymous' ;

//
// Constructor
procedure TUserForm.FormCreate(Sender: TObject);
begin
    pbConfigRead := False ;
end;

procedure TUserForm.CancelButtonClick(Sender: TObject);
begin
    Close ;
end;

//
// Click on delete button
procedure TUserForm.DeleteButtonClick(Sender: TObject);
var
    // Name of selected user
    lsUser : String ;
    // Index of selected user
    liIndex : Integer ;
begin
    if UsersListBox.ItemIndex <> -1
    then begin
        lsUser := UsersListBox.Items[UsersListBox.ItemIndex] ;

        if LowerCase(lsUser) = ANONYMOUS_USER
        then begin
            Application.MessageBox('You can''t delete anonymous user. Disabled it !',
                'Error', MB_ICONERROR or MB_OK) ;
        end
        else if Application.MessageBox(PChar(Format('Are you sur you want delete %s ?',
            [lsUser])),
            'Delete user', MB_ICONQUESTION or MB_YESNO) = IDYES
        then begin
            if DeleteFile(psUsersDirectory + lsUser + '.ini')
            then begin
                liIndex := UsersListBox.ItemIndex ;

                UsersListBox.Items.Delete(liIndex) ;

                // Select next user in list
                if liIndex = (UsersListBox.Items.Count)
                then begin
                    // Don't use Count - 1, cause we avec delete a index
                    // Last user
                    UsersListBox.ItemIndex := UsersListBox.Items.Count - 1 ;
                end
                else begin
                    UsersListBox.ItemIndex := liIndex ;
                end ;

                UsersListBox.SetFocus ;

                //ReadConfigUser ;
            end
            else begin
                Application.MessageBox('Can delete user file !',
                    'Error', MB_ICONERROR or MB_OK) ;
            end ;
        end ;
    end ;
end;

//
// When form show
procedure TUserForm.FormShow(Sender: TObject);
begin
    if not pbConfigRead
    then begin
        ReadListUsers ;
    end ;
end;

procedure TUserForm.SaveButtonClick(Sender: TObject);
begin
    WriteConfigUser ;
end;

procedure TUserForm.UsersListBoxSelectionChange(Sender: TObject; User: boolean);
begin
    ReadConfigUser ;
end;

//
// Read list of users
procedure TUserForm.ReadListUsers ;
var
    // Result of find
    liFindResult : Integer ;
    // Search structure
    lrSearchRec : TSearchRec ;
    // Find file
    lsFindFilePath : String ;
begin
    psUsersDirectory := AddTrailing(psUsersDirectory, DirectorySeparator,
        false) ;

    lsFindFilePath := psUsersDirectory + '*.*' ;

    liFindResult := FindFirst(lsFindFilePath, faanyfile, lrSearchRec) ;

    while liFindResult = 0 do
    begin
        if ((lrSearchRec.Attr and faHidden) = 0)
            and ((lrSearchRec.Attr and faSysFile) = 0)
            and ((lrSearchRec.Attr and faVolumeID) = 0)
        then begin
            if (lrSearchRec.Attr and faDirectory) = 0
            then begin
                UsersListBox.Items.Add(
                    ExtractFileNameWithoutExt(lrSearchRec.Name)) ;
            end ;
        end ;

        liFindResult := FindNext(lrSearchRec) ;
    end ;

    FindClose(lrSearchRec) ;

    UsersListBox.Sorted := True ;
end ;

//
// Read config user
procedure TUserForm.ReadConfigUser;
var
    // Ini file
    loUserConfig : TIniFile ;
    // File to read
    lsFile : String ;
    // Name of selected user
    lsUser : String ;
    // Byte rate
    liByteRate : Integer ;
    lsByteRate : String ;
begin
    lsUser := UsersListBox.Items[UsersListBox.ItemIndex] ;

    lsFile := ExpandFileName(psUsersDirectory + lsUser + '.ini') ;

    loUserConfig := TIniFile.Create(lsFile) ;

    loUserConfig.CaseSensitive := False ;
    loUserConfig.StripQuotes := True ;

    HomeDirectoryEdit.Text := loUserConfig.ReadString(psUserSection,
        USER_CONF_ROOT, DEFAULT_USER_ROOT) ;

    DownloadCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
         USER_CONF_DOWNLOAD, DEFAULT_USER_DOWNLOAD) = YES_VALUE) ;

    UploadCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_UPLOAD, DEFAULT_USER_UPLOAD) = YES_VALUE) ;

    RenameCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_RENAME, DEFAULT_USER_RENAME) = YES_VALUE) ;

    DeleteCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_DELETE, DEFAULT_USER_DELETE) = YES_VALUE) ;

    CreateDirectoryCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_MAKE_DIRECTORY, DEFAULT_USER_MAKE_DIRECTORY) = YES_VALUE) ;

    DeleteDirectoryCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_DELETE_DIRECTORY, DEFAULT_USER_DELETE_DIRECTORY) = YES_VALUE) ;

    SubDirCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_SUB_DIR, DEFAULT_USER_SUB_DIR) = YES_VALUE) ;

    ModifyFileTimeCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_MODIFY_FILE_TIME, DEFAULT_USER_MODIFY_FILE_TIME) = YES_VALUE) ;

    DisabledCheckBox.Checked := (loUserConfig.ReadString(psUserSection,
        USER_CONF_DISABLED, DEFAULT_USER_DISABLED) = YES_VALUE) ;

    lsByteRate := loUserConfig.ReadString(psUserSection, USER_CONF_BYTE_RATE,
        DEFAULT_USER_CONF_BYTE_RATE) ;

    liByteRate := DEFAULT_USER_BYTE_RATE ;

    TryIntParse(lsByteRate, liByteRate) ;

    UserByteRateSpinEdit.Value := liByteRate ;

    PasswordLabeledEdit.Text := '******' ;

    PasswordLabeledEdit.Modified := False ;

    if LowerCase(lsUser) = ANONYMOUS_USER
    then begin
        PasswordLabeledEdit.Visible := False ;
    end
    else begin
        PasswordLabeledEdit.Visible := True ;
    end ;

    loUserConfig.Free ;
end ;

//
// Convert boolean to yes/no value
//
// @param abValue value to convert
//
// @return 'yes' or 'no'
function TUserForm.YesNoValue(const abValue : Boolean) : String ;
begin
    if abValue
    then begin
        Result := YES_VALUE ;
    end
    else begin
        Result := NO_VALUE ;
    end;
end ;

//
// Write config user
procedure TUserForm.WriteConfigUser;
var
    // Ini file
    loUserConfig : TIniFile ;
    // File to read
    lsFile : String ;
    // Name of selected user
    lsUser : String ;
begin
    lsUser := UsersListBox.Items[UsersListBox.ItemIndex] ;

    lsFile := ExpandFileName(psUsersDirectory + lsUser + '.ini') ;

    loUserConfig := TIniFile.Create(lsFile) ;

    loUserConfig.CaseSensitive := False ;
    loUserConfig.StripQuotes := True ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_ROOT, HomeDirectoryEdit.Text) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_DOWNLOAD, YesNoValue(DownloadCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_UPLOAD, YesNoValue(UploadCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_RENAME, YesNoValue(RenameCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_DELETE, YesNoValue(DeleteCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_MAKE_DIRECTORY, YesNoValue(CreateDirectoryCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_DELETE_DIRECTORY, YesNoValue(DeleteDirectoryCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_SUB_DIR, YesNoValue(SubDirCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_MODIFY_FILE_TIME, YesNoValue(ModifyFileTimeCheckBox.Checked)) ;

    loUserConfig.WriteString(psUserSection,
        USER_CONF_DISABLED, YesNoValue(DisabledCheckBox.Checked)) ;

    loUserConfig.WriteInteger(psUserSection,
        USER_CONF_BYTE_RATE, UserByteRateSpinEdit.Value) ;

    if PasswordLabeledEdit.Modified
    then begin
        loUserConfig.WriteString(psUserSection,
            USER_CONF_PASSWORD, MD5(PasswordLabeledEdit.Text)) ;

        PasswordLabeledEdit.Modified := False ;
    end;

    loUserConfig.Free ;
end ;

initialization
  {$I user.lrs}

end.

