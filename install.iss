[Setup]
AppName=PPFtp Server
AppVerName=PPFtp Server 0.7.0
DefaultDirName={pf}\ppftp_server
DefaultGroupName=PPFtp Server
UninstallDisplayIcon={uninstallexe}
LicenseFile=license
WizardImageFile=WizModernImage-IS.bmp
WizardSmallImageFile=WizModernSmallImage-IS.bmp
LanguageDetectionMethod=none
OutPutDir=".\"
OutputBaseFilename="ppftp_server-0.7.0"

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"

[Types]
; Type d'installation
Name: "standard"; Description: "Standard installation";
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "program"; Description: "Base core"; Types: standard custom;

[Files]
Source: "product\gui.exe"; DestDir: "{app}"; DestName: "ppftpserver_gui.exe"; Components: program;
Source: "product\ftpserver.exe"; DestDir: "{app}"; DestName: "ppftpserver_console.exe"; Components: program;
Source: "license"; DestDir: "{app}"; DestName: "license.txt"; Components: program;
Source: "product\ftp.access"; DestDir: "{app}"; Components: program;
Source: "product\README"; DestDir: "{app}"; DestName: "README.txt"; Components: program;
Source: "product\ppftpconf.ini"; DestDir: "{app}"; Components: program;
Source: "product\users\anonymous.ini"; DestDir: "{app}\users"; Components: program;
Source: "product\users\emeric.ini"; DestDir: "{app}\users"; Components: program;

[Icons]
Name: "{group}\PPFtpServer (GUI)"; Filename: "{app}\ppftpserver_gui.exe"; WorkingDir: "{app}"
Name: "{group}\PPFtpServer (Console)"; Filename: "{app}\ppftpserver_console.exe"; WorkingDir: "{app}"
Name: "{group}\Readme"; Filename: "{app}\README.txt"; WorkingDir: "{app}"
Name: "{group}\Licence"; Filename: "{app}\license.txt"; WorkingDir: "{app}"

