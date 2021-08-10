unit aboutform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ftpversion, ftpmain ;

type

  { TAboutForm2 }

  TAboutForm2 = class(TForm)
    Button1: TButton;
    CompilationDateTimeLabel: TLabel;
    CopyrightLabel: TLabel;
    FreePascalVersionLabel: TLabel;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    CompiledWithLabel: TLabel;
    ServerTypeLabel: TLabel;
    PowerByLabel: TLabel;
    TitleLabel: TLabel;
    VersionLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    pbUpdated : Boolean ;
  public
    { public declarations }
  end; 

var
  AboutForm2: TAboutForm2;

implementation

{ TAboutForm2 }

procedure TAboutForm2.Button1Click(Sender: TObject);
begin
    Close ;
end;

procedure TAboutForm2.FormCreate(Sender: TObject);
begin
    pbUpdated := False ;
end;

procedure TAboutForm2.FormShow(Sender: TObject);
var
    loLabel : TLabel ;
    lsDate : String ;
begin

    if not pbUpdated
    then begin
        loLabel := (GroupBox1.ControlByName('CompiledWithLabel') as TLabel) ;
        loLabel.Caption := loLabel.Caption + '0.98.2' ;

        loLabel := (GroupBox1.ControlByName('FreePascalVersionLabel') as TLabel) ;
        loLabel.Caption := loLabel.Caption + {$I %FPCVERSION%} ;

        lsDate := {$I %date%} + ' ' + {$I %time%} ;

        lsDate[5] := '-' ;
        lsDate[8] := '-' ;

        loLabel := (GroupBox1.ControlByName('CompilationDateTimeLabel') as TLabel) ;
        loLabel.Caption := loLabel.Caption + lsDate ;

        VersionLabel.Caption := VersionLabel.Caption + FTP_VERSION ;

        ServerTypeLabel.Caption := ServerTypeLabel.Caption + TFtpMain.GetServerType ;

        pbUpdated := True ;
    end ;
end;

initialization
  {$I aboutform.lrs}

end.

