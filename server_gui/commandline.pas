unit commandline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TCommandLineForm }

  TCommandLineForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CommandLineForm: TCommandLineForm;

implementation

{ TCommandLineForm }

procedure TCommandLineForm.Button1Click(Sender: TObject);
begin
    Close ;
end;

initialization
  {$I commandline.lrs}

end.

