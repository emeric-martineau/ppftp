program gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, maingui, LResources, commandline, aboutform
  { you can add units after this };

{$IFDEF WINDOWS}{$R gui.rc}{$ENDIF}

begin
  {$I gui.lrs}
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

