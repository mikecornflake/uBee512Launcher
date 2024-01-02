Program MichaelBeeTools;

{$mode objfpc}{$H+}

Uses {$IFDEF UNIX}
  Cthreads, {$ENDIF} {$IFDEF HASAMIGA}
  Athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  FormMain,
  CPMSupport,
  cpmtoolsSupport,
  StringSupport,
  FileSupport,
  OSSupport,
  FormSettings,
  uBee512Support { you can add units after this };

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Title := 'MichaelBee Tools';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
End.
