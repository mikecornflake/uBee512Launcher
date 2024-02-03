Program uBee512Launcher;

{$mode objfpc}{$H+}

Uses {$IFDEF UNIX}
  Cthreads, {$ENDIF} {$IFDEF HASAMIGA}
  Athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormMain, DialogDebug, uBee512Support, 
  uBee512Validators { you can add units after this };

{$R *.res}

Begin
  {$IFDEF DEBUG}
  SetHeapTraceOutput('HeapTrace.log');
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdlgDebug, dlgDebug);
  Application.Run;
End.
