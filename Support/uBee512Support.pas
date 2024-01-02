Unit uBee512Support;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Function uBee512Available: Boolean;
Function uBee512Path: String;
Procedure SetuBee512Path(AValue: String);
Procedure InitializeuBee512;

Implementation

Uses
  Forms, FileUtil, FileSupport;

Var
  FPath: String;

Function uBee512Available: Boolean;
Begin
  Result := (FPath <> '') And (FileExists(FPath));
End;

Function uBee512Path: String;
Begin
  Result := FPath;
End;

Procedure SetuBee512Path(AValue: String);
Begin
  If FileExists(AValue) Then
    FPath := AValue;
End;

Procedure InitializeuBee512;
Var
  sExe: String;
Begin
  If FPath = '' Then
  Begin
    sExe := Format('ubee512%s', [GetExeExt]);

    // By default, use the folder distributed with the app
    FPath := IncludeTrailingBackslash(Application.Location) + sExe;
    If FileExists(FPath) Then
      Exit;

    // By default, use the folder distributed with the app
    FPath := IncludeTrailingBackslash(Application.Location) + sExe;
    If FileExists(FPath) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FPath := FindDefaultExecutablePath(sExe);
    If FileExists(FPath) Then
      Exit;

    FPath := '';
  End;

  If FPath <> '' Then
    FPath := FPath;
End;

Initialization

End.
