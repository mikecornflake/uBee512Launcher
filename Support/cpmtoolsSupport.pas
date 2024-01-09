Unit cpmtoolsSupport;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Types;

Function cpmtoolsAvailable: Boolean;
Function cpmtoolsPath: String;
Procedure SetcpmtoolsPath(AValue: String);
Procedure Initializecpmtools;
Function cpmtoolsIsDisk(AExt: String): Boolean;

Function cpmtoolsLS(AFilename: String; Var ARawOutput: String): String;
Function cpmtoolsLS(AFilename: String): String;

Implementation

Uses
  Forms, FileUtil, OSSupport, FileSupport, CPMSupport, StrUtils, Logging;

Var
  FPath: String;

Function cpmtoolsAvailable: Boolean;
Begin
  Result := (FPath <> '') And (DirectoryExists(FPath));
End;

Function cpmtoolsPath: String;
Begin
  Result := FPath;
End;

Procedure SetcpmtoolsPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FPath := IncludeSlash(AValue);
End;

Procedure Initializecpmtools;
Var
  sCPMToolsPath: String;
Begin
  Debug('Initializecpmtools');
  If FPath = '' Then
  Begin
    sCPMToolsPath := 'cpmtools-2.10' + DirectorySeparator + 'tools';

    // By default, use the folder distributed with the app
    FPath := IncludeTrailingBackslash(Application.Location) + sCPMToolsPath;
    If DirectoryExists(FPath) Then
      Exit;

    // How about up one level?
    FPath := IncludeTrailingBackslash(Application.Location) + '..' +
      DirectorySeparator + sCPMToolsPath;
    If DirectoryExists(FPath) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FPath := FindDefaultExecutablePath(Format('cpmchmod%s', [GetExeExt]));
    If DirectoryExists(FPath) Then
      Exit;

    FPath := '';
  End;

  If FPath <> '' Then
    FPath := IncludeSlash(FPath);

  Debug('detected cpmtools=' + FPath);
End;

Function cpmtoolsIsDisk(AExt: String): Boolean;
Var
  sExt: String;
Begin
  // TODO Find out how to determine which file extensions cpmtools supports
  sExt := Lowercase(Trim(AExt));

  Result := (sExt = '.dsk');
End;

Function cpmtoolsLS(AFilename: String; Var ARawOutput: String): String;
Var
  sCommand, sDSK, sTemp, sUser, sDebug: String;
  slTemp: TStringList;
  i: Integer;

  Function HasFile(AInput: String): Boolean;
  Var
    arrStrings: TStringDynArray;
  Begin
    // 0: -rw-rw-rw- 10496 Jan 01 1970  access.box
    Result := False;
    If AInput <> '' Then
    Begin
      arrStrings := SplitString(AInput, ' ');

      Result := (Length(arrStrings) = 7);
    End;
  End;

Begin
  Result := '';
  If FPath <> '' Then
  Begin
    Debug('cpmtoolsLS');
    sDSK := DSKFormat(AFilename);

    sCommand := Format('%scpmls%s', [IncludeSlash(FPath), GetExeExt]);
    sDebug := Format('"%scpmls%s" -l -f %s -T dsk "%s"', [IncludeSlash(FPath),
      GetExeExt, sDSK, AFilename]);
    Debug(sDebug);
    sTemp := RunEx(sCommand, ['-l', '-f', sDSK, '-T', 'dsk', AFilename], True, nil);
    Debug(LineEnding + sTemp);

    ARawOutput := '>' + sDebug + sLineBreak + sTemp;

    slTemp := TStringList.Create;
    Try
      slTemp.Text := sTemp;

      // Add user area and tidy string
      sUser := '';
      For i := 0 To slTemp.Count - 1 Do
      Begin
        sTemp := Trim(slTemp[i]);
        If (Length(sTemp) = 2) And (Copy(sTemp, 2, 1) = ':') Then
          sUser := sTemp
        Else
        Begin
          sTemp := Trim(slTemp[i]);

          While Pos('  ', sTemp) > 0 Do
            sTemp := ReplaceStr(sTemp, '  ', ' ');

          While Pos(Chr(127), sTemp) > 0 Do
            sTemp := ReplaceStr(sTemp, Chr(127), '');

          slTemp[i] := sUser + ' ' + sTemp;
        End;
      End;

      // Remove junk files
      //For i := slTemp.Count - 1 Downto 0 Do
      //Begin
      //  sTemp := Trim(slTemp[i]);
      //  If (sTemp = '') Or (Not HasFile(sTemp)) Then
      //    slTemp.Delete(i);
      //End;

      Result := slTemp.Text;
    Finally
      slTemp.Free;
    End;
  End;
End;

Function cpmtoolsLS(AFilename: String): String;
Var
  sTemp: String;
Begin
  sTemp := '';

  Result := cpmtoolsLS(AFilename, sTemp);
End;

Initialization
  FPath := '';

End.
