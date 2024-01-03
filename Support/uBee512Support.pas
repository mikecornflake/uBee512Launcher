Unit uBee512Support;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections;

Type
  TSystemMacro = Class
  Public
    Macro: String;
    Model: String;
    Title: String;
    A: String;
    Col: String;
    SRAM: String;
    SRAM_file: String;
    Status: String;
    Description: String;
    RC: String;
  End;

  TSystemMacros = Specialize TObjectList<TSystemMacro>;

// You know, this is really beginning to feel like a class...
Procedure InitializeuBee512;
Function uBee512Available: Boolean;
Function uBee512Path: String;
Function uBee512RCPath: String;
Procedure SetuBee512Path(AValue: String);
Function uBee512SystemMacros: TSystemMacros;
Procedure uBee512LoadRC;
Function uBee512MacroRC(ASystemMacro: String): String;
Function uBee512MacroRCByTitle(ASystemTitle: String): String;
Function uBee512Models: String;
Function uBee512Titles(AModel: String): String;

Implementation

Uses
  Forms, FileUtil, OSSupport, StringSupport, FileSupport;

Var
  FPath: String;
  FSystemMacros: TSystemMacros;
  FLoadedRC: String;

Function uBee512Available: Boolean;
Begin
  Result := (FPath <> '') And (FileExists(FPath));
End;

Function uBee512Path: String;
Begin
  Result := FPath;
End;

Function uBee512RCPath: String;
Var
  sFolder: String;
Begin
  Result := '';

  If FileExists(FPath) Then
  Begin
    sFolder := IncludeSlash(ExtractFileDir(FPath));
    If FileExists(sFolder + 'ubee512rc') Then
      Result := sFolder + 'ubee512rc';
  End;
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

    // How about the folder above?
    FPath := IncludeTrailingBackslash(Application.Location) + '..' + DirectorySeparator + sExe;
    If FileExists(FPath) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FPath := FindDefaultExecutablePath(sExe);
    If FileExists(FPath) Then
      Exit;

    FPath := '';
  End;
End;

Function uBee512SystemMacros: TSystemMacros;
Begin
  Result := FSystemMacros;
End;

Procedure uBee512LoadRC;
Var
  slTemp: TStringList;
  s, sLine, sTag, sNewTag: String;
  sDescription: String;
  sProperty, sValue: String;
  c1, c2: Char;
  oSystemMacro: TSystemMacro;
  iCount: Integer;
Begin
  // Only load the file once
  If (FLoadedRC = uBee512RCPath) Then
    Exit;

  // And only try to load it if we know where the file is
  If FileExists(uBee512RCPath) Then
  Begin
    SetBusy;
    slTemp := TStringList.Create;
    FSystemMacros.Clear;
    Try
      slTemp.LoadFromFile(uBee512RCPath);
      oSystemMacro := nil;
      sTag := '';
      sDescription := '';
      iCount := 0;

      For s In slTemp Do
      Begin
        sLine := Trim(s);

        If Length(sLine) > 2 Then
        Begin
          c1 := sLine[1];
          c2 := sLine[2];

          Case c1 Of
            '#':
              If c2 = '=' Then
              Begin
                If Assigned(oSystemMacro) And (oSystemMacro.Macro <> '') And (iCount = 0) Then
                Begin
                  sDescription := '';
                  iCount := 1;
                End;
              End
              Else
                sDescription := Trim(sDescription + ' ' + TrimChars(sLine, ['#', ' ']));
            '[':
            Begin
              sNewTag := TextBetween(sLine, '[', ']');

                // Exclude non-system Tags
              If (sNewTag = 'global-start') Or (sNewTag = 'global-end') Or
                (sNewTag = 'list') Or (sNewTag = 'listall') Then
              Begin
                sTag := '';
                sDescription := '';
              End
              Else If (sTag <> sNewTag) Then
              Begin
                // Save the previous system macro, we're starting a new one
                If Assigned(oSystemMacro) Then
                  FSystemMacros.Add(oSystemMacro);

                // And now we're starting a new System Macro
                sTag := sNewTag;
                oSystemMacro := TSystemMacro.Create;
                oSystemMacro.Macro := sTag;
                oSystemMacro.Description := sDescription;
                iCount := 0;
              End;
            End;
            '-': If (sTag <> '') And Assigned(oSystemMacro) Then
              Begin
                If oSystemMacro.RC = '' Then
                  oSystemMacro.RC := sLine
                Else
                  oSystemMacro.RC := oSystemMacro.RC + sLineBreak + sLine;

                If (Pos('--', sLine) > 0) Then
                Begin
                  If (Pos('=', sLine) > 0) Then
                  Begin
                    sProperty := Lowercase(Trim(TextBetween(sLine, '--', '=')));
                    sValue := Trim(TextBetween(sLine, '=', ''));
                  End
                  Else
                  Begin
                    sProperty := Lowercase(Trim(TextBetween(sLine, '--', '')));
                    sValue := '';
                  End;
                End
                Else
                Begin
                  sProperty := Lowercase(Trim(TextBetween(sLine, '-', ' ')));
                  sValue := Trim(TextBetween(sLine, ' ', ''));
                End;

                If sProperty = 'a' Then oSystemMacro.A := sValue
                Else If sProperty = 'col' Then oSystemMacro.Col := 'Colour'
                Else If sProperty = 'monitor' Then
                  If sValue = 'a' Then
                    oSystemMacro.Col := 'Amber'
                  Else
                    oSystemMacro.Col := sValue
                Else If sProperty = 'model' Then oSystemMacro.Model := sValue
                Else If sProperty = 'sram' Then oSystemMacro.SRAM := sValue
                Else If sProperty = 'sram-file' Then oSystemMacro.SRAM_file := sValue
                Else If sProperty = 'status' Then oSystemMacro.Status := sValue
                Else If sProperty = 'title' Then oSystemMacro.Title := TrimChars(sValue, ['"']);
              End;
          End;
        End;
      End;
    Finally
      slTemp.Free;
      ClearBusy;

      FLoadedRC := uBee512RCPath;
    End;
  End;
End;

Function uBee512MacroRC(ASystemMacro: String): String;
Var
  oMacro: TSystemMacro;
Begin
  Result := '';
  For oMacro In FSystemMacros Do
    If (oMacro.Macro = ASystemMacro) Then
    Begin
      Result := oMacro.RC;
      Break;
    End;
End;

Function uBee512MacroRCByTitle(ASystemTitle: String): String;
Var
  oMacro: TSystemMacro;
Begin
  Result := '';
  For oMacro In FSystemMacros Do
    If (oMacro.Title = ASystemTitle) Then
    Begin
      Result := oMacro.RC;
      Break;
    End;
End;

// Return a comma seperated sorted list of Microbee Models defined in the uBee512RC
Function uBee512Models: String;
Var
  slModels: TStringList;
  oMacro: TSystemMacro;
Begin
  slModels := TStringList.Create;
  Try
    // use the TStringlist to build up a sorted, unique list of models
    slModels.Sorted := True;
    slModels.Duplicates := dupIgnore;

    For oMacro In FSystemMacros Do
      If Trim(oMacro.Model) <> '' Then
        slModels.Add(oMacro.Model);

    Result := slModels.CommaText;
  Finally
    slModels.Free;
  End;
End;

// Return a comma seperated list of Microbee systems defined in uBee512rc for
// a particular Microbee Model
Function uBee512Titles(AModel: String): String;
Var
  slTitles: TStringList;
  oMacro: TSystemMacro;
  sModel: String;
Begin
  sModel := LowerCase(AModel);
  slTitles := TStringList.Create;
  Try
    // use the TStringlist to build up a sorted, unique list of models
    slTitles.Sorted := True;
    slTitles.Duplicates := dupIgnore;

    For oMacro In FSystemMacros Do
      If Lowercase(oMacro.Model) = sModel Then
        If Trim(oMacro.Title) <> '' Then
          slTitles.Add(oMacro.Title);

    Result := slTitles.CommaText;
  Finally
    slTitles.Free;
  End;
End;

Initialization
  FPath := '';
  FLoadedRC := '';

  FSystemMacros := TSystemMacros.Create(True);
  // OwnsObjects => No need for additional code to free contents

Finalization
  FreeAndNil(FSystemMacros);

End.
