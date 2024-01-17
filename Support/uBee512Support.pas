Unit uBee512Support;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections, Validators;

Type
  TMbeeType = (mtFDD, mtROM, mtCustom);  // As built by Microbee

  { TSystemMacro }

  TSystemMacro = Class
  Private
    FValidators: TValidators;
  Public
    Macro: String;
    Model: String;
    MbeeType: TMbeeType;
    Title: String;
    A: String;
    Col: String;
    SRAM: String;
    SRAM_file: String;
    Status: String;
    Description: String;
    RC: String;

    Constructor Create;
    Destructor Destroy; Override;

    Property Validators: TValidators read FValidators;
  End;

  TModel = Class
  Public
    Model: String;
    MbeeType: TMbeeType;
  End;

  TSystemMacros = Specialize TObjectList<TSystemMacro>;
  TModels = Specialize TObjectList<TModel>;

  { TuBee512 }

  TuBee512 = Class
    FExe: String;
    FRC: String;
    FLoadedRC: String;
    FSystemMacros: TSystemMacros;
    FModels: TModels;

    Function GetExe: String;
    Procedure SetExe(AValue: String);

    Procedure LoadRC;
    Function GetRC: String;
    Procedure SetRC(AValue: String);
  Private
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Initialize;
    Function Available: Boolean;

    Function SystemMacros: TSystemMacros;
    Function Macro(ASystemMacro: String): TSystemMacro;
    Function MacroByTitle(ATitle: String): TSystemMacro;
    Function RCbyMacro(ASystemMacro: String): String;
    Function RCByTitle(ATitle: String): String;
    Function Models: String; // comma separated
    Function ModelsByType(AMbeeType: TMbeeType): String;
    Function Titles(AModel: String): String; // comma separated
    Function MbeeType(AModel: String): TMbeeType;
    Function IsDisk(AExt: String): Boolean;

    Property Exe: String read GetExe write SetExe;
    Property RC: String read GetRC write SetRC;
  End;

Const
  MBTypeStr: Array[TMbeeType] Of String = ('Disk', 'ROM', 'Custom');

Function uBee512: TuBee512;

Implementation

Uses
  Forms, FileUtil, OSSupport, StringSupport, Logging;

Var
  FuBee512: TuBee512;

Function uBee512: TuBee512;
Begin
  If Not Assigned(FuBee512) Then
    FuBee512 := TuBee512.Create;

  Result := FuBee512;
End;

{ TSystemMacro }

Constructor TSystemMacro.Create;
Begin
  FValidators := TValidators.Create(True);
  FValidators.Add(TSystemMacroValidator.Create);
End;

Destructor TSystemMacro.Destroy;
Begin
  FreeAndNil(FValidators);
  Inherited Destroy;
End;

  { TuBee512 }

Constructor TuBee512.Create;

  Procedure AddModel(AModel: String; AMBeeType: TMbeeType);
  Var
    oModel: TModel;
  Begin
    oModel := TModel.Create;
    oModel.Model := AModel;
    oModel.MbeeType := AMBeeType;

    FModels.Add(oModel);
  End;

Begin
  FExe := '';
  FRC := '';
  FLoadedRC := '';

  // OwnsObjects => No need for additional code to free contents
  FSystemMacros := TSystemMacros.Create(True);
  FModels := TModels.Create(True);

  // From ubee512 readme
  AddModel('1024k', mtFDD);
  AddModel('128k', mtFDD);
  AddModel('256k', mtFDD);
  AddModel('256tc', mtFDD);
  AddModel('2mhz', mtROM);
  AddModel('2mhzdd', mtFDD);
  AddModel('512k', mtFDD);
  AddModel('56k', mtFDD);
  AddModel('64k', mtFDD);
  AddModel('dd', mtFDD);
  AddModel('ic', mtROM);
  AddModel('p1024k', mtFDD);
  AddModel('p128k', mtFDD);
  AddModel('p256k', mtFDD);
  AddModel('p512k', mtFDD);
  AddModel('p64k', mtFDD);
  AddModel('pc', mtROM);
  AddModel('pc85', mtROM);
  AddModel('pc85b', mtROM);
  AddModel('pcf', mtCustom);
  AddModel('ppc85', mtROM);
  AddModel('scf', mtCustom);
  AddModel('tterm', mtROM);
End;

Destructor TuBee512.Destroy;
Begin
  Inherited Destroy;

  FreeAndNil(FModels);
  FreeAndNil(FSystemMacros);
End;

Function TuBee512.Available: Boolean;
Begin
  Result := (FExe <> '') And (FileExists(FExe));
End;

Function TuBee512.GetExe: String;
Begin
  Result := FExe;
End;

Function TuBee512.GetRC: String;
Begin
  Result := '';

  If FileExists(FRC) Then
    Result := FRC;
End;

Procedure TuBee512.SetExe(AValue: String);
Begin
  If FileExists(AValue) Then
    FExe := AValue;
End;

Procedure TuBee512.SetRC(AValue: String);
Begin
  If FileExists(AValue) Then
    FRC := AValue;
End;

Procedure TuBee512.Initialize;
Var
  sRC: String;

  Procedure GetExePath;
  Var
    sExe: String;
  Begin
    sExe := Format('ubee512%s', [GetExeExt]);

    // By default, use the folder distributed with the app
    FExe := IncludeTrailingBackslash(Application.Location) + sExe;
    If FileExists(FExe) Then
      Exit;

    // How about the folder above?
    FExe := IncludeTrailingBackslash(Application.Location) + '..' + DirectorySeparator + sExe;
    If FileExists(FExe) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FExe := FindDefaultExecutablePath(sExe);
    If FileExists(FExe) Then
      Exit;

    FExe := '';
  End;

Begin
  Debug('TuBee512.Initialize');

  If FExe = '' Then
  Begin
    FExe := '';

    GetExePath;
  End;

  If (FRC = '') And (FExe <> '') Then
  Begin
    sRC := 'ubee512rc';

    // Look in the same folder as ubee512
    FRC := IncludeTrailingBackslash(ExtractFileDir(FExe)) + sRC;
    If Not FileExists(FRC) Then
    Begin
      // OK, lets find the users home directory
      FRC := IncludeTrailingBackslash(GetUserDir) + '.ubee512' + DirectorySeparator + sRC;
      If Not FileExists(FRC) Then
      Begin
        // Last chance, is it on the PATH?
        FRC := FindDefaultExecutablePath(sRC);
        If Not FileExists(FRC) Then
          FRC := '';
      End;
    End;
  End;

  Debug('detected ubee512=' + FExe);
  Debug('detected ubee512rc=' + FRC);
End;

Function TuBee512.SystemMacros: TSystemMacros;
Begin
  Result := FSystemMacros;
End;

Procedure TuBee512.LoadRC;
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
  If (FLoadedRC = FRC) Then
  Begin
    Debug('Cancel loading ubee512rc.  Already loaded.');
    Exit;
  End;

  // And only try to load it if we know where the file is
  If FileExists(FRC) Then
  Begin
    SetBusy;
    Debug('Start loading ' + FRC);
    slTemp := TStringList.Create;
    FSystemMacros.Clear;

    //FSystemMacros.Clear;
    Try
      slTemp.LoadFromFile(FRC);
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
                // Start a new System Macro
                sTag := sNewTag;
                oSystemMacro := TSystemMacro.Create;
                oSystemMacro.Macro := sTag;
                oSystemMacro.Description := sDescription;
                FSystemMacros.Add(oSystemMacro);
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
                Else If sProperty = 'model' Then
                Begin
                  oSystemMacro.Model := sValue;
                  oSystemMacro.MbeeType := MbeeType(sValue);
                End
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
      Debug(Format('End loading %s.  %d Macros loaded', [FRC, FSystemMacros.Count]));
      ClearBusy;

      FLoadedRC := FRC;
    End;
  End;
End;

Function TuBee512.Macro(ASystemMacro: String): TSystemMacro;
Var
  oMacro: TSystemMacro;
Begin
  Result := nil;

  For oMacro In FSystemMacros Do
    If (oMacro.Macro = ASystemMacro) Then
    Begin
      Result := oMacro;
      Break;
    End;
End;

Function TuBee512.RCbyMacro(ASystemMacro: String): String;
Var
  oMacro: TSystemMacro;
Begin
  Result := '';
  oMacro := Macro(ASystemMacro);
  If Assigned(oMacro) Then
    Result := oMacro.RC;
End;

Function TuBee512.MacroByTitle(ATitle: String): TSystemMacro;
Var
  oMacro: TSystemMacro;
Begin
  Result := nil;

  For oMacro In FSystemMacros Do
    If (oMacro.Title = ATitle) Then
    Begin
      Result := oMacro;
      Break;
    End;
End;

Function TuBee512.RCByTitle(ATitle: String): String;
Var
  oMacro: TSystemMacro;
Begin
  Result := '';

  oMacro := MacroByTitle(ATitle);
  If Assigned(oMacro) Then
    Result := oMacro.RC;
End;

// Return a comma seperated sorted list of Microbee Models defined in the uBee512RC
Function TuBee512.Models: String;
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

// Return a comma seperated list of Microbee Models by Type
Function TuBee512.ModelsByType(AMbeeType: TMbeeType): String;
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
      If (Trim(oMacro.Model) <> '') And (oMacro.MbeeType = AMbeeType) Then
        slModels.Add(oMacro.Model);

    Result := slModels.CommaText;
  Finally
    slModels.Free;
  End;
End;

// Return a comma seperated list of Microbee systems defined in uBee512rc for
// a particular Microbee Model
Function TuBee512.Titles(AModel: String): String;
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

Function TuBee512.MbeeType(AModel: String): TMbeeType;
Var
  oModel: TModel;
  sModel: String;
Begin
  Result := mtCustom;
  sModel := Lowercase(AModel);
  For oModel In FModels Do
    If Lowercase(oModel.Model) = sModel Then
    Begin
      Result := oModel.MbeeType;
      Break;
    End;
End;

Function TuBee512.IsDisk(AExt: String): Boolean;
Var
  sExt: String;
Begin
  // TODO Implement uBee512 supported formats
  sExt := Lowercase(Trim(AExt));

  Result := (sExt = '.dsk');
End;

Initialization
  FuBee512 := nil;

Finalization
  FreeAndNil(FuBee512);

End.
