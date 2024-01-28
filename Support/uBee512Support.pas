Unit uBee512Support;

{$mode ObjFPC}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
Interface

Uses
  Classes, SysUtils, Generics.Collections, Validators, uBee512Validators;

Type
  TMbeeType = (mtFDD, mtROM, mtCustom);  // As built by Microbee

  // A Definition is a [] entry inside ubee512rc)
  TDefinition = Class
  Private
    FValidator: TDefinitionValidator;
  Public
    Definition: String;
    Model: String;
    MbeeType: TMbeeType;
    Title: String;
    A, B, C: String;
    Col: String;
    SRAM: String;
    SRAM_file: String;
    IDE: String;
    TapeI, TapeO: String;
    Description: String;
    RC: String;

    Constructor Create;
    Destructor Destroy; Override;

    Property Validator: TDefinitionValidator read FValidator;
  End;

  TDefinitions = Class(Specialize TObjectList<TDefinition>)
  Private
    Function GetDefinition(ADefinition: String): TDefinition;
  Public
    Function DefinitionByTitle(ATitle: String): TDefinition;
    Function RCbyDefinition(ADefinition: String): String;
    Function RCByTitle(ATitle: String): String;
    Function Models: String; // comma separated
    Function ModelsByType(AMbeeType: TMbeeType): String;
    Function Titles(AModel: String): String; // comma separated

    Property Definition[ADefinition: String]: TDefinition read GetDefinition; Default;
  End;

  // Hard coded list of Microbee Models (official and homebrew)
  TModel = Class
  Public
    Model: String;
    Description: String;
    MbeeType: TMbeeType;
  End;

  TModels = Class(Specialize TObjectList<TModel>)
  Private
    Function GetModel(AModel: String): TModel;
  Public
    Function MbeeType(AModel: String): TMbeeType;
    Property Model[AModel: String]: TModel read GetModel; Default;
  End;

  // Entry within "disks.alias"
  TDiskAlias = Class
  Private
    FValidator: TDiskAliasValidator;
  Public
    // Next two for lines with aliases
    Alias: String;
    Filename: String;

    // For blank lines or comments
    Line: String;

    Constructor Create;
    Destructor Destroy; Override;

    Property Validator: TDiskAliasValidator read FValidator;
  End;

  // All entries within "disks.alias"

  { TDiskAliases }

  TDiskAliases = Class(Specialize TObjectList<TDiskAlias>)
  Private
    FFilename: String;
    FValidators: TValidators;
  Public
    Constructor Create(AOwnsObjects: Boolean);
    Destructor Destroy; Override;

    Function Load: Boolean;
    Function Save: Boolean;

    Function FilenameByAlias(AAlias: String): String;

    Function ValidAliases: TStringArray;

    Property Filename: String read FFilename;
    Property Validators: TValidators read FValidators;
  End;

  { TuBee512 }

  TuBee512 = Class
  Private
    FExe: String;
    FRC: String;
    FLoadedRC: String;
    FDefinitions: TDefinitions;
    FModels: TModels;

    FDisksAliases: TDiskAliases;

    Function GetExe: String;
    Procedure SetExe(AValue: String);

    Function GetRC: String;
    Procedure SetRC(AValue: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Initialize;        // Try to detect ubee512 binary/rc
    Function Available: Boolean; // Has the ubee512 binary been found?
    Property Exe: String read GetExe write SetExe; // ubee512 binary location
    Function WorkingDir: String; // Location of ubee512 "home dir" (changes by OS)

    // ubee512rc routines
    Property RC: String read GetRC write SetRC;  // ubee512rc location
    Function LoadRC: Boolean;                    // load (pseudo parse) ubee512rc

    // Helper Routines
    Function IsDisk(AExt: String): Boolean;
    Function ValidFile(ASubfolder: String; AFilename: String): Boolean;

    Property DiskAliases: TDiskAliases read FDisksAliases;  // Contents of "disks.alias"
    Property Definitions: TDefinitions read FDefinitions;   // entries within ubee512rc
    Property Models: TModels read FModels;                  // Hard coded list
  End;

Const
  MBTypeStr: Array[TMbeeType] Of String = ('Disk', 'ROM', 'Custom');
  ALIAS_NOT_FOUND = '!ALIAS_NOT_FOUND';

Function uBee512: TuBee512;

Implementation

Uses
  Forms, FileUtil, FileSupport, OSSupport, StringSupport, StrUtils, Logs;

Var
  FuBee512: TuBee512;

Function uBee512: TuBee512;
Begin
  If Not Assigned(FuBee512) Then
    FuBee512 := TuBee512.Create;

  Result := FuBee512;
End;

{ TDiskAlias }

Constructor TDiskAlias.Create;
Begin
  Inherited Create;

  FValidator := TDiskAliasValidator.Create(Self);
End;

Destructor TDiskAlias.Destroy;
Begin
  FreeAndNil(FValidator);

  Inherited Destroy;
End;

{ TDiskAliases }

Constructor TDiskAliases.Create(AOwnsObjects: Boolean);
Begin
  Inherited Create(AOwnsObjects);

  // Create with False as this list doesn't own each TValidator
  FValidators := TValidators.Create(False);
End;

Destructor TDiskAliases.Destroy;
Begin
  FreeAndNil(FValidators);

  Inherited Destroy;
End;

Function TDiskAliases.Load: Boolean;
Var
  oItem: TDiskAlias;
  oAliases: TStringList;
  sLine, sTemp: String;
Begin
  Result := False;
  Clear;
  FValidators.Clear;

  FFilename := IncludeSlash(uBee512.WorkingDir) + 'disks.alias';
  If FileExists(FFilename) Then
  Begin
    oAliases := TStringList.Create;
    Try
      oAliases.LoadFromFile(FFilename);

      For sLine In oAliases Do
      Begin
        sTemp := Trim(sLine);
        oItem := TDiskAlias.Create;
        Add(oItem);
        FValidators.Add(oItem.Validator); // Grab a _ref for Summary purposes

        If (sTemp <> '') And (Copy(sTemp, 1, 1) <> '#') Then
        Begin
          // Split sTemp into sName and sValue using spaces or tabs
          oItem.Alias := Trim(ExtractWord(1, sTemp, [' ', #9]));
          oItem.Filename := Trim(ExtractWord(2, sTemp, [' ', #9]));

          oItem.Validator.Process;
        End
        Else
          oItem.Line := sTemp;
      End;
    Finally
      oAliases.Free;
    End;

    Result := True;
  End;
End;

Function TDiskAliases.Save: Boolean;
Var
  slTemp: TStringList;
  oItem: TDiskAlias;
Begin
  // TODO TDiskAliases.Save is untested.  Pending ability to edit disks.alias in UI
  Result := False;

  If (Trim(FFilename) <> '') Then
    FFilename := IncludeSlash(uBee512.WorkingDir) + 'disks.alias';

  slTemp := TStringList.Create;
  Try
    For oItem In Self Do
      If Trim(oItem.Alias) = '' Then
        slTemp.Add(oItem.Line)
      Else
        slTemp.Add('%s     %s', [oItem.Alias, oItem.Filename]);

    Try
      slTemp.SaveToFile(Filename);
    Finally
      Result := True;
    End;
  Finally
    slTemp.Free;
  End;
End;

Function TDiskAliases.FilenameByAlias(AAlias: String): String;
Var
  oItem: TDiskAlias;
  sAlias: String;
Begin
  Result := ALIAS_NOT_FOUND;
  sAlias := Lowercase(AAlias);

  For oItem In Self Do
    If Lowercase(oItem.Alias) = sAlias Then
    Begin
      Result := oItem.Filename;
      Break;
    End;
End;

Function TDiskAliases.ValidAliases: TStringArray;
Var
  oItem: TDiskAlias;
Begin
  SetLength(Result, 0);

  For oItem In Self Do
    If oItem.Validator.ErrorLevel = elInfo Then
      AddStringToArray(Result, oItem.Alias);
End;

{ TDefinition }

Constructor TDefinition.Create;
Begin
  FValidator := TDefinitionValidator.Create(Self);
End;

Destructor TDefinition.Destroy;
Begin
  FreeAndNil(FValidator);
  Inherited Destroy;
End;

{ TDefinitions }

Function TDefinitions.GetDefinition(ADefinition: String): TDefinition;
Var
  oDefinition: TDefinition;
Begin
  Result := nil;

  For oDefinition In Self Do
    If (oDefinition.Definition = ADefinition) Then
    Begin
      Result := oDefinition;
      Break;
    End;
End;

Function TDefinitions.RCbyDefinition(ADefinition: String): String;
Var
  oDefinition: TDefinition;
Begin
  Result := '';
  oDefinition := GetDefinition(ADefinition);
  If Assigned(oDefinition) Then
    Result := oDefinition.RC;
End;

Function TDefinitions.DefinitionByTitle(ATitle: String): TDefinition;
Var
  oDefinition: TDefinition;
Begin
  Result := nil;

  If (Trim(ATitle) <> '') Then
    For oDefinition In Self Do
      If (oDefinition.Title = ATitle) Then
      Begin
        Result := oDefinition;
        Break;
      End;
End;

Function TDefinitions.RCByTitle(ATitle: String): String;
Var
  oDefinition: TDefinition;
Begin
  Result := '';

  oDefinition := DefinitionByTitle(ATitle);
  If Assigned(oDefinition) Then
    Result := oDefinition.RC;
End;

// Return a comma seperated sorted list of Microbee Models defined in the uBee512RC
Function TDefinitions.Models: String;
Var
  slModels: TStringList;
  oDefinition: TDefinition;
Begin
  slModels := TStringList.Create;
  Try
    // use the TStringlist to build up a sorted, unique list of models
    slModels.Sorted := True;
    slModels.Duplicates := dupIgnore;

    For oDefinition In Self Do
      If Trim(oDefinition.Model) <> '' Then
        slModels.Add(oDefinition.Model);

    Result := slModels.CommaText;
  Finally
    slModels.Free;
  End;
End;

// Return a comma seperated list of Microbee Models by Type
Function TDefinitions.ModelsByType(AMbeeType: TMbeeType): String;
Var
  slModels: TStringList;
  oDefinition: TDefinition;
Begin
  slModels := TStringList.Create;
  Try
    // use the TStringlist to build up a sorted, unique list of models
    slModels.Sorted := True;
    slModels.Duplicates := dupIgnore;

    For oDefinition In Self Do
      If (Trim(oDefinition.Model) <> '') And (oDefinition.MbeeType = AMbeeType) Then
        slModels.Add(oDefinition.Model);

    Result := slModels.CommaText;
  Finally
    slModels.Free;
  End;
End;

// Return a comma seperated list of Microbee systems defined in uBee512rc for
// a particular Microbee Model
Function TDefinitions.Titles(AModel: String): String;
Var
  slTitles: TStringList;
  oDefinition: TDefinition;
  sModel: String;
Begin
  sModel := LowerCase(AModel);
  slTitles := TStringList.Create;
  Try
    // use the TStringlist to build up a sorted, unique list of models
    slTitles.Sorted := True;
    slTitles.Duplicates := dupIgnore;

    For oDefinition In Self Do
      If Lowercase(oDefinition.Model) = sModel Then
        If Trim(oDefinition.Title) <> '' Then
          slTitles.Add(oDefinition.Title);

    Result := slTitles.CommaText;
  Finally
    slTitles.Free;
  End;
End;

{ TModels }

Function TModels.GetModel(AModel: String): TModel;
Var
  oModel: TModel;
Begin
  Result := nil;
  For oModel In Self Do
    If oModel.Model = AModel Then
    Begin
      Result := oModel;
      Break;
    End;
End;

Function TModels.MbeeType(AModel: String): TMbeeType;
Var
  oModel: TModel;
  sModel: String;
Begin
  Result := mtCustom;
  sModel := Lowercase(AModel);
  For oModel In Self Do
    If Lowercase(oModel.Model) = sModel Then
    Begin
      Result := oModel.MbeeType;
      Break;
    End;
End;

{ TuBee512 }

Constructor TuBee512.Create;

  Procedure AddModel(AModel: String; ADescription: String; AMBeeType: TMbeeType);
  Var
    oModel: TModel;
  Begin
    oModel := TModel.Create;
    oModel.Model := AModel;
    oModel.Description := ADescription;
    oModel.MbeeType := AMBeeType;

    FModels.Add(oModel);
  End;

Begin
  FExe := '';
  FRC := '';
  FLoadedRC := '';

  // OwnsObjects => No need for additional code to free contents
  FDefinitions := TDefinitions.Create(True);
  FModels := TModels.Create(True);
  FDisksAliases := TDiskAliases.Create(True);

  // From ubee512 readme
  AddModel('1024k', 'Standard Premium Plus, 1024K DRAM FDD', mtFDD);
  AddModel('128k', 'Standard, 128K DRAM FDD (SBC)', mtFDD);
  AddModel('256k', 'Standard, 256K DRAM FDD (64K CIAB to 256K upgrade)', mtFDD);
  AddModel('256tc', '256TC Telecomputer, 256K DRAM FDD', mtFDD);
  AddModel('2mhz', 'First model and kits, 32K ROM', mtROM);
  AddModel('2mhzdd', '56K FDD', mtFDD);
  AddModel('512k', 'Standard, 512K DRAM FDD', mtFDD);
  AddModel('56k', 'APC 56K RAM, ROM/FDD (Advanced Personal Computer)', mtFDD);
  AddModel('64k', 'Standard, 64K DRAM FDD (CIAB)', mtFDD);
  AddModel('dd', '56K FDD', mtFDD);
  AddModel('ic', 'IC 32K ROM', mtROM);
  AddModel('p1024k', 'Premium Plus, 1024K DRAM FDD', mtFDD);
  AddModel('p128k', 'Premium, 128K DRAM FDD', mtFDD);
  AddModel('p256k', 'Premium, 256K DRAM FDD (64K Premium to 256K upgrade)', mtFDD);
  AddModel('p512k', 'Premium, 512K DRAM FDD', mtFDD);
  AddModel('p64k', 'Premium, 64K DRAM FDD', mtFDD);
  AddModel('pc', 'PC 32K ROM (Personal Communicator)', mtROM);
  AddModel('pc85', 'Standard, PC85 32K ROM using 8K Pak ROMs', mtROM);
  AddModel('pc85b', 'Standard, PC85 32K ROM using 8/16K Pak ROMs', mtROM);
  AddModel('pcf', 'Premium Compact Flash Core board.', mtCustom);
  AddModel('ppc85', 'Premium, PC85 32K ROM', mtROM);
  AddModel('scf', 'Standard Compact Flash Core board.', mtCustom);
  AddModel('tterm', 'Teleterm, ROM', mtROM);
End;

Destructor TuBee512.Destroy;
Begin
  Inherited Destroy;

  FreeAndNil(FDisksAliases);
  //FreeAndNil(FROMAlias);

  FreeAndNil(FModels);
  FreeAndNil(FDefinitions);
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

Function TuBee512.LoadRC: Boolean;
Var
  slTemp: TStringList;
  s, sLine, sTag, sNewTag: String;
  sDescription: String;
  sProperty, sValue: String;
  c1, c2: Char;
  oDefinition: TDefinition;
  iCount: Integer;
Begin
  Result := False;

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
    FDefinitions.Clear;

    //FDefinitions.Clear;
    Try
      slTemp.LoadFromFile(FRC);
      oDefinition := nil;
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
                If Assigned(oDefinition) And (oDefinition.Definition <> '') And (iCount = 0) Then
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
                // Start a new System Definition
                sTag := sNewTag;
                oDefinition := TDefinition.Create;
                oDefinition.Definition := sTag;
                oDefinition.Description := sDescription;
                FDefinitions.Add(oDefinition);
                iCount := 0;
              End;
            End;
            '-': If (sTag <> '') And Assigned(oDefinition) Then
              Begin
                If oDefinition.RC = '' Then
                  oDefinition.RC := sLine
                Else
                  oDefinition.RC := oDefinition.RC + sLineBreak + sLine;

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

                Case sProperty Of
                  'a': oDefinition.A := sValue;
                  'b': oDefinition.B := sValue;
                  'c': oDefinition.C := sValue;
                  'col': oDefinition.Col := 'Colour';
                  'monitor':
                    If sValue = 'a' Then
                      oDefinition.Col := 'Amber'
                    Else
                      oDefinition.Col := sValue;
                  'model':
                  Begin
                    oDefinition.Model := sValue;
                    oDefinition.MbeeType := FModels.MbeeType(sValue);
                  End;
                  'ide-a0': oDefinition.IDE := sValue;
                  'tapei': oDefinition.TapeI := sValue;
                  'tapeo': oDefinition.TapeO := sValue;
                  'sram': oDefinition.SRAM := sValue;
                  'sram-file': oDefinition.SRAM_file := sValue;
                  'title': oDefinition.Title := TrimChars(sValue, ['"']);
                End;
              End;
          End;
        End;
      End;
    Finally
      slTemp.Free;
      Debug(Format('End loading %s.  %d Definitions loaded', [FRC, FDefinitions.Count]));
      ClearBusy;

      FLoadedRC := FRC;
      Result := True;

      FDisksAliases.Load;
    End;
  End;
End;

Function TuBee512.IsDisk(AExt: String): Boolean;
Var
  sExt: String;
Begin
  // TODO Return uBee512 supported formats (see email from uBee)
  sExt := Lowercase(Trim(AExt));

  Result := (sExt = '.dsk');
End;

Function TuBee512.WorkingDir: String;
Begin
{$IFDEF WINDOWS}
  Result := ExtractFileDir(FExe);
{$ELSE}
  Result := IncludeTrailingBackslash(getuserdir) + '.ubee512';
{$ENDIF}
End;

Function TuBee512.ValidFile(ASubfolder: String; AFilename: String): Boolean;
Var
  sFile: String;
Begin
  If IsFileAbsolute(AFilename) Then
    sFile := AFilename
  Else
    sFile := IncludeSlash(WorkingDir) + IncludeSlash(ASubfolder) + AFilename;

  Result := FileExists(sFile);
End;

Initialization
  FuBee512 := nil;

Finalization
  FreeAndNil(FuBee512);

End.
