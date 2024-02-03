Unit uBee512Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Validators, Graphics;

Type
  { TDefinitionValidator }

  TDefinitionValidator = Class(TValidator)
  Protected
    Function GetTarget: String; Override;
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TDiskAliasValidator }

  TDiskAliasValidator = Class(TValidator)
  Protected
    Function GetTarget: String; Override;
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TModelValidator }

  TModelValidator = Class(TValidator)
  Protected
    Function GetTarget: String; Override;
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TInstallationValidator }

  TInstallationValidator = Class(TValidator)
  Protected
    Function GetTarget: String; Override;
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

Implementation

Uses uBee512Support, FileSupport, StrUtils;

  { TInstallationValidator }

Function TInstallationValidator.GetTarget: String;
Begin
  If Assigned(FOwner) And (FOwner Is TuBee512) Then
    Result := '"' + TuBee512(FOwner).WorkingDir + '"'
  Else
    Result := Inherited GetTarget;
End;

Constructor TInstallationValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'uBee512 Installation';
  FDescription := 'This performs simple checks for each Model.';
End;

Procedure TInstallationValidator.Process;
Var
  sBaseFolder: String;

  Procedure Check(ASubfolder: String; AFilename: String; AObject: String;
    AErrorLevel: TErrorLevel; ARec: String = '');
  Var
    sFolder: String;
  Begin
    If AFilename <> '' Then
    Begin
      sFolder := sBaseFolder + ASubfolder;

      If Not uBee512.ValidFile(ASubfolder, AFilename) Then
      Begin
        SetLevel(AErrorLevel);
        AddOC('%s "%s" not found in "%s"', [AObject, AFilename, sFolder], AErrorLevel);
        If (ARec = '') Then
          AddRM('Download "%s" from Repository', [AFilename])
        Else
          AddRM(ARec, []);
      End;
    End;
  End;

Begin
  Inherited Process;

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeSlash(ubee512.WorkingDir);

  If Assigned(FOwner) And (FOwner Is TuBee512) Then
  Begin
    Check('roms', 'charrom.bin', 'Character ROM', elError);
    Check('roms', 'rom1.bin', 'Default boot ROM BN54', elError);

    Check('', 'ubee512rc', 'uBee512 Setting file', elError,
      'Copy & rename "configs\ubee512rc.sample", and place in uBee512 folder');
    Check('', 'roms.alias', 'ROM lookup file', elWarning,
      'Copy & rename "configs\roms.alias.sample", and place in uBee512 folder');
    Check('', 'disks.alias', 'ROM lookup file', elWarning,
      'Copy & rename "configs\disks.alias.sample", and place in uBee512 folder');
  End;
End;

{ TModelValidator }

Function TModelValidator.GetTarget: String;
Begin
  If Assigned(FOwner) And (FOwner Is TModel) Then
    Result := '--model=' + TModel(FOwner).Model
  Else
    Result := Inherited GetTarget;
End;

Constructor TModelValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'Microbee Models';
  FDescription := 'This performs simple checks for each Model.';
End;

Procedure TModelValidator.Process;
Var
  sBaseFolder, sModelBootDisk, sAliasFilename, sBoot: String;
  oItem: TModel;
  bHasBoot, bAliasExists, bAliasFilenameExists, bGenericBootExists, bReadOnly: Boolean;
Begin
  Inherited Process;

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeSlash(ubee512.WorkingDir) + includeSlash(SUBFOLDER_DISKS);

  If Assigned(FOwner) And (FOwner Is TModel) Then
  Begin
    oItem := TModel(FOwner);

    If (oItem.MbeeType = mtFDD) Or (oItem.MBeeType = mtCustom) Then
    Begin
      sModelBootDisk := oItem.Model + '.dsk';
      sBoot := '';

      bHasBoot := FileExists(sBaseFolder + sModelBootDisk);
      sAliasFilename := uBee512.DiskAliases.FilenameByAlias(sModelBootDisk);
      bAliasExists := (sAliasFilename <> ALIAS_NOT_FOUND);
      bAliasFilenameExists := FileExists(sBaseFolder + sAliasFilename);
      bGenericBootExists := FileExists(sBaseFolder + 'boot.dsk');

      If bHasBoot Then
        sBoot := sBaseFolder + sModelBootDisk
      Else If bAliasFilenameExists Then
        sBoot := sBaseFolder + sAliasFilename
      Else If bGenericBootExists Then
        sBoot := sBaseFolder + 'boot.dsk'
      Else
        sBoot := '';

      bReadOnly := FileExists(sBoot) And (FileIsReadOnly(sBoot));

      // Is there an obvious boot disk?
      If bHasBoot Then
      Begin
        If bReadOnly Then
        Begin
          SetLevel(elError);
          AddOC('Default boot disk for Model "%s" exists, and is "%s", ' +
            'However, file is ReadOnly', [oItem.Model, sBoot], elError);
          AddRM('Use file system tools to unset the ReadOnly flag on this file', []);
        End
        Else
        Begin
          SetLevel(elInfo);
          AddOC('Default boot disk for Model "%s" exists, and is "%s"',
            [oItem.Model, sBoot], elInfo);
          AddRM('You do not need to specify a boot disk in order to run this model',
            []);
        End;
      End
      Else If bAliasExists Then
      Begin
        SetLevel(elWarning);
        AddOC('Boot disk alias "%s" is not configured', [sModelBootDisk], elWarning);

        AddRM('  Use of Alias is optional: ', []);
        StartRMList;
        AddRM('    You can directly populate "Disk A"', []);
        EndRMList;
        AddRM('  Or, to configure the Alias:', []);
        StartRMList;
        AddRM('    either place the Disk in "%s" ', [sBaseFolder]);
        AddRM('    and add just the filename to "disks.alias", ', []);
        AddRM('    Or, add the absolute path to the Disk to "disks.alias"', []);
        EndRMList;
      End
      Else
      Begin
        SetLevel(elWarning);
        AddOC('No default boot disk found', [], elWarning);
        AddRM('A boot disk will need to be selected prior to launching uBee512.', []);
        StartRMList;
        AddRM('Either by editing "ubee512rc", "disks.alias",', []);
        AddRM('Or by populating "Disk A" in uBee512Launcher', []);
        EndRMList;
      End;
    End;

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

{ TDiskAliasValidator }

Function TDiskAliasValidator.GetTarget: String;
Begin
  If Assigned(FOwner) And (FOwner Is TDiskAlias) Then
    Result := '"' + TDiskAlias(FOwner).Alias + '"'
  Else
    Result := Inherited GetTarget;
End;

Constructor TDiskAliasValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := '"Disks.Alias"';
  FDescription := 'This performs simple checks on each entry in Disks.Alias Entry Checker.';
End;

Procedure TDiskAliasValidator.Process;
Var
  sBaseFolder: String;
  oItem: TDiskAlias;
Begin
  Inherited Process;

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeSlash(ubee512.WorkingDir) + includeSlash(SUBFOLDER_DISKS);

  If Assigned(FOwner) And (FOwner Is TDiskAlias) Then
  Begin
    oItem := TDiskAlias(FOwner);

    If Trim(oItem.Alias) <> '' Then
      If Trim(oItem.Filename) = '' Then
      Begin
        FErrorLevel := elWarning;
        AddOC('%s is defined in "disks.alias", but has no lookup filename!',
          [oItem.Alias], elWarning);

        StartRMList;
        AddRM('Alias "%s" is optional; do not use it, ', [oItem.Alias]);
        AddRM('Or, place the Disk in "%s" and assign in "disks.alias", ',
          [sBaseFolder]);
        AddRM('Or, assign the absolute path to the Disk in "disks.alias"', []);
        EndRMList;
      End
      Else If uBee512.ValidFile(SUBFOLDER_DISKS, oItem.Filename) Then
      Begin
        FErrorLevel := elInfo;
        If IsFileAbsolute(oItem.Filename) Then
          AddOC('Alias "%s" is valid and will use file "%s"',
            [oItem.Alias, oItem.Filename], elInfo)
        Else
          AddOC('Alias "%s" is valid and will use file "%s"',
            [oItem.Alias, sBaseFolder + oItem.Filename], elInfo);
      End
      Else If IsFileAbsolute(oItem.Filename) And Not (FileExists(oItem.Filename)) Then
      Begin
        FErrorLevel := elError;
        AddOC('Alias "%s" lookup file "%s" does not exist!',
          [oItem.Alias, oItem.Filename], elError);
        AddRM('Ensure file %s exists:', [oItem.Filename]);
        StartRMList;
        AddRM('Check filepath?', []);
        AddRM('Download file from Repository?', []);
        EndRMList;
      End
      Else If Not IsFileAbsolute(oItem.Filename) And Not FileExists(
        sBaseFolder + oItem.Filename) Then
      Begin
        FErrorLevel := elError;
        AddOC('Alias "%s" lookup file "%s" can not be found in "%s"!',
          [oItem.Alias, oItem.Filename, sBaseFolder], elError);
        AddRM('Ensure file "%s" exists:', [oItem.Filename]);
        StartRMList;
        AddRM('Check filepath?', []);
        AddRM('Download file from Repository?', []);
        EndRMList;
      End;

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

{ TDefinitionValidator }

Function TDefinitionValidator.GetTarget: String;
Begin
  If Assigned(FOwner) And (FOwner Is TDefinition) Then
    Result := '[' + TDefinition(FOwner).Definition + ']'
  Else
    Result := Inherited GetTarget;
End;

Constructor TDefinitionValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'Definitions';
  FDescription :=
    'Checks the contents of each definition in ubee512rc.  Currently only checks disks, sram & tapes.';
End;

Procedure TDefinitionValidator.Process;
Var
  oDefinition: TDefinition;
  sBaseFolder: String;

  Procedure Check(ASubfolder: String; AFilename: String; AObject: String);
  Var
    sFolder, sAlias: String;
  Begin
    If AFilename <> '' Then
    Begin
      sFolder := sBaseFolder + ASubfolder;

      If Not uBee512.ValidFile(ASubfolder, AFilename) Then
        Case ASubfolder Of
          SUBFOLDER_DISKS:
            If IsFileAbsolute(AFilename) Then
            Begin
              SetLevel(elError);
              AddOC('%s "%s" not found', [AObject, AFilename], elError);
              AddRM('Ensure valid file %s exists', [AFilename]);
            End
            Else
            Begin
              // Check "disks.alias"
              sAlias := uBee512.DiskAliases.FilenameByAlias(AFilename);

              If (sAlias = ALIAS_NOT_FOUND) Then
              Begin
                SetLevel(elError);

                AddOC('%s "%s" not found in "disks.alias" or "%s"',
                  [AObject, AFilename, sFolder], elError);

                AddRM('Ensure valid %s file exists (i.e. download from Repository)',
                  [AObject]);
                StartRMList;
                AddRM('Either add "%s" entry to "disks.alias" ' +
                  '(the file can then have any name and be stored anywhere)',
                  [AFilename]);
                AddRM('Or ensure file is named "%s" and placed directly in "%s"',
                  [AFilename, sFolder]);
                EndRMList;
              End
              Else If Not uBee512.ValidFile(ASubfolder, sAlias) Then
              Begin
                SetLevel(elError);
                AddOC('%s "%s" found in "disks.alias", and resolves to "%s"',
                  [AObject, AFilename, sAlias], elError);
                If IsFileAbsolute(sAlias) Then
                Begin
                  AddOC('%s "%s" not found', [AObject, AFilename], elError);
                  AddRM('Ensure file "%s" exists', [AFilename]);
                End
                Else
                Begin
                  AddOC('%s "%s" not found in "%s"',
                    [AObject, sAlias, sFolder], elError);
                  AddRM('Ensure file "%s" exists in "%s"', [AFilename, sFolder]);
                End;
              End
              Else
              Begin
                SetLevel(elInfo);
                AddOC('%s "%s" found in "disks.alias", and resolves to "%s"',
                  [AObject, AFilename, sAlias], elInfo);
              End;
            End;
          Else // Case ASubfolder Of
          Begin
            SetLevel(elError);
            AddOC('%s "%s" not found in "%s"', [AObject, AFilename, sFolder], elError);
            AddRM('Ensure file "%s" exists:', [AFilename]);
            StartRMList;
            AddRM('Check filepath?', []);
            AddRM('Download file from Repository?', []);
            EndRMList;
          End;
        End;
    End;
  End;

Begin
  Inherited Process;

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeTrailingBackslash(ubee512.WorkingDir);

  If Assigned(FOwner) And (FOwner Is TDefinition) Then
  Begin
    oDefinition := TDefinition(FOwner);

    Check(SUBFOLDER_DISKS, oDefinition.A, 'Disk');
    Check(SUBFOLDER_DISKS, oDefinition.B, 'Disk');
    Check(SUBFOLDER_DISKS, oDefinition.C, 'Disk');
    Check(SUBFOLDER_DISKS, oDefinition.IDE, 'IDE image');
    Check('sram', oDefinition.SRAM_file, 'SRAM');
    Check('tapes', oDefinition.TapeI, 'Input Tape');
    Check('tapes', oDefinition.TapeO, 'Output Tape');

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

End.
