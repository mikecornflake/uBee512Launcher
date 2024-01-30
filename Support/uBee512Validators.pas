Unit uBee512Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Validators, Graphics;

Type
  { TDefinitionValidator }

  TDefinitionValidator = Class(TValidator)
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TDiskAliasValidator }

  TDiskAliasValidator = Class(TValidator)
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TModelValidator }

  TModelValidator = Class(TValidator)
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

  { TInstallationValidator }

  TInstallationValidator = Class(TValidator)
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

Implementation

Uses uBee512Support, FileSupport, StrUtils;

{ TInstallationValidator }

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
        AddOutcome('%s "%s" not found in "%s"', [AObject, AFilename, sFolder]);
        If (ARec = '') Then
          AddRecommendation('  Download "%s" from Repository', [AFilename])
        Else
          AddRecommendation('  ' + ARec, []);
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
          FErrorLevel := elError;
          AddOutcome('Default boot disk for Model "%s" exists, and is "%s"', [oItem.Model, sBoot]);
          AddOutcome('However, file "%s" is ReadOnly', [sBoot]);
          AddRecommendation('Use file system tools to unset the ReadOnly flag on this file', []);
        End
        Else
        Begin
          FErrorLevel := elInfo;
          AddOutcome('Default boot disk for Model "%s" exists, and is "%s"', [oItem.Model, sBoot]);
          AddRecommendation('You do not need to specify a boot disk in order to run this model',
            []);
        End;
      End
      Else If bAliasExists Then
      Begin
        FErrorLevel := elWarning;
        AddOutcome('Boot disk alias "%s" is not configured', [sModelBootDisk]);

        AddRecommendation('  Use of Alias is optional: ', []);
        AddRecommendation('    You can directly populate "Disk A"', []);
        AddRecommendation('  To configure the Alias:', []);
        AddRecommendation('    either place the Disk in "%s" ', [sBaseFolder]);
        AddRecommendation('    and add just the filename to "disks.alias", ', []);
        AddRecommendation('    Or, add the absolute path to the Disk to "disks.alias"', []);
      End
      Else
      Begin
        FErrorLevel := elWarning;
        AddOutcome('No default boot disk found', []);
        AddRecommendation('  A boot disk will need to be selected prior to launching uBee512.',
          []);
        AddRecommendation('  Either by editing "ubee512rc", "disks.alias",', []);
        AddRecommendation('  Or by populating "Disk A" in uBee512Launcher', []);
      End;
    End;

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

{ TDiskAliasValidator }

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
        AddOutcome('%s is defined in "disks.alias", but has no lookup filename!', [oItem.Alias]);
        AddRecommendation('  Either: Do not use alias "%s", ', [oItem.Alias]);
        AddRecommendation('    Or, place the Disk in "%s" and add just the filename to "disks.alias", ', [sBaseFolder]);
        AddRecommendation('    Or, add the absolute path to the Disk to "disks.alias"', []);
      End
      Else If uBee512.ValidFile(SUBFOLDER_DISKS, oItem.Filename) Then
      Begin
        FErrorLevel := elInfo;
        If IsFileAbsolute(oItem.Filename) Then
          AddOutcome('Alias "%s" is valid and will use file "%s"', [oItem.Alias, oItem.Filename])
        Else
          AddOutcome('Alias "%s" is valid and will use file "%s"',
            [oItem.Alias, sBaseFolder + oItem.Filename]);
      End
      Else If IsFileAbsolute(oItem.Filename) And Not (FileExists(oItem.Filename)) Then
      Begin
        FErrorLevel := elError;
        AddOutcome('Alias "%s" lookup file "%s" does not exist!', [oItem.Alias, oItem.Filename]);
        AddRecommendation('  Ensure file %s exists:', [oItem.Filename]);
        AddRecommendation('    Check filepath?', []);
        AddRecommendation('    Download file from Repository?', []);
      End
      Else If Not IsFileAbsolute(oItem.Filename) And Not FileExists(sBaseFolder +
        oItem.Filename) Then
      Begin
        FErrorLevel := elError;
        AddOutcome('Alias "%s" lookup file "%s" can not be found in "%s"!',
          [oItem.Alias, oItem.Filename, sBaseFolder]);
        AddRecommendation('  Ensure file %s exists:', [oItem.Filename]);
        AddRecommendation('    Check filepath?', []);
        AddRecommendation('    Download file from Repository?', []);
      End;

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

{ TDefinitionValidator }

Constructor TDefinitionValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'Definitions';
  FDescription := 'Checks the contents of each definition in ubee512rc.  Currently only checks disks, sram & tapes.';
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
              AddOutcome('%s "%s" not found', [AObject, AFilename]);
              AddRecommendation('Ensure valid file %s exists', [AFilename]);
              SetLevel(elError);
            End
            Else
            Begin
              // Check "disks.alias"
              sAlias := uBee512.DiskAliases.FilenameByAlias(AFilename);

              If (sAlias = ALIAS_NOT_FOUND) Then
              Begin
                AddOutcome('%s "%s" not found in "disks.alias" or "%s"',
                  [AObject, AFilename, sFolder]);
                AddRecommendation('Ensure valid %s file exists (i.e. download from Repository)',
                  [AObject]);
                AddRecommendation(
                  '- Either add "%s" entry to "disks.alias" (the file can then have any name and be stored anywhere)',
                  [AFilename]);
                AddRecommendation('- Or ensure file is named "%s" and placed directly in "%s"',
                  [AFilename, sFolder]);
                AddRecommendation('', []);
                SetLevel(elError);
              End
              Else If Not uBee512.ValidFile(ASubfolder, sAlias) Then
              Begin
                AddOutcome('%s "%s" found in "disks.alias", and resolves to "%s"',
                  [AObject, AFilename, sAlias]);
                If IsFileAbsolute(sAlias) Then
                Begin
                  AddOutcome('%s "%s" not found', [AObject, AFilename]);
                  AddRecommendation('Ensure file %s exists', [AFilename]);
                  SetLevel(elError);
                End
                Else
                Begin
                  AddOutcome('%s "%s" not found in "%s"', [AObject, sAlias, sFolder]);
                  AddRecommendation('Ensure file "%s" exists in "%s"', [AFilename, sFolder]);
                  SetLevel(elError);
                End;
              End
              Else
              Begin
                AddOutcome('%s "%s" found in "disks.alias", and resolves to "%s"',
                  [AObject, AFilename, sAlias]);
                SetLevel(elInfo);
              End;
            End;
          Else // Case ASubfolder Of
          Begin
            AddOutcome('%s "%s" not found in "%s"', [AObject, AFilename, sFolder]);
            SetLevel(elError);
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
