Unit uBee512Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Validators, Graphics, Controls;

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

  { TSettingsValidator }

  TSettingsValidator = Class(TValidator)
  Protected
    Function GetTarget: String; Override;
  Public
    Constructor Create(AOwner: TObject); Override;
    Procedure Process; Override;
  End;

Implementation

Uses uBee512Support, FileSupport, StrUtils, FormMain, DialogSettings,
  CPMSupport;

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

{ TSettingsValidator }

Function TSettingsValidator.GetTarget: String;
Begin
  If Assigned(FOwner) And (FOwner Is TfrmMain) Then
    Result := TfrmMain(FOwner).Caption
  Else
    Result := Inherited GetTarget;
End;

Constructor TSettingsValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'uBee512Launcher';
  FDescription := 'This performs simple checks for currently selected options.';
End;

Procedure TSettingsValidator.Process;
Var
  oModel: TModel;
  oDefinition: TDefinition;
  sFormatA, sDiskB, sFormatB: String;
  sDiskA, sDiskC, sFormatC: TCaption;
Begin
  Inherited Process;
  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  If Assigned(FOwner) And (FOwner Is TSettings) Then
  Begin
    // Breaking a rule between UI and Logic here.
    // Really - UI should immediately update FSettings
    // And this code should validate those...
    // TODO Refactor so TSettingsValidator is actually validating TSettings...
    oModel := ubee512.Models[frmMain.cboModel.Text];
    oDefinition := ubee512.Definitions.DefinitionByTitle(frmMain.cboTitle.Text);

    If Not Assigned(oModel) Then
    Begin
      SetLevel(elError);
      AddOC('No Microbee Model is selected', [], elError);
      AddRM('This should not be possible.  Please report a bug, including a screenshot', []);
      Exit;
    End;

    Case oModel.MbeeType Of
      mtFDD:
      Begin
        // We must have an A:
        sDiskA := '';
        If Assigned(oDefinition) Then
          sDiskA := uBee512.DiskAliases.Resolve(oDefinition.A);
        If (sDiskA = '') Then
          sDiskA := uBee512.DiskAliases.Resolve(frmMain.cboDiskA.Text);
        If (sDiskA = '') Then
          sDiskA := oModel.DefaultBootDisk;

        sFormatA := '';
        If sDiskA = '' Then
        Begin
          SetLevel(elError);
          AddOC('No Disk A selected', [], elError);
          AddRM('Select a valid Disk A using the main form, or ensure one of the following boot disks exist: boot.dsk, %s.dsk', [oModel.Model]);
        End
        Else
        Begin
          SetLevel(elInfo);
          AddOC('Disk A="%s"', [sDiskA], elInfo);
          sFormatA := ubee512.DiskFormat(sDiskA, frmMain.cboFormatA.Text);

          If sFormatA = FORMAT_UNKNOWN Then
          Begin
            SetLevel(elWarning);
            AddOC('Unable to infer Disk A format', [], elWarning);
            AddRM('Unknown Disk A format might not be an issue.  ubee512 gets format by analysing file structure, ubee512launcher only checks filename.', []);
          End;
        End;

        sDiskB := '';
        If Assigned(oDefinition) Then
          sDiskB := uBee512.DiskAliases.Resolve(oDefinition.B);
        If (sDiskB = '') Then
          sDiskB := uBee512.DiskAliases.Resolve(frmMain.cboDiskB.Text);

        If sDiskB <> '' Then
        Begin
          SetLevel(elInfo);
          AddOC('Disk B="%s"', [sDiskB], elInfo);
          sFormatB := ubee512.DiskFormat(sDiskA, frmMain.cboFormatB.Text);

          If sFormatB = FORMAT_UNKNOWN Then
          Begin
            SetLevel(elWarning);
            AddOC('Unable to infer Disk B format', [], elWarning);
            AddRM('Unknown Disk B format might not be an issue.  ubee512 gets format by analysing file structure, ubee512launcher only checks filename.', []);
          End
          Else If (sFormatA <> FORMAT_UNKNOWN) And (sFormatA <> sFormatB) Then
          Begin
            SetLevel(elWarning);
            AddOC('Disk A format (%s) does not match Disk B format (%s)',
              [sFormatA, sFormatB], elWarning);
            StartRMList;
            AddRM('Conflicting formats might be an issue; only some disks (or maybe models?) support different formats.', []);
            AddRM('Alternatively: You may be able to use SETDRIVE', []);
            EndRMList;
          End;
        End;

        sDiskC := '';
        If Assigned(oDefinition) Then
          sDiskC := uBee512.DiskAliases.Resolve(oDefinition.C);
        If (sDiskC = '') Then
          sDiskC := uBee512.DiskAliases.Resolve(frmMain.cboDiskC.Text);

        If sDiskC <> '' Then
        Begin
          SetLevel(elInfo);
          AddOC('Disk C="%s"', [sDiskC], elInfo);
          sFormatC := ubee512.DiskFormat(sDiskA, frmMain.cboFormatC.Text);

          If sFormatC = FORMAT_UNKNOWN Then
          Begin
            SetLevel(elWarning);
            AddOC('Unable to infer Disk C format', [], elWarning);
            AddRM('Unknown Disk C format might not be an issue.  ubee512 gets format by analysing file structure, ubee512launcher only checks filename.', []);
          End
          Else If (sFormatA <> FORMAT_UNKNOWN) And (sFormatA <> sFormatC) Then
          Begin
            SetLevel(elWarning);
            AddOC('Disk A format (%s) does not match Disk C format (%s)',
              [sFormatA, sFormatC], elWarning);
            AddRM('Conflicting formats might be an issue; only some disks (or maybe models?) support different formats.', []);
            AddRM('Alternatively: You may be able to use SETDRIVE', []);
          End;
        End;

        If (sDiskA = '') And (sDiskB = '') And (sDiskC = '') Then
        Begin
          SetLevel(elError);
          AddOC('Disk Model selected, but no disks have been chosen', [], elError);
          AddRM('Either select Disk A using the main form, or ensure one of the following boot disks exist: boot.dsk, %s.dsk', [oModel.Model]);
        End;
      End;
      mtROM:
      Begin
        ;  // TODO Add valid Settings checks for ROM based machines
        ;
      End;
      Else
      Begin
        ;  // TODO Add valid Settings checks for Custom machines
        ;
      End;
    End;
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
      If Not IsFileAbsolute(sAliasFilename) Then
        sAliasFilename := sBaseFolder + sAliasFilename;
      bAliasFilenameExists := FileExists(sAliasFilename);
      bGenericBootExists := FileExists(sBaseFolder + 'boot.dsk');

      If bHasBoot Then
        sBoot := sBaseFolder + sModelBootDisk
      Else If bAliasFilenameExists Then
        sBoot := sBaseFolder + sAliasFilename
      Else If bGenericBootExists Then
        sBoot := sBaseFolder + 'boot.dsk'
      Else
        sBoot := '';

      bHasBoot := FileExists(sBaseFolder + sModelBootDisk);
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
        If bAliasFilenameExists Then
        Begin
          SetLevel(elInfo);
          AddOC('Boot disk alias "%s" resolves to "%s"', [sModelBootDisk, sAliasFilename], elInfo);
        End
        Else
        Begin
          SetLevel(elWarning);
          AddOC('Boot disk alias "%s" is not configured', [sModelBootDisk], elWarning);

          AddRM('  Use of Alias is optional: ', []);
          StartRMList;
          AddRM('    You can instead directly populate "Disk A"', []);
          EndRMList;
          AddRM('  Alternatively, to configure the Alias:', []);
          StartRMList;
          AddRM('    either place the Disk in "%s" and add just the filename to "disks.alias",',
            [sBaseFolder]);
          AddRM('    Or, add the absolute path to the Disk to "disks.alias"', []);
          EndRMList;
        End;
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
              Else
              Begin
                If Not IsFileAbsolute(sAlias) Then
                  sAlias := IncludeSlash(sFolder) + sAlias;

                If Not FileExists(sAlias) Then
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
