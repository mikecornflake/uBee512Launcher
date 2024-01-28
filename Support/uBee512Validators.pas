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

Implementation

Uses uBee512Support, FileSupport, StrUtils;

{ TDiskAliasValidator }

Constructor TDiskAliasValidator.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);

  FDisplayName := 'Disks.Alias Entry Checker';
  FDescription := 'This performs simple checks on each entry in Disks.Alias Entry Checker';
End;

Procedure TDiskAliasValidator.Process;
Var
  sBaseFolder: String;
  oItem: TDiskAlias;

  Procedure AddOutcome(AFormatStr: String; arrParams: Array Of Const);
  Begin
    FOutcome += Format(AFormatStr, arrParams) + LineEnding;
  End;

  Procedure AddRecommendation(AFormatStr: String; arrParams: Array Of Const);
  Begin
    FRecommendation += Format(AFormatStr, arrParams) + LineEnding;
  End;

Begin
  Inherited Process;

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeSlash(ubee512.WorkingDir) + includeSlash('disks');

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

  FDisplayName := 'Definition Checker';
  FDescription := 'Definitions are defined in ubee512rc.' + LineEnding +
    'This performs simple checks that the required files are present';
End;

Procedure TDefinitionValidator.Process;
Var
  oDefinition: TDefinition;
  sBaseFolder: String;

  Procedure AddOutcome(AFormatStr: String; arrParams: Array Of Const);
  Begin
    FOutcome += Format(AFormatStr, arrParams) + LineEnding;
  End;

  Procedure AddRecommendation(AFormatStr: String; arrParams: Array Of Const);
  Begin
    FRecommendation += Format(AFormatStr, arrParams) + LineEnding;
  End;

  Procedure Check(ASubfolder: String; AFilename: String; AObject: String);
  Var
    sFolder, sAlias: String;
  Begin
    If AFilename <> '' Then
    Begin
      sFolder := sBaseFolder + ASubfolder;

      If Not uBee512.ValidFile(ASubfolder, AFilename) Then
        Case ASubfolder Of
          'disks':
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

    Check('disks', oDefinition.A, 'Disk');
    Check('disks', oDefinition.B, 'Disk');
    Check('disks', oDefinition.C, 'Disk');
    Check('sram', oDefinition.SRAM_file, 'SRAM');
    Check('disks', oDefinition.IDE, 'IDE image');
    Check('tapes', oDefinition.TapeI, 'Input Tape');
    Check('tapes', oDefinition.TapeO, 'Output Tape');

    FOutcome := TrimRightSet(FOutcome, [' ', #10, #13]);
    FRecommendation := TrimRightSet(FRecommendation, [' ', #10, #13]);
  End;
End;

End.
