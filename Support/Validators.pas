Unit Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections, Graphics;

Type

  TErrorLevel = (elNone, elInfo, elWarning, elError);

  { TValidator }

  TValidator = Class
  Protected
    FRecommendation: String;
    FErrorLevel: TErrorLevel;
    FDescription: String;
    FDisplayname: String;
    FOutcome: String;

    Procedure SetLevel(AErrorLevel: TErrorLevel);
  Public
    Constructor Create; Virtual;

    Procedure Process({%H-}ATarget: TObject); Virtual;

    Property DisplayName: String read FDisplayname;
    Property Description: String read FDescription;

    Property ErrorLevel: TErrorLevel read FErrorLevel;
    Property Outcome: String read FOutcome;
    Property Recommendation: String read FRecommendation;
  End;

  { TValidators }

  TValidators = Class(Specialize TObjectList<TValidator>)
  Private
    FErrorLevel: TErrorLevel;
  Public
    Procedure Process(ATarget: TObject);

    Function Outcome: String;
    Function Recommendation: String;

    Property ErrorLevel: TErrorLevel read FErrorLevel;
  End;

  { TDefinitionValidator }

  TDefinitionValidator = Class(TValidator)
  Public
    Constructor Create; Override;
    Procedure Process(ATarget: TObject); Override;
  End;

  { TDiskAliasValidator }

  TDiskAliasValidator = Class(TValidator)
  Public
    Constructor Create; Override;
    Procedure Process(ATarget: TObject); Override;
  End;


Const
  ERRORLEVEL_COLOR: Array[TErrorLevel] Of TColor =
    (clBlack, TColor($006400), TColor($FF8C00), clRed);

Implementation

Uses
  uBee512Support, Logs, FileSupport, StrUtils;

{ TDiskAliasValidator }

Constructor TDiskAliasValidator.Create;
Begin
  Inherited Create;

  FDisplayName := 'Disks.Alias Entry Checker';
  FDescription := 'This performs simple checks on each entry in Disks.Alias Entry Checker';
End;

Procedure TDiskAliasValidator.Process(ATarget: TObject);
Var
  sBaseFolder: Rawbytestring;
Begin
  Inherited Process(ATarget);

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeTrailingBackslash(ubee512.WorkingDir);

  If Assigned(ATarget) And (ATarget Is TDefinition) Then;
   // TODO Implement
End;

{ TDefinitionValidator }

Constructor TDefinitionValidator.Create;
Begin
  Inherited Create;

  FDisplayName := 'Definition Checker';
  FDescription := 'Definitions are defined in ubee512rc.' + LineEnding +
    'This performs simple checks that the required files are present';
End;

Procedure TDefinitionValidator.Process(ATarget: TObject);
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
  Inherited Process(ATarget);

  FErrorLevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  sBaseFolder := IncludeTrailingBackslash(ubee512.WorkingDir);

  If Assigned(ATarget) And (ATarget Is TDefinition) Then
  Begin
    oDefinition := TDefinition(ATarget);

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

{ TValidators }

Procedure TValidators.Process(ATarget: TObject);
Var
  oValidator: TValidator;

Begin
  FErrorLevel := elNone;

  For oValidator In Self Do
  Begin
    oValidator.Process(ATarget);
    If oValidator.ErrorLevel > FErrorLevel Then
      FErrorLevel := oValidator.ErrorLevel;

    If oValidator.ErrorLevel <> elNone Then
      Debug(oValidator.Outcome);
  End;
End;

Function TValidators.Outcome: String;
Var
  oValidator: TValidator;
  sResult: String;
Begin
  sResult := '';

  For oValidator In Self Do
    If oValidator.ErrorLevel <> elNone Then
      sResult := sResult + oValidator.Outcome + LineEnding;

  sResult := TrimRightSet(sResult, [' ', #10, #13]);

  Result := sResult;
End;

Function TValidators.Recommendation: String;
Var
  oValidator: TValidator;
  sResult: String;
Begin
  sResult := '';

  For oValidator In Self Do
    If oValidator.ErrorLevel <> elNone Then
      sResult := sResult + oValidator.Recommendation + LineEnding;

  sResult := TrimRightSet(sResult, [' ', #10, #13]);

  Result := sResult;
End;

{ TValidator }

Constructor TValidator.Create;
Begin
  Inherited Create;

  FDescription := '';
  FDisplayname := '';

  FErrorlevel := elNone;
  FOutcome := '';
  FRecommendation := '';
End;

Procedure TValidator.Process(ATarget: TObject);
Begin
  FErrorlevel := elNone;
  FOutcome := '';
  FRecommendation := '';
End;

Procedure TValidator.SetLevel(AErrorLevel: TErrorLevel);
Begin
  If AErrorLevel > FErrorLevel Then
    FErrorLevel := AErrorLevel;
End;

End.
