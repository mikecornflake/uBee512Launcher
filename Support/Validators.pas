Unit Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections;

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
    Property ErrorLevel: TErrorLevel read FErrorLevel;
  End;

  { TDefinitionValidator }

  TDefinitionValidator = Class(TValidator)
  Public
    Constructor Create; Override;
    Procedure Process(ATarget: TObject); Override;
  End;

Implementation

Uses
  uBee512Support, Logs, FileSupport;

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

  Procedure AddOutcome(AFormatStr: String; arrParams: Array of Const);
  Begin
    FOutcome += Format(AFormatStr, arrParams) + LineEnding;
  end;

  Procedure AddRecommendation(AFormatStr: String; arrParams: Array of Const);
  Begin
    FRecommendation += Format(AFormatStr, arrParams) + LineEnding;
  end;

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
          Begin
            If FileIsAbsolute(AFilename) Then
            BEgin
              AddOutcome('%s "%s" not found', [AObject, AFilename]);
              AddRecommendation('Ensure file %s exists', [AFilename]);
              SetLevel(elError);
            end
            Else
            Begin
              sAlias := uBee512.DiskAlias(AFilename);

              if (sAlias=ALIAS_NOT_FOUND) Then
              Begin
                AddOutcome('%s "%s" not found in "disks.alias" or "%s"', [AObject, AFilename, sFolder]);
                AddRecommendation('Ensure file "%s" exists', [AFilename]);
                AddRecommendation('Add "%s" entry to "disks.alias"', [AFilename]);
                SetLevel(elError);
              end
              Else
                If Not uBee512.ValidFile(ASubfolder, sAlias) Then
                Begin
                  AddOutcome('%s "%s" found in "disks.alias", and resolves to "%s"', [AObject, AFilename, sAlias]);
                  if FileIsAbsolute(sAlias) Then
                  Begin
                    AddOutcome('%s "%s" not found', [AObject, AFilename]);
                    AddRecommendation('Ensure file %s exists', [AFilename]);
                    SetLevel(elError);
                  end
                  Else
                  Begin
                    AddOutcome('%s "%s" not found in "%s"', [AObject, sAlias, sFolder]);
                    AddRecommendation('Ensure file "%s" exists in "%s"', [AFilename, sFolder]);
                    SetLevel(elError);
                  end;
                end
                Else
                Begin
                  AddOutcome('%s "%s" found in "disks.alias", and resolves to "%s"', [AObject, AFilename, sAlias]);
                  SetLevel(elInfo);
                end;
            end;

            FRecommendation := '';
          End;
          Else
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

  If sResult.EndsWith(LineEnding) Then
    sResult.TrimRight([#10, #13]);

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
