Unit Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections;

Type

  { TValidator }

  TValidator = Class
  Protected
    FValid: Boolean;
    FDescription: String;
    FDisplayname: String;
    FOutcome: String;
  Public
    Constructor Create; Virtual;

    Procedure Process(ATarget: TObject); Virtual;

    Property DisplayName: String read FDisplayname;
    Property Description: String read FDescription;
    Property Outcome: String read FOutcome;

    Property Valid: Boolean read FValid;
  End;

  { TValidators }

  TValidators = Class(Specialize TObjectList<TValidator>)
  Private
    FValid: Boolean;
  Public
    Procedure Process(ATarget: TObject);

    Function Outcome: String;
    Property Valid: Boolean read FValid;
  End;

  { TSystemMacroValidator }

  TSystemMacroValidator = Class(TValidator)
  Public
    Constructor Create; Override;
    Procedure Process(ATarget: TObject); Override;
  End;

Implementation

Uses
  uBee512Support, Logging;

{ TSystemMacroValidator }

Constructor TSystemMacroValidator.Create;
Begin
  Inherited Create;

  FDisplayName := 'System Macro Checker';
  FDescription := 'System Macros are defined in ubee512rc.' + LineEnding +
    'This performs simple checks that the required files are present';
End;

Procedure TSystemMacroValidator.Process(ATarget: TObject);
Var
  oMacro: TSystemMacro;
  sBaseFolder: String;

  Procedure Check(ASubfolder: String; AFilename: String; AObject: String);
  Var
    sFolder, sFile: String;
  Begin
    If AFilename <> '' Then
    Begin
      sFolder := sBaseFolder + ASubfolder;
      sFile := IncludeTrailingBackslash(sFolder) + AFilename;

      If DirectoryExists(sFolder) And Not FileExists(sFile) Then
      Begin
        FValid := False;
        FOutcome += Format('%s "%s" not found in "%s"', [AObject, AFilename, sFolder]) +
          LineEnding;
      End;
    End;
  End;

Begin
  Inherited Process(ATarget);

  FValid := True;
  FOutcome := '';

  sBaseFolder := IncludeTrailingBackslash(ubee512.WorkingDir);

  If Assigned(ATarget) And (ATarget Is TSystemMacro) Then
  Begin
    oMacro := TSystemMacro(ATarget);

    Check('disks', oMacro.A, 'Disk');
    Check('disks', oMacro.B, 'Disk');
    Check('disks', oMacro.C, 'Disk');
    Check('sram', oMacro.SRAM_file, 'SRAM');
    Check('disks', oMacro.IDE, 'IDE image');
    Check('tapes', oMacro.TapeI, 'Input Tape');
    Check('tapes', oMacro.TapeO, 'Output Tape');
  End;
End;

{ TValidators }

Procedure TValidators.Process(ATarget: TObject);
Var
  oValidator: TValidator;
Begin
  FValid := True;
  For oValidator In Self Do
  Begin
    oValidator.Process(ATarget);
    FValid := FValid And oValidator.Valid;

    If Not oValidator.Valid Then
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
    If Not oValidator.Valid Then
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
  FValid := True;
  FOutcome := '';
End;

Procedure TValidator.Process(ATarget: TObject);
Begin
  FValid := True;
  FOutcome := '';
End;

End.
