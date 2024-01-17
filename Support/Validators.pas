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
  sDiskFolder: Rawbytestring;
  sDisk: String;
Begin
  Inherited Process(ATarget);

  If Assigned(ATarget) And (ATarget Is TSystemMacro) Then
  Begin
    // TODO: This folder works on Windows only.
    // Need to find the folder for Linux & macOS
    sDiskFolder := IncludeTrailingBackslash(ExtractFileDir(ubee512.RC)) + 'disks';
    oMacro := TSystemMacro(ATarget);

    sDisk := oMacro.A;
    If (sDisk <> '') And DirectoryExists(sDiskFolder) And Not
      FileExists(IncludeTrailingBackslash(sDiskFolder) + sDisk) Then
    Begin
      FValid := False;
      FOutcome := Format('Disk "%s" not found in "%s"', [sDisk, sDiskFolder]);
    End;
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
