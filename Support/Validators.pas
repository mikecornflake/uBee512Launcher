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

    Procedure Process; Virtual;

    Property DisplayName: String read FDisplayname;
    Property Description: String read FDescription;
    Property Outcome: String read FOutcome;

    Property Valid: Boolean read FValid;
  End;

  { TValidators }

  TValidators = Class(Specialize TObjectList<TValidator>)
  Public
    Procedure Process;

    Function Outcome: String;
  End;

  { TSystemMacroValidator }

  TSystemMacroValidator = Class(TValidator)
  Public
    Constructor Create; Override;
    Procedure Process; Override;
  End;

Implementation

Uses
  uBee512Support;

{ TSystemMacroValidator }

Constructor TSystemMacroValidator.Create;
Begin
  Inherited Create;

  FDisplayName := 'System Macro Checker';
  FDescription := 'System Macros are defined in ubee512rc.' + LineEnding +
    'This performs simple checks that the required files are present';
End;

Procedure TSystemMacroValidator.Process;
Var
  oMacro: TSystemMacro;
  sDiskFolder: Rawbytestring;
  sDisk: String;
Begin
  Inherited Process;

  // This has moved away from the planned design.
  // One Validator per SystemMacro please
  FOutcome := '';
  sDiskFolder := IncludeTrailingBackslash(ExtractFileDir(ubee512.RC)) + 'disks';

  For oMacro In ubee512.SystemMacros Do
  Begin
    sDisk := oMacro.A;
    If (sDisk <> '') And DirectoryExists(sDiskFolder) And Not
      FileExists(IncludeTrailingBackslash(sDiskFolder) + sDisk) Then
    Begin
      FValid := False;
      FOutcome := FOutcome + Format('Macro [%s]: Disk "%s" not found in "%s"',
        [oMacro.Macro, sDisk, sDiskFolder]) + LineEnding;
    End;
  End;
End;

{ TValidators }

Procedure TValidators.Process;
Var
  oValidator: TValidator;
Begin
  For oValidator In Self Do
    oValidator.Process;
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
  FOutcome := '';
  FValid := False;
End;

Procedure TValidator.Process;
Begin
  FOutcome := '';
  FValid := False;
End;

End.
