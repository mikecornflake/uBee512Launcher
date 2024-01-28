Unit Validators;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Generics.Collections, Graphics;

Type

  TErrorLevel = (elNone, elInfo, elWarning, elError);
  TErrorLevels = Set Of TErrorLevel;

  { TValidator }

  TValidator = Class
  Protected
    FOwner: TObject;
    FRecommendation: String;
    FErrorLevel: TErrorLevel;
    FDescription: String;
    FDisplayname: String;
    FOutcome: String;

    Procedure SetLevel(AErrorLevel: TErrorLevel);
  Public
    Constructor Create(AOwner: TObject); Virtual;

    Procedure Process; Virtual;

    Property DisplayName: String read FDisplayname;
    Property Description: String read FDescription;

    Function Summary: TStringArray;

    Property ErrorLevel: TErrorLevel read FErrorLevel;
    Property Outcome: String read FOutcome;
    Property Recommendation: String read FRecommendation;
  End;

  { TValidators }

  TValidators = Class(Specialize TObjectList<TValidator>)
  Private
    FErrorLevel: TErrorLevel;
  Public
    Function Outcome: String;
    Function Recommendation: String;

    Function Count(AErrorLevels: TErrorLevels): Integer;
    Function Summary(AErrorLevels: TErrorLevels): TStringArray;

    Property ErrorLevel: TErrorLevel read FErrorLevel;
  End;

Const
  ERROR_LEVEL: Array[TErrorLevel] Of String = ('None', 'Info', 'Warning', 'Error');

  ERRORLEVEL_COLOR: Array[TErrorLevel] Of TColor =
    (clBlack, TColor($006400), TColor($FF8C00), clRed);


Implementation

Uses
  Logs, StrUtils, StringSupport;

{ TValidator }

Constructor TValidator.Create(AOwner: TObject);
Begin
  Inherited Create;

  FOwner := AOwner;

  FDescription := '';
  FDisplayname := '';

  FErrorlevel := elNone;
  FOutcome := '';
  FRecommendation := '';
End;

Procedure TValidator.Process;
Begin
  FErrorlevel := elNone;
  FOutcome := '';
  FRecommendation := '';
End;

Function TValidator.Summary: TStringArray;
Begin
  SetLength(Result, 0);
  AddStringToArray(Result, '  ' + ERROR_LEVEL[FErrorLevel] + ': ' + FOutcome);
  If Trim(FRecommendation) <> '' Then
    AddStringToArray(Result, '  Recommendation: ' + Trim(FRecommendation));
End;

Procedure TValidator.SetLevel(AErrorLevel: TErrorLevel);
Begin
  If AErrorLevel > FErrorLevel Then
    FErrorLevel := AErrorLevel;
End;

{ TValidators }

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

Function TValidators.Count(AErrorLevels: TErrorLevels): Integer;
Var
  oValidator: TValidator;
Begin
  Result := 0;

  For oValidator In Self Do
    If oValidator.ErrorLevel In AErrorLevels Then
      Inc(Result);
End;

Function TValidators.Summary(AErrorLevels: TErrorLevels): TStringArray;
Var
  oValidator: TValidator;
Begin
  SetLength(Result, 0);

  For oValidator In Self Do
    If oValidator.ErrorLevel In AErrorLevels Then
    Begin
      AddStringsToArray(Result, oValidator.Summary);
      AddStringToArray(Result, '');
    End;

  // Remove the trailing blank line
  If Length(Result) > 0 Then
    SetLength(Result, Length(Result) - 1);
End;

End.
