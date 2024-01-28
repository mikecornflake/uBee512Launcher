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

    Property ErrorLevel: TErrorLevel read FErrorLevel;
  End;

Const
  ERRORLEVEL_COLOR: Array[TErrorLevel] Of TColor =
    (clBlack, TColor($006400), TColor($FF8C00), clRed);

Implementation

Uses
  Logs, StrUtils;

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

End.
