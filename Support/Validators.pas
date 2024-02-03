Unit Validators;

{$mode ObjFPC}{$H+}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
Interface

Uses
  Classes, SysUtils, Generics.Collections, Graphics;

Type

  TErrorLevel = (elNone, elInfo, elWarning, elError);
  TErrorLevels = Set Of TErrorLevel;

  { TValidator }

  TValidator = Class
  Private
  Protected
    FOwner: TObject;
    FRecommendation: String;
    FErrorLevel: TErrorLevel;
    FDescription: String;
    FDisplayname: String;
    FOutcome: String;
    FRMList: Boolean;

    Procedure AddOC(AFormatStr: String; arrParams: Array Of Const;
      AError: TErrorLevel = elNone);
    Procedure AddRM(AFormatStr: String; arrParams: Array Of Const);
    Procedure StartRMList;
    Procedure EndRMList;
    Procedure SetLevel(AErrorLevel: TErrorLevel);
    Function GetTarget: String; Virtual;
  Public
    Constructor Create(AOwner: TObject); Virtual;

    Procedure Process; Virtual;

    Property Target: String read GetTarget;
    Property DisplayName: String read FDisplayname;
    Property Description: String read FDescription;

    Function Summary(AShowHeader: Boolean = False): TStringArray;

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
    Function Summary(AErrorLevels: TErrorLevels; AShowHeader: Boolean = False): TStringArray;

    Property ErrorLevel: TErrorLevel read FErrorLevel;
  End;

Const
  ERROR_LEVEL: Array[TErrorLevel] Of String = ('None', 'Info', 'Warning', 'Error');
  ERROR_LEVEL_HTML: Array[TErrorLevel] Of String = ('black', 'green', 'orange', 'red');
  ERRORLEVEL_COLOR: Array[TErrorLevel] Of TColor =
    (clBlack, TColor($006400), TColor($0055FF), clRed);

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

  FRMList := False;
End;

Procedure TValidator.Process;
Begin
  FErrorlevel := elNone;
  FOutcome := '';
  FRecommendation := '';

  Debug(FDisplayname + ': validating ' + Target);
End;

Function TValidator.Summary(AShowHeader: Boolean = False): TStringArray;
Begin
  SetLength(Result, 0);

  If AShowHeader And (Trim(FOutcome) <> '') Then
  Begin
    AddStringToArray(Result, '<h3>' + FDisplayName + '</h3>');
    AddStringToArray(Result, '<blockquote>' + FDescription + '</blockquote>');
  End;

  If Trim(FOutcome) <> '' Then
    AddStringToArray(Result, '<p><b>' + ERROR_LEVEL[FErrorLevel] + '</b>: ' +
      FOutcome + '</p>');

  If Trim(FRecommendation) <> '' Then
    AddStringToArray(Result, LineEnding + '<p>Recommendation(s):<p>' +
      LineEnding + Trim(FRecommendation));
End;

Procedure TValidator.SetLevel(AErrorLevel: TErrorLevel);
Begin
  If AErrorLevel > FErrorLevel Then
    FErrorLevel := AErrorLevel;
End;

Function TValidator.GetTarget: String;
Begin
  Result := '';
End;

Procedure TValidator.AddOC(AFormatStr: String; arrParams: Array Of Const;
  AError: TErrorLevel = elNone);
Var
  sP: String;
Begin
  sP := '<span style="color:' + ERROR_LEVEL_HTML[AError] + ';">';

  FOutcome += sP + Format(AFormatStr, arrParams) + '</span>';
  FOutcome += LineEnding;
End;

Procedure TValidator.AddRM(AFormatStr: String; arrParams: Array Of Const);
Begin
  If FRMList Then
    FRecommendation += '<li>';

  FRecommendation += Format(AFormatStr, arrParams);

  If Not FRMList Then
    FRecommendation += LineEnding;

  If FRMList Then
    FRecommendation += '</li>';

  FRecommendation += LineEnding;
End;

Procedure TValidator.StartRMList;
Begin
  FRMList := True;

  FRecommendation += '<ul>' + LineEnding;
End;

Procedure TValidator.EndRMList;
Begin
  FRMList := False;

  FRecommendation += '</ul>' + LineEnding;
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

  //If sResult <> '' Then
  //  sResult := '<p>' + sResult + '</p>';

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

  //If sResult <> '' Then
  //  sResult := '<p>' + sResult + '</p>';

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

Function TValidators.Summary(AErrorLevels: TErrorLevels;
  AShowHeader: Boolean = False): TStringArray;
Var
  oValidator: TValidator;
  oLast: TValidator;
Begin
  SetLength(Result, 0);

  oLast := nil;

  For oValidator In Self Do
  Begin
    If AShowHeader And (Not Assigned(oLast) Or (oValidator.ClassType <> oLast.ClassType)) Then
    Begin
      AddStringToArray(Result, '<h1>' + oValidator.DisplayName + '</h1>');
      AddStringToArray(Result, oValidator.Description);

      oLast := oValidator;
    End;

    If oValidator.ErrorLevel In AErrorLevels Then
    Begin
      AddStringsToArray(Result, oValidator.Summary);
      AddStringToArray(Result, '');
    End;
  End;

  // Remove the trailing blank line
  If Length(Result) > 0 Then
    SetLength(Result, Length(Result) - 1);
End;

End.
