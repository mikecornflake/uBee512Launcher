Unit DialogDebug;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TdlgDebug }

  TdlgDebug = Class(TForm)
    Button1: TButton;
    bntClear: TButton;
    memDebug: TMemo;
    Panel1: TPanel;
    Procedure bntClearClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    FFile: String;
    FActivated: Boolean;
  Public
    Procedure Load(AFilename: String);
  End;

Var
  dlgDebug: TdlgDebug;

Implementation

{$R *.lfm}

{ TdlgDebug }

Procedure TdlgDebug.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FFile := '';
End;

Procedure TdlgDebug.FormActivate(Sender: TObject);
Var
  iLen: SizeInt;
Begin
  If Not FActivated Then
  Begin
    FActivated := True;

    iLen := Length(memDebug.Text);
    If iLen > 0 Then
    Begin
      memDebug.SelStart := iLen - 1;
      memDebug.SelLength := 0;
    End;
  End;
End;

Procedure TdlgDebug.bntClearClick(Sender: TObject);
Begin
  memDebug.Lines.Clear;
  If FileExists(FFile) Then
    DeleteFile(FFile);
End;

Procedure TdlgDebug.Load(AFilename: String);
Begin
  FFile := AFilename;

  If FileExists(AFilename) Then
    memDebug.Lines.LoadFromFile(AFilename)
  Else
    memDebug.Lines.Clear;
End;

End.
