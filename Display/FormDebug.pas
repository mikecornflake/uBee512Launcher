Unit FormDebug;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TfrmDebug }

  TfrmDebug = Class(TForm)
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
  frmDebug: TfrmDebug;

Implementation

{$R *.lfm}

{ TfrmDebug }

Procedure TfrmDebug.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FFile := '';
End;

Procedure TfrmDebug.FormActivate(Sender: TObject);
Var
  iLen: SizeInt;
Begin
  If Not FActivated Then
  Begin
    FActivated := True;

    iLen := Length(memDebug.Text);
    If iLen > 0 Then
    Begin
      memDebug.SelStart := iLen-1;
      memDebug.SelLength := 0;
    end;
  End;
End;

Procedure TfrmDebug.bntClearClick(Sender: TObject);
Begin
  memDebug.Lines.Clear;
  If FileExists(FFile) Then
    DeleteFile(FFile);
End;

Procedure TfrmDebug.Load(AFilename: String);
Begin
  FFile := AFilename;

  If FileExists(AFilename) Then
    memDebug.Lines.LoadFromFile(AFilename)
  Else
    memDebug.Lines.Clear;
End;

End.
