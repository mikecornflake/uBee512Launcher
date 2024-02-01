Unit Unit1;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Ipfilebroker, IpHtml;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    Procedure FormCreate(Sender: TObject);
  Private
    Procedure SetHTMLAsString(AHTML: String);
    Procedure LoadFromStream(AStream: TStream);

  Public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Procedure TForm1.FormCreate(Sender: TObject);
Var
  s: String;
Begin
  s := '<b>Test</b> <u>test 2</u> </BR>';
  s += '<p style="background-color:Tomato;">Lorem ipsum...</p>';
  s += '<p style="color:red;">Red text</p>';
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
  SetHTMLAsString(s);
End;

Procedure TForm1.SetHTMLAsString(AHTML: String);
Var
  oStream: TStringStream;
Begin
  oStream := TStringStream.Create(AHTML);
  Try
    LoadFromStream(oStream);
  Finally
    oStream.Free;
  End;
End;

Type
  // Expose the OnGetImageX property
  THackIpHtml = Class(TIpHtml);

Procedure TForm1.LoadFromStream(AStream: TStream);
Var
  oHTML: THackIpHtml;
Begin
  Try
    oHTML := THackIpHtml.Create; // Beware: Will be freed automatically by htmlPanel

    AStream.Position := 0;
    oHTML.LoadFromStream(AStream);

    AStream.Position := 0;

    IpHtmlPanel1.SetHtml(oHTML);
  Except
    On E: Exception Do
      MessageDlg('Unable to open HTML' + #13 + 'Error: ' + E.Message,
        mtError, [mbCancel], 0);
  End;
End;

End.
