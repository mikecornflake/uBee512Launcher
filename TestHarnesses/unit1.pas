Unit Unit1;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IpHtml;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    Procedure FormCreate(Sender: TObject);
  Private
    Procedure SetHTMLAsString(AHTML: String);

  Public

  End;

Var
  Form1: TForm1;

Implementation

Uses
  ControlsSupport;

{$R *.lfm}

Procedure TForm1.FormCreate(Sender: TObject);
Var
  s: String;
Begin
  s := '<b>Test</b> <u>test 2</u> </BR>';
  s += '<p style="background-color:Tomato;">Lorem ipsum...</p>';
  s += '<p style="color:red;">Red text</p>';
  SetHTMLAsString(s);
  SetHTMLAsString(s);  //Testing for memleak
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
Begin
  SetHTML(IpHtmlPanel1, AHTML);
End;

End.
