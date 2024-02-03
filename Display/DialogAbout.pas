Unit DialogAbout;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics, StdCtrls, SysUtils;

Type

  { TdlgAbout }

  TdlgAbout = Class(TForm)
    Bevel1: TBevel;
    btnOK: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblImageMagick1: TLabel;
    lblImageMagick2: TLabel;
    lblSDKs: TLabel;
    lblImageMagick: TLabel;
    lblXPDF1: TLabel;
    lblXPDF2: TLabel;
    lblHTMLLabel6: TLabel;
    lblHTMLLabel7: TLabel;
    lblXPDF3: TLabel;
    memImageMagick: TMemo;
    Memo1: TMemo;
    memuBee512: TMemo;
    memXPDF: TMemo;
    memReadme: TMemo;
    pcAbout: TPageControl;
    imgAbout: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblApplicationTitle: TLabel;
    lblHTMLLabel: TLabel;
    lblHTMLLabel1: TLabel;
    memLicence: TMemo;
    memAbout: TMemo;
    tsuBee512: TTabSheet;
    tsImageMagick: TTabSheet;
    tsXPDF: TTabSheet;
    tsCredits: TTabSheet;
    tsAbout: TTabSheet;
    tsReadme: TTabSheet;
    tsLicence: TTabSheet;
    Procedure btnOKClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure URLLabelMouseEnter(Sender: TObject);
    Procedure URLLabelMouseLeave(Sender: TObject);
  End;

Procedure ShowAbout;

Implementation

Uses
  LCLIntf, VersionSupport, XPDFSupport, ImageMagickSupport, OSSupport,
  uBee512Support, FileSupport;

  {$R *.lfm}

Procedure ShowAbout;
Var
  oAbout: TdlgAbout;
Begin
  oAbout := TdlgAbout.Create(Application.MainForm);
  Try
    oAbout.ShowModal;
  Finally
    oAbout.Free;
  End;
End;

Procedure TdlgAbout.FormCreate(Sender: TObject);
Var
  oResourceStrings: TStringList;
  sFolder: String;
Begin
  Inherited;

  sFolder := IncludeSlash(ExtractFilePath(Application.ExeName));
  pcAbout.ActivePage := tsAbout;

  If FileExists(sFolder + 'licence.txt') Then
  Begin
    tsLicence.TabVisible := True;
    memLicence.Lines.LoadFromFile(sFolder + 'licence.txt');
  End;

  If FileExists(sFolder + 'readme.txt') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.txt');
  End
  Else If FileExists(sFolder + 'readme.md') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.md');
  End;

  If (ImageMagickAvailable) And (FileExists(IncludeSlash(ImageMagickPath) + 'License.txt')) Then
  Begin
    tsImageMagick.TabVisible := True;
    memImageMagick.Lines.LoadFromFile(IncludeSlash(ImageMagickPath) + 'License.txt');
    lblImageMagick.Visible := True;
  End
  Else
    tsImageMagick.TabVisible := False;

  If (XPDFAvailable) And (FileExists(FixOSPathDelimiter(XPDFPath + '\..\README'))) Then
  Begin
    tsXPDF.TabVisible := True;
    memXPDF.Lines.LoadFromFile(FixOSPathDelimiter(XPDFPath + '\..\README'));
    lblXPDF1.Visible := True;
    lblXPDF2.Visible := True;
  End
  Else
    tsXPDF.TabVisible := False;

  If uBee512.Available Then
  Begin
    tsuBee512.TabVisible := True;

    If DirectoryExists(uBee512.WorkingDir) And
      FileExists(FixOSPathDelimiter(uBee512.WorkingDir + '\doc\license.txt')) Then
      memuBee512.Lines.LoadFromFile(FixOSPathDelimiter(uBee512.WorkingDir + '\doc\license.txt'));
  End
  Else
    tsuBee512.TabVisible := False;

  lblSDKs.Visible := lblImageMagick.Visible Or lblXPDF1.Visible;

  memAbout.Lines.Clear;
  memAbout.Lines.Add(Application.exename);
  memAbout.Lines.Add('');
  oResourceStrings := TStringList.Create;
  Try
    GetResourceStrings(oResourceStrings);

    memAbout.Lines.AddStrings(oResourceStrings);

    If oResourceStrings.Count > 0 Then
      memAbout.Lines.Add('');
  Finally
    oResourceStrings.Free;
  End;

  memAbout.Lines.Add('Built for ' + GetTargetInfo);
  memAbout.Lines.Add(' with ' + GetCompilerInfo + ' on ' + GetCompiledDate);
  memAbout.Lines.Add(' and using ' + GetLCLVersion + ' and ' + GetWidgetset);

  lblApplicationTitle.Caption := Application.Title;
End;

Procedure TdlgAbout.FormDestroy(Sender: TObject);
Begin
End;

Procedure TdlgAbout.btnOKClick(Sender: TObject);
Begin
  Close;
End;

Procedure TdlgAbout.URLLabelMouseLeave(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
End;

Procedure TdlgAbout.URLLabelMouseEnter(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
End;

Procedure TdlgAbout.URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  OpenURL(TLabel(Sender).Caption);
End;

End.
