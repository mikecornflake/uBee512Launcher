Unit DialogDiskAlias;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, SynHighlighterIni, SynEdit, uBee512Support, IpHtml;

Type

  { TdlgDiskAlias }

  TdlgDiskAlias = Class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ilMain: TImageList;
    htmlSummary: TIpHtmlPanel;
    lvAlias: TListView;
    memFile: TSynEdit;
    pcMain: TPageControl;
    pnlBottom: TPanel;
    pnlSummary: TPanel;
    Splitter1: TSplitter;
    SynIniSyn1: TSynIniSyn;
    tbMain: TToolBar;
    btnInsert: TToolButton;
    btnDelete: TToolButton;
    btnChange: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tsAliases: TTabSheet;
    tsDiskAlias: TTabSheet;
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lvAliasSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Private
    FActivated: Boolean;
    FSelected: TDiskAlias;

  Public
    Procedure LoadDiskAlias;

    Procedure RefreshUI;
  End;

Implementation

{$R *.lfm}

Uses
  Validators,
  OSSupport, FileSupport, ControlsSupport, StringSupport;

{ TdlgDiskAlias }

Procedure TdlgDiskAlias.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FSelected := Nil;
End;

Procedure TdlgDiskAlias.lvAliasSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  sSummary: String;
Begin
  If Assigned(Item) And Assigned(Item.Data) And (TObject(Item.Data) Is TDiskAlias) Then
    FSelected := TDiskAlias(Item.Data)
  Else
    FSelected := nil;

  if Assigned(FSelected) Then
  Begin
    sSummary := ArrayToString(FSelected.Validator.Summary(False), '');
    SetHTML(htmlSummary, '<body>'+sSummary+'</body>');
  end;
End;

Procedure TdlgDiskAlias.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    LoadDiskAlias;

    FActivated := True;
  End;
End;

Procedure TdlgDiskAlias.LoadDiskAlias;
Var
  oAlias: TDiskAlias;
  sFolder, sFile: String;
  oItem: TListItem;
Begin
  SetBusy;
  Try
    sFolder := IncludeSlash(uBee512.WorkingDir) + IncludeSlash('disks');

    memFile.Lines.LoadFromFile(IncludeSlash(uBee512.WorkingDir)+'disks.alias');

    lvAlias.Items.Clear;
    lvAlias.Items.BeginUpdate;
    Try
      For oAlias In uBee512.DiskAliases Do
        If oAlias.Validator.ErrorLevel >= elInfo Then
        Begin
          oItem := lvAlias.Items.Add;
          oItem.Caption := oAlias.Alias;
          oItem.SubItems.Add(oAlias.Filename);
          If (oAlias.Filename <> '') And Not IsFileAbsolute(oAlias.Filename) Then
          Begin
            sFile := sFolder + oAlias.Filename;
            if FileExists(sFile) Then
              oItem.SubItems.Add(sFolder + oAlias.Filename)
            Else
              oItem.SubItems.Add('<Alias filename does not exist>');
          end
          Else
            oItem.SubItems.Add(oAlias.Filename);
          oItem.Data := oAlias;

          oAlias.Validator.Process;
          oItem.ImageIndex := Integer(oAlias.Validator.ErrorLevel);
        End;
    Finally
      lvAlias.Items.EndUpdate;
    End;
  Finally
    ClearBusy;
  End;
End;

Procedure TdlgDiskAlias.RefreshUI;
Begin

End;

End.
