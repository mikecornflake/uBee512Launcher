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
    dlgFile: TOpenDialog;
    pcMain: TPageControl;
    pnlBottom: TPanel;
    pnlSummary: TPanel;
    Splitter1: TSplitter;
    SynIniSyn1: TSynIniSyn;
    tbMain: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    btnAssign: TToolButton;
    ToolButton2: TToolButton;
    btnClear: TToolButton;
    tsAliases: TTabSheet;
    tsDiskAlias: TTabSheet;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnAssignClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lvAliasCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure lvAliasSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure btnClearClick(Sender: TObject);
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
  FSelected := nil;
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

    memFile.Lines.LoadFromFile(IncludeSlash(uBee512.WorkingDir) + 'disks.alias');

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
            If FileExists(sFile) Then
              oItem.SubItems.Add(sFolder + oAlias.Filename)
            Else
              oItem.SubItems.Add('<Alias filename does not exist>');
          End
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
    RefreshUI;
    ClearBusy;
  End;
End;

Procedure TdlgDiskAlias.lvAliasSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Var
  sSummary: String;
Begin
  If Assigned(Item) And Assigned(Item.Data) And (TObject(Item.Data) Is TDiskAlias) Then
    FSelected := TDiskAlias(Item.Data)
  Else
    FSelected := nil;

  If Assigned(FSelected) Then
  Begin
    sSummary := ArrayToString(FSelected.Validator.Summary(False), '');
    SetHTML(htmlSummary, '<body>' + sSummary + '</body>');
  End;

  RefreshUI;
End;

Procedure TdlgDiskAlias.btnAssignClick(Sender: TObject);
Begin
  If Assigned(FSelected) Then
  Begin
    dlgFile.InitialDir := uBee512.WorkingDir;
    If dlgFile.Execute Then
    Begin
      FSelected.Filename := uBee512.ShrinkFile('disks', dlgFile.FileName);
      FSelected.Validator.Process;
      lvAlias.Selected.SubItems[0] := FSelected.Filename;
      lvAlias.Selected.SubItems[1] := dlgFile.FileName;
      lvAlias.Selected.ImageIndex := Integer(FSelected.Validator.ErrorLevel);

      lvAliasSelectItem(lvAlias, lvAlias.Selected, True);
      lvAlias.Refresh;

      RefreshUI;
    End;
  End;
End;

Procedure TdlgDiskAlias.btnAddClick(Sender: TObject);
Var
  oAlias: TDiskAlias;
  sAlias: String;
  oItem: TListItem;
Begin
  sAlias := InputBox('New Alias', 'Please enter a new Alias', '');
  If (sAlias <> '') Then
  Begin
    oAlias := uBee512.DiskAliases[sAlias];

    If Assigned(oAlias) Then
    Begin
      MessageDlg('Unable to add. ' + sAlias + ' already exists!', mtError, [mbOK], 0);
      Exit;
    End
    Else
    Begin
      oAlias := uBee512.DiskAliases.Add(sAlias);
      If Assigned(oAlias) Then
      Begin
        oItem := lvAlias.Items.Add;
        oItem.Caption := sAlias;
        oItem.SubItems.Add('');
        oItem.SubItems.Add('');
        oItem.Data := oAlias;

        oItem.ImageIndex := Integer(oAlias.Validator.ErrorLevel);
        oItem.Selected := True;
        oItem.MakeVisible(False);
      End
      Else
        MessageDlg('Unable to add ' + sAlias + '.  Unknown reason!', mtError, [mbOK], 0);
    End;
  End;
End;

Procedure TdlgDiskAlias.btnDeleteClick(Sender: TObject);
Begin
  If Assigned(FSelected) Then
    If MessageDlg('Do you want to delete ' + FSelected.Alias + '?', mtConfirmation,
      mbYesNo, 0) = mrYes Then
    Begin
      // TODO
      uBee512.DiskAliases.Delete(FSelected);
      FSelected := nil;
      lvAlias.Selected.Data := nil;
      lvAlias.Items.Delete(lvAlias.Selected.Index);

      RefreshUI;
    End;
End;

Procedure TdlgDiskAlias.lvAliasCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
Var
  oAlias: TDiskAlias;
Begin
  oAlias := TDiskAlias(Item.Data);

  lvAlias.Canvas.Font.Color := ERRORLEVEL_COLOR[oAlias.Validator.ErrorLevel];
End;

Procedure TdlgDiskAlias.btnClearClick(Sender: TObject);
Begin
  If Assigned(FSelected) Then
  Begin
    FSelected.Filename := '';
    FSelected.Validator.Process;
    lvAlias.Selected.SubItems[0] := '';
    lvAlias.Selected.SubItems[1] := '';
    lvAlias.Selected.ImageIndex := Integer(FSelected.Validator.ErrorLevel);

    lvAliasSelectItem(lvAlias, lvAlias.Selected, True);
    lvAlias.Refresh;
  End;
End;

Procedure TdlgDiskAlias.RefreshUI;
Begin
  btnAdd.Enabled := True;
  btnDelete.Enabled := Assigned(FSelected);
  btnClear.Enabled := Assigned(FSelected) And (FSelected.Filename <> '');
  btnAssign.Enabled := Assigned(FSelected);
End;

End.
