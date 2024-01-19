Unit FormMacroExplorer;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

Type

  { TfrmDefinitionExplorer }

  TfrmDefinitionExplorer = Class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ilFlags: TImageList;
    lvSystemMacros: TListView;
    memRC: TMemo;
    memSystems: TMemo;
    memIssues: TMemo;
    Panel1: TPanel;
    pnlBottom: TPanel;
    pcRC: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tsMacros: TTabSheet;
    tsRC: TTabSheet;
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lvSystemMacrosCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure lvSystemMacrosDblClick(Sender: TObject);
    Procedure lvSystemMacrosSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Private
    Function GetTitle: String;
    Procedure SetTitle(AValue: String);
  Private
    FActivated: Boolean;
    Function GetModel: String;
    Procedure LoadRC;
  Public
    Property Title: String read GetTitle write SetTitle;
    Property Model: String read GetModel;
  End;

Implementation

Uses
  uBee512Support, OSSupport, Logging;

{$R *.lfm}

Procedure TfrmDefinitionExplorer.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  LoadRC;

  Caption := 'System Macro Explorer: ' + ubee512.RC;
End;

Procedure TfrmDefinitionExplorer.FormActivate(Sender: TObject);
Begin
  If Not Factivated Then
  Begin
    MakeFullyVisible;

    FActivated := True;
  End;
End;

Procedure TfrmDefinitionExplorer.lvSystemMacrosDblClick(Sender: TObject);
Begin
  If (Assigned(lvSystemMacros.Selected)) And (btnOK.Enabled) Then
    ModalResult := mrOk;
End;

Procedure TfrmDefinitionExplorer.lvSystemMacrosSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
Var
  oMacro: TSystemMacro;
  sRC: String;
Begin
  btnOK.Enabled := True;

  If (Selected) And (Assigned(Item)) Then
  Begin
    oMacro := uBee512.Macro(Item.Caption);
    sRC := '# ' + oMacro.Description + LineEnding;
    sRC += '[' + oMacro.Macro + ']' + LineEnding;
    sRC += oMacro.RC;

    memSystems.Lines.Text := sRC;
    memIssues.Lines.Text := oMacro.Validators.Outcome;

    btnOK.Enabled := TSystemMacro(Item.Data).Validators.Valid;
  End
  Else
    memSystems.Lines.Clear;
End;

Procedure TfrmDefinitionExplorer.lvSystemMacrosCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
Var
  oMacro: TSystemMacro;
Begin
  oMacro := TSystemMacro(Item.Data);

  If oMacro.Validators.Valid Then
    lvSystemMacros.Canvas.Font.Color := clBlack
  Else
    lvSystemMacros.Canvas.Font.Color := clRed;
End;

Function TfrmDefinitionExplorer.GetTitle: String;
Begin
  If Assigned(lvSystemMacros.Selected) Then
    Result := lvSystemMacros.Selected.SubItems[1];
End;

Function TfrmDefinitionExplorer.GetModel: String;
Begin
  If Assigned(lvSystemMacros.Selected) Then
    Result := lvSystemMacros.Selected.SubItems[0];
End;

Procedure TfrmDefinitionExplorer.SetTitle(AValue: String);
Var
  sTitle: String;
  oItem: TListItem;
Begin
  sTitle := Lowercase(AValue);
  For oItem In lvSystemMacros.Items Do
    If Lowercase(oItem.Subitems[1]) = sTitle Then
    Begin
      lvSystemMacros.ItemIndex := oItem.Index;
      Break;
    End;
End;

Procedure TfrmDefinitionExplorer.LoadRC;
Var
  oMacro: TSystemMacro;
  oItem: TListItem;
  sRam: String;
Begin
  If uBee512.Available And FileExists(uBee512.RC) Then
  Begin
    SetBusy;
    Try
      memRC.Lines.LoadFromFile(uBee512.RC);
      uBee512.LoadRC;
      pcRC.ActivePage := tsMacros;

      lvSystemMacros.Items.Clear;
      memSystems.Lines.Clear;
      lvSystemMacros.Items.BeginUpdate;

      For oMacro In uBee512.SystemMacros Do
        If oMacro.Title <> '' Then
        Begin
          oItem := lvSystemMacros.Items.Add;

          oItem.Caption := oMacro.Macro;
          oItem.SubItems.Add(oMacro.Model);
          oItem.SubItems.Add(oMacro.Title);
          oItem.SubItems.Add(MBTypeStr[oMacro.MbeeType]);
          oItem.SubItems.Add(oMacro.A);
          sRam := '';
          If (Trim(oMacro.SRAM) <> '') Then
            sRam += oMacro.SRAM + 'k';
          If (Trim(oMacro.SRAM) <> '') And (Trim(oMacro.SRAM_File) <> '') Then
            sRam += '=';
          If (Trim(oMacro.SRAM_File) <> '') Then
            sRam += oMacro.SRAM_File;
          oItem.SubItems.Add(sRam);
          oItem.SubItems.Add(oMacro.Description);

          oMacro.Validators.Process(oMacro);
          oItem.Data := oMacro;

          If oMacro.Validators.Valid Then
            oItem.ImageIndex := 0
          Else
            oItem.ImageIndex := 1;
        End;
    Finally
      lvSystemMacros.Items.EndUpdate;
      lvSystemMacros.AutoSize := True;

      ClearBusy;
    End;
  End;
End;

End.
