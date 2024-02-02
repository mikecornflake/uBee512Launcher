Unit FormDefinitionExplorer;

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
    lvDefinitions: TListView;
    memIssues: TMemo;
    memRecommendations: TMemo;
    memRC: TMemo;
    memSystems: TMemo;
    Panel1: TPanel;
    pnlRight: TPanel;
    pnlBottom: TPanel;
    pcRC: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    tsDefinitions: TTabSheet;
    tsRC: TTabSheet;
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lvDefinitionsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      {%H-}State: TCustomDrawState; Var {%H-}DefaultDraw: Boolean);
    Procedure lvDefinitionsDblClick(Sender: TObject);
    Procedure lvDefinitionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
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
  uBee512Support, OSSupport, Logs, Validators;

{$R *.lfm}

Procedure TfrmDefinitionExplorer.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  LoadRC;

  Caption := 'System Definition Explorer: ' + ubee512.RC;
End;

Procedure TfrmDefinitionExplorer.FormActivate(Sender: TObject);
Begin
  If Not Factivated Then
  Begin
    MakeFullyVisible;

    FActivated := True;
  End;
End;

Procedure TfrmDefinitionExplorer.lvDefinitionsDblClick(Sender: TObject);
Begin
  If (Assigned(lvDefinitions.Selected)) And (btnOK.Enabled) Then
    ModalResult := mrOk;
End;

Procedure TfrmDefinitionExplorer.lvDefinitionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
Var
  oDefinition: TDefinition;
  sRC: String;
Begin
  btnOK.Enabled := True;

  If (Selected) And (Assigned(Item)) Then
  Begin
    oDefinition := uBee512.Definitions[Item.Caption];
    sRC := '# ' + oDefinition.Description + LineEnding;
    sRC += '[' + oDefinition.Definition + ']' + LineEnding;
    sRC += oDefinition.RC;

    memSystems.Lines.Text := sRC;

    memIssues.Font.Color := ERRORLEVEL_COLOR[oDefinition.Validator.ErrorLevel];
    memIssues.Lines.Text := oDefinition.Validator.Outcome;
    memRecommendations.Lines.Text := oDefinition.Validator.Recommendation;

    btnOK.Enabled := oDefinition.Validator.ErrorLevel <> elError;
  End
  Else
    memSystems.Lines.Clear;
End;

Procedure TfrmDefinitionExplorer.lvDefinitionsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
Var
  oDefinition: TDefinition;
Begin
  oDefinition := TDefinition(Item.Data);

  lvDefinitions.Canvas.Font.Color := ERRORLEVEL_COLOR[oDefinition.Validator.ErrorLevel];
End;

Function TfrmDefinitionExplorer.GetTitle: String;
Begin
  Result := '';
  If Assigned(lvDefinitions.Selected) Then
    Result := lvDefinitions.Selected.SubItems[1];
End;

Function TfrmDefinitionExplorer.GetModel: String;
Begin
  Result := '';
  If Assigned(lvDefinitions.Selected) Then
    Result := lvDefinitions.Selected.SubItems[0];
End;

Procedure TfrmDefinitionExplorer.SetTitle(AValue: String);
Var
  sTitle: String;
  oItem: TListItem;
Begin
  sTitle := Lowercase(AValue);
  For oItem In lvDefinitions.Items Do
    If Lowercase(oItem.Subitems[1]) = sTitle Then
    Begin
      lvDefinitions.ItemIndex := oItem.Index;
      Break;
    End;
End;

Procedure TfrmDefinitionExplorer.LoadRC;
Var
  oDefinition: TDefinition;
  oItem: TListItem;
  sRam: String;
Begin
  If uBee512.Available And FileExists(uBee512.RC) Then
  Begin
    SetBusy;
    Try
      memRC.Lines.LoadFromFile(uBee512.RC);
      uBee512.LoadRC;
      pcRC.ActivePage := tsDefinitions;

      lvDefinitions.Items.Clear;
      memSystems.Lines.Clear;
      lvDefinitions.Items.BeginUpdate;

      For oDefinition In uBee512.Definitions Do
        If oDefinition.Title <> '' Then
        Begin
          oItem := lvDefinitions.Items.Add;

          oItem.Caption := oDefinition.Definition;
          oItem.SubItems.Add(oDefinition.Model);
          oItem.SubItems.Add(oDefinition.Title);
          oItem.SubItems.Add(MBTypeStr[oDefinition.MbeeType]);
          oItem.SubItems.Add(oDefinition.A);
          sRam := '';
          If (Trim(oDefinition.SRAM) <> '') Then
            sRam += oDefinition.SRAM + 'k';
          If (Trim(oDefinition.SRAM) <> '') And (Trim(oDefinition.SRAM_File) <> '') Then
            sRam += '=';
          If (Trim(oDefinition.SRAM_File) <> '') Then
            sRam += oDefinition.SRAM_File;
          oItem.SubItems.Add(sRam);
          oItem.SubItems.Add(oDefinition.Description);

          oDefinition.Validator.Process;
          oItem.Data := oDefinition;

          oItem.ImageIndex := Integer(oDefinition.Validator.ErrorLevel);
        End;
    Finally
      lvDefinitions.Items.EndUpdate;
      lvDefinitions.AutoSize := True;

      ClearBusy;
    End;
  End;
End;

End.
