Unit DialogDefinitionExplorer;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, IpHtml;

Type

  { TdlgDefinitionExplorer }

  TdlgDefinitionExplorer = Class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ilFlags: TImageList;
    lvDefinitions: TListView;
    htmlIssues: TIpHtmlPanel;
    memRC: TMemo;
    htmlRecommendations: TIpHtmlPanel;
    htmlSystems: TIpHtmlPanel;
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
  uBee512Support, OSSupport, ControlsSupport, StringSupport, Logs, Validators;

{$R *.lfm}

Procedure TdlgDefinitionExplorer.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  LoadRC;

  Caption := 'System Definition Explorer: ' + ubee512.RC;
End;

Procedure TdlgDefinitionExplorer.FormActivate(Sender: TObject);
Begin
  If Not Factivated Then
  Begin
    MakeFullyVisible;

    FActivated := True;
  End;
End;

Procedure TdlgDefinitionExplorer.lvDefinitionsDblClick(Sender: TObject);
Begin
  If (Assigned(lvDefinitions.Selected)) And (btnOK.Enabled) Then
    ModalResult := mrOk;
End;

Procedure TdlgDefinitionExplorer.lvDefinitionsSelectItem(Sender: TObject;
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

    SetHTML(htmlSystems, '<body>' + ValidateHTML(sRC) + '</body>');

    htmlIssues.Font.Color := ERRORLEVEL_COLOR[oDefinition.Validator.ErrorLevel];
    setHTML(htmlIssues, '<body>' + oDefinition.Validator.Outcome + '</body>');
    setHTML(htmlRecommendations, '<body>' + oDefinition.Validator.Recommendation + '</body>');

    btnOK.Enabled := oDefinition.Validator.ErrorLevel <> elError;
  End
  Else
  Begin
    SetHTML(htmlSystems, '<body/>');
    setHTML(htmlIssues, '<body/>');
    setHTML(htmlRecommendations, '<body/>');
  End;
End;

Procedure TdlgDefinitionExplorer.lvDefinitionsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
Var
  oDefinition: TDefinition;
Begin
  oDefinition := TDefinition(Item.Data);

  lvDefinitions.Canvas.Font.Color := ERRORLEVEL_COLOR[oDefinition.Validator.ErrorLevel];
End;

Function TdlgDefinitionExplorer.GetTitle: String;
Begin
  Result := '';
  If Assigned(lvDefinitions.Selected) Then
    Result := lvDefinitions.Selected.SubItems[1];
End;

Function TdlgDefinitionExplorer.GetModel: String;
Begin
  Result := '';
  If Assigned(lvDefinitions.Selected) Then
    Result := lvDefinitions.Selected.SubItems[0];
End;

Procedure TdlgDefinitionExplorer.SetTitle(AValue: String);
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

Procedure TdlgDefinitionExplorer.LoadRC;
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

      SetHTML(htmlSystems, '');
      setHTML(htmlIssues, '');
      setHTML(htmlRecommendations, '');

      lvDefinitions.Items.Clear;
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
