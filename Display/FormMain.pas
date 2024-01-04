Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, ShellCtrls, ExtCtrls, Buttons, Menus,
  FormSettings;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnExplorer: TBitBtn;
    btnLaunchuBee512: TBitBtn;
    cboModel: TComboBox;
    cboParallelPort: TComboBox;
    cboTitle: TComboBox;
    edtDiskA: TFileNameEdit;
    edtDiskB: TFileNameEdit;
    edtDiskC: TFileNameEdit;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label5: TLabel;
    lblDiskA: TLabel;
    lblDiskB: TLabel;
    lblDiskC: TLabel;
    lblPP: TLabel;
    lvcpmtoolsFiles: TListView;
    lvcpmtoolsWorkingFolder: TListView;
    MainMenu1: TMainMenu;
    memRC: TMemo;
    memOutput: TMemo;
    Separator1: TMenuItem;
    mnuSettings: TMenuItem;
    mnuExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuFile: TMenuItem;
    pcPreview: TPageControl;
    pnluBee512: TPanel;
    pnldskFiles: Tpanel;
    pnlCPMToolsMain: Tpanel;
    btnClearA: TSpeedButton;
    btnClearB: TSpeedButton;
    btnClearC: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    btnAddDSKtoA: TToolButton;
    ToolButton1: TToolButton;
    btnAddFolderToA: TToolButton;
    ToolButton2: TToolButton;
    btnAddDSKtoC: TToolButton;
    btnAddDSKtoB: TToolButton;
    btnAddFolderToB: TToolButton;
    btnAddFolderToC: TToolButton;
    tsFiles: TTabSheet;
    tsText: TTabSheet;
    Toolbar1: Ttoolbar;
    tvFolders: TShellTreeView;
    Procedure btnAddDSKtoAClick(Sender: TObject);
    Procedure btnAddDSKtoBClick(Sender: TObject);
    Procedure btnAddDSKtoCClick(Sender: TObject);
    Procedure btnClearAClick(Sender: TObject);
    Procedure btnClearBClick(Sender: TObject);
    Procedure btnClearCClick(Sender: TObject);
    Procedure btnExplorerClick(Sender: TObject);
    Procedure btnLaunchuBee512Click(Sender: TObject);
    Procedure cboModelChange(Sender: TObject);
    Procedure cboTitleChange(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure lvcpmtoolsWorkingFolderSelectItem(Sender: TObject; Item: TListItem;
      {%H-}Selected: Boolean);
    Procedure mnuAboutClick(Sender: TObject);
    Procedure mnuExitClick(Sender: TObject);
    Procedure mnuSettingsClick(Sender: TObject);
    Procedure tvFoldersChange(Sender: TObject; {%H-}Node: TTreeNode);
  Private
    FSettings: TSettings;
    FActivated: Boolean;
    FLoadingDSK: Boolean;
    FWorkingDir: String;

    Procedure LoadRC;
    Procedure LoadSettings;
    Procedure RefreshUI;
    Procedure SaveSettings;

    Function SelectedFile: String;
    Procedure SetCombo(ACombo: TComboBox; AValue: String);
  Public

  End;

Var
  frmMain: TfrmMain;

Implementation

Uses
  IniFiles, FileSupport, CPMSupport, cpmtoolsSupport, LazFileUtils, StringSupport,
  StrUtils, OSSupport, uBee512Support, FormMacroExplorer;

{$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;
End;

Procedure TfrmMain.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    LoadSettings;

    FActivated := True;
  End;
End;

Procedure TfrmMain.FormDestroy(Sender: TObject);
Begin
  SaveSettings;

  FreeAndNil(FSettings);
End;

Procedure TfrmMain.mnuAboutClick(Sender: TObject);
Begin
  ShowMessage('TODO');
End;

Procedure TfrmMain.mnuExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.mnuSettingsClick(Sender: TObject);
Var
  oSettings: TfrmSettings;
Begin
  FSettings.WorkingFolder := FWorkingDir;

  oSettings := TfrmSettings.Create(Self);
  Try
    oSettings.Settings := FSettings;

    If oSettings.ShowModal = mrOk Then
    Begin
      FSettings := oSettings.Settings;
      FSettings.ValidatePaths;

      LoadRC;

      FWorkingDir := FSettings.WorkingFolder;

      If DirectoryExists(FWorkingDir) Then
        tvFolders.Path := FWorkingDir;
    End;
  Finally
    oSettings.Free;
  End;
End;

Procedure TfrmMain.SetCombo(ACombo: TComboBox; AValue: String);
Var
  iIndex: Integer;
Begin
  iIndex := ACombo.Items.IndexOf(AValue);
  If (iIndex >= 0) Then
  Begin
    ACombo.ItemIndex := iIndex;
    ACombo.OnChange(Self);
  End;
End;

Procedure TfrmMain.LoadSettings;
Var
  iLeft, iWidth, iTop, iHeight: Integer;
  oIniFile: TIniFile;
  sModel, sTitle: String;
Begin

  oIniFile := TIniFile.Create(ChangeFileExt(Application.Exename, '.ini'));
  Try
    // Default, load Window Settings
    iLeft := oInifile.ReadInteger(Name, 'Left', Application.MainForm.Left);
    iTop := oInifile.ReadInteger(Name, 'Top', Application.MainForm.Top);
    iWidth := oInifile.ReadInteger(Name, 'Width', Application.MainForm.Width);
    iHeight := oInifile.ReadInteger(Name, 'Height', Application.MainForm.Height);

    SetBounds(iLeft, iTop, iWidth, iHeight);
    MakeFullyVisible;

    If oInifile.ReadBool(Name, 'Maximised', False) Then
      Application.MainForm.WindowState := wsMaximized
    Else
      Application.MainForm.WindowState := wsNormal;

    FSettings.LoadSettings(oIniFile);

    FWorkingDir := FSettings.WorkingFolder;

    If DirectoryExists(FWorkingDir) Then
      tvFolders.Path := FWorkingDir;

    edtDiskA.Text := oIniFile.ReadString('Selected', 'Disk A', '');
    edtDiskB.Text := oIniFile.ReadString('Selected', 'Disk B', '');
    edtDiskC.Text := oIniFile.ReadString('Selected', 'Disk C', '');

    LoadRC;

    sModel := oIniFile.ReadString('Selected', 'Model', 'p128k');
    sTitle := oIniFile.ReadString('Selected', 'Title', 'Premium 128K');

    SetCombo(cboModel, sModel);
    SetCombo(cboTitle, sTitle);
  Finally
    oInifile.Free;
  End;
End;

Procedure TfrmMain.SaveSettings;
Var
  oInifile: TIniFile;
Begin
  oInifile := TIniFile.Create(ChangeFileExt(Application.Exename, '.ini'));

  // Do all the SaveSettings work in memory
  oInifile.CacheUpdates := True;
  Try
    oInifile.WriteBool(Name, 'Maximised', Application.MainForm.WindowState = wsMaximized);

    If WindowState <> wsMaximized Then
    Begin
      oInifile.WriteInteger(Name, 'Left', Application.MainForm.Left);
      oInifile.WriteInteger(Name, 'Top', Application.MainForm.Top);
      oInifile.WriteInteger(Name, 'Width', Application.MainForm.Width);
      oInifile.WriteInteger(Name, 'Height', Application.MainForm.Height);
    End;

    FSettings.WorkingFolder := FWorkingDir;
    FSettings.SaveSettings(oInifile);

    oIniFile.WriteString('Selected', 'Disk A', edtDiskA.Text);
    oIniFile.WriteString('Selected', 'Disk B', edtDiskB.Text);
    oIniFile.WriteString('Selected', 'Disk C', edtDiskC.Text);

    oIniFile.WriteString('Selected', 'Model', cboModel.Text);
    oIniFile.WriteString('Selected', 'Title', cboTitle.Text);

    // And flush the settings out in one go
    // This works around an AVG issue whereby
    // it locks the ini file during repeated writes
    // causing a CreateError exception to be thrown
    oInifile.UpdateFile;
  Finally
    oInifile.Free;
  End;
End;

Procedure TfrmMain.LoadRC;
Begin
  uBee512LoadRC;

  cboModel.Items.CommaText := uBee512Models;

  If (cboModel.ItemIndex <> 0) And (cboModel.Items.Count > 0) Then
  Begin
    cboModel.ItemIndex := 0;
    cboModelChange(Self);
  End;
End;

Procedure TfrmMain.tvFoldersChange(Sender: TObject; Node: TTreeNode);
Var
  oSearchRec: TSearchRec;
  oListItem: TListItem;
  bBoot: Boolean;
  sExt: Rawbytestring;
Begin
  If Not FLoadingDSK Then
  Begin
    SetBusy;
    Try
      FLoadingDSK := True;
      FWorkingDir := IncludeSlash(tvFolders.Path);

      lvcpmtoolsWorkingFolder.Items.BeginUpdate;
      Try
        // Clear current folder file list
        lvcpmtoolsWorkingFolder.Clear;

        // Clear Preview
        memOutput.Lines.Clear;
        lvcpmtoolsFiles.Clear;

        If FindFirstUTF8(FWorkingDir + '*.*', faAnyFile, oSearchRec) = 0 Then
          Repeat
            If (oSearchRec.Name <> '.') And (oSearchRec.Name <> '..') And
              (oSearchRec.Name <> '') Then
            Begin
              sExt := Lowercase(ExtractFileExt(oSearchRec.Name));
              bBoot := (sExt = '.dsk') And (IsCPMBootableFile(FWorkingDir + oSearchRec.Name));

              oListItem := lvcpmtoolsWorkingFolder.Items.Add;
              oListItem.Caption := ExtractFileNameWithoutExt(oSearchRec.Name);
              oListItem.SubItems.Add(sExt);
              oListItem.SubItems.Add(BOOLEAN_YES_NO[bBoot]);
              oListItem.SubItems.Add(Format('%.1f', [oSearchRec.Size / 1024]));
              oListItem.SubItems.Add(DateTimeToStr(oSearchRec.TimeStamp));
            End;
          Until FindNextUTF8(oSearchRec) <> 0;

        FindCloseUTF8(oSearchRec);
      Finally
        lvcpmtoolsWorkingFolder.Items.EndUpdate;
        FLoadingDSK := False;
      End;
    Finally
      ClearBusy;
    End;
  End;
End;

Procedure TfrmMain.lvcpmtoolsWorkingFolderSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
Var
  slTemp: TStringList;
  sFile: String;
  arrStrings, arrFile: TStringDynArray;
  oListItem: TListItem;
  sRawOutput: String;
Begin
  If Assigned(Item) And (Item.SubItems.Count > 1) And
    (FileExists(FWorkingDir + Item.Caption + Item.SubItems[0])) And (Item.Selected) Then
  Begin
    SetBusy;
    Try
      lvcpmtoolsFiles.Items.BeginUpdate;
      lvcpmtoolsFiles.Items.Clear;
      memOutput.Lines.Text := '';
      sFile := FWorkingDir + Item.Caption + Item.SubItems[0];

      If Item.SubItems[0] = '.dsk' Then
      Begin
        slTemp := TStringList.Create;
        Try
          sRawOutput := '';

          slTemp.Text := cpmtoolsLS(sFile, sRawOutput);
          memOutput.Lines.Text := sRawOutput;

          For sFile In slTemp Do
          Begin
            // 0: -rw-rw-rw- 10496 Jan 01 1970  access.box
            arrStrings := SplitString(sFile, ' ');

            If Length(arrStrings) = 7 Then
            Begin
              arrFile := SplitString(arrStrings[6], '.');

              oListItem := lvcpmtoolsFiles.Items.Add;
              oListItem.Caption := arrStrings[0];
              oListItem.SubItems.Add(arrFile[0]); // Filename
              oListItem.SubItems.Add(arrFile[1]); // Ext
              oListItem.SubItems.Add(arrStrings[1]); // Attr
              oListItem.SubItems.Add(arrStrings[2]); // Size
              oListItem.SubItems.Add(Format('%s %s %s',
                [arrStrings[3], arrStrings[4], arrStrings[5]]));  // Date
            End;
          End;
        Finally
          slTemp.Free;
        End;

        pcPreview.ActivePage := tsFiles;
      End
      Else If IsTextFile(Item.SubItems[0]) Then
      Begin
        slTemp := TStringList.Create;
        Try
          slTemp.LoadFromFile(sFile);

          memOutput.Lines.Text := slTemp.Text;
        Finally
          slTemp.Free;
        End;

        pcPreview.ActivePage := tsText;
      End;

      // There may be an error message the user needs to see
      If (lvcpmtoolsFiles.Items.Count = 0) And (pcPreview.ActivePage = tsFiles) Then
        pcPreview.ActivePage := tsText;

      lvcpmtoolsFiles.Items.EndUpdate;
    Finally
      ClearBusy;
    End;
  End;

  RefreshUI;
End;

Procedure TfrmMain.btnClearAClick(Sender: TObject);
Begin
  edtDiskA.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearBClick(Sender: TObject);
Begin
  edtDiskB.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearCClick(Sender: TObject);
Begin
  edtDiskC.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnExplorerClick(Sender: TObject);
Var
  oForm: TfrmMacroExplorer;
Begin
  oForm := TfrmMacroExplorer.Create(Self);
  Try
    oForm.Title := cboTitle.Text;
    If oForm.ShowModal = mrOk Then
    Begin
      SetCombo(cboModel, oForm.Model);
      SetCombo(cboTitle, oForm.Title);
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.btnAddDSKtoAClick(Sender: TObject);
Begin
  edtDiskA.Text := SelectedFile;
  RefreshUI;
End;

Procedure TfrmMain.btnAddDSKtoBClick(Sender: TObject);
Begin
  edtDiskB.Text := SelectedFile;
  RefreshUI;
End;

Procedure TfrmMain.btnAddDSKtoCClick(Sender: TObject);
Begin
  edtDiskC.Text := SelectedFile;
  RefreshUI;
End;

Procedure TfrmMain.cboModelChange(Sender: TObject);
Begin
  cboTitle.Items.CommaText := uBee512Titles(cboModel.Text);

  If (cboTitle.ItemIndex <> 0) And (cboTitle.Items.Count > 0) Then
  Begin
    cboTitle.ItemIndex := 0;
    cboTitleChange(Self);
  End;
End;

Procedure TfrmMain.cboTitleChange(Sender: TObject);
Begin
  memRC.Lines.Text := uBee512MacroRCByTitle(cboTitle.Text);
  cboTitle.Hint := cboTitle.Text;
End;

Procedure TfrmMain.btnLaunchuBee512Click(Sender: TObject);

  Function DriveAsParam(AEdit: TFilenameEdit): String;
  Var
    sFormat: TCaption;
  Begin
    Result := '';

    If Trim(AEdit.Text) = '' Then
      Exit;

    If FileExists(AEdit.Text) Then
    Begin
      sFormat := DSKFormat(AEdit.Text);

      If (sFormat <> '') Then
        Result := '--format=' + sFormat;
    End
    Else If DirectoryExists(AEdit.Text) Then
      Result := '--type=rcpmfs --format=ds80';
  End;

Var
  sCommand: String;
  bHasA: Boolean;

  Function AddDriveToCommand(ADrive: String; AEdit: TFilenameEdit): Boolean;
  Var
    sFormat: String;
  Begin
    Result := False;

    sFormat := DriveAsParam(AEdit);
    If sFormat <> '' Then
    Begin
      sCommand := Format('%s %s -%s "%s"', [sCommand, sFormat, ADrive, AEdit.Text]);
      Result := True;
    End;
  End;

Begin
  sCommand := Format('"%s" %s', [FSettings.UBEE512_exe, cboModel.Text]);

  bHasA := AddDriveToCommand('a', edtDiskA);

  If bHasA Then
  Begin
    AddDriveToCommand('b', edtDiskB);
    AddDriveToCommand('c', edtDiskC);

    Run(sCommand);
  End;

End;

Function TfrmMain.SelectedFile: String;
Var
  oItem: TListItem;
Begin
  Result := '';
  If Assigned(lvcpmtoolsWorkingFolder.Selected) Then
    oItem := lvcpmtoolsWorkingFolder.Selected;

  If oItem.SubItems.Count > 2 Then
    Result := FWorkingDir + oItem.Caption + oItem.SubItems[0];
End;

Procedure TfrmMain.RefreshUI;
Var
  bFileSelected, bDSKBootable, bDSK: Boolean;
  oItem: TListItem;
Begin
  bDSKBootable := False;
  bDSK := False;

  bFileSelected := Assigned(lvcpmtoolsWorkingFolder.Selected);

  If bFileSelected Then
  Begin
    oItem := lvcpmtoolsWorkingFolder.Selected;

    If oItem.SubItems.Count > 2 Then
    Begin
      bDSKBootable := oItem.SubItems[1] = 'Yes';
      bDSK := (oItem.SubItems[0] = '.dsk');
    End;
  End;

  btnAddDSKToA.Enabled := bDSK And bDSKBootable;
  btnAddDSKToB.Enabled := bDSK;
  btnAddDSKToC.Enabled := bDSK;

  btnAddFolderToA.Enabled := False;
  btnAddFolderToB.Enabled := False;
  btnAddFolderToC.Enabled := False;

  btnLaunchuBee512.Enabled := uBee512Available And (edtDiskA.Text <> '');
End;

End.
