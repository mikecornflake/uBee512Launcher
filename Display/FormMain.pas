Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, ShellCtrls, ExtCtrls, Buttons, Menus,
  FormSettings, Logging;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnClearA: TSpeedButton;
    btnClearB: TSpeedButton;
    btnClearC: TSpeedButton;
    btnExplorer: TBitBtn;
    cboFormatB: TComboBox;
    cboFormatC: TComboBox;
    cboModel: TComboBox;
    cboType: TComboBox;
    cboParallelPort: TComboBox;
    cboTitle: TComboBox;
    cboFormatA: TComboBox;
    edtDiskA: TFileNameEdit;
    edtDiskB: TFileNameEdit;
    edtDiskC: TFileNameEdit;
    GroupBox1: TGroupBox;
    ilMain: TImageList;
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblDiskA: TLabel;
    lblDiskB: TLabel;
    lblDiskC: TLabel;
    lblPP: TLabel;
    lvcpmtoolsFiles: TListView;
    lvcpmtoolsWorkingFolder: TListView;
    MainMenu1: TMainMenu;
    memRC: TMemo;
    memOutput: TMemo;
    mnuDebug: TMenuItem;
    pcOptions: TPageControl;
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
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    btnAddDSKtoA: TToolButton;
    ToolBar2: TToolBar;
    btnSettings: TToolButton;
    ToolButton3: TToolButton;
    btnLaunchuBee512: TToolButton;
    ToolButton4: TToolButton;
    btnDebug: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    btnTest: TToolButton;
    ToolButton7: TToolButton;
    tsDrive: TTabSheet;
    tsROMs: TTabSheet;
    TabSheet3: TTabSheet;
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
    Procedure btnAddFolderToAClick(Sender: TObject);
    Procedure btnAddFolderToBClick(Sender: TObject);
    Procedure btnAddFolderToCClick(Sender: TObject);
    Procedure btnClearAClick(Sender: TObject);
    Procedure btnClearBClick(Sender: TObject);
    Procedure btnClearCClick(Sender: TObject);
    Procedure btnExplorerClick(Sender: TObject);
    Procedure btnLaunchuBee512Click(Sender: TObject);
    Procedure btnTestClick(Sender: TObject);
    Procedure cboModelChange(Sender: TObject);
    Procedure cboTitleChange(Sender: TObject);
    Procedure cboTypeChange(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure lvcpmtoolsWorkingFolderSelectItem(Sender: TObject; Item: TListItem;
      {%H-}Selected: Boolean);
    Procedure mnuAboutClick(Sender: TObject);
    Procedure mnuExitClick(Sender: TObject);
    Procedure mnuDebugClick(Sender: TObject);
    Procedure mnuSettingsClick(Sender: TObject);
    Procedure tvFoldersChange(Sender: TObject; {%H-}Node: TTreeNode);
  Private
    FSettings: TSettings;
    FActivated: Boolean;
    FLoadingDSK: Boolean;
    FWorkingDir: String;

    FLog: TLog;

    Procedure LoadRC;
    Procedure LoadSettings;
    Procedure RefreshUI;
    Procedure SaveSettings;

    Function SelectedFile: String;
    Procedure SetMacroCombo(ACombo: TComboBox; AValue: String);
    Procedure SetSelectedDisk(AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
    Procedure SetSelectedFolder(AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
  End;

Var
  frmMain: TfrmMain;

Implementation

Uses
  IniFiles, FileSupport, CPMSupport, cpmtoolsSupport, LazFileUtils, StringSupport,
  StrUtils, OSSupport, uBee512Support, FormMacroExplorer, FormDebug;

  {$R *.lfm}

Const
  DSK_ICO = 9;
  TXT_ICO = 10;

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;

  FLog := TLog.Create(ChangeFileExt(Application.Exename, '.log'));
  Debug(LineEnding + '-----------------------');

  Debug(Application.ExeName);
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
  FreeAndNil(FLog);
End;

Procedure TfrmMain.mnuAboutClick(Sender: TObject);
Begin
  ShowMessage('TODO');
End;

Procedure TfrmMain.mnuExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.mnuDebugClick(Sender: TObject);
Var
  oDebug: TfrmDebug;
Begin
  oDebug := TfrmDebug.Create(Self);
  Try
    oDebug.Load(FLog.Filename);

    oDebug.ShowModal;
  Finally
    oDebug.Free;
  End;
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
      Debug('Validating new settings');
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

Procedure TfrmMain.SetMacroCombo(ACombo: TComboBox; AValue: String);
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
  sIniFile, sType: String;
  mtType: TMbeeType;

Begin
  sIniFile := ChangeFileExt(Application.Exename, '.ini');
  Debug('Loading ' + sInifile);
  FLog.IncIndent;

  oIniFile := TIniFile.Create(sIniFile);
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

    cboFormatA.Text := oIniFile.ReadString('Selected', 'Disk A Format', 'Format?');
    cboFormatB.Text := oIniFile.ReadString('Selected', 'Disk B Format', 'Format?');
    cboFormatC.Text := oIniFile.ReadString('Selected', 'Disk C Format', 'Format?');

    LoadRC;

    sModel := oIniFile.ReadString('Selected', 'Model', 'p128k');
    sTitle := oIniFile.ReadString('Selected', 'Title', 'Premium 128K');

    mtType := uBee512.MbeeType(sModel);
    sType := MBTypeStr[mtType];

    SetMacroCombo(cboType, sType);
    SetMacroCombo(cboModel, sModel);
    SetMacroCombo(cboTitle, sTitle);
  Finally
    FLog.DecIndent;
    oInifile.Free;
  End;
End;

Procedure TfrmMain.SaveSettings;
Var
  oInifile: TIniFile;
  sInifile: String;
Begin
  sInifile := ChangeFileExt(Application.Exename, '.ini');
  Debug('Saving ' + sInifile);


  oInifile := TIniFile.Create(sInifile);

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

    oIniFile.WriteString('Selected', 'Disk A Format', cboFormatA.Text);
    oIniFile.WriteString('Selected', 'Disk B Format', cboFormatB.Text);
    oIniFile.WriteString('Selected', 'Disk C Format', cboFormatC.Text);

    // No need to save Type, it's inferred from Model
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
Var
  sPrev: TCaption;
  iPrev: Integer;
Begin
  Debug(Format('Loading ubee512rc [%s]', [uBee512.RC]));
  FLog.IncIndent;
  Try
    uBee512.LoadRC;

    sPrev := cboModel.Text;

    cboModel.Items.CommaText := uBee512.Models;

    If (cboModel.ItemIndex <> 0) And (cboModel.Items.Count > 0) Then
    Begin
      // Try and load the previous value instead of resetting
      iPrev := cboModel.Items.IndexOf(sPrev);
      If iPrev >= 0 Then
        cboModel.ItemIndex := iPrev
      Else
        cboModel.ItemIndex := 0;
      cboModel.OnChange(Self);
    End;
  Finally
    FLog.DecIndent;
  End;
End;

Procedure TfrmMain.tvFoldersChange(Sender: TObject; Node: TTreeNode);
Var
  oSearchRec: TSearchRec;
  oListItem: TListItem;
  bBoot, bIsDisk, bIsText: Boolean;
  sExt: Rawbytestring;
Begin
  If Not FLoadingDSK Then
  Begin
    SetBusy;
    Try
      FLoadingDSK := True;
      FWorkingDir := IncludeSlash(tvFolders.Path);
      Debug('Start scanning folder: ' + FWorkingDir);

      lvcpmtoolsWorkingFolder.Items.BeginUpdate;
      Try
        // Clear current folder file list
        lvcpmtoolsWorkingFolder.Clear;

        // Clear Preview
        memOutput.Lines.Clear;
        lvcpmtoolsFiles.Clear;

        If FindFirst(FWorkingDir + '*.*', faAnyFile, oSearchRec) = 0 Then
          Repeat
            If (oSearchRec.Name <> '.') And (oSearchRec.Name <> '..') And
              (oSearchRec.Name <> '') Then
            Begin
              sExt := Lowercase(ExtractFileExt(oSearchRec.Name));
              bIsDisk := ubee512.IsDisk(sExt) Or cpmtoolsIsDisk(sExt);
              bIsText := IsTextfile(sExt);
              bBoot := bIsDisk And (IsCPMBootableFile(FWorkingDir + oSearchRec.Name));

              oListItem := lvcpmtoolsWorkingFolder.Items.Add;
              oListItem.Caption := ExtractFileNameWithoutExt(oSearchRec.Name);
              oListItem.SubItems.Add(sExt);
              oListItem.SubItems.Add(BOOLEAN_YES_NO[bBoot]);
              oListItem.SubItems.Add(Format('%.1f', [oSearchRec.Size / 1024]));
              oListItem.SubItems.Add(DateTimeToStr(oSearchRec.TimeStamp));

              If bIsDisk Then
                oListItem.ImageIndex := DSK_ICO
              Else If bIsText Then
                oListItem.ImageIndex := TXT_ICO;
            End;
          Until FindNext(oSearchRec) <> 0;

        FindClose(oSearchRec);
      Finally
        lvcpmtoolsWorkingFolder.Items.EndUpdate;
        FLoadingDSK := False;
        Debug('End scanning folder: ' + FWorkingDir);
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
  sSelectedFile, sDSKFile: String;
  arrStrings, arrFile: TStringDynArray;
  oListItem: TListItem;
  sRawOutput: String;
Begin
  If Assigned(Item) And (Item.SubItems.Count > 1) And
    (FileExists(FWorkingDir + Item.Caption + Item.SubItems[0])) And (Item.Selected) Then
  Begin
    SetBusy;
    sSelectedFile := FWorkingDir + Item.Caption + Item.SubItems[0];
    Debug('Start Preview: ' + sSelectedFile);
    FLog.IncIndent;

    Try
      lvcpmtoolsFiles.Items.BeginUpdate;
      lvcpmtoolsFiles.Items.Clear;
      memOutput.Lines.Text := '';

      If Item.SubItems[0] = '.dsk' Then
      Begin
        slTemp := TStringList.Create;
        Try
          sRawOutput := '';

          slTemp.Text := cpmtoolsLS(sSelectedFile, sRawOutput);
          memOutput.Lines.Text := sRawOutput;

          For sDSKFile In slTemp Do
          Begin
            // 0: -rw-rw-rw- 10496 Jan 01 1970  access.box
            arrStrings := SplitString(sDSKFile, ' ');

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
          slTemp.LoadFromFile(sSelectedFile);

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
      FLog.DecIndent;
      Debug('End Preview: ' + sSelectedFile);

      ClearBusy;
    End;
  End;

  RefreshUI;
End;

Procedure TfrmMain.btnClearAClick(Sender: TObject);
Begin
  edtDiskA.Text := '';
  cboFormatA.Text := 'Format?';
  RefreshUI;
End;

Procedure TfrmMain.btnClearBClick(Sender: TObject);
Begin
  edtDiskB.Text := '';
  cboFormatB.Text := 'Format?';
  RefreshUI;
End;

Procedure TfrmMain.btnClearCClick(Sender: TObject);
Begin
  edtDiskC.Text := '';
  cboFormatC.Text := 'Format?';
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
      SetMacroCombo(cboModel, oForm.Model);
      SetMacroCombo(cboTitle, oForm.Title);
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.SetSelectedDisk(AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
Var
  sFormat: String;
  iFormat: Integer;
Begin
  AFilenameEdit.Text := ExcludeSlash(tvFolders.Path);

  sFormat := 'rcpmfs/ds80';
  iFormat := AFormatCombo.Items.IndexOf(sFormat);

  If iFormat >= 0 Then
    AFormatCombo.ItemIndex := iFormat
  Else
    AFormatCombo.Text := sFormat;

  RefreshUI;
End;

Procedure TfrmMain.SetSelectedFolder(AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
Var
  sFormat: String;
  iFormat: Integer;
Begin
  AFilenameEdit.Text := SelectedFile;

  // Make an educated guess as to Disk Format
  sFormat := DSKFormat(AFilenameEdit.Text);
  iFormat := AFormatCombo.Items.IndexOf(sFormat);

  If iFormat >= 0 Then
    AFormatCombo.ItemIndex := iFormat
  Else
    AFormatCombo.Text := sFormat;

  RefreshUI;
End;

Procedure TfrmMain.btnAddDSKtoAClick(Sender: TObject);
Begin
  SetSelectedDisk(edtDiskA, cboFormatA);
End;

Procedure TfrmMain.btnAddDSKtoBClick(Sender: TObject);
Begin
  SetSelectedDisk(edtDiskB, cboFormatB);
End;

Procedure TfrmMain.btnAddDSKtoCClick(Sender: TObject);
Begin
  SetSelectedDisk(edtDiskC, cboFormatC);
End;

Procedure TfrmMain.btnAddFolderToAClick(Sender: TObject);
Begin
  SetSelectedFolder(edtDiskA, cboFormatA);
End;

Procedure TfrmMain.btnAddFolderToBClick(Sender: TObject);
Begin
  SetSelectedFolder(edtDiskB, cboFormatB);
End;

Procedure TfrmMain.btnAddFolderToCClick(Sender: TObject);
Begin
  SetSelectedFolder(edtDiskC, cboFormatC);
End;

Procedure TfrmMain.cboTypeChange(Sender: TObject);
Var
  mtType: TMbeeType;
  sPrev: TCaption;
  iPrev: Integer;
Begin
  If cboType.ItemIndex >= 0 Then
  Begin
    mtType := TMbeeType(cboType.ItemIndex);

    sPrev := cboModel.Text;

    cboModel.Items.CommaText := uBee512.ModelsByType(mtType);
    If (cboModel.ItemIndex <> 0) And (cboModel.Items.Count > 0) Then
    Begin
      iPrev := cboModel.Items.IndexOf(sPrev);
      If iPrev >= 0 Then
        cboModel.ItemIndex := iPrev
      Else
        cboModel.ItemIndex := 0;

      cboModelChange(Self);
    End;
  End;

End;

Procedure TfrmMain.cboModelChange(Sender: TObject);
Var
  sPrev: TCaption;
  iPrev: Integer;
Begin
  sPrev := cboTitle.Text;

  cboTitle.Items.CommaText := uBee512.Titles(cboModel.Text);

  If (cboTitle.ItemIndex <> 0) And (cboTitle.Items.Count > 0) Then
  Begin
    iPrev := cboTitle.Items.IndexOf(sPrev);
    If iPrev >= 0 Then
      cboTitle.ItemIndex := iPrev
    Else
      cboTitle.ItemIndex := 0;

    cboTitleChange(Self);
  End;
End;

Procedure TfrmMain.cboTitleChange(Sender: TObject);
Var
  oMacro: TSystemMacro;
  sRC: String;
Begin
  oMacro := ubee512.MacroByTitle(cboTitle.Text);
  sRC := '# ' + oMacro.Description + LineEnding;
  sRC += '[' + oMacro.Macro + ']' + LineEnding;
  sRC += oMacro.RC;

  memRC.Lines.Text := sRC;

  edtDiskA.Enabled := (Trim(oMacro.A) = '');
  edtDiskB.Enabled := (Trim(oMacro.B) = '');
  edtDiskC.Enabled := (Trim(oMacro.C) = '');

  cboFormatA.Enabled := edtDiskA.Enabled;
  cboFormatB.Enabled := edtDiskB.Enabled;
  cboFormatC.Enabled := edtDiskC.Enabled;

  btnClearA.Enabled := edtDiskA.Enabled;
  btnClearB.Enabled := edtDiskB.Enabled;
  btnClearC.Enabled := edtDiskC.Enabled;
End;

Procedure TfrmMain.btnLaunchuBee512Click(Sender: TObject);

  Function DriveAsParam(AEdit: TFilenameEdit; AFormat: TCombobox): String;
  Var
    sFormat: TCaption;
  Begin
    Result := '';

    If Trim(AEdit.Text) = '' Then
      Exit;

    If FileExists(AEdit.Text) Then
    Begin
      sFormat := AFormat.Text;

      If (sFormat <> '') Then
        Result := '--format=' + sFormat;
    End
    Else If DirectoryExists(AEdit.Text) Then
      Result := '--type=rcpmfs --format=ds80';
  End;

Var
  sCommand, sResult, sDebug, s: String;
  bHasA: Boolean;
  slParams: TStringList;
  oMacro: TSystemMacro;

  Function AddDriveToCommand(ADrive: String; AEdit: TFilenameEdit; ACombo: TComboBox): Boolean;
  Var
    sFormat: String;
  Begin
    Result := False;

    If AEdit.Enabled Then
    Begin
      sFormat := DriveAsParam(AEdit, ACombo);
      If sFormat <> '' Then
      Begin
        If Pos('--type=rcpmfs', sFormat) > 0 Then
        Begin
          slParams.Add(TextBetween(sFormat, '', ' '));
          slParams.Add(TextBetween(sFormat, ' ', ''));
        End
        Else
          slParams.Add(sFormat);
        slParams.Add('-' + ADrive);
        slParams.Add(Format('%s', [AEdit.Text]));

        Result := True;
      End;
    End;
  End;

Begin
  slParams := TStringList.Create;
  Try
    oMacro := ubee512.MacroByTitle(cboModel.Text);

    // ubee512launcher will only work with
    sCommand := Format('%s', [FSettings.UBEE512_exe]);
    slParams.Add(oMacro.Macro);

    bHasA := AddDriveToCommand('a', edtDiskA, cboFormatA);
    If bHasA Then
    Begin
      AddDriveToCommand('b', edtDiskB, cboFormatB);
      AddDriveToCommand('c', edtDiskC, cboFormatC);
    End;

    sDebug := 'Launching ubee512 with:' + LineEnding;
    sDebug += '  ' + sCommand;
    For s In slParams Do
      sDebug += '  ' + s;

    Debug(sDebug);

    sResult := Trim(RunEx(sCommand, slParams));

    If sResult <> '' Then
      Debug('ubee512 returned:' + LineEnding + sResult);
  Finally
    slParams.Free;
  End;
End;

Procedure TfrmMain.btnTestClick(Sender: TObject);
Var
  sResult, sCommand: String;
Begin
  sCommand := Format('%s', [FSettings.UBEE512_exe]);
  sResult := Trim(RunEx(sCommand, ['--conio', '--echo', '@UBEE_USERHOME@', '--exit=-1']));
  Debug('Test ' + sResult);
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

  btnAddFolderToA.Enabled := True;
  btnAddFolderToB.Enabled := True;
  btnAddFolderToC.Enabled := True;

  btnLaunchuBee512.Enabled := (uBee512.Available) And (cboTitle.Text <> '');
End;

End.
